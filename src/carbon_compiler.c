#include "carbon_compiler.h"
#include "vm/carbon_chunk.h"
#include "ast/carbon_expressions.h"
#include "ast/carbon_statements.h"
#include "carbon.h"
#include "carbon_object.h"
#include "carbon_token.h"
#include "carbon_value.h"
#include "utils/carbon_table.h"
#include "utils/carbon_memory.h"
#include "vm/carbon_chunk.h"
#include "vm/carbon_vm.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
extern char *CarbonValueTypeName[];

static CarbonToken v2token[] = {
	[ValueDouble] = {.type = TokenDouble,
					 .lexeme = NULL,
					 .line = UINT32_MAX,
					 .length = 0},
	[ValueInt] = {
		.type = TokenInt, .lexeme = NULL, .line = UINT32_MAX, .length = 0}};

static CarbonValueType t2value[] = {
	[TokenInt] = ValueInt,		 [TokenUInt] = ValueUInt,
	[TokenDouble] = ValueDouble, [TokenIdentifier] = ValueInstance,
	[TokenBool] = ValueBool,	 [TokenString] = ValueString,
	[TokenVoid] = ValueVoid};

typedef struct {
	enum global_type { GlobalVariable, GlobalFunction } type;
	bool declared;
	CarbonString *name;
	CarbonValueType valueType;
} Global;

typedef struct {
	Global global;
	CarbonValueType returnType;
	uint8_t arity;
	struct func_args {
		CarbonString *name;
		CarbonValueType type;
	} * arguments;
} GlobalFunc;

#define global(name, valuetype, sizeType, type)                                \
	newGlobal(name, valuetype, type, sizeof(sizeType))

#define newVar(name, type) newGlobal(name, type, GlobalVariable, sizeof(Global))

static Global *newGlobal(CarbonString *name, CarbonValueType valueType,
						 enum global_type type, size_t size) {
	Global *g = carbon_reallocate(0, size, NULL);
	g->declared = false;
	g->name = name;
	g->type = type;
	g->valueType = valueType;
	return g;
}

static GlobalFunc *newFunc(CarbonString *name, CarbonValueType returnType,
						   uint32_t arity) {
	GlobalFunc *g =
		(GlobalFunc *) global(name, ValueFunction, GlobalFunc, GlobalFunction);
	g->arity = arity;
	g->returnType = returnType;
	g->arguments = carbon_reallocate(0, sizeof(struct func_args) * arity, NULL);
	return g;
}

#undef global

static CarbonValueType resolveLocalType(CarbonString *name, CarbonCompiler *c) {
	for (int16_t i = c->localCount - 1; i >= 0; i--) {
		CarbonLocal *l = &c->locals[i];
		if (l->name == name) {
			return l->type;
		}
	}
	return ValueVoid;
}

static int16_t resolveLocal(CarbonString *name, CarbonCompiler *c) {
	for (int16_t i = c->localCount - 1; i >= 0; i--) {
		CarbonLocal *l = &c->locals[i];
		if (l->name == name) {
			return i;
		}
	}
	return -1;
}

static CarbonExpr *promoteNumerics(CarbonValueType wants, CarbonExpr *given) {
	if (wants <= ValueDouble && given->evalsTo < wants) {
		CarbonExpr *r =
			(CarbonExpr *) carbon_newCastExpr(v2token[wants], given);
		r->evalsTo = wants;
		return r;
	}
	return given;
}

static bool canUnary(CarbonTokenType op, CarbonValueType operand) {
	switch (operand) {
		case ValueDouble:
		case ValueUInt:
		case ValueInt:
			return op == TokenMinus;
		case ValueBool:
			return op == TokenBang;
		default:
			return false;
	}
}

static bool canCast(CarbonValueType from, CarbonToken to) {
	switch (from) {
		case ValueUInt:
			return to.type == TokenInt || to.type == TokenDouble ||
				   to.type == TokenBool;
		case ValueInt:
			return to.type == TokenUInt || to.type == TokenDouble ||
				   to.type == TokenBool;
		case ValueDouble:
			return to.type == TokenUInt || to.type == TokenInt;
		case ValueBool:
			return to.type == TokenInt || to.type == TokenUInt;
		default:
			return false;
	}
}

static bool canBinary(CarbonTokenType op, CarbonValueType left,
					  CarbonValueType right) {

	bool leftNumeric =
		(left == ValueInt) || (left == ValueUInt) || (left == ValueDouble);
	bool rightNumeric =
		(right == ValueInt) || (right == ValueUInt) || (right == ValueDouble);

	bool bothStrings = left == ValueString && right == ValueString;

	switch (op) {
		case TokenPlus:
			if (bothStrings)
				return true;
		case TokenMinus:
		case TokenSlash:
		case TokenStar:
		case TokenGreaterThan:
		case TokenLessThan:
		case TokenGEQ:
		case TokenLEQ:
			return leftNumeric && rightNumeric;
		case TokenAnd:
		case TokenOr:
			return (left == ValueBool) && (right == ValueBool);
		default: // TokenEqualsEquals, TokenBangEquals
			return true;
	}
}

static bool canAssign(CarbonValueType to, CarbonValueType from) {
	return (to == from) || (to <= ValueDouble && from <= to);
}

static void cantPrint(CarbonToken tok, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Cannot print void values.\n", tok.line);
	c->hadError = true;
}

static void expectedReturnStatement(CarbonToken func, CarbonCompiler *c) {
	fprintf(stderr,
			"[Line %u] Function '%.*s' needs at least one return statement.\n",
			func.line, func.length, func.lexeme);
	c->hadError = true;
}

static void noReturnWanted(CarbonToken tok, CarbonString *functionName,
						   CarbonCompiler *c) {
	fprintf(stderr,
			"[Line %u] The function %s has a return type of void, it should "
			"have no return statements.\n",
			tok.line, functionName->chars);
	c->hadError = true;
}

static void wrongReturnType(CarbonToken tok, CarbonValueType wanted,
							CarbonValueType got, CarbonCompiler *c) {
	fprintf(stderr,
			"[Line %u] Wrong return type: function needs to return '%s', not "
			"'%s'.\n",
			tok.line, CarbonValueTypeName[wanted], CarbonValueTypeName[got]);
	c->hadError = true;
}

static void invalidCallee(uint32_t line, CarbonExpr *expr, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Type '%s' is not callable.\n", line,
			CarbonValueTypeName[expr->evalsTo]);
	c->hadError = true;
}

static void wrongArity(CarbonToken func, uint32_t wanted, uint32_t given,
					   CarbonCompiler *c) {
	fprintf(stderr,
			"[Line %u] Wrong number of arguments while calling function %.*s: "
			"Expected %u, got %u.\n",
			func.line, func.length, func.lexeme, wanted, given);
	c->hadError = true;
}
static void wrongArgumentType(CarbonToken argument, uint8_t n,
							  CarbonValueType wanted, CarbonValueType given,
							  CarbonCompiler *c) {
	fprintf(stderr,
			"[Line %u] Argument %u is of the wrong type: "
			"Expected %s, got %s.\n",
			argument.line, n, CarbonValueTypeName[wanted],
			CarbonValueTypeName[given]);
	c->hadError = true;
}

static void tooManyLocals(CarbonToken name, CarbonCompiler *c) {
	fprintf(stderr,
			"[Line %u] Too many locals: Not enough stack space for '%.*s'.\n",
			name.line, name.length, name.lexeme);
	c->hadError = true;
}

static void localRedeclaration(CarbonToken name, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Redeclaration of local variale '%.*s'.\n",
			name.line, name.length, name.lexeme);
	c->hadError = true;
}

static void globalRedeclaration(CarbonToken name, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Global %.*s has already been declared.\n",
			name.line, name.length, name.lexeme);
	c->hadError = true;
}

static void cantAssign(CarbonValueType to, CarbonValueType from, uint32_t line,
					   CarbonCompiler *c) {
	if (from != ValueUnresolved)
		fprintf(stderr, "[Line %u] Cannot assign type %s to type %s\n", line,
				CarbonValueTypeName[from], CarbonValueTypeName[to]);
	c->hadError = true;
}
static void globalFunctionAssignment(CarbonToken token, CarbonCompiler *c) {
	fprintf(stderr,
			"[Line %u] Global function(s) %.*s cannot be assigned to.\n",
			token.line, token.length, token.lexeme);
	c->hadError = true;
}

static void unaryOpNotSupported(CarbonToken op, char *type, CarbonCompiler *c) {
	fprintf(stderr,
			"[Line %u] Operator '%.*s' not supported for operand type %s\n",
			op.line, op.length, op.lexeme, type);
	c->hadError = true;
}

static void binaryOpNotSupported(CarbonToken op, char *left, char *right,
								 CarbonCompiler *c) {
	fprintf(
		stderr,
		"[Line %u] Operator '%.*s' not supported for operand types %s and %s\n",
		op.line, op.length, op.lexeme, left, right);
	c->hadError = true;
}
static void castNotSupported(CarbonValueType from, CarbonToken to,
							 CarbonCompiler *c) {

	fprintf(stderr, "[Line %u] Cannot cast from type %s to %.*s\n", to.line,
			CarbonValueTypeName[from], to.length, to.lexeme);
	c->hadError = true;
}

static void globalNotFound(CarbonToken token, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Could not resolve global %.*s\n", token.line,
			token.length, token.lexeme);
	c->hadError = true;
}

void carbon_markGlobal(CarbonStmt *stmt, CarbonCompiler *c, CarbonVM *vm) {
	CarbonValue dummy;
	switch (stmt->type) {
		case StmtVarDec: {
			CarbonStmtVarDec *vardec = (CarbonStmtVarDec *) stmt;
			CarbonString *name = carbon_strFromToken(vardec->identifier, vm);
			if (carbon_tableGet(&c->globals, (CarbonObj *) name, &dummy)) {
				globalRedeclaration(vardec->identifier, c);
				break;
			}
			Global *var = newVar(name, t2value[vardec->type.type]);
			carbon_tableSet(&c->globals, (CarbonObj *) name,
							CarbonObject((CarbonObj *) var));
			break;
		}
		case StmtFunc: {
			CarbonStmtFunc *func = (CarbonStmtFunc *) stmt;
			CarbonString *name = carbon_strFromToken(func->identifier, vm);
			if (carbon_tableGet(&c->globals, (CarbonObj *) name, &dummy)) {
				globalRedeclaration(func->identifier, c);
				break;
			}
			CarbonValueType returnType = t2value[func->returnType.type];
			GlobalFunc *g = newFunc(name, returnType, func->arity);
			g->arity = func->arity;
			for (uint32_t i = 0; i < func->arity; i++) {
				struct carbon_arg argument = func->arguments[i];
				CarbonValueType type = t2value[argument.type.type];
				CarbonString *name = carbon_strFromToken(argument.name, vm);
				g->arguments[i].name = name;
				g->arguments[i].type = type;
			}
			carbon_tableSet(&c->globals, (CarbonObj *) name,
							CarbonObject((CarbonObj *) g));
			break;
		}
		default:
			break;
	}
}

static void typecheck(CarbonExpr *expr, CarbonCompiler *c, CarbonVM *vm) {
	switch (expr->type) {
		case ExprUnary: {
			CarbonExprUnary *un = (CarbonExprUnary *) expr;
			if (un->operand == NULL) {
				expr->evalsTo = ValueUnresolved;
				return;
			}
			typecheck(un->operand, c, vm);
			CarbonValueType operandType = un->operand->evalsTo;
			if (operandType == ValueUnresolved) {
				expr->evalsTo = ValueUnresolved;
				return;
			}
			if (!canUnary(un->op.type, operandType)) {
				unaryOpNotSupported(un->op, CarbonValueTypeName[operandType],
									c);
				expr->evalsTo = ValueUnresolved;
			}
			switch (operandType) {
				case ValueBool:
				case ValueInt:
				case ValueDouble:
					expr->evalsTo = operandType;
					return;
				case ValueUInt:
					expr->evalsTo = ValueInt;
					return;
				default:
					expr->evalsTo = ValueUnresolved;
					return;
			}
		}
		case ExprBinary: {
			CarbonExprBinary *bin = (CarbonExprBinary *) expr;
			if (bin->left == NULL || bin->right == NULL) {
				expr->evalsTo = ValueUnresolved;
				return;
			}
			typecheck(bin->left, c, vm);
			typecheck(bin->right, c, vm);
			CarbonValueType leftType = bin->left->evalsTo;
			CarbonValueType rightType = bin->right->evalsTo;

			if (leftType == ValueUnresolved || rightType == ValueUnresolved) {
				expr->evalsTo = ValueUnresolved;
				return;
			}

			if (!canBinary(bin->op.type, leftType, rightType)) {
				binaryOpNotSupported(bin->op, CarbonValueTypeName[leftType],
									 CarbonValueTypeName[rightType], c);
				expr->evalsTo = ValueUnresolved;
				return;
			}

			CarbonValueType higherType = leftType;
			if (leftType <= ValueDouble && rightType <= ValueDouble) {
				if (rightType > leftType) {
					higherType = rightType;

					bin->left = (CarbonExpr *) carbon_newCastExpr(
						v2token[higherType], bin->left);
					bin->left->evalsTo = higherType;
				} else if (leftType > rightType) {
					bin->right = (CarbonExpr *) carbon_newCastExpr(
						v2token[higherType], bin->right);
					bin->right->evalsTo = higherType;
				}
			}

			switch (bin->op.type) {
				case TokenMinus:
					if (higherType == ValueUInt) {
						expr->evalsTo = ValueInt;
						return;
					}
				case TokenPlus:
				case TokenSlash:
				case TokenStar: {
					expr->evalsTo = higherType;
					return;
				}
				case TokenLessThan:
				case TokenGreaterThan:
				case TokenLEQ:
				case TokenGEQ:
				case TokenAnd:
				case TokenOr:
				case TokenEqualsEquals:
				case TokenBangEquals:
					expr->evalsTo = ValueBool;
					return;
				default:
					expr->evalsTo = ValueUnresolved; // Reaching here is a bug
					return;
			}
		}
		case ExprLiteral: {
			CarbonExprLiteral *lit = (CarbonExprLiteral *) expr;
			switch (lit->token.type) {
				case TokenTrue:
				case TokenFalse:
					expr->evalsTo = ValueBool;
					return;
				case TokenInteger:
					expr->evalsTo = ValueUInt;
					return;
				case TokenDecimal:
					expr->evalsTo = ValueDouble;
					return;
				case TokenNull:
					expr->evalsTo = ValueInstance;
				case TokenStringLiteral:
					expr->evalsTo = ValueString;
				default:
					return; // Should never reach here
			}
			return;
		}
		case ExprGrouping: {
			CarbonExprGrouping *group = (CarbonExprGrouping *) expr;
			if (group->expression == NULL) {
				expr->evalsTo = ValueUnresolved;
				return;
			}
			typecheck(group->expression, c, vm);
			expr->evalsTo = group->expression->evalsTo;
			return;
		}
		case ExprCast: {
			CarbonExprCast *cast = (CarbonExprCast *) expr;
			if (cast->expression == NULL) {
				expr->evalsTo = ValueUnresolved;
				return;
			}
			typecheck(cast->expression, c, vm);
			if (cast->expression->evalsTo == ValueUnresolved) {
				expr->evalsTo = ValueUnresolved;
				return;
			}

			if (!canCast(cast->expression->evalsTo, cast->to)) {
				castNotSupported(cast->expression->evalsTo, cast->to, c);
				expr->evalsTo = ValueUnresolved;
				return;
			}
			expr->evalsTo = t2value[cast->to.type];
			return;
		}
		case ExprVar: {
			CarbonExprVar *var = (CarbonExprVar *) expr;
			CarbonString *name = carbon_strFromToken(var->token, vm);

			CarbonValueType l = resolveLocalType(name, c);
			if (l != ValueVoid) {
				expr->evalsTo = l;
				return;
			}

			Global *out;
			if (carbon_tableGet(&c->globals, (CarbonObj *) name,
								(CarbonValue *) &out))
				if (out->declared || c->compilingTo != NULL) {
					expr->evalsTo = ((Global *) out)->valueType;
					return;
				}
			globalNotFound(var->token, c);
			expr->evalsTo = ValueUnresolved;
			return;
		}
		case ExprAssignment: {
			CarbonExprAssignment *assignment = (CarbonExprAssignment *) expr;
			if (assignment->right == NULL) {
				expr->evalsTo = ValueUnresolved;
				return;
			}

			typecheck(assignment->right, c, vm);
			CarbonString *name = carbon_strFromToken(assignment->left, vm);
			CarbonValueType leftType;

			CarbonValueType l = resolveLocalType(name, c);
			bool found = false;
			bool global;
			if (l != ValueVoid) {
				leftType = l;
				found = true;
				global = false;
			} else {
				Global *out;
				carbon_tableGet(&c->globals, (CarbonObj *) name,
								(CarbonValue *) &out);
				leftType = out->valueType;
				found = out->declared || c->compilingTo != NULL;
				global = true;
			}
			if (found) {
				if (leftType == ValueFunction && global) {
					globalFunctionAssignment(assignment->left, c);
				}
				if (!canAssign(leftType, assignment->right->evalsTo)) {
					cantAssign(leftType, assignment->right->evalsTo,
							   assignment->left.line, c);
					expr->evalsTo = ValueUnresolved;
					return;
				}

				assignment->right =
					promoteNumerics(leftType, assignment->right);

				expr->evalsTo = leftType;
				return;
			}

			globalNotFound(assignment->left, c);
			expr->evalsTo = ValueUnresolved;

			return;
		}
		case ExprCall: {
			CarbonExprCall *call = (CarbonExprCall *) expr;
			if (call->callee == NULL)
				return;
			typecheck(call->callee, c, vm);
			if (call->callee->evalsTo != ValueFunction &&
				call->callee->evalsTo != ValueUnresolved) {
				invalidCallee(call->line, call->callee, c);
				break;
			}
			CarbonExprVar *var = (CarbonExprVar *) call->callee;
			CarbonString *name = carbon_strFromToken(var->token, vm);
			GlobalFunc *g;
			if (!carbon_tableGet(&c->globals, (CarbonObj *) name,
								 (CarbonValue *) &g))
				break;

			if (g->arity != call->arity) {
				wrongArity(var->token, g->arity, call->arity, c);
				break;
			}

			for (uint8_t i = 0; i < g->arity; i++) {
				typecheck(call->arguments[i], c, vm);
				if (!canAssign(g->arguments[i].type,
							   call->arguments[i]->evalsTo)) {
					wrongArgumentType(var->token, i, g->arguments[i].type,
									  call->arguments[i]->evalsTo, c);
					break;
				}
				call->arguments[i] =
					promoteNumerics(g->arguments[i].type, call->arguments[i]);
				int x;
				x++;
			}
			expr->evalsTo = g->returnType;
			break;
		}
		default:
			fprintf(stderr, "COMPILER BUG: UNKNOWN EXPRESSION TYPE");
			expr->evalsTo = ValueUnresolved;
			return;
	}
}

static void push(uint16_t index, CarbonChunk *chunk, CarbonToken token) {
	if (index > UINT8_MAX) {
		carbon_writeToChunk(chunk, OpLoadConstant16, token.line);
		carbon_writeToChunk(chunk, (uint8_t)(index >> 8), token.line);
		carbon_writeToChunk(chunk, (uint8_t)(index & 0xFF), token.line);
	} else {
		carbon_writeToChunk(chunk, OpLoadConstant, token.line);
		carbon_writeToChunk(chunk, index, token.line);
	}
}

static void pushValue(CarbonValue value, CarbonChunk *chunk,
					  CarbonToken token) {
	uint16_t index = carbon_addConstant(chunk, value);
	push(index, chunk, token);
}

static void pushLiteral(CarbonExprLiteral *lit, CarbonChunk *chunk,
						CarbonVM *vm) {
	CarbonValue toPush;
	switch (lit->expr.evalsTo) {
		case ValueBool:
			if (lit->token.type == TokenTrue)
				carbon_writeToChunk(chunk, OpPush1, lit->token.line);
			else
				carbon_writeToChunk(chunk, OpPush0, lit->token.line);
			return;
		case ValueInstance:
			carbon_writeToChunk(chunk, OpPush0, lit->token.line);
			return;
		case ValueUInt: {
			uint64_t returnValue = 0;
			for (uint32_t i = 0; i < lit->token.length; i++) {
				returnValue = returnValue * 10 + lit->token.lexeme[i] - '0';
			}
			if (returnValue == 0) {
				carbon_writeToChunk(chunk, OpPush0, lit->token.line);
				return;
			} else if (returnValue == 1) {
				carbon_writeToChunk(chunk, OpPush1, lit->token.line);
				return;
			}
			toPush = CarbonUInt(returnValue);
			break;
		}
		case ValueDouble: {
			toPush = CarbonDouble(strtod(lit->token.lexeme, NULL));
			break;
		}
		case ValueString: {
			CarbonString *str = carbon_copyString(lit->token.lexeme + 1,
												  lit->token.length - 2, vm);
			toPush = CarbonObject((CarbonObj *) str);
			break;
		}
		default:
			return; // should never reach here
	}
	pushValue(toPush, chunk, lit->token);
}

void carbon_compileExpression(CarbonExpr *expr, CarbonChunk *chunk,
							  CarbonCompiler *c, CarbonVM *vm) {

	if (expr->evalsTo == ValueUntypechecked)
		typecheck(expr, c, vm);
	if (expr->evalsTo == ValueUnresolved)
		return;
	if (c->parserHadError)
		return;

	switch (expr->type) {
		case ExprUnary: {
			CarbonExprUnary *un = (CarbonExprUnary *) expr;
			carbon_compileExpression(un->operand, chunk, c, vm);
			if (un->op.type == TokenMinus)
				switch (un->operand->evalsTo) {
					case ValueUInt:
						carbon_writeToChunk(chunk, OpNegateUInt, un->op.line);
						break;
					case ValueInt:
						carbon_writeToChunk(chunk, OpNegateInt, un->op.line);
						break;
					case ValueDouble:
						carbon_writeToChunk(chunk, OpNegateDouble, un->op.line);
						break;
					default:
						break;
				}
			else if (un->op.type == TokenBang)
				carbon_writeToChunk(chunk, OpNegateBool, un->op.line);
			break;
		}

		case ExprBinary: {

#define binInstruction(type, instruction)                                      \
	case type:                                                                 \
		carbon_writeToChunk(chunk, instruction, bin->op.line)

			CarbonExprBinary *bin = (CarbonExprBinary *) expr;

			carbon_compileExpression(bin->left, chunk, c, vm);
			carbon_compileExpression(bin->right, chunk, c, vm);

			switch (bin->op.type) {
				case TokenPlus:
					switch (bin->left->evalsTo) {
						binInstruction(ValueInt, OpAddInt);
						break;
						binInstruction(ValueUInt, OpAddInt);
						break;
						binInstruction(ValueDouble, OpAddDouble);
						break;
						binInstruction(ValueString, OpConcat);
						break;
						default:
							break;
					}
					break;
				case TokenMinus:
					switch (bin->left->evalsTo) {
						binInstruction(ValueInt, OpSubInt);
						break;
						binInstruction(ValueUInt, OpSubInt);
						break;
						binInstruction(ValueDouble, OpSubDouble);
						break;
						default:
							break;
					}
					break;
				case TokenSlash:
					switch (bin->left->evalsTo) {
						binInstruction(ValueInt, OpDivInt);
						break;
						binInstruction(ValueUInt, OpDivUInt);
						break;
						binInstruction(ValueDouble, OpDivDouble);
						break;
						default:
							break;
					}
					break;
				case TokenStar:
					switch (bin->left->evalsTo) {
						binInstruction(ValueInt, OpMulInt);
						break;
						binInstruction(ValueUInt, OpMulUInt);
						break;
						binInstruction(ValueDouble, OpMulDouble);
						break;
						default:
							break;
					}
					break;
				case TokenGreaterThan:
					switch (bin->left->evalsTo) {
						binInstruction(ValueInt, OpCompareInt);
						break;
						binInstruction(ValueUInt, OpCompareUInt);
						break;
						binInstruction(ValueDouble, OpCompareDouble);
						break;
						default:
							break;
					}
					carbon_writeToChunk(chunk, OpGreater, bin->op.line);
					break;
				case TokenLessThan:
					switch (bin->left->evalsTo) {
						binInstruction(ValueInt, OpCompareInt);
						break;
						binInstruction(ValueUInt, OpCompareUInt);
						break;
						binInstruction(ValueDouble, OpCompareDouble);
						break;
						default:
							break;
					}
					carbon_writeToChunk(chunk, OpLess, bin->op.line);
					break;
				case TokenLEQ:
					switch (bin->left->evalsTo) {
						binInstruction(ValueInt, OpCompareInt);
						break;
						binInstruction(ValueUInt, OpCompareUInt);
						break;
						binInstruction(ValueDouble, OpCompareDouble);
						break;
						default:
							break;
					}
					carbon_writeToChunk(chunk, OpLEQ, bin->op.line);
					break;
				case TokenGEQ:
					switch (bin->left->evalsTo) {
						binInstruction(ValueInt, OpCompareInt);
						break;
						binInstruction(ValueUInt, OpCompareUInt);
						break;
						binInstruction(ValueDouble, OpCompareDouble);
						break;
						default:
							break;
					}
					carbon_writeToChunk(chunk, OpGEQ, bin->op.line);
					break;
				case TokenEqualsEquals:
					carbon_writeToChunk(chunk, OpEquals, bin->op.line);
					break;
				case TokenBangEquals:
					carbon_writeToChunk(chunk, OpNotEquals, bin->op.line);
					break;
				default:
					break;
			}
			break;
#undef binInstruction
		}
		case ExprLiteral: {
			CarbonExprLiteral *lit = (CarbonExprLiteral *) expr;
			pushLiteral(lit, chunk, vm);
			break;
		}
		case ExprGrouping: {
			CarbonExprGrouping *group = (CarbonExprGrouping *) expr;
			carbon_compileExpression(group->expression, chunk, c, vm);
			break;
		}
		case ExprCast: {
			CarbonExprCast *cast = (CarbonExprCast *) expr;
			carbon_compileExpression(cast->expression, chunk, c, vm);
			CarbonValueType from = cast->expression->evalsTo;
			switch (cast->expr.evalsTo) {
				case ValueInt:
					if (from == ValueDouble)
						carbon_writeToChunk(chunk, OpDoubleToInt,
											cast->to.line);
					break;
				case ValueUInt:
					if (from == ValueDouble)
						carbon_writeToChunk(chunk, OpDoubleToUInt,
											cast->to.line);
					break;
				case ValueDouble:
					if (from == ValueInt)
						carbon_writeToChunk(chunk, OpIntToDouble,
											cast->to.line);
					else if (from == ValueUInt)
						carbon_writeToChunk(chunk, OpUIntToDouble,
											cast->to.line);
					break;
				default: // should never reach here
					break;
			}

			break;
		}
		case ExprVar: {
			CarbonExprVar *var = (CarbonExprVar *) expr;
			CarbonString *name = carbon_strFromToken(var->token, vm);

			int16_t slot = resolveLocal(name, c);
			if (slot != -1) {
				carbon_writeToChunk(chunk, OpGetLocal, var->token.line);
				carbon_writeToChunk(chunk, slot & 0xFF, var->token.line);
				break;
			}

			uint16_t index =
				carbon_addConstant(chunk, CarbonObject((CarbonObj *) name));
			if (index > UINT8_MAX) {
				push(index, chunk, var->token);
				carbon_writeToChunk(chunk, OpGetGlobal, var->token.line);
				break;
			}
			carbon_writeToChunk(chunk, OpGetGlobalInline, var->token.line);
			carbon_writeToChunk(chunk, index & 0xFF, var->token.line);
			break;
		}
		case ExprAssignment: {
			CarbonExprAssignment *assignment = (CarbonExprAssignment *) expr;
			CarbonString *name = carbon_strFromToken(assignment->left, vm);
			carbon_compileExpression(assignment->right, chunk, c, vm);

			int16_t slot = resolveLocal(name, c);
			if (slot != -1) {
				carbon_writeToChunk(chunk, OpSetLocal, assignment->left.line);
				carbon_writeToChunk(chunk, slot & 0xFF, assignment->left.line);
				break;
			}

			uint16_t index =
				carbon_addConstant(chunk, CarbonObject((CarbonObj *) name));
			if (index > UINT8_MAX) {
				push(index, chunk, assignment->left);
				carbon_writeToChunk(chunk, OpSetGlobal, assignment->left.line);
				break;
			}
			carbon_writeToChunk(chunk, OpSetGlobalInline,
								assignment->left.line);
			carbon_writeToChunk(chunk, index & 0xFF, assignment->left.line);
			break;
		}
		case ExprCall: {
			CarbonExprCall *call = (CarbonExprCall *) expr;
			carbon_compileExpression(call->callee, chunk, c, vm);
			for (uint8_t i = 0; i < call->arity; i++) {
				carbon_compileExpression(call->arguments[i], chunk, c, vm);
			}
			carbon_writeToChunk(chunk, OpCall, call->line);
			carbon_writeToChunk(chunk, call->arity, call->line);
			break;
		}
	}
}

static bool inline isObject(CarbonValueType type) {
	return type >= ValueString && type <= ValueError;
}

static CarbonValue defaultState(CarbonValueType type, CarbonVM *vm) {
	switch (type) {
		case ValueString:
			return CarbonObject((CarbonObj *) carbon_copyString("", 0, vm));

		default:
			return CarbonUInt(0); // should never reach here;
	}
}

void carbon_compileStatement(CarbonStmt *stmt, CarbonChunk *chunk,
							 CarbonCompiler *c, CarbonVM *vm) {

#define emit(opcode, line)                                                     \
	if (!(c->parserHadError || c->hadError))                                   \
	carbon_writeToChunk(chunk, opcode, line)

	switch (stmt->type) {
		case StmtPrint: {
			CarbonStmtPrint *print = (CarbonStmtPrint *) stmt;
			if (print->expression == NULL)
				break;
			carbon_compileExpression(print->expression, chunk, c, vm);
			CarbonOpCode op;

			switch (print->expression->evalsTo) {
				case ValueInt:
					op = OpPrintInt;
					break;
				case ValueUInt:
					op = OpPrintUInt;
					break;
				case ValueDouble:
					op = OpPrintDouble;
					break;
				case ValueBool:
					op = OpPrintBool;
					break;
				case ValueVoid:
					cantPrint(print->token, c);
					break;
				default:
					op = OpPrintObj;
					break;
			}
			emit(op, print->token.line);
			break;
		}
		case StmtExpr: {
			CarbonStmtExpr *expr = (CarbonStmtExpr *) stmt;
			if (expr->expression == NULL)
				break;
			carbon_compileExpression(expr->expression, chunk, c, vm);
			if (expr->expression->evalsTo != ValueVoid)
				emit(OpPop, expr->last.line);
			break;
		}
		case StmtVarDec: {
			CarbonStmtVarDec *vardec = (CarbonStmtVarDec *) stmt;
			CarbonString *name = carbon_strFromToken(vardec->identifier, vm);

			bool isLocal = c->compilingTo != NULL;

			Global *g;
			carbon_tableGet(&c->globals, (CarbonObj *) name,
							(CarbonValue *) &g);

			CarbonValueType vartype = t2value[vardec->type.type];

			if (vardec->initializer != NULL) {
				typecheck(vardec->initializer, c, vm);
				if (!canAssign(vartype, vardec->initializer->evalsTo)) {
					cantAssign(vartype, vardec->initializer->evalsTo,
							   vardec->identifier.line, c);
					if (!isLocal)
						g->declared = true;
					break;
				} else {
					vardec->initializer =
						promoteNumerics(vartype, vardec->initializer);

					carbon_compileExpression(vardec->initializer, chunk, c, vm);
				}
			} else {
				if (isObject(vartype))
					pushValue(defaultState(vartype, vm), chunk,
							  vardec->identifier);
				else
					emit(OpPush0, vardec->identifier.line);
			}

			if (!isLocal) {
				g->declared = true;
				uint16_t index =
					carbon_addConstant(chunk, CarbonObject((CarbonObj *) name));
				if (index > UINT8_MAX) {
					push(index, chunk, vardec->identifier);
					emit(OpSetGlobal, vardec->identifier.line);
				} else {
					emit(OpSetGlobalInline, vardec->identifier.line);
					emit(index, vardec->identifier.line);
				}
				emit(OpPop, vardec->identifier.line);
				if (!isObject(vartype))
					carbon_tableSet(&vm->primitives, (CarbonObj *) name,
									CarbonUInt(0));
			} else {
				int16_t depth = resolveLocal(name, c);
				if (depth != c->depth) {
					CarbonLocal l = {c->depth, name, vartype};
					if (c->localCount != 255)
						c->locals[c->localCount++] = l;
					else {
						tooManyLocals(vardec->identifier, c);
					}
				} else {
					localRedeclaration(vardec->identifier, c);
				}
			}
			break;
		}
		case StmtFunc: {
			CarbonStmtFunc *sfunc = (CarbonStmtFunc *) stmt;
			CarbonString *name = carbon_strFromToken(sfunc->identifier, vm);

			Global *g;
			carbon_tableGet(&c->globals, (CarbonObj *) name,
							(CarbonValue *) &g);

			CarbonValueType returnType = t2value[sfunc->returnType.type];
			CarbonFunction *ofunc =
				carbon_newFunction(name, sfunc->arity, returnType, vm);
			c->compilingTo = ofunc;
			g->declared = true;
			c->localCount = sfunc->arity;
			for (uint8_t i = 0; i < sfunc->arity; i++) {
				CarbonToken name = sfunc->arguments[i].name;
				c->locals[i].depth = 0;
				c->locals[i].name = carbon_strFromToken(name, vm);
				c->locals[i].type = t2value[sfunc->arguments[i].type.type];
			}
			bool hadReturn = false;

			for (uint32_t i = 0; i < sfunc->statements.count; i++) {
				if (sfunc->statements.arr[i]->type == StmtReturn)
					hadReturn = true;
				carbon_compileStatement(sfunc->statements.arr[i], &ofunc->chunk,
										c, vm);
			}
			if (!hadReturn && c->compilingTo->returnType != ValueVoid) {
				expectedReturnStatement(sfunc->identifier, c);
			}

			c->compilingTo = NULL;
			c->localCount = 0;
			if (returnType == ValueVoid) {
				carbon_writeToChunk(&ofunc->chunk, OpReturnVoid, sfunc->end);
			}
			carbon_tableSet(&vm->globals, (CarbonObj *) name,
							CarbonObject((CarbonObj *) ofunc));

			break;
		}
		case StmtReturn: {
			CarbonStmtReturn *ret = (CarbonStmtReturn *) stmt;
			if (ret->expression != NULL) {
				if (c->compilingTo->returnType == ValueVoid) {
					noReturnWanted(ret->token, c->compilingTo->name, c);
					break;
				}
				typecheck(ret->expression, c, vm);
				if (ret->expression->evalsTo == ValueUnresolved)
					break;
				CarbonValueType wanted = c->compilingTo->returnType;
				CarbonValueType given = ret->expression->evalsTo;
				if (!canAssign(wanted, given)) {
					wrongReturnType(ret->token, wanted, given, c);
				}

				ret->expression = promoteNumerics(wanted, ret->expression);
				carbon_compileExpression(ret->expression, chunk, c, vm);

				carbon_writeToChunk(chunk, OpReturn, ret->token.line);
				break;
			}
			if (c->compilingTo->returnType != ValueVoid) {
				wrongReturnType(ret->token, c->compilingTo->returnType,
								ValueVoid, c);
				break;
			}
			carbon_writeToChunk(chunk, OpReturnVoid, ret->token.line);
			break;
		}
	}
#undef emit
}

void carbon_initCompiler(CarbonCompiler *compiler, CarbonParser *parser) {
	compiler->hadError = false;
	compiler->parserHadError = parser->hadError;
	compiler->compilingTo = NULL;
	compiler->localCount = 0;
	compiler->depth = 0;
	carbon_tableInit(&compiler->globals);
}
void carbon_freeCompiler(CarbonCompiler *compiler) {
	compiler->hadError = false;
	compiler->parserHadError = false;
	for (uint32_t i = 0; i < compiler->globals.capacity; i++) {
		if (compiler->globals.entries[i].key != NULL) {
			Global *g = (Global *) compiler->globals.entries[i].value.obj;
			if (g->type == GlobalVariable)
				carbon_reallocate(sizeof(Global), 0, g);
			else if (g->type == GlobalFunction) {
				GlobalFunc *f = (GlobalFunc *) g;
				carbon_reallocate(sizeof(struct func_args) * f->arity, 0,
								  f->arguments);
				carbon_reallocate(sizeof(GlobalFunc), 0, g);
			}
		}
	}
	carbon_tableFree(&compiler->globals);
}

#undef newVar
