#include "carbon_compiler.h"
#include "vm/carbon_chunk.h"
#include "ast/carbon_expressions.h"
#include "ast/carbon_statements.h"
#include "carbon.h"
#include "carbon_object.h"
#include "carbon_token.h"
#include "carbon_value.h"
#include "utils/carbon_table.h"
#include "vm/carbon_chunk.h"
#include <stdint.h>
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
	[TokenBool] = ValueBool,	 [TokenString] = ValueString};

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

static void unaryOpNotSupported(CarbonToken op, char *type, CarbonCompiler *c) {
	fprintf(stderr,
			"[Line %d] Operator '%.*s' not supported for operand type %s\n",
			op.line, op.length, op.lexeme, type);
	c->hadError = true;
}

static void binaryOpNotSupported(CarbonToken op, char *left, char *right,
								 CarbonCompiler *c) {
	fprintf(
		stderr,
		"[Line %d] Operator '%.*s' not supported for operand types %s and %s\n",
		op.line, op.length, op.lexeme, left, right);
	c->hadError = true;
}
static void castNotSupported(CarbonValueType from, CarbonToken to,
							 CarbonCompiler *c) {

	fprintf(stderr, "[Line %d] Cannot cast from type %s to %.*s\n", to.line,
			CarbonValueTypeName[from], to.length, to.lexeme);
	c->hadError = true;
}

static void globalNotFound(CarbonToken token, CarbonCompiler *c) {
	fprintf(stderr, "[Line %d] Could not resolve global %.*s\n", token.line,
			token.length, token.lexeme);
	c->hadError = true;
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
			if (leftType < ValueString && rightType < ValueString) {
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
			CarbonString *name =
				carbon_copyString(var->token.lexeme, var->token.length, vm);

			CarbonValue out;
			if (carbon_tableGet(&c->globals, (CarbonObj *) name, &out)) {
				expr->evalsTo = out.uint;
				return;
			}
			globalNotFound(var->token, c);
			expr->evalsTo = ValueUnresolved;
			return;
		}
		default:
			fprintf(stderr, "COMPILER BUG: UNKNOWN EXPRESSION TYPE");
			expr->evalsTo = ValueUnresolved;
			return;
	}
}

static void pushValue(CarbonValue value, CarbonChunk *chunk,
					  CarbonToken token) {
	uint16_t index = carbon_addConstant(chunk, value);
	if (index > UINT8_MAX) {
		carbon_writeToChunk(chunk, OpLoadConstant16, token.line);
		carbon_writeToChunk(chunk, (uint8_t)(index >> 8), token.line);
		carbon_writeToChunk(chunk, (uint8_t)(index & 0xFF), token.line);
	} else {
		carbon_writeToChunk(chunk, OpLoadConstant, token.line);
		carbon_writeToChunk(chunk, index, token.line);
	}
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

	if(expr->evalsTo == ValueUntypechecked)
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
			CarbonString *name =
				carbon_copyString(var->token.lexeme, var->token.length, vm);
			pushValue(CarbonObject((CarbonObj *) name), chunk, var->token);
			carbon_writeToChunk(chunk, OpGetGlobal, var->token.line);
			break;
		}
	}
}

static bool canAssign(CarbonValueType to, CarbonValueType from) {
	return to <= ValueDouble && from <= to;
}

static void cantAssign(CarbonValueType to, CarbonValueType from, uint32_t line,
					   CarbonCompiler *c) {
	if (from != ValueUnresolved)
		fprintf(stderr, "[Line %d] Cannot assign type %s to type %s\n", line,
				CarbonValueTypeName[from], CarbonValueTypeName[to]);
	c->hadError = true;
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
			emit(OpPop, expr->last.line);
			break;
		}
		case StmtVarDec: {
			CarbonStmtVarDec *vardec = (CarbonStmtVarDec *) stmt;
			CarbonValueType vartype = t2value[vardec->type.type];
			CarbonString *name = carbon_copyString(
				vardec->identifier.lexeme, vardec->identifier.length, vm);
			if (vardec->initializer != NULL) {
				typecheck(vardec->initializer, c, vm);
				if (!canAssign(vartype, vardec->initializer->evalsTo)) {
					cantAssign(vartype, vardec->initializer->evalsTo,
							   vardec->identifier.line, c);
					carbon_tableSet(&c->globals, (CarbonObj *) name,
									CarbonUInt(vartype));
					break;
				} else {
					CarbonExpr *init = vardec->initializer;
					if (init->evalsTo <= ValueDouble &&
						init->evalsTo < vartype) {
						vardec->initializer = (CarbonExpr *) carbon_newCastExpr(
							v2token[vartype], init);
						vardec->initializer->evalsTo = vartype;
					}
					carbon_compileExpression(vardec->initializer, chunk, c, vm);
				}
			} else {
				emit(OpPush0, vardec->identifier.line);
			}
			pushValue(CarbonObject((CarbonObj *) name), chunk,
					  vardec->identifier);
			emit(OpSetGlobal, vardec->identifier.line);
			emit(OpPop, vardec->identifier.line);
			carbon_tableSet(&c->globals, (CarbonObj *) name,
							CarbonUInt(vartype));
			break;
		}
	}
#undef emit
}

void carbon_initCompiler(CarbonCompiler *compiler, CarbonParser *parser) {
	compiler->hadError = false;
	compiler->parserHadError = parser->hadError;
	carbon_tableInit(&compiler->globals);
}
void carbon_freeCompiler(CarbonCompiler *compiler) {
	compiler->hadError = false;
	compiler->parserHadError = false;
	carbon_tableFree(&compiler->globals);
}
