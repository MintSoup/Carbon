#include "carbon_compiler.h"
#include "vm/carbon_chunk.h"
#include "ast/carbon_expressions.h"
#include "carbon_object.h"
#include "carbon_token.h"
#include "carbon_value.h"
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
	[TokenInt] = ValueInt,
	[TokenUInt] = ValueUInt,
	[TokenDouble] = ValueDouble,
	[TokenIdentifier] = ValueInstance,
};

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
			if(bothStrings) return true;
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

static void typecheck(CarbonExpr *expr, CarbonCompiler *c) {
	switch (expr->type) {
		case ExprUnary: {
			CarbonExprUnary *un = (CarbonExprUnary *) expr;
			if (un->operand == NULL) {
				expr->evalsTo = ValueUnresolved;
				return;
			}
			typecheck(un->operand, c);
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
			typecheck(bin->left, c);
			typecheck(bin->right, c);
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
			if(leftType < ValueString && rightType < ValueString)
			{
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
			typecheck(group->expression, c);
			expr->evalsTo = group->expression->evalsTo;
			return;
		}
		case ExprCast: {
			CarbonExprCast *cast = (CarbonExprCast *) expr;
			if (cast->expression == NULL) {
				expr->evalsTo = ValueUnresolved;
				return;
			}
			typecheck(cast->expression, c);
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
		default:
			fprintf(stderr, "COMPILER BUG: UNKNOWN EXPRESSION TYPE");
			expr->evalsTo = ValueUnresolved;
			return;
	}
}

static CarbonValue getLiteralValue(CarbonExprLiteral *lit, CarbonVM *vm) {
	switch (lit->expr.evalsTo) {
		case ValueBool: {
			if (lit->token.length == 4)
				return CarbonBool(true);
			return CarbonBool(false); // lmao
		}
		case ValueInstance:
			return CarbonUInt(0); // Null
		case ValueUInt: {
			uint64_t returnValue = 0;
			for (uint32_t i = 0; i < lit->token.length; i++) {
				returnValue = returnValue * 10 + lit->token.lexeme[i] - '0';
			}
			return CarbonUInt(returnValue);
		}
		case ValueDouble: {
			return CarbonDouble(strtod(lit->token.lexeme, NULL));
		}
		case ValueString: {
			CarbonString *str =
				carbon_copyString(lit->token.lexeme + 1, lit->token.length - 2, vm);
			return CarbonObject((CarbonObj *) str);
		}
		default:
			return CarbonInt(0);
	}
}

void carbon_compileExpression(CarbonExpr *expr, CarbonChunk *chunk,
							  CarbonCompiler *c, CarbonVM *vm) {

	typecheck(expr, c);
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
			CarbonValue value = getLiteralValue(lit, vm);
			uint16_t index = carbon_addConstant(chunk, value);
			if (index > UINT8_MAX) {
				carbon_writeToChunk(chunk, OpLoadConstant16, lit->token.line);
				carbon_writeToChunk(chunk, (uint8_t)(index >> 8),
									lit->token.line);
				carbon_writeToChunk(chunk, (uint8_t)(index & 0xFF),
									lit->token.line);
			} else {
				carbon_writeToChunk(chunk, OpLoadConstant, lit->token.line);
				carbon_writeToChunk(chunk, index, lit->token.line);
			}
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
	}
}

void carbon_initCompiler(CarbonCompiler *compiler, CarbonParser *parser) {
	compiler->hadError = false;
	compiler->parserHadError = parser->hadError;
}
void carbon_freeCompiler(CarbonCompiler *compiler) {
	compiler->hadError = false;
	compiler->parserHadError = false;
}
