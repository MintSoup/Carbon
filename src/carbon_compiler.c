#include "carbon_compiler.h"
#include "vm/carbon_chunk.h"
#include "ast/carbon_expressions.h"
#include "carbon_token.h"
#include "carbon_value.h"
#include "vm/carbon_chunk.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern char *CarbonValueTypeName[];

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

static bool canBinary(CarbonTokenType op, CarbonValueType left,
					  CarbonValueType right) {
	if (left != right) {
		if ((left == ValueUInt && right == ValueInt) ||
			(left == ValueInt && right == ValueUInt))
			;
		else
			return false;
	}

	switch (op) {
		case TokenPlus:
		case TokenMinus:
		case TokenSlash:
		case TokenStar:
		case TokenGreaterThan:
		case TokenLessThan:
		case TokenGEQ:
		case TokenLEQ:
			return (left == ValueUInt) || (left == ValueInt) ||
				   (left == ValueDouble);
		case TokenAnd:
		case TokenOr:
			return left == ValueBool;
		default: // TokenEqualsEquals, TokenBangEquals
			return true;
	}
}

static void unaryOpNotSupported(CarbonToken op, char *type) {
	fprintf(stderr,
			"[Line %d] Operator '%.*s' not supported for operand type %s\n",
			op.line, op.length, op.lexeme, type);
}

static void binaryOpNotSupported(CarbonToken op, char *left, char *right) {
	fprintf(
		stderr,
		"[Line %d] Operator '%.*s' not supported for operand types %s and %s\n",
		op.line, op.length, op.lexeme, left, right);
}

static void typecheck(CarbonExpr *expr) {
	if (expr == NULL)
		return;
	switch (expr->type) {
		case ExprUnary: {
			CarbonExprUnary *un = (CarbonExprUnary *) expr;
			typecheck(un->operand);
			CarbonValueType operandType = un->operand->evalsTo;
			if (operandType == ValueUnresolved) {
				expr->evalsTo = ValueUnresolved;
				return;
			}
			if (!canUnary(un->op.type, operandType)) {
				unaryOpNotSupported(un->op, CarbonValueTypeName[operandType]);
				expr->evalsTo = ValueUnresolved;
			}
			switch (operandType) {
				case ValueBool:
				case ValueInt:
				case ValueDouble:
					expr->evalsTo = operandType;
					return;
				case ValueUInt:
					expr->evalsTo = ValueUInt;
					return;
				default:
					expr->evalsTo = ValueUnresolved;
					return;
			}
		}
		case ExprBinary: {
			CarbonExprBinary *bin = (CarbonExprBinary *) expr;
			typecheck(bin->left);
			typecheck(bin->right);
			CarbonValueType leftType = bin->left->evalsTo;
			CarbonValueType rightType = bin->right->evalsTo;

			if (leftType == ValueUnresolved) {
				expr->evalsTo = ValueUnresolved;
				return;
			}
			if (rightType == ValueUnresolved) {
				expr->evalsTo = ValueUnresolved;
				return;
			}

			if (!canBinary(bin->op.type, leftType, rightType)) {
				binaryOpNotSupported(bin->op, CarbonValueTypeName[leftType],
									 CarbonValueTypeName[rightType]);
				expr->evalsTo = ValueUnresolved;
				return;
			}
			switch (bin->op.type) {
				case TokenPlus:
				case TokenMinus:
				case TokenSlash:
				case TokenStar:
					expr->evalsTo = leftType;
					return;
				case TokenLessThan:
				case TokenGreaterThan:
				case TokenLEQ:
				case TokenGEQ:
				case TokenAnd:
				case TokenOr:
					expr->evalsTo = ValueBool;
					return;
				default:
					expr->evalsTo = ValueUnresolved;
					return;
			}
		}
		case ExprLiteral: {
			CarbonExprLiteral *lit = (CarbonExprLiteral *) expr;
			if (lit->token.lexeme[0] == '\'') {
				expr->evalsTo = ValueString;
				return;
			}
			if (lit->token.length == 4) {
				if (memcmp(lit->token.lexeme, "null", 4) == 0) {
					expr->evalsTo = ValueInstance;
					return;
				}
				if (memcmp(lit->token.lexeme, "true", 4) == 0) {
					expr->evalsTo = ValueBool;
					return;
				}
			}
			if (lit->token.length == 5) {
				if (memcmp(lit->token.lexeme, "false", 5) == 0) {
					expr->evalsTo = ValueBool;
					return;
				}
			}

			for (uint32_t i = 0; i < lit->token.length; i++) {
				if (lit->token.lexeme[i] == '.') {
					expr->evalsTo = ValueDouble;
					return;
				}
			}
			expr->evalsTo = ValueUInt;
			return;
		}
		case ExprGrouping: {
			CarbonExprGrouping *group = (CarbonExprGrouping *) expr;
			typecheck(group->expression);
			expr->evalsTo = group->expression->evalsTo;
			return;
		}
		default:
			fprintf(stderr, "COMPILER BUG: UNKNOWN EXPRESSION TYPE");
			expr->evalsTo = ValueUnresolved;
			return;
	}
}

static CarbonValue getLiteralValue(CarbonExprLiteral *lit) {
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
			// I hate my life
			// Why didn't I do this in the lexer
			// Note to self: make all token lexemes null-terminated if you
			// have to do this again
			char *lex = malloc(lit->token.length + 1);
			memcpy(lex, lit->token.lexeme, lit->token.length);
			lex[lit->token.length] = 0;
			double result = strtod(lex, NULL);
			free(lex);
			return CarbonDouble(result);
		}
		default:
			return CarbonInt(0);
	}
}

void carbon_compileExpression(CarbonExpr *expr, CarbonChunk *chunk) {
	typecheck(expr);
	if (expr->evalsTo == ValueUnresolved)
		return;

	switch (expr->type) {
		case ExprUnary: {
			CarbonExprUnary *un = (CarbonExprUnary *) expr;
			carbon_compileExpression(un->operand, chunk);
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
				carbon_writeToChunk(chunk, OpNegateDouble, un->op.line);
			break;
		}

		case ExprBinary: {

#define binInstruction(type, instruction)                                      \
	case type:                                                                 \
		carbon_writeToChunk(chunk, instruction, bin->op.line)

			CarbonExprBinary *bin = (CarbonExprBinary *) expr;
			carbon_compileExpression(bin->left, chunk);
			carbon_compileExpression(bin->right, chunk);
			switch (bin->op.type) {
				case TokenPlus:
					switch (bin->left->evalsTo) {
						binInstruction(ValueInt, OpAddInt);
						break;
						binInstruction(ValueUInt, OpAddUInt);
						break;
						binInstruction(ValueDouble, OpAddDouble);
						break;
						default:
							break;
					}
					break;
				case TokenMinus:
					switch (bin->left->evalsTo) {
						binInstruction(ValueInt, OpSubInt);
						break;
						binInstruction(ValueUInt, OpSubUInt);
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
				default:
					break;
			}
			break;
#undef binInstruction
		}
		case ExprLiteral: {
			CarbonExprLiteral *lit = (CarbonExprLiteral *) expr;
			CarbonValue value = getLiteralValue(lit);
			uint16_t index = carbon_addConstant(chunk, value);
			if (index > UINT8_MAX) {
				carbon_writeToChunk(chunk, OpLoadConstant16, lit->token.line);
				carbon_writeToChunk(chunk, (uint8_t) index >> 8,
									lit->token.line);
				carbon_writeToChunk(chunk, (uint8_t) index | 0xFF,
									lit->token.line);
			} else {
				carbon_writeToChunk(chunk, OpLoadConstant, lit->token.line);
				carbon_writeToChunk(chunk, index, lit->token.line);
			}
			break;
		}
		case ExprGrouping: {
			CarbonExprGrouping *group = (CarbonExprGrouping *) expr;
			carbon_compileExpression(group->expression, chunk);
			break;
		}
	}
}
