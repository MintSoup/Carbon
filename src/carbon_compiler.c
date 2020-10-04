#include "carbon_compiler.h"
#include "ast/carbon_expressions.h"
#include "carbon_token.h"
#include "carbon_value.h"
#include <stdio.h>
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

static CarbonValueType typecheck(CarbonExpr *expr) {
	switch (expr->type) {
	case ExprUnary: {
		CarbonExprUnary *un = (CarbonExprUnary *) expr;
		CarbonValueType operandType = typecheck(un->operand);
		if (!canUnary(un->op.type, operandType)) {
			unaryOpNotSupported(un->op, CarbonValueTypeName[operandType]);
			return ValueUnresolved;
		}
		switch (operandType) {
		case ValueBool:
		case ValueInt:
		case ValueDouble:
			return operandType;
		case ValueUInt:
			return ValueInt;
		default: // This should never be called
			return ValueUnresolved;
		}
	}
	case ExprBinary: {
		CarbonExprBinary *bin = (CarbonExprBinary *) expr;
		CarbonValueType leftType = typecheck(bin->left);
		CarbonValueType rightType = typecheck(bin->right);
		if (!canBinary(bin->op.type, leftType, rightType)) {
			binaryOpNotSupported(bin->op, CarbonValueTypeName[leftType],
								 CarbonValueTypeName[rightType]);
			return ValueUnresolved;
		}
		switch (bin->op.type) {
		case TokenPlus:
		case TokenMinus:
		case TokenSlash:
		case TokenStar:
			return leftType;
		case TokenLessThan:
		case TokenGreaterThan:
		case TokenLEQ:
		case TokenGEQ:
		case TokenAnd:
		case TokenOr:
			return ValueBool;
		default:
			return ValueUnresolved;
		}
	}
	case ExprLiteral: {
		CarbonExprLiteral *lit = (CarbonExprLiteral *) expr;
		if (lit->token.lexeme[0] == '\'') return ValueString;
		if (lit->token.length == 4 || lit->token.length == 5) {
			if (memcmp(lit->token.lexeme, "null", 4) == 0) return ValueInstance;
			if (memcmp(lit->token.lexeme, "true", 4) == 0) return ValueBool;
			if (memcmp(lit->token.lexeme, "false", 5) == 0) return ValueBool;
		}
		for (uint32_t i = 0; i < lit->token.length; i++) {
			if (lit->token.lexeme[i] == '.') return ValueDouble;
		}
		return ValueUInt;
	}
	case ExprGrouping: {
		CarbonExprGrouping *group = (CarbonExprGrouping *) expr;
		return typecheck(group->expression);
	}
	default: return ValueUnresolved; // should never reach here
	}
}

void carbon_compile(CarbonExpr *expr, CarbonVM *vm) {
	printf("%s\n", CarbonValueTypeName[typecheck(expr)]);
}
