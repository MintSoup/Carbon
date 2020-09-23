#include "carbon_token.h"
#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "ast/carbon_expressions.h"
#include "utils/carbon_memory.h"
#include <stdlib.h>

CarbonExpr *carbon_newExpr(size_t size, CarbonExprType type,
						   CarbonValueType valueType) {
	CarbonExpr *expr = carbon_reallocate(0, size, NULL);
	expr->type = type;
	expr->evaluatesTo = valueType;
	return expr;
}

#define allocateNode(type, typeTag, valueType)                                 \
	carbon_newExpr(sizeof(type), typeTag, valueType)

CarbonExprBinary *carbon_newBinaryExpr(CarbonExpr *right, CarbonExpr *left,
									   CarbonToken op,
									   CarbonValueType evaluatesTo) {

	CarbonExprBinary *bin = (CarbonExprBinary *) allocateNode(
		CarbonExprBinary, ExprBinary, evaluatesTo);

	bin->left = left;
	bin->right = right;
	bin->op = op;

	return bin;
}

CarbonExprUnary *carbon_newUnaryExpr(CarbonExpr *operand, CarbonToken op,
									 CarbonValueType evaluatesTo) {
	CarbonExprUnary *un = (CarbonExprUnary *) allocateNode(
		CarbonExprUnary, ExprUnary, evaluatesTo);
	un->operand = operand;
	un->op = op;
	return un;
}

CarbonExprLiteral *carbon_newLiteralExpr(CarbonToken token,
										 CarbonValueType evaluatesTo) {
	CarbonExprLiteral *literal = (CarbonExprLiteral *) allocateNode(
		CarbonExprLiteral, ExprLiteral, evaluatesTo);
	literal->token = token;
	return literal;
}

CarbonExprGrouping *carbon_newGroupingExpr(CarbonExpr *expr) {
	CarbonExprGrouping *grouping = (CarbonExprGrouping *) allocateNode(
		CarbonExprGrouping, ExprGrouping, expr->evaluatesTo);
	return grouping;
}

void carbon_freeExpr(CarbonExpr *expr) {
	switch (expr->type) {
	case ExprUnary: {
		CarbonExprUnary *un = (CarbonExprUnary *) expr;
		carbon_freeExpr(un->operand);
		carbon_reallocate(sizeof(CarbonExprUnary), 0, expr);
	}
	case ExprBinary: {
		CarbonExprBinary *bin = (CarbonExprBinary *) expr;
		carbon_freeExpr(bin->left);
		carbon_freeExpr(bin->right);
		carbon_reallocate(sizeof(CarbonExprBinary), 0, expr);
	}
	case ExprGrouping: {
		CarbonExprGrouping *group = (CarbonExprGrouping *) expr;
		carbon_freeExpr(group->expression);
		carbon_reallocate(sizeof(CarbonExprGrouping), 0, expr);
	}
	case ExprLiteral: {
		carbon_reallocate(sizeof(CarbonExprLiteral), 0, expr);
	}
	}
}
