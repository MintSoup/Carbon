#include "carbon_token.h"
#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "ast/carbon_expressions.h"
#include "utils/carbon_memory.h"
#include <stdlib.h>
#include <stdio.h>

CarbonExpr *carbon_newExpr(size_t size, CarbonExprType type) {
	CarbonExpr *expr = carbon_reallocate(0, size, NULL);
	expr->type = type;
	expr->evalsTo = ValueUntypechecked;
	return expr;
}

#define allocateNode(type, typeTag) carbon_newExpr(sizeof(type), typeTag)

CarbonExprBinary *carbon_newBinaryExpr(CarbonExpr *left, CarbonExpr *right,
									   CarbonToken op) {

	CarbonExprBinary *bin =
		(CarbonExprBinary *) allocateNode(CarbonExprBinary, ExprBinary);

	bin->left = left;
	bin->right = right;
	bin->op = op;

	return bin;
}

CarbonExprUnary *carbon_newUnaryExpr(CarbonExpr *operand, CarbonToken op) {
	CarbonExprUnary *un =
		(CarbonExprUnary *) allocateNode(CarbonExprUnary, ExprUnary);
	un->operand = operand;
	un->op = op;
	return un;
}

CarbonExprLiteral *carbon_newLiteralExpr(CarbonToken token) {
	CarbonExprLiteral *literal =
		(CarbonExprLiteral *) allocateNode(CarbonExprLiteral, ExprLiteral);
	literal->token = token;
	return literal;
}

CarbonExprGrouping *carbon_newGroupingExpr(CarbonExpr *expr) {
	CarbonExprGrouping *grouping =
		(CarbonExprGrouping *) allocateNode(CarbonExprGrouping, ExprGrouping);
	grouping->expression = expr;
	return grouping;
}

CarbonExprCast *carbon_newCastExpr(CarbonToken to, CarbonExpr *expr) {
	CarbonExprCast *cast =
		(CarbonExprCast *) allocateNode(CarbonExprCast, ExprCast);
	cast->expression = expr;
	cast->to = to;
	return cast;
}

CarbonExprVar *carbon_newVarExpr(CarbonToken token) {
	CarbonExprVar *var = (CarbonExprVar *) allocateNode(CarbonExprVar, ExprVar);
	var->token = token;
	return var;
}

CarbonExprAssignment *carbon_newAssignmentExpr(CarbonToken left,
											   CarbonExpr *right) {
	CarbonExprAssignment *assignment = (CarbonExprAssignment *) allocateNode(
		CarbonExprAssignment, ExprAssignment);
	assignment->left = left;
	assignment->right = right;
	return assignment;
}

void carbon_freeExpr(CarbonExpr *expr) {
	if (expr == NULL)
		return;
	switch (expr->type) {
		case ExprUnary: {
			CarbonExprUnary *un = (CarbonExprUnary *) expr;
			carbon_freeExpr(un->operand);
			carbon_reallocate(sizeof(CarbonExprUnary), 0, expr);
			break;
		}
		case ExprBinary: {
			CarbonExprBinary *bin = (CarbonExprBinary *) expr;
			carbon_freeExpr(bin->left);
			carbon_freeExpr(bin->right);
			carbon_reallocate(sizeof(CarbonExprBinary), 0, expr);
			break;
		}
		case ExprGrouping: {
			CarbonExprGrouping *group = (CarbonExprGrouping *) expr;
			carbon_freeExpr(group->expression);
			carbon_reallocate(sizeof(CarbonExprGrouping), 0, expr);
			break;
		}
		case ExprLiteral: {
			carbon_reallocate(sizeof(CarbonExprLiteral), 0, expr);
			break;
		}
		case ExprCast: {
			CarbonExprCast *cast = (CarbonExprCast *) expr;
			carbon_freeExpr(cast->expression);
			carbon_reallocate(sizeof(CarbonExprCast), 0, expr);
			break;
		}
		case ExprVar: {
			carbon_reallocate(sizeof(CarbonExprVar), 0, expr);
			break;
		}
		case ExprAssignment: {
			CarbonExprAssignment *assignment = (CarbonExprAssignment *) expr;
			carbon_freeExpr(assignment->right);
			carbon_reallocate(sizeof(CarbonExprAssignment), 0, expr);
			break;
		}
	}
}

#undef allocateNode
