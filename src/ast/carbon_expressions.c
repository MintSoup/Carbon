#include "carbon_token.h"
#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "ast/carbon_expressions.h"
#include "utils/carbon_memory.h"
#include <stdlib.h>
#include <stdio.h>

CarbonExpr *carbon_newExpr(uint32_t size, CarbonExprType type) {
	CarbonExpr *expr = carbon_reallocate(0, size, NULL);
	expr->type = type;
	CarbonValueType t = {.tag = ValueUntypechecked};
	expr->evalsTo = t;
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

CarbonExprCast *carbon_newCastExpr(CarbonTypename to, CarbonExpr *expr) {
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
											   CarbonExpr *right,
											   CarbonToken equals) {
	CarbonExprAssignment *assignment = (CarbonExprAssignment *) allocateNode(
		CarbonExprAssignment, ExprAssignment);
	assignment->left = left;
	assignment->right = right;
	assignment->equals = equals;
	return assignment;
}

CarbonExprCall *carbon_newCallExpr(CarbonExpr *callee, CarbonToken paren) {
	CarbonExprCall *call =
		(CarbonExprCall *) allocateNode(CarbonExprCall, ExprCall);
	call->callee = callee;
	call->arity = 0;
	call->arguments = NULL;
	call->argumentCapacity = 0;
	call->paren = paren;
	return call;
}

CarbonExprArray *carbon_newArrayExpr(CarbonToken bracket) {
	CarbonExprArray *arr =
		(CarbonExprArray *) allocateNode(CarbonExprArray, ExprArray);
	arr->bracket = bracket;
	arr->capacity = 0;
	arr->count = 0;
	arr->members = NULL;
	arr->type.base.type = TokenNone;
	return arr;
}

CarbonExprIndex *carbon_newIndexExpr(CarbonExpr *object, CarbonExpr *index,
									 CarbonToken bracket) {
	CarbonExprIndex *i =
		(CarbonExprIndex *) allocateNode(CarbonExprIndex, ExprIndex);
	i->index = index;
	i->object = object;
	i->bracket = bracket;
	return i;
}
CarbonExprIndexAssignment *carbon_newIndexAssignmentExpr(CarbonExprIndex *left,
														 CarbonExpr *right,
														 CarbonToken equals) {
	CarbonExprIndexAssignment *index =
		(CarbonExprIndexAssignment *) allocateNode(CarbonExprIndexAssignment,
												   ExprIndexAssignment);
	index->equals = equals;
	index->left = left;
	index->right = right;
	return index;
}

CarbonExprDot *carbon_newDotExpr(CarbonExpr *left, CarbonToken right,
								 CarbonToken dot) {
	CarbonExprDot *node =
		(CarbonExprDot *) allocateNode(CarbonExprDot, ExprDot);

	node->left = left;
	node->right = right;
	node->dot = dot;
	return node;
}

CarbonExprDotAssignment *carbon_newDotAssignmentExpr(CarbonExprDot *left,
													 CarbonExpr *right,
													 CarbonToken equals) {

	CarbonExprDotAssignment *da = (CarbonExprDotAssignment *) allocateNode(
		CarbonExprDotAssignment, ExprDotAssignment);
	da->left = left;
	da->right = right;
	da->equals = equals;
	return da;
}

CarbonExprIs *carbon_newIsExpr(CarbonExpr *left, CarbonTypename type,
							   CarbonToken tok) {
	CarbonExprIs *is = (CarbonExprIs *) allocateNode(CarbonExprIs, ExprIs);
	is->left = left;
	is->right = type;
	is->tok = tok;
	return is;
}

void carbon_freeSignature(CarbonFunctionSignature sig) {
	if (sig.returnType != NULL) {
		carbon_freeType(*sig.returnType);
		carbon_reallocate(sizeof(CarbonValueType), 0, sig.returnType);
	}
	for (uint8_t i = 0; i < sig.arity; i++) {
		carbon_freeType(sig.arguments[i]);
	}
	carbon_reallocate(sig.arity * sizeof(CarbonValueType), 0, sig.arguments);
}

void carbon_freeType(CarbonValueType t) {
	if (t.compound.memberType == NULL)
		return;
	switch (t.tag) {
		case ValueGenerator:
		case ValueArray: {
			carbon_freeType(*t.compound.memberType);
			carbon_reallocate(sizeof(CarbonValueType), 0,
							  t.compound.memberType);
			break;
		}
		case ValueFunction: {
			carbon_freeSignature(*t.compound.signature);
			carbon_reallocate(sizeof(CarbonFunctionSignature), 0,
							  t.compound.signature);
			break;
		}
		default:
			break;
	}
}

CarbonValueType carbon_cloneType(CarbonValueType type) {
	CarbonValueType cloned = {.tag = type.tag};
	switch (cloned.tag) {
		case ValueFunction: {
			cloned.compound.signature =
				carbon_reallocate(0, sizeof(CarbonFunctionSignature), NULL);
			cloned.compound.signature->arity = type.compound.signature->arity;

			cloned.compound.signature->returnType =
				carbon_reallocate(0, sizeof(CarbonValueType), NULL);
			*cloned.compound.signature->returnType =
				carbon_cloneType(*type.compound.signature->returnType);

			cloned.compound.signature->arity = type.compound.signature->arity;

			uint32_t s =
				sizeof(CarbonValueType) * cloned.compound.signature->arity;
			cloned.compound.signature->arguments =
				carbon_reallocate(0, s, NULL);

			for (uint8_t i = 0; i < type.compound.signature->arity; i++) {
				cloned.compound.signature->arguments[i] =
					carbon_cloneType(type.compound.signature->arguments[i]);
			}

			break;
		}
		case ValueGenerator:
		case ValueArray: {
			cloned.compound.memberType =
				carbon_reallocate(0, sizeof(CarbonValueType), NULL);
			*cloned.compound.memberType =
				carbon_cloneType(*type.compound.memberType);
			break;
		}
		case ValueInstance: {
			cloned.compound.instanceName = type.compound.instanceName;
			break;
		}
		default:
			break;
	}
	return cloned;
}

void carbon_freeExpr(CarbonExpr *expr) {

#define castNode(type, name) type *name = (type *) expr;

	if (expr == NULL)
		return;
	carbon_freeType(expr->evalsTo);
	switch (expr->type) {
		case ExprUnary: {
			castNode(CarbonExprUnary, un);
			carbon_freeExpr(un->operand);
			carbon_reallocate(sizeof(CarbonExprUnary), 0, expr);
			break;
		}
		case ExprBinary: {
			castNode(CarbonExprBinary, bin);
			carbon_freeExpr(bin->left);
			carbon_freeExpr(bin->right);
			carbon_reallocate(sizeof(CarbonExprBinary), 0, expr);
			break;
		}
		case ExprGrouping: {
			castNode(CarbonExprGrouping, group);
			carbon_freeExpr(group->expression);
			carbon_reallocate(sizeof(CarbonExprGrouping), 0, expr);
			break;
		}
		case ExprLiteral: {
			carbon_reallocate(sizeof(CarbonExprLiteral), 0, expr);
			break;
		}
		case ExprCast: {
			castNode(CarbonExprCast, cast);
			carbon_freeExpr(cast->expression);
			carbon_freeTypename(cast->to);
			carbon_reallocate(sizeof(CarbonExprCast), 0, expr);
			break;
		}
		case ExprVar: {
			carbon_reallocate(sizeof(CarbonExprVar), 0, expr);
			break;
		}
		case ExprAssignment: {
			castNode(CarbonExprAssignment, assignment);
			carbon_freeExpr(assignment->right);
			carbon_reallocate(sizeof(CarbonExprAssignment), 0, expr);
			break;
		}
		case ExprInit:
		case ExprCall: {
			castNode(CarbonExprCall, call);
			for (uint8_t i = 0; i < call->arity; i++) {
				carbon_freeExpr(call->arguments[i]);
			}
			uint32_t oldSize = call->argumentCapacity * sizeof(CarbonExpr *);
			carbon_reallocate(oldSize, 0, call->arguments);
			carbon_freeExpr(call->callee);
			carbon_reallocate(sizeof(CarbonExprCall), 0, expr);
			break;
		}
		case ExprArray: {
			castNode(CarbonExprArray, arr);
			for (uint8_t i = 0; i < arr->count; i++) {
				carbon_freeExpr(arr->members[i]);
			}
			carbon_reallocate(arr->capacity * sizeof(CarbonExpr *), 0,
							  arr->members);
			if (arr->type.base.type != TokenNone)
				carbon_freeTypename(arr->type);
			carbon_reallocate(sizeof(CarbonExprArray), 0, expr);
			break;
		}
		case ExprIndex: {
			castNode(CarbonExprIndex, index);
			carbon_freeExpr(index->index);
			carbon_freeExpr(index->object);
			carbon_reallocate(sizeof(CarbonExprIndex), 0, expr);
			break;
		}
		case ExprIndexAssignment: {
			castNode(CarbonExprIndexAssignment, ie);
			carbon_freeExpr((CarbonExpr *) ie->left);
			carbon_freeExpr(ie->right);
			carbon_reallocate(sizeof(CarbonExprIndexAssignment), 0, expr);
			break;
		}
		case ExprDot: {
			castNode(CarbonExprDot, dot);
			carbon_freeExpr(dot->left);
			carbon_reallocate(sizeof(CarbonExprDot), 0, expr);
			break;
		}
		case ExprDotAssignment: {
			castNode(CarbonExprDotAssignment, da);
			carbon_freeExpr((CarbonExpr *) da->left);
			carbon_freeExpr(da->right);
			carbon_reallocate(sizeof(CarbonExprDotAssignment), 0, expr);
			break;
		}
		case ExprIs: {
			castNode(CarbonExprIs, is);
			carbon_freeExpr(is->left);
			carbon_freeTypename(is->right);
			carbon_reallocate(sizeof(CarbonExprIs), 0, expr);
			break;
		}
	}
#undef castNode
}

void carbon_freeTypename(CarbonTypename t) {
	CarbonTypename *types = t.templates;
	for (uint8_t i = 0; i < t.templateCount; i++) {
		carbon_freeTypename(types[i]);
	}
	carbon_reallocate(sizeof(CarbonTypename) * t.templateCount, 0, types);
}

#undef allocateNode
