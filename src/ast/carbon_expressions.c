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
											   CarbonExpr *right) {
	CarbonExprAssignment *assignment = (CarbonExprAssignment *) allocateNode(
		CarbonExprAssignment, ExprAssignment);
	assignment->left = left;
	assignment->right = right;
	return assignment;
}

CarbonExprCall *carbon_newCallExpr(CarbonExpr *callee, uint32_t line) {
	CarbonExprCall *call =
		(CarbonExprCall *) allocateNode(CarbonExprCall, ExprCall);
	call->callee = callee;
	call->arity = 0;
	call->arguments = NULL;
	call->argumentCapacity = 0;
	call->line = line;
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
			if (t.compound.signature->returnType != NULL) {
				carbon_freeType(*t.compound.signature->returnType);
				carbon_reallocate(sizeof(CarbonValueType), 0,
								  t.compound.signature->returnType);
			}
			for (uint8_t i = 0; i < t.compound.signature->arity; i++) {
				carbon_freeType(t.compound.signature->arguments[i]);
			}
			carbon_reallocate(t.compound.signature->arity *
								  sizeof(CarbonValueType),
							  0, t.compound.signature->arguments);

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

			size_t s =
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
	if (expr == NULL)
		return;
	carbon_freeType(expr->evalsTo);
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
			carbon_freeTypename(cast->to);
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
		case ExprCall: {
			CarbonExprCall *call = (CarbonExprCall *) expr;
			for (uint8_t i = 0; i < call->arity; i++) {
				carbon_freeExpr(call->arguments[i]);
			}
			size_t oldSize = call->argumentCapacity * sizeof(CarbonExpr *);
			carbon_reallocate(oldSize, 0, call->arguments);
			carbon_freeExpr(call->callee);
			carbon_reallocate(sizeof(CarbonExprCall), 0, expr);
			break;
		}
		case ExprArray: {
			CarbonExprArray *arr = (CarbonExprArray *) expr;
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
			CarbonExprIndex *index = (CarbonExprIndex *) expr;
			carbon_freeExpr(index->index);
			carbon_freeExpr(index->object);
			carbon_reallocate(sizeof(CarbonExprIndex), 0, expr);
			break;
		}
		case ExprIndexAssignment: {
			CarbonExprIndexAssignment *ie = (CarbonExprIndexAssignment *) expr;
			carbon_freeExpr((CarbonExpr *) ie->left);
			carbon_freeExpr(ie->right);
			carbon_reallocate(sizeof(CarbonExprIndexAssignment), 0, expr);
			break;
		}
	}
}

bool carbon_typesEqual(CarbonValueType a, CarbonValueType b) {
	if (a.tag != b.tag)
		return false;
	switch (a.tag) {
		case ValueFunction: {
			if (a.compound.signature->arity != b.compound.signature->arity)
				return false;
			if (!carbon_typesEqual(*a.compound.signature->returnType,
								   *b.compound.signature->returnType))
				return false;
			for (uint8_t i = 0; i < a.compound.signature->arity; i++)
				if (!carbon_typesEqual(a.compound.signature->arguments[i],
									   b.compound.signature->arguments[i]))
					return false;
			return true;
		}
		case ValueGenerator:
		case ValueArray: {
			return carbon_typesEqual(*a.compound.memberType,
									 *b.compound.memberType);
		}
		case ValueInstance: {
			return a.compound.instanceName == b.compound.instanceName;
		}
		default:
			return true;
	}
}

void carbon_freeTypename(CarbonTypename t) {
	CarbonTypename *types = t.templates;
	for (uint8_t i = 0; i < t.templateCount; i++) {
		carbon_freeTypename(types[i]);
	}
	carbon_reallocate(sizeof(CarbonTypename) * t.templateCount, 0, types);
}

#undef allocateNode
