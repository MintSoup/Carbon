#pragma once

#include "carbon_token.h"
#include "carbon_value.h"

typedef enum {
	ExprBinary,
	ExprUnary,
	ExprGrouping,
	ExprLiteral,
	ExprCast,
	ExprVar
} CarbonExprType;

typedef struct {
	CarbonExprType type;
	CarbonValueType evalsTo;
} CarbonExpr;

typedef struct {
	CarbonExpr expr;

	CarbonExpr *left;
	CarbonExpr *right;
	CarbonToken op;
} CarbonExprBinary;

typedef struct {
	CarbonExpr expr;

	CarbonExpr *operand;
	CarbonToken op;
} CarbonExprUnary;

typedef struct {
	CarbonExpr expr;

	CarbonExpr *expression;
} CarbonExprGrouping;

typedef struct {
	CarbonExpr expr;
	CarbonToken token;
} CarbonExprLiteral;

typedef struct {
	CarbonExpr expr;
	CarbonToken to;
	CarbonExpr *expression;
} CarbonExprCast;

typedef struct {
	CarbonExpr expr;
	CarbonToken token;
} CarbonExprVar;

CarbonExprBinary *carbon_newBinaryExpr(CarbonExpr *left, CarbonExpr *right,
									   CarbonToken op);

CarbonExprUnary *carbon_newUnaryExpr(CarbonExpr *operand, CarbonToken op);

CarbonExprLiteral *carbon_newLiteralExpr(CarbonToken token);

CarbonExprGrouping *carbon_newGroupingExpr(CarbonExpr *expr);

CarbonExprCast *carbon_newCastExpr(CarbonToken to, CarbonExpr *expr);
CarbonExprVar *carbon_newVarExpr(CarbonToken token);

void carbon_freeExpr(CarbonExpr *expr);
