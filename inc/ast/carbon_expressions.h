#pragma once

#include "carbon_token.h"
#include "carbon_value.h"

typedef enum {
	ExprBinary,
	ExprUnary,
	ExprGrouping,
	ExprLiteral
} CarbonExprType;

typedef struct {
	CarbonExprType type;
	CarbonValueType evaluatesTo;
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

CarbonExprBinary *carbon_newBinaryExpr(CarbonExpr *right, CarbonExpr *left,
									   CarbonToken op,
									   CarbonValueType evaluatesTo);

CarbonExprUnary *carbon_newUnaryExpr(CarbonExpr *operand, CarbonToken op,
									 CarbonValueType evaluatesTo);

CarbonExprLiteral *carbon_newLiteralExpr(CarbonToken token,
										 CarbonValueType evaluatesTo);

CarbonExprGrouping *carbon_newGroupingExpr(CarbonExpr *expr);

void carbon_freeExpr(CarbonExpr *expr);
