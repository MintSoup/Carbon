#pragma once

#include "carbon_token.h"
#include "carbon_value.h"

typedef enum {
	ExprBinary,
	ExprUnary,
	ExprGrouping,
	ExprLiteral
} CarbonExpressionType;

typedef struct {
	CarbonExpressionType type;
	CarbonValueType evaluatesTo;
} CarbonExpression;

typedef struct {
	CarbonExpression expr;

	CarbonExpression* left;
	CarbonExpression* right;
	CarbonToken op;
} CarbonExprBinary;

typedef struct {
	CarbonExpression expr;

	CarbonExpression* operand;
	CarbonToken op;
} CarbonExprUnary;

typedef struct {
	CarbonExpression expr;

	CarbonExpression* expression;
} CarbonExprGrouping;

typedef struct {
	CarbonExpression expr;
	CarbonToken token;
} CarbonExprLiteral;
