#pragma once

#include "carbon_lexer.h"
#include "carbon_token.h"
#include "ast/carbon_expressions.h"

typedef struct {
	CarbonLexer *lexer;
	CarbonToken previous;
	CarbonToken current;
	bool panic;
	bool hadError;
} CarbonParser;

void carbon_initParser(CarbonParser *parser, CarbonLexer *lexer);
CarbonExpr *carbon_parseExpression(CarbonParser *p);
