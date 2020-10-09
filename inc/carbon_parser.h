#pragma once

#include "carbon_lexer.h"
#include "carbon_token.h"
#include "ast/carbon_expressions.h"

typedef struct {
	CarbonToken *tokens;
	uint32_t totalTokens;
	uint32_t currentToken;
	bool panic;
	bool hadError;
} CarbonParser;

void carbon_initParser(CarbonParser *parser, CarbonLexer *lexer);
void carbon_freeParser(CarbonParser *p);

CarbonExpr *carbon_parseExpression(CarbonParser *p);
