#pragma once
#include "utils/carbon_commons.h"
#include "carbon_token.h"
typedef struct {
	CarbonTokenType lastToken;
	char *source;
	char *current;
	char *start;
	char* file;
	uint32_t line;
	uint32_t length;
} CarbonLexer;

CarbonToken carbon_scanToken(CarbonLexer *lexer);
CarbonLexer carbon_initLexer(char *source, uint32_t length, char *file);
