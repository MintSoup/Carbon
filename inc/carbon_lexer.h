#pragma once
#include "utils/carbon_commons.h"
#include "carbon_token.h"
typedef struct{
	char* source;
	uint32_t length; 
	char* current;
	char* start;
	uint32_t line;
} CarbonLexer;

CarbonToken carbon_scanToken(CarbonLexer *lexer);
CarbonLexer carbon_initLexer(char *source, uint32_t length);
