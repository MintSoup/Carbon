#pragma once

#include "carbon_compiler.h"
#include "carbon_lexer.h"
#include "carbon_parser.h"
#include "utils/carbon_commons.h"

typedef struct {
	CarbonLexer lexer;
	CarbonParser parser;
	CarbonCompiler compiler;
	CarbonVM vm;
} CarbonInstance;

typedef enum carbon_runresult {
	Carbon_OK,
	Carbon_Parser_Error,
	Carbon_Compiler_Error,
	Carbon_Runtime_Error
} CarbonRunResult;

CarbonRunResult carbon_execute(CarbonInstance *instance, char *source,
							   uint32_t length);
void carbon_init(CarbonInstance *instance);
void carbon_free(CarbonInstance *instance);
