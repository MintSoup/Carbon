#include "carbon.h"
#include "utils/carbon_disassembler.h"

void carbon_init(CarbonInstance *instance) {
	carbon_initVM(&instance->vm);
}
CarbonRunResult carbon_execute(CarbonInstance *instance, char *source,
							   uint32_t length) {
	instance->lexer = carbon_initLexer(source, length);
	carbon_initParser(&instance->parser, &instance->lexer);
	CarbonExpr *expr = carbon_parseExpression(&instance->parser);
	carbon_freeParser(&instance->parser);
	if (expr != NULL) {
		carbon_initCompiler(&instance->compiler, &instance->parser);
		carbon_compileExpression(expr, &instance->vm.chunk, &instance->compiler,
								 &instance->vm);
		carbon_freeExpr(expr);

		if(instance->parser.hadError) return Carbon_Parser_Error;
		if(instance->compiler.hadError) return Carbon_Compiler_Error;

		carbon_writeToChunk(&instance->vm.chunk, OpReturn, 9999);
		carbon_disassemble(&instance->vm.chunk);
		return carbon_run(&instance->vm);
	} else
		return Carbon_Parser_Error;
}
