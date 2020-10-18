#include "carbon.h"
#include "ast/carbon_statements.h"
#include "carbon_compiler.h"
#include "carbon_parser.h"
#include "utils/carbon_disassembler.h"
#include "vm/carbon_vm.h"

void carbon_init(CarbonInstance *instance) {
	carbon_initVM(&instance->vm);
}
CarbonRunResult carbon_execute(CarbonInstance *instance, char *source,
							   uint32_t length) {
	instance->lexer = carbon_initLexer(source, length);
	carbon_initParser(&instance->parser, &instance->lexer);
	CarbonStmt *stmt = carbon_parseStatement(&instance->parser);
	carbon_freeParser(&instance->parser);
	if (stmt != NULL) {
		carbon_initCompiler(&instance->compiler, &instance->parser);
		carbon_compileStatement(stmt, &instance->vm.chunk, &instance->compiler,
								 &instance->vm);
		carbon_freeStmt(stmt);

		if(instance->parser.hadError) return Carbon_Parser_Error;
		if(instance->compiler.hadError) return Carbon_Compiler_Error;

		carbon_writeToChunk(&instance->vm.chunk, OpReturn, 9999);
		carbon_disassemble(&instance->vm.chunk);
		return carbon_run(&instance->vm);
	} else
		return Carbon_Parser_Error;
}
void carbon_free(CarbonInstance *instance){
	carbon_freeVM(&instance->vm);
}
