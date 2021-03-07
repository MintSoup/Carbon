#include "carbon.h"
#include "ast/carbon_statements.h"
#include "carbon_compiler.h"
#include "carbon_object.h"
#include "carbon_parser.h"
#include "carbon_token.h"
#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "utils/carbon_disassembler.h"
#include "utils/carbon_memory.h"
#include "utils/carbon_table.h"
#include "vm/carbon_chunk.h"
#include "vm/carbon_vm.h"
#include <stdio.h>
#include <string.h>

extern uint32_t heapSize;

void carbon_init(CarbonState *instance) {
	carbon_initVM(&instance->vm);
	carbon_stmtList_init(&instance->statements);
}

CarbonRunResult carbon_execute(CarbonState *instance, char *source,
							   uint32_t length, struct CarbonFlags flags) {
	instance->lexer = carbon_initLexer(source, length);
	carbon_initParser(&instance->parser, &instance->lexer);
	carbon_initCompiler(&instance->compiler, &instance->parser);
	while (instance->parser.tokens[instance->parser.currentToken].type !=
		   TokenEOF) {
		CarbonStmt *stmt = carbon_parseStatement(&instance->parser);
		if (stmt != NULL)
			carbon_stmtList_add(&instance->statements, stmt);
	}

	CarbonFunction *topLevel = carbon_newFunction(NULL, 0, NULL, &instance->vm);

	for (uint32_t i = 0; i < instance->statements.count; i++) {
		CarbonStmt *stmt = instance->statements.arr[i];
		carbon_scoutClass(stmt, &instance->compiler, &instance->vm);
	}

	for (uint32_t i = 0; i < instance->statements.count; i++) {
		CarbonStmt *stmt = instance->statements.arr[i];
		carbon_markGlobal(stmt, &instance->compiler, &instance->vm);
	}
	instance->vm.classCount = instance->compiler.classCount;
	instance->vm.classes = carbon_reallocate(
		0, instance->vm.classCount * sizeof(struct carbon_class), NULL);

	for (uint32_t i = 0; i < instance->statements.count; i++) {
		CarbonStmt *stmt = instance->statements.arr[i];
		carbon_compileStatement(stmt, &topLevel->chunk, &instance->compiler,
								&instance->vm);
	}

	carbon_writeToChunk(&topLevel->chunk, OpReturnVoid, -1);

	bool compilerHadError = instance->compiler.hadError;
	bool parserHadError = instance->parser.hadError;

	carbon_freeParser(&instance->parser);
	carbon_freeCompiler(&instance->compiler);
	carbon_stmtList_free(&instance->statements);

	if (parserHadError)
		return Carbon_Parser_Error;
	if (compilerHadError)
		return Carbon_Compiler_Error;

	if (flags.disassemble) { // disassembly
		CarbonObj *o = instance->vm.objects;
		while (o != NULL) {
			if (o->type == CrbnObjFunc) {
				CarbonFunction *f = (CarbonFunction *) o;
				if (f->name == NULL)
					printf("-<global>-\n");
				else
					printf("-<function %s>-\n", f->name->chars);
				carbon_disassemble(&f->chunk);
				puts("");
			}
			o = o->next;
		}
	}

	if (!flags.norun) {
		CarbonRunResult r = carbon_run(&instance->vm, topLevel);
		return r;
	} else
		return Carbon_OK;
}

bool carbon_isPrimitive(CarbonState *instance, char *name) {
	CarbonString *str = carbon_copyString(name, strlen(name), &instance->vm);
	CarbonValue dummy;
	return carbon_tableGet(&instance->vm.primitives, (CarbonObj *) str, &dummy);
}

bool carbon_getValue(CarbonState *instance, char *name, CarbonValue *out) {
	CarbonString *str = carbon_copyString(name, strlen(name), &instance->vm);
	return carbon_tableGet(&instance->vm.globals, (CarbonObj *) str, out);
}

void carbon_free(CarbonState *instance) {
	carbon_freeVM(&instance->vm);
}
