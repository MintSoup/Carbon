#include "carbon.h"
#include "ast/carbon_statements.h"
#include "carbon_compiler.h"
#include "carbon_lexer.h"
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
#include <stdlib.h>

extern uint32_t heapSize;

void carbon_init(CarbonState *instance,
				 char *(*readFile)(char *, char *, uint32_t *) ) {
	carbon_initVM(&instance->vm);
	carbon_stmtList_init(&instance->statements);
	instance->readFile = readFile;
}

static bool add(CarbonState *state, char *source, uint32_t length, char *from) {
	CarbonLexer lexer = carbon_initLexer(source, length, from);
	CarbonParser parser;
	carbon_initParser(&parser, &lexer);
	carbon_initCompiler(&state->compiler, &parser);

	bool hadError = false;

	while (parser.tokens[parser.currentToken].type != TokenEOF) {
		CarbonStmt *stmt = carbon_parseStatement(&parser);
		if (stmt == NULL)
			continue;

		if (stmt->type == StmtImport) {
			CarbonStmtImport *imp = (CarbonStmtImport *) stmt;
			if (imp->token.type == TokenStringLiteral) {
				char *name = malloc(imp->token.length - 1);
				memcpy(name, imp->token.lexeme + 1, imp->token.length - 2);
				name[imp->token.length - 2] = 0;
				uint32_t length;
				char *file = state->readFile(name, from, &length);
				if (file != NULL)
					hadError = hadError || add(state, file, length, name);
			}
			carbon_freeStmt(stmt);
		} else
			carbon_stmtList_add(&state->statements, stmt);
	}
	hadError = hadError || parser.hadError;
	carbon_freeParser(&parser);
	return hadError;
}

CarbonRunResult carbon_execute(CarbonState *state, char *source,
							   uint32_t length, char *name,
							   struct CarbonFlags flags) {

	bool parserHadError = add(state, source, length, name);

	CarbonFunction *topLevel = carbon_newFunction(NULL, 0, NULL, &state->vm);

	for (uint32_t i = 0; i < state->statements.count; i++) {
		CarbonStmt *stmt = state->statements.arr[i];
		carbon_scoutClass(stmt, &state->compiler, &state->vm);
	}

	for (uint32_t i = 0; i < state->statements.count; i++) {
		CarbonStmt *stmt = state->statements.arr[i];
		carbon_markGlobal(stmt, &state->compiler, &state->vm);
	}
	state->vm.classCount = state->compiler.classCount;
	state->vm.classes = carbon_reallocate(
		0, state->vm.classCount * sizeof(struct carbon_class), NULL);

	for (uint32_t i = 0; i < state->statements.count; i++) {
		CarbonStmt *stmt = state->statements.arr[i];
		carbon_compileStatement(stmt, &topLevel->chunk, &state->compiler,
								&state->vm);
	}

	carbon_writeToChunk(&topLevel->chunk, OpReturnVoid, -1);

	bool compilerHadError = state->compiler.hadError;

	carbon_freeCompiler(&state->compiler);
	carbon_stmtList_free(&state->statements);

	if (parserHadError)
		return Carbon_Parser_Error;
	if (compilerHadError)
		return Carbon_Compiler_Error;

	if (flags.disassemble) { // disassembly
		CarbonObj *o = state->vm.objects;
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
		CarbonRunResult r = carbon_run(&state->vm, topLevel);
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
