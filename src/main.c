#include <stdio.h>
#include <stdlib.h>
#include "ast/carbon_expressions.h"
#include "carbon_compiler.h"
#include "carbon_lexer.h"
#include "carbon_parser.h"
#include "carbon_token.h"
#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "utils/carbon_disassembler.h"
#include "vm/carbon_chunk.h"
#include "vm/carbon_vm.h"

uint8_t instructions[] = {
	OpLoadConstant, 0, OpLoadConstant, 1,		OpAddInt, OpIntToDouble,
	OpLoadConstant, 2, OpDivDouble,	   OpReturn};

extern size_t heapSize;
int main(int argc, char *argv[]) {
	if (argc != 2) {
		puts("Usage: carbon <filename>");
		return 1;
	}
	FILE *f = fopen(argv[1], "rb");

	if (!f)
		return 2;

	fseek(f, 0, SEEK_END);
	size_t size = ftell(f);
	fseek(f, 0, SEEK_SET);
	char *t = malloc(size + 1);
	if (!t)
		return 3;
	t[size] = 0;
	fread(t, size, 1, f);
	fclose(f);

	CarbonParser parser;
	CarbonLexer lexer = carbon_initLexer(t, size);
	carbon_initParser(&parser, &lexer);
	CarbonExpr *expr = carbon_parseExpression(&parser);
	if (expr != NULL) {
		CarbonVM vm;
		carbon_initVM(&vm);
		CarbonCompiler c;
		carbon_initCompiler(&c, &parser);
		carbon_compileExpression(expr, &vm.chunk, &c);
		if (!c.hadError && !c.parserHadError) {
			carbon_writeToChunk(&vm.chunk, OpReturn, 100);
			carbon_disassemble(&vm.chunk);
			carbon_run(&vm);
			printf("%ld\n", vm.stack[vm.stackTop - 1].uint);
		}
		carbon_freeVM(&vm);
	}

	carbon_freeExpr(expr);

	free(t);

	return 0;
}
