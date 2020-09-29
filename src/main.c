#include <stdio.h>
#include <stdlib.h>
#include "ast/carbon_expressions.h"
#include "carbon_lexer.h"
#include "carbon_parser.h"
#include "carbon_token.h"
#include "carbon_value.h"
#include "utils/carbon_commons.h"
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

	if (!f) return 2;

	fseek(f, 0, SEEK_END);
	size_t size = ftell(f);
	fseek(f, 0, SEEK_SET);
	char *t = malloc(size + 1);
	if (!t) return 3;
	t[size] = 0;
	fread(t, size, 1, f);
	fclose(f);

	CarbonVM vm;
	carbon_initVM(&vm);

	carbon_addConstant(&vm.chunk, CarbonInt(7));
	carbon_addConstant(&vm.chunk, CarbonInt(8));
	carbon_addConstant(&vm.chunk, CarbonDouble(2));

	for (int i = 0; i < sizeof(instructions); i++) {
		carbon_writeToChunk(&vm.chunk, instructions[i], 0);
	}

	carbon_run(&vm);

	printf("%lf\n", vm.stack[vm.stackTop - 1].dbl);
	carbon_freeVM(&vm);

	
	free(t);

	return 0;
}
