#include <stdio.h>
#include <stdlib.h>
#include "ast/carbon_expressions.h"
#include "carbon.h"
#include "carbon_compiler.h"
#include "carbon_lexer.h"
#include "carbon_object.h"
#include "carbon_parser.h"
#include "carbon_token.h"
#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "utils/carbon_disassembler.h"
#include "vm/carbon_chunk.h"
#include "vm/carbon_vm.h"

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

	CarbonInstance instance;
	carbon_init(&instance);

	CarbonRunResult isOk = carbon_execute(&instance, t, size);

	if (isOk == Carbon_OK) {
		CarbonValue mainFunction;
		bool success = false;
		if (carbon_getValue(&instance, "main", &mainFunction))
			if (!carbon_isPrimitive(&instance, "main"))
				if (mainFunction.obj->type == CrbnObjFunc) {
					CarbonFunction *func = (CarbonFunction *) mainFunction.obj;
					if (func->arity == 0 && func->returnType == ValueVoid) {
						carbon_run(&instance.vm, func);
						success = true;
					}
				}
		if (!success)
			printf("Need void main() function in the code\n");
	}
	carbon_free(&instance);

	free(t);

	return 0;
}
