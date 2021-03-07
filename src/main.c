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
#include <string.h>

extern uint32_t heapSize;

struct CarbonFlags flags;

static bool parseArguments(int argc, char *argv[]) {
	uint16_t i = 0;
	bool r = false;
	for (i = 1; i < argc - 1; i++) {
		if (argv[i][0] != '-' && argv[i][1] != '-') {
			fprintf(stderr, "Flags must precede the filename\n");
			return true;
		}

		if (!strcmp(argv[i], "--disassemble")) {
			flags.disassemble = true;
		} else if (!strcmp(argv[i], "--disassemble-only")) {
			flags.disassemble = true;
			flags.norun = true;
		} else {
			fprintf(stderr, "Unknown flag: %s\n", argv[i]);
			r = true;
		}
	}
	return r;
}

int main(int argc, char *argv[]) {
	if (argc == 1) {
		puts("Usage: carbon <filename>");
		return 1;
	}

	if (parseArguments(argc, argv)) {
		return 3;
	}
	FILE *f = fopen(argv[argc - 1], "rb");

	if (!f) {
		fprintf(stderr, "Cannot open file %s\n", argv[argc - 1]);
		return 2;
	}

	fseek(f, 0, SEEK_END);
	uint32_t size = ftell(f);
	fseek(f, 0, SEEK_SET);
	char *t = malloc(size + 1);
	if (!t)
		return 3;
	t[size] = 0;
	fread(t, size, 1, f);
	fclose(f);

	CarbonState instance;
	carbon_init(&instance);

	CarbonRunResult isOk = carbon_execute(&instance, t, size, flags);

	if (isOk == Carbon_OK) {
		CarbonValue mainFunction;
		bool success = false;
		if (carbon_getValue(&instance, "main", &mainFunction))
			if (!carbon_isPrimitive(&instance, "main"))
				if (mainFunction.obj->type == CrbnObjFunc) {
					CarbonFunction *func = (CarbonFunction *) mainFunction.obj;
					if (func->arity == 0) {
						if (func->sig->returnType->tag == ValueVoid) {
							if (!flags.norun)
								carbon_run(&instance.vm, func);
							success = true;
						}
					}
				}
		if (!success)
			printf("Need void main() function in the code\n");
	}

	carbon_free(&instance);

	if (heapSize > 0) {
		if (instance.vm.objectHeapSize > 0) {
			if (instance.vm.objectHeapSize == heapSize) {
				printf("***MEMORY LEAK: LEAKING %" PRIu32
					   " BYTES FROM VM OBJECT "
					   "MEMORY***\n",
					   instance.vm.objectHeapSize);
			} else {
				printf("***MEMORY LEAK: LEAKING %" PRIu32 " BYTES, %" PRId32
					   " FROM VM OBJECT "
					   "MEMORY***\n",
					   heapSize, instance.vm.objectHeapSize);
			}
		} else {
			printf("***MEMORY LEAK: LEAKING %" PRIu32 " BYTES***\n ", heapSize);
		}
	}
	free(t);

	return 0;
}
