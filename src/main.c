#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "carbon.h"
#include "carbon_object.h"

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

typedef struct file {
	char *name;
	char *contents;
	uint32_t length;
} file;

file *files;
uint16_t filesCount = 0;
uint16_t filesZile = 0;

void freeFiles() {
	for (uint16_t i = 0; i < filesCount; i++) {
		free(files[i].name);
		free(files[i].contents);
	}
	free(files);
}

// This is not a good example of a readFile function.
// This is here just to demonstrate how one may read a file.
// If we can retrieve a valid string, return it, otherwise NULL
// In this case, we check if the file has already been included,
// and if so, we return NULL so we don't process the file more than
// once. If we want to signal an error, we must set *length to 1
// to let Carbon know that the code should not be executed.
char *readFile(char *name, char *from, uint32_t *length) {
	for (uint16_t i = 0; i < filesCount; i++) {
		if (!strcmp(files[i].name, name))
			return NULL;
	}
	if (filesCount >= filesZile) {
		uint32_t new;
		if (filesZile != 0)
			new = filesZile * 2;
		else
			new = 8;
		files = realloc(files, new * sizeof(file));
		filesZile = new;
	}
	FILE *f = fopen(name, "rb");
	if (!f) {
		if (from)
			fprintf(stderr, "%s: Cannot open file %s\n", from, name);
		else
			fprintf(stderr, "Cannot open file %s\n", name);
		if (length != NULL)
			*length = 1;
		return NULL;
	}
	fseek(f, 0, SEEK_END);
	uint32_t size = ftell(f);
	fseek(f, 0, SEEK_SET);
	char *t = malloc(size + 1);
	if (!t) {
		if (length != NULL)
			*length = 1;
		return NULL;
	}
	t[size] = 0;

	fread(t, size, 1, f);
	fclose(f);
	files[filesCount].name = name;
	files[filesCount].contents = t;
	files[filesCount].length = size;
	filesCount++;

	if (length != NULL)
		*length = size;

	return t;
}

int main(int argc, char *argv[]) {
	if (argc == 1) {
		puts("Usage: carbon <filename>");
		return 1;
	}

	if (parseArguments(argc, argv)) {
		return 4;
	}

	CarbonState instance;
	carbon_init(&instance, readFile);

	uint32_t length = strlen(argv[argc - 1]) + 1;
	char *first = malloc(length);
	strcpy(first, argv[argc - 1]);
	first[length - 1] = 0;
	if (readFile(first, NULL, NULL) == NULL) {
		return 1;
	}
	CarbonRunResult isOk = carbon_execute(
		&instance, files[0].contents, files[0].length, argv[argc - 1], flags);

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
	freeFiles();
	return 0;
}
