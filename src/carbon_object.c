#include "carbon_object.h"
#include "carbon_value.h"
#include "utils/carbon_memory.h"
#include <stdio.h>
#include <string.h>

#define ALLOC(size, type) createObject(sizeof(size), type, vm)

static CarbonObj *createObject(size_t size, CarbonObjectType type,
							   CarbonVM *vm) {
	CarbonObj *obj = carbon_reallocate(0, size, NULL);
	obj->type = type;
	obj->next = vm->objects;
	vm->objects = obj;
	return obj;
}



// TODO: after hashtables are implemented, do string interning here

CarbonString *carbon_copyString(char *chars, uint32_t length, CarbonVM *vm) {
	CarbonString *str = (CarbonString *) ALLOC(CarbonString, CrbnObjString);
	str->chars = carbon_reallocate(0, length + 1, NULL);
	str->chars[length] = 0;
	memcpy(str->chars, chars, length);
	str->length = length;
	return str;
}
CarbonString* carbon_takeString(char* chars, uint32_t length, CarbonVM* vm){
	CarbonString *str = (CarbonString *) ALLOC(CarbonString, CrbnObjString);
	str->chars = chars;
	str->length = length;
	return str;
}

void carbon_freeObject(CarbonObj *obj, CarbonVM *vm) {

	CarbonObj *current = vm->objects;
	CarbonObj *previous = NULL;
	while (current != obj) {
		previous = current;
		if (current == NULL)
			fprintf(stderr, "CRITICAL: OBJECT NOT FOUND IN VM\n");
		current = current->next;
	}
	if (current == vm->objects) {
		vm->objects = current->next;
	} else
		previous->next = current->next;

	switch (obj->type) {
		case CrbnObjString: {
			CarbonString *str = (CarbonString *) obj;
			carbon_reallocate(str->length + 1, 0, str->chars);
			carbon_reallocate(sizeof(CarbonString), 0, str);
			break;
		}
	}
}

#undef ALLOC
