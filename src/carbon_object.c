#include "carbon_object.h"
#include "carbon_value.h"
#include "utils/carbon_memory.h"
#include "utils/carbon_table.h"
#include <stdio.h>
#include <string.h>

#define ALLOC(size, type) createObject(sizeof(size), type, vm)

void *carbon_reallocateObj(size_t oldSize, size_t newSize, void *oldptr,
						   CarbonVM *vm) {
	vm->objectHeapSize += newSize - oldSize;
	return carbon_reallocate(oldSize, newSize, oldptr);
}

static CarbonObj *createObject(size_t size, CarbonObjectType type,
							   CarbonVM *vm) {
	CarbonObj *obj = carbon_reallocateObj(0, size, NULL, vm);
	obj->type = type;
	obj->next = vm->objects;
	vm->objects = obj;
	return obj;
}

static uint32_t hashString(char *str, uint32_t length) {
	uint32_t hash = 2166136261u;
	for (int i = 0; i < length; i++) {
		hash ^= str[i];
		hash *= 16777619;
	}
	return hash;
}

CarbonString *carbon_copyString(char *chars, uint32_t length, CarbonVM *vm) {
	uint32_t hashCode = hashString(chars, length);
	CarbonString *interned =
		carbon_tableFindString(&vm->strings, chars, length, hashCode);
	if (interned != NULL)
		return interned;

	CarbonString *str = (CarbonString *) ALLOC(CarbonString, CrbnObjString);
	str->chars = carbon_reallocateObj(0, length + 1, NULL, vm);
	str->chars[length] = 0;
	memcpy(str->chars, chars, length);
	str->length = length;
	str->obj.hashCode = hashCode;
	carbon_tableSet(&vm->strings, (CarbonObj *) str, CarbonUInt(0));
	return str;
}
CarbonString *carbon_takeString(char *chars, uint32_t length, CarbonVM *vm) {
	uint32_t hashCode = hashString(chars, length);
	CarbonString *interned =
		carbon_tableFindString(&vm->strings, chars, length, hashCode);
	if (interned != NULL) {
		carbon_reallocateObj(length + 1, 0, chars, vm);
		return interned;
	}

	CarbonString *str = (CarbonString *) ALLOC(CarbonString, CrbnObjString);
	str->chars = chars;
	str->length = length;
	str->obj.hashCode = hashCode;
	carbon_tableSet(&vm->strings, (CarbonObj *) str, CarbonUInt(0));
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
			carbon_reallocateObj(str->length + 1, 0, str->chars, vm);
			carbon_reallocateObj(sizeof(CarbonString), 0, str, vm);
			break;
		}
	}
}

#undef ALLOC
