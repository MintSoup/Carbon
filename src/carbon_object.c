#include "carbon_object.h"
#include "carbon_value.h"
#include "utils/carbon_memory.h"
#include "utils/carbon_table.h"
#include "vm/carbon_chunk.h"
#include "carbon_token.h"
#include <stdio.h>
#include <string.h>
#include <math.h>

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

CarbonString *carbon_strFromToken(CarbonToken token, CarbonVM *vm) {
	return carbon_copyString(token.lexeme, token.length, vm);
}

CarbonFunction *carbon_newFunction(CarbonString *name, uint32_t arity,
								   CarbonValueType returnType, CarbonVM *vm) {
	CarbonFunction *func =
		(CarbonFunction *) ALLOC(CarbonFunction, CrbnObjFunc);
	func->arity = arity;
	func->name = name;
	func->returnType = returnType;
	carbon_initChunk(&func->chunk);
	return func;
}
CarbonArray *carbon_newArray(uint64_t initSize, enum CarbonValueTag type,
							 CarbonVM *vm) {
	CarbonArray *arr = (CarbonArray *) ALLOC(CarbonArray, CrbnObjArray);
	arr->type = type;
	arr->count = 0;
	arr->capacity = initSize;
	arr->members =
		carbon_reallocateObj(0, sizeof(CarbonValue) * initSize, NULL, vm);
	return arr;
}

CarbonGenerator *carbon_newGenerator(CarbonValue first, CarbonValue last,
									 CarbonValue delta,
									 enum CarbonValueTag type, CarbonVM *vm) {
	if (last.uint == first.uint)
		return NULL;
	switch (type) {
		case ValueUInt:
			if (delta.uint == 0)
				return NULL;
		case ValueInt:
			if (delta.sint == 0)
				return NULL;
			if ((last.sint - first.sint) * delta.sint < 0)
				return NULL;
		case ValueDouble:
			if (delta.dbl == 0)
				return NULL;
			if ((last.dbl - first.dbl) * delta.dbl < 0)
				return NULL;

		default:
			break; // Should never reach here
	}

	CarbonGenerator *gen =
		(CarbonGenerator *) ALLOC(CarbonGenerator, CrbnObjGenerator);
	gen->first = first;
	gen->last = last;
	gen->delta = delta;
	gen->type = type;

	switch (type) {
		case ValueUInt:
			gen->n = ceil((float) (last.uint - first.uint) / delta.uint);
			break;
		case ValueInt:
			gen->n = ceil((float) (last.sint - first.sint) / delta.sint);
			break;
		case ValueDouble:
			gen->n = ceil((float) (last.dbl - first.dbl) / delta.dbl);
			break;
		default:
			break; // Should never reach here
	}
	return gen;
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
	if (current == vm->objects)
		vm->objects = current->next;
	else
		previous->next = current->next;

	switch (obj->type) {
		case CrbnObjString: {
			CarbonString *str = (CarbonString *) obj;
			carbon_reallocateObj(str->length + 1, 0, str->chars, vm);
			carbon_reallocateObj(sizeof(CarbonString), 0, obj, vm);
			break;
		}
		case CrbnObjFunc: {
			CarbonFunction *func = (CarbonFunction *) obj;
			carbon_freeChunk(&func->chunk);
			carbon_reallocateObj(sizeof(CarbonFunction), 0, obj, vm);
			break;
		}
		case CrbnObjArray: {
			CarbonArray *arr = (CarbonArray *) obj;
			carbon_reallocateObj(arr->capacity * sizeof(CarbonValue), 0,
								 arr->members, vm);
			carbon_reallocateObj(sizeof(CarbonArray), 0, arr, vm);
			break;
		}
		case CrbnObjGenerator: {
			carbon_reallocateObj(sizeof(CarbonGenerator), 0, obj, vm);
			break;
		}
	}
}

#undef ALLOC
