#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "utils/carbon_memory.h"
#include "carbon_object.h"
#include "utils/carbon_table.h"
#include "vm/carbon_vm.h"
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

uint32_t heapSize = 0;

void *carbon_reallocate(uint32_t oldSize, uint32_t newSize, void *oldptr) {
	if (newSize == 0) {
		free(oldptr);
		heapSize -= oldSize;
		return NULL;
	}
	heapSize += newSize - oldSize;
	return realloc(oldptr, newSize);
}

static int compare(const void *a, const void *b) {
	return *((intptr_t *) a) - *((intptr_t *) b);
}

static bool find(CarbonValue v, CarbonVM *vm) {
	uint32_t left = 0;
	uint32_t right = vm->gcarrSize - 1;
	while (left <= right) {
		uint32_t middle = (left + right) / 2;
		uint64_t obj = (uint64_t) vm->gcarr[middle];
		if (v.uint == obj)
			return false;
		else if (v.uint < obj)
			right = middle - 1;
		else
			left = middle + 1;
	}
	return true;
}

static void markObject(CarbonObj *o, CarbonVM *vm) {
	if (o == NULL || o->marked)
		return;

	if (vm->greyTop >= vm->greySize) {
		uint32_t old = vm->greySize;
		uint32_t new = 4;
		if (old != 0)
			new = old * 2;
		vm->greySize = new;
		vm->greyStack =
			carbon_reallocate(old * sizeof(CarbonObj *),
							  new * sizeof(CarbonObj *), vm->greyStack);
	}
	vm->greyStack[vm->greyTop++] = o;
	o->marked = true;
}

static void mark(CarbonValue v, CarbonVM *vm) {
	if (v.uint < (uint64_t) vm->gcarr[0] ||
		v.uint > (uint64_t) vm->gcarr[vm->gcarrSize - 1])
		return;
	if (find(v, vm))
		return;
	markObject(v.obj, vm);
}

static void blacken(CarbonObj *obj, CarbonVM *vm) {
#define castObj(type, name) type *name = (type *) obj;
	switch (obj->type) {
		case CrbnObjString:
		case CrbnObjGenerator:
			break;
		case CrbnObjFunc: {
			castObj(CarbonFunction, func);
			markObject((CarbonObj *) func->name, vm);
			CarbonValueArray *consts = &func->chunk.constants;
			for (uint16_t i = 0; i < consts->count; i++) {
				if (!consts->primitive[i])
					markObject(consts->arr[i].obj, vm);
			}
			break;
		}
		case CrbnObjArray: {
			castObj(CarbonArray, arr);
			if (!isObject(*arr->member))
				return;
			for (uint32_t i = 0; i < arr->count; i++)
				markObject(arr->members[i].obj, vm);
			break;
		}
		case CrbnObjBuiltin: {
			castObj(CarbonBuiltin, bltin);
			markObject(bltin->parent, vm);
			break;
		}
		case CrbnObjInstance: {
			castObj(CarbonInstance, inst);
			struct carbon_class *class = &vm->classes[inst->type];
			for (uint8_t i = 0; i < class->fieldCount; i++)
				mark(inst->fields[i], vm);
			break;
		}
		case CrbnObjMethod: {
			castObj(CarbonMethod, mthd);
			markObject((CarbonObj *) mthd->func, vm);
			markObject((CarbonObj *) mthd->parent, vm);
			break;
		}
	}
#undef castObj
}

void carbon_gc(CarbonVM *vm) {
	// init
	if (vm->objectCount > vm->gcarrSize) {
		vm->gcarr =
			carbon_reallocate(vm->gcarrSize * sizeof(CarbonValue),
							  sizeof(CarbonValue) * vm->objectCount, vm->gcarr);
		vm->gcarrSize = vm->objectCount;
	}
	memset(vm->gcarr, 0, vm->gcarrSize);
	CarbonObj *o = vm->objects;
	uint32_t i = 0;
	while (o != NULL) {
		vm->gcarr[i] = o;
		o = o->next;
		i++;
	}
	qsort(vm->gcarr, vm->gcarrSize, sizeof(CarbonValue), compare);

	// Marking roots

	// stack
	for (i = 0; i < vm->stackTop; i++)
		mark(vm->stack[i], vm);

	// methods
	for (i = 0; i < vm->classCount; i++) {
		struct carbon_class *c = &vm->classes[i];
		for (uint8_t j = 0; j < c->methodCount; j++)
			markObject((CarbonObj *) c->methods[j], vm);
	}

	// call stack
	for (i = 0; i < vm->callDepth; i++) {
		markObject((CarbonObj *) vm->callStack[i].func, vm);
	}

	// globals
	for (i = 0; i < vm->globals.capacity; i++) {
		CarbonEntry *e = &vm->globals.entries[i];
		if (e->key == NULL)
			continue;
		CarbonValue dummy;
		if (carbon_tableGet(&vm->primitives, e->key, &dummy))
			continue;
		markObject(e->value.obj, vm);
	}

	// trace
	while (vm->greyTop)
		blacken(vm->greyStack[--vm->greyTop], vm);

	// remove dead strings from strings table
	for (i = 0; i < vm->strings.capacity; i++) {
		CarbonEntry *e = &vm->strings.entries[i];
		if (e->key != NULL && !e->key->marked) {
			e->key = NULL;
			e->value = CarbonUInt(1);
		}
	}

	// sweep
	o = vm->objects;
	while (o != NULL) {
		CarbonObj *next = o->next;
		if (!o->marked) {
			carbon_freeObject(o, vm);
		} else
			o->marked = false;
		o = next;
	}
}
