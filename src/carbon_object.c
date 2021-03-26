#include "carbon_object.h"
#include "ast/carbon_expressions.h"
#include "carbon_value.h"
#include "utils/carbon_memory.h"
#include "vm/carbon_chunk.h"
#include "carbon_token.h"
#include "vm/carbon_vm.h"
#include "utils/carbon_memory.h"
#include <stdio.h>
#include <string.h>
#include <math.h>

#define ALLOC(size, type) createObject(sizeof(size), type, vm)
extern char *CarbonValueTypeLexeme[];

void *carbon_reallocateObj(uint32_t oldSize, uint32_t newSize, void *oldptr,
						   CarbonVM *vm) {
	vm->objectHeapSize += newSize - oldSize;
	if (oldSize < newSize && vm->gc)
		carbon_gc(vm);
	return carbon_reallocate(oldSize, newSize, oldptr);
}

static CarbonObj *createObject(uint32_t size, CarbonObjectType type,
							   CarbonVM *vm) {
	CarbonObj *obj = carbon_reallocateObj(0, size, NULL, vm);
	obj->type = type;
	obj->marked = false;
	return obj;
}

static inline void regObj(CarbonObj *obj, CarbonVM *vm) {
	obj->next = vm->objects;
	vm->objects = obj;
	vm->objectCount++;
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
	regObj((CarbonObj *) str, vm);
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
	regObj((CarbonObj *) str, vm);
	return str;
}

CarbonString *carbon_strFromToken(CarbonToken token, CarbonVM *vm) {
	return carbon_copyString(token.lexeme, token.length, vm);
}

CarbonFunction *carbon_newFunction(CarbonString *name, uint32_t arity,
								   CarbonFunctionSignature *sig, CarbonVM *vm) {
	CarbonFunction *func =
		(CarbonFunction *) ALLOC(CarbonFunction, CrbnObjFunc);
	func->arity = arity;
	func->name = name;
	func->sig = sig;
	carbon_initChunk(&func->chunk);
	regObj((CarbonObj *) func, vm);
	return func;
}
CarbonArray *carbon_newArray(uint64_t initSize, CarbonValueType *type,
							 CarbonVM *vm) {
	CarbonArray *arr = (CarbonArray *) ALLOC(CarbonArray, CrbnObjArray);
	arr->member = type;
	arr->count = 0;
	arr->capacity = initSize;
	arr->members =
		carbon_reallocateObj(0, sizeof(CarbonValue) * initSize, NULL, vm);
	regObj((CarbonObj *) arr, vm);
	return arr;
}

CarbonGenerator *carbon_newGenerator(CarbonValue first, CarbonValue last,
									 CarbonValue delta, CarbonValueType *type,
									 CarbonVM *vm) {
	if (last.uint == first.uint)
		return NULL;
	switch (type->tag) {
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

	switch (type->tag) {
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
	regObj((CarbonObj *) gen, vm);
	return gen;
}

CarbonBuiltin *carbon_newBuiltin(CarbonObj *parent,
								 char *(*func)(CarbonObj *, CarbonValue *,
											   CarbonVM *vm),
								 CarbonFunctionSignature *sig, CarbonVM *vm) {
	CarbonBuiltin *bltin =
		(CarbonBuiltin *) ALLOC(CarbonBuiltin, CrbnObjBuiltin);
	bltin->func = func;
	bltin->parent = parent;
	bltin->sig = sig;
	regObj((CarbonObj *) bltin, vm);
	return bltin;
}

CarbonInstance *carbon_newInstance(uint8_t type, CarbonVM *vm) {
	CarbonInstance *inst =
		(CarbonInstance *) ALLOC(CarbonInstance, CrbnObjInstance);
	inst->type = type;
	uint32_t size = vm->classes[type].fieldCount * sizeof(CarbonValue);
	inst->fields = carbon_reallocateObj(0, size, NULL, vm);
	memset(inst->fields, 0, size);
	regObj((CarbonObj *) inst, vm);
	return inst;
}
CarbonMethod *carbon_newMethod(CarbonInstance *parent, CarbonFunction *func,
							   CarbonVM *vm) {
	CarbonMethod *method = (CarbonMethod *) ALLOC(CarbonMethod, CrbnObjMethod);
	method->func = func;
	method->parent = parent;
	regObj((CarbonObj *) method, vm);
	return method;
}

static uint64_t normalizeIndex(CarbonValue i, CarbonObj *obj) {
	return i.sint < 0 ? (carbon_objLength(obj) + i.sint) : i.uint;
}

char *carbon_appendArray(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	CarbonArray *arr = (CarbonArray *) parent;
	if (arr == NULL)
		return "Cannot append to a null array";
	CarbonValue val = args[0];

	if (arr->count == arr->capacity) {
		uint32_t oldSize = arr->capacity * sizeof(CarbonValue);
		if (arr->capacity > 0)
			arr->capacity *= 1.5;
		else
			arr->capacity = 8;
		uint32_t newSize = arr->capacity * sizeof(CarbonValue);
		arr->members = carbon_reallocateObj(oldSize, newSize, arr->members, vm);
	}
	arr->members[arr->count++] = val;
	return NULL;
}

char *carbon_splice(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	char *msg;
	if (carbon_checkBounds(parent, args[0], &msg))
		return msg;
	if (carbon_checkBounds(parent, args[1], &msg))
		return msg;

	uint64_t a = normalizeIndex(args[0], parent);
	uint64_t b = normalizeIndex(args[1], parent);
	int64_t lower = a < b ? a : b;
	int64_t higher = a > b ? a : b;

	if (parent->type == CrbnObjArray) {
		CarbonArray *parr = (CarbonArray *) parent;
		CarbonArray *arr =
			carbon_newArray(higher - lower + 1, parr->member, vm);
		arr->count = arr->capacity;
		uint64_t n = 0;
		if (a <= b)
			for (int64_t i = lower; i <= higher; (i++, n++))
				arr->members[n] = parr->members[i];
		else
			for (int64_t i = higher; i >= lower; (i--, n++))
				arr->members[n] = parr->members[i];
		vm->stack[vm->stackTop++] = CarbonObject((CarbonObj *) arr);
	} else {
		CarbonString *pstr = (CarbonString *) parent;
		char *str = carbon_reallocateObj(0, higher - lower + 2, NULL, vm);
		str[higher - lower + 1] = 0;
		uint64_t n = 0;
		if (a <= b)
			for (int64_t i = lower; i <= higher; (i++, n++))
				str[n] = pstr->chars[i];
		else
			for (int64_t i = higher; i >= lower; (i--, n++))
				str[n] = pstr->chars[i];
		CarbonString *out = carbon_takeString(str, higher - lower + 1, vm);
		vm->stack[vm->stackTop++] = CarbonObject((CarbonObj *) out);
	}

	return NULL;
}

void carbon_freeObject(CarbonObj *obj, CarbonVM *vm) {

#define castObj(type, name) type *name = (type *) obj;

	CarbonObj *current = vm->objects;
	CarbonObj *previous = NULL;

	vm->objectCount--;

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
			castObj(CarbonString, str);
			carbon_reallocateObj(str->length + 1, 0, str->chars, vm);
			carbon_reallocateObj(sizeof(CarbonString), 0, obj, vm);
			break;
		}
		case CrbnObjFunc: {
			castObj(CarbonFunction, func);
			carbon_freeChunk(&func->chunk);
			carbon_reallocateObj(sizeof(CarbonFunction), 0, obj, vm);
			break;
		}
		case CrbnObjArray: {
			castObj(CarbonArray, arr);
			carbon_reallocateObj(arr->capacity * sizeof(CarbonValue), 0,
								 arr->members, vm);
			carbon_reallocateObj(sizeof(CarbonArray), 0, arr, vm);
			break;
		}
		case CrbnObjGenerator: {
			carbon_reallocateObj(sizeof(CarbonGenerator), 0, obj, vm);
			break;
		}
		case CrbnObjBuiltin: {
			carbon_reallocateObj(sizeof(CarbonBuiltin), 0, obj, vm);
			break;
		}
		case CrbnObjInstance: {
			castObj(CarbonInstance, inst);
			carbon_reallocateObj(sizeof(CarbonValue) *
									 vm->classes[inst->type].fieldCount,
								 0, inst->fields, vm);
			carbon_reallocateObj(sizeof(CarbonInstance), 0, obj, vm);
			break;
		}
		case CrbnObjMethod: {
			carbon_reallocateObj(sizeof(CarbonMethod), 0, obj, vm);
			break;
		}
	}
#undef castObj
}

void carbon_printObject(CarbonObj *obj) {

#define castObj(type, name) type *name = (type *) obj;

	switch (obj->type) {
		case CrbnObjString: {
			castObj(CarbonString, str);
			printf("%s", str->chars);
			break;
		}
		case CrbnObjFunc: {
			castObj(CarbonFunction, func);
			if (func->name)
				printf("function <%s>", func->name->chars);
			else
				puts("<top level code>");
			break;
		}
		case CrbnObjArray: {
			castObj(CarbonArray, arr);
			printf("[");
			if (arr->count == 0) {
				printf("<%s>]", CarbonValueTypeLexeme[arr->member->tag]);
				return;
			}
			switch (arr->member->tag) {
				case ValueUInt:
					for (uint32_t i = 0; i < arr->count - 1; i++) {
						printf("%" PRIu64 ", ", arr->members[i].uint);
					}
					printf("%" PRIu64, arr->members[arr->count - 1].uint);
					break;
				case ValueInt:
					for (uint32_t i = 0; i < arr->count - 1; i++) {
						printf("%" PRId64 ", ", arr->members[i].sint);
					}
					printf("%" PRId64, arr->members[arr->count - 1].sint);
					break;
				case ValueDouble:
					for (uint32_t i = 0; i < arr->count - 1; i++) {
						printf("%lf, ", arr->members[i].dbl);
					}
					printf("%lf", arr->members[arr->count - 1].dbl);
					break;
				case ValueBool:
					for (uint32_t i = 0; i < arr->count - 1; i++) {
						printf(arr->members[i].boolean ? "true, " : "false, ");
					}
					printf(arr->members[arr->count - 1].boolean ? "true, "
																: "false, ");
					break;
				default:
					for (uint32_t i = 0; i < arr->count - 1; i++) {
						if (arr->member->tag == ValueString)
							printf("'");
						carbon_printObject(arr->members[i].obj);
						if (arr->member->tag == ValueString)
							printf("'");
						printf(", ");
					}
					if (arr->member->tag == ValueString)
						printf("'");
					carbon_printObject(arr->members[arr->count - 1].obj);
					if (arr->member->tag == ValueString)
						printf("'");
					break;
			}
			printf("]");
			break;
		}
		case CrbnObjGenerator: {
			castObj(CarbonGenerator, gen);
			switch (gen->type->tag) {
				case ValueUInt:
					printf("[%" PRIu64 "..%" PRIu64 ":%" PRIu64 "]",
						   gen->first.uint, gen->last.uint, gen->delta.uint);
					break;
				case ValueInt:
					printf("[%" PRId64 "..%" PRId64 ":%" PRId64 "]",
						   gen->first.sint, gen->last.sint, gen->delta.sint);
					break;
				case ValueDouble:
					printf("[%lf..%lf:%lf]", gen->first.dbl, gen->last.dbl,
						   gen->delta.dbl);
					break;
				default:
					break; // Should never reach here
			}
			break;
		}
		case CrbnObjBuiltin: {
			printf("<builtin function>");
			break;
		}
		case CrbnObjInstance: {
			castObj(CarbonInstance, inst);
			printf("<instance id %u>", inst->type);
			break;
		}
		case CrbnObjMethod: {
			castObj(CarbonMethod, mthd);
			printf("<method of %u>", mthd->parent->type);
			break;
		}
	}
#undef castObj
}

CarbonValue carbon_getObjIndex(CarbonObj *obj, CarbonValue i, CarbonVM *vm) {
	uint64_t index = normalizeIndex(i, obj);
	switch (obj->type) {
		case CrbnObjString: {
			CarbonString *str = (CarbonString *) obj;
			CarbonString *c = carbon_copyString(str->chars + index, 1, vm);
			return CarbonObject((CarbonObj *) c);
		}
		case CrbnObjArray: {
			return ((CarbonArray *) obj)->members[index];
		}
		case CrbnObjGenerator: {
			return carbon_getGeneratorIndex((CarbonGenerator *) obj, index);
		}
		default:
			return CarbonUInt(0); // Should never reach here
	}
}

CarbonValue carbon_getGeneratorIndex(CarbonGenerator *g, uint64_t i) {
	switch (g->type->tag) {
		case ValueUInt:
			return CarbonUInt(g->first.uint + g->delta.uint * i);
		case ValueInt:
			return CarbonInt(g->first.sint + g->delta.sint * i);
		case ValueDouble:
			return CarbonDouble(g->first.dbl + g->delta.dbl * i);
		default:
			return CarbonUInt(0); // Should never reach here
	}
}

uint64_t carbon_objLength(CarbonObj *obj) {
	switch (obj->type) {
		case CrbnObjString:
			return ((CarbonString *) obj)->length;
		case CrbnObjArray:
			return ((CarbonArray *) obj)->count;
		case CrbnObjGenerator:
			return ((CarbonGenerator *) obj)->n;
		default:
			return 0; // Should never reach here
	}
}

bool carbon_checkBounds(CarbonObj *obj, CarbonValue index, char **msg) {
	uint64_t olength = carbon_objLength(obj);
	if ((index.sint >= 0 && olength <= index.sint) ||
		(index.sint < 0 && olength < -index.sint)) {
		switch (obj->type) {
			case CrbnObjString:
				*msg = "Out of bounds error on string";
				break;
			case CrbnObjArray:
				*msg = "Out of bounds error on array";
				break;
			case CrbnObjGenerator:
				*msg = "Out of bounds error on generator";
				break;
			default:
				*msg = "";
				break; // Should never reach here
		}
		return true;
	}
	return false;
}

#undef ALLOC
