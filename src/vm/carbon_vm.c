#include "vm/carbon_vm.h"
#include "carbon.h"
#include "carbon_object.h"
#include "carbon_value.h"
#include "utils/carbon_memory.h"
#include "utils/carbon_table.h"
#include "vm/carbon_chunk.h"
#include "utils/carbon_commons.h"
#include <stdio.h>
#include <string.h>

extern char *CarbonValueTypeLexeme[];

void carbon_initVM(CarbonVM *vm) {
	vm->stackTop = 0;
	vm->objects = NULL;
	vm->objectHeapSize = 0;
	vm->callDepth = 0;
	carbon_tableInit(&vm->strings);
	carbon_tableInit(&vm->globals);
	carbon_tableInit(&vm->primitives);
}
void carbon_freeVM(CarbonVM *vm) {
	while (vm->objects != NULL) {
		CarbonObj *obj = vm->objects;
		carbon_freeObject(obj, vm);
	}
	carbon_tableFree(&vm->strings);
	carbon_tableFree(&vm->globals);
	carbon_tableFree(&vm->primitives);
}

static inline CarbonValue pop(CarbonVM *vm) {
	return vm->stack[--vm->stackTop];
}

static inline void push(CarbonValue v, CarbonVM *vm) {
	vm->stack[vm->stackTop++] = v;
}
static inline CarbonChunk getChunk(CarbonVM *vm) {
	return vm->callStack[vm->callDepth - 1].func->chunk;
}

static inline CarbonValue c16(CarbonVM *vm, uint8_t *ip) {
	uint8_t higher = *ip;
	ip++;
	uint8_t lower = *ip;
	uint16_t index = (higher << 8) | lower;
	return getChunk(vm).constants.arr[index];
}

static void printObject(CarbonObj *obj) {

#define castObj(type, name) type *name = (type *) obj;

	switch (obj->type) {
		case CrbnObjString: {
			castObj(CarbonString, str);
			printf("%s", str->chars);
			break;
		}
		case CrbnObjFunc: {
			castObj(CarbonFunction, func);
			printf("function <%s>", func->name->chars);
			break;
		}
		case CrbnObjArray: {
			castObj(CarbonArray, arr);
			printf("[");
			if (arr->count == 0) {
				printf("<%s>]", CarbonValueTypeLexeme[arr->type]);
				return;
			}
			switch (arr->type) {
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
						if (arr->type == ValueString)
							printf("'");
						printObject(arr->members[i].obj);
						if (arr->type == ValueString)
							printf("'");
						printf(", ");
					}
					if (arr->type == ValueString)
						printf("'");
					printObject(arr->members[arr->count - 1].obj);
					if (arr->type == ValueString)
						printf("'");
					break;
			}
			printf("]");
			break;
		}
		case CrbnObjGenerator: {
			castObj(CarbonGenerator, gen);
			switch (gen->type) {
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
	}
#undef castObj
}

static uint64_t length(CarbonObj *obj) {
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

static bool checkBounds(CarbonObj *obj, CarbonValue index, char **msg) {
	uint64_t olength = length(obj);
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

static CarbonValue getGeneratorIndex(CarbonGenerator *g, uint64_t i) {
	switch (g->type) {
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

static CarbonValue getIndex(CarbonObj *obj, CarbonValue i, CarbonVM *vm) {
	uint64_t index = i.sint < 0 ? (length(obj) + i.sint) : i.uint;
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
			return getGeneratorIndex((CarbonGenerator *) obj, index);
		}
		default:
			return CarbonUInt(0); // Should never reach here
	}
}

static void setIndex(CarbonObj *obj, CarbonValue i, CarbonValue v,
					 CarbonVM *vm) {
	uint64_t index = i.sint < 0 ? (length(obj) + i.sint) : i.uint;
	switch (obj->type) {
		case CrbnObjArray:
			((CarbonArray *) obj)->members[index] = v;
		default:
			return; // Should never reach here
	}
}

static void append(CarbonArray *arr, CarbonValue val, CarbonVM *vm) {
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
}

static uint8_t callFunction(CarbonFunction *func, CarbonVM *vm) {
	if (vm->callDepth == 255) {
		fprintf(stderr,
				"Carbon: Stack overflow while trying to call function '%s'.\n",
				func->name->chars);
		return 1;
	}

	CarbonCallframe frame = {func, func->chunk.code,
							 &(vm->stack[vm->stackTop - func->arity])};
	vm->callStack[vm->callDepth++] = frame;
	return 0;
}

static uint8_t call(CarbonObj *obj, CarbonVM *vm) {
	if (obj->type == CrbnObjFunc)
		return callFunction((CarbonFunction *) obj, vm);
	return 0; // should never reach here
}

static CarbonRunResult runtimeError(char *msg, CarbonVM *vm) {
	CarbonCallframe *frame = &vm->callStack[vm->callDepth - 1];
	CarbonChunk *chunk = &frame->func->chunk;
	uint32_t line = chunk->lines[frame->ip - chunk->code];
	printf("[Line %d] %s.", line, msg);
	return Carbon_Runtime_Error;
}

static bool nullcheck(CarbonObj *obj, char *msg, CarbonVM *vm) {
	if (obj == NULL) {
		runtimeError(msg, vm);
		return true;
	}
	return false;
}

CarbonRunResult carbon_run(CarbonVM *vm, CarbonFunction *func) {

#define ReadByte() *(frame->ip)
#define push(x) push(x, vm)
#define pop() pop(vm)
#define peek() vm->stack[vm->stackTop - 1]

#define binary(type, operator, cast, fieldName)                                \
	do {                                                                       \
		type a = pop().fieldName;                                              \
		type b = pop().fieldName;                                              \
		push(cast(b operator a));                                              \
	} while (false)

#define unary(type, operator, cast, fieldName)                                 \
	do {                                                                       \
		type a = pop().fieldName;                                              \
		push(cast(operator a));                                                \
	} while (false)

#define compare(type, fieldName)                                               \
	do {                                                                       \
		type a = pop().fieldName;                                              \
		type b = pop().fieldName;                                              \
		if (a < b)                                                             \
			push(CarbonInt(1));                                                \
		else if (a > b)                                                        \
			push(CarbonInt(-1));                                               \
		else                                                                   \
			push(CarbonInt(0));                                                \
	} while (false)

#define cast(from, to, nativeType) push(to((nativeType) pop().from))
#define ReadConstant8() getChunk(vm).constants.arr[*frame->ip]
#define ReadConstant16() c16(vm, frame->ip)

	vm->callDepth = 1;
	vm->callStack[0].func = func;
	vm->callStack[0].ip = func->chunk.code;
	vm->callStack[0].slots = (CarbonValue *) &vm->stack;

	CarbonCallframe *frame = &vm->callStack[0];

	while (true) {
		switch (*frame->ip) {
			case OpReturnVoid:
				vm->callDepth--;
				vm->stackTop = frame->slots - vm->stack;
				frame = &vm->callStack[vm->callDepth - 1];
				if (vm->callDepth == 0)
					return Carbon_OK;
				break;
			case OpReturn: {
				CarbonValue v = peek();
				vm->callDepth--;
				vm->stackTop = frame->slots - vm->stack - 1;
				frame = &vm->callStack[vm->callDepth - 1];
				push(v);
				break;
			}
			case OpPopn:
				frame->ip++;
				vm->stackTop -= ReadByte();
				frame->ip++;
				break;
			case OpPop:
				pop();
				frame->ip++;
				break;
			// Signed integer binary ops
			case OpAddInt:
				binary(int64_t, +, CarbonInt, sint);
				frame->ip++;
				break;
			case OpSubInt:
				binary(int64_t, -, CarbonInt, sint);
				frame->ip++;
				break;
			case OpDivInt:
				binary(int64_t, /, CarbonInt, sint);
				frame->ip++;
				break;
			case OpMulInt:
				binary(int64_t, *, CarbonInt, sint);
				frame->ip++;
				break;
			case OpMod:
				binary(int64_t, %, CarbonInt, sint);
				frame->ip++;
				break;

			// Unsigned integer binary ops
			case OpDivUInt:
				binary(uint64_t, /, CarbonUInt, uint);
				frame->ip++;
				break;
			case OpMulUInt:
				binary(uint64_t, *, CarbonUInt, uint);
				frame->ip++;
				break;

			// Double binary ops
			case OpAddDouble:
				binary(double, +, CarbonDouble, dbl);
				frame->ip++;
				break;
			case OpSubDouble:
				binary(double, -, CarbonDouble, dbl);
				frame->ip++;
				break;
			case OpDivDouble:
				binary(double, /, CarbonDouble, dbl);
				frame->ip++;
				break;
			case OpMulDouble:
				binary(double, *, CarbonDouble, dbl);
				frame->ip++;
				break;

			// Unary ops
			case OpNegateBool:
				unary(bool, !, CarbonBool, boolean);
				frame->ip++;
				break;
			case OpNegateUInt:
				unary(uint64_t, -, CarbonInt, uint);
				frame->ip++;
				break;
			case OpNegateInt:
				unary(int64_t, -, CarbonInt, sint);
				frame->ip++;
				break;
			case OpNegateDouble:
				unary(double, -, CarbonDouble, dbl);
				frame->ip++;
				break;

			// Primitive casts
			case OpDoubleToInt:
				cast(dbl, CarbonInt, int64_t);
				frame->ip++;
				break;
			case OpDoubleToUInt:
				cast(dbl, CarbonUInt, uint64_t);
				frame->ip++;
				break;
			case OpIntToDouble:
				cast(sint, CarbonDouble, double);
				frame->ip++;
				break;
			case OpUIntToDouble:
				cast(uint, CarbonDouble, double);
				frame->ip++;
				break;

			// Comparison and equality
			case OpCompareInt:
				compare(int64_t, sint);
				frame->ip++;
				break;
			case OpCompareUInt:
				compare(uint64_t, uint);
				frame->ip++;
				break;
			case OpCompareDouble:
				compare(double, dbl);
				frame->ip++;
				break;

			case OpGreater: {
				int64_t top = pop().sint;
				push(CarbonBool(top == 1));
				frame->ip++;
				break;
			}
			case OpLess: {
				int64_t top = pop().sint;
				push(CarbonBool(top == -1));
				frame->ip++;
				break;
			}
			case OpGEQ: {
				int64_t top = pop().sint;
				push(CarbonBool(top != -1));
				frame->ip++;
				break;
			}
			case OpLEQ: {
				int64_t top = pop().sint;
				push(CarbonBool(top != 1));
				frame->ip++;
				break;
			}

			case OpEquals: {
				uint64_t a = pop().uint;
				uint64_t b = pop().uint;
				push(CarbonBool(a == b));
				frame->ip++;
				break;
			}
			case OpNotEquals: {
				uint64_t a = pop().uint;
				uint64_t b = pop().uint;
				push(CarbonBool(a != b));
				frame->ip++;
				break;
			}

			// Constant instructions
			case OpLoadConstant:
				frame->ip++;
				push(ReadConstant8());
				frame->ip++;
				break;
			case OpLoadConstant16:
				frame->ip++;
				push(ReadConstant16());
				frame->ip += 2;
				break;

			case OpConcat: {
				CarbonString *b = (CarbonString *) pop().obj;
				CarbonString *a = (CarbonString *) pop().obj;
				uint32_t length = a->length + b->length;
				char *concat =
					(char *) carbon_reallocateObj(0, length + 1, NULL, vm);
				concat[length] = 0;
				memcpy(concat, a->chars, a->length);
				memcpy(concat + a->length, b->chars, b->length);
				CarbonString *out = carbon_takeString(concat, length, vm);
				push(CarbonObject((CarbonObj *) out));
				frame->ip++;
				break;
			}

			case OpPrintInt:
				printf("%" PRId64 "\n", pop().sint);
				frame->ip++;
				break;
			case OpPrintUInt:
				printf("%" PRIu64 "\n", pop().uint);
				frame->ip++;
				break;
			case OpPrintDouble:
				printf("%lf\n", pop().dbl);
				frame->ip++;
				break;
			case OpPrintBool:
				printf("%s\n", pop().uint ? "true" : "false");
				frame->ip++;
				break;
			case OpPrintObj: {
				CarbonObj *o = pop().obj;
				if (nullcheck(o, "Cannot print a null object", vm))
					return Carbon_Runtime_Error;

				printObject(o);
				puts("");
				frame->ip++;
				break;
			}

			case OpPush1:
				push(CarbonUInt(1));
				frame->ip++;
				break;
			case OpPush0:
				push(CarbonUInt(0));
				frame->ip++;
				break;

			case OpGetGlobal: {
				CarbonObj *name = pop().obj;
				CarbonValue value;
				carbon_tableGet(&vm->globals, name, &value);
				push(value);
				frame->ip++;
				break;
			}
			case OpSetGlobal: {
				CarbonObj *name = pop().obj;
				CarbonValue value = peek();
				carbon_tableSet(&vm->globals, name, value);
				frame->ip++;
				break;
			}
			case OpGetGlobalInline: {
				frame->ip++;
				CarbonObj *name = ReadConstant8().obj;
				CarbonValue value;
				carbon_tableGet(&vm->globals, name, &value);
				push(value);
				frame->ip++;
				break;
			}
			case OpSetGlobalInline: {
				frame->ip++;
				CarbonObj *name = ReadConstant8().obj;
				CarbonValue value = peek();
				carbon_tableSet(&vm->globals, name, value);
				frame->ip++;
				break;
			}
			case OpSetLocal: {
				frame->ip++;
				uint8_t slot = ReadByte();
				frame->ip++;
				frame->slots[slot] = peek();
				break;
			}
			case OpGetLocal: {
				frame->ip++;
				uint8_t slot = ReadByte();
				frame->ip++;
				push(frame->slots[slot]);
				break;
			}
			case OpCall: {
				frame->ip++;
				uint8_t args = ReadByte();
				frame->ip++;
				CarbonObj *func = vm->stack[vm->stackTop - args - 1].obj;
				if (nullcheck(func, "Cannot call a null function", vm))
					return Carbon_Runtime_Error;
				if (call(func, vm)) {
					return Carbon_Runtime_Error;
				}
				frame = &vm->callStack[vm->callDepth - 1];
				break;
			}
			case OpJumpOnFalse: {
				frame->ip++;
				uint8_t top = ReadByte();
				frame->ip++;
				uint8_t bottom = ReadByte();
				uint16_t range = ((uint16_t) top << 8) | bottom;
				if (!peek().boolean)
					frame->ip += range;
				else
					frame->ip++;
				break;
			}
			case OpJumpOnTrue: {
				frame->ip++;
				uint8_t top = ReadByte();
				frame->ip++;
				uint8_t bottom = ReadByte();
				uint16_t range = ((uint16_t) top << 8) | bottom;
				if (peek().boolean)
					frame->ip += range;
				else
					frame->ip++;
				break;
			}
			case OpJump: {
				frame->ip++;
				uint8_t top = ReadByte();
				frame->ip++;
				uint8_t bottom = ReadByte();
				uint16_t range = ((uint16_t) top << 8) | bottom;
				frame->ip += range;
				break;
			}
			case OpIf: {
				frame->ip++;
				uint8_t top = ReadByte();
				frame->ip++;
				uint8_t bottom = ReadByte();
				uint16_t range = ((uint16_t) top << 8) | bottom;
				if (!pop().boolean)
					frame->ip += range;
				else
					frame->ip++;
				break;
			}
			case OpLoop: {
				frame->ip++;
				uint8_t top = ReadByte();
				frame->ip++;
				uint8_t bottom = ReadByte();
				uint16_t range = ((uint16_t) top << 8) | bottom;
				frame->ip -= range;
				break;
			}

			// Array/generator/length related stuff
			case OpLen: {
				CarbonObj *obj = pop().obj;
				if (nullcheck(obj, "Cannot find the length of a null object",
							  vm))
					return Carbon_Runtime_Error;
				push(CarbonUInt(length(obj)));
				frame->ip++;
				break;
			}
			case OpMakeArray: {
				frame->ip++;
				uint64_t size = ReadByte();
				frame->ip++;
				enum CarbonValueTag type = ReadByte();
				push(CarbonObject(
					(CarbonObj *) carbon_newArray(size, type, vm)));
				frame->ip++;
				break;
			}
			case OpMakeArray64: {
				frame->ip++;
				enum CarbonValueTag type = ReadByte();
				uint64_t size = pop().uint;
				push(CarbonObject(
					(CarbonObj *) carbon_newArray(size, type, vm)));
				frame->ip++;
				break;
			}
			case OpInitArray: {
				CarbonValue v = pop();
				CarbonArray *arr = (CarbonArray *) peek().obj;
				arr->count = arr->capacity;
				for (uint64_t i = 0; i < arr->capacity; i++) {
					arr->members[i] = v;
				}
				frame->ip++;
				break;
			}
			case OpMakeGenerator: {
				frame->ip++;
				enum CarbonValueTag type = ReadByte();
				CarbonValue first = pop();
				CarbonValue last = pop();
				CarbonValue delta = pop();
				CarbonGenerator *g;
				if ((g = carbon_newGenerator(first, last, delta, type, vm)) ==
					NULL)
					return runtimeError("Invalid numbers for generator", vm);
				push(CarbonObject((CarbonObj *) g));
				frame->ip++;
				break;
			}
			case OpAppend: {
				CarbonValue top = pop();
				CarbonArray *arr = (CarbonArray *) peek().obj;
				if (nullcheck((CarbonObj *) arr,
							  "Cannot append to a null object", vm))
					return Carbon_Runtime_Error;
				append(arr, top, vm);
				frame->ip++;
				break;
			}
			case OpGetIndex: {
				CarbonValue index = pop();
				CarbonObj *obj = pop().obj;
				if (nullcheck(obj, "Cannot index a null object", vm))
					return Carbon_Runtime_Error;

				char *msg;
				if (checkBounds(obj, index, &msg)) {
					return runtimeError(msg, vm);
				}
				push(getIndex(obj, index, vm));
				frame->ip++;
				break;
			}
			case OpSetIndex: {
				CarbonValue v = pop();
				CarbonValue index = pop();
				CarbonObj *obj = pop().obj;
				if (nullcheck(obj, "Cannot index a null object", vm))
					return Carbon_Runtime_Error;
				char *msg;
				if (checkBounds(obj, index,
								&msg) /* && obj->type != CrbnObjTable */) {
					return runtimeError(msg, vm);
				}
				setIndex(obj, index, v, vm);
				push(v);
				frame->ip++;
				break;
			}
		}
	}
#undef ReadByte
#undef push
#undef pop
#undef peek
#undef binary
#undef unary
#undef compare
#undef cast
#undef ReadConstant8
#undef ReadConstant16
}
