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

void carbon_initVM(CarbonVM *vm) {
	vm->stackTop = 0;
	vm->objects = NULL;
	vm->objectHeapSize = 0;
	carbon_initChunk(&vm->chunk);
	carbon_tableInit(&vm->strings);
	carbon_tableInit(&vm->globals);
}
void carbon_freeVM(CarbonVM *vm) {
	carbon_freeChunk(&vm->chunk);
	while (vm->objects != NULL) {
		CarbonObj *obj = vm->objects;
		carbon_freeObject(obj, vm);
	}
	carbon_tableFree(&vm->strings);
	carbon_tableFree(&vm->globals);
}

static inline CarbonValue pop(CarbonVM *vm) {
	return vm->stack[--vm->stackTop];
}

static inline void push(CarbonValue v, CarbonVM *vm) {
	vm->stack[vm->stackTop++] = v;
}

static inline CarbonValue c16(CarbonVM *vm, uint8_t *ip) {
	uint8_t higher = *ip;
	ip++;
	uint8_t lower = *ip;
	uint16_t index = (higher << 8) | lower;
	return vm->chunk.constants.arr[index];
}

static void printObject(CarbonObj *obj) {
	switch (obj->type) {
		case CrbnObjString: {
			CarbonString *str = (CarbonString *) obj;
			printf("%s\n", str->chars);
			break;
		}
	}
}

CarbonRunResult carbon_run(CarbonVM *vm) {

#define ReadByte() *(ip++)
#define push(x) push(x, vm)
	register uint8_t *ip = vm->chunk.code;
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
#define ReadConstant8() vm->chunk.constants.arr[*ip]
#define ReadConstant16() c16(vm, ip)
	while (true) {
		switch (*ip) {
			case OpReturn:
				return Carbon_OK;
			case OpPop:
				pop();
				ip++;
				break;

			// Signed integer binary ops
			case OpAddInt:
				binary(int64_t, +, CarbonInt, sint);
				ip++;
				break;
			case OpSubInt:
				binary(int64_t, -, CarbonInt, sint);
				ip++;
				break;
			case OpDivInt:
				binary(int64_t, /, CarbonInt, sint);
				ip++;
				break;
			case OpMulInt:
				binary(int64_t, *, CarbonInt, sint);
				ip++;
				break;

			// Unsigned integer binary ops
			case OpDivUInt:
				binary(uint64_t, /, CarbonUInt, uint);
				ip++;
				break;
			case OpMulUInt:
				binary(uint64_t, *, CarbonUInt, uint);
				ip++;
				break;

			// Double binary ops
			case OpAddDouble:
				binary(double, +, CarbonDouble, dbl);
				ip++;
				break;
			case OpSubDouble:
				binary(double, -, CarbonDouble, dbl);
				ip++;
				break;
			case OpDivDouble:
				binary(double, /, CarbonDouble, dbl);
				ip++;
				break;
			case OpMulDouble:
				binary(double, *, CarbonDouble, dbl);
				ip++;
				break;

			// Unary ops
			case OpNegateBool:
				unary(bool, !, CarbonBool, boolean);
				ip++;
				break;
			case OpNegateUInt:
				unary(uint64_t, -, CarbonInt, uint);
				ip++;
				break;
			case OpNegateInt:
				unary(int64_t, -, CarbonInt, sint);
				ip++;
				break;
			case OpNegateDouble:
				unary(double, -, CarbonDouble, dbl);
				ip++;
				break;

			// Primitive casts
			case OpDoubleToInt:
				cast(dbl, CarbonInt, int64_t);
				ip++;
				break;
			case OpDoubleToUInt:
				cast(dbl, CarbonUInt, uint64_t);
				ip++;
				break;
			case OpIntToDouble:
				cast(sint, CarbonDouble, double);
				ip++;
				break;
			case OpUIntToDouble:
				cast(uint, CarbonDouble, double);
				ip++;
				break;

			// Comparison and equality
			case OpCompareInt:
				compare(int64_t, sint);
				ip++;
				break;
			case OpCompareUInt:
				compare(uint64_t, uint);
				ip++;
				break;
			case OpCompareDouble:
				compare(double, dbl);
				ip++;
				break;

			case OpGreater: {
				int64_t top = pop().sint;
				push(CarbonBool(top == 1));
				ip++;
				break;
			}
			case OpLess: {
				int64_t top = pop().sint;
				push(CarbonBool(top == -1));
				ip++;
				break;
			}
			case OpGEQ: {
				int64_t top = pop().sint;
				push(CarbonBool(top != -1));
				ip++;
				break;
			}
			case OpLEQ: {
				int64_t top = pop().sint;
				push(CarbonBool(top != 1));
				ip++;
				break;
			}

			case OpEquals: {
				uint64_t a = pop().uint;
				uint64_t b = pop().uint;
				push(CarbonBool(a == b));
				ip++;
				break;
			}
			case OpNotEquals: {
				uint64_t a = pop().uint;
				uint64_t b = pop().uint;
				push(CarbonBool(a != b));
				ip++;
				break;
			}

			// Constant instructions
			case OpLoadConstant:
				ip++;
				push(ReadConstant8());
				ip++;
				break;
			case OpLoadConstant16:
				ip++;
				push(ReadConstant16());
				ip += 2;
				break;

			case OpConcat: {
				CarbonString *b = (CarbonString *) pop().obj;
				CarbonString *a = (CarbonString *) pop().obj;
				size_t length = a->length + b->length;
				char *concat =
					(char *) carbon_reallocateObj(0, length + 1, NULL, vm);
				concat[length] = 0;
				memcpy(concat, a->chars, a->length);
				memcpy(concat + a->length, b->chars, b->length);
				CarbonString *out = carbon_takeString(concat, length, vm);
				push(CarbonObject((CarbonObj *) out));
				ip++;
				break;
			}

			case OpPrintInt:
				printf("%" PRId64 "\n", pop().sint);
				ip++;
				break;
			case OpPrintUInt:
				printf("%" PRIu64 "\n", pop().uint);
				ip++;
				break;
			case OpPrintDouble:
				printf("%lf\n", pop().dbl);
				ip++;
				break;
			case OpPrintBool:
				printf("%s\n", pop().uint ? "true" : "false");
				ip++;
				break;
			case OpPrintObj:
				printObject(pop().obj);
				ip++;
				break;

			case OpPush1:
				push(CarbonUInt(1));
				ip++;
				break;
			case OpPush0:
				push(CarbonUInt(0));
				ip++;
				break;

			case OpGetGlobal: {
				CarbonObj *name = pop().obj;
				CarbonValue value;
				carbon_tableGet(&vm->globals, name, &value);	
				push(value);
				ip++;
				break;
			}
			case OpSetGlobal: {
				CarbonObj *name = pop().obj;
				CarbonValue value = peek();
				carbon_tableSet(&vm->globals, name, value);	
				ip++;
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
