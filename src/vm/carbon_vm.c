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

extern char *builtinFunctionNames[];

static char *(*builtinPtrs[])(CarbonObj *, CarbonValue *, CarbonVM *) = {
	[BuiltinAppend] = carbon_appendArray,
	[BuiltinSplice] = carbon_splice,
	[BuiltinRemoveAt] = carbon_removeAt,
	[BuiltinRemove] = carbon_remove,
	[BuiltinRemoveAll] = carbon_removeAll,
	[BuiltinFirst] = carbon_first,
	[BuiltinLast] = carbon_last,
	[BuiltinCloneArr] = carbon_cloneArray,
	[BuiltinUpper] = carbon_upper,
	[BuiltinLower] = carbon_lower,
};

void carbon_initVM(CarbonVM *vm) {
	vm->stackTop = 0;
	vm->objectHeapSize = 0;
	vm->callDepth = 0;
	vm->gcarrSize = 0;
	vm->objectCount = 0;
	vm->greySize = 0;
	vm->greyTop = 0;
	vm->typeCount = 0;
	vm->typeCapacity = 0;
	vm->gc = false;
	vm->objects = NULL;
	vm->greyStack = NULL;
	vm->gcarr = NULL;
	vm->typeData = NULL;
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
	for (uint8_t i = 0; i < vm->classCount; i++) {
		uint32_t methodSize =
			sizeof(CarbonFunction *) * vm->classes[i].methodCount;
		uint32_t referenceSize = sizeof(bool) * vm->classes[i].fieldCount;
		carbon_reallocate(methodSize, 0, vm->classes[i].methods);
		carbon_reallocate(referenceSize, 0, vm->classes[i].reference);
	}
	carbon_reallocate(vm->classCount * sizeof(struct carbon_class), 0,
					  vm->classes);
	carbon_reallocate(vm->gcarrSize * sizeof(CarbonValue), 0, vm->gcarr);
	carbon_reallocate(vm->greySize * sizeof(CarbonObj *), 0, vm->greyStack);

	for (uint16_t i = 0; i < vm->typeCount; i++) {
		carbon_freeType(vm->typeData[i]);
	}
	carbon_reallocate(vm->typeCapacity * sizeof(CarbonValueType), 0,
					  vm->typeData);
}

static bool isInstane(CarbonObj *o, uint8_t n, CarbonVM *vm) {
	if (o->type != CrbnObjInstance)
		return false;
	CarbonInstance *i = (CarbonInstance *) o;
	if (i->type == n)
		return true;
	struct carbon_class *class = &vm->classes[i->type];
	while (class->superclass >= 0) {
		if (class->superclass == n)
			return true;
		class = &vm->classes[class->superclass];
	}
	return false;
}

static inline CarbonValue pop(CarbonVM *vm) {
	return vm->stack[--vm->stackTop];
}

static inline void push(CarbonValue v, CarbonVM *vm) {
	//TODO: Cleanly exit on stack overflow
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

static inline CarbonValueType *t16(CarbonVM *vm, uint8_t *ip) {
	uint8_t higher = *ip;
	ip++;
	uint8_t lower = *ip;
	uint16_t index = (higher << 8) | lower;
	return &vm->typeData[index];
}

static inline CarbonValueType newType(enum CarbonValueTag tag) {
	CarbonValueType typ;
	typ.tag = tag;
	typ.compound.instanceName = NULL;
	return typ;
}

static inline bool is(CarbonObj *obj, CarbonValueType *type) {
	switch (obj->type) {
		case CrbnObjGenerator: {
			CarbonGenerator *gen = (CarbonGenerator *) obj;
			return type->tag == ValueGenerator &&
				   carbon_typesEqual(*gen->type, *type->compound.memberType);
		}
		case CrbnObjArray: {
			CarbonArray *arr = (CarbonArray *) obj;
			return type->tag == ValueArray &&
				   carbon_typesEqual(*type->compound.memberType, *arr->member);
		}
		case CrbnObjString:
			return carbon_canAssign(*type, newType(ValueString), NULL);
		case CrbnObjFunc: {
			CarbonFunction *func = (CarbonFunction *) obj;
			CarbonValueType t = newType(ValueFunction);
			t.compound.signature = func->sig;
			return carbon_canAssign(*type, t, NULL);
		}
		case CrbnObjBuiltin: {
			CarbonBuiltin *func = (CarbonBuiltin *) obj;
			CarbonValueType t = newType(ValueFunction);
			t.compound.signature = func->sig;
			return carbon_canAssign(*type, t, NULL);
		}
		default:
			return false; // Should never reach here
	}
}

static void setIndex(CarbonObj *obj, CarbonValue i, CarbonValue v,
					 CarbonVM *vm) {
	uint64_t index = i.sint < 0 ? (carbon_objLength(obj) + i.sint) : i.uint;
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

static CarbonRunResult runtimeError(char *msg, CarbonVM *vm) {
	CarbonCallframe *frame = &vm->callStack[vm->callDepth - 1];
	CarbonChunk *chunk = &frame->func->chunk;
	uint32_t line = carbon_getLine(chunk, frame->ip - chunk->code);
	fprintf(stderr, "[Line %d] %s.\n", line, msg);
	return Carbon_Runtime_Error;
}

static uint8_t callFunction(CarbonFunction *func, uint8_t arity, CarbonVM *vm) {
	if (vm->callDepth == 255) {
		fprintf(stderr,
				"Carbon: Stack overflow while trying to call function '%s'.\n",
				func->name->chars);
		return 1;
	}

	CarbonCallframe frame = {func, func->chunk.code,
							 &(vm->stack[vm->stackTop - arity])};
	vm->callStack[vm->callDepth++] = frame;
	return 0;
}

static uint8_t call(CarbonObj *obj, CarbonVM *vm, uint8_t arity) {
	switch (obj->type) {
		case CrbnObjFunc:
			return callFunction((CarbonFunction *) obj, arity, vm);
		case CrbnObjBuiltin: {
			CarbonBuiltin *bltin = (CarbonBuiltin *) obj;
			char *msg = bltin->func(bltin->parent,
									&vm->stack[vm->stackTop - arity], vm);
			if (msg != NULL) {
				runtimeError(msg, vm);
				return 1;
			}
			if (bltin->sig->returnType->tag != ValueVoid) {
				CarbonValue ret = vm->stack[vm->stackTop - 1];
				vm->stackTop -= arity + 1;
				vm->stack[vm->stackTop - 1] = ret;
			} else {
				vm->stackTop -= arity + 1;
			}
			break;
		}
		case CrbnObjMethod: {
			CarbonMethod *mthd = (CarbonMethod *) obj;
			push(CarbonObject((CarbonObj *) mthd->parent), vm);
			callFunction(mthd->func, arity + 1, vm);
			break;
		}
		default:
			break;
	}
	return 0;
}

static bool nullcheck(CarbonObj *obj, char *msg, CarbonVM *vm) {
	if (obj == NULL) {
		runtimeError(msg, vm);
		return true;
	}
	return false;
}

uint16_t carbon_pushType(CarbonVM *vm, CarbonValueType type) {
	if (vm->typeCapacity <= vm->typeCount) {
		uint32_t oldSize = vm->typeCount;
		uint32_t newSize = 8;
		if (oldSize != 0)
			newSize = oldSize * 2;
		vm->typeData =
			carbon_reallocate(oldSize * sizeof(CarbonValueType),
							  newSize * sizeof(CarbonValueType), vm->typeData);
		vm->typeCapacity = newSize;
	}
	vm->typeData[vm->typeCount] = type;
	return vm->typeCount++;
}

CarbonRunResult carbon_run(CarbonVM *vm, CarbonFunction *func) {

#define ReadByte() *(frame->ip)
#define push(x) push(x, vm)
#define pop() pop(vm)
#define peek() vm->stack[vm->stackTop - 1]
#define peekn(x) vm->stack[vm->stackTop - 1 - x]

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
#define ReadType() t16(vm, frame->ip)

	vm->callDepth = 1;
	vm->callStack[0].func = func;
	vm->callStack[0].ip = func->chunk.code;
	vm->callStack[0].slots = (CarbonValue *) &vm->stack;

	CarbonCallframe *frame = &vm->callStack[0];

	vm->gc = true;

	while (true) {
		switch (*frame->ip) {
			case OpReturnVoid:
				vm->callDepth--;
				if (vm->callDepth == 0) {
					vm->stackTop = 0;
					vm->gc = false;
					return Carbon_OK;
				}
				vm->stackTop = frame->slots - vm->stack - 1;
				frame = &vm->callStack[vm->callDepth - 1];
				break;
			case OpReturn: {
				CarbonValue v = peek();
				vm->callDepth--;
				if (vm->callDepth == 0) {
					vm->stackTop = 1;
					vm->stack[0] = v;
					vm->gc = false;
					return Carbon_OK;
				}
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
				if (peek().uint == 0)
					return runtimeError("Division by zero", vm);
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
				if (peek().uint == 0)
					return runtimeError("Division by zero", vm);
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
			case OpToBool:
				cast(obj, CarbonBool, bool);
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
				CarbonString *b = (CarbonString *) peek().obj;
				CarbonString *a = (CarbonString *) peekn(1).obj;
				uint32_t length = a->length + b->length;
				char *concat =
					(char *) carbon_reallocateObj(0, length + 1, NULL, vm);
				concat[length] = 0;
				memcpy(concat, a->chars, a->length);
				memcpy(concat + a->length, b->chars, b->length);
				CarbonString *out = carbon_takeString(concat, length, vm);
				vm->stackTop -= 2;
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
				printf("%g\n", pop().dbl);
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

				carbon_printObject(o);
				fflush(stdout);
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
				uint8_t arity = ReadByte();
				frame->ip++;
				CarbonObj *func = vm->stack[vm->stackTop - arity - 1].obj;
				if (nullcheck(func, "Cannot call a null function", vm))
					return Carbon_Runtime_Error;
				if (call(func, vm, arity)) {
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
				push(CarbonUInt(carbon_objLength(obj)));
				frame->ip++;
				break;
			}
			case OpMakeArray: {
				frame->ip++;
				uint8_t size = ReadByte();
				frame->ip++;
				push(CarbonObject(
					(CarbonObj *) carbon_newArray(size, ReadType(), vm)));
				frame->ip += 2;
				break;
			}
			case OpMakeArray64: {
				frame->ip++;
				uint64_t size = pop().uint;
				push(CarbonObject(
					(CarbonObj *) carbon_newArray(size, ReadType(), vm)));
				frame->ip += 2;
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
				CarbonValue first = peek();
				CarbonValue last = peekn(1);
				CarbonValue delta = peekn(2);
				CarbonGenerator *g;
				if ((g = carbon_newGenerator(first, last, delta, ReadType(),
											 vm)) == NULL)
					return runtimeError("Invalid numbers for generator", vm);
				vm->stackTop -= 3;
				push(CarbonObject((CarbonObj *) g));
				frame->ip += 2;
				break;
			}
			case OpAppend: {
				CarbonValue top = peek();
				CarbonArray *arr = (CarbonArray *) peekn(1).obj;
				if (nullcheck((CarbonObj *) arr,
							  "Cannot append to a null object", vm))
					return Carbon_Runtime_Error;
				append(arr, top, vm);
				pop();
				frame->ip++;
				break;
			}
			case OpGetIndex: {
				CarbonValue index = pop();
				CarbonObj *obj = pop().obj;
				if (nullcheck(obj, "Cannot index a null object", vm))
					return Carbon_Runtime_Error;

				char *msg;
				if (carbon_checkBounds(obj, index, &msg)) {
					return runtimeError(msg, vm);
				}
				push(carbon_getObjIndex(obj, index, vm));
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
				if (carbon_checkBounds(obj, index,
								&msg) /* && obj->type != CrbnObjTable */) {
					return runtimeError(msg, vm);
				}
				setIndex(obj, index, v, vm);
				push(v);
				frame->ip++;
				break;
			}
			case OpFor: {
				frame->ip++;
				uint8_t top = ReadByte();
				frame->ip++;
				uint8_t bottom = ReadByte();
				uint16_t range = ((uint16_t) top << 8) | bottom;

				uint64_t i = peek().uint;
				CarbonObj *obj = vm->stack[vm->stackTop - 2].obj;
				if (i < carbon_objLength(obj)) {
					vm->stack[vm->stackTop - 1].uint++;
					push(carbon_getObjIndex(obj, CarbonUInt(i), vm));
					frame->ip++;
				} else {
					frame->ip += range;
				}
				break;
			}
			case OpBuiltin: {
				frame->ip++;
				CarbonObj *obj = peek().obj;

				char *(*func)(CarbonObj *, CarbonValue *, CarbonVM *) =
					builtinPtrs[*frame->ip++];

				CarbonBuiltin *bltin = carbon_newBuiltin(
					obj, func, ReadType()->compound.signature, vm);
				pop();
				push(CarbonObject((CarbonObj *) bltin));
				frame->ip += 2;
				break;
			}
			case OpIs: {
				frame->ip++;
				CarbonObj *object = pop().obj;
				if (nullcheck(object, "Cannot check type of a null object", vm))
					return Carbon_Runtime_Error;
				CarbonValueType *wanted = ReadType();
				push(CarbonBool(is(object, wanted)));
				frame->ip += 2;
				break;
			}
			case OpCastcheck: {
				frame->ip++;
				CarbonObj *object = peek().obj;
				if (nullcheck(object, "Cannot cast a null object", vm))
					return Carbon_Runtime_Error;
				CarbonValueType *wanted = ReadType();
				if (!is(object, wanted))
					return runtimeError("Cast failed", vm);
				frame->ip += 2;
				break;
			}
			case OpIsInstance: {
				frame->ip++;
				CarbonObj *object = pop().obj;
				if (nullcheck(object, "Cannot check type of a null object", vm))
					return Carbon_Runtime_Error;
				uint8_t n = ReadByte();
				push(CarbonBool(isInstane(object, n, vm)));
				frame->ip++;
				break;
			}
			case OpInstanceCastcheck: {
				frame->ip++;
				CarbonObj *object = peek().obj;
				if (nullcheck(object, "Cannot cast a null object", vm))
					return Carbon_Runtime_Error;
				uint8_t n = ReadByte();
				if (!isInstane(object, n, vm))
					return runtimeError("Cast failed", vm);
				frame->ip++;
				break;
			}
			case OpDot: {
				frame->ip++;
				uint8_t index = ReadByte();
				CarbonInstance *i = (CarbonInstance *) pop().obj;
				if (nullcheck((CarbonObj *) i, "Cannot access a null object",
							  vm))
					return Carbon_Runtime_Error;
				push(i->fields[index]);
				frame->ip++;
				break;
			}
			case OpDotSet: {
				frame->ip++;
				uint8_t index = ReadByte();
				CarbonValue val = pop();
				CarbonInstance *i = (CarbonInstance *) pop().obj;
				if (nullcheck((CarbonObj *) i, "Cannot access a null object",
							  vm))
					return Carbon_Runtime_Error;
				i->fields[index] = val;
				push(val);
				frame->ip++;
				break;
			}
			case OpMethod: {
				frame->ip++;
				uint8_t index = ReadByte();
				CarbonInstance *i = (CarbonInstance *) peek().obj;
				if (nullcheck((CarbonObj *) i, "Cannot access a null object",
							  vm))
					return Carbon_Runtime_Error;
				CarbonFunction *f = vm->classes[i->type].methods[index];
				CarbonMethod *m = carbon_newMethod(i, f, vm);
				pop();
				push(CarbonObject((CarbonObj *) m));
				frame->ip++;
				break;
			}
			case OpMakeInstance: {
				frame->ip++;
				uint8_t index = ReadByte();
				CarbonInstance *i = carbon_newInstance(index, vm);
				push(CarbonObject((CarbonObj *) i));
				frame->ip++;
				break;
			}
			case OpInitInstance: {
				frame->ip++;
				uint8_t index = ReadByte();
				struct carbon_class *class = &vm->classes[index];
				CarbonFunction *init = class->methods[class->init];
				push(CarbonObject((CarbonObj *) init));
				frame->ip++;
				break;
			}
			case OpSuper: {
				frame->ip++;
				uint8_t class = ReadByte();
				frame->ip++;
				uint8_t id = ReadByte();
				frame->ip++;
				uint8_t slot = ReadByte();
				frame->ip++;
				CarbonFunction *func = vm->classes[class].methods[id];
				CarbonInstance *self =
					(CarbonInstance *) frame->slots[slot].obj;
				CarbonMethod *m = carbon_newMethod(self, func, vm);
				push(CarbonObject((CarbonObj *) m));
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
#undef ReadType
}
