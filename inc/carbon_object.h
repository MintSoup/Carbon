#pragma once

#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "vm/carbon_chunk.h"
#include "vm/carbon_vm.h"
#include "carbon_token.h"

enum CarbonBuiltinID {
	BuiltinAppend,
	BuiltinSplice,
	BuiltinRemoveAt,
	BuiltinRemove,
	BuiltinRemoveAll,
	BuiltinFirst,
	BuiltinLast,
	BuiltinCloneArr,
	BuiltinUpper,
	BuiltinLower
};

typedef enum {
	CrbnObjString,
	CrbnObjFunc,
	CrbnObjArray,
	CrbnObjGenerator,
	CrbnObjBuiltin,
	CrbnObjInstance,
	CrbnObjMethod,
} CarbonObjectType;

struct carbon_object {
	struct carbon_object *next;
	CarbonObjectType type;
	uint32_t hashCode;
	bool marked;
};
struct carbon_string {
	CarbonObj obj;
	char *chars;
	uint32_t length;
};

struct carbon_function {
	CarbonObj obj;
	CarbonChunk chunk;
	CarbonString *name;
	CarbonFunctionSignature *sig;
	uint16_t arity;
};

typedef struct {
	CarbonObj obj;
	CarbonValue *members;
	uint64_t count;
	uint64_t capacity;
	CarbonValueType *member;
} CarbonArray;

typedef struct {
	CarbonObj obj;
	CarbonValue first;
	CarbonValue last;
	CarbonValue delta;
	CarbonValueType *type;
	uint64_t n;
} CarbonGenerator;

typedef struct {
	CarbonObj obj;
	CarbonObj *parent;
	CarbonFunctionSignature *sig;
	char *(*func)(CarbonObj *, CarbonValue *, CarbonVM *);
} CarbonBuiltin;

typedef struct {
	CarbonObj obj;
	CarbonValue *fields;
	uint8_t type;
} CarbonInstance;

typedef struct {
	CarbonObj obj;
	CarbonFunction *func;
	CarbonInstance *parent;
} CarbonMethod;

CarbonString *carbon_copyString(char *chars, uint32_t length, CarbonVM *vm);
CarbonString *carbon_takeString(char *chars, uint32_t length, CarbonVM *vm);
CarbonString *carbon_strFromToken(CarbonToken token, CarbonVM *vm);
CarbonFunction *carbon_newFunction(CarbonString *name, uint32_t arity,
								   CarbonFunctionSignature *sig, CarbonVM *vm);
CarbonArray *carbon_newArray(uint64_t initSize, CarbonValueType *type,
							 CarbonVM *vm);
CarbonGenerator *carbon_newGenerator(CarbonValue first, CarbonValue last,
									 CarbonValue delta, CarbonValueType *type,
									 CarbonVM *vm);
CarbonBuiltin *carbon_newBuiltin(CarbonObj *parent,
								 char *(*func)(CarbonObj *, CarbonValue *,
											   CarbonVM *vm),
								 CarbonFunctionSignature *sig, CarbonVM *vm);
CarbonInstance *carbon_newInstance(uint8_t type, CarbonVM *vm);
CarbonMethod *carbon_newMethod(CarbonInstance *parent, CarbonFunction *func,
							   CarbonVM *vm);

char *carbon_appendArray(CarbonObj *arr, CarbonValue *args, CarbonVM *vm);
char *carbon_splice(CarbonObj *it, CarbonValue *args, CarbonVM *vm);
char *carbon_removeAt(CarbonObj *it, CarbonValue *args, CarbonVM *vm);
char *carbon_remove(CarbonObj *it, CarbonValue *args, CarbonVM *vm);
char *carbon_removeAll(CarbonObj *it, CarbonValue *args, CarbonVM *vm);
char *carbon_first(CarbonObj *it, CarbonValue *args, CarbonVM *vm);
char *carbon_last(CarbonObj *it, CarbonValue *args, CarbonVM *vm);
char *carbon_cloneArray(CarbonObj *it, CarbonValue *args, CarbonVM *vm);
char *carbon_upper(CarbonObj *parent, CarbonValue *args, CarbonVM *vm);
char *carbon_lower(CarbonObj *parent, CarbonValue *args, CarbonVM *vm);
void carbon_printObject(CarbonObj *obj);

CarbonValue carbon_getObjIndex(CarbonObj *obj, CarbonValue i, CarbonVM *vm);
CarbonValue carbon_getGeneratorIndex(CarbonGenerator *g, uint64_t i);
uint64_t carbon_objLength(CarbonObj *obj);
bool carbon_checkBounds(CarbonObj *obj, CarbonValue index, char **msg);

void carbon_freeObject(CarbonObj *obj, CarbonVM *vm);
void *carbon_reallocateObj(uint32_t oldSize, uint32_t newSize, void *oldptr,
						   CarbonVM *vm);
