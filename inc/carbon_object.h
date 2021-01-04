#pragma once

#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "vm/carbon_chunk.h"
#include "vm/carbon_vm.h"
#include "carbon_token.h"

typedef enum {
	CrbnObjString,
	CrbnObjFunc,
	CrbnObjArray,
	CrbnObjGenerator
} CarbonObjectType;

typedef struct carbon_object {
	CarbonObjectType type;
	struct carbon_object *next;
	uint32_t hashCode;
} CarbonObj;
typedef struct carbon_string {
	CarbonObj obj;
	char *chars;
	uint32_t length;
} CarbonString;

typedef struct carbon_function {
	CarbonObj obj;
	CarbonChunk chunk;
	CarbonString *name;
	uint16_t arity;
	CarbonValueType returnType;
} CarbonFunction;

typedef struct carbon_array {
	CarbonObj obj;
	CarbonValue *members;
	uint64_t count;
	uint64_t capacity;
	enum CarbonValueTag type;
} CarbonArray;

typedef struct carbon_generator {
	CarbonObj obj;
	CarbonValue first;
	CarbonValue last;
	CarbonValue delta;
	enum CarbonValueTag type;
	uint64_t n;
} CarbonGenerator;

CarbonString *carbon_copyString(char *chars, uint32_t length, CarbonVM *vm);
CarbonString *carbon_takeString(char *chars, uint32_t length, CarbonVM *vm);
CarbonString *carbon_strFromToken(CarbonToken token, CarbonVM *vm);

CarbonFunction *carbon_newFunction(CarbonString *name, uint32_t arity,
								   CarbonValueType returnType, CarbonVM *vm);
CarbonArray *carbon_newArray(uint64_t initSize, enum CarbonValueTag type,
							 CarbonVM *vm);
CarbonGenerator *carbon_newGenerator(CarbonValue first, CarbonValue last,
									 CarbonValue delta,
									 enum CarbonValueTag type, CarbonVM *vm);

void carbon_freeObject(CarbonObj *obj, CarbonVM *vm);
void *carbon_reallocateObj(size_t oldSize, size_t newSize, void *oldptr,
						   CarbonVM *vm);
