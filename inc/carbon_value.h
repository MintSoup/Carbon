#pragma once

#include "utils/carbon_commons.h"

typedef struct carbon_object CarbonObj;
typedef struct carbon_valueType CarbonValueType;
typedef struct carbon_string CarbonString;
typedef struct carbon_compiler CarbonCompiler;

typedef struct {
	CarbonValueType *returnType;
	CarbonValueType *arguments;
	uint8_t arity;
} CarbonFunctionSignature;

typedef struct carbon_valueType {
	union carbon_typeData {
		struct carbon_valueType *memberType;
		CarbonFunctionSignature *signature;
		struct carbon_string *instanceName;
	} compound;
	enum CarbonValueTag {
		ValueUInt,	 // -|
		ValueInt,	 //	 |--- The order of these three is important
		ValueDouble, // -|
		ValueBool,
		ValueString,
		ValueInstance,
		ValueHashtable,
		ValueArray,
		ValueGenerator,
		ValueFunction,
		ValueObject,
		ValueError,
		ValueVoid,
		ValueUnresolved,
		ValueUntypechecked,
		ValueNull
	} tag;
} CarbonValueType;

typedef union {
	double dbl;
	uint64_t uint;
	int64_t sint;
	bool boolean;
	CarbonObj *obj;
} CarbonValue;

typedef struct {
	CarbonValue *arr;
	bool *primitive;
	uint32_t count;
	uint32_t capacity;
} CarbonValueArray;

uint16_t carbon_writeToValueArray(CarbonValueArray *arr, CarbonValue val, bool primitive);
void carbon_initValueArray(CarbonValueArray *arr);
void carbon_freeCarbonValueArray(CarbonValueArray *arr);
bool carbon_typesEqual(CarbonValueType a, CarbonValueType b);
bool carbon_canAssign(CarbonValueType to, CarbonValueType from,
					  CarbonCompiler *c);
static inline bool isObject(CarbonValueType type) {
	return type.tag >= ValueString && type.tag <= ValueError;
}

#define CarbonInt(x)                                                           \
	(CarbonValue) { .sint = x }
#define CarbonUInt(x)                                                          \
	(CarbonValue) { .uint = x }
#define CarbonDouble(x)                                                        \
	(CarbonValue) { .dbl = x }
#define CarbonBool(x)                                                          \
	(CarbonValue) { .boolean = x }
#define CarbonObject(x)                                                        \
	(CarbonValue) { .obj = x }
