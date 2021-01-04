#pragma once

#include "utils/carbon_commons.h"

typedef struct carbon_object CarbonObj;
typedef struct carbon_valueType CarbonValueType;

typedef struct {
	CarbonValueType *returnType;
	uint8_t arity;
	CarbonValueType *arguments;
} CarbonFunctionSignature;

typedef struct carbon_valueType {
	enum CarbonValueTag {
		ValueUInt,	 // The order of these three is important
		ValueInt,	 // The order of these three is important
		ValueDouble, // The order of these three is important
		ValueBool,
		ValueString,
		ValueInstance,
		ValueHashtable,
		ValueArray,
		ValueGenerator,
		ValueFunction,
		ValueError,
		ValueVoid,
		ValueUnresolved,
		ValueUntypechecked,
		ValueNull
	} tag;
	union carbon_typeData {
		struct carbon_valueType *memberType;
		CarbonFunctionSignature *signature;
		struct carbon_string *instanceName;
	} compound;
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
	uint32_t count;
	uint32_t capacity;
} CarbonValueArray;

uint16_t carbon_writeToValueArray(CarbonValueArray *arr, CarbonValue val);
void carbon_initValueArray(CarbonValueArray *arr);
void carbon_freeCarbonValueArray(CarbonValueArray *arr);

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
