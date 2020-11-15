#pragma once

#include "utils/carbon_commons.h"

typedef struct carbon_object CarbonObj;

typedef enum {
	ValueUInt,	 // The order of these three is important
	ValueInt,	 // The order of these three is important
	ValueDouble, // The order of these three is important
	ValueBool,
	ValueString,
	ValueInstance,
	ValueHashtable,
	ValueArray,
	ValueFunction,
	ValueError,
	ValueVoid,
	ValueUnresolved,
	ValueUntypechecked,
	ValueNull,
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
