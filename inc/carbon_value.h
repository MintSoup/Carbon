#pragma once

#include "utils/carbon_commons.h"

typedef enum {
	ValueInt,
	ValueUInt,
	ValueString,
	ValueDouble,
	ValueBool,
	ValueInstance,
	ValueHashtable,
	ValueArray,
	ValueFunction,
	ValueError,
	ValueUnresolved
} CarbonValueType;

typedef union {
	double dbl;
	uint64_t uint;
	int64_t sint;
	bool boolean;
} CarbonValue;

typedef struct {
	CarbonValue* arr;
	uint32_t count;
	uint32_t capacity;
} CarbonValueArray;


void carbon_writeToValueArray(CarbonValueArray *arr, CarbonValue val) ;
void carbon_initValueArray(CarbonValueArray *arr); 
void carbon_freeCarbonValueArray(CarbonValueArray *arr);

#define CarbonInt(x)                                                           \
	(CarbonValue) { .sint = x }
#define CarbonUInt(x)                                                           \
	(CarbonValue) { .uint = x }
#define CarbonDouble(x)                                                           \
	(CarbonValue) { .dbl = x }
#define CarbonBool(x)                                                           \
	(CarbonValue) { .boolean = x }
