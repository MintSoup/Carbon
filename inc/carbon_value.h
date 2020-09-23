#pragma once

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
	ValueError
} CarbonValueType;
