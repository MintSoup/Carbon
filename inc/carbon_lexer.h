#pragma once
#include "utils/carbon_commons.h"

typedef struct{
	char* source;
	uint32_t length; 
	char* current;
	char* start;
	uint32_t line;
} CarbonLexer;

