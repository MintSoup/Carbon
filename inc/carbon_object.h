#pragma once

#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "vm/carbon_vm.h"



typedef enum {
	CrbnObjString
} CarbonObjectType;


typedef struct carbon_object{
	CarbonObjectType type;
	struct carbon_object* next;
} CarbonObj;


typedef struct {
	CarbonObj obj;
	char* chars;
	uint32_t length;
} CarbonString;



CarbonString* carbon_copyString(char* chars, uint32_t length, CarbonVM* vm);
CarbonString* carbon_takeString(char* chars, uint32_t length, CarbonVM* vm);


void carbon_freeObject(CarbonObj* obj, CarbonVM* vm);
