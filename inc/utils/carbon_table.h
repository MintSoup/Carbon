#pragma once

#include "carbon_value.h"
#include "utils/carbon_commons.h"

typedef struct {
	CarbonObj *key;
	CarbonValue value;
} CarbonEntry;

typedef struct {
	CarbonEntry *entries;
	uint32_t count;
	uint32_t capacity;
} CarbonTable;

void carbon_tableInit(CarbonTable *table);
void carbon_tableFree(CarbonTable *table);
bool carbon_tableSet(CarbonTable *table, CarbonObj *key, CarbonValue value);
void carbon_tableAddAll(CarbonTable *from, CarbonTable *to);
bool carbon_tableGet(CarbonTable *table, CarbonObj *key, CarbonValue *out);
bool carbon_tableRemove(CarbonTable *table, CarbonObj *key);
CarbonString *carbon_tableFindString(CarbonTable *table, char *chars,
									 uint32_t length, uint32_t hashCode);
