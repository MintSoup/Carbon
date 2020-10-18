#include "utils/carbon_table.h"
#include "carbon_value.h"
#include "utils/carbon_memory.h"
#include "carbon_object.h"
#include <string.h>

static const float TABLE_MAX_LOAD = 0.75;

void carbon_tableInit(CarbonTable *table) {
	table->capacity = 0;
	table->count = 0;
	table->entries = NULL;
}
void carbon_tableFree(CarbonTable *table) {
	carbon_reallocate(sizeof(CarbonEntry) * table->capacity, 0, table->entries);
	carbon_tableInit(table);
}

static CarbonEntry *findEntry(CarbonEntry *entries, uint32_t capacity,
							  CarbonObj *key) {

	uint32_t index = key->hashCode % capacity;
	CarbonEntry *grave = NULL;
	while (true) {
		CarbonEntry *entry = &entries[index];
		if (entry->key == key) {
			return entry;
		} else if (entry->key == NULL) {
			if (entry->value.uint == 0) {
				if (grave != NULL)
					return grave;
				return entry;
			} else {
				if (grave == NULL)
					grave = entry;
			}
		}
		index = (index + 1) % capacity;
	}
}

void carbon_tableAddAll(CarbonTable *from, CarbonTable *to) {
	for (uint32_t i = 0; i < from->capacity; i++) {
		CarbonEntry *e = &from->entries[i];
		if (e->key != NULL) {
			carbon_tableSet(to, e->key, e->value);
		}
	}
}

void adjustCapacity(CarbonTable *table, uint32_t newCapacity) {
	CarbonEntry *entries = (CarbonEntry *) carbon_reallocate(
		0, newCapacity * sizeof(CarbonEntry), NULL);
	for (uint32_t i = 0; i < newCapacity; i++) {
		entries[i].key = NULL;
		entries[i].value.uint = 0;
	}
	table->count = 0;
	for (uint32_t i = 0; i < table->capacity; i++) {
		CarbonEntry *oldEntry = &table->entries[i];
		if (oldEntry->key != NULL) {
			CarbonEntry *newEntry =
				findEntry(entries, newCapacity, oldEntry->key);
			*newEntry = *oldEntry;
			table->count++;
		}
	}
	carbon_reallocate(table->capacity * sizeof(CarbonEntry), 0, table->entries);
	table->entries = entries;
	table->capacity = newCapacity;
}

bool carbon_tableSet(CarbonTable *table, CarbonObj *key, CarbonValue value) {
	if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
		uint32_t capacity = table->capacity == 0 ? 8 : table->capacity * 2;
		adjustCapacity(table, capacity);
	}
	CarbonEntry *e = findEntry(table->entries, table->capacity, key);
	bool isNew = e->key == NULL;
	if (isNew && e->value.uint == 0)
		table->count++;
	e->value = value;
	e->key = key;
	return isNew;
}
bool carbon_tableGet(CarbonTable *table, CarbonObj *key, CarbonValue *out) {
	if (table->count == 0)
		return false;
	CarbonEntry *e = findEntry(table->entries, table->capacity, key);
	if (e->key == NULL)
		return false;

	*out = e->value;
	return true;
}
bool carbon_tableRemove(CarbonTable *table, CarbonObj *key) {
	if (table->count == 0)
		return false;
	CarbonEntry *e = findEntry(table->entries, table->capacity, key);
	if (e->key == NULL)
		return false;
	e->key = NULL;
	e->value.uint = 1;
	return true;
}
CarbonString *carbon_tableFindString(CarbonTable *table, char *chars,
									 uint32_t length, uint32_t hashCode) {
	if (table->count == 0)
		return NULL;
	uint32_t index = hashCode % table->capacity;
	while (true) {
		CarbonEntry *entry = &table->entries[index];
		CarbonString *key = (CarbonString *) entry->key;
		if (entry->key == NULL) {
			if (entry->value.uint == 0) {
				return NULL;
			}
		} else if (key->length == length && entry->key->hashCode == hashCode &&
				   memcmp(key->chars, chars, length) == 0) {
			return key;
		}
		index = (index + 1) % table->capacity;
	}
}
