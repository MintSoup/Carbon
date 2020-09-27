#include "carbon_value.h"
#include "utils/carbon_memory.h"

static inline void growArray(CarbonValueArray *arr) {
	uint32_t oldSize = arr->capacity;
	uint32_t newSize = 8;
	if (oldSize != 0) newSize = oldSize * 2;
	arr->arr = carbon_reallocate(oldSize, newSize * sizeof(CarbonValue), arr->arr);
	arr->capacity = newSize;
}
void carbon_writeToValueArray(CarbonValueArray *arr, CarbonValue val) {
	if (arr->capacity <= arr->count) {
		growArray(arr);
	}
	arr->arr[arr->count] = val;
	arr->count++;
}
void carbon_initValueArray(CarbonValueArray *arr) {
	arr->arr = NULL;
	arr->capacity = 0;
	arr->count = 0;
}
void carbon_freeCarbonValueArray(CarbonValueArray *arr) {
	carbon_reallocate(arr->capacity * sizeof(CarbonValue), 0, arr->arr);
	carbon_initValueArray(arr);
}