#include "utils/carbon_commons.h"
#include "utils/carbon_memory.h"
#include <stdlib.h>

void *carbon_reallocate(size_t oldSize, size_t newSize, void *oldptr) {
	if (newSize == 0) {
		free(oldptr);
		return NULL;
	}
	return realloc(oldptr, newSize);
}
