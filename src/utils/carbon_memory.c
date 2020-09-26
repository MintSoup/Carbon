#include "utils/carbon_commons.h"
#include "utils/carbon_memory.h"
#include <stdlib.h>

size_t heapSize = 0;


void *carbon_reallocate(size_t oldSize, size_t newSize, void *oldptr) {
	if (newSize == 0) {
		free(oldptr);
		heapSize -= oldSize;
		return NULL;
	}
	heapSize += (newSize - oldSize);
	return realloc(oldptr, newSize);
}
