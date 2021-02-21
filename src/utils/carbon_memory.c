#include "utils/carbon_commons.h"
#include "utils/carbon_memory.h"
#include <stdlib.h>
#include <stdio.h>

uint32_t heapSize = 0;

void *carbon_reallocate(uint32_t oldSize, uint32_t newSize, void *oldptr) {
	if (newSize == 0) {
		free(oldptr);
		heapSize -= oldSize;
		return NULL;
	}
	heapSize += newSize - oldSize;
	return realloc(oldptr, newSize);
}
