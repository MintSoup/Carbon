#include "carbon_value.h"
#include "utils/carbon_memory.h"

static inline void growArray(CarbonValueArray *arr) {
	uint32_t oldSize = arr->capacity;
	uint32_t newSize = 8;
	if (oldSize != 0)
		newSize = oldSize * 2;
	arr->arr = carbon_reallocate(oldSize * sizeof(CarbonValue),
								 newSize * sizeof(CarbonValue), arr->arr);
	arr->capacity = newSize;
}
uint16_t carbon_writeToValueArray(CarbonValueArray *arr, CarbonValue val) {
	if (arr->capacity <= arr->count) {
		growArray(arr);
	}
	arr->arr[arr->count] = val;
	return arr->count++;
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

char *CarbonValueTypeLexeme[] = {
	[ValueInt] = "int",
	[ValueUInt] = "uint",
	[ValueString] = "string",
	[ValueDouble] = "double",
	[ValueBool] = "bool",
	[ValueInstance] = "instance",
	[ValueHashtable] = "hashtable",
	[ValueObject] = "object",
	[ValueArray] = "array",
	[ValueFunction] = "function",
	[ValueError] = "Error",
	[ValueUnresolved] = "unresolved",
	[ValueVoid] = "void",
	[ValueGenerator] = "generator",
	[ValueNull] = "Null",
};

bool carbon_typesEqual(CarbonValueType a, CarbonValueType b) {
	if (a.tag != b.tag)
		return false;
	switch (a.tag) {
		case ValueFunction: {
			if (a.compound.signature->arity != b.compound.signature->arity)
				return false;
			if (!carbon_typesEqual(*a.compound.signature->returnType,
								   *b.compound.signature->returnType))
				return false;
			for (uint8_t i = 0; i < a.compound.signature->arity; i++)
				if (!carbon_typesEqual(a.compound.signature->arguments[i],
									   b.compound.signature->arguments[i]))
					return false;
			return true;
		}
		case ValueGenerator:
		case ValueArray: {
			return carbon_typesEqual(*a.compound.memberType,
									 *b.compound.memberType);
		}
		case ValueInstance: {
			return a.compound.instanceName == b.compound.instanceName;
		}
		default:
			return true;
	}
}

bool carbon_canAssign(CarbonValueType to, CarbonValueType from) {

	if (to.tag == from.tag) {
		switch (to.tag) {
			case ValueGenerator:
			case ValueArray:
				return carbon_typesEqual(*to.compound.memberType,
										 *from.compound.memberType);
			case ValueFunction: {
				if (to.compound.signature->arity !=
					from.compound.signature->arity)
					return false;

				if (!carbon_typesEqual(*to.compound.signature->returnType,
									   *from.compound.signature->returnType))
					return false;

				for (uint8_t i = 0; i < to.compound.signature->arity; i++)
					if (!carbon_typesEqual(
							from.compound.signature->arguments[i],
							to.compound.signature->arguments[i]))
						return false;

				return true;
			}
			default:
				return true;
		}
	}

	if (to.tag <= ValueDouble && from.tag <= to.tag)
		return true;
	if (to.tag == ValueUInt && from.tag == ValueInt)
		return true;
	if (from.tag == ValueNull && isObject(to))
		return true;
	if (to.tag == ValueObject)
		return isObject(from);

	return false;
}
