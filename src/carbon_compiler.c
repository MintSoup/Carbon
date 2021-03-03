#include "ast/carbon_expressions.h"
#include "ast/carbon_statements.h"
#include "carbon.h"
#include "carbon_compiler.h"
#include "carbon_object.h"
#include "carbon_token.h"
#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "utils/carbon_table.h"
#include "utils/carbon_memory.h"
#include "vm/carbon_chunk.h"
#include "vm/carbon_vm.h"
#include "utils/carbon_disassembler.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
extern char *CarbonValueTypeLexeme[];
static CarbonToken v2token[] = {
	[ValueDouble] = {.type = TokenDouble,
					 .lexeme = NULL,
					 .line = UINT32_MAX,
					 .length = 0},
	[ValueInt] = {
		.type = TokenInt, .lexeme = NULL, .line = UINT32_MAX, .length = 0}};

static enum CarbonValueTag t2value[] = {[TokenInt] = ValueInt,
										[TokenUInt] = ValueUInt,
										[TokenDouble] = ValueDouble,
										[TokenClassname] = ValueInstance,
										[TokenBool] = ValueBool,
										[TokenString] = ValueString,
										[TokenVoid] = ValueVoid,
										[TokenArray] = ValueArray,
										[TokenError] = ValueError,
										[TokenFunction] = ValueFunction,
										[TokenGenerator] = ValueGenerator,
										[TokenObject] = ValueObject,
										[TokenTable] = ValueHashtable};

typedef struct {
	bool declared;
	CarbonValueType valueType;
} Global;

typedef struct carbon_classInfo Class;

static Class *newClass(uint8_t id) {
	Class *class = carbon_reallocate(0, sizeof(Class), NULL);
	class->id = id;
	class->methods = NULL;
	class->fields = NULL;
	class->parent = NULL;
	class->fieldCount = 0;
	class->fieldCap = 0;
	class->methodCount = 0;
	class->methodCap = 0;
	class->hasInit = false;
	class->declared = false;
	return class;
}

static void sealClass(Class *class) {
	uint32_t old = class->fieldCap * sizeof(struct field);
	uint32_t new = class->fieldCount * sizeof(struct field);
	class->fieldCap = class->fieldCount;
	class->fields = carbon_reallocate(old, new, class->fields);

	old = class->methodCap * sizeof(struct method);
	new = class->methodCount * sizeof(struct method);
	class->methodCap = class->methodCount;
	class->methods = carbon_reallocate(old, new, class->methods);
}

static void freeClass(Class *class) {
	for (uint8_t i = 0; i < class->fieldCount; i++) {
		carbon_freeType(class->fields[i].type);
	}

	for (uint8_t i = 0; i < class->methodCount; i++) {
		carbon_freeSignature(class->methods[i].sig);
	}
	carbon_reallocate(class->fieldCap * sizeof(struct field), 0, class->fields);
	carbon_reallocate(class->methodCap * sizeof(struct method), 0,
					  class->methods);
	carbon_reallocate(sizeof(Class), 0, class);
}

static void pushField(CarbonString *name, CarbonValueType type, Class *class) {
	if (class->fieldCount >= class->fieldCap) {
		uint32_t old = class->fieldCap;
		uint32_t new = 8;
		if (old != 0)
			new = old * 2;
		class->fields =
			carbon_reallocate(old * sizeof(struct field),
							  new * sizeof(struct field), class->fields);
		class->fieldCap = new;
	}
	class->fields[class->fieldCount].name = name;
	class->fields[class->fieldCount].type = type;
	class->fieldCount++;
}

static void pushMethod(CarbonString *name, CarbonFunctionSignature sig,
					   Class *class) {
	if (class->methodCount >= class->methodCap) {
		uint32_t old = class->methodCap;
		uint32_t new = 8;
		if (class->methodCap != 0)
			new = old * 2;
		class->methods =
			carbon_reallocate(old * sizeof(struct method),
							  new * sizeof(struct method), class->methods);
		class->methodCap = new;
	}
	class->methods[class->methodCount].name = name;
	class->methods[class->methodCount].sig = sig;
	class->methodCount++;
}

static int16_t findMember(Class *c, CarbonString *name) {
	for (uint8_t i = 0; i < c->fieldCount; i++) {
		if (c->fields[i].name == name)
			return (int16_t) i + 1;
	}
	for (uint8_t i = 0; i < c->methodCount; i++) {
		if (c->methods[i].name == name) {
			int16_t r = -i - 1;
			return r;
		}
	}
	return 0;
}

static Global *newGlobal(CarbonValueType valueType) {
	Global *g = carbon_reallocate(0, sizeof(Global), NULL);
	g->declared = false;
	g->valueType = valueType;
	return g;
}

#undef global

static inline CarbonValueType newType(enum CarbonValueTag tag) {
	CarbonValueType typ;
	typ.tag = tag;
	typ.compound.instanceName = NULL;
	return typ;
}

static CarbonValueType resolveLocalType(CarbonString *name, CarbonCompiler *c) {
	for (int16_t i = c->localCount - 1; i >= 0; i--) {
		CarbonLocal *l = &c->locals[i];
		if (l->name == name) {
			return l->type;
		}
	}
	return newType(ValueVoid);
}

static int16_t resolveLocal(CarbonString *name, CarbonCompiler *c) {
	for (int16_t i = c->localCount - 1; i >= 0; i--) {
		CarbonLocal *l = &c->locals[i];
		if (l->name == name) {
			return i;
		}
	}
	return -1;
}

static CarbonExpr *promoteNumerics(CarbonValueType wants, CarbonExpr *given) {
	if (wants.tag <= ValueDouble && given->evalsTo.tag < wants.tag) {
		CarbonTypename tn = {
			.base = v2token[wants.tag], .templateCount = 0, .templates = NULL};
		CarbonExpr *r = (CarbonExpr *) carbon_newCastExpr(tn, given);
		r->evalsTo = wants;
		return r;
	}
	return given;
}

static bool canUnary(CarbonTokenType op, CarbonValueType operand) {
	switch (operand.tag) {
		case ValueDouble:
		case ValueUInt:
		case ValueInt:
			return op == TokenMinus;
		case ValueBool:
			return op == TokenBang;
		default:
			return false;
	}
}

bool carbon_isSuperclass(CarbonValueType sub, CarbonValueType super,
						 CarbonCompiler *c) {
	Class *a;
	Class *b;
	if (!carbon_tableGet(&c->classes, (CarbonObj *) sub.compound.instanceName,
						 (CarbonValue *) &a))
		return true;
	if (!carbon_tableGet(&c->classes, (CarbonObj *) super.compound.instanceName,
						 (CarbonValue *) &b))
		return true;

	while (a->parent != NULL) {
		if (a->parent == b)
			return true;
		a = a->parent;
	}

	return false;
}

static bool canCast(CarbonValueType from, CarbonValueType to,
					CarbonCompiler *c) {
	if (to.tag == ValueBool)
		return true;

	switch (from.tag) {
		case ValueUInt:
			return to.tag == ValueInt || to.tag == ValueDouble;
		case ValueInt:
			return to.tag == ValueUInt || to.tag == ValueDouble;
		case ValueDouble:
			return to.tag == ValueUInt || to.tag == ValueInt;
		case ValueBool:
			return to.tag == ValueInt || to.tag == ValueUInt;
		case ValueObject:
			return isObject(from);
		case ValueInstance:
			return to.tag == ValueInstance && carbon_isSuperclass(to, from, c);
		default:
			return false;
	}
}

static bool canBinary(CarbonTokenType op, CarbonValueType left,
					  CarbonValueType right) {

	bool leftNumeric = (left.tag == ValueInt) || (left.tag == ValueUInt) ||
					   (left.tag == ValueDouble);
	bool rightNumeric = (right.tag == ValueInt) || (right.tag == ValueUInt) ||
						(right.tag == ValueDouble);

	bool bothStrings = left.tag == ValueString && right.tag == ValueString;

	switch (op) {
		case TokenPlus:
		case TokenPlusEquals:
			if (bothStrings)
				return true;
		case TokenMinus:
		case TokenSlash:
		case TokenStar:
		case TokenMinusEquals:
		case TokenSlashEquals:
		case TokenStarEquals:

		case TokenGreaterThan:
		case TokenLessThan:

		case TokenGEQ:
		case TokenLEQ:
			return leftNumeric && rightNumeric;
		case TokenPercent:
		case TokenPercentEquals:
			return (left.tag <= ValueInt) && (right.tag <= ValueInt);
		case TokenAnd:
		case TokenOr:
			return (left.tag == ValueBool) && (right.tag == ValueBool);
		default: // TokenEqualsEquals, TokenBangEquals
			return true;
	}
}

static bool alwaysReturns(CarbonStmt *stmt) {
	switch (stmt->type) {
		case StmtReturn:
			return true;
		case StmtIf: {
			CarbonStmtIf *sif = (CarbonStmtIf *) stmt;
			if (sif->then == NULL)
				return true;
			if (sif->notThen == NULL)
				return false;
			return alwaysReturns(sif->then) && alwaysReturns(sif->notThen);
		}
		case StmtBlock: {
			CarbonStmtBlock *blk = (CarbonStmtBlock *) stmt;
			for (uint32_t i = 0; i < blk->statements.count; i++) {
				if (alwaysReturns(blk->statements.arr[i]))
					return true;
			}
			return false;
		}
		default:
			return false;
	}
}

// ERRORS

static void noInit(CarbonToken t, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Enclosing class has no initializer method\n",
			t.line);
	c->hadError = true;
}

static void noSuperclass(CarbonToken t, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Enclosing class has no superclass\n", t.line);
	c->hadError = true;
}
static void superOutsideMethod(CarbonToken t, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] 'super' cannot be used outside class methods\n",
			t.line);
	c->hadError = true;
}
static void noFunctionInSuperclass(CarbonToken t, CarbonCompiler *c) {
	fprintf(stderr,
			"[Line %u] Method %.*s has not been defined on superclass\n",
			t.line, t.length, t.lexeme);
	c->hadError = true;
}

static void undefinedSuperclass(CarbonToken name, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Superclass %.*s has not been defined\n",
			name.line, name.length, name.lexeme);
	c->hadError = true;
}

static void wrongSignature(CarbonToken name, CarbonCompiler *c) {
	fprintf(stderr,
			"[Line %u] Method %.*s's signature doesn't match the superclass "
			"definition\n",
			name.line, name.length, name.lexeme);
	c->hadError = true;
}

static void selfAssignment(CarbonToken self, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] 'self' cannot be assigned to\n", self.line);
	c->hadError = true;
}

static void cantDotAssign(CarbonValueType type, CarbonToken equals,
						  CarbonCompiler *c) {
	if (type.tag == ValueUnresolved)
		return;
	fprintf(stderr, "[Line %u] Type ", equals.line);
	carbon_printType(stderr, type);
	fprintf(stderr, " does not support member assignment\n");
	c->hadError = true;
}

static void classNotFound(CarbonToken name, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Class %.*s is not defined\n", name.line,
			name.length, name.lexeme);
	c->hadError = true;
}

static void invalidInitializer(CarbonToken funcName, CarbonCompiler *c) {
	fprintf(stderr,
			"[Line %u] Initializer functions must have a return type of void\n",
			funcName.line);
	c->hadError = true;
}

static void memberRedeclaration(CarbonToken class, CarbonToken member,
								CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Redefinition of member '%.*s' in %.*s\n",
			member.line, member.length, member.lexeme, class.length,
			class.lexeme);
	c->hadError = true;
}

static void notObjectType(CarbonTypename type, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Type %.*s is not an object type\n",
			type.base.line, type.base.length, type.base.lexeme);
	c->hadError = true;
}

static void memberNotFound(CarbonValueType type, CarbonToken property,
						   CarbonCompiler *c) {
	if (type.tag == ValueUnresolved)
		return;
	fprintf(stderr, "[Line %u] Type ", property.line);
	carbon_printType(stderr, type);
	fprintf(stderr, " has no member '%.*s'\n", property.length,
			property.lexeme);
	c->hadError = true;
}

static void needUintLength(CarbonToken t, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Array length must be a uint\n", t.line);
	c->hadError = true;
}

static void cantIndexAssign(CarbonValueType type, CarbonToken token,
							CarbonCompiler *c) {
	if (type.tag == ValueUnresolved)
		return;
	fprintf(stderr, "[Line %u] Type ", token.line);
	carbon_printType(stderr, type);
	fprintf(stderr, " does not support index assignment\n");
	c->hadError = true;
}

static void unindexableType(CarbonToken token, CarbonValueType type,
							CarbonCompiler *c) {
	if (type.tag == ValueUnresolved)
		return;
	fprintf(stderr, "[Line %u] Type ", token.line);
	carbon_printType(stderr, type);
	fprintf(stderr, " is not indexable\n");
	c->hadError = true;
}

static void wrongIndexType(CarbonToken bracket, CarbonValueType given,
						   CarbonValueType wanted, CarbonCompiler *c) {
	if (given.tag == ValueUnresolved || wanted.tag == ValueUnresolved)
		return;
	fprintf(stderr, "[Line %u] Index has wrong type: wanted ", bracket.line);
	carbon_printType(stderr, wanted);
	fprintf(stderr, ", given ");
	carbon_printType(stderr, given);
	fprintf(stderr, "\n");
	c->hadError = true;
}

static void wrongGeneratorType(CarbonToken t, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Generators support only numeric types\n",
			t.length);
	c->hadError = true;
}

static void wrongMemberType(CarbonToken tok, uint64_t i, CarbonValueType wanted,
							CarbonValueType given, CarbonCompiler *c) {
	if (given.tag == ValueUnresolved || wanted.tag == ValueUnresolved)
		return;
	fprintf(stderr, "[Line %u] Type of array member %" PRIu64 " (", tok.line,
			i);
	carbon_printType(stderr, given);
	fprintf(stderr, ") does not match the type of the first member (");
	carbon_printType(stderr, wanted);
	fprintf(stderr, ").\n");
	c->hadError = true;
}

static void wrongTemplatecount(CarbonToken tok, uint8_t wanted, uint8_t given,
							   bool atLeast, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Type %.*s requires %s%u templates. Given %u\n",
			tok.line, tok.length, tok.lexeme, atLeast ? "at least " : "",
			wanted, given);
	c->hadError = true;
}

static void jumpTooLong(CarbonToken tok, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Too many lines to jump over.\n", tok.line);
	c->hadError = true;
}

static void cantPrint(CarbonToken tok, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Cannot print void values.\n", tok.line);
	c->hadError = true;
}

static void expectedReturnStatement(CarbonToken func, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Function '%.*s' misses a return statement.\n",
			func.line, func.length, func.lexeme);
	c->hadError = true;
}

static void noReturnWanted(CarbonToken tok, CarbonString *functionName,
						   CarbonCompiler *c) {
	fprintf(stderr,
			"[Line %u] The function %s has a return type of void, it should "
			"have no return statements.\n",
			tok.line, functionName->chars);
	c->hadError = true;
}

static void wrongReturnType(CarbonToken tok, CarbonValueType wanted,
							CarbonValueType given, CarbonCompiler *c) {
	if (wanted.tag == ValueUnresolved || given.tag == ValueUnresolved)
		return;

	fprintf(stderr, "[Line %u] Wrong return type: function needs to return ",
			tok.line);
	carbon_printType(stderr, wanted);
	fprintf(stderr, ", not ");
	carbon_printType(stderr, given);
	fprintf(stderr, "\n");
	c->hadError = true;
}

static void invalidCallee(uint32_t line, CarbonExpr *expr, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Type ", line);
	carbon_printType(stderr, expr->evalsTo);
	fprintf(stderr, " is not callable\n");
	c->hadError = true;
}

static void wrongArity(CarbonToken func, uint32_t wanted, uint32_t given,
					   CarbonCompiler *c) {
	fprintf(stderr,
			"[Line %u] Wrong number of arguments while calling function %.*s: "
			"Expected %u, got %u.\n",
			func.line, func.length, func.lexeme, wanted, given);
	c->hadError = true;
}
static void wrongArgumentType(CarbonToken argument, uint8_t n,
							  CarbonValueType wanted, CarbonValueType given,
							  CarbonToken name, CarbonCompiler *c) {
	if (given.tag == ValueUnresolved)
		return;

	fprintf(stderr,
			"[Line %u] Argument %u of function call %.*s is of the wrong "
			"type: Expected ",
			argument.line, n, name.length, name.lexeme);
	carbon_printType(stderr, wanted);
	fprintf(stderr, ", got ");
	carbon_printType(stderr, given);
	fprintf(stderr, "\n");
	c->hadError = true;
}

static void tooManyLocals(CarbonToken name, CarbonCompiler *c) {
	fprintf(stderr,
			"[Line %u] Too many locals: Not enough stack space for '%.*s'.\n",
			name.line, name.length, name.lexeme);
	c->hadError = true;
}

static void localRedeclaration(CarbonToken name, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Redeclaration of local variale '%.*s'.\n",
			name.line, name.length, name.lexeme);
	c->hadError = true;
}

static void globalRedeclaration(CarbonToken name, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Global %.*s has already been declared.\n",
			name.line, name.length, name.lexeme);
	c->hadError = true;
}

static void cantAssign(CarbonValueType to, CarbonValueType from, uint32_t line,
					   CarbonCompiler *c) {

	if (from.tag == ValueUnresolved || to.tag == ValueUnresolved)
		return;
	fprintf(stderr, "[Line %u] Cannot assign type ", line);
	carbon_printType(stderr, from);
	fprintf(stderr, " to ");
	carbon_printType(stderr, to);
	fprintf(stderr, "\n");
	c->hadError = true;
}

static void unaryOpNotSupported(CarbonToken op, CarbonValueType type,
								CarbonCompiler *c) {
	if (type.tag == ValueUnresolved)
		return;
	fprintf(stderr, "[Line %u] Operator '%.*s' not supported for operand type ",
			op.line, op.length, op.lexeme);
	carbon_printType(stderr, type);
	fprintf(stderr, "\n");
	c->hadError = true;
}

static void binaryOpNotSupported(CarbonToken op, CarbonValueType left,
								 CarbonValueType right, CarbonCompiler *c) {
	if (left.tag == ValueUnresolved || right.tag == ValueUnresolved)
		return;
	fprintf(stderr,
			"[Line %u] Operator '%.*s' not supported for operand types ",
			op.line, op.length, op.lexeme);
	carbon_printType(stderr, left);
	fprintf(stderr, " and ");
	carbon_printType(stderr, right);
	fprintf(stderr, "\n");
	c->hadError = true;
}
static void castNotSupported(CarbonValueType from, CarbonValueType to,
							 CarbonToken tok, CarbonCompiler *c) {
	if (to.tag == ValueUnresolved || from.tag == ValueUnresolved)
		return;
	fprintf(stderr, "[Line %u] Cannot cast from type ", tok.line);
	carbon_printType(stderr, from);
	fprintf(stderr, " to ");
	carbon_printType(stderr, to);
	fprintf(stderr, "\n");
	c->hadError = true;
}

static void globalNotFound(CarbonToken token, CarbonCompiler *c) {
	fprintf(stderr, "[Line %u] Could not resolve global %.*s\n", token.line,
			token.length, token.lexeme);
	c->hadError = true;
}

static inline bool idntfLexCmp(CarbonToken tok, char *lex, uint16_t length) {
	return tok.length == length && !memcmp(tok.lexeme, lex, tok.length);
}

static uint32_t emitIf(CarbonChunk *chunk, uint32_t line) {
	uint32_t position = chunk->count;
	carbon_writeToChunk(chunk, OpIf, line);
	carbon_writeToChunk(chunk, 0, line);
	carbon_writeToChunk(chunk, 0, line);
	return position;
}

static uint32_t emitJump(CarbonChunk *chunk, uint32_t line) {
	uint32_t position = chunk->count;
	carbon_writeToChunk(chunk, OpJump, line);
	carbon_writeToChunk(chunk, 0, line);
	carbon_writeToChunk(chunk, 0, line);
	return position;
}

static uint32_t emitLogicalJump(CarbonChunk *chunk, uint32_t line,
								CarbonOpCode opc) {
	uint32_t position = chunk->count;
	carbon_writeToChunk(chunk, opc, line);
	carbon_writeToChunk(chunk, 0, line);
	carbon_writeToChunk(chunk, 0, line);
	return position;
}

static void patchJump(CarbonChunk *chunk, uint32_t position, CarbonToken t,
					  CarbonCompiler *c) {
	uint32_t offset = chunk->count - position - 2;
	if (offset > UINT16_MAX) {
		jumpTooLong(t, c);
		return;
	}
	chunk->code[position + 1] = offset >> 8;
	chunk->code[position + 2] = offset;
}

static CarbonValueType resolveType(CarbonTypename tn, CarbonCompiler *c,
								   CarbonVM *vm) {
	CarbonValueType typ = newType(t2value[tn.base.type]);

	if (typ.tag == ValueArray) {
		if (tn.templateCount != 1) {
			wrongTemplatecount(tn.base, 1, tn.templateCount, false, c);
			typ.compound.signature = NULL;
			return typ;
		}
		typ.compound.memberType =
			carbon_reallocate(0, sizeof(CarbonValueType), NULL);
		*typ.compound.memberType = resolveType(tn.templates[0], c, vm);
	} else if (typ.tag == ValueGenerator) {
		if (tn.templateCount != 1) {
			wrongTemplatecount(tn.base, 1, tn.templateCount, false, c);
			typ.compound.signature = NULL;
			return typ;
		}
		typ.compound.memberType =
			carbon_reallocate(0, sizeof(CarbonValueType), NULL);
		*typ.compound.memberType = resolveType(tn.templates[0], c, vm);
	} else if (typ.tag == ValueInstance) {
		CarbonString *name = carbon_strFromToken(tn.base, vm);
		typ.compound.instanceName = name;
		CarbonValue dummy;
		if (!carbon_tableGet(&c->classes, (CarbonObj *) name, &dummy))
			classNotFound(tn.base, c);
	} else if (typ.tag == ValueFunction) {
		if (tn.templateCount < 1) {
			wrongTemplatecount(tn.base, 1, tn.templateCount, true, c);
			typ.compound.signature = NULL;
			return typ;
		}
		typ.compound.signature =
			carbon_reallocate(0, sizeof(CarbonFunctionSignature), NULL);
		uint8_t arity = typ.compound.signature->arity = tn.templateCount - 1;
		typ.compound.signature->returnType =
			carbon_reallocate(0, sizeof(CarbonValueType), NULL);
		*typ.compound.signature->returnType =
			resolveType(tn.templates[0], c, vm);
		if (arity > 0) {
			typ.compound.signature->arguments =
				carbon_reallocate(0, arity * sizeof(CarbonValueType), NULL);
			for (uint8_t i = 1; i < tn.templateCount; i++) {
				typ.compound.signature->arguments[i - 1] =
					resolveType(tn.templates[i], c, vm);
			}
		}
	} else if (tn.templateCount != 0) {
		wrongTemplatecount(tn.base, 0, tn.templateCount, false, c);
		typ.compound.signature = NULL;
	}
	return typ;
}

static void markVar(CarbonStmtVarDec *vardec, CarbonCompiler *c, CarbonVM *vm) {
	CarbonString *name = carbon_strFromToken(vardec->identifier, vm);
	CarbonValue dummy;
	if (carbon_tableGet(&c->globals, (CarbonObj *) name, &dummy)) {
		globalRedeclaration(vardec->identifier, c);
		return;
	}
	Global *var = newGlobal(resolveType(vardec->type, c, vm));
	carbon_tableSet(&c->globals, (CarbonObj *) name,
					CarbonObject((CarbonObj *) var));
}

static void markFunc(CarbonStmtFunc *func, CarbonCompiler *c, CarbonVM *vm) {
	CarbonValue dummy;
	CarbonString *name = carbon_strFromToken(func->identifier, vm);
	if (carbon_tableGet(&c->globals, (CarbonObj *) name, &dummy)) {
		globalRedeclaration(func->identifier, c);
		return;
	}

	CarbonValueType type = newType(ValueFunction);
	type.compound.signature =
		carbon_reallocate(0, sizeof(CarbonFunctionSignature), NULL);

	type.compound.signature->arity = func->arity;

	type.compound.signature->returnType =
		carbon_reallocate(0, sizeof(CarbonValueType), NULL);

	*type.compound.signature->returnType = resolveType(func->returnType, c, vm);

	type.compound.signature->arguments =
		carbon_reallocate(0, func->arity * sizeof(CarbonValueType), NULL);

	for (uint32_t i = 0; i < func->arity; i++) {
		struct carbon_arg argument = func->arguments[i];
		CarbonValueType atype = resolveType(argument.type, c, vm);
		type.compound.signature->arguments[i] = atype;
	}

	Global *g = newGlobal(type);
	carbon_tableSet(&c->globals, (CarbonObj *) name,
					CarbonObject((CarbonObj *) g));
}

static void markClass(CarbonStmtClass *sClass, CarbonCompiler *c,
					  CarbonVM *vm) {
	Class *class;
	Class *super = NULL;
	CarbonString *name = carbon_strFromToken(sClass->name, vm);
	carbon_tableGet(&c->classes, (CarbonObj *) name, (CarbonValue *) &class);
	class->declared = true;

	if (sClass->hasSuperclass) {
		if (carbon_tableGet(
				&c->classes,
				(CarbonObj *) carbon_strFromToken(sClass->superclass, vm),
				(CarbonValue *) &super)) {
			for (uint8_t i = 0; i < super->fieldCount; i++) {
				pushField(super->fields[i].name,
						  carbon_cloneType(super->fields[i].type), class);
			}
			for (uint8_t i = 0; i < super->methodCount; i++) {
				CarbonFunctionSignature src = super->methods[i].sig;
				CarbonFunctionSignature sig;
				sig.arity = src.arity;
				sig.returnType =
					carbon_reallocate(0, sizeof(CarbonValueType), NULL);
				*sig.returnType = carbon_cloneType(*src.returnType);

				sig.arguments = carbon_reallocate(
					0, sig.arity * sizeof(CarbonValueType), NULL);
				for (uint8_t i = 0; i < sig.arity; i++) {
					sig.arguments[i] = carbon_cloneType(src.arguments[i]);
				}
				pushMethod(super->methods[i].name, sig, class);
			}
			class->parent = super;
		}
	}

	uint8_t ovrdSize = class->parent != NULL && class->parent->methodCount > 0
						   ? class->parent->methodCount
						   : 1;
	bool overridden[ovrdSize];
	memset(overridden, 0, ovrdSize);

	for (uint32_t i = 0; i < sClass->statements.count; i++) {
		CarbonStmt *st = sClass->statements.arr[i];
		if (st == NULL)
			return;
		if (st->type == StmtVarDec) {
			CarbonStmtVarDec *vardec = (CarbonStmtVarDec *) st;
			CarbonString *name = carbon_strFromToken(vardec->identifier, vm);
			CarbonValueType type = resolveType(vardec->type, c, vm);
			if (findMember(class, name)) {
				memberRedeclaration(sClass->name, vardec->identifier, c);
				carbon_freeType(type);
				continue;
			}
			pushField(name, type, class);
		} else {
			CarbonStmtFunc *func = (CarbonStmtFunc *) st;
			CarbonString *name = carbon_strFromToken(func->identifier, vm);
			int16_t override = 0;
			if (super != NULL) {
				override = findMember(super, name);
			}
			if (findMember(class, name)) {
				if (override >= 0 || overridden[-override - 1]) {
					memberRedeclaration(sClass->name, func->identifier, c);
					continue;
				} else
					overridden[-override - 1] = true;
			}

			CarbonFunctionSignature sig;
			sig.arity = func->arity;

			sig.returnType =
				carbon_reallocate(0, sizeof(CarbonValueType), NULL);

			*sig.returnType = resolveType(func->returnType, c, vm);

			sig.arguments = carbon_reallocate(
				0, func->arity * sizeof(CarbonValueType), NULL);

			for (uint32_t i = 0; i < func->arity; i++) {
				struct carbon_arg argument = func->arguments[i];
				sig.arguments[i] = resolveType(argument.type, c, vm);
			}

			if (override < 0) {
				CarbonValueType t1 = newType(ValueFunction);
				t1.compound.signature = &sig;
				CarbonValueType t2 = newType(ValueFunction);
				t2.compound.signature = &class->methods[-override - 1].sig;
				if (!carbon_typesEqual(t1, t2))
					wrongSignature(func->identifier, c);
				carbon_freeSignature(sig);
				continue;
			}

			if (name->length == sClass->name.length - 1 &&
				!memcmp(name->chars, sClass->name.lexeme + 1, name->length)) {
				if (sig.returnType->tag != ValueVoid)
					invalidInitializer(func->identifier, c);
				class->init = class->methodCount;
				class->hasInit = true;
			}

			pushMethod(name, sig, class);
		}
	}
	sealClass(class);
}

void carbon_scoutClass(CarbonStmt *stmt, CarbonCompiler *c, CarbonVM *vm) {
	if (stmt == NULL || stmt->type != StmtClass)
		return;
	CarbonStmtClass *sClass = (CarbonStmtClass *) stmt;
	CarbonValue dummy;
	CarbonString *name = carbon_strFromToken(sClass->name, vm);
	if (carbon_tableGet(&c->classes, (CarbonObj *) name, &dummy)) {
		globalRedeclaration(sClass->name, c);
		return;
	}
	if (sClass->hasSuperclass) {
		CarbonString *supername = carbon_strFromToken(sClass->superclass, vm);
		if (!carbon_tableGet(&c->classes, (CarbonObj *) supername, &dummy))
			undefinedSuperclass(sClass->superclass, c);
	}
	Class *class = newClass(c->classCount++);
	carbon_tableSet(&c->classes, (CarbonObj *) name,
					CarbonObject((CarbonObj *) class));
}

void carbon_markGlobal(CarbonStmt *stmt, CarbonCompiler *c, CarbonVM *vm) {
	if (stmt == NULL)
		return;
	switch (stmt->type) {
		case StmtVarDec: {
			markVar((CarbonStmtVarDec *) stmt, c, vm);
			break;
		}
		case StmtFunc: {
			markFunc((CarbonStmtFunc *) stmt, c, vm);
			break;
		}
		case StmtClass: {
			markClass((CarbonStmtClass *) stmt, c, vm);
			break;
		}
		default:
			break;
	}
}

static void typecheck(CarbonExpr *expr, CarbonCompiler *c, CarbonVM *vm) {

#define castNode(type, name) type *name = (type *) expr;

	if (expr->evalsTo.tag != ValueUntypechecked)
		return;

	switch (expr->type) {
		case ExprUnary: {
			castNode(CarbonExprUnary, un);
			if (un->operand == NULL) {
				expr->evalsTo = newType(ValueUnresolved);
				return;
			}
			typecheck(un->operand, c, vm);
			CarbonValueType operandType = un->operand->evalsTo;
			if (operandType.tag == ValueUnresolved) {
				expr->evalsTo = newType(ValueUnresolved);
				return;
			}
			if (!canUnary(un->op.type, operandType)) {
				unaryOpNotSupported(un->op, operandType, c);
				expr->evalsTo = newType(ValueUnresolved);
			}
			switch (operandType.tag) {
				case ValueBool:
				case ValueInt:
				case ValueDouble:
					expr->evalsTo = carbon_cloneType(operandType);
					return;
				case ValueUInt:
					expr->evalsTo = newType(ValueInt);
					return;
				default:
					expr->evalsTo = newType(ValueUnresolved);
					return;
			}
		}
		case ExprBinary: {
			castNode(CarbonExprBinary, bin);
			if (bin->left == NULL || bin->right == NULL) {
				expr->evalsTo = newType(ValueUnresolved);
				return;
			}
			typecheck(bin->left, c, vm);
			typecheck(bin->right, c, vm);
			CarbonValueType leftType = bin->left->evalsTo;
			CarbonValueType rightType = bin->right->evalsTo;

			if (leftType.tag == ValueUnresolved ||
				rightType.tag == ValueUnresolved) {
				expr->evalsTo = newType(ValueUnresolved);
				return;
			}

			if (!canBinary(bin->op.type, leftType, rightType)) {
				binaryOpNotSupported(bin->op, leftType, rightType, c);
				expr->evalsTo = newType(ValueUnresolved);
				return;
			}

			CarbonValueType higherType = leftType;
			if (leftType.tag <= ValueDouble && rightType.tag <= ValueDouble) {
				if (rightType.tag > leftType.tag) {
					higherType = rightType;
					CarbonTypename tn = {.base = v2token[higherType.tag],
										 .templates = NULL,
										 .templateCount = 0};
					bin->left =
						(CarbonExpr *) carbon_newCastExpr(tn, bin->left);
					bin->left->evalsTo = higherType;
				} else if (leftType.tag > rightType.tag) {
					CarbonTypename tn = {.base = v2token[higherType.tag],
										 .templates = NULL,
										 .templateCount = 0};
					bin->right =
						(CarbonExpr *) carbon_newCastExpr(tn, bin->right);
					bin->right->evalsTo = higherType;
				}
			}

			switch (bin->op.type) {
				case TokenMinus:
					if (higherType.tag == ValueUInt) {
						expr->evalsTo = newType(ValueInt);
						return;
					}
				case TokenPlus:
				case TokenSlash:
				case TokenPercent:
				case TokenMinusEquals:
				case TokenPlusEquals:
				case TokenSlashEquals:
				case TokenPercentEquals:
				case TokenStarEquals:
				case TokenStar: {
					expr->evalsTo = carbon_cloneType(higherType);
					return;
				}
				case TokenLessThan:
				case TokenGreaterThan:
				case TokenLEQ:
				case TokenGEQ:
				case TokenAnd:
				case TokenOr:
				case TokenEqualsEquals:
				case TokenBangEquals:
					expr->evalsTo = newType(ValueBool);
					return;
				default:
					expr->evalsTo.tag =
						ValueUnresolved; // Reaching here is a bug
					return;
			}
		}
		case ExprLiteral: {
			castNode(CarbonExprLiteral, lit);
			switch (lit->token.type) {
				case TokenTrue:
				case TokenFalse:
					expr->evalsTo = newType(ValueBool);
					return;
				case TokenInteger:
					expr->evalsTo = newType(ValueUInt);
					return;
				case TokenDecimal:
					expr->evalsTo = newType(ValueDouble);
					return;
				case TokenNull:
					expr->evalsTo = newType(ValueNull);
					return;
				case TokenStringLiteral:
					expr->evalsTo = newType(ValueString);
					return;
				default:
					return; // Should never reach here
			}
			return;
		}
		case ExprGrouping: {
			castNode(CarbonExprGrouping, group);
			if (group->expression == NULL) {
				expr->evalsTo = newType(ValueUnresolved);
				return;
			}
			typecheck(group->expression, c, vm);
			expr->evalsTo = carbon_cloneType(group->expression->evalsTo);
			return;
		}
		case ExprCast: {
			CarbonExprCast *cast = (CarbonExprCast *) expr;
			if (cast->expression == NULL) {
				expr->evalsTo = newType(ValueUnresolved);
				return;
			}
			typecheck(cast->expression, c, vm);
			if (cast->expression->evalsTo.tag == ValueUnresolved) {
				expr->evalsTo = newType(ValueUnresolved);
				return;
			}

			CarbonValueType type = resolveType(cast->to, c, vm);

			if (!canCast(cast->expression->evalsTo, type, c)) {
				castNotSupported(cast->expression->evalsTo, type, cast->to.base,
								 c);
				expr->evalsTo = newType(ValueUnresolved);
				carbon_freeType(type);
				return;
			}
			expr->evalsTo = type;
			return;
		}
		case ExprVar: {
			castNode(CarbonExprVar, var);
			CarbonString *name = carbon_strFromToken(var->token, vm);

			CarbonValueType l = resolveLocalType(name, c);
			if (l.tag != ValueVoid) {
				expr->evalsTo = carbon_cloneType(l);
				return;
			}

			Global *out;
			if (carbon_tableGet(&c->globals, (CarbonObj *) name,
								(CarbonValue *) &out))
				if (out->declared || c->compilingTo != NULL) {
					expr->evalsTo = carbon_cloneType(out->valueType);
					return;
				}
			globalNotFound(var->token, c);
			expr->evalsTo = newType(ValueUnresolved);
			return;
		}
		case ExprAssignment: {
			castNode(CarbonExprAssignment, assignment);
			if (assignment->right == NULL) {
				expr->evalsTo = newType(ValueUnresolved);
				return;
			}

			typecheck(assignment->right, c, vm);

			CarbonString *name = carbon_strFromToken(assignment->left, vm);
			CarbonValueType leftType;

			CarbonValueType l = resolveLocalType(name, c);
			bool found = false;
			if (l.tag != ValueVoid) {
				leftType = l;
				found = true;
			} else {
				Global *out;
				if (carbon_tableGet(&c->globals, (CarbonObj *) name,
									(CarbonValue *) &out)) {
					leftType = out->valueType;
					// if we are in a function, then the global variable has
					// already been inited
					found = out->declared || c->compilingTo != NULL;
				} else
					found = false;
			}
			if (found) {

				if (c->class != NULL) {
					if (assignment->left.length == 4 &&
						!memcmp(assignment->left.lexeme, "self", 4)) {
						selfAssignment(assignment->left, c);
					}
				}

				if (!carbon_canAssign(leftType, assignment->right->evalsTo,
									  c)) {
					if (assignment->equals.type == TokenEquals)
						cantAssign(leftType, assignment->right->evalsTo,
								   assignment->left.line, c);
					else
						binaryOpNotSupported(assignment->equals, leftType,
											 assignment->right->evalsTo, c);
					expr->evalsTo = newType(ValueUnresolved);
					return;
				}

				assignment->right =
					promoteNumerics(leftType, assignment->right);

				expr->evalsTo = carbon_cloneType(leftType);
				return;
			}

			globalNotFound(assignment->left, c);
			expr->evalsTo = newType(ValueUnresolved);

			return;
		}
		case ExprIndexAssignment: {
			castNode(CarbonExprIndexAssignment, ie);
			typecheck((CarbonExpr *) ie->left, c, vm);

			if (ie->right == NULL)
				return;
			typecheck(ie->right, c, vm);

			CarbonValueType leftType = ie->left->expr.evalsTo;
			CarbonValueType rightType = ie->right->evalsTo;

			if (ie->left->object->evalsTo.tag != ValueArray) {
				cantIndexAssign(leftType, ie->equals, c);
				expr->evalsTo = newType(ValueUnresolved);
				return;
			}

			if (!carbon_canAssign(leftType, rightType, c)) {
				if (ie->equals.type == TokenEquals)
					cantAssign(leftType, rightType, ie->equals.line, c);
				else
					binaryOpNotSupported(ie->equals, leftType,
										 ie->right->evalsTo, c);
				expr->evalsTo = newType(ValueUnresolved);
				return;
			}

			ie->right = promoteNumerics(leftType, ie->right);
			expr->evalsTo = carbon_cloneType(leftType);
			return;
		}
		case ExprCall: {
			castNode(CarbonExprCall, call);
			if (call->callee == NULL)
				return;

			if (call->callee->type == ExprLiteral) {
				CarbonExprLiteral *lit = (CarbonExprLiteral *) call->callee;
				if (lit->token.type == TokenSuper) {
					if (c->class == NULL) {
						superOutsideMethod(lit->token, c);
						expr->evalsTo = newType(ValueUnresolved);
						break;
					}
					Class *class = c->class;
					if (class->parent == NULL) {
						noSuperclass(lit->token, c);
						expr->evalsTo = newType(ValueUnresolved);
						break;
					}
					if (!class->parent->hasInit) {
						noInit(lit->token, c);
						expr->evalsTo = newType(ValueUnresolved);
						break;
					}
					CarbonFunctionSignature *sig =
						&class->parent->methods[class->parent->init].sig;
					expr->evalsTo = carbon_cloneType(*sig->returnType);
					break;
				}
			}

			typecheck(call->callee, c, vm);
			if (call->callee->evalsTo.tag != ValueFunction &&
				call->callee->evalsTo.tag != ValueUnresolved) {
				invalidCallee(call->line, call->callee, c);
				break;
			}

			CarbonFunctionSignature *sig =
				call->callee->evalsTo.compound.signature;

			if (sig == NULL) {
				expr->evalsTo = newType(ValueUnresolved);
				return;
			}

			// Check if we can retrieve name from function call
			// If so, print it out in the error message

			CarbonToken name = {.type = TokenNone};

			if (call->callee->type == ExprVar) {
				name = ((CarbonExprVar *) call->callee)->token;
			}

			if (sig->arity != call->arity) {
				wrongArity(name, sig->arity, call->arity, c);
			}

			uint8_t lower = sig->arity < call->arity ? sig->arity : call->arity;

			for (uint8_t i = 0; i < lower; i++) {
				if (call->arguments[i] == NULL)
					continue;
				typecheck(call->arguments[i], c, vm);
				if (!carbon_canAssign(sig->arguments[i],
									  call->arguments[i]->evalsTo, c)) {
					wrongArgumentType(call->arguments[i]->first, i,
									  sig->arguments[i],
									  call->arguments[i]->evalsTo, name, c);
				}
				call->arguments[i] =
					promoteNumerics(sig->arguments[i], call->arguments[i]);
			}
			expr->evalsTo = carbon_cloneType(*sig->returnType);
			break;
		}
		case ExprInit: {
			castNode(CarbonExprCall, init);
			CarbonExprLiteral *lit = (CarbonExprLiteral *) init->callee;
			CarbonToken name = lit->token;
			CarbonString *strname = carbon_strFromToken(name, vm);
			Class *class;

			if (!carbon_tableGet(&c->classes, (CarbonObj *) strname,
								 (CarbonValue *) &class)) {
				expr->evalsTo = newType(ValueUnresolved);
				classNotFound(name, c);
				break;
			}
			CarbonFunctionSignature *sig;
			if (class->hasInit) {
				sig = &class->methods[class->init].sig;
			} else {
				sig =
					carbon_reallocate(0, sizeof(CarbonFunctionSignature), NULL);
				sig->arguments = NULL;
				sig->arity = 0;
				sig->returnType =
					carbon_reallocate(0, sizeof(CarbonValueType), NULL);
				*sig->returnType = newType(ValueInstance);
				sig->returnType->compound.instanceName = strname;
			}

			if (sig->arity != init->arity) {
				wrongArity(name, sig->arity, init->arity, c);
			}

			uint8_t lower = sig->arity < init->arity ? sig->arity : init->arity;

			for (uint8_t i = 0; i < lower; i++) {
				if (init->arguments[i] == NULL)
					continue;
				typecheck(init->arguments[i], c, vm);
				if (!carbon_canAssign(sig->arguments[i],
									  init->arguments[i]->evalsTo, c)) {
					wrongArgumentType(init->arguments[i]->first, i,
									  sig->arguments[i],
									  init->arguments[i]->evalsTo, name, c);
				}
				init->arguments[i] =
					promoteNumerics(sig->arguments[i], init->arguments[i]);
			}

			expr->evalsTo = newType(ValueInstance);
			expr->evalsTo.compound.instanceName = strname;
			if (!class->hasInit) {
				carbon_freeSignature(*sig);
				carbon_reallocate(sizeof(CarbonFunctionSignature), 0, sig);
			}
			break;
		}
		case ExprArray: {
			castNode(CarbonExprArray, arr);
			expr->evalsTo = newType(
				arr->imethod == ImethodGenerator ? ValueGenerator : ValueArray);
			if (arr->imethod == ImethodContracted) {

				expr->evalsTo.compound.memberType =
					carbon_reallocate(0, sizeof(CarbonValueType), NULL);
				*expr->evalsTo.compound.memberType =
					resolveType(arr->type, c, vm);

				if (arr->members[0] != NULL)
					typecheck(arr->members[0], c, vm);
				if (arr->members[1] != NULL)
					typecheck(arr->members[1], c, vm);

				if (arr->members[0]->evalsTo.tag != ValueUInt)
					needUintLength(arr->members[0]->first, c);

				if (!carbon_canAssign(*expr->evalsTo.compound.memberType,
									  arr->members[1]->evalsTo, c))
					cantAssign(expr->evalsTo, arr->members[1]->evalsTo,
							   arr->members[1]->first.line, c);

				arr->members[1] = promoteNumerics(
					*expr->evalsTo.compound.memberType, arr->members[1]);

				break;
			}
			CarbonValueType highestType = newType(ValueUInt);
			CarbonValueType firstType;
			for (uint64_t i = 0; i < arr->count; i++) {
				if (arr->members[i] != NULL) {
					typecheck(arr->members[i], c, vm);
					if (arr->members[i]->evalsTo.tag > highestType.tag) {
						highestType = arr->members[i]->evalsTo;
						if (arr->imethod == ImethodGenerator &&
							highestType.tag > ValueDouble &&
							highestType.tag != ValueUnresolved) {
							wrongGeneratorType(arr->members[i]->first, c);
						}
					}
				}
			}
			if (highestType.tag <= ValueDouble) {
				for (uint64_t i = 0; i < arr->count; i++) {
					if (arr->members[i] != NULL)
						arr->members[i] =
							promoteNumerics(highestType, arr->members[i]);
				}
				expr->evalsTo.compound.memberType =
					carbon_reallocate(0, sizeof(CarbonValueType), NULL);
				*expr->evalsTo.compound.memberType =
					carbon_cloneType(highestType);
				break;
			}
			firstType = arr->members[0]->evalsTo;
			for (uint64_t i = 0; i < arr->count; i++) {
				if (!carbon_canAssign(firstType, arr->members[i]->evalsTo, c) &&
					arr->imethod != ImethodGenerator) {
					wrongMemberType(arr->members[i]->first, i, firstType,
									arr->members[i]->evalsTo, c);
				}
			}
			expr->evalsTo.compound.memberType =
				carbon_reallocate(0, sizeof(CarbonValueType), NULL);
			*expr->evalsTo.compound.memberType = carbon_cloneType(firstType);
			break;
		}
		case ExprIndex: {
			castNode(CarbonExprIndex, index);
			if (index->object == NULL) {
				expr->evalsTo = newType(ValueUnresolved);
				return;
			}
			typecheck(index->object, c, vm);

			if (index->index != NULL)
				typecheck(index->index, c, vm);
			CarbonValueType requiredType;
			switch (index->object->evalsTo.tag) {
				case ValueArray:
				case ValueGenerator:
				case ValueString:
					if (index->object->evalsTo.tag == ValueString)
						expr->evalsTo = newType(ValueString);
					else
						expr->evalsTo = carbon_cloneType(
							*index->object->evalsTo.compound.memberType);
					requiredType = newType(ValueInt);
					break;
				default:
					unindexableType(index->bracket, index->object->evalsTo, c);
					expr->evalsTo = newType(ValueUnresolved);
					return;
			}
			if (index->index != NULL)
				if (!carbon_canAssign(requiredType, index->index->evalsTo, c)) {
					wrongIndexType(index->bracket, index->index->evalsTo,
								   requiredType, c);
					break;
				}
			break;
		}
		case ExprDot: {
			castNode(CarbonExprDot, dot);
			if (dot->left == NULL) {
				expr->evalsTo = newType(ValueUnresolved);
				break;
			}
			if (dot->left->type == ExprLiteral) {
				CarbonExprLiteral *lit = (CarbonExprLiteral *) dot->left;
				if (lit->token.type == TokenSuper) {
					if (c->class == NULL) {
						superOutsideMethod(lit->token, c);
						expr->evalsTo = newType(ValueUnresolved);
						break;
					}
					Class *class = c->class;
					if (class->parent == NULL) {
						noSuperclass(lit->token, c);
						expr->evalsTo = newType(ValueUnresolved);
						break;
					}
					CarbonString *name = carbon_strFromToken(dot->right, vm);
					int16_t i = findMember(class->parent, name);
					if (i >= 0) {
						noFunctionInSuperclass(dot->right, c);
						expr->evalsTo = newType(ValueUnresolved);
						break;
					}
					CarbonValueType type = newType(ValueFunction);
					type.compound.signature =
						&class->parent->methods[-i - 1].sig;
					expr->evalsTo = carbon_cloneType(type);
					break;
				}
			}
			typecheck(dot->left, c, vm);
			switch (dot->left->evalsTo.tag) {
				case ValueArray:
					if (idntfLexCmp(dot->right, "append", strlen("append"))) {
						CarbonValueType t = newType(ValueFunction);
						t.compound.signature = carbon_reallocate(
							0, sizeof(CarbonFunctionSignature), NULL);
						t.compound.signature->arity = 1;
						t.compound.signature->arguments =
							carbon_reallocate(0, sizeof(CarbonValueType), NULL);
						t.compound.signature->returnType =
							carbon_reallocate(0, sizeof(CarbonValueType), NULL);
						*t.compound.signature->arguments = carbon_cloneType(
							*dot->left->evalsTo.compound.memberType);
						*t.compound.signature->returnType = newType(ValueVoid);
						expr->evalsTo = t;
						break;
					}
				case ValueString:
				case ValueGenerator: {
					if (idntfLexCmp(dot->right, "length", strlen("length"))) {
						expr->evalsTo = newType(ValueUInt);
					} else {
						memberNotFound(dot->left->evalsTo, dot->right, c);
						expr->evalsTo = newType(ValueUnresolved);
					}
					break;
				}
				case ValueInstance: {
					CarbonString *name =
						dot->left->evalsTo.compound.instanceName;
					Class *class;
					expr->evalsTo = newType(ValueUnresolved);
					if (!carbon_tableGet(&c->classes, (CarbonObj *) name,
										 (CarbonValue *) &class))
						return;

					CarbonString *propName =
						carbon_strFromToken(dot->right, vm);
					for (uint8_t i = 0; i < class->fieldCount; i++) {
						if (class->fields[i].name == propName) {
							expr->evalsTo =
								carbon_cloneType(class->fields[i].type);
							return;
						}
					}
					for (uint8_t i = 0; i < class->methodCount; i++) {
						if (class->methods[i].name == propName) {
							CarbonValueType tmp = newType(ValueFunction);
							tmp.compound.signature = &class->methods[i].sig;
							tmp = carbon_cloneType(tmp);
							expr->evalsTo = tmp;
							return;
						}
					}

					if (expr->evalsTo.tag == ValueUnresolved)
						memberNotFound(dot->left->evalsTo, dot->right, c);
					break;
				}

				default:
					unaryOpNotSupported(dot->dot, dot->left->evalsTo, c);
					expr->evalsTo = newType(ValueUnresolved);
					break;
			}

			break;
		}
		case ExprIs: {
			castNode(CarbonExprIs, is);
			if (is->left != NULL) {
				typecheck(is->left, c, vm);
				if (!isObject(is->left->evalsTo))
					unaryOpNotSupported(is->tok, is->left->evalsTo, c);
				CarbonTokenType t = is->right.base.type;
				if (t == TokenUInt || t == TokenInt || t == TokenDouble ||
					t == TokenBool)
					notObjectType(is->right, c);
			}
			expr->evalsTo = newType(ValueBool);
			break;
		}
		case ExprDotAssignment: {
			castNode(CarbonExprDotAssignment, da);
			typecheck((CarbonExpr *) da->left, c, vm);

			if (da->right == NULL)
				return;
			typecheck(da->right, c, vm);

			CarbonValueType leftType = da->left->expr.evalsTo;
			CarbonValueType rightType = da->right->evalsTo;

			if (da->left->left->evalsTo.tag != ValueInstance) {
				cantDotAssign(leftType, da->equals, c);
				expr->evalsTo = newType(ValueUnresolved);
				return;
			}

			if (!carbon_canAssign(leftType, rightType, c)) {
				if (da->equals.type == TokenEquals)
					cantAssign(leftType, rightType, da->equals.line, c);
				else
					binaryOpNotSupported(da->equals, leftType,
										 da->right->evalsTo, c);
				expr->evalsTo = newType(ValueUnresolved);
				return;
			}

			da->right = promoteNumerics(leftType, da->right);
			expr->evalsTo = carbon_cloneType(leftType);
			break;
		}
	}
#undef castNode
}

static void push(uint16_t index, CarbonChunk *chunk, CarbonToken token) {
	if (index > UINT8_MAX) {
		carbon_writeToChunk(chunk, OpLoadConstant16, token.line);
		carbon_writeToChunk(chunk, index >> 8, token.line);
		carbon_writeToChunk(chunk, index, token.line);
	} else {
		carbon_writeToChunk(chunk, OpLoadConstant, token.line);
		carbon_writeToChunk(chunk, index, token.line);
	}
}

static void pushValue(CarbonValue value, CarbonChunk *chunk,
					  CarbonToken token) {
	uint16_t index = carbon_addConstant(chunk, value);
	push(index, chunk, token);
}

static void pushLiteral(CarbonExprLiteral *lit, CarbonChunk *chunk,
						CarbonVM *vm) {
	CarbonValue toPush;
	switch (lit->expr.evalsTo.tag) {
		case ValueBool:
			if (lit->token.type == TokenTrue)
				carbon_writeToChunk(chunk, OpPush1, lit->token.line);
			else
				carbon_writeToChunk(chunk, OpPush0, lit->token.line);
			return;
		case ValueNull:
			carbon_writeToChunk(chunk, OpPush0, lit->token.line);
			return;
		case ValueUInt: {
			uint64_t returnValue = 0;
			for (uint32_t i = 0; i < lit->token.length; i++) {
				returnValue = returnValue * 10 + lit->token.lexeme[i] - '0';
			}
			if (returnValue == 0) {
				carbon_writeToChunk(chunk, OpPush0, lit->token.line);
				return;
			} else if (returnValue == 1) {
				carbon_writeToChunk(chunk, OpPush1, lit->token.line);
				return;
			}
			toPush = CarbonUInt(returnValue);
			break;
		}
		case ValueDouble: {
			toPush = CarbonDouble(strtod(lit->token.lexeme, NULL));
			break;
		}
		case ValueString: {
			CarbonString *str = carbon_copyString(lit->token.lexeme + 1,
												  lit->token.length - 2, vm);
			toPush = CarbonObject((CarbonObj *) str);
			break;
		}
		default:
			return;
	}
	pushValue(toPush, chunk, lit->token);
}

void carbon_compileExpression(CarbonExpr *expr, CarbonChunk *chunk,
							  CarbonCompiler *c, CarbonVM *vm);

static void compileUnaryExpression(CarbonExprUnary *un, CarbonChunk *chunk,
								   CarbonCompiler *c, CarbonVM *vm) {
	carbon_compileExpression(un->operand, chunk, c, vm);
	if (un->op.type == TokenMinus)
		switch (un->operand->evalsTo.tag) {
			case ValueUInt:
				carbon_writeToChunk(chunk, OpNegateUInt, un->op.line);
				break;
			case ValueInt:
				carbon_writeToChunk(chunk, OpNegateInt, un->op.line);
				break;
			case ValueDouble:
				carbon_writeToChunk(chunk, OpNegateDouble, un->op.line);
				break;
			default:
				break;
		}
	else if (un->op.type == TokenBang)
		carbon_writeToChunk(chunk, OpNegateBool, un->op.line);
}

void carbon_compileBinaryExpression(CarbonExprBinary *bin, CarbonChunk *chunk,
									CarbonCompiler *c, CarbonVM *vm) {
#define binInstruction(type, instruction)                                      \
	case type:                                                                 \
		carbon_writeToChunk(chunk, instruction, bin->op.line)

	if (bin->op.type == TokenAnd || bin->op.type == TokenOr) {
		carbon_compileExpression(bin->left, chunk, c, vm);
		CarbonOpCode op =
			bin->op.type == TokenAnd ? OpJumpOnFalse : OpJumpOnTrue;
		uint32_t pos = emitLogicalJump(chunk, bin->op.line, op);
		carbon_writeToChunk(chunk, OpPop, bin->op.line);
		carbon_compileExpression(bin->right, chunk, c, vm);
		patchJump(chunk, pos, bin->op, c);
		return;
	}

	carbon_compileExpression(bin->left, chunk, c, vm);
	carbon_compileExpression(bin->right, chunk, c, vm);

	switch (bin->op.type) {
		case TokenPlus:
		case TokenPlusEquals:
			switch (bin->left->evalsTo.tag) {
				binInstruction(ValueInt, OpAddInt);
				break;
				binInstruction(ValueUInt, OpAddInt);
				break;
				binInstruction(ValueDouble, OpAddDouble);
				break;
				binInstruction(ValueString, OpConcat);
				break;
				default:
					break;
			}
			break;
		case TokenMinus:
		case TokenMinusEquals:
			switch (bin->left->evalsTo.tag) {
				binInstruction(ValueInt, OpSubInt);
				break;
				binInstruction(ValueUInt, OpSubInt);
				break;
				binInstruction(ValueDouble, OpSubDouble);
				break;
				default:
					break;
			}
			break;
		case TokenSlash:
		case TokenSlashEquals:
			switch (bin->left->evalsTo.tag) {
				binInstruction(ValueInt, OpDivInt);
				break;
				binInstruction(ValueUInt, OpDivUInt);
				break;
				binInstruction(ValueDouble, OpDivDouble);
				break;
				default:
					break;
			}
			break;
		case TokenStar:
		case TokenStarEquals:
			switch (bin->left->evalsTo.tag) {
				binInstruction(ValueInt, OpMulInt);
				break;
				binInstruction(ValueUInt, OpMulUInt);
				break;
				binInstruction(ValueDouble, OpMulDouble);
				break;
				default:
					break;
			}
			break;
		case TokenPercent:
		case TokenPercentEquals:
			switch (bin->left->evalsTo.tag) {
				binInstruction(ValueInt, OpMod);
				break;
				binInstruction(ValueUInt, OpMod);
				break;
				default:
					break;
			}
			break;
		case TokenGreaterThan:
			switch (bin->left->evalsTo.tag) {
				binInstruction(ValueInt, OpCompareInt);
				break;
				binInstruction(ValueUInt, OpCompareUInt);
				break;
				binInstruction(ValueDouble, OpCompareDouble);
				break;
				default:
					break;
			}
			carbon_writeToChunk(chunk, OpGreater, bin->op.line);
			break;
		case TokenLessThan:
			switch (bin->left->evalsTo.tag) {
				binInstruction(ValueInt, OpCompareInt);
				break;
				binInstruction(ValueUInt, OpCompareUInt);
				break;
				binInstruction(ValueDouble, OpCompareDouble);
				break;
				default:
					break;
			}
			carbon_writeToChunk(chunk, OpLess, bin->op.line);
			break;
		case TokenLEQ:
			switch (bin->left->evalsTo.tag) {
				binInstruction(ValueInt, OpCompareInt);
				break;
				binInstruction(ValueUInt, OpCompareUInt);
				break;
				binInstruction(ValueDouble, OpCompareDouble);
				break;
				default:
					break;
			}
			carbon_writeToChunk(chunk, OpLEQ, bin->op.line);
			break;
		case TokenGEQ:
			switch (bin->left->evalsTo.tag) {
				binInstruction(ValueInt, OpCompareInt);
				break;
				binInstruction(ValueUInt, OpCompareUInt);
				break;
				binInstruction(ValueDouble, OpCompareDouble);
				break;
				default:
					break;
			}
			carbon_writeToChunk(chunk, OpGEQ, bin->op.line);
			break;
		case TokenEqualsEquals:
			carbon_writeToChunk(chunk, OpEquals, bin->op.line);
			break;
		case TokenBangEquals:
			carbon_writeToChunk(chunk, OpNotEquals, bin->op.line);
			break;
		default:
			break; // Should never reach here
	}
#undef binInstruction
}

static void compileCastExpression(CarbonExprCast *cast, CarbonChunk *chunk,
								  CarbonCompiler *c, CarbonVM *vm) {
	carbon_compileExpression(cast->expression, chunk, c, vm);
	CarbonValueType from = cast->expression->evalsTo;
	switch (cast->expr.evalsTo.tag) {
		case ValueInt:
			if (from.tag == ValueDouble)
				carbon_writeToChunk(chunk, OpDoubleToInt, cast->to.base.line);
			break;
		case ValueUInt:
			if (from.tag == ValueDouble)
				carbon_writeToChunk(chunk, OpDoubleToUInt, cast->to.base.line);
			break;
		case ValueDouble:
			if (from.tag == ValueInt)
				carbon_writeToChunk(chunk, OpIntToDouble, cast->to.base.line);
			else if (from.tag == ValueUInt)
				carbon_writeToChunk(chunk, OpUIntToDouble, cast->to.base.line);
			break;
		case ValueInstance: {
			Class *class = NULL;
			CarbonObj *name =
				(CarbonObj *) cast->expr.evalsTo.compound.instanceName;
			if (!carbon_tableGet(&c->classes, name, (CarbonValue *) &class))
				break;
			carbon_writeToChunk(chunk, OpInstanceCastcheck, cast->to.base.line);
			carbon_writeToChunk(chunk, class->id, cast->to.base.line);
			break;
		}
		case ValueBool:
			carbon_writeToChunk(chunk, OpToBool, cast->to.base.line);
			break;
		default: {
			uint16_t n =
				carbon_pushType(chunk, carbon_cloneType(cast->expr.evalsTo));
			carbon_writeToChunk(chunk, OpCastcheck, cast->to.base.line);
			carbon_writeToChunk(chunk, n >> 8, cast->to.base.line);
			carbon_writeToChunk(chunk, n, cast->to.base.line);
			break;
		}
	}
}
static void compileVarExpression(CarbonExprVar *var, CarbonChunk *chunk,
								 CarbonCompiler *c, CarbonVM *vm) {
	CarbonString *name = carbon_strFromToken(var->token, vm);

	int16_t slot = resolveLocal(name, c);
	if (slot != -1) {
		carbon_writeToChunk(chunk, OpGetLocal, var->token.line);
		carbon_writeToChunk(chunk, slot & 0xFF, var->token.line);
		return;
	}

	uint16_t index =
		carbon_addConstant(chunk, CarbonObject((CarbonObj *) name));
	if (index > UINT8_MAX) {
		push(index, chunk, var->token);
		carbon_writeToChunk(chunk, OpGetGlobal, var->token.line);
		return;
	}
	carbon_writeToChunk(chunk, OpGetGlobalInline, var->token.line);
	carbon_writeToChunk(chunk, index & 0xFF, var->token.line);
}

static void compileAssignmentExpression(CarbonExprAssignment *assignment,
										CarbonChunk *chunk, CarbonCompiler *c,
										CarbonVM *vm) {
	CarbonString *name = carbon_strFromToken(assignment->left, vm);
	if (assignment->equals.type != TokenEquals) {
		CarbonExprVar *var = carbon_newVarExpr(assignment->left);
		CarbonExprBinary *bin = carbon_newBinaryExpr(
			(CarbonExpr *) var, assignment->right, assignment->equals);
		assignment->right = (CarbonExpr *) bin;
	}

	carbon_compileExpression(assignment->right, chunk, c, vm);

	int16_t slot = resolveLocal(name, c);
	if (slot != -1) {
		carbon_writeToChunk(chunk, OpSetLocal, assignment->left.line);
		carbon_writeToChunk(chunk, slot & 0xFF, assignment->left.line);
		return;
	}

	uint16_t index =
		carbon_addConstant(chunk, CarbonObject((CarbonObj *) name));
	if (index > UINT8_MAX) {
		push(index, chunk, assignment->left);
		carbon_writeToChunk(chunk, OpSetGlobal, assignment->left.line);
		return;
	}
	carbon_writeToChunk(chunk, OpSetGlobalInline, assignment->left.line);
	carbon_writeToChunk(chunk, index & 0xFF, assignment->left.line);
}

static void compileCallExpression(CarbonExprCall *call, CarbonChunk *chunk,
								  CarbonCompiler *c, CarbonVM *vm) {

	if (call->callee->type == ExprLiteral) {
		CarbonExprLiteral *lit = (CarbonExprLiteral *) call->callee;
		if (lit->token.type == TokenSuper) {
			carbon_writeToChunk(chunk, OpInitInstance, lit->token.line);
			carbon_writeToChunk(chunk, c->class->parent->id, lit->token.line);
			for (uint8_t i = 0; i < call->arity; i++) {
				carbon_compileExpression(call->arguments[i], chunk, c, vm);
			}
			carbon_writeToChunk(chunk, OpGetLocal, lit->token.line);
			carbon_writeToChunk(
				chunk, resolveLocal(carbon_copyString("self", 4, vm), c),
				lit->token.line);
			carbon_writeToChunk(chunk, OpCall, call->line);
			carbon_writeToChunk(chunk, call->arity + 1, call->line);
			return;
		}
	}

	carbon_compileExpression(call->callee, chunk, c, vm);
	for (uint8_t i = 0; i < call->arity; i++) {
		carbon_compileExpression(call->arguments[i], chunk, c, vm);
	}
	carbon_writeToChunk(chunk, OpCall, call->line);
	carbon_writeToChunk(chunk, call->arity, call->line);
}

static void compileInitExpression(CarbonExprCall *call, CarbonChunk *chunk,
								  CarbonCompiler *c, CarbonVM *vm) {
	Class *class;
	CarbonToken name = ((CarbonExprLiteral *) call->callee)->token;
	carbon_tableGet(&c->classes, (CarbonObj *) carbon_strFromToken(name, vm),
					(CarbonValue *) &class);
	if (class->hasInit) {
		carbon_writeToChunk(chunk, OpInitInstance, name.line);
		carbon_writeToChunk(chunk, class->id, name.line);
		for (uint8_t i = 0; i < call->arity; i++) {
			carbon_compileExpression(call->arguments[i], chunk, c, vm);
		}

		carbon_writeToChunk(chunk, OpMakeInstance, name.line);
		carbon_writeToChunk(chunk, class->id, name.line);

		carbon_writeToChunk(chunk, OpCall, call->line);
		carbon_writeToChunk(chunk, call->arity + 1, call->line);
	} else {
		carbon_writeToChunk(chunk, OpMakeInstance, name.line);
		carbon_writeToChunk(chunk, class->id, name.line);
	}
}

static void compileArrayExpression(CarbonExprArray *arr, CarbonChunk *chunk,
								   CarbonCompiler *c, CarbonVM *vm) {
	if (arr->count <= 255) {
		carbon_writeToChunk(chunk, OpMakeArray, arr->bracket.line);
		carbon_writeToChunk(chunk, arr->count, arr->bracket.line);
	} else {
		pushValue(CarbonUInt(arr->count), chunk, arr->bracket);
		carbon_writeToChunk(chunk, OpMakeArray64, arr->bracket.line);
	}

	uint16_t n = carbon_pushType(
		chunk, carbon_cloneType(*arr->expr.evalsTo.compound.memberType));
	carbon_writeToChunk(chunk, n >> 8, arr->bracket.line);
	carbon_writeToChunk(chunk, n, arr->bracket.line);

	for (uint64_t i = 0; i < arr->count; i++) {
		carbon_compileExpression(arr->members[i], chunk, c, vm);
		carbon_writeToChunk(chunk, OpAppend, arr->bracket.line);
	}
}
static void compileArrayInitExpression(CarbonExprArray *arr, CarbonChunk *chunk,
									   CarbonCompiler *c, CarbonVM *vm) {
	carbon_compileExpression(arr->members[0], chunk, c, vm);
	carbon_writeToChunk(chunk, OpMakeArray64, arr->bracket.line);
	uint16_t n = carbon_pushType(
		chunk, carbon_cloneType(*arr->expr.evalsTo.compound.memberType));
	carbon_writeToChunk(chunk, n >> 8, arr->bracket.line);
	carbon_writeToChunk(chunk, n, arr->bracket.line);
	carbon_compileExpression(arr->members[1], chunk, c, vm);
	carbon_writeToChunk(chunk, OpInitArray, arr->bracket.line);
}

static void compileGeneratorExpression(CarbonExprArray *arr, CarbonChunk *chunk,
									   CarbonCompiler *c, CarbonVM *vm) {
	if (arr->capacity == 3)
		carbon_compileExpression(arr->members[2], chunk, c, vm);
	else if (arr->expr.evalsTo.compound.memberType->tag == ValueDouble)
		pushValue(CarbonDouble(1), chunk, arr->expr.first);
	else
		pushValue(CarbonInt(1), chunk, arr->expr.first);
	carbon_compileExpression(arr->members[1], chunk, c, vm);
	carbon_compileExpression(arr->members[0], chunk, c, vm);
	carbon_writeToChunk(chunk, OpMakeGenerator, arr->bracket.line);
	uint16_t n = carbon_pushType(
		chunk, carbon_cloneType(*arr->expr.evalsTo.compound.memberType));
	carbon_writeToChunk(chunk, n >> 8, arr->bracket.line);
	carbon_writeToChunk(chunk, n, arr->bracket.line);
}
static void compileIndexExpression(CarbonExprIndex *index, CarbonChunk *chunk,
								   CarbonCompiler *c, CarbonVM *vm) {
	carbon_compileExpression(index->object, chunk, c, vm);
	carbon_compileExpression(index->index, chunk, c, vm);
	carbon_writeToChunk(chunk, OpGetIndex, index->bracket.line);
}
static void
carbon_compileIndexAssignmentExpression(CarbonExprIndexAssignment *ie,
										CarbonChunk *chunk, CarbonCompiler *c,
										CarbonVM *vm) {

	carbon_compileExpression(ie->left->object, chunk, c, vm);
	carbon_compileExpression(ie->left->index, chunk, c, vm);
	if (ie->equals.type != TokenEquals) {
		ie->right = (CarbonExpr *) carbon_newBinaryExpr((CarbonExpr *) ie->left,
														ie->right, ie->equals);
		ie->left = NULL; // Without this, ie->left appears twice in the AST (the
						 // other is ie->right->left) which will cause
						 // memory-related errors. Nulling it here ensures it
						 // will appear exactly once, and doesn't affect normal
						 // program operation as it has already been compiled.
	}
	carbon_compileExpression(ie->right, chunk, c, vm);
	carbon_writeToChunk(chunk, OpSetIndex, ie->equals.line);
}

static void compileBuiltinDot(CarbonExprDot *dot, CarbonChunk *chunk,
							  CarbonCompiler *c, CarbonVM *vm) {
	switch (dot->left->evalsTo.tag) {
		case ValueArray:
			if (idntfLexCmp(dot->right, "append", strlen("append"))) {
				carbon_writeToChunk(chunk, OpBuiltin, dot->right.line);
				carbon_writeToChunk(chunk, BuiltinAppend, dot->right.line);
				uint16_t n =
					carbon_pushType(chunk, carbon_cloneType(dot->expr.evalsTo));
				carbon_writeToChunk(chunk, n >> 8, dot->right.line);
				carbon_writeToChunk(chunk, n, dot->right.line);
				break;
			}
		case ValueString:
		case ValueGenerator: {
			if (idntfLexCmp(dot->right, "length", strlen("length")))
				carbon_writeToChunk(chunk, OpLen, dot->right.line);
			break;
		}
		default:
			break;
	}
}

static void compileDotExpression(CarbonExprDot *dot, CarbonChunk *chunk,
								 CarbonCompiler *c, CarbonVM *vm) {

	if (dot->left->type == ExprLiteral) {
		CarbonExprLiteral *lit = (CarbonExprLiteral *) dot->left;
		if (lit->token.type == TokenSuper) {
			CarbonString *propName = carbon_strFromToken(dot->right, vm);
			uint8_t id = c->class->parent->id;
			uint8_t self = resolveLocal(carbon_copyString("self", 4, vm), c);
			int16_t i = findMember(c->class->parent, propName);
			carbon_writeToChunk(chunk, OpSuper, lit->token.line);
			carbon_writeToChunk(chunk, id, lit->token.line);
			carbon_writeToChunk(chunk, -i - 1, lit->token.line);
			carbon_writeToChunk(chunk, self, lit->token.line);
			return;
		}
	}

	carbon_compileExpression(dot->left, chunk, c, vm);

	if (dot->left->evalsTo.tag != ValueInstance) {
		compileBuiltinDot(dot, chunk, c, vm);
		return;
	}

	CarbonString *name = dot->left->evalsTo.compound.instanceName;
	CarbonString *propName = carbon_strFromToken(dot->right, vm);
	Class *class;
	carbon_tableGet(&c->classes, (CarbonObj *) name, (CarbonValue *) &class);

	int16_t i = findMember(class, propName);
	if (i > 0) {
		carbon_writeToChunk(chunk, OpDot, dot->right.line);
		carbon_writeToChunk(chunk, i - 1, dot->right.line);
	} else if (i < 0) {
		carbon_writeToChunk(chunk, OpMethod, dot->right.line);
		carbon_writeToChunk(chunk, -i - 1, dot->right.line);
	}
}

static void compileIsExpression(CarbonExprIs *is, CarbonChunk *chunk,
								CarbonCompiler *c, CarbonVM *vm) {
	carbon_compileExpression(is->left, chunk, c, vm);
	CarbonValueType t = resolveType(is->right, c, vm);
	if (t.tag == ValueInstance) {
		Class *class = NULL;
		CarbonObj *name = (CarbonObj *) t.compound.instanceName;
		if (!carbon_tableGet(&c->classes, name, (CarbonValue *) &class))
			return;
		carbon_writeToChunk(chunk, OpIsInstance, is->tok.line);
		carbon_writeToChunk(chunk, class->id, is->right.base.line);
	} else {
		uint16_t n = carbon_pushType(chunk, t);
		carbon_writeToChunk(chunk, OpIs, is->right.base.line);
		carbon_writeToChunk(chunk, n >> 8, is->right.base.line);
		carbon_writeToChunk(chunk, n, is->right.base.line);
	}
}

static void compileDotAssignmentExpression(CarbonExprDotAssignment *da,
										   CarbonChunk *chunk,
										   CarbonCompiler *c, CarbonVM *vm) {
	carbon_compileExpression(da->left->left, chunk, c, vm);

	CarbonString *name = da->left->left->evalsTo.compound.instanceName;
	CarbonString *propName = carbon_strFromToken(da->left->right, vm);

	if (da->equals.type != TokenEquals) {
		da->right = (CarbonExpr *) carbon_newBinaryExpr((CarbonExpr *) da->left,
														da->right, da->equals);
		da->left = NULL; // Refer to compileIndexAssignment for clarification
	}
	carbon_compileExpression(da->right, chunk, c, vm);
	carbon_writeToChunk(chunk, OpDotSet, da->equals.line);

	Class *class;
	carbon_tableGet(&c->classes, (CarbonObj *) name, (CarbonValue *) &class);

	for (uint8_t i = 0; i < class->fieldCount; i++) {
		if (class->fields[i].name == propName) {
			carbon_writeToChunk(chunk, i, da->equals.line);
			break;
		}
	}
}

void carbon_compileExpression(CarbonExpr *expr, CarbonChunk *chunk,
							  CarbonCompiler *c, CarbonVM *vm) {

	if (expr == NULL)
		return;
	typecheck(expr, c, vm);
	if (expr->evalsTo.tag == ValueUnresolved)
		return;
	if (c->parserHadError)
		return;

	switch (expr->type) {
		case ExprUnary: {
			compileUnaryExpression((CarbonExprUnary *) expr, chunk, c, vm);
			break;
		}
		case ExprBinary: {
			carbon_compileBinaryExpression((CarbonExprBinary *) expr, chunk, c,
										   vm);
			break;
		}
		case ExprLiteral: {
			CarbonExprLiteral *lit = (CarbonExprLiteral *) expr;
			pushLiteral(lit, chunk, vm);
			break;
		}
		case ExprGrouping: {
			CarbonExprGrouping *group = (CarbonExprGrouping *) expr;
			carbon_compileExpression(group->expression, chunk, c, vm);
			break;
		}
		case ExprCast: {
			compileCastExpression((CarbonExprCast *) expr, chunk, c, vm);
			break;
		}
		case ExprVar: {
			compileVarExpression((CarbonExprVar *) expr, chunk, c, vm);
			break;
		}
		case ExprAssignment: {
			compileAssignmentExpression((CarbonExprAssignment *) expr, chunk, c,
										vm);
			break;
		}
		case ExprIndexAssignment: {
			carbon_compileIndexAssignmentExpression(
				(CarbonExprIndexAssignment *) expr, chunk, c, vm);
			break;
		}
		case ExprCall: {
			compileCallExpression((CarbonExprCall *) expr, chunk, c, vm);
			break;
		}
		case ExprInit: {
			compileInitExpression((CarbonExprCall *) expr, chunk, c, vm);
			break;
		}
		case ExprArray: {
			CarbonExprArray *arr = (CarbonExprArray *) expr;
			if (arr->imethod == ImethodGenerator)
				compileGeneratorExpression(arr, chunk, c, vm);
			else if (arr->imethod == ImethodContracted)
				compileArrayInitExpression(arr, chunk, c, vm);
			else if (arr->imethod == ImethodStandard)
				compileArrayExpression(arr, chunk, c, vm);
			break;
		}
		case ExprIndex: {
			compileIndexExpression((CarbonExprIndex *) expr, chunk, c, vm);
			break;
		}
		case ExprDot: {
			compileDotExpression((CarbonExprDot *) expr, chunk, c, vm);
			break;
		}
		case ExprDotAssignment: {
			compileDotAssignmentExpression((CarbonExprDotAssignment *) expr,
										   chunk, c, vm);
			break;
		}
		case ExprIs: {
			compileIsExpression((CarbonExprIs *) expr, chunk, c, vm);
			break;
		}
	}
}

static CarbonValue defaultState(CarbonValueType type, CarbonVM *vm) {
	switch (type.tag) {
		case ValueString:
			return CarbonObject((CarbonObj *) carbon_copyString("", 0, vm));

		default:
			return CarbonUInt(0); // should never reach here;
	}
}

void carbon_compileStatement(CarbonStmt *stmt, CarbonChunk *chunk,
							 CarbonCompiler *c, CarbonVM *vm);

#define emit(opcode, line)                                                     \
	if (!(c->parserHadError || c->hadError))                                   \
	carbon_writeToChunk(chunk, opcode, line)

static void compilePrintStmt(CarbonStmtPrint *print, CarbonChunk *chunk,
							 CarbonCompiler *c, CarbonVM *vm) {
	if (print->expression == NULL)
		return;
	carbon_compileExpression(print->expression, chunk, c, vm);
	CarbonOpCode op;

	switch (print->expression->evalsTo.tag) {
		case ValueInt:
			op = OpPrintInt;
			break;
		case ValueUInt:
			op = OpPrintUInt;
			break;
		case ValueDouble:
			op = OpPrintDouble;
			break;
		case ValueBool:
			op = OpPrintBool;
			break;
		case ValueVoid:
			cantPrint(print->token, c);
			break;
		default:
			op = OpPrintObj;
			break;
	}
	emit(op, print->token.line);
}

static void compileExprStmt(CarbonStmtExpr *expr, CarbonChunk *chunk,
							CarbonCompiler *c, CarbonVM *vm) {
	if (expr->expression == NULL)
		return;
	carbon_compileExpression(expr->expression, chunk, c, vm);

	if (expr->expression->evalsTo.tag == ValueVoid) {
		if (expr->expression->type == ExprCall) {
			CarbonExprCall *call = (CarbonExprCall *) expr->expression;
			if (call->callee->type == ExprLiteral) {
				// the only way to call a literal
				// is if it is super, so no need
				// to check the actual token
				emit(OpPop, expr->last.line);
				return;
			}
		}
		return;
	}
	emit(OpPop, expr->last.line);
}

static void compileVarDecStmt(CarbonStmtVarDec *vardec, CarbonChunk *chunk,
							  CarbonCompiler *c, CarbonVM *vm) {

	CarbonString *name = carbon_strFromToken(vardec->identifier, vm);

	bool isLocal = c->compilingTo != NULL;

	Global *g;
	carbon_tableGet(&c->globals, (CarbonObj *) name, (CarbonValue *) &g);

	// CLONE !!!!
	// If global:
	// 		Get from Global g
	// If local:
	// 		resolve type here *
	// 		store in c.locals *
	// 		discard when popping
	CarbonValueType vartype;

	if (isLocal) {
		vartype = resolveType(vardec->type, c, vm);
	} else {
		vartype = g->valueType;
	}

	if (vardec->initializer != NULL) {
		typecheck(vardec->initializer, c, vm);
		if (!carbon_canAssign(vartype, vardec->initializer->evalsTo, c)) {
			cantAssign(vartype, vardec->initializer->evalsTo,
					   vardec->identifier.line, c);
			if (!isLocal)
				g->declared = true;
		} else {
			vardec->initializer = promoteNumerics(vartype, vardec->initializer);
			carbon_compileExpression(vardec->initializer, chunk, c, vm);
		}
	} else {
		if (isObject(vartype))
			pushValue(defaultState(vartype, vm), chunk, vardec->identifier);
		else
			emit(OpPush0, vardec->identifier.line);
	}

	if (!isLocal) {
		g->declared = true;
		uint16_t index =
			carbon_addConstant(chunk, CarbonObject((CarbonObj *) name));
		if (index > UINT8_MAX) {
			push(index, chunk, vardec->identifier);
			emit(OpSetGlobal, vardec->identifier.line);
		} else {
			emit(OpSetGlobalInline, vardec->identifier.line);
			emit(index, vardec->identifier.line);
		}
		emit(OpPop, vardec->identifier.line);
		if (!isObject(vartype))
			carbon_tableSet(&vm->primitives, (CarbonObj *) name, CarbonUInt(0));
	} else {
		int16_t depth = resolveLocal(name, c);
		if (depth == -1 || c->locals[depth].depth != c->depth) {
			CarbonLocal l = {vartype, name, c->depth};
			if (c->localCount != 255)
				c->locals[c->localCount++] = l;
			else {
				tooManyLocals(vardec->identifier, c);
			}
		} else {
			localRedeclaration(vardec->identifier, c);
		}
	}
}

static void compileFuncStatement(CarbonStmtFunc *sfunc, CarbonChunk *chunk,
								 CarbonCompiler *c, CarbonVM *vm) {

	CarbonString *name = carbon_strFromToken(sfunc->identifier, vm);

	Global *g;
	carbon_tableGet(&c->globals, (CarbonObj *) name, (CarbonValue *) &g);

	CarbonFunctionSignature *sig = g->valueType.compound.signature;

	// ofunc needs to have a reference to its own signature.
	// We store it here so it gets freed correctly later on

	CarbonValueType tmp = newType(ValueFunction);
	tmp.compound.signature = sig;
	tmp = carbon_cloneType(tmp);
	carbon_pushType(chunk, tmp);

	CarbonValueType returnType = *sig->returnType;

	CarbonFunction *ofunc =
		carbon_newFunction(name, sfunc->arity, tmp.compound.signature, vm);

	c->compilingTo = ofunc;
	g->declared = true;
	c->localCount = sfunc->arity;
	for (uint8_t i = 0; i < sfunc->arity; i++) {
		CarbonToken name = sfunc->arguments[i].name;
		c->locals[i].depth = 0;
		c->locals[i].name = carbon_strFromToken(name, vm);
		c->locals[i].type = carbon_cloneType(sig->arguments[i]);
	}
	bool hadReturn = false;

	for (uint32_t i = 0; i < sfunc->statements.count; i++) {
		hadReturn = hadReturn || alwaysReturns(sfunc->statements.arr[i]);
		carbon_compileStatement(sfunc->statements.arr[i], &ofunc->chunk, c, vm);
	}
	if (!hadReturn && c->compilingTo->sig->returnType->tag != ValueVoid) {
		expectedReturnStatement(sfunc->identifier, c);
	}

	c->compilingTo = NULL;
	for (; c->localCount > 0; c->localCount--) {
		carbon_freeType(c->locals[c->localCount - 1].type);
	}

	if (returnType.tag == ValueVoid) {
		carbon_writeToChunk(&ofunc->chunk, OpReturnVoid, sfunc->end);
	}
	carbon_tableSet(&vm->globals, (CarbonObj *) name,
					CarbonObject((CarbonObj *) ofunc));
}

static void compileReturnStatement(CarbonStmtReturn *ret, CarbonChunk *chunk,
								   CarbonCompiler *c, CarbonVM *vm) {

	if (ret->expression != NULL) {
		typecheck(ret->expression, c, vm);
		if (c->compilingTo->sig->returnType->tag == ValueVoid) {
			noReturnWanted(ret->token, c->compilingTo->name, c);
			return;
		}
		if (ret->expression->evalsTo.tag == ValueUnresolved)
			return;
		CarbonValueType wanted = *c->compilingTo->sig->returnType;
		CarbonValueType given = ret->expression->evalsTo;
		if (!carbon_canAssign(wanted, given, c)) {
			wrongReturnType(ret->token, wanted, given, c);
		}

		ret->expression = promoteNumerics(wanted, ret->expression);
		carbon_compileExpression(ret->expression, chunk, c, vm);

		carbon_writeToChunk(chunk, OpReturn, ret->token.line);
		return;
	}
	if (c->compilingTo->sig->returnType->tag != ValueVoid) {
		wrongReturnType(ret->token, *c->compilingTo->sig->returnType,
						newType(ValueVoid), c);
		return;
	}
	if (c->class != NULL && c->selfSlot != -1) {
		carbon_writeToChunk(chunk, OpGetLocal, ret->token.line);
		carbon_writeToChunk(chunk, c->selfSlot, ret->token.line);
		carbon_writeToChunk(chunk, OpReturn, ret->token.line);
	}
	carbon_writeToChunk(chunk, OpReturnVoid, ret->token.line);
}

static void compileBlockStatement(CarbonStmtBlock *block, CarbonChunk *chunk,
								  CarbonCompiler *c, CarbonVM *vm) {

	c->depth++;
	for (uint32_t i = 0; i < block->statements.count; i++) {
		carbon_compileStatement(block->statements.arr[i], chunk, c, vm);
	}
	if (block->locals == 1) {
		carbon_writeToChunk(chunk, OpPop, block->locals);
	} else if (block->locals >= 1) {
		carbon_writeToChunk(chunk, OpPopn, block->locals);
		carbon_writeToChunk(chunk, block->locals, block->locals);
	}
	c->depth--;
	if (c->localCount > 0)
		while (c->locals[c->localCount - 1].depth > c->depth)
			carbon_freeType(c->locals[c->localCount--].type);
}

static void compileIfStatement(CarbonStmtIf *sif, CarbonChunk *chunk,
							   CarbonCompiler *c, CarbonVM *vm) {

	if (sif->condition != NULL)
		carbon_compileExpression(sif->condition, chunk, c, vm);
	uint32_t first = emitIf(chunk, sif->token.line);
	uint32_t second = 0;
	carbon_compileStatement(sif->then, chunk, c, vm);
	if (sif->notThen != NULL)
		second = emitJump(chunk, sif->token.line);
	patchJump(chunk, first, sif->token, c);
	if (sif->notThen != NULL) {
		carbon_compileStatement(sif->notThen, chunk, c, vm);
		patchJump(chunk, second, sif->elseToken, c);
	}
}

static void compileWhileStatement(CarbonStmtWhile *whl, CarbonChunk *chunk,
								  CarbonCompiler *c, CarbonVM *vm) {

	uint32_t backpos = chunk->count;
	if (whl->condition != NULL)
		carbon_compileExpression(whl->condition, chunk, c, vm);

	uint32_t p = emitIf(chunk, whl->token.line);
	uint32_t ejectExit = 0; // To suppress "may be unitialized" warning
							// that arises Even though the variable is
							// guaranteed to be inited if used

	c->loopDepth++;
	c->depth++;

	for (uint32_t i = 0; i < whl->body->statements.count; i++) {
		carbon_compileStatement(whl->body->statements.arr[i], chunk, c, vm);
	}
	c->loopDepth--;

	if (c->breaksCount > 0)
		for (uint8_t i = c->breaksCount - 1;
			 c->breaks[i].depth == c->loopDepth + 1; i--) {
			if (!c->breaks[i].isBreak) {
				patchJump(chunk, c->breaks[i].position, c->breaks[i].token, c);
			}
		}

	if (whl->body->locals == 1) {
		carbon_writeToChunk(chunk, OpPop, whl->token.line);
	} else if (whl->body->locals >= 1) {
		carbon_writeToChunk(chunk, OpPopn, whl->token.line);
		carbon_writeToChunk(chunk, whl->body->locals, whl->token.line);
	}

	c->depth--;
	if (c->localCount > 0)
		while (c->locals[c->localCount - 1].depth > c->depth)
			carbon_freeType(c->locals[c->localCount--].type);

	if (whl->body->hasBreak && whl->body->locals > 0) {
		uint32_t eject = emitJump(chunk, whl->token.line);
		for (uint8_t i = c->breaksCount - 1;
			 c->breaks[i].depth == c->loopDepth + 1; i--) {
			if (c->breaks[i].isBreak) {
				patchJump(chunk, c->breaks[i].position, c->breaks[i].token, c);
			}
		}
		if (whl->body->locals == 1) {
			carbon_writeToChunk(chunk, OpPop, whl->token.line);
		} else if (whl->body->locals >= 1) {
			carbon_writeToChunk(chunk, OpPopn, whl->token.line);
			carbon_writeToChunk(chunk, whl->body->locals, whl->token.line);
		}
		ejectExit = emitJump(chunk, whl->token.line);
		patchJump(chunk, eject, whl->token, c);
	}

	uint32_t offset = chunk->count - backpos + 2;
	if (offset > UINT16_MAX) {
		jumpTooLong(whl->token, c);
		return;
	}

	carbon_writeToChunk(chunk, OpLoop, whl->token.line);
	carbon_writeToChunk(chunk, offset >> 8, whl->token.line);
	carbon_writeToChunk(chunk, offset, whl->token.line);

	patchJump(chunk, p, whl->token, c);
	if (whl->body->hasBreak) {
		if (whl->body->locals == 0)
			for (uint8_t i = c->breaksCount - 1;
				 c->breaks[i].depth == c->depth + 1; i--) {
				if (c->breaks[i].isBreak) {
					patchJump(chunk, c->breaks[i].position, c->breaks[i].token,
							  c);
				}
			}
		else
			patchJump(chunk, ejectExit, whl->token, c);
	}

	if (c->breaksCount > 0)
		while (c->breaks[c->breaksCount - 1].depth == c->loopDepth + 1)
			c->breaksCount--;
}

static void compileBreakStatement(CarbonStmtBreak *brk, CarbonChunk *chunk,
								  CarbonCompiler *c, CarbonVM *vm) {

	c->breaks[c->breaksCount].depth = c->loopDepth;
	c->breaks[c->breaksCount].isBreak = brk->token.type == TokenBreak;
	c->breaks[c->breaksCount].token = brk->token;
	c->breaks[c->breaksCount].position = emitJump(chunk, brk->token.line);
	c->breaksCount++;
}

static void compileForStatement(CarbonStmtFor *fr, CarbonChunk *chunk,
								CarbonCompiler *c, CarbonVM *vm) {
	if (fr->arr == NULL)
		return;
	typecheck(fr->arr, c, vm);
	CarbonValueType varType;
	switch (fr->arr->evalsTo.tag) {
		case ValueString:
			varType = newType(ValueString);
			break;
		case ValueArray:
		case ValueGenerator:
			varType = carbon_cloneType(*fr->arr->evalsTo.compound.memberType);
			break;
		default:
			unindexableType(fr->arr->first, fr->arr->evalsTo, c);
			return;
	}

	carbon_compileExpression(fr->arr, chunk, c, vm);
	carbon_writeToChunk(chunk, OpPush0, fr->arr->first.line);
	c->localCount += 2;

	uint32_t forpos = emitLogicalJump(chunk, fr->token.line, OpFor);

	// We have to compile the body ourselved instead of relying the function
	// because we need finer control

	c->depth++;
	c->loopDepth++;

	CarbonLocal l = {varType, carbon_strFromToken(fr->var, vm), c->depth};
	if (c->localCount != 255)
		c->locals[c->localCount++] = l;
	else {
		tooManyLocals(fr->var, c);
	}

	for (uint32_t i = 0; i < fr->body->statements.count; i++) {
		carbon_compileStatement(fr->body->statements.arr[i], chunk, c, vm);
	}

	c->loopDepth--;
	c->depth--;

	if (c->breaksCount > 0)
		for (uint8_t i = c->breaksCount - 1;
			 c->breaks[i].depth == c->loopDepth + 1; i--) {
			if (!c->breaks[i].isBreak) {
				patchJump(chunk, c->breaks[i].position, c->breaks[i].token, c);
			}
		}

	if (fr->body->locals == 0) {
		carbon_writeToChunk(chunk, OpPop, fr->token.line);
	} else if (fr->body->locals >= 1) {
		carbon_writeToChunk(chunk, OpPopn, fr->token.line);
		carbon_writeToChunk(chunk, fr->body->locals + 1, fr->token.line);
	}

	while (c->locals[c->localCount - 1].depth > c->depth)
		carbon_freeType(c->locals[c->localCount--].type);

	// BREAK SECTION
	uint32_t jumpAfterBreak = 0; // to avoid "may be uninitialized" warnins
	if (fr->body->hasBreak) {
		uint32_t jumpOverBreak = emitJump(chunk, fr->token.line); // 1963

		if (c->breaksCount > 0)
			for (uint8_t i = c->breaksCount - 1;
				 c->breaks[i].depth == c->loopDepth + 1; i--) {
				if (c->breaks[i].isBreak) {
					patchJump(chunk, c->breaks[i].position, c->breaks[i].token,
							  c);
				}
			}

		if (fr->body->locals == 0) {
			carbon_writeToChunk(chunk, OpPop, fr->token.line);
		} else {
			carbon_writeToChunk(chunk, OpPopn, fr->token.line);
			carbon_writeToChunk(chunk, fr->body->locals + 1, fr->token.line);
		}
		jumpAfterBreak = emitJump(chunk, fr->token.line); // 1976
		patchJump(chunk, jumpOverBreak, fr->token, c);
	}
	// BREAK SECTION OVER
	if (forpos > UINT16_MAX) {
		jumpTooLong(fr->token, c);
		return;
	}

	uint16_t offset = chunk->count - forpos + 2;

	carbon_writeToChunk(chunk, OpLoop, fr->token.line);
	carbon_writeToChunk(chunk, offset >> 8, fr->token.line);
	carbon_writeToChunk(chunk, offset, fr->token.line);

	patchJump(chunk, forpos, fr->token, c);
	if (fr->body->hasBreak)
		patchJump(chunk, jumpAfterBreak, fr->token, c);

	carbon_writeToChunk(chunk, OpPopn, fr->token.line);
	carbon_writeToChunk(chunk, 2, fr->token.line);

	c->localCount -= 2;
	if (c->breaksCount > 0)
		while (c->breaks[c->breaksCount - 1].depth == c->loopDepth + 1)
			c->breaksCount--;
}

static void compileClassStatement(CarbonStmtClass *sClass, CarbonChunk *chunk,
								  CarbonCompiler *c, CarbonVM *vm) {
	struct carbon_class class;
	Class *csig;
	CarbonString *classname = carbon_strFromToken(sClass->name, vm);
	carbon_tableGet(&c->classes, (CarbonObj *) classname,
					(CarbonValue *) &csig);

	// Make sure we only compile the first class
	// in case there is a redefinition

	class.fieldCount = csig->fieldCount;
	class.methodCount = csig->methodCount;
	class.init = csig->init;

	class.methods = carbon_reallocate(
		0, csig->methodCount * sizeof(CarbonFunction *), NULL);

	if (csig->parent != NULL) {
		class.superclass = csig->parent->id;
		for (uint8_t i = 0; i < vm->classes[class.superclass].methodCount;
			 i++) {
			class.methods[i] = vm->classes[class.superclass].methods[i];
		}
	} else {
		class.superclass = -1;
	}

	for (uint8_t i = 0; i < sClass->statements.count; i++) {

		CarbonStmt *stmt = sClass->statements.arr[i];
		if (stmt->type != StmtFunc)
			continue;
		CarbonStmtFunc *sfunc = (CarbonStmtFunc *) stmt;

		CarbonString *name = carbon_strFromToken(sfunc->identifier, vm);

		int16_t n = findMember(csig, name);
		uint8_t mthd;
		if (n < 0)
			mthd = -n - 1;
		else
			continue;

		CarbonFunctionSignature sig = csig->methods[mthd].sig;

		CarbonValueType tmp = newType(ValueFunction);
		tmp.compound.signature = &sig;
		tmp = carbon_cloneType(tmp);
		carbon_pushType(chunk, tmp);

		CarbonValueType returnType = *sig.returnType;

		uint32_t length = sfunc->identifier.length + sClass->name.length + 1;
		char *namechars = carbon_reallocateObj(0, length + 1, NULL, vm);
		strncpy(namechars, sClass->name.lexeme, sClass->name.length);
		namechars[sClass->name.length] = '.';
		namechars[length] = 0;
		strncpy(namechars + sClass->name.length + 1, sfunc->identifier.lexeme,
				sfunc->identifier.length);

		CarbonFunction *ofunc =
			carbon_newFunction(carbon_takeString(namechars, length, vm),
							   sig.arity, tmp.compound.signature, vm);

		c->localCount = sig.arity + 1;
		CarbonValueType this = newType(ValueInstance);
		this.compound.instanceName = classname;

		uint8_t lower = sfunc->arity < sig.arity ? sfunc->arity : sig.arity;

		for (uint8_t i = 0; i < lower; i++) {
			CarbonToken name = sfunc->arguments[i].name;
			c->locals[i].depth = 0;
			c->locals[i].name = carbon_strFromToken(name, vm);
			c->locals[i].type = carbon_cloneType(sig.arguments[i]);
		}

		uint8_t self = c->localCount - 1;
		c->locals[self] =
			(CarbonLocal){this, carbon_copyString("self", 4, vm), 0};
		bool hadReturn = false;

		if (name->length == sClass->name.length - 1 &&
			!memcmp(name->chars, sClass->name.lexeme + 1, name->length))

			c->selfSlot = self;
		else
			c->selfSlot = -1;

		c->compilingTo = ofunc;
		c->class = csig;

		for (uint32_t i = 0; i < sfunc->statements.count; i++) {
			hadReturn = hadReturn || alwaysReturns(sfunc->statements.arr[i]);
			carbon_compileStatement(sfunc->statements.arr[i], &ofunc->chunk, c,
									vm);
		}

		if (!hadReturn && c->compilingTo->sig->returnType->tag != ValueVoid) {
			expectedReturnStatement(sfunc->identifier, c);
		}

		c->compilingTo = NULL;
		c->class = NULL;

		if (c->selfSlot != -1) {
			carbon_writeToChunk(&ofunc->chunk, OpGetLocal, sfunc->end);
			carbon_writeToChunk(&ofunc->chunk, self, sfunc->end);
			carbon_writeToChunk(&ofunc->chunk, OpReturn, sfunc->end);
		} else if (returnType.tag == ValueVoid) {
			carbon_writeToChunk(&ofunc->chunk, OpReturnVoid, sfunc->end);
		}

		for (; c->localCount > 0; c->localCount--) {
			carbon_freeType(c->locals[c->localCount - 1].type);
		}

		class.methods[mthd] = ofunc;
		c->selfSlot = -1;
	}
	vm->classes[csig->id] = class;
}

void carbon_compileStatement(CarbonStmt *stmt, CarbonChunk *chunk,
							 CarbonCompiler *c, CarbonVM *vm) {
	if (stmt == NULL)
		return;

	switch (stmt->type) {
		case StmtPrint: {
			compilePrintStmt((CarbonStmtPrint *) stmt, chunk, c, vm);
			break;
		}
		case StmtExpr: {
			compileExprStmt((CarbonStmtExpr *) stmt, chunk, c, vm);
			break;
		}
		case StmtVarDec: {
			compileVarDecStmt((CarbonStmtVarDec *) stmt, chunk, c, vm);
			break;
		}
		case StmtFunc: {
			compileFuncStatement((CarbonStmtFunc *) stmt, chunk, c, vm);
			break;
		}
		case StmtReturn: {
			compileReturnStatement((CarbonStmtReturn *) stmt, chunk, c, vm);
			break;
		}
		case StmtBlock: {
			compileBlockStatement((CarbonStmtBlock *) stmt, chunk, c, vm);
			break;
		}
		case StmtIf: {
			compileIfStatement((CarbonStmtIf *) stmt, chunk, c, vm);
			break;
		}
		case StmtWhile: {
			compileWhileStatement((CarbonStmtWhile *) stmt, chunk, c, vm);
			break;
		}
		case StmtBreak: {
			compileBreakStatement((CarbonStmtBreak *) stmt, chunk, c, vm);
			break;
		}
		case StmtFor: {
			compileForStatement((CarbonStmtFor *) stmt, chunk, c, vm);
			break;
		}
		case StmtClass: {
			compileClassStatement((CarbonStmtClass *) stmt, chunk, c, vm);
			break;
		}
	}
#undef emit
}

void carbon_initCompiler(CarbonCompiler *compiler, CarbonParser *parser) {
	compiler->hadError = false;
	compiler->selfSlot = -1;
	compiler->parserHadError = parser->hadError;
	compiler->compilingTo = NULL;
	compiler->class = NULL;
	compiler->localCount = 0;
	compiler->depth = 0;
	compiler->breaksCount = 0;
	compiler->loopDepth = 0;
	compiler->classCount = 0;
	carbon_tableInit(&compiler->globals);
	carbon_tableInit(&compiler->classes);
}
void carbon_freeCompiler(CarbonCompiler *compiler) {
	compiler->hadError = false;
	compiler->parserHadError = false;
	for (uint32_t i = 0; i < compiler->globals.capacity; i++) {
		if (compiler->globals.entries[i].key != NULL) {
			Global *g = (Global *) compiler->globals.entries[i].value.obj;
			carbon_freeType(g->valueType);
			carbon_reallocate(sizeof(Global), 0, g);
		}
	}
	carbon_tableFree(&compiler->globals);
	for (uint32_t i = 0; i < compiler->classes.capacity; i++) {
		if (compiler->classes.entries[i].key != NULL) {
			Class *class = (Class *) compiler->classes.entries[i].value.obj;
			freeClass(class);
		}
	}
	carbon_tableFree(&compiler->classes);
}
