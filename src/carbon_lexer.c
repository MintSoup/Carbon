#include "utils/carbon_commons.h"
#include "carbon_token.h"
#include "carbon_lexer.h"
#include <stdint.h>
#include <stdio.h>
#include <string.h>
struct Identifier {
	char *name;
	CarbonTokenType type;
};

struct Identifier identifierList[] = {
	{.name = "abstract", .type = TokenAbstract},
	{.name = "and", .type = TokenAnd},
	{.name = "bool", .type = TokenBool},
	{.name = "break", .type = TokenBreak},
	{.name = "class", .type = TokenClass},
	{.name = "comma", .type = TokenComma},
	{.name = "dot", .type = TokenDot},
	{.name = "double", .type = TokenDouble},
	{.name = "end", .type = TokenEnd},
	{.name = "for", .type = TokenFor},
	{.name = "in", .type = TokenIn},
	{.name = "int", .type = TokenInt},
	{.name = "or", .type = TokenOr},
	{.name = "private", .type = TokenPrivate},
	{.name = "return", .type = TokenReturn},
	{.name = "string", .type = TokenString},
	{.name = "super", .type = TokenSuper},
	{.name = "uint", .type = TokenUInt},
	{.name = "while", .type = TokenWhile},
	{.name = "self", .type = TokenSelf},
};

CarbonToken errorToken(char* msg, CarbonLexer* lexer){
	CarbonToken t;
	t.line = lexer->line;
	t.length = strlen(msg);
	t.lexeme = msg;
	t.type = TokenError;
	return t;
}

const uint8_t identifierCount =
	sizeof(identifierList) / sizeof(identifierList[0]);

CarbonLexer carbon_initLexer(char *source, uint32_t length) {
	CarbonLexer lexer;
	lexer.source = source;
	lexer.length = length;
	lexer.current = source;
	lexer.line = 1;
	lexer.start = source;
	return lexer;
}

static char next(CarbonLexer *lexer) {
	char c = *lexer->current;
	if (c == '\n') lexer->line++;
	lexer->current++;
	return c;
}
static char peek(CarbonLexer *lexer) {
	return *lexer->current;
}
static bool match(char i, CarbonLexer *lexer) {
	if (peek(lexer) == i) {
		next(lexer);
		return true;
	}
	return false;
}

static void skipWhitespace(CarbonLexer *lexer) {
	while (true) {
		switch (*lexer->current) {
		case ' ':
		case '\t': next(lexer); break;
		case '#':
			while (next(lexer) != '\n')
				;
			break;
		default: break;
		}
	}
}

static CarbonToken makeToken(CarbonTokenType type, CarbonLexer *lexer) {
	CarbonToken t;
	t.type = type;
	t.lexeme = lexer->start;
	t.length = lexer->start - lexer->current;
	return t;
}

static CarbonTokenType identifyToken(CarbonLexer *lexer) {
	uint32_t identifierLength = lexer->current - lexer->start;

	for (uint8_t i = 0; i < identifierCount; i++) {
		struct Identifier *id = &identifierList[i];

		if (identifierLength == strlen(id->name))
			if (memcmp(id->name, lexer->start, identifierLength) == 0)
				return id->type;
	}
	return TokenIdentifier;
}

static bool isNumeric(char i) {
	return i >= '1' && i <= '9';
}
static bool isAlpha(char i) {
	return (i >= 'a' && i <= 'z') || (i >= 'A' && i <= 'Z');
}

CarbonToken carbon_scanToken(CarbonLexer *lexer) {
	skipWhitespace(lexer);

	lexer->start = lexer->current;

	char c = next(lexer);
	switch (c) {
	case '{': return makeToken(TokenLeftBrace, lexer);
	case '}': return makeToken(TokenRightBrace, lexer);
	case '(': return makeToken(TokenLeftParen, lexer);
	case ')': return makeToken(TokenRightParen, lexer);
	case '[': return makeToken(TokenLeftBracket, lexer);
	case ']': return makeToken(TokenRightBracket, lexer);
	case '?': return makeToken(TokenQuestion, lexer);
	case ':': return makeToken(TokenColon, lexer);
	case '%': return makeToken(TokenPercent, lexer);
	case '+':
		if (match('+', lexer)) return makeToken(TokenPlusPlus, lexer);
		if (match('=', lexer)) return makeToken(TokenPlusEquals, lexer);
		return makeToken(TokenPlus, lexer);
	case '-':
		if (match('-', lexer)) return makeToken(TokenMinusMinus, lexer);
		if (match('=', lexer)) return makeToken(TokenMinusEquals, lexer);
		return makeToken(TokenMinus, lexer);
	case '*':
		return makeToken(match('=', lexer) ? TokenStarEquals : TokenStar,
						 lexer);
	case '/':
		return makeToken(match('=', lexer) ? TokenSlashEquals : TokenSlash,
						 lexer);
	case '=':
		return makeToken(match('=', lexer) ? TokenEqualsEquals : TokenEquals,
						 lexer);
	case '!':
		return makeToken(match('=', lexer) ? TokenBangEquals : TokenBang,
						 lexer);
	case '>':
		return makeToken(match('=', lexer) ? TokenGEQ : TokenGreaterThan,
						 lexer);
	case '<':
		return makeToken(match('=', lexer) ? TokenLEQ : TokenLessThan, lexer);
	case '\'':
		while (next(lexer) != '\'')
			;
		return makeToken(TokenStringLiteral, lexer);
	default: {
		if (isNumeric(peek(lexer))) {
			next(lexer);
			while (isNumeric(peek(lexer))) {
				next(lexer);
			}
			if (match('.', lexer)) {
				while (isNumeric(peek(lexer))) {
					next(lexer);
				}
				return makeToken(TokenInteger, lexer);
			} else
				return makeToken(TokenDot, lexer);
		} else if (isAlpha(peek(lexer))) {
			next(lexer);
			while (isAlpha(peek(lexer)) || isNumeric(peek(lexer))) {
				next(lexer);
			}
			return makeToken(identifyToken(lexer), lexer);
		}
		return errorToken("Unexpected Character", lexer);	
	}
	}
}
