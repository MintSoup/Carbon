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
	{.name = "and", .type = TokenAnd},
	{.name = "or", .type = TokenOr},

	{.name = "bool", .type = TokenBool},
	{.name = "uint", .type = TokenUInt},
	{.name = "int", .type = TokenInt},
	{.name = "double", .type = TokenDouble},
	{.name = "string", .type = TokenString},
	{.name = "array", .type = TokenArray},
	{.name = "generator", .type = TokenGenerator},
	{.name = "error", .type = TokenError},
	{.name = "table", .type = TokenTable},
	{.name = "function", .type = TokenFunction},

	{.name = "while", .type = TokenWhile},
	{.name = "break", .type = TokenBreak},
	{.name = "continue", .type = TokenContinue},

	{.name = "for", .type = TokenFor},
	{.name = "in", .type = TokenIn},
	{.name = "if", .type = TokenIf},
	{.name = "else", .type = TokenElse},
	{.name = "elif", .type = TokenElif},

	{.name = "class", .type = TokenClass},
	{.name = "self", .type = TokenSelf},

	{.name = "false", .type = TokenFalse},
	{.name = "true", .type = TokenTrue},

	{.name = "void", .type = TokenVoid},
	{.name = "return", .type = TokenReturn},
	{.name = "end", .type = TokenEnd},

	{.name = "null", .type = TokenNull},
	{.name = "print", .type = TokenPrint}};

static bool isAtEnd(CarbonLexer *lexer) {
	return *lexer->current == 0;
}

static CarbonToken errorToken(char unexpected, CarbonLexer *lexer) {
	CarbonToken t;
	t.line = lexer->line;
	t.length = unexpected;
	t.type = ErrorToken;
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
	lexer.lastToken = TokenNone;
	return lexer;
}

static char next(CarbonLexer *lexer) {
	char c = *lexer->current;
	if (c == '\n')
		lexer->line++;
	lexer->current++;
	return c;
}
static char peek(CarbonLexer *lexer) {
	return *lexer->current;
}
static bool match(char i, CarbonLexer *lexer) {
	if (isAtEnd(lexer))
		return false;
	if (peek(lexer) == i) {
		next(lexer);
		return true;
	}
	return false;
}

static bool canEndStatement(CarbonTokenType type) {
	switch (type) {
		case TokenRightParen:
		case TokenRightBracket:
		case TokenRightBrace:
		case TokenRightAInit:
		case TokenPlusPlus:
		case TokenMinusMinus:
		case TokenStringLiteral:
		case TokenInteger:
		case TokenDecimal:
		case TokenIdentifier:
		case TokenBreak:
		case TokenContinue:
		case TokenReturn:
		case TokenSelf:
		case TokenTrue:
		case TokenFalse:
		case TokenNull:
			return true;
		default:
			return false;
	}
}

static bool skipWhitespace(CarbonLexer *lexer) {
	bool eos = false;
	while (true) {
		char c = peek(lexer);
		switch (c) {
			case '\t':
			case ' ':
			case '\r':
				next(lexer);
				break;
			case '\n':
				if (canEndStatement(lexer->lastToken))
					eos = true;
				next(lexer);
				break;
			case '#':
				while (peek(lexer) != '\n' && !isAtEnd(lexer))
					next(lexer);
				break;
			default:
				return eos;
		}
	}
}

static CarbonToken makeToken(CarbonTokenType type, CarbonLexer *lexer) {
	CarbonToken t;
	t.length = lexer->current - lexer->start;
	t.line = lexer->line;
	t.type = type;
	t.lexeme = lexer->start;
	lexer->lastToken = type;
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

static inline bool isNumeric(char i) {
	return i >= '0' && i <= '9';
}
static inline bool isAlpha(char i) {
	return (i >= 'a' && i <= 'z') || (i >= 'A' && i <= 'Z') || (i == '_');
}

CarbonToken carbon_scanToken(CarbonLexer *lexer) {
	if (skipWhitespace(lexer))
		return makeToken(TokenEOS, lexer);

	if (isAtEnd(lexer))
		return makeToken(TokenEOF, lexer);

	lexer->start = lexer->current;

	char c = next(lexer);
	switch (c) {
		case '{':
			return makeToken(TokenLeftBrace, lexer);
		case '}':
			return makeToken(TokenRightBrace, lexer);
		case '(':
			return makeToken(TokenLeftParen, lexer);
		case ')':
			return makeToken(TokenRightParen, lexer);
		case '[':
			return makeToken(
				match('<', lexer) ? TokenLeftAInit : TokenLeftBracket, lexer);
		case ']':
			return makeToken(TokenRightBracket, lexer);
		case '?':
			return makeToken(TokenQuestion, lexer);
		case ':':
			return makeToken(TokenColon, lexer);
		case '%':
			return makeToken(TokenPercent, lexer);
		case '.':
			return makeToken(match('.', lexer) ? TokenDotDot : TokenDot, lexer);
		case ',':
			return makeToken(TokenComma, lexer);
		case ';':
			return makeToken(TokenEOS, lexer);
		case '+':
			if (match('+', lexer))
				return makeToken(TokenPlusPlus, lexer);
			if (match('=', lexer))
				return makeToken(TokenPlusEquals, lexer);
			return makeToken(TokenPlus, lexer);
		case '-':
			if (match('-', lexer))
				return makeToken(TokenMinusMinus, lexer);
			if (match('=', lexer))
				return makeToken(TokenMinusEquals, lexer);
			return makeToken(TokenMinus, lexer);
		case '*':
			return makeToken(match('=', lexer) ? TokenStarEquals : TokenStar,
							 lexer);
		case '/':
			return makeToken(match('=', lexer) ? TokenSlashEquals : TokenSlash,
							 lexer);
		case '=':
			return makeToken(
				match('=', lexer) ? TokenEqualsEquals : TokenEquals, lexer);
		case '!':
			return makeToken(match('=', lexer) ? TokenBangEquals : TokenBang,
							 lexer);
		case '>':
			if (match(']', lexer))
				return makeToken(TokenRightAInit, lexer);
			if (match('=', lexer))
				return makeToken(TokenGEQ, lexer);
			else
				return makeToken(TokenGreaterThan, lexer);

		case '<':
			return makeToken(match('=', lexer) ? TokenLEQ : TokenLessThan,
							 lexer);
		case '\'': {
			uint32_t line = lexer->line;
			while (next(lexer) != '\'') {
				if (isAtEnd(lexer)) {
					// Dirtiest trick ever
					uint32_t current = lexer->line;
					lexer->line = line;
					CarbonToken err = errorToken('\'', lexer);
					lexer->line = current;
					return err;
				}
			}
			return makeToken(TokenStringLiteral, lexer);
		}
		default: {
			if (isNumeric(c)) {
				while (isNumeric(peek(lexer))) {
					next(lexer);
				}
				if (peek(lexer) == '.') {
					if (*(lexer->current + 1) == '.')
						return makeToken(TokenInteger, lexer);
					next(lexer);
					while (isNumeric(peek(lexer))) {
						next(lexer);
					}
					return makeToken(TokenDecimal, lexer);
				} else
					return makeToken(TokenInteger, lexer);
			} else if (isAlpha(c)) {
				while (isAlpha(peek(lexer)) || isNumeric(peek(lexer))) {
					next(lexer);
				}
				return makeToken(identifyToken(lexer), lexer);
			}
			return errorToken(c, lexer);
		}
	}
}
