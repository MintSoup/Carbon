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

// clang-format off
struct Identifier identifierList[] = {
	{"and", TokenAnd},
	{"or", TokenOr},
	{"is", TokenIs},
	{"as", TokenAs},

	{"bool", TokenBool},
	{"uint", TokenUInt},
	{"int", TokenInt},
	{"double", TokenDouble},
	{"string", TokenString},
	{"array", TokenArray},
	{"generator", TokenGenerator},
	{"error", TokenError},
	{"table", TokenTable},
	{"function", TokenFunction},
	{"object", TokenObject},

	{"while", TokenWhile},
	{"break", TokenBreak},
	{"continue", TokenContinue},

	{"for", TokenFor},
	{"in", TokenIn},
	{"if", TokenIf},
	{"else", TokenElse},
	{"elif", TokenElif},

	{"class", TokenClass},
	{"super", TokenSuper},

	{"false", TokenFalse},
	{"true", TokenTrue},

	{"void", TokenVoid},
	{"return", TokenReturn},
	{"end", TokenEnd},

	{"null", TokenNull},
	{"import", TokenImport},
	{"print", TokenPrint}
};

// clang-format on

static bool isAtEnd(CarbonLexer *lexer) {
	return *lexer->current == 0;
}

static CarbonToken errorToken(char unexpected, CarbonLexer *lexer) {
	CarbonToken t;
	t.line = lexer->line;
	t.length = unexpected;
	t.type = ErrorToken;
	t.file = lexer->file;
	return t;
}

const uint8_t identifierCount =
	sizeof(identifierList) / sizeof(identifierList[0]);

CarbonLexer carbon_initLexer(char *source, uint32_t length, char *file) {
	CarbonLexer lexer;
	lexer.source = source;
	lexer.length = length;
	lexer.current = source;
	lexer.line = 1;
	lexer.start = source;
	lexer.lastToken = TokenNone;
	lexer.file = file;
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
		case TokenStringLiteral:
		case TokenInteger:
		case TokenDecimal:
		case TokenIdentifier:
		case TokenBreak:
		case TokenContinue:
		case TokenReturn:
		case TokenTrue:
		case TokenFalse:
		case TokenNull:
		case TokenUInt:
		case TokenInt:
		case TokenString:
		case TokenBool:
		case TokenDouble:
		case TokenObject:
		case TokenClassname:
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
	t.file = lexer->file;
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
static inline bool isHex(char i) {
	return isNumeric(i) || (i >= 'A' && i <= 'F') || (i >= 'a' && i <= 'f');
}
static inline bool isBin(char i) {
	return i == '1' || i == '0';
}
static inline bool isOct(char i) {
	return i >= '0' && i <= '7';
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
			return makeToken(
				match('=', lexer) ? TokenPercentEquals : TokenPercent, lexer);
		case '.':
			return makeToken(match('.', lexer) ? TokenDotDot : TokenDot, lexer);
		case ',':
			return makeToken(TokenComma, lexer);
		case ';':
			return makeToken(TokenEOS, lexer);
		case '+':
			return makeToken(match('=', lexer) ? TokenPlusEquals : TokenPlus,
							 lexer);
		case '-':
			return makeToken(match('=', lexer) ? TokenMinusEquals : TokenMinus,
							 lexer);
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
		case '@': {
			if (!isAlpha(peek(lexer)) && !isNumeric(peek(lexer)))
				return errorToken(peek(lexer), lexer);
			next(lexer);
			while (isAlpha(peek(lexer)) || isNumeric(peek(lexer))) {
				next(lexer);
			}
			return makeToken(TokenClassname, lexer);
		}
		default: {
			if (isNumeric(c)) {
				bool onlyZeroes = true;
				while (isNumeric(peek(lexer)))
					if (next(lexer) != '0')
						onlyZeroes = false;

				if (peek(lexer) == '.') {
					if (*(lexer->current + 1) == '.')
						return makeToken(TokenInteger, lexer);
					next(lexer);
					while (isNumeric(peek(lexer)))
						next(lexer);

					return makeToken(TokenDecimal, lexer);
				} else if (onlyZeroes && peek(lexer) == 'x' &&
						   isHex(*(lexer->current + 1))) {
					next(lexer);
					while (isHex(peek(lexer)))
						next(lexer);
					return makeToken(TokenInteger, lexer);
				} else if (onlyZeroes && peek(lexer) == 'o' &&
						   isOct(*(lexer->current + 1))) {
					next(lexer);
					while (isOct(peek(lexer)))
						next(lexer);
					return makeToken(TokenInteger, lexer);
				} else if (onlyZeroes && peek(lexer) == 'b' &&
						   isBin(*(lexer->current + 1))) {
					next(lexer);
					while (isBin(peek(lexer)))
						next(lexer);
					return makeToken(TokenInteger, lexer);
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
