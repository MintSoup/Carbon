#pragma once
#include "utils/carbon_commons.h"

typedef enum {
	TokenLeftParen,
	TokenRightParen,
	TokenLeftBracket,
	TokenRightBracket,
	TokenLeftBrace,
	TokenRightBrace,
	TokenPlus,
	TokenMinus,
	TokenSlash,
	TokenStar,
	TokenPercent,
	TokenPlusEquals,
	TokenMinusEquals,
	TokenSlashEquals,
	TokenStarEquals,
	TokenPlusPlus,
	TokenMinusMinus,
	TokenStringLiteral,
	TokenInteger,
	TokenDecimal,
	TokenBang,
	TokenEquals,
	TokenEqualsEquals,
	TokenBangEquals,
	TokenGEQ,
	TokenLEQ,
	TokenLessThan,
	TokenGreaterThan,
	TokenColon,
	TokenIdentifier,
	TokenFor,
	TokenIn,
	TokenWhile,
	TokenAnd,
	TokenOr,
	TokenEnd,
	TokenComma,
	TokenDot,
	TokenBreak,
	TokenReturn,
	TokenInt,
	TokenDouble,
	TokenUInt,
	TokenString,
	TokenBool,
	TokenQuestion,
	TokenClass,
	TokenAbstract,
	TokenSuper,
	TokenPrivate,
	TokenSelf,
	TokenError,
	TokenEOF,
	TokenEOS
} CarbonTokenType;

typedef struct {
	CarbonTokenType type;
	char *lexeme;
	int length;
	uint32_t line;
} CarbonToken;