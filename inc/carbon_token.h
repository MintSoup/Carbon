#pragma once
#include "utils/carbon_commons.h"

typedef enum {
	TokenLeftParen,
	TokenRightParen,
	TokenLeftBracket,
	TokenRightBracket,
	TokenLeftBrace,
	TokenRightBrace,
	TokenLeftAInit,
	TokenRightAInit,
	TokenPlus,
	TokenMinus,
	TokenSlash,
	TokenStar,
	TokenPercent,
	TokenPlusEquals,
	TokenMinusEquals,
	TokenSlashEquals,
	TokenStarEquals,
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
	TokenArray,
	TokenGenerator,
	TokenFunction,
	TokenTable,
	TokenError,
	TokenWhile,
	TokenAnd,
	TokenOr,
	TokenEnd,
	TokenComma,
	TokenDot,
	TokenDotDot,
	TokenBreak,
	TokenContinue,
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
	ErrorToken,
	TokenEOF,
	TokenEOS,
	TokenIf,
	TokenElse,
	TokenElif,
	TokenTrue,
	TokenFalse,
	TokenPrint,
	TokenVoid,
	TokenNull,
	TokenNone
} CarbonTokenType;

typedef struct {
	CarbonTokenType type;
	char *lexeme;
	uint32_t length;
	uint32_t line;
} CarbonToken;
