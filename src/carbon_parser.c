#include "ast/carbon_expressions.h"
#include "ast/carbon_statements.h"
#include "carbon_lexer.h"
#include "carbon_token.h"
#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "carbon_parser.h"
#include "utils/carbon_memory.h"
#include "ast/carbon_statements.h"
#include <stdio.h>

static void error(CarbonToken at, char *msg, CarbonParser *p) {
	if (p->panic)
		return;
	p->panic = true;
	p->hadError = true;
	fprintf(stderr, "[Line %d]", at.line);
	switch (at.type) {
		case TokenEOF: {
			fprintf(stderr, " EOF: %s", msg);
			break;
		}
		case TokenError: {
			fprintf(stderr, ": Unexpected charater '%c'", at.length);
			break;
		}
		case TokenEOS: {
			fprintf(stderr, " at end of statement: %s", msg);
			break;
		}
		default: {
			fprintf(stderr, ": %s", msg);
			break;
		}
	}
	fprintf(stderr, "\n");
}

static inline void errorAtCurrent(char *msg, CarbonParser *p) {
	error(p->tokens[p->currentToken], msg, p);
}

static inline CarbonToken next(CarbonParser *p) {
	return p->tokens[p->currentToken++];
}

static inline CarbonToken peekn(uint32_t n, CarbonParser *p) {
	return p->tokens[p->currentToken + n];
}

static inline CarbonToken peek(CarbonParser *p) {
	return p->tokens[p->currentToken];
}

static inline CarbonToken previous(CarbonParser *p) {
	return p->tokens[p->currentToken - 1];
}

static bool match(CarbonTokenType t, CarbonParser *p) {
	if (peek(p).type == t) {
		next(p);
		return true;
	}
	return false;
}

static bool consume(CarbonTokenType t, char *msg, CarbonParser *p) {
	if (peek(p).type == t) {
		next(p);
		return true;
	}
	errorAtCurrent(msg, p);
	return false;
}

void carbon_initParser(CarbonParser *parser, CarbonLexer *lexer) {

	parser->currentToken = 0;
	parser->totalTokens = 8;
	parser->tokens =
		carbon_reallocate(0, parser->totalTokens * sizeof(CarbonToken), NULL);
	while (true) {
		CarbonToken p;
		while (true) {
			p = carbon_scanToken(lexer);
			if (p.type != TokenError)
				break;
			error(p, NULL, parser);
		}

		if (parser->currentToken == parser->totalTokens) {
			uint32_t oldSize = parser->totalTokens * sizeof(CarbonToken);
			parser->totalTokens *= 2;
			uint32_t newSize = parser->totalTokens * sizeof(CarbonToken);
			parser->tokens =
				carbon_reallocate(oldSize, newSize, parser->tokens);
		}
		parser->tokens[parser->currentToken] = p;
		parser->currentToken++;
		if (p.type == TokenEOF) {
			uint32_t oldSize = parser->totalTokens * sizeof(CarbonToken);
			uint32_t newSize = parser->currentToken * sizeof(CarbonToken);
			parser->tokens =
				carbon_reallocate(oldSize, newSize, parser->tokens);
			parser->totalTokens = parser->currentToken;
			parser->currentToken = 0;
			break;
		}
	}

	parser->panic = false;
	parser->hadError = false;
}

void carbon_freeParser(CarbonParser *p) {
	carbon_reallocate(p->totalTokens * sizeof(CarbonToken), 0, p->tokens);
}

static CarbonStmtPrint *printStatement(CarbonParser *p);
static CarbonStmtExpr *expressionStatement(CarbonParser *p);

static CarbonExpr *equality(CarbonParser *p);
static CarbonExpr *comparison(CarbonParser *p);
static CarbonExpr *addition(CarbonParser *p);
static CarbonExpr *multiplication(CarbonParser *p);
static CarbonExpr *unary(CarbonParser *p);
static CarbonExpr *primary(CarbonParser *p);

static CarbonExpr *expression(CarbonParser *p) {
	return (CarbonExpr *) equality(p);
}

static CarbonStmt *statement(CarbonParser *p) {
	switch (peek(p).type) {
		case TokenPrint:
			return (CarbonStmt *) printStatement(p);
		default:
			return (CarbonStmt *) expressionStatement(p);
	}
}

CarbonStmt *carbon_parseStatement(CarbonParser *p) {
	return statement(p);
}

CarbonExpr *carbon_parseExpression(CarbonParser *p) {
	return expression(p);
}

static CarbonStmtPrint *printStatement(CarbonParser *p) {
	CarbonToken print = next(p);
	consume(TokenLeftParen, "Expected '(' after print", p);
	CarbonExpr* expr = expression(p);
	consume(TokenRightParen, "Expected ')' after print expression", p);
	consume(TokenEOS, "Expected EOS after print statement", p);
	return carbon_newPrintStmt(expr, print);
}
static CarbonStmtExpr *expressionStatement(CarbonParser *p) {
	CarbonExpr *expr = expression(p);
	consume(TokenEOS, "Expected EOS after expression statement", p);
	return carbon_newExprStmt(expr, previous(p));
}

static CarbonExpr *equality(CarbonParser *p) {
	CarbonExpr *expr = comparison(p);
	while (match(TokenEqualsEquals, p) || match(TokenBangEquals, p)) {
		CarbonToken tok = previous(p);
		expr = (CarbonExpr *) carbon_newBinaryExpr(expr, comparison(p), tok);
	}
	return expr;
}

static CarbonExpr *comparison(CarbonParser *p) {
	CarbonExpr *expr = addition(p);
	while (match(TokenGreaterThan, p) || match(TokenLessThan, p) ||
		   match(TokenLEQ, p) || match(TokenGEQ, p)) {
		CarbonToken tok = previous(p);
		expr = (CarbonExpr *) carbon_newBinaryExpr(expr, addition(p), tok);
	}
	return expr;
}

static CarbonExpr *addition(CarbonParser *p) {
	CarbonExpr *expr = multiplication(p);
	while (match(TokenPlus, p) || match(TokenMinus, p)) {
		CarbonToken tok = previous(p);
		expr =
			(CarbonExpr *) carbon_newBinaryExpr(expr, multiplication(p), tok);
	}
	return expr;
}

static CarbonExpr *multiplication(CarbonParser *p) {
	CarbonExpr *expr = unary(p);
	while (match(TokenStar, p) || match(TokenSlash, p)) {
		CarbonToken tok = previous(p);
		expr = (CarbonExpr *) carbon_newBinaryExpr(expr, unary(p), tok);
	}
	return expr;
}

static CarbonExpr *unary(CarbonParser *p) {
	if (match(TokenBang, p) || match(TokenMinus, p)) {
		CarbonToken tok = previous(p);
		return (CarbonExpr *) carbon_newUnaryExpr(unary(p), tok);
	}
	if (peek(p).type == TokenLeftParen && peekn(2, p).type == TokenRightParen) {
		switch (peekn(1, p).type) {
			case TokenBool:
			case TokenInt:
			case TokenUInt:
			case TokenDouble: {
				next(p);
				CarbonToken to = next(p);
				next(p);
				return (CarbonExpr *) carbon_newCastExpr(to, unary(p));
			}
			default:
				break;
		}
	}
	return primary(p);
}

static CarbonExpr *primary(CarbonParser *p) {
	switch (peek(p).type) {
		case TokenStringLiteral:
		case TokenInteger:
		case TokenDecimal:
		case TokenTrue:
		case TokenFalse: {
			return (CarbonExpr *) carbon_newLiteralExpr(next(p));
		}
		case TokenLeftParen: {
			next(p);
			CarbonExpr *expr = expression(p);
			expr = (CarbonExpr *) carbon_newGroupingExpr(expr);
			consume(TokenRightParen, "Expected expression", p);
			return expr;
		}
		default: {
			errorAtCurrent("Expected expression", p);
			return NULL;
		}
	}
}
