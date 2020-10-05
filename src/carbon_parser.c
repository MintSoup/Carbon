#include "ast/carbon_expressions.h"
#include "carbon_lexer.h"
#include "carbon_token.h"
#include "utils/carbon_commons.h"
#include "carbon_parser.h"
#include <stdio.h>

static void error(CarbonToken at, char *msg, CarbonParser *p) {
	if (p->panic) return;
	p->panic = true;
	p->hadError = true;
	fprintf(stderr, "[Line %d]", at.line);
	switch (at.type) {
	case TokenEOF: {
		fprintf(stderr, ", EOF: %s", msg);
		break;
	}
	case TokenError: {
		fprintf(stderr, ": Unexpected charater '%c'", at.length);
		break;
	}
	case TokenEOS: {
		fprintf(stderr, ", at end of statement: %s", msg);
		break;
	}
	default: {
		fprintf(stderr, ": %s", msg);
		break;
	}
	}
	fprintf(stderr, "\n");
}

static void errorAtCurrent(char *msg, CarbonParser *p) {
	error(p->current, msg, p);
}

static CarbonToken next(CarbonParser *p) {
	p->previous = p->current;
	while (true) {
		p->current = carbon_scanToken(p->lexer);
		if (p->current.type != TokenError) break;
		errorAtCurrent(p->current.lexeme, p);
	}
	return p->previous;
}

static bool match(CarbonTokenType t, CarbonParser *p) {
	if (p->current.type == t) {
		next(p);
		return true;
	}
	return false;
}

static bool consume(CarbonTokenType t, char *msg, CarbonParser *p) {
	if (p->current.type == t) {
		next(p);
		return true;
	}
	errorAtCurrent(msg, p);
	return false;
}

void carbon_initParser(CarbonParser *parser, CarbonLexer *lexer) {
	parser->lexer = lexer;
	parser->panic = false;
	parser->hadError = false;
	next(parser);
}

static CarbonExpr *addition(CarbonParser *p);
static CarbonExpr *multiplication(CarbonParser *p);
static CarbonExpr *unary(CarbonParser *p);
static CarbonExpr *primary(CarbonParser *p);

static CarbonExpr *expression(CarbonParser *p) {
	return (CarbonExpr *) addition(p);
}

CarbonExpr *carbon_parseExpression(CarbonParser *p) {
	return expression(p);
}

static CarbonExpr *addition(CarbonParser *p) {
	CarbonExpr *expr = multiplication(p);
	while (match(TokenPlus, p) || match(TokenMinus, p)) {
		CarbonToken tok = p->previous;
		expr =
			(CarbonExpr *) carbon_newBinaryExpr(expr, multiplication(p), tok);
	}
	return expr;
}

static CarbonExpr *multiplication(CarbonParser *p) {
	CarbonExpr *expr = unary(p);
	while (match(TokenStar, p) || match(TokenSlash, p)) {
		CarbonToken tok = p->previous;
		expr = (CarbonExpr *) carbon_newBinaryExpr(expr, unary(p), tok);
	}
	return expr;
}

static CarbonExpr *unary(CarbonParser *p) {
	if (match(TokenBang, p) || match(TokenMinus, p)) {
		CarbonToken tok = p->previous;
		return (CarbonExpr *) carbon_newUnaryExpr(unary(p), tok);
	}
	return primary(p);
}

static CarbonExpr *primary(CarbonParser *p) {
	switch (p->current.type) {
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
