#include "ast/carbon_expressions.h"
#include "ast/carbon_statements.h"
#include "carbon_lexer.h"
#include "carbon_token.h"
#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "carbon_parser.h"
#include "utils/carbon_memory.h"
#include "ast/carbon_statements.h"
#include <string.h>
#include <stdio.h>

static void error(CarbonToken at, char *msg, CarbonParser *p) {
	if (p->panic)
		return;
	p->panic = true;
	p->hadError = true;
	fprintf(stderr, "[Line %d] ", at.line);
	switch (at.type) {
		case TokenEOF: {
			fprintf(stderr, "EOF: %s", msg);
			break;
		}
		case TokenError: {
			if (at.length == '\'') {
				fprintf(stderr, "Unterminated string");
				break;
			}
			fprintf(stderr, "Unexpected charater '%c'", at.length);
			break;
		}
		case TokenEOS: {
			fprintf(stderr, "at end of statement: %s", msg);
			break;
		}
		default: {
			fprintf(stderr, "at '%.*s': %s", at.length, at.lexeme, msg);
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

static void sync(CarbonParser *p) {
	CarbonTokenType pk;
	while ((pk = peek(p).type) != TokenEOF) {
		switch (pk) {
			case TokenFor:
			case TokenWhile:
			case TokenIf:
			case TokenPrint:
			case TokenReturn:
			case TokenClass:
			case TokenVoid:
				p->panic = false;
				return;
			case TokenEnd:
			case TokenEOS:
				next(p);
				p->panic = false;
				return;
			case TokenUInt:
			case TokenInt:
			case TokenDouble:
			case TokenString:
			case TokenBool:
				if (peekn(1, p).type != TokenRightParen) {
					p->panic = false;
					return;
				}
			default:
				next(p);
		}
	}
}

void carbon_initParser(CarbonParser *parser, CarbonLexer *lexer) {

	parser->currentToken = 0;
	parser->totalTokens = 8;
	parser->panic = false;
	parser->hadError = false;
	parser->tokens =
		carbon_reallocate(0, parser->totalTokens * sizeof(CarbonToken), NULL);
	while (true) {
		CarbonToken current;
		CarbonToken prev;
		while (true) {
			current = carbon_scanToken(lexer);
			if (current.type == TokenEOF)
				break;
			if (parser->panic) {
				switch (current.type) {
					case TokenFor:
					case TokenWhile:
					case TokenIf:
					case TokenPrint:
					case TokenReturn:
					case TokenClass:
						parser->panic = false;
						break;
					case TokenEnd:
					case TokenEOS:
						current = carbon_scanToken(lexer);
						parser->panic = false;
						break;
					case TokenUInt:
					case TokenInt:
					case TokenDouble:
					case TokenString:
					case TokenBool:
						if (prev.type != TokenLeftParen) {
							parser->panic = false;
							break;
						}
					default:
						break;
				}
			}
			prev = current;
			if (current.type == TokenError) {
				error(current, NULL, parser);
			}
			if (!parser->panic)
				break;
		}

		if (parser->currentToken == parser->totalTokens) {
			uint32_t oldSize = parser->totalTokens * sizeof(CarbonToken);
			parser->totalTokens *= 2;
			uint32_t newSize = parser->totalTokens * sizeof(CarbonToken);
			parser->tokens =
				carbon_reallocate(oldSize, newSize, parser->tokens);
		}
		parser->tokens[parser->currentToken] = current;
		parser->currentToken++;
		if (current.type == TokenEOF) {
			uint32_t oldSize = parser->totalTokens * sizeof(CarbonToken);
			uint32_t newSize = parser->currentToken * sizeof(CarbonToken);
			parser->tokens =
				carbon_reallocate(oldSize, newSize, parser->tokens);
			parser->totalTokens = parser->currentToken;
			parser->currentToken = 0;
			break;
		}
	}
}

void carbon_freeParser(CarbonParser *p) {
	carbon_reallocate(p->totalTokens * sizeof(CarbonToken), 0, p->tokens);
}

static bool isTypename(CarbonToken token) {
	switch (token.type) {
		case TokenUInt:
		case TokenInt:
		case TokenString:
		case TokenBool:
		case TokenDouble:
			return true;
		default:
			return false;
	}
}

static CarbonStmtFunc *funcDeclaration(CarbonParser *p);
static CarbonStmtReturn *returnStatement(CarbonParser *p);
static CarbonStmtPrint *printStatement(CarbonParser *p);
static CarbonStmtExpr *expressionStatement(CarbonParser *p);
static CarbonStmtVarDec *varDeclaration(CarbonParser *p);

static CarbonExpr *assignment(CarbonParser *p);
static CarbonExpr *equality(CarbonParser *p);
static CarbonExpr *comparison(CarbonParser *p);
static CarbonExpr *addition(CarbonParser *p);
static CarbonExpr *multiplication(CarbonParser *p);
static CarbonExpr *unary(CarbonParser *p);
static CarbonExpr *postfix(CarbonParser *p);
static CarbonExpr *primary(CarbonParser *p);

static CarbonExpr *expression(CarbonParser *p) {
	return assignment(p);
}

static CarbonStmt *statement(CarbonParser *p) {
	if (p->panic)
		sync(p);
	CarbonToken n = peek(p);
	if (n.type == TokenEOF)
		return NULL;
	else if (n.type == TokenPrint)
		return (CarbonStmt *) printStatement(p);
	else if (isTypename(n))
		return (CarbonStmt *) varDeclaration(p);
	else if (n.type == TokenReturn)
		return (CarbonStmt *) returnStatement(p);
	else
		return (CarbonStmt *) expressionStatement(p);
}

CarbonStmt *carbon_parseStatement(CarbonParser *p) {
	if (p->panic)
		sync(p);
	CarbonToken n = peek(p);
	if (n.type == TokenEOF)
		return NULL;
	else if (isTypename(n)) {
		CarbonToken third = peekn(2, p);
		if (third.type == TokenLeftParen)
			return (CarbonStmt *) funcDeclaration(p);
		return (CarbonStmt *) varDeclaration(p);
	} else if (n.type == TokenVoid) {
		return (CarbonStmt *) funcDeclaration(p);
	} else {
		next(p);
		error(previous(p), "Only declarations are allowed in top-level code.",
			  p);
		return NULL;
	}
}

CarbonExpr *carbon_parseExpression(CarbonParser *p) {
	return expression(p);
}

static CarbonStmtFunc *funcDeclaration(CarbonParser *p) {
	CarbonToken returnType = next(p);
	CarbonToken identifier = next(p);
	consume(TokenLeftParen, "Expected '(' after function identifier.", p);
	CarbonStmtFunc *func = carbon_newFuncStmt(returnType, identifier);
	bool tooMany = false;
	if (!match(TokenRightParen, p)) {
		do {
			CarbonToken type = next(p);
			CarbonToken name = next(p);
			if (!isTypename(type) && type.type != TokenVoid) {
				error(type, "Type name expected", p);
			} else {
				bool defined = false;
				for (uint8_t i = 0; i < func->arity; i++) {
					CarbonToken arg = func->arguments[i].name;
					if (arg.length == name.length &&
						memcmp(arg.lexeme, name.lexeme, arg.length) == 0) {
						defined = true;
						error(name,
							  "Function argument with that name has already "
							  "been defined",
							  p);
						break;
					}
				}
				if (!defined) {
					if (func->arity == 255) {
						if (!tooMany)
							error(name,
								  "Functions can have a maximum of "
								  "255 arguments.",
								  p);
						tooMany = true;
					} else {
						if (func->arity == func->argumentCapacity) {
							uint32_t oldSize = sizeof(struct carbon_arg) *
											   func->argumentCapacity;
							if (func->argumentCapacity == 0)
								func->argumentCapacity = 4;
							else
								func->argumentCapacity *= 2;
							uint32_t newSize = func->argumentCapacity *
											   sizeof(struct carbon_arg);
							func->arguments = carbon_reallocate(
								oldSize, newSize, func->arguments);
						}
						struct carbon_arg arg = {type, name};
						func->arguments[func->arity] = arg;
						func->arity++;
					}
				}
			}
		} while (match(TokenComma, p));
		consume(TokenRightParen, "Expected ')' after function argument list",
				p);
	}
	consume(TokenColon, "Expected ':' after function declaration", p);
	while (peek(p).type != TokenEnd) {
		if (peek(p).type == TokenEOF) {
			errorAtCurrent("Unexpected EOF inside function", p);
			return func;
		}
		CarbonStmt *stmt = statement(p);
		if (stmt != NULL) {
			if (stmt->type == StmtFunc) {
				CarbonStmtFunc *f = (CarbonStmtFunc *) stmt;
				error(f->identifier, "Nested functions are not supported", p);
			} else {
				carbon_stmtList_add(&func->statements, stmt);
			}
		}
	}
	consume(TokenEnd, "Expected 'end' after function body", p);
	func->end = previous(p).line;
	return func;
}

static CarbonStmtReturn *returnStatement(CarbonParser *p) {
	CarbonToken tok = next(p);
	CarbonStmtReturn *ret = carbon_newReturnStmt(tok);
	if (match(TokenEOS, p)) {
		return ret;
	}
	ret->expression = expression(p);
	consume(TokenEOS, "Expected EOS after return statement", p);
	return ret;
}

static CarbonStmtPrint *printStatement(CarbonParser *p) {
	CarbonToken print = next(p);
	consume(TokenLeftParen, "Expected '(' after print", p);
	CarbonExpr *expr = expression(p);
	consume(TokenRightParen, "Expected ')' after print expression", p);
	consume(TokenEOS, "Expected EOS after print statement", p);
	return carbon_newPrintStmt(expr, print);
}
static CarbonStmtExpr *expressionStatement(CarbonParser *p) {
	CarbonExpr *expr = expression(p);
	consume(TokenEOS, "Expected EOS after expression statement", p);
	if (p->currentToken == 0)
		return NULL;
	return carbon_newExprStmt(expr, previous(p));
}

static CarbonStmtVarDec *varDeclaration(CarbonParser *p) {
	CarbonToken type = next(p);
	consume(TokenIdentifier, "Expected identifier after variable declaration",
			p);
	CarbonToken identifier = previous(p);
	switch (peek(p).type) {
		case TokenEOS:
			next(p);
			return carbon_newVarDecStmt(identifier, type, NULL);
		case TokenEquals:
			next(p);
			CarbonExpr *init = expression(p);
			consume(TokenEOS, "Expected EOS after variable initializer", p);
			return carbon_newVarDecStmt(identifier, type, init);
		default:
			errorAtCurrent(
				"Unexpected token after variable declaration identifier", p);
			return NULL;
	}
}

static CarbonExpr *assignment(CarbonParser *p) {
	CarbonExpr *target = equality(p);
	if (match(TokenEquals, p)) {
		CarbonToken equals = previous(p);
		CarbonExpr *value = assignment(p);
		switch (target->type) {
			case ExprVar: {
				CarbonExprVar *var = (CarbonExprVar *) target;
				carbon_freeExpr(target);
				return (CarbonExpr *) carbon_newAssignmentExpr(var->token,
															   value);
			}
			default:
				error(equals, "Invalid assignment target", p);
		}
	}
	return target;
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
		if (isTypename(peekn(1, p))) {
			next(p);
			CarbonToken to = next(p);
			next(p);
			return (CarbonExpr *) carbon_newCastExpr(to, unary(p));
		}
	}
	return postfix(p);
}

static CarbonExpr *postfix(CarbonParser *p) {
	CarbonExpr *expr = primary(p);
	if (match(TokenLeftParen, p)) {
		CarbonExprCall *call = carbon_newCallExpr(expr, previous(p).line);
		bool tooMany = false;
		if (!match(TokenRightParen, p)) {
			do {
				if (!tooMany && call->arity == 255) {
					errorAtCurrent(
						"Functions can have a maximum of 255 arguments.", p);
					tooMany = true;
				}
				CarbonExpr *e = expression(p);
				if (tooMany) {
					carbon_freeExpr(e);
					continue;
				}
				if (e != NULL) {
					if (call->arity == call->argumentCapacity) {
						uint32_t oldSize =
							call->argumentCapacity * sizeof(CarbonExpr);
						if (call->argumentCapacity == 0)
							call->argumentCapacity = 8;
						else
							call->argumentCapacity *= 2;
						uint32_t newSize =
							call->argumentCapacity * sizeof(CarbonExpr *);
						call->arguments = carbon_reallocate(oldSize, newSize,
															call->arguments);
					}
					call->arguments[call->arity] = e;
					call->arity++;
				}
			} while (match(TokenComma, p));
			consume(TokenRightParen,
					"Expected ')' after function call arguments.", p);
		}
		return (CarbonExpr *) call;
	}
	return expr;
}

static CarbonExpr *primary(CarbonParser *p) {
	switch (peek(p).type) {
		case TokenStringLiteral:
		case TokenInteger:
		case TokenDecimal:
		case TokenTrue:
		case TokenFalse:
			return (CarbonExpr *) carbon_newLiteralExpr(next(p));
		case TokenIdentifier:
			return (CarbonExpr *) carbon_newVarExpr(next(p));

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
