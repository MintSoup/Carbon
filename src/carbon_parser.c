#include "ast/carbon_expressions.h"
#include "ast/carbon_statements.h"
#include "carbon_lexer.h"
#include "carbon_token.h"
#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "carbon_parser.h"
#include "utils/carbon_memory.h"
#include "ast/carbon_statements.h"
#include "vm/carbon_vm.h"
#include <string.h>
#include <stdio.h>
#include <strings.h>

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
		case ErrorToken: {
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
			case TokenGenerator:
			case TokenArray:
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
	parser->innermostLoop = NULL;
	parser->tokens =
		carbon_reallocate(0, parser->totalTokens * sizeof(CarbonToken), NULL);
	while (true) {
		CarbonToken current;
		CarbonToken prev =
			{}; // To suppress the "may be uninitialized" warning that otherwise
				// shows up on line 151, even though the variable is guaranteed
				// to be inited if used
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
			if (current.type == ErrorToken) {
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
		case TokenVoid:
		case TokenFunction:
		case TokenArray:
		case TokenGenerator:
			return true;
		default:
			return false;
	}
}

static CarbonStmtBreak *breakStatement(CarbonParser *p);
static CarbonStmtIf *ifStatement(CarbonParser *p);
static CarbonStmtWhile *whileStatement(CarbonParser *p);
static CarbonStmtFunc *funcDeclaration(CarbonTypename returnType,
									   CarbonParser *p);
static CarbonStmtReturn *returnStatement(CarbonParser *p);
static CarbonStmtPrint *printStatement(CarbonParser *p);
static CarbonStmtExpr *expressionStatement(CarbonParser *p);
static CarbonStmtVarDec *varDeclaration(CarbonTypename type, CarbonParser *p);

static CarbonExpr *logicalOr(CarbonParser *p);
static CarbonExpr *logicalAnd(CarbonParser *p);
static CarbonExpr *assignment(CarbonParser *p);
static CarbonExpr *equality(CarbonParser *p);
static CarbonExpr *comparison(CarbonParser *p);
static CarbonExpr *addition(CarbonParser *p);
static CarbonExpr *multiplication(CarbonParser *p);
static CarbonExpr *unary(CarbonParser *p);
static CarbonExpr *postfix(CarbonParser *p);
static CarbonExpr *primary(CarbonParser *p);

static CarbonExpr *expression(CarbonParser *p) {
	CarbonToken first = peek(p);
	CarbonExpr *expr = assignment(p);
	if (expr != NULL)
		expr->first = first;
	return expr;
}

static CarbonTypename parseType(CarbonParser *p) {
	CarbonToken base = next(p);
	CarbonTypename tn = {
		.base = {TokenNone}, .templateCount = 0, .templates = NULL};
	if (!isTypename(base)) {
		error(base, "Expected a type", p);
		return tn;
	}
	tn.base = base;

	if (peek(p).type != TokenLessThan) {
		return tn;
	}
	next(p);

	bool tooMany = false;

	do {
		if (tn.templateCount == 255) {
			if (!tooMany) {
				errorAtCurrent("Cannot have more than 255 templates.", p);
				tooMany = true;
			}
			carbon_freeTypename(parseType(p));
		} else {
			uint32_t oldSize = tn.templateCount * sizeof(CarbonTypename);
			uint32_t newSize = ++tn.templateCount * sizeof(CarbonTypename);
			tn.templates = carbon_reallocate(oldSize, newSize, tn.templates);
			tn.templates[tn.templateCount - 1] = parseType(p);
		}
	} while (match(TokenComma, p));
	consume(TokenGreaterThan, "Expected closing '>' after template.", p);
	p->panic = false;
	return tn;
}

static CarbonStmt *statement(CarbonParser *p) {
	if (p->panic)
		sync(p);
	CarbonToken n = peek(p);
	if (n.type == TokenEOF)
		return NULL;
	else if (n.type == TokenPrint)
		return (CarbonStmt *) printStatement(p);
	else if (isTypename(n) && n.type != TokenVoid)
		return (CarbonStmt *) varDeclaration(parseType(p), p);
	else if (n.type == TokenReturn)
		return (CarbonStmt *) returnStatement(p);
	else if (n.type == TokenIf)
		return (CarbonStmt *) ifStatement(p);
	else if (n.type == TokenWhile)
		return (CarbonStmt *) whileStatement(p);
	else if (n.type == TokenBreak || n.type == TokenContinue)
		return (CarbonStmt *) breakStatement(p);
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
		CarbonTypename type = parseType(p);
		CarbonToken paren = peekn(1, p);
		if (n.type == TokenVoid || paren.type == TokenLeftParen)
			return (CarbonStmt *) funcDeclaration(type, p);
		return (CarbonStmt *) varDeclaration(type, p);
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

static CarbonStmtBreak *breakStatement(CarbonParser *p) {
	CarbonToken tok = next(p);
	char *msg;
	if (previous(p).type == TokenBreak)
		msg = "Expected EOS after break";
	else
		msg = "Expected EOS after 'continue'";
	consume(TokenEOS, msg, p);
	if (p->innermostLoop == NULL)
		error(tok, "Cannot have break/continue statements outside loops.", p);
	else
		p->innermostLoop->hasBreak = true;
	return carbon_newBreakStmt(tok);
}

static CarbonStmtBlock *block(CarbonParser *p, char *cmsg, char *eofmsg,
							  bool isLoop) {
	consume(TokenColon, cmsg, p);

	CarbonStmtBlock *block = carbon_newBlockStmt();
	CarbonStmtBlock *outerLoop;
	if (isLoop) {
		outerLoop = p->innermostLoop;
		p->innermostLoop = block;
	}
	while (peek(p).type != TokenEnd && peek(p).type != TokenElif &&
		   peek(p).type != TokenElse) {
		if (peek(p).type == TokenEOF) {
			errorAtCurrent(eofmsg, p);
			return block;
		}
		CarbonStmt *stmt = statement(p);
		if (stmt != NULL) {
			if (stmt->type == StmtVarDec)
				block->locals++;
			carbon_stmtList_add(&block->statements, stmt);
		}
	}

	if (isLoop) {
		p->innermostLoop = outerLoop;
	}

	// We deliberately do not consume the 'end' token because if statement
	// blocks can end with an 'else' and 'elif'
	return block;
}

static CarbonStmtIf *ifStatement(CarbonParser *p) {
	CarbonToken tok = next(p);
	CarbonStmtIf *stmt = carbon_newIfStmt(expression(p), tok);
	CarbonStmtIf *last = stmt;
	stmt->then = (CarbonStmt *) block(p, "Expected ':' after if statement",
									  "Unexpected EOF inside if block", false);
	while (match(TokenElif, p) || match(TokenElse, p)) {
		CarbonToken tok = previous(p);
		if (tok.type == TokenElif) {
			CarbonStmtIf *elif = carbon_newIfStmt(expression(p), tok);
			elif->then = (CarbonStmt *) block(
				p, "Expected ':' after elif",
				"Unexpected EOF inside elif block ", false);
			last->notThen = (CarbonStmt *) elif;
			last->elseToken = tok;
			last = elif;
		} else {
			last->notThen =
				(CarbonStmt *) block(p, "Expected : after else",
									 "Unexpected EOF inside else block", false);
			last->elseToken = tok;
			break;
		}
	}
	consume(TokenEnd, "Expected 'end' after if block", p);
	return stmt;
}

static CarbonStmtWhile *whileStatement(CarbonParser *p) {
	CarbonToken tok = next(p);
	CarbonStmtWhile *w = carbon_newWhileStmt(expression(p), tok);
	w->body = block(p, "Expected ':' after while statement",
					"Unexpected EOF inside while block", true);
	consume(TokenEnd, "Expected 'end' after while block", p);
	return w;
}

static CarbonStmtFunc *funcDeclaration(CarbonTypename returnType,
									   CarbonParser *p) {
	CarbonToken identifier = next(p);
	consume(TokenLeftParen, "Expected '(' after function identifier.", p);
	CarbonStmtFunc *func = carbon_newFuncStmt(returnType, identifier);
	bool tooMany = false;
	if (!match(TokenRightParen, p)) {
		do {
			CarbonTypename type = parseType(p);
			CarbonToken name = next(p);
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
						uint32_t oldSize =
							sizeof(struct carbon_arg) * func->argumentCapacity;
						if (func->argumentCapacity == 0)
							func->argumentCapacity = 4;
						else
							func->argumentCapacity *= 2;
						uint32_t newSize =
							func->argumentCapacity * sizeof(struct carbon_arg);
						func->arguments = carbon_reallocate(oldSize, newSize,
															func->arguments);
					}
					struct carbon_arg arg = {type, name};
					func->arguments[func->arity] = arg;
					func->arity++;
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
			carbon_stmtList_add(&func->statements, stmt);
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

static CarbonStmtVarDec *varDeclaration(CarbonTypename type, CarbonParser *p) {
	if (type.base.type == TokenVoid) {
		error(type.base, "Variable cannot have a void.", p);
	}
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
			carbon_freeTypename(type);
			return NULL;
	}
}

static CarbonExpr *assignment(CarbonParser *p) {
	CarbonExpr *target = logicalOr(p);
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
			case ExprIndex:
				return (CarbonExpr *) carbon_newIndexAssignmentExpr(
					(CarbonExprIndex *) target, value, equals);

			default:
				error(equals, "Invalid assignment target", p);
		}
	}
	return target;
}

static CarbonExpr *logicalOr(CarbonParser *p) {
	CarbonExpr *expr = logicalAnd(p);
	while (match(TokenOr, p)) {
		CarbonToken tok = previous(p);
		expr = (CarbonExpr *) carbon_newBinaryExpr(expr, logicalAnd(p), tok);
	}
	return expr;
}

static CarbonExpr *logicalAnd(CarbonParser *p) {
	CarbonExpr *expr = equality(p);
	while (match(TokenAnd, p)) {
		CarbonToken tok = previous(p);
		expr = (CarbonExpr *) carbon_newBinaryExpr(expr, equality(p), tok);
	}
	return expr;
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
	while (match(TokenStar, p) || match(TokenSlash, p) ||
		   match(TokenPercent, p)) {
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
	if (peek(p).type == TokenLeftParen) {
		if (isTypename(peekn(1, p)) && peekn(1, p).type != TokenVoid) {
			next(p);
			CarbonTypename to = parseType(p);
			consume(TokenRightParen, "Expected ')' after cast type", p);
			return (CarbonExpr *) carbon_newCastExpr(to, unary(p));
		}
	}
	return postfix(p);
}

static CarbonExpr *postfix(CarbonParser *p) {
	CarbonExpr *expr = primary(p);
	while (match(TokenLeftParen, p) || match(TokenLeftBracket, p)) {
		if (previous(p).type == TokenLeftParen) {
			CarbonExprCall *call = carbon_newCallExpr(expr, previous(p).line);
			bool tooMany = false;
			if (!match(TokenRightParen, p)) {
				do {
					if (!tooMany && call->arity == 255) {
						errorAtCurrent(
							"Functions can have a maximum of 255 arguments.",
							p);
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
								call->argumentCapacity * sizeof(CarbonExpr *);
							if (call->argumentCapacity == 0)
								call->argumentCapacity = 8;
							else
								call->argumentCapacity *= 2;
							uint32_t newSize =
								call->argumentCapacity * sizeof(CarbonExpr *);
							call->arguments = carbon_reallocate(
								oldSize, newSize, call->arguments);
						}
						call->arguments[call->arity] = e;
						call->arity++;
					}
				} while (match(TokenComma, p));
				consume(TokenRightParen,
						"Expected ')' after function call arguments.", p);
			}
			expr = (CarbonExpr *) call;
		} else if (previous(p).type == TokenLeftBracket) {
			CarbonToken bracket = previous(p);
			CarbonExpr *i = expression(p);
			consume(TokenRightBracket, "Expected ']' to close index expression",
					p);
			expr = (CarbonExpr *) carbon_newIndexExpr(expr, i, bracket);
		}
	}
	return expr;
}

static CarbonExprArray *array(CarbonParser *p) {
	CarbonToken t = next(p);
	CarbonExprArray *arr = carbon_newArrayExpr(t);
	CarbonExpr *first = expression(p);
	if (match(TokenDotDot, p)) {
		arr->imethod = ImethodGenerator;
		CarbonExpr *second = expression(p);

		if (match(TokenColon, p)) {
			CarbonExpr *d = expression(p);
			consume(TokenRightBracket, "Expected ']' to close iterator object.",
					p);
			arr->capacity = 3;
			arr->count = 3;
			arr->members = carbon_reallocate(0, 3 * sizeof(CarbonExpr *), NULL);
			arr->members[0] = first;
			arr->members[1] = second;
			arr->members[2] = d;
			return arr;
		}
		consume(TokenRightBracket, "Expected ']' to close iterator object", p);
		arr->capacity = 2;
		arr->count = 2;
		arr->members = carbon_reallocate(0, 2 * sizeof(CarbonExpr *), NULL);
		arr->members[0] = first;
		arr->members[1] = second;
		return arr;
	}
	arr->imethod = ImethodStandard;

	arr->capacity = 8;
	arr->count = 1;

	arr->members = carbon_reallocate(0, 8 * sizeof(CarbonExpr *), NULL);
	arr->members[0] = first;
	while (match(TokenComma, p)) {
		if (arr->count == arr->capacity) {
			uint32_t oldSize = arr->capacity * sizeof(CarbonExpr *);
			arr->capacity *= 2;
			uint32_t newSize = arr->capacity * sizeof(CarbonExpr *);
			arr->members = carbon_reallocate(oldSize, newSize, arr->members);
		}
		arr->members[arr->count++] = expression(p);
	}
	consume(TokenRightBracket, "Expected ']' to close array", p);
	return arr;
}

static CarbonExprArray *arrayinit(CarbonParser *p) {
	CarbonExprArray *arr = carbon_newArrayExpr(next(p));

	arr->imethod = ImethodContracted;
	arr->capacity = 2;
	arr->count = 2;
	arr->members = carbon_reallocate(0, 2 * sizeof(CarbonExpr *), NULL);

	arr->type = parseType(p);

	consume(TokenComma, "Expected ',' after array init type", p);
	arr->members[0] = expression(p);

	consume(TokenComma, "Expected ',' after array init size", p);
	arr->members[1] = expression(p);

	consume(TokenRightAInit, "Expected ']' to close array", p);
	return arr;
}

static CarbonExpr *primary(CarbonParser *p) {
	switch (peek(p).type) {
		case TokenStringLiteral:
		case TokenInteger:
		case TokenDecimal:
		case TokenTrue:
		case TokenFalse:
		case TokenNull:
			return (CarbonExpr *) carbon_newLiteralExpr(next(p));
		case TokenIdentifier:
			return (CarbonExpr *) carbon_newVarExpr(next(p));
		case TokenLeftBracket:
			return (CarbonExpr *) array(p);
		case TokenLeftAInit:
			return (CarbonExpr *) arrayinit(p);

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
