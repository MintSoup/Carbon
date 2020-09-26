#include <stdio.h>
#include <stdlib.h>
#include "ast/carbon_expressions.h"
#include "carbon_lexer.h"
#include "carbon_parser.h"
#include "carbon_token.h"
#include "utils/carbon_commons.h"

int main(int argc, char *argv[]) {
	if (argc != 2) {
		puts("Usage: carbon <filename>");
		return 1;
	}
	FILE *f = fopen(argv[1], "rb");

	if (!f) return 2;

	fseek(f, 0, SEEK_END);
	size_t size = ftell(f);
	fseek(f, 0, SEEK_SET);
	char *t = malloc(size + 1);
	if (!t) return 3;
	t[size] = 0;
	fread(t, size, 1, f);
	fclose(f);

	CarbonParser p;
	CarbonLexer l = carbon_initLexer(t, size);
	carbon_initParser(&p, &l);
	if (p.panic) return 1;
	CarbonExpr *e = carbon_parseExpression(&p);

	carbon_printExpr(e);
	carbon_freeExpr(e);

	free(t);

	return 0;
}
