#include <stdio.h>
#include <stdlib.h>

#include "carbon_lexer.h"
#include "carbon_token.h"
#include "utils/carbon_commons.h"
#include "vm/carbon_chunk.h"

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
	CarbonLexer lexer = carbon_initLexer(t, size);
	while (1) {
		CarbonToken t = carbon_scanToken(&lexer);
		if (t.type == TokenEOF)
			break;
		else if (t.type == TokenError) {
			printf("Line %d: %s %c\n", t.line, t.lexeme, *(lexer.current - 1));
		} else
			printf("%.*s\n", t.length, t.lexeme);
	}

	free(t);

	return 0;
}
