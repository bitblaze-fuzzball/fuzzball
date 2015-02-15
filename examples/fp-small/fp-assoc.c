#include <stdio.h>
#include <stdlib.h>

double a, b, c;

int main(int argc, char **argv) {
    if (argc == 4) {
	a = atof(argv[1]);
	b = atof(argv[2]);
	c = atof(argv[3]);
    } else if (argc == 1) {
	/* For FuzzBALL's use */
    } else {
	fprintf(stderr, "Usage: fp-assoc a b c\n");
	exit(1);
    }
    double s1 = a + (b + c);
    double s2 = (a + b) + c;
    if (s1 == s2) {
	printf("Associative\n");
    } else {
	printf("Not associative\n");
    }
    printf("%a + (%a + %a) = %a\n", a, b, c, s1);
    printf("(%a + %a) + %a = %a\n", a, b, c, s2);
    return 0;
}
