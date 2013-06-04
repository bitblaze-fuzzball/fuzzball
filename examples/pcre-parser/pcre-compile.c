#include <stdio.h>
#include <string.h>
#include <pcre.h>

char pattern[4096] = "a|b";

int main(int argc, char **argv) {
    pcre *re;
    const char *error;
    int error_offset;
    re = pcre_compile("[ab]|c", 0, &error, &error_offset, NULL);
    printf("About to compile\n"); fflush(stdout);
    re = pcre_compile(pattern, 0, &error, &error_offset, NULL);
    if (re) {
	printf("Compilation succeeded\n");  fflush(stdout);
    } else {
	printf("Compilation failed: %s at %d\n", error, error_offset);
	fflush(stdout);
    }
    return 0;
}
