#include <stdio.h>
#include <stdlib.h>

float a[10];

int main(int argc, char **argv) {
    int i;
    for (i = 0; i < 10; i++) {
	if (a[i] < 0.0) {
	    printf("Negative\n");
	    exit(1);
	}
	if (a[1] > 1.0) {
	    printf("More than one\n");
	    exit(2);
	}
    }
    float last = a[0];
    float total = a[0];
    for (i = 1; i < 10; i++) {
	if (a[i] > last) {
	    printf("Increasing\n");
	    exit(3);
	}
	total += a[i];
	if (total > 1.0) {
	    printf("Too big\n");
	    exit(4);
	}
	last = a[i];
    }
    if (total < 1.0) {
	printf("Too small\n");
	exit(5);
    } else if (total == 1.0) {
	printf("Just right\n");
	exit(0);
    } else if (total > 1.0) {
	printf("Can't happen!\n");
	exit(42);
    } else {
	printf("Comparison failure\n");
	exit(6);
    }
}
