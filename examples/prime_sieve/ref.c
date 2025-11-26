#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int sieve(int n) {
    unsigned char *primes = malloc(n);
    if (!primes) {
        fprintf(stderr, "allocation failed\n");
        exit(1);
    }

    // initialize all entries to 1
    for (int i = 0; i < n; i++) {
        primes[i] = 1;
    }

    for (int p = 2; p * p < n; p++) {
        if (primes[p] == 1) {
            for (int j = p * p; j < n; j += p) {
                primes[j] = 0;
            }
        }
    }

    int count = 0;
    for (int i = 2; i < n; i++) {
        if (primes[i] == 1)
            count++;
    }

    free(primes);
    return count;
}

int main(void) {
    int limit = 50000000;
    printf("counting primes up to %d\n", limit);

    int c = sieve(limit);

    printf("answer: %d\n", c);
    return 0;
}
