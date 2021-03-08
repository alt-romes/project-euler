#include <stdlib.h>
#include <stdio.h>

static int* get_primes_smaller_than(int n) {

    int* primes = malloc(n * sizeof(int));
    primes[0] = 0;
    primes[1] = 0;
    for (int i=2; i<n; i++)
        primes[i] = i;

    int p = 2;
    while (p*p <= n) { // p <= sqrt(n)

        // If not zero then it's a prime, and we remove all its multiples bc they aren't primes
        if (primes[p] != 0)
            for (int i=p+p; i<n; i+=p)
                primes[i] = 0;

        p++;

    }

    return primes;
    
}

static void solution_10() {

    int* primes = get_primes_smaller_than(2000000);

    long sum = 0;

    for (int i=0; i<2000000; i++)
        sum += primes[i];

    free(primes);

    printf("%ld\n", sum);

}

int main(int argc, char *argv[]) {

    return 0;
}
