#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

static char *str_alloc(size_t n) {
    char *s = (char *)malloc(n + 1);
    if (!s) { perror("malloc"); exit(1); }
    s[0] = '\0';
    return s;
}

static char *str_copy(const char *src) {
    size_t n = strlen(src);
    char *s = (char *)malloc(n + 1);
    if (!s) { perror("malloc"); exit(1); }
    memcpy(s, src, n + 1);
    return s;
}

static void str_free(char *s) {
    free(s);
}

/* like your str(factor) */
static char *str_from_int(long long v) {
    char buf[64];
    int len = snprintf(buf, sizeof(buf), "%lld", v);
    if (len < 0) { perror("snprintf"); exit(1); }
    char *s = (char *)malloc((size_t)len + 1);
    if (!s) { perror("malloc"); exit(1); }
    memcpy(s, buf, (size_t)len + 1);
    return s;
}

/* concat_free(x, y): concat and free both inputs */
static char *concat_free(char *x, char *y) {
    size_t nx = strlen(x), ny = strlen(y);
    char *r = (char *)malloc(nx + ny + 1);
    if (!r) { perror("malloc"); exit(1); }
    memcpy(r, x, nx);
    memcpy(r + nx, y, ny + 1);
    free(x);
    free(y);
    return r;
}

/* ---- minimal line reader (keeps input as typed, minus trailing '\n') ---- */

static char *read_line(void) {
    size_t cap = 128, len = 0;
    char *buf = (char *)malloc(cap);
    if (!buf) { perror("malloc"); exit(1); }

    int c;
    while ((c = fgetc(stdin)) != EOF) {
        if (len + 1 >= cap) {
            cap = cap * 2;
            char *nb = (char *)realloc(buf, cap);
            if (!nb) { free(buf); perror("realloc"); exit(1); }
            buf = nb;
        }
        if (c == '\n') break;
        buf[len++] = (char)c;
    }

    if (len == 0 && c == EOF) {
        free(buf);
        return NULL; // no input
    }

    buf[len] = '\0';
    return buf;
}

/* ---- smallest_factor (straight port) ---- */

static long long smallest_factor(long long number) {
    long long i = 2;
    while (i < number) {
        if (number % i == 0) {
            return i;
        }
        i = i + 1;
    }
    return number;
}

/* ---- main ---- */

int main(void) {
    printf("enter a positive integer:\n");
    char *input_str = read_line();
    if (!input_str) {
        puts("string is not a number");
        return 1;
    }

    // Parse integer like parse_int; treat trailing junk as failure.
    char *end = NULL;
    // strtoll skips leading whitespace; that's fine.
    long long number = strtoll(input_str, &end, 10);
    // Valid only if we consumed something and there is no trailing non-space.
    if (end == input_str) {
        puts("string is not a number");
        free(input_str);
        return 1;
    }
    // If there are trailing characters, they must be all spaces.
    for (; *end; ++end) {
        if (!isspace((unsigned char)*end)) {
            puts("string is not a number");
            free(input_str);
            return 1;
        }
    }
    if (number < 0) {
        puts("string is not a number");
        free(input_str);
        return 1;
    }

    char *result = str_alloc(0);
    while (number > 1) {
        long long factor = smallest_factor(number);
        char *factor_str = str_from_int(factor);
        if (result[0] != '\0') {
            result = concat_free(result, str_copy(" * "));
        }
        result = concat_free(result, factor_str);
        number = number / factor;
    }

    // Match your print sequence
    fputs(input_str, stdout);
    str_free(input_str);
    fputs(" = ", stdout);
    puts(result);

    str_free(result);
    return 0;
}

