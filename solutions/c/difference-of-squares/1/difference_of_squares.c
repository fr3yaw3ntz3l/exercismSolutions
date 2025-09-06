#include "difference_of_squares.h"

unsigned int square_of_sum(unsigned int n) {
    unsigned int i, res = 0;
    for (i = 1; i <= n; i++) {
        res = res + i;
    }
    return res * res;
}

unsigned int sum_of_squares(unsigned int n) {
    unsigned int i, res = 0;
    for (i = 1; i <= n; i++) {
        res = res + i * i;
    }
    return res;
}

unsigned int difference_of_squares(unsigned int n) {
    return square_of_sum(n) - sum_of_squares(n);
}