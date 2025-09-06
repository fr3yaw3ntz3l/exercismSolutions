#include "grains.h"

uint64_t square(uint8_t index) {
    if (index > 0 && index <= 64) {
        uint64_t res = 1;
        for (int i = 1; i < index; i++) {
            res *= 2;
        }
        return res;
    }
    return 0;
}

uint64_t total(void) {
    uint64_t sum = 0;
    for (int i = 1; i <= 64; i++) {
        sum += square(i);
    }
    return sum;
}