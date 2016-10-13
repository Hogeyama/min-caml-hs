#include <stdio.h>

typedef union {
    int    i[2];
    double d;
} dbl;

int gethi(double f) {
    dbl x;
    x.d = f;
    return x.i[0];
}
int getlo(double f) {
    dbl x;
    x.d = f;
    return x.i[1];
}

//int main(int argc, char const* argv[])
//{
//    fi x;
//    printf("%d\n", gethi(1.0));
//    return 0;
//}


