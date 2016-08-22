#include <stdio.h>

typedef union {
    int   i;
    float f;
} fi;

float getlo(int n) {
    fi x;
    x.i = n;
    return x.f;
}

int gethi(float f) {
    fi x;
    x.f = f;
    return x.i;
}

//int main(int argc, char const* argv[])
//{
//    fi x;
//    printf("%d\n", gethi(1.0));
//    return 0;
//}


