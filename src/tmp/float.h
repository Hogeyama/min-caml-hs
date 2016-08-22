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



//value gethi(value v) {
//  dbl d;
//  d.d = Double_val(v);
//  return copy_int32(d.i[0]);
//}
//
//value getlo(value v) {
//  dbl d;
//  d.d = Double_val(v);
//  return copy_int32(d.i[1]);
//}
