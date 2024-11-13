#include <stdio.h>
#include <HsFFI.h>

void generate(char* c, int w, int h, char* o);

void hs_init_c() {
    int argc = 2;
    char *argv[] = { (char *)"+RTS", (char *)"-A32m", NULL };
    char **pargv = argv;

    // Initialize Haskell runtime
    hs_init(&argc, &pargv);
}

void hs_exit_c() {
    hs_exit();
}

int generate_c(char* c, int w, int h, char* o) {
    generate(c, w, h, o);
    return 0;
}

int main(int argc, char** argv) {
    hs_init_c();
    generate_c("gol", 400, 400, "simple.gif");
    hs_exit();
    return 0;
}
