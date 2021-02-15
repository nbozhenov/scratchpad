#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    char buffer[128];
    int i = 1;
    for (; i < argc; ++i) {
        snprintf(buffer, 128, "killall %s", argv[i]);
        system(buffer);
    }
    return 0;
}
