#include <stdio.h>
#include <stdlib.h>
#include <proj.h>

int main() {
    printf("%d.%d.%d\n", PROJ_VERSION_MAJOR, PROJ_VERSION_MINOR, PROJ_VERSION_PATCH);
    exit(0);
}
