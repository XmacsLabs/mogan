
#include <stdio.h>
#include "scheme.hpp"

int main (int argc, char *argv[]) {
    printf("argc: %d\n", argc);
    ASSERT (argc == 3, "argc must be 2");
    printf ("/----------------------------------------\n \
            this is a tm2html tool\n    \
            ----------------------------------------/\n");
    
}
