#include <stdio.h>
#include <stdlib.h>

#include "re.h"


int main(int argc, char **argv) {
    regex_compile_ctx_t result;

    regexCompileCtxInit(&result, argv[1]);
    if(argc > 1) {
        if(regexCompile(&result) != eCompileOk) {
            printf("Compile failed: %s", regexGetCompileStatusStr(result.status));
        }
    }

    return 0;
}
