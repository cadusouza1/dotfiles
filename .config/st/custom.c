#include "st.h"
#include <stdio.h>
#include <string.h>

void exec_nvim(const Arg *arg) {
    char cmd[256];
    snprintf(cmd, 256 - strlen("nvim %s \n"), "nvim %s \n", arg->s);
    ttywrite(cmd, strlen(cmd) * sizeof(char), 0);
}
