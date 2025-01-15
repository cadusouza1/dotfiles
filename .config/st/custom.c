#include "st.h"
#include <stddef.h>
#include <unistd.h>

void exec_command(const Arg *arg) {
    const char *argv = {arg->s, NULL};
    execvp(arg->s, (char *const *)argv);
}
