#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/wait.h>

int main(int argc, char *argv[])
{
    int rc = fork();
    if(rc == 0)
    {
        printf("child pid:%d\n", (int) getpid());

        char *args[3];
        args[0] = "wc";
        args[1] = "exec.c";
        args[2] = NULL;

        execvp(args[0], args);
    }
    else if(rc > 0)
    {
        int wc = wait(NULL);
        printf("parent of %d(wc:%d) pid:%d\n", rc, wc, (int) getpid());
    }

    return 0;
}
