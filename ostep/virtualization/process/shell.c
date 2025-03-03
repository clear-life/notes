#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>

int main(int argc, char *argv[])
{
    int rc = fork();
    if(rc == 0)
    {
        close(STDOUT_FILENO);
        open("./shell.output", O_CREAT|O_WRONLY|O_TRUNC, S_IRWXU);

        char *args[3];
        args[0] = "wc";
        args[1] = "shell.c";
        args[2] = NULL;

        execvp(args[0], args);
    }
    else if(rc > 0)
    {
        int wc = wait(NULL);
    }
    return 0;
}
