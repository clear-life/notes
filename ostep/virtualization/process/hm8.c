#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

int main(int argc, char *argv[])
{
    int fd[2];
    char buf[10];

    int pc = pipe(fd);

    int rc = fork();

    if(rc == 0)
    {
        close(fd[1]);
        read(fd[0], buf, 10);
        close(fd[0]);    
        printf("%s world!\n", buf);
    }
    else if(rc > 0)
    {
        int rc = fork();
        if(rc == 0)
        {
            close(fd[0]);
            write(fd[1], "hello", 6);
            close(fd[1]);
        }
        else
            wait(NULL);
    }
    return 0;
}
