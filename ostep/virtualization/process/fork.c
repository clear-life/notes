#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

int main(int argc, char *argv[])
{
    printf("once pid:%d\n", (int) getpid());

    int rc = fork();
    if(rc < 0)
    {
        fprintf(stderr, "failed\n");
        exit(1);
    }
    else if(rc == 0)
        printf("child pid:%d\n", (int) getpid());
    else
    {
        int wc = wait(NULL);
        printf("parent(pid:%d) of %d(wc:%d)\n", (int) getpid(), rc, wc);
    }
}