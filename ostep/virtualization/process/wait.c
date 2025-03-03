#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

int main(int argc, char *argv[])
{
    printf("once pid:%d\n", (int) getpid());

    int rc = fork();
    printf("pid:%d rc:%d\n",(int) getpid(), rc);

    int wc = wait(NULL);
    printf("pid:%d wc:%d\n",(int) getpid(), wc);
}
