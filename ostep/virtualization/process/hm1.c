#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

int main(int argc, char *argv[])
{
    int x = 0;
    printf("%d\n", x);

    int rc = fork();
    if(rc == 0)
    {
        printf("child %d\n", x);
        x++;
        printf("child %d\n", x);
    }
    else if(rc > 0)
    {
        printf("parent %d\n", x);
        x++;
        printf("parent %d\n", x);
    }

    return 0;
}
