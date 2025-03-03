#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/wait.h>

int main(int argc, char *argv[])
{
    int fd = open("./test", O_CREAT|O_WRONLY|O_TRUNC, S_IRWXU);

    int rc = fork();

    if(rc == 0)
    {
        write(fd, "123",3);
    }
    else if(rc > 0)
    {
        write(fd, "456",3);
        wait(NULL);
        close(fd);
    }
    return 0;
}
