#ifndef SHELL_H
#define SHELL_H

#include <stdio.h>
#include <unistd.h>

void shell()
{
    printf("prompt\n");

    int rc = fork();
    if(rc == 0)
    {
        close(stdout);
        open("out.txt");
        exec("cmd");
    }
    else
    {
        int wc = wait(NULL);
        printf("prompt\n");
    }
}

#endif