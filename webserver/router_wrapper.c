#include <stdio.h>
#include <stdlib.h>
#include <string.h>

__attribute__((visibility("default"))) void router_wrapper(
    const char *method, const char *url, const char *body, char *response, int response_size)
{
    char command[1024];
    FILE *fp;

    snprintf(command, sizeof(command), "./router.sh %s \"%s\" \"%s\"", method, url, body);

    fp = popen(command, "r");
    if (fp == NULL) {
        snprintf(response, response_size, "ERROR: Failed to execute router.sh");
        return;
    }

    fgets(response, response_size, fp);
    pclose(fp);
}