#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_RESPONSE_SIZE 4096

void trim_string(char *str) {
    int len = strlen(str);

    while (len > 0 && (isspace((unsigned char)str[len - 1]) || str[len - 1] == '\n' || str[len - 1] == '\r')) {
        str[len - 1] = '\0';
        len--;
    }
}

__attribute__((visibility("default"))) void router_wrapper(
    char *method, char *url, char *body, char *response, int response_size)
{
    char command[1024];
    FILE *fp;
    char buffer[256];
    int total_read = 0;

    trim_string(method);
    trim_string(url);
    trim_string(body);

    snprintf(command, sizeof(command), "./router.sh \"%s\" \"%s\" \"%s\"", method, url, body);

    printf("DEBUG: COMMAND: %s\n", command);
    fp = popen(command, "r");
    if (fp == NULL) {
        snprintf(response, response_size, "ERROR: Failed to execute router.sh");
        return;
    }
    response[0] = '\0';
    while (fgets(buffer, sizeof(buffer), fp) != NULL) {
        if (total_read + strlen(buffer) < response_size - 1) {
            strcat(response, buffer);
            total_read += strlen(buffer);
        } else {
            break;
        }
    }

    pclose(fp);
}
