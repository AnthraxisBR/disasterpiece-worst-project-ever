#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/select.h>

#define PORT 8080
#define RESPONSE_SIZE 52
#define BUFFER_SIZE 1024

int server_fd, client_fd;
struct sockaddr_in server_addr, client_addr;
socklen_t client_addr_len = sizeof(client_addr);
char request_buffer[BUFFER_SIZE];
int request_size;

char response[] =
    "HTTP/1.1 200 OK\r\n"
    "Content-Length: 2\r\n"
    "Connection: close\r\n"
    "\r\nOK";

void open_socket() {
    server_fd = socket(AF_INET, SOCK_STREAM, 0);
    printf("Server socket created\n");
    printf("Server socket fd: %d\n", server_fd);
    if (server_fd < 0) {
        perror("Error: Could not create socket");
        exit(1);
    }
}

void config_socket() {
    int opt_value = 1;
    if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt_value, sizeof(opt_value)) < 0) {
        perror("Error: setsockopt failed");
        exit(1);
    }
}

void bind_socket() {
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = INADDR_ANY;
    server_addr.sin_port = htons(PORT);

    if (bind(server_fd, (struct sockaddr *)&server_addr, sizeof(server_addr)) < 0) {
        perror("Error: Bind failed");
        exit(1);
    }
}

void listen_socket() {
    if (listen(server_fd, 5) < 0) {
        perror("Error: Listen failed");
        exit(1);
    }

    printf("Server listening on port %d\n", PORT);
}

void accept_client() {
    client_fd = accept(server_fd, (struct sockaddr *)&client_addr, &client_addr_len);
    if (client_fd < 0) {
        perror("Error: Accept failed");
        exit(1);
    }
}

void receive_request() {
    request_size = recv(client_fd, request_buffer, sizeof(request_buffer) - 1, 0);
    if (request_size < 0) {
        perror("Error: Receive failed");
        exit(1);
    }

    request_buffer[request_size] = '\0';
    printf("Received Request: %s\n", request_buffer);
}

void send_response() {
    int response_size = strlen(response);
    int sent_size = send(client_fd, response, response_size, 0);
    if (sent_size < 0) {
        perror("Error: Send failed");
        exit(1);
    }

    printf("Response sent: %s\n", response);
}

void shutdown_connection() {
    if (shutdown(client_fd, SHUT_RDWR) < 0) {
        perror("Error: Shutdown failed");
        exit(1);
    }

    close(client_fd);
}

int main() {
    open_socket();
    config_socket();
    bind_socket();
    listen_socket();

    while (1) {
        accept_client();
        receive_request();
        send_response();
        shutdown_connection();
    }

    close(server_fd);
    return 0;
}
