#ifndef _LIBSIPC_H
#define _LIBSIPC_H
#include <stddef.h>

#define SI_MAX_MESSAGE_SIZE 1024

#define SI_ERROR_STRING_SIZE 256
#define SI_TYPE_STRING_SIZE 256

#define SI_SEND_OKAY 0
#define SI_SEND_PARTIAL 1
#define SI_SEND_ERROR -1
#define SI_SEND_FAILURE -2

#define SIQ_PRINTF_BUFFER_SIZE 256

typedef enum {
	SI_STRING = 0,
	SI_BINARY,
	SI_CLOSE,
	_SI_ERROR,
} si_type;

typedef struct {
	si_type type;
	unsigned int data_len;
	unsigned char data[];
} si_message;

typedef enum {
	SIE_ACCEPT= 0, //Sock accept failure
	SIE_READ, //Sock read failure
	SIE_PCONCLS, //Sock closed before read complete
	SIE_INVALID, //Invalid message
} si_error;

typedef int (*si_callback)(const si_message *msg);
typedef int (*si_error_callback)(si_error err);

int si_bind(const char* file); //Returns sd, or -1 on failure.
int si_listen(int sd, si_error_callback on_error, si_callback on_message); //Returns non-zero on clean exit, -1 on error.
void si_close(int sd); //Close sock (must be called after si_listen()

char* si_error_string(si_error err);
char* si_type_string(si_type ty);

int si_connect(const char* file); //Returns sd, or -1 on failure.
int si_sendmsg(int sd, const si_message *msg); //Returns 0 on okay, 1 if whole message could not be sent, -1 on send error, -2 on weird send error. (see SI_SEND_*)

//Quick funcs
int siqs_string(int sd, const char* string); //quick send string
int siqs_close(int sd); //quick send close
int siqs_binary(int sd, const unsigned char* buffer, size_t size); //quick send binary
int siqs_printf(int sd, const char* format, ...); //quick send string (printf format)

#endif /* _LIBSIPC_H */
