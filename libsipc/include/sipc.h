#ifndef _LIBSIPC_H
#define _LIBSIPC_H
#include <stddef.h>
#include <stdint.h>

#define SI_MAX_MESSAGE_SIZE (1024*1024)*5 //5MB

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

#define SI_NORESP (1<<0)	//Do not expect a response.
#define SI_DISCARD (1<<1)	//Do not process this.
#define SI_NOSIGN (1<<2)	//This message is not signed.

#define _SI_HEADER_CHECK 0xbeefbeefabad1dealu

typedef struct {
	union {
		si_type type;
		uint32_t __pad;
	};
	uint32_t flags;
	uint32_t data_len;
	uint32_t __unused; //fix weird padding problem.
	uint64_t check0;
	uint64_t check;
	uint8_t data[];
} si_message;

_Static_assert(sizeof(union { si_type t0; uint32_t pad;}) == sizeof(uint32_t), "packing error: sizeof(enum)!=sizeof(uint32_t)");
_Static_assert(sizeof(si_message) == (sizeof(uint32_t)*4) + (sizeof(uint64_t)*2), "packing error: bad size, header padded size != 32 bytes?");

#define SIEF_WARNING  0xaff000
typedef enum {
	SIE_ACCEPT= 0, //Sock accept failure
	SIE_READ, //Sock read failure
	SIE_PCONCLS, //Sock closed before read complete
	SIE_INVALID, //Invalid message

	SIE_R_INVALID, //This message cannot be responded to like that
	SIE_R_MULTI, //A response has already been sent
	SIE_R_DISABLE, //The client asked to not be responded to

	SIE_CHECKSUM, //Bad checksum

	//Recoverable errors (warnings)

	SIW_CHECKSUM = (SIEF_WARNING)+1, //No checksum.
} si_error;

typedef int (*si_callback)(const si_message *msg);
typedef int (*si_error_callback)(si_error err);

int si_bind(const char* file); //Returns sd, or -1 on failure.
int si_listen(int sd, si_error_callback on_error, si_callback on_message); //Returns non-zero on clean exit, -1 on error.
void si_close(int sd); //Close sock (must be called after si_listen()

void si_timeout(int sd, int seconds); //Set sock timeout (0 for infinite).

char* si_error_string(si_error err);
char* si_type_string(si_type ty);

int si_connect(const char* file); //Returns sd, or -1 on failure.
int si_sendmsg_r(int sd, const si_message* msg,  si_message** response); //si_sendmmsg but optionally receive response (make sure to free() *response after you're done, NULL to discard response, if there is no response *response is not modified)
int si_sendmsg(int sd, const si_message *msg); //Returns 0 on okay, 1 if whole message could not be sent, -1 on send error, -2 on weird send error. (see SI_SEND_*)
void si_sign(si_message *msg); //Sign message. Use this before sending one. (note: you still have to call this, even when setting SI_NOSIGN flag.

//Quick funcs
int siqs_string_r(int sd, const char* string, unsigned int flags, si_message** resp); //quick send string
int siqs_string(int sd, const char* string); //quick send string (discard response)
int siqs_close_r(int sd, unsigned int flags, si_message** resp); //quick send close
int siqs_close(int sd); //quick send close (discard response)
int siqs_binary_r(int sd, const unsigned char* buffer, size_t size, unsigned int flags, si_message** resp); //quick send binary
int siqs_binary(int sd, const unsigned char* buffer, size_t size); //quick send binary (discard response)

int siqs_printf_r(int sd, unsigned int flags, si_message** resp, const char* format, ...); //quick send string (printf format)
#define siqs_printf(sd, ...) siqs_printf_r(sd, 0, NULL, __VA_ARGS__) //quick send string (printf format, discard response)

int siqr_string(const si_message* sd, const char* string); //quick send response string
int siqr_binary(const si_message* sd, const unsigned char* bin, size_t size); //quick send response binary
int siqr_close (const si_message* sd); //quick send response close (kinda useless)
int siqr_printf(const si_message* sd, const char* fmt, ...); //quick send response string (printf format)

#endif /* _LIBSIPC_H */
