#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/un.h>
#include <string.h>
#include <stdarg.h>

#include <sipc.h>

int si_bind(const char* file)
{
	int sd, rc;
	struct sockaddr_un server;

	sd = socket(AF_UNIX, SOCK_STREAM, 0);
	if(sd<0)
	{
		return -1;
	}

	memset(&server,0,sizeof(server));
	server.sun_family = AF_UNIX;
	strncpy(server.sun_path, file, sizeof(server.sun_path));

	rc = bind(sd, (struct sockaddr *)&server, SUN_LEN(&server));
	if(rc<0)
	{
		close(sd);
		return -1;
	}

	return sd;
}

static int _si_valid_header(const si_message *msg)
{
	int kk = (int)msg->type;
	return kk>=0 && kk<_SI_ERROR && msg->data_len < SI_MAX_MESSAGE_SIZE;
}

static int _si_read_rest(int sd, si_message *message)
{
	int read = 0;
	int rd=0;

	while( (read += (rd = recv(sd, message->data, message->data_len, 0))) < message->data_len)
	{
		if(rd<0) return -1;
		else if(rd==0) return 1;
	}

	return 0;
}

int si_listen(int sd, si_error_callback on_error, si_callback on_message)
{
	int rc = listen(sd, 10);
	if(rc<0) {
		return -1;
	}

	while(1) {
		int csd = accept(sd, NULL,NULL);
		if(csd<0) {
			rc = on_error(SIE_ACCEPT);
			if(rc<0) break;
		}
		unsigned char buffer[sizeof(si_message)];
		si_message *message = (si_message*)buffer;
		int read=0;
		int rd=0;
		rc=0;
		int rc2=0;
		while( (read += (rd = recv(csd, buffer, sizeof(si_message), 0))) < sizeof(si_message))
		{
			if(rd<0)
			{
				rc = on_error(SIE_READ);
				rc2=1;
				break;
			} else if(rd==0)
			{
				rc = on_error(SIE_PCONCLS);
				rc2=1;
				break;
			}
		}
		if(rc<0) {
			close(csd);
			break;
		} else if (!rc2) {
			//message header has been read.
			si_message *full;
			if(_si_valid_header(message))
			{
				full = malloc(sizeof(si_message)+message->data_len+1);
				memset(full,0,sizeof(si_message)+message->data_len+1); //always have null-term
				memcpy(full, message, sizeof(si_message));
				rc = _si_read_rest(csd, full);
				if(rc!=0) {
					if(rc==-1)
						rc = on_error(SIE_READ);
					else
						rc = on_error(SIE_PCONCLS);
					if(rc<0) {
						close(csd);
						break;
					}
				}
			  	else {
					//Message has been read.
					rc = on_message(full);
					free(full);	
				}
			}
			else {
				rc = on_error(SIE_INVALID);
			}
		}
		close(csd);
		if(rc!=0) break;
	}
	return rc;
}

void si_close(int sd)
{
	close(sd);
}

char *si_error_string(si_error err)
{
	static char buffer[SI_ERROR_STRING_SIZE];
	memset(buffer, 0, SI_ERROR_STRING_SIZE);
#define put(...) snprintf(buffer, SI_ERROR_STRING_SIZE-1, __VA_ARGS__)
	switch(err) {
		case SIE_ACCEPT:
			put("SIE_ACCEPT: Socket accept error");
			break;
		case SIE_READ:
			put("SIE_READ: Socket read error");
			break;
		case SIE_PCONCLS:
			put("SIE_PCONCLS: Socket closed");
			break;
		case SIE_INVALID:
			put("SIE_INVALID: Bad message");
		default:
			put("SIE_UNKNOWN: Unknown EC %d", (int)err);
			break;
	}
#undef put
	return buffer;
}

char *si_type_string(si_type type)
{
	static char buffer[SI_ERROR_STRING_SIZE];
	memset(buffer, 0, SI_ERROR_STRING_SIZE);
#define put(...) snprintf(buffer, SI_ERROR_STRING_SIZE-1, __VA_ARGS__)
	switch(type) {
		case SI_STRING:
			put("SI_STRING");
			break;
		case SI_BINARY:
			put("SI_BINARY");
			break;
		case SI_CLOSE:
			put("SI_CLOSE");
			break;
		default:
			put("SI_UNKNWON");
			break;
	}
#undef put
	return buffer;
}

int si_connect(const char *file)
{
	int sd=-1, rc;
	struct sockaddr_un server;
	
	sd = socket(AF_UNIX, SOCK_STREAM, 0);
	
	if(sd<0) {
		return -1;
	}

	memset(&server,0,sizeof(server));
	server.sun_family = AF_UNIX;
	strncpy(server.sun_path, file, sizeof(server.sun_path));

	rc = connect(sd, (struct sockaddr*)&server, SUN_LEN(&server));
	if(rc<0) {
		close(sd);
		return -1;
	}

	return sd;
}

int si_sendmsg(int sd, const si_message *msg)
{
	int rc = send(sd, msg, sizeof(si_message)+msg->data_len, 0);
	if(rc<0)
		return SI_SEND_ERROR;
	else if(rc==0)
		return SI_SEND_FAILURE;
	else if(rc != sizeof(si_message)+msg->data_len)
		return SI_SEND_PARTIAL;

	return SI_SEND_OKAY;
}

//Quick send functions

int siqs_string(int sd, const char* string)
{
	si_message *msg = malloc(sizeof(si_message)+strlen(string));
	memset(msg,0,sizeof(si_message)+strlen(string));

	msg->type = SI_STRING;
	msg->data_len = strlen(string);

	memcpy(msg->data, string, msg->data_len);

	int rc = si_sendmsg(sd, msg);

	free(msg);

	return rc;
}

int siqs_close(int sd) 
{
	si_message *msg = malloc(sizeof(si_message));
	memset(msg,0,sizeof(si_message));

	msg->type = SI_CLOSE;
	msg->data_len=0;

	int rc = si_sendmsg(sd, msg);

	free(msg);
	return rc;
}

int siqs_binary(int sd, const unsigned char* buffer, size_t size)
{
	si_message *msg = malloc(sizeof(si_message)+size);
	memset(msg,0,sizeof(si_message)+size);

	msg->type = SI_BINARY;
	msg->data_len = size;

	memcpy(msg->data, buffer, msg->data_len);

	int rc = si_sendmsg(sd, msg);

	free(msg);
	return rc;
}

int siqs_printf(int sd, const char* format, ...)
{
	char buffer[SIQ_PRINTF_BUFFER_SIZE];
	va_list list;
	va_start(list, format);
	memset(buffer,0,SIQ_PRINTF_BUFFER_SIZE);

	vsnprintf(buffer, SIQ_PRINTF_BUFFER_SIZE-1, format, list);

	int rc = siqs_string(sd, buffer);

	va_end(list);

	return rc;
}
