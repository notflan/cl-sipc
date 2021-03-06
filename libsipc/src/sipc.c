#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/un.h>
#include <string.h>
#include <errno.h>
#include <stdarg.h>

#include <sipc.h>
#include "crc64.h"

#define _SIRH_CHECK 0xabad
struct si_response_header {
	unsigned short check;
	int sd;
	int resp_sent;
};

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

int si_validate_message(const si_message *msg)
{
	char buffer[sizeof(si_message)];

	if(msg->flags & SI_NOSIGN)
		return 0; //Don't bother checking if it says there's no sign.
	
	memset(buffer,0,sizeof(si_message));
	si_message *head = (si_message*)buffer;
	memcpy(buffer,msg,sizeof(si_message));
	head->check = _SI_HEADER_CHECK;
	uint64_t crc0 = crc64(0, (unsigned char*)head, sizeof(si_message));
	uint64_t crc = crc64(crc0,  msg->data, head->data_len);
	return msg->check == crc;
}

static int _si_valid_header(const si_message *msg)
{
	int kk = (int)msg->type;
	return kk>=0 && kk<_SI_ERROR && msg->data_len < SI_MAX_MESSAGE_SIZE && msg->check0 == _SI_HEADER_CHECK;
}

static int _si_read_rest(int sd, si_message *message)
{
	int read = 0;
	int rd=0;

	if(message->data_len<1) return 0;

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

	struct timeval t;
	int sett=0;
	socklen_t len = sizeof(t);
	getsockopt(sd, SOL_SOCKET, SO_RCVTIMEO, &t, &len);

	//printf("bind timeout : %ld\n", t.tv_sec);	
	
#define ISTSET(t) (sett||(!!(t.tv_sec+t.tv_usec)))

	if(ISTSET(t)) {
		//Do we want to keep the accept timeout?
		struct timeval t2;
		t2.tv_usec=0;
		t2.tv_sec=0;
		setsockopt(sd, SOL_SOCKET, SO_RCVTIMEO, &t2, sizeof(t2));
		sett=1;
	}

	while(1) {
		int csd = accept(sd, NULL,NULL);
		if(csd<0) {
			rc = on_error(SIE_ACCEPT);
			if(rc<0) break;
			else continue;
		}
		if(ISTSET(t)) {
			struct timeval tt;
			memcpy(&tt, &t, sizeof(t));
			printf("setting sock timeout to %ld\n", tt.tv_sec);
			setsockopt(csd, SOL_SOCKET, SO_RCVTIMEO, &tt, sizeof(tt));
		}
		unsigned char buffer[sizeof(si_message)];
		memset(buffer,0,sizeof(si_message));
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
		//printf("%d: %s\n", errno, strerror(errno));
		if(rc<0) {
			close(csd);
			break;
		} else if (!rc2) {
			//message header has been read.
			si_message *full;
			if(_si_valid_header(message))
			{
				struct si_response_header* full0 = malloc(sizeof(si_message)+message->data_len+1+sizeof(struct si_response_header));
				memset(full0,0,sizeof(si_message)+message->data_len+1+sizeof(struct si_response_header)); //always have null-term
				full0->check = _SIRH_CHECK;
				full0->sd = csd;

				full = (si_message*)(((unsigned char*)full0)+sizeof(struct si_response_header));
				memcpy(full, message, sizeof(si_message));

				rc = _si_read_rest(csd, full);
				int valid=0;

				if(rc!=0) {
					if(rc==-1)
						rc = on_error(SIE_READ);
					else
						rc = on_error(SIE_PCONCLS);
					if(rc<0) {
						close(csd);
						free(full0);
						break;
					}
				}
			  	else if( (valid=si_validate_message(full)) || (full->flags & SI_NOSIGN) ){
					//Message has been read.
					
					if(!valid) {
						valid = on_error(SIW_CHECKSUM) == 0; 
					}
					if(valid) {
						rc = (full->flags & SI_DISCARD) ? 0 : on_message(full);

						if(!full0->resp_sent && !(full->flags & SI_NORESP))
						{
							//Send blank response. (Unless client asked for none.)
							si_message *resp = malloc(sizeof(si_message));
							resp->type = SI_BINARY;
							resp->flags = SI_DISCARD | SI_NORESP;
							resp->data_len = 0;
							si_sign(resp);
							int rc2 = si_sendmsg(csd, resp);

							if (rc2 != SI_SEND_OKAY)
								rc = on_error((si_error)rc2);

							free(resp);

						}
					} else {
						rc = on_error(SIE_CHECKSUM);
						if(rc<0) 
						{
							close(csd);
							free(full0);
							break;
						}
					}
				} else { // Bad checksum
					rc = on_error(SIE_CHECKSUM);
					if(rc<0) {
						close(csd);
						free(full0);
						break;
					}
				}
				free(full0);
			}
			else {
				rc = on_error(SIE_INVALID);
			}
		}
		close(csd);
		if(rc!=0) break;
	}
	if(ISTSET(t))
		setsockopt(sd, SOL_SOCKET, SO_RCVTIMEO, &t, sizeof(t));
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
			break;
		case SIE_R_INVALID:
			put("SIE_R_INVALID: Cannot respond to this");
			break;
		case SIE_R_MULTI:
			put("SIE_R_MULTI: A response has already been sent");
			break;
		case SIE_R_DISABLE:
			put("SIE_R_DISABLE: A response is not expected");
			break;
		case SIE_CHECKSUM:
			put("SIE_CHECKSUM: Bad checksum");
			break;
		case SIW_CHECKSUM:
			put("SIW_CHECKSUM: No checksum");
			break;
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

int si_read_response(int sd, si_message** resp)
{
	unsigned char buffer[sizeof(si_message)];
	si_message* head = (si_message*)buffer;

	int rc=0;
	int read,rd;
	read=rd=0;
	
	memset(buffer,0,sizeof(si_message));

	while( (read += (rd = recv(sd, buffer, sizeof(si_message), 0))) < sizeof(si_message))
	{
		if(rd<0)
		{
			rc = (int)SIE_READ;
			break;
		} else if(rd==0)
		{
			rc = (int)SIE_PCONCLS;
			break;
		}
	}
	if(rc==0 && _si_valid_header(head))
	{
		si_message *full = malloc(sizeof(si_message)+head->data_len+1);
		memset(full,0,sizeof(si_message)+head->data_len+1);
		memcpy(full, head, sizeof(si_message));
		rc = _si_read_rest(sd, full);
		int valid=0;
		switch(rc) {
			case 0:

				if((valid=si_validate_message(full)) || (full->flags & SI_NOSIGN)) {
					if(full->flags & SI_DISCARD)
						free(full);
					else 
						*resp = full;
					return valid?0:(int)SIW_CHECKSUM;
				} else {
					free(full);
					return (int)SIE_CHECKSUM;
				}
			case -1:
				free(full);
				return (int)SIE_READ;
			case -2:
				free(full);
				return (int)SIE_PCONCLS;
		}
	} else if(rc==0)
		rc = (int)SIE_INVALID;
	return rc;
}

void si_timeout(int sd, int seconds)
{
	struct timeval t;
	t.tv_sec = seconds;
	t.tv_usec=0;
	setsockopt(sd, SOL_SOCKET, SO_RCVTIMEO, (const char*) &t, sizeof(t));
}

static int _si_sendmsg(int sd, const si_message* msg)
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

void si_sign(si_message* msg)
{
	msg->check = (uint64_t)(msg->check0 = _SI_HEADER_CHECK);
	if(msg->flags & SI_NOSIGN) return;
	uint64_t crc = crc64(0, (unsigned char*)msg, sizeof(si_message)+msg->data_len);
	msg->check = crc;
}

int si_sendmsg_r(int sd, const si_message *msg, si_message** resp)
{
	int rc = _si_sendmsg(sd, msg);

	if(!(msg->flags & SI_NORESP))
	{
		si_message* _resp=NULL;
		rc = si_read_response(sd, &_resp);
		if(resp && _resp && !(_resp->flags & SI_DISCARD) && rc==0)
		{
			//printf("setting resp\n");
			*resp = _resp;
		}
		else {
			//printf("not setting %d %d %d %d\n", !!resp, !!_resp, _resp && (_resp->flags & SI_DISCARD), rc);
			if(_resp)
				free(_resp);
		}
		return rc==0?SI_SEND_OKAY:rc;
	}

	return SI_SEND_OKAY;
}

int si_sendmsg(int sd, const si_message *msg)
{
	return si_sendmsg_r(sd,msg,NULL);
}

int si_response(const si_message *to, const si_message *msg)
{
	struct si_response_header *rhead = (struct si_response_header*)(((intptr_t)to)-sizeof(struct si_response_header));
	if(rhead->check != _SIRH_CHECK)
		return (int)SIE_R_INVALID;

	if(rhead->resp_sent)
		return (int)SIE_R_MULTI;

	if(rhead->resp_sent)
		return (int)SIE_R_DISABLE;

	int rc = _si_sendmsg(rhead->sd, msg);

	rhead->resp_sent = 1;

	return rc;
}

//Quick send functions

int siqs_string_r(int sd, const char* string, unsigned int flags, si_message** resp)
{
	si_message *msg = malloc(sizeof(si_message)+strlen(string)+1);
	memset(msg,0,sizeof(si_message)+strlen(string)+1);

	msg->type = SI_STRING;
	msg->data_len = strlen(string)+1;
	msg->flags = flags;

	memcpy(msg->data, string, msg->data_len);

	si_sign(msg);
	int rc = si_sendmsg_r(sd, msg, resp);

	free(msg);

	return rc;
}
int siqs_string(int sd, const char* string)
{
	return siqs_string_r(sd, string, 0, NULL);
}

int siqs_close_r(int sd, unsigned int flags, si_message** resp)
{
	si_message *msg = malloc(sizeof(si_message));
	memset(msg,0,sizeof(si_message));

	msg->type = SI_CLOSE;
	msg->data_len=0;
	msg->flags = flags;

	si_sign(msg);
	int rc = si_sendmsg_r(sd, msg, resp);

	free(msg);
	return rc;
}
int siqs_close(int sd)
{
	return siqs_close_r(sd, 0, NULL);
}

int siqs_binary_r(int sd, const unsigned char* buffer, size_t size, unsigned int flags, si_message** resp)
{
	si_message *msg = malloc(sizeof(si_message)+size);
	memset(msg,0,sizeof(si_message)+size);

	msg->type = SI_BINARY;
	msg->data_len = size;
	msg->flags = flags;

	memcpy(msg->data, buffer, msg->data_len);

	si_sign(msg);
	int rc = si_sendmsg_r(sd, msg, resp);

	free(msg);
	return rc;
}
int siqs_binary(int sd, const unsigned char* buffer, size_t size)
{
	return siqs_binary_r(sd,buffer,size,0,NULL);
}

// -- 

int siqs_printf_r(int sd, unsigned int flags, si_message** resp, const char* format, ...)
{
	char buffer[SIQ_PRINTF_BUFFER_SIZE];
	va_list list;
	va_start(list, format);
	memset(buffer,0,SIQ_PRINTF_BUFFER_SIZE);

	vsnprintf(buffer, SIQ_PRINTF_BUFFER_SIZE-1, format, list);

	int rc = siqs_string_r(sd, buffer, flags,resp);

	va_end(list);

	return rc;
}

//Quick resp funcs

int siqr_string(const si_message* sd, const char* string)
{
	si_message *msg = malloc(sizeof(si_message)+strlen(string)+1);
	memset(msg,0,sizeof(si_message)+strlen(string)+1);

	msg->type = SI_STRING;
	msg->data_len = strlen(string)+1;

	memcpy(msg->data, string, msg->data_len);

	si_sign(msg);
	int rc = si_response(sd, msg);

	free(msg);
	return rc;
}

int siqr_binary(const si_message* sd, const unsigned char* buffer, size_t size)
{
	si_message *msg = malloc(sizeof(si_message)+size);
	memset(msg,0,sizeof(si_message)+size);

	msg->type = SI_BINARY;
	msg->data_len = size;

	memcpy(msg->data, buffer, msg->data_len);

	si_sign(msg);
	int rc = si_response(sd, msg);

	free(msg);
	return rc;
}

int siqr_close(const si_message* sd)
{
	si_message *msg = malloc(sizeof(si_message));
	memset(msg,0,sizeof(si_message));

	msg->type = SI_CLOSE;
	msg->data_len = 0;

	si_sign(msg);
	int rc = si_response(sd, msg);

	free(msg);
	return rc;
}

int siqr_printf(const si_message* sd, const char* format, ...)
{
	char buffer[SIQ_PRINTF_BUFFER_SIZE];
	va_list list;
	va_start(list, format);
	memset(buffer,0,SIQ_PRINTF_BUFFER_SIZE);

	vsnprintf(buffer, SIQ_PRINTF_BUFFER_SIZE-1, format, list);

	int rc = siqr_string(sd, buffer);

	va_end(list);

	return rc;
}

#include "crc64.c" //why not keep single translation unit?
