#include <sipc.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

int on_error(si_error err)
{
	printf("[E] <- %s\n", si_error_string(err));

	return 0;
}

void binchar(char *byte, unsigned char c)
{
	sprintf(byte, "%x", c);
}

#define STRBIN_MAX_SIZE 64
char* strbin(const unsigned char* data, size_t sz)
{
	static char buffer[STRBIN_MAX_SIZE];
	memset(buffer,0,STRBIN_MAX_SIZE);

	register int j=0;
	for(register int i=0;i<sz && j<STRBIN_MAX_SIZE-3;i+=1,j+=2)
	{
		binchar(buffer+j, data[i]);
	}

	if(j>=(STRBIN_MAX_SIZE-3))
		sprintf(buffer+(STRBIN_MAX_SIZE-4), "...");

	return buffer;
}

int on_message(const si_message *msg)
{
	char* text;
	const char* strty = si_type_string(msg->type);
	
	int rc=0;

	switch(msg->type)
	{
		case SI_CLOSE: 
			rc=1;
			text = "...";
			break;
		case SI_STRING:
			text = (char*) msg->data;
			break;
		case SI_BINARY:
			text = strbin(msg->data, (size_t)msg->data_len);
			break;
		default:
			text= "(unbound)";
			break;
	}

	printf("<- (%s) %s\n", strty, text);

	return rc;
}

int server(const char* bindto)
{
	int sd = si_bind(bindto);
	int rc=-1;
	if(sd<0) {
		printf("error binding\n");
	} else {
		rc = si_listen(sd, &on_error, &on_message);
		printf("listen stopped with rc %d\n", rc);
		if(rc>=0) //positive rc is okay
			rc=0;
	}
	si_close(sd);
	return rc;
}

int cli_return(int rrc)
{
	int rc=-1;
	switch(rrc) {
		case SI_SEND_OKAY:
			printf("send okay\n");
			rc=0;
			break;
		case SI_SEND_ERROR:
			printf("send error\n");
			break;
		case SI_SEND_FAILURE:
			printf("send failure\n");
			break;
		case SI_SEND_PARTIAL:
			printf("partial send failure\n");
			break;
		default:
			printf("unknown send error");
			break;
	}
	return rc;
}

int client(const char* conto, const char* string, int bin)
{
	si_message *msg = malloc(sizeof(si_message)+strlen(string));
	memset(msg,0,sizeof(si_message)+strlen(string));

	msg->type = bin ? SI_BINARY : SI_STRING;
	msg->data_len = strlen(string);

	memcpy(msg->data, string, msg->data_len);

	int rc=-1;

	int sd = si_connect(conto);
	if(sd<0) {
		printf("connect error\n");
	} else {
		int rrc = si_sendmsg(sd, msg);
		rc = cli_return(rrc);
		si_close(sd);
	}

	free(msg);
	return rc;
}

int client_close(const char* conto)
{
	si_message *msg = malloc(sizeof(si_message));
	memset(msg,0,sizeof(si_message));

	msg->type = SI_CLOSE;
	msg->data_len=0;

	int rc=-1;
	int sd = si_connect(conto);
	if(sd<0) {
		printf("connect error\n");
	} else {
		int rrc = si_sendmsg(sd, msg);
		rc = cli_return(rrc);	
		si_close(sd);
	}

	free(msg);
	return rc;
}

int main(int argc, char** argv)
{
	int rc=0;
	if(argv[1] && argv[2] && argv[1][0]=='-') {

		switch(argv[1][1]) {
			case 'l':
				//Listen
				if(argv[1][2] == 'f')
					unlink(argv[2]);
				rc = server(argv[2]);
				break;
			case 'p':
				//Write
				if(argv[3])
				{
					if(argv[1][2] == 'b')
						rc = client(argv[2], argv[3], 1);
					else
						rc = client(argv[2], argv[3], 0);
					printf("client rc %d\n", rc);
				} else printf("no message\n");
				break;
			case 'c':
				//Close
				rc = client_close(argv[2]);
				if(rc==0) {
					if(argv[1][2] == 'f')
						unlink(argv[2]);
				}
				printf("client rc %d\n", rc);
				break;
			default:
				printf("i don't know how to do that\n");
				break;
		}
	} else 
	{
		printf("usage: %s -l[f] <socket>\nusage: %s -p[b] <socket> <message>\nusage: %s -c[f] <socket>\n", argv[0], argv[0], argv[0]);
		printf("\n-l[f]\tlisten on socket. (f to unlink file first)\n");
		printf("-p[b]\twrite to socket. (b to send as binary)\n");
		printf("-c[f]\tsend cose signal to socket. (f to unlink file after)\n");
	}
	return rc;
}
