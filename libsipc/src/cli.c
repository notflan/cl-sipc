#include <sipc.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

int silent_level=0;
int signing=1;

#define Vprintf(...) if(silent_level<0) printf("[l] " __VA_ARGS__)
#define Eprintf(...) if(silent_level <2) fprintf(stderr, "[e] " __VA_ARGS__)
#define Pprintf(...) if(silent_level<1) printf(__VA_ARGS__)

int on_error(si_error err)
{

	if(err & SIEF_WARNING) { //Different handler for warnings
		Eprintf("<- (w) %s\n", si_error_string(err));
		if(err == SIW_CHECKSUM && signing) {
			return -1; //We don't want to accept this, we're expecting signed packets.
		} else return 0;
	 } else {
		Eprintf("<- %s\n", si_error_string(err));
	 }

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

int server_aresp =0;
int timeout=0;

char* textmessage(const si_message *msg)
{
	char* text;
	switch(msg->type)
	{
		case SI_CLOSE:
		        if(silent_level<2)	
				text = "...";
			else
				text="";
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
	return text;
}

int on_message(const si_message *msg)
{
	char* text;
	const char* strty = si_type_string(msg->type);
	
	int rc=0;

	if(server_aresp)
	{
		int rc = siqr_printf(msg, "-> %s", textmessage(msg));
		if(rc==SI_SEND_OKAY) {
			Vprintf("response send ok\n");
		}
		else {
			Eprintf("%s\n", si_error_string((si_error)rc));
		}
	}

	switch(msg->type)
	{
		case SI_CLOSE: 
			rc=1;
			if(silent_level<2)
				text="...";
			else
				text = "";
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

	if(silent_level>1)
		printf("%s\n", text);
	else
		printf("<- (%s) %s\n", strty, text);

	return rc;
}

int server(const char* bindto, int secho)
{
	int sd = si_bind(bindto);
	int rc=-1;
	server_aresp = secho;

	if(sd<0) {
		Eprintf("error binding\n");
	} else {
		Vprintf("bind ok\n");
		if(timeout)
		{
			si_timeout(sd, timeout);
			Vprintf("timeout set to %d\n", timeout);
		}
		rc = si_listen(sd, &on_error, &on_message);
		Vprintf("listen stopped with rc %d\n", rc);
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
			Vprintf("send okay\n");
			rc=0;
			break;
		case SI_SEND_ERROR:
			Eprintf("send error\n");
			break;
		case SI_SEND_FAILURE:
			Eprintf("send failure\n");
			break;
		case SI_SEND_PARTIAL:
			Eprintf("partial send failure\n");
			break;
		default:
			Eprintf("unknown send error");
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
		Eprintf("connect error\n");
	} else {
		Vprintf("connect ok\n");
		if(timeout) {
			si_timeout(sd, timeout);
			Vprintf("timeout set to %d\n", timeout);
		}
		si_message *response = NULL;
		if(!signing)
			msg->flags |= SI_NOSIGN;	
		si_sign(msg);
		int rrc = si_sendmsg_r(sd, msg, &response);
		if(response) {
			if(silent_level>1)
				printf("%s\n", textmessage(response));
			else
				printf(" <- (%s) %s\n", si_type_string(response->type), textmessage(response)); 
			free(response);
		}
		rc = cli_return(rrc);
		Vprintf("client rc %d\n", rc);
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
		Eprintf("connect error\n");
	} else {
		Vprintf("connect ok\n");
		if(timeout) {
			si_timeout(sd, timeout);
			Vprintf("timeout set to %d\n", timeout);
		}
		si_message* response = NULL;
		if(!signing)
			msg->flags |= SI_NOSIGN;	
		si_sign(msg);
		int rrc = si_sendmsg_r(sd, msg, &response);
		if(response) {
			if(silent_level>1)
				printf("%s\n", textmessage(response));
			else
				printf(" <- (%s) %s\n", si_type_string(response->type), textmessage(response)); 
			free(response);
		}
		rc = cli_return(rrc);
		Vprintf("client rc %d\n", rc);
		si_close(sd);
	}

	free(msg);
	return rc;
}

int pfa(char** argv)
{
	char * carg = argv[1]+1;
	int rc=0;
	int nrc=0;
	while (*carg) 
	{
		switch(*carg) {
			case 'q':
				silent_level =1;
				break;
			case 'Q':
				silent_level =2;
				break;
			case 'v':
				silent_level =-1;
				break;
			case 'u':
				signing = 0;
				break;
			case 't':
				nrc+=1;
				timeout = atoi(*(argv+1+nrc));
				break;
			default:
				carg++;
				continue;
		}
		rc = 1;
		carg++;
	}
	return rc+nrc;
}

int hang(const char* sock)
{
	int sd = si_connect(sock);
	int rc =0;
	if(sd<0) {
		Eprintf("connection failed\n");
		rc=-1;
	} else {
		Vprintf("connection established, waiting for %d seconds\n", (timeout?timeout:5) );
		//si_timeout(sd, 5);
		sleep(timeout?timeout:5);
		Vprintf("abandoning\n");
	}

	return rc;
}

int main(int argc, char** argv)
{
	int rc=0,secho=0;
	char** _av = argv;
	if(argv[1] && argv[1][0] == '-')
	{
		argv+=pfa(argv);
	}
	if(argv[1] && argv[2] && argv[1][0]=='-') {

		switch(argv[1][1]) {
			case 'l':
				//Listen

				if(argv[1][2] == 'f')
					unlink(argv[2]);
				secho = (argv[1][2] == 'e' ||
				  (argv[1][2] && argv[1][3] == 'e'));

				rc = server(argv[2], secho);
				if(rc!=0)
					Eprintf("listener stopped with rc %d\n", rc);
				break;
			case 'p':
				//Write
				if(argv[3])
				{
					if(argv[1][2] == 'b')
						rc = client(argv[2], argv[3], 1);
					else
						rc = client(argv[2], argv[3], 0);
					if(rc!=0)
						Eprintf("client rc %d\n", rc);
				} else Eprintf("no message\n");
				break;
			case 'c':
				//Close
				rc = client_close(argv[2]);
				if(rc==0) {
					if(argv[1][2] == 'f')
						unlink(argv[2]);
				}
				if(rc!=0)
					Eprintf("client rc %d\n", rc);
				break;
			case 'h':
				rc = hang(argv[2]);
				Vprintf("hang rc %d\n", rc);
				break;
			default:
				Eprintf("i don't know how to do that\n");
				break;
		}
	} else 
	{
		argv = _av;
		printf("usage: %s [-uqQvt] [<timeout>] -l[fe] <socket>\nusage: %s [-uqQvt] [<timeout>] -p[b] <socket> <message>\nusage: %s [-uqQvt] [<timeout>] -c[f] <socket>\nusage: %s [-qQvt] [<timeout>] -h <socket>\n", argv[0], argv[0], argv[0], argv[0]);
		printf("\n-l[fe]\tlisten on socket. (f to unlink file first, e to send response)\n");
		printf("-p[b]\twrite to socket. (b to send as binary)\n");
		printf("-c[f]\tsend cose signal to socket. (f to unlink file after)\n");
		printf("-h\thang this socket (for 5 seconds, or set timeout if there is one)\n");
		printf("\nother options:\n");
		printf(" -q\tquiet mode, don't print errors\n");
		printf(" -Q\tsilent mode, only print responses\n");
		printf(" -v\tverbose mode, print additional messages\n");
		printf(" -t\tset socket timeout to next arg (in seconds)\n");
		printf(" -u\tdo not sign messages (or do not verify, for server)\n");
	}
	return rc;
}
