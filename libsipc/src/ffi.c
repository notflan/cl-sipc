#include <sipc.h>
#include <stdlib.h>

//Some FFI helpers

si_type sif_type(const si_message* msg)
{
	return msg->type;
}

unsigned int sif_size(const si_message* msg)
{
	return msg->data_len;
}

const unsigned char* sif_data(const si_message* msg)
{
	return msg->data;
}

si_message** sif_heap_alloc(int keepresp)
{
	if(keepresp)
	{
		si_message **resp = malloc(sizeof(si_message*));
		*resp = NULL;
		return resp;
	}
	else return NULL;
}

void sif_heap_free(si_message **msg)
{
	if(msg) {
		if(*msg) {
			free(*msg);
			*msg = NULL;
		}
		free(msg);
	}
}
