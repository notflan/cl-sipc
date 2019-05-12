#include <sipc.h>

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
