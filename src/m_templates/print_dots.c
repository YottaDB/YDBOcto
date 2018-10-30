#include <assert.h>

#include "octo_types.h"
#include "template_helpers.h"

int print_dots(char *buffer, int buffer_length, int dots) {
	int i;
	char *buff_ptr = buffer;

	assert(buffer_length > dots);
	for(i=0; i < dots; i++) {
		*buff_ptr++ = '.';
	}
	if(dots > 0)
		*buff_ptr++ = ' ';
	return buff_ptr - buffer;
}
