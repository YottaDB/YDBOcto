#include <stdio.h>

#include "octod/message_formats.h"

int main() {
	AuthenticationOk ok;
	ok.type = 'R';
	*(unsigned int *)(&ok.length) = 8;
	*(unsigned int *)(&ok.result) = 0;
	printf("Size of type is: %ld\n", sizeof(AuthenticationOk));
	return 0;
}
