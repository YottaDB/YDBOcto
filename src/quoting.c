#include <stdlib.h>
#include <string.h>
#include <assert.h>

char *m_escape_string(const char *string) {
	char *buf, *e;
	const char *c;
	int len = strlen(string);
	buf = malloc(2*len);
	for(c = string, e = buf; *c != '\0'; c++) {
		if(*c == '"') {
			*e++ = '"';
			*e++ = '"';
		} else
			*e++ = *c;
	}
	*e = '\0';
	return buf;
}

char *m_unescape_string(const char *string) {
	char *buf, *e;
	const char *c;
	int len = strlen(string), quote_count = 0;
	buf = malloc(2*len);
	for(c = string, e = buf; *c != '\0'; c++) {
		if(*c == '"') {
			if(quote_count == 1) {
				quote_count = 0;
				*e++ = '"';
			} else {
				quote_count++;
			}
		} else {
			*e++ = *c;
			assert(quote_count == 0);
		}
	}
	*e = '\0';
	return buf;
}
