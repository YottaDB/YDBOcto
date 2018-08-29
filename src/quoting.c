#include <stdlib.h>
#include <string.h>

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
