#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <readline/readline.h>
#include <readline/history.h>

#include "octo.h"
#include "octo_types.h"

int readline_getc(FILE *inputFile, char *buff, int size) {
  int i = 0, c;
  char *input_buffer = NULL, *input_buffer_i;
  char *prompt1 = "OCTO> ", *prompt2 = "OCTO# ", *cur_prompt = prompt1;

  if(inputFile != stdin) {
    while(!feof(inputFile))
    {
      assert(i < size);
      c = fgetc(inputFile);
      if(c != -1)
        buff[i++] = c;
      if(c == ';')
        break;
    }
    buff[i] = '\0';
  } else {
    while(!feof(inputFile))
    {
      if(input_buffer) {
        free(input_buffer);
        input_buffer = NULL;
      }
      input_buffer = readline(cur_prompt);
      if(!input_buffer)
        return -1;
      if(*input_buffer)
        add_history(input_buffer);
      for(input_buffer_i = input_buffer; *input_buffer_i != '\0'; input_buffer_i++) {
        assert(i < size);
        buff[i++] = *input_buffer_i;
        if(*input_buffer_i == ';') {
          buff[i++] = '\0';
          return 0;
        }
      }
      cur_prompt = prompt2;
      buff[i++] = '\n';
    }
  }
}
