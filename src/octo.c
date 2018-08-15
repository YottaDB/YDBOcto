#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>

FILE* yyin;

extern int yyparse();

static int verbose_flag = 0;

int main(int argc, char **argv)
{
  int c;
  /* Parse input parameters */
  while (1)
  {
    static struct option long_options[] =
      {
        {"verbose", no_argument, &verbose_flag, 1},
        {0, 0, 0, 0}
      };
    int option_index = 0;

    c = getopt_long(argc, argv, "v", long_options, &option_index);
    if(c == -1)
      break;

    switch(c)
    {
    case 0:
      if(long_options[option_index].flag != 0)
        break;
      break;
    case 'v':
      verbose_flag = 1;
      break;
    }
  }

  yyin = stdin;

  do {
    printf("OCTO> ");
    yyparse();
    printf("\n\nCommand processed\n");
  } while(!feof(yyin));
  return 0;
}

void yyerror(char const *s)
{
  printf("yyerror: %s\n", s);
}

int yywrap ( void )
{
  return 1;
}
