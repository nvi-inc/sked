
#include <stdio.h>

main (argc,argv)
int argc;
char *argv[];
{
  char *cfile;
  int clen;

  if (argc > 1)
    cfile = argv[1];
  else
    cfile = NULL;

  clen = strlen(cfile);
  fsked(cfile,clen);
}
