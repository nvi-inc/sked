/* upper - convert the input buffer to UPPER case 
940505 nrv Created
*/

#include <string.h>

void upper(inbuf)
char *inbuf;

{
  int i,l;

  l=strlen(inbuf);
  for (i=0;i<l;i++) 
    inbuf[i]=toupper(inbuf[i]);
}
