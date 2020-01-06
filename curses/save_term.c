#include <stdio.h>
#include <termio.h>
#include <ctype.h>

/* AEM 20041125 add int */ 
int save_term(term)
struct termio *term;
{
   ioctl(0,TCGETA,term);   
/*   return(NULL);
 AEM 20041125 use 0 instead NULL */
   return(0);
}

/* AEM 20041125 add int */
int set_term(path)
char **path;
{
   FILE *fp;
   struct termio term;

   fp = fopen(*path,"r");
   fread((char *) &term, sizeof (struct termio), 1, fp);
   ioctl(0,TCSETA,&term);
   fclose(fp);
/* AEM 20041125 use 0 instead NULL */
   return(0);
}

/* AEM 20041125 add int */
int w_trm(path,term)
char **path;
struct termio *term;
{
   FILE *fp;

   fp = fopen(*path,"w");
   fwrite((char *) term, sizeof (struct termio), 1, fp);
   fclose(fp);
/* AEM 20041125 use 0 instead NULL */
   return(0);
}
