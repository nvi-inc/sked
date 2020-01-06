/* getydhms extracts the day, hours, minutes, and seconds from
   a time field.

Called by: vlogx
981021 nrv Created. Remove the format-dependent code from vlog9.
990324 nrv SNAP time is the same regardless of fstype.
990326 nrv Wrong! SNAP time is different but it may not be
           consistent with the FS version.
990520 nrv Return the year.
*/

#include <stdio.h>
#include <string.h>
#include <math.h>

void getydhms(inbuf,fstype,cyear,cday,chr,cmin,csec)
/* Input */
char *inbuf;
char *fstype;
/* Output */
char *cyear;  /* also input */
char *cday; 
char *chr;
char *cmin;
char *csec;

{
/* Local */
char *ptr;
  if (strncmp(inbuf,":!",2)==0) { /* SNAP time */
      /*  :!dddhhmmss[ss] */
    ptr = strchr(inbuf,'.'); /* look for periods */
    if ( ptr != NULL) {
      memcpy(cday,inbuf+7,3);
      memcpy(chr,inbuf+11,2);
      memcpy(cmin,inbuf+14,2);
      memcpy(csec,inbuf+17,2);
    }
    else { /* old format */
      memcpy(cday,inbuf+2,3);
      memcpy(chr,inbuf+5,2);
      memcpy(cmin,inbuf+7,2);
      memcpy(csec,inbuf+9,2);
    }
  } /* SNAP time */
  else { /* log time */
    if (strcmp(fstype,"8") == 0) { 
      /*  dddhhmmss */
      memcpy(cday,inbuf,3);
      memcpy(chr,inbuf+3,2);
      memcpy(cmin,inbuf+5,2);
      memcpy(csec,inbuf+7,2);
    }
    else if ((strcmp(fstype,"9") == 0) || (strcmp(fstype,"2") == 0)) { 
      /*  yydddhhmmss[ss] */
      memcpy(cday,inbuf+2,3);
      memcpy(chr,inbuf+5,2);
      memcpy(cmin,inbuf+7,2);
      memcpy(csec,inbuf+9,2);
    }
    else if (strcmp(fstype,"y") == 0) {
      /*  yyyy.ddd.hh:mm:ss.ss */
      memcpy(cyear,inbuf,4);
      memcpy(cday,inbuf+5,3);
      memcpy(chr,inbuf+9,2);
      memcpy(cmin,inbuf+12,2);
      memcpy(csec,inbuf+15,2);
    } /* log time */
  }

  return;
}
