/* parse_cal_line - Parse a line from the VLBA CAL file

001114 nrv Extracted from vlogx main.

*/

#include <stdio.h>
#include <string.h>

void parse_cal_line(inbuf,scanid,vsource,vsn,vfstart,vstart,vstop,vfstop,vhdpos,vdir,vdrive,vtrack)

/* Input */
char *inbuf;      /* input buffer with the line to parse */
/* Output */
char *scanid,*vsource,*vsn,*vfstart,*vstart,*vstop,*vfstop,*vhdpos,*vdir,*vdrive,*vtrack;

{
  char *ptr_cal,*ptr2;
  int d1;
  char sd1[3];
  

/* The input buffer contains a line from the VLBA cal file. 
Example: 
*RUN-ID  SOURCE   TAPE #    FEET     START      STOP    FEET STATUS HEAD *
188-1839 4C39.25  VLBA1102     0 188-18:39:30 18:45:00   157 ---  -271 F 2 2
*/

  ptr_cal = strtok(inbuf," "); /* scan ID from cal file */
  strcpy(scanid,ptr_cal);
  ptr2 = strtok(NULL," "); /* source name */
  strcpy(vsource,ptr2);
  ptr_cal = strtok(NULL," "); /* VSN */
  strcpy(vsn,ptr_cal);
  ptr_cal = strtok(NULL," "); /* start footage */
  strcpy(vfstart,ptr_cal);
  ptr_cal = strtok(NULL," "); /* start time */
  strcpy(vstart,ptr_cal);
  ptr_cal = strtok(NULL," "); /* stop time */
  memcpy(vstop,vstart,4); /* same day -- MUST ADJUST THIS FOR NEW DAY */
  strcpy(vstop+4,ptr_cal);
  if (strncmp(vstart+4,"23",2)==0 && strncmp(vstop+4,"00",0)==0) { /* new day */
    sscanf(vstart,"%3d",&d1);
    d1++;
    sprintf(sd1,"%03d",d1);
    memcpy(vstop,sd1,3);
  } /* new day */
  ptr_cal = strtok(NULL," "); /* stop footage */
  strcpy(vfstop,ptr_cal);
  ptr_cal = strtok(NULL," "); /* status field */
  ptr_cal = strtok(NULL," "); /* head position */
  strcpy(vhdpos,ptr_cal);
  ptr_cal = strtok(NULL," "); /* direction */
  strcpy(vdir,ptr_cal);
  ptr_cal = strtok(NULL," "); /* tape drive unit */
  strcpy(vdrive,ptr_cal);
  ptr_cal = strtok(NULL," "); /* lowest track used */
  strcpy(vtrack,ptr_cal);

  return;
}
