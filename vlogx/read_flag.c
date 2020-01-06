/* read_flag - Read the VLBA flag file for VLOGX

001030 nrv New. 

*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int read_flag(fp_flag,station_id2,flagtime)
FILE *fp_flag;         /* file pointers */
char *station_id2;      /* station ID, 2 characters upper case */
int flagtime[][2]; /* flag times */

{
  char inbuf[100];
  int inbuf_len = 100;
  char *ptr,*ptrf,*ptr2,*ptrf2;
  char ssearch[15];
  int day1,hour1,imin1,sec1,day2,hour2,imin2,sec2;
  int first_day;
  int nflag; /* number of lines found for this station */

/* 1. Read first line. Set up match string. 
   Sample line: ant_name='SC' timerang=188,19,14,30,188,19,16,58
*/

  strcpy(ssearch,"ant_name=\'");
  upper(station_id2);
  strcat(ssearch,station_id2);
  ptr = fgets(inbuf,inbuf_len,fp_flag);
  nflag = 0;
  first_day=0;

  while (ptr != NULL) { /* get each line */
    ptr2 = strstr(inbuf,ssearch);
    if (ptr2 != NULL) { /* matching station line */
      ptrf = strtok(inbuf," "); /* ant_name field */
      ptrf = strtok(NULL," ");  /* timerang field */
      ptrf2 = strtok(ptrf,"=");  /* timerang= field */
      ptrf2 = strtok(NULL,",");  /* first day */
      sscanf(ptrf2,"%d",&day1);
      if (first_day == 0) first_day=day1; /* save first day */
      ptrf2 = strtok(NULL,",");  /* first hour */
      sscanf(ptrf2,"%d",&hour1);
      ptrf2 = strtok(NULL,",");  /* first min */
      sscanf(ptrf2,"%d",&imin1);
      ptrf2 = strtok(NULL,",");  /* first sec */
      sscanf(ptrf2,"%d",&sec1);
      ptrf2 = strtok(NULL,",");  /* second day */
      sscanf(ptrf2,"%d",&day2);
      ptrf2 = strtok(NULL,",");  /* second hour */
      sscanf(ptrf2,"%d",&hour2);
      ptrf2 = strtok(NULL,",");  /* second min */
      sscanf(ptrf2,"%d",&imin2);
      ptrf2 = strtok(NULL,",");  /* second sec */
      sscanf(ptrf2,"%d",&sec2);
      flagtime[nflag][0] = (day1-first_day)*86400 + hour1*3600 + imin1*60 + sec1;
      flagtime[nflag][1] = (day2-first_day)*86400 + hour2*3600 + imin2*60 + sec2;
      nflag++;
      if (nflag > 1000) { /* too many */
        printf("Too many flags. Max is 1000.\n");
        exit(1);
      } /* too many */
    } /* matching station line */

  ptr = fgets(inbuf,inbuf_len,fp_flag);
  } /* get each line */

  return nflag;
}
