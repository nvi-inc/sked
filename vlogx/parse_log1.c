/* parse_log1 parses the first line of a Mark III log file.

Called by: 
940422 nrv Created.
951003 nrv Read FS9 format log files.
960306 nrv Read station name from FS9 "location" line. Change
           call to send in file pointer
970324 nrv Read 1-letter code from first comment line if the
           FS version is >= 9.3.8.
981021 nrv Recognize the new time format if the FS
           FS version is >= 9.3.201.
990325 nrv Correct the use of strtok to parse version 8 first line.
2003JMG    Corrected memcpy which wasn't using lenght.
	   Correected checking for field system version
2007Sep27 JMGipson.   Tsukuba logs didn't have ";" in the correct place.
          Fixed so that if it doesn't find  this, searches for "." in 5th char of input string.

This routine scans inbuf to find the station name, year, FS version,
and occupation code. A typical first line is:

for fstype "8"
fstype 8 is for any version 8
075192633;MARK IV Field System Version  8.2 gilcreek 1994 40479302

for fstype "9"
fstype 9 is for versions 9 <=9.3.7
9507519263324;Log Opened: MARK IV Field System Version 9.0.6
9507519263324;location, NRAO20M ,W, ... etc.

for fstype "2"
fstype 2 is for versions 9 >=9.3.8
9707017122220;Log Opened: Mark IV Field System Version 9.3.8
9707017122221;location,HartRAO ,J, etc.
9707017122223:" ca005     1997 hartrao   j Hh

for fstype "y"
fstype y is for versions >=9.3.201
1998.293.17:56:22.16;Log Opened: Mark IV Field System Version 9.3.201
1998.293.17:56:22.16;location,GGAO7108,Z,etc.
1998.293.17:56:22.21:" NA286     1998 NYALES20  O   Ny

Formats for the fstype are initially distinguished by where the first
";" is found: 
   offset 9 (for fstype 8) 
   offset 13 (for fstype 9 and 2)
   offset 20 (for fstype y)

The format for fstype "8" is read as follows:
1) scan up to the ";"
2) skip the string "MARK IV Field System Version"
3) pick up the next field as a floating value, the version number
4) pick up the next field as a string, the station name
5) pick up the next field as an integer, the full year
6) pick up the next field as a string, the occupation code

The format for fstype "9" and "2" and "y" is read as follows:
1) scan up to the ";"
2) use the first two characters of the line as year number,
   add 1900 to values greater than 90, 2000 to values smaller.
3) pick off the last token as the full version number
4) read the next lines until ";location" is found on a line
5) scan up to "location," and use the next field delimited by commas
   as the station name

Additionally for "2" and "y":
6) Find the next comment line from the schedule in the :" line.
7) Take the 4th field after the " as the 1-letter code FOR THIS EXPERIMENT.
   (For "8" and "9" the last letter of the file name is the 1-letter code.)

	Last change:  JQ   26 Sep 2007    8:50 pm
*/

#include <stdio.h>
#include <string.h>
#include <math.h>

void parse_log1(fp_in,fstype,station_name,station_id,year,cversion)
/* Input */
FILE *fp_in;
/* Output */
char *fstype;
char *station_name;
char *cversion;
char *station_id;
int *year;


{
  /* Local */
  char *ptr_save, *ptr, *fptr;
  char inbuf[180];
  int inbuf_len,v1,v2,v3,off;
  int ilen;
  int icount;
/* Read first line of log file. */
  inbuf_len=180;
  fptr = fgets(inbuf,inbuf_len,fp_in);

/* Determine fstype */
  ptr = strchr(inbuf,';');
  if(ptr == NULL){          /* probably a mess-up.  */
    ptr=strchr(inbuf,'.');  /* See if we find "." in the 4th */
    if(ptr== inbuf+4) ptr=inbuf+20;
    }
  if (ptr == inbuf+9) {
    strcpy(fstype,"8");
    off=9;
  }
  else if (ptr == inbuf+13) {
    strcpy(fstype,"9"); /* type "2" is determined later from version */
    off=13;
  }
  else if (ptr == inbuf+20) {
    strcpy(fstype,"y");
    off=20;
  }
  else {
    strcpy(fstype,"0");
    strcpy(cversion,"0");
  }

/* fstype 8 */

  if (strcmp(fstype,"8")==0) {
/* 075192633;MARK IV Field System Version  8.2 gilcreek 1994 40479302 */
    ptr = strtok(inbuf," ");
    ptr = strtok(NULL," ");
    ptr = strtok(NULL," ");
    ptr = strtok(NULL," ");
    ptr = strtok(NULL," ");
    ptr = strtok(NULL," ");
    sscanf(ptr,"%s",cversion);
    ptr = strtok(NULL," ");
    strcpy(station_name,ptr);
    ptr = strtok(NULL," ");
    sscanf(ptr,"%d",year);
    ptr = strtok(NULL,"\012");
  }

/* fstype 9 (and 2) and y */

  else if (strcmp(fstype,"0")!=0 ) { /* beyond 8 */
    if (strcmp(fstype,"9") == 0 ) { /* types 9 and 2 */
/* 9707017122220;Log Opened: Mark IV Field System Version 9.3.8 */
      sscanf(inbuf,"%2d",year); /* first 2 char of the line */
      if (*year > 90) 
        *year=*year+1900;
      else
        *year=*year+2000;
    }
    else if (strcmp(fstype,"y") == 0) { /* type y */
/* 1998.293.17:56:22.16;Log Opened: Mark IV Field System Version 9.3.201 */
      sscanf(inbuf,"%4d",year); /* full year on the line */
    }
    ptr = strtok(inbuf,"V");
    while (ptr != NULL) { /* find last field */
      ptr_save=ptr;
      ptr=strtok(NULL," \012");
    } /* find last field */
    ilen=strlen(ptr_save);
    memcpy(cversion,ptr_save,ilen);  /* FS version field */

/*  Determine the difference between types 9 and 2 from the version. */
    sscanf(cversion,"%d.%d.%d",&v1,&v2,&v3);
    if (v1 >=9 && v2 >= 3 && v3 >= 8) strcpy(fstype,"2");
    if (v1 >=9 && v2 >= 3 && v3 >= 201) strcpy(fstype,"y");
    if (v1 >=9 && v2 >= 4) strcpy(fstype,"y");

  } /* beyond 8 */ 
  else {
    return;
  }

  fptr = fgets(inbuf,inbuf_len,fp_in); /* read until "location" line */
  while (fptr!=NULL && strncmp(fptr+off+1,"location",8)!=0) {
    fptr = fgets(inbuf,inbuf_len,fp_in);
  }
  ptr = strtok(inbuf,"location");
  ptr = strtok(NULL,",");
  ptr = strtok(NULL,","); /* station name ends at 2nd comma */
  strcpy(station_name,ptr); 

  icount=0;
  if (strcmp(fstype,"y") == 0 || strcmp(fstype,"2")==0) { /* get 1-letter code */
    fptr = fgets(inbuf,inbuf_len,fp_in); /* read until first comment */
    while (fptr!=NULL && strncmp(fptr+off,":\"",2)!=0) {
      fptr = fgets(inbuf,inbuf_len,fp_in);
      if(icount++==100){printf("WARNING: Station ID not found!\n"); return;}
    }
    ptr = strtok(inbuf,":\"");
    ptr = strtok(NULL," ");
    ptr = strtok(NULL," ");
    ptr = strtok(NULL," ");
    ptr = strtok(NULL," ");
    ptr = strtok(NULL," "); /* 1-letter code ends at 4th blank */
    strncpy(station_id,ptr,1);
  } /* get 1-letter code */

  return;
}
