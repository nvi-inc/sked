/* read_cal - Read the VLBA cal file for VLOGX

001114 nrv New. Removed from main. Add speed calculation. On return fp_in 
           is pointed at the comment line for the requested station.
001116 nrv Quit if you don't find the right section in the file.
           Rewind between searches for various lines.
2007Sep26 JMG. Replace gets by fgets
2008May08 JMG.  Added stdlib.h

*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int read_cal(station_id2,fp_in,tape_speed,year,inbuf)
FILE *fp_in;         /* file pointers */
char *station_id2;      /* station ID, 2 characters upper case */
char *inbuf;
float *tape_speed; /* calculated speed */
int *year;

{
  int inbuf_len = 100;
  int got_VLBA;
  char *ptr,*ptr2;
  char ssearch[30];
  char scanid[30]; /* scan ID */
  char vsource  [20]; /* source from log file */
  char vsn[9];    /* tape VSN */
  char vstart[19]; /* nnndnnhnnmnns */
  char vfstart [5]; /* starting footage */
  char vfstop  [5]; /* stoping footage */
  char vstop [19]; /* nnndnnhnnmnns */
  char vhdpos[9];  /* head position */
  char vdir[2]; /* direction */
  char vdrive[2];  /* tape drive unit */
  char vtrack[3]; /* lowest track used */
  int scday2,scday,schr,scmin,scsec; /* scanned time fields */
  int start_time, stop_time; /* start, stop in seconds */
  int start_feet, stop_feet; /* start, stop footages */
  float speed; /* speed calculated from one scan line */
  int valid_speed; /* true when a valid speed is found */
  float ok_speed[8]; /* list of valid speeds, ips */
  float max_speed,min_speed; /* ok_speed +/- 10% */
  int max_speed_index = 4; 
  int is;
  int ispeed; /* integer version for printing */
  int y; /* scanned year */

    ok_speed[0] = 40;
    ok_speed[1] = 80;
    ok_speed[2] =160;
    ok_speed[3] =270;
    ok_speed[4] =320;

/* Find the year first. Sample line:
! For UT timerange: 2000JUL06/188 at 18:37:59 to 2000JUL07/189 at 19:07:59
*/ 
    strcpy(ssearch,"timerange");
    got_VLBA = 0;
    while ((ptr!=NULL) && !got_VLBA) {
      ptr = fgets(inbuf,inbuf_len,fp_in);
      ptr2 = strstr(inbuf,ssearch); /* look for station ID line */
      got_VLBA = (ptr2 != NULL);
    }
    ptr = strtok(inbuf," "); /*  !  */
    ptr2 = strtok(NULL," ");     /* For */
    ptr2 = strtok(NULL," ");     /* UT */
    ptr2 = strtok(NULL," ");     /* timerange */
    ptr2 = strtok(NULL," ");     /* timerange */
    sscanf(ptr2,"%4d",&y);
    *year = y;

/*  Sample line to look for: * ----- MkIII information for SC ----- */
/*    memcpy(ssearch,"information for ",16); */
    rewind(fp_in);
    strcpy(ssearch,"information for ");
    upper(station_id2);
    strcat(ssearch,station_id2);
    strcat(ssearch," ");
    got_VLBA = 0;
    while ((ptr!=NULL) && !got_VLBA) {
      ptr = fgets(inbuf,inbuf_len,fp_in);
      ptr2 = strstr(inbuf,ssearch); /* look for station ID line */
      got_VLBA = (ptr2 != NULL);
    }
    if (got_VLBA) {
      printf("VLBA CAL file section found: %s",inbuf);
    }
    else {
      printf("Didn't find the string < %s > in the VLBA cal file.\nQuitting.\n",ssearch);
      exit(1);
    }

/*  Now read some lines to calculate the speed */
    valid_speed = 0;
    while (ptr!=NULL && !valid_speed) { /* read cal file */
      if (strncmp(inbuf,"*",1)!=0) { /* valid line */
        parse_cal_line(inbuf,scanid,vsource,vsn,vfstart,vstart,vstop,vfstop,vhdpos,vdir,vdrive,vtrack);
        sscanf(vfstart,"%d",&start_feet);
        sscanf(vfstop,"%d",&stop_feet);
        sscanf(vstart,"%3d-%2d:%2d:%2d",&scday,&schr,&scmin,&scsec);
        start_time = schr*3600 + scmin*60 + scsec;
        sscanf(vstop,"%3d-%2d:%2d:%2d",&scday2,&schr,&scmin,&scsec);
        stop_time = schr*3600 + scmin*60 + scsec;
        if (scday2>scday) stop_time = stop_time + 86400;
        speed = 12.0*abs(stop_feet-start_feet)/(stop_time-start_time);
        is = 0;
        valid_speed = 0;
        while (is<=max_speed_index && !valid_speed) { /* test speed */
          max_speed = ok_speed[is]*1.10;
          min_speed = ok_speed[is]*0.90;
          valid_speed = speed > min_speed && speed < max_speed; 
          is++;
        } /* test speed */

      } /* valid line */

      if (valid_speed) {
        is--;
        ispeed = ok_speed[is];
        printf("Tape speed is estimated to be %d ips based on times and footages.\n",ispeed);
        *tape_speed = ok_speed[is];
      }
      ptr = fgets(inbuf,inbuf_len,fp_in);
    } /* read cal file */

/*  If a valid speed wasn't found, ask for one. */
    while ( !valid_speed ) { 
      printf("Valid tape speed was not found in the VLBA cal file.\nValid speeds are 40,80,133.33,135,160,270,266.66,320\nPlease enter correct tape speed, :: to quit  ");
      fgets(inbuf,inbuf_len,stdin);
      ptr=strchr(inbuf,'\n');
      if(ptr) *ptr='\0';           /* replace new line by null */


      if (strncmp(inbuf,"::",2) == 0) exit(1);
      sscanf(inbuf,"%f",speed);
      is = 0;
      valid_speed = 0;
      while (is<=max_speed_index && !valid_speed) { /* test speed */
        valid_speed = (ok_speed[is]+0.05*ok_speed[is]) < speed &&
                      (ok_speed[is]-0.05*ok_speed[is]) > speed; 
        is++;
      } /* test speed */
    }

/*  Go back and find the right section again to leave the file pointer there */
    rewind(fp_in);
    got_VLBA = 0;
    ptr = fgets(inbuf,inbuf_len,fp_in);
    while ((ptr!=NULL) && !got_VLBA) {
      ptr = fgets(inbuf,inbuf_len,fp_in);
      ptr2 = strstr(inbuf,ssearch); /* look for station ID line */
      got_VLBA = (ptr2 != NULL);
    }

    return;

}
