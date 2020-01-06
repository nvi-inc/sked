/* VLOGX - Generate a field log summary

940411 nrv Created using the Fortran version as a model.
940422 nrv More creation.
940503 nrv Finish writing first part of output file.
940722 nrv Generate run-ID to match schedule time, not tape
           start time
951003 nrv Handle FS9 format log files
970324 nrv New time format with centisec. 1-letter code is in log.
981021 nrv Expanded time format for Y2K. More diagnostic output.
990325 nrv Don't overwrite start time if already found.
990325 nrv Recognize "data_valid" command.
990519 nrv Add option to write VEX output blocks.
991208 nrv Add 'ft' after tape footage. Add 'endscan'.
000113 nrv Clear buffer for source and head positions.
000118 nrv Append source name to scan ID.
000209 nrv Print comments as they appear into the VEX output.
000612 nrv If scan_name commands are in the log, use them, otherwise
           continue to append the source name to the generated scan.
000818 nrv Clear buffer for scan name.
000830 nrv Add sumname to getfiles call. This file has the SNAP summary
           Option 5 listing for the VLBA station.
000830 nrv Allow DATA_VALID=OFF to signal end of scan.
000904 nrv Read and parse VLBA cal file as input.
           Read drudg summary file to get scan names.
001013 nrv Re-set end_tape flag to 0 if it's the one following
           the SOURCE command in continuous recording.
001013 nrv Enable writing old style output for VLBA input.
001030 nrv Add call to read_flag. Add flag file name to getfiles call.
001114 nrv Move to subroutine finding the VLBA station section in
           the cal file. Return the speed too.
001115 nrv Remove parsing cal line to parse_cal_line.
001115 nrv Put the year into the VLBA lvex output.
020327 nrv Add Mk5.
020417 nrv Change start_tape to start_disc for Mk5. Only one
           colon between serial numbers. 
020424 nrv Handle multiple serial numbers.
020425 nrv Generate disc_set_ID label information.
020621 nrv Put disc_set_ID and disc_serial in all scan blocks.
020903 nrv disc_set_ID one char shorter, new format "ss-yyddd-hhmm/nn"
020903 nrv Make inbuf longer, to read long list of disk serial numbers
020903 nrv drudg did not put DISC_POS in parallel with the TAPE command
           at early start, and the disc position is not recognized when
           it appears at normal tape start time. Change the logic to
           allow the disc_pos to be used when it appears.
2003Mar03  JMG Made DISC_SERIAL process two kinds of scans.
               Made routine ignore blank lines in log file.
               Ignore commands: /DISC_POS/!" which cause vlogx to crash.
2003Jun02  JMG Modified scan_name to work with both versions of scan_name:
           old:   scan_name=147-1700a
           new:   scan_name=147-1700a,r1072,190
2003Aug26  JMG  modifed to find VSN # for Mark5 recording from bank_check.
                Also, extract disc serial #s from bank_check command.
2003Nov06  JMG  Due to change in MK5 software, field system puts wrong
                thing in bank_check command. Put in a fix to correctly
                get the serial #s.
2003Dec02  JMG  Modified so that would end gracefully if log file ended unexpectedly.
                Previously would hang.
2003Dec23  JMG  Fixed bugs introduced Dec02:  a) space after scan name.
                b) Didn't end with endef
2004Mar24  JMG  Modified to output VSN number in mark5 mode.
                Also, previously VSN was restricted to 9 chars. Now it can be 30.
2004Apr14  JMG  Modified to get rid of "OK" at the end of VSN #'s.
2004Aug30  JMG  Tweaked becuause of changes in FS, i.e. disc_pos-->disk_pos etc
2004Sep03  JMG  r1137wf had ^^ErasedOK at the end of the VSNs for some reason. Got rid of them.
2005Mar11  JMG  Added "rewind" after parse_log1. This is because sometimes the bank_check
                is done before the experiment starts. W/o the rewind, wasn't picking up the bankcheck.
2005Mar25  JMG  Some log files didn't have a disc_serial or disk_serial command.
                In this case vlogx didn't generate apropriate disc_set_id value.
                Modified to generate using results from bank-check.
2006Nov02  JMG  Converted to run under linux
2007Sep27  JMG  Some string variables initialized to "UNKNOWN"
                Replaced call to gets with fgets.
2008May08 JMG   Incorporated changes  made by Kerry in bringing up at USNO.
                disc_serial was not correctly parsing serial numbers.
2008May09 JMG   Vlogx was truncating first serial number. Fixed it.
	Last change:  JMG   9 May 2008   11:34 am
  */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/*
void  close_files(FILE * fp_in, FILE * fp_out,char * sumname);
*/

main (argc,argv)

int argc;
char *argv[];

{
  char *inname;           /* pointer to name of input log file entered on run line*/
  char *drname;           /* pointer to name of input drudg file entered on run line*/
  char *flagname;         /* pointer to name of input flag file entered on run line*/
  char *outname;          /* pointer to name of output summary file entered on run line*/
  char *append;           /* pointer to char "a" or "o" for append or overwrite */
  char vexout[2];           /* pointer to char "v" for VEX output or */
                            /*                 "s" for the old version */
                            /*                 "5" for Mk5 version */
  int  ivexout;
  int  idisc_set_id_found;
  char *ptr,*ptr1,*ptr2,*ptr3,*ptr_cal,*ptr_drg;  /* general use pointers */
  FILE *fp_in;       /* file pointer for input log or cal file */
  FILE *fp_out;      /* file pointer for summary output file */
  FILE *fp_drg;      /* file pointer for drudg summary file, NULL if none */
  FILE *fp_flag;     /* file pointer for flag file, NULL if none */
  char logname[64];
  char sumname[64];
  char inbuf[380];        /* buffer for reading */
  char inbuf_dr[380];        /* buffer for reading from drudg summary file*/
  int inbuf_len = 380;    /* length of inbuf */
  char ans[2];
  int iy,year;               /* year from first log line */
  int ic,d1;
  char sd1[4];
  char cyear[5];          /* year converted to char */
  char fstype[2];         /* FS type */
  char station_name[9];   /* station name from first log line */
  char station_id[2]="?\0";     /* 1-letter station ID */
  char station_id2[3]="??\0";     /* 2-letter station ID for VLBA files*/
  char two_letter_code[3];     /* 2-letter station ID */
  char cversion[9];       /* full FS version, e.g. 9.3.7 */
  char version_msg[25];   /* screen message with version */
  char ssearch[20];       /* search string in VLBA file */
  char outbuf[180];
  int kscan;              /* false until message is printed */
  int scan_name_found;    /* 1=found scan name in the log,
                             0=never found, use generated IDs */
  int done_with_this_scan;  /* 1=found all the lines in the log for a scan, */
  int done_with_this_station;  /* true when a comment line is found in the
                                  VLBA cal file */
  int started_station;  /* true when after the VLBA station processing has
                             already started */
  int preob_flag,midob_flag,postob_flag,start_tape,end_tape,head_flag;
  int first_scan;
  int FS_input;          /* true if the input log is a FS log file */
  int flag_file;        /* true if there is a flag file */
  int dr_scan;          /* true if the scan names come from the drudg summary file */
  int got_VLBA;
  char lasttime[24];
  char lasthead[12];
  int nc,nc_head,nc_source;                 /* character counter */
  int nc_maxsource;  /* longest source name */
  int nc_maxscan;    /* longest scan name */
  int i;
  int off;                        /* offset of control character */
  int idisc,ndisc; /* index for disc count */
  int toff;  /* offset for lasttime buffer */
  char cday[4],chr[3],cmin[3],csec[3]; /* day,hr,min,sec from time field */
  char scanid[30]; /* scan ID */
  char drscanid[30]; /* scan ID from drudg summary file*/
  char scanid_prev[30]; /* scan ID */
  char scan_name[30]; /* scan name from log */
  char vsn[30]="UNKNOWN";    /* tape or disc VSN */
  int max_disc=16; /* maximum number of discs in a set */
  char vser[16][20];    /* disc serial numbers, up to 20 char each */
  char disc_set_ID[17]="UNKNOWN"; /* ss-yyddd-hhmm/nn */
  char vhdpos[9];  /* head position */
  char vstart[19]; /* yyyyynnndnnhnnmnns */
  char v_short[14]; /* nnndnnhnnmnns (lacks the year) */
  char vstop [19]; /* yyyyynnndnnhnnmnns */
  char vfstart [5]; /* starting footage */
  char vdstart [20] ="UNKNOWN"; /* starting disc position when Mark5 starts recording */
  char vdstart0 [20]="UNKNOWN"; /* disc position detected by "disk_pos" */
  int dfeet,fstart; /* footages used for calculations */
  char vfstop  [5]; /* stoping footage */
  char vdstop  [20]="UNKNOWN"; /* stoping disc position */
  char vsource  [20]; /* source from log file */
  char drsource  [20]; /* source from drudg summary*/
  char vsource_prev  [20]; /* previous scan's source */
  char vdir[2],vdir_prev[2];
  char vtrack[3],vdrive[2]; /* lowest track and drive unit */
  int sctime,scday,schr,scmin,scsec; /* day,hr,min,sec from logged scan */
  int drtime,drday,drhr,drmin; /* day,hr,min from drudg scan name */
  int start_time; /* scan start time */
  int old_start; /* saved scan start time */
  int first_day; /* set to first day number found in scans */
  int idnew,ihnew,imnew,isnew; /* adjusted start time */
  int drsc_match; /* true when drudg and logged scan time and source match */
  int dtime;
  int flagtime[1000][2]; /* flag times from flag file, in seconds */
  int found_flag; /* true when flag entry is found */
  int nflag; /* number of station flag entries found in file */
  float tape_speed; /* tape speed calculated by read_cal */
  int iflag; /* counter */
  int iline_read;    /*Count how many lines read  */
  int ivdstart_flag;

/* Initialization */
  ivdstart_flag=0;


/* 1. Get the input log file name and the output file name, and open
      these files.
*/
  if (argc > 1)
    inname = argv[1];  /* log file or VLBA cal file */
  else
    inname = NULL;
  if (argc > 2)
    drname = argv[2];  /* drudg listing file, with scan names */
  else
    drname = NULL;
  if (argc > 3)
    flagname = argv[3]; /* VLBA flag file */
  else
    flagname = NULL;
  if (argc > 4)
    outname = argv[4]; /* output file name */
  else
    outname = NULL;
  if (argc > 5)
    append = argv[5];  /* append or overwrite the output file */
  else
    append = NULL;
  if (argc > 6)
    strcpy(vexout,argv[6]); /* vex or standard or Mk5 output or disc labels */
  else
    vexout[0]='\0';
  if (argc > 7)
    strcpy(station_id2,argv[7]); /* VLBA station ID */
  else
    station_id2[0]='\0';

  strcpy(version_msg,"VLOGX version 2008May09 JMGipson");
  printf("%s\n",version_msg);

  getfiles(&fp_in,&fp_out,&fp_drg,&fp_flag,logname,sumname,inname,drname,flagname,outname,append,vexout);

  dr_scan = (fp_drg != NULL);
  ptr = strstr(logname,".log");
  FS_input = (ptr != NULL);
  strcpy(two_letter_code,ptr-2);
  flag_file = (fp_flag != NULL);
  idisc_set_id_found=0;
/*  ivexout=0
  if (strcmp(vexout,"v")==0 || strcmp(vexout,"5")==0) {ivexout=1;}
*/
  ivexout=(strcmp(vexout,"v")==0 || strcmp(vexout,"5")==0);

/**********************************************************
   2. Read first line of log file to get station name.
      Write header into output file.
************************************************************/

  printf ("Output type is %s\n", vexout); 
  if (FS_input) { /* FS log file input */
    parse_log1(fp_in,fstype,station_name,station_id,&year,cversion);
    sprintf (cyear,"%i",year);
    printf ("FS type is %s\n", fstype);
    rewind(fp_in);

/*  For FS8, the station ID is the last character before 
    the dot in the log file name, e.g.  
          rd9302a.log      ("a" is the station ID)
*/
    if (strcmp(fstype,"8")==0) {
/*        station ID is final character of file name */
      ptr = strchr(logname,'.');
      strncpy(station_id,ptr-1,1);
      station_id[0] = toupper(station_id[0]);
      upper(station_name);
      off=9;
    }
    else if (strcmp(fstype,"9")==0) {
/*        station ID is final character of file name */
      ptr = strchr(logname,'.');
      strncpy(station_id,ptr-1,1);
      station_id[0] = toupper(station_id[0]);
      upper(station_name);
      off=13;
    }
    else if (strcmp(fstype,"2")==0) {
/*    Got station ID from comments in log file, e.g.
          europ2wz.log     ("z" is NOT the station ID) */
      station_id[0] = toupper(station_id[0]);
      off=13;
    }
    else if (strcmp(fstype,"y")==0) {
/*    Got station ID from comments in log file, e.g.
          europ2wz.log     ("z" is NOT the station ID) */
      station_id[0] = toupper(station_id[0]);
      off=20;
    }
    else {
      if (strcmp(cversion,"0")==0) 
        printf("VLOGX98 - That's probably not a log file.\n");
      else
        printf("VLOGX99 - Don't know how to handle logs from FS version %s.\n",cversion);
      exit(1);
    }
  } /* FS log file input */

  else  { /* VLBA cal file input */
    if (strncmp(station_id2,NULL,1)==0) {
      printf("Enter name of VLBA station to process, :: to quit  ");
/*      gets(inbuf);  */
      fgets(inbuf,250, stdin);
      if (strncmp(inbuf,"::",2) == 0)
        exit(1);
      strncpy(station_id2,inbuf,2);
    }
/*  Read drudg summary file to get the 1-letter station ID for lvex output */
    read_drg(fp_drg,station_name,station_id);
/*  Read flag file */
    if (flag_file) nflag = read_flag(fp_flag,station_id2,flagtime);
    printf("Found %d entries for %s in flag file.\n",nflag,station_id2);
  } /* FS or VLBA cal file input */

  if (strcmp(vexout,"s") == 0) {  /* normal header */
    fprintf(fp_out,"*\n");
    fprintf(fp_out,"*Summary of %s for %s, using Field System version %s\n",logname,station_name,cversion);
    printf("*Summary of %s for %s. Station ID %s.\n",logname,station_name,station_id);
    fprintf(fp_out,"*Generated by %s\n",version_msg);
    fprintf(fp_out,"*\n");
    fprintf(fp_out,"*$$%s [<rategen>],[<fwd bias>],[<rev bias>],[<peaking period(sec)>]\n",station_id);
    fprintf(fp_out,"*\n");
    fprintf(fp_out,"$$%s\n",station_id);
    fprintf(fp_out,"*Scan-Id   Source  ##Tape## Foot     Start      Stop  Foot Status Head\n");
  } 
  else if (strcmp(vexout,"v") == 0 || strcmp(vexout,"5") == 0) {  /* lvex header */
    fprintf(fp_out,"*Summary of %s for %s. Station ID %s. \n",logname,station_name,station_id);
    printf("*LVEX summary of %s for %s. Station ID %s. \n",logname,station_name,station_id);
    fprintf(fp_out,"*\n  def %s;   * %s\n",station_id,station_name);
    printf("  def %s   *%s\n",station_id,station_name);
  } /* normal or lvex header */

/********************************************************************
   3. This is the main loop of the program for standard FS log input. 
      For standard output, info goes into outbuf.
      For VEX output, info is collected in variables and written at end.
***********************************************************************/
  /* Get the first line */
  ptr = fgets(inbuf,inbuf_len,fp_in);

  /* find station section and calculated speed in VLBA file */
  if (!FS_input) read_cal(station_id2,fp_in,&tape_speed,&year,inbuf);

  memcpy(outbuf+20,"-  --   ",8);
  done_with_this_station = 0;
  started_station = 0;
  head_flag=0;
  nc_maxsource = 20;
  nc_maxscan = 30;
  first_day = 0;
  first_scan=1;
  kscan=0;
  scan_name_found = 0;

  while (ptr != NULL && !done_with_this_station) { /* process a scan */
/*********************************************************************
    FS log file input section
**********************************************************************/
    if (FS_input) { /* FS log file input */
    if (strncmp(inbuf+off,";\"",2)!=0 && strncmp(inbuf+off,":\"",2)!=0
      && strncmp(inbuf+off,":scan",5)!=0) 
    upper(inbuf); /* uppercase non-comments but not scan_name */
    memcpy(outbuf,"   --        --     ",20);
    /* Leave tape label unchanged  from last scan */
    memcpy(outbuf+28,"  --       --          --     --   ---  -     \0",48);
    done_with_this_scan = 0;
    while (ptr != NULL && !done_with_this_scan) { /* read log file */
/*
      printf("line # %d : %s", iline_read++, inbuf);
      if(iline_read==7914){
	printf("Debug stop \n");}
*/

      if (strncmp(inbuf+off,";\"",2)==0 || strncmp(inbuf+off,":\"",2)==0) {
/*      don't print out the data start/stop comments */
        if (strncmp(inbuf+off+2,"data start",10)!=0 && strncmp(inbuf+off+2,"data stop",9)!=0) {
          if (strcmp(vexout,"s") == 0) /* comments for standard */
            fprintf(fp_out,"\"%s  %s",station_id,inbuf);
          if (strcmp(vexout,"v")==0 || strcmp(vexout,"5")==0) /* comments for VEX */
            fprintf(fp_out,"* %s  %s",station_id,inbuf);
        }
      }
      if (strncmp(inbuf+off,":SOURCE",7)==0 || 
          strncmp(inbuf+off,":scan_name=",11)==0) { /* start a scan */
        lasttime[0]='\0';
        preob_flag=0;
        midob_flag=0;
        postob_flag=0;
        start_tape=0;
        end_tape=0;
        if (strncmp(inbuf+off,":SOURCE",7)==0 ) { /* SOURCE command */
          ptr1=strchr(inbuf+1+off,'=')+1; 
          ptr2=strchr(inbuf+1+off,','); 
          if (ptr2 != NULL) { 
            nc_source = ptr2-ptr1;
            memcpy(outbuf+10,ptr1,nc_source);
            strncpy(vsource,ptr1,nc_source);
            vsource[nc_source]=0;
          }
        }
        else if (strncmp(inbuf+off,":scan_name=",11)==0) { /* SCAN_NAME command */
          scan_name_found = 1;
          ptr1=strchr(inbuf+1+off,'=')+1;
          ptr2=strchr(ptr1,',');
          if(ptr2==NULL){
            nc = strlen(inbuf+off)-11-1;
        }
          else{
            nc=ptr2-ptr1;
        }
          if (nc>30) nc=30;
          strncpy(scan_name,ptr1,nc);
          scan_name[nc]='\0';
        }
      } /* start a scan */
      else if (strncmp(inbuf+off,"/LABEL",6)==0) { /* new tape label command */
        ptr1=strchr(inbuf+1+off,'/')+1;
        ptr2=strchr(ptr1,',');
        nc=ptr2-ptr1;
        memcpy(outbuf+20,ptr1,nc);
        strncpy(vsn,ptr1,nc);
        vsn[nc]='\0';
        }
/* Bank Check Command. Put in 26Aug2003 */
      else if(strncmp(inbuf+off,"/BANK_CHECK/",12)==0){
        ptr1=inbuf+off+12;
        ptr2=strchr(ptr1,',');
        nc=ptr2-ptr1;
/* check to see if VSN is terminated by "OK". If so, get rid of it */
        strncpy(vsn,ptr1,nc);
/* This checks for \36 or hex 1E  */
        if(ptr3=strchr(ptr1,'\36')){
           nc=ptr3-ptr1;
        }
        if(strncmp(vsn+nc-2,"OK",2)==0){
            nc=nc-2;
       }
        vsn[nc]='\0';
/* Now extract serial #s */

        ndisc=0;
        while (ndisc<max_disc && nc>0 && ptr1 != NULL && ptr2 != NULL) { /* parse */
          ptr1=strchr(ptr2,',');
          if(ptr1 != NULL){
            ptr1=ptr1+1;
            ptr2=strchr(ptr1,',');
            if(ptr2==NULL)
              nc=strlen(ptr1)-1;
            else
              nc=ptr2-ptr1;
            if(nc !=0) {
              strncpy(vser[ndisc],ptr1,nc);
              vser[ndisc++][nc]='\0';
              ptr1=ptr2;
              }
        }
        } /* parse */
        ndisc;

/* Generate pseudo disc_set_id if we haven't already done it. */
        if(!idisc_set_id_found){
          idisc_set_id_found=1;              /* Indicate that we have this */
          getydhms(inbuf,fstype,cyear,cday,chr,cmin,csec);
          upper(two_letter_code);
          memcpy(disc_set_ID,two_letter_code,2);
          memcpy(disc_set_ID+2,"-",1);
          memcpy(disc_set_ID+3,cyear+2,2);
          memcpy(disc_set_ID+5,cday,3);
          memcpy(disc_set_ID+8,"-",1);
          memcpy(disc_set_ID+9,chr,2);
          memcpy(disc_set_ID+11,cmin,2);
          sprintf(disc_set_ID+13,"/%02d",ndisc);
          disc_set_ID[16]=0;
           }
        }
      else if ((strncmp(inbuf+off,"/DISC_SERIAL",12)==0) ||
               (strncmp(inbuf+off,"/DISK_SERIAL",12)==0))   { /* disc serial numbers */
       printf("Are here \n");

/*  On entry to loop ptr1 points at first serial number */
        ptr1=strchr(inbuf+1+off,'/');
        ptr2=strchr(ptr1,',');   /*just so we don't jump out of loop */
        nc=ptr2-ptr1; 

        ndisc=0;
        while (ndisc<max_disc && nc>0 && ptr1 != NULL && ptr2 != NULL) { /* parse */
          if(ndisc !=0) ptr1=strchr(ptr2,',');    /* Skip on first */
          if(ptr1 != NULL){
            ptr1=ptr1+1;
            ptr2=strchr(ptr1,',');
            if(ptr2==NULL)
              nc=strlen(ptr1)-1;
            else
              nc=ptr2-ptr1;
            if(nc != 0) {
              strncpy(vser[ndisc],ptr1,nc);
              vser[ndisc++][nc]='\0';
              ptr1=ptr2;
            }
        }
        } /* parse */

/* Generate disc_set_ID using the actual time that serial numbers were logged */
/* Form is ss-yyddd-hhmm/n where
    ss station 2-letter code
    date/time refers to the time the serial numbers were logged
    n is the number of discs in the set 
*/
/*   disc labels
      if (strcmp(vexout,"d")==0 ) {  */
        idisc_set_id_found=1;              /* Indicate that we have this */
        getydhms(inbuf,fstype,cyear,cday,chr,cmin,csec);
        upper(two_letter_code); 
        memcpy(disc_set_ID,two_letter_code,2);
        memcpy(disc_set_ID+2,"-",1);
        memcpy(disc_set_ID+3,cyear+2,2);
        memcpy(disc_set_ID+5,cday,3);
        memcpy(disc_set_ID+8,"-",1);
        memcpy(disc_set_ID+9,chr,2);
        memcpy(disc_set_ID+11,cmin,2);
        sprintf(disc_set_ID+13,"/%02d",ndisc);
        disc_set_ID[16]=0;
/*        fprintf(fp_out,"disc_set_ID is %s \n",disc_set_ID);
      }  */
      } /* disc serial numbers */
      else if (strncmp(inbuf+off,":!",2)==0) { /* save the ! time command */
          toff = off;
          if ( strchr(inbuf+off,'.') != NULL ) toff=20;
          /* The log time and the SNAP wait times may be different
             formats if the SNAP was not made with the same FS version. */
          memcpy(lasttime,inbuf+off,toff); /* keep the ! in the string */
      }
      else if (strncmp(inbuf+off,"/TAPE",5)==0) { /* TAPE command */
/* 2006Nov02 Fixed bug. Sometimes spurious TAPE w/o ',' */
        ptr1=strchr(inbuf+1+off,',');
        if(ptr1!= NULL){
          ptr1=ptr1+1;
          ptr2=strchr(ptr1,',');
          nc=ptr2-ptr1;
          if (nc >0 && nc <=8) { /* valid tape footage */
            if (!start_tape && !end_tape) {
              memcpy(outbuf+29,ptr1,nc); /* starting footage */
              strncpy(vfstart,ptr1,nc);
            }
            else if (end_tape || done_with_this_scan) {
              memcpy(outbuf+57,ptr1,nc); /* ending footage */
              strncpy(vfstop,ptr1,nc);
            }
          }
        }
      }
      else if (strncmp(inbuf+off,"/DISC_POS/!",11)==0 ||
               strncmp(inbuf+off,"/DISK_POS/!",11)==0)
           {    /*Ignore */
    }
      else if (strncmp(inbuf+off,"/DISC_POS",9)==0 ||
               strncmp(inbuf+off,"/DISK_POS",9)==0)  { /* DISC_POS command */
        ptr1=strchr(inbuf+1+off,'/')+1;
        ptr2=strchr(ptr1,',');
        nc=ptr2-ptr1;
          if (!start_tape && !end_tape) { 
            strncpy(vdstart0,ptr1,nc); /* starting disc position */
            vdstart0[nc]='\0';
          }
          else if (end_tape || done_with_this_scan) {
            strncpy(vdstop,ptr1,nc); /* ending disc position */
            vdstop[nc]='\0';
          }
      }
      else if(strncmp(inbuf+off,":DISK_RECORD=ON",15)==0) {
          strcpy(vdstart,vdstart0);
          ivdstart_flag=1;}

      else if (strncmp(inbuf+off,":ST",3)==0 ||
               strncmp(inbuf+off,":\"data start",12)==0 ||
               strncmp(inbuf+off,":DATA_VALID=ON",14)==0 ||
               strncmp(inbuf+off,":DISC_START",11)==0    ||
               strncmp(inbuf+off,":DISK_START",11)==0) { /* ST, "data start", or data_valid */
               if(ivdstart_flag !=1) {
                strcpy(vdstart,vdstart0);
                ivdstart_flag=1;}
        if (!start_tape) { /* no start time yet */
          start_tape=1;
          /* NOTE: Scan ID will get overwritten when MIDOB appears. */
          if (lasttime[0] != '\0') { /* Generate scan ID using ! time command */
            getydhms(lasttime,fstype,cyear,cday,chr,cmin,csec);
            memcpy(outbuf,cday,3);
            memcpy(outbuf+3,"-",1);
            memcpy(outbuf+4,chr,2);
            memcpy(outbuf+6,cmin,2);
            memcpy(scanid,outbuf,8);
            scanid[8]=0;
          }
          else { /* Generate scan ID using actual tape start time */
            getydhms(inbuf,fstype,cyear,cday,chr,cmin,csec);
            memcpy(outbuf,cday,3); 
            memcpy(outbuf+3,"-",1);
            memcpy(outbuf+4,chr,2);
            memcpy(outbuf+6,cmin,2);
            memcpy(scanid,outbuf,8);
            scanid[8]=0;
          }
/*        Put the start time into the output file. */
          getydhms(inbuf,fstype,cyear,cday,chr,cmin,csec);
          memcpy(outbuf+35,cday,3);
          memcpy(outbuf+38,"-",1);
          memcpy(outbuf+39,chr,2);
          memcpy(outbuf+41,":",1);
          memcpy(outbuf+42,cmin,2);
          memcpy(outbuf+44,":",1);
          memcpy(outbuf+45,csec,2);
          memcpy(vstart,cyear,4);
          memcpy(vstart+4,"y",1);
          memcpy(vstart+5,cday,3);
          memcpy(vstart+8,"d",1);
          memcpy(vstart+9,chr,2);
          memcpy(vstart+11,"h",1);
          memcpy(vstart+12,cmin,2);
          memcpy(vstart+14,"m",1);
          memcpy(vstart+15,csec,2);
          memcpy(vstart+17,"s\0",2);
          if (head_flag) {
            memcpy(outbuf+67,lasthead,nc_head);
            strncpy(vhdpos,lasthead,nc_head);
            vhdpos[nc_head]='\0';
          }
        }
      }
      else if (strncmp(inbuf+off,":ET",3)==0 ||
               strncmp(inbuf+off,":DATA_VALID=OFF",15)==0 ||
               strncmp(inbuf+off,":DISC_END",9)==0  ||
               strncmp(inbuf+off,":DISK_END",9)==0) { /* ET command or DATA_VALID=OFF */
        end_tape=1;
/*      Don't set end_tape until start_tape has been set. This prevents
        the ET following the SOURCE command in continuous from setting
        the end_tape flag too soon.
*/
        if (!start_tape) end_tape=0;
        getydhms(inbuf,fstype,cyear,cday,chr,cmin,csec);
        memcpy(outbuf+48,chr,2);
        memcpy(outbuf+50,":",1);
        memcpy(outbuf+51,cmin,2);
        memcpy(outbuf+53,":",1);
        memcpy(outbuf+54,csec,2);
        memcpy(vstop,cyear,4);
        memcpy(vstop+4,"y",1);
        memcpy(vstop+5,cday,3);
        memcpy(vstop+8,"d",1);
        memcpy(vstop+9,chr,2);
        memcpy(vstop+11,"h",1);
        memcpy(vstop+12,cmin,2);
        memcpy(vstop+14,"m",1);
        memcpy(vstop+15,csec,2);
        memcpy(vstop+17,"s\0",2);
      }
      else if (strncmp(inbuf+off,"/PASS",5)==0) { /* HEAD command */
        head_flag=1;
        ptr1=strchr(inbuf+1+off,',')+1;
        ptr1=strchr(ptr1,',')+1;
        ptr1=strchr(ptr1,',')+1;
        ptr2=strchr(ptr1,',');
        nc_head=ptr2-ptr1;
        memcpy(lasthead,ptr1,nc_head);
      }
      else if (strncmp(inbuf+off,":PREOB",6)==0) { /* PREOB command */
        preob_flag=1;
      }
      else if (strncmp(inbuf+off,":MIDOB",6)==0) { /* MIDOB command */
        midob_flag=1;
/*      Use the following 3 lines if you want to have the Run-ID generated
        for the good data time.  */
        getydhms(lasttime,fstype,cyear,cday,chr,cmin,csec);
        memcpy(outbuf,cday,3);
        memcpy(outbuf+3,"-",1);
        memcpy(outbuf+4,chr,2); 
        memcpy(outbuf+6,cmin,2); 
        memcpy(scanid,outbuf,8);
        scanid[8]=0;
        sscanf(cday,"%d",&scday);
        sscanf(chr,"%d",&schr);
        sscanf(cmin,"%d",&scmin);
      }
      else if (strncmp(inbuf+off,":POSTOB",7)==0  ||
               strncmp(inbuf+off,":DSNPO",6)==0) { /* POSTOB command */
        postob_flag=1;
        done_with_this_scan=1;
      }
      ptr = fgets(inbuf,inbuf_len,fp_in);   /* read next line */

      if(strncmp(inbuf+off+1,"*end of schedule",16)==0)
       {close_files(fp_in,fp_out,sumname,ivexout,0);}
      if(!ptr){
         close_files(fp_in, fp_out, sumname,ivexout,1);}
      while(inbuf[0]=='\n'){            /*ignore blank lines */
        ptr = fgets(inbuf,inbuf_len,fp_in); /* read next line */
       if(!ptr){
         close_files(fp_in, fp_out, sumname,ivexout,1);}
       }

    if (strncmp(inbuf+off,";\"",2)!=0 && strncmp(inbuf+off,":\"",2)!=0
      && strncmp(inbuf+off,":scan",5)!=0) 
      upper(inbuf); /* uppercase non-comments but not scan_name */
    } /* read log file */
    } /* FS log file input */

/*********************************************************************
    VLBA cal file input section
**********************************************************************/
    else { /* VLBA cal file input */
      done_with_this_scan = 0;
      memcpy(outbuf,"   --        --     ",20);
      /* Leave tape label unchanged  from last scan */
      memcpy(outbuf+28,"  --       --          --     --   ---  -     \0",48);
      while (ptr != NULL && !done_with_this_scan &&
          !done_with_this_station) { /* read input file */
        if (strncmp(inbuf,"*",1)!=0) { /* valid line */
          parse_cal_line(inbuf,scanid,vsource,vsn,vfstart,vstart,vstop,vfstop,vhdpos,vdir,vdrive,vtrack);
          nc=strlen(scanid);
          memcpy(outbuf,scanid,nc);
          nc_source = strlen(vsource);
          memcpy(outbuf+10,vsource,nc_source);
          nc=strlen(vsn);
          memcpy(outbuf+20,vsn,nc);
          nc=strlen(vfstart);
          memcpy(outbuf+29,vfstart,nc); 
          sscanf(vstart,"%3d-%2d:%2d:%2d",&scday,&schr,&scmin,&scsec);
          if (first_day == 0) first_day = scday;
          start_time = (scday-first_day)*86400 + schr*3600 + scmin*60 + scsec;

/*        Adjust starting time depending on flagging. If the starting
          time falls between a flagged pair, meaning the stations was
          off source, change the start time to be the end of that
          period plus 1 second.
*/
          if ( flag_file ) { /* got a flag file */
            iflag = 0;
            found_flag = 0;
            while (iflag <= nflag && !found_flag) { /* search flag array */
/*              if (start_time >= flagtime[iflag][0] && start_time <= flagtime[iflag][1]) { */
              if (start_time <= flagtime[iflag][1]) { 
                old_start = start_time;
                start_time = flagtime[iflag][1] + 1; /* change start time */
                found_flag = 1;
/*              Now change vstart to have the new value */
                idnew =  start_time/86400.0;
                ihnew = (start_time-idnew*86400.0)/3600.0;
                imnew = (start_time-idnew*86400.0-ihnew*3600.0)/60.0;
                isnew =  start_time-idnew*86400.0-ihnew*3600.0-imnew*60.0;
                idnew = idnew + first_day;
                sprintf(vstart,"%03dd%02dh%02dm%02d",idnew,ihnew,imnew,isnew);
/*              Adjust footage for the new time */
                dfeet = (start_time-old_start)*tape_speed/12.0;
                sscanf(vfstart,"%d",&fstart);
                sprintf(vfstart,"%d",fstart+dfeet);
              }
              if (start_time < flagtime[iflag][0] && !found_flag) { 
                fprintf(fp_out,"* %s   antenna was off-source during the following scan\n",station_id);
                found_flag = 1;
              }
              iflag++;
            } /* search flag array */
          } /* got a flag file */

          memcpy(vstart+3,"d",1);
          memcpy(vstart+6,"h",1);
          memcpy(vstart+9,"m",1);
          strcat(vstart,"s");
          memcpy(outbuf+35,vstart,3);
          memcpy(outbuf+38,"-",1);
          memcpy(outbuf+39,vstart+4,2);
          memcpy(outbuf+41,":",1);
          memcpy(outbuf+42,vstart+7,2);
          memcpy(outbuf+44,":",1);
          memcpy(outbuf+45,vstart+10,2);
          memcpy(vstop+3,"d",1);
          memcpy(vstop+6,"h",1);
          memcpy(vstop+9,"m",1);
          strcat(vstop,"s");
          memcpy(outbuf+48,vstop+4,2);
          memcpy(outbuf+50,":",1);
          memcpy(outbuf+51,vstop+7,2);
          memcpy(outbuf+53,":",1);
          memcpy(outbuf+54,vstop+10,2);
/*        Fix up vstart and vstop which don't have the year yet */
          memcpy(v_short,vstart,13); /* save ddddhhhmmmsss */
          sprintf(vstart,"%4dy",year); /* put yyyyy into vstart */
          memcpy(vstart+5,v_short,13); /* move dhms back into vstart */
          memcpy(v_short,vstop,13); /* save ddddhhhmmmsss */
          sprintf(vstop,"%4dy",year); /* put yyyyy into vstart */
          memcpy(vstop+5,v_short,13); /* move dhms back into vstart */
          nc=strlen(vfstop);
          memcpy(outbuf+57,vfstop,nc); /* ending footage */
          nc_head=strlen(vhdpos);
          memcpy(outbuf+67,vhdpos,nc_head);
          done_with_this_scan=1;
          started_station=1;
        } /* valid line */
        ptr = fgets(inbuf,inbuf_len,fp_in); /* read next line */
        done_with_this_station = (strncmp(inbuf,"*",1)==0 ||
          ptr==NULL) && started_station;
      } /* read input file */
    } /* VLBA cal file input */

/*********************************************************************
    Write output line or lvex block 
**********************************************************************/
    if (done_with_this_scan) { /* write output for this scan */
      if (strcmp(vexout,"s")==0) { /* old style output */
        fprintf(fp_out,"%s\n",outbuf);
      } /* old style output */
      else if (strcmp(vexout,"v")==0 || strcmp(vexout,"5")==0) { /* VEX */
        if (dr_scan) { /* scan names from drudg summary file */
          if (kscan == 0) printf("Scan names taken from drudg summary file.\n");
          kscan=1;
          drsc_match = 0;
          ptr_drg = fgets(inbuf_dr,inbuf_len,fp_drg); /* next scan line */
          while (ptr_drg!=NULL && !drsc_match ) { /* match time and source */
            while (ptr_drg!=NULL && (strncmp(inbuf_dr+1,"1",1)!=0 && 
                strncmp(inbuf_dr+1,"2",1)!=0 && 
                strncmp(inbuf_dr+1,"3",1)!=0)) { /* get valid line */
              ptr_drg = fgets(inbuf_dr,inbuf_len,fp_drg);
              } /* get valid line */
            if (ptr_drg!=NULL) { /* valid line */
              ptr = strtok(inbuf_dr," ");
              strcpy(drscanid,ptr);
              sscanf(ptr,"%3d-%2d%2d",&drday,&drhr,&drmin);
              ptr = strtok(NULL," "); /* skip line # */
              ptr = strtok(NULL," "); /* source name */
              strcpy(drsource,ptr);
              sscanf(vstart,"%4dy%3d-%2d:%2d:%2d",&iy,&scday,&schr,&scmin,&scsec);
              sctime = schr*60 + scmin;
              drtime = drhr*60 + drmin;
              if (drday != scday) drtime = drtime + 1440;
              dtime = drtime-sctime;
/*          printf("Matching dr: %s %03d-%02d%02d to log: %s %03d-%02d%02d\n",
              drsource,drday,drhr,drmin,vsource,scday,schr,scmin);
*/
              drsc_match = (dtime < 30) && (strcmp(drsource,vsource)==0);
            } /* valid line */
            if (!drsc_match) ptr_drg = fgets(inbuf_dr,inbuf_len,fp_drg); 
          } /* match time and source */
          if (drsc_match) {
            fprintf(fp_out,"    scan %s;\n",drscanid);
          }
          else { 
            fprintf(fp_out,"    scan %s; * scan name not found in drudg summary \n",scanid);
            printf("No scan name found for %s %03d-%02d%02d\n",
                    vsource,scday,schr,scmin);
            rewind(fp_drg);
          }
        } /* scan names from drudg summary file */
        else if ( scan_name_found ) { /* logged scan names */
          if (kscan == 0) printf("Scan names found in log file.\n");
          kscan=1;
          fprintf(fp_out,"    scan %s;\n",scan_name);
        } /* logged scan names */
        else { /* generated scan id + source name */
          if (kscan == 0) printf("Scan names generated from logged tape start times.\n");
          kscan=1;
          strncat(scanid,"_",1); 
          strcat(scanid,vsource);
          fprintf(fp_out,"    scan %s;\n",scanid);
        } /* old/drudg/logged/generated */

        if (strcmp(vexout,"v")==0) { /* vex tape output */
          fprintf(fp_out,"      VSN = %s;\n",vsn);
          fprintf(fp_out,"      head_pos = %s um;\n",vhdpos);
          fprintf(fp_out,"      start_tape = %s : %s ft : 0 in/sec;\n",vstart,vfstart);
          fprintf(fp_out,"      stop_tape =  %s : %s ft ;\n",vstop,vfstop);
          fprintf(fp_out,"      source = %s;\n",vsource);
        } /* vex tape output */
        else if (strcmp(vexout,"5")==0) { /* vex Mk5 output */
/*        write serial numbers on first scan only
          if (first_scan)    */
/* Added 2004Mar24 JMGipson */
            fprintf(fp_out,"      VSN = %s;\n",vsn);
/* End  2004Mar24 JMGipson */
            fprintf(fp_out,"      disc_set_ID = %s : %d ;\n",disc_set_ID,ndisc);
            fprintf(fp_out,"      disc_serial = ");
            for(idisc=0;idisc<ndisc;idisc++){
              fprintf(fp_out,"%s : ",vser[idisc]);}
              fprintf(fp_out,"; \n");

            first_scan=0;
/*          }  */
          fprintf(fp_out,"      start_disc = %s : %s ;\n",vstart,vdstart);
          fprintf(fp_out,"      stop_disc =  %s : %s ;\n",vstop,vdstop);
          fprintf(fp_out,"      source = %s;\n",vsource);
          ivdstart_flag=0;          /* Flag indicating need to reset start. */
        } /* vex Mk5 output */
        if (!FS_input) { /* check for reversals */
          if (strcmp(vsource,vsource_prev)==0 &&
              strcmp(vdir,vdir_prev)!=0) { 
            printf("Auto-reverse during %s\n",scanid_prev);
          }
        } /* check for reversals */
        strcpy(vsource_prev,vsource);
        strcpy(vdir_prev,vdir);
        strcpy(scanid_prev,scanid);
        fprintf(fp_out,"    endscan;\n");
      } /* old style/lvex output */
    } /* write output for this scan */
    
  } /* process a scan */

/* 4. Close files.
*/
  close_files(fp_in,fp_out,sumname,ivexout,0);
}


int close_files(FILE *fp_in,FILE *fp_out,char *sumname,int ivexout,int ierr){
/*  FILE *fp_in;
  FILE *fp_out;      If these are in, program bombs! */


  if(ivexout){
    fprintf(fp_out,"  enddef;\n*\n");
  }
  fclose(fp_in);
  fclose(fp_out);
  if(ierr){
    printf("Log file ended unexpectedly. Output file %s written.\n",sumname);
    exit(3);}
  else{
    printf("Output file %s written.\n",sumname);
    exit(0);}
}

