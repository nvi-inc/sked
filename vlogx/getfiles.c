/* getfiles - Get file names for VLOGX and VLOGD

940505 nrv Extracted from VLOGX
000830 nrv Add input for drudg summary file for VLBA station
001030 nrv Add input flag file
001115 nrv Get flag file name after drudg summary name
020327 nrv Add option '5' to vexout.
020425 nrv Add option 'd' to vexout.
2007Sep27 JMG  Replaced 'gets' with fgets
2008May08 JMG  Added stdlib.h 

	Last change:  JQ   26 Sep 2007    8:33 pm
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void getfiles(fp_in,fp_out,fp_drg,fp_flag,logname,sumname,inname,drname,flagname,outname,append,vexout)
FILE **fp_drg,**fp_in,**fp_out,**fp_flag;         /* file pointers */
char *logname,*sumname;        /* input and output file names */
             /* actual names are needed by caller, not just the file pointers */
char *inname,*drname,*flagname,*outname,*append,*vexout; 
                              /* command line inputs, may be null */

{
  char inbuf[100];
  char ans[3];
  char *ptr;

/* 1.  Input log file */

  *fp_in = NULL;
  if (inname != NULL) {
    *fp_in = fopen(inname,"r");
    if (*fp_in == NULL) {
      printf("Can't open input file %s\n",inname);
      inname = NULL;
    }
  }
  while (*fp_in == NULL) {
    printf("Enter name of log file or VLBA summary file (.cal) , :: to quit  ");
    fgets(inbuf,100,stdin);
    ptr=strchr(inbuf,'\n');
    if(ptr) *ptr='\0';           /* replace new line by null */
    if (strncmp(inbuf,"::",2) == 0)
      exit(1);
    *fp_in = fopen(inbuf,"r");
    if (*fp_in == NULL) printf("Can't open input file %s\n",inbuf);
  }
  if (inname == NULL)
    strcpy(logname,inbuf);
  else
    strcpy(logname,inname);


/* 1.5 Input drudg SNAP summary file with scan names */

  *fp_drg = NULL;
  if (drname == NULL) { /* no drudg file, leave fp null */
  }
  else { /* get drname */
    if (drname != NULL) {
      *fp_drg = fopen(drname,"r");
      if (*fp_drg == NULL) {
        printf("Can't open input file %s\n",drname);
        drname = NULL;
      }
    }
    memcpy(inbuf,"-",1);
    while (*fp_drg == NULL && strncmp(inbuf,"0",1)!=0) {
      printf("Enter name of drudg summary file, :: to quit, 0 for none ");
      fgets(inbuf,100,stdin);
      ptr=strchr(inbuf,'\n');
      if(ptr) *ptr='\0';           /* replace new line by null */

      if (strncmp(inbuf,"::",2) == 0)
        exit(1);
      if (strncmp(inbuf,"0",1) != 0) {
          *fp_drg = fopen(inbuf,"r");
        if (*fp_drg == NULL) printf("Can't open drudg summary file %s\n",inbuf);
      }
    }
    if (strncmp(inbuf,"0",1) == 0)
      *fp_drg = NULL;
  } /* get drname */

/* 1.25 Input flag file */

  *fp_flag = NULL;
  if (flagname==0) { /* no flag file, leave fp null */
  }
  else { /* get flagname */
    if (flagname != NULL) {
      *fp_flag = fopen(flagname,"r");
      if (*fp_flag == NULL) {
        printf("Can't open input flag file %s\n",flagname);
        flagname = NULL;
      }
    }
    memcpy(inbuf,"-",1);
    while (*fp_flag == NULL && strncmp(inbuf,"0",1)!=0) {
      printf("Enter name of flag file, :: to quit, 0 for none ");
      fgets(inbuf,100,stdin);
      ptr=strchr(inbuf,'\n');
      if(ptr) *ptr='\0';           /* replace new line by null */

      if (strncmp(inbuf,"::",2) == 0)
        exit(1);
      if (strncmp(inbuf,"0",1) != 0) {
          *fp_flag = fopen(inbuf,"r");
        if (*fp_flag == NULL) printf("Can't open flag file %s\n",inbuf);
      }
    }
    if (strncmp(inbuf,"0",1) == 0)
      *fp_flag = NULL;
  } /* get flagname */

/* 2. Output summary file */

  *fp_out = NULL;
  if (outname != NULL) {
    *fp_out = fopen(outname,"r");
    if (*fp_out != NULL) { /* file already exists */
      if (append == NULL) {
        printf("Output file already exists, (o)verwrite or (a)ppend, :: to quit  ");
        fgets(ans,3,stdin);
        ptr=strchr(ans,'\n');
       if(ptr) *ptr='\0';           /* replace new line by null */

        if (strncmp(ans,"::",2) == 0)
          exit(1);
        while (ans[0] != 'a' && ans[0] != 'o') {
          printf("Enter either o for overwrite or a for append  ");
          fgets(ans,3,stdin);
          ptr=strchr(ans,'\n');
          if(ptr) *ptr='\0';           /* replace new line by null */

        }
      }
      else 
        ans[0] = append[0];
      if (ans[0] == 'a')
        *fp_out = fopen(outname,ans);
      else
        *fp_out = fopen(outname,"w");
    }
    else { 
      *fp_out = fopen(outname,"w");
    }
    if (*fp_out == NULL) {
      printf("Can't open output file %s\n",outname);
      outname = NULL;
    }
  }
  while (*fp_out == NULL) {
    printf("Enter name of output file, :: to quit  ");
    fgets(inbuf,100,stdin);
    ptr=strchr(inbuf,'\n');
    if(ptr) *ptr='\0';           /* replace new line by null */

    if (strncmp(inbuf,"::",2) == 0)
      exit(1);
    *fp_out = fopen(inbuf,"r");
    if (*fp_out != NULL) { /* file already exists */
      printf("Output file already exists, (o)verwrite or (a)ppend, :: to quit  ");
      fgets(ans,3,stdin);
      ptr=strchr(ans,'\n');
      if(ptr) *ptr='\0';           /* replace new line by null */

      if (strncmp(ans,"::",2) == 0)
        exit(1);
      while (ans[0] != 'a' && ans[0] != 'o') {
        printf("Enter either o for overwrite or a for append  ");
        ptr=strchr(ans,'\n');
        if(ptr) *ptr='\0';           /* replace new line by null */
        fgets(ans,3,stdin);
      }
      if (ans[0] == 'a')
        *fp_out = fopen(inbuf,ans);
      else
        *fp_out = fopen(inbuf,"w");
    }
    else {
      *fp_out = fopen(inbuf,"w");
    }
    if (*fp_out == NULL) printf("Can't open output file %s\n",inbuf);
  }
  if (outname == NULL)
    strcpy(sumname,inbuf);
  else
    strcpy(sumname,outname);

  while (strcmp(vexout,"") == 0) {
    printf("Output file (v)ex, (s)tandard, Mk(5), or (d)isc labels :: to quit  ");
    fgets(ans,3,stdin);
    ptr=strchr(ans,'\n');
    if(ptr) *ptr='\0';           /* replace new line by null */
    if (strncmp(ans,"::",2) == 0)
      exit(1);
    while (ans[0] != 'v' && ans[0] != 's' && ans[0] != '5' && ans[0] != 'd') {
      printf("Enter v for VEX, s for standard, 5 for Mk5, d for disc labels  ");
      fgets(ans,3,stdin);
      ptr=strchr(ans,'\n');
      if(ptr) *ptr='\0';           /* replace new line by null */
    }
    if (ans[0] == 's') 
      strcpy(vexout,"s");
    if (ans[0] == 'v') 
      strcpy(vexout,"v");
    if (ans[0] == '5') 
      strcpy(vexout,"5");
    if (ans[0] == 'd') 
      strcpy(vexout,"d");
  }

  return;
}
