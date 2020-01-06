#include <string.h>
#include <stdio.h>

void pc8(device,ctlfile)
char *ctlfile; /* name of control file */
int *device;  /* 1=X, 2=HPGLS, 3=HPGLL 4=PS */

/* This function sends commands to the system that
   execute the pc8 program to make plots.
   nrv 930429 commands are hard-coded.
   nrv 930430 form command with input file name
   nrv 930514 More elaborate command string
980113 nrv Add file output to make .ps file
*/

{
  int system();
  char command[80],commstr[300];
  int getpid();
  char cpid[6];

  strcpy(command,"pc8 ");
  strcat(command,ctlfile);
  if (*device == 1) {
    strcat(command," /Xw");
    system(command);
  }
  else if (*device == 2) {
    strcat(command," /HPGLS");
    sprintf(commstr,"echo \"Processing...\";%s 1>$HOME/.holdpgid;tmpgfil=`/usr/bin/awk 'NR==2 {print substr($0,3,length($0))}' $HOME/.holdpgid`;export tmpgfil;echo \"Created plotfile $tmpgfil\";/usr/bin/lp -onb $tmpgfil;/bin/rm $tmpgfil", command);
    system(commstr);
  }
  else if (*device == 3) {
    strcat(command," /HPGLL");
    sprintf(commstr,"echo \"Processing...\";%s 1>$HOME/.holdpgid;tmpgfil=`/usr/bin/awk 'NR==2 {print substr($0,3,length($0))}' $HOME/.holdpgid`;export tmpgfil;echo \"Created plotfile $tmpgfil\";/usr/bin/lp -onb $tmpgfil;/bin/rm $tmpgfil", command);
    system(commstr);
/*  strcpy(command,"lp -onb /tmp/hpgll");
    sprintf(cpid,"%d",getpid());
    strcat(command,cpid);
*/  system(command);
  }
  else if (*device == 4) {
    strcat(command," /PS");
    system(command);
  }
}
