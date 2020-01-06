#include <string.h>

void skpar(param_file,program_path)
char *param_file; /* name of parameter file */
char *program_path; /* path to Parameters program */

/* This function sends commands to the system that start and end the 
   parameters program.
000326 nrv New. Copied from skcat.
*/

{
  int system();
  char command[300];

/* if parameter file does not contain "SK" then it is a pid */

  if (strstr(param_file,"SK") == 0) { /* not a parameter file */
    strcpy(command,"kill -9 ");
    strcat(command,param_file);
    system(command);
  }
  else {
    strcpy(command,program_path);
    strcat(command,"/Parameters ");   /* this is the script that runs java */
    strcat(command,param_file);     /* first argument is parameter file */
    strcat(command," ");
    strcat(command,program_path);  /* second argument is path for cd  command,
                                      used by the script */

/* strcpy(command,"set -x;name=`pwd`;cd ");  Use set -x to view the commands */
/*    strcpy(command,"name=`pwd`;cd ");  
    strcat(command,program_path);
    strcat(command,";java Catalogues ");
    strcat(command,control_file);         
    strcat(command," \\&;cd $name");    */
  
    system(command);
  }

}
