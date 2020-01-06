#include <string.h>

void skcat(control_file,program_path)
char *control_file; /* name of control file */
char *program_path; /* path to Catalogues program */

/* This function sends commands to the system that start the catalog program.
991108 nrv New. Copied from pc8.
991109 nrv Add 'stop' option. Need pid as input instead of file.
991117 nrv Put run script commands into a single system command.
000107 nrv Change script name to "Catalogues".
000317 nrv Fix up the command, it had its parts mixed up.
*/

{
  int system();
  char command[300];

/* if control file does not contain "SK" then it is a pid */

  if (strstr(control_file,"SK") == 0) { /* not a control file */
    strcpy(command,"kill -9 ");
    strcat(command,control_file);
    system(command);
  }
  else {
    strcpy(command,program_path);
    strcat(command,"/Catalogues ");   /* this is the script that runs java */
    strcat(command,control_file);     /* first argument is control file */
    strcat(command," ");
    strcat(command,program_path);  /* second argument is path for cd */

/* strcpy(command,"set -x;name=`pwd`;cd ");  Use set -x to view the commands */
/*    strcpy(command,"name=`pwd`;cd ");  
    strcat(command,program_path);
    strcat(command,";java Catalogues ");
    strcat(command,control_file);         
    strcat(command," \\&;cd $name");    */
  
    system(command);
  }

}
