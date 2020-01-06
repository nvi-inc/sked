#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <dirent.h>
#include <pwd.h>
#include <sys/utsname.h>
#include <sys/types.h>
#include <netdb.h>
#include <unistd.h>
#ifdef SUN
  #define _UTSNAME_DOMAIN_LENGTH _SYS_NMLN
#endif

/* # ************************************************************************/ 
/* # *                                                                      */ 
/* # *    routine get_system_constant returns values of some UNIX system    */
/* # *    constants defined in system headers.                              */
/* # *                                                                      */ 
/* # * ------------------- Input parameter: ------------------------------- */
/* # *                                                                      */ 
/* # *  name ( CHARACTER ) -- constant name as it used in Solve.            */ 
/* # *                                                                      */ 
/* # * ------------------- Output parameter: ------------------------------ */
/* # *                                                                      */ 
/* # *  arg  ( INTEGER*? ) -- argument. Meaning depends on contenxt.        */ 
/* # *  len  ( INTEGER*4 ) -- Lenght of the argument in bytes.              */ 
/* # *                                                                      */ 
/* # * ## 03-DEC-2003 get_system_constant v1.2 (c) L. Petrov 04-NOV-2005 ## */ 
/* # *                                                                      */ 
/* # ************************************************************************/
void get_system_constant ( char *name, long *arg, int *len, int name_len )
{
struct dirent  dir_struct;
struct passwd  passwd_struct;
struct utsname utsname_struct;
long a1, a2;
  if ( strncmp ( name, "O_WRONLY", name_len ) == 0 ) 
     {
          *arg = O_WRONLY;
          *len = 1;
     }
    else if ( strncmp ( name, "O_CREAT", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = O_CREAT;
          *len = 1;
     }
    else if ( strncmp ( name, "O_RDONLY", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = O_RDONLY;
          *len = 1;
     }
    else if ( strncmp ( name, "O_RDWR", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = O_RDWR;
          *len = 1;
     }
    else if ( strncmp ( name, "S_IFDIR", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_IFDIR;
          *len = 4;
     }
    else if ( strncmp ( name, "S_IRUSR", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_IRUSR;
          *len = 4;
     }
    else if ( strncmp ( name, "S_IWUSR", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_IWUSR;
          *len = 4;
     }
    else if ( strncmp ( name, "S_IRGRP", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_IRGRP;
          *len = 4;
     }
    else if ( strncmp ( name, "S_IWGRP", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_IWGRP;
          *len = 4;
     }
    else if ( strncmp ( name, "S_IROTH", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_IROTH;
          *len = 4;
     }
    else if ( strncmp ( name, "S_IWOTH", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_IWOTH;
          *len = 4;
     }
    else if ( strncmp ( name, "d_name", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          a1 = (long) &dir_struct.d_name;
          a2 = (long) &dir_struct;
          *arg = a1 - a2;
          *len = 1;
     }
    else if ( strncmp ( name, "MAXNAMLEN", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = MAXNAMLEN;
          *len = 1;
     }
    else if ( strncmp ( name, "pw_name", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          a1 = (long) &passwd_struct.pw_name;
          a2 = (long) &passwd_struct;
          *arg = a1 - a2;
          *len = 4;
     }
    else if ( strncmp ( name, "pw_gecos", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          a1 = (long) &passwd_struct.pw_gecos;
          a2 = (long) &passwd_struct;
          *arg = a1 - a2;
          *len = 4;
     }
    else if ( strncmp ( name, "sysname", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          a1 = (long) &utsname_struct.sysname;
          a2 = (long) &utsname_struct;
          *arg = a1 - a2;
          *len = 4;
     }
    else if ( strncmp ( name, "sysname_len", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
#       ifdef HPUX
          *arg = _SYS_NMLN;
#       endif
#       ifdef LINUX
          *arg = _UTSNAME_LENGTH; 
#       endif
          *len = 1;
     }
    else if ( strncmp ( name, "release", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          a1 = (long) &utsname_struct.release;
          a2 = (long) &utsname_struct;
          *arg = a1 - a2;
          *len = 4;
     }
    else if ( strncmp ( name, "release_len", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
#       ifdef HPUX
          *arg = _SYS_NMLN;
#       endif
#       ifdef LINUX
          *arg = _UTSNAME_LENGTH; 
#       endif
          *len = 1;
     }
    else if ( strncmp ( name, "nodename", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          a1 = (long) &utsname_struct.nodename;
          a2 = (long) &utsname_struct;
          *arg = a1 - a2;
          *len = 4;
     }
    else if ( strncmp ( name, "nodename_len", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
#       ifdef HPUX
          *arg = _SYS_NMLN;
#       endif
#       ifdef LINUX
          *arg = _UTSNAME_NODENAME_LENGTH; 
#       endif
          *len = 1;
     }
    else if ( strncmp ( name, "machine", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          a1 = (long) &utsname_struct.machine;
          a2 = (long) &utsname_struct;
          *arg = a1 - a2;
          *len = 4;
     }
    else if ( strncmp ( name, "machine_len", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
#       ifdef HPUX
          *arg = _SYS_NMLN;
#       endif
#       ifdef LINUX
          *arg = _UTSNAME_LENGTH; 
#       endif
          *len = 1;
     }
    else if ( strncmp ( name, "domainname", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
#       ifdef HPUX
	  printf ( "get_system_constant: argument %s is not defined \n", name );
          *len = 0;
	  exit ( 1 );
#endif
#       ifdef LINUX
#          ifdef __USE_GNU
              a1 = (long) &utsname_struct.domainname;
#            else
              a1 = (long) &utsname_struct.__domainname;
#          endif
           a2 = (long) &utsname_struct;
           *arg = a1 - a2;
           *len = 4;
#endif
     }
    else if ( strncmp ( name, "domainname_len", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
#       ifdef HPUX
	  printf ( "get_system_constant: argument %s is not defined \n", name );
          *len = 0;
	  exit ( 1 );
#endif
#       ifdef LINUX
          *arg = _UTSNAME_DOMAIN_LENGTH;
          *len = 4;
#endif
     }
    else if ( strncmp ( name, "idnumber", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
#       ifdef HPUX
           a1 = (long) &utsname_struct.__idnumber;
           a2 = (long) &utsname_struct;
           *arg = a1 - a2;
           *len = 4;
#endif
#       ifdef LINUX
	  printf ( "get_system_constant: argument %s is not defined \n", name );
          *len = 0;
	  exit ( 1 );
#endif
     }
    else if ( strncmp ( name, "idnumber_len", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
#       ifdef HPUX
          *arg = _SNLEN;
          *len = 1;
#endif
#       ifdef LINUX
	  printf ( "get_system_constant: argument %s is not defined \n", name );
          *len = 0;
	  exit ( 1 );
#endif
     }
    else if ( strncmp ( name, "SEEK_CUR", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SEEK_CUR;
          *len = 1;
      }
    else if ( strncmp ( name, "SEEK_END", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SEEK_END;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGCHLD", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGCHLD;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGHUP", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGHUP;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGINT", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGINT;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGQUIT", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGQUIT;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGTERM", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGTERM;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGUSR1", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGUSR1;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGUSR2", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGUSR2;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGABRT", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGABRT;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGTSTP", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGTSTP;
          *len = 1;
      }
    else if ( strncmp ( name, "SIG_IGN", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) SIG_IGN;
          *len = 1;
      }
    else if ( strncmp ( name, "SIG_DFL", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) SIG_DFL;
          *len = 1;
      }
    else if ( strncmp ( name, "WNOHANG", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) WNOHANG;
          *len = 1;
      }
    else if ( strncmp ( name, "WUNTRACED", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) WUNTRACED;
          *len = 1;
      }
    else
    /* --*/
      {
	  printf ( "get_system_constant: wrong argument: %s \n", name );
          *len = 0;
	  exit ( 1 );
      }  
   return ;
}
