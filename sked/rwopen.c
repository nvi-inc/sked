#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

short rwopen(cr)
char *cr;

/*
   Check to see if opening an illegal file. i.e. a directory.
*/

{
     /* extern int errno; */
     int    fd, exist;

     exist = access(cr,F_OK);

     if (exist == 0) {
         fd = open(cr,O_RDWR);
         if (fd >= 0) {
            close(fd);
            return((short) 0);
         } else {
            return((short) fd);
         }
     } else {
         fd = open(cr,O_RDWR | O_CREAT, 0);
         if (fd >= 0) {
            close(fd);
            unlink(cr);
            return((short) 0);
         } else {
            return((short) fd);
         }

     }

}
