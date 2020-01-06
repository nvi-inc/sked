#include <stdio.h>

void cshell()

/*
   Pat Ryan      7.8.88

      This function calls the C-Shell via the 'system()'
   command.  The user returns to SKED by entering an EOF
   (^D).
*/

{
    int system();
    printf("Enter ^D or \'exit\' to return\n");
    system("/bin/csh");
    printf("\nExiting shell\n");
}
