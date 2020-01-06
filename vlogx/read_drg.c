/* read_drg - Read the drudg summary file for VLOGX

001116 nrv New. Removed from main. Parse the lines instead of
           assuming they are in order.

*/

#include <stdio.h>
#include <string.h>

int read_drg(fp_in,station_name,station_id)
FILE *fp_in;         /* file pointers */
char *station_id;      /* station ID, 1 characters upper case */
char *station_name;   /* station name */

{
  char inbuf[100];
  int inbuf_len = 100;
  int got_name;
  char *ptr,*ptr2;
  char ssearch[30];
  char scanid[30]; /* scan ID */

/* Search for the string that says we've got the right line. 
   First lines look like:

 Schedule file: rdv22sc.snp                                   Page   1
 Station: SC-VLBA  (Sc)(E)
 Experiment: RDV22   
*/
    got_name = 0;
    strcpy(ssearch,"Station:");
    while ((ptr!=NULL) && !got_name) {
      ptr = fgets(inbuf,inbuf_len,fp_in);
      ptr2 = strstr(inbuf,ssearch); /* look for station ID line */
      got_name = (ptr2 != NULL);
    }

    ptr = strtok(inbuf," "); /*  Station:  */
    ptr2 = strtok(NULL," ");     /* name */
    strcpy(station_name,ptr2);
    ptr2 = strtok(NULL," ");     /* Id fields */
    memcpy(station_id,ptr2+5,1);

    return;

}
