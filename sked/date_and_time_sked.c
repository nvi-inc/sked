# include <time.h>

void date_and_time_sked ( time_arr )
long *time_arr;

/***********************************************************/
/*                                                         */
/*  Routine date_and_time_sked mimics Fortran90 built-in   */
/*  function date_and_time                                 */
/*                                                         */
/***********************************************************/

{
  struct tm *tm_arr;
  time_t now;

  now = time((time_t *)NULL);
  tm_arr = localtime ( &now );
    time_arr[0] = 1900 + tm_arr->tm_year ;
    time_arr[1] = 1 + tm_arr->tm_mon ;
    time_arr[2] = tm_arr->tm_mday ;
    time_arr[3] = tm_arr->tm_hour ;
    time_arr[4] = tm_arr->tm_min ;
    time_arr[5] = tm_arr->tm_sec ;
  return;
}
