#define HPUX
#ifndef HPUX
#include <windows.h>
#endif

#include <stdio.h>
#include <string.h>

#ifdef HPUX
#include "mysql.h"
#else
#include <mysql.h>
#endif

#ifdef HPUX
#define c2f(name) name
#else
#define c2f(name) name_
#endif

MYSQL_FIELD	* fd ;
MYSQL_ROW	row ;


/* ***************************************************************
/ Lahey Fortran to MYSQL interface.
/    Note: Must add a "_" at the end of the routines.
/ Should work for other fortrans with minor changes, mostly with passing of character arrays.
/
/ Wrapper functions to allow lahey to call MYSQL C API.
/ Given in order of MYSQL manual, which is alphabetical for the most part.
/
/ Before the API we start with some routines that return useful info, mostly
/ Dealing with the size of structures. This makes sure we allocate enough space in fortran.
/
/ Routines written as they were needed, started on Oct 30, 2003.
/ John Gipson   jmg@leo.gsfc.nasa.gov
	Last change:  JG   29 Jan 2004   11:50 am
*/

/* ----Start of auxiliar functions-------------------------
 ***************************************************************
/----2003Oct30    JMGipson------
*/
#ifdef HPUX
int imysql_handle_size
#else
int imysql_handle_size_
#endif
(void) {return sizeof(MYSQL);}

/* ***************************************************************
 ----2003Oct31    JMGipson------ */
#ifdef HPUX
int imysql_res_size
#else
int imysql_res_size_
#endif
(void) {return sizeof(MYSQL_RES);}

/* ***************************************************************
 ----2003Oct31    JMGipson------   */
#ifdef HPUX
int imysql_port
#else
int imysql_port_
#endif
       (void){return MYSQL_PORT;}

/* ----Start of API interface--------------------------------------- */

/* ******************************************************
/ Nov 21 2003 JMGipson */
#ifdef HPUX
int imysql_affected_rows
#else
int imysql_affected_rows_
#endif
       (MYSQL *myhandle){
        return mysql_affected_rows( myhandle);
       }
/* ******************************************************
*/
#ifdef HPUX
int imysql_change_user
#else
int imysql_change_user_
#endif
        (){return 0;}
/* ******************************************************
*/
int imysql_character_set_name_(){return 0;}

/* ******************************************************
/----2003Oct31    JMGipson------
*/

#ifdef HPUX
int imysql_close
#else
int imysql_close_
#endif
    (MYSQL *myhandle){
    mysql_close(myhandle);
    return 0;}

/* ******************************************************
*/
#ifdef HPUX
int imysql_connect
#else
int imysql_connect_
#endif
     (){return 0;}
/* ******************************************************
*/
#ifdef HPUX
int imysql_create_db
#else
int imysql_create_db_
#endif
     (){return 0;}
/* ******************************************************
*/
#ifdef HPUX
int imysql_data_seek
#else
int imysql_data_seek_
#endif
    (){return 0;}
/* ******************************************************
*/
int imysql_debug_(){return 0;}
/* ******************************************************
*/
int imysql_drop_db_(){return 0;}

/* ******************************************************
*/
int imysql_dump_debug_info_(){return 0;}
/* ******************************************************
*/
int imysql_eof_(){return 0;}

/* ******************************************************
/ Nov 21 2003 JMGipson
*/
#ifdef HPUX
int imysql_errno
#else
int imysql_errno_
#endif
    (MYSQL *myhandle){return mysql_errno(myhandle);}

/* ******************************************************
/ Nov 20 2003 JMGipson
*/
#ifdef HPUX
int imysql_error
#else
int imysql_error_
#endif
    (MYSQL *myhandle){
    printf("\n SQL Error:  %s \n", mysql_error(myhandle));
    return 0;}

/* ******************************************************
*/
int imysql_escape_string_(){return 0;}

/* ******************************************************
/----2003Oct31    JMGipson------
*/
#ifdef HPUX
int imysql_fetch_field
#else
int imysql_fetch_field_
#endif
    (MYSQL_RES **myres, char * fdName, char *fdTable, char *fdDefault,
    int *fdType, int *fdLen, int* fdMaxLen,int * fdFlag, int * fdDec,
    int len_fdName,int len_fdTable,int len_fdDefault)
    {
    fd=mysql_fetch_field(*myres);
    if(fd){
        strncpy(fdName, fd->name,len_fdName);
        strncpy(fdTable,fd->table,len_fdTable);
        if(fd->def)
          strcpy(fdDefault,fd->def);
        else
          {*fdDefault='\0';}
        *fdLen=fd->length;
        *fdType=fd->type;
        *fdMaxLen=fd->max_length;
        *fdFlag=fd->flags;
        *fdDec=fd->decimals;
      }

      return !fd;    /* not NULL is success. */
    }
/* ******************************************************
*/
#ifdef HPUX
int imysql_fetch_fields
#else
int imysql_fetch_fields_
#endif
     ()
     {return 0;}
/* ******************************************************
*/
int imysql_fetch_field_direct_(){return 0;}

/* ******************************************************
*/
#ifdef HPUX
int imysql_fetch_lengths
#else
int imysql_fetch_lengths_
#endif
    (MYSQL_RES **myres, unsigned long ilen_vec[]){
    int i,NumField;
    unsigned long *lengths;

    NumField =mysql_num_fields(*myres);
    lengths  =mysql_fetch_lengths(*myres);

    for(i=0;i<NumField;i++){
/*      printf("Length %d %d",i+1, lengths[i]); */
      ilen_vec[i]=lengths[i];
    }
    return NumField;
    }

/* ******************************************************
*/
#ifdef HPUX
int imysql_fetch_row
#else
int imysql_fetch_row_
#endif
    (MYSQL_RES **myres,char ldum[],int ilen){
    int NumField;
    int i,j;
    unsigned long *lengths;
    int imin;

    NumField =mysql_num_fields(*myres);
    row      =mysql_fetch_row(*myres);
    if(!row) return -1;         /* A failure. */

    lengths  =mysql_fetch_lengths(*myres);

    for ( i=0;i<NumField;i++ ){
       strncpy(&ldum[i*ilen],row[i],ilen);
    }
    return 0;}
/* ******************************************************
*/
#ifdef HPUX
int imysql_field_count
#else
int imysql_field_count_
#endif
    (MYSQL *mysql){
    return (int)mysql_field_count(mysql);}

/* ******************************************************
*/
int imysql_field_seek_(){return 0;}
/* ******************************************************
*/
int imysql_field_tell_(){return 0;}
/* ***************************************************************
/----2003Oct31    JMGipson------
*/
#ifdef HPUX
int imysql_free_result
#else
int imysql_free_result_
#endif
    (MYSQL_RES **myres){
    mysql_free_result(*myres);
    return 0;
}

/* ******************************************************
*/
int imysql_get_client_info_(){return 0;}
/* ******************************************************
*/
int imysql_get_server_version_(){return 0;}
/* ******************************************************
*/
int imysql_get_host_info_(){return 0;}
/* ******************************************************
*/
int imysql_get_proto_info_(){return 0;}
/* ******************************************************
*/
int imysql_get_server_info_(){return 0;}
/* ******************************************************
*/
int imysql_info_(){return 0;}
/* ***************************************************************
/----2003Oct30    JMGipson------
*/
#ifdef HPUX
int imysql_init
#else
int imysql_init_
#endif
    (MYSQL *myhandle)   {
    myhandle=mysql_init(myhandle);
    return !myhandle;       /* Success is non-Null */
    }
/* ******************************************************
*/
int imysql_insert_id_(){return 0;}
/* ******************************************************
*/
int imysql_kill_(){return 0;}
/* ******************************************************
/  2003Nov20  JMGipson
*/
#ifdef HPUX
int imysql_list_dbs
#else
int imysql_list_dbs_
#endif
(MYSQL *myhandle, MYSQL_RES **myres,char * wild, int len_wild){
   char * wild_in=NULL;
   if ( len_wild!=0 && wild[0]){wild_in=wild;}
   *myres=mysql_list_dbs(myhandle, wild_in);
   return !myres;}              /* Successful if myres is not NULL. */

/* ******************************************************
/  2003Nov20  JMGipson
/  Note:  If table is not in current database, then get strange results.
/         myres is not defined.
*/
#ifdef HPUX
int imysql_list_fields
#else
int imysql_list_fields_
#endif
   (MYSQL *myhandle, MYSQL_RES **myres,const char * table,
   char * wild, int len_table, int len_wild){

   char * wild_in=NULL;
   printf("\n table       %s",table);
   if ( len_wild!=0 && wild[0]){wild_in=wild;}
   *myres=mysql_list_fields(myhandle, table,wild_in);
   return !myres;}              /* Successful if myres is not NULL. */

/* ******************************************************
/  2003Nov20  JMGipson
*/
#ifdef HPUX
int imysql_list_processes
#else
int imysql_list_processes_
#endif
   (MYSQL *myhandle, MYSQL_RES **myres){
   *myres=mysql_list_processes(myhandle);
   return !myres;}              /* Successful if myres is not NULL. */

/* ******************************************************
*/
#ifdef HPUX
int imysql_list_tables
#else
int imysql_list_tables_
#endif
(MYSQL *myhandle, MYSQL_RES **myres,char * wild, int len_wild){
   char * wild_in=NULL;
   if ( len_wild!=0 && wild[0]){wild_in=wild;}
   *myres=mysql_list_tables(myhandle, wild_in);
   return !myres;}              /* Successful if myres is not NULL. */

/* ******************************************************
/----2003Oct31    JMGipson------
*/
#ifdef HPUX
int imysql_num_fields
#else
int imysql_num_fields_
#endif
    (MYSQL_RES **myres){
    return (int)mysql_num_fields(*myres);}
/* ****************************************************************
/----2003Oct31    JMGipson------
*/
#ifdef HPUX
int imysql_num_rows
#else
int imysql_num_rows_
#endif
   (MYSQL_RES **myres){
    return (int)mysql_num_rows(*myres);}

/* ******************************************************
*/
int imysql_options_(){return 0;}
/* ******************************************************
*/
int imysql_ping_(){return 0;}
/* ***************************************************************
/----2003Oct31    JMGipson------
*/
#ifdef HPUX
int imysql_query
#else
int imysql_query_
#endif
     (MYSQL *myhandle, char * szSQL, int ilenSQL){
     return (int)mysql_query(myhandle,szSQL);
    }
/* ***************************************************************
/----2003 Nov 20 all args. JMGipson------
*/
#ifdef HPUX
int imysql_real_connect
#else
int imysql_real_connect_
#endif
       (MYSQL *myhandle,
       const char *host, const char *user, const char * password, const char *db,
       unsigned int * port, char * unix_socket, unsigned int * client_flag,
       int len_host,  int len_user, int len_password,int len_db,int len_unix_socket){

const char * host_in=NULL;
const char * user_in=NULL;
const char * password_in=NULL;
const char * db_in=NULL;
const char * unix_socket_in=NULL;

/*
printf("\n host        %s ", host);
printf("\n user        %s ", user);
printf("\n password    %s ", password);
printf("\n db          %s ", db);
printf("\n unix_socket %s ", unix_socket);
printf("\n client flag %d ", *client_flag);
printf("\n port        %d ", *port);
printf("\n len_host    %d ", len_host);
printf("\n len_user    %d ", len_user);
printf("\n len_db      %d ", len_db);
	Last change:  JG    5 Dec 2003    3:00 pm
 */

if ( len_host!=0        && host[0]){host_in=host;}
if ( len_user!=0        && user[0]){user_in=user;}
if ( len_password!=0    && password[0]){password_in=password;}
if ( len_db!=0          && db[0]){db_in=db;}
if ( len_unix_socket!=0 && unix_socket[0]){unix_socket_in=unix_socket;}

/* printf("\n host, host_in %d %d", host,host_in); */

       myhandle=mysql_real_connect( myhandle,host_in,user_in,password_in,db_in,
       *port,unix_socket_in,*client_flag);
       return !myhandle;      /* Succesful if we don't get a NULL. */
    }

/* ******************************************************
/----2003Oct30    JMGipson------
*/
#ifdef HPUX
int imysql_real_connect0
#else
int imysql_real_connect0_
#endif
       (MYSQL *myhandle){
       myhandle=mysql_real_connect( myhandle, NULL, NULL, NULL, NULL, MYSQL_PORT,
			   NULL, 0 );
       return !myhandle;      /* Succesful if we don't get a NULL.*/
    }

/* ******************************************************
*/
int imysql_real_escape_string_(){return 0;}
/* ******************************************************
*/
int imysql_real_query_(){return 0;}
/* ******************************************************
*/
int imysql_reload_(){return 0;}
/* ******************************************************
*/
int imysql_row_seek_(){return 0;}
/* ******************************************************
*/
int imysql_row_tell_(){return 0;}

/* ***************************************************************
/----2003Oct30    JMGipson------
*/
#ifdef HPUX
int imysql_select_db
#else
int imysql_select_db_
#endif
     (MYSQL *myhandle, char * szDB, int ilenDB){
     return mysql_select_db( myhandle, szDB );
    }

/* ******************************************************
*/
int imysql_sqlstate_(){return 0;}
/* ******************************************************
*/
int imysql_shutdown_(){return 0;}
/* ******************************************************
*/
int imysql_stat_(){return 0;}

/* ***************************************************************
/----2003Oct31    JMGipson------
*/
#ifdef HPUX
int imysql_store_result
#else
int imysql_store_result_
#endif
(MYSQL *myhandle, MYSQL_RES **myres){
    *myres=mysql_store_result(myhandle);
    return !myres;           /* NULL is failure; */
}
/* ******************************************************
*/
int imysql_thread_id_(){return 0;}
/* ******************************************************
*/
int imysql_use_result_(){return 0;}
/* ******************************************************
*/
int imysql_commit_(){return 0;}
/* ******************************************************
*/
int imysql_rollback_(){return 0;}
/* ******************************************************
*/
int imysql_autocommit_(){return 0;}
/* ******************************************************
*/
int imysql_more_results_(){return 0;}
/* ******************************************************
*/
int imysql_next_result_(){return 0;}
/*
/    C API Prepared Statements
/    C API Prepared Statement Datatypes
/    C API Prepared Statement Function Overview
/    C API Prepared Statement Function Descriptions
 ******************************************************
*/
int imysql_prepare_(){return 0;}
/* ******************************************************
*/
int imysql_param_count_(){return 0;}
/* ******************************************************
*/
int imysql_get_metadata_(){return 0;}
/* ******************************************************
*/
int imysql_bind_param_(){return 0;}
/* ******************************************************
*/
int imysql_execute_(){return 0;}
/* ******************************************************
*/
int imysql_stmt_affected_rows_(){return 0;}
/* ******************************************************
*/
int imysql_bind_result_(){return 0;}
/* ******************************************************
*/
int imysql_stmt_store_result_(){return 0;}
/* ******************************************************
*/
int imysql_stmt_data_seek_(){return 0;}
/* ******************************************************
*/
int imysql_stmt_row_seek_(){return 0;}
/* ******************************************************
*/
int imysql_stmt_row_tell_(){return 0;}
/* ******************************************************
*/
int imysql_stmt_num_rows_(){return 0;}
/* ******************************************************
*/
int imysql_fetch_(){return 0;}
/* ******************************************************
*/
int imysql_send_long_data_(){return 0;}
/* ******************************************************
*/
int imysql_stmt_close_(){return 0;}
/* ******************************************************
*/
int imysql_stmt_errno_(){return 0;}
/* ******************************************************
*/
int imysql_stmt_error_(){return 0;}
/* ******************************************************
*/
int imysql_stmt_sqlstate_(){return 0;}
/* ******************************************************
*/
int imy_init_(){return 0;}
/* ******************************************************
*/
int imysql_thread_init_(){return 0;}
/* ******************************************************
*/
int imysql_thread_end_(){return 0;}
/* ******************************************************
*/
int imysql_thread_safe_(){return 0;}
/*    C API Embedded Server Function Descriptions
/ ******************************************************
*/
int imysql_server_init_(){return 0;}
/* ******************************************************
*/
int imysql_server_end_(){return 0;}
