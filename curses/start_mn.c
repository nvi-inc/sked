#include <curses.h>
#include <ctype.h>
#include <stdio.h>
/* AEM 20041125 add include string.h, term.h */
#include <string.h>
#include <term.h>


void start_mn(errret)
int *errret;
{
    static int first = TRUE;
/* AEM 20041215 initialize string with \0 or null */
    char *value = "\0";
    char *getenv();

    *errret=1;
    if (first) {
      value = getenv("TERM");
/* AEM 20041215 compare string not with NULL but with "\0"
 status: critical, cause segmentation fault on Intel C compiler */

      if ((strcmp(value,"unknown")==0) || (strcmp(value,"\0")==0)) { 
        *errret=-1;
        return;
      }
      else { 
        setupterm(value,1,errret);
        if (*errret != 1) /* invalid */
          return;
      }
      if (initscr() == (WINDOW *)NULL) {
        *errret=-1;
        return;
      }
      nonl();
      cbreak();
      noecho();
      intrflush(stdscr,FALSE);
      keypad(stdscr,TRUE);
      scrollok(stdscr,TRUE);
      idlok(stdscr,TRUE);
      first = FALSE;
    }
    else {
      clear();
      refresh();
    }

      return;
}

/* AEM 20041125 add void */
void end_mn()
{
      endwin();
}

/* AEM 20041125 add void */
void nl_mn()
{
      addch('\n');
}

/* AEM 20041125 add void */
void addstr_mn(str)
char *str;
{
      addstr(str);
}

/* AEM 20041125 add void */
void getxy_mn(x,y)
int *y,*x;
{
      getyx(stdscr, *y, *x);
}

int getstr_mn(str)
char *str;
{

      nocbreak();
      echo();
      nl();
      clrtoeol();
      refresh();
      getstr(str);
      cbreak();
      noecho();
      nonl();
      return(strlen(str));
}

/* AEM 20041125 add void */
void setcr_mn(x,y)
int *x, *y;
{
      move(*y,*x);
}

/* AEM 20041125 add void */
void senxy_mn(x,y)
int *x, *y;
{
      int c;

      keypad(stdscr,TRUE);
      getyx(stdscr, *y, *x);
      refresh();
      while (TRUE)
      {
         c=getch();
         switch (c)
         {
            case KEY_LEFT:
            case 'h':
                *x=*x-1;
                if (*x<0) *x=0;
                break;
            case KEY_RIGHT:
            case 'l':
                *x=*x+1;
                break;
            case KEY_UP:
            case 'k':
                *y=*y-1;
                if (*y<0) *y=0;
                break;
            case KEY_DOWN:
            case 'j':
                *y=*y+1;
                break;
            case ' ':
                return;
         }
         move(*y,*x);
         refresh();
      }
}

/* AEM 20041125 add void */
/* AEM 20041217 int(4b)->short int(2b) */
void senkr_mn(x,y,ikey)
int *x, *y;
short int *ikey;
{
      getyx(stdscr, *y, *x);
      refresh();
      while (TRUE)
      {
/*         *ikey=toupper(getch());
*/
         *ikey=getch();
         switch (*ikey)
         {
            case KEY_LEFT:
            case 'h':
                *x=*x-1;
                if (*x<0) *x=0;
                break;
            case KEY_RIGHT:
            case 'l':
                *x=*x+1;
/* AEM 20041221 add upper-limit control such as in seop.f:107
 now you can't lose your cursor within right border */
/* JMG removed            if (*x>80) *x=80; break;  */
               break;
            case KEY_UP:
            case 'k':
                *y=*y-1;
                if (*y<0) *y=0;
                break;
            case KEY_DOWN:
            case 'j':
                *y=*y+1;
                break;
            default:
      		beep();
                return;
         }
         move(*y,*x);
	 refresh();
      }
}

/* AEM 20041125 add void */
void reverse_on_mn()
{
    attron(A_REVERSE);
}

void reverse_off_mn()
{
    attroff(A_REVERSE);
}

void blink_on_mn()
{
    attron(A_BLINK);
}

/* AEM 20041125 add void */
void blink_off_mn()
{
    attroff(A_BLINK);
}

/* AEM 20041125 add void */
void beep_mn()
{
    beep();
}

void clear_mn()
{
    clear();
}

void deleteln_mn()
{
    deleteln();
}

void clrtobot_mn()
{
    clrtobot();
}

/* AEM 20041125 add void */
void clrtoeol_mn()
{
    clrtoeol();
}

/*  AEM 20041125 add void */
void refresh_mn()
{
    refresh();
}

/* AEM 20041125 add void */
void setscrreg_mn(top,bot)
int *top,*bot;
{
    setscrreg(*top,*bot);
}
