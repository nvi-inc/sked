      character*1 function sochr(isrc)

      implicit none
C
C     SOCHR returns a character given the source number
C  INPUT
      integer isrc
! 2005Nov29 JMGipson. Modified to make it clearer.
! 2009Sep29 JMGipson.  Modified to wrap around after 90 characters.


! First 90 characters are defined.
!
      integer itmp
      character*90 charvec
!      data charvec/
!     > "1234567890abcdefghijklmnopqrstuvwxyz"//  ! 1-36
!     > "ABCDEFGHIJKLMNOPQRSTUVWXYZ()<>{}[]" /    !37-70
!     > "+-*/|\,.:;!?#$%^&_"/                    !71-90

      charvec=
     > "1234567890abcdefghijklmnopqrstuvwxyz"//  ! 1-36
     > "ABCDEFGHIJKLMNOPQRSTUVWXYZ()<>{}[]"//    !37-70
     > "+-*/|\,.:;!?#$%^&_~'"                    !71-90

! The character "@"=ASCII 64 is reserved for the SUN
! The character "*" is reserved for plotting the ecliptic.
! Wrap around after 90

      itmp=mod(isrc-1,90)+1
      sochr=charvec(itmp:itmp)

      end
