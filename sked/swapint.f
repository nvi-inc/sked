      subroutine swapint(int1,int2)
      
! AEM 20050120 add implicit none
      implicit none
! Swap two numbers
      integer int1,int2
      integer itemp

      itemp=int1
      int1=int2
      int2=itemp
      return
      end
