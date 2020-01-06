      SUBROUTINE SET_SIGNAL_CTRLC ( IPAR_PETOOLS )
! ************************************************************************
! *                                                                      *
! *   Routine SET_SIGNAL_CTRLC sets the signal handler on Ctrl/C         *
! *                                                                      *
! *  ### 21-JUN-2007               v1.0 (c)  L. Petrov  21-JUN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IPAR_PETOOLS, IS, LN, SIGTERM, SIGHUP, SIGINT
      INTEGER*4  IPAR
      COMMON   / PETOOLS_SIGNAL_HANDLER / IPAR
      INTEGER*4, EXTERNAL :: SIGNAL, PETOOLS_SIGNAL_HANDLER_ROUTINE 
!
      IPAR = IPAR_PETOOLS
      CALL GET_SYSTEM_CONSTANT ( 'SIGTERM', SIGTERM, LN )
      CALL GET_SYSTEM_CONSTANT ( 'SIGHUP',  SIGHUP,  LN )
      CALL GET_SYSTEM_CONSTANT ( 'SIGINT',  SIGINT,  LN )
      IS = SIGNAL ( %VAL(SIGTERM), PETOOLS_SIGNAL_HANDLER_ROUTINE )
      IS = SIGNAL ( %VAL(SIGHUP),  PETOOLS_SIGNAL_HANDLER_ROUTINE )
      IS = SIGNAL ( %VAL(SIGINT),  PETOOLS_SIGNAL_HANDLER_ROUTINE )
      RETURN
      END  SUBROUTINE  SET_SIGNAL_CTRLC  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PETOOLS_SIGNAL_HANDLER_ROUTINE ( )
! ************************************************************************
! *                                                                      *
! *   Signal hadler routine for child termination
! *                                                                      *
! * # 06-SEP-2006 SIGNAL_HANDLER_ROUTINE v1.0 (c) L. Petrov 06-SEP-2006 #*
! *                                                                      *
! ************************************************************************
      INTEGER*4  PETOOLS_SIGNAL_HANDLER_ROUTINE 
      INTEGER*4  IPAR
      COMMON   / PETOOLS_SIGNAL_HANDLER / IPAR
! 
! 2009Mar03 JMGipson. Chagned "==" to ".eq." in logical statements
!@#ifndef PETOOLS
!@      LOGICAL*4, EXTERNAL :: IS_CURLIB_ON
!@#endif
!
!      CALL FLUSH ( 6 )
      IF ( IPAR .eq. 0 ) THEN
         ELSE IF ( IPAR .eq. 1 ) THEN
           WRITE ( 6, '(A)' ) ' '
         ELSE IF ( IPAR .eq. 2 ) THEN
           WRITE ( 6, '(A)' ) ' '
           WRITE ( 6, '(A)' ) 'The process is stopped by Ctrl/C'
         ELSE IF ( IPAR .eq.  3 ) THEN
!@#ifndef PETOOLS
!@           IF ( IS_CURLIB_ON() ) THEN
!@                CALL SETCR_MN ( 1, 20 )
!@                CALL REFRESH()
!@           END IF
!@#endif
           WRITE ( 6, '(A)' ) ' '
           WRITE ( 6, '(A)' ) 'The process is stopped by Ctrl/C'
      END IF
      CALL EXIT ( 1 ) 
!
      PETOOLS_SIGNAL_HANDLER_ROUTINE = 1
      RETURN
      END  FUNCTION   PETOOLS_SIGNAL_HANDLER_ROUTINE   !#!  
