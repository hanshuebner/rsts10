$JOB/DCL/NOLIMIT/ERROR:FATAL/PRIO:-16 [170,20]
!+
! DCLAUX.CTL -- Control file to assemble DCLAUX
!
! INPUTS:
!	CUI:CUIMAC.MLB		CUI macro library
!	DCL:DCLMAC.MLB		DCL macro library
!	DCL:DCLAUX.MAC
!	DCL:AUX???.MAC
!	COMMON.MAC,KERNEL.MAC
!
! OUTPUTS:
!	DCL:DCLAUX.OBJ
!	DCL:AUX???.OBJ
!-
! EDIT HISTORY:
!
! 03-Jun-85	GMB	Created this file.
! 02-Sep-86	GMB	Added AUXPOL
! 13-Nov-89	BTB	Added AUXDVS
!

$ASSIGN D:[170,20] DCL:
$ASSIGN D:[170,19] CUI:

$RUN $MACRO
DCL:DCLAUX,DCL:DCLAUX/C=DCL:DCLAUX
DCL:AUXSRT,DCL:AUXSRT/C=DCL:AUXSRT
DCL:AUXTRN,DCL:AUXTRN/C=DCL:AUXTRN
DCL:AUXKEY,DCL:AUXKEY/C=DCL:AUXKEY
DCL:AUXFIL,DCL:AUXFIL/C=DCL:AUXFIL
DCL:AUXPRG,DCL:AUXPRG/C=DCL:AUXPRG
DCL:AUXMSG,DCL:AUXMSG/C=DCL:AUXMSG
DCL:AUXDVS,DCL:AUXDVS/C=DCL:AUXDVS
DCL:AUXPOL,DCL:AUXPOL/C=DCL:AUXPOL
$EOD

$RUN $TKB
DCL:DCLAUX.TSK,DCL:DCLAUX.MAP/-SH,DCL:DCLAUX.STB = DCL:DCLAUX/MP
//
$EOD

$EOJ
