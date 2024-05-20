$JOB/DCL/NOLIMIT/ERROR=FATAL
!+
! DCL.CTL -- Build DCL
!
! INPUTS:
!	CUI:CUIMAC.MLB
!	CUI:CUIOBJ.OLB
!	DCL:*.MAC
!
! OUTPUTS:
!	OUT:DCL.RTS		Vanilla DCL run-time system
!	OUT:DCL.MAP
!	OUT:DCLODT.RTS		DCL with ODT
!	OUT:DCLODT.MAP
!-

$RUN $UTILTY
REMOVE LOGICAL LIB
REMOVE LOGICAL BL
ADD LOGICAL D:          BL
ADD LOGICAL D:$        LIB
ADD LOGICAL D:[170,0]   CMN
ADD LOGICAL BL:[170,53] RSX
ADD LOGICAL BL:[170,20] DCL
ADD LOGICAL BL:[170,19] CUI
LIST LOGICAL
EXIT

$ASSIGN DCL: OUT:

$RUN LIB:PIP
OUT:*.BAK,OUT:*.OBJ,OUT:*.LST/DE:NOWARN/NOLOG
$EOD
