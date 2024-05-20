$!******************************************************************************
$!
$! RSXBLD.COM - command file to build the RSX RTS and RSX libraries.
$!
$!******************************************************************************
$!
$!
$ _ON ERROR THEN GOTO EXIT
$ _SET ECHO
$ _SET JOB/PRIORITY=-16
$!
$ _ASSIGN/USER/REPLACE D:         BL
$ _ASSIGN/USER/REPLACE D:$        LIB
$ _ASSIGN/USER/REPLACE D:[170,0]  CMN
$ _ASSIGN/USER/REPLACE D:[170,90] RMS
$!
$! Set up logicals to point to local (test) account
$!
$ _ASSIGN/USER/REPLACE D:[170,53] RSX
$ _ASSIGN/USER/REPLACE D:[170,60] FCS
$ _ASSIGN/USER/REPLACE D:[170,61] UTL
$ _ASSIGN/USER/REPLACE D:[170,62] MAC
$ _ASSIGN/USER/REPLACE D:[170,63] LBR
$ _ASSIGN/USER/REPLACE D:[170,64] TKB
$ _ASSIGN/USER/REPLACE D:[170,65] RNO
$ _ASSIGN/USER/REPLACE D:[170,66] PAT
$ _ASSIGN/USER/REPLACE D:[170,67] CRF
$!
$ _ASSIGN/USER/REPLACE D:[170,53] SYSTEM
$ _ASSIGN/USER/REPLACE D:[170,53] LST
$ _ASSIGN/USER/REPLACE D:[170,53] LB
$!
$ _SET PROT/DEFAULT 60
$!
$! Delete old versions
$!
$ _DELETE/NOLOG/NOWARN  SYSTEM:*.BAK
$ _DELETE/NOLOG/NOWARN  SYSTEM:*.OBJ
$ _DELETE/NOLOG/NOWARN  SYSTEM:*.LST
$ _DELETE/NOLOG/NOWARN  SYSTEM:*.MAP
$!
$! Set protections
$!
$ _SET FILE/PROT=40 SYSTEM:COMMON.MAC
$ _SET FILE/PROT=40 SYSTEM:RXMDIR.MAC
$ _SET FILE/PROT=40 SYSTEM:RSTSMC.MAC
$ _SET FILE/PROT=40 SYSTEM:PLSMAC.MAC
$!
$RUN LIB:PIP
SYSTEM:*.*/RMS:FB=CMN:ERR.STB
SYSTEM:*.*/RMS:FB=CMN:RTSODT.OBJ
$EOD
$!
$RUN LIB:LBR
@RSX:RSXMAC
$EOD
$!
$RUN LIB:MAC
@RSX:RSXASM
SYSTEM:RSXPRE,LST:RSXPRE/CR=CMN:COMMON,RSXCOM,RSX:RSXPRE,TT:
L$$IST=1
.END
L$$IST=1
.END
$EOD
$!
$RUN LIB:CRF
@RSX:RSXCRF
$EOD
$!
$RUN LIB:MAC
SYSTEM:ATRG  ,SYSTEM:ATRG  =SYSTEM:SYLDEF,SYSTEM:ATRG  
SYSTEM:RLSCT ,SYSTEM:RLSCT =SYSTEM:SYLDEF,SYSTEM:RLSCT 
SYSTEM:XERR  ,SYSTEM:XERR  =SYSTEM:SYLDEF,SYSTEM:F4,SYSTEM:XERR
SYSTEM:XPAA  ,SYSTEM:XPAA  =SYSTEM:SYLDEF,SYSTEM:XPAA  
SYSTEM:XEXT  ,SYSTEM:XEXT  =SYSTEM:SYLDEF,SYSTEM:XEXT  
SYSTEM:GTTSK ,SYSTEM:GTTSK =SYSTEM:SYLDEF,SYSTEM:GTTSK 
SYSTEM:GTMCL ,SYSTEM:GTMCL =SYSTEM:SYLDEF,SYSTEM:GTMCL 
SYSTEM:WFSNE ,SYSTEM:WFSNE =SYSTEM:SYLDEF,SYSTEM:WFSNE 
SYSTEM:SSPND ,SYSTEM:SSPND =SYSTEM:SYLDEF,SYSTEM:SSPND 
SYSTEM:SETTIM,SYSTEM:SETTIM=SYSTEM:SYLDEF,SYSTEM:SETTIM
SYSTEM:GTPPC ,SYSTEM:GTPPC =SYSTEM:SYLDEF,SYSTEM:GTPPC 
SYSTEM:GTLUN ,SYSTEM:GTLUN =SYSTEM:SYLDEF,SYSTEM:GTLUN 
SYSTEM:ASLUN ,SYSTEM:ASLUN =SYSTEM:SYLDEF,SYSTEM:ASLUN 
SYSTEM:EXTTSK,SYSTEM:EXTTSK=SYSTEM:SYLDEF,SYSTEM:EXTTSK
SYSTEM:ALSCT ,SYSTEM:ALSCT =SYSTEM:SYLDEF,SYSTEM:ALSCT 
$EOD
$!
$RUN LIB:LBR
SYSTEM:SYSLIB.OLB/CR:250.:2048.:512.:OBJ
SYSTEM:SYSLIB/RP=FCS:BOTLIB
SYSTEM:SYSLIB/RP=FCS:NOALIB
SYSTEM:SYSLIB/RP/-EP=FCS:FCSNEP
@UTL:TKBRTO
SYSTEM:SYSLIB/RP=UTL:SYSLIB
SYSTEM:SYSLIB/RP/-EP=UTL:UTLNEP
SYSTEM:SYSLIB/RP/-EP=TKB:VMLIB
@RSX:SYSLIB
;
;SYSTEM:ANSLIB.OLB/CO=SYSTEM:SYSLIB.OLB
;SYSTEM:ANSLIB.OLB/RP=FCS:ANSLIB.OBJ
;SYSTEM:ANSLIB.OLB/CO=SYSTEM:ANSLIB.OLB
;
;SYSTEM:MULLIB.OLB/CO=SYSTEM:SYSLIB.OLB
;SYSTEM:MULLIB.OLB/RP=FCS:MULLIB.OBJ
;SYSTEM:MULLIB.OLB/CO=SYSTEM:MULLIB.OLB
SYSTEM:SYSLIB/RP=RMS:RMSFUN
SYSTEM:SYSLIB.OLB/RP=SYSTEM:ATRG
SYSTEM:SYSLIB.OLB/RP=SYSTEM:RLSCT 
SYSTEM:SYSLIB.OLB/RP=SYSTEM:XERR  
SYSTEM:SYSLIB.OLB/RP=SYSTEM:XPAA  
SYSTEM:SYSLIB.OLB/RP=SYSTEM:XEXT  
SYSTEM:SYSLIB.OLB/RP=SYSTEM:GTTSK 
SYSTEM:SYSLIB.OLB/RP=SYSTEM:GTMCL 
SYSTEM:SYSLIB.OLB/RP=SYSTEM:WFSNE 
SYSTEM:SYSLIB.OLB/RP=SYSTEM:SSPND 
SYSTEM:SYSLIB.OLB/RP=SYSTEM:SETTIM
SYSTEM:SYSLIB.OLB/RP=SYSTEM:GTPPC 
SYSTEM:SYSLIB.OLB/RP=SYSTEM:GTLUN 
SYSTEM:SYSLIB.OLB/RP=SYSTEM:ASLUN 
SYSTEM:SYSLIB.OLB/RP=SYSTEM:EXTTSK
SYSTEM:SYSLIB.OLB/RP=SYSTEM:ALSCT 
SYSTEM:SYSLIB.OLB/CO=SYSTEM:SYSLIB.OLB
$EOD
$!
$ _COPY/NOLOG/REPL SYSTEM:RTSODT.OBJ SYSTEM:RTSOBJ.ODT
$ _DELETE/NOWARN/NOLOG SYSTEM:*.OBJ
$ _COPY/NOLOG/REPL SYSTEM:RTSOBJ.ODT SYSTEM:RTSODT.OBJ 
$ _SET FILE/PROT=40 SYSTEM:SYSLIB.OLB
$ _SET FILE/PROT=40 SYSTEM:RSXMAC.SML
$ _COPY/PROT=40/NOLOG/REPL SYSTEM:SYSLIB.OLB SYSTEM:VMLIB.OLB
$!
$RUN LIB:TKB
@RSX:RSXSKL
$!
$RUN LIB:MAKSIL
RSX/RTS/DEBUG
SYSTEM:RSX.TSK
YES
RSX:RSXSKL.CMD
SYSTEM:RSXBLD.CMD
$!
$RUN LIB:TKB
@SYSTEM:RSXBLD
$EOD
$!
$RUN LIB:CRF
SYSTEM:RSX.MAP
$EOD
$!
$RUN LIB:MAKSIL
RSX/RTS/DEBUG
SYSTEM:RSX.TSK
NO
YES
SYSTEM:RSX.STB
SYSTEM:RSX.RTS
$EOD
$!
$ _SET FILE/PROT=40 SYSTEM:RSX.RTS
$!
$ _DELETE/NOLOG/NOWARN SYSTEM:RSX.TSK 
$ _DELETE/NOLOG/NOWARN SYSTEM:RSX.STB 
$!
$ _SET FILE/RUNTIME=RSX SYSTEM:ERR.STB
$ _SET FILE/RUNTIME=RSX SYSTEM:RTSODT.OBJ
$ _SET FILE/RUNTIME=RSX SYSTEM:RSX.RTS
$!
$EXIT:
$ _IF $SEVERITY .EQ. 1 THEN _WRITE 0 "RSXBLD completed successfully"
$ _IF $SEVERITY .NE. 1 THEN _WRITE 0 "RSXBLD completed with errors"
$!
$ _SET JOB/PRIORITY=-8
$ _exit