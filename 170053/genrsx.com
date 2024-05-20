$!******************************************************************************
$!
$! GENRSX.COM - command file to build the RSX RTS and RSX libraries.
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
$! Set up logicals to point to local (test) accounts
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
$!
$! Set protections
$!
$ _SET FILE/PROT=40 RSX:COMMON.MAC
$ _SET FILE/PROT=40 RSX:RXMDIR.MAC
$ _SET FILE/PROT=40 RSX:PLSMAC.MAC
$ _SET FILE/PROT=40 RSX:SYSLIB.OLB
$ _SET FILE/PROT=40 RSX:VMLIB.OLB
$ _SET FILE/PROT=40 RSX:RSXMAC.SML
$ _SET FILE/PROT=40 FCS:FCSMAC.MAC
$ _SET FILE/PROT=40 FCS:PRTMAC.MAC
$ _SET FILE/PROT=40 FCS:QIOMAC.MAC
$ _SET FILE/PROT=40 FCS:RSTS.MAC
$ _SET FILE/PROT=40 UTL:CSIMAC.MAC
$ _SET FILE/PROT=40 UTL:GCLMAC.MAC
$ _SET FILE/PROT=40 UTL:EGCMMA.MAC
$ _SET FILE/PROT=40 UTL:HLPMAC.MAC
$ _SET FILE/PROT=40 UTL:TPMAC.MAC
$ _SET FILE/PROT=40 UTL:TKBRTO.CMD
$ _SET FILE/PROT=40 UTL:LOAD.DGE
$ _SET FILE/PROT=40 UTL:AUTOT.DGE
$ _SET FILE/PROT=40 UTL:AUTO.DGE
$ _SET FILE/PROT=40 UTL:AUTOX.DGE
$ _SET FILE/PROT=40 UTL:AUTOY.DGE
$ _SET FILE/PROT=40 UTL:OVCTC.DGE
$ _SET FILE/PROT=40 UTL:OVCTR.DGE
$ _SET FILE/PROT=40 UTL:OVCTL.DGE
$ _SET FILE/PROT=40 TKB:HDRDF.MAC
$ _SET FILE/PROT=40 TKB:LBLDF.MAC
$ _SET FILE/PROT=40 TKB:SEGDF.MAC
$ _SET FILE/PROT=40 TKB:PLSDF.MAC
$ _SET FILE/PROT=40 TKB:MACFLM.MAC
$!
$ _SUBMIT/QUEUE=BASELEVEL:/NOTIFY FCS:FCSBLD.COM/LOGF=FCS.LOG
$ _SUBMIT/QUEUE=BASELEVEL:/NOTIFY UTL:UTLBLD.COM/LOGF=UTL.LOG
$ _SUBMIT/QUEUE=BASELEVEL:/NOTIFY RSX:RSXBLD.COM/LOGF=RSX.LOG
$ _SUBMIT/QUEUE=BASELEVEL:/NOTIFY TKB:TKBBLD.COM/LOGF=TKB.LOG
$ _SUBMIT/QUEUE=BASELEVEL:/NOTIFY MAC:MACBLD.COM/LOGF=MAC.LOG
$ _SUBMIT/QUEUE=BASELEVEL:/NOTIFY LBR:LBRBLD.COM/LOGF=LBR.LOG
$ _SUBMIT/QUEUE=BASELEVEL:/NOTIFY PAT:PATBLD.COM/LOGF=PAT.LOG
$ _SUBMIT/QUEUE=BASELEVEL:/NOTIFY RNO:RNOBLD.COM/LOGF=RNO.LOG
$ _SUBMIT/QUEUE=BASELEVEL:/NOTIFY CRF:CRFBLD.COM/LOGF=CRF.LOG
$!
$EXIT:
$ _IF $SEVERITY .EQ. 1 THEN _WRITE 0 "GENRSX.COM completed successfully"
$ _IF $SEVERITY .NE. 1 THEN _WRITE 0 "GENRSX.COM completed with errors"
$!
$ _SET JOB/PRIORITY=-8
$ _exit
