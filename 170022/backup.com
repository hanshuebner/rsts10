$!
$! BACKUP.COM - Build the BACKUP package
$!
$! Modification History:
$!
$! Date		Edit		Reason
$! ====		====		======
$! 22-NOV-83	001 KPH		Creation
$! 27-OCT-89    002 BGN         Add image code
$! 20-Mar-90	003 JJT		Assign D:[170,106] OMS
$! 15-Aug-90	004 BGN		Add cross reference for the .LST and .MAC files
$!
$!
$! Set up defaults for symbols
$!
$!	P1 = Account BACKUP lives in
$!	P2 = Account where utilities can be found
$!
$ IF P1.EQS."" THEN $P1 = "D:[170,22]"
$ IF P2.EQS."" THEN $P2 = "D:$"
$
$! Set up private logical names
$!
$ ASSIGN 'P1' SYSTEM
$ ASSIGN D:[170,106] OMS
$!
$! Set up system logical names
$!
$ ASSIGN/SYSTEM/REPLACE D:[170,19] CUI
$ ASSIGN/SYSTEM/REPLACE D:[170,46] SGC
$ ASSIGN/SYSTEM/REPLACE D:[170,0] CMN
$ SHOW LOG/SYS/ALL
$!
$! Assemble the sources
$!
$DEFINE/COMMAND/SYSTEM/REPLACE DMAC 'P2'MAC.TSK
$MACRO := DMAC
$MACRO SYSTEM:BACKUP,SYSTEM:BACKUP/CR=SYSTEM:BACKUP
$MACRO SYSTEM:BCKSUM,SYSTEM:BCKSUM/CR=SYSTEM:BCKSUM
$MACRO SYSTEM:BCKLIS,SYSTEM:BCKLIS/CR=SYSTEM:BCKLIS
$MACRO SYSTEM:BCKCUI,SYSTEM:BCKCUI/CR=SYSTEM:BCKCUI
$MACRO SYSTEM:BCKKEY,SYSTEM:BCKKEY/CR=SYSTEM:BCKKEY
$MACRO SYSTEM:BCKMSG,SYSTEM:BCKMSG/CR=SYSTEM:BCKMSG
$MACRO SYSTEM:BCKCMP,SYSTEM:BCKCMP/CR=SYSTEM:BCKCMP
$MACRO SYSTEM:BCKDTA,SYSTEM:BCKDTA/CR=SYSTEM:BCKDTA
$MACRO SYSTEM:BCKRMS,SYSTEM:BCKRMS/CR=SYSTEM:BCKRMS
$MACRO SYSTEM:BCKFIL,SYSTEM:BCKFIL/CR=SYSTEM:BCKFIL
$MACRO SYSTEM:BCKBUF,SYSTEM:BCKBUF/CR=SYSTEM:BCKBUF
$MACRO SYSTEM:BCKTHR,SYSTEM:BCKTHR/CR=SYSTEM:BCKTHR
$MACRO SYSTEM:CRCTAB,SYSTEM:CRCTAB/CR=SYSTEM:CRCTAB
$MACRO SYSTEM:BCKSUB,SYSTEM:BCKSUB/CR=SYSTEM:BCKSUB
$MACRO SYSTEM:BCKINI,SYSTEM:BCKINI/CR=SYSTEM:BCKINI
$MACRO SYSTEM:RSTRMS,SYSTEM:RSTRMS/CR=SYSTEM:RSTRMS
$MACRO SYSTEM:RSTFIL,SYSTEM:RSTFIL/CR=SYSTEM:RSTFIL
$MACRO SYSTEM:RSTDIR,SYSTEM:RSTDIR/CR=SYSTEM:RSTDIR
$MACRO SYSTEM:RSTSUM,SYSTEM:RSTSUM/CR=SYSTEM:RSTSUM
$MACRO SYSTEM:IMGFIL,SYSTEM:IMGFIL/CR=SYSTEM:IMGFIL
$MACRO SYSTEM:IMGRMS,SYSTEM:IMGRMS/CR=SYSTEM:IMGRMS
$DELETE/COMMAND/SYSTEM DMAC
$!
$! Build the cross reference
$!
$RUN 'P2'CRF
SYSTEM:BACKUP
SYSTEM:BCKSUM
SYSTEM:BCKLIS
SYSTEM:BCKCUI
SYSTEM:BCKKEY
SYSTEM:BCKMSG
SYSTEM:BCKCMP
SYSTEM:BCKDTA
SYSTEM:BCKRMS
SYSTEM:BCKFIL
SYSTEM:BCKBUF
SYSTEM:BCKTHR
SYSTEM:CRCTAB
SYSTEM:BCKSUB
SYSTEM:BCKINI
SYSTEM:RSTRMS
SYSTEM:RSTFIL
SYSTEM:RSTDIR
SYSTEM:RSTSUM
SYSTEM:IMGFIL
SYSTEM:IMGRMS
$EOD
$!
$! Build a library
$! Add the modules to the library
$!
$RUN 'P2'LBR.TSK
SYSTEM:BACKUP/CR:250.
SYSTEM:BACKUP/IN=SYSTEM:BCKSUM,SYSTEM:BCKLIS,SYSTEM:BCKCUI,SYSTEM:BCKMSG
SYSTEM:BACKUP/IN=SYSTEM:BCKTHR,SYSTEM:BCKKEY,SYSTEM:BCKCMP,SYSTEM:BCKDTA
SYSTEM:BACKUP/IN=SYSTEM:BCKRMS,SYSTEM:BCKFIL,SYSTEM:BCKBUF,SYSTEM:CRCTAB
SYSTEM:BACKUP/IN=SYSTEM:BCKSUB,SYSTEM:BCKINI,SYSTEM:RSTRMS,SYSTEM:RSTFIL
SYSTEM:BACKUP/IN=SYSTEM:RSTDIR,SYSTEM:RSTSUM,SYSTEM:BACKUP,SYSTEM:IMGFIL
SYSTEM:BACKUP/IN=SYSTEM:IMGRMS
$EOD
$!
$! Make the BACKUP library contiguous to speed TKB
$!
$RUN 'P2'PIP.SAV
SYSTEM:BACKUP.OLB/MO:64=SYSTEM:BACKUP.OLB
$EOD
$!
$! Now Task-build BACKUP
$!
$SET NOON
$RUN 'P2'TKB.TSK
@SYSTEM:BACKUP.TKB
$SET ON
$!
$! Now make BACKUP a SIL
$!
$ RUN 'P2'MAKSIL
BACKUP
SYSTEM:BACKUP.TSK
YES
SYSTEM:BACKUP.STB
SYSTEM:BACKUP.TSK<232>
$!
$! Make a cross reference of the .MAP
$!
$RUN 'P2'CRF
SYSTEM:BACKUP.MAP
$EOD
$_EXIT
