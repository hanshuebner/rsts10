$! BLDSDA.COM - Build the unsupported SDA.4TH Utility
$!
$_IF F$ENVI("PROCEDURE").NES.F$SEAR("UNSUPP$:BLDSDA.COM") THEN _GOTO BL_VERSION
$ ASSIGN UNSUPP$: IN
$ ASSIGN UNSUPP$: OUT
$ ASSIGN UNSUPP$: FORTH
$_GOTO BUILD_SDA

$BL_VERSION:
$ ASSIGN D:[170,107] IN
$ ASSIGN D:[170,107] OUT
$ ASSIGN D:[170,107] FORTH
$ ASSIGN D:[170,40] TBL		!Location of file TBL.MAC
$!
$! Create a temporary file (SDAPFX.VER) which contains only the
$! TITLE line from D:[170,40]TBL.MAC.  This file is updated for
$! every baselevel, so it's a good file to get the version line
$! from.
$!
$ CLOSE/ALL
$ OPEN/WRITE/REPLACE 1 OUT:SDAPFX.VER
$ IF F$SEARCH("TBL:TBL.MAC") .EQS. "" THEN GOTO END
$ OPEN/READ 2 TBL:TBL.MAC

$LOOP:
$ READ/END=END 2 TEMP
$ TEMP = F$EDIT(TEMP,8+16)
$ IF F$INSTR(1,TEMP,"TITLE ") .NE. 1 THEN GOTO LOOP
$ WRITE 1 TEMP
$END:
$ CLOSE 1
$
$!
$! Create a temporary .MAC file (SDAPFX.TMP) which has all monitor 
$! definitions which we want to have pre-built into SDA.
$!
$_SET DATA/END="#"
$_CREATE/REPLACE OUT:SDAPFX.TMP
.INCLUDE	/D:[170,0]COMMON.MAC/
.INCLUDE	/D:[170,0]KERNEL.MAC/
.INCLUDE	/D:[170,42]INIPFX.MAC/
.INCLUDE	/D:[170,0]KBDEF.MAC/
.INCLUDE	/D:[170,0]MTDEF.MAC/
.INCLUDE	/D:[170,0]HDRDEF.MAC/
.INCLUDE	/D:[170,0]PFBDEF.MAC/
.INCLUDE	/D:[170,0]LATDEF.MAC/
.LIBRARY	/D:[170,0]NETDEF.SML/
.ENABL	MCL
$CCB
$NOB
$LLB
$LLX
$NETDDB
$NETFQB
$MSGFUN
$ADJDEF
$RTEMSC
.INCLUDE	/OUT:SDAPFX.VER/
.END
#EOD

$!
$! Now run SDAPFX.TMP through the MACRO assembler to get a listing file
$!
$_RUN $MACRO
,OUT:SDAPFX/N/N:TOC/L:SYM=OUT:SDAPFX.TMP
$_EOD
$_DELETE/NOLOG OUT:SDAPFX.TMP,OUT:SDAPFX.VER

$!
$! Run SDAPFX.LST through a TECO program to get it into shape 
$!
$_RUN FORTH:SDAPFX.TEC

$!
$! Now make it an RMS file for SORT to handle.  Create a temporary
$! definition file for this purpose.
$!
$_RUN $PIP               
OUT:SDAPFX.DEF/RMS=OUT:SDAPFX.LST
$_EOD
$_DELETE/NOLOG OUT:SDAPFX.LST

$!
$! Sort the SDAPFX.DEF file and delete the temporary definition file
$! 
$_SORT OUT:SDAPFX.DEF OUT:SDAPFX.OUT/KEY=(POS:1,SIZE:6,CHAR)
$_DELETE/NOLOG OUT:SDAPFX.DEF

$!
$! Create the output file including all SDA built-in definitions
$!
$_RUN $PIP
OUT:SDAPFX.DEF=OUT:SDAPFX.OUT/RMS
$_EOD
$_DELETE/NOLOG OUT:SDAPFX.OUT

$BUILD_SDA:
$!
$! Now build SDA.  It is expected to get lots of "xxxx isn't unique" messages.
$! Because some start with a question mark, we'll finish this up with a
$! SET NOON in effect.  
$!
$_SET NOON
$_SET JOB/KEYBOARD_MONITOR=FORTH
FLOAD IN:SDA.FTH
SAVE START OUT:SDA
(BYE)
$_EOD
$_EXIT
