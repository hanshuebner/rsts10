! TKBPBS.COM -- Build the RSTS Print/Batch Services (PBS) package

$ _SET NOON

$! Assign and display logicals

$ _ASSIGN/SYSTEM/REPLACE D:$		LIB
$ _ASSIGN/SYSTEM/REPLACE D:[170,0]	CMN
$ _ASSIGN D:[170,58] SYSTEM
$ _ASSIGN D:[170,58] LST
$ _ASSIGN D:[1,1]    LB
$ _SHOW LOGICAL/SYSTEM/ALL
$ _SHOW LOGICAL/ALL
$ _SET PROT=40/DEFAULT

$! Task Build the RMS resident library version of PBS
$ _RUN LIB:TKB
@SYSTEM:PBS

$! Task Build the RMS disk library (overlaid) version of PBS
$ _RUN LIB:TKB
@SYSTEM:PBSOVR

$! Task Build the debugger version of PBS
$ _RUN LIB:TKB
@SYSTEM:PBSDBG

$! Cref the taskbuild maps
$ _RUN SYSTEM:BCRF
LST:PBS.MAP
LST:PBSOVR.MAP
LST:PBSDBG.MAP
$ _EOD

$!  Build special TAP version (PBSTAP.TSK)
$ _COPY/REPLACE SYSTEM:PBS.TSK SYSTEM:PBSTAP.TSK
$ _RUN LIB:ONLPAT
SYSTEM:PBSTAP.CMD
SYSTEM:PBSTAP.TSK/N
$ _EOD

$!  Build PBS error file PBSERR.ERR
$ _BASIC/BPLUS
RUN SYSTEM:PBSERR.BAS
SYSTEM:PBSERR.TXT
SYSTEM:PBSERR.ERR
$ _EOD
$ _SET JOB/KEYBOARD=DCL

$! PBS build complete
$EXIT
