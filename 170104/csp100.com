 
$!
$!	CSP100.COM
$!
$!	Command to create CSP100.LIB
$!
$ _Deassign/All
$ _Assign D:[170,104] CSP
$ _Run $MAC
CSP:PCHCSP,CSP:PCHCSP/CR/-SP=CSP:PRE,CSP:PCHCSP
$ _Run $CRF
CSP:PCHCSP
$ _Run $TKB
@CSP:CSP100
$ _Run $MAKSIL
CSP:CSP100
CSP:CSP100.TSK
YES
CSP:CSP100.STB
CSP:CSP100.LIB
$ _Copy CSP:CSP100.TSK D:[1,1]
$ _Copy CSP:CSP100.STB D:[1,1]
$ _Exit
