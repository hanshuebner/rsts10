$!JOB/CCL/ERROR:FATAL/NOLIMIT [170,137]
$ _set job/keyb=RT11
ASSIGN D: LST
ASSIGN LST: NL
ASSIGN NL: LST
DEASSIGN NL
ASSIGN D: SY
ASSIGN D: DK
ASSIGN <40>
R SY:PIP
*.BAK/DE:NOWARN/LOG
$ _eod
R SY:MACRO
LINK0,LST:LINK0/C=LNKCND,LINK0
LINK1,LST:LINK1/C=LNKCND,LINK1
LINK2,LST:LINK2/C=LNKCND,LINK2
LINK3,LST:LINK3/C=LNKCND,LINK3
LINK4,LST:LINK4/C=LNKCND,LINK4
LINK5,LST:LINK5/C=LNKCND,LINK5
LINK6,LST:LINK6/C=LNKCND,LINK6
LINK7,LST:LINK7/C=LNKCND,LINK7
LINK8,LST:LINK8/C=LNKCND,LINK8
LNKEM,LST:LNKEM/C=LNKCND,LNKEM
LNKLB1,LST:LNKLB1/C=LNKCND,LNKLB1
$ _eod
R SY:LIBR
LNKLB1=LNKLB1
$ _eod
R SY:LINK
LINK.SV/B:1400/M:1400,LST:LINK/W/N,LINK=LINK0,LNKLB1/K:34/D//
LINK1/O:1
LINK2/O:1
LINK3/O:1
LINK4/O:1
LINK5/O:1
LINK6/O:1
LINK7/O:1
LINK8/O:1
LNKEM/O:1//
BITST
GETBUF
WRIT0
WRTLRU
ZSWFIL

$ _eod
R SY:LINK
LINKOD.SV/B:1400/M:1400,LST:LINKOD,LINKOD=LINK0,LNKLB1/K:34/I/T/D//
D:[170,46]RSTSLB
LINK1/O:1
LINK2/O:1
LINK3/O:1
LINK4/O:1
LINK5/O:1
LINK6/O:1
LINK7/O:1
LINK8/O:1
LNKEM/O:1//
O.ODT
O.ODT

BITST
GETBUF
WRIT0
WRTLRU
ZSWFIL

$ _eod
R SY:SILUS
LINK.SAV,LINK=LINK.SV
$ _eod
R SY:SILUS
LINKOD.SAV,LINKOD=LINKOD.SV
$ _exit
