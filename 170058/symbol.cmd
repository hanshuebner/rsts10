;
;	SYMBOL.CMD
;
;	COPYRIGHT (C) 1983
;	DIGITAL EQUIPMENT CORP., MAYNARD, MASS.
;
;	TKB command file to define SPL global references
;
;
;File: SKED.MAC
;
;GBLREF=DSPAT,NAME,P.XMIT,PATCH,PROMAX,JOBMAX
;GBLREF=SPRO,SJOB,CIS,DEVLST,UCONTXT,LUNTBL
;GBLREF=LUN0,LUN1,LUN2,LUN3,LUN4,LUN5
;GBLREF=LUN6,LUN7,LUN8,LUN9,LUN10,LUN11
;GBLREF=LUN12,LUN13,LUN14,LUN15
;GBLREF=DETSTR,START,NXTJOB,SKED,PAGER,RUN
;GBLREF=STALL,GETJDB,GETLUN,RETLUN,GETDEV,RETDEV
;GBLREF=RETDAL,GTCBUF,CLRBUF,MOVSTR,GETBUF,RETBUF
;GBLREF=GETSPACE,XBUFF,TSKSIZ,CRASH,SHUTUP
;GBLREF=CRLF
GBLREF=..RUN		;** PATCH TO NOP TO ALLOW 'RUN SPL'
GBLREF=..SPTB
GBLREF=..IDLE
GBLREF=..RESW
GBLREF=...JOB
GBLREF=.SPLER
GBLREF=..PPRT
GBLREF=..PBAT
GBLREF=..SPRT
GBLREF=..SBAT
;
;File: SKDINI.MAC
;
GBLREF=INISYS,SKDCRA
GBLREF=..NMRM		; ** PATCH TO NOP TO DISABLE MSR RIB REMOVE
;
;File: SKDPMD.MAC
;
;GBLREF=PMD
;
;File: RMSOPN.MAC
;
GBLREF=RMSINIT,RMSOPN
GBLREF=$RMCLO
GBLREF=..BIO		; ** PATCH TO NOP TO DISABLE BLOCK IO
GBLREF=..MBKT		; ** PATCH FOR MAX LEGAL BUCKET SIZE
;
;File: RMSGET.MAC
;
GBLREF=RMSGET
;
;File: RMSCLS.MAC
;
GBLREF=RMSCLS
;
;File: GETTXT.MAC
;
GBLREF=GETTXT,ERRFIL,ERRTBL
;
;File: XMIT.MAC
;
GBLREF=XMITQM,XMITQ,XMIT
GBLREF=$$RTRY		; ** PATCH FOR ?
;
;File: QMAN.MAC
;
GBLREF=QMAN
GBLREF=..NOCA		; ** PATCH TO NOP TO DISABLE CACHE/RAN OF QUE FILE
GBLREF=..NCPR		; ** PATCH TO 'BR QNCP' TO SKIP QUEUE COMPRESS
;
;File: MSGSER.MAC
;
GBLREF=MSGSER
;
;File: SPLER.MAC
;
GBLREF=PRTSER
GBLREF=BATSER
GBLREF=..OBLN		; ** PATCH TO CHANGE SIZE OF OUTPUT BUFFER
GBLREF=..NNET		; ** PATCH TO BR 10$ TO DISABLE NETWORK FILES SPECS
GBLREF=..DFSL		; ** PATCH TO CHANGE DEFAULT SLEEP TIMER
GBLREF=..LNGW		; ** PATCH TO CHANGE LENGTH OF A 'LONG' WRITE ATTEMPT
GBLREF=..SUCP		; ** PATCH TO CHANGE MAX SUCCESS-PUT COUNTER
GBLREF=..LPWT		; ** PATCH TO CHANGE LP WAIT TIMER
GBLREF=..ZERP		; ** PATCH TO CHANGE MAX ZERO-PUT COUNTER
GBLREF=..SMLW		; ** PATCH TO CHANGE LENGTH OF A 'SHORT' WRITE ATTEMPT
GBLREF=..MAXS		; ** PATCH TO CHANGE MAXIMUM SLEEP TIMER
GBLREF=..ZERC		; ** PATCH TO CHANGE COUNT OF FAILURES TO INC SLEEPER
;
;File: CHRGEN.MAC
;
GBLREF=CHRLIN
;
;File: CHARS.MAC
;
GBLREF=CHARS
;
; END

