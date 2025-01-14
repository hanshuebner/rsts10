; RNOBLD.CMD
;
;	BUILD RUNOFF TASK
;
;	RUNOFF VERSION M01
;
SYSTEM:RNO,RNO/MA/CR/-SP,RNO=RNO:RNOBLD/MP
;
;	TASK IDENTIFICATION OPTIONS
;
TASK=...RNO		; TASK NAME FOR MCR INITIATION
PAR=GEN:0:177000	; SET PARTITION BASE,SIZE FOR MAPPED SYSTEM
UIC=[1,2]		; SET TO RUN UNDER PRIVILEGED ACCOUNT
;
;	DEVICE SPECIFYING OPTIONS
;
UNITS=5			; RESERVE HEADER SPACE FOR 5 LUNS
ASG=TI:1:2:3:4		; ASSIGN COMMAND INPUT AND OUTPUT DEVICES
;
;	MEMORY ALLOCATION OPTIONS
;
STACK=64		; ALLOCATE 64 WORDS FOR STACK
EXTSCT=.99998:1000	; ALLOCATE PATCH SPACE IN ROOT
;EXTTSK=2048		; SPECIFY DYNAMIC AREA
;
;	STORAGE ALTERING OPTIONS
;
GBLPAT=RNO:LSTBLK:4		; GENERATE ASCII STREAM OUTPUT
GBLPAT=CLOSE:$.NOA0:0		; DON'T WRITE FILE ATTRIBUTES
GBLPAT=CLOSE:$.NOA1:0		; ...ON OPEN AS WELL AS CLOSE
;
; GLOBAL REFERENCES TO ROOT AND OVERLAY SEGMENTS
;
;	GLOBAL REFERENCES FROM RNO.OLB
;
;GBLREF=F.1,S1,$HDSSW,$GCISW,$SDISW,$ULMSW,$ULNSW; Module:RUNOFF
;GBLREF=$ULSSW,$AUTSW,$CFLSW,$HDRSW,$HFLSW,$HPHSW;
;GBLREF=$NUMSW,$PGPSW,$PERSW,$SBPSW,$ULLSW,LIN	;
;GBLREF=ENDBCM,ENDCM,COMNT,LCR,LGO,TEXT,ILCM	;
;GBLREF=ONPAG,EXPAND,ALPH1,ALPH,ALPH2,LINSET	;
;GBLREF=ULBSET,RCNR,RCNO,$FRCND,OUTNJ,SKIPS,SKIP1;
;GBLREF=SKIPN,NSPAC,PSTRPA,PSTRAZ,CCSPC,GCIN,CCIN;
;GBLREF=PAGEC,TPAGE,BPAGE,DECPRT,GCI,WCIFTN	;
;GBLREF=WLNIN1,WCI				;
;GBLREF=AUTOP,NAUTO,DSAFL,DSCFL,ENCFL,DSHFL,ENHFL; Module:RNCMD
;GBLREF=HYPHN,NHYPH,INDENT,BREAK,LWCAS,UPCAS	;
;GBLREF=NUMON,NUMOF,CPAGE,SETRM,SETLM,STAND,SETPG;
;GBLREF=PARAG,PARTP,LSTTP,TSTPG,TESTP,SSP,SETSTL	;
;GBLREF=FTITL,SETTL,SETBF,SKIPL,LINSKP,FIGUR	;
;GBLREF=LITRL,ELTRL,JUSTN,JUSOF,FILLN,FILOF	;
;GBLREF=SETTAB,CENTER,FOOTN,PERSP,NPERS,SHFUC	;
;GBLREF=FMSG,CRLF,FOUT,FIN,OUTPUT,TTC3,TTC33	; Module:RNFIO
;GBLREF=TTC4N,TTC1				;
;GBLREF=TTBLK,TTIBLK,TTLDMY,LSTBLK,SWTBL,ULSWT	; Module:RNORSX
;GBLREF=SPSAV,$SWTCH				;
;GBLREF=DOINX,PINDX				; Module:PINDX
;GBLREF=APNDX,CHPTR,HEADP,NHEAD,HEADR,LISTC,LSTEL; Module:FMTCM
;GBLREF=ELIST,NOTE,ENOTC,NAPDX,NCHPT		;
;GBLREF=CMTAB,ELCMD,ENOTE,ELSTC,ECTAB		; Module:CMTAB
;GBLREF=CMN					; Module:COMND
;GBLREF=$START,RUNOFF,ENDFIL			; Module:START
;GBLREF=BFOVF,ILCMM,ILCMM2,JUSRM1,HALTM,OUTERR	; Module:ERMSG
;GBLREF=INPERR,CORERR,LSTERR,NOTERR,MGCMDE,MGOFER;
;GBLREF=MGOPER,MGIFER,MGIPER,MGPOST,$ERMSG,$INITL;
;GBLREF=INDEX 					; Module:INDEX
;GBLREF=HYPHEN					; Module:HYPHEN
;
;	GLOBAL REFERENCES FROM SYSLIB.OLB
;
;GBLREF=.GETSQ					; Module:GETSQ
;GBLREF=.PUTSQ					; Module:PUTSQ
;;GBLREF=.GCML1,.GCML2,.GCML3			; Module:.GCML
;;GBLREF=.POINT,.MARK				; Module:PNTMRK
;;GBLREF=.XQIO					; Module:XQIOU
;;GBLREF=..PNT1					; Module:POINT
;GBLREF=.CSI1					; Module:.CSI1
;GBLREF=.CSI2					; Module:.CSI2
;GBLREF=.OPEN					; Module:OPEN
;GBLREF=.CLOSE					; Module:CLOSE
;GBLREF=..CREA					; Module:CREATE
;GBLREF=..DEL1					; Module:DEL
;GBLREF=.FINIT,..FINI				; Module:FINIT
;GBLREF=..MKDL					; Module:MKDL
;GBLREF=.OPFNB					; Module:OPFNB
;GBLREF=$RLCB,$RQCB				; Module:RQLCB
;GBLREF=.PARSE,..PARS,..STFN			; Module:PARSE
//
