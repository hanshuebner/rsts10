;
;
EXTSCT=.99998:1000	; ALLOCATE PATCH SPACE IN ROOT
;
; GLOBAL REFERENCES TO ROOT AND OVERLAY SEGMENTS
;
;	GLOBAL REFERENCES FROM PAT.OLB
;
GBLREF=$PATCH,$RSTRT,$EXIT			; Module:PATCH
GBLREF=$ERBF,$ERBFE,$IBUF,$INSWT,$LNBUF,$LNDES	; Module:PATIO
GBLREF=$PTCMD,$INFDB,$OUFDB,$DFNB,$INFNB,$CRFNB	;
GBLREF=$OUFNB,$FILPT  				;
GBLREF=S$YNXT,S$YM,S$YFLG,S$CNT,S$YVAL,C$SLGH	; Module:PATBL
GBLREF=S$YSCT,S$YLGH,$ERVCT,$CLRBG,$CRCMT,$CSCNT;
GBLREF=$CKSUM,$CRCS,$CRCK,$CRSW,$INCS,$INCK,$INSW;
GBLREF=$INPSW,$MODNM,$IDENT,$SCTHD,$SWTCH,$SYMHD;
GBLREF=$XFRAD					;
GBLREF=$ALBLK					; Module:PTALO
GBLREF=$ISYMR,$SRCHR				; Module:PTSRC
GBLREF=$OPNOU,$OPNIN				; Module:OPNFL
GBLREF=$PATIN					; Module:PATIN
GBLREF=$INIDM					; Module:INIDM
GBLREF=$CMLIO					; Module:CMLIO
GBLREF=$SCANI,$SCANO,$SYNTX			; Module:PTSCN
;GBLREF=$OBSCN					; Module:OBSCN
GBLREF=$SRCMN					; Module:SRCMN
GBLREF=$BLGSD					; Module:BLGSD
GBLREF=$EDGSD					; Module:EDGSD
;GBLREF=$NBYTE,$GTBYT,$GTWRD			; Module:GTBYT
GBLREF=$MRGFL					; Module:MRGFL
GBLREF=$SCRLD					; Module:SCRLD
GBLREF=$ERTRP,$ERMSG				; Module:PATER
;
;	GLOBAL REFERENCES FROM SYSLIB.OLB
;
GBLREF=$EXST					; Module:EXST
GBLREF=.SAVR1					; Module:SAVR1
GBLREF=.GCML1,.GCML2,.GCML3			; Module:.GCML
GBLREF=.CSI1					; Module:.CSI1
GBLREF=.CSI2					; Module:.CSI2
GBLREF=.OPFNB					; Module:OPFNB
GBLREF=.FINIT,..FINI				; Module:FINIT
GBLREF=.PARSE,..PARS,..STFN			; Module:PARSE
GBLREF=.XQIO					; Module:XQIOU
GBLREF=.ENTER,.REMOV,.FIND			; Module:UDIREC
GBLREF=.POINT,.MARK				; Module:PNTMRK
GBLREF=.GETSQ					; Module:GETSQ
GBLREF=.PUTSQ					; Module:PUTSQ
GBLREF=.CLOSE					; Module:CLOSE
