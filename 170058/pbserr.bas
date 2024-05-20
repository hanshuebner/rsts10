1	!	P B S E R R . B A S   &
	&
	!	This utility takes the standard RMS error message file &
	!	and creates a FIXED 64-byte record file with 'unused' RMS &
	!	error numbers added.  The input file to this utility is &
	!	a standard stream ASCII file. &
	&
	&
	ON ERROR GOTO 19000 &
\	DIM M%(30), REC$(7) &

100	PRINT IF CCPOS(0%) &
\	PRINT "PBSERR", DATE$(0%), TIME$(0%) &
\	PRINT "Build RMS error file PBSERR.ERR" &
\	PRINT &
		!display program heading &

200	CHANGE SYS(CHR$(6%)+CHR$(26%)) TO M% &
\	PPN$ = "[" + NUM1$(M%(22%) AND 255%) + "," + NUM1$(M%(21%)) + "]" &
\	IN.DFLT$ = "SY:" + PPN$ + "PBSERR.TXT" &
\	OUT.DFLT$ = "SY:" + PPN$ + "PBSERR.ERR" &
		!build defaults for input and output files &

300	PRINT "Input file <"; IN.DFLT$; ">"; &
\	INPUT LINE IN$ &
\	IN$ = CVT$$(IN$,-1%) &
\	IN$ = IN.DFLT$ UNLESS LEN(IN$) &
\	IN$ = IN$ + "/RO" UNLESS INSTR(1%,IN$,"/RO") &
\	OPEN IN$ FOR INPUT AS FILE 1% &
		!prompt for input file &
		!open input file &

400	PRINT "Output file <"; OUT.DFLT$; ">"; &
\	INPUT LINE OUT$ &
\	OUT$ = CVT$$(OUT$,-1%) &
\	OUT$ = OUT.DFLT$ UNLESS LEN(OUT$) &
\	OPEN OUT$ FOR OUTPUT AS FILE 2% &
\	FIELD #2%, 64%*Z% AS Z$, 64% AS REC$(Z%) &
		FOR Z% = 0% TO 7% &
		!prompt for output file &
		!open and field output file &

500	NULLS$ = STRING$(64%,0%) &
\	NXT%,INTERVAL%=-4% &
\	INTABS%=ABS(INTERVAL%)-1% &
\	NO.RECS% = 0% &
\	BUF.IDX% = 0% &
\	PRINT "Creating RMS error file "; OUT$; "..." &
		!init constants, counters, indexes &
		!display start message &

1000	!	m a i n   p r o g r a m   l o o p   &
	&
	&
	INPUT LINE #1%, LN$ &
\	LN$=CVT$$(LN$,4%) &
\	STRT%=INSTR(2%,LN$,'	') &
\	COMA1%=INSTR(STRT%+1%,LN$,',') &
\	COMA2%=INSTR(COMA1%+1%,LN$,',') &
\	COMA3%=INSTR(COMA2%+1%,LN$,',') &
\	MNEMONIC$=MID(LN$,STRT%+1%,COMA2%-COMA1%-1%) &
\	ORECO$=MID(LN$,COMA1%+1%,COMA2%-COMA1%-1%) &
\	RECO$=MID(LN$,COMA2%+1%,COMA3%-COMA2%-2%) &
\	RECO%=VAL(RECO$) &
\	TEXT$=MID(LN$,COMA3%+2%,LEN(LN$)-COMA3%+2%-4%) &
		!input next record &
		!parse all the fields &

1100	IF RECO% < NXT% THEN &
		TXT$ = "	UNUSED RMS ERROR MESSAGE NUMBER " + &
			NUM1$(NXT%) + "." &
\		GOSUB 10000 &
\		NO.RECS% = NO.RECS% + 1% &
\		NXT%=NXT%+INTERVAL% &
\		GOTO 1100 &
		!if not 'next' error number, &
		!	write unused error number to file &
		!	incr record counter &
		!	incr interval counter &
		!	and try again &

1200	PRINT "%Bad interval: "; LN$ &
		IF RECO% AND INTABS% &
\	TXT$ = MNEMONIC$ + "	" + TEXT$ &
\	GOSUB 10000 &
\	NO.RECS% = NO.RECS% + 1% &
\	NXT%=NXT%+INTERVAL% &
\	GOTO 1000 &
		!display bad interval warning &
		!	if not a multiple of 8 &
		!write text to file &
		!incr record counter &
		!incr interval counter &
		!go get next input record &

2000	!	p o s t - f i l e   p r o c e s s i n g   &
	&
	&
	PUT #2% IF BUF.IDX% &
\	PRINT "Build completed -"; NO.RECS%; "error messages in file" &
\	GOTO 32000 &
		!write any partial buffer to disk &
		!display completion msg &
		!skip to exit &

10000	!	w r i t e   n e x t   r e c o r d   t o   f i l e   &
	&
	&
	IF BUF.IDX% > 7% THEN &
		PUT #2% &
\		BUF.IDX% = 0% &
		!if buffer is full, &
		!	write buffer to disk &
		!	reset buffer index &

10100	LSET REC$(BUF.IDX%) = TXT$ + NULLS$ &
\	BUF.IDX% = BUF.IDX% + 1% &
\	RETURN &
		!write record to buffer &
		!incr buffer index &
		!exit &

19000	!	e r r o r   h a n d l i n g   &
	&
	&
	IF ERR = 11% THEN &
		IF ERL < 1000% THEN &
			RESUME 32000 &
		ELSE	RESUME 2000 &
		!handle EOF errors &

19999	ON ERROR GOTO 0 &

32000	!	p r o g r a m   e x i t   &
	&
	&
	CLOSE 1%,2% &
		!close files &

32767	END
