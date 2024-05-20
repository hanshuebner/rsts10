2!		PROGRAM		: RUNOFF.BAS
5!		VERSION		: V10.1
6!		EDIT		: A
7!		EDIT DATE	: 10-MAY-91
10	EXTEND		!USE BASIC-PLUS EXTEND MODE
11	! &
	&
	&
	!		  C O P Y R I G H T &
	&
	&
  !		      Copyright (C) 1974, 1991 by &
  !	        Digital Equipment Corporation, Maynard, Mass. &
  !	&
  !	&
  !	This software is furnished under a license and may be used and &
  !	copied  only  in accordance with the terms of such license and &
  !	with the  inclusion  of  the  above  copyright  notice.   This &
  !	software  or  any  other copies thereof may not be provided or &
  !	otherwise made available to any other person.  No title to and &
  !	ownership of the software is hereby transferred. &
  !	&
  !	The information in this software is subject to change  without &
  !	notice  and should not be construed as a commitment by Digital &
  !	Equipment Corporation. &
  !	&
  !	DIGITAL assumes no responsibility for the use  or  reliability &
  !	of its software on equipment that is not supplied by DIGITAL. &
  !	&
  !******************************************************************* &
	&

20	! &
	&
	&
	!	M O D I F I C A T I O N    H I S T O R Y &
	&
	&

21	! VER/ED	EDIT DATE	REASON &
	! V9.6		26-JAN-88	(REG) PRINT TO PBS IF $QUE ABSENT &
	! &

100	DIM	Z%(30%),Z1%(30%), &
		PAR%(2%), &
		T8%(33%),T9%(33%), &
		FLGS%(10%),SVFLGS%(10%) &
		! Z%(),Z1%() - PARSING FILE NAMES &
		! PAR%() - PARAGRAPH COMMAND INFO: &
		!	PAR%(0%) = # OF SPACES TO INDENT PARAGRAPHS (5) &
		!	PAR%(1%) = VERTICAL SPACING BETWEEN PARAS   (-1) &
		!	PAR%(2%) = "TEST PAGE" BEFORE PARA          (2) &
		! TAB SETTINGS T9%() IS THE ORIGINAL - IN INCREMENTS OF 8 &
		! FLAG SETTINGS - OLD/TEMP FLAG SETTINGS. &

140	DIM C%(256%),D%(256%) &
	\ DIM #4%,I9$(1000%)=64% &
	\ DIM #4%,I8$(1000%,31%)=2% &
		! CHARACTERS. &
		! AND VIRTUAL BUFFER. &
	&

500	E0% = 0% &
		! SIGNAL NORMAL ENTRY FOR HERALD PRINTOUT &

600	I7$="V10.1-A" &
		! SET UP THE VERSION/EDIT #. &

610	PRINT "RUNOFF";CHR$(9%);I7$;CHR$(9%); &
	CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)),3%),4%) IF E0% = 0% &
		! PRINT THE HEADER FOR 'RUN' ENTRIES. &

620	I7$="" &
	\ ON ERROR GOTO 19000 &
	\ GOSUB 10300 &
		! SET UP STANDARD ERROR TRAP. &
		! SET UP FILENAMES (FOOTNOTE) AND FLAGS. &

630	F1%,KB%,QUE%,X9%=0% &
	\ FORM.SIZ%=66% &
	\ PRINT &
	\ PRINT "Source File"; &
	\ GOSUB 10000 &
	\ GOSUB 10100 &
	\ INFILE$=FNFIL.NAM$(FIL$,"_SY:.RNO") &
	\ GOTO 630 UNLESS LEN(INFILE$) &
	\ OPEN INFILE$ FOR INPUT AS FILE 1% &
	\ CHANGE SYS(CHR$(12%)) TO Z% &
	\ NICE.NAM$=RAD$(Z%(7%)+SWAP%(Z%(8%)))+ &
		RAD$(Z%(9%)+SWAP%(Z%(10%)))+"."+ &
		RAD$(Z%(11%)+SWAP%(Z%(12%))) &
		! CLEAR OUTPUT TO KB: INDICATOR, QUE FILE INDICATOR, AND &
		!  CONVERSION FILE FLAG. &
		! SET DEFAULT FORM SIZE. &
		! GET THE INPUT SPEC. &
		! SEPARATE AND SCAN ANY SWITCHES (/QUE OR /CONVERT). &
		! FILL IN FILENAME DEFAULTS AND OPEN THE FILE. &

640 	IF ASCII(RIGHT(I1$,LEN(I1$)))=27% THEN &
		PRINT &
	\	I%=INSTR(1%,INFILE$,".") &
	\	OUTFILE$=LEFT(INFILE$,I%)+"DOC" &
	\	OPEN OUTFILE$ FOR OUTPUT AS FILE 2% &
	\	KB%=(STATUS AND 255%)=2% &
	\	SIM.FF%=0% &
	\	UNDERSCR%,UNDERSCR.SP%=-1% &
	\	UNDERSCR.BS.LF%=10% &
	\	UNDERSCR.CHAR%=45% &
	\	GOTO 680 &
		! IF THE SOURCE FILE IS TERMINATED WITH AN 'ESC' THEN &
		!	OUTPUT FILE HAS SAME NAME WITH .DOC EXTENSION &
		!	KB%<>0 IF TERMINAL OUTPUT SPECIFIED. &
		!	NO FORM-FEED SIMULATION. &
		!	SET UNDERLINING AS FOR "S" &

650	PRINT IF CCPOS(0%)<>0% &
	\ PRINT "Output File"; &
	\ GOSUB 10000 &
	\ GOSUB 10100 &
	\ OUTFILE$=FNFIL.NAM$(FIL$,"_SY:"+LEFT(NICE.NAM$,6%)+".DOC") &
	\ GOTO 650 UNLESS LEN(OUTFILE$) &
	\ OPEN OUTFILE$ FOR OUTPUT AS FILE 2% &
	\ KB%=(STATUS AND 255%)=2% &
		! GET THE OUTPUT SPEC. &
		! SEPARATE AND SCAN ANY SWITCHES (/QUE OR /CONVERT). &
		! FILL IN FILENAME DEFAULTS AND OPEN THE FILE. &
		! (NICE.NAM$ INSURES A 6 CHAR FILENAME.) &
		! KB%<>0 IF TERMINAL OUTPUT SPECIFIED. &
	&

660	PRINT IF CCPOS(0%)<>0% &
	\ PRINT "Underscore (B, L, S or N)"; &
	\ GOSUB 10000 &
	\ UNDERSCR.SP%=NOT FNS%("SPACES") &
	\ X%=ASCII(CVT$$(I$,-2%)) &
	\ UNDERSCR%=(X%=76% OR X%=83% OR X%=66%) &
	\ UNDERSCR.BS.LF%=0% &
	\ UNDERSCR.CHAR%=95% &
	\ IF UNDERSCR% THEN &
			IF X%=66% THEN &
				UNDERSCR.BS.LF%=8% &
		ELSE	IF X%=83% THEN &
				UNDERSCR.BS.LF%=10% &
	\			UNDERSCR.CHAR%=45% &
		! GET/SET UNDERSCORING VARIABLES. &

670	PRINT IF CCPOS(0%)<>0% &
	\ PRINT "Simulate Form Feed (Y or N)"; &
	\ GOSUB 10000 &
	\ SIM.FF%=(ASCII(CVT$$(I$,-2%))=89%) &
	\ IF SIM.FF% THEN &
		PRINT "Form Size <66>"; &
	\	GOSUB 10000 &
	\	I$=CVT$$(I$,-2%) &
	\	FORM.SIZ%=VAL(I$) IF LEN(I$) &
		! SIMULATE FORM-FEEDS? &
		! IF SO, GET THE FORM SIZE (DEFAULT=66) &

680	DAY$=XLATE(DATE$(0%),X9$) &
	\ PAUS%=-1% &
	\ IF KB% THEN &
		PRINT "Pause"; &
	\	GOSUB 10000 &
	\ 	PAUS%=(ASCII(CVT$$(I$,-2%))<>89%) &
		! SET DEFAULT TO NO PAUSE. &
		! IF OUTPUT IS KB: THEN &
		! 	SEE IF USER WANTS 'PAUSE' SET. &
	&

689	! &
	&
	&
	!	I N I T I A L I Z E    D E F A U L T S &
	&

690	F1%=1% &
	\ F2%=2% &
	\ L$,R$,SUBTTL$,TITLE$="" &
	\ R%,D%(0%)=0% &
	\ PAG.HDR$="Page" &
	\ PERIOD%,CASE%,FILLL%,J9%,J7%=-1% &
	\ T0%,T1%,U7%,E9%,Z9%,I9%,AUTOPAR%,DAT%=0% &
	\ PAGE.SIZ%,LINES.LEFT.ON.PAG%=58% &
	\ W1%,R1%,W9%,RGT.MRG%=60% &
	\ L1%,LFT.MRG%,INDENT%,FOOTNOT.LINES%=0% &
	\ I7$="" &
	\ HDR%=5% &
	\ S1%,S9%=1% &
	\ PAG.CREATED%,PAG.NO%=1% &
	\ D9%=-1% &
		! SET DEFAULTS. &

700	PAR%(0%)=5% &
	\ PAR%(1%)=0% &
	\ PAR%(2%)=2% &
	\ T9%(I%)=8%*I% FOR I%=1% TO 16% &
	\ T9%(I%)=0% FOR I%=17% TO 33% &
	\ T9%(0%)=16% &
	\ T0=TIME(1) &
	\ KILL FOOTNOT.FIL$ &
		! SET UP 'PARAGRAPH' AND TAB DEFAULTS. &
		! SAVE TIME - GET RID OF FOOTNOTE FILE. &

710	LINES.LEFT.ON.PAG%=LINES.LEFT.ON.PAG%-1% &
	\ IF PAUS% THEN &
		PRINT #F2% &
	ELSE	PRINT #F2%,XLATE(CHR$(12%)+CHR$(7%)+CHR$(7%),X9$); &
	\	INPUT #F2%,PAUS% &

1000	I6$="" &
	\ GOSUB 10000 &
	\ IF ASCII(I$)=FLGS%(1%) THEN &
		GOTO 5000 &
	ELSE	IF E9% THEN GOTO 1030 &
	ELSE	IF AUTOPAR%=0% OR ASCII(I$)<>0% AND ASCII(I$)<>32% GOTO 1030 &
	ELSE	IF I$="" THEN &
		GOSUB 10000 &
	\	IF ASCII(I$)=FLGS%(1%) THEN &
			GOTO 5000 &

1020	Z9%=-1% &
	\ I7$=CVT$$(I$,8%) &
	\ GOTO 6620 &

1030	L$=FNT$(L$+FNO$(I$,FILLL%)) &
	\ IF L$<>"" AND &
	(UNDERSCR%=0% OR UNDERSCR.SP%<>0% OR &
	(UNDERSCR%<>0% AND UNDERSCR.SP%=0% AND U7%=0%)) THEN &
		L$=L$+" " UNLESS RIGHT(L$,LEN(L$))=" " &
	\	IF PERIOD% THEN &
			L$=L$+" " &
			IF INSTR(1%,".:;!?",MID(L$,LEN(L$)-1%,1%)) &

1040	GOTO 1000 IF (L$="" AND NOT E9%) OR (FILLL% AND LEN(L$)<=RGT.MRG%) &

1050	GOTO 1060 IF ASCII(MID(L$,L%,1%))=32% &
			FOR L%=RGT.MRG%+1% TO 1% STEP -1% &
		! STARTING FROM THE RIGHT, BACK UP TILL WE FIND A SPACE. &

1060	R%=L%+1% &
	\ L%=L%-1% &
	\ R%=R%+1% WHILE ASCII(MID(L$,R%,1%))=32% &
	\ R$=RIGHT(L$,R%) &
	\ L%=L%-1% WHILE L%>2% AND ASCII(MID(L$,L%,1%))=32% &
	\ L$=FNJ$(LEFT(L$,L%)) &
	\ GOSUB 10400 &
	\ IF NOT E9% THEN &
		GOTO 1040 &
	ELSE	IF F1%=3% THEN &
			GOTO 10730 &

3000	GOSUB 10400 &
	\ GOSUB 10500 IF LINES.LEFT.ON.PAG%<PAGE.SIZ%-HDR% &
	\ CLOSE F1%,F2%,4% &
	\ PRINT OUTFILE$;" created,";PAG.CREATED%;"pages" &
	\ PRINT (TIME(1)-T0)/10;"sec. CPU time used" &
		! DONE WITH THIS FILE &

3020	IF QUE%=0% THEN &
		GOTO 620 &
	ELSE	X$=SYS(CHR$(8%)+"$RUNOFF"+CHR$(13%)+CVT%$(3030%) &
		+"Q"+L0$+"="+CVT$$(OUTFILE$,-2%)+"/NH") &
	\	CHAIN "$QUE" LINE 31000 &
	\	STOP &
		! IF NO QUEUEING REQUESTED THEN CONTINUE &
		! ELSE SET UP AND CHAIN TO QUE &

3030	ON ERROR GOTO 19000 &
	\ E0%=-1% &
	\ E0$=SYS(CHR$(7%)) &
	\ GOTO 600 IF ASCII(E0$)=0% &
	\ PRINT IF CCPOS(0%)<>0% &
	\ PRINT "?Error("+NUM1$(ASCII(E0$))+") in QUE request - " &
		+RIGHT(E0$,2%) &
	\ PRINT &
	\ GO TO 600 &
		! RE-ENTRY POINT AFTER GOING TO 'QUE' &
		! CHECK FOR AND PROCESS POSSIBLE ERROR RETURN &

4000	VF$=SYS(CHR$(6%)+CHR$(-10%)+CVT$$(OUTFILE$,-2%)) &
		! CONVERT$$ THE OUTPUT FILE NAME, AND FSS IT &
	\ VFN$=MID(VF$,5%,8%) &
	\ VFD$=MID(VF$,23%,4%) &
		! PULL OUT THE GOOD PARTS &
	\ V$=	CHR$(6%)+CHR$(-28%)	! OLD SPOOL SYS CALL &
		+CVT%$(SWAP%(0%))	! ZERO &
		+VFN$			! FILE NAME &
		+STRING$(4%,0%)		! SPOOL DEVICE NAME &
		+CVT%$(SWAP%(0%))	! ZERO &
		+CVT%$(SWAP%(		! FLAGS &
				16384%	!	PBS SPOOLER &
			OR 	32% ))	!	/NH (NO FLAG_PAGES) &
		+CVT%$(SWAP%(0%))	! ZERO &
		+VFD$			! PRINT FILE DEVICE NAME &
		+STRING$(4%,0%)		! ZERO &

4110	PRINT "%OPSER Spooling Package not available," &
	\	PRINT "Queueing ";CVT$$(OUTFILE$,-2%);"/NOFLAG_PAGES" &
	\	PRINT " to Print/Batch Services..." &

4120	RETRY.COUNT%=RETRY.COUNT%+1% &
		! INCREMENT THE RETRY COUNT &
	\ V1$=SYS(V$) &
		! TRY THE SPOOL SYS CALL &
	\ E0%=-1% &
		! TELLS RUNOFF NOT TO PRINT THE HERALD LINE AGAIN &
	\ GOTO 600 &
		! DO IT ALL AGAIN &
	&

5000	C9%=1% &
	\ I%=INSTR(3%,I$+";",";") &
	\ J%=INSTR(2%,I$,CHR$(FLGS%(1%))) &
	\ I%=J% IF J% AND J%<I% &
	\ IF I% THEN &
		I7$=RIGHT(I$,I%) &
	\	I$=LEFT(I$,I%-1%) &
		! .COMMAND; OR .COMMAND.COMMAND &

5010	I%=INSTR(3%,I$,"!") &
	\ IF I% THEN &
		I6$=RIGHT(I$,I%) &
	\	I$=LEFT(I$,I%-1%) &
		! COMMENT &

5020	C%=FNN% &
	\ IF C%=0% OR C%=32% THEN &
		GOTO 5210 &
	ELSE	IF C%=59% OR C%=33% THEN &
			GOTO 1000 &
		ELSE	IF C%<>65% THEN &
				GOTO 5100 &
			ELSE	IF FNC%("AP") THEN &
				IF FNC%("AUTOPARAGRAPH") GOTO 8500
5030	AUTOPAR%=-1% &
	\ GOTO 1000 &
	&

5100	IF C%<>66% GOTO 5300 ELSE &
	IF FNC%("BLANK") GOTO 5200 &
		! COMMAND BEGINNING WITH 'B' &

5110	GOSUB 10400 &
	\ ARG%=1% IF Z9% &
	\ IF ARG%<1% GOTO 8020 &
	ELSE	GOSUB 10500 IF LINES.LEFT.ON.PAG%<0% &
	\	IF LINES.LEFT.ON.PAG%<ARG%+2% GOTO 6810 &
		ELSE	PRINT #F2% FOR I%=1% TO ARG% &
	\		LINES.LEFT.ON.PAG%=LINES.LEFT.ON.PAG%-ARG% &
	\		GOTO 1000 &

5200	IF FNC%("BREAK") GOTO 8500
5210	GOSUB 10400 &
	\ GOTO 1000 &
	&

5300	IF C%<>67% GOTO 5400 ELSE &
	IF FNC%("CENTER") THEN IF FNC%("CENTRE") GOTO 5350
5310	GOSUB 10400 &
	\ L$="" &
	\ ARG%=ARG%+W9% IF Z9% OR INSTR(C9%,I$,"+") OR INSTR(C9%,I$,"-") &
	\ GOSUB 10000 &
	\ L$=CVT$$(FNO$(I$,0%),128%) &
	\ I%=(ARG%-LEN(L$))/2% &
	\ L$=SPACE$(I%)+L$ &
	\ GOTO 5210 UNLESS D%(0%)
5320	D%(X%+I%)=D%(X%) FOR X%=D%(0%) TO 1% STEP -1% &
	\ D%(X%)=32% FOR X%=1% TO I% &
	\ D%(0%)=D%(0%)+I% &
	\ GOTO 5210 &

5350	IF FNC%("COMMENT") GOTO 8500
5360	I7$="" &
	\ GOTO 1000 &
	&

5400	IF C%<>68% GOTO 5450 ELSE &
	IF FNC%("DATE") GOTO 8500
5410	DAT%=-1% &
	\ GOTO 1000 &
	&

5450	IF C%<>69% GOTO 5500 ELSE &
	IF FNC%("END SUBPAGE") GOTO 8500
5460	PRINT "Unimplemented Feature" &
	\ GOTO 8500 &

5500	GOTO 5750 UNLESS C%=70% &
	\ IF FNC%("FILL") THEN GOTO 5550 &
		!	F &

5510	FILLL%=-1% &
	\ J9%=J7% &
	\ GOTO 5210 &

5550	IF FNC%("FG") THEN IF FNC%("FIGURE") GOTO 5600
5560	GOSUB 10400 &
	\ ARG%=1% IF Z9% &
	\ IF ARG%<1% THEN &
		GOTO 8020 &
	ELSE	GOSUB 10500 IF LINES.LEFT.ON.PAG%<ARG%+2% &
	\	PRINT #F2% FOR I%=1% TO ARG% &
	\	LINES.LEFT.ON.PAG%=LINES.LEFT.ON.PAG%-ARG% &
	\	GOTO 1000 &

5600	IF FNC%("FIRST TITLE") GOTO 5650
5610	IF T1% THEN &
		GOTO 8200 &
	ELSE	T0%=-1% &
	\	LINES.LEFT.ON.PAG%=-ABS(LINES.LEFT.ON.PAG%) &
	\	GOTO 1000 &

5650	IF FNC%("FLAGS") GOTO 5700
5660	IF FNFLAGS% THEN &
		GOTO 8020 &
	ELSE	X%=ASCII(CVT$$(RIGHT(FNI$,C9%),8%)) AND 127% &
	\	IF X% THEN &
			IF B%<>E% GOTO 8020 &
			ELSE	SVFLGS%(B%)=X%
5670	FLGS%(X%)=SVFLGS%(X%) FOR X%=B% TO E%
5675	IF E%>7% THEN &
		FLGS%(X%)=ASCII(MID(".!^\&#_<>%",X%,1%)) &
			IF FLGS%(X%)=0% FOR X%=B% TO E%
5680	CHANGE FLGS% TO FLAGS.IN.EFFECT$ &
	\ GOTO 1000 &

5700	IF FNC%("FN") THEN IF FNC%("FOOTNOTE") GOTO 8500
5710	IF Z9% THEN &
		GOTO 8020 &
	ELSE	IF F1%=3% GOTO 8040 &
		ELSE	U%=ARG%*S9% &
	\		GOSUB 10500 IF LINES.LEFT.ON.PAG%<=U% &
	\		LINES.LEFT.ON.PAG%=LINES.LEFT.ON.PAG%-U% &
	\		U1%=CASE% UNLESS FOOTNOT.LINES% &
	\		FOOTNOT.LINES%=FOOTNOT.LINES%+U% &
	\		X%=0% &
	\		OPEN FOOTNOT.FIL$ AS FILE 3%, MODE 2% &

5720	GOSUB 10000 &
	\ IF ASCII(I$)<>FLGS%(2%) THEN &
		PRINT #3%, I$ &
	\	X%=X%+1% &
	\	GOTO 5720 IF X%<=U%+60% &
	\	PRINT 'Page';ABS(PAG.NO%);", Missing Footnote Terminator (!)"
5730	CLOSE 3% &
	\ GOTO 1000 &

5750	GOTO 5800 UNLESS C%=72 &
	\ IF FNC%("HD") THEN &
		IF FNC%("HEADER") THEN &
			GOTO 8500 &
		!	H &

5760	P$="Page" &
	\ O$=CVT$$(RIGHT(I$,C9%),34%) &
	\ IF INSTR(1%,O$,"MIXED") THEN &
		GOTO 5780 &
	ELSE	IF INSTR(1%,O$,"UPPER") THEN &
			P$=CVT$$(P$,32%) &
	\		GOTO 5780 &

5770	IF INSTR(1%,O$,"LOWER") THEN &
		P$="page" &
	ELSE	IF O$<>"" GOTO 8500 &

5780	PAG.HDR$=P$ &
	\ HDR%=5% &
	\ GOTO 1000 &
	&

5800	GOTO 6000 UNLESS C%=73% &
	\ IF FNC%("INDENT") THEN GOTO 5900 &
		!	I &

5810	GOSUB 10400 &
	\ IF Z9% THEN &
		GOTO 8020 &
	ELSE	INDENT%=LFT.MRG%+ARG% &
	\	IF INDENT%<0% GOTO 8010 &
		ELSE	L$=SPACE$(INDENT%) &
	\		D%(0%) = 0% &
	\		GOTO 1000
5900	IF FNC%("INDEX") GOTO 8500
5910	U%=CASE% &
	\ I9$=CVT$$(FNO$(RIGHT(FNI$,C9%),0%),136%) &
	\ CASE%=U% &
	\ IF I9$="" THEN &
		GOTO 8200 &
	ELSE	GOSUB 10800 &
	\	GOTO 1000 &

6000	GOTO 6100 UNLESS C%=74% &
	\ IF FNC%("JUSTIFY") THEN GOTO 8500 &

6010	J7%,J9%=-1% &
	\ GOTO 5210 &
	&

6100	GOTO 6200 UNLESS C%=76% &
	\ IF FNC%("LEFT MARGIN") GOTO 6150 &
		!	L &

6110	GOSUB 10400 &
	\ IF Z9% THEN &
		GOTO 8020 &
	ELSE	L6%=LFT.MRG% &
	\ 	LFT.MRG%=0% UNLESS INSTR(C9%,I$,"+")+INSTR(C9%,I$,"-") &
	\ 	LFT.MRG%=LFT.MRG%+ARG% &
	\	IF LFT.MRG%<0% OR RGT.MRG%<=LFT.MRG% THEN &
			LFT.MRG%=L6% &
	\		GOTO 8020
6120	INDENT%=LFT.MRG% &
	\ D%(0%)=0% &
	\ L$=SPACE$(LFT.MRG%) &
	\ L1%=LFT.MRG% UNLESS T1% &
	\ GOTO 1000 &

6150	IF FNC%("LOWER CASE") GOTO 8500
6160	CASE%=0% &
	\ GOTO 1000 &
	&

6200	GOTO 6600 UNLESS C%=78% &
	\ IF FNC%("NAP") THEN &
		IF FNC%("NO AUTOPARAGRAPH") THEN GOTO 6225 &
		!	N &

6210	AUTOPAR%=0% &
	\ GOTO 1000 &

6225	IF FNC%("NO DATE") GOTO 6250
6230	DAT%=0% &
	\ GOTO 1000 &

6250	IF FNC%("NO FILL") GOTO 6300
6260	FILLL%,J9%=0% &
	\ GOTO 5210 &

6300	IF FNC%("NO FLAGS") GOTO 6350
6310	GOTO 8020 IF FNFLAGS% &
	\ FOR X%=B% TO E% &
	\ 	SVFLGS%(X%)=FLGS%(X%) IF FLGS%(X%) &
	\ FLGS%(X%)=0% &
	\ NEXT X% &
	\ GOTO 5680 &

6350	IF FNC%("NHD") THEN IF FNC%("NO HEADER") GOTO 6400
6360	HDR%=1% &
	\ GOTO 1000 &

6400	IF FNC%("NO JUSTIFY") GOTO 6450
6410	J7%,J9%=0% &
	\ GOTO 5210 &

6450	IF FNC%("NNM") THEN IF FNC%("NO NUMBER") GOTO 6500
6460	PAG.NO%=-ABS(PAG.NO%) &
	\ GOTO 1000 &

6500	IF FNC%("NPR") THEN IF FNC%("NO PERIOD") GOTO 6550
6510	PERIOD%=0% &
	\ GOTO 1000 &

6550	IF FNC%("NM") THEN IF FNC%("NUMBER") GOTO 8500
6560	ARG%=ARG%+ABS(PAG.NO%) &
		IF Z9% OR INSTR(C9%,I$,"+") OR INSTR(C9%,I$,"-") &
	\ PAG.NO%=ABS(ARG%) &
	\ GOTO 1000 &
	&

6600	IF C%<>80% GOTO 7000 ELSE &
	IF FNC%("PARAGRAPH") GOTO 6700
6610	O$=","+CVT$$(RIGHT(I$,C9%),24%) &
	\ K%=1% &
	\ FOR X%=0% TO 2% &
	\	X$=FNF$(O$,K%) &
	\	K%=K%+LEN(X$)+1% &
	\	IF X$="" THEN &
			T8%(X%)=32767% &
		ELSE	T8%(X%)=VAL(X$)
6615	NEXT X% &
	\ IF T8%(0%)<32767% AND T8%(0%)+LFT.MRG%<0% THEN &
		GOTO 8010 &
	ELSE	IF T8%(1%)<32767% AND ABS(T8%(1%)-2%)>3% &
		OR T8%(1%)=0% OR T8%(2%)<0% GOTO 8020
6616	T8%(1%)=0% IF T8%(1%)<0% &
	\ PAR%(X%)=T8%(X%) UNLESS T8%(X%)=32767% FOR X%=0% TO 2%
6620	GOSUB 10400 &
	\ N%=PAR%(1%)-1% &
	\ N%=(S9%+1%)/2% IF N%<0% &
	\ IF LINES.LEFT.ON.PAG%<=N% THEN &
		N%=0% &
	\	GOSUB 10500
6630	PRINT #F2% FOR I%=1% TO N% &
	\ LINES.LEFT.ON.PAG%=LINES.LEFT.ON.PAG%-N% &
	\ IF PAR%(0%) THEN &
		X$=CHR$(FLGS%(1%))+"I "+NUM$(PAR%(0%)) &
	\	X$=X$+";" UNLESS ASCII(I7$)=59% OR I7$="" &
	\	I7$=X$+I7$ &

6640	IF PAR%(2%) THEN &
		X$=CHR$(FLGS%(1%))+"TP "+NUM$(PAR%(2%)) &
	\	X$=X$+";" UNLESS ASCII(I7$)=59% OR I7$="" &
	\	I7$=X$+I7$ &

6645	D9%=-1% &
	\ GOTO 1000 &

6700	IF FNC%("PAGE SIZE") THEN IF FNC%("PAPER SIZE") GOTO 6800
6710	GOSUB 10400 &
	\ O$=","+RIGHT(I$,C9%) &
	\ IF O$="," THEN &
		GOTO 8020 &
	ELSE	X$=FNF$(O$,1%) &
	\	ARG%=VAL(X$) &
	\ 	O$=RIGHT(O$,LEN(X$)+2%) &
	\ 	IF ARG%<10% GOTO 8020
6720	IF O$<>"" THEN &
		N%=VAL(FNF$(O$,1%)) &
	\	IF N%<=LFT.MRG% GOTO 8020 &
		ELSE	RGT.MRG%,W9%=N% &
	\		R1%,W1%=RGT.MRG% UNLESS T1% &

6730	GOSUB 10500 IF LINES.LEFT.ON.PAG%<0% &
	\ LINES.LEFT.ON.PAG%=LINES.LEFT.ON.PAG%+ARG%-PAGE.SIZ% &
	\ PAGE.SIZ%=ARG% &
	\ GOTO 1000 &

6800	IF FNC%("PG") THEN IF FNC%("PAGE") GOTO 6850
6810	GOSUB 10400 &
	\ LINES.LEFT.ON.PAG%=-ABS(LINES.LEFT.ON.PAG%) &
		IF LINES.LEFT.ON.PAG%<PAGE.SIZ%-HDR% &
	\ GOTO 1000 &

6850	IF FNC%("PR") THEN IF FNC%("PERIOD") GOTO 6900
6860	PERIOD%=-1% &
	\ GOTO 1000 &

6900	IF FNC%("PX") THEN IF FNC%("PRINT INDEX") GOTO 8500
6910	IF F1%=3% THEN &
		GOTO 8040 &
	ELSE	GOSUB 10400 &
	\	GOTO 1000 UNLESS I9% &
	\	I9$(0%)=CVT%$(0%)+CVT%$(I9%-7%)+CHR$(1%)+CHR$(8%)+ &
		CVT%$(1%)+CVT%$(64%)+CVT%$(64%)+ &
		INDX.FIL$+SPACE$(13%)+ &
		CVT%$(1%+1000%*1%)+CVT%$(62%)
6920	P7%=P7%+1% &
	\ GOSUB 10000 &
	\ P7%=0% IF E9% &

6930	C%(1%)=AUTOPAR% &
	\ C%(2%)=INDENT% &
	\ C%(3%)=UNDERSCR.BS.LF% &
	\ C%(4%)=UNDERSCR.CHAR% &
	\ C%(5%+I%)=FLGS%(I%) FOR I%=0% TO 10% &
	\ C%(16%)=SIM.FF% &
	\ C%(17%)=FILLL% &
	\ C%(18%+I%)=SVFLGS%(I%) FOR I%=0% TO 10% &
	\ C%(29%+I%)=PAR%(I%) FOR I%=0% TO 2% &
	\ C%(32%)=HDR% &
	\ C%(33%)=PAGE.SIZ% &
	\ C%(34%)=J7% &
	\ C%(35%)=J9% &
	\ C%(36%)=L1% &
	\ C%(37%)=LFT.MRG% &
	\ C%(38%)=PAG.CREATED% &
	\ C%(39%)=PERIOD% &
	\ C%(40%)=P7% &
	\ C%(41%)=PAUS% &
	\ C%(42%)=QUE% &
	\ C%(43%)=R1% &
	\ C%(44%)=RGT.MRG% &
	\ C%(45%)=S1% &
	\ C%(46%)=S9% &
	\ C%(47%)=T1% &
	\ C%(48%)=UNDERSCR% &
	\ C%(49%)=U1% &
	\ C%(50%)=U7% &
	\ C%(51%)=CASE% &
	\ C%(52%)=FOOTNOT.LINES% &
	\ C%(53%)=W1% &
	\ C%(54%)=W9% &
	\ C%(55%)=X9% &
	\ C%(56%)=UNDERSCR.SP% &
	\ C%(57%)=DAT% &
	\ C%(58%)=FORM.SIZ% &
	\ C%(0%)=58% &
	\ CHANGE C% TO I9$ &
	\ I9$(1%)=I9$ &
	\ C%(1%+I%)=T9%(I%) FOR I%=0% TO 33% &
	\ C%(0%)=34% &
	\ CHANGE C% TO I9$ &
	\ I9$(2%)=I9$+ &
		CVT%$(LINES.LEFT.ON.PAG%)+CVT%$(PAG.NO%)+CVTF$(T0)+CHR$(13%)+L0$ &
	\ I9$(3%)=INFILE$+CHR$(13%)+OUTFILE$+CHR$(13%)+DAY$+CHR$(13%)+PAG.HDR$ &
	\ I9$(4%)=SUBTTL$ &
	\ I9$(5%)=TITLE$ &
	\ I9$(6%)=LEFT(X9$,64%) &
	\ I9$(7%)=MID(X9$,65%,64%) &
	\ CLOSE 4% &
	\ GOTO 31000 &
		! STORE ALL VARIABLES FOR CALL TO SORT PROGRAM &

7000	GOTO 7100 UNLESS C%=82% &
	\ IF FNC%("RIGHT MARGIN") THEN GOTO 8500 &
		!	R &

7010	IF Z9% THEN &
		GOTO 8020 &
	ELSE	R6%=RGT.MRG% &
	\	RGT.MRG%=0% UNLESS INSTR(C9%,I$,"+") OR INSTR(C9%,I$,"-") &
	\	RGT.MRG%=RGT.MRG%+ARG% &
	\	W9%=RGT.MRG% UNLESS RGT.MRG%<=W9% &
	\	IF RGT.MRG%<=LFT.MRG% THEN &
			RGT.MRG%=R6% &
	\		GOTO 8020 &

7020	IF T1% THEN &
		GOTO 5210 &
	ELSE	R1%=RGT.MRG% &
	\	W1%=W9% &
	\	GOTO 5210 &
	&

7100	GOTO 7500 UNLESS C%=83% &
	\ IF FNC%("SKIP") THEN GOTO 7200 &
		!	S &

7110	GOSUB 10400 &
	\ ARG%=1% IF Z9% &
	\ GOSUB 10500 IF LINES.LEFT.ON.PAG%<0% &
	\ IF LINES.LEFT.ON.PAG%<(ARG%+2%)*S9% GOTO 6810
7120	FOR X%=1% TO ARG% &
	\	L$=CHR$(160%) &
	\	GOSUB 10400 &
	\ NEXT X% &
	\ GOTO 1000 &

7200	IF FNC%("SPACING") GOTO 7300
7210	GOSUB 10400 &
	\ IF Z9% THEN &
		GOTO 8020 &
	ELSE	IF ARG%<1% OR 5%<ARG% GOTO 8020 &
		ELSE	S9%=ARG% &
	\		S1%=S9% UNLESS T1% &
	\		GOTO 1000 &

7300	IF FNC%("SPG") THEN IF FNC%("SUB PAGE") GOTO 7400
7310	PRINT "Unimplemented Feature" &
	\ GOTO 8500 &

7400	IF FNC%("ST") THEN &
	IF FNC%("SUBTTL") THEN IF FNC%("SUBTITLE") GOTO 8500
7410	SUBTTL$=CVT$$(FNO$(RIGHT(FNI$,C9%),0%),136%) &
	\ GOTO 1000 &
	&

7500	IF C%<>84% GOTO 7800 ELSE &
	IF FNC%("TITLE") GOTO 7600
7510	TITLE$=CVT$$(FNO$(RIGHT(FNI$,C9%),0%),136%) &
	\ GOTO 1000 &

7600	IF FNC%("TAB STOPS") GOTO 7700
7610	O$=","+CVT$$(RIGHT(I$,C9%),16%) &
	\ K%=1% &
	\ T8%(0%)=0% &
	\ FOR X%=1% TO 33%
7620		IF K%>LEN(O$) THEN &
			GOTO 7640 &
		ELSE	X$=FNF$(O$,K%) &
	\		K%=K%+LEN(X$)+1% &
	\		IF X$="" THEN &
				T8%(X%)=T9%(X%) &
			ELSE	T8%(X%)=VAL(X$)
7630		GOTO 8020 UNLESS T8%(X%)>T8%(X%-1%) &
	\ NEXT X% &
	\ GOTO 8030
7640	T8%(0%)=X%-1% &
	\ T9%(X%)=0% FOR X%=X% TO 33% &
	\ T9%(X%)=T8%(X%) FOR X%=0% TO T8%(0%) &
	\ GOTO 1000 &

7700	IF FNC%("TEST PAGE") GOTO 8500
7710	GOSUB 10400 &
	\ IF Z9% GOTO 8020 &
	ELSE	IF LINES.LEFT.ON.PAG%<ARG% GOTO 6810 &
		ELSE	1000 &
	&

7800	IF C%<>85% GOTO 7900 ELSE &
	IF FNC%("UPPER CASE") GOTO 8500
7810	CASE%=-1% &
	\ GOTO 1000 &
	&

7900	IF C%<>88% GOTO 8500 ELSE &
	IF FNC%("X") GOTO 8500 ELSE 5910 &

8010	PRINT "Negative Indent Attempted" &
	\ INDENT%=LFT.MRG% &
	\ GOTO 8200 &

8020	PRINT "Missing or Invalid Argument" &
	\ GOTO 8200 &

8030	PRINT "Too Many Tab Stops" &
	\ GOTO 8200 &

8040	PRINT "Invalid Inside Footnote" &

8200	PRINT 'Page';ABS(PAG.NO%);', Invalid Command: "';I$;'"' &
	\ GOTO 1000 &

8500	PRINT 'Page';ABS(PAG.NO%);', Unrecognized Command: "';I$;'"' &
	\ GOTO 1000 &

10000	! &
	&
	&
	!	S U B R O U T I N E S &
	&
	&
	&
	!	I N P U T    R O U T I N E &
	&

10010	IF I7$<>"" THEN &
		I$=I7$ &
	ELSE	INPUT LINE #F1%, I1$ &
	\	I$=CVT$$(I1$,4%) &

10020	I7$="" &
	\ I$=RIGHT(I$,2%) IF ASCII(I$)=59% &

10030	RETURN &

10100	FIL$=I$ &
	\ CHANGE SYS(CHR$(6%)+CHR$(-23%)+I$) TO Z% &
	\ IF RECOUNT=0% THEN &
		GOTO 10150 &
	ELSE	FIL$=LEFT(I$,LEN(I$)-RECOUNT) &
	\	I$=CVT$$(RIGHT(I$,LEN(I$)-RECOUNT+1%),-2%) &
	\	X9%=X9% OR FNS%("CONVERT") &
	\	X9$=FNCONVERT$(O$) IF O$<>"" &
	\	QUE%=QUE% OR FNS%("QUE") &
	\	L0$=O$ IF O$<>"" &
	\	I%=INSTR(1%,I$,"/") &
	\	GOTO 10150 UNLESS I% &
	\	L0$=L0$+RIGHT(I$,I%) &
	\	I$=LEFT(I$,I%-1%) &
		! SAVE USER INPUT AS INPUT OR OUTPUT SPEC. &
		! SCAN TO SEE IF ANY NON-FILE SPEC SWITCHES SPECIFIED &
		! IF NOT THEN &
		!	GET OUT &
		! ELSE	PEEL OFF THE FILENAME. &
		!	PEEL OFF SWITCHES. &
		!	SCAN FOR "CONVERT" - HANDLE CONVERT FILE &
		!	SCAN FOR "QUE" - STORE PRINTER NAME - IF ANY &
		!	 MORE SWITCHES FOLLOW, THEY MUST BE QUE MODIFIERS &
		!	 SO TACK THEM ON. &

10150	FIL$="_KB:RUNOFF" UNLESS LEN(FIL$) &
	\ RETURN &
		! SET UP DEFAULT DEV:FILNAM IF NONE SPECIFIED. &

10200	FOR I%=1%+W9%/2%*2%-W9% TO W9% STEP 2% &
	\	C%(I%)=46% &
	\	C%(I%+1%)=32% &
	\ NEXT I% &
	\ C%(0%)=W9% &
	\ CHANGE C% TO D9$ &
	\ RETURN &

10300	X$=SYS(CHR$(6%)+CHR$(-21%)) &
	\ FOOTNOT.FIL$="RNFF" &
	+RIGHT(NUM1$(100%+ASCII(SYS(CHR$(6%)+CHR$(9%)))/2%),2%)+".TMP" &
	\ INDX.FIL$="RNFI"+RIGHT(FOOTNOT.FIL$,5%) &
	\ C%(X%),C%(X%+128%)=X%-1% FOR X%=1% TO 128% &
	\ C%(0%)=256% &
	\ CHANGE C% TO X9$ &
	\ FLAGS.IN.EFFECT$=".!^\&#_<>%" &
	\ CHANGE FLAGS.IN.EFFECT$ TO FLGS% &
	\ CHANGE FLAGS.IN.EFFECT$ TO SVFLGS% &
	\ FLGS%(X%)=0% FOR X%=8% TO 10% &
	\ CHANGE FLGS% TO FLAGS.IN.EFFECT$ &
		! DROP TEMPORARY PRIVILEGES. &
		! SET UP FOOTNOTE FILE NAME, CHARACTER ARRAYS, AND FLAGS &
		! ARRAYS. &

10320	RETURN &

10400	CHANGE D% TO X$ &
	\ IF L$="" AND X$="" THEN &
		RETURN &
	ELSE	T1%=-1% &
	\	IF R% THEN &
			CHANGE SPACE$(LFT.MRG%)+RIGHT(X$,R%) TO D% IF D%(0%) &
		ELSE	D%(0%)=0% &
	\		R%=LEN(L$)+1%
10410	X$=CVT$$(LEFT(X$,R%-1%),128%) &
	\ L$=LEFT(L$,LEN(X$)) &
			+CVT$$(RIGHT(L$,LEN(X$)+1%),128%) &
	\ X$=LEFT(X$,LEN(X$)-1%) WHILE LEN(X$)>LEN(L$) &
	\ GOSUB 10500 IF LINES.LEFT.ON.PAG%<S9%-(UNDERSCR.BS.LF%=10% AND X$<>"") &

10420	PRINT #F2% FOR I%=2% TO S9% &
	\ LINES.LEFT.ON.PAG%=LINES.LEFT.ON.PAG%-S9% &
	\ IF UNDERSCR.BS.LF%<>8% OR X$="" THEN 10440 &
	ELSE	I%=LEN(X$) &

10430	IF I% THEN &
		L$=LEFT(L$,I%)+CHR$(UNDERSCR.BS.LF%)+ &
		MID(X$,I%,1%)+RIGHT(L$,I%+1%) &
			IF ASCII(MID(X$,I%,1%))<>32% &
	\	I%=I%-1% &
	\	GOTO 10430 &

10440	PRINT #F2%, XLATE(L$,X9$); &
	\ IF X$<>"" AND UNDERSCR.BS.LF%<>8% THEN &
		PRINT #F2%, XLATE(CHR$(13%)+CHR$(UNDERSCR.BS.LF%)+X$,X9$); &
	\ 	LINES.LEFT.ON.PAG%=LINES.LEFT.ON.PAG%-1% IF UNDERSCR.BS.LF% &

10450	PRINT #F2% &
	\ L$=SPACE$(LFT.MRG%)+R$ &
	\ R$="" &
	\ R%=0% &
	\ INDENT%=LFT.MRG% &
	\ RETURN &

10500	LINES.LEFT.ON.PAG%=ABS(LINES.LEFT.ON.PAG%) &
	\ IF T0% THEN &
		GOTO 10520 &
	ELSE	IF FOOTNOT.LINES% THEN &
			PRINT #F2% FOR I%=1% TO LINES.LEFT.ON.PAG% &
	\		LINES.LEFT.ON.PAG%=FOOTNOT.LINES% &
	\		GOTO 10700 &

10510	IF SIM.FF% THEN &
		PRINT #F2% FOR I%=1% TO LINES.LEFT.ON.PAG%+FORM.SIZ%-PAGE.SIZ% &
	ELSE	PRINT #F2%, XLATE(CHR$(12%),X9$); &

10520	IF PAUS% THEN &
		PRINT #F2%, XLATE(SPACE$(20%),X9$) UNLESS E9% &
	ELSE	IF NOT T0% THEN &
			PRINT #F2%, XLATE(CHR$(7%)+CHR$(7%),X9$); &
	\	INPUT #F2%, PAUS% &

10530	IF E9% THEN &
		IF LINES.LEFT.ON.PAG%<=2% THEN &
			GOTO 10540 &
		ELSE	RETURN &

10540	IF NOT T0% THEN &
		PAG.CREATED%=PAG.CREATED%+1% &
	\	PAG.NO%=SGN(PAG.NO%)*(ABS(PAG.NO%)+1%) &
	\	PAG.NO%=1% UNLESS PAG.NO% &

10550	IF HDR%=1% THEN &
		GOTO 10580 &
	ELSE	PRINT #F2%, XLATE(LEFT(TITLE$,W9%-10%),X9$); &
	\	IF PAG.NO%<0% THEN &
			PRINT #F2% &
		ELSE	P$=XLATE(CVT$$(PAG.HDR$+NUM$(ABS(PAG.NO%)),144%),X9$) &
	\	 	PRINT #F2%, TAB(W9%-LEN(P$));P$ &

10560	PRINT #F2%, XLATE(LEFT(SUBTTL$,W9%-10%),X9$); &
	\ IF PAG.NO%<0% THEN &
		PRINT #F2% &
	ELSE	IF DAT%=0% THEN &
			PRINT #F2% &
		ELSE	PRINT #F2%, TAB(W9%-LEN(DAY$));DAY$ &

10570	PRINT #F2% &
	\ PRINT #F2% &

10580	LINES.LEFT.ON.PAG%=PAGE.SIZ%-HDR%-FOOTNOT.LINES% &
	\ T0%=0% &
	\ RETURN &
	&

10700	B8%=INDENT% &
	\ L8%=LFT.MRG% &
	\ R8%=RGT.MRG% &
	\ S8%=S9% &
	\ E8%=E9% &
	\ I8$=I7$ &
	\ L8$=L$ &
	\ CHANGE D% TO D8$ &
	\ X8$=X$ &
	\ F8%=FILLL% &
	\ J8%=J9% &
	\ U8%=CASE% &
	\ N8%=ARG% &
	\ Z8%=Z9% &
	\ R8$=R$ &
	\ R7%=R% &
	\ FILLL%,J9%=-1% &
	\ INDENT%,LFT.MRG%=L1% &
	\ RGT.MRG%=R1% &
	\ S9%=S1% &
	\ W9%=W1% &
	\ CASE%=U1% &
	\ L$=SPACE$(LFT.MRG%) &
	\ I7$,R$="" &
	\ D%(0%),R%,FOOTNOT.LINES%,E9%=0%
10720	OPEN FOOTNOT.FIL$ FOR INPUT AS FILE 3% &
	\ KILL FOOTNOT.FIL$ &
	\ F1%=3% &
	\ GOTO 1000 &

10730	CLOSE 3% &
	\ F1%=1% &
	\ E9%=E8% &
	\ FILLL%=F8% &
	\ J9%=J8% &
	\ CASE%=U8% &
	\ LFT.MRG%=L8% &
	\ RGT.MRG%=R8% &
	\ INDENT%=B8% &
	\ S9%=S8% &
	\ ARG%=N8% &
	\ Z9%=Z8% &
	\ I7$=I8$ &
	\ L$=L8$ &
	\ R$=R8$ &
	\ R%=R7% &
	\ CHANGE D8$ TO D% &
	\ X$=X8$ &
	\ IF LINES.LEFT.ON.PAG%<PAGE.SIZ%-HDR% THEN &
		GOTO 10500 &
	ELSE	RETURN &

10800	GOTO 10810 IF I9% &
	\ OPEN INDX.FIL$ AS FILE 4% &
	\ S8$=SPACE$(64%) &
	\ I9%=7% &

10810	I9%=I9%+1% &
	\ I9$(I9%)=I9$+S8$ &
	\ I8$(I9%,31%)=CVT%$(ABS(PAG.NO%)-(LINES.LEFT.ON.PAG%<S9%)) &
	\ RETURN &

15000	! &
	&
	&
	!	F U N C T I O N S &
	&

15010	DEF* FNO$(I$,FL%) &
	\ CHANGE FNC1$(FNQ$(I$)+" ",FL%) TO C% &
	\ C%(0%)=C%(0%)-1% &
	\ GOTO 15230 UNLESS C%(0%)>0% &
	\ I5%,U3%,U4%,U5%,J%=0% &
	\ C%=32% &
	\ FOR I%=1% TO C%(0%) &
	\	D%=C%(I%) &
	\	K%=INSTR(3%,FLAGS.IN.EFFECT$,CHR$(C%)) &
	\	GOTO 15140 UNLESS K% &
	\ 	ON K%-2% &
		GOTO 15030, 15040, 15050, 15140, 15140, 15060, 15070, 15080
15030		D%=D%+1024% &
	\	GOTO 15090 &
		!			^ &

15040		D%=D%+2048% &
	\	GOTO 15090 &
		!			\ &

15050		U4%=-1% &
	\	GOTO 15140 &
		!			AMPERSAND &

15060		U6%=CASE% &
	\	U5%,CASE%=-1% &
	\	GOTO 15140 &
		!			< &

15070		I5%=-1% &
	\	I6%=J% &
	\	GOTO 15140 &
		!			> &

15080		U3%=-1% &
	\	GOTO 15140 &
		!			% &

15090		C%=(D% AND 127%) OR 128% &
	\	IF D%=FLGS%(3%)+1024% THEN &
			U6%,CASE%=-1% &
	\		GOTO 15190 &
		!			^^ &

15110		IF D%=FLGS%(4%)+2048% THEN &
			U6%,CASE%=0% &
	\		GOTO 15190 &
		!			\ &

15120		IF D%=FLGS%(5%)+1024% THEN &
			U7%=-1% &
	\		GOTO 15190 &
		!			^AMPERSAND &

15130		IF D%=FLGS%(5%)+2048% THEN &
			U7%=0% &
	\		GOTO 15190 &
		!			\ &

15140		C%=D% AND 255% &
	\	IF C%=FLGS%(6%) THEN &
			D%=160% &
		ELSE	IF INSTR(3%,FLAGS.IN.EFFECT$,CHR$(C%)) GOTO 15190 &
		!			# AND ^\AMPERSAND<>% &

15150		IF (C%=32% OR C%=255%) AND UNDERSCR.SP% THEN &
			GOTO 15180 &
		ELSE	IF UNDERSCR% AND (U7% OR U4%) AND (NOT U3%) THEN &
				D%(X%)=32% FOR X%=D%(0%)+1% TO LEN(L$)+J%+1% &
	\		 	D%(X%)=UNDERSCR.CHAR% &
	\			D%(0%)=X%
15160		IF 64%<C% AND C%<91% AND ((D% AND 2048%) OR &
		(CASE%=0% AND ((D% AND 1024%)=0%))) THEN &
			D%=D%+32% &
		!	L O W E R    C A S E &

15170		IF U3% THEN &
			U3%=0% &
	\		D%(X%)=32% FOR X%=D%(0%)+1% TO LEN(L$)+J% &
	\		D%(X%)=D% &
	\		D%(0%)=X% &
	\		GOTO 15190 &

15180		U4%=0% &
	\	J%=J%+1% &
	\	C%(J%),C%=D% AND 255% &

15190		IF I%<C%(0%) THEN &
			IF C%<>FLGS%(9%) THEN &
				GOTO 15220 UNLESS C%=32% OR C%=255% OR C%=160% &
			ELSE	C%=C% OR 128% IF I5% &
	\			GOTO 15210 &

15200		IF U5% THEN &
			U5%=0% &
	\		CASE%=U6% &

15210		IF I5% THEN &
			I5%=0% &
	\		I9$="" &
	\		I9$=I9$+CHR$(C%(X%)) FOR X%=I6%+1% TO J% &
	\		I9$=CVT$$(I9$,140%) &
	\		GOSUB 10800
15220	NEXT I% &
	\ C%(0%)=J% &
	\ IF U7%<>0% AND UNDERSCR%<>0% AND UNDERSCR.SP%=0% THEN &
		D%(X%)=32% FOR X%=D%(0%)+1% TO LEN(L$)+J%+1% &
	\	D%(X%)=UNDERSCR.CHAR% &
	\	D%(0%)=X% &
	\	J%=J%+1% &
	\	C%(J%)=32% &
	\	C%(0%)=J% &
		! IF WE ARE IN THE MIDDLE OF AN UNDERLINING REQUEST (^&) &
		! AND ARE SUPPOSED TO UNDERLINE EMBEDDED SPACES THEN &
		!	TREAT THE LINE TERMINATOR AS A SPACE &
	&

15230	CHANGE C% TO I$ &
	\ FNO$=I$ &
	\ FNO$=CHR$(160%) UNLESS FL% OR I$<>"" &
	\ FNEND &

15300	DEF* FNT$(L$) &
	\ I%,J%=1%
15310	I%=INSTR(I%,L$,CHR$(-1%)) &
	\ GOTO 15340 UNLESS I% AND I%<=RGT.MRG% &
	\ GOTO 15320 IF I%<=T9%(X%) FOR X%=J% TO T9%(0%) &
	\ X%=33% &
	\ T9%(X%)=I%
15320	J%=X%+1% &
	\ L$=LEFT(L$,I%-1%)+STRING$(T9%(X%)-I%+1%,160%)+RIGHT(L$,I%+1%)
15330	IF D%(0%)<I% THEN &
		GOTO 15310 &
	ELSE	D%(D%+T9%(X%)-I%)=D%(D%) FOR D%=D%(0%) TO I% STEP -1% &
	\	D%(0%)=D%(0%)+T9%(X%)-I% &
	\	D%(D%)=D%(T9%(X%)) FOR D%=I% TO T9%(X%)-1% &
	\	GOTO 15310 &

15340	FNT$=L$ &
	\ FNEND &
	&

15400	DEF* FNJ$(L$) &
	\ IF J9%=0% OR LEN(L$)>=RGT.MRG% THEN &
		GOTO 15460 &
	ELSE	I%=INDENT% &
	\	X%=0% &

15410	I%=INSTR(I%+2%,L$,CHR$(32%)) &
	\ IF I% THEN &
		X%=X%+1% &
	\	C%(X%)=I% &
	\	GOTO 15410 &

15420	GOTO 15450 UNLESS X% &
	\ B%=1% &
	\ E%=X% &
	\ IF D9%<0% THEN &
		B%=X% &
	\	E%=1% &

15430	FOR I%=B% TO E% STEP D9% &
	\	L$=LEFT(L$,C%(I%)-1%)+CHR$(32%)+RIGHT(L$,C%(I%)) &
	\	IF D%(0%)>=C%(I%) THEN &
			D4%=D%(C%(I%)) &
	\		D%(D%+1%)=D%(D%) FOR D%=D%(0%) TO C%(I%) STEP -1% &
	\	 	R%=R%+1% &
	\		D%(0%)=D%(0%)+1% &
	\		D%(D%)=32% &
	\		D%(D%)=UNDERSCR.CHAR% IF D4%=UNDERSCR.CHAR% &
	\		D%(LEN(L$)+1%)=32%
15440		C%(J%)=C%(J%)+1% FOR J%=I% TO X% &
	\	GOTO 15460 IF LEN(L$)>=RGT.MRG% &
	\ NEXT I% &
	\ GOTO 15430 &

15450	PRINT "Can't Justify Line" &

15460	D9%=-D9% &
	\ FNJ$=L$ &
		! SET THE FUNCTION. &

15470	FNEND &

15600	DEF* FNC%(X$) &
	\ I%,FNC%=0% &
	\ C8%=C9% &
	\ FOR X%=2% TO LEN(X$)+1%
15610		C%=FNN% &
	\	IF C%<>ASCII(MID(X$,X%,1%)) THEN &
			X%=INSTR(X%,X$," ") &
	\		X%=LEN(X$)+1% UNLESS X% &
	\ 		IF C%<>32% AND C%<>9% THEN &
				X%=X%+1% &
	\ 			IF C%<>ASCII(MID(X$,X%,1%)) THEN &
					I%,FNC%=-1% &
	\				X%=LEN(X$)+1% &
	\				C9%=C8%
15620	NEXT X% &
	\ Z9%=-1% &
	\ IF I% OR C9%>=LEN(I$) THEN &
		ARG%=0% &
	ELSE	ARG%=VAL(RIGHT(I$,C9%+1%)) &
	\	Z9%=0%
15630	FNEND &
	&

15700	DEF* FNFLAGS% &
	\ FNFLAGS%=0% &
	\ I0$=CVT$$(RIGHT(I$,C9%),34%) &
	\ X$="" &
	\ RESTORE &
	\ READ X$ UNTIL X$="*FLAGS"
15710	READ X$,B%,E% &
	\ IF X$="" THEN &
		FNFLAGS%=LEN(I0$) &
	ELSE	IF LEFT(I0$,LEN(X$))<>X$ THEN &
			GOTO 15710 &
		ELSE	C9%=INSTR(C9%,CVT$$(I$,32%),X$)+LEN(X$) &

15720	FNEND &

15730	DATA *FLAGS, &
	ALL,3,10, &
	CONTROL,1,1,	ENDFOOTNOTE,2,2, &
	UPPERCASE,3,3,	LOWERCASE,4,4
15740	DATA &
	UNDERLINE,5,5, &
	SPACE,6,6,	QUOTE,7,7, &
	CAPITALIZE,8,8,	INDEX,9,9, &
	OVERSTRIKE,10,10, &
	"",3,10 &
	&

15800	DEF* FNN% &
	\ C9%=C9%+1% &
	\ FNN%=ASCII(CVT$$(MID(I$,C9%,1%),32%)) &
	\ FNEND &

15850	DEF* FNI$ &
	\ FNI$=I$+I6$+I7$ &
	\ I6$,I7$="" &
	\ FNEND &

15900	DEF* FNFIL.NAM$(OLD.STG$,MRG.STG$) &
		! FUNCTION TO MERGE FILESPEC WITH DEFAULTS &
	\ FNFIL.NAM$ = "" &
		! SET A REASONABLE INITIAL VALUE. &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+MRG.STG$) TO Z1% &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+OLD.STG$) TO Z% &
		! PARSE THE MERGE AND USER (OLD) STRINGS. &

15910	OLD.FS2% = Z%(29%)+SWAP%(Z%(30%)) &
	\ MRG.FS2% = Z1%(29%)+SWAP%(Z1%(30%)) &
	\ Z%(I%)=Z1%(I%) FOR I%=7% TO 10% &
		IF (MRG.FS2% AND 1%) UNLESS (OLD.FS2% AND 1%) &
			! COPY FILENAME IF WE HAVE A NEW ONE UNLESS &
			!  THEY SUPPLIED US WITH ONE &
	\ Z%(I%)=Z1%(I%) FOR I%=11% TO 12% &
		IF (MRG.FS2% AND 8%) UNLESS (OLD.FS2% AND 8%) &
			! COPY EXTENSION IF WE HAVE ONE AND THEY DON'T. &
	\ Z%(I%)=Z1%(I%) FOR I%=23% TO 26% &
		IF (MRG.FS2% AND 4096%) UNLESS (OLD.FS2% AND 4096%) &
			! COPY DEVICE IF WE HAVE ONE AND THEY DON'T. &

15920	NEW.STG$ = "" &
	\ NEW.FS2% = OLD.FS2% OR MRG.FS2% &
	\ NEW.FS1% = (Z%(27%) OR Z1%(27%)) + SWAP%( (Z%(28%) OR Z1%(28%)) ) &
		! SET FLAG BITS FOR MERGED FILE SPEC &
	\ NEW.STG$ = "_" + CHR$(Z%(23%))+CHR$(Z%(24%))+ &
		LEFT(NUM1$(Z%(25%)),Z%(26%))+":" IF NEW.FS2% AND 8192% &
	\ NEW.STG$ = NEW.STG$ + &
		"["+NUM1$(Z%(6%))+","+NUM1$(Z%(5%))+"]" &
		IF NEW.FS2% AND 128% &
	\ NEW.STG$ = NEW.STG$ + &
		RAD$(Z%(7%)+SWAP%(Z%(8%)))+RAD$(Z%(9%)+SWAP%(Z%(10%))) &
		IF NEW.FS2% AND 1% &
	\ NEW.STG$ = NEW.STG$ + &
		"."+RAD$(Z%(11%)+SWAP%(Z%(12%))) IF NEW.FS1% AND 512% &
	\ NEW.STG$ = NEW.STG$ + &
		"<"+NUM1$(Z%(22%))+">" IF NEW.FS1% AND 2048% &
	\ NEW.STG$ = NEW.STG$ + &
		"/MO:"+NUM1$((Z%(17%)+SWAP%(Z%(18%))) AND 32767%) &
			IF NEW.FS1% AND 2% &
	\ NEW.STG$ = NEW.STG$ + &
		"/SI:"+NUM1$(Z%(13%)+SWAP%(Z%(14%))) IF NEW.FS1% AND 4% &
	\ NEW.STG$ = NEW.STG$ + &
		"/CL:"+NUM1$(Z%(15%)+SWAP%(Z%(16%))) IF NEW.FS1% AND 1% &
	\ NEW.STG$ = NEW.STG$ + &
		"/PO:"+NUM1$(Z%(19%)+SWAP%(Z%(20%))) IF NEW.FS1% AND 8% &

15930	CHANGE SYS(CHR$(6%)+CHR$(-10%)+NEW.STG$) TO Z% &
		!CHECK OUT THE NEW STRING &
	\ NEW.FS2%=Z%(29%)+SWAP%(Z%(30%)) &
	\ IF (NEW.FS2% AND (2%+4%+32%+64%+256%+512%))<>0% &
			! IF ANYTHING IS WILD, &
		OR NEW.FS2%<0% &
			! OR ILLEGAL LOGICAL DEVICE WAS SPECIFIED &
		OR (NEW.FS2% AND 1%)=0% THEN &
			! OR NO FILENAME WAS SUPPLIED &
	&
			PRINT IF CCPOS(0%) &
	\		PRINT "?Illegal file name--"; OLD.STG$ &
	\		NEW.STG$ = "" &

15940	FNFIL.NAM$=NEW.STG$ &
	\ FNEND &

16000	DEF* FNQ$(I$) &
	\ I%=0%
16010	I%=INSTR(I%+1%,I$,CHR$(FLGS%(7%))) &
	\ IF I% THEN &
		I$=LEFT(I$,I%-1%) &
		   +CHR$(128% OR ASCII(MID(I$,I%+1%,1%))) &
		   +RIGHT(I$,I%+2%) &
	\ 	GOTO 16010
16020	FNQ$=I$ &
	\ FNEND &
	&

16200	DEF* FNS$(I$) &
	\ I%=0%
16210	I%=INSTR(I%+1%,I$,CHR$(9%)) &
	\ IF I% THEN &
		I$=LEFT(I$,I%-1%)+CHR$(-1%)+RIGHT(I$,I%+1%) &
	\	GOTO 16210 &

16220	FNS$=I$ &
	\ FNEND &
	&

16300	DEF* FNF$(I$,K%) &
	\ I%=INSTR(K%+1%,I$+CHR$(32%),CHR$(32%)) &
	\ J%=INSTR(K%+1%,I$,",") &
	\ I%=J% IF J% AND J%<I% &
	\ FNF$,I0$=MID(I$,K%+1%,I%-K%-1%) &
	\ FNF$=SPACE$(LEN(I0$)+1%)+FNF$(I$,I%) &
			UNLESS I0$<>"" OR I%>LEN(I$) OR &
				MID(I$,K%,1%)=MID(I$,I%,1%) &
	\ FNEND &

16400	DEF* FNS%(S$) &
	\ FNS%=0% &
	\ O$="" &
	\ I%=INSTR(1%,CVT$$(I$,32%),"/"+CHR$(ASCII(S$))) &
	\ GOTO 16420 UNLESS I% &
	\ FNS%=-1% &
	\ J%=INSTR(I%+2%,I$+"/","/") &
	\ O$=MID(I$,I%,J%-I%) &
	\ I$=LEFT(I$,I%-1%)+RIGHT(I$,J%) &
	\ I%=INSTR(3%,O$+"=","=") &
	\ J%=INSTR(3%,O$,":") &
	\ I%=J% IF J% AND J%<I% &
	\ O$=RIGHT(O$,I%+1%)
16420	FNEND &
	&

16500	DEF* FNCONVERT$(F$) &
	\ OPEN F$ FOR INPUT AS FILE 12% &
	\ CHANGE X9$ TO C% &
		! /CONVERT WAS SPECIFIED - OPEN THE CONVERSION FILE. &

16510	INPUT #12%, X%,Y% &
	\ X%=X% AND 127% &
	\ C%(X%+1%),C%(X%+129%)=Y% &
	\ GOTO 16510
16520	CLOSE 12% &
	\ CHANGE C% TO F$ &
	\ FNCONVERT$=F$ &
	\ FNEND &

16900	DEF* FNC1$(I$,FL%) &
	\ I$=FNS$(I$) &
	\ IF FL% THEN &
		I$=CVT$$(I$,24%) &
	ELSE	GOTO 16950 &

16910	IF PERIOD%=0% THEN &
		GOTO 16950 &
	ELSE	I%=INSTR(I%+2%,I$," ") &
	\	IF I%=0% THEN &
			GOTO 16950 &
		ELSE	GOTO 16910 IF INSTR(1%,".:;!?",MID(I$,I%-1%,1%))=0% &

16920	FOR I1%=2% TO 3% &
	\	I2%=ASCII(MID(I$,I%-I1%,1%)) &
	\	GOTO 16930 IF (I2%>=65% AND I2%<=90%) &
			OR (I2%>=97% AND I2%<=122%) &
	\	GOTO 16910
16930	NEXT I1% &

16940	I$=LEFT(I$,I%-1%)+CHR$(32%)+RIGHT(I$,I%) &
	\ GOTO 16910 &

16950	FNC1$=I$ &
	\ FNEND &
	&
	&

19000	! &
	&
	&
	!	E R R O R    H A N D L I N G &
	&

19010	IF ERR=11% THEN &
		IF ERL=16510% THEN RESUME 16520 &
		ELSE	RESUME 32767 UNLESS F1% &
	\		E9%=-1% &
	\		AUTOPAR%=0% UNLESS F1%=3% &
	\		FILLL%,J9%=0% &
	\		I$="" &
	\		RESUME 10020 &

19020	IF ERR=51% OR ERR=52% THEN &
		IF ERL<>15620% THEN RESUME 8020 &
		ELSE	ARG%=0% &
	\		Z9%=-1% &
	\		RESUME 15630 &

19030	IF ERR=5% THEN &
		IF ERL=700% THEN RESUME 710 &
		ELSE	IF ERL=31010% THEN &
				PRINT "'PRINT INDEX' Unavailable" &
	\			OPEN OUTFILE$ AS FILE 2%, MODE 2% &
	\			F2%=2% &
	\			RESUME 31180 &

19040	IF ERR=5% THEN &
		IF ERL=3020% THEN RESUME 4000 &
	! IF WE TRIED TO CHAIN TO $QUE AND COULN'T FINE IT, THEN WE WILL &
	! SEND THE QUEUE REQUEST TO PBS INSTEAD. &

19050	IF (ERR=4%) OR (ERR=32%) THEN &
	IF ERL=4120% THEN &
		ON ERROR GOTO 0 IF RETRY.COUNT%>10% &
		\ RESUME 4120 &
	! PENDING SPOOL MESSAGES ARE AT MESSAGE MAX, OR &
	! NO SMALL BUFFERS AVAILABLE -- &
	! TRY AGAIN UP TO 10 TIMES, THEN GIVE UP &

19060	IF ERR=5% AND ERL=4120% THEN &
		ON ERROR GOTO 0 IF RETRY.COUNT%>10% &
	\	RESUME 4120 &
	! IF WE GET THIS ERROR, RETRY A FEW TIMES, THEN GIVE UP &

19070	ON ERROR GOTO 0 &
		! JUST GIVE UP IF WE GET ANY OTHER ERROR &
	&

31000	I9$="$"+CHR$(13%)+"$RUNOFF"+CHR$(13%)+CVT%$(31100%)+CHR$(8%)+ &
	CVT%$(0%)+CVT%$(I9%-7%)+CVT%$(0%)+CVT%$(0%)+CVT%$(0%)+CVT%$(8%)+ &
	CVT%$(64%)+CVT%$(0%)+CVT%$(0%)+CVT%$(0%)+CVT%$(8%)+CVT%$(64%)+ &
	CVT%$(0%)+INDX.FIL$+CHR$(13%)+INDX.FIL$+CHR$(13%) &

31010	CLOSE F1%,F2% &
	\ I9$=SYS(CHR$(8%)+I9$) &
	\ CHAIN "$SQWIK" LINE 5 &

31100	GOSUB 10300 &
	\ OPEN INDX.FIL$ AS FILE 4% &
	\ KILL INDX.FIL$ &
	\ I9%=CVT$%(MID(I9$(0%),3%,2%))+7% &
	\ X9$=I9$(6%)+I9$(7%) &
	\ X9$=X9$+X9$ &
	\ TITLE$=I9$(5%) &
	\ SUBTTL$=I9$(4%) &
	\ I9$=I9$(3%) &
	\ I%=INSTR(1%,I9$,CHR$(13%)) &
	\ INFILE$=LEFT(I9$,I%-1%) &
	\ I9$=RIGHT(I9$,I%+1%) &
	\ I%=INSTR(1%,I9$,CHR$(13%)) &
	\ OUTFILE$=LEFT(I9$,I%-1%) &
	\ I9$=RIGHT(I9$,I%+1%) &
	\ I%=INSTR(1%,I9$,CHR$(13%)) &
	\ DAY$=LEFT(I9$,I%-1%) &
	\ PAG.HDR$=RIGHT(I9$,I%+1%) &
	\ CHANGE I9$(1%) TO C% &
	\ C%(I%)=0% FOR I%=C%(0%)+1% TO 64% &
	\ AUTOPAR%=C%(1%) &
	\ INDENT%=C%(2%) &
	\ UNDERSCR.BS.LF%=C%(3%) &
	\ UNDERSCR.CHAR%=C%(4%) &
	\ FLGS%(I%)=C%(5%+I%) FOR I%=0% TO 10% &
	\ SIM.FF%=C%(16%) &
	\ FILLL%=C%(17%) &
	\ SVFLGS%(I%)=C%(18%+I%) FOR I%=0% TO 10% &
	\ CHANGE FLGS% TO FLAGS.IN.EFFECT$ &
	\ PAR%(I%)=C%(29%+I%) FOR I%=0% TO 2% &
	\ HDR%=C%(32%) &
	\ PAGE.SIZ%=C%(33%) &
	\ J7%=C%(34%) &
	\ J9%=C%(35%) &
	\ L1%=C%(36%) &
	\ LFT.MRG%=C%(37%) &
	\ PAG.CREATED%=C%(38%) &
	\ PERIOD%=C%(39%) &
	\ P7%=C%(40%) &
	\ PAUS%=C%(41%) &
	\ QUE%=C%(42%) &
	\ R1%=C%(43%) &
	\ RGT.MRG%=C%(44%) &
	\ S1%=C%(45%) &
	\ S9%=C%(46%) &
	\ T1%=C%(47%) &
	\ UNDERSCR%=C%(48%) &
	\ U1%=C%(49%) &
	\ U7%=C%(50%) &
	\ CASE%=C%(51%) &
	\ FOOTNOT.LINES%=C%(52%) &
	\ W1%=C%(53%) &
	\ W9%=C%(54%) &
	\ X9%=C%(55%) &
	\ UNDERSCR.SP%=C%(56%) &
	\ DAT%=C%(57%) &
	\ FORM.SIZ%=C%(58%) &
	\ CHANGE LEFT(I9$(2%),34%) TO C% &
	\ T9%(I%)=C%(1%+I%) FOR I%=0% TO 33% &
	\ I9$=RIGHT(I9$(2%),35%) &
	\ LINES.LEFT.ON.PAG%=CVT$%(I9$) &
	\ PAG.NO%=CVT$%(RIGHT(I9$,3%)) &
	\ T0=CVT$F(RIGHT(I9$,5%)) &
	\ L0$=RIGHT(I9$,INSTR(9%,I9$,CHR$(13%))+1%) &
	\ OPEN OUTFILE$ AS FILE 2%, MODE 2% &
	\ F2%=2% &
	\ I9$="" &
	\ GOSUB 10200 &
	\ FOR I8%=8% TO I9% &
	\	IF LEFT(I9$(I8%),62%)=I9$ GOTO 31160 &
		ELSE	GOSUB 10400 &
	\		IF ASCII(I9$(I8%))<>ASCII(I9$) THEN &
				GOSUB 10500 &
				IF LINES.LEFT.ON.PAG%<FOOTNOT.LINES%+3% &
	\			PRINT #F2% &
	\			LINES.LEFT.ON.PAG%=LINES.LEFT.ON.PAG%-1%
31150		I9$=LEFT(I9$(I8%),62%) &
	\	L$=SPACE$(LFT.MRG%)+CVT$$(I9$,128%)+CHR$(32%) &
	\	L$=L$+RIGHT(D9$,W9%-(LFT.MRG%+RGT.MRG%)/2%+LEN(L$)+1%) &
	\	GOTO 31170
31160		L$=L$+","
31170		L$=L$+CVT$$(NUM$(CVT$%(I8$(I8%,31%))),2%) &
	\ NEXT I8% &
	\ L$=L$+" " &
	\ GOSUB 10400
31180	ON ERROR GOTO 19000 &
	\ P7%=0%
31200	CLOSE 4% &
	\ I9%=0% &
	\ IF P7% GOTO 1000 &
	ELSE	E9%=-1% &
	\	GOTO 3000 &

32767	END
