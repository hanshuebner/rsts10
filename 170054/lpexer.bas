2!		PROGRAM		: LPEXER.BAS
5!		VERSION		: V10.1
6!		EDIT		: A
7!		EDIT DATE	: 10-MAY-91
10	EXTEND			!USE EXTEND MODE
11	! &
	&
	&
	!		  C O P Y R I G H T &
	&
	&
  !		      Copyright (C) 1976, 1991 by &
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
	!
99!	&

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&

101!		LPEXER TESTS ANY ONE OF 8 LINE PRINTERS &
   !		WITH A ROTATING ASCII PATTERN. &
   !		IT ASKS THE USER FOR THE UNIT NUMBER TO TEST, &
   !		AND THE NUMBER OF PAGES OF OUTPUT DESIRED. &
   !	&
   !		THE DEFAULTS FOR LPEXER ARE 4 PAGES AND LINE PRINTER UNIT 0 . &
   !	&

299!
300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&

301!	CHANNEL #		USED FOR &
   !
302!		1		USED FOR THE OPEN LINE PRINTER &

399!
400	! &
	&
	&
	!	V A R I A B L E    D E F I N I T I O N S &
	&
	&

401!	VARIABLE NAME		USED FOR &
   !
799!
800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&

801!	FUNCTION/SUBROUTINE		USE &
   !
899!
900	! &
	&
	&
	!	D I M E N S I O N   S T A T E M E N T S &
	&

910	DIM S1%(30%), FSS.ARRAY%(30%) &
	! SPACE FOR DDB SYS CALL, FILENAME STRING SCAN SYS CALL. &
	&

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&
	&

1000	ON ERROR GO TO 19000 &
	\ TEMP$=SYS(CHR$(6%)+CHR$(-21%)) &
	! SET UP STANDARD ERROR TRAP &
	! PERMANENTLY DROP TEMPORARY PRIVILEGES. &

1010	I$="V10.1-A" &
	! SET UP VERSION AND EDIT NUMBER &

1020	PRINT "LPEXER";CHR$(9%);I$;CHR$(9%); &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)),3%),4%) &
	! SET UP HEADER AND PRINT IT &

1030	JOB.NO%=ASCII(SYS(CHR$(6%)+CHR$(9%)))/2% &
		! GET OUR JOB NUMBER. &

1100	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Line printer to test"; &
	\ INPUT LINE P1$ &
	\ GOTO 32767 IF ASCII(P1$)=27% &
	\ P1$=CVT$$(P1$,254%) &
	\ P1$=FNCHEK.DEV$(P1$,6%) &
	\ GOTO 1100 UNLESS LEN(P1$) &
		! GET DEVICE TO TEST. &

1110	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Number of pages to output <4>"; &
	\ INPUT LINE TEMP$ &
	\ GOTO 1100 IF ASCII(TEMP$)=27% &
	\ TEMP$=CVT$$(TEMP$,254%) &
	\ TEMP$="4" UNLESS LEN(TEMP$) &
	\ S=VAL(TEMP$) &
	\ IF S=-1. THEN &
		S=1000000000000000000000000000000. &
	  ELSE	IF S<=0. THEN &
			PRINT IF CCPOS(0%) &
	\		PRINT "?Illegal number of iterations selected" &
	\		GOTO 1110 &
		! GET NUMBER OF PAGES TO PRINT OUT. &

1120	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Detach <No>"; &
	\ INPUT LINE TEMP$ &
	\ GOTO 1110 IF ASCII(TEMP$)=27% &
	\ IF ASCII(CVT$$(TEMP$,254%))=89% THEN &
		PRINT "Type ATTACH";JOB.NO%;"when SYSTAT shows job"; &
			JOB.NO%;"in HB state." &
	\	PRINT &
	\	TEMP$=SYS(CHR$(6%)+CHR$(7%)) &
		! DETACH IF HE WANTS TO. &

1190	L$=L$ + CHR$(I%) FOR I%=32% TO 126% &
	\ L$=L$+L$ &
	! CREATE DOUBLE STRING OF 95 CHARACTER SET TO ROTATE &

1210	OPEN P1$ AS FILE 1% &
	! OPEN THE LINE PRINTER &

1220	S$=SYS(CHR$(6%)+CHR$(-8%)+CHR$(1%)) &
	\ CHANGE S$ TO S1% &
	\ W1%=S1%(20%) -1% &
	! GET NUMBER OF CHARACTERS PER LINE FROM DDB BYTE +21(8) &

1230	H1%=S1%(22%) &
	! GET VERTICAL FORM LENGTH FROM DDB BYTE +23(8) &

1300	FOR L1=1. TO S &
		! START LOOP FOR PAGES SPECIFIED &

1320		PRINT #1%,CHR$(12%) &
		! START AT TOP OF FORM &

1330		FOR L2%=1% TO H1%-6% &
			! START LOOP FOR THIS FORM LENGTH &

1340			PRINT #1%, LEFT(L$,W1%) &
			! PRINT ONE LINE &

1360			L$=RIGHT(L$,2%)+LEFT(L$,1%) &
			! ROTATING ONE CHARACTER TO THE LEFT &

1370		NEXT L2% &
		! END OF PAGE LOOP &

1380	NEXT L1 &
	! END OF PAGING LOOP &

1400	CLOSE #1% &
	! END OF THE LINE FOR THIS TEST &

1410	PRINT &
	\ PRINT "LPEXER finished at ";TIME$(0%) &
	! DONE MSG. + TIME &

1420	GOTO 32767 &
	! ALL DONE NOW LEAVE &
	&

16100	! &
	&
	!	C H E C K    D E V I C E    S P E C I F I E D &
	&
	&

16110	DEF* FNCHEK.DEV$(DEV.NAM$,DHI%) &

16120	CHANGE SYS(CHR$(6%)+CHR$(-23%)+DEV.NAM$) TO FSS.ARRAY% &
	&
	\ IF	(STATUS AND 255%)<>DHI% OR &
		RECOUNT<>0% OR &
		FSS.ARRAY%(27%)<>0% OR &
		(FSS.ARRAY%(28%) AND 143%)<>0% OR &
		(FSS.ARRAY%(30%) AND 16%)=0% OR &
		(FSS.ARRAY%(30%) AND 128%)<>0% THEN &
	&
			TEMP$="?Illegal device specified: "+DEV.NAM$ &
	\		GOTO 16170 &
	&
		!ONLY A DEVICE SPECIFIED? &

16130	FSS.ARRAY%(25%)=0% UNLESS FSS.ARRAY%(26%) &
	\ TEMP$="_"+ &
		CHR$(FSS.ARRAY%(23%))+CHR$(FSS.ARRAY%(24%))+ &
		NUM1$(FSS.ARRAY%(25%))+":" &
	&
	\ IF	FSS.ARRAY%(30%) AND 64% THEN &
			PRINT  IF CCPOS(0%) &
	\		PRINT "%Warning: "; DEV.NAM$; &
				" is a logical device:  "; &
				TEMP$; " will be used" &
	\		DEV.NAM$=TEMP$ &
	\		GOTO 16120 &
		!RE-CREATE THE DEVICE STRING.  IF IT WAS A LOGICAL, &
		! REPORT THE FACT AND RE-SCAN THE STRING. &

16140	ON ERROR GOTO 16150 &
	\ OPEN TEMP$ FOR INPUT AS FILE 1% &
	\ CLOSE -1% &
	\ GOTO 16190 &
		!TRY TO OPEN THE DEVICE.  IF SUCCESSFUL, DO A &
		! RESET AND GO AWAY. &

16150	TEMP$="?Open failure on "+TEMP$+" "+ &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),3%),4%) &
	\ RESUME 16170 &
		!HANDLE RANDOM OPEN ERRORS. &

16170	PRINT IF CCPOS(0%) &
	\ PRINT TEMP$ &
	\ TEMP$="" &
		!PRINT A MESSAGE AND RETURN UNSUCCESSFULLY. &

16190	ON ERROR GOTO 19000 &
	\ FNCHEK.DEV$=TEMP$ &
	\ FNEND &

19000	! &
	&
	&
	!	S T A N D A R D   E R R O R   T R A P &
	&
	&

19010	S$=SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)) &
	\ GOTO 32767 IF ERR=11% AND ERL<1200% &
	\ IF ERR=52% AND ERL=1110% THEN &
		PRINT "?Illegal number of pages selected." &
	\ RESUME 1110 &
	! QUIETLY GO AWAY IF USER TYPED CTRL/Z TO A PROMPT. &
	! GO AWAY UNLESS JUST A BAD NUMBER. &

19015	PRINT CVT$$(RIGHT(S$,3%),4%);" at line";ERL;" in LPEXER ";I$ &
	! PRINT THE ERROR MESSAGE. &

19020	RESUME 32700 &
	! RESUME AT ERROR EXIT &

32700	PRINT &
	\ PRINT "LPEXER Aborting at ";TIME$(0%) &
	! PRINT THE TIME &

32767	END
