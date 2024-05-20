2!		PROGRAM		: DDEXER.BAS
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
  !		      Copyright (C) 1981, 1991 by &
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
	! &

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&

101!	THE TU58 EXERCISER IS DESIGNED TO TEST THE NORMAL OPERATION &
   !	OF THE TU58 DECTAPE II CONTROLLER AND ANY ONE OF EIGHT TU58 &
   !	DECTAPE II DRIVES. &
   !	DDEXER OPENS THE DESIRED TAPE NON-FILE STRUCTURED AND
102!	ZEROES IT.  IT FILLS THE TAPE WITH A ROTATING PATTERN OF FLOATING &
   !	POINT NUMBERS.  THERE ARE 512 BLOCKS ON A DECTAPE II. &
   !
103!	THE NUMBERS WRITTEN ON THE TAPE ARE READ AND CHECKED, KEEPING &
   !	A COUNT OF THE INCORRECT VALUES.  IF AT THE END OF THE PASS &
   !	THE ERROR COUNT IS NOT ZERO, DDEXER WILL PRINT THE ERROR COUNT. &
   !
104!	THE DEFAULT NUMBER OF ITERATIONS &
   !	IS 10 WITH 1 TO 1000 ALLOWED.  THE NUMBER OF RECORDS OR &
   !	BLOCKS USED IS 180. &
   !	&

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&

301!		CHANNEL #	USED FOR &
   !
302!		1		DDN: (NON-FILE STRUCTURED TU58) &
   !	&

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
899!	&
	&

900	! &
	&
	&
	&
	!	D I M E N S I O N   S T A T E M E N T S &
	&

910	DIM A%(512%),B%(512%), FSS.ARRAY%(30%) &
	! BUFFER ARRAYS, FILENAME STRING SCAN ARRAY. &
	&
	&

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&

1000	ON ERROR GOTO 19000 &
	\ TEMP$=SYS(CHR$(6%)+CHR$(-21%)) &
	! SET UP STANDARD ERROR TRAP &
	! PERMANENTLY DROP TEMPORARY PRIVILEGES. &

1010	I$="V10.1-A" &
	! SET UP VERSION AND EDIT NUMBER &

1020	PRINT "DDEXER";CHR$(9%);I$;CHR$(9%); &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)),3%),4%) &
	! SET UP HEADER AND PRINT IT &

1025	PRINT &
	\ PRINT "Warning: This exerciser will destroy data on the"; &
		" tested TU58." &
	! LET THEM KNOW THAT THIS MIGHT BE DANGEROUS. &

1030	JOB.NO%=ASCII(SYS(CHR$(6%)+CHR$(9%)))/2% &
	\ E=0. &
	\ C9%=512% &
		! GET OUR JOB NUMBER. &
		! INITIALIZE ERROR COUNT TO ZERO. &
		! SET # BLOCKS TO TEST TO 512. &

1040	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "TU58 drive to test"; &
	\ INPUT LINE P1$ &
	\ GOTO 32767 IF ASCII(P1$)=27% &
	\ P1$=CVT$$(P1$,254%) &
	\ P1$=FNCHEK.DEV$(P1$,30%) &
	\ GOTO 1040 UNLESS LEN(P1$) &
		! GET NAME OF DEVICE TO TEST. &
		! CHECK OUT THE USER'S SPEC, MAKING SURE IT WAS &
		! TU58 (HANDLER INDEX=30), NO SWITCHES SPECIFIED, ETC. &
		! NULL STRING RETURNED MEANS SPEC WAS ILLEGAL. &

1050	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Number of iterations <10>"; &
	\ INPUT LINE TEMP$ &
	\ GOTO 1040 IF ASCII(TEMP$)=27% &
	\ TEMP$=CVT$$(TEMP$,254%) &
	\ TEMP$="10" UNLESS LEN(TEMP$) &
	\ Z8=VAL(TEMP$) &
	\ IF Z8=-1. THEN &
		Z8=1000000000000000000000000000000. &
	  ELSE	IF Z8<=0. THEN &
			PRINT IF CCPOS(0%) &
	\		PRINT "?Illegal number of iterations selected" &
	\		GOTO 1050 &
		! GET THE NUMBER OF ITERATIONS. &

1060	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Detach <No>"; &
	\ INPUT LINE TEMP$ &
	\ GOTO 1050 IF ASCII(TEMP$)=27% &
	\ IF ASCII(CVT$$(TEMP$,254%))=89% THEN &
		PRINT "Type ATTACH";JOB.NO%;"when SYSTAT shows job"; &
			JOB.NO%;"in HB state." &
	\	PRINT &
	\	TEMP$=SYS(CHR$(6%)+CHR$(7%)) &
		! DETACH IF HE WANTS TO. &

1070	FSS.ARRAY%(1%)=6% &
	\ FSS.ARRAY%(2%)=13% &
	\ FSS.ARRAY%(I%)=0% FOR I%=3% TO 22% &
	\ CHANGE FSS.ARRAY% TO S2$ &
		! SET UP STRING FOR SYS CALL TO ZERO TU58. &
	&

1140	FOR Z9=1. TO Z8 &
		! START MAIN ITERATION LOOP &

1310		OPEN P1$  AS FILE 1% &
	\	FIELD #1%, 511% AS B$ &
		! OPEN THIS TU58 FILE NON-FILE STRUCTURED &

1320		A%(0%)= 512% &
	\	A%(1%)=1% &
		! SET UP ARRAY LENGTH &

1330		FOR I8%=1% TO C9% &
			! LOOP FOR REQUESTED NUMBER OF BLOCKS &

1340			A%(I%)=(A%(I%-1%)+1%) AND 255% FOR I%=1% TO 512% &
	\		CHANGE A% TO A$ &
	\		LSET B$=A$ &
			! CREATE 511 BYTE BUFFER FOR THIS BLOCK &

1350			PUT #1% &
			! PUT THE DESIRED RECORD TO TU58 SEQUENTIALLY &

1360			A%(1%)=(A%(512%)+1%) AND 255% &
			! CREATE ROTATING PATTERN FROM LAST WORD &

1370		NEXT I8% &
	\	CLOSE 1% &
		! CLOSE FILE AT END OF WRITE LOOP &

1400		OPEN P1$ FOR INPUT AS FILE 1% &
	\	FIELD #1%, 511% AS B$ &
		! OPEN FILE NON-FILE STRUCTURED &

1410		E1%=0% &
	\	A%(1%)=0% &
		! SET UP COMPARE ARRAY &

1420		FOR I8%=1% TO C9% &
			! START LOOP FOR VERIFY &

1430			A%(I%)=(A%(I%-1%)+1%) AND 255% FOR I%=1% TO 512% &
			! CREATE ARRAY TO COMPARE TO &

1440			GET #1% &
	\		CHANGE B$ TO B% &
			! INPUT RECORDS IN SEQUENTIAL ORDER &

1500			E1%=E1% +1% IF VAL(NUM$(A%(I%)))<>VAL(NUM$(B%(I%))) &
				FOR I%=1% TO 512% &
			! COMPARE BUFFERS &

1530			A%(1%)=(A%(512%)+1%) AND 255% &
			! CREATE ROTATING PATTERN FROM LAST WORD &

1540		NEXT I8% &
	\	CLOSE 1% &
		! END VERIFY LOOP AND CLOSE FILE &

1550		IF E1%=0% THEN 1580 &

1560		PRINT "?DDEXER -";E1%; &
		"Errors detected on ";P1$;" in iteration";Z9 &
			IF E1% &
	\	E=E+E1% &
		! PRINT ERRORS FOR THIS PASS IF ANY &

1580	NEXT Z9 &
	\ CLOSE 1% &
	! END OF MAIN ITERATION LOOP &

1590	PRINT "No"; IF E=0. &
	\ PRINT "?";NUM1$(E); IF E<>0. &
	\ PRINT " error"; &
	\ PRINT "s"; UNLESS E=1. &
	\ PRINT " detected on ";P1$ &
	\ PRINT &
	\ PRINT "DDEXER Finished at ";TIME$(0%) &

1610	GOTO 32767 &
	&
	&

16100	! &
	&
	!	C H E C K    D E V I C E    S P E C I F I E D &
	&
	&

16110	DEF* FNCHEK.DEV$(DEV.NAM$,DHI%) &

16120	CHANGE SYS(CHR$(6%)+CHR$(-23%)+DEV.NAM$) TO FSS.ARRAY% &
	\ IF	(STATUS AND 255%)<>DHI% OR &
		RECOUNT<>0% OR &
		FSS.ARRAY%(27%)<>0% OR &
		(FSS.ARRAY%(28%) AND 143%)<>0% OR &
		(FSS.ARRAY%(30%) AND 16%)=0% OR &
		(FSS.ARRAY%(30%) AND 128%)<>0% THEN &
			TEMP$="?Illegal device specified: "+DEV.NAM$ &
	\		GOTO 16170 &
		! MAKE SURE ONLY A DEVICE SPECIFIED, AND THAT IT &
		! WAS A TU58 (CHECK DEVICE HANDLER INDEX). &

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
	\ GET #1% &
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
		! (CLEARED STRING SIGNALS AN ERROR ON RETURN.) &

16190	ON ERROR GOTO 19000 &
	\ FNCHEK.DEV$=TEMP$ &
	\ FNEND &
	&

19000	! &
	&
	&
	!	S T A N D A R D   E R R O R   T R A P &
	&

19010	S$=SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)) &
		! GET ERROR MESSAGE TEXT &
	\ RESUME 32767 IF ERR=11% AND ERL<1100% &
	\ PRINT IF CCPOS(0%) &
	\ IF ERR=52% AND ERL=1050 THEN &
		PRINT "?Illegal number of iterations specified." &
	\	RESUME 1050 &
		! EXIT QUIETLY IF USER TYPED CTRL/Z TO A PROMPT. &

19020	PRINT IF CCPOS(0%) &
	\ PRINT CVT$$(RIGHT(S$,3%),4%);" at line";ERL;"in DDEXER ";I$ &
	! PRINT ERROR MESSAGE &

19050	RESUME 19060 &

19060	ON ERROR GOTO 19070 &
	\ CLOSE 1% &
	\ GOTO 32700 &
	! GOTO ERROR EXIT &

19070	RESUME 32700 &
	! GOTO ERROR EXIT &

32700	PRINT "DDEXER Aborting at", TIME$(0%) &

32767	END
