2!		PROGRAM		: MTEXER.BAS
5!		VERSION		: V10.1
6!		EDIT		: L
7!		EDIT DATE	: 10-AUG-92
10	EXTEND			!USE EXTEND MODE
11	! &
	&
	&
	!		  C O P Y R I G H T &
	&
	&
  !		      Copyright (C) 1976, 1992 by &
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
	! 9.1-04	16-Jul-85	(VAM) Add MU and high density support &
	! 10.1-K	10-Aug-92	(VAM) Obfuscate TK50 description &
	!
99!	&

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&

101!	The magtape exerciser MTEXER is used to check the normal &
   !	operation of the TM11/TU10/TS03 magtape drive and controller, &
   !	the TM02/TU16/TU45 and the TM03/TE16 magtape drives and controllers, &
   !	the TS11/TK25 magtape drives and controller, and the TU81/TK50 TMSCP &
   !	magtape drives and controllers. &
   !	MTEXER allows the operator to select the drive, the length of &
   !	of tape in feet, and the number of iterations to be done. &
   !	on each iteration the tape is zeroed, opened non-file structured &
   !	for output 800 bpi dump mode or 1600 bpi phase encoded, &
   !	and data written for the requested &
   !	length.  The tape is then rewound, the tape opened for input, &
   !	read and verified.  If errors are found, a count of the number &
   !	of bytes in error is printed before the next iteration. &
   !	&
   !	The data pattern used is the worst case NRZ pattern for 9-track &
   !	drives.  This pattern is loaded into a 512 byte buffer and n &
   !	puts are used to write the tape.  X is the repetition counter &
   !	so that X identical blocks are written on each iteration. &
   !	The pattern buffer is changed and the process continues &
   !	until the requested length is reached.  The pattern buffer &
   !	changes with x so the pattern is different on each pass. &
   !	&
   !	The defaults are 100 feet and 2 iterations. &
   !	&

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&

301!	CHANNEL #		USED FOR &
   !	&
   !		1		MAGTAPE DRIVE TO BE TESTED NFS &
   !	&

400	! &
	&
	&
	!	V A R I A B L E    D E F I N I T I O N S &
	&
	&

401!	VARIABLE NAME		USED FOR &
   !	&
   !		D1%		MODE 256% FOR 1600 BPI PE MODE &
   !				MODE 12% FOR 800 BPI DUMP MODE &
   !	&

800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&

801!	FUNCTION/SUBROUTINE		USE &
   !	&

900	! &
	&
	&
	&
	!	D I M E N S I O N   S T A T E M E N T S &
	&

910	DIM A%(512%), T%(512%), FSS.ARRAY%(30%) &
	! ARRAYS FOR BUFFERS, FILENAME STRING SCAN DATA ARRAY. &
	&

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&

1000	ON ERROR GOTO 19000 &
	\ S$=SYS(CHR$(6%)+CHR$(-21%)) &
	! SET UP STANDARD ERROR TRAP &
	! PERMANENTLY DROP TEMPORARY PRIVILEGES. &

1010	I$="V10.1-L" &
	! SET UP VERSION AND EDIT NUMBER. &

1020	PRINT "MTEXER";CHR$(9%);I$;CHR$(9%); &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)),3%),4%) &
	! SET UP STANDARD HEADER AND PRINT IT. &

1025	PRINT &
	\ PRINT "Warning: This exerciser will destroy data on the"; &
		" tested magtape." &
	! LET THEM KNOW THAT THIS MIGHT BE DANGEROUS. &

1030	JOB.NO%=ASCII(SYS(CHR$(6%)+CHR$(9%)))/2% &
		! FIND OUR JOB NUMBER. &

1100	CLOSE 1% &
	\ PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Magtape drive to test"; &
	\ INPUT LINE DEV.NAM$ &
	\ GOTO 32767 IF ASCII(DEV.NAM$)=27% &
	\ DEV.NAM$=CVT$$(DEV.NAM$,254%) &
	\ DEV.NAM$=FNCHEK.DEV$(DEV.NAM$,14%) &
	\ GOTO 1100 UNLESS LEN(DEV.NAM$) &

1110	OPEN DEV.NAM$ AS FILE 1% &
	\ I%=MAGTAPE(3%,0%,1%) &
	\ I%=MAGTAPE(12%,32767%,1%) &
	\ I1%=MAGTAPE(12%,1%,1%) &
	\ MAX.DENS%,MIN.DENS%=0% &
	\ S$="" &
	\ RESTORE &
	\ READ S$ UNTIL S$="*END" &
	\ READ DRIVE.TYPE$,MAX.DENS%,MIN.DENS% &
		UNTIL MAX.DENS%=I% AND MIN.DENS%=I1% &
	\ PRINT &
	\ PRINT "Drive type is: ";DRIVE.TYPE$ &
	\ IF MAX.DENS%=MIN.DENS% THEN &
		DENSITY%=MAX.DENS% &
		\ GOTO 1130 &
	! Open and rewind the magtape. &
	! Get the highest and lowest density. &
	! See if we know what kind of drive it is. &
	! If so, just print it out. &
	! If we've only got one density, skip asking for it. &

1120	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Density (";NUM1$(MIN.DENS%);" or ";NUM1$(MAX.DENS%);") <"; &
		NUM1$(MAX.DENS%);">"; &
	\ INPUT LINE S$ &
	\ GOTO 1100 IF ASCII(S$)=27% &
	\ S$=CVT$$(S$,254%) &
	\ IF LEN(S$)=0% THEN &
		DENSITY%=MAX.DENS% ELSE &
		DENSITY%=VAL(S$) &
		\ GOTO 1120 IF DENSITY%<2% OR DENSITY%>32766% &
	! Tell 'em what the legal densities are. &
	! See if they want the default, or take a user-supplied one. &
	! If it's user-supplied, make sure it's OK. &

1130	I%=MAGTAPE(12%,32767%+1%+DENSITY%,1%) &
	! Set the density. &

1140	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Number of feet to test <100>"; &
	\ INPUT LINE S$ &
	\ GOTO 1110 IF ASCII(S$)=27% &
	\ S$=CVT$$(S$,254%) &
	\ S$="100" UNLESS LEN(S$) &
	\ N2%=(VAL(S$)*12.)/3.1 &
		! CALCULATE NUMBER OF BLOCKS FROM NUMBER OF FEET. &

1150	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Number of iterations <2>"; &
	\ INPUT LINE S$ &
	\ GOTO 1140 IF ASCII(S$)=27% &
	\ S$=CVT$$(S$,254%) &
	\ S$="2" UNLESS LEN(S$) &
	\ ITER.NO=VAL(S$) &
	\ IF ITER.NO=-1. THEN &
		ITER.NO=1000000000000000000000000000000. &
	  ELSE	IF ITER.NO<=0. THEN &
			PRINT IF CCPOS(0%) &
	\		PRINT "?Illegal number of iterations selected" &
	\		GOTO 1120 &
		! GET NUMBER OF ITERATIONS TO DO. &

1160	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Detach <No>"; &
	\ INPUT LINE S$ &
	\ GOTO 1150 IF ASCII(S$)=27% &
	\ PRINT &
	&

1180	IF (MAGTAPE(7%,0%,1%) AND 1024%+512%+32%) THEN &
		PRINT IF CCPOS(0%) &
	\	PRINT "?Drive not ready" &
	\	CLOSE 1% &
	\	GOTO 1100 &
		! CHECK FOR FILE PROT, BEYOND EOT, AND OFFLINE &

1190	I%=MAGTAPE(3%,0%,1%) &
	\ CLOSE 1% &
		! REWIND TAPE AND CLOSE CHANNEL FOR NOW &
	\ S$=CVT$$(S$,254%) &
	\ IF ASCII(S$)=89% THEN &
		PRINT "Type ATTACH";JOB.NO%;"when SYSTAT shows job"; &
			JOB.NO%;"in HB state." &
	\	PRINT &
	\	S$=SYS(CHR$(6%)+CHR$(7%)) &
		! DETACH IF HE WANTS TO. &

1200	E1%=0% &
	! SET ERROR COUNT TO 0 FOR THIS PASS &

1210	FOR R1=1. TO ITER.NO &
		! START MAIN ITERATION LOOP &

1230		R1%=R1-32768.*INT(R1/32768.) &
		\ B%=2%**(R1% AND 7%) &
		! CREATE NEW ARRAY STARTING VALUE FOR THIS PASS &

1240		OPEN DEV.NAM$ FOR OUTPUT AS FILE 1% &
		\ FIELD #1%,512% AS T$ &
		\ I%=MAGTAPE(3%,0%,1%) &
		\ I%=MAGTAPE(12%,32767%+1%+DENSITY%,1%) &
		! Field the channel and rewind the tape. &
		! Set the density also. &

1250		A%(I%)=B%+I%-1%		FOR I%=1% TO 511% STEP 2% &
		\ A%(I%)=-(B%+I%-2%)-1%	FOR I%=2% TO 512% STEP 2% &
		\ A%(0%)=512% &
		! SET UP INTEGER ARRAY FOR THIS BLOCK &

1300		FOR I8%= 1% TO N2% &
			! START WRITE LOOP &

1310			CHANGE A% TO A$ &
			\ LSET T$=A$ &
			! CONVERT TO CHARACTER STRING AND MOVE TO I/O BUFFER &

1320			PUT #1% &
			\ W9=W9+1 &
			! WRITE AND INCREMENT COUNT &

1330			A%(I%)=A%(I%)-1%+2%*(I% AND 1%)	FOR I%=1% TO 512% &
			! ROTATE PATTERN FOR NEXT LOOP &

1340		NEXT I8% &
		! END OF WRITE LOOP &

1370		CLOSE 1% &
		! END OF OUTPUT PORTION &

1380		OPEN DEV.NAM$ FOR INPUT AS FILE 1% &
		\ FIELD #1%,512 AS T$ &
		\ I%=MAGTAPE(3%,0%,1%) &
		\ I%=MAGTAPE(12%,32767%+1%+DENSITY%,1%) &
		! Open the magtape and field it. &
		! Rewind it and set the density. &

1390		A%(I%)=B%+I%-1%		FOR I%=1% TO 511% STEP 2% &
		\ A%(I%)=-(B%+I%-2%)-1%	FOR I%=2% TO 512% STEP 2% &
		\ A%(0%)=512% &
		! SET UP COMPARE BUFFER FOR THIS BLOCK &

1400		FOR I8%= 1% TO N2% &
			! START READ COMPARE LOOP &

1410			CHANGE A% TO A$ &
			! CONVERT BUFFER TO STRING FOR COMPARE &

1420			GET #1% &
			\ R9=R9+1 &
			! READ BLOCK AND INCREMENT READ COUNT &

1430			IF T$=A$ THEN 1470 ELSE CHANGE T$ TO T% &
			! IF OK THEN SKIP CHARACTER COMPARE LOOP &

1440			FOR I%=1% TO 512% &
			! START CHARACTER BY CHARACTER COMPARE &

1450			IF ((A%(I%) AND 255%)<>(T%(I%) AND 255%)) THEN &
				E1%=E1%+1% &
			! INCREMENT ERROR COUNT FOR EACH BAD CHARACTER COMPARE &

1460			NEXT I% &
			! END OF CHARACTER BY CHARACTER COMPARE LOOP &

1470			A%(I%)=A%(I%)-1%+2%*(I% AND 1%) FOR I%=1% TO 512% &
			! UPDATE ARRAY FOR NEXT LOOP &

1480		NEXT I8% &
		! END OF READ LOOP &

1490		I%=MAGTAPE(3%,0%,1%) &
		\ CLOSE 1% &
		! REWIND TAPE AND CLOSE CHANNEL FOR INPUT &

1500		IF E1%=0% THEN 1520 &
		! IF NO ERRORS ON THIS PASS &

1510		PRINT "?MTEXER - "; E1%;" Errors detected on ";DEV.NAM$; &
		"During repetition"; R1 &
		\ E1=E1+E1% &
		\ E1%=0% &
		! PRINT ERROR COUNT FOR THIS PASS AND THEN CLEAR IT &

1520	NEXT R1 &
	! END OF MAIN ITERATION LOOP &

1530	GOSUB 8000 &
	! PRINT OUT STATUS DATA &

1540	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "No"; IF E1=0. &
	\ PRINT NUM1$(E1) IF E1<>0. &
	\ PRINT " errors detected on ";DEV.NAM$ &
	! REPORT HOW MANY ERRORS FOUND. &

1550	GOTO 32767 &
	! NORMAL EXIT &

6000	! &
	&
	&
	!	E R R O R   S T A T U S   R O U T I N E &
	&

6010	RESTORE &
	\ READ S$ FOR Y%=0% TO (Y0% AND 7%) &
	! GET LAST COMMAND &

6020	PRINT "Magtape status summary :" &
	\ PRINT "Last command was ";S$ &
	\ PRINT "  Density Parity  Tracks" &

6030	Y9%=MAGTAPE(12%,0%,1%) &
	\ S$=NUM1$(Y9%) &
	\ PRINT S$;SPACE$(7%-LEN(S$)); &
	! Tell 'em what the density was. &

6050	IF Y0% AND 2048% THEN PRINT " Even"; ELSE PRINT " Odd"; &

6060	IF Y0% AND 4096% THEN PRINT "	7" ELSE PRINT "	9" &

6070	PRINT "	W-Lock	EOT	BOT	EOF	RLE" &
	\ Y%=1024% &

6080	IF Y% AND Y0% THEN PRINT "	Yes"; ELSE PRINT "	No"; &

6090	Y%=Y%/2% &
	\ IF Y%<>32% THEN 6080 ELSE PRINT &

6100	IF Y0%<0% THEN &
	PRINT "?MTEXER - An error occurred on last command" &

6110	IF Y0% AND 32% THEN &
	PRINT "?MTEXER - Selection error occurred" &

6120	RETURN &

6900	DATA	OFFLINE, &
		READ, &
		WRITE, &
		"WRITE EOF", &
		REWIND, &
		"SKIP RECORD", &
		"BACKSPACE RECORD", &
		"*END" &
	&

8000	! &
	&
	&
	!	I / O   D A T A   R O U T I N E &
	&

8010	PRINT &
	\ PRINT "Magtape MTEXER I/O Data" &
	!TITLE OF PRINTOUT &

8020	PRINT "Device","Reads","Words","Writes","Words" &
	!HDR LINE COLS &

8040	PRINT DEV.NAM$, &
	!UNIT # PRINT OUT &

8060	IF R9<1000. THEN PRINT NUM$(R9), &
		ELSE PRINT NUM$(R9/1000.)+"K", &
	!# OF READS IN K &

8100	IF (R9*256)<1000. THEN PRINT NUM$(R9*256), &
		ELSE PRINT NUM$((R9*256)/1000.)+"K", &
	!# OF WORDS READ &

8140	IF W9<1000. THEN PRINT NUM$(W9), &
		ELSE PRINT NUM$(W9/1000.)+"K", &
	!# OF WRITES IN K &

8180	IF (W9*256)<1000. THEN PRINT NUM$(W9*256) &
		ELSE PRINT NUM$((W9*256)/1000.)+"K" &
	!# OF WORDS WRITTEN &

8220	PRINT &
	\ RETURN &
	! END OF DATA PRINT ROUTINE &
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
	\ FNEND
19000	! &
	&
	&
	!	S T A N D A R D   E R R O R   T R A P &
	&

19010	S$=SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)) &
	\ RESUME 32767 IF ERR=11% AND ERL<1190 &
	\ GOTO 19020 UNLESS ERR=52% AND &
		(ERL=1120% OR ERL=1130% OR ERL=1140% OR ERL=1150%) &
	\ PRINT IF CCPOS(0%) &
	\ PRINT "?Illegal "; &
	\ PRINT "number of feet"; IF ERL=1140% &
	\ PRINT "number of iterations"; IF ERL=1150% &
	\ PRINT "density"; IF ERL=1120% OR ERL=1130% &
	\ PRINT " selected." &
	\ IF ERL=1130% THEN &
		RESUME 1120% ELSE &
		RESUME &
	! GET ERROR MESSAGE TEXT, BUT DO CATCH ILLEGAL NUMBERS. &
	! Also check for invalid density stuff. &

19020	GOTO 19025 UNLESS ERR=65% &
	\ PRINT IF CCPOS(0%) &
	\ PRINT "?Magtape unit is offline" &
	\ PRINT &
	\ RESUME 1100 &
	! Here's where we go if we couldn't get the density. &

19025	GOTO 19030 UNLESS ERR=57% AND ERL=1110% &
	\ PRINT &
	\ PRINT "Drive type is UNKNOWN" &
	\ PRINT "Highest legal density will be used for testing" &
	\ DENSITY%=32767% &
	\ RESUME 1140 &
	! If we fell off the end of the list of supported drive types, &
	!  call it UNKNOWN. &
	! Tell 'em what we're gonna do about density, and set it up. &
	! Go join the mainstream. &

19030	PRINT IF CCPOS(0%) &
	\ PRINT CVT$$(RIGHT(S$,3%),4%);" at line";ERL;"in MTEXER ";I$ &
	\ PRINT &
	\ RESUME 19040 &
	! PRINT ERROR MESSAGE. &

19040	ON ERROR GOTO 19050 &
	\ Y0%=MAGTAPE(7%,0%,1%) &
	\ GOSUB 6000 &
	\ CLOSE 1% &
	\ ON ERROR GOTO 19000 &
	\ GOTO 19060 &
	! GET MAGTAPE STATUS IF CHANNEL IS OPEN &

19050	RESUME 19060 &
	! RECOVER FROM NON-OPEN FILE &

19060	GOSUB 8000 &
	! PRINT OUT STATUS DATA &

20000	! &
	!	L i s t   o f   S u p p o r t e d   M a g t a p e   T y p e s &
	! &
	! &
	!		Drive type		Maximum dens	Minimum dens &
	! &
	DATA		"TU81",			6250,		1600, &
			"TK25",			8000,		8000, &
			"TK50 compatible",	6667,		6667, &
			"TS11/TS05/TU80",	1600,		1600, &
			"TE16/TU45/TU77",	1600,		800, &
			"TE10/TS03",		800,		800 &

32767	END
