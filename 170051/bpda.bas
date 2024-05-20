2!		PROGRAM		: BPDA
5!		VERSION		: V10.1
6!		EDIT		: A
7!		EDIT DATE	: 10-MAY-91
10	EXTEND		!USE BASIC-PLUS EXTEND MODE
11!	&
	! &
	! &
	!                          C O P Y R I G H T &
	! &
	! &
	!		      Copyright (C) 1978, 1991 by &
	!            Digital Equipment Corporation, Maynard, Mass. &
	! &
	! &
	! This software is furnished under a  license  and  may  be  used  and &
	! copied  only  in  accordance with the terms of such license and with &
	! the inclusion of the above copyright notice.  This software  or  any &
	! other copies thereof may not be provided or otherwise made available &
	! to any other person.  No title to and ownership of the  software  is &
	! hereby transferred. &
	! &
	! The information in this software is subject to change without notice &
	! and  should  not  be  construed as a commitment by Digital Equipment &
	! Corporation. &
	! &
	! DIGITAL assumes no responsibility for the use or reliability of  its &
	! software on equipment that is not supplied by DIGITAL. &
	! &
	! ******************************************************************** &

20	! &
	! &
	! &
	!	M O D I F I C A T I O N    H I S T O R Y &
	! &
	! &
	! &

21	! VER/ED	EDIT DATE	REASON &
	! &

100	! &
	! &
	! &
	!	P R O G R A M   D E S C R I P T I O N &
	! &
	! &
	! &

300	! &
	! &
	! &
	!	I / O    C H A N N E L S &
	! &
	! &
	! &

301!	     CHANNEL #		USED FOR &
	! &

400	! &
	! &
	! &
	!	V A R I A B L E   D E F I N I T I O N S &
	! &
	! &
	! &

401!	     VARIABLE NAME	DEFINITION &
	! &

800	! &
	! &
	! &
	!	F U N C T I O N S   A N D   S U B R O U T I N E S &
	! &
	! &
	!
801!		PROGRAM DEFINED SUBROUTINES &
	! &
	!	TITLE	LINE RANGE	DESCRIPTION &
	!
825!		PROGRAM DEFINED FUNCTIONS &
	! &
	!	TITLE	LINE RANGE	DESCRIPTION &
	! &
	! &

900	! &
	! &
	! &
	!	D I M E N S I O N    S T A T E M E N T S &
	! &
	! &
	! &

901!		ARRAYS &
	! &

910	  DIM Z%(30%), Z1%(30%) &
		! USED TO PARSE FILENAMES &
	\ DIM #12%, CORE%(16383%) &
		! USED TO LOOK AT THE DUMP FILE &

920	  ENTRY.TYP% = 0% &

951!	DIM STATEMENT &
		! DIMENSION STATEMENT DESCRIPTION &

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&

1000	  ON ERROR GOTO 19000 &
	&
	! SET UP STANDARD ERROR TRAP. &

1010	  I$="V10.1-A" &
	\ PRIV.OFF$=SYS(CHR$(6%)+CHR$(-21%)) &
	&
	! SET UP VERSION/EDIT #. &
	! AND DROP PRIVILEGES &

1020	  OUT.FIL% = 11% &
	\ PMD.FIL% = 12% &
	\ F.RM$ = CHR$(12%) &
	\ A.ST$ = "*" &
	\ D.OT$ = "." &
	\ P.ER$ = "%" &
	\ NONE$ = "None" &
	\ T.AB$ = CHR$(9%) &
	\ CR.LF$ = CHR$(13%)+CHR$(10%) &
	&
		! SET UP THE CHANNEL ASSIGNMENTS &
		! AND SOME USEFUL STRINGS &

1030	  IF ENTRY.TYP%=0% &
	  THEN &
		  PRINT IF CCPOS(0%) &
		\ PRINT "BPDA";T.AB$;I$;T.AB$; FNERR.MSG$(0%) &
	&
	! 	IF WE HAVE A RUN ENTRY THEN &
	!	RETURN KB TO LEFT MARGIN &
	!	PRINT THE SYSTEM HEADER &

1070	  VAR.TAB% = 1214% &
	\ BYTS.PER.LINE% = 12% &
	&
		!START OF VARIABLE TABLE AND &
		!NUMBER OF STRING BYTES TO DISPLAY ON EACH PHYSICAL LINE &

1090	!	***** INITIALIZE PARAMETERS &
		! INITIALIZE PROGRAM PARAMETERS &

1100	! &
	! &
	! &
	!	M A I N    L I N E    C O D E &
	! &
	! &
	! &

1105	  GO TO 1115 IF ENTRY.TYP% <> 0% &
	&
		! SKIP THE COMMAND LINE INPUT IF WE HAVE A CCL &
		! OR A CHAIN ENTRY. &

1110	  GO TO 32767 IF ENTRY.TYP% <> 0% &
	\ PRINT "Input"; &
	\ INPUT LINE PMD.FIL$ &
	\ PMD.FIL$ = CVT$$(PMD.FIL$,-2%) &
	\ GOTO 1110 UNLESS LEN(PMD.FIL$) &
	&
		! GET A COMMAND LINE AND MAKE IT CLEAN. &
		! PROMPT AGAIN IF HE DIDN'T SAY MUCH. &

1115	  I% = INSTR(0%,PMD.FIL$,"=") &
	\ OUT.FIL$ = LEFT(PMD.FIL$,I%-1%) &
	\ PMD.FIL$ = RIGHT(PMD.FIL$,I%+1%) &
	\ GOTO 1110 UNLESS LEN(PMD.FIL$) &
	&
		! CHECK FOR OUTFILE=INFILE, AND EXTRACT &
		!  THE FILE NAME(S). &
		! AGAIN, IGNORE LINES WITH NO INPUT FILE. &

1120	  PMD.FIL$ = FNFIL.NAM$(PMD.FIL$,"_SY:.PMD",-1%) &
	\ GOTO 1110 UNLESS LEN(PMD.FIL$) &
	\ OPEN PMD.FIL$+"/RONLY" FOR INPUT AS FILE PMD.FIL%, RECORDSIZE 2048% &
	\ CHANGE SYS(CHR$(12%)) TO Z% &
	\ HI.ADDR% = Z%(13%)+SWAP%(Z%(14%)) &
	\ NICE.FIL.NAM$ = RAD$(Z%(7%)+SWAP%(Z%(8%))) &
			+ RAD$(Z%(9%)+SWAP%(Z%(10%))) + D.OT$ &
			+ RAD$(Z%(11%)+SWAP%(Z%(12%))) &
	\ IF	HI.ADDR% > 64% &
	     OR ((HI.ADDR% AND 3%) <> 0%) &
	     OR	(Z%(4%) <> 0%) &
	     OR  HI.ADDR% = 0% &
	  THEN &
		  E$ = "?" + NICE.FIL.NAM$ +" is not a BASIC-PLUS dump file" &
		\ GOTO 19100 &
	&
		! YELL AT USER IF BAD FILE NAME &
		! ALLEGED MEMORY DUMP HAS AN ILLEGAL FORMAT &
		!	(I.E. GREATER THAN 16K, OR NOT AN EVEN &
		!	      1K MULTIPLE, OR ZERO LENGTH) &

1130	  CHANGE SYS(CHR$(6%)+CHR$(-26%)+CHR$(PMD.FIL%)) TO Z% &
	\ RTS.NAME$ = RAD$(Z%(27%) OR SWAP%(Z%(28%))) + &
		      RAD$(Z%(29%) OR SWAP%(Z%(30%))) &
	\ RTS.NAME$ = CVT$$(RTS.NAME$,-2%) &
	\ HI.ADDR% = ( ( HI.ADDR% - 1% ) * 512% ) + 511% &
			! GET THE RUN-TIME SYSTEM NAME OF THE DUMP FILE &
			! CALCULATE HIGH ADDRESS IN WORDS &

1135	  IF LEN(OUT.FIL$)=0% &
	  THEN &
		  PRINT "Output"; &
		\ INPUT LINE OUT.FIL$ &
		\ OUT.FIL$=CVT$$(OUT.FIL$,-2%) &
		\ OUT.FIL$="_KB:" UNLESS LEN(OUT.FIL$) &
	&
			! GET AN OUTPUT FILE NAME IF WE DON'T HAVE &
			! ONE ALREADY &

1140	  OUT.FIL$ = FNFIL.NAM$(OUT.FIL$, "_SY:" &
		     + LEFT(NICE.FIL.NAM$,6%) + ".PDA",0%) &
	\ GOTO 1135 UNLESS LEN(OUT.FIL$) &
	\ OPEN OUT.FIL$ FOR OUTPUT AS FILE OUT.FIL% &
			! MERGE THE DEFAULTS.  NOTE THAT IT IS ESSENTIAL &
			!  THAT NICE.FIL.NAM$ WAS CREATED WITH &
			!  RAD$()+RAD$()+"."+RAD$() TO ENSURE A 6 CHAR &
			!  FILE NAME. &
			! AND THEN OPEN THE FILE SPECIFIED &

1200	  PRINT #OUT.FIL%, F.RM$ &
	\ PRINT #OUT.FIL% FOR I%=1% TO 2% &
	\ PRINT #OUT.FIL%, T.AB$; &
		"Dump Analysis of "; NICE.FIL.NAM$; " on "; &
		DATE$(0%); " at "; TIME$(0%) &
	\ PRINT #OUT.FIL% FOR I%=1% TO 3% &

1210	  GOSUB 11800 &
		! INITIALIZE ALL POINTERS &

1220	  GOSUB 10100 &
		! PRINT LOW-CORE GOODIES &

1230	  GOSUB 11900 &
		! PRINT THE TRACEBACK INFORMATION &
	&

1300	  RESTORE &
	\ FOR SWITCH% = 1% TO 9% &
		\ READ V2$,V2% &
		\ PRINT #OUT.FIL%, CR.LF$;CR.LF$;V2$; &
		\ P.FLG% = FALSE% &
		\ FOR JINDEX% = VAR.TAB% TO VAR.TAB%+50% STEP 2% &
			\ TEMPX% = JINDEX% &

1310			  WHILE (FNP%(TEMPX%)<>0%) &
				\ TEMPX% = TEMPX% + FNP%(TEMPX%) &
				\ TEMPY% = TEMPX%-1% &
				\ V2$ = CHR$((JINDEX%-VAR.TAB%)/2%+65%) &

1320				  V0% = FNB%(TEMPY%) &
				\ IF V0% AND 128% &
				  THEN &
					  TEMPY%=TEMPY%-1% IF (TEMPY% AND 1%) &
				  ELSE &
					  V2$ = V2$ + CHR$(V0%) &
					\ TEMPY% = TEMPY% - 1% &
					\ GO TO 1320 &

1330				  IF ((FNB%(TEMPY%) AND 127%)=V2%) &
				  THEN &
					  PRINT #OUT.FIL%,CR.LF$ UNLESS P.FLG% &
					\ ON SWITCH% GOSUB &
					  10200,10300,10400,10500,10600, &
					  10700,10800,10900,11000 &
					\ P.FLG% = TRUE% &

1340				  IF (FNP%(TEMPY%-2%)<>0%) &
				  THEN &
					  TEMPY% = TEMPY%-2%+FNP%(TEMPY%-2%) &
					\ GO TO 1330 &

1350			  NEXT &
	&
		\ NEXT JINDEX% &
		\ PRINT #OUT.FIL%, NONE$ UNLESS P.FLG% &
	&
	\ NEXT SWITCH% &

1500	  GOSUB 11500 &
		! PRINT I/O CHANNEL STUFFS &

2000	  PRINT #OUT.FIL% FOR I%=1% TO 3% &
	\ GOTO 9000 &

9000	! &
	! &
	! &
	!	P R O G R A M    C L E A N    U P &
	! &
	! &
	! &

9010	  PRINT #OUT.FIL% FOR I%=1% TO 3% &
	\ CLOSE PMD.FIL%, OUT.FIL% &
	\ GOTO 32767 &

10000	! &
	! &
	! &
	!	P R O G R A M M E R    D E F I N E D &
	!		S U B R O U T I N E S &
	! &
	! &

10100	! &
	! &
	! &
	!	F O R M A T    A N D    P R I N T    L O W    C O R E &
	! &
	! &

10110	PRINT #OUT.FIL%, "BASIC-PLUS ("; RTS.NAME$; ") Internal Version "; &
			    NUM1$(FNB%(R1%+1%)); &
	\ R1% = R1%+2% &
		!POINT TO SYSVEL &
	\ PRINT #OUT.FIL%, ", RSTS/E Version "; CVT%$(SWAP%(FNP%(R1%))); &
		D.OT$; CHR$(FNB%(R1%-2%)); CR.LF$ &
	\ R1% = R1% + 2% &
		!POINT TO CSR AREA POINTER &
	\ R1% = R1% + 2% &
		!POINT TO COUNT OF SAVED CSR'S &
	\ T% = FNP%(R1%) &
		!GET COUNT OF SAVED CSR'S &
	\ R1% = R1% + 2% &
		!POINT TO SAVED CSR'S &
	\ R1% = R1% + T%*2% &
		!POINT TO .MATH./FLTLEN &
	\ PRINT #OUT.FIL%, "Math Package Description:" &
	\ PRINT #OUT.FIL%, T.AB$; &
	\ T% = FNB%(R1%+1%) &
	\ IF (T%<128%) AND T% THEN &
		PRINT #OUT.FIL%, "4-Word Decimal Math package" &
	  ELSE	T% = -T% AND 255% &
			!NEGATE IT &
	\	PRINT #OUT.FIL%, NUM1$(FNB%(R1%)); "-Word Math package" &
	\	PRINT #OUT.FIL%, T.AB$; "Scale factor:"; T% &
			IF FNB%(R1%) = 4% &

10120	T% = FNP%(432%) &
		!GET THE EDITOR FLAG WORD &
	\ PRINT #OUT.FIL%, CR.LF$; "Editor Flags:"; T%; "( #"; &
		FNWORD$(T%); " )" &
	\ PRINT #OUT.FIL%, T.AB$; "Program is "; &
	\ PRINT #OUT.FIL%, FNNO.NOT$((T% AND 32%)=0%); IF T% AND 16% &
	\ PRINT #OUT.FIL%, "compiled" &
	\ PRINT #OUT.FIL%, T.AB$; "CONTinue is "; &
		FNNO.NOT$(T% AND 128%); "allowed" &
	\ PRINT #OUT.FIL%, T.AB$; "Program uses "; &
	\ PRINT #OUT.FIL%, "NO"; UNLESS T% AND 2048% &
	\ PRINT #OUT.FIL%, "EXTEND mode (the default is "; &
	\ PRINT #OUT.FIL%, "NO"; UNLESS T% AND 8192% &
	\ PRINT #OUT.FIL%, "EXTEND mode)"; CR.LF$ &
	\ T% = FNP%(256%) &
		!GET THE JOB FLAG WORD &
	\ PRINT #OUT.FIL%, "Job Flags:"; T%; "( #"; &
		FNWORD$(T%); " )" &
	\ PRINT #OUT.FIL%, T.AB$; "User is "; &
		FNNO.NOT$(T% AND 1024%); "privileged" &
	\ PRINT #OUT.FIL%, T.AB$; "CTRL/C trapping is "; &
		FNNO.NOT$(T% AND 64%); "enabled" &
	\ PRINT #OUT.FIL%, T.AB$; "TRACE is enabled" &
		IF T% AND 8% &
	\ PRINT #OUT.FIL% IF T% AND (1024%+64%+8%) &
	\ IF T% AND 1% THEN &
		! IF BREAK WAS ENABLED &
	&
		T% = FNP%(46%) &
			!FIND THE BREAK FLAGS &
	\	IF T% < 0% THEN &
			PRINT #OUT.FIL%, T.AB$; &
				"BREAK is enabled for all line numbers" &
		ELSE	PRINT #OUT.FIL%, T.AB$; &
				"BREAK is enabled for these lines:" &
	\		PRINT #OUT.FIL%, T.AB$, FNP%(I%) &
				IF FNP%(I%) FOR I%=122% TO 140% STEP 2% &
	\		PRINT #OUT.FIL%, CR.LF$ &

10130	PRINT #OUT.FIL%, "Core Common"; &
	\ T% = FNB%(304%) &
		! FIND LENGTH OF CORE COMMON &
	\ IF T%=0% THEN &
		PRINT #OUT.FIL%, " is empty"; CR.LF$; &
	  ELSE	PRINT #OUT.FIL%, ":"; CR.LF$ &
	\	T% = FNSHO.STG%(305%,T%) &
			!SHOW CORE COMMON IN ASCII AND DECIMAL &

10140	PRINT #OUT.FIL%, CR.LF$;  "User-Assigned Logicals"; CR.LF$ &
	\ PRINT #OUT.FIL%, FNPAD.STG$("@ PPN : ",24%); &
	\ T% = FNP%(476%) &
	\ PRINT #OUT.FIL%, "<none>" UNLESS T% &
	\ PRINT #OUT.FIL%, "["; NUM1$(FNB%(477%)); ","; &
		NUM1$(FNB%(476%)); "]" IF T% &
	\ PRINT #OUT.FIL%, FNPAD.STG$("Protection Code : ",24%); &
	\ T% = FNB%(478%) &
	\ PRINT #OUT.FIL%, "<none>" UNLESS T% &
	\ PRINT #OUT.FIL%, "<"; NUM1$(FNB%(479%)); ">" IF T% &
	\ PRINT #OUT.FIL% &
	\ T% = FNP%(504%)=-1% &
		! T% FLAGS PRESENCE/ABSENCE OF ASSOCIATED PPN'S &
	\ FOR I% = 0% TO 24%+(T%*8%) STEP 8% &
		! LOOP THRU USER LOGICALS &
	&
	\	IF FNP%(480%+I%) THEN &
			! IF WE HAVE A LOGICAL HERE &
			PRINT #OUT.FIL%, TAB(18%); &
			 CHR$(FNB%(480%+I%+4%)); CHR$(FNB%(480%+I%+5%)); &
			 LEFT(NUM1$(FNB%(480%+I%+6%)),FNB%(480%+I%+7%)); &
				IF FNP%(480%+I%+4%) &
	\		PRINT #OUT.FIL%, TAB(22%); ":"; &
	\		PRINT #OUT.FIL%, " ["; &
			 FNPAD.N$(FNB%(506%+(I%/8%)*2%+1%),3%); &
			 ","; &
			 NUM1$(FNB%(506%+(I%/8%)*2%)); TAB(32%); "]"; &
				IF FNP%(506%+(I%/8%)*2%) IF T% &
	\		PRINT #OUT.FIL%, " "; &
			 RAD$(FNP%(480%+I%)); RAD$(FNP%(480%+I%+2%)) &

10150	NEXT I% &
	\ PRINT #OUT.FIL% &

10160	PRINT #OUT.FIL%, "Last Opened File  < SYS(CHR$(12)) >"; CR.LF$ &
	\ PRINT #OUT.FIL%, T.AB$; CHR$(FNB%(36%)); CHR$(FNB%(37%)); &
		! DEVICE NAME &
	\ PRINT #OUT.FIL%, NUM1$(FNB%(38%)); IF FNB%(39%) &
		! UNIT NUMBER, IF ANY &
	\ PRINT #OUT.FIL%, ": ["; &
		NUM1$(FNB%(19%)); ","; NUM1$(FNB%(18%)); "] "; &
			! PPN &
		RAD$(FNP%(20%)); RAD$(FNP%(22%)); D.OT$; RAD$(FNP%(24%)); &
			! FILENAME &
		" <"; NUM1$(FNB%(35%)); ">"; &
			! PROTECTION CODE &
	\ T = FNB%(17%) &
	\ T = 0. IF T > 64. &
			! A RUN ENTRY SEEMS TO GENERATE #377 HERE. &
	\ T = T * 256. + FNB%(27%) &
	\ T = T * 256. + FNB%(26%) &
			! CALCULATE A POSSIBLY LARGE FILE SIZE &
	\ PRINT #OUT.FIL%, " /SIZE:"; NUM1$(T); &
			! PRINT IT &
	\ T% = FNB%(34%) &
	\ T% = 256% UNLESS T% &
			! CLUSTERSIZE IS MOD 256 HERE &
	\ PRINT #OUT.FIL%, " /CLUS:"; NUM1$(T%); &
	\ PRINT #OUT.FIL%, " /MODE:"; NUM1$(FNP%(30%) AND 32767%); &
		IF FNP%(30%)<0% &
	\ PRINT #OUT.FIL%, CR.LF$ &
	\ PRINT #OUT.FIL%, T.AB$; &
		"   BUFFERSIZE ="; FNP%(28%); &
		"   STATUS ="; FNP%(32%); &
		" ( #"; FNWORD$(FNP%(32%)); " )"; CR.LF$ &

10170	  PRINT #OUT.FIL%, CR.LF$; "Predefined variables:"; CR.LF$ &
	\ I% = FNP%(SPDA%-26%) &
	\ PRINT #OUT.FIL%, "ERR";T.AB$;FNPAD.N$(I%,6%);P.ER$; &
	\ PRINT #OUT.FIL%, TAB(20%);FNERR.MSG$(I%); IF I% &
	\ PRINT #OUT.FIL%, CR.LF$; &
		FNSPDA.REL$("ERL"    ,-60%,1%); &
		FNSPDA.REL$("LINE"   ,-58%,1%); &
		FNSPDA.REL$("RECOUNT",-54%,1%); &
		FNSPDA.REL$("STATUS" ,-62%,1%); &
		FNSPDA.REL$("NUM"    ,-30%,1%); &
		FNSPDA.REL$("NUM2"   ,-34%,1%); &
		FNSPDA.REL$("DET"    ,-24%,2%); &
		CR.LF$
10190	  RETURN &
	&

10200	! &
	! &
	! &
	!			P R I N T &
	!	       I N T E G E R   S C A L A R &
	!		    V A R I A B L E S &
	! &
	! &
	! &

10210	  INT.PTR% = TEMPY%-4% &
	\ V2$ = V2$ + P.ER$ &

10220	  PRINT #OUT.FIL%, V2$;TAB(49%);FNNUMBER$(INT.PTR%,7%,1%) &
	\ RETURN &

10300	! &
	! &
	! &
	!			P R I N T &
	!	F L O A T I N G   P O I N T   S C A L A R &
	!		    V A R I A B L E S &
	! &
	! &
	! &

10310	  FLT.PTR% = TEMPY%-FLOAT.LEN% &

10320	  PRINT #OUT.FIL%, V2$;TAB(49%);FNNUMBER$(FLT.PTR%,13%,2%) &
	\ RETURN &

10400	! &
	! &
	! &
	!			P R I N T &
	!		S T R I N G   S C A L A R &
	!		    V A R I A B L E S &
	! &
	! &
	! &

10410	  STR.PTR% = TEMPY%-8% &
	\ V2$ = V2$ + "$" &

10420	  PRINT #OUT.FIL%, V2$;"=";NUM1$(FNP%(STR.PTR%+4%)) &
	\ P.FLG% = FNSHO.STG%(FNP%(STR.PTR%+2%)+STR.PTR%,FNP%(STR.PTR%+4%)) &
	\ PRINT #OUT.FIL% &
	\ RETURN &
	&

10500	! &
	! &
	! &
	!			P R I N T &
	!		I N T E G E R   A R R A Y &
	!		    V A R I A B L E S &
	! &
	! &
	! &

10510	  ARY.PTR% = TEMPY%-28% &
	\ ARY.NAM$ = V2$+"%(" &
	\ GOSUB 12000 &
	\ RETURN IF NO.DATA% &
	\ INT.PTR% = FNP%(ARY.PTR%+2%) + ARY.PTR% &
	\ GO TO 10520 IF Y% &
	\ FOR INDEX.X% = 0% TO X% &
		\ V2$ = ARY.NAM$ + NUM1$(INDEX.X%) + ")" &
		\ GO SUB 10220 &
		\ INT.PTR% = INT.PTR% + 2% &
	\ NEXT INDEX.X% &
	\ RETURN &

10520	  FOR INDEX.X% = 0% TO X% &
		\ FOR INDEX.Y% = 0% TO Y% &
			\ V2$ = ARY.NAM$ + NUM1$(INDEX.X%) + "," + &
				NUM1$(INDEX.Y%) + ")" &
			\ GO SUB 10220 &
			\ INT.PTR% = INT.PTR% + 2% &
		\ NEXT INDEX.Y% &
	\ NEXT INDEX.X% &
	\ RETURN &
	&

10600	! &
	! &
	! &
	!			P R I N T &
	!	  F L O A T I N G   P O I N T   A R R A Y &
	!		    V A R I A B L E S &
	! &
	! &
	! &

10610	  ARY.PTR% = TEMPY% - 28% &
	\ ARY.NAM$ = V2$+"(" &
	\ GOSUB 12000 &
	\ RETURN IF NO.DATA% &
	\ FLT.PTR% = FNP%(ARY.PTR%+2%) + ARY.PTR% &
	\ GO TO 10620 IF Y% &
	\ FOR INDEX.X% = 0% TO X% &
		\ V2$ = ARY.NAM$ + NUM1$(INDEX.X%) + ")" &
		\ GO SUB 10320 &
		\ FLT.PTR% = FLT.PTR% + FLOAT.SIZE%*2% &
	\ NEXT INDEX.X% &
	\ RETURN &

10620	  FOR INDEX.X% = 0% TO X% &
		\ FOR INDEX.Y% = 0% TO Y% &
			\ V2$ = ARY.NAM$ + NUM1$(INDEX.X%) + "," + &
				NUM1$(INDEX.Y%) + ")" &
			\ GO SUB 10320 &
			\ FLT.PTR% = FLT.PTR% + FLOAT.SIZE%*2% &
		\ NEXT INDEX.Y% &
	\ NEXT INDEX.X% &
	\ RETURN &

10700	! &
	! &
	! &
	!			P R I N T &
	!		 S T R I N G   A R R A Y &
	!		    V A R I A B L E S &
	! &
	! &
	! &

10710	  ARY.NAM$ = V2$+"$(" &
	\ ARY.PTR% = TEMPY%-28% &
	\ GOSUB 12000 &
	\ RETURN IF NO.DATA% &
	\ STR.PTR% = FNP%(ARY.PTR%+2%) + ARY.PTR% &
	\ GO TO 10720 IF Y% &
	\ FOR INDEX.X% = 0% TO X% &
		\ V2$ = ARY.NAM$ + NUM1$(INDEX.X%) + ")" &
		\ GO SUB 10420 &
		\ STR.PTR% = STR.PTR% + FNP%(STR.PTR%) &
	\ NEXT INDEX.X% &
	\ RETURN &

10720	  FOR INDEX.X% = 0% TO X% &
		\ FOR INDEX.Y% = 0% TO Y% &
			\ V2$ = ARY.NAM$ + NUM1$(INDEX.X%) + "," + &
				NUM1$(INDEX.Y%) + ")" &
			\ GO SUB 10420 &
			\ STR.PTR% = STR.PTR% + FNP%(STR.PTR%) &
		\ NEXT INDEX.Y% &
	\ NEXT INDEX.X% &
	\ RETURN &

10800	! &
	! &
	! &
	!			P R I N T &
	!		      I N T E G E R &
	!		    F U N C T I O N S &
	! &
	! &
	! &

10810	  PRINT #OUT.FIL%, "FN";V2$;P.ER$, &
	\ RETURN &

10900	! &
	! &
	! &
	!			P R I N T &
	!		F L O A T I N G   P O I N T &
	!		    F U N C T I O N S &
	! &
	! &
	! &

10910	  PRINT #OUT.FIL%, "FN";V2$, &
	\ RETURN &
	&

11000	! &
	! &
	! &
	!			P R I N T &
	!		       S T R I N G &
	!		    F U N C T I O N S &
	! &
	! &
	! &

11010	  PRINT #OUT.FIL%, "FN";V2$;"$", &
	\ RETURN &

11500	! &
	! &
	! &
	!	P R I N T   I / O   C H A N N E L   S T A T I &
	! &
	! &
	! &

11510	  IOCHAN% = SPDA% + 44% + 16% &
	\ PRINT #OUT.FIL%, F.RM$;CR.LF$;"Open I/O channels: "; &
	\ P.FLG% = FALSE% &
	\ FOR JINDEX% = 1% TO 12% &
	\ TEMPY% = IOCHAN% + JINDEX%*16% &
	\ IF FNP%(TEMPY%+2%) &
	  THEN &
		  PRINT #OUT.FIL%, UNLESS P.FLG% &
		\ PRINT #OUT.FIL%, CR.LF$ &
		\ PRINT #OUT.FIL%, "Channel ";FNB%(TEMPY%+10%)/2% &
		\ PRINT #OUT.FIL%, "Buffersize = ";FNP%(TEMPY%+4%) &
		\ PRINT #OUT.FIL%, "Current block ="; &
		\ CURBLK = FNB%(TEMPY%+13%) &
		\ CURBLK = CURBLK * 65536. &
		\ CURBLK = CURBLK + (FNP%(TEMPY%+14%) AND 32767%) &
		\ CURBLK = CURBLK + 32768. IF FNP%(TEMPY%+14%) < 0% &
		\ PRINT #OUT.FIL%, CURBLK &
		\ PRINT #OUT.FIL%, "Current pointer = "; &
			FNP%(TEMPY%+8%)-FNP%(TEMPY%+2%) &
		\ PRINT #OUT.FIL%, "Bytes in buffer = "; &
			FNP%(TEMPY%+6%) &
		\ PRINT #OUT.FIL%, "Device characteristics: " &
		\ PRINT #OUT.FIL%, "Channels position (CCPOS) ="; &
			FNB%(TEMPY%+12%) &
		\ PRINT #OUT.FIL%, "Current buffer contents: "; &
				   CR.LF$ &
		\ BUFFR% = FNP%(TEMPY%+2%)+TEMPY% &
		\ IF BUFFR% &
		  THEN &
			  P.FLG% = FNBUF.PRT%(BUFFR%,FNP%(TEMPY%+4%)-2%) &
	&

11520	  NEXT JINDEX% &
	\ PRINT #OUT.FIL%, NONE$ UNLESS P.FLG% &
	\ RETURN &

11800	! &
	! &
	! &
	!	I N I T I A L I Z E    A L L   L O W   C O R E &
	! &
	!			P O I N T E R S &
	! &
	! &

11810	  SCTH%    = FNP%(440%) &
	\ SPTA%	   = FNP%(442%) &
	\ SPDA%    = FNP%(444%) &
	\ R1%      = FNP%(514%) &
	\ VAR.TAB% = SPDA% + 1214% &
	\ SCAL.FACT = 10.^(255% AND (256%-FNB%(R1%+39%))) &
	\ FLOAT.SIZE% = FNB%(R1%+38%) &
	\ FLOAT.LEN% = 4%+(2%*FLOAT.SIZE%)-2% &
	\ M1% = LEN(CVTF$(0%))/2% - 1% &
	\ M2% = 0% &
	\ IF ((M1% = 3%) AND (FLOAT.SIZE% = 2%)) &
	  THEN &
		  M1% = 1% &
		\ M2% = 4% &

11830	  PAD.STRING$ = STRING$(M2%,0%) &
	\ TAB.PLACE% = 7% + BYTS.PER.LINE%*4% &
	\ TRUE%  = (1% = 1%) &
	\ FALSE% = (1% = 2%) &

11899	RETURN &

11900	! &
	! &
	! &
	!	T R A C E B A C K    S U B R O U T I N E    C A L L S &
	! &
	! &

11910	TAGBIN% = 10% &
		! OFFSET TO THE STATEMENT NUMBER &
	\ R1% = FNP%(514%) &
		! BASE OF THE R1 STACK &
	\ R1% = R1% + (15%+5%)*2% &
		! POINT PAST THE CSR SAVE AREA &
	\ R1CORG% = FNP%(446%) &
		! FIND THE TOP OF THE R1 STACK &
	\ R1CORG% = R1CORG% - 4% &
		! ALLOW FOR 2 WORDS REMAINING ON THE STACK &
	\ PRINT #OUT.FIL% &
	\ PRINT #OUT.FIL%, "Traceback information:"; &
	\ IF R1% >= R1CORG% THEN &
		! CAN'T TRACEBACK IF NOTHING TO TRACE &
	&
		PRINT #OUT.FIL%, "  "; NONE$ &
	\	GOTO 11940 &

11920	  PRINT #OUT.FIL% FOR I% = 1% TO 2% &
	\ GOSUB 11950 &
		! GET THE LAST-ENCOUNTERED LINE NUMBER &
	\ PRINT #OUT.FIL%, TAB(14%); &
		"Current line is"; CURLIN% &
	\ RESLOC%=FNP%(SPDA%+316%) &
		! GET NUMBER OF MOST RECENT LINE WITH AN ERROR &
	\ PRINT #OUT.FIL%, TAB(14%); "( From error at"; &
		FNP%(SPTA%+RESLOC%+TAGBIN%); ")" IF RESLOC% &
			! TELL THEM ABOUT AN UN-RESUMED ERROR &
	\ WHILE R1% < R1CORG% &
		! STEP THROUGH THE SUBROUTINE/FUNCTION CALLS &
	&
	\	GOSUB 11950 &
			!GET THE PREVIOUS LINE NUMBER &
	\	IF SUB.FLAG% THEN &
			PRINT #OUT.FIL%, TAB(14%); "    called from"; &
		   ELSE	PRINT #OUT.FIL%, TAB(14%); "FN invoked from"; &

11930		PRINT #OUT.FIL%, CURLIN% &
	\ NEXT &

11940	RETURN &

11950	R1% = R1% + 2% &
		! SKIP PAST THE RETURN ADDRESS &
	\ SCTH% = FNP%(R1%) + SPTA% &
		! GET THE PREVIOUS TEXT HEADER POINTER (CURRENT IS &
		!  FOR THE IMMEDIATE MODE HEADER) &
	\ R1% = R1% + 2% &
		! POP IT FROM THE STACK &
	\ CURLIN% = FNP%(SCTH%+TAGBIN%) &
		! GET THE LINE NUMBER &
	\ R1% = R1% + 4% &
		! POPPOP THE R1 STACK &
	\ SUB.FLAG% = -1% &
		! ASSUME A SUBROUTINE CALL &
	\ IF FNP%(R1%-2%) THEN &
		! THIS IS A FUNCTION RATHER THAN A SUBROUTINE &
	&
		R1% = R1% + 4% &
			! POPPOP AGAIN &
	\	SUB.FLAG% = 0% &

11960	RETURN &

12000	! &
	! &
	!		P R I N T    F I X E D    S T U F F &
	!			F O R    A R R A Y S &
	! &
	! &
	! &

12010	PRINT #OUT.FIL% &
	\ PRINT #OUT.FIL%, ARY.NAM$; NUM1$(FNP%(ARY.PTR%+22%)); &
		! PRINT PDIM1 (PERMANENT DIMENSION 1) &
	\ PRINT #OUT.FIL%, ",";NUM1$(FNP%(ARY.PTR%+24%)); &
				IF FNP%(ARY.PTR%+24%)<>0% &
		! PRINT PDIM2, IF IT EXISTS &
	\ PRINT #OUT.FIL%, ")"; &
	\ PRINT #OUT.FIL%, " #";NUM1$(FNB%(ARY.PTR%+10%)/2%);P.ER$; &
				IF FNB%(ARY.PTR%+10%) &
		! PRINT ARYSLT IF THIS IS VIRTUAL CORE &
	\ PRINT #OUT.FIL%, "    (Not yet referenced)"; &
		UNLESS FNP%(ARY.PTR%+4%) &
		! TELL THEM WHY NO DATA WILL APPEAR &
		!  (ARRAY NOT ALLOCATED YET) &
	\ PRINT #OUT.FIL% &
	\ X% = FNP%(ARY.PTR%+6%) &
		! GRAB CURRENT FIRST DIMENSION &
	\ Y% = FNP%(ARY.PTR%+8%) &
		! GRAB CURRENT SECOND DIMENSION &
	\ NO.DATA% = (FNB%(ARY.PTR%+10%)<>0%) OR &
		(FNP%(ARY.PTR%+4%)=0%) &
		! THERE'S NO DATA TO PRINT IF THIS IS VIRTUAL CORE (AN &
		!  I/O CHANNEL IS ASSIGNED TO IT), OR IF THE ARRAY HAS &
		!  NOT BEEN ALLOCATED (ZERO LENGTH) &
	\ RETURN &
	&

15000	! &
	&
	&
	!	F U N C T I O N S &
	&
	&

15200	DEF* FNBYTE$(Q%) &
	! FUNCTION	FNBYTE$	GET A 3 POSITION OCTAL STRING &

15210	Q%=Q% AND 255% &
	\ Q0%=64% &
	\ Q$="" &
	\ WHILE Q0%>0% &
	\	Q1%=Q%/Q0% &
	\	Q%=Q%-Q0%*Q1% &
	\	Q0%=Q0%/8% &
	\	Q$=Q$+CHR$(48%+Q1%) &
	\ NEXT &
	! CONVERT THE NUMBER TO OCTAL, SAVING EACH DIGIT &

15230	FNBYTE$=Q$ &
	! SET THE FUNCTION &

15240	FNEND &
	&

15400	DEF* FNWORD$(Q%) &
	! FUNCTION	FNWORD$	GET A 6 POSITION OCTAL STRING &

15410	Q$="0" &
	\ Q0%=4096% &
	\ IF Q%<0% THEN &
		Q$="1" &
	\	Q%=Q%+32767%+1% &
	! SET UP TO CONVERT THE NUMBER &
	! TAKE CARE OF NEGATIVE NUMBERS &

15420	WHILE Q0%>0% &
	\	Q1%=Q%/Q0% &
	\	Q%=Q%-Q0%*Q1% &
	\	Q0%=Q0%/8% &
	\	Q$=Q$+CHR$(48%+Q1%) &
	\ NEXT &
	! CONVERT THE NUMBER TO OCTAL, SAVING EACH DIGIT &

15430	FNWORD$=Q$ &
	! SET THE FUNCTION &

15440	FNEND &
	&

15500	DEF* FNP%(Q%) &
	! FUNCTION	FNP%	THIS FUNCTION IS EQUIVALENT &
	!			TO A PEEK OF AN ADDRESS &

15510	FNP%=0% &
	\ IF Q% > HI.ADDR% OR Q% < 0% THEN &
		E$ = "?PEEK out of bounds" &
	\	GOTO 19100 &
	! MUST BE POSITIVE AND WITHIN ALLOWABLE RANGE &

15520	FNP%=CORE%(Q%/2%) &
		! DO A 'PEEK' &
	\ FNP% = SWAP%(CORE%(Q%/2%)) IF Q% AND 1% &
		! SWAP THE BYTES TO GET LOW BYTE=DESIRED BYTE &
		!  FOR ODD (BYTE) PEEKS. &

15530	FNEND &

15550	DEF* FNB%(Q%) = FNP%(Q%) AND 255% &
		! THIS FUNCTION PEEKS FOR A SINGLE BYTE &

15600	DEF* FNSHO.STG%(START%,STG.LEN%) &
	\ FNSHO.STG% = 0% &
		!IT IS DIRTY NOT TO DEFINE SOME VALUE TO A FUNCTION. &
	\ WHILE STG.LEN% > 0% &
	&
	\	BYT.CNT% = BYTS.PER.LINE% &
			!TRY TO FIND A COUPLE OF BYTES OF STUFF. &
	\	BYT.CNT% = STG.LEN% IF STG.LEN% < BYT.CNT% &
			! PRINT NO MORE THAN WE HAVE AVAILABLE &
	\	STG.LEN% = STG.LEN% - BYT.CNT% &
			!CALCULATE BYTES REMAINING FOR NEXT PASS &
	\	Z%(I%) = FNB%(START%+I%-1%) FOR I% = 1% TO BYT.CNT% &
			!PUT THIS BATCH OF BYTES IN Z%() &
	\	START% = START% + BYT.CNT% &
			! POINT TO THE NEXT CHUNK FOR NEXT TIME &
	\	PRINT #OUT.FIL%, FNPAD.STG$("",4%); &
			!ENSURE BLANKS IN THE LEFT MARGIN &
	\ 	PRINT #OUT.FIL%, FNPAD.N$(Z%(I%),4%); &
		    FOR I%=1% TO BYT.CNT% &
			!PRINT THE NUMERIC REPRESENTATION OF THE BYTES &
	\	PRINT #OUT.FIL%, TAB(TAB.PLACE%);A.ST$; &
	\	FOR I% = 1% TO BYT.CNT% &
			!PRINT THE CHARS, ONE AT A TIME &
	&
	\		IF Z%(I%) > 31% AND Z%(I%) < 127% THEN &
				!IF IT IS A PRINTABLE CHAR, THEN &
	&
				PRINT #OUT.FIL%, CHR$(Z%(I%)); &
			ELSE	PRINT #OUT.FIL%, D.OT$; &

15610		NEXT I% &
	\ 	PRINT #OUT.FIL%, A.ST$;CR.LF$; &
			!DOUBLE SPACE HERE &
	\ NEXT &
		!TAKE ANOTHER BYTE OF THE STRING &

15620	FNEND &

15650	DEF* FNNO.NOT$(I%) &
	\ FNNO.NOT$="" &
	\ FNNO.NOT$="not " UNLESS I% &
	\ FNEND &

15700	DEF* FNERR.MSG$(E%)= &
	CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(E%)),3%),4%) &
	! EXTRACT AN ERROR MESSAGE. &

15800	DEF* FNPAD.STG$(STG$,S.LEN%)=SPACE$(S.LEN%-LEN(STG$))+STG$ &
	! LEFT PAD A STRING WITH SPACES. &

15900	DEF* FNFIL.NAM$(OLD.STG$,MRG.STG$,DSK.FLG%) &
		! FUNCTION TO MERGE FILESPEC WITH DEFAULTS &
	\ FNFIL.NAM$ = "" &
		! SET A REASONABLE INITIAL VALUE. &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+OLD.STG$) TO Z% &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+MRG.STG$) TO Z1% &
		! PARSE THE OLD AND MERGE STRINGS &

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
		D.OT$+RAD$(Z%(11%)+SWAP%(Z%(12%))) IF NEW.FS1% AND 512% &
	\ NEW.STG$ = NEW.STG$ + &
		"<"+NUM1$(Z%(22%))+">" IF NEW.FS1% AND 2048% &
	\ NEW.STG$ = NEW.STG$ + &
		"/MO:"+NUM1$( Z%(17%)+SWAP%(Z%(18%)) AND 32767%) &
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

16000	DEF* FNPAD.N$(VALUE%,LEN.GTH%) = &
		FNPAD.STG$(NUM1$(VALUE%),LEN.GTH%) &

16100	DEF* FNSPDA.REL$(S.TR$,OFFSET%,T.YPE%)= &
	S.TR$ + T.AB$ + FNPAD.STG$(FNVAR$(OFFSET%+SPDA%,T.YPE%),7%) + CR.LF$ &

16105	DEF* FNNUMBER$(LOC%,PAD%,T.YPE%) = &
	FNPAD.STG$(FNVAR$(LOC%,T.YPE%),PAD%) &
	&

16200	DEF* FNVAR$(LOC%,T.YPE%) &
	\ ON T.YPE% GO TO 16210, 16250 &
	&

16210	V$ = NUM1$(FNP%(LOC%))+P.ER$ &
	\ GO TO 16290 &
	&

16250	V$ = PAD.STRING$ &
	\  V$ = V$ + CVT%$(FNP%(LOC%+I%*2%)) &
		FOR I% = M1% TO 0% STEP -1% &
	\ V$ =  CVT$$(NUM$(CVT$F(V$)/SCAL.FACT),-2%) &
	\ V$ =  V$ + D.OT$ UNLESS INSTR(0%,V$,D.OT$) &
	&

16290	FNVAR$ = V$ &
	\ FNEND &
	&

16300	  DEF* FNBUF.PRT%(BUF.PTR%,LEN.GTH%) &
	\ FNBUF.PRT% = TRUE% &
	\ CUR.PTR% = BUF.PTR% &
	\ TOO.HIGH% = BUF.PTR% + LEN.GTH% &
	\ WHILE CUR.PTR% <= TOO.HIGH% &
		\ PRINT #OUT.FIL%, FNWORD$(CUR.PTR%-BUF.PTR%);"/ "; &
	&
		\ FOR I% = 1% TO 6% &
			\ IF CUR.PTR% <= TOO.HIGH% &
			  THEN &
				  Z%(I%*2%-2%) = FNB%(CUR.PTR%) &
				\ Z%(I%*2%-1%) = FNB%(CUR.PTR%+1%) &
				\ BYT.CNT% = I%*2% &
				\ PRINT #OUT.FIL%,FNWORD$(FNP%(CUR.PTR%));" "; &
				\ CUR.PTR% = CUR.PTR% + 2% &

16310		  NEXT I% &
	&
		\ PRINT#OUT.FIL%, TAB(55%);A.ST$; &
		\ FOR I% = 0% TO BYT.CNT%-1% &
			\ IF Z%(I%) > 31% AND Z%(I%) < 127% &
			  THEN &
				  PRINT #OUT.FIL%, CHR$(Z%(I%)); &
			  ELSE &
				  PRINT #OUT.FIL%, D.OT$; &

16320		  NEXT I% &
		\ PRINT #OUT.FIL%, A.ST$ &
	\ NEXT &
	\ PRINT #OUT.FIL%, F.RM$ &
	\ FNEND &

19000	! &
	! &
	! &
	!	E R R O R    T R A P    H A N D L I N G    R O U T I N E &
	! &
	! &
	! &

19005	IF ERR=11% THEN RESUME 19200 &
		! TRAP FOR CONTROL Z THEN EXIT NORMALLY &

19010	E$=FNERR.MSG$(ERR) &
	\ RESUME 19100 &

19100	PRINT IF CCPOS(0%) &
	\ PRINT E$ &

19200	CLOSE -PMD.CHN%, OUT.FIL% &
	\ GOTO 32767 &

20000	! &
	! &
	! &
	!	S T A N D A R D     F U N C T I O N S &
	! &
	! &
	! &

30000	! &
	! &
	! &
	!	C C L    E N T R Y &
	! &
	! &
	! &

30010	  ENTRY.TYP% = 1% &
	\ PMD.FIL$ = SYS(CHR$(7%)) &
	\ JUNK$ = SYS(CHR$(8%)) &
	\ I% = INSTR(0%,PMD.FIL$," ") &
	\ IF I%=0% &
	  THEN &
		  ENTRY.TYP% = 0% &
	  ELSE &
		  PMD.FIL$ = RIGHT(PMD.FIL$,I%+1%) &
		\ I% = INSTR(0%,PMD.FIL$,"=") &
		\ IF I%=0% &
		  THEN &
			  PMD.FIL$ = "_KB:="+PMD.FIL$ &

30020	GOTO 1000 &
		! SET ENTRY TYPE FOR CCL ENTRY GOTO PROGRAM START. &

31000	! &
	! &
	! &
	!	C H A I N    E N T R Y &
	! &
	! &
	! &

31010	  ENTRY.TYP%=2% &
	\ GOTO 1000 &
		! SET ENTRY TYPE FOR CHAIN ENTRY AND GOTO PROGRAM START. &

32700	! &
	! &
	! &
	!	P R O G R A M    C O M P L E T I O N &
	! &
	! &
	! &

32710	!	****	SELF-KILL, CHAIN TO, ETC. &

32766	DATA	"Integer scalar variables: "	,	1, &
		"Floating point scalar variables: ",	2, &
		"String scalar variables: "	,	4, &
		"Integer arrays: "		,	9, &
		"Floating point arrays: "	,	10, &
		"String arrays: "		,	12, &
		"Integer functions: "		,	17, &
		"Floating point functions: "	,	18, &
		"String functions: "		,	20 &

32767	END
