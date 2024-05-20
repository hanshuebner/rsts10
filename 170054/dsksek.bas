2 !		PROGRAM		: DSKSEK.BAS
3 !		VERSION		: V10.1
4 !		EDIT		: J
5 !		EDIT DATE	: 27-MAR-92
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

21	! VER/ED	EDIT DATE	REASON &
	! &
	!  9.5-01	25-Jun-87	Fix BADB.SYS retrieval lookup.  (VAM) &
	!  9.7-08	08-Mar-89	Fix for large DCS MSCP disks	(FEK) &
	! 10.0-01	21-Jul-89	Get right disk size all the time(FEK) &
	! 10.0-0C	04-Oct-89	Don't do cluster number calculation &
	!				when in block mode &

99!	&

300	! &

310	!CHANNEL #	USED FOR &

320	!  1 - 8        DISK EXERCISER FILES &

400 !	&

900	! &

920	DIM FSS.ARRAY%(30%), M%(30%), SYS.BADB%(161%) &
		! FILENAME STRING SCAN ARRAY. &
		! ARRAY FOR OTHER SYS CALLS. &
		! ARRAY TO HOLD DCN'S OF BADB.SYS CLUSTERS. &

1000	ON ERROR GOTO 19000 &
	\ TEMP$=SYS(CHR$(6%)+CHR$(-21%)) &
	\ RANDOMIZE &
	! Set error trap &
	! Permanently drop temporary privileges. &
	! Get a new set of random numbers. &

1010	I$="V10.1-J" &
	! SET UP VERSION/EDIT LEVEL. &

1020	PRINT "DSKSEK";CHR$(9%);I$;CHR$(9%); &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)),3%),4%) &
	! Print the header. &

1030	JOB.NO%=ASCII(SYS(CHR$(6%)+CHR$(9%)))/2% &
	! FIND OUR JOB NUMBER. &

1100	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Disk drive to test"; &
	\ INPUT LINE DEV.NAM$ &
	\ GOTO 32767 IF ASCII(DEV.NAM$)=27% &
	\ DEV.NAM$=CVT$$(DEV.NAM$,254%) &
	\ DEV.NAM$=FNCHEK.DEV$(DEV.NAM$,0%) &
	\ GOTO 1100 UNLESS LEN(DEV.NAM$) &

1110	OPEN DEV.NAM$ AS FILE 1%, MODE 128% &
	\ CHANGE SYS(CHR$(12%)) TO M% &
		! OPEN THE DISK AND GET LAST OPENED FILE INFO. &
	\ DEV.CLU.SIZE%=M%(21%) &
		! GET DEVICE CLUSTERSIZE OF DISK. &
	\ CLOSE 1% IF DEV.CLU.SIZE%<=16% &
	\ OPEN DEV.NAM$ AS FILE 1%, RECORDSIZE 512%+32767%+1% &
					IF DEV.CLU.SIZE%<=16% &
	\ CHANGE SYS(CHR$(12%)) TO M% IF DEV.CLU.SIZE%<=16% &
		! switch over to cluster mode if its a "small" disk &
		! and reset the status and the cluster mode size &
	\ K%=M%(19%)+SWAP%(M%(20%)) &
		! Get the value of STATUS for the OPEN. &
	\ MOUNTED%=((K% AND 1024%)<>0%) &
		! See if we were granted write access to the device - &
		!  If SO, then the disk is NOT mounted. &
		!  If NOT, then the disk IS mounted. &
	\ MAX.DCN=256.*M%(14%)+M%(13%)-1% &
	\ MAX.DCN=MAX.DCN+65536.*M%(4%) IF DEV.CLU.SIZE%>16% &
		! FIND THE MAXIMUM DEVICE CLUSTER NUMBER ON THE DISK. &
		! but get the biggest block number it its really big &
	\ IF MOUNTED% THEN &
		! If the disk is mounted, then &
		GOSUB 17000 &
			! Go get BADB.SYS information. &
		\ GOTO 32767 IF SUBERR% &
			! If something nasty happened, punt. &
		\ GOTO 1200 &
			! Otherwise, on with the show! &

1120	PRINT "%Device ";DEV.NAM$;" is not a mounted disk" &
	\ PRINT "%Bad block information cannot be determined" &
	\ print "%I/O to known bad blocks cannot be prevented" &
		! Say that we can't tell where the bad blocks are. &
		!  So, if we accidentally hit one during the DSKSEK run, &
		!  a number of "transfer errors" will be logged, and the run &
		!  may be aborted prematurely. &

1200	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Number of test iterations <30>"; &
	\ INPUT LINE T$ &
	\ GOTO 1100 IF ASCII(T$)=27% &
	\ T$=CVT$$(T$,254%) &
	\ T$="30" UNLESS LEN(T$) &
	\ ITER.NO=VAL(T$) &
	\ IF ITER.NO=-1. THEN &
		ITER.NO=1000000000000000000000000000000. &
	  ELSE	IF ITER.NO<=0. THEN &
			PRINT IF CCPOS(0%) &
	\		PRINT "?Illegal number of iterations specified" &
	\		GOTO 1110 &

1300	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Detach <No>"; &
	\ INPUT LINE T$ &
	\ GOTO 1200 IF ASCII(T$)=27% &
	\ IF ASCII(CVT$$(T$,254%))=89% THEN &
		PRINT "Type ATTACH";JOB.NO%;"when SYSTAT shows job"; &
			JOB.NO%;"in HB state." &
	\	PRINT &
	\	T$=SYS(CHR$(6%)+CHR$(7%)) &
		! Detach if necessary. &

1700	FOR K=1. TO 1000.*ITER.NO &

1720	    DCN=RND*(MAX.DCN-1.)+1. &
		! PICK A DEVICE CLUSTER NUMBER AT RANDOM. &
	\   IF DEV.CLU.SIZE%>16% THEN &
		GET #1%, BLOCK DCN &
	    ELSE &
		DCN%=DCN+65536.*(DCN>32767.) &
		! PACK THE DCN INTO A 16-BIT UNSIGNED INTEGER. &
		! this will be ignored for large DCS disks because &
		! they are MSCP and don't have bad blocks &
	\	GOTO 1720 IF (DCN%-SYS.BADB%(J%)< &
			(BADB.CLU.SIZE%/DEV.CLU.SIZE%) AND &
			DCN%-SYS.BADB%(J%) > -1%) FOR J%=1% TO NO.SYS.BADB% &
		! IF THIS DCN LIES IN A BADB.SYS CLUSTER, GET ANOTHER &
		! RANDOM NUMBER. &
	\	GET #1%, RECORD DCN% &
		! TRY GETTING THE RANDOM DEVICE CLUSTER. &

1730	NEXT K &
	\ CLOSE 1% &
		! DO THE EXERCISE. &

2000	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "No"; IF BAD.BLOCK=0. &
	\ PRINT NUM1$(BAD.BLOCK);" possible"; IF BAD.BLOCK<>0% &
	\ PRINT " bad block detections in";1000.*ITER.NO; &
		"gets from ";DEV.NAM$ &
	\ GOTO 32767 IF BAD.BLOCK=0. &
	\ PRINT "Please check the system error log for a list" &
	\ PRINT "of possible bad blocks." &
	\ GOTO 32767 &

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
	\ OPEN TEMP$ FOR INPUT AS FILE 1%, mode 128% &
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

16190	ON ERROR GOTO 19000 &
	\ FNCHEK.DEV$=TEMP$ &
	\ FNEND &

17000	! &
	! &
	! &
	!	L O O K    F O R    B A D B . S Y S &
	! &
	! &
	! &
	ON ERROR GOTO 17020 &
		! SET LOCAL ERROR TRAP. &
	\ SUBERR%=0% &
		! Pre-clear our subroutine error flag. &
	\ OPEN DEV.NAM$+"[0,1]BADB.SYS" FOR INPUT AS FILE 2% &
		! TRY TO OPEN THE BAD BLOCK FILE IF A RSTS DISK. &
	\ CHANGE SYS(CHR$(12%)) TO M% &
		! RETURN LAST OPENED FILE INFO. &
	\ BADB.CLU.SIZE%=M%(21%) &
		! CLUSTERSIZE OF BADB.SYS. &
	\ J%=M%(13%)+SWAP%(M%(14%)) &
		! FILESIZE OF BADB.SYS IN BLOCKS. &
	\ NO.SYS.BADB%=(J%+BADB.CLU.SIZE%-1%)/BADB.CLU.SIZE% &
		! NUMBER OF BADB.SYS CLUSTERS. &
	\ FOR I%=1% TO NO.SYS.BADB% &
	\    J%=(I%-1%)*BADB.CLU.SIZE%+1% &
		! J% = NUMBER (VBN) OF FIRST BLOCK IN BADB.SYS CLUSTER I% &
	\    M%(K%)=0% FOR K%=0% TO 30% &
		! Clear everything out first. &
	\    M%(0%)=30% &
	\    M%(1%)=6% &
	\    M%(2%)=-26% &
	\    M%(3%)=2% &
	\    M%(4%)=16% &
	\    M%(13%)=J% &
	\    M%(14%)=SWAP%(J%) &
		! SET UP TO FIND OUT WHERE BLOCK J% IS ON DISK. &
	\    CHANGE M% TO T$ &
	\    CHANGE SYS(T$) TO M% &
	\    SYS.BADB%(I%)=M%(3%)+SWAP%(M%(4%)) &
		! STORE THE DCN CONTAINING BLOCK J%. &
	\ NEXT I% &
		! NOW WE'VE LOCATED ALL THE CLUSTERS OF BADB.SYS. &

17010	ON ERROR GOTO 19000 &
	\ CLOSE 2% &
	\ RETURN &
		! RESET STANDARD ERROR TRAP AND RETURN. &

17020	PRINT "?Disk ";DEV.NAM$;" has an invalid file structure" &
	\ PRINT "?Please REBUILD it before attempting to use it again" &
	\ CLOSE 2% &
	\ SUBERR%=ERR &
	\ RESUME 17010 &
		! If the disk was mounted, all errors are fatal. &
		! Give a fatal-type message, close the file, &
		!  set SUBERR so our caller will know that something &
		!  evil happened, and exit. &

19000	! &
	! &
	!	E R R O R    H A N D L I N G    R O U T I N E &
	! &
	! &

19010	T$=RIGHT(CVT$$(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),4%),3%) &
		! RETURN ERROR MESSAGE. &
	\ PRINT IF CCPOS(0%) &
		! RETURN CARRIAGE. &
	\ IF ERR=52% AND ERL=1200% THEN &
		PRINT "?Illegal number of iterations selected." &
	\	RESUME 1200 &
			! TRAP ILLEGAL NUMBERS. &

19110	IF ERR = 13% AND ERL = 1720% THEN &
		BAD.BLOCK=BAD.BLOCK+1. &
	\	RESUME 1730 &
		! INCREMENT COUNT OF POSSIBLE BAD BLOCKS. &

19120	IF ERR = 14% AND ERL = 1720% THEN &
		PRINT T$;" - ";DEV.NAM$ &
	\ RESUME 32767 &
	!Prints error message. &

19130	IF ERR=11% AND (ERL=1100% OR ERL=1200% OR ERL=1300%) THEN &
		RESUME 32767 &
		! USER CTRL/Z TO PROMPT. &

19150	PRINT T$;" at line";ERL;"in DSKSEK ";I$ &
	\ GOTO 32767 &
		! UNEXPECTED ERROR. &

32767	END
