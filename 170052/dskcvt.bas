2!		PROGRAM		: DSKCVT.BAS
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
	&
	&
	&
	&

20	! &
	&
	&
	!	M O D I F I C A T I O N    H I S T O R Y &
	&
	&

30	! VER/ED	EDIT DATE	REASON &
	! V9.0-01	20-MAR-84	(KMF) Update to convert to RDS 1.2 &
	!				      Support password hashing &
	! V9.0-06	17-Aug-84	(JJT) Fix bug when disk is &
	!				      initially dismounted &
	! V9.0-08	30-Oct-84	(GPK) Fix privilege check &
	! V9.0-12	01-FEB-85	(PRL) Don't assign EXQTA privilege &
	!		19-Feb-85	(GPK) Test for ? in passwords &
	! V9.0-14	23-Apr-85	(GPK) Bugfix in SATT handling &
	! =================================================================== &
	! V9.2-02	01-Oct-85	(PRL) Allow passwd cnvrt on sys disk &
	! V9.2-03	11-Nov-85	(PRL) Delete p/w block in [0,*] acnts &
	! V9.2-03	12-Dec-85	(JJT) Fix bug in patch for [0,*] acnts &
	! =================================================================== &
	! V9.3-03	12-FEB-86	(PRL) Restore "missing" PCS% line &

980	!	D I M E N S I O N   S T A T E M E N T S &
	&
	&
	DIM 	GFDBLK%(255), GFDTOP%(255), TMPDCN%(256), &
		OLDMFDDCN%(7), NEW11DCN%(7), ENTRY%(7), ENTRY$(7), &
		DIRLINK%(1736), DIRPPN%(1736), USERS%(255), &
		GFDSEGS%(255), CLUMAP%(7), FIRQB%(30), &
		MASK1%(8), MASK2%(8), ATTR%(13), &
		PRIVS$(2), QUOTAS$(1) &
	&
\	DIM #1, M%(3583,7) &
		! Define old MFD as a virtual array &

1000	!	M A I N   P R O G R A M &
	&
	&
	ON ERROR GOTO 19000 &
		!SET UP STANDARD ERROR TRAP &

1010	I$="V10.1-A" &
		! SET UP VERSION/EDIT # &

1020	PRINT "DSKCVT "; I$; "   "; FNERROR$(0%) &
		! PRINT THE SYSTEM HEADER. &

1030	CHR.6$ = CHR$(6%) &
\	CHR.0$ = CHR$(0%) &
\	CHR.00$ = CHR.0$ + CHR.0$ &
		! Define useful constants for SYS calls &
\	MAX.REV% = SWAP%(1%) + 2% &
		! Define max disk rev level supported = 1.2 &
\	PRIV.SYS$ = CHR.6$+CHR$(32%)+CHR$(1%)+STRING$(3%,0%) &
		! Define priv name to bit SYS call string &
\	DISMOUNT.SYS$ = CHR.6$+CHR$(3%)+CHR$(2%)+STRING$(19%,0%) &
		! Define dismount a disk pack SYS call string &
\	MOUNT.SYS$ = CHR.6$+CHR$(3%)+STRING$(14%,0%)+ &
			CVT%$(SWAP%(-8704%))+STRING$(4%,0%) &
		! Define mount a disk pack SYS call string &
		!	-12800% = -32768% (set mode bits defined below) &
		!		+  16384% (PRIVATE) &
		!		+   4096% (override READ-ONLY) &
		!		+   2048% (NOSHARE) &
		!		+   1024% (lookup pack ID) &
		!		+    512% (Write access to 0.0) &
\	UNLOCK.SYS$ = CHR.6$+CHR$(3%)+CHR$(6%)+STRING$(19%,0%) &
		! Define UNLOCK a disk pack SYS call string &

1040	DEV$ = "" &
\	A$=SYS(CHR.6$+CHR$(-21%)) &
\	IF NOT FNPRV%("ALL") THEN &
		PRINT &
\		PRINT "?All privileges required" &
\		GOTO 32767 &
	! Init disk device name &
	! Permanently drop any temp privs &
	! Check if user has all privileges &
	! If not, display error and exit &

1100	PRINT "?Invalid disk name" &
		IF LEN(DEV$) &
\	PRINT &
\	INPUT "Disk to convert"; DEV$ &
\	DEV$=CVT$$(DEV$,-2%) &
\	IF LEN(DEV$)=0% THEN &
		PRINT "?Disk name required" &
\		GOTO 1100 &
		! Display error msg if non-null device name &
		! Prompt for disk to convert &
		! Trim user's response &
		! If null, tell no defaults and re-prompt &

1110	DEV$ = DEV$ + ":" &
		UNLESS INSTR(LEN(DEV$),DEV$,":") &
\	A$=SYS(CHR.6$+CHR$(-10%)+DEV$) &
\	CHANGE A$ TO FIRQB% &
\	S1% = FIRQB%(29%)+SWAP%(FIRQB%(30%)) &
\	GOTO 1100 IF S1% < 0% &
\	DEV$ = "" &
\	IF (STATUS AND 255%) <> 0% THEN &
		PRINT "?Device must be disk" &
\		GOTO 1100 &
		! Append trailing colon to name if none found &
		! FSS the device name and save flag word 2 &
		! Re-prompt (with error msg) if untranslated logical &
		! Null DEV$ variable to allow re-prompting w/o error msg &
		! If device not a disk, then say so and re-prompt &

1120	IF FIRQB%(26%) = 0% THEN &
		PRINT "?Unit number required" &
\		GOTO 1100 &
		! Check for explicit unit no. &
		! Display error & reprompt if none &

1130	DEV.FSS$=MID(A$,23%,4%) &
\	DSK$ = "_" + LEFT(DEV.FSS$,2%) + NUM1$(FIRQB%(25%)) + ":" &
		! Save FSS devnam and devunt as a string &
		! Build complete device name (_ddn:) &
\	SYS.DSK% = 0% &
\	CHANGE SYS(CHR.6$+CHR$(-25%)+CHR$(-4%)+STRING$(19%,0%)+DEV.FSS$) &
		TO FIRQB% &
\	SYS.DSK% = (FIRQB%(22%) = 1%) &
		! Init as not system disk &
		! Read disk pack attributes &
		! Set flag if system disk &

1140	OPEN DSK$ FOR INPUT AS FILE 1% &
\	CHANGE SYS(CHR$(12%)) TO FIRQB% &
\	DEVSIZ%=FIRQB%(13%)+SWAP%(FIRQB%(14%)) &
\	DCS%=FIRQB%(21%) &
\	DEVNAM$=CHR$(FIRQB%(23%))+CHR$(FIRQB%(24%)) &
\	BSIZ%=BUFSIZ(1%) &
\	GET #1%, RECORD 1% &
\	FIELD #1%,	6% AS Z$,	! Skip header and MFD pointer &
			2% AS REV$,	! Rev level &
			2% AS PCS$,	! Pack clustersize &
			2% AS STS$,	! Pack status &
			2% AS ID1$,	! Pack ID (1st half) &
			2% AS ID2$	! Pack ID (2nd half) &
\	PCS% = SWAP%(CVT$%(PCS$)) &
\	PSTAT%=SWAP%(CVT$%(STS$)) &
\	CLURAT%=PCS%/DCS% &
\	PID$=CVT$$(RAD$(SWAP%(CVT$%(ID1$)))+RAD$(SWAP%(CVT$%(ID2$))),2%) &
\	REV%=SWAP%(CVT$%(REV$)) &
\	REV%=0% UNLESS PSTAT% AND 8192% &
\	LVL$ = NUM1$(SWAP%(REV%) AND 255%)+"."+NUM1$(REV% AND 255%) &
\	CLOSE 1% &
\	CUR.LVL% = (LVL$ = "1.2") &
		! Open the disk non-file structured &
		! Save assorted attributes &
		! Build disk revision level string ("x.x") &
		! Set flag if at current rev level &

1150	PRINT &
\	PRINT "** CURRENT DISK ATTRIBUTES **" &
\	PRINT &
\	PRINT "    Disk:     ";DSK$; &
\	PRINT " (System disk)"; &
		IF SYS.DSK% &
\	PRINT &
\	PRINT "   Label:     ";PID$ &
\	PRINT "   Level:     ";LVL$ &
\	PRINT &
\	IF REV% > MAX.REV% THEN &
		PRINT "?Disk level "; LVL$; " is not supported" &
\		GOTO 1100 &
		! Display some disk attributes &
		! Ensure level not larger than max level supported &
		! If so, complain and re-prompt &

1160	CNVRT.NOUSER%,CNVRT.NOLOOKUP% = 0% &
\	IF CUR.LVL% THEN &
		PRINT "Disk is already at current level" &
	ELSE	IF NOT SYS.DSK% THEN &
			CNVRT.NOUSER% = &
				FNYES.NO%("Convert accounts to NOUSER","NO") &
\			GOTO 1100 IF CTRL.Z% &
		! Init for no conversion &
		! If at current level, &
		!	Display message to say so, &
		! Else	If not the system disk, &
		!		Ask to convert accounts to NOUSER accounts &
		!		Re-prompt for new disk if CTRL/Z &

1170	IF NOT CNVRT.NOUSER% THEN &
		CNVRT.NOLOOKUP% = &
			FNYES.NO%("Convert passwords to NOLOOKUP","YES") &
\		GOTO 1100 IF CTRL.Z% &
		! If not converting to NOUSER accounts, &
		!	Prompt to convert passwords to NOLOOKUP &
		!	Re-prompt for new disk if CTRL/Z &

1180	IF CUR.LVL% THEN &
		IF NOT CNVRT.NOLOOKUP% THEN &
			GOTO 1100 &
		! If already at current level, &
		!	If no password conversion, &
		!		Skip to reprompt &

1190	DISPLAY% = FNYES.NO%("Display accounts","YES") &
\	GOTO 1100 IF CTRL.Z% &
		! Prompt if user wants to display account messages &
		! Re-prompt for new disk if CTRL/Z &
	&
\	Z% = FNYES.NO%("Proceed (YES or NO)","") &
\	GOTO 1100 IF CTRL.Z% OR NOT Z% &
		! Prompt to proceed &
		! Re-prompt for device if CTRL/Z or NO answer &

1200	A$=SYS(DISMOUNT.SYS$+DEV.FSS$) &
		UNLESS SYS.DSK% &
\	PSTAT%=PSTAT% AND 32767% &
		! Dismount the disk to make sure we can have it &
		!	Unless the system disk, &
		! Show the pack not dirty (clear PSTAT% sign bit) &

1210	IF PSTAT% < 0% THEN &
		PRINT "?Disk pack needs rebuilding" &
\		GOTO 1100 &
		! Check if the pack needs rebuilding &
		! If so, tell user and reprompt &

1220	IF NOT SYS.DSK% THEN &
		A$ = SYS(MOUNT.SYS$+DEV.FSS$) &
		! If not the system disk, &
		!	Mount it &

1230	CURR.DATE% = PEEK(512%) &
\	CURR.TIME% = PEEK(514%) &
		! Save current date and time &
		! (For loading date/time attribute blockette) &

1240	IF NOT SYS.DSK% THEN &
		GOSUB 10000 IF LVL$ = "0.0" &
\		GOSUB 11000 IF LVL$ = "1.1" &
		! If not the system disk, &
		!	Do 0.0 -> 1.1 conversion (10000) if level 0.0 &
		!	Do 1.1 -> 1.2 conversion (11000) if level 0.0 or 1.1 &

1250	GOSUB 12000 IF CNVRT.NOLOOKUP% &
		! Do LOOKUP -> NOLOOKUP password conversion if requested &

1260	PRINT IF CCPOS(0%) &
\	PRINT &
\	PRINT "Disk "; DSK$; " conversion completed on "; &
		DATE$(0%); " at "; TIME$(0%) &
\	GOTO 32700 &
		! Display completion message &
		! Exit program &

10000	!	C O N V E R T   L E V E L   0 . 0   T O   1 . 1 &
	&
	&
	PRINT IF CCPOS(0%) &
\	PRINT &
\	PRINT "Converting disk "; DSK$; " to level 1.1 ..." &
		! Display start message &
\	MFDLNK% = 0% &
		! Init 1st MFD link &
\	BLKTS% = 8% &
\	BLKTS% = 6% IF CNVRT.NOUSER% &
		! Define no. blockettes to allocate &
		! Subtract two if converting to NOUSER accounts &
		! ***** CHANGE THIS FOR NEW DISK REV LEVEL ***** &
		! ************** CANNOT EXCEED 11 ************** &
\	KILL DSK$+"[1,1]NEW11.DAT" &
		! Delete any work file still around &

10010	KILL DSK$+"[1,1]MFDGFD.DAT" &
		! Delete any MFD/GFD work file still around &

10020	OPEN DSK$+"[1,1]" FOR INPUT AS FILE 1%, MODE 8192% &
\	MFDCLU%=M%(31%,0%) &
\	MFDSIZ%=0% &
\	FOR I%=1% TO 7% &
\		OLDMFDDCN%(I%)=M%(31%,I%) &
\		MFDSIZ%=MFDSIZ%+MFDCLU% IF OLDMFDDCN%(I%) &
\	NEXT I% &
	&
\	OPEN DSK$+"[1,1]NEW11.DAT" FOR OUTPUT AS FILE 2%, &
			CLUSTERSIZE MFDCLU%, &
			FILESIZE MFDSIZ%, &
			MODE 1536% &
	&
\	CLOSE 1% &
\	OPEN DSK$+"[1,1]" FOR INPUT AS FILE 1%, MODE 8192% &
\	NEW11%=FNLINK%(M%(0%,0%),MFDCLU%) &
\	A$=RAD$(M%(NEW11%,1%))+RAD$(M%(NEW11%,2%))+RAD$(M%(NEW11%,3%)) &
\	IF A$<>"NEW11 DAT" THEN &
		PRINT "??Work file not found in [1,1] -- aborting" &
\		GOTO 32700 &
		! Open the old MFD &
		! Get its clustersize and total size &
		! Open a work file to hold new copy of MFD &
		! Close/reopen old MFD to force writes to it &
		! Get its directory pointer &
		! Check that we have the right entry &

10030	RP%=FNLINK%(M%(NEW11%,7%),MFDCLU%) &
\	NEW11DCN%=M%(RP%,1%) &
\	NEW11MAP$=CVT%$(SWAP%(MFDCLU%)) &
\	FOR I%=1% TO 7% &
\		NEW11DCN%(I%)=M%(RP%,I%) &
\		NEW11MAP$=NEW11MAP$+CVT%$(SWAP%(NEW11DCN%(I%))) &
\	NEXT I% &
	&
\	NEW11LBL$=CVT%$(-1%)+STRING$(8%,0%)+CVT%$(257%) &
		+MID(SYS(CHR.6$+CHR$(-10%)+"UFD"),7%,2%) &
\	FIELD #2%, 2% AS A$, 14% AS LBL$, 480% AS A$, 16% AS CLUMAP$ &
\	FOR I%=1% TO MFDSIZ% &
\		GET #513%, RECORD I% &
\		LSET LBL$=NEW11LBL$ IF I%=1% &
\		LSET CLUMAP$=NEW11MAP$ &
\		PUT #2%, RECORD I% &
\	NEXT I% &
	&
\	CLOSE 2% &
\	GFDBLK%(I%)=0% FOR I%=0% TO 255% &
\	PPNS%=0% &
\	PRINT "Sorting PPNs ..." &
		! Get retrieval data of new [1,1] workfile &
		! Build new [1,1] cluster MAP &
		! Copy data from old MFD to [1,1] workfile &
		! (Updating the cluster map in the process) &
		! Initialize count of PPNs found &

10040	B%=FNLINK%(MFDLINK%,MFDCLU%) &
\	IF (M%(B%,4%) AND 192%)=64% &
	THEN	PPNS%=PPNS%+1% &
\		DIRLINK%(PPNS%)=B% &
\		DIRPPN%(PPNS%)=M%(B%,1%) &
		! Save each account and its position in the MFD &

10050	MFDLINK%=M%(B%,0%) AND -16% &
\	GOTO 10040 IF MFDLINK% &
\	FOR I%=1% TO PPNS%-1% &
\		FOR J%=I%+1% TO PPNS% &
\			IF (DIRPPN%(I%) EQV 32767%) > (DIRPPN%(J%) EQV 32767%) &
			THEN	T%=DIRPPN%(I%) &
\				DIRPPN%(I%)=DIRPPN%(J%) &
\				DIRPPN%(J%)=T% &
\				T%=DIRLINK%(I%) &
\				DIRLINK%(I%)=DIRLINK%(J%) &
\				DIRLINK%(J%)=T% &

10060		NEXT J% &
\	NEXT I% &
\	USERS%=1% &
\	GROUP%=DIRPPN%(1%) AND -256% &
\	FOR I%=2% TO PPNS% &
\		IF GROUP%=(DIRPPN%(I%) AND -256%) &
		THEN	USERS%=USERS%+1% &
		ELSE	USERS%(SWAP%(GROUP%))=USERS% &
\			USERS%=1% &
\			GROUP%=DIRPPN%(I%) AND -256% &

10070	NEXT I% &
\	USERS%(SWAP%(GROUP%))=USERS% &
\	MAXUSERS%=0% &
\	MAXUSERS%=USERS%(I%) IF USERS%(I%)>MAXUSERS% &
		FOR I%=0% TO 255% &
\	NUMBLKS%=BLKTS%*MAXUSERS%+1% &
\	GFDCLU%=PCS% &
\	GFDCLU%=4% IF PCS%<4% &
\	GFDCLU%=GFDCLU%*2% WHILE (7%*GFDCLU%-2%)*31% < NUMBLKS% &
\	OPEN DSK$+"[1,1]MFDGFD.DAT" FOR OUTPUT AS FILE 2%, &
		CLUSTERSIZE GFDCLU%, MODE 1536% &
\	FIELD #2%, 512% AS Z$ &
\	LSET Z$=STRING$(512%,0%) &
\	FIELD #2%, 2% AS F$, 2% AS F1$, 8% AS A$, 2% AS P$, 2% AS ID$ &
\	LSET F$=CVT%$(256%) &
\	LSET F1$=CVT%$(-1%) &
\	LSET P$=CVT%$(SWAP%(-1%)) &
\	LSET ID$=MID(SYS(CHR.6$+CHR$(-10%)+"MFD"),7%,2%) &
\	PUT #2% &
\	LSET Z$=STRING$(512%,0%) &
\	PUT #2% FOR I%=2% TO GFDCLU% &
\	MFDLINK%=M%(0%,0%) &
\	TMPBLK%=GFDCLU%+1% &
\	MFDPOS%=1% &
		! Sort PPNs in ascending order &
		! Compute number of users in each group &
		! Compute largest users/group count &
		! From that, compute number of blockettes needed (+ GFD label) &
		! Compute MFD/GFD clustersize to use &
		! Adjust clustersize to ensure largest group fits &
		! Adjust clustersize to be >= largest UFD clustersize &
		! Open the work file (to become new MFD/GFD) (first file mode) &
		! Zero the new MFD cluster &
		! Get first MFD link and MFD clustersize &
		! Initialize pointers &
		! Initialize PPN table pointer &

10080	B%=DIRLINK%(MFDPOS%) &
\	UFDDCN%=M%(B%,7%) &
\	PPN%=M%(B%,1%) &
\	UFDDCN%=NEW11DCN% IF PPN%=257% &
\	USER%=PPN% AND 255% &
\	GROUP%=SWAP%(PPN%) AND 255% &
\	GFDBLK%=GFDBLK%(GROUP%) &
\	GOTO 10100 IF GFDBLK% &
		! Interpret link to entry &
		! Look up status &
		! Skip if marked for delete or not a UFD &
		! Pick up starting DCN of UFD &
		! Pick up PPN &
		! Use new [1,1] workfile DCN if we're working on [1,1] &
		! Announce account being processed &
		! Get user and group numbers &
		! Get the starting position of this group in the work file &
		! Skip ahead if group already encountered before &

10090	GFDBLK%,GFDBLK%(GROUP%)=TMPBLK% &
\	USERS%=USERS%(GROUP%) &
\	BLOCKS%=(USERS%*BLKTS%)/31%+3% &
\	CLUSTERS%=(BLOCKS%+GFDCLU%-1%)/GFDCLU% &
\	BLOCKS%=CLUSTERS%*GFDCLU% &
\	TMPBLK%=TMPBLK%+BLOCKS% &
\	FIELD #2%, 512% AS Z$ &
\	LSET Z$=STRING$(512%,0%) &
\	FIELD #2%, 2% AS F$, 2% AS A1$, 8% AS A$, 2% AS P$, 2% AS ID$ &
\	LSET F$=CVT%$(256%) &
\	LSET F1$=CVT%$(-1%) &
\	LSET P$=CVT%$(SWAP%(PPN% OR 255%)) &
\	LSET ID$=MID(SYS(CHR.6$+CHR$(-10%)+"GFD"),7%,2%) &
\	PUT #2%, RECORD GFDBLK% &
\	LSET Z$=STRING$(512%,0%) &
\	PUT #2%, RECORD I% FOR I%=GFDBLK%+1% TO GFDBLK%+BLOCKS%-1% &
\	GFDTOP%(GROUP%)=16% &
\	GFDSEGS%(GROUP%)=CLUSTERS% &
		! Set up starting position of this group &
		! Compute amount of space needed for this group &
		! Advance the work file pointer &
		! Initialize the GFD label &
		! Clear out this GFD's part &
		! Initialize the next free blockette pointer &
		! Save the number of allocated clusters &

10100	GET #2%, RECORD GFDBLK%+2% &
\	FIELD #2%, USER%*2% AS A$, 2% AS A$ &
\	IF CVT$%(A$) THEN &
		PRINT "%Duplicate PPN "; FNPPN$(PPN%); " - ignored" &
\		GOTO 10145 &
		! Read the name entry link table &
		! Field the slot for this UFD &
		! Skip account if it's a duplicate &

10110	SLOT%=GFDTOP%(GROUP%) &
\	LSET A$=CVT%$(SWAP%(FNBUILDLINK%(SLOT%,GFDCLU%))) &
\	PUT #2%, RECORD GFDBLK%+2% &
\	GET #2%, RECORD GFDBLK%+1% &
\	LSET A$=CVT%$(SWAP%(UFDDCN%)) &
\	PUT #2%, RECORD GFDBLK%+1% &
\	A%=FNGET.ENTRY%(SLOT%) &
\	ENTRY%(I%)=M%(B%,I%) FOR I%=1% TO 5% &
\	ACTSLOT%=FNNEXT.SLOT%(SLOT%) &
\	DATSLOT%=FNNEXT.SLOT%(ACTSLOT%) &
\	PASSLOT%=FNNEXT.SLOT%(DATSLOT%) &
\	ENTRY%(0%)=FNBUILDLINK%(DATSLOT%,GFDCLU%) &
\	ENTRY%(6%)=FNBUILDLINK%(ACTSLOT%,GFDCLU%) &
\	ENTRY%(7%)=UFDDCN% &
\	PAS$=CVT$$(RAD$(ENTRY%(2%))+RAD$(ENTRY%(3%)),-2%) &
\	PASSLOT%=0% IF CNVRT.NOUSER% &
\	ENTRY%(2%),ENTRY%(3%)=0% &
\	A%=FNPUT.ENTRY%(SLOT%) &
		! Update link table &
		! Update DCN pointer table &
		! Get the name entry's blockette &
		! Copy over the data &
		! Allocate slots for accting, date/time, and attr blockettes &
		! Update UFD DCN pointer in case we're working on [1,1] &
		! Update links to those two &
		! Convert password to ASCII &
		! Clear out old password field &
		! Don't create passwd blockette if converting to NOUSER accts &
		! Write the name blockette &

10120	A%=FNGET.ENTRY%(ACTSLOT%) &
\	ATR%=FNLINK%(M%(B%,6%),MFDCLU%) &
\	ENTRY%(I%)=M%(ATR%,I%) FOR I%=1% TO 7% &
\	ENTRY%(0%)=1% &
\	A%=FNPUT.ENTRY%(ACTSLOT%) &
\	A%=FNGET.ENTRY%(DATSLOT%) &
\	ENTRY%(I%)=0% FOR I%=0% TO 3% &
\	ENTRY%(1%)=4% &
\	ENTRY%(4%),ENTRY%(6%) = CURR.DATE% &
\	ENTRY%(5%),ENTRY%(7%) = CURR.TIME% &
\	ENTRY%(0%)=FNBUILDLINK%(PASSLOT%,GFDCLU%) IF PASSLOT% &
\	A%=FNPUT.ENTRY%(DATSLOT%) &
		! Get the accounting entry's blockette &
		! Copy the data over &
		! Make sure its link is cleared &
		! Write the accounting blockette &
		! Get the date/time recording blockette &
		! Zero it out, and set the type &
		! Load account creation date/time &
		! Load last password change date/time &
		! Set the link to the password blockette if we use one &

10130	IF PASSLOT%=0% &
	THEN	GFDTOP%(GROUP%)=FNNEXT.SLOT%(DATSLOT%) &
	ELSE	A%=FNGET.ENTRY%(PASSLOT%) &
\		FIELD #2%, BLKOFFSET% AS A$, 2% AS L$, &
			   1% AS TYP$, LEN(PAS$) AS P$ &
\		LSET L$=CVT%$(256%) &
\		LSET TYP$=CHR$(3%) &
\		LSET P$=PAS$ &
\		PUT #2%, RECORD BLKNUM% &
\		GFDTOP%(GROUP%)=FNNEXT.SLOT%(PASSLOT%) &
			! If no password blockette, set new top of GFD &
			! Else get the attribute entry's blockette &
			! Field the data &
			! Mark as in use &
			! Set type = password &
			! Load the password &
			! Write it back &
			! Update top of GFD data = next available slot &
	&

10140	IF DISPLAY% THEN &
	TEXT$ = "" &
\	TEXT$ = " to NOUSER account" &
		UNLESS PASSLOT% &
\	PRINT "Account "; FNPPN$(PPN%); " converted"; TEXT$ &
		! If display requested, &
		!	Show NOUSER completion text if no password blockette &
		!	Display converted message &

10145	MFDPOS%=MFDPOS%+1% &
\	GOTO 10080 IF MFDPOS% <= PPNS% &
\	CLOSE 1% &
\	OPEN DSK$+"[1,1]" FOR INPUT AS FILE 1%, MODE 8192% &
\	WORK%=FNLINK%(M%(0%,0%),MFDCLU%) &
\	A$=RAD$(M%(WORK%,1%))+RAD$(M%(WORK%,2%))+RAD$(M%(WORK%,3%)) &
\	IF A$<>"MFDGFDDAT" &
	THEN	PRINT "??Work file not found in [1,1] -- aborting" &
\		GOTO 32700 &
		! Advance pointer to next PPN table entry &
		! If any left, go back to process another &
		! Close/reopen old MFD to force writes to it &
		! Get link to first entry in MFD &
		! Verify that it's our work file &
		! Abort if it isn't (implies someone else accessing disk) &

10150	PRINT "Updating Group File Directories ..." &
\	C%=0% &
\	RP%=FNLINK%(M%(WORK%,7%),MFDCLU%) &
\	WHILE RP% &
\		FOR I%=1% TO 7% &
\			P%=M%(RP%,I%) &
\			GOTO 10160 UNLESS P% &
\			TMPDCN%(C%)=P% &
\			C%=C%+1% &
\		NEXT I% &
\		RP%=FNLINK%(M%(RP%,0%),MFDCLU%) &
\	NEXT &
		! Get link to first retrieval entry &
		! Loop through all retrieval entries &
		! Quit when we reach the end &
		! Store away a cluster number &
		! Advance the pointer &

10160	CLUMAP%(1%)=TMPDCN%(0%) &
\	A%=FNUPDATE.MAP%(1%,1%) &
\	FOR GROUP%=0% TO 255% &
\		BLK%=GFDBLK%(GROUP%) &
\		IF BLK% &
		THEN	RP%=BLK%/GFDCLU% &
\			CLUSTERS%=GFDSEGS%(GROUP%) &
\			CLUMAP%(I%)=TMPDCN%(I%-1%+RP%) FOR I%=1% TO CLUSTERS% &
\			A%=FNUPDATE.MAP%(CLUSTERS%,BLK%) &

10170	NEXT GROUP% &
\	GET #2%, RECORD 2% &
\	FOR I%=0% TO 255% &
\		BLK%=GFDBLK%(I%) &
\		IF BLK% &
		THEN	DCN1%=TMPDCN%(BLK%/GFDCLU%) &
\			FIELD #2%, I%*2% AS A$, 2% AS A$ &
\			LSET A$=CVT%$(SWAP%(DCN1%)) &

10180	NEXT I% &
\	PUT #2%, RECORD 2% &
\	CLOSE 1%,2% &
\	OPEN DSK$+"[1,1]NEW11.DAT" FOR INPUT AS FILE 1% &
\	PRINT "Updating Master File Directory ..." &
\	RP%=FNLINK%(M%(NEW11%,7%),MFDCLU%) &
\	M%(RP%,I%)=0% FOR I%=0% TO 7% &
\	MFDLINK%=FNLINK%(M%(0%,0%),MFDCLU%) &
\	PREV%=0% &
\	WHILE MFDLINK% &
\		A%=FNLINK%(M%(MFDLINK%,0%),MFDCLU%) &
\		IF MFDLINK%<>NEW11% AND (M%(MFDLINK%,4%) AND 64%)=0% &
		THEN	PREV%=MFDLINK% &
		ELSE	B%=FNLINK%(M%(MFDLINK%,6%),MFDCLU%) &
\			M%(PREV%,0%)=(M%(PREV%,0%) AND 15%) &
				OR (M%(MFDLINK%,0%) AND -16%) &
\			M%(MFDLINK%,I%)=0% FOR I%=0% TO 7% &
\			M%(B%,I%)=0% FOR I%=0% TO 7% &

10190		MFDLINK%=A% &
\	NEXT &
\	CLOSE 1% &
		! Update the cluster map of the new MFD &
		! Update the cluster map of each defined GFD &
		! Update the DCN pointer table in the MFD &
		! Close the work file &
		! Open new [1,1] file &
		! Deallocate the retrieval pointer for the new MFD workfile, &
		! And its name/acct entries &
		! Deallocate the name and acct. entries of all UFDs from [1,1] &
		! Close new [1,1] workfile &

10200	PRINT "Updating Storage Allocation Table ..." &
\	OPEN DSK$+"[0,1]" FOR INPUT AS FILE 1%, MODE 8192% &
\	SYSCLU%=M%(31%,0%) &
\	B%=FNLINK%(M%(0%,0%),SYSCLU%) &
\	WHILE B% &
\		A$=RAD$(M%(B%,1%))+RAD$(M%(B%,2%))+RAD$(M%(B%,3%)) &
\		GOTO 10210 IF A$="SATT  SYS" &
\		B%=FNLINK%(M%(B%,0%),SYSCLU%) &
\	NEXT &
\	PRINT "??File SATT.SYS not found in [0,1] -- aborting" &
\	GOTO 32700 &
		! Open the [0,1] UFD &
		! Save its clustersize &
		! Scan for SATT.SYS &
		! Abort if not found &

10210	SATDCN%=M%(FNLINK%(M%(B%,7%),SYSCLU%),1%) &
\	CLOSE 1% &
\	A$=SYS(DISMOUNT.SYS$+DEV.FSS$) &
		! Save starting DCN of SATT.SYS &
		! Close the UFD &
		! Dismount the pack &

10220	OPEN DSK$ AS FILE 2% &
\	GET #2%, RECORD 1% &
\	FIELD #2%, 2% AS F$, 2% AS F1$, 2% AS MDCN$, 2% AS PLVL$, &
		2% AS A$, 2% AS PSTAT$, 4% AS PID$, 496% AS Z$ &
\	LSET F$=CVT%$(256%) &
\	LSET F1$=CVT%$(-1%) &
\	LSET MDCN$=CVT%$(SWAP%(TMPDCN%(0%))) &
\	LSET PLVL$=CVT%$(257%) 	! Level 1.1 &
\	LSET PSTAT$=CVT%$(SWAP%(SWAP%(CVT$%(PSTAT$)) OR -24576%)) &
\	LSET Z$=STRING$(496%,0%) &
\	PUT #2%, RECORD 1% &
\	LVL$ = "1.1" &
		! Open the disk non-file-structured &
		! Get the pack label &
		! Field it &
		! Set the new pack label contents, clear out rest of block &
		! Note disk is marked dirty until we fix the SATT &
		! Write back the label &
		! Show disk is level 1.1 now &

10230	A%=FNUPDATE.SAT%(OLDMFDDCN%(I%),MFDCLU%/DCS%,0%) &
		IF OLDMFDDCN%(I%) &
			FOR I%=1% TO 7% &
\	A%=FNUPDATE.SAT%(1%,PCS%/DCS%,-1%) &
\	IF (DEVNAM$="DB" OR DEVNAM$="DR") THEN &
		IF (DEVSIZ%=-22587% OR DEVSIZ%=-22953%) THEN &
			A%=FNUPDATE.SAT%(-23736%,(DEVSIZ%-(-23736%)),0%) &
		! Deallocate the old MFD &
		! Allocate the pack label &
		! Deallocate the newly available blocks on RP04/5/6 &

10240	GET #2%, RECORD 1% &
\	FIELD #2%, 10% AS A$, 2% AS PSTAT$ &
\	LSET PSTAT$=CVT%$(SWAP%(SWAP%(CVT$%(PSTAT$)) AND 32767%)) &
\	PUT #2%, RECORD 1% &
\	CLOSE 2% &
\	A$=SYS(MOUNT.SYS$+DEV.FSS$) &
		! Update the pack label to indicate the pack is now clean &
		! Remount the pack &

10250	RETURN &
		! Exit 0.0 -> 1.1 conversion &

11000	!	C O N V E R T   L E V E L   1 . 1   T O   1 . 2 &
	&
	&
	PRINT IF CCPOS(0%) &
\	PRINT &
\	PRINT "Converting disk "; DSK$; " to level 1.2 ..." &
		!Display start message &
	&
\	IF NOT CNVRT.NOUSER% THEN &
		! If not converting to NOUSER accounts, &
		CHANGE MID(SYS(PRIV.SYS$+"SYSMOD"),7%,8%) TO MASK1% &
		!	Build mask with SYSMOD bit set &
\		CHANGE MID(SYS(PRIV.SYS$+"EXQTA"),7%,8%) TO MASK2% &
		!	Build mask with EXQTA bit set &
\		FOR I% = 1% TO 8% &
\			MASK1%(I%) = NOT (MASK1%(I%) OR MASK2%(I%)) &
\			MASK2%(I%) = NOT MASK2%(I%) &
\		NEXT I% &
\		CHANGE MASK2% TO PRIVS$(0%) &
		!	Save privs mask for account [1,1] &
		!	(All privs except EXQTA) &
\		CHANGE MASK1% TO PRIVS$(1%) &
		!	Save privs mask for accounts [1,*] except [1,1] &
		!	(All privs except SYSMOD and EXQTA) &
\		PRIVS$(2%) = STRING$(8%,0%) &
		!	Save privs mask for all other (non [1,*]) accounts &
		!	(No privs) &
	&
\		QUOTAS$(0%) = STRING$(5%,255%) &
		!	Save non-disk quotas for [1,*] accounts &
		!	(JOB unlimited, RIB unlimited, MESSAGE unlimited) &
\		QUOTAS$(1%) = CHR$(255%)+CVT%$(SWAP%(3%))+CVT%$(SWAP%(12%)) &
		!	Save non-disk quotas for non-[1,*] accounts &
		!	(JOB unlimited, RIB 3, MESSAGE 12) &

11010	FOR ACT% = 1% UNTIL 0% &
\		TEXT$ = " converted" &
		! Do until no more accounts &
		!	Init completion text &

11020		TMP$ = SYS(CHR.6$+CHR$(14%)+CHR$(ACT%)+CHR$(SWAP%(ACT%)) &
			+STRING$(4%,0%)+CHR$(4%)+STRING$(13%,0%)+DEV.FSS$) &
\		CHANGE TMP$ TO FIRQB% &
\		PROJ% = FIRQB%(8%) &
\		PROG% = FIRQB%(7%) &
\		PPN.FSS$ = MID(TMP$,7%,2%) &
\		PPN% = SWAP%(CVT$%(PPN.FSS$)) &
		! Read accounting data &
		! (New style - return disk quota attributes) &
		! Save PPN as integers and strings &

11030		CHANGE MID(TMP$,8%,13%) TO ATTR% &
\		ATTR%(1%) = 0% &
\		ATTR%(1%) = -1% IF PROJ% = 1% &
\		CHANGE ATTR% TO ATTR$ &
\		TMP$ = FNWRITE.ATTR$(1%,ATTR$,PPN.FSS$,DEV.FSS$) &
\		GOTO 32700 IF FATAL% &
		! Copy disk quota attributes into array &
		! Init detached job quota to 0 &
		! Change to unlimited if [1,*] account &
		! Convert array to string &
		! Write disk quota attribute block &
		! Exit if fatal error &

11040		PASSWORD$ = CVT$$(FNREAD.ATTR$(3%,PPN.FSS$,DEV.FSS$),-2%) &
\		ATTR$ = FNREAD.ATTR$(4%,PPN.FSS$,DEV.FSS$) &
\		ATTR%(12%), ATTR%(13%) = -1% &
\		IF NOT CNVRT.NOUSER% THEN &
			IF PASSWORD$ = "??????" THEN &
				ATTR%(12%) = 1000% &
		\		ATTR%(13%) = SWAP%(1000%) &
		\		TEXT$ = TEXT$ + " to EXPIRED account" &
		! Save password &
		! Get date/time attribute blockette &
		! Set expiration date to NOEXPIRE &
		! If not converting to NOUSER account, &
		!	If password contains all ?'s, &
		!		Set account to EXPIRE &
		!		Change completion text to show expired &

11045		IF (ATTR%(6%) OR ATTR%(7%)) = 0% THEN &
			ATTR%(6%) = CURR.DATE% &
		\	ATTR%(7%) = SWAP%(CURR.DATE%) &
		\	ATTR%(8%) = CURR.TIME% &
		\	ATTR%(9%) = SWAP%(CURR.TIME%) &
		! If date of last password change is null, &
		!	Load current date into field &
		!	Load current time into field &

11050		IF (ATTR%(10%) OR ATTR%(11%)) = 0% THEN &
			ATTR%(10%) = CURR.DATE% &
		\	ATTR%(11%) = SWAP%(CURR.DATE%) &
		! If account creation date field is null, &
		!	Load current date into field &

11060		CHANGE ATTR% TO ATTR$ &
\		TMP$ = FNWRITE.ATTR$(4%,ATTR$,PPN.FSS$,DEV.FSS$) &
\		GOTO 32700 IF FATAL% &
		! Update date/time blockette &
		! Exit if fatal error &

11080		IF CNVRT.NOUSER% THEN &
			TMP$ = SYS(CHR.6$+CHR$(8%)+CHR.00$+PPN.FSS$+ &
				STRING$(16%,0%)+DEV.FSS$+CHR$(-1%)) &
\			TEXT$ = TEXT$ + " to NOUSER account" &
\			GOTO 11130 &
		! If converting to NOUSER account, &
		!	Delete any password blockette &
		!	Change completion text to show NOUSER account &
		!	Skip creating priv mask and non-disk quota block &

11090		IF PROJ% <> 1% THEN &
			IDX% = 2% &
		ELSE	IF PROG% <> 1% THEN &
				IDX% = 1% &
			ELSE	IDX% = 0% &
		! If account not in [1,*] group, &
		!	Use priv mask for non-[1,*] accounts &
		! Else	If not [1,1] account, &
		!		Use priv mask for [1,*] accounts exc [1,1] &
		!	Else	Use priv mask for account [1,1] &

11100		TMP$ = FNWRITE.ATTR$(2%,CHR.0$+PRIVS$(IDX%),PPN.FSS$,DEV.FSS$) &
\		GOTO 32700 IF FATAL% &
		! Create privilege mask attribute block &
		! Exit if fatal &

11110		IDX% = 1% &
\		IDX% = 0% &
			IF PROJ% = 1% &
\		TMP$ = FNWRITE.ATTR$(6%,QUOTAS$(IDX%),PPN.FSS$,DEV.FSS$) &
\		GOTO 32700 IF FATAL% &
		! Use non-priv'd job, RIB and message quotas &
		! Use priv'd quotas if [1,*] account &
		! Create non-disk quota attribute block &
		! Exit if fatal error &

11130		PRINT "Account "; FNPPN$(PPN%); TEXT$ &
			IF DISPLAY% &
		! Display converted message &
\	NEXT ACT% &
		! Do for all accounts &

11140	A$=SYS(DISMOUNT.SYS$+DEV.FSS$) &
		! Dismount the pack &

11150	OPEN DSK$ AS FILE 2% &
\	GET #2%, RECORD 1% &
\	FIELD #2%, 6% AS Z$, 2% AS PLVL$, 16% AS Z1$, Z% AS P1D2$, &
		486% AS Z2$ &
\	LSET P1D2$ = CVT%$(0%) &
\	LSET PLVL$=CVT%$(SWAP%(258%)) 	! Level 1.2 &
\	PUT #2%, RECORD 1% &
\	CLOSE 2% &
\	LVL$ = "1.2" &
		! Write back the label &
		! Show disk is level 1.2 now &

11160	A$=SYS(MOUNT.SYS$+DEV.FSS$) + SYS(UNLOCK.SYS$+DEV.FSS$) &
		! Mount and unlock the pack &

11170	RETURN &
		! Exit 1.1 -> 1.2 conversion &

12000	!	C O N V E R T   P A S S W O R D S   T O   N O L O O K U P &
	&
	&
	PRINT IF CCPOS(0%) &
\	PRINT &
\	PRINT "Converting disk ";DSK$;" passwords to NOLOOKUP ..." &
		! Open KB using MODE 16 to disable CTRL/C trapping &
		!  Display start message &

12020	FOR ACT% = 1% UNTIL 0% &
\		TEXT$ = " password converted" &
		! Do until no more accounts &
		!	Init completion text &

12030		CHANGE SYS(CHR.6$+CHR$(14%)+CHR$(ACT%)+CHR$(SWAP%(ACT%)) &
				+STRING$(4%,0%)+CHR$(9%)+STRING$(13%,0%) &
				+DEV.FSS$+STRING$(3%,0%)) &
		TO FIRQB% &
\		PROJ% = FIRQB%(8%) &
\		PROG% = FIRQB%(7%) &
\		PPN.FSS$ = CHR$(PROG%) + CHR$(PROJ%) &
\		PPN% = SWAP%(CVT$%(PPN.FSS$)) &
\		PASSWORD$ = CVT$$(RAD$(FIRQB%(9%)+SWAP%(FIRQB%(10%))) + &
				RAD$(FIRQB%(11%)+SWAP%(FIRQB%(12%))),2%) &
		!	Read accounting data old style &
		!	Save assorted PPN values &
		!	Save the account's password (trimmed) &

12040		If Proj% = 0% then &
			If Prog% <> 1% then &
				TMP$ = FNWRITE.ATTR$(3%,"",PPN.FSS$,DEV.FSS$) &
\				GOTO 32700 IF FATAL% &
\				TEXT$ = " is a NOUSER account" &
\				GOTO 12090 &
		!	If project 0, &
		!		If not [0,1], &
		!			Delete any password attr block &
		!			Exit if fatal error &
		!			Update text to show a NOUSER account &
		!			Skip to next account &

12050		TMP$ = FNREAD.ATTR$(4%,PPN.FSS$,DEV.FSS$) &
\		IF ATTR%(9%) AND 8% THEN &
			TEXT$ = " password is already NOLOOKUP" &
\			GOTO 12090 &
		!	Read date/time attribute block &
		!	Skip password conversion if account already NOLOOKUP &
		!	If already NOLOOKUP, &
		!		Set text for completion message &
		!		Skip converting account &

12060		IF LEN(PASSWORD$) = 0% THEN &
			TEXT$ = " is a NOUSER account" &
\			GOTO 12090 &
		!	If null password (NOUSER account), &
		!		Set text for completion message &
		!		Skip converting account &

12070		ATTR%(9%) = ATTR%(9%) OR 8% &
\		CHANGE ATTR% TO ATTR$ &
\		TMP$ = FNWRITE.ATTR$(4%,ATTR$,PPN.FSS$,DEV.FSS$) &
\		GOTO 32700 IF FATAL% &
		!	Set NOLOOKUP flag in block &
		!	Convert array to string for function call &
		!	Write date/time attribute block &

12080		IF INSTR(1%,PASSWORD$,"?") = 0% THEN &
			TMP$ = SYS(CHR.6$+CHR$(8%)+CHR.00$+PPN.FSS$+ &
				FNPAD$(PASSWORD$,16%)+DEV.FSS$+CHR$(-1%)) &
		ELSE	TMP$ = FNWRITE.ATTR$(3%,"?",PPN.FSS$,DEV.FSS$) &
\			GOTO 32700 IF FATAL% &
\			TEXT$ = " password is invalid (contains ?'s)" &
		!	If password has no ?'s in it, &
		!		Use change pw sys call to convert to NOLOOKUP &
		!	Else	Write "impossible" pw value to blockette &
		!		Exit if fatal error &
		!		Update converted text &

12090		PRINT "Account ";FNPPN$(PPN%); TEXT$ &
			IF DISPLAY% &
		!	Display completion message &

12100	NEXT ACT% &
		! Next account &

12110	RETURN &
	! Close KB channel &
	! Exit LOOKUP -> NOLOOKUP password conversion &

15000	!		F  U  N  C  T  I  O  N  S &
	&
	&
	&
	!	F N B U I L D L I N K % &
	&
	! CONVERT (BYTE) DIRECTORY ADDRESS INTO DIRECTORY LINK &
	&
	&
	DEF FNBUILDLINK%(SLOT%,CLU%)= &
		((SLOT%/(512%*CLU%))*512%)+ &
		(SLOT% AND (CLU%-1%)*512%)*8%+(SLOT% AND 496%) &

15100	!	F N L I N K % &
	&
	! CONVERT DIRECTORY LINK INTO DIRECTORY BLOCKETTE NUMBER &
	&
	&
	DEF FNLINK%(Q%,Q1%)= &
		(((Q% AND 3584%)/512%)*Q1%+ &
		(SWAP%(Q% AND -4096%)/16%))*32%+((Q% AND 496%)/16%) &

15200	!	F N P P N $ &
	&
	! CONVERT PPN TO A STRING &
	&
	&
	DEF FNPPN$(Q%) &
\	X1$ = "   " &
\	RSET X1$ = NUM1$(SWAP%(Q%)AND 255%) &
\	X2$ = "   " &
\	LSET X2$ = NUM1$(Q% AND 255%) &
\	FNPPN$ = "[" + X1$ + "," + X2$ + "]" &
\	FNEND &

15300	!	F N N E X T . S L O T % &
	&
	! RETURN NEXT DIRECTORY BLOCKETTE BYTE ADDRESS &
	&
	&
	DEF FNNEXT.SLOT%(SLOT%) &
\	SLOT%=SLOT%+16% &
\	SLOT%=SLOT%+16% IF (SLOT% AND 496%)=496% &
\	SLOT%=1536% IF SLOT%=512% &
\	FNNEXT.SLOT%=SLOT% &
\	FNEND &
		! ADVANCE TO NEXT BLOCKETTE &
		! SKIP CLUSTER MAP AREA &
		! SKIP SECOND AND THIRD BLOCK &
		! RETURN THE RESULT &

15400	!	F N G E T . E N T R Y % &
	&
	! GET A BLOCKETTE INTO ENTRY%(0..7) &
	&
	&
	DEF FNGET.ENTRY%(SLOT%) &
\	BLKNUM%=GFDBLK%+(SLOT%/512%) &
\	BLKOFFSET%=SLOT% AND 511% &
\	GET #2%, RECORD BLKNUM% &
\	FOR O%=0% TO 7% &
\		FIELD #2%, BLKOFFSET%+O%*2% AS A$, 2% AS ENTRY$(O%) &
\		ENTRY%(O%)=SWAP%(CVT$%(ENTRY$(O%))) &
\	NEXT O% &
\	FNEND &

15500	!	F N P U T . E N T R Y % &
	&
	! STORE ENTRY%(0..7) INTO A BLOCKETTE &
	&
	&
	DEF FNPUT.ENTRY%(SLOT%) &
\	BLKNUM%=GFDBLK%+(SLOT%/512%) &
\	BLKOFFSET%=SLOT% AND 511% &
\	FOR O%=0% TO 7% &
\		FIELD #2%, BLKOFFSET%+O%*2% AS A$, 2% AS ENTRY$(O%) &
\		LSET ENTRY$(O%)=CVT%$(SWAP%(ENTRY%(O%))) &
\	NEXT O% &
\	PUT #2%, RECORD BLKNUM% &
\	FNEND &

15600	!	F N U P D A T E . M A P % &
	&
	! LOAD THE CLUSTER MAP FOR AN MFD OR GFD &
	&
	! ARGUMENTS: &
	!	CNT%	NUMBER OF CLUSTERS &
	!	BLK%	STARTING BLOCK IN MFDGFD.DAT WORKFILE &
	&
	&
	DEF FNUPDATE.MAP%(CNT%,BLK%) &
\	MAXMAP%=CNT%*GFDCLU%-1% &
\	CLUMAP%(I%)=0% FOR I%=CNT%+1% TO 7% &
\	CLUMAP$=CVT%$(SWAP%(GFDCLU% EQV 32767%)) &
\	CLUMAP$=CLUMAP$+CVT%$(SWAP%(CLUMAP%(I%))) FOR I%=1% TO 7% &
\	FIELD #2%, 496% AS A$, 16% AS M$ &
\	GET #2%, RECORD BLK% &
\	LSET M$=CLUMAP$ &
\	PUT #2%, RECORD BLK% &
\	FOR O%=3% TO MAXMAP% &
\		GET #2%, RECORD BLK%+O% &
\		LSET M$=CLUMAP$ &
\		PUT #2%, RECORD BLK%+O% &
\	NEXT O% &
\	FNEND &

15700	!	F N U P D A T E . S A T % &
	&
	! UPDATE THE SATT (ASSUMED TO BE OPEN ON CHANNEL 2) &
	&
	! ARGUMENTS ARE: &
	!	DCN%	DEVICE CLUSTER NUMBER TO START AT &
	!	CLUSIZ%	SIZE OF ITEM (IN DEVICE CLUSTERS) &
	!	SETCLR%	-1 TO SET BIT (ALLOCATE), 0 TO CLEAR (DEALLOCATE) &
	&
	&
	DEF FNUPDATE.SAT%(DCN%,CLUSIZ%,SETCLR%) &
\	PCN%=DCN%-1% &
\	PCN%=PCN%/CLURAT% AND (32767%/(CLURAT%/2%)) IF CLURAT%>1% &
\	CURREC%=-1% &
\	FOR CNT%=1% TO CLUSIZ%/CLURAT% &
\		O%=(PCN% AND -8%)/8% AND 8191% &
\		B%=O% AND (BSIZ%-1%) &
\		REC%=O%/BSIZ%+SATDCN% &
\		BIT%=2%^(PCN% AND 7%) &
\		PUT #2%, RECORD CURREC% IF REC%<>CURREC% IF CURREC%<>-1% &
\		GET #2%, RECORD REC% IF REC%<>CURREC% &
\		CURREC%=REC% &
\		FIELD #2%, B% AS A$, 1% AS A$ &
\		LSET A$=CHR$((ASCII(A$) AND (NOT BIT%)) OR (BIT% AND SETCLR%)) &
\		PCN%=PCN%+1% &
\	NEXT CNT% &
\	PUT #2%, RECORD REC% &
\	FNEND &
		! COMPUTE PACK CLUSTER NUMBER FROM DEVICE CLUSTER NUMBER &
		! COMPUTE COUNT OF PACK CLUSTERS TO UPDATE (UNSIGNED) &
		! COMPUTE DCN OFFSET, BYTE IN BUFFER AND BIT IN BYTE &
		! WRITE OLD BUFFER BACK IF NEED BE &
		! GET NEW BLOCK IF NOT IN BUFFER ALREADY &
		! UPDATE IT &
		! ADVANCE CLUSTER NUMBER &
		! LOOP UNTIL DONE &
		! WRITE BACK THE BUFFER &

15800	!	F N P R V % &
	&
	! CHECK FOR PRIVILEGE &
	&
	&
	DEF FNPRV%(PRIV$) &
\	CHANGE SYS(CHR.6$+CHR$(32%)+CHR$(1%)+STRING$(3%,0%)+PRIV$) &
		TO FIRQB% &
\	FNPRV% = (FIRQB%(3%) = 0%) &
\	FNEND &
	! Check to see if job currently has privilege named &
	! If privileged then return -1% &
	! Else return 0% &

15900	!	F N Y E S . N O % &
	&
	! GET YES/NO ANSWER &
	&
	&
	DEF* FNYES.NO%(PROMPT$,DFLT$) &
\	CTRL.Z%,FNYES.NO% = 0% &
		! Init response as NO &
		! Init CTRL/Z flag (off) &

15910	PRINT PROMPT$; &
\	PRINT " <";DFLT$;">"; IF LEN(DFLT$) &
\	INPUT Z$ &
\	Z$ = CVT$$(Z$,-2%) &
\	Z$ = DFLT$ UNLESS LEN(Z$) &
\	Z$ = "??" UNLESS LEN(Z$) &
		! Display prompt &
		! Display <default> if any &
		! Get user's response &
		! Trim response &
		! Use default if null response &
		! Force invalid response if null and no default &
\	IF INSTR(1%,"YES",Z$) = 1% THEN &
		FNYES.NO% = -1% &
	ELSE	IF INSTR(1%,"NO",Z$) = 1% THEN &
			FNYES.NO% = 0% &
		ELSE	PRINT "?Please answer YES or NO" &
\			GOTO 15910 &
		! Return -1 if Y[ES] response &
		! Return 0 if N[O] response &
		! Else display error msg and re-prompt &
		! (CTRL/Z forces CTRL.Z% = -1%) &

15920	FNEND &
		! End FNYES.NO% function &

16000	!	F N R E A D . A T T R $ &
	&
	! READ ACCOUNT ATTRIBUTE BLOCK INTO ATTR%() ARRAY &
	&
	! Arguments: &
	!	TYP%	- Attribute type code &
	!	PPN$	- 2-byte PPN string as returned by FSS &
	!	DEV$	- 4-byte device/unit string as returned by FSS &
	&
	! Returns: &
	!	FNREAD.ATTR$	- 13-byte attribute value &
	!	ATTR%(1-13)	- 13-cell attribute array (ATTR%(0%) = 13%) &
	&
	&
	DEF* FNREAD.ATTR$ (TYP%,PPN$,DEV$) &
\	TMP$ = MID(SYS(CHR.6$+CHR$(-25%)+CHR$(-1%)+ &
			CHR$(TYP%)+PPN$+STRING$(16%,0%)+DEV$),8%,13%) &

16010	FNREAD.ATTR$ = TMP$ &
\	CHANGE TMP$ TO ATTR% &
\	FNEND &

16100	!	F N W R I T E . A T T R $ &
	&
	! WRITE ACCOUNT ATTRIBUTE BLOCK &
	&
	! Arguments: &
	!	TYP%	- Attribute type code &
	!	ATTR$	- 13-byte attribute string &
	!	PPN$	- 2-byte PPN string as returned by FSS &
	!	DEV$	- 4-byte device/unit string as returned by FSS &
	&
	! Returns: &
	!	FNWRITE.ATTR$	- Same as ATTR$ padded to length 13 &
	&
	&
	DEF* FNWRITE.ATTR$ (TYP%,ATTR$,PPN$,DEV$) &
\	FNWRITE.ATTR$,ATTR$ = FNPAD$(ATTR$,13%) &
\	TMP$ = SYS(CHR.6$+CHR$(-25%)+CHR$(-2%)+ &
			CHR$(TYP%)+PPN$+CHR.0$+ATTR$+CHR.00$+DEV$) &
\	FATAL% = 0% &
\	GOTO 16120 &

16110	PRINT "??Not enough free disk space to convert account "; &
		FNPPN$(PPN%); " - aborting" &
\	FATAL% = -1% &

16120	FNEND &

16200	!	F N E R R O R $ &
	&
	! RETURN ERROR MESSAGE TEXT &
	&
	&
	DEF FNERROR$(ERROR%) = &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERROR%)),3%),4%) &

16300	!	F N P A D $ &
	&
	! PAD STRING WITH NULLS &
	&
	&
	DEF FNPAD$(STRNG$,LNG%) = &
		LEFT(STRNG$+STRING$(LNG%,0%),LNG%) &

19000	!	E R R O R   H A N D L E R &
	&
	&
	ERROR$=FNERROR$(ERR) &
	&
\	IF ERR=11% THEN &
		IF ERL=1100% THEN &
			RESUME 32767 &
		ELSE	IF ERL=15910% THEN &
				CTRL.Z% = -1% &
\				RESUME 15920 &
		! Save error message text &
		! If user typed CTRL/Z to prompt, &
		!	exit if initial prompt &
		!	else if in YES/NO prompt function, &
		!		set CTRL.Z% flag and resume to end function &

19010	IF ERL = 11020% THEN &
		IF ERR = 5% THEN &
			RESUME 11140 &
		! If no more accounts during 1.1 -> 1.2 conversion, &
		!	Resume to exit conversion loop &

19020	IF ERL = 12030% THEN &
		IF ERR = 5% THEN &
			RESUME 12110 &
		! If no more accounts during password conversion, &
		!	Resume to exit conversion loop &

19030	IF ERL = 16000% THEN &
		IF ERR = 11% THEN &
			TMP$ = STRING$(13%,0%) &
\			RESUME 16010 &
		! If error reading attribute blockette, &
		!	If EOF (blockette not found), &
		!		Return null blockette &
		!		Resume to exit FNREAD.ATTR$ function &

19040	IF ERL = 1110% THEN &
		RESUME 1100 &
		! If error FSSing the device name, &
		!	Resume to re-prompt &

19050	IF ERL = 1130% THEN &
		IF ERR = 21% THEN &
			RESUME 1140 &
		! If device not mounted trying to read pack attributes, &
		!	Resume to continue (cannot be the system disk) &

19060	IF ERL = 1200% THEN &
		IF ERR = 21% THEN &
			RESUME 1210 &
		! If disk not mounted error trying to dismount disk, &
		! 	Resume to continue &

19070	IF ERL = 1140% OR ERL = 1200% OR ERL = 1220% THEN &
		Z$ = "access" &
\		Z$ = "mount" if ERL = 1220% &
\		PRINT "?Unable to "; Z$; " disk "; DSK$ &
\		PRINT ERROR$ &
\		RESUME 1100 &
		! If error opening, dismounting or mounting disk, &
		!	Build corresponding error text &
		!	Print message and re-prompt &

19080	IF ERL = 10000% THEN &
		IF ERR = 5% THEN &
			RESUME 10010 &
		! Resume to continue if no work file to delete &

19090	IF ERL = 10010% THEN &
		IF ERR = 5% THEN &
			RESUME 10020 &
		! Resume to continue if no MFD/GFD work file to delete &

19100	IF ERL = 32700% THEN &
		RESUME 32710 &
		! Resume to continue if error deting NEW11 temp file &

19110	IF ERL = 32710% THEN &
		RESUME 32720 &
		! Resume to continue if error deleting MFD/GFD temp file &

19300	IF ERR = 4% THEN &
		IF ERL = 16100% THEN &
			RESUME 16110 &
		ELSE	IF ERL = 10070% OR ERL = 10090% THEN &
				PRINT "??Not enough free disk space to "; &
					" complete conversion - aborting" &
\			RESUME 32700 &
		! If not enough free space on disk, &
		!	If happened during FNWRITE.ATTR$ function, &
		!		Resume to error handler in function, &
		!	Else	If happened during 0.0 -> 1.1 conversion, &
		!			Display the bad news &
		!			And exit &

19999	PRINT "??Program failure in DSKCVT" &
\	PRINT ERROR$; " at line"; ERL &
\	RESUME 32700 &
		! Unexpected error &
		! Display the cause and line no. &
		! Resume to end program &

32700	!	E X I T   P R O G R A M &
	&
	KILL DSK$+"[1,1]NEW11.DAT" &
		! Kill NEW11 temp file if it exists &

32710	KILL DSK$+"[1,1]MFDGFD.DAT" &
		! Kill MFDGFD temp file if it exists &

32720	CLOSE 1%, 2% &
		! Close any channels we used &

32767	END
