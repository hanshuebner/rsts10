2!		PROGRAM		: DCLUTL
5!		VERSION		: V10.1
6!		EDIT		: A
7!		EDIT DATE	: 10-MAY-91
10		EXTEND
11	! &
	&
	&
	! C O P Y R I G H T &
	&
	&
	!		      Copyright (C) 1982, 1991 by &
	!	       Digital Equipment Corporation, Maynard, Mass. &
	! &
	! &
	! This software is furnished under a license and may be used and &
	! copied  only  in accordance with the terms of such license and &
	! with the  inclusion  of  the  above  copyright  notice.   This &
	! software  or  any  other copies thereof may not be provided or &
	! otherwise made available to any other person.  No title to and &
	! ownership of the software is hereby transferred. &
	! &
	! The information in this software is subject to change  without &
	! notice  and should not be construed as a commitment by Digital &
	! Equipment Corporation. &
	! &
	! DIGITAL assumes no responsibility for the use  or  reliability &
	! of its software on equipment that is not supplied by DIGITAL. &
	! &
	!******************************************************************* &
	&
	&

20	! &
	&
	&
	! M O D I F I C A T I O N    H I S T O R Y &
	&
	&
	&
	! VER/ED	EDIT DATE	REASON &
	! V8.0-07	20-APR-84	Changed DENSITY DEFAULT lookup to byte &
	! V9.0		12-MAR-85	Added RESUMES out of error handlers &
	! V9.1		15-May-85	Added high-density support &
	! V9.1		10-Jun-85	Support for new format bootable tape. &
	!				Also, improved tape format checking. &
	! V9.3		14-Nov-86	Get magtape density before rewind &
	! V10.0		23-Aug-88	Add RESUME to ^Z error handling &
	! V10.0		16-Mar-90	Don't check density if they didn't say &
	!				/DENSITY and they said /FORMAT=FOREIGN &
	&

100	! &
	&
	&
	! G E N E R A L    D E S C R I P T I O N &
	&
	&
	&
	! The DCLUTL program supports the DCL MOUNT, DISMOUNT, and INITIALIZE &
	! commands for tape. &
	! &
	! This program relies on DCL to pass a syntactically valid command &
	! line.  DCLUTL checks the argument of the /DENSITY qualifier for &
	! validity; DCL is assumed to have checked everything else. &
	! &
	! DCLUTL prompts for a label if the tape is in ANSI format and the &
	! user has not supplied one on the command line. &
	! &
	! COMMAND FORMATS PASSED BY DCL: &
	!	MOUNT dev: [label] /quals &
	!		/JOB:n		Assign the tape to job n &
	!				 // JOB:n Not Implimented // &
	!		/FORMAT:DOS	Tape is in DOS Format &
	!		/FORMAT:ANSI	Tape is in ANSI Standard Format &
	!				(The Volume ID should be specified &
	!				with an ANSI tape) &
	!		/FORMAT:FOREIGN	Tape is in foreign (not ANSI or DOS) &
	!				format &
	!		/WRITE		MOUNT the tape read/write. &
	!		/NOWRITE	MOUNT the tape read-only &
	!			Default - mount the tape read/write unless &
	!				drive is write protected.  Give &
	!				warning if drive is write protected. &
	!		/DENSITY:nnnn	Tape density is nnnn BPI &
	! &
	!	DISMOUNT dev: /quals &
	!		/UNLOAD		Rewind the tape and take it off-line &
	!				(default) &
	!		/NOUNLOAD	Rewind the tape, but don't take it &
	!				offline &
	! &
	!	INITIALIZE dev: [label] /quals &
	!		/FORMAT:DOS	DOS Format &
	!		/FORMAT:ANSI	ANSI Standard Format &
	!				(The ID label should be specified with &
	!				an ANSI tape) &
	!		/DENSITY:nnnn	Set the tape density to nnnn BPI &
	! &
	! Qualifiers must be at the END of the command line.  Colons must &
	! separate the qualifiers from their arguments. &
	! &

300	! &
	&
	! I / O    C H A N N E L S &
	&
	&
	!	CHANNEL #		USED FOR &
	!		1		The device being MOUNTed &
	!		2		KB input channel. &
	&
	&
	&

400	! &
	&
	&
	! V A R I A B L E    D E F I N I T I O N S &
	&
	&
	&
	!	NAME		USED FOR &
	! &
	!	BADDIR%		Constant ?Bad directory for device &
	!	CMD$		Command string &
	!	CMD.KEYWD$	Keyword MOUNT, DISMOUNT, or INITIALIZE &
	!	COLON.POS%	Position of colon in string &
	!	DATERR%		Constant ?Data error on device &
	!	DEV$		Device designator &
	!	DEV.CHAN%	Constant channel on which device is open &
	!	DEV.FIRQB$	FSS of DEV$ &
	!	DEV.FIRQB%()	FSS of DEV$ &
	!	DEV.FLAG2%	Flag word 2 for DEV$, returned by .FSS &
	!	DEV.WPR.MSG$	Constant "Device is write protected" &
	!	ENTRY.TYP%	Entry type: &
	!			0 => run entry (line 0) &
	!			16 => DCL entry (line 30500) //check// &
	!	EOF%		Constant ?End of file on device &
	!	ERR.CODE%	Number of error to retrieve &
	!	ERR.TEXT$	Message text to print &
	!	FALSE%		Constant 0, logical false &
	!	FIRQB$		SYS call string &
	!	GOT.DEN%	Density of tape &
	!	GOT.ERR%	FALSE% if no error yet, TRUE% if error &
	!	HDR.LABEL$	Tape's header label &
	!	HNGDEV%		Constant ?Device hung or write locked &
	!	ID.LABEL$	Tape label specified by user &
	!	INC.DEN.ERR$	Constant "Data error or incorrect density" &
	!	KB.CHAN%	Constant channel on which KB input is taken &
	!	M%()		SYS call string &
	!	MAG.BACKSP%	Constant magtape backspace function &
	!	MAG.REW%	Constant magtape rewind function &
	!	MAG.REW.ONCLOSE% Constant magtape rewind on close function &
	!	MAG.SET.DEN%	Constant magtape set density function &
	!	MAG.SKIP%	Constant magtape skip function &
	!	MAG.STATUS%	Constant magtape status function &
	!	MAG.TAPE.MARK%	Constant magtape write tape mark function &
	!	MAG.SET.XDN%	Constant magtape set extended density function &
	!	MAGLBL%		MAGLBL pointer in monitor &
	!			-> format default - 0 for DOS, -1 for ANSI. &
	!			  density default - value is density &
	!	MINMAX%		Minumum/Maximum density specified flag &
	!	PRIV.ON$	SYS call to assert privilege &
	!	PRIV.OFF$	SYS call to drop privilege &
	!	Q.ARG$		Qualifier argument &
	!	Q.ARG$(s%)	Argument of qualifier s%. "" if qualifier not &
	!			present. &
	!	Q.DEN%		Code for /DENSITY qualfier &
	!	Q.FMT%		Code for /FORMAT qualifier &
	!	Q.JOB%		Code for /JOB qualifier //not implemented// &
	!	Q.KEYWD$	Qualifier keyword &
	!	Q.NOUNLOAD%	Code for /NOUNLOAD qualifier &
	!	Q.NOWRITE%	Code for /NOWRITE qualifier &
	!	Q.PRESENT%(s%)	Is qualifier s% present on the command line? &
	!			Indexed by qualifier code. &
	!	Q.TEXT$		Temporary string for parsing qualifier &
	!	Q.UNLOAD%	Code for /UNLOAD qualifier &
	!	Q.WRITE%	Code for /WRITE qualifier &
	!	QUAL$		Temporary string for parsing qualifier &
	!	QUALS$		Qualifiers remaining to parse &
	!	SCAN.POS%	Scan position in CMD$ (/) &
	!	SEV%		Severity of error, if one occurred. &
	!			WARN.SEV% => warning (%) &
	!			ERR.SEV% => user error (?) (default) &
	!			INFO.SEV% => information &
	!	SYS.FSS$	FSS SYS call &
	!	TAPE.DEN$	Tape density &
	!	TAPE.FMT$	DOS/ANSI indicator &
	!			"ANSI" => ANSI format &
	!			"DOS" => DOS format &
	!	TAPE.STAT%	Tape status variable &
	!	TRUE%		Constant -1 - logical TRUE &
	!	UNIT.NO%	Tape unit number &
	!	WANT.DEN%	Desired tape density &
	! &

800	! &
	&
	&
	! F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&
	&
	!	FUNC./SUBR.	LINE	USE &
	! &
	!	FNPROC.QUAL%	10010	Process qualifiers &
	!	FNMNT.TAPE%	14000	Mount tape &
	!	FNSET.DEN%	14400	Set tape density &
	!	FNMNT.SET.FMT%	14600	Set tape format, MOUNT &
	!	FNINIT.SET.FMT%	14700	Set tape format, INITIALIZE &
	!	FNGET.LABEL%	14800	Prompt for and parse ID label &
	!	FNDISMNT.TAPE%	15000	Dismount tape &
	!	FNOPEN.DEV%	15200	Open device and get TAPE.STAT% &
	!	FNINIT.TAPE%	16000	Initialize a tape &
	!	FNSCAN.DEV%	17100	Scan a device name &
	!	FNPRINT.RSTS.ERR% 17500 Print RSTS/E error message &
	!	FNPRINT.MSG%	17600	Print message &
	!	FNGET.ERR$	17700	Get error message &
	!	FNBUG%		17800	Report program bug &

900	! &
	&
	! D I M E N S I O N    S T A T E M E N T S &
	&
	&

910	DIM DEV.FIRQB%(31%),M%(31%) &
	!	File name string scan arrays &

950	DIM Q.PRESENT%(15%), Q.ARG$(15%) &
	!	Qualifier processor arrays &
	&
	&

999	! &
	&
	&
	! M A I N    C O D I N G    A R E A &
	&
	&

1000	ON ERROR GOTO 19000 &
	\ TRUE% = -1% &
	\ FALSE% = 0% &
	\ VER$ = "DCLUTL	V10.1-A" &
	\ MAG.OFFLINE% = 1% &
	\ MAG.TAPE.MARK% = 2% &
	\ MAG.REW% = 3% &
	\ MAG.SKIP% = 4% &
	\ MAG.BACKSP% = 5% &
	\ MAG.SET.DEN% = 6% &
	\ MAG.STATUS% = 7% &
	\ MAG.FILE.CHAR% = 8% &
	\ MAG.REW.ONCLOSE% = 9% &
	\ MAG.SET.XDN% = 12% &
	\ PRIV.OFF$ = CHR$(6%)+CHR$(-21%)+CHR$(255%) &
	\ PRIV.ON$ = CHR$(6%)+CHR$(-21%)+CHR$(0%) &
	\ SYS.FSS$ = CHR$(6%)+CHR$(-10%) &
	\ BADDIR% = 1% &
	\ INUSE% = 3% &
	\ NODEVC% = 6% &
	\ PRVIOL% = 10% &
	\ EOF% = 11% &
	\ DATERR% = 13% &
	\ HNGDEV% = 14% &
	\ MAGRLE% = 40% &
	\ BNDERR% = 52% &
	\ DEV.CHAN% = 1% &
	\ KB.CHAN% = 2% &
	\ Q.JOB% = 1% &
	\ Q.FMT% = 2% &
	\ Q.UNLOAD% = 3% &
	\ Q.NOUNLOAD% = 4% &
	\ Q.DEN% = 5% &
	\ Q.WRITE% = 6% &
	\ Q.NOWRITE% = 7% &
	\ Q.OVERRIDE% = 8% &
	\ DEV.WPR.MSG$ = "Device is write protected" &
	\ INC.DEN.ERR$ = "Data error or incorrect density" &
	\ NOT.DOS$ = "Not a DOS format tape" &
	\ NOT.ANSI$ = "Not an ANSI format tape" &
	\ NOT.DOS.ANSI$="Not a DOS or ANSI format tape" &
	!	Set up standard error trap. &
	!	Set up Boolean constants. &
	!	Set up version string. &
	!	Set up constants for magtape subfunctions. &
	!	Set up command codes. &
	!	Set up standard SYS() call strings. &
	!	Set up RSTS/E error mnemnonics. &
	!	Set up channel numbers. &
	!	Set up qualifier codes. NOTE:  order is important. &
	!		Must be same as &
	!		DATA items at line 9000 and following. &
	!	Set up error severity codes. &
	!	Set up names for common messages. &

1010	IF ENTRY.TYP%=0% THEN &
		PRINT IF CCPOS(0%) &
	\	PRINT VER$; CHR$(9%); CHR$(9%); FNGET.ERR$(0%) &
	\	JUNK% = FNPRINT.MSG%("?","Please use DCL commands") &
	\	GOTO 32767 &
	!	Print out header string if RUN entry. &
	!	Give error message. &
	!	Exit program. &

1012	&
	&
	! M A I N   P R O G R A M &
	! &
	! Get the command string in CMD$. &

1015	JUNK$ = SYS(PRIV.OFF$) &
	\ GOT.ERR% = FALSE% &
	!	Drop temp privilege. &
	!	Initialize error flags to no error. &

1020	CMD$ = SYS(CHR$(7%)) &
	!	Get the command out of core common. &

1025	! Extract the command keyword. &
	CMD$ = CVT$$(CMD$,4%+8%+16%+32%+128%) &
	!	Trim off excess spaces, convert command to upper case. &
	\ SCAN.POS% = INSTR(1%,CMD$+CHR$(32%),CHR$(32%)) &
	\ CMD.KEYWD$ = LEFT(CMD$,SCAN.POS%-1%) &
	\ CMD$ = RIGHT(CMD$,SCAN.POS%+1%) &
	!	Extract the command keyword. &

1055	SCAN.POS% = INSTR(1%,CMD$,"/") &
	\ QUALS$ = "" &
	\ QUALS$ = RIGHT(CMD$,SCAN.POS%) IF SCAN.POS% &
	\ CMD$ = LEFT(CMD$,SCAN.POS%-1%) IF SCAN.POS% &
	\ JUNK% = FNPROC.QUAL% &
	\ GOTO 1090 IF GOT.ERR% &
	\ JUNK% = FNSCAN.DEV%(CMD$) &
	\ GOTO 1090 IF GOT.ERR% &
	!	If any qualifiers, split them away from &
	!	rest of command string. &
	!	Process qualifiers, if any. &
	!	Exit if error. &
	!	Scan the device name and label (if any). &
	!	Exit if error. &

1087	IF CMD.KEYWD$="MOUNT" THEN &
		JUNK% = FNMNT.TAPE% &
	ELSE IF CMD.KEYWD$="DISMOUNT" THEN &
		JUNK% = FNDISMNT.TAPE% &
	ELSE IF CMD.KEYWD$="INITIALIZE" THEN &
		JUNK% = FNINIT.TAPE% &
	ELSE &
		JUNK% = FNPRINT.MSG%("?", "Invalid command") &
	\	GOTO 32767 &
	!	Mount tape if that's the operation. &
	!	Dismount tape if that's the operation. &
	!	Initialize tape if that's the operation. &
	!	Else unexpected error. Exit program. &

1090	&
	&
	&
	! F I N I S H   U P &
	&
	&

1095	ON ERROR GOTO 19000 &
	\ CLOSE #-I% FOR I%=1% TO 12% &
	\ GOTO 32767 &
	!	Set up standard error trap, for safety. &
	!	Reset all channels. &
	!	Get another command if RUN entry, else exit. &
	&

9000	&
	&
	! S W I T C H   T A B L E &
	! &
	! NOTE: codes in this table must be in the same order as &
	! the qualifier codes assigned &
	! at line 1000. &

9010	DATA	"JOB", &
		"FORMAT", &
		"UNLOAD", &
		"NOUNLOAD", &
		"DENSITY", &
		"WRITE", &
		"NOWRITE", &
		"OVERRIDE" &

9015	DATA	"*ENDQ" &

10000	&
	&
	&
	! F U N C T I O N S &
	&
	&

10010	DEF* FNPROC.QUAL% &
	! &
	! &
	! F U N C T I O N   F N P R O C . Q U A L % &
	! &
	! Process qualifiers in command string &
	! &
	! INPUTS: &
	!	QUALS$		Command string qualifiers.  If this string &
	!			is non-null then the first character of it is &
	!			definately a slash. &
	! &
	! USES: &
	!	QUAL$		Qualifier name &
	! &
	! OUTPUTS ON SUCCESSFUL COMPLETION: &
	!	Qualifiers have been processed. &
	!	Q.ARG$(s%)	Argument of qualifier s%, if qualifier &
	!			was specified. &
	!	Q.PRESENT%(s%)	TRUE% if qualifier s% was specified, &
	!			else FALSE%. &
	! &
	! OUTPUTS ON ERROR COMPLETION: &
	!	GOT.ERR%	TRUE% &
	! &

10020	Q.PRESENT%(I%) = FALSE% FOR I%=0% TO 15% &
	\ Q.ARG$(I%) = "" FOR I%=0% TO 15% &
	!	Initialize qualifier arrays. &

10030	WHILE LEN(QUALS$) &

10105		SCAN.POS% = INSTR(2%,QUALS$,"/") &
	\	SCAN.POS% = LEN(QUALS$)+1% IF SCAN.POS%=0% &
	\	Q.TEXT$ = MID(QUALS$,2%,SCAN.POS%-2%) &
	\	QUALS$ = RIGHT(QUALS$,SCAN.POS%) &
	!		Find a slash or end-of-string. &
	!		Extract the leftmost qualifier and its argument, &
	!		if any, discarding the slash. &
	!		Leave remaining qualifiers in QUALS$. &

10110		Q.ARG$ = "" &
	\	Q.KEYWD$ = Q.TEXT$ &
	\	COLON.POS% = INSTR(1%,Q.TEXT$,":") &
	\	IF COLON.POS%<>0% THEN &
			Q.ARG$ = RIGHT(Q.TEXT$,COLON.POS%+1%) &
	\		Q.KEYWD$ = LEFT(Q.TEXT$,COLON.POS%-1%) &
	!		Extract the qualifier argument, if there is one, &
	!		and get the keyword in Q.KEYWD$. &

10115		RESTORE &
	\	FOR QUAL% = 1% TO 15% &
	\		READ QUAL$ &
	\		IF QUAL$="*ENDQ" THEN &
				JUNK% = FNPRINT.MSG%("?", &
					"Invalid qualifier - /"+Q.KEYWD$) &
	\			GOTO 10399 &
	!		Scan data list for keyword. &
	!		If no match then &
	!			Unexpected error. Print message and return. &

10125			GOTO 10200 IF LEFT(QUAL$,LEN(Q.KEYWD$))=Q.KEYWD$ &
	\	NEXT QUAL% &
	!		Scan for qualifier. &
	\	STOP &
	!		Can't get here. &

10200		Q.PRESENT%(QUAL%) = TRUE% &
	\	Q.ARG$(QUAL%) = Q.ARG$ &
	\ NEXT &
	!	Note argument. &
	!	Continue while there are more qualifiers to process. &

10399	FNPROC.QUAL% = JUNK% &
	\ FNEND &

14000	DEF* FNMNT.TAPE% &
	! &
	! F U N C T I O N   F N M N T . T A P E % &
	! &
	! MOUNT a tape. &
	! &
	! INPUTS: &
	!	DEV$		Device specification of device &
	!	UNIT.NO%	Unit number of device &
	!	Density &
	!	//etc// &
	! &
	! USES: &
	!	M%()	SYS() call array &
	! &

14040	ON ERROR GOTO 14220 &
	\ OPEN "_"+DEV$+":" AS FILE DEV.CHAN% &
	!	Open tape non-file-structured. &
	!	On error, print message and return. &
	\ TAPE.STAT% = MAGTAPE(MAG.STATUS%,0%,DEV.CHAN%) &
	!	Get tape status &

14060	IF (TAPE.STAT% AND 1024%)<>0% THEN &
		GOTO 14120 &
	ELSE IF Q.PRESENT%(Q.NOWRITE%) THEN &
		JUNK% = FNPRINT.MSG%("?","Device not write protected") &
	\	GOTO 14240 &
	!	If device is write protected, go handle that. &
	!	If device is not write protected then &
	!		If /NOWRITE specified then &
	!			Print error message go do error wrapup. &

14110	GOTO 14140 &

14120	IF Q.PRESENT%(Q.WRITE%) THEN &
		JUNK% = FNPRINT.MSG%("?",DEV.WPR.MSG$) &
	\	GOTO 14240 &
	!	Device is write protected. &
	!	If he said /WRITE then &
	!		Print message and go do error wrapup. &

14130	IF NOT Q.PRESENT%(Q.NOWRITE%) THEN &
		JUNK% = FNPRINT.MSG%("%",DEV.WPR.MSG$) &
	!	If /NOWRITE was not specified then &
	!		Print warning message. &

14140	JUNK% = FNSET.DEN% &
	\ GOTO 14240 IF GOT.ERR% &
	!	Open device, set density , &
	!	and read first block of tape. &
	!	If error occurred go do error wrapup. &
	\ JUNK% = FNMNT.SET.FMT% &
	!	Set tape format and allocate device. &
	\ GOTO 14240 IF GOT.ERR% &
	!	If error occurred go do error wrapup. &
	\ ON ERROR GOTO 14220 &
	!	Trap errors while closing the channel. &
	\ CLOSE #DEV.CHAN% &
	!	Close the channel. &
	\ ON ERROR GOTO 19000 &
	!	Restore standard error trap. &
	\ GOTO 14299 &
	! &
	! &
	!14150	 //The following lines of code are commented out because &
	!	   they are not implimented yet.  When and if they are, then &
	!	   they also need to be tested. // &
	!	 Reassign the device if /JOB:n specified. &
	!	 //If error occurs during this, should deallocate the device &
	!	 unless we owned it to begin with.// &
	! &
	!14160	GOTO 14299 UNLESS Q.PRESENT%(Q.JOB%) &
	!	\ DEV.FIRQB%(1%) = 6% &
	!	\ DEV.FIRQB%(2%) = 10% &
	!	\ DEV.FIRQB%(I%) = 0% FOR I%=3% TO 22% &
	!	\ DEV.FIRQB%(I%) = 0% FOR I%=27% TO 30% &
	!	\ DEV.FIRQB%(7%) = VAL(Q.ARG$(Q.JOB%)) &
	!		If not /JOB:n then we're done. &
	!		Else set up to reassign the device. &
	! &
	!14170	CHANGE DEV.FIRQB% TO DEV.FIRQB$ &
	!	\ ON ERROR GOTO 14180 &
	!	\ JUNK$ = SYS(DEV.FIRQB$) &
	!	\ ON ERROR GOTO 19000 &
	!	\ GOTO 14299 &
	!		Trap error reassigning the drive. &
	!		Do the SYS() call to reassign the tape drive. &
	!		Restore standard error trap. &
	!		Successful completion. &
	! &
	!14180	RESUME 14190 &
	!		Error trying to reassign the device. &
	! &
	!14190	JUNK% = FNPRINT.MSG%("?", &
	!		"Cannot reassign device "+DEV$+":"+" to job"+ &
	!			Q.ARG$(Q.JOB%)) &
	!	\ IF ERR=BNDERR% THEN &
	!		JUNK% = FNPRINT.MSG%("?","Job does not exist") &
	!	ELSE &
	!		JUNK% = FNPRINT.RSTS.ERR%("?",ERR) &
	!		If error is ?Illegal number, make it ?Job does not &
	!		exist. &
	!		Print the message. &
	! &
	!14200	GOTO 14240 &
	!		Go do error wrapup. &
	! &

14220	! ERROR WRAPUP FOR TAPE MOUNT &
	! &
	! 14220 is the ON ERROR GOTO entry point. &
	! Print the message and wrap up. &
	! &
	! 14240 is the GOTO entry point.  Just wrap up. &
	! &
	! Rewind the tape and close the channel. &

14230	ON ERROR GOTO 19000 &
	!	Restore standard error trap. &
	\ JUNK% = FNPRINT.RSTS.ERR%("?",ERR) &
	!	Print message. &
	\ RESUME 14240 &

14240	ON ERROR GOTO 14290 &
	!	Trap errors that occur while wrapping up. &

14250	JUNK% = MAGTAPE(MAG.REW%,0%,DEV.CHAN%) &
	!	Rewind tape &

14260	CLOSE #DEV.CHAN% &
	!	Close channel &

14280	ON ERROR GOTO 19000 &
	\ GOTO 14299 &
	!	Restore standard error trap, and return. &

14290	! &
	! Error trap &
	! &
	RESUME 14260 IF ERL=14250% &
	\ RESUME 14280 &
	!	Resume at the next step of the error wrapup routine. &

14299	FNMNT.TAPE% = JUNK% &
	\ FNEND &

14400	DEF* FNSET.DEN% &
	&
	! F U N C T I O N   F N S E T . D E N % &
	! &
	! Set tape density &
	! &
	! For the INITIALIZE command, we set the tape according to the user's &
	! /DENSITY qualifier argument, or the system default if a density was &
	! not given by the user. &
	! &
	! For the MOUNT command, we do more checking for incorrect density. &
	! &
	! HOW THIS ROUTINE DEALS WITH INCORRECT DENSITY: &
	! &
	! If device only supports one density then &
	!	If user specified /DENSITY=n then &
	!		If user specified the density supported by the device &
	!			then &
	!			If tape is at that density then &
	!				Good. &
	!			Else &
	!				?Data error or incorrect density &
	!		Else user specified wrong density. &
	!			?Invalid density for this device &
	!	Else user didn't specify /DENSITY=n. &
	!		If tape is at density supported by the device then &
	!			Good. &
	!		Else &
	!			?Data error or incorrect density. &
	! Else device supports multiple densities. &
	!	If controller automatically determines tape's density then &
	!		If user specified /DENSITY=n then &
	!			If tape is at that density then &
	!				Good. &
	!			Else &
	!				?Incorrect density &
	!		Else user didn't specify /DENSITY=n. &
	!			Good. &
	!	Else controller doesn't automatically determine tape's &
	!		density. &
	!		If tape is at system default density or the density &
	!			specified by the user then &
	!			Good. &
	!		Else &
	!			?Data error or incorrect density &
	! &
	! INPUTS: &
	!	CMD.KEYWD$ &
	!	Q.PRESENT%(Q.DEN%) &
	!	Q.ARG$(Q.DEN%) &
	! &
	! OUTPUTS: &
	!	TAPE.STAT% 	Tape status word &
	!	GOT.ERR%	TRUE% if error occurred, else FALSE% &
	!	HDR.LABEL$	MOUNT only. Header label for device; &
	!			"" if no label. Header record has been &
	!			read and buffered from the device &
	!			(unless an error occurred). &
	!	Device open on DEV.CHAN%. &
	! &
	! MOUNT MESSAGES: &
	!	?Tape has not been initialized &
	!	Density is nnn &
	!	Format is ANSI &
	!	Format is DOS &
	! &
	! USES: &
	!	TEMP% &
	! &
	&
	\ ON ERROR GOTO 14500 &

14410	JUNK% = MAGTAPE(MAG.REW%,0%,DEV.CHAN%) &
	!	Rewind tape. (If the tape controller automatically &
	!	determines tape density, then the rewind also &
	!	clears any ideas the controller may have about the density; &
	!	thus the SET DENSITY will &
	!	not be ignored when we issue it later.) &
	\ MINMAX% = FALSE% &
	\ IF Q.PRESENT%(Q.DEN%) THEN &
		Q.ARG$(Q.DEN%) = CVT$$(Q.ARG$(Q.DEN%),32%) &
	\	Q.ARG$(Q.DEN%) = "1" &
			IF INSTR(0%,"MINIMUM",Q.ARG$(Q.DEN%))=1% &
	\	Q.ARG$(Q.DEN%) = "32767" &
			IF INSTR(0%,"MAXIMUM",Q.ARG$(Q.DEN%))=1% &
	\	WANT.DEN% = VAL(Q.ARG$(Q.DEN%)) &
	\	GOTO 14440 IF (WANT.DEN% > 0%) &
	\	JUNK% = FNPRINT.MSG%("?","Invalid density - "+Q.ARG$(Q.DEN%)) &
	\	GOT.ERR% = TRUE% &
	\	GOTO 14499 &
	!	Init the Minimum/Maximum flag &
	!	If the user specified a density, check it out. &
	!	Set it to a 1 if user specified MINIMUM &
	!	Set it to 32767 if user specified MAXIMUM &
	!	Get the value &
	!	Continue if valid ( 0 < density <= 32767 ) &
	!	Error if it isn't &

14430	! User didn't specify density, so find the current density. &
	GOTO 14499 IF (Q.PRESENT%(Q.FMT%)) &
			AND (INSTR(0%,"FOREIGN",Q.ARG$(Q.FMT%))=1%) &
	!	Skip tape density check if they said /FORMAT=FOREIGN &
	\ WANT.DEN% = MAGTAPE(MAG.SET.XDN%,0%,DEV.CHAN%) &
	!	Get the current density &

14440	MINMAX% = (WANT.DEN% = 1%) OR (WANT.DEN% = 32767%) &
	!	Set the Minimum/Maximum flag if true &
	\ JUNK% = MAGTAPE(MAG.SET.XDN%, WANT.DEN%+32767%+1%, DEV.CHAN%) &
	!	Attempt to set the density. &
	\ TAPE.STAT% = MAGTAPE(MAG.STATUS%,0%,DEV.CHAN%) &
	!	Get tape status. &
	\ GOT.DEN% = MAGTAPE(MAG.SET.XDN%,0%,DEV.CHAN%) &
	!	Get the current density &

14450	IF CMD.KEYWD$="MOUNT" THEN &
	!	If the command is MOUNT then &
		HDR.LABEL$ = "" &
	!	Preset null header label in case we get an EOF on the read. &
	\	TEMP%=0% &
	!	Preset our label size temporary variable (also in case of EOF) &
	\	GET #DEV.CHAN% &
	\	TEMP%=RECOUNT &
	!	Read header label. &
	!	If controller automatically determines tape density, &
	!	it will do so now. &
	!	Otherwise we will get a data error if the density is wrong. &
	!	Set up a variable with number of bytes read; in case we get a &
	!	 record length error, this allows us to assume it's a tape &
	!	 in the new bootable format, and continue accordingly. &

14460	IF CMD.KEYWD$="MOUNT" THEN &
	!	If the command is MOUNT then &
		GOT.DEN% = MAGTAPE(MAG.SET.XDN%,0%,DEV.CHAN%) &
	!	Get the current density &
	\	FIELD #DEV.CHAN%, TEMP% AS HDR.LABEL$ &
	!	Get the tape header label. &
	\	TAPE.STAT% = MAGTAPE(MAG.STATUS%,0%,DEV.CHAN%) &
	!	Get tape status. &
	\ 	JUNK% = MAGTAPE(MAG.REW%,0%,DEV.CHAN%) &
	!	Rewind. &

14465	IF CMD.KEYWD$="MOUNT" THEN &
	!	If the command is MOUNT then &
		IF (Q.PRESENT%(Q.DEN%) AND (WANT.DEN%<>GOT.DEN%)) AND &
		   (MINMAX%=FALSE%) THEN &
	!	If requested density is not the same as tape's density &
			JUNK% = FNPRINT.MSG%("?","Incorrect density") &
	\		GOT.ERR% = TRUE% &
	\		GOTO 14499 &
	!		Tell user and exit. &

14470	IF (NOT (Q.PRESENT%(Q.DEN%))) OR &
		(MINMAX% <> FALSE%) THEN &
	!	If user didn't specify density then &
	!	   or user specified Minimum or Maximum &
		IF CMD.KEYWD$="MOUNT" THEN &
			JUNK% = FNPRINT.MSG%("","Density is "+NUM1$(GOT.DEN%)) &
		ELSE &
			JUNK% = FNPRINT.MSG%("","Density will be "+ &
				NUM1$(GOT.DEN%)) &
	!		Tell him what the density is if the command is MOUNT &
	!		Else tell him what it will be. &

14499	FNSET.DEN% = JUNK% &
	\ ON ERROR GOTO 19000 &
	\ FNEND &

14500	! &
	! Error trap &
	! &
	ON ERROR GOTO 14500 &
	\ IF (ERL=14450) AND (ERR=MAGRLE%) THEN &
	!	If we got a ?Magtape record length error reading header then &
			TEMP%=14% &
	!			Pretend it was a real DOS label. &
			\ RESUME 14460 &
	!			Go back for more. &

14505	IF (ERL=14450) AND (ERR=EOF%) THEN &
	!	If EOF reading header label then &
		RESUME 14460 &
	!		Continue inline. &
	ELSE IF (ERL=14450) AND (ERR=HNGDEV%) THEN &
	!	If ?Device hung or write locked reading header label then &
		JUNK% = FNPRINT.MSG%("?", &
			"Incorrect density or uninitialized tape") &
	!		Tape has not been initialized, or density &
	!		is incorrect. &
	\	GOT.ERR% = TRUE% &
	\	RESUME 14499 &
	!		Go return. &

14510	IF ERR=DATERR% THEN &
	!	If ?Data error on device then &
		JUNK% = FNPRINT.MSG%("?",INC.DEN.ERR$) &
	!		Make it ?Data error or incorrect density &
	\	RESUME 14499 &
	!		Go return. &

14513	IF (ERL=14440) AND (ERR=52%) THEN &
		IF NOT (Q.PRESENT%(Q.DEN%)) THEN &
			RESUME 14450 &
		ELSE &
			JUNK% = FNPRINT.MSG%("?", &
				"Invalid density for this device") &
	\		RESUME 14499 &
	!	If tape drive cannot support this density &
	!	   If user did not specify a density &
	!	      then continue &
	!	   Else &
	!	      print error message and exit &

14515	IF (ERL=14410) OR (ERL=14440) OR (ERL=14450) THEN &
		JUNK% = FNPRINT.RSTS.ERR%("?",ERR) &
	\	RESUME 14499 &
	!	If other error, report it and return. &

14520	JUNK% = FNBUG% &
	!	Report program failure to the user and exit. &
	&

14600	DEF* FNMNT.SET.FMT% &
	&
	! F U N C T I O N   F N M N T . S E T . F M T % &
	! &
	! Allocate device, and set tape format (DOS or ANSI) &
	! &
	! INPUTS: &
	!	Q.PRESENT%(Q.FMT%) &
	!	Q.ARG$(Q.FMT%) &
	!	HDR.LABEL$		Tape header label; "" if none present &
	!	Device open on channel DEV.CHAN% &
	! &
	! OUTPUTS: &
	!	GOT.ERR%		TRUE% if error occurred. &
	!	TAPE.FMT$		"FOREIGN", "ANSI", or "DOS" &
	! &
	! MESSAGES: &
	!	?Not a DOS format tape &
	!	?Not an ANSI format tape &
	!	?Bad directory for device &
	!	Tape is in ANSI format &
	!	Tape is in DOS format &
	!	%Label should be specified when you mount an ANSI tape &
	!	?ID labels don't match &
	&
	\ ON ERROR GOTO 14680 &
	\ TAPE.FMT$ = Q.ARG$(Q.FMT%) &
	\ IF Q.PRESENT%(Q.FMT%) THEN &
	    GOTO 14675 IF INSTR(1%,"FOREIGN",TAPE.FMT$)=1% &
	  ELSE &
	    IF LEN(HDR.LABEL$)>=80% THEN &
	      TAPE.FMT$ = "ANSI" &
	    ELSE &
	      IF LEN(HDR.LABEL$)=14% OR LEN(HDR.LABEL$)=0% THEN &
		TAPE.FMT$ = "DOS" &
	      ELSE &
		TAPE.FMT$="FOREIGN" &
	! Get specified format type &
	!   If /FORMAT was specified &
	!     Skip all the rest of these checks if /FORMAT=FOREIGN &
	!    Else he didn't specify a format &
	!     Look at the header label, and guess at the format. &

14620	IF TAPE.FMT$="ANSI" THEN &
		IF	LEN(HDR.LABEL$) < 80% &
		OR	MID(HDR.LABEL$,1%,4%)<>"VOL1" &
		THEN		JUNK% = FNPRINT.MSG%("?",NOT.ANSI$) &
		\		GOT.ERR% = TRUE% &
		\		GOTO 14699 &
	!	If we think it's an ANSI tape then &
	!		If it really isn't one then &
	!			Say ?Not an ANSI format tape, and return &

14630	GOTO 14650 IF TAPE.FMT$<>"DOS" &
	\ IF LEN(HDR.LABEL$) > 14% THEN &
		JUNK% = FNPRINT.MSG%("?",NOT.DOS$) &
		\ GOT.ERR% = TRUE% &
		\ GOTO 14699 &
	! Skip this entire section if the format isn't DOS. &
	! If the header label is more than 14 bytes long, then &
	!	Tell the user the tape is not DOS. &

14635	IF LEN(HDR.LABEL$)<>0% THEN &
		CHANGE HDR.LABEL$ TO M% &
		\ IF	((M%(1%)+SWAP%(M%(2%))) EQV 32767%)+32768. >= 1600. &
		  AND	M%(10%)=0% &
		  THEN 14650 &
		  ELSE	JUNK% = FNPRINT.MSG%("?",NOT.DOS$) &
	\		GOT.ERR% = TRUE% &
	\		GOTO 14699 &
	!	If it appears to be a DOS tape, then &
	!	 put the header into an array for easier processing. &
	!	If the first word (unsigned) is >=1600 (RAD50 "A  "), and &
	!	 a byte that must be zero is indeed zero, then &
	!	 skip the rest of the DOS tests, &
	!	 otherwise call it a bogus tape. &

14640	GET #DEV.CHAN% &
	\ JUNK%=FNPRINT.MSG%("?",NOT.DOS.ANSI$) &
	\ GOT.ERR% = TRUE% &
	! If we hit a tape mark on the first read, then &
	!  try to get a record. &
	! If we succeed, then there are not at least two tape marks at BOT, &
	!  so give an error &

14645	JUNK% = MAGTAPE(MAG.REW%,0%,DEV.CHAN%) &
	\ GOTO 14699 IF GOT.ERR% &
	! Rewind the tape if the GET above failed (not an error) &
	! Leave if we got an error above. &

14650	IF NOT Q.PRESENT%(Q.FMT%) THEN &
		JUNK% = FNPRINT.MSG%("","Tape is in "+TAPE.FMT$+" format") &
	!	If the user did not specify format, then &
	!		Print it out. &

14660	Q.PRESENT%(Q.OVERRIDE%) = FALSE% IF LEN(ID.LABEL$) &
	\ GOTO 14670 IF (TAPE.FMT$ <> "ANSI") OR (Q.PRESENT%(Q.OVERRIDE%)) &
	\ IF (DEV.FLAG2% AND 1%)=0% THEN &
		JUNK% = FNGET.LABEL% &
	!	If the tape is in ANSI format &
	!		And a label has not been specified, &
	!		And /OVERRIDE was not specified, &
	!		Prompt for and parse the label. &
	\ 	GOTO 14699 IF GOT.ERR% &
	!		Wrap things up if we got an error &

14665	IF ID.LABEL$<>MID(HDR.LABEL$,5%,6%) THEN &
		JUNK% = FNPRINT.MSG%("?","ID labels don't match") &
	\	GOT.ERR% = TRUE% &
	\	GOTO 14699 &
	!	The specified volume ID must match the real volume ID. &

14670	IF ((DEV.FLAG2% AND 1%)<>0%) AND (TAPE.FMT$="DOS") THEN &
		JUNK% = FNPRINT.MSG%("%","ID label ignored") &
	!	Warning if DOS and identification label present. &

14675	CHANGE SYS(SYS.FSS$+"_"+DEV$+":."+TAPE.FMT$) TO M% &
	\ M%(1%) = 6% &
	\ M%(2%) = 10% &
	\ M%(I%) = 0% FOR I%=3% TO 10% &
	\ M%(I%) = 0% FOR I%=13% TO 22% &
	\ M%(I%) = 0% FOR I%=27% TO 30% &
	\ M%(11%),M%(12%) = 0% IF TAPE.FMT$ = "FOREIGN" &
	\ CHANGE M% TO FIRQB$ &
	!	Set up to allocate the device. &
	!	Set up appropriate filetype for the tape's format. &
	!	If it's FOREIGN, don't set up a type at all. &
	\ JUNK$ = SYS(FIRQB$) &
	\ GOTO 14699 &
	!	Allocate the device and assign its format. &
	!	Get out of here. &

14680	! &
	! Error handling for this function. &
	! &
	IF ERR=11% AND ERL=14640% &
		THEN	RESUME 14645 &
		ELSE	JUNK% = FNPRINT.RSTS.ERR%("?",BADDIR%) &
			\ GOT.ERR% = TRUE% &
			\ RESUME 14699 &
	! If we hit an EOF when verifying a newly-initialized DOS tape, &
	!  everything's OK - just go on. &
	! Otherwise, it's a fatal error!! &

14699	ON ERROR GOTO 19000 &
	\ FNMNT.SET.FMT% = JUNK% &
	\ FNEND &

14700	DEF* FNINIT.SET.FMT% &
	&
	! F U N C T I O N   F N I N I T . S E T . F M T % &
	! &
	! Allocate device, and set tape format (DOS or ANSI) &
	! &
	! If the user did not specify a format, then we use the system &
	! default. &
	! &
	! INPUTS: &
	!	Q.PRESENT%(Q.FMT%) &
	!	Q.ARG$(Q.FMT%) &
	!	Device open on channel DEV.CHAN% &
	! &
	! OUTPUTS: &
	!	GOT.ERR%		TRUE% if error occurred. &
	!	TAPE.FMT$		"FOREIGN", "ANSI", or "DOS" &
	! &
	! MESSAGES: &
	!	Tape will be in ANSI format &
	!	Tape will be in DOS format &
	!	?Response required &
	&
	\ TAPE.FMT$ = Q.ARG$(Q.FMT%) &
	!	Set DOS or ANSI format if he specified. &
	!	(Else null string.) &

14715	IF NOT Q.PRESENT%(Q.FMT%) THEN &
	!	User didn't specify a format, so use the system default &
		CHANGE SYS(CHR$(6%)+CHR$(-29%)) TO M% &
	!	Get monitor tables, part 3. &
	\	MAGLBL% = M%(13%)+SWAP%(M%(14%)) &
	!	Get MAGLBL pointer. &
	\	JUNK$ = SYS(PRIV.ON$) &
	!	Assert temporary privilege. &
	\	TEMP% = PEEK(MAGLBL%) &
	!	Peek at format default. &
	\	JUNK$ = SYS(PRIV.OFF$) &
	!	Drop temporary privilege. &
	\	TAPE.FMT$ = "ANSI" &
	\	TAPE.FMT$ = "DOS" IF TEMP% = 0% &
	\	JUNK% = FNPRINT.MSG%("","Tape will be in "+TAPE.FMT$+ &
			" format") &
	!	Tell him what the format will be &

14730	JUNK% = FNPRINT.MSG%("", &
			"Any existing files on the tape will be deleted") &
	!	Give the user this informational message &
	\ GOTO 14760 UNLESS (TAPE.FMT$="ANSI") &
	\ IF (DEV.FLAG2% AND 1%)=0% THEN &
	!	If format is ANSI and user didn't specify a label, &
		JUNK% = FNGET.LABEL% &
	!		Prompt for and parse label. &
	\ 	GOTO 14799 IF GOT.ERR% &
	!		If an error occurred, wrap things up. &

14760	IF ((DEV.FLAG2% AND 1%)<>0%) AND (TAPE.FMT$="DOS") THEN &
		JUNK% = FNPRINT.MSG%("%","ID label ignored") &
	!	Warning if DOS and identification label present. &

14780	CHANGE SYS(SYS.FSS$+"_"+DEV$+":."+TAPE.FMT$) TO M% &
	\ M%(1%) = 6% &
	\ M%(2%) = 10% &
	\ M%(I%) = 0% FOR I%=3% TO 10% &
	\ M%(I%) = 0% FOR I%=13% TO 22% &
	\ M%(I%) = 0% FOR I%=27% TO 30% &
	\ CHANGE M% TO FIRQB$ &
	!	Set up to allocate the device. &
	!	Set up appropriate filetype for the tape's format. &
	\ JUNK$ = SYS(FIRQB$) &
	!	Allocate the device and assign its format. &

14799	FNINIT.SET.FMT% = JUNK% &
	\ FNEND &

14800	DEF* FNGET.LABEL% &
	! &
	! F U N C T I O N  F N G E T . L A B E L % &
	! &
	! Prompt for and parse a label. &
	! &
	! OUTPUTS: &
	! 	ID.LABEL$ &
	&

14820	ON ERROR GOTO 14890 &
	!	Trap CTRL/Z &
	\ OPEN 'KB:' FOR INPUT AS FILE #KB.CHAN% &
	!	Want to suppress the '?' &
	\ INPUT #KB.CHAN%,"Label:   ";ID.LABEL$ &
	!	Prompt for the ID label. &
	\ ID.LABEL$ = CVT$$(ID.LABEL$,4%+8%+16%+32%+128%) &
	!	Trim off excess spaces, convert ID label to upper case. &

14830	ON ERROR GOTO 14880 &
	\ CHANGE SYS(SYS.FSS$+ID.LABEL$) TO M% &
	!	FSS the label to make sure it's okay. &
	\ ON ERROR GOTO 19000 &
	!	Restore standard error trap &
	\ TEMP%=M%(29%)+SWAP%(M%(30%)) &
	!	Get the FSS flag word. &
	\ IF (TEMP% AND 1%) = 0% THEN &
		JUNK% =FNPRINT.MSG%("?","Response required") &
	\	GOTO 14820 &
	! 	If the user did not specify a label, then &
	!		tell him he must, and reprompt. &

14835	IF (TEMP% AND (2%+4%+8%+128%+1024%)) THEN &
		JUNK% = FNPRINT.MSG%("?","Invalid label") &
	\	GOTO 14820 &
	!	If the ID label contained any invalid characters then &
	!		Tell him that it was an invalid label, and re-prompt. &

14840 	GOTO 14899 &

14880	! &
	! Error trap &
	! &
	! 	If error in user's ID label, then &
	!	we get a "?Illegal file specification" &
	ON ERROR GOTO 19000 &
	!	Restore standard error trap. &
	\ JUNK% = FNPRINT.MSG%("?","Invalid label") &
	!	Make it "?Invalid label" &
	\ RESUME 14820 &
	!	Go reprompt &

14890	! 	If the user types CTRL/Z then &
	ON ERROR GOTO 19000 &
	!	Restore standard error trap. &
	\ GOT.ERR% = TRUE% &
	! 	Flag that an error is found. &
	\ RESUME 14899 &
	!	Continue &

14899 	FNGET.LABEL% = JUNK% &
	\ FNEND &

15000	DEF* FNDISMNT.TAPE% &
	! &
	! F U N C T I O N   F N D I S M N T . T A P E % &
	! &
	! DISMOUNT a tape. &
	! &
	! INPUTS: &
	!	DEV$			Device name. &
	!	Q.PRESENT%(Q.UNLOAD%) &
	!	Q.PRESENT%(Q.NOUNLOAD%) &
	&
	\ ON ERROR GOTO 15100 &
	!	Trap error opening, unloading, or closing the device. &
	\ OPEN "_"+DEV$+":" AS FILE DEV.CHAN% &

15010	IF Q.PRESENT%(Q.NOUNLOAD%) THEN &
	!	If /NOUNLOAD specified then &
		JUNK% = MAGTAPE(MAG.REW%,0%,DEV.CHAN%) &
	!		Rewind the tape. &
	ELSE &
		JUNK% = MAGTAPE(MAG.OFFLINE%,0%,DEV.CHAN%) &
	!		Else rewind and unload the tape. &
	!		//Should only do that if /UNLOAD specified?// &

15020	CLOSE #DEV.CHAN% &
	!	Close the channel. &
	\ ON ERROR GOTO 19000 &
	!	Restore standard error trap. &

15030	DEV.FIRQB%(1%) = 6% &
	\ DEV.FIRQB%(2%) = 11% &
	\ DEV.FIRQB%(I%) = 0% FOR I%=3% TO 22% &
	\ DEV.FIRQB%(I%) = 0% FOR I%=27% TO 30% &
	\ CHANGE DEV.FIRQB% TO DEV.FIRQB$ &
	\ JUNK$ = SYS(DEV.FIRQB$) &
	!	Deallocate the device. &
	!	(No error is possible;  the SYS() call has no effect &
	!	if we don't own the device.) &
	\ GOTO 15199 &
	!	Return. &

15100	! &
	! Error trap &
	! &
	ON ERROR GOTO 19000 &
	!	Restore standard error trap. &
	\ IF ERL=15000% THEN &
	!	If error opening the tape then we probably don't own it. &
		JUNK% = FNPRINT.RSTS.ERR%("?",ERR) &
	!		Print the error message. &
	\	RESUME 15199 &
	!		Go return. &

15110	JUNK% = FNPRINT.RSTS.ERR%("%",ERR) &
	!	Print the warning message. &
	\ CLOSE #-DEV.CHAN% &
	\ RESUME 15030 &
	!	Reset the channel.  (Presumably no error is possible.) &
	!	Go deallocate the drive. &

15199	FNDISMNT.TAPE% = JUNK% &
	!	Restore standard error trap. &
	\ FNEND &

16000	DEF* FNINIT.TAPE% &
	! &
	! F U N C T I O N   F N I N I T . T A P E % &
	! &
	! INITIALIZE a tape. &
	! &
	! INPUTS: &
	!	DEV$			Device name. &
	!	Q.PRESENT%(Q.FMT%) &
	!	Q.PRESENT%(Q.DEN%) &
	&

16045	ON ERROR GOTO 16220 &
	\ OPEN "_"+DEV$+":" AS FILE DEV.CHAN% &
	!	Open tape non-file-structured. &
	!	On error, print message and return. &
	\ TAPE.STAT% = MAGTAPE(MAG.STATUS%,0%,DEV.CHAN%) &
	!	Get tape status &
	\ IF (TAPE.STAT% AND 1024%)<>0% THEN &
		JUNK% = FNPRINT.MSG%("?",DEV.WPR.MSG$) &
	\	GOTO 16240 &
	!	Device is write protected. &
	!	Print error message and go handle error wrappup &

16050	JUNK% = FNSET.DEN% &
	\ GOTO 16240 IF GOT.ERR% &
	!	Set density. &
	!	If error occurred go do error wrappup. &

16100	JUNK% = FNINIT.SET.FMT% &
	\ GOTO 16240 IF GOT.ERR% &
	!	Set tape format, and allocate device. &
	! &
	!	If error occurred go do error wrapup. &

16140	ON ERROR GOTO 16220 &
	\ CLOSE #DEV.CHAN% &
	\ ON ERROR GOTO 19000 &
	!	Trap errors while closing the channel. &
	!	Close the channel. &
	!	Restore standard error trap. &
	&

16160	ON ERROR GOTO 16235 &
	!	Trap CTRL/Z &
	\ OPEN 'KB:' FOR INPUT AS FILE #KB.CHAN% &
	!	Want to suppress the '?' &
	\ INPUT #KB.CHAN%,"Proceed (Y or N)?   "; RESPONSE$ &
	\ ON ERROR GOTO 16220 &
	!	Ask user if want to proceed, and restore error trap. &
	\ RESPONSE$ = CVT$$(RESPONSE$,4%+8%+16%+32%+128%) &
	!	Trim off excess spaces, convert response to upper case. &
	\ IF LEN(RESPONSE$) = 0% THEN &
	!	If the user did not type anything, then &
		JUNK% = FNPRINT.MSG%("?","You must specify Y or N") &
	! 	Print warning message &
	\	GOTO 16160 &
	!	And re-prompt. &

16180	GOTO 16200 IF RESPONSE$ = LEFT("YES",LEN(RESPONSE$)) &
	! 	If response is "YES" then proceed &
	\ GOTO 16240 IF RESPONSE$ = LEFT("NO",LEN(RESPONSE$)) &
	!	If response is "NO" then abort. &
	\ JUNK% = FNPRINT.MSG%("?","Please specify Y or N") &
	\ GOTO 16160 &
	! 	Else the user typed something besides Y or N. &
	!	Go reprompt. &

16200 	CHANGE SYS(SYS.FSS$+"_"+DEV$+":"+ID.LABEL$+"."+TAPE.FMT$) TO M% &
	\ M%(1%) = 6% &
	\ M%(2%) = 13% &
	\ M%(I%) = 0% FOR I%=3% TO 6% &
	\ M%(I%) = 0% FOR I%=7% TO 10% IF TAPE.FMT$ = "DOS" &
	\ M%(I%) = 0% FOR I%=11% TO 22% &
	\ M%(I%) = 0% FOR I%=27% TO 30% &
	\ CHANGE M% TO FIRQB$ &
	!	Set up to initialize the device. &
	!	If the tape format is DOS then clear out the ID field. &
	!	Set up appropriate filetype for the tape's format. &
	\ JUNK$ = SYS(FIRQB$) &
	!	Initialize the device. &
	\ GOTO 16299 &
	!	Go wrap things up. &

16220	&
	&
	! ERROR WRAPUP FOR TAPE INITIALIZE &
	! &
	! 16220 is the ON ERROR GOTO entry point.  Print the message and wrap &
	! up. &
	! &
	! 16240 is the GOTO entry point, and 16235 is the CTRL/Z trap. &

16230	JUNK% = FNPRINT.RSTS.ERR%("?",ERR) &
	!	Print message. &

16235	RESUME 16240 &

16240	ON ERROR GOTO 19000 &
	!	Restore standard error trap. &

16299	FNINIT.TAPE% = JUNK% &
	\ FNEND &

17100	DEF* FNSCAN.DEV%(TEMP$) &
	! &
	! &
	! F U N C T I O N   F N S C A N . D E V % &
	! &
	! Scan device name and label. &
	! &
	! INPUTS: &
	!	TEMP$	String to scan &
	! &
	! OUTPUTS: &
	!	DEV$		Device designator &
	!	UNIT.NO%	Device unit number &
	!	ID.LABEL$	Identification label specified by user &
	!	DEV.FIRQB%() 	FSS formatted string. &
	!	DEV.FLAG2%	Flag word 2, returned by FSS &
	&
	\ CHANGE SYS(SYS.FSS$+TEMP$) TO DEV.FIRQB% &
	\ DEV.FLAG2% = DEV.FIRQB%(29%)+SWAP%(DEV.FIRQB%(30%)) &
	!	Do the FSS.  Get flag word. &

17160	ID.LABEL$ = RAD$(DEV.FIRQB%(7%)+SWAP%(DEV.FIRQB%(8%))) &
		+RAD$(DEV.FIRQB%(9%)+SWAP%(DEV.FIRQB%(10%))) &
		IF DEV.FLAG2% AND 1% &
	\ DEV$ = CVT%$(DEV.FIRQB%(24%)+SWAP%(DEV.FIRQB%(23%))) &
	\ UNIT.NO% = 0% &
	\ IF DEV.FIRQB%(26%)<>0% THEN &
		UNIT.NO% = DEV.FIRQB%(25%) &
	\	DEV$ = DEV$+NUM1$(UNIT.NO%) &
	!	Set up device designator and identification label. &

17199	FNSCAN.DEV% = JUNK% &
	\ FNEND &

17500	DEF* FNPRINT.RSTS.ERR%(SEV$,ERR.CODE%) &
	! &
	! &
	! F U N C T I O N   F N P R I N T . R S T S . E R R % &
	! &
	! Display a RSTS/E error message. &
	! &
	! INPUTS: &
	!	SEV$		Severity character - &
	!		?	for error &
	!		%	for warning &
	!		<null>	for informational &
	!	ERR.CODE%	RSTS/E error code &
	&
	\ TEMP$ = RIGHT(FNGET.ERR$(ERR.CODE%),2%) &
	\ JUNK% = FNPRINT.MSG%(SEV$,TEMP$) &

17599	FNPRINT.RSTS.ERR% = JUNK% &
	\ FNEND &

17600	DEF* FNPRINT.MSG%(SEV$,ERR.TEXT$) &
	! &
	! F U N C T I O N   F N P R I N T . E R R % &
	! &
	! Display an error message. &
	! &
	! INPUTS: &
	!	SEV$		Severity character - &
	!		?	for error &
	!		%	for warning &
	!		<null>	for informational &
	!	TEXT$		Message text &
	! OUTPUTS: &
	!	GOT.ERR%	Did we just print an error message? &
	&
	\ GOT.ERR% = (SEV$="?") &
	\ JUNK$ = SYS(CHR$(0%)+CHR$(0%)) &
		IF GOT.ERR% &
	\ PRINT IF CCPOS(0%) &
	\ PRINT SEV$; ERR.TEXT$ &
	\ FNPRINT.MSG% = JUNK% &
	!	Cancel CTRL/O effect if warning or error message. &
	!	Bring cursor to left margin. &
	!	Print the severity character and the message. &

17699	FNEND &

17700	DEF* FNGET.ERR$(ERR.CODE%) &
	! F U N C T I O N   F N G E T . E R R $ &
	! &
	! Get RSTS/E error message text. &
	&
	\ IF ERR.CODE%=39% THEN &
	!	If it's ?Magtape select error then &
		FNGET.ERR$ = "?Device offline" &
	!		Make it ?Device offline. &
	ELSE &
		FNGET.ERR$ = CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+ &
			CHR$(ERR.CODE%)),3%),4%) &

17790	FNEND &

17800	DEF* FNBUG% &
	&
	! F U N C T I O N   F N B U G % &
	! &
	! Inform user of unexpected error, and exit. &
	! Call this routine after an ON ERROR GOTO.  Do not execute &
	! a RESUME first. &
	&
	\ JUNK% = FNPRINT.MSG%("?","Program failure in "+VER$) &
	\ CLOSE #-I% FOR I%=1% TO 12% &
	\ ON ERROR GOTO 0 &
	! At this point, RSTS will take over and print it's standard error &
	!  message stuff. &

17899	FNEND &

19000	! &
	! &
	! T R A P   F O R   U N H A N D L E D   E R R O R S &
	! &
	! &

19010	JUNK% = FNBUG% &
	!	Report program bug and exit. &

30500	! &
	! &
	! D C L    E N T R Y &
	! &
	! &

30510	ENTRY.TYP% = 16% &
	\ GOTO 1000% &
	!	Set up as DCL entry, and join mainline. &
	&
	&

32767   CLOSE #KB.CHAN% &
	\ END
