2!		PROGRAM		: BACPRM.BAS
5!		VERSION		: V10.1
6!		EDIT		: A
7!		EDIT DATE	: 10-MAY-91
10		EXTEND
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
	&
	! VER/ED	EDIT DATE	REASON &
	! KMF 01	17-MAY-84	DELETED BACKUP OPTION &
	! &

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&
	&
!	THIS MODULE CREATES A PROMPT AND SPECIAL PARAMETER TABLE FILE &
!	AND THE BACKUP HELP FILE.  THE PROMPT AND SPECIAL PARAMETER &
!	FILE IS CALLED BACKUP.PRM AND IS USED BY THE COMMAND DECODER &
!	(BACKUP.B?S).  THE HELP FILE IS CALLED BACKUP.HLP AND IS ALSO &
!	USED EXCLUSIVELY BY THE RESTOR MODULE. &
!	&
!	THIS PROGRAM MUST BE RUN PRIOR TO ANY BACKUP RUN TO CREATE &
!	THESE NECESSARY FILES.  NOTE THAT IT IS AUTOMATICALLY RUN AS &
!	PART OF THE STANDARD BACKUP PACKAGE BUILD PROCEDURE. &

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&
	&
!	CHANNEL #		USED FOR &
!	   4			VIRTUAL CORE ARRAY FOR THE PROMPT TABLE. &
!	   8			VIRTUAL CORE ARRAY FOR THE HELP FILE. &

400	! &
	&
	&
	!	V A R I A B L E    D E F I N I T I O N S &
	&
	&
	&
!	NAME		USED FOR &
	&
!	B0%		# OF "*BEGIN"'S PROCESSED. &
!			(EACH *BEGIN-*END BLOCK REPRESENTS THE HELP TEXT &
!			FOR A GIVEN QUESTION.) &
!	I%()		HELP FILE TEXT POINTER VIRTUAL ARRAY. &
!	I$		VERSION-EDIT. &
!	L$		CURRENT LINE OF HELP TEXT. &
!	P$()		PROMPT ARRAY. &
!	P%()		PARAMETER ARRAY. &
!	S%(,)		TWO DIMENSIONAL STACK FOR HELP FILE CREATION. &
!	S%		STACK POINTER. &
!	T$()		HELP FILE TEXT VIRTUAL ARRAY. &
!	Z$		TEMPORARY VARIABLE. &
!	Z%		INDEX VARIABLE. &
!	Z1%		INDEX VARIABLE. &

900	! &
	&
	&
	!	D I M E N S I O N    S T A T E M E N T S &
	&
	&

901	DIM #4%, P$(15%,1%)=64%,P%(15%,15%) &
		! PROMPT TABLE AND SPECIAL PARAMETER VIRTUAL ARRAY. &

902	DIM Q%(30%) &
		! USED TO FIND OUT WHERE THE PROGRAM RAN FROM. &

903	DIM #8%, I%(255%),T$(1024%)=16% &
		! THE HELP FILE VIRTUAL ARRAYS (I%() HOLDS THE HELP TEXT &
		! POINTERS AND T$() HOLDS THE TEXT). &

904	DIM S%(2%,25%) &
		! STACK. &

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&

1000	ON ERROR GOTO 19000 &
		! STANDARD ERROR HANDLER &
	\ Z$=SYS(CHR$(6%)+CHR$(-21%)) &
		! DROP ALL PRIVILEGES. &

1010	I$="BACPRM	V10.1-A" &
		! SET UP PART OF HEADER. &

1020	PRINT I$;CHR$(9%);RIGHT(SYS(CHR$(6%)+ &
		CHR$(9%)+CHR$(0%)),3%) &
		! PRINT OUT THE HEADER. &

1200	CHANGE SYS(CHR$(12%)) TO Q% &
	\ Z$="["+NUM1$(Q%(6%))+","+NUM1$(Q%(5%))+"]" &
	\ IF (Q%(26%) AND 1%)=0% THEN &
		DEF.DEV$="_SY:"+Z$ &
	  ELSE	DEF.DEV$="_"+CHR$(Q%(23%))+CHR$(Q%(24%)) &
	\	DEF.DEV$=DEF.DEV$+NUM1$(Q%(25%)) IF Q%(26%) &
	\	DEF.DEV$=DEF.DEV$+":"+Z$ &
		! SET UP THE NAME OF THE DEVICE AND THE ACCOUNT FROM WHICH WE &
		!  WERE RUN. &
	&
		! NOTE : THE BOTTOM TWO BITS OF BYTE 26 OF THE RETURNED STRING &
		! ARE CODED AS : &
		!	BIT 0 = 0 => SOURCE DEVICE IS PUBLIC &
		!		1 => SOURCE DEVICE IS PRIVATE &
		!	BIT 1 = 0 => GENERAL SPECIFICATION (IE, SY:) &
		!		1 => SPECIFIC SPECIFICATION (E.G., SY0:, DK0:) &
	&

2000	! &
	&
	&
	!	B U I L D    T H E    P R O M P T    F I L E &
	&

2010	KILL DEF.DEV$+"BACKUP.PRM" &
		! KILL THE OLD FILE IF THERE IS ONE. &

2020	OPEN DEF.DEV$+"BACKUP.PRM" FOR OUTPUT AS FILE 4% &
		! OPEN UP THE FILE. &

2030	RESTORE &
	\ READ L$ UNTIL L$="*STARTPROMPT" &
	\ FOR Z%=0% TO 15% &
		\ READ P$(Z%,0%),P$(Z%,1%) &
		\ READ P%(Z%,Z1%) FOR Z1%=0% TO 15% &
	\ NEXT Z% &
		! INPUT THE VALUES. &

2040	CLOSE 4% &
		! CLOSE THE PROMPT FILE. &

3000	! &
	&
	&
	!	B U I L D    T H E    H E L P    F I L E &
	&

3010	KILL DEF.DEV$+"BACKUP.HLP" &
		! KILL THE OLD FILE IF THERE IS ONE. &

3020	OPEN DEF.DEV$+"BACKUP.HLP" FOR OUTPUT AS FILE 8% &
		! OPEN UP THE FILE. &

3030	I%(Z%)=-1% FOR Z%=0% TO 255% &
	\ S%,B0%,T%=0% &
	\ READ L$ UNTIL L$="*STARTHELP" &
		! INITIALIZE HELP FILE INDEX AND VARIABLES. &

3040	ON ERROR GOTO 3500 &
	\ READ L$ &
	\ L$=L$+CHR$(10%)+CHR$(13%) &
	\ ON ERROR GOTO 19000 &
	\ IF LEFT(L$,1%)="*" THEN &
		IF MID(L$,2%,1%)="B" THEN &
			GOTO 3060 &
		ELSE	GOTO 3070 &
		! SET SPECIAL ERROR TRAP FOR READ &
		! READ LINE GO TO APPROPRIATE PLACE &
		! EITHER *BEGIN, *END OR OTHER. &

3050	IF S%(2%,S%)=0% THEN &
		GOTO 3600 &
	ELSE	L%=1% &
		\ T%=T%+1% &
		\ T$(T%)=MID(L$,L%,16%) &
		\ WHILE ASCII(MID(T$(T%),16%,1%)) &
			\ T%=T%+1% &
			\ L%=L%+16% &
			\ T$(T%)=MID(L$,L%,16%) &
		\ NEXT &
		\ GOTO 3040 &
		! WE ARRIVE HERE ONLY IF THE LINE IS NOT EITHER A &
		! "*BEGIN" OR "*END" &
		! IF THERE IS NOTHING IN THE CURRENT POSITION OF THE STACK &
		! THEN WE ARE FINISHED &
		! OTHERWISE, THE LINE MUST BE TRANSFERRED IN 16 CHARACTER &
		! SEGMENTS TO THE CELLS OF THE T$() ARRAY. &

3060	S%=S%+1% &
	\ B0%=B0%+1% &
	\ S%(1%,S%)=T% &
	\ S%(2%,S%)=B0% &
	\ T%=-1% IF B0%=1% &
	\ GOTO 3040 &
		! IF "*BEGIN" WAS FOUND THEN &
		! PUSH VALUE PAIR ONTO STACK, WHERE THE VALUE PAIR IS: &
		! CURRENT T$() CELL AND THE "*BEGIN" COUNT. &

3070	Z%=S%(2%,S%)-1% &
	\ I%(Z%)=S%(1%,S%) &
	\ I%(255%-Z%)=T% &
	\ S%=S%-1% &
	\ GOTO 3040 &
		! IF "*END" WAS FOUND THEN &
		! SET UP I%() VALUES FOR CURRENT BEGIN-END BLOCK. &

3500	IF ERR=57% THEN &
		RESUME 3600 &
	ELSE	RESUME 19000 &
		! IF ERROR "END OF DATA", WE'RE DONE &
		! ELSE WE DIE &

3600	Z%=((T%+1%)/32%)*32% &
	\ IF Z%<>T% THEN &
		T$(Z1%)="" FOR Z1%=T%+1% TO Z%+31% &
		! NULL FILL THE REMAINDER OF THE CURRENT BLOCK. &

3610	CLOSE 8% &
	\ GOTO 32767 &
		! CLOSE CHANNEL AND END. &

9500	! THIS IS THE PROMPT TABLE  AND SPECIAL PARAMETERS DATA. &

9501	DATA	"*STARTPROMPT"
9502	DATA	"RES[TORE], LOA[DINDEX] OR LIS[T]",""
9504	DATA	32,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0
9506	DATA	"INDIRECT FILE NAME","_SY:"
9508	DATA	32,11,0,0,0,0,0,0,16378,0,0,0,0,0,0,0
9510	DATA	"WORK FILE NAME",""
9512	DATA	32,3,0,0,0,0,0,0,16382,0,0,0,0,0,0,0
9514	DATA	"LISTING FILE","_KB:"
9516	DATA	32,11,0,0,0,10,-1,0,16368,0,0,5,0,0,0,0
9518	DATA	"FROM DISK","_SY:"
9520	DATA	32,11,0,0,1154,9,-1,4095,16382,0,0,2,0,0,0,0
9522	DATA	"INDEX FILE","PRIMARY"
9524	DATA	32,3,0,0,36,0,-1,0,16252,0,0,9,0,0,0,0
9526	DATA	"FROM DEVICE","_MT:"
9528	DATA	32,11,0,0,1026,0,-1,4095,16254,0,0,2,0,0,0,0
9530	DATA	"FROM FILES","*.*"
9532	DATA	32,11,0,0,1024,0,-1,-8192,0,0,128,6,0,-1,0,0
9534	DATA	"TO DEVICE","_MT:"
9536	DATA	32,11,0,0,1026,0,-1,4095,16254,0,0,4,0,0,0,0
9538	DATA	"TO DISK","_SY:[*,*]"
9540	DATA	32,11,0,0,2,9,-1,3199,16382,0,0,4,0,0,0,0
9542	DATA	"BEGIN AT","[*,*]*.*"
9544	DATA	32,3,0,0,0,0,-1,-8192,0,-32768,0,8,0,-1,0,0
9546	DATA	"ENTER ACCOUNTS","YES"
9548	DATA	32,15,0,0,256,0,0,0,0,0,0,0,0,0,0,0
9550	DATA	"DELETE FILES","NONE"
9552	DATA	32,3,0,0,4096,0,-1,-8192,0,0,128,7,0,-1,0,0
9554	DATA	"SUPERSEDE FILES","NONE"
9556	DATA	32,3,0,0,512,0,-1,-8192,0,0,128,7,0,-1,0,0
9558	DATA	"COMPARE FILES","NONE"
9560	DATA	32,3,0,0,8192,0,-1,-8192,0,0,128,7,0,-1,0,0
9562	DATA	"TO FILE",""
9564	DATA	32,3,0,0,0,9,-1,0,16382,0,0,18,0,0,0,0 &

10000	DATA &
"*STARTHELP", &
"*BEGIN", &
"Type '/HELP' to receive help for any question.", &
"",	&
"RES[TORE], LOA[DINDEX] or LIS[T] ?", &
"				 Specify one of the four modes:", &
"				RESTORE   - to restore the specified files", &
"					    to the system from a previously", &
"					    created off line copy.", &
"				LOADINDEX - to copy a BACKUP Index file from", &
"					    a Backup Set to a disk file.", &
"				LIST      - to list the contents of a", &
"					    Backup Set.", &
"				By appending the /SA[VE] switch, an", &
"				indirect command file can be created", &
"				from the dialogue to follow.  In order", &
"				to run a previously created indirect", &
"				file, the user should specify the", &
"				filename of the indirect command file", &
"				preceded by '@' after having specified", &
"				the desired mode.", &
""	&

10010	DATA "*BEGIN", &
"INDIRECT FILE NAME ?		 If /SA[VE] was specified in the first", &
"				question, the user must now specify the", &
"				name of the indirect file he/she wishes", &
"				to create.  The default name and", &
"				extension are enclosed in angle", &
"				brackets.", &
"",	&
"*END"	&

10020	DATA "*BEGIN", &
"WORK FILE NAME ?		 If the user wishes to rename the work", &
"				file which BACKUP will use for this", &
"				particular run, he/she responds with", &
"				the desired file specification here.  By", &
"				renaming the work file with an extension", &
"				other than '.TMP', the user can guarentee", &
"				that the auxiliary index file will not", &
"				be deleted upon logout. This index file", &
"				can then be used for some future RESTORE", &
"				or LIST operation from this Backup Set.", &
"				By placing the work file on a private", &
"				disk, the user gains processing", &
"				efficiency.", &
"",	&
"*END"	&

10030	DATA "*BEGIN", &
"LISTING FILE ?			 A file specification should be given to", &
"				route the listing file for this run.", &
"				The default is the user's own keyboard.", &
"				Legal devices are disk, keyboard,", &
"				line-printer, and DECtape.", &
"",	&
"*END"	&

10040	DATA "*BEGIN", &
"FROM DISK ?			 In a BACKUP operation, the user", &
"				must specify which disk unit is to", &
"				be backed up.  The default is the", &
"				public structure.", &
"",	&
"*END"	&

10050	DATA "*BEGIN", &
"INDEX FILE ?			 In a RESTORE or LIST operation, the user", &
"				must specify where the index for the", &
"				Backup Set is to be found.  The Index", &
"				can either be the Primary Index file", &
"				found within the Backup Set itself, or", &
"				can be the original work file used in", &
"				the BACKUP operation used to create the", &
"				Backup Set.", &
"",	&
"*END"	&

10060	DATA "*BEGIN", &
"FROM DEVICE ?			 In a RESTORE, LOADINDEX or LIST operation,", &
"				the user must specify which (generic)", &
"				device contains the Backup Set. This device", &
"				must be either magtape or disk. The default", &
"				is magtape.", &
"",	&
"*END"	&

10070	DATA "*BEGIN", &
"FROM FILES ?			 The user here specifies which files are", &
"				to be backed up or restored.  The", &
"				default is all files from his/her", &
"				account.", &
"",	&
"*END"	&

10080	DATA "*BEGIN", &
"TO DEVICE ?			 In a BACKUP operation, the user must", &
"				specify which (generic) device is to be", &
"				used to create the Backup Set.  This must be", &
"				either magtape or disk. The default is", &
"				magtape.", &
"",	&
"*END"	&

10090	DATA "*BEGIN", &
"TO DISK ?			 In a RESTORE operation, the user must", &
"				specify the destination disk unit to", &
"				which files will be RESTOREd.  The", &
"				default is the public structure under", &
"				the original accounts from which the", &
"				files were BACKed up.", &
"",	&
"*END"	&

10100	DATA "*BEGIN", &
"BEGIN AT ?			 The response to this question allows", &
"				the user to choose a starting point", &
"				(account and file, or account only)", &
"				among the files he/she has specified to", &
"				be BACKed up or RESTOREd.  The Backup or", &
"				RESTORE will start at the specified", &
"				ACCOUNT AND FILE and will exclude all", &
"				accounts before the specified account", &
"				and all files within the account coming", &
"				before the specified file.  The default", &
"				is 'Start at the beginning'.", &
"",	&
"*END"	&

10110	DATA "*BEGIN", &
"ENTER ACCOUNTS ?		 In a RESTORE operation this question", &
"				is asked of a privileged user.  If 'Yes'", &
"				(the default) is specified, any accounts", &
"				to be RESTOREd which do not exist on the", &
"				destination disk will be entered there", &
"				so that files can be RESTOREd.  If 'No'", &
"				is specified, no accounts will be", &
"				entered and the files for missing", &
"				accounts will not be transferred.", &
"",	&
"*END"	&

10120	DATA "*BEGIN", &
"DELETE FILES ?			 In a BACKUP operation, files specified", &
"				in response to this question will be", &
"				deleted after transfer, provided no", &
"				errors were encountered on transfer.", &
"",	&
"*END"	&

10130	DATA "*BEGIN", &
"SUPERSEDE FILES ?		 In a RESTORE operation, files specified", &
"				in response to this question will be", &
"				overwritten if they already exist on the", &
"				destination disk.", &
"",	&
"*END"	&

10140	DATA "*BEGIN", &
"COMPARE FILES ?			 Files specified in response to this", &
"				question will be verified after transfer", &
"				on both a BACKUP and a RESTORE", &
"				operation.", &
"",	&
"*END"	&

10150	DATA "*BEGIN", &
"TO FILE ?			 In a LOADINDEX operation, the user must", &
"				specify the disk file into which the Backup", &
"				Set Index file is to be loaded.", &
"",	&
"*END",	&
"*END"	&
	&

19000	! &
	&
	&
	!	E R R O R    H A N D L E R &
	&
	&

19010	RESUME 2020 IF ERL=2010 &
	\ RESUME 3020 IF ERL=3010 &
		! FILE DOES NOT EXIST AND WE ARE TRYING TO KILL IT. &

19020	IF ERL=2020 THEN PRINT "YOU ARE NOT PRIVILEGED" &
			\ RESUME 32767 &
		! CAN NOT OPEN THE FILE. &

19030	PRINT "ERROR#";ERR;" AT LINE#";ERL;RIGHT(SYS(CHR$(6%)+ &
				CHR$(9%)+CHR$(ERR)),3%) &
			\ RESUME 32767 &
		! ANY OTHER ERROR IS UNEXPECTED. &

32767	END &

