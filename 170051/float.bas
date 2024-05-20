2!		PROGRAM		: FLOAT.BAS
5!		VERSION		: V10.1
6!		EDIT		: A
7!		EDIT DATE	: 10-MAY-91
10	EXTEND
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

101	! &
	!	This program will compute the floating CSR assignments &
	!	for devices supported by RSTS/E V9.0 &

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&

301	! &
	!	There are no I/O channels in this program &

400	! &
	&
	&
	!	V A R I A B L E    D E F I N I T I O N S &
	&
	&

401	!	VARIABLE NAME		USED FOR &
	! &

800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&

801	!	FUNCTION/SUBROUTINE		USE &
	! &
	!	10100			Converts 16 bit numeric value &
	!				to octal Ascii string of digits &
	&

900	! &
	&
	&
	!	D I M E N S I O N    S T A T E M E N T S &
	&
	&

920	DIM DEVICE$(50%), DEVNAM$(50%), CSRSIZE%(50%), OUTSTR$(800%) &
	! &
	! DEVICE$()	Array with Ascii device names &
	! DEVNAM$()	RSTS/E device names &
	! CSRSIZE%()	Size of CSR for controller &
	! OUTSTR$()	Array of controllers configured &
	! &

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&

1000	PRINT IF CCPOS(0%)<>0% &
	\ ON ERROR GOTO 19000 &
	\ CSR% = -8192% &
	\ CNTDEV% = 0% &
	!	Return the KB to the left hand margin &
	!	Set up the standard error traps &
	!	-8192% is octal 160000 which is the base of &
	!	the floating address region &
	!	Set up a counter of devices in table &

1010	I$="V10.1-A" &
	!	Set up the version number &

1020	PRINT "FLOAT	" + I$ + CHR$(9%)+ &
	CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)),3%),4%) &
	\ PRINT &
	\ PRINT "Please respond with the number of controllers for" &
	\ PRINT "each device type prompted below. A summary of the" &
	\ PRINT "CSRs for  each unit you  select will be output to" &
	\ PRINT "your terminal.   If you do not have a device type" &
	\ PRINT "configured on your system, you may just press the" &
	\ PRINT "RETURN key." &
	\ PRINT &
	!	Display the header and give some directions. &
	&

1030	DATA &
	"DJ11",				"DJ",	8, &
	"DH11",				"DH",	16, &
	"DQ11",				"..",	8, &
	"DU11/DUV11",			"DU",	8, &
	"DUP11",			"DUP",	8, &
	"LK11-A",			"..",	8, &
	"DMC11/DMR11",			"XM",	8, &
	"DZ11/DZV11/DZ32",		"DZ",	8, &
	"KMC11",			"XK",	8, &
	"LPP11",			"..",	8, &
	"VMV21",			"..",	8, &
	"VMV31",			"..",	16, &
	"DWR70",			"..",	8, &
	"Additional RL11/RLV11s",	"..",	8, &
	"LPA11-K",			"..",	16, &
	"KW11-C",			"..",	8, &
	"Reserved",			"..",	8, &
	"Additional RX11/RX211s",	"RX",	8, &
	"DR11-W",			"..",	8, &
	"DR11-B",			"..",	8, &
	"DMP11",			"XD",	8, &
	"DPV11",			"DPV",	8, &
	"ISB11",			"..",	8, &
	"DMV11",			"XD",	16, &
	"Additional UNAs",		"XE",	8, &
	"Additional MSCPs",		"RU",	4, &
	"DMF32",			"..",	32, &
	"KMS11",			"..",	16, &
	"VS100",			"..",	16, &
	"Additional TMSCP's",		"MU",	4, &
	"KMV11",			"..",	16, &
	"DHV11/DHU11",			"VH",	16, &
	"*END*",			"..",	0 &

1035	! &
	! Table of Devices, Device Names, and CSR sizes &
	! You may add any number of new devices to the end &
	! of this table for newly announced devices, but &
	! make sure the number of devices do NOT exceed the &
	! size of the arrays. &
	! &
	! The format of the table entries is: &
	! &
	! o  The Ascii Device name &
	! o  The RSTS/E system device name (eg XD:) &
	! o  The CSR size &
	! &
	! Even though RSTS/E does not support some of the &
	! devices in the floating address region, you MUST &
	! include them as they will generate a gap to indicate &
	! that none of these devices exists. &
	! &
	! If your RSTS/E system has some devices not supported &
	! by RSTS/E but actually on the system, you may change &
	! the ".." to some other name, and the program will &
	! prompt you for the number of controllers for that &
	! device.  This may be useful for those of you that &
	! want to run this program to configure addresses for &
	! a machine that will run RSX-11M. &
	! &
	&

1040	WHILE DEVICE$(CNTDEV%) <> "*END*" &
	\	CNTDEV% = CNTDEV% + 1% &
	\	READ DEVICE$(CNTDEV%), DEVNAM$(CNTDEV%), CSRSIZE%(CNTDEV%) &
	\ NEXT &
	\ CNTDEV% = CNTDEV% - 1% &
	! &
	!	While we have not found the end of the table &
	!	Increment the array pointer for each line &
	!	Read in the entry &
	!	Continue looping then when we get the *END* we &
	!	Decrement the count of entries by one which &
	!	skips the *END* we just READ in. &

1050	FOR I%=1% TO CNTDEV% &
	! &
	!	For how many devices we found in the table &

1060	 NUMBER%=0% &
	\ IF DEVNAM$(I%)<>".." THEN &
		PRINT DEVICE$(I%); &
	\	INPUT NUMBER% &
	! &
	!	Set the default number of controllers for this &
	!	device type to zero (we have none). &
	!	Then, if we support this device on the system, prompt &
	!	the user for the number of controllers he/she has. &

1070	IF NUMBER%<0% OR NUMBER%>16% THEN &
		PRINT "%Outside valid range" &
	\	GOTO 1060 &
	! &
	!	Mini error check - valid range is 0 thru 16 inclusive &

1080	CSR% = CSR% + CSRSIZE%(I%) AND -CSRSIZE%(I%) &
	\ IF NUMBER%=0% THEN GOTO 1110 &
	! &
	!	The gap is determined by the next device's CSR set &
	!	size.  Add the device size and round down to a multiple &
	!	of that device's size. &

1090	FOR N%=0% TO NUMBER%-1% &
	\	GOSUB 10100 &
	\	OUTSTR% = OUTSTR% + 1% &
	\	OUTSTR$(OUTSTR%) = DEVNAM$(I%) + NUM1$(N%) + &
			":  " + CSRADDR$ &
	\	OUTSTR$(OUTSTR%) = DEVNAM$(I%) + NUM1$(N%+1%) + &
			":  " + CSRADDR$ &
			IF DEVNAM$(I%) = "RX" OR DEVNAM$(I%)="RU" &
			OR DEVNAM$(I%) = "XE" OR DEVNAM$(I%)="MU" &
	\	CSR% = CSR% + CSRSIZE%(I%) &
	! &
	!	For the number of controllers for this device &
	!	Go convert the the CSR address to an Ascii string &
	!	of octal digits &
	!	Bump the pointer of devices configured &
	!	Save this device in the array &
	!	Compute the next valid CSR address &

1100	NEXT N% &
	! &
	!	Continue for each unit selected for this device &

1110	CSR% = CSR% + 2% &
	\ IF NUMBER% > 0%  THEN &
		OUTSTR% = OUTSTR% + 1% &
	\	OUTSTR$(OUTSTR%) = "" &
	! &
	!	There must be at least a 1 word gap between each &
	!	device type. &
	!	If the number of controllers for this device type &
	!	was greater than zero then we will also create a &
	!	blank line between the device types on the output. &

1120	NEXT I% &
	! &
	!	Next device from table &

1130	PRINT &
	\ PRINT "The standard floating address assignments are:" &
	\ PRINT &
	\ PRINT OUTSTR$(I%)	FOR I%=1% TO OUTSTR% &
	! &
	!	Display all the devices configured and their &
	!	standard addresses &

1140	GOTO 32767 &
	! &
	!	Well, its time to quit &

10100	! This routine will convert an integer number (CSR%) into it's &
	! octal representation in Ascii format (CSRADDR$) &

10110	ADDR% = CSR% &
	\ CSRADDR$ = "1" &
	\ IF ADDR% < 0% THEN &
		ADDR% = ADDR% AND 32767% &
	ELSE 	CSRADDR$ = "0" &
	! &
	!	Save the number we intend to convert to octal &
	!	Start off assuming that the sign bit is on, and &
	!	thus the octal number starts with "1". &
	!	If the number is negative save the rest of the &
	!	bits else change the first digit of the number to "0". &

10120	FOR M% = 1% TO 5% &
	\	OCT% = ADDR%/(8%**(5%-M%)) AND 7% &
	\	CSRADDR$ = CSRADDR$ + CHR$(48%+OCT%) &
	\ NEXT M% &
	! &
	!	For the other five possible octal digits &
	!	Compute an octal digit &
	!	Add the octal digit to the string &
	!	Continue for all digits of the number &

10130	RETURN &
	! &

19000	! &
	&
	!	E R R O R    H A N D L I N G &
	&

19010	IF ERL=1060% THEN &
		RESUME 32767 IF ERR=11% &
	\	IF ERR=50% THEN &
			PRINT "%Illegal number" &
	\		RESUME 1060 &
	! &
	!	If we get an error when the user is entering the &
	!	number of controllers for this device type then: &
	!	  quit if they typed CTRL/Z &
	!	  if they entered an illegal numeric quantity, tell &
	!	  them and give them another chance. &
	!	All other errors at this line fall through. &

19020	PRINT "?Unexpected error"; ERR; "at line"; ERL &
	\ PRINT CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),3%),4%) &
	\ RESUME 32767 &
	! &
	!	Hit an unexpected error. Tell them the error number, &
	!	line, and text. &

32767	END
