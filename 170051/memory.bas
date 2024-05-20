2!		PROGRAM		: MEMORY.BAS
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
  !		      Copyright (C) 1979, 1991 by &
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
	! VER/ED	EDIT DATE	REASON &
	! 9.0-08 bgn	22-Aug-84	ADD SUPPORT FOR LARGE CHUNKS OF &
	!				XBUF, LOCKED  OUT MEMORY, VIRTUAL &
	!				DISK &
	! 9.2-03 DLS	6-DEC-85	CHANGED PEEKS INTO JDB2 TO USE &
	!				INFORMATION RETURNED BY UU.SYS &
	! 10.0-I REG	07-Feb-90	FIX DV0:/LOCKED-OUT BUG &
	! &
	&

1000	! &
	! &
	! &
	!	M A I N    P R O G R A M &
	! &
	! &
	! &
	DIM F%(30%) &
	\ CHANGE SYS(CHR$(6%)+CHR$(-3%)) TO F% &
	\    UNTCLU%=F%(17%)+SWAP%(F%(18%)) &
	\    UNTCNT%=F%(19%)+SWAP%(F%(20%)) &
	\    SATCTL%=F%(21%)+SWAP%(F%(22%)) &
	\    JOBTBL%=F%(11%)+SWAP%(F%(12%)) &
	\    J%=F%(9%)+SWAP%(F%(10%)) &
	\ CHANGE SYS(CHR$(6%)+CHR$(-12%)) TO F% &
	\    DEVNAM%=F%(5%)+SWAP%(F%(6%)) &
	\    CSRTBL%=F%(7%)+SWAP%(F%(8%)) &
	\ CHANGE SYS(CHR$(6%)+CHR$(-29%)) TO F% &
	\    SATEND%=F%(7%)+SWAP%(F%(8%)) &
	\ GOSUB 10200 &
	&
	\ PRINT &
	\ PRINT "Memory layout:" &
	\ PRINT "Start    End  Length  Content" &
		! &
		! LOOKUP MONITOR TABLES PART I, II, III. &
		! GET THE ROOT OF MEMLST. &
		! GET THE LOCATION OF JOBTBL. &
		! PRINT HEADER. &

1020	IF (J% AND 8%+16%) THEN &
		C$=RAD$(PEEK(J%-6%))+RAD$(PEEK(J%-4%)) &
	\	I%=0% &
	\	IF (J% AND 8%) &
		    THEN &
			IF (PEEK(J%+20%) AND 128%) &
			    THEN &
				T$="LIB" &
			    ELSE &
				T$="RTS" &
		    ELSE &
			GOSUB 10000 &
	\		C$=SYS(CHR$(6%)+CHR$(26%)+CHR$(I%/2%)+CHR$(0%)) &
	\		C$=RAD$(SWAP%(CVT$%(MID(C$,17%,2%))))+ &
			   RAD$(SWAP%(CVT$%(MID(C$,19%,2%)))) &
	! &
	! IDENTIFY JOB, RTS OR LIB. &
	! GET THE NAME OF THE RTS OR LIB BY PEEKING AT &
	!	THE APPROPRIATE PLACE. &
	! IF WE HAVE A RTS OR LIB, DISTINGUISH BETWEEN THE TWO. &
	! IF WE HAVE A JOB, GO FIND ITS JOB NUMBER. &
	! GET THE NAME OF THE JOB. &
	! &
	! THE LOW ORDER BITS OF J% (THE ADDRESS OF THE MEMLST &
	! ENTRY) DETERMINE THE TYPE OF ENTRY AS THE MEMLST &
	! ENTRY HAS DIFFERENT OFFSETS IN THE DATA STRUCTURE &
	! DESCRIBING THE ENTITY DEPENDING ON THE ENTITY. &
	! THE OFFSETS ARE DEFINED AS FOLLOWS: &
	!	0	MONITOR &
	!	2	EXTENDED BUFFER POOL (XBUF) &
	!	4	LOCKED OUT MEMORY (LCK) &
	!	6	NON-EXISTANT MEMORY (NXM) &
	!	8	RUN TIME SYSTEM OR RESIDENT LIBRARY &
	!      16	USER JOB &

1030	C$=MID("MONITOR   ** XBUF **Locked out   NXM   ", &
		(J% AND 6%)*5%+1%,10%) UNLESS (J% AND 8%+16%) &
	\ C$=C$+" "+T$ IF (J% AND 8%+16%) &
	\ P%=PEEK(J%+8%)/32% &
	\ P%=2048%+P% IF P%<0% &
	\ N%=PEEK(J%+2%) &
	\ IF N%=0% THEN &
		PRINT FNK$(P%);" -  *** END ***" &
	  ELSE	T%=PEEK(J%+4%) &
	\	S%=(PEEK(J%+6%) AND 255%) &
	\       GOSUB 10100 UNLESS (J% AND 8%+16%) OR (N% AND 8%+16%) &
	\	GOSUB 10250 IF ((J% AND 6%)=4%) AND ((J% AND 8%+16%)=0%) &
	\	S1%=S% &
	\	S%=VSIZE% IF P%=VSTAR% AND VSTAT%>=0% &
	\	PRINT	FNL$(P%,S%);C$ &
	\	PRINT	FNL$(P%+VSIZE%,S1%-VSIZE%);"Locked out" &
			IF P%=VSTAR% AND VSTAT%>=0% AND VSIZE%<S1% &
	\	PRINT	FNL$(P%+S%,T%-S%);"  (Free)  " &
			IF (T%-S%) AND (P%<>VSTAR%) &
	\	J%=N% &
	\	GOTO 1020 &
		! &
		! IF IT WASN'T A RTS, LIB, OR USER JOB, &
		!	THEN FIGURE OUT WHAT KIND OF MEMORY IT WAS. &
		! LOOK AT THE MEMLST ENTRY TO FIND OUT LOCATION INFO. &
		! THE MEMLST ENTRY HAS THE FOLLOWING FORMAT: &
		!      BYTES	CONTENTS &
		!	0-1	POINTER TO PREVIOUS MEMLST ENTRY. &
		!	2-3	POINTER TO NEXT MEMLST ENTRY. &
		!	4-5	TOTAL SIZE OF ITEM IN K (INCL. UNUSED MEMORY) &
		!	 6	AMNT. USED MEMORY FOR THIS ITEM IN K. &
		!	 7	CONTROL INFORMATION FOR THIS ITEM. &
		!	8-9	PHYSICAL ADDRESS OF START OF ITEM &
		!			DIVIDED BY 100(8) IN BYTES. &
		! IF THE POINTER TO THE NEXT ENTRY IS ZERO, THEN WE &
		!	HAVE THE LAST ENTRY IN MEMLST AND ARE THEREFORE &
		!	AT THE END OF KNOWN MEMORY. &
		! PRINT OUT THE STARTING PLACE AND SIZE OF THIS ITEM. &
		! IF THERE IS UNUSED SPACE LEFT OVER, PRINT A SEPARATE &
		!	LINE INDICATING "(Free)" AS CONTENTS. &

1040	GOTO 32767 &
		! &
		! IF WE PRINTED "*** END ***" ABOVE, WE FALL THROUGH &
		! TO HERE.  WE'RE ALL THROUGH.  GO EXIT. &

10000	! &
	! &
	! &
	!	S U B R O U T I N E &
	! &
	! &
	! &
	I%=I%+2% &
	\ N%=PEEK(JOBTBL%+I%) &
	\ GOTO 10000 IF N%<>J%-16% AND N%<>-1% &
	\ I%=0% IF N%=-1% &
	\ T$="J"+RIGHT(NUM1$(100%+I%/2%),2%) &
	\ RETURN &
		! &
		! GIVEN I%=0%, FIND OUT WHICH JOB HAS ITS JDB AT &
		!	LOCATION J%-16%. &
		! RETURN JOB NUMBER IN T$ IN THE FORM "Jnn". &
		! IF THE JOB WAS KILLED BEFORE WE COULD FIND IT, &
		!	RETURN T$="J00". &

10100	IF (N% AND 6%) = (J% AND 6%) THEN &
		S%=S%+(PEEK(N%+6%) AND 255%) &
	\	T%=T%+PEEK(N%+4%) &
	\ 	N%=PEEK(N%+2%) &
	\	GOTO 10100 UNLESS N% AND 8%+16% &
		! IF THIS IS LOCKED OUT MEMORY, XBUF, OR NXM &
		! PRINT OUT ALL OF IT, NOT IN 127K CHUNKS &

10110	RETURN &
		! RETURN IF PREVIOUSLY CALLED (AND WE FOUND THE VIRTUAL DISK) &

10200	DV%=ASCII("D")+ASCII("V")*256% &
	\ INDEX%=0% &
		! INDEX INTO MONITOR TABLES SET TO ZERO &

10210	DEV%=PEEK(DEVNAM%+INDEX%) &
	\ IF DEV%=-1% THEN GOTO 10240 &
		! END OF THE LIST? &

10220	IF DEV%<>DV% THEN &
	  INDEX%=INDEX%+2% &
	\ GOTO 10210 &
		! FOUND IT?  IF NOT KEEP GOING THROUGH DEVNAM TABLE &

10230	VSTAT%=PEEK(UNTCNT%+INDEX%) &
	\ IF VSTAT%>=0% THEN FREE%=PEEK(SATCTL%+INDEX%) &
	\  VSTAR%=PEEK(CSRTBL%+INDEX%) &
	\  VSTAR%=VSTAR%+2048% IF VSTAR%<0% &
	\  VSIZE%=(PEEK(UNTCLU%+INDEX%) AND 255%)* &
		  (PEEK(SATEND%+INDEX%)+1%)+1% &
	\  VSIZE%=INT((VSIZE%/4.0)+.9999) &
		! IF IT'S MOUNTED GET FREE SPACE, START ADDRESS, TOTAL SIZE &

10240	RETURN &

10250	IF VSTAR%=P% AND VSTAT%>=0% THEN &
	   C$="** Virtual Disk ** (Free Space = "+NUM1$(FREE%)+" Blocks)" &
		! IF THE VIRTUAL DISK IS MOUNTED (AND THIS IS REALLY THE &
		! START OF THE VIRTUAL DISK) THEN PRINT IT &

10260	RETURN &

15000	! &
	! &
	! &
	!	F U N C T I O N S &
	! &
	! &
	! &
	DEF* FNK$(I%) &
	\ K$=NUM1$(I%) &
	\ FNK$=SPACE$(4%-LEN(K$))+K$+"K" &
	\ FNEND &
		! &
		! RETURN NUMBER LEFT JUSTIFIED WITH SPACES IN A &
		! FOUR CHARACTER FIELD FOLLOWED BY "K". &

16000	DEF* FNL$(P%,S%)=FNK$(P%)+" - "+FNK$(P%+S%-1%)+" ("+FNK$(S%)+") " &
		! &
		! RETURN A STRING OF THE FORM "nnnnK - mmmmK (jjjjK)". &

30000	! &
	! &
	! &
	!	C C L    E N T R Y    P O I N T &
	! &
	! &
	! &
	GOTO 1000 &
		! &
		! SAME AS A RUN ENTRY. &

32767	END
