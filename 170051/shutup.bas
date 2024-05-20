2!		PROGRAM		: SHUTUP
5!		VERSION		: V10.1
6!		EDIT		: H
7!		EDIT DATE	: 23-DEC-91
8!
10		EXTEND		! PROGRAM IS WRITTEN IN EXTEND MODE
11	! &
	! &
	! &
	!		  C O P Y R I G H T &
  !	&
  !	&
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
  !*******************************************************************
20	! &
	! &
	! &
	!	M O D I F I C A T I O N    H I S T O R Y &
	&
	&

21	! VER/ED	EDIT DATE	REASON &
	! V9.0-06	09-MAY-84	(KMF) Add priv check &
	! &
	! V9.0-08	02-OCT-84	(PRL) Remove "OPSER not running" msg &
	!				(PRL) Change <esc> to '^' &
	!				(PRL) Fix various error messages &
	!				(PRL) Remove "OPSER not running" msg &
	! &
	! V9.0-09	05-NOV-84	(PRL) Change OPSER1.WRK loc to OPSER$: &
	!				(PRL) Fix phase heading print function &
	! &
	! V9.0-11	08-Feb-85	(GPK) Don't try to remove default KBM &
	! &
	! V9.0-12	01-MAR-85	(DLS) Improved priv check &
	!				(DLS) Initial job kill phase changes &
	!				(DLS) Final job kill phase changes &
	! &
	! V9.2-09	04-APR-86	(DLS) CHECK ERRCPY JOB AND RECVR &
	!				      BEFORE GOING TO THE NEXT PHASE &
	! &
	! V9.3-14	18-AUG-86	(DLS) Improve sleep loops &
	!				(DLS) Improved initial and final job &
	!					killing loops &
	!				(DLS) Changed shutdown time prompts &
	!				(DLS) Fixed bug from prev baselevel &
	! 9.3-17	10-NOV-86	(D G) ADD UNDERSCORE TO NL:'S &
	! &
	! 9.3-18	20-NOV-86	(DLS) Try to remove swap files twice &
	!				(DLS) Display Node name if DECnet &
	! &
	! 9.4-1		06-FEB-87	(DRW) Allow shutdown of EMT receiver &
	!				(DRW) local object type (LOT) = 2 &
	! V9.4-02	25-Feb-87	(JJT) Fix command backup &
	! V9.6		25-Jan-88	(REG) Allow running from any local, &
	!					attached KB: &
	! V9.6-06	03-Feb-88	(REG) Ask if oper wants AUTO RESTART &
	! V9.6-08	21-Mar-88	(JJT) Allow local or dialup consoles &
	! V9.6-10	02-May-88	(REG) Broadcast messages to all KB's &
	!				except our own. &
	! V9.7-01	16-Nov-88	(JJT) Trap error 10 at line 1210 &
	! V9.7-05	15-Feb-89	(REG) Trap open-file-on-disk errors &
	! V9.7-06	15-Mar-89	(WRM) Add log file close phase &
	! V10.0-E	??-Oct-89	(DBM) Add OMS Shutdown phase &
	! V10.0-F	10-Nov-89	(JJT) Add error trapping on line 2980 &
	! V10.0-K	11-Apr-90	(JRM) Delay shutup SYS call to allow &
	!				all LAT terminal output to complete. &
	! V10.1-B	20-Jun-91	Fix "abort when trying to kill non- &
	!				existent job" bug &
	! V10-1-D	07-Aug-91	(JJT) Provide a CCL silent entry &
	! V10-1-H	05-Dec-91	(JJT) Fix comments for CCL entry &
	&

100	! &
	! &
	! &
	!	P R O G R A M   D E S C R I P T I O N &
	&

110!	The SHUTUP program is used for causing an orderly ceasing of time- &
   !	sharing operations under RSTS/E.  It should be used in all cases &
   !	except extreme emergency, in order to protect system and user file &
   !	structures and data. &
   !	&
   !	SHUTUP may be used in cooperation with OPSER, if the Spooling &
   !	package is installed, to permit orderly shutdown of the various &
   !	spooling programs which may be running on the system. &
   !	&
   !	SHUTUP proceeds through a series of "phases", during which specific &
   !	procedures are followed to reach a required "state" in the shutdown &
   !	process.  As the "phases" are described below, the various modes of &
   !	interaction between SHUTUP and OPSER will also be discussed. &
   !	&
   !	SHUTUP must be run from a local attached KB: &
   !	&
   !	&
   !	SHUTUP must be run only from the compiled form (.TSK). Otherwise, &
   !	certain operations will fail later in the process. &
   !	&
   !	SHUTUP can be run by a CCL entry. To do this, define the CCL SHUTUP to &
   !	point to the CCL line (30000) of _SY0:[1,2]SHUTUP.TSK.  Then, execute &
   !	the CCL by typing 'SHUTUP/T:n' where n is a time in minutes from 0 to &
   !	99 which is the minutes until system shutdown.  The argument n must be &
   !	specified.  SHUTUP will then answer all of the prompts itself and will &
   !	not display any messages.  It will then shutdown the system and reboot &
   !	the system. &
   !	&

120!	#########  Set-up Dialogue Phase  ######### &
   !	&
   !	During this initial phase, several queries may appear to obtain &
   !	user specified parameters for controlling the shutdown process. &
   !	If OPSER is present, the following query will appear: &
   !	&
   !	Use OPSER for utilities shutdown <YES>? &
   !	&
   !	If the user responds with N or NO, then the following query appears &
   !	to reinforce his desire to not use OPSER: &
   !	&
   !	Are you sure you don't want to use OPSER <NO>? &
   !	&
   !	If the answer to this is Y, YE or YES, then OPSER will be ignored &
   !	and shutdown will proceed, handling the spooling package programs as &
   !	normally running user programs. &
   !	&
   !	If the answer is N or NO or <cr>, then the original OPSER query &
   !	will re-appear. &
   !	&
   !	If OPSER is going to be used during shutdown, the following query &
   !	will appear: &
   !	&
   !	Allow utilities to reach logical end point <YES>? &
   !	&
   !	If the answer is <cr>, Y, YE or YES, then OPSER will shutdown &
   !	the spooling programs at the end of their current job processing. &
   !	This means that any current BATCH jobs will be completed and any &
   !	print jobs will be completed before the spooling programs go off &
   !    line. &
   !	&
   !	If the answer is N or NO, then OPSER will ask each spooling program &
   !	to ABORT its current job, if any in process, and end operations &
   !	immediately.  In either case, when all spoolers are finished, OPSER &
   !	will terminate itself and go away. &
   !	&
   !	For timing purposes, SHUTUP will allow OPSER 120 seconds to &
   !	complete its operations in the immediate mode case and 60 minutes &
   !	complete its operations in the logical end mode case. &
   !	&
   !	The following query will always appear to get the parameter &
   !	for the next phase, when user warning messages will be sent out: &
   !	&
   !	Minutes until system shutdown (0-99) <5>? &
   !	&
   !	The user types in the number of minutes before SHUTUP is to &
   !	really begin the shutdown of operations.  To specify no waiting &
   !	time (0 minutes), the value of zero (0) must be entered explicitly. &
   !	&
   !	If DECnet/E is present, SHUTUP will print the following message: &
   !	&
   !	Minutes until new network activity is disabled (0-99) <5>? &
   !	&
   !	"Shutdown with automatic RESTART <NO>?" asks the operator if (s)he &
   !	wants that option. &
   !
130!	########  Warning Message Phase  ######## &
   !	&
   !	The waiting time value obtained in the last query is used to govern &
   !	the sending of warning messages to system users as follows. Beginning &
   !	immediately, all candidate terminals in the system are sent the first &
   !	warning message (messages do not go to our own KB:): &
   !	&
   !	System going down in <n> minutes, Please finish up &
   !	&
   !	where <n> is the time specified in the query. &
   !	&
   !	The following messages appear on the shutup console: &
   !	&
   !	Further LOGINs are now disabled &
   !	Further network activity is now disabled - If DECnet/E is running &
   !	<n> minute warning message sent &
   !	&
   !	The waiting period before sending the next warning message is computed &
   !	as follows: &
   !	&
   !	Waiting time = (time left)/5% + 1% minutes &
   !	&
   !	which can be seem to converge to one (1) minute intervals at two (2) &
   !	minutes before shutdown begins. After the waiting time period expires, &
   !	the termina warning message is sent to all terminals and the operator &
   !	warning message is sent to the shutup console. &
   !	&
   !	When the waiting time reduces to zero (0), or the initial period was &
   !	specified as zero (0), the final warning message appears: &
   !	&
   !	**** FINAL WARNING  System shutting down **** &
   !	NOTE: THERE ARE  EXCLAMATION POINTS IN THE FINAL WARNING MESSAGE. &
	&

140!	########  Initial Job Killing Phase  ######## &
   !	&
   !	During this phase, the first "shot" at killing jobs is performed. &
   !	Jobs are killed in one of two (2) ways: &
   !	&
   !	For detached jobs, use the SYS call for killing a job &
   !	For attached jobs, force CTRL/C, '$_LOGOUT/BRIEF', <cr> to the jobs &
   !	terminal. &
   !	&
   !	This phase makes two passes through the table of currently active jobs. &
   !	In the first pass, "killing" proceeds as above.  If a second pass is &
   !	necessary, the only the SYS call "kill" is done to all remaining jobs. &
   !	(One additional pass may be made simply for verification purposes &
   !	see that all "killed" jobs actually went away.) &
   !	&
   !	When OPSER is in use, two modifications are made to the above passes. &
   !	First, the on-line job table for OPSER is scanned and any jobs which &
   !	are on-line to OPSER are skipped during the passes.  Additionally, &
   !	all attached jobs are checked to see if their console terminal is a &
   !	psuedo-keyboard terminal(could be controlled by a BATCH spooler). &
   !	These jobs are also skipped. &
   !	&
   !	If at any time during the passes, SHUTUP finds logins set to something &
   !	other than 1, it will abort the shutdown procedure with the error &
   !	message: &
   !	&
   !	?LOGINs are not disabled &
   !	&
   !	This same check and error message may occur at other places during the &
   !	shutdown run. &
   !	&
   !	During the phase, if the total number of jobs remaining in the system &
   !	goes to one (1), then only SHUTUP can be running and the program will &
   !	immediately skip to the final shutdown phases.  This check and operation &
   !	is used several places during the first few phases to provide for the &
   !	fastest shutdown operation possible. &
   !	&
   !	If SHUTUP gets to the end of the second pass, and all jobs which &
   !	should have been killed were not, the procedure is aborted with the &
   !	following error message: &
   !	&
   !	?SHUTUP failed during initial job killing phase &
   !	&
   !	During the first pass, a count of the number of jobs "killed" by the &
   !	terminal "force" method is kept.  At the end of the pass, a pause via &
   !	the SLEEP construct is made, the length of the pause being calculated &
   !	as follows: &
   !	&
   !	SLEEP value = SLEEP.BASE% + SLEEP.INCREMENT% * counter &
   !	&
   !	The current version of SHUTUP comes with the following values of &
   !	SLEEP.BASE% and SLEEP.INCREMENT%: &
   !	&
   !	SLEEP.BASE%	= 10%	(seconds) &
   !	SLEEP.INCREMENT%=  2%	(seconds) &
   !	&
   !	The installation may change these values at statement # 1031. &
   !
145!	######## Print/Batch Shutdown Phase  ######## &
   !	&
   !	This phase is carried out only if PBS is running. If it is not, &
   !	SHUTUP continues with the next phase (no message is shown). &
   !	&
   !	First, SHUTUP is declared as a receiver, and sends an offline &
   !	message to QMAN telling QMAN to shutdown PBS.  The offline &
   !	message consists of the command number and spl.flag with the &
   !	following values: &
   !	&
   !		Command = 16	(offline request) &
   !		SPL.FLAG = 0	(normal shutdown, complete running jobs) &
   !		SPL.FLAG = 1	(immediate shutdown, terminate running jobs) &
   !	&
   !	QMAN acknowledges the shutdown command by sending back a &
   !	confirmation message.  If no confirmation message is received by &
   !	SHUTUP within two minutes after the offline request command is &
   !	sent, SHUTUP will abort shutdown operations with the following &
   !	error message : &
   !	&
   !	?No response from Print/Batch Services after 2 minutes. &
   !	&
   !	Otherwise, SHUTUP prints the number of entries currently processing &
   !	in PBS which are left to complete or which are being terminated at &
   !	that point.  This message appears in one of the following formats: &
   !	&
   !	No jobs in Print/Batch Services. &
   !	Print/Batch Services shutting down - # jobs are completing. &
   !	Print/Batch Services shutting down - # jobs are being terminated. &
   !	&
   !	then, SHUTUP begins waiting on QMAN to shutdown PBS.  The length &
   !	of the wait period is: &
   !	&
   !	Normal shutdown:  allow running jobs to complete  -  1 hour &
   !	Immediate shutdown:  terminate running jobs       -  1 minute &
   !	&
   !	If QMAN's receiver does not disappear within the specified amount &
   !	of time, SHUTUP aborts with the following error message: &
   !	&
   !	Print/Batch Services shutdown taking too long &
   !	&
   !	Otherwise, SHUTUP will display the following message and continue &
   !	with the next phase: &
   !	&
   !	Print/Batch Services shutdown complete at hh:mm am/pm. &
   !
150!	########  OPSER Shutdown Phase ######## &
   !	&
   !	This phase occurs only if OPSER being used as part of the shutdown &
   !	process. &
   !	&
   !	First, SHUTUP detaches from the shutup console.  This is &
   !	done so that OPSER can attach to the console for potential operator &
   !	interaction during the shutdown process. &
   !	&
   !	Next, SHUTUP sends a special command to OPSER instructing OPSER &
   !	to begin shutting down the spooling system.  This command has a single &
   !	argument: &
   !	&
   !	SLE	- shutdown spoolers at logical end points &
   !	SIM	- shutdown spoolers immediately &
   !	&
   !	OPSER will respond to the command by sending back a message with the &
   !	text "CONFIRM" in it.  If the confirming message is not received by &
   !	SHUTUP within 60 seconds after the OPSER shutdown command is sent, &
   !	SHUTUP will abort with the following error message: &
   !	&
   !	OPSER not active &
   !	&
   !	Otherwise, SHUTUP begins waiting on OPSER to shutdown the spooling &
   !	package.  The length of the wait period is: &
   !	&
   !	For SLE - 60 minutes (3600 seconds) &
   !	For SIM - 120 seconds &
   !	&
   !	During this waiting period, SHUTUP is checking OPSERs status and &
   !	as soon as OPSER is gone, the next phase is entered. &
   !
155!	######## Operator/Message Services Shutdown Phase  ######## &
   !	&
   !	This phase is carried out only if OMS is running. If it is not, &
   !	SHUTUP continues with the next phase (no message is shown). &
   !	&
   !	First, SHUTUP is declared as a receiver, and performs a send w/privs &
   !	to send a STOP/OPERATOR_SERVICES/ABORT message to OMS telling it to &
   !	shutdown.  The STOP message consists of the command number in the 1st &
   !	byte of the parameter area, and the /ABORT flag in the data area. &
   !	Since the initial job killing phase has already been executed, and &
   !	PBS and OPSER have already been shutdown, there is no possibility of &
   !	OMS having a pending request. &
   !	&
   !		Command =  4	(STOP/OPERATOR_SERVICES command) &
   !		OMS.AFLD = 6	(/[NO]ABORT field identifier) &
   !		OMS.AFLG = 1	(immediate shutdown, abort pending requests) &
   !	&
   !	OMS acknowledges the shutdown command by sending back a &
   !	confirmation message.  If no confirmation message is received by &
   !	SHUTUP within two minutes after the STOP/OPERATOR command is &
   !	sent, SHUTUP will abort shutdown operations with the following &
   !	error message : &
   !	&
   !	?No response from Operator/Message Services after 2 minutes. &
   !	&
   !	SHUTUP then begins waiting on OMS to complete its shutdown. &
   !	SHUTUP will wait up to one (1) minute for the OMS job to kill itself. &
   !	If the OMS job does not go away within the specified amount of time, &
   !	SHUTUP aborts with the following error message: &
   !	&
   !	?Operator/Message Services shutdown taking too long &
   !	&
   !	Otherwise, SHUTUP will display the following message and continue &
   !	with the next phase: &
   !	&
   !	Operator/Message Services shutdown complete at hh:mm am/pm. &
   !
160!	########  EVTLOG Shutdown Phase  ######## &
   !	&
   !	This phase is entered if the Event Logger used by DECnet/E is running. &
   !	During the job killing phase above, EVTLOG was ignored in the scan &
   !	of the job table. Now EVTLOG will be terminated. &
   !	&
   !	A 60 second delay is built in to allow the Event Logger time enough &
   !	to shutdown. &
   !	&
   !	########  ERRCPY Shutdown Phase  ######## &
   !	&
   !	This phase is entered if the system error logging program ERRCPY is &
   !	running.  During the job killing phase above, ERRCPY was ignored in &
   !	the scan of the job table.  Now, during this phase, ERRCPY will be &
   !	terminated. &
   !	&
   !	SHUTUP instructs ERRCPY to shutdown via a special error message &
   !	(error code = 56).  ERRCPY is given 60 seconds to shutdown.  If it &
   !	does not, SHUTUP aborts with the following error message: &
   !	&
   !	ERRCPY failed to shutdown &
   !	&
   !	When ERRCPY goes away, the next phase is entered. &
   !	&
   !	########  Final Job Killing Phase  ######## &
   !	&
   !	If more than one (1) job remains active in the system, a pass is &
   !	through the job table, killing all remaining jobs via the SYS call &
   !	"kill".  If all go away, shutdown continues.  If the end of the job &
   !	table is reached and there is some other job, besides SHUTUP still &
   !	present in the system, then SHUTUP aborts with the error message: &
   !	&
   !	SHUTUP failed in final job killing phase &
   !	&
   !	########  EMT logging Shutdown Phase ###### &
   !	&
   !	This phase is entered if the EMT logging is used.  During the job &
   !	killing phase above, the local reciever with the object type of 2 was &
   !	ignored.  Now the EMT logging job will be terminated. &
   !	&
   !	########  Close Log File Phase  ######## &
   !	&
   !	To close any log file that may be open on the SHUTUP job, SHUTUP &
   !	logs itself off, this flushes and closes the log file if there is &
   !	one. SHUTUP then logs itself back into the same account and &
   !	proceeds. &
   !	&
   !	########  Remove RTS/RES LIB Phase  ######## &
   !	&
   !	During this phase, any current Run-Time Systems and Resident Libraries &
   !	which are present in the system (not including the SYSTEM default RTS) &
   !	are unloaded and removed from the system. &
   !	&
   !	########  SWAP File Removal Phase  ######## &
   !	&
   !	This phase closes down all swapping files to insure that no files are &
   !	open in the system prior to shutdown.  Any abnormal conditions may &
   !	cause SHUTUP to abort with SYS USAGE error(s). &
   !	&
   !	########  Disk DISMOUNT Phase  ######## &
   !	&
   !	During this phase, all mounted disks, except the system disk (SY0:) &
   !	are dismounted via the DISMOUNT SYS call. &
   !	&
   !	########  Final Shutdown Phase  ######## &
   !	&
   !	This is the last phase in the shutdown procedure.  It results in &
   !	the following instructional message: &
   !	&
   !	Please wait for system to re-boot itself &
   !	&
   !	SHUTUP now waits for the console terminal to complete output &
   !	of all text, after which the special SHUTUP SYS call is performed. &
   !	&
   !	The system monitor will now re-boot itself and the OPTIONS? query &
   !	will eventually appear on the console terminal. &
   !	&
   !	The shutdown procedure is complete at this point. &
   !	&

300	! &
	! &
	! &
	!	I / O    C H A N N E L S &
	&
	&

301!	CHANNEL #		USED FOR &
   !
311!		1	USER DIALOGUE &
   !		2	VIRTUAL ARRAY COVER FOR OPSER &
   !			   COMMAND FILE &
   !		3	SHUTUP - QMAN DIALOGUE
399!	&

400	! &
	! &
	! &
	!	V A R I A B L E    D E F I N I T I O N S &

401!		VARIABLE   DEFINITION &
   !		--------   ----------
500!		A1%	   = 0% FOR OPSER 'SLE' MODE &
   !		A2%	   = 1% FOR OPSER 'SIM' MODE &
   !	&
   !		CRLF$	   CRLF TEXT STRING &
   !		CONTEXT%   CONTEXT FIELD IN DATA PACKET TO SEND &
   !				-- SHUTUP'S TASK IDENTIFIER &
   !		CTX%	   CONTEXT FIELD IN DATA PACKET TO RECEIVE &
   !				-- INTEGER FORM &
   !		CTX$	   CONTEXT FIELD IN DATA PACKET TO RECEIVE &
   !				-- STRING FORM &
   !	&
   !		D0%	   = -1% IF SHUTUP IS DETACHED FROM ITS KB: &
   !			   =  0% IF SHUTUP IS ATTACHED TO ITS KB: &
   !		 D$	   WORKING STRING &
   !		DLOG%	   FLAG INDICATING LOGINS ARE DISABLED &
   !		DNET%	   FLAG INDICATING NETWORK ACTIVITY IS DISABLED &
   !	&
   !		E0%	   = 0% Indicates a RUN entry &
   !			   = -1% Indicates a CCL entry &
   !	&
   !		H0%	  LENGTH OF PHASE HEADING PRINT ZONE &
	&
   !		 I%	   WORKING VARIABLE &
   !		 I$	   VERSION/EDIT #'S TEXT  STRING &
   !		INFO%	   NUMBER OF ACTIVE JOBS IN PBS -- INTEGER FORM &
   !		INFO$	   NUMBER OF ACTIVE JOBS IN PBS -- STRING FORM &
   !	&
   !		 J%	   CURRENT JOB BEING PROCESSED &
   !		 J%( , )   OPSER ON-LINE JOB TABLE FILE &
   !				(VIRTUAL ARRAY) &
   !		J0%	   SHUTUP JOB NUMBER &
   !		J1%	   OPSER  JOB NUMBER &
   !		J2%	   ERRCPY JOB NUMBER &
   !		J3%	   EVTLOG JOB NUMBER &
   !		J4%	   EMT LOGGING JOB NUMBER &
   !		J5%	   QMAN JOB NUMBER &
   !		J9%	   WORKING VARIABLE &
   !	&
   !		 K%	   = #  IF CURRENT JOB KB#: ATTACHED &
   !			   =-1% IF CURRENT JOB KB   DETACHED &
   !		CONSOLE%   KEYBOARD # FOR SHUTUP &
   !	&
   !		 L%	   JOB KILLING PHASE INDEX &
   !	&
   !		 M%( )	   FIRST PART OF MONITOR TABLES &
   !		M0%( )	   SYS CALL WORK ARRAY &
   !		M1%( , )   OPSER ON-LINE JOB TABLE FILE &
   !				(VIRTUAL ARRAY) &
   !		M9%( )	   SECOND PART OF MONITOR TABLES &
   !		 M$	   SYS CALL WORK STRING &
   !	&
   !		 N%	   NUMBER OF JOBS KILLED/PHASE &
   !		NET.ON%    DECNET PRESENT FLAG. -1=YES; 0=NO &
   !		NET.FILE$  DECNET/E'S VOLATILE PARAMETER FILE &
   !		NETWORK%   USED IN FNM1% FOR RECIEVER TYPE &
   !		ERRCPY.NAME$	   "ERRCPY" LOGICAL RECEIVER NAME &
   !		OPSER.NAME$	     "OPSER" LOGICAL RECEIVER NAME &
   !		EVTLOG.NAME$		"EVTLOG" LOGICAL RECEIVER NAME &
   !	&
   !		 O%( , )   OPSER ON-LINE JOB TABLE FILE &
   !				(VIRTUAL ARRAY) &
   !		O9%	   = -1% IF NO OPSER RUNNING &
   !			   = #  OPSER JOB # IF OPSER RUNNING &
   !	&
   !		 P% 	   = 0% CURRENT JOB KB IS NOT PSUEDO KB &
   !			   = 1% CURRENT JOB KB IS     PSUEDO KB &
   !		 P$	   FNP% PROMPT TEXT ARGUMENT STRING &
   !		PPN$		[PPN] that SHUTUP running from &
   !	&
   !		RESPONSE%	ACK/NACK FIELD IN DATA PACKET TO RECEIVE &
   !				-- INTEGER FORM &
   !		RESPONSE$	ACK/NACK FIELD IN DATA PACKET TO RECEIVE &
   !				-- STRING FORM &
   !		RETURN.JOB%	JOB NUMBER RETURNED FROM RECEIVE &
   !	&
   !		 S$	   HOLDS RETURNED STRING FROM SYS CALL FOR &
   !				ERROR MESSAGE ERR = 0% &
   !		 S.ACC%	   TYPE OF RECEIVER. &
   !		 S.OBJ%	   OBJECT TYPE OF RECEIVER &
   !		SLEEP.SECS %	AMOUNT OF TIME TO SLEEP &
   !		PBS.FLAG%  = 0% IF A1% = 0% - ALLOW PBS JOBS TO COMPLETE &
   !			   = 1% OTHERWISE   - ABORT PBS JOBS &
   !	&
   !		 T%	   HOLDS # OF MINUTES TO SHUTDOWN BEGINS &
   !		T2%	   OPSER SHUTDOWN SLEEP QUANTUM &
   !				( IN SECONDS ) &
   !		T3%	   OPSER SHUTDOWN INTERVAL MAXIMUM VALUE &
   !				( IN SECONDS ) &
   !		T4%	   TIME SINCE LAST BROADCAST OF OPSER ON-LINE &
   !				JOB TABLE TO KB0: &
   !		TLOG%	   HOLDS # MINUTES TO LOGINS DISABLED &
   !		TNET%	   HOLDS # MINUTES TO NETWORK ACTIVITY DISABLED &
   !	&
   !		 W%	   WORKING VARIABLE &
   !		W1%	   ADDRESS OF DEVICE NAME TABLE &
   !		W2%	   INDEX ON DISK TYPE IN DEVICE NAME TABLE &
   !		W3%	   INDEX ON UNIT # &
   !		W4%	   WORKING VARIABLE &
   !		 W$	   WORK STRING &
   !		W0$	   WORK STRING &
   !		WAIT.TIME% AMOUNT OF TIME TO WAIT FOR RECEIVE (IN SECONDS) &
   !			   OR FOR QMAN RECEIVER TO DISAPPEAR &
   !	&
   !		X$,X1$	   WORK STRINGS &

700	! &
	! &
	! &
	!	P R O G R A M    L A Y O U T &
	&

701!	STMT#	DESCRIPTION &
   !	-----	-----------
710!	  100	PROGRAM DESCRIPTION &
   !	  300	CHANNEL NUMBER ASSIGNMENTS &
   !	  400	VARIABLE AND ARRAY ASSIGNMENTS &
   !	  700	PROGRAM LAYOUT &
   !	  800	ROUTINE DICTIONARY &
   !	  850	FUNCTION DICTIONARY &
   !	  900	DIMENSION DECLARATIONS &
   !	 1000	MAIN PROGRAM LOGIC &
   !		   INITIAL SETUP FUNCTIONS &
   !	 1049	   GET SHUTDOWN OPTIONS &
   !	 1100	   WARNING MESSAGE PHASE &
   !	 1150	   INITIAL JOB KILLING PHASE &
   !	 1231	   PBS SHUTDOWN PHASE &
   !	 1249	   OPSER SHUTDOWN PHASE(IF OPSER RUNNING) &
   !	 1600	   OMS SHUTDOWN PHASE &
   !	 1800	   SHUTDOWN EVTLOG &
   !	 2000	   SHUTDOWN ERRCPY &
   !     2100	   FINAL JOB KILLING PHASE &
   !	 2200	   SHUTDOWN EMT LOGGING &
   !	 2500	   UNLOAD AND REMOVE RUN-TIME SYSTEMS &
   !	 2900	   CLOSE LOG FILE PHASE &
   !	 3000	   REMOVE SWAP FILES &
   !	 3030	   DISMOUNT ANY MOUNTED NON-SYSTEM DISKS &
   !	 4000	   FINAL SHUTDOWN OPERATION &
   !	 6000	   DIALOGUE SUPPORT &
   !	 9000	   GENERAL PROGRAM ABORT PROCESS &
   !	10000	ROUTINES &
   !	15000	FUNCTIONS &
   !	19000	ERROR TRAP PROCESSING
760!	32767	END STATEMENT &
	&
	&
	&

800	! &
	! &
	! &
	!	D I C T I O N A R I E S &
	&

801!	SUBROUTINE	USE &
   !	----------      ---
802!	   10000	SCAN AND PROCESS JOB TABLE FOR CURRENT JOB &
   !	   11000	GET MONITOR TABLES &
   !	   13000	CHECK FOR PRESENCE OF OPSER &
   !	   14000	RE-ATTACH TO SHUTUP KB: KEYBOARD &
   !	&
   !	FUNCTION	STMT#	DESCRIPTION &
   !	--------	-----	----------- &
   !	FNJ%		15000	GET CURRENT # OF JOBS ON SYSTEM &
   !	FNJ1%(J%)	15015	GET JDB, GIVEN JOB # &
   !	FNL%		15020	GET CURRENT NUMBER OF LOGINS ALLOWED &
   !	FND%		15100	FIND OUT IF CURRENT JOB BELONGS TO OPSER &
   !	FNM%(S$)	16000	FIND JOB #, GIVEN RECEIVER 'ID' &
   !	FNM1%(S%)	16050	FIND JOB #, GIVEN OBJECT TYPE OF LOCAL REC. &
   !	FNB%		16100	BROADCAST OPSER ON-LINE JOB TABLE TO KB0: &
   !	FNP%(R$)	16200	DIALOGUE FUNCTION(RETURNS ONE OF 5 RESPONSES) &
   !	FNS%(M$)	16400	PRINT OUT PHASE STARTUP OPR MESSAGE &
   !	FNPRV%(PRIV$)	16450	CHECK TO SEE IF CURRENT JOB HAS PRIV$ PRIV &
   !	&

900	! &
	! &
	! &
	! 	D I M E N S I O N     S T A T E M E N T S &
	&

910	DIM  M%(30%), &
	    M0%(30%), &
	    M9%(30%) &
		! SYS CALL WORK ARRAYS &

920	DIM INTERACTIVE.JOB%(64%) &

930	DIM #2%, J%(23%,7%), &
		 O%(15%,3%), &
		M1%(63%,11%) &
		! OPSER WORKING FILE VIRTUAL ARRAYS &
	&
	&

999	! &
	! &
	! &
	!	M A I N   P R O G R A M   L O G I C &
	&

1000	  ON ERROR GOTO 19000 &
		! SET UP STANDARD ERROR TRAP &

1010	I$="V10.1-H" &
\	E0% = 0% &
		! VERSION - EDIT #'S. &
		! Indicate RUN entry &

1015	S$=SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)) &
	\ PRINT IF CCPOS(0%) &
	\ PRINT "SHUTUP   ";I$;"   ";CVT$$(RIGHT(S$,3%),4%) &
	! READ SYSTEM HEADER INFORMATION &
	! INSURE WE'RE AT THE LEFT MARGIN &
	! PRINT PROGRAM AND SYSTEM INFORMATION &

1020	READ PRV$ &
	\ GOTO 1030 UNLESS LEN(PRV$) &
	\ IF FNPRIV%(PRV$) &
	    THEN &
		GOTO 1020 &
	    ELSE &
		PRINT &
	\	PRINT "?Program must be privileged" &
	\	GOTO 32767 &
	! GET A PRIVILEGE NAME &
	! WHEN WE ARE AT THE END OF THE LIST OF PRIVS GO CHECK ACCOUNT PRIVS &
	! IF THIS PROGRAM HAS THE PRIV THEN &
	! 	GET THE NEXT PRIV TO CHECK ELSE &
	!	TELL THE USER AND EXIT &

1025	DATA	DEVICE,WWRITE,WREAD,WACNT,RDMEM,PBSCTL,MOUNT,JOBCTL,INSTAL, &
		SYSIO,SWCTL,SHUTUP,SEND,"" &

1030	PRIV.OFF$=SYS(CHR$(6%)+CHR$(-21%)+CHR$(255%)) &
	\ IF NOT (FNPRIV%("SHUTUP")) &
	    THEN &
		  PRINT &
	\	  PRINT "?SHUTUP privilege required" &
	\	  GOTO 32767 &
	! DROP TEMP PRIVS (TEMPORARILY) &
	! IF ACCOUNT DOES NOT HAVE SHUTUP PRIVILEGE THEN &
	!	TELL THE USER AND EXIT &

1045	PRIV.ON$=SYS(CHR$(6%)+CHR$(-21%)+CHR$(0%)) &
	\ RJS$=SYS(CHR$(6%)+CHR$(26%)+CHR$(0%)+CHR$(2%)) &
		! RETURN JOB STATUS WITH DATA SELECTOR=2 &
	\ CONSOLE%=ASCII(MID(RJS$,4%,1%)) &
		! GET JOB'S CONSOLE NUMBER &
	\ J0%=ASCII(RJS$) &
		! GET JOB'S JOB NUMBER &
	\ ACC.ESS%=ASCII(MID(RJS$,15%,1%)) &
		! GET JOB'S ACCESS CODE &
	\ A1%,A2% = 1% &
	\ ERRCPY.NAME$ = "ERRLOG" &
	\ OPSER.NAME$ = "OPSER" &
	\ EVTLOG.NAME$ = "EVTLOG" &
	\ CRLF$ = CHR$(13%) + CHR$(10%) &
	\ D0% = 0% &
	\ H0% = 30% &
	! REGAIN TEMP PRIVS &
	! GET THIS KB# AND JOB# &
	! SET DEFAULT VALUES FOR MODE FLAGS &
	! ALSO, A CRLF TEXT STRING &

1050	  IF CONSOLE% < 0% &
	    THEN &
		  PRINT &
		\ PRINT "?SHUTUP must be run from an attached job" &
		\ GOTO 32767 &
			! CHECK FOR RUNNING ATTACHED.  IF NOT, QUIT &
			! IMMEDIATELY. &

1052	IF ACC.ESS% > 1% &
		THEN PRINT &
	\	PRINT "?SHUTUP must be run from a local or dialup keyboard" &
	\	GOTO 32767 &
			! CHECK FOR LOCAL JOB.  IF NOT, QUIT &

1058	! &
	! &
	! &
	!	G E T    S H U T D O W N    O P T I O N S &
	&

1059	OPEN "_KB:SHUTUP.CMD" FOR INPUT AS FILE 1% &
	\ GOTO 1060 IF E0% &
	\ I% = FNS%("Set-up Dialogue") &
	\ PRINT &
	\ PRINT "Type '^' to any query to backup one question" &
	\ PRINT
1060	  GOSUB 13000 &
	\ GOSUB 11000 &
	\ IF E0% THEN &
		PBS.FLAG% = 0%			!Allow PBS jobs to complete &
\		A2% = 0%			!Shutdown OPSER orderly &
\		A1% = 0%			!Allow OPSER to reach end point &
\		TLOG% = 0%			!Zero minutes until no logins &
\		TNET% = 0%			!Zero minutes until no DECnet &
\		TNET% = T% - 1% IF T% > 1%	!Recalc minutes if needed &
\		RESTART% = 1%			!Automatic restart &
\		GOTO 1078			!Go shutdown the system &
	! GAIN CONTROL OF THE KEYBOARD &
	! Go around if this is a CCL entry &
	! INFORM OPERATOR WE ARE ENTERING FIRST PHASE. GIVE HELP &
	! INFORMATION &
	! CHECK TO SEE IF OPSER RUNNING. IF SO, INFORM USER AND SET &
	! FLAGS &
	! GET LATEST MONITOR TABLE INFO &
	! If this is a CCL entry &
	!  Set the canned responses &
	!  Go shutdown the system &

1062	  PBS.FLAG% = 0% &
	\  IF FNM1%(3%) THEN &
		ON FNP%("Allow Print/Batch Services jobs to complete <YES>?") &
		GOTO &
			1062,	1065,	6050,	1065,	1062 &
	! ANSWER WAS	^	YES	NO	DFLT	INVALID &

1065	  GOTO 1070	IF O9% = -1% &
	\ ON FNP%("Use OPSER for utilities shutdown <YES>?") &
	  GOTO &
			1062,	6100,	6150,	6100,	1065 &
	 ! ANSWER WAS	^	YES	NO	DFLT	INVALID &

1067	  GOTO 1065	IF A2% = 1% &
	\ ON FNP%("Allow OPSER utilities to reach logical end point <YES>?") &
	  GOTO &
			1065,	6170,	6190,	6170,	1067 &
	! ANSWER WAS	^	YES	NO	DFLT	INVALID &

1069	  IF O9% <> -1% &
	  THEN &
		GOTO 1067 &
	  ELSE &
		  IF FNM1%(3%) &
		  THEN &
			GOTO 1062 &
		  ELSE &
			GOTO 1070 &
			! IF OPSER IS WORKING, THEN GO BACK TO PREVIOUS QUERY &
			! OTHERWISE, IF PBS IS WORKING, THEN GO BACK THERE &
			! OTHERWISE, BACK TO SAME QUERY &

1070	  T% = 5% &
	\ TLOG%,TNET% = 0% &
	\ ON FNP%("Minutes until system shutdown (0-99) <5>?") &
	  GOTO &
			1069,	1070,	1070,	1074,	1072 &
	! ANSWER WAS 	^	YES	NO	DFLT	DATA INPUT &
	! THE DEFAULT VALUE IS 5 MINUTES &
	! AND INITIALIZE TIME DOWN VALUES FOR LOGIN AND NETWORK. &

1072	  T% = VAL(W$) &
	\ GOTO 1070	IF (T% < 0%) OR (T% > 99%) &
	\ GOTO 1078	IF (T% < 2%) &
		! CHECK INPUT RESPONSE FOR VALID INTEGER IN RANGE 0 - 99 &
		! SKIP NEXT PROMPT IF WE'RE SHUTING DOWN NOW. &

1074	  TLOG% = 0% &
	\ ON FNP%("Minutes until logins are disabled (0-"+NUM1$(T%-1%)+ &
		") <0>?") GOTO &
			1070,	1074,	1074,	1078,	1076 &
	! ANSWER WAS 	^	YES	NO	DFLT	DATA INPUT &
	! THE DEFAULT VALUE IS 0 MINUTES &

1076	  TLOG% = VAL(W$) &
	\ GOTO 1074	IF (TLOG% < 0%) OR (TLOG% > T%-1%) &
		! CHECK INPUT RESPONSE FOR VALID INTEGER IN RANGE 0 - (T%-1%) &

1078	W$ = SYS(CHR$(6%)+CHR$(22%)+CHR$(-20%)+ &
		 CHR$(2%)+STRING$(23%,0%)+CHR$(0%)) &
	\ NET.ON% = -1% &
	\ GOTO 1100 IF E0%			!Continue if CCL entry &
	\ GOTO 1082	IF T% < 2% &
	\ TNET% = T%-1% &
	\ ON FNP%("Minutes until new network activity is disabled (0-"+ &
		NUM1$(TNET%)+") <"+NUM1$(TNET%)+">?") GOTO &
			1074,	1078,	1078,	1082,	1080 &
	! ANSWER WAS 	^	YES	NO	DFLT	DATA INPUT &
	! &
	! SEND A NO-OP MESSAGE TO DECNET. &
	! *** ERROR TRAP SENDS CONTROL TO LINE 1082 IF THERE IS NO DECNET &
	! IF DECNET EXISTS SET THE NET.ON FLAG TO -1 &
	! SKIP THE USER PROMPT IF WE'RE SHUTTING DOWN RIGHT AWAY. &
	! OTHERWISE SET THE DEFAULT VALUE TO T%-1% AND PROMPT. &

1080	  TNET% = VAL(W$) &
	\ GOTO 1078	IF ((TNET% < 0%) OR (TNET% > T%-1%)) &
	\ GOTO 1082 &
		! CHECK INPUT RESPONSE FOR VALID INTEGER IN RANGE 0 - 99 &
		! OTHERWISE, CONTINUE &

1081	GOTO 1078 IF (NET.ON% <> 0%) &
			AND (T% > 1%) &
\	GOTO 1074 IF T% > 1% &
\	GOTO 1070 &
		! GO BACK TO THE DECNET PROMPT IF DECNET IS ON &
		!  AND WE ARE NOT SHUTTING DOWN RIGHT AWAY &
		! OTHERWISE, GO BACK TO THE LOGINS DISABLED PROMPT &

1082	GOTO 1100 IF E0%		!Continue if CCL entry &
	\ RESTART%=0% &
	\ ON FNP%("Shutdown with automatic RESTART <NO>?") &
	  GOTO &
			1081,	1084,	1100,	1100,	1082 &
	! ANSWER WAS 	^	YES	NO	DFLT	DATA INPUT &
	! THE DEFAULT VALUE IS "NO" &

1084	RESTART%=1% &
	&

1100	! &
	! &
	! &
	!	W A R N I N G    M E S S A G E   P H A S E &

1103	  I% = FNS%("Warning Message") &
	\ DLOG%,DNET%=0% &
	\ NODE$ = "System" &
	\ NODE$ = MID(SYS(CHR$(6%)+CHR$(22%)+CHR$(-19%)+CHR$(1%)),5%,6%) &
	\ NODE$ = CVT$$(NODE$,-2%) +"::" &
		! TELL 'EM WHAT'S HAPPENING &
		! INITIALIZE "LOGINS DISABLED" AND "NETWORK DISABLED" FLAGS &
		! GET THE NODE NAME &

1105	  WHILE T% > 0% &
	\   W$ = NODE$+" going down in "+NUM1$(T%)+" minute" &
	\   W$ = W$ + "s"	IF T% > 1% &
	\   W$ = W$ + ".  Please finish up" &
	\   GOSUB 1120 &
	\   PRINT NUM1$(T%);" minute warning message sent" &
	\   GOSUB 1140 IF ((TLOG% = 0%) AND (DLOG% = 0%)) &
	\   GOSUB 1145 IF ((TNET% = 0%) AND (DNET% = 0%) AND (NET.ON% = -1%)) &
	\   I% = T%/5%+1% &
	\   SLEEP 60%*I% &
	\   T% = T%-I% &
	\   TLOG%=TLOG%-(((I%>TLOG%)*I%)+I%)-((((TLOG%+1%)>I%)*TLOG%)+TLOG%) &
	\   TNET%=TNET%-(((I%>TNET%)*I%)+I%)-((((TNET%+1%)>I%)*TNET%)+TNET%) &
	\ NEXT &
	\ GOSUB 1140 IF (DLOG% = 0%) &
	\ GOSUB 1145 IF (DNET% = 0%) AND (NET.ON%=-1%) &
	\ W$ = "**** FINAL WARNING!!!!!  "+NODE$+" shutting down ****" &
	\ GOSUB 1120 &
	\ GOTO 1150 &
	! DONE IF NO MORE WAITING TIME &
	!   DISABLE LOGINS IF TIME TO DO SO &
	!   DISABLE NETWORK IF TIME TO DO SO &
	!   BROADCAST MESSAGE TO USERS FOR THIS WARNING &
	!   OUTPUT MESSAGE TO OPERATOR FOR THIS WARNING &
	!   CALCULATE NEXT WAIT INTERVAL AND SLEEP &
	!   SLEEP FOR CALCULATED WAIT INTERVAL &
	!   ADJUST SYSTEM SHUTDOWN TIMER &
	!   ADJUST DISABLE LOGINS AND DISABLE NETWORK TIMER &
	!   **NOTE** THE STRANGE CALCULATION DOES THE FOLLOWING: &
	!		IF I% > TLOG% THEN TLOG%=0% ELSE TLOG%=TLOG%-I% &
	!		IF I% > TNET% THEN TNET%=0% ELSE TNET%=TNET%-I% &
	! DO IT AGAIN IF MORE WAIT TIME &
	! BROADCAST FINAL WARNING MESSAGE TO USERS &
	! DO INITIAL JOB KILLING PHASE &

1120	  FOR W% = 1% TO M%(4%) &
	\   GOTO 1130	IF W% = J1% &
	\   CHANGE SYS(CHR$(6%)+CHR$(26%)+CHR$(W%)+CHR$(2%)) TO M0% &
	\   IF (M0%(4%)<=M%(3%)) &
		AND ((M0%(15%) AND 7%)<=4%) &
		AND (M0%(4%)<>CONSOLE%) &
		    THEN &
			D$ = SYS(CHR$(6%) &
				+CHR$(-5%) &
				+CHR$(M0%(4%)) &
				+TIME$(0%)+" " &
				+DATE$(0%)+" " &
				+W$+CRLF$) &
	! PREPARE TO LOOP THROUGH ALL SYSTEM JOBS &
	!   DON'T SEND MESSAGES TO OPSER &
	!   GET JOB STATUS &
	!   *NOTE* PSEUDO KEYBOARDS FOR BATCH JOBS ARE GAGGED &
	!   IF THE JOB IS NOT DETACHED AND HAS AN ACCESS CODE <= 4 &
	!	AND IS NOT OUR OWN KB: &
	!	    THEN BROADCAST THE MESSAGE TO THE JOB'S TERMINAL &
	&

1130	  NEXT W% &
	\ RETURN &
	! SCAN ALL SYSTEM JOBS AND BROADCAST MSGS WHEN APPROPRIATE &

1140	W$ = SYS(CHR$(6%)+CHR$(-2%)) &
	\ PRINT "Further LOGINs are now disabled" UNLESS E0% !Be quiet &
	\ DLOG% = -1% &
	\ RETURN &
	! SET LOGINS TO 1 &
	! SET "LOGINS DISABLED" FLAG TO -1 &
	! &

1145	W$ = SYS(CHR$(6%)+CHR$(22%)+CHR$(-20%)+ &
		CHR$(2%)+STRING$(23%,0%)+CHR$(3%)) &
	\ PRINT "Further network activity is now disabled" UNLESS E0%!Be quiet &
		! SEND MESSAGE TO DECNET TO DISABLE NEW NET ACTIVITY &
		!	"SET EXECUTOR STATE SHUT" &

1146	DNET% = -1% &
	\ RETURN &
		! SET "DECNET DISABLED" FLAG TO -1% &
		! &

1150	! &
	! &
	! &
	! &
	!	I N I T I A L    J O B    K I L L I N G    P H A S E &
	&

1155	IF NET.ON% THEN &
		I%=FNS%("DECNET Shutdown") &
	\	W$=SYS(CHR$(6%)+CHR$(22%)+CHR$(-20%)+ &
			CHR$(2%)+STRING$(23%,0%)+CHR$(1%)) &
	\	SLEEP 15% IF ASCII(MID(W$,21%,1%))=0% &
	! IF NETWORK IS RUNNING THEN &
	!	SEND ABORT MESSAGE TO DECNET "SET EXECUTOR STATE OFF" &
	!	WAIT FOR DECNET TO SHUT DOWN IF IT WASN'T ALREADY DOWN. &
	&

1160	  I% = FNS%("Initial Job Killing") &
	\ GOSUB 11000 &
	\ JOBTBL%=M%(11%) &
	! INFORM OPERATOR WE ARE ENTERING JOB KILL PHASE &
	! GET LATEST MONITR TABLE INFORMATION &

1170	J2%, J3%, J4%, J5%, J6% = -1% &
	\ J2% = FNM%(ERRCPY.NAME$) IF FNM%(ERRCPY.NAME$) &
	\ J3% = FNM%(EVTLOG.NAME$) IF FNM%(EVTLOG.NAME$) &
	\ J4% = FNM1%(2%) IF FNM1%(2%) &
	\ J5% = FNM1%(3%) IF FNM1%(3%) &
	\ J6% = FNM1%(11%) IF FNM1%(11%) &
	\ S0%,J%=0% &
	\ GOTO 9500	IF FNL% > 1% &
	\ GOTO 2500	IF FNJ% = 1% &
	\ GOSUB 10000 &
	! SEARCH FOR PRESENCE OF ERROR COPY, EVENT LOGGER, &
	! EMT LOGGING, AND THE PBS PACKAGE &
	! SET JOBS KILLED THIS PASS AND INITIAL JOB # TO ZERO &
	! EXIT IF SOME ONE SET LOGINS > 1 &
	! IF SHUTUP IS ONLY JOB RUNNING, SKIP TO RTS/RES LIB UNLOAD PHASE &
	! GET THE FIRST ACTIVE JOB FROM SYSTEM JOB TABLE &

1180	WHILE J% &
	\ IF K%<>-1% &
	  AND P%<>-1% &
	  AND J%<>J2% &
	  AND J%<>J3% &
	  AND J%<>J4% &
	  AND J%<>J5% &
	    THEN &
		W$=SYS(CHR$(6%)+CHR$(-4%)+CHR$(K%)+CHR$(3%)+ &
			CHR$(3%)+"$_LOGOUT/FULL"+CHR$(13%)) &
	\	S0% = S0% + 1% &
	\	INTERACTIVE.JOB%(S0%)=J% &
	! &
	! LOOP IF THERE IS AN ACTIVE JOB &
	! IF THE JOB IS NOT DETACHED AND &
	! THE JOB IS NOT RUNNING ON A PSEUDO KEYBOARD AND &
	! THE JOB IS NOT ERRCPY AND &
	! THE JOB IS NOT EVTLOG AND &
	! THE JOB IS NOT EMTLOG AND &
	! THE JOB IS NOT PBS THEN &
	! FORCE '^C^C$_LOGOUT/FULL' TO THE JOBS KEYBOARD &
	! COUNT THE JOBS WHICH WE HAVE FORCED TO LOGOUT &
	! STORE THESE JOB NUMBERS IN AN ARRAY &

1190	GOSUB 10000 &
	\ NEXT &
	! &
	! GET THE NEXT ACTIVE JOB FROM THE SYSTEM JOB TABLE &
	! LOOP UNTIL WE ARE AT THE END OF THE SYSTEM JOB TABLE &
	&

1210	FOR N%=1% TO 30% &
	\   SLP%=0% &
	\   FOR L%=1% TO S0% &
	\	J%=INTERACTIVE.JOB%(L%) &
	\	M%(I%)=0% FOR I%=0% TO 30% &
	\	CHANGE SYS(CHR$(6%)+CHR$(26%)+CHR$(J%/2%)+CHR$(1%)) TO M% &
				IF PEEK(JOBTBL%+J%) <> 0% &
	\	W$=SYS(CHR$(6%)+CHR$(8%)+CHR$(J%/2%)+STRING$(23%,0%)+CHR$(0%)+ &
		CHR$(255%))	IF (M%(5%) AND 8%)=8% &
				OR (M%(9%) AND 2%)=2%
1220		SLP%=-1% UNLESS M%(0%)=0% &
	\   NEXT L% &
	\   SLEEP 2% IF SLP% &
	\   N%=30% UNLESS SLP% &
	\ NEXT N% &
	\ FOR L%=1% TO S0% &
	\	W$= SYS(CHR$(6%)+CHR$(8%)+CHR$(INTERACTIVE.JOB%(L%)/2%)+ &
			STRING$(23%,0%)+CHR$(0%)+CHR$(255%)) &
				IF PEEK(JOBTBL%+INTERACTIVE.JOB%(L%))<>0%
1230	  NEXT L% &
	! LOOP 30 TIMES &
	!   SET SLEEP REQUIRED FLAG TO "NO" &
	!   FOR ALL JOBS WE FORCED TO LOGOUT &
	!	GET A JOB NUMBER FROM THE LIST &
	!	INIT THE JOB STATS ARRAY EACH TIME THROUGH THE LOOP &
	!	GET STATS ABOUT THIS JOB NUMBER IF IT'S STILL HANGING AROUND &
	!	KILL THE JOB IF IT'S IN ^C STATE OR KB WAIT STATE &
	!	SET SLEEP REQUIRED FLAG TO "YES" IF THE JOB WAS STILL AROUND &
	!   LOOK AT THE NEXT JOB ON THE LIST &
	!   SLEEP AT LEAST 1 SECOND IF ANY JOBS ON THE LIST WERE STILL AROUND &
	!   EXIT THE LOOP IF NO JOBS ON THE LIST ARE HANGING AROUND &
	! 30 TRIES IS LONG ENOUGH &
	! KILL ANY LEFT OVER JOBS FROM THE LIST &
	&

1231	! &
	! &
	! &
	! &
	!	P B S   S h u t d o w n   P h a s e &
	&

1232	IF FNM1%(3%) = 0% THEN &
	    GOTO  1246 &
	    !  IF PBS IS NOT RUNNING (QMAN's LOT DOES NOT EXIST) &
	    !  THEN GO ON TO NEXT PHASE IN SHUTUP &
	    !  OTHERWISE PROCEED TO SHUTDOWN PBS &

1233	I% = FNS%("Print/Batch Shutdown") &
	    \ GOSUB 11000 &
	!  INFORM SYSTEM MANAGER THAT WE ARE IN THE PBS SHUTDOWN PHASE &
	!  GET MONITOR TABLES &
	&

1234	W$ = SYS(CHR$(6%) 	! UUO call &
	        +CHR$(22%) 	! Send/receive call &
		+CHR$(0%)	! Remove RIB sub fun &
	        +CHR$(0%) 	! Remove RIBs from this job &
	        +STRING$(31%,0%) &
		+CHR$(-1%))	! Remove ALL RIBs from this job &
	!  REMOVE SHUTUP AS A RECEIVER (JUST IN CASE) &

1235	W$ = SYS(CHR$(6%) 	! UUO call &
	        +CHR$(22%) 	! Send/receive call &
	        +CHR$(1%) 	! Declare sub fun &
	        +CHR$(0%) 	! Reserved &
	        +"SHUTUP" 	! Our receiver name &
	        +STRING$(11%,0%) &
	        +CHR$(1%+2%) 	! Declare local and privilege &
	        +STRING$(2%,0%) &
	        +CHR$(4%) 	! four as message max (just in case) &
	        +STRING$(15%,0%)) &
	!  DECLARE SHUTUP AS A RECEIVER &

1236	CONTEXT% = PEEK(516%) &
	!  GET SHUTUP'S TASK IDENTIFIER &
	&
	\ W$ = (CHR$(0%) 	! HD.FLG &
	       +CHR$(7%) 	! HD.VER &
	       +STRING$(2%,0%) 	! HD.CID &
	       +CHR$(1%) 	! HD.RTQ &
	       +CHR$(16%) 	! HD.CMD &
	       +"SHUTUP" 	! HD.RTN &
	       +CHR$(CONTEXT%) 	! HD.CTX &
	       +CHR$(SWAP%(CONTEXT%)) &
	       +STRING$(14%,0%)	! OF.SEQ-OF.STS-2 &
	       +CHR$(PBS.FLAG%) ! OF.STS &
	       +STRING$(13%,0%)) ! OF.FLG &
	!  BUILD OFFLINE REQUEST PACKET TO SEND TO QMAN &

1237	W$ = SYS(CHR$(6%) 	! UUO call &
	        +CHR$(22%) 	! Mesage send/receive function &
	        +CHR$(-11%) 	! Send with privs sub fun &
	        +CHR$(128%+3%)	! Send to QMAN's LOT &
	        +STRING$(36%,0%) &
	        +W$) &
	!  SEND PACKET TO QMAN's LOT &

1238	OPEN "_NL:" AS FILE 3%, RECORDSIZE 40% &
	&
	\ FIELD #3%, 5% AS W$, &
	           1% AS RESPONSE$, &
	           6% AS W$, &
	           2% AS CTX$, &
	           12% AS W$, &
	           2% AS INFO$ &
	!  OPEN CHANNEL AND SET UP VARIABLES TO ACCESS DATA RETURNED &
	&
	\ WAIT.TIME% = 60% &
	\ SLEEP.SECS% = 2% &
	!  SET MAXIMUM WAITING TIME TO 2 MIN WITH 2 SEC SLEEP INTERVALS &

1239	W$ = SYS(CHR$(6%) 	! UUO call &
	        +CHR$(22%)	! Message send/receive call &
	        +CHR$(2%)	! Receive sub fun &
	        +CHR$(0%) 	! Local messages only (** this should be 4 **) &
	        +STRING$(6%,0%) &
	        +CHR$(3%) 	! Channel number to receive on &
	        +CHR$(0%) 	! reserved &
	        +CVT%$(SWAP%(40%)) ! Message length &
	        +STRING$(26%,0%)) &
		!  RECEIVE MESSAGE FROM QMAN &
	&
	\ RETURN.JOB% = ASCII(MID(W$,4%,1%)) &
	\ RESPONSE% = SWAP%(CVT$%(RESPONSE$)) &
	\ CTX% = SWAP%(CVT$%(CTX$)) &
	\ INFO% = SWAP%(CVT$%(INFO$)) &
	!  DECODE RETURN MESSAGE &
	&
	\ IF (RETURN.JOB% = J5%) AND (CTX% = CONTEXT%) THEN &
	    GOTO 1241 &
	!  IF JOB NUMBER AND CONTEXT FIELDS CONFIRM VALID HANDSHAKE &
	!  THEN GO CHECK RESPONSE &

1240	SLEEP SLEEP.SECS% &
 	\ WAIT.TIME% = WAIT.TIME% - SLEEP.SECS% &
	\ IF WAIT.TIME% > 0% THEN &
	    GOTO 1239 &
	    !  TRY TO GET RECEIVE FROM QMAN AGAIN &
	  ELSE &
	    PRINT "?No response from Print/Batch Services after 2 minutes" &
	    \ GOTO 9000 &
	    !  QMAN EXCEEDED ITS ALLOTTED TIME TO ANSWER SHUTUP &
	    !  SO, ABORT SHUTDOWN OPERATION &

1241	IF RESPONSE% <> 5% THEN &
	    GOTO 9000 &
	    !  ABORT SHUTDOWN RUN DUE TO BAD RESPONSE FROM QMAN &
	&
	!  ELSE ACK RESPONSE FROM QMAN &

1242  IF E0% = 0% THEN 				!If a RUN entry &
	IF INFO% > 0% THEN &
		X$ = NUM1$(INFO%)+" job" &
		\ X$ = X$+"s" IF INFO% <> 1% &
		\ X1$ = " completing..." &
		\ X1$ = " being terminated..." IF PBS.FLAG% = 1% &
		\ PRINT "Print/Batch Services shutting down - ";X$;X1$ &

1243	IF PBS.FLAG% = 0% THEN &
	    WAIT.TIME% = 3600% &
	    !  ALLOW ANY JOBS IN PBS TO COMPLETE &
	    !  (WITHIN 1 HOUR WITH 2 SECOND SLEEP INTERVALS) &
	ELSE  ! PBS.FLAG% = 1% &
	    WAIT.TIME% = 60% &
	    !  OR, &
	    !  SHUTDOWN PBS IMMEDIATELY &
	    !  (WITHIN 1 MINUTE WITH 2 SECOND SLEEP INTERVALS) &

1244	IF FNJ1%(J5%) = 0% THEN &
	    PRINT "Print/Batch Services shutdown complete at "; &
		   TIME$(0%) UNLESS E0% !Be quiet &
	    \ GOTO 1246 &
	    !  QMAN JOB HAS DISAPPEARED &
	    !  PBS SHUTDOWN OPERATION IS COMPLETE &
	    !  SO, GO ON TO NEXT PHASE &

1245	SLEEP SLEEP.SECS% &
	\ WAIT.TIME% = WAIT.TIME% - SLEEP.SECS% &
	\ IF WAIT.TIME% > 0% THEN &
	    GOTO 1244 &
	!  CHECK AGAIN TO SEE IF QMAN RECEIVER HAS DISAPPEARED &
	  ELSE &
	    PRINT "?Print/Batch Services shutdown taking too long" &
	    \ GOTO 9000 &
	    !  WAIT FOR QMAN RECEIVER TO DISAPPEAR &
	    !  IF TAKING MORE THAN ALLOTTED TIME THEN ABORT &

1246	W$ = SYS(CHR$(6%) &
	        +CHR$(22%) &
	        +CHR$(0%) &
	        +CHR$(0%)) &
	!  REMOVE SHUTUP AS A RECEIVER &
	&
	\  CLOSE 3% &
	!  CLOSE CHANNEL USED FOR SHUTUP - QMAN DIALOGUE &
	&
	!  END OF PHASE &

1249	! &
	! &
	! &
	!	O P S E R    S H U T D O W N    P H A S E &
	&

1250	  GOTO 1600	IF A2% <>  0% &
			OR O9%  = -1% &
	\ I% = FNS%("OPSER Shutdown") &
		! IF NOT USING OPSER, SKIP OPSER PHASES. OTHERWISE, TELL OPR &

1260	  PRINT #1%, "Detaching..."+CHR$(12%)+CHR$(13%) UNLESS E0% !Be quiet &
	\ A$=SYS(CHR$(11%)) &
	\ A$=SYS(CHR$(11%) + CHR$(1%)) &
	\ CLOSE 1% &
	\ W$=SYS(CHR$(6%) &
		+CHR$(7%) &
		+CHR$(128%)) &
	\ D0% = -1% &
		!IF OPSER IS TO BE USED THEN DETACH &

1270	  W$=SYS(CHR$(6%) &
		+CHR$(22%) &
		+CHR$(0%) &
		+CHR$(0%)) &
		! REMOVE SHUTUP AS A RECEIVER (JUST IN CASE) &

1275	  W$=SYS(CHR$(6%) &
		+CHR$(22%) &
		+CHR$(1%) &
		+CHR$(0%) &
		+"SHUTUP" &
		+STRING$(11%,0%) &
		+CHR$(1%) &
		+CHR$(0%) &
		+CHR$(0%) &
		+CHR$(3%)) &
		! DECLARE SHUTUP AS A RECEIVER &

1280	  W$=CHR$(5%)+CHR$(192%) &
	\ IF A1% = 0% &
	  THEN &
		W$=W$+"SLE" &
	  ELSE &
		W$=W$+"SIM" &
		! SET W$ TO OPSER COMMAND STRING &

1290	  W$=SYS(CHR$(6%) &
		+CHR$(22%) &
		+CHR$(-1%) &
		+CHR$(0%) &
		+"OPSER " &
		+STRING$(10%,0%) &
		+W$) &
	\  T2% = 60% &
		! SEND TO OPSER &
		! SET TIMER FOR OPSER NOT WORKING TIMEOUT &

1300	  GOTO 1570	IF T2% <= 0% &
	\ W$=SYS(CHR$(6%) &
		+CHR$(22%) &
		+CHR$(2%) &
		+CHR$(1%) &
		+STRING$(22%,0%) &
		+CHR$(T2%)+CHR$(SWAP%(T2%))) &
		! IF TIMER HAS EXPIRED, THEN OPSER IS NOT ACTIVE SO ABORT; &
		! ELSE ASK FOR NEXT MESSAGE.  IF NONE THERE, WILL IMMEDIATELY &
		! TRAP WITH ERROR 5 &

1310	  GOTO 1300	IF ASCII(MID(W$,4%,1%)) <> J1% &
			OR MID(W$,21%,7%) <> "CONFIRM" &
		! IF JOB NUMBER OF SENDER WASN'T OPSER'S OR IF MESSAGE IS &
		! GARBLED, THEN ABORT &

1500	  GOTO 1510	IF A1%=1% &
	\ T2%=5% &
	\ T3%=3600% &
	\ T4%=0% &
	\ GOTO 1520 &
		! SKIP IF SIM MODE &
		! SET UP COUNTERS &

1510	  T2%=1% &
	\ T3%=120% &
	\ T4%=0% &
		! SET UP COUNTERS FOR SIM MODE &

1520	  GOSUB 11000 &
	\ GOTO 9500	IF FNL%>1% &
	\ GOTO 2500	IF FNJ%=1% &
	\ SLEEP T2% &
	\ T3%=T3%-T2% &
	\ T4%=T4%+T2% &
		! IF LOGINS ARE GREATER THAN 1 ABORT &
		! IF # JOBS=1 THEN FORGET THIS PHASE &
		! SLEEP &
		! FIX THE TIMEOUT COUNTER &
		! FIX THE TIME TO BROADCAST COUNTER &

1530	  W%=FNM%(OPSER.NAME$) &
	\ GOTO 1600	IF W% = 0% &
	\ IF PEEK(M%(11%)+W%) <> 0% &
	  THEN &
		GOTO 1540 &
	  ELSE &
		GOTO 9000 &
		! LOOK UP OPSER'S JOB # FROM MSG ID TABLE &
		! IF IT'S THERE AND THE JOB IS ACTIVE, CONTINUE &
		! OTHERWISE, ABORT &

1540	  IF T3% <= 0% &
	  THEN &
		  W%=FNB% &
		\ X$ = "?OPSER shutdown taking too long" + CRLF$ &
			+" SHUTUP aborting shutdown operation" + CRLF$ &
		\ X$ = SYS(CHR$(6%) &
			  +CHR$(-5%) &
			  +CHR$(0%) &
			  +X$       ) &
		\ CLOSE 1%, 2% &
		\ GOTO 9200 &
		! IF THE TOTAL TIME ALLOWED FOR OPSER SHUTDOWN HAS ELAPSED, &
		! BRAODCAST THE OPSER JOB TABLE TO KB0: &

1550	  IF T4% >= 300% &
	  THEN &
		  W%=FNB% &
		\ T4%=0% &
		! IF ITS BEEN 5 MINUTES SINCE WE LAST BROADCAST OPSER'S &
		! JOB TABLE, DO IT AGAIN. &

1560	  GOTO 1520 &
		! BACK FOR MORE WAITING &

1570	  GOSUB 14000 &
	\ PRINT "?OPSER not active" &
	\ GOTO 9000 &
		! OPSER WAS APPARENTLY PRESENT, BUT TURNED OUT TO BE IN-ACTIVE &
		! SO WE ABORT OPERATIONS &
	&

1600	! &
	! &
	! &
	! &
	!	O M S   S h u t d o w n   P h a s e &
	&

1610	  GOSUB 14000 IF D0% &
	\ GOTO 1800 UNLESS FNM1%(11%) &
	\ X$ = "Operator/Message Services" &
	\ I% = FNS%("OMS Shutdown") &
	\ GOSUB 11000 &
	\ W$ = SYS(CHR$(6%) 	! UUO call &
	        +CHR$(22%) 	! Send/receive call &
		+CHR$(0%)	! Remove RIB sub fun &
	        +CHR$(0%) 	! Remove RIBs from this job &
	        +STRING$(31%,0%) &
		+CHR$(-1%))	! Remove ALL RIBs from this job &
	! RE-ATTACH TO CONSOLE AFTER OPSER PHASE IF DETACHED &
	! IF OMS IS NOT RUNNING, (OMS' LOT DOES NOT EXIST) &
	! THEN GO ON TO NEXT PHASE IN SHUTUP &
	! OTHERWISE PROCEED TO SHUTDOWN OMS &
	! INFORM SYSTEM MANAGER THAT WE ARE IN THE OMS SHUTDOWN PHASE &
	! GET MONITOR TABLES &
	! REMOVE SHUTUP AS A RECEIVER (JUST IN CASE) &
	&

1620	  CONTEXT% = PEEK(516%) &
	\ WAIT.TIME% = 60% &
	\ SLEEP.SECS% = 2% &
	\ W$ = SYS(CHR$(6%) 	! UUO call &
	        +CHR$(22%) 	! Send/receive call &
	        +CHR$(1%) 	! Declare sub fun &
	        +CHR$(0%) 	! Reserved &
	        +"SHUTUP" 	! Our receiver name &
	        +STRING$(11%,0%) &
	        +CHR$(1%+2%) 	! Declare local and privilege &
	        +STRING$(2%,0%) &
	        +CHR$(4%) 	! four as message max (just in case) &
	        +STRING$(15%,0%)) &
		! &
		! GET SHUTUP'S TASK IDENTIFIER &
		! SET MAXIMUM WAITING TIME TO 2 MIN WITH 2 SEC SLEEP INTERVALS &
		! DECLARE SHUTUP AS A RECEIVER &

1630	  W$ = SYS(CHR$(6%)		! UUO call &
		+ CHR$(22%)		! Message send/receive &
		+ CHR$(-11%)		! Send with privileges &
		+ CHR$(128%+11%)	! Send to OMS' Local Object Type &
		+ STRING$(24%, 0%)	! Skip to parameter area: &
		+ CHR$(4%)		!   STOP/OPERATOR_SERVICES command &
		+ CHR$(0%)		!   Reserved &
		+ "SHUTUP"		!   Return receiver name &
		+ CVT%$(CONTEXT%)	!   Confirmation value &
		+ CVT%$(0%)		! Skip to data area: &
		+ CHR$(6%)		!   /[NO]ABORT field &
		+ CHR$(1%)		!     /ABORT &
		+ CHR$(4%)		!   /FACILITY field &
		+ CHR$(6%)		!     length &
		+ "SHUTUP")		!     name &
		! &
		! SEND STOP/OPERATOR/ABORT PACKET TO OMS' LOT &

1640	  SLEEP SLEEP.SECS% &
	\ W$ = SYS(CHR$(6%) 	! UUO call &
	        +CHR$(22%)	! Message send/receive call &
	        +CHR$(2%))	! Receive sub fun &
	! &
	\ RETURN.JOB% = ASCII(MID(W$,4%,1%)) &
	\ RETURN.CTX% = CVT$%(MID(W$,29%,2%)) &
	\ RETURN.ERR% = SWAP%(CVT$%(MID(W$,31%,2%))) &
	\ RETURN.FLD% = SWAP%(CVT$%(MID(W$,33%,2%))) &
	! &
	\ IF (RETURN.JOB% = J6%) AND (RETURN.CTX% = CONTEXT%) THEN &
	    GOTO 1660 &
	! GIVE OMS TIME TO PROCESS MESSAGE &
	! TRY TO RECEIVE MESSAGE &
	! DECODE RETURN MESSAGE &
	! IF JOB NUMBER AND CONTEXT FIELDS CONFIRM VALID HANDSHAKE &
	! THEN GO CHECK RESPONSE &

1650 	  WAIT.TIME% = WAIT.TIME% - SLEEP.SECS% &
	\ GOTO 1660 IF WAIT.TIME% > 0% &
	\ PRINT "?No response from "; X$; " after 2 minutes" &
	\ GOTO 9000 &
	!  DEDUCT SLEEP TIME FROM WAIT TIME &
	!  KEEP TRYING TO GET MESSAGE FROM OMS IF TIME LEFT &
	!  IF OMS EXCEEDED ITS ALLOTTED TIME TO ANSWER SHUTUP &
	!  THEN ABORT SHUTDOWN OPERATION &

1660	  IF RETURN.ERR% &
	  THEN &
		PRINT "?Invalid response from "; X$ &
	\	GOTO 9000 &
		! ABORT SHUTDOWN RUN DUE TO BAD RESPONSE FROM OMS &

1670	  W$ = "" &
	\ W$ = "s" IF RETURN.FLD% > 1% &
	\ PRINT X$; " shutting down after aborting"; &
		RETURN.FLD%; "request"; W$  IF RETURN.FLD%  UNLESS E0% &
	\ WAIT.TIME% = 60% &
	! PRINT # OF PENDING REQUESTS (SHOULD BE 0) &
	! START WAIT TIME AT ONE MINUTE &

1680	  SLEEP SLEEP.SECS% &
	\ IF FNJ1%(J6%) = 0% &
	  THEN &
		PRINT X$; " shutdown complete at "; TIME$(0%)  UNLESS E0% &
	\	GOTO 1699 &
		! GIVE OMS SOME TIME TO SHUT DOWN &
		! IF OMS JOB HAS DISAPPEARED &
		! THEN OMS SHUTDOWN OPERATION IS COMPLETE &
		! SO, GO ON TO NEXT PHASE &

1690	  WAIT.TIME% = WAIT.TIME% - SLEEP.SECS% &
	\ GOTO 1680 IF WAIT.TIME% > 0% &
	\ PRINT "?"; X$; " shutdown taking too long" &
	\ GOTO 9000 &
	! DEDUCT SLEEP TIME FROM WAIT TIME &
	! CHECK JOB AGAIN IF TIME LEFT &
	! IF TAKING MORE THAN ALLOTTED TIME THEN ABORT &

1699	  W$ = SYS(CHR$(6%) &
	        +CHR$(22%) &
	        +CHR$(0%) &
	        +CHR$(0%)) &
		! REMOVE SHUTUP AS A RECEIVER &
		! END OF PHASE &

1800	! &
	! &
	! &
	!	S H U T D O W N    E V T L O G &
	! &

1810	GOSUB 14000 IF D0% &
		! GO BACK TO CONTROL KEYBOARD FOR NEXT PHASE IF USING &
		! OPSER &

1820	I% = FNS%("EVTLOG Shutdown") &
	\ GOTO 9500 IF FNL% > 1% &
	\ GOTO 2500 IF FNJ% = 1% &
	\ GOSUB 11000 &
	\ J3% = FNM%(EVTLOG.NAME$) &
	\ GOTO 2010 IF J3% = 0% OR PEEK(M%(11%)+J3%) = 0% &
		! &
		! PRINT PHASE MESSAGE. &
		! TEST FOR LOGINS > 1 - IF SO GO ABORT. &
		! TEST FOR # JOBS = 1 - IF SO SKIP THE REMAINDER OF &
		!      THE JOB KILLING PHASE. &
		! GOTO TO EMTLOG PHASE IF EVENT LOGGER IS NOT THERE. &
		! &

1830	W$ = CHR$(56%)+ &
	     CHR$(J3%)+ &
	     STRING$(8%,0%) &
		! &
		! FORM THE EVTLOG MESSAGE. &
		! &

1840	W$ = SYS(CHR$(6%)+CHR$(22%)+CHR$(-1%)+CHR$(0%)+ &
		 EVTLOG.NAME$+STRING$(10%,0%)+W$) &
		! &
		! SEND THE MESSAGE TO THE EVENT LOGGER. &
		! &

1890	  TARGET.TIME=TIME(0%)+60 &
	\ WHILE TIME(0%)<TARGET.TIME &
	\	SLEEP 2% &
	\	GOSUB 11000 &
	\	GOTO 2500 IF FNJ% = 1% &
	\	W% = FNM%(EVTLOG.NAME$) &
	\	GOTO 2010	IF W% = 0% &
				AND PEEK(M%(11%)+J3%) = 0% &
	\ NEXT &
		! &
		! WAIT FOR 1 SECOND AND CHECK TO SEE IF THE EVENT LOGGER &
		! HAS GONE AWAY OF IF EVTLOG RECEIVER ENTRY HAS DISAPPEARED &
		! &

1895	PRINT "?EVTLOG failed to shutdown" &
	\ GOTO 9000 &
		! &
		! ABORT THE SYSTEM SHUTDOWN PROCESS IF THE EVENT LOGGER &
		! FAILED TO SHUTDOWN. &
		! &
	&

2000	! &
	! &
	! &
	!	S H U T D O W N    E R R C P Y &
	&

2010	  I% = FNS%("ERRCPY Shutdown") &
	\ GOTO 9500  IF FNL%>1% &
	\ GOTO 2500  IF FNJ%=1% &
	\ GOSUB 11000 &
	\ W%=FNM%(ERRCPY.NAME$) &
	\ GOTO 2100 IF W% = 0% OR PEEK(M%(11%)+W%) = 0% &
		! PRINT PHASE &
		! TEST FOR LOGINS > 1 - IF SO GO ABORT &
		! TEST FOR # JOBS = 1 - IF SO ABORT. &
		! TEST FOR ERRCPY SKIP PHASE IF NOT THERE. &

2020	  W$ =	 CHR$(56%) &
		+CHR$(J0%) &
		+CHR$( 0%) &
		+CHR$( 0%) &
		+CVT%$(0%) &
		+CVT%$(SWAP%(PEEK(512%))) &
		+CVT%$(SWAP%(PEEK(514%))) &
		+CVT%$(SWAP%(PEEK(516%))) &
		+STRING$(8%,0%) &
		! FORM UP ERRCPY MESSAGE (ERR=56). &

2030	  W$=SYS(CHR$(6%) &
		+CHR$(22%) &
		+CHR$(-1%) &
		+CHR$(0%) &
		+ERRCPY.NAME$ &
		+STRING$(10%,0%) &
		+W$) &
		! SEND IT TO ERRCPY &

2040	TARGET.TIME=TIME(0%)+60 &
	\ WHILE TIME(0%)<TARGET.TIME &
	\	SLEEP 2% &
	\	GOSUB 11000 &
	\	GOTO 2500	IF FNJ% = 1% &
	\	W%=FNM%(ERRCPY.NAME$) &
	\	GOTO 2100	IF (W% = 0%) &
 				AND (PEEK(M%(11%)+J2%) = 0%) &
	\ NEXT &
		! SLEEP AT LEAST 1 SECOND, THEN CHECK TO SEE IF ERRCPY &
		! RECEIVER ENTRY &
		! AND THE ERRCPY JOB HAVE GONE AWAY &

2060	  PRINT "?ERRCPY failed to shutdown" &
	\ GOTO 9000 &
		! ERRCPY DIDN'T GO AWAY, SO TELL THE OPERATOR AND ABORT THE &
		! RUN. &
	&

2100	! &
	! &
	! &
	!	F I N A L    J O B    K I L L I N G    P H A S E &
	&

2110	I% = FNS%("Final Job Killing") &
	\ GOTO 9500	IF FNL% > 1% &
	\ GOTO 2500	IF FNJ% = 1% &
	\ N%,J%,EMTLOG.PRESENT% = 0% &
	\ GOSUB 10000 &
	! INFORM OPERATOR WE WILL KILL ALL REMAINING JOBS &
	! ERROR IF LOGINS NOT STILL 1 &
	! GOTO RTS/RES LIB UNLOAD PHASE IF SHUTUP IS ONLY JOB RUNNING &
	! INITIALIZE TO SEARCH JOB TABLE &
	! GET A JOB NUMBER &

2120	  WHILE J% <> 0% &
	\   IF J% = J4% &
		THEN &
		    EMTLOG.PRESENT%=-1% &
		ELSE &
		    W$=SYS(CHR$(6%)+CHR$(8%)+CHR$(J%/2%)+STRING$(24%,0%)+ &
			CHR$(255%)) &
	\	    N%=N%+1%
2130	    GOSUB 10000 &
	\ NEXT &
	! LOOP HERE IF NOT AT THE END OF THE JOB TABLE &
	!   IF THE JOB IS EMTLOG &
	!	THEN SET THE FLAG TELLING US THAT EMTLOG IS PRESENT &
	! &
	!	ELSE KILL THE JOB &
	!	KEEP TRACK OF THE NUMBER OF JOBS WE KILL HERE &
	!   GET ANOTHER JOB NUMBER &
	! DO IT AGAIN &

2140	GOTO 2200 IF N%=0% &
	\ JOBS.ALLOWED%=1%-EMTLOG.PRESENT% &
	\ TARGET.TIME=TIME(0%)+10%+(N%*2%) &
	\ WHILE TIME(0%)<TARGET.TIME &
	\   SLEEP 2% &
	\   GOSUB 11000 &
	\   GOTO 2200 IF FNJ% = JOBS.ALLOWED% &
	\ NEXT &
	\ PRINT "?SHUTUP failed in final job killing phase" &
	\ GOTO 9000 &
	! DO NEXT PHASE IF NO JOBS HAD TO BE KILLED &
	! SET NUMBER OF JOBS ALLOWED AT THE END OF THIS PHASE &
	! SETUP SOME TIME FOR JOBS TO GO AWAY &
	! LOOP IF WE HAVE MORE TIME &
	!   SLEEP AT LEAST 1 SECOND &
	!   GET LATEST MONITOR TABLES &
	!   EXIT TO NEXT PHASE IF NUMBER OF JOBS = NUMBER OF JOBS ALLOWED &
	! DO IT TILL WE RUN OUT OF TIME &
	! WE GOT HERE BECAUSE THE JOBS DIDN'T GO AWAY &
	! ABORT SHUTUP &

2200	! &
	! &
	! &
	!	S H U T D O W N    E M T L O G &
	! &

2210	GOTO 9500 IF FNL% > 1% &
	\ GOTO 2500 IF FNJ% = 1% &
	\ GOSUB 11000 &
	\ J4% = FNM1%(2%) &
	\ GOTO 2500 IF J4% = 0% OR PEEK(M%(11%)+J4%) = 0% &
	\ I% = FNS%("EMT logging Shutdown") &
		! &
		! TEST FOR LOGINS > 1 - IF SO GO ABORT. &
		! TEST FOR # JOBS = 1 - IF SO SKIP EMT LOGGING &
		! GOTO TO RTS REMOVAL PHASE IF EMT LOGGER IS NOT THERE. &
		! PRINT PHASE MESSAGE IF EMT LOGGING PRESENT. &
		! &

2240	W$ = SYS(CHR$(6%)+CHR$(22%)+CHR$(-1%)+CHR$(128%+2%)+ &
		 STRING$(16%,0%)+CHR$(-1%)) &
		! &
		! SEND THE MESSAGE TO THE EMT LOGGER. &
		! Local object type (LOT) = 2. &
		! 128%+LOT sends message to all EMT loggers. &
		! &

2290	FOR I% = 1% TO 30% &
	\	SLEEP 2% &
	\	GOSUB 11000 &
	\	GOTO 2500 IF FNJ% = 1% &
	\	J4% = FNM1%(2%) &
	\	GOTO 2500 IF J4% = 0% OR PEEK(M%(11%)+J4%) = 0% &
	\ NEXT I% &
		! &
		! WAIT FOR 2 SECONDS AND CHECK TO SEE IF THE EMT LOGGER &
		! HAS GONE AWAY OR IF EMTLOG RECEIVER ENTRY HAS DISAPPEARED &
		! &

2295   PRINT "%EMT logger failed to shutdown - killing EMT logger job =";J4%/2% &
	\ W$=SYS(CHR$(6%) &
		+CHR$(8%) &
		+CHR$(J4%/2%) &
		+STRING$(23%,0%) &
		+CHR$(0%) &
		+CHR$(255%)) &
	\ GOTO 2500 &
		! &
		! KILL THE EMT LOGGING JOB IF IT FAILS TO SHUT DOWN IN &
		! 60 SECONDS. &
		! &
	&

2500	! &
	! &
	! &
	!	U N L O A D    A N D    R E M O V E    R U N - T I M E &
	! &
	!   S Y S T E M S    A N D    R E S I D E N T    L I B R A R I E S &
	&

2510	  GOSUB 14000	IF D0% &
	\ CLOSE 1%, 2% &
	\ GOSUB 11000 &
	\ I% = FNS%("Remove RTS/RES LIB") &
		! RE-ATTACH IF NECESSARY &
		! CLOSE OPSER WORK FILE IF OPENED &
		! GET A FRESH COPY OF PART 1 & 2 MONITOR TABLE ADDRESSES &
		! INFORM OPERATOR WE WILL NOW UNLOAD/REMOVE RUN-TIME SYSTEMS &

2520	  W%=PEEK(PEEK(M9%(15%))) &
	\ W%=PEEK(W%) IF W%=PEEK(M9%(15%)-2%) &
	\ GOTO 2525  IF W%=0% &
	\ GOTO 2530 &
		! GET SECOND ENTRY IN RTS LINKED LIST &
		! IF LIST ENDS THEN GOTO RES LIB REMOVAL &
		! IF THIS IS SYSTEM DEFAULT KBM THEN USE THIRD ENTRY INSTEAD &

2525	RES.LIB%=-1% &
	\ W%=PEEK(M9%(15%)+2%) &
	\ GOTO 2900 IF W%=0% &
		! SET RES LIB FLAG &
		! GET FIRST ENTRY IN RES.LIB LINKED LIST &
		! IF LIST ENDS THEN EXIT &
	&

2530	  D$ = RAD$(PEEK(W%+2%)) &
	      +RAD$(PEEK(W%+4%)) &
	\ CHANGE SYS(CHR$(6%) &
		    +CHR$(-10%) &
		    +D$)	 TO M0% &
	\ M0%(0%) = 30% &
	\ M0%(1%) =  6% &
	\ M0%(2%) = -18% &
	\ M0%(3%) = 4% &
	\ M0%(3%) =  20% IF RES.LIB% &
	\ M0%(W1%) = 0% 	FOR W1% = 13% TO 18% &
	\ CHANGE M0% TO M$ &
	\ M$ = SYS(M$) &
		! GET RTS/RES LIB SYSTEM NAME.  FILE NAME STRING SCAN NAME &
		! INTO SYS CALL ARRAY.  COMPLETE REST OF ARRAY FOR SYS CALL. &
		! CHANGE TO STRING AND MAKE REMOVE CALL TO SYSTEM &

2550	  GOTO 2520 UNLESS RES.LIB% &
	\ GOTO 2525 &
		! GO BACK FOR MORE IN LIST &
	&

2900	! &
	! &
	! &
	!	C L O S E   L O G   F I L E   P H A S E &
	! &
	&
	&

2950 	I% = FNS%( "Log File Closing") &
	\ W$=SYS(CHR$(6%) &
		+CHR$(26%) &
		+CHR$(0%) &
		+CHR$(0%)) &
	\ PPN$=MID(W$,21%,2%) &
		!  Get current [PPN] so we know where to log back into &
		!  again &

2960	W$=SYS(CHR$(6%)+CHR$(5%)+CVT%$(SWAP%(2%))) &
		!  Log out SHUTUP with exceed quota bit set &
		!  This will flush and close any open log file for this job &

2970	IF ASCII(MID(W$,3%,1%))=255% THEN &
		PRINT "%Could not close log file, continuing" &
		\ GOTO 3000 &
		! Check the logout function for the only possible failure &
		! error 0 won't comeback here, error -2 we don,t care about &

2980	ON ERROR GOTO 2985 &
\	  W$=SYS(CHR$(6%) &
		+CHR$(4%) &
		+CHR$(0%) &
		+CHR$(8%) &
		+PPN$) &
\	GOTO 2990 &
		!  Set local error trap &
		!  Log SHUTUP back in under the same [PPN] as before &
		!  with no password checking and &
		!  because it has EXQTA this should not fail &
		!  Continue &

2985	W$ = "" &
\	RESUME 2990 &
		! Tell line 2990 that the logging-back-in failed &
		! Clear the error flag and continue &

2990	ON ERROR GOTO 19000 &
\	IF ASCII(RIGHT(W$,2)) <> 255% THEN &
		PRINT "%Could not log back in; continuing logged-out" &
		!Reset standard error trap &
		!See if logged in  again ok &
		!re-try message if login failed, should not happen &
		! if it should fail however for some unknown &
		! reason finish up logged out. &
	&
	&

3000	! &
	! &
	! &
	!	R E M O V E    S W A P    F I L E S &
	&

3010	  I% = FNS%("SWAP File Removal") &
	\ SWAPS.REMOVED% = 0% &
		! INFORM OPERATOR WE ARE REMOVING SWAP FILES &
		! Clear the flag ( we haven't removed the swap files yet) &

3020	  M$=SYS(CHR$(6%) &
		+CHR$(23%) &
		+CHR$(W%) &
		+CHR$(0%))	 UNLESS W%=2% &
			FOR W%=0% TO 6% &
	\ SWAPS.REMOVED% = -1% &
		! &
		! REMOVE SWAP FILES AND &
		! REMOVE DECNET FILES &
		! Set the flag (swap files have been removed) &
		! &

3030	! &
	! &
	! &
	!	D I S M O U N T    A N Y     M O U N T E D &
	! &
	!	      N O N - S Y S T E M    D I S K S &

3035	I% = FNS%("Disk DISMOUNT") &
		! INFORM OPERATOR WE ARE DISMOUNTING ALL NON-SYSTEM DISKS &

3040	D$=SYS(CHR$(6%)+CHR$(12%)) &
	\ W%=M%(19%) &
	\ W1%=M9%(5%) &
		! DEASSIGN ALL DEVICES. &
		! GET ADDRESS OF UNIT COUNT TABLE THEN ADDRESS OF &
		! DEVICE NAME TABLE &

3050	FOR W2%=0% TO (M9%(9%)-2%) STEP 2% &
		! ITERATE FOR EACH POSSIBLE DISK TYPE IN DEVNAM TABLE &

3060		FOR W3%=0% TO PEEK(W2%+M%(5%)) &
			! ITERATE FOR EACH UNIT NUMBER FROM UNIT 0 TO VALUE IN &
			! DEVCNT TABLE &

3070			W4%=PEEK(W%) &
	\		RE.TRY%=0% &
	\		IF W4% < 0% OR W% = PEEK(M%(7%)-2%) THEN &
				GOTO 3090 &
					! GET THE UNTCNT ENTRY FOR CURRENT DISK. &
					! IF IT ISN'T MOUNTED OR IF IT IS THE &
					! SYSTEM DISK, THEN SKIP DISMOUNT CALL &

3080			  D$=SYS(CHR$(6%) &
				+CHR$(3%) &
				+CHR$(2%) &
				+STRING$(19%,0%) &
				+CHR$(PEEK(W1%)) &
				+CHR$(SWAP%(PEEK(W1%))) &
				+CHR$(W3%) &
				+CHR$(255%)) &
				! DISMOUNT THE DISK &

3090			W%=W%+2% &
		\ NEXT W3% &
			! INCREMENT UNIT # AND REPEAT LOOP &

3100		  W1%=W1%+2% &
	\ NEXT W2% &
		! INCREMENT DEVNAM TABLE POINT AND REPEAT LOOP &
	&

4000	! &
	! &
	! &
	!	F I N A L    S H U T D O W N    O P E R A T I O N &
	&

4010	GOTO 4015 IF E0%			!Say nothing if CCL entry &
	\ I% = FNS%("Final Shutdown") &
	\ PRINT &
	\ PRINT "Please wait for system to re-boot itself"; &
	\ PRINT " on KB0:" IF CONSOLE% <> 0% &
	\ PRINT		IF CONSOLE% = 0% &
	\ PRINT 	FOR I% = 1% TO 10%
4015	  W%=PEEK(PEEK(PEEK(520%))) &
		! INFORM OPERATOR WE ARE ENTERING FINAL SHUTDOWN ACTIVITY &
		! PRINT THEN GET SHUTUP'S TERMINAL DDB &

4020	GOTO 4020	IF PEEK(W%+10%) <> PEEK(W%+12%) &
	\ SLEEP 1% &
	\ D$=SYS(CHR$(6%) &
		+CHR$(-16%) &
		+CHR$(RESTART%)) &
	\ STOP &
		! LOOP UNTIL WE HAVE FINISHED OUTPUTTING. &
		! EXECUTE SHUTDOWN &
		! STOP &
	&

6000	! &
	! &
	! &
	!	D I A L O G U E    S U P P O R T &
	&

6001!	THESE BRIEF OUT-OF-LINE CODE SEGMENTS ARE USED FOR SETTING THE &
    !	OPTION FLAGS, PBS.FLAG%, A1%, AND A2%, DURING THE DIALOGUE SESSION WHILE &
    !	SHUTUP IS BEING SETUP. &

6050	  PBS.FLAG% = 1% &
	\ GOTO 1065 &
	! SET PBS.FLAG% TO INDICATE THAT PBS SHUTDOWN IS IMMEDIATE &

6100	  A2%=0% &
	\ GOTO 1067 &
		! SET SWITCH TO INDICATE OPSER IS TO BE USED &
		! CONTINUE DIALOGUE &

6150	  ON FNP%("Are you sure you don't want to use OPSER <NO>?") &
	  GOTO &
			1065,	6155,	1065,	1065,	6150 &
	! ANSWER WAS	^	YES	NO	DLFT	ILLEGAL &

6155	  A2% = 1% &
	\ O9% = -1% &
	\ GOTO 1070 &
		! SET FLAGS TO NOT USE OPSER FOR UTILITIES SHUTUDOWN &
		! RETURN TO DIALOGUE &

6170	  A1%=0% &
	\ GOTO 1070 &
		! SET SWITCH TO INDICATE OPSER SLE MODE &
		! CONTINUE DIALOGUE &

6190	  A1%=1% &
	\ GOTO 1070 &
		! SET SWTICH TO INDICATE OPSER SIM MODE &
		! CONTINUE DIALOGUE &
	&
	&

9000	! &
	! &
	! &
	!	P R O G R A M    A B O R T S    H E R E &
	&

9001!	ANY ERROR OR IRRECONCILABLE CONDITION CAUSES CONTROL TO BE &
    !	TRANSFERRED HERE SO THAT THE PROGRAM CAN PERFORM AN ORDERLY &
    !	ABORT OF ITS OPERATIONS. &
    !
9100	  CLOSE 1%, 2%, 3% &
	\ PRINT "??SHUTUP aborting - please try again later" &
		! CLOSE FILES AND TELL OPERATOR WE ARE FINISHED FOR NOW &

9200	  W$=SYS(CHR$( 6%) &
		+CHR$(22%) &
		+CHR$(0%) &
		+CHR$(0%)) &
		! REMOVE US FROM THE RECEIVER TABLE ALSO &

9300	  GOTO 32767 &
		! GO TO THE END OF THE PROGRAM &

9500	  GOSUB 14000	IF D0% &
	\ PRINT &
	\ W$=SYS(CHR$(0%)) &
	\ PRINT "?Logins not disabled" &
	\ GOTO 9000 &
		! WE COME HERE IF WE REACH CHECKPOINTS WHERE LOGINS MUST BE &
		! ONE AND SOMEONE HAS TAMPERED WITH SYSTEM &
	&

10000	! &
	! &
	! &
	!	S U B R O U T I N E S &

10010	! &
	! &
	! &
	!	S C A N   J O B   T A B L E &

10015	  GOSUB 11000 &
		! UPDATE MONITOR TABLES INFORMATION &

10020	  J%=J%+2% &
	\ W%=PEEK(M%(11%)+J%) &
	\ IF W% = -1% &
	  THEN &
		  J%=0% &
		\ RETURN &
			! INCREMENT JOB NUMBER &
			! GET CONTENTS OF JOB TABLE INDEXED BY J% &
			! IF END OF TABLE SET J%=0% AND RETURN &

10030	  IF J% = J0% &
	  OR W% = 0% &
	  THEN &
		  GOTO 10020 &
	  ELSE &
		  W1%=PEEK(PEEK(W%)) &
		\ K% = SWAP%(PEEK(W1%+2%)) AND 255% &
			! IF THE JOB IS SHUTUP'S OR THE JOB IS NOT ACTIVE, &
			! GET NEXT JOB; OTHERWISE SET W% = ADDRESS OF CHNL DDB &
			! SET K%=KEYBOARD # &

10040	  IF J% <> ASCII(CHR$(PEEK(W1%+2%))) &
	  OR (PEEK(W1%+6%) AND 8192%) = 0% &
	  THEN &
		  K%=-1% &
		\ P% = 0% &
		\ RETURN &
			! SET K%=-1% IF KEYBOARD IS DETACHED OR IS NOT THE JOB'S &
			! CONSOLE - FORCE OFF PSUEDO KEYBAORD RESULT AND RETURN &

10050	  P% = ((PEEK(W1%+30%) AND 255%) = 8%) &
		! P% = -1% IF PSUEDO KEYBOARD. P% = 0% OTHERWISE &

10060	  RETURN &
	&
	&

11000	! &
	! &
	! &
	!	G E T    M O N I T O R    T A B L E S &

11010	  CHANGE SYS(CHR$(6%) &
		    +CHR$(-3%)) TO M% &
	\ M%(W%) = M%(W%) + SWAP%(M%(W%+1%))	FOR W%=5% TO 25% STEP 2% &
	\ CHANGE SYS(CHR$(6%) &
		    +CHR$(-12%)) TO M9% &
	\ M9%(W%) = M9%(W%) + SWAP%(M9%(W%+1%))	FOR W%=3% TO 29% STEP 2% &
	\ RETURN &
		! GET MONITOR TABLES PART 1 AND 2  - CONVERT TO INTEGER ARRAY. &
	&

13000	! &
	! &
	! &
	!	C H E C K    F O R    P R E S E N C E    O F    O P S E R &
	&

13010	  GOSUB 11000 &
		! GET SYSTEM MONITOR TABLES &

13020	  O9% = FNM%(OPSER.NAME$) &
		! ATTEMPT TO GET OPSER JOB NUMBER &

13025	  IF O9% = 0% &
	  THEN &
		O9% = -1% &
		\ RETURN &
			! CHECK FOR OPSER IN RECEIVER TABLE.  IF NOT, &
			! SET JOB NO. TO -1 AND EXIT &

13030	  OPEN "OPSER$:OPSER1.WRK" FOR INPUT AS FILE 2% &
		! OPEN THE OPSER ON-LINE LOG FILE &

13040	  J1% = O9% &
	\ O9% = M1%(0%,4%) &
		! SAVE THE OPSER JOB NUMBER &
		! GET THE OPSER KEYBOARD NUMBER &

13050	  RETURN &
	  	! EXIT FROM ROUTINE &
	&

14000	! &
	! &
	! &
	!	R E - A T T A C H    T O   K E Y B O A R D &
	&

14010	  W$=SYS(CHR$( 6%) &
		+CHR$( 6%) &
		+CHR$(J0%/2%) &
		+CHR$(CONSOLE%)) &
		! THE ACTUAL RE-ATTACH SYS CALL &

14015	  OPEN "_KB:SHUTUP.CMD" AS FILE 1% &
		! RE-OPEN THE KEYBOARD FILE &

14020	GOTO 14025 IF E0%			!Say nothing if CCL entry &
	\ PRINT &
	\ PRINT "Re-attaching..." &
	\ PRINT &
		! SIGNAL WHAT'S HAPPENING TO USER &

14025	  D0% = 0% &
		! RESET DETACHED FLAG &

14030	  RETURN &
		! EXIT AND RETURN &
	&

15000	! &
	&
	&
	!	G E T     C U R R E N T   N U M B E R   O F   J O B S &

15001!				FNJ% &
     !	&
     !	THE FUNCTION RETURNS THE NUMBER OF JOBS CURRENTLY RUNNING ON THE &
     !	SYSTEM &
     !
15010	  DEF* FNJ%=PEEK(M9%(13%)) AND 255% &

15015!	&
	&
	&
     !		G E T   J D B   O F   J O B   N U M B E R &

15016!				FNJ1%(J%) &
     !	&
     !  THIS FUNCTION RETURNS THE JOB DEFINITION BLOCK (JDB) OF THE JOB &
     !  INDICATED BY THE JOB NUMBER IN PARAMETER J%.  IF THE JOB HAS &
     !  DISAPPEARED, THE FUNCTION RETURNS 0%. &
     !
15017	  DEF* FNJ1%(J%)=PEEK(M%(11%)+(J%)) &

15020	! &
	&
	&
	!	G E T   N U M B E R   O F   L O G I N S   A L L O W E D &

15021!				FNL% &
     !	&
     !	THE FUNCTION RETURNS THE CURRENT LOGIN LIMIT &
     !
15030	  DEF* FNL% = (SWAP%(PEEK(M9%(13%)))) AND 255% &
	&

15100	! &
	&
	&
	!	F I N D   O U T   I F    J O B    B E L O N G S &
	&
	!			T O    O P S E R &
	&

15101!					FND% &
     !	&
     !	THE FUNCTION RETURNS TRUE (-1%) IF THE CURRENT JOB J% IS OWNED &
     !	BY OPSER, IN ITS ON-LINE JOB TABLE.  OTHERWISE, THE FUNCION &
     !	RETURNS FALSE (0%). &
     !
15110	  DEF* FND% &
		! THE FUNCTION DEFINITION &

15115		  IF O9% = -1% &
		  THEN &
			   FND% = 0% &
			\  GOTO 15140 &
				! IF OPSER IS NOT RUNNING, DISABLE FUNCTION &
				! AND RETURN FALSE RESULT &

15120	 	  FND% = -1% &
		\ GOTO 15140	IF J% = J1% &
		\ GOTO 15140	IF J%(J9%,1%) = J%/2% &
			FOR J9% = 0% TO 23% &
			! PRESET MATCH RESULT.  FOR ALL ENTRIES IN JOB &
			! TABLE, SEARCH AND EXIT IF MATCH FOUND &
			! EXIT WITH MATCH IF JOB IS OPSER ALSO &

15130		  FND% = 0% &
			! OTHERWISE, RETURN A FALSE RESULT &

15140	  FNEND &
		! THE FUNCTION EXIT AND END POINT &
	&

15200	! &
	&
	&
	!	C O N V E R T    P R O J - P R O G    D A T A &
	&

15201!				FNN2$ ( J% ) &
     !	&
     !	GIVEN EITHER A PROJECT # OR A PROGRAMMER # IN J%, RETURN &
     !	AN "*" FOR A VALUE OF 255, OR A NUMERIC TEXT STRING FOR ALL OTHER &
     !	VALUES &

15210	  DEF* FNN2$(J%) &
		! THE FUNCTION DEFINITION &

15220		  FNN2$ = "" &
		\ IF (J% AND 255%) = 255% &
		  THEN &
			  FNN2$ = "*" &
		  ELSE &
			  FNN2$ = NUM1$(J% AND 255%) &
				! PRESET BLANK STRING, CONVERT TO REAL VALUE &

15230	  FNEND &
		! END OF FUNCTION &
	&

16000	! &
	! &
	! &
	!	F I N D   J O B   N U M B E R   F R O M &
	! &
	!	      R E C E I V E R    I D &
	&

16001!				FNM% ( A$ ) &
     !	&
     !	GIVEN A RECEIVER NAME IN A$, THIS FUNCTION RETURNS THE VALUE 0% IF &
     !	NO MATCHING ENTRY IN THE CURRENT SYSTEM RECEIVE TABLE EXISTS. &
     !	OTHERWISE, THE JOB NUMBER OF THE 'RECEIVER' IS RETURNED &
     !
16010	  DEF* FNM%(A$) &
		\ FNM%=0% &
		! THE FUNCTION DEFINITION &

16020	  W%=M9%(19%) &
		! GET THE BASE ADDRESS OF THE RECEIVER TABLE &

16030	  IF W% = 0% &
	  THEN &
		  FNM% = 0% &
		\ GOTO 16045 &
			! SEARCH FOR RECEIVER TABLE POINTER. IF NONE, SET &
			! NO RECEIVER RESULT AND EXIT &

16040	  W1% = PEEK(W%) &
	\ IF A$=CVT%$(SWAP%(PEEK(W%+2%)))+ &
	        CVT%$(SWAP%(PEEK(W%+4%)))+ &
	        CVT%$(SWAP%(PEEK(W%+6%))) &
	  THEN &
		  FNM%= (PEEK(W%+8%) AND 255%)  IF PEEK(M%(15%)+(PEEK(W%+8%) AND 255%)) &
	  ELSE &
		  W%=W1% &
		\ GOTO 16030 &
			! CHECK THIS ENTRY IN TABLE. IF MATCH, EXTRACT ASSOC. &
			! JOB NUMBER AND RETURN. ELSE, ADV TABLE POINTER TO &
			! NEXT ENTRY &

16045	  FNEND &
		! END OF FUNCTION &
	&

16050	! &
	! &
	! &
	!	F I N D   J O B   N U M B E R   F R O M &
	! &
	!      R E C E I V E R    O B J E C T    T Y P E &
	&

16051!				FNM1% ( N% ) &
     !	&
     !	GIVEN A RECEIVER TYPE IN N%, THIS FUNCTION RETURNS THE VALUE 0% IF &
     !	NO MATCHING ENTRY IN THE CURRENT SYSTEM RECEIVE TABLE EXISTS. &
     !	OTHERWISE, THE JOB NUMBER OF THE 'RECEIVER' IS RETURNED &
     !
16060	  DEF* FNM1%(N%) &
		\ FNM1%=0% &
		\ W%=M9%(19%) &
		\ WHILE W% &
			\ S.ACC% = PEEK(W%+10%) AND 255% &
			\ S.OBJ% = SWAP%(PEEK(W%+8%)) AND 255% &
			\ NETWORK% = S.ACC% AND ( 255%-1%-2%-16%-128% ) &
			\ IF ( NOT NETWORK% ) AND ( S.OBJ%=N% ) THEN &
				  FNM1%=PEEK(W%+8%) AND 255% &
				\ GOTO 16095 &
		! THE FUNCTION DEFINITION &
		! GET THE BASE ADDRESS OF THE RECEIVER TABLE &
			! CHECK THIS ENTRY IN TABLE. IF MATCH, EXTRACT ASSOC. &
			! JOB NUMBER AND RETURN. &

16070	  		W% = PEEK(W%) &
		\ NEXT &
		! SET THE NEXT POINTER AND CONTINUE. &

16095	  FNEND &
		! END OF FUNCTION &
	&

16100	! &
	! &
	! &
	!	B R O A D C A S T    O P S E R ' S    O N - L I N E &
	! &
	!	        J O B    T A B L E    T O    K B 0 : &
	&

16110	  DEF* FNB% &
	\ FNB%=0% &
	\ W1%=M1%(63%,11%) &
		! FUNCTION DEFINITION &
		! PRESET RESULT TO ZERO &
		! MAKE SURE WE GET A FRESH COPY OF OPSER'S JOB TABLE &

16120	  W$="On-line jobs:"+CHR$(10%)+CHR$(13%) &
	\ W1%=0% &
		! SET UP FIRST PART OF MESSAGE. &
		! SET TABLE SEARCH INDEX &

16130	  FOR W%=0% TO 23% &
		\ IF J%(W%, 0%) <> 0% &
		  THEN &
			W$=W$+"  #"+ &
			FNN2$(J%(W%,1%))+ &
			"["+ &
			FNN2$(SWAP%(J%(W%,2%)))+ &
			","+ &
			FNN2$(J%(W%,2%))+ &
			"]"+ &
			"'"+RAD$(J%(W%,5%))+RAD$(J%(W%,6%))+"'" &
			+"SL="+NUM1$(J%(W%,7%)) &
		\ W1%=W1%+1% &
		\ W$ = W$ + CRLF$		IF (W1% AND 1%) = 0% &
			! FORM UP REST OF MESSAGE FOR EACH ENTRY &
			! ADV LIST POINTER &
			! ADD CR FOR EVERY OTHER ENTRY &

16140		  NEXT W% &
		\ IF W1%=0% &
		  THEN &
			W$=W$+"	(none)" &
				! IF NO ENTRIES IN TABLE, SIGNAL AS SUCH &

16150	  W$=SYS(CHR$(6%) &
		+CHR$(-5%) &
		+CHR$(0%) &
		+W$+CRLF$) &
			! BROADCAST TO THE OPERATOR CONSOLE &

16160	  FNEND &
		! THE FUNCTION END &
	&

16200	! &
	! &
	! &
	!	D I A L O G U E    F U N C T I O N &
	&

16201!			FNP%( P$ ) &
     !	&
     !	FNP% IS USED TO PRINT A DIALOGUE PROMPT P$ AND THEN GET THE USERS &
     !	RESPONSE.  THE FUNCTION RETURNS ONE OF THE FOLLOWING VALUES: &
     !	&
     !		1	USER TYPED '^' (BACKUP) KEY &
     !		2	USER TYPED Y, OR Y<TEXT> RESPONSE &
     !		3	USER TYPED N OR N<TEXT> RESPONSE &
     !		4	USER TYPED DEFAULT RESPONSE(<CR> ONLY &
     !						OR  <LF> ONLY) &
     !		5	UNKNOWN RESPONSE(INVALID RESPONSE) &
     !	&
     !	VARIABLE W$ CONTAINS THE TEXT OF THE USER RESPONSE, WITH ANY &
     !	RECEIVED TERMINATOR(S) REMOVED. &
     !
16202!	THIS ROUTINE ASSUMES THAT THE USER'S KEYBOARD IS OPEN ON CHANNEL &
     !	1%. &
     !
16210	DEF* FNP%(P$) &
		! THE FUNCTION DEFINITION &

16220	FNP% = 5% &
		! DEFAULT TO INVALID RESPONSE RESULT &

16230	GOTO 16250	IF LEN(P$) = 0% &
		! SKIP IF NO PROMPT TEXT &

16240	PRINT  IF CCPOS(0%) &
	\ PRINT P$; "  "; &
	  	! PRINT PROMPT &

16250	INPUT LINE #1%, W$ &
	\ PRINT IF CCPOS(0%) &
		! GET USER RESPONSE &

16260	  W$ = CVT$$(W$,4%+8%+32%+128%) &
		! REMOVE TERMINATOR &
		! STRIP LEADING/TRAILING BLANKS AND TABS &
		! CONVERT L/C -> U/C &

16270		  IF LEN(W$) = 0% &
		  THEN &
			  FNP% = 4% &
			\ GOTO 16310 &
				! CHECK FOR AND PROCESS DEFAULT RESPONSE &
				! (<CR> ONLY OR <LF> ONLY) &

16280		  IF W$ = "^" &
		  THEN &
			  FNP% = 1% &
			\ W$ = "" &
			\ GOTO 16310 &
				! CHECK FOR AND PROCESS BACKUP (^) INDICATOR &

16290		  IF INSTR(1%,"YES",W$) = 1% &
		  THEN &
			  FNP% = 2% &
			\ GOTO 16310 &
				! CHECK FOR AND PROCESS YES RESPONSE &

16300		  IF INSTR(1%,"NO",W$) = 1% &
		  THEN &
			  FNP% = 3% &
				!CHECK FOR AND PROCESS NO RESPONSE &

16310	  FNEND &
		! END OF FUNCTION &
	&

16400	! &
	! &
	! &
	!	P R I N T   P H A S E   S T A R T   M E S S A G E &
	&

16401!	&
     !				FNS% ( M$ ) &
     !	&
     !	THIS FUNCTION PROVIDES AN ORDERLY DISPLAY OF ALL PHASE STARTUP &
     !	OPERATOR INFORMATION MESSAGES &
     !
16410	  DEF* FNS%(M$) &
		\ FNS% = 0% &
		\ GOTO 16440 IF E0%		!Say nothing if CCL entry &
		\ M$ = CVT$$(M$,8%+128%)+" Phase" &
		\ PRINT &
		\ PRINT TIME$(0%);" ";DATE$(0%);"  ######## "; &
			LEFT(SPACE$((H0%-LEN(M$))/2%)+M$+SPACE$(H0%),H0%); &
			" ########" &
			! PRINT THE MESSAGE CENTERED IN LIGHTS &

16440	  FNEND &
		! END THE FUNCTION &

16450	! &
	! &
	!		C H E C K    P R I V I L E G E S &
	! &
	! &
	! Checks to see if the privilege passed is available to &
	! the user or account &
	! &
	! &
	!  			Return value: &
	!			0 = Does not have the privilege &
	!		       -1 = Has the privilege &
	DEF* FNPRIV%(PRIV$) &
	\	TMP$=MID(SYS(CHR$(6%)+CHR$(32%)+CHR$(1%)+STRING$(3%,0%)+ &
			PRIV$),3%,1%) &
	\	FNPRIV%=(ASCII(TMP$)=0%) &
	\ FNEND &

19000	! &
	! &
	! &
	!	E R R O R    H A N D L I N G &
	&

19010	RESUME 1130 IF ERR=10% AND ERL=1120% &
	! IGNORE (NO JOB) ERRORS WHEN BROADCASTING MESSAGES &

19020	  RESUME 1105 IF ERL=1103 &
		! FORGET THE NODE NAME IF WE DON'T HAVE DECNET &

19025	RESUME 1220 IF ((ERR = 18%) OR (ERR = 10%)) AND (ERL = 1210) &
\	RESUME 1230 IF (ERR = 18%) AND (ERL = 1220) &
		! HANDLE JOBS THAT HAVE GONE AWAY UNEXPECTEDLY &

19030	  IF ERR = 4% OR ERR = 32% &
	  THEN &
		SLEEP 2% &
		\ RESUME 1237 IF ERL = 1237% &
		\ RESUME 1270 IF ERL = 1270% &
		\ RESUME 1275 IF ERL = 1275% &
		\ RESUME 1290 IF ERL = 1290% &
		\ RESUME 1630 IF ERL = 1630% &
		\ RESUME 1840 IF ERL = 1840% &
		\ RESUME 2030 IF ERL = 2030% &
		\ RESUME 9200 IF ERL = 9200% &
			! HANDLE NO SMALL BUFFERS FOR MESSAGE SEND BY TRYING &
			! AGAIN &

19050	  IF  ERR = 5% THEN &
		RESUME 1246 IF ERL = 1237% &
	\	RESUME 1240 IF ERL = 1239% &
	\	RESUME 1699 IF ERL = 1630% &
	\	RESUME 1650 IF ERL = 1640% &
	\	RESUME 2000 IF ERL = 1840% &
	\	RESUME 2100 IF ERL = 2030% &
	\	IF ERL = 1300% THEN &
			SLEEP 1% &
	\		T2% = T2% - 1% &
	\		RESUME 1300 &
			! OK IF CAN'T FIND ON SEND TO QMAN, ERRCPY, OMS OR &
			! EVTLOG, RESUME TO LOOP IF NO MESSAGE FROM QMAN &
			! OR OMS, LOOP WAITING FOR MESSAGE FROM OPSER &
			! COUNT DOWN ON TIMER AFTER WAITING FOR 1 SECOND &

19060	  IF ERL = 14010% &
	  THEN &
		  SLEEP 1% &
		\ RESUME 14010 &
			! IF ERROR ON RE-ATTACH, WAIT AND TRY AGAIN &

19070	  IF  ERR = 5% &
	  AND ERL = 13030% &
	  THEN &
		  O9% = 0% &
		\ RESUME 13025 &
			! IF OPSER WORKFILE NOT PRESENT, THEN MARK &
			! OPSER AS NOT USABLE AND GO BACK TO DISABLE &

19080	  IF ERL = 3020 &
	    THEN &
		IF NOT(SWAPS.REMOVED%) &
		   THEN &
			SWAPS.REMOVED% = -1% &
	\		SLEEP 1% &
	\		RESUME 3020 &

19090	RESUME 2130 IF ERR=18% AND ERL=2120% &
		! IGNORE SYS CALL ERRORS DURING &
		! FINAL JOB KILLING PHASE &

19095	IF ERL=3080 THEN &
		RE.TRY%=RE.TRY%+1% &
	\	RESUME 3080 IF RE.TRY%=1% &
	\	RESUME 3090 IF ERR=21% &
	\	IF ERR=3% THEN PRINT &
		"?Can not proceed because of open file(s) on disk "; &
	\		PRINT CVT%$(SWAP%(PEEK(W1%)));NUM1$(W3%);":" &
	\		RESUME 32767 &
		! HANDLE DISK DISMOUNT ERRORS I.E. DISK HUNG ETC. &
		! RETRY ONCE IF ANYTHING GOES WRONG. &
		! SECOND TRY ONLY 'DISK PACK IS NOT MOUNTED' IS ALLOWED &
		! ELSE DIE. &

19110	IF ERR = 62% OR ERR = 66% THEN &
		RESUME 1146 IF ERL = 1145% &
	\	RESUME 1160 IF ERL = 1155% &
	\	IF ERL = 1078% THEN &
			TNET%=0% &
	\		NET.ON% = 0% &
	\		RESUME 1082 &
		! &
		! IF THIS IS NOT A DECNET SYSTEM, THEN IGNORE ERRORS ON &
		! SEND MESSAGE SYS CALLS TO DECNET &
		! &

19115	IF ERR = 1% AND ERL = 1078% THEN &
		W$ = SYS(CHR$(6%)+CHR$(22%)+CHR$(-20%)+ &
		         CHR$(2%)+STRING$(23%,0%)+CHR$(1%)) &
	\	W$ = "0" &
	\	NET.ON% = 0% &
	\	RESUME 1080 &
		! &
		! ISSUE "SET EXECUTOR STATE OFF" IF DECNET'S DATA &
		! BASE IS CORRUPTED. &

19120	IF ERR = 52% THEN &
		RESUME 1070 IF ERL = 1072% &
	\	RESUME 1074 IF ERL = 1076% &
	\	RESUME 1078 IF ERL = 1080% &
		! &
		! HANDLE DATA FORMAT ERRORS ON USER INPUT &
		! &

19130	RESUME 1895 IF ERL = 1840% &
		! &
		! ABORT SHUTDOWN IF ANY ERRORS OCCUR ON SEND TO EVTLOG. &
		! &

19140	IF ERR=11% AND ERL=16250% THEN &
		CLOSE 1%, 2%, 3% &
	\	PRINT IF CCPOS(0%) &
	\	PRINT &
	\	RESUME 32767 &

19900	W$=SYS(CHR$(0%)) &
	\ PRINT &
	\ PRINT "??Program failure in SHUTUP" &
	\ PRINT CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),3%),4%); &
	\ PRINT " at line"; ERL &
	\ CLOSE 1%, 2%, 3% &
	\ PRINT &
	\ RESUME 32767 &
		! FOR ALL UNKNOWN ERRORS, PRINT ERROR MESSAGE GIVING ERR &
		! #, TEXT AND LINE WHERE IT OCCURRED.  CLOSE ANY OPEN FILES &
		! AND ABORT PROGRAM &

30000	! &
	! &
	! &
	!	C C L    E N T R Y &
	&
	&

30010	ON ERROR GOTO 19000				!Set error trap &
\	E0% = 0%					!Default to RUN &
\	S$ = CVT$$(SYS(CHR$(7%)),4%+8%+16%+32%+128%+256%) !Get Core Common &
\	S% = INSTR(1%,S$,"/T:")				!Find the timeout qual &
\	GOTO 1020 IF S% = 0%				!Goto mainline code &
							! If no qualifier &

30020	ON ERROR GOTO 30040				!Set local error trap &
\	S$ = RIGHT(S$,S%+3%)				!Get the argument &
\	GOTO 30045 UNLESS LEN(CVT$$(S$,-2%))		!Error if no argument &
\	T% = VAL(S$)					!Get the time &
\	GOTO 30030 IF (T% >= 0%) AND (T% <= 99%)	!Within the range? &
\	PRINT "?Number not in range 0 to 99"		!Print error &
\	GOTO 32767					! and exit &

30030	ON ERROR GOTO 19000				!Put error trap back &
\	E0% = -1%					!Set CCL entry flag &
\	GOTO 1020					!Goto mainline code &

30040	PRINT "?Illegal number"				!Print error &
\	RESUME 32767					! and exit &

30045	PRINT "?Missing argument"			!Print error &
\	GOTO 32767					! and exit &

32766	! &
	! &
	! &
	!	E N D   O F   P R O G R A M &
	&

32767	END
