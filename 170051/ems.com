$
$	!	EMS$:EMS.COM
$
$ ! Command file to forward EMS mail to a DECmail-11 user.
$
$ _ON ERROR THEN _GOTO EXIT
$
$ ! First check to see if there were any EMS messages received.
$
$ EMS_COM=F$SEARCH("EMS$:EMS.COM")
$ F_NAME=F$SEARCH("EMS$:*.*")
$ _IF F_NAME .EQS. "" THEN _GOTO EXIT
$
$ ! Yes, we have a file, so see if it contains a "TO:" text in it.
$
$Again:
$
$ _IF F_NAME .EQS. EMS_COM THEN _GOTO NO_TO
$ _OPEN/READ 1 'F_NAME'
$
$More_line:
$
$ _READ/END=NO_TO 1 LINE
$ LINE = F$EDIT(LINE,48)
$ TO = F$INSTR(1,LINE,"TO:")
$ _IF TO .LE. 0 THEN _GOTO MORE_LINE
$
$ ! We found the "TO:" text, so the rest is a DECmail-11 user name.
$
$ U_NAME = F$RIGHT(LINE,TO+3)
$ _CLOSE 1
$ _ON WARNING THEN _GOTO  NO_TO
$ _ON ERROR THEN _GOTO NO_TO
$
$ ! Now, send the file to the user name we found
$
$ ONE_TRY = 0
$Send_mail:
$
$ _WRITE 0 "Sending EMS file ",F_NAME," to user ",U_NAME
$ _SET NOECHO
$ _SET DATA
$ _MAIL SEND/-CC/-EDI/-QUERY/-TEX/SUBJ="EMS Message" 'F_NAME' TO 'U_NAME'
GOTO KLUDGE_CITY
$ _IF ONE_TRY .EQ. 1 THEN _GOTO GOT_ERROR
$ ONE_TRY = 1
$
$ ! Search a names data base, and if found then goto Send_mail and
$ ! try one more time to send the mail there.
$
$ _GOTO GOT_ERROR
$
$ ! This kludge is caused by DECmail-11 always exiting with success even
$ ! if the send didn't work.  So, if it does work, then we exit normally
$ ! and then execute the GOTO KLUDGE_CITY.  If there is any error, then
$ ! DECmail-11 will prompt, and "eat" the GOTO KLUDGE_CITY statement, and
$ ! we will endup at the next DCL statement.
$
$Kludge_city:
$
$ _DELETE 'F_NAME'
$Got_error:
$ _SET NODATA
$ _SET ECHO
$ _ON ERROR THEN _GOTO EXIT
$
$No_to:
$
$ ! Done with this file so go see if there is another EMS message.
$
$ _CLOSE 1
$ F_NAME=F$SEARCH()
$ _IF F_NAME .NES. "" THEN _GOTO AGAIN
$
$Exit:
$
$ _IF EMS_COM .EQS. "" THEN _GOTO FATAL
$Any_left:
$ F_NAME=F$SEARCH(EMS$:*.*")
$ _IF F_NAME .EQS. EMS_COM THEN _GOTO ANY_LEFT
$ _IF F_NAME .EQS. "" THEN _GOTO BACK_LATER
$
$ ! The next section provides two options of notification if all messages
$ ! in the EMS$: account can not be set properly.  Modify the following
$ ! lines if you wish to be notified of unsent EMS messages by BROADCAST
$ ! and/or MAIL.
$
$ _GOTO BACK_LATER
$
$ _BROADCAST KB0: "Not all EMS messages could be sent."
$
$ _SET NOECHO
$ _MAIL SEND/-CC/-EDI/-QUERY/-TEX/SUBJ="Not all EMS messages could be sent." -
TO SYSTEM
$ _SET ECHO
$
$Back_later:
$
$ ! All done for now, so setup for us to come back in a little while.
$
$ _ON ERROR THEN _GOTO FATAL
$ _SUBMIT EMS$:EMS.COM/AFTER=+5MIN
$
$Fatal:
$
$ _EXIT
