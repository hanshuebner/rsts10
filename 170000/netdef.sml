@  Ї: 	                А .                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               И░Тg   ╝"
║─"
*У 	#KБ6╫#KБ7e t'
' р2   ╜L;{	0х бE{Р3ЙvQ
╚ Ш rTЛ6} \X╓( │ ▌f;g3┐ Oq■v ч OqJw   сyу}2 ╤╕[40 ьи%? г ;йА
 °<й%Q ЗGйHS$ GйДЕ Lй╒Mg ibйfwa = dйe ═ mйG3 ─ mй╫┼ ЎmйЗd6 wmйуe: √ mйf╞ ЪЬйЗ2< P Ьйе2 ╥Ьйу3? 7ЮйвKП ╚ЮйеKб +ЮйНMЛ o Юй╒M~ ЕЮйЗ}Т ├ ЮйМ}Ъ ╣Юй9Аг Щ ай"w Ю ┼й|X# & бке}$ dйк&XuйкUZn > мкА
(  мк!АM н мк Ц/ БокfEс ╓кхГн Ь █к┼, B єкА
1 yєквdЕ Ы єкжd@мєк
f7/їкд}о ╘ їке}м R їк~F Т∙кИМ д ¤кжK¤ 7  кА
% ) кUk g кЗC   кж■ леd# №лf╢ ўлg[ я л&? ў лUAi Y л╣AV i .лL CMлА

ъ ЩлА
	 й гл%XO ч длД ┴ одл[!└ Ц Єл═Й пЄл
XN ЄлЫY╜ YЄлБZ^ Ъ 
▒%! i П▒Д:	 t                                                                                                                                                                                                                                                                                                                                                                                 .MACRO	REGSCR			;REGISTER SAVE CO-ROUTINE (RTS PC TO RESTORE)
	CALLX	REGSCR,R5
.DSABL	CRF
TOS.R0	=	0+1*2
TOS.R1	=	1+1*2
TOS.R2	=	2+1*2
TOS.R3	=	3+1*2
TOS.R4	=	4+1*2
TOS.R5	=	5+1*2
TOS.RA	=	6+1*2
.ENABL	CRF
.ENDM	REGSCR
.MACRO	REGSAV	METHOD
.IF	IDN	<METHOD>,<INLINE>
	 MOV	R5,-(SP)
	 MOV	R4,-(SP)
	 MOV	R3,-(SP)
	 MOV	R2,-(SP)
	 MOV	R1,-(SP)
	 MOV	R0,-(SP)
.IFF
.IF	NB	<METHOD>
.ERROR	; ILLEGAL ARGUMENT "METHOD"
.ENDC
	CALLX	REGSAV,R5
.ENDC
.DSABL	CRF
TOS.R0	=	0*2
TOS.R1	=	1*2
TOS.R2	=	2*2
TOS.R3	=	3*2
TOS.R4	=	4*2
TOS.R5	=	5*2
TOS.PC	=	6*2
TOS.PS	=	7*2
.ENABL	CRF
.ENDM	REGSAV
.MACRO	.MDELET	MAC
.MACRO	MAC
.ENDM	MAC
.ENDM	.MDELET
.MACRO	$ADJDEF

.BSECT	,NOCREF		;Define adjacency flags (in ADJFLG)		;007
AF.ARE:	.BLKB	.	;Adjacent node is an area router		;007
AF.PH4:	.BLKB	.	;Adjacent node is Phase IV			;007
AF.END:	.BLKB	.	;Adjacent node is an end node			;007
AF.INI:	.BLKB	.	;Adjacency is initializing			;007
AF.AUP:	.BLKB	.	;Adjacency should be logged as up		;007
	.BLKB	.	;Reserved					;007
	.BLKB	.	;Reserved					;007
	.BLKB	.	;Reserved					;007

ADJVIR	=:	140000+2 ;Start of adjacency data			;007
OAJVIR	=:	140000+2 ;Start of output adjacency data		;007

.MCALL	.MDELET								;007
.MDELET $ADJDEF								;007
.ENDM	$ADJDEF								;007
.MACRO	$COUNT
.MACRO	COUNTL	LOC,?L
.NLIST
.NTYPE	$$$$$$,LOC
$$$$$$	=	$$$$$$ & 70
.IF EQ	<$$$$$$-10> * <$$$$$$-60>
	ADD	#1,LOC
.IF EQ	<$$$$$$-10>
	ADC	2'LOC
.IFF
	ADC	2+LOC
.IFTF
	BCC	L
.IFT
	DEC	2'LOC
.IFF
	DEC	2+LOC
.ENDC
	DEC	LOC
L:
.IFF
	.ERROR	; ILLEGAL PARAMETER TYPE "LOC"
.ENDC
.LIST
.ENDM	COUNTL

; INCREMENT A WORD COUNTER

.MACRO	COUNTW	LOC,?L
.NLIST
	INC	LOC
	BNE	L
	DEC	LOC
L:
.LIST
.ENDM	COUNTW

; INCREMENT A BYTE COUNTER

.MACRO	COUNTB	LOC,?L
.NLIST
	INCB	LOC
	BNE	L
	DECB	LOC
L:
.LIST
.ENDM	COUNTB

; ADD TO A LONGWORD COUNTER
;
; MAY BE USED WITH INDIRECT (TYPE 1) OR INDEXED/RELATIVE (TYPE 6)
; DESTINATION OPERAND ONLY

.MACRO	ADDL	AMT,LOC,?L
.NLIST
.NTYPE	$$$$$$,LOC
$$$$$$	=	$$$$$$ & 70
.IF EQ	<$$$$$$-10> * <$$$$$$-60>
	ADD	AMT,LOC
.IF EQ	<$$$$$$-10>
	ADC	2'LOC
.IFF
	ADC	2+LOC
.IFTF
	BCC	L
.IFT
	DEC	2'LOC
.IFF
	DEC	2+LOC
.ENDC
	MOV	#177777,LOC
L:
.IFF
	.ERROR	; ILLEGAL PARAMETER TYPE "LOC"
.ENDC
.LIST
.ENDM	ADDL

; ADD TO A WORD COUNTER

.MACRO	ADDW	AMT,LOC,?L
.NLIST
	ADD	AMT,LOC
	BCC	L
	MOV	#177777,LOC
L:
.LIST
.ENDM	ADDW

.MCALL	.MDELET
.MDELET	$COUNT
.ENDM	$COUNT
.MACRO	HEX	TEXT
$$$$$0 = 0
.IRPC	..CHR.,<TEXT>
 .IF	DIF	..CHR.,-
  $$$$$1 = 0
  $$$$$2 = 0
  .IRPC ..HEX.,<0123456789ABCDEF>
   .IF	IDN	..CHR.,..HEX.
    $$$$$2 = 200!$$$$$1
   .ENDC ;IDN ..CHR.,..HEX.
   $$$$$1 = $$$$$1 + 1
  .ENDR ;..HEX.
  .IF EQ $$$$$2
   .ERROR ;Invalid character ''..CHR.'' in hex string ''TEXT''
   .MEXIT
  .ENDC ;EQ $$$$$2
  .IF	EQ	$$$$$0
   $$$$$3 = <$$$$$2&17>*20
  .IFF
   .BYTE	$$$$$3!<$$$$$2&17>
  .ENDC ;EQ $$$$$0
   $$$$$0 = <-$$$$$0> + 1
   .IFF
   .IF	NE	$$$$$0
	.BYTE	$$$$$3/20
    $$$$$0	=	0
   .ENDC ;NE $$$$$0
  .ENDC ;DIF ..CHR.,-
 .ENDR ;.IRPC ..CHR.
.IF	NE	$$$$$0
	.BYTE	$$$$$3/20
$$$$$0 = 0
.ENDC	;NE $$$$$0
.ENDM	HEX
.MACRO	.POINT	FOO
	  .WORD	FOO-20000
.ENDM	.POINT
.MACRO	$RIB
.DSECT

S.LINK:	.BLKW		;LINK TO NEXT BLOCK
S.RCID:	.BLKB	6	;RECEIVER ID IN ASCII
S.JBNO:	.BLKB		;JOB NUMBER TIMES 2
S.OBJT:	.BLKB		;OBJECT TYPE (NETWORKS)
S.ACCS:	.BLKB		;ACCESS CONTROL
S.SRBN:	.BLKB		;SUB RIB NUMBER
S.BMAX:	.BLKW		;BUFFER MAXIMUM IN BYTES
S.MMAX:	.BLKB		;MESSAGE MAXIMUM
S.MCNT:	.BLKB		;MESSAGE COUNT
S.MLST:	.BLKW	2	;MESSAGE LIST ROOT/TAIL POINTERS
S.LMAX:	.BLKB		;LINK MAXIMUM
S.LCNT:	.BLKB		;LINK COUNT
S.LLST:	.BLKW	2	;LINK LIST ROOT/TAIL POINTERS
S.OMAX:	.BLKB		;Outbound logical link maximum			;000
S.PQTA:	.BLKB		;Packet per message quota (for EMT logger)	;000
	.BLKW	2	;Reserved

.BSECT			;BITS IN S.ACCS

SA.LCL:	.BLKB	.	;ALLOW LOCAL SENDERS
SA.PRV:	.BLKB	.	;ALLOW ONLY PRIVILEGED LOCAL SENDERS
SA.NET:	.BLKB	.	;ALLOW NETWORK SENDERS
SA.1SH:	.BLKB	.	;NETWORK SINGLE LINK MODE
SA.NCS:	.BLKB	.	;NO CONDITIONAL SLEEP CHECK ON THIS RIB
SA.TRA:	.BLKB	.	;THIS IS THE DECNET TRACER
SA.EVT:	.BLKB	.	;THIS IS THE DECNET EVENT LOGGER
SA.XOF:	.BLKB	.	;FURTHER LOCAL SENDS ARE 'XOFFED'

.MCALL	.MDELET
.MDELET	$RIB

.ENDM	$RIB
.MACRO	$PMB
.DSECT

P$LINK:	.BLKW			;LINK TO NEXT PENDING MESSAGE BLOCK
P$BUFA:	.BLKW			;CONTORTED BUFFER ADDRESS (0 IF NONE)
P$TYPE:	.BLKB			;MESSAGE TYPE CODE
P$SNDR:	.BLKB			;LOCAL: 0 OR ODD = SYSTEM PROCESS
				;       ELSE     = SENDING JOB # * 2
P$SPPN:	.BLKW			;PPN OF LOCAL SENDER
	.BLKW			;RESERVED
P$BREM:	.BLKW			;BYTES REMAINING IN DATA MESSAGE (IN BUFFER)
P$PARM:	.BLKW	10		;USER OR DECNET DEFINED PARAMETERS BEGIN HERE

; PENDING MESSAGE BLOCK (PMB) DECNET DEFINITIONS

.DSECT	P$SNDR

P$ULA:	.BLKB			;USER LINK ADDRESS
P$LLA:	.BLKW			;LOCAL  LINK ADDRESS
P$RLA:	.BLKW			;REMOTE LINK ADDRESS
P$BREM:	.BLKW			;BYTES REMAINING IN DATA MESSAGE (IN BUFFER)
P$MFLG:	.BLKB			;MESSAGE FLAGS (DM)
	.BLKB			;RESERVED

P$RMOD:	;BLKB			;REMOTE LINK MODIFIERS (CI, CC)
P$RCNT:				;SEGMENT OR MESSAGE REQUEST COUNT
P$RESN:	.BLKW			;REJECTION/DISCONNECTION REASON (CR,DI,AB)

P$RMAX:	.BLKW			;RECEIVE MAXIMUM (CI,CC)
P$TMAX:	.BLKW			;TRANSMIT MAXIMUM (CI,CC)

	.BLKB	12.		;RESERVED

; P$MFLG DEFINITIONS FOR DATA MESSAGES

.BSECT

DM.BOM:	.BLKB	.		;THIS IS BEGINNING OF MESSAGE
DM.EOM:	.BLKB	.		;THIS IS END OF MESSAGE
DM.TRN:	.BLKB	.		;TOO MUCH DATA IN RECEIVED MESSAGE -
				;HAS BEEN TRUNCATED TO MAX ALLOWED
.MCALL	.MDELET
.MDELET	$PMB
.ENDM	$PMB
.MACRO	$CCB
.DSECT

CC.LNK:	.BLKW			;LINK TO NEXT CCB IN SOME LIST
CC.WAK:	.BLKW			;NSP TRANSMIT QUE LINK (WAITING FOR ACK)
CC.FC:	.BLKB			;FUNCTION CODE (SEE DEFINITIONS BELOW)
CC.DFL:	.BLKB			;DEVICE/BUFFER DISPOSITION FLAGS
CC.FLG:	.BLKW			;MSGFLGS FIELD VALUE
CC.RMA:				;Next address for routing message
CC.LLB:				;LLB ADDRESS
CC.RIB:	.BLKW			;RECEIVER ID BLOCK POINTER
CC.FCV:				;FLOW CONTROL VALUE FOR LS MESSAGE
CC.ADR:				;NODE ADDRESS OF REMOTE NODE
CC.NOB:	.BLKW			;Pointer to NOB for loop testing (ECL->Routing)
CC.AKN:				;ACKNUM FROM RECEIVED MESSAGES
CC.FQB:	.BLKW			;PTR TO FIRQB ON USER SYNCHRONOUS CALLS
CC.SGN:	.BLKW			;SEGNUM FOR CHECKING ACKNOWLEDGEMENTS
CC.SEC:	.BLKW	4		;SECONDARY (USER) BUFFER DESCRIPTOR
CC.BUF:	.BLKW	4		;PRIMARY (XBUF) BUFFER DESCRIPTOR

; FIELDS WITHIN CC.SEC USED IN TRN AND THE DRIVERS

.DSECT	CC.SEC
	.BLKW			;USED ONLY AS BF.ADR
CC.ERR:	.BLKW			;ERROR CODE (WITH FC$ERR)
CC.DDB:	.BLKW			;DDB address of circuit
CC.HOP:	.BLKW			;Next hop (on xmit) or prev. hop (on recv) ;007

; OFFSETS WITHIN BUFFER DESCRIPTORS (CC.BUF AND CC.SEC)
;
; REFERENCES TO THE BUFFER DESCRIPTORS SHOULD BE OF THE FORM
;
; E.G.	MOV	XXXX,CC.SEC+BF.ADR(R5)

.DSECT

BF.ADR:	.BLKW			;BUFFER ADDRESS (RSTS SHIFTED MMU FORMAT)
BF.VIR:	.BLKW			;BUFFER ADDRESS (VIRTUAL)
BF.MMU:	.BLKW			;KISAR6 VALUE FOR THIS BUFFER
MS.SIZ:				;MESSAGE SIZE (DRIVER OR CC.SEC ONLY)
BF.END:	.BLKW			;END OF MESSAGE (VIRTUAL)

; DEVICE/BUFFER DISPOSITION FLAGS (CC.DFL)
;
; Note: These bits have different meanings depending on the origin of the
; message. DF.TRN controls the origin of the message. If clear, it
; is an ECL message. If set, it is a Routing message. (ROU) denotes;
; the bit meaning from the routing layer; (ECL) denotes the bit meaning
; from ECL. If the bit belongs to the data link layer, it is denoted
; by (DLL).

.BSECT	,NOCREF

DF.CNT:	.BLKB	.		;Packet continued in Next CCB/buffer	;003
DF.ERR:				;(DLL) Message has erred out once	;003
DF.MOP:	.BLKB	.		;(DLL) Ethernet Internal MOP buffer	;003
DF.TRY:				;(ECL) Try hard to deliver this message ;007
DF.RTM:	.BLKB	.		;(ROU) This is a routing message buffer
DF.TRN:	.BLKB	.		;Message originated in routing layer
DF.FWD:				;(ROU) Message is a forwarding message	;012
DF.RTS:	.BLKB	.		;(ECL) Return to sender if undeliverable
DF.NSP:	.BLKB	.		;(ECL) Transmit in progress
DF.RXM:	.BLKB	.		;(ECL) Retransmission is needed
DF.WAK:	.BLKB	.		;(ECL) Message is waiting for an ACK

; CCB FUNCTION CODE (CC.FC) DEFINITIONS

; USER FUNCTIONS (USER --> NSP)

.DSECT

FC$BAD:	.BLKW			;ILLEGAL FUNCTION
FC$REM:	.BLKW			;NSP  REMOVE ALL LINKS (IMMEDIATELY)
FC$DCL:	.BLKW			;RECEIVER DECLARATION
FC$RCV:	.BLKW			;USER RECEIVE
FC$UCI:	.BLKW			;USER CONNECT INITIATE
FC$UCC:	.BLKW			;USER CONNECT CONFIRM
FC$UCR:	.BLKW			;USER CONNECT REJECT
FC$UDM:	.BLKW			;USER DATA MESSAGE
FC$INT:	.BLKW			;USER INTERRUPT
FC$ULS:	.BLKW			;USER LINK SERVICE
FC$DIS:	.BLKW			;USER DISCONNECT
FC$ABT:	.BLKW			;USER ABORT LOGICAL LINK
FC$EVT:	.BLKW			;USER LOG EVENT
FC$ELM:	.BLKW			;USER SET EVENT LOGGER MASKS
FC$SPC:	.BLKW			;USER PERFORM DEVICE DEPENDENT SPEC FUNCTION
FC$GLT:	.BLKW			;GET NSP/LLT INFORMATION

; TRANSPORT -> NSP FUNCTION CODES

FC$RCP:	.BLKW			;RECEIVE COMPLETE
FC$SPE:	.BLKW			;Monitor -> NSP : spawn failed		;000
FC$XCP:	.BLKW			;TRANSMIT COMPLETE
FC$XER:	.BLKW			;ERROR IN TRANSMITTING MESSAGE
FC$RET:	.BLKW			;MESSAGE RETURNED - ADDRESSEE UNKNOWN

; NSP -> TRANSPORT FUNCTION CODES

.DSECT	.READ&377

	.BLKW			;RESERVED
	.BLKW			;RESERVED
FC$XMA:	.BLKW			;TRANSMIT MESSAGE GIVEN A NODE ADDRESS
FC$XMD:	.BLKW			;TRANSMIT MESSAGE VIA A SPECIFIED DEVICE

; DLL -> TRANSPORT FUNCTION CODES

.DSECT	FC$RCP

FC$RCP:	.BLKW			;RECEIVE COMPLETE - DATA MESSAGE RECEIVED
FC$ERR:	.BLKW			;ERROR DETECTED
FC$XCP:	.BLKW			;TRANSMIT COMPLETE (USED ONLY WITH TRN)
FC$XER:	.BLKW			;ERROR IN TRANSMIT (USED ONLY WITH TRN)
FC$RER:	.BLKW			;Buffer received in error
FC$MUL:	.BLKW			;Enable Multicasts end notification	;003
FC$PHY:	.BLKW			;Change Physical Address end notify	;003

; DLL FUNCTION CODES

.DSECT	.READ&377

FC$RD:	.BLKW			;USER RECEIVE REQUEST
FC$WRT:	.BLKW			;USER TRANSMIT REQUEST
FC$XMT:	.BLKW			;TRANSPORT TRANSMIT REQUEST
FC$BUF:	.BLKW			;TRANSPORT RELEASE RECEIVE BUFFER TO DDCMP
FC$STA:	.BLKW			;GET LINE/CIRCUIT STATUS

.ASSUME	FC$RD EQ .READ&377
.ASSUME FC$WRT EQ .WRITE&377

CBFSIZ	=:	1200		;Size of a communications buffer

.MCALL	.MDELET
.MDELET	$CCB
.ENDM	$CCB
.MACRO	$ETHDEF
.DSECT	,NOCREF
EP.DST:	.BLKB	6	;Destination address
EP.SRC:	.BLKB	6	;Source address
EP.PRO:	.BLKB	2	;Protocol type
	.BLKB	2	;Reserved for DLL use
EP.SIZ:			;Size of protocol header
.MCALL	.MDELET
.MDELET $ETHDEF
.ENDM	$ETHDEF
.MACRO	$DDCDEF								;026
.DSECT	,NOCREF
D.SYN:	.BLKB	8.		;Sync bytes
D.MTYP:	.BLKB			;Message type
D.STYP:	.BLKB			;Sub-type
D.RSN:	.BLKB			;Reason for NAK
D.RESP:	.BLKB			;Response number
D.NUM:	.BLKB			;Sequence number
D.ADDR:	.BLKB			;Address (1 for point to point)
D.HCRC:	.BLKB	2		;Header CRC
D.HDRL:				;Length of DDCMP header
.ENDM	$DDCDEF								;026
.MACRO	$MSGFUN
.DSECT	-24.

SRFMIN:				;Minimum Send/receive function code
SR$CTL:	.BLKB			; -24  misc.   control
SR$LNK:	.BLKB			; -23  link    control
SR$NOD:	.BLKB			; -22  node    control
SR$LIN:	.BLKB			; -21  circuit control
SR$NSP:	.BLKB			; -20  NSP     control
SR$EXE:	.BLKB			; -19  executor information
	.BLKB			; -18  illegal
	.BLKB			; -17  illegal
	.BLKB			; -16  illegal
	.BLKB			; -15  illegal
	.BLKB			; -14  illegal
	.BLKB			; -13  illegal
SR$LSE:	.BLKB			; -12  LAT set functions		;021
SR$SLM:	.BLKB			; -11  send local data with mask	;000
SR$EVT:	.BLKB			; -10  send event
SR$ABT:	.BLKB			; -9 - user abort
SR$DIS:	.BLKB			; -8 - user disconnect
SR$ULS:	.BLKB			; -7 - user link service
SR$INT:	.BLKB			; -6 - user interrupt
SR$UDM:	.BLKB			; -5 - user data message
SR$UCR:	.BLKB			; -4 - user connect reject
SR$UCC:	.BLKB			; -3 - user connect confirm
SR$UCI:	.BLKB			; -2 - user connect initiate
LCL$MN:				; Start of local function codes		;000
SR$SLD:	.BLKB			; -1 - local send			;000
SR$TRA:				;  0 - trace data (PMB type code only)
SR$REM:	.BLKB			;  0 - NSP  remove all links
SR$DCL:	.BLKB			;  1 - user declare
SR$RCV:	.BLKB			;  2 - user receive
SR$DCR:	.BLKB			;  3 - declare and receive (local only)	;000
LCL$MX:				; End of local function codes		;000
SR$XPK:	.BLKB			;  4 - extended peek hook		;000
	.BLKB			;  5 - illegal
	.BLKB			;  6 - illegal
	.BLKB			;  7 - illegal
	.BLKB			;  8 - illegal
SR$SPC:	.BLKB			;  9 - device dependent spec function
SR$GLT:	.BLKB			; 10 - NSP information (obsolete)
	.BLKB			; 11 - illegal
SR$LSH:	.BLKB			; 12 - LAT show functions		;021
SR$RCL:	.BLKB			; 13 - Recall commands			;028
	.BLKB			; 14 - illegal
SR$CTR:	.BLKB			; 15 - read counters
SRFMAX:				;Maximum send/receive function code

; Send/receive sub-function code definitions (FIRQB byte 5)

.DSECT	1		;Sub-functions of SR$EXE
SF$LNN:	.BLKB			;Get local node name

.DSECT	1		;Sub-functions of SR$NSP
SF$NSO:	.BLKB			;Set nsp state to on
SF$STA:	.BLKB			;Change nsp state
SF$NCP:	.BLKB			;Change nsp parameters

.DSECT	1		;Sub-functions of SR$LSH			;021
SF$NOD: .BLKB			;Node characteristics 			;021
SF$SVC:	.BLKB			;Service characteristics		;021
SF$LIN:	.BLKB			;Ethernet Line settings			;021
SF$COU:	.BLKB			;Node, Server and Ethernet Counters	;021
SF$SVR:	.BLKB			;Server characteristics			;021
SF$SES:	.BLKB			;Session information			;021
SF$SHP:	.BLKB			;Show outbound LAT port Characteristics	;027

.DSECT	1    		;Sub-functions of SR$LSE 			;021
SF$NDM:	.BLKB			;Modify Node characteristics 		;021
.ASSUME SF$NOD EQ SF$NDM						;021
SF$SCM:	.BLKB			;Modify Service characteristics		;021
.ASSUME SF$SVC EQ SF$SCM						;021
SF$LNM:	.BLKB			;Modify Enable/Disable Ethernet Lines	;021
.ASSUME SF$LIN EQ SF$LNM						;021
SF$CRP:	.BLKB			;Create outbound LAT port		;027
SF$DLP:	.BLKB			;Delete outbound LAT port		;027
SF$STP:	.BLKB			;Set outbound LAT port characteristics	;027

.DSECT	1		;Sub-functions of SR$LIN
SF$ASN:	.BLKB			;Set line owner exe
SF$DEA:	.BLKB			;Clear line owner
SF$LON:	.BLKB			;Set line state to on
SF$LOF:	.BLKB			;Set line state to off
SF$LCH:	.BLKB			;Change line parameters

.DSECT	1		;Sub-functions of SR$NOD
SF$ALN:	.BLKB			;Add loop node
SF$NUM:	.BLKB			;Look up node by number
SF$DLN:	.BLKB			;Delete loop node

.DSECT	1		;Sub-functions of SR$LNK
SF$ABT:	.BLKB			;Abort link

.DSECT	1		;Sub-functions of SR$CTL
SF$MSK:	.BLKB			;Set event logger masks
	.BLKB			;Reserved
SF$TB1:	.BLKB			;Get NSP tables 1

.DSECT	1		;Sub-functions of SR$CNT
SF$NCT:	.BLKB			;Read or read/clear node counters

.MCALL	.MDELET
.MDELET	$MSGFUN
.ENDM	$MSGFUN
.MACRO	NETFUN	NAM,SUBTTL
.SBTTL	SUBTTL
	TMPORG	DNDTBL,NC$'NAM,GBL					;025
	 .WORD	NET'NAM
	UNORG
.LIST

NET'NAM:
.NLIST
.ENDM	NETFUN
.MACRO	$OVLFUN
.DSECT

NC$NOP:	.BLKW			;NOP
NC$NSO:	.BLKW			;NSP ON
NC$NCP:	.BLKW			;NSP CHANGE PARAMETERS
NC$ASN:	.BLKW			;ASSIGN DEVICE TO NSP
NC$DEA:	.BLKW			;DEASSIGN DEVICE FROM NSP
NC$LON:	.BLKW			;CIRCUIT ON
NC$LOF:	.BLKW			;CIRCUIT OFF
NC$LCH:	.BLKW			;CHANGE CIRCUIT PARAMETERS
NC$NCH:	.BLKW			;CHANGE NODE PARAMETERS
NC$SID:	.BLKW			;SET SYSTEM ID
NC$ABT:	.BLKW			;SYSTEM MANAGER ABORT
NC$CCI:	.BLKW			;CONTINUE CONNECT INITIATE FROM NSP
NC$CIR:	.BLKW			;Connect initiate received processing
NC$NUM:	.BLKW			;LOOK UP NODE INFO BY NUMBER
NC$SDA:	.BLKW			;SET DEFAULT ACCOUNT NUMBER (PPN)
NC$TB1:	.BLKW			;NSP TABLES 1
NC$LNN:	.BLKW			;LOCAL NODE NAME (ETC.)
NC$ALN:	.BLKW			;ADD LOOP NODE
NC$DLN:	.BLKW			;DELETE LOOP NODE
NC$STA:	.BLKW			;NCP SET STATE
NC$NCT:	.BLKW			;READ OR READ/CLEAR NODE COUNTERS
NC$MSK:	.BLKW			;SET EVENT LOGGER MASKS
NC$AUP:	.BLKW			;Log adjacency up event			;007
NC$NOD:	.BLKW			;Show LAT node characteristics		;021
NC$SVC:	.BLKW			;Show LAT service characteristics	;021
NC$LIN:	.BLKW			;Show LAT Ethernet Line settings	;021
NC$COU:	.BLKW			;Show LAT Node,Server, Ethernet Counters;021
NC$SVR:	.BLKW			;Show LAT server characteristics	;021
NC$SES:	.BLKW			;Show LAT session information		;021
NC$NDM:	.BLKW			;Modify LAT Node characteristics	;021
NC$SCM:	.BLKW			;Modify LAT Service characteristics	;021
NC$LNM:	.BLKW			;Modify (Enable/Disable) LAT Enet Lines	;021
NC$SHP:	.BLKW			;Show outbound LAT port characteristics	;027
NC$CRP:	.BLKW			;Create outbound LAT port		;027
NC$DLP:	.BLKW			;Delete outbound LAT port		;027
NC$STP:	.BLKW			;Set outbound LAT port characteristics	;027
NCFMAX:				;MAXIMUM OVERLAID INTERNAL SUBFUNCTION CODE

.MCALL	.MDELET
.MDELET	$OVLFUN
.ENDM	$OVLFUN
.MACRO	$FUNFLG
.BSECT	HIGH

FC.CFG:	.BLKB	.	;Caller must have SWCFG privilege
FC.NSP:	.BLKB	.	;NSP MUST BE ENABLED
FC.CTL:	.BLKB	.	;Caller must have SWCTL privilege
FC.RIB:	.BLKB	.	;CALLER MUST BE A DECLARED RECEIVER
FC.LAT:	.BLKB	.	;LAT must be present on system			;021
FC.OVL:	.BLKB	.	;SET FOR NON-RESIDENT FUNCTIONS
FC.NSY:	.BLKB	.	;FUNCTION IS NON-SYNC IF SET
FC.LCK:	.BLKB	.	;JOB TO REMAIN LOCKED IF SET

.MCALL	.MDELET
.MDELET	$FUNFLG
.ENDM	$FUNFLG
.MACRO	$NSPDEF
.DSECT	1			;IN NSPSTA

NSP$OF:	.BLKB			;OFF, NSP IS NOT RUNNING
NSP$ON:	.BLKB			;ON, ALL CONNECTS ALLOWED
NSP$SH:	.BLKB			;SHUTTING DOWN, CHANGE TO OFF WHEN LNKCNT=0
NSP$RS:	.BLKB			;RESTRICTED, REJECT INBOUND CONNECTS

.MCALL	.MDELET
.MDELET	$NSPDEF
.ENDM	$NSPDEF
.MACRO	$COMM	Z
Z'.IRP	NAME,<XM,XD,XE,XH,TT>						;026
.ENDM	$COMM
.MACRO	$LATDEF

.BSECT			;Define bits used by LAT Send/Receive (SF$COU)	;022
	.BLKB	.	; Bit 0, Reserved				;022
	.BLKB	.	; Bit 1, Reserved				;022
	.BLKB	.	; Bit 2, Reserved				;022
	.BLKB	.	; Bit 3, Reserved				;022
	.BLKB	.	; Bit 4, Reserved				;022
	.BLKB	.	; Bit 5, Reserved				;022
	.BLKB	.	; Bit 6, Reserved				;022
LF.ZER:	.BLKB	.	; Bit 7, Zero LAT {Server!Node!Ethernet} counters;022

.MCALL	.MDELET
.MDELET	$LATDEF
.ENDM	$LATDEF
.MACRO	$NOB
.DSECT

	.BLKW			;RESERVED (FOR FIP PROCESSING)
	.BLKW			;RESERVED (FOR FIP PROCESSING)
N.DELY:	.BLKW			;DELAY VALUE FOR NODE
N.NAME:	.BLKB	6		;NODE NAME - 6 ASCII CHARACTERS 0 PAD
N.DDB:				;DDB address of output circuit (loop nodes only)
N.ADDR:	.BLKW			;REMOTE NODE ADDRESS - WORD
N.NHOP:	.BLKW			;Next hop (ethernet loop nodes only)	;007
	.BLKW	2		;Reserved				;007
N.NCB:	.BLKW			;POINTER TO NODE COUNTER BLOCK
N.FLGS:	.BLKB			;NODE FLAG BITS (SEE DEFINITIONS BELOW)
N.LCNT:	.BLKB			;COUNT OF LINKS TO THIS NODE 
N.LLLB:	.BLKW	2		;LLB LIST FOR THIS NODE
N.LINK:	.BLKW			;LINK TO NEXT NODE BLOCK
N.CTMR:	.BLKW			;COUNTER TIMER CLOCK

.ASSUME	N.DELY EQ FQ$FUN
.ASSUME	N.NAME EQ FQ$NOD
.ASSUME	N.ADDR EQ FQ$NAD
.ASSUME N.ADDR+2 EQ FQ$AKA						;005

; NODE FLAG BIT (N.FLGS) DEFINITIONS

.BSECT

NF.REA:	.BLKB	.		;Entry is in use (in database only)
NF.LUP:	.BLKB	.		;NODE BEING LOOKED UP
NF.LOP:	.BLKB	.		;LOOP NODE NOB
NF.PH4:	.BLKB	.		;Phase 4 node	(remote only)		;000
NF.END:	.BLKB	.		;End node	(exec only)		;000
NF.IBP:	.BLKB	.		;Honor proxy on received CI messages	;016
NF.OBP:	.BLKB	.		;Request proxy on originated CI messages ;016
NF.DEL:	.BLKB	.		;Marked for delete

.MCALL	.MDELET
.MDELET	$NOB
.ENDM	$NOB
.MACRO	$LLB
.DSECT

LL.LNK:	.BLKW			;LINK TO NEXT LLB FOR THIS RECEIVER
LL.RIB:	.BLKW			;BACK PTR TO RIB FOR OWNER OF THIS LINK
LL.LLX:	.BLKW			;ADDRESS OF LOGICAL LINK BLOCK EXTENSION (LLX)
LL.TMR:	.BLKW			;LINK TIMER WORD
LL.PPN:	.BLKW			;PPN for this link, if verification = on ;000
LL.ACQ:	.BLKW			;Link to next LLB on action queue	;015
LL.LFL:	.BLKB			;LOCAL  LINK FLAGS
LL.RFL:	.BLKB			;REMOTE LINK FLAGS
LL.ULA:	.BLKB			;USER  LINK ADDRESS (USER'S REFERENCE NUMBER)
LL.STA:	.BLKB			;LINK STATE (STATE DEFINITIONS BELOW)
LL.TRY:	.BLKB			;TRANSMIT RETRY COUNT
LL.MOD:	.BLKB			;FLOW CONTROL MODIFIERS (SEE BELOW)
LL.LLA:	.BLKW			;LOCAL LINK ADDRESS (NSP'S  REFERENCE NUMBER)
LL.RLA:	.BLKW			;REMOTE LINK ADDRESS 
LL.RMX:	.BLKW			;MAX SIZE OF DATA MESSAGE OUR USER WILL ACCEPT
LL.TMX:	.BLKW			;MAX SIZE OF DATA MESSAGES REMOTE WILL ACCEPT
LL.XTM:				;TIME WHEN MSG XMITTED (FOR DELAY EST.)
LL.CMA:				;ADDRESS OF CI MESSAGE IN CIR STATE
LL.DIR:	.BLKW			;DISCONNECT REASON FOR DI AND DC MSG'S
LL.LLB:	.BLKW			;THREAD THROUGH ALL LLB'S FOR THIS NODE
LL.NOB:	.BLKW			;PTR TO NOB FOR REMOTE NODE

; LINK TIMER (LL.TMR) USAGE NOTES

; THIS BLOCK IS USED FOR VARIOUS TIMERS ASSOCIATED WITH THE LOGICAL LINK.
; CURRENTLY THE FOLLOWING TIMERS ARE IMPLEMENTED:
;
; 1) INCOMING CONNECT TIMER	(SETTABLE, CIR STATE ONLY)
; 2) OUTGOING CONNECT TIMER	(SETTABLE, CIS/CID STATE ONLY)
; 3) Pending action timer	(1-3 seconds, RUN state only)
;
; OTHER TIMERS CAN BE FOUND IN THE LOGICAL LINK EXTENSION BLOCK (LLX).
; THEY ARE:
;
; 4) DATA RETRY TIMER		(N*DELAY, RUN/DIS STATE ONLY)
; 5) INTERRUPT RETRY TIMER	(N*DELAY, RUN STATE ONLY)
; 6) IDLE TIMER			(SETTABLE, RUN STATE ONLY)

; LOCAL LINK FLAGS (LL.LFL) BIT DEFINITIONS

.BSECT

LF.DBL:	.BLKB	.		;DATA   SUBCHANNEL BLOCKED IF SET
LF.LBL:	.BLKB	.		;INT/LS SUBCHANNEL BLOCKED IF SET
LF.PST:	.BLKB	.		;POSTING REQUIRED IF SET
LF.DAK:	.BLKB	.		;Data ACK is needed			;012
LF.LAK:	.BLKB	.		;INT/LS ACK is needed			;012
LF.SBP:	.BLKB	.		;SEND BACKPRESSURE ON/OFF IF SET
LF.EOM:	.BLKB	.		;LAST SEGMENT WAS AN EOM (OR NONE SENT EVER)
LF.BPS:	.BLKB	.		;LOCAL BACKPRESSURE SWITCH (1=FLOW OFF,0=ON)

; REMOTE LINK FLAGS (LL.RFL) BIT DEFINITIONS

.BSECT

	.BLKB	.		;RESERVED
	.BLKB	.		;RESERVED
	.BLKB	.		;RESERVED
	.BLKB	.		;RESERVED
RF.P4P:	.BLKB	.		;Remote node is Phase 4+ node 		;016
RF.PH4:	.BLKB	.		;Remote node is phase 4	or higher node	;016
RF.PH2:	.BLKB	.		;REMOTE NODE IS PHASE 2 NODE
RF.BPS:	.BLKB	.		;REM. BACKPRESSURE SWITCH (1=FLOW OFF,0=ON)

; LINK STATE (LL.STA) DEFINITIONS

.BSECT

	.BLKB	.		;RESERVED
ST$CID:	.BLKB	.		;CI DELIVERED - WAITING FOR CR OR DI
ST$CIS:	.BLKB	.		;CI SENT - WAITING FOR ACK OR REPLY
ST$CIR:	.BLKB	.		;CI RCVD - WAITING FOR USER ACTION
ST$CCS:	.BLKB	.		;CONNECT CONFIRM SENT - WAITING FOR ACK
ST$RUN:	.BLKB	.		;DATA TRANSFER MODE - LINK IS ESTABLISHED
ST$DIP:	.BLKB	.		;DISCONNECT INITIATE PENDING -
				;DI (BY NSP) WILL BE SENT WHEN BUFFERS FREE UP
ST$DIS:	.BLKB	.		;DISC. INITIATE SENT - WAITING FOR ACK

; LINK MODIFIERS (LL.MOD) DEFINITIONS

; THE FLOW CONTROL FLAGS FOR BOTH THE LOCAL AND THE REMOTE USER ARE PACKED
; INTO THIS BYTE.  THE REMOTE FLAGS ARE IN THE SAME PLACE AS IN THE MESSAGE;
; THE LOCAL FLAGS ARE SHIFTED DOWN BY 2 BITS

.BSECT
	.BLKB	2		;
LM.FLO:				;LOCAL FLOW CONTROL OPTION BITS
LM$NON	=	0		;NO FLOW CONTROL
LM$SEG	=	1		;SEGMENT FLOW CONTROL
LM$MSG	=	2		;MESSAGE FLOW CONTROL
;	=	3		;RESERVED

RM.TYP:	.BLKB	11		;LINK TYPE BITS (FROM RECEIVED CI/CC)
;	=	0		;RESERVED
RM$NLL	=	1		;NORMAL LOGICAL LINK
;	=	2		;RESERVED
;	=	3		;RESERVED

RM.FLO:	.BLKB	4		;REMOTE FLOW CONTROL OPTION BITS
RM$NON	=	0*4		;NO FLOW CONTROL
RM$SEG	=	1*4		;SEGMENT FLOW CONTROL
RM$MSG	=	2*4		;MESSAGE FLOW CONTROL
;	=	3*4		;RESERVED

	.BLKB	.		;RESERVED
	.BLKB	.		;RESERVED
	.BLKB	.		;RESERVED
	.BLKB	.		;RESERVED

.MCALL	.MDELET
.MDELET	$LLB
.ENDM	$LLB
.MACRO	$LLX
.DSECT

				;DATA SUB CHANNEL CONTROL INFO
LX.DTI:	.BLKW			;DATA SUBCHANNEL ACK TIMEOUT
LX.DCH:	.BLKW			;Out of order cache list head		;012
LX.DRN:	.BLKW			;RECEIVE SEGMENT NUMBER
LX.DTN:	.BLKW			;TRANSMIT SEGMENT NUMBER
LX.LDR:	.BLKB			;CNT OF DATA SEG'S REQ'D BY US  BUT NOT YET RCVD
LX.RDR:	.BLKB			;CNT OF DATA SEG'S REQ'D BY REM BUT NOT YET SENT
LX.DTM:	.BLKB			;TRANSMIT QUEUE MAXIMUM
LX.DTC:	.BLKB			;TRANSMIT QUEUE COUNT
LX.DTQ:	.BLKW	2		;TRANSMIT QUEUE

				;INT/LS SUBCHANNEL CONTROL INFO
LX.LTI:	.BLKW			;INT/LS SUBCHANNEL ACK TIMEOUT (ALSO FOR IDLE)
LX.LCH:	.BLKW			;Out of order cache list head		;012
LX.LRN:	.BLKW			;RECEIVE SEGMENT NUMBER
LX.LTN:	.BLKW			;TRANSMIT SEGMENT NUMBER
LX.LIR:	.BLKB			;CNT OF INTERRUPTS REQ'D BY US  BUT NOT YET RCVD
LX.RIR:	.BLKB			;CNT OF INTERRUPTS REQ'D BY REM BUT NOT YET SENT
LX.LTM:	.BLKB			;TRANSMIT QUEUE MAXIMUM
LX.LTC:	.BLKB			;TRANSMIT QUEUE COUNT
LX.LTQ:	.BLKW	2		;TRANSMIT QUEUE

.MCALL	.MDELET
.MDELET	$LLX
.ENDM	$LLX
.MACRO	$NCB
.DSECT

NC.DAT:	.BLKW			;DATE LAST ZEROED
NC.TIM:	.BLKW			;TIME LAST ZEROED
NC.BYR:	.BLKW	2		;USER BYTES RECEIVED
NC.BYX:	.BLKW	2		;USER BYTES TRANSMITTED
NC.MSR:	.BLKW	2		;USER MESSAGES (SEGMENTS) RECEIVED
NC.MSX:	.BLKW	2		;USER MESSAGES (SEGMENTS) TRANSMITTED
NC.TBR:	.BLKW	2		;Total bytes received
NC.TBX:	.BLKW	2		;Total bytes transmitted
NC.TMR:	.BLKW	2		;Total blocks received
NC.TMX:	.BLKW	2		;Total blocks transmitted
NC.CIR:	.BLKW			;CONNECTS RECEIVED
NC.CIS:	.BLKW			;CONNECTS SENT
NC.TMO:	.BLKW			;RESPONSE TIMEOUTS
NC.NRR:	.BLKW			;NO RESOURCE RESPONSES RECEIVED
NCBSIZ:				;LENGTH OF COUNTER PORTION OF NCB
	.BLKW	9.		;Reserved
NC.CTI:	.BLKW			;COUNTER TIMER INTERVAL
NCBTSZ:				;Total size of NCB

.ASSUME	.&37 EQ 0		;Must be a small buffer multiple

.MCALL	.MDELET
.MDELET	$NCB
.ENDM	$NCB
.MACRO	$DMCCOU

.DSECT

; The format of the date/time field is:
;
;	Date word:	Julian date + ((Year-1970) * 1000.)
;	Time word:	Time in (Seconds-til-midnight)/2 for the above date

XM$DAC:	.BLKW	1		;RSTS/E Current date
XM$SEC:	.BLKW	1		;(Secs-til-midnight)/2 for current date
XM$DAZ:	.BLKW	1		;RSTS/E Date Ctrs Last Zeroed
XM$SEZ:	.BLKW	1		;(Secs-til-midnight)/2 for above

; The following are the Transport circuit counters.
; They are in the same order as the Transport counters 
; returned with the DMP/DMV counters.

XM$TRC:	.BLKW	2		;Count of transit packets received
XM$TXM:	.BLKW	2		;Count of transit packets sent
XM$LRC:	.BLKW	2		;Count of local NSP packets received
XM$LXM:	.BLKW	2		;Count of local NSP packets sent
XM$TCN:	.BLKW			;Count of transit packet congestion loss
XM$LCN:	.BLKW			;Count of local NSP packet congestion loss
XM$LND:	.BLKW			;Count of number of times line down
XM$IER:	.BLKW			;Count of initialization failures

; The following are the DMC/DMR basetable error counts.

XM$NKR:	.BLKB	1		;Basetable: NAKs received
XM$NSB:	.BLKB	1		;Basetable: NAKs sent - no buffer available
XM$NSH:	.BLKB	1		;Basetable: NAKs sent - Bad header rcvd
XM$NSD:	.BLKB	1		;Basetable: NAKs sent - Bad data BCC
XM$RPS:	.BLKB	1		;Basetable: REPLYs sent (remote reply timeouts)
XM$RPR:	.BLKB	1		;Basetable: REPLYs rec'd (local reply timeouts)

; The following are 2-word integers, LSB in low word.

XM$XBY:	.BLKW	2		;Traffic: #Bytes transmitted
XM$XMS:	.BLKW	2		;Traffic: #Messages transmitted
XM$RBY:	.BLKW	2		;Traffic: #Bytes received
XM$RMS:	.BLKW	2		;Traffic: #Messages received

XM$CSZ=.			;Size of counter block

.MCALL	.MDELET
.MDELET	$DMCCOU
.ENDM	$DMCCOU
.MACRO	$DMPCOU

.DSECT

; The format of the date/time field is:
;
;	Date word:	Julian date + ((Year-1970) * 1000.)
;	Time word:	Time in (Seconds-til-midnight)/2 for the above date

XD$DAC:	.BLKW	1		;RSTS/E Current date
XD$SEC:	.BLKW	1		;(Secs-til-midnight)/2 for current date
XD$DAZ:	.BLKW	1		;RSTS/E Date Ctrs Last Zeroed
XD$SEZ:	.BLKW	1		;(Secs-til-midnight)/2 for above

; The following are the Transport circuit counters.
; They are in the same order as the Transport counters 
; returned with the DMC/DMR counters.

XD$TRC:	.BLKW	2		;Count of transit packets received
XD$TXM:	.BLKW	2		;Count of transit packets sent
XD$LRC:	.BLKW	2		;Count of local NSP packets received
XD$LXM:	.BLKW	2		;Count of local NSP packets sent
XD$TCN:	.BLKW			;Count of transit packet congestion loss
XD$LCN:	.BLKW			;Count of local NSP packet congestion loss
XD$LND:	.BLKW			;Count of number of times line down
XD$IER:	.BLKW			;Count of initialization failures

; The following are 2-word integers, LSB in low word.

XD$XBY:	.BLKW	2		;Traffic: #Bytes transmitted
XD$XMS:	.BLKW	2		;Traffic: #Messages transmitted
XD$RBY:	.BLKW	2		;Traffic: #Bytes received
XD$RMS:	.BLKW	2		;Traffic: #Messages received

; The following are 16-bit and 8-bit counters, as indicated.

XD$LSI:	.BLKW	1		;Selection intervals
XD$DEO:	.BLKB	1		;Total data errors outbound
XD$DOF:	.BLKB	1		;Data errors outbound flags
XD$DEI:	.BLKB	1		;Total data errors inbound
XD$DIF:	.BLKB	1		;Data errors inbound flags

XD$LBE:	.BLKB	1		;Local buffer errors
XD$LBF:	.BLKB	1		;Local buffer error flags
XD$RBE:	.BLKB	1		;Remote buffer errors
XD$RBF:	.BLKB	1		;Remote buffer error flags
XD$STO:	.BLKB	1		;Selection timeouts
XD$STF:	.BLKB	1		;Selection timeout flags

XD$LRT:	.BLKB	1		;Local reply timeouts
XD$RRT:	.BLKB	1		;Remote reply timeouts

XD$CSZ=.			;Size of counter block

.MCALL	.MDELET
.MDELET	$DMPCOU
.ENDM	$DMPCOU
.MACRO	$DMPLCO

.DSECT

; The format of the date/time field is:
;
;	Date word:	Julian date + ((Year-1970) * 1000.)
;	Time word:	Time in (Seconds-til-midnight)/2 for the above date
;
;			(Use labels from $DMPCOU macro above)

	.BLKW	1		;RSTS/E Current date
	.BLKW	1		;(Secs-til-midnight)/2 for current date
	.BLKW	1		;RSTS/E Date Ctrs Last Zeroed
	.BLKW	1		;(Secs-til-midnight)/2 for above

; The following are the device line counters.

XD$LRP:	.BLKB			;Count of remote process errors
XD$LRF:	.BLKB			;Remote process error flags
XD$LLP:	.BLKB			;Count of local process errors
XD$LLF:	.BLKB			;Local process error flags

XD$LHC:	.BLKW			;Data errors inbound (header block check)
XD$MDC:	.BLKW			;Maintenance data errors (MOP not implemented)

XD$LSZ=.			;Size of counter block

.MCALL	.MDELET
.MDELET	$DMPLCO
.ENDM	$DMPLCO
.MACRO	$ETHCOU								;013

.DSECT									;013

; The format of the date/time field is:					;013
;
;	Date word:	Julian date + ((Year-1970) * 1000.)		;013
;	Time word:	Time in (Seconds-til-midnight)/2 for above date	;013

XE$DAC:	.BLKW		;RSTS/E Current date				;013
XE$SEC:	.BLKW		;(Secs-til-midnight)/2 for current date		;013
XE$DAZ:	.BLKW		;RSTS/E Date Ctrs Last Zeroed			;013
XE$SEZ:	.BLKW		;(Secs-til-midnight)/2 for above		;013

; The following are the Transport circuit counters.			;013
; They are in the same order as the Transport counters 			;013
; returned with the DMC/DMR counters and the DMP/DMV counters.		;013
; All Ethernet circuit counter offset def's are prefixed by "XE$".	;013

XE$TRC:	.BLKW	2	;Count of transit packets received		;013
XE$TXM:	.BLKW	2	;Count of transit packets sent			;013
XE$LRC:	.BLKW	2	;Count of local NSP packets received		;013
XE$LXM:	.BLKW	2	;Count of local NSP packets sent		;013
XE$TCN:	.BLKW		;Count of transit packet congestion loss	;013
XE$LCN:	.BLKW		;Count of local NSP packet congestion loss	;013
XE$LND:	.BLKW		;Count of number of times line down		;013
XE$IER:	.BLKW		;Count of initialization failures		;013

; The following are 2-word integers, LSB in low word.			;013

	.BLKW	4	;Date/time since last zeroed from drivers	;017
XE$RBY:	.BLKW	2	;Traffic: #Bytes received			;013
XE$XBY:	.BLKW	2	;Traffic: #Bytes transmitted			;013
XE$RMS:	.BLKW	2	;Traffic: #Messages received			;013
XE$XMS:	.BLKW	2	;Traffic: #Messages transmitted			;013

; Last remaining Ethernet circuit counter is 16-bit width:		;013

XE$EUB:	.BLKW		;User buffer unavailable			;013
XE$CSZ=.		;Size of counter block				;013

.MCALL	.MDELET								;013
.MDELET	$ETHCOU								;013
.ENDM	$ETHCOU								;013
.MACRO	$ETHLCO								;013

.DSECT									;013

; The format of the date/time field is:					;013
;
;	Date word:	Julian date + ((Year-1970) * 1000.)		;013
;	Time word:	Time in (Seconds-til-midnight)/2 for above date	;013
;
;			(Use same labels from $ETHCOU macro above)	;017

	.BLKW		;RSTS/E Current date				;013
	.BLKW		;(Secs-til-midnight)/2 for current date		;013
	.BLKW		;RSTS/E Date Ctrs Last Zeroed			;013
	.BLKW		;(Secs-til-midnight)/2 for above		;013

; The following are the Ethernet device line counters.			;013
; Unfortunately some different line counters are mixed in with the	;013
; circuit counters with the same name.  Therefore, not that ALL of	;013
; the line counters have their own offset definition prefixed by "XE.".	;013

XE.RBY:	.BLKW	2	;Bytes received					;013
XE.XBY:	.BLKW	2	;Bytes sent					;013
XE.MBR:	.BLKW	2	;Multicast bytes received			;013
XE.RMS:	.BLKW	2	;Data blocks received				;013
XE.XMS:	.BLKW	2	;Data blocks sent				;013
XE.BLR:	.BLKW	2	;Multicast blocks received			;013
XE.BID:	.BLKW	2	;Blocks sent, initially deferred		;013
XE.BSC:	.BLKW	2	;Blocks sent, single collision			;013
XE.BMC:	.BLKW	2	;Blocks sent, multiple collisions		;013
XE.ESF:	.BLKW		;Send failure					;013
XE.SFF:	.BLKW		; send failure (flag bits)			;013
XE.EDF:	.BLKW		;Collision detect check failure			;013
XE.ERF:	.BLKW		;Receive failure				;013
XE.RFF:	.BLKW		; receive failure (flag bits)			;013
XE.EUD:	.BLKW		;Unrecognized frame destination			;013
XE.EDO:	.BLKW		;Data overrun					;013
XE.ESB:	.BLKW		;System buffer unavailable			;013
XE.EUB:	.BLKW		;User buffer unavailable			;013
XE.LSZ=.		;Size of line counter block			;013

.MCALL	.MDELET								;013
.MDELET	$ETHLCO								;013
.ENDM	$ETHLCO								;013
.MACRO	$NODCOU

.DSECT

; The format of the date/time field is:
;
;	Date word:	Julian date + ((Year-1970) * 1000.)
;	Time word:	Time in (Seconds-til-midnight)/2 for the above date

NO$DAC:	.BLKW	1		;RSTS/E Current date
NO$SEC:	.BLKW	1		;(Secs-til-midnight)/2 for current date
NO$DAZ:	.BLKW	1		;RSTS/E Date Ctrs Last Zeroed
NO$SEZ:	.BLKW	1		;(Secs-til-midnight)/2 for above
;
; The following counters are from the node counter block.
; (See $NCB macro.)
;
NO$BYR:	.BLKW	2		;User bytes received
NO$BYX:	.BLKW	2		;User bytes transmitted
NO$MSR:	.BLKW	2		;User messages (segments) received
NO$MSX:	.BLKW	2		;User messages (segments) transmitted
NO$TBR:	.BLKW	2		;Total bytes received
NO$TBX:	.BLKW	2		;Total bytes transmitted
NO$TMR:	.BLKW	2		;Total blocks received
NO$TMX:	.BLKW	2		;Total blocks transmitted
NO$CIR:	.BLKW	1		;Connects received
NO$CIS:	.BLKW	1		;Connects sent
NO$TMO:	.BLKW	1		;Response timeouts
NO$NRR:	.BLKW	1		;"No resource" responses received
.ASSUME	.-NO$BYR EQ NCBSIZ-NC.BYR

;
; The following is an NSP counter supplied for the executor node only.
; (See $LLTTAB macro.)
;
NO$LTP:	.BLKW	1		;Highest link count since NSP on.
;
; The following are Transport counters supplied for the executor node only.
; (See $TRNCTR macro.)
;
NO$NNU:	.BLKW	1		;Node unreachable packet loss
NO$NAP:	.BLKB	1		;Aged packet loss (visit count)
NO$NNO:	.BLKB	1		;Node out-of-range packet loss
NO$NOP:	.BLKB	1		;Oversized packet loss
NO$NPF:	.BLKB	1		;Packet format errors
NO$NPR:	.BLKB	1		;Partial routing update loss
NO$NVR:	.BLKB	1		;Verification reject count
;
; The following are RSTS-specific Transport counters, supplied for the
; executor node only.
;
RCHNOD:	.BLKW	1		;Current number of reachable nodes
RCHMAX:	.BLKW	1		;Highest reachable node count since NSP on

NO$CSZ=.			;Size of counter block

.MCALL	.MDELET
.MDELET	$NODCOU
.ENDM	$NODCOU
.MACRO	$NETFQB
.DSECT

	.BLKW			;RESERVED
FQJOB:	.BLKB			;CALLER'S JOB NUMBER TIMES 2
	.BLKB			;RESERVED
FQ$FUN:	.BLKB			;NSP FUNCTION CODE
FQ$ULA:	.BLKB			;USER LINK ADDRESS
FQ$SPC:	;BLKW			;SR$SPC FUNCTION CODE
FQ$LLA:	.BLKW			;LOCAL  LINK ADDRESS
FQ$RLA:	;BLKW			;REMOTE LINK ADDRESS
FQ$QLM:	;BLKW			;ORIGINATING QUEUE LIMIT		;000
FQ$DLF:	.BLKB			;NSP DELAY FACTOR
FQ$DLW:	.BLKB			;NSP DELAY WEIGHT
FQ$NSI:	.BLKW			;NSP INACTIVITY TIMER
FQ$MFL:	.BLKB			;MESSAGE FLAGS (DM)
	.BLKB			;RESERVED

FQ$LMD:	;BLKB			;LOCAL LINK MODIFIERS (CI,CC)
FQ$RSN:	;BLKW			;REJECT/DISCONNECT REASON CODE
FQ$LDR:	.BLKB			;SEGMENT OR MESSAGE REQUEST COUNT (LS) 
FQ$LIR:	.BLKB			;INTERRUPT REQUEST COUNT (LS)

FQ$RMX:	.BLKW			;RECEIVE MAXIMUM (CI,CC)
FQ$TMX:	.BLKW			;TRANSMIT MAXIMUM (CI)

	.BLKW	6		;RESERVED
.ASSUME	. EQ 40

; FQ$MFL BIT DEFINITIONS FOR DATA MESSAGES

.BSECT

DM.BOM:	.BLKB	.		;BEGINNING OF MESSAGE
DM.EOM:	.BLKB	.		;END OF MESSAGE

; FIRQB	DEFINITIONS FOR NON-RESIDENT FUNCTIONS

.DSECT

	.BLKW			;RESERVED
FQJOB:	.BLKB			;CALLER'S JOB NUMBER TIMES 2
	.BLKB			;RESERVED

FQ$EIB:	;BLKW			;ADDRESS OF EVENT INFO BUFFER POINTER
FQ$FUN:	.BLKB			;NSP FUNCTION CODE
FQ$SUB:	.BLKB			;SUBFUNCTION CODE

FQ$VIR:	;BLKW			;VIRTUAL ADDRESS OF PARM BUFFER (DMP DRIVER)
FQ$NOL:	;BLKW			;ADDRESS OF NODE LIST ROOT
FQ$SID:	;BLKB	24.		;SYSTEM ID STRING
FQ$NOD:	;BLKB	6		;NODE NAME
FQ$LLA:	.BLKW			;LOCAL  LINK ADDRESS

FQ$CNT:	;BLKW			;BYTE COUNT OF PARM BUFFER (DMP DRIVER)
FQ$ILA:	;BLKW			;INTERNAL COPY OF ULA FOR CONNECT INIT
FQ$IDL:	;BLKW			;ADDRESS OF UNUSED NODE LIST ROOT
FQ$RLA:	.BLKW			;REMOTE LINK ADDRESS
FQ$RIB:	.BLKW			;ADDRESS OF RIB LIST ROOT

FQ$JDB:	;BLKW			;ADDRESS OF NSP JDB POINTER
FQ$NAD:	;BLKW			;NODE ADDRESS
FQ$MFL:	.BLKB			;MESSAGE FLAGS 
	.BLKB			;RESERVED

FQ$SBF:	;BLKW			;ADDRESS OF SYSTEM-ID BUFFER POINTER
FQ$AKA:	;BLKB	6.		;NODE ALIAS NAME
FQ$TPW:	;BLKB	8.		;TRANSMIT PASSWORD
FQ$RSN:	;BLKW			;ABORT REASON CODE
FQSIZ:	;BLKW			;DMC RECEIVE BUFFER SIZE
FQ$LMX:	.BLKB			;LINK MAXIMUM
FQ$VIS:	.BLKB			;MAX VISITS

FQ$CPV:	;BLKW			;ADDRESS OF CIRCUIT POINTER VECTOR POINTER
FQ$DTQ:	.BLKB			;DATA   TRANSMIT QUEUE MAXIMUM
FQ$LTQ:	.BLKB			;INT/LS TRANSMIT QUEUE MAXIMUM

FQ$LLT:	;BLKW			;ADDRESS OF LOGICAL LINK TABLE POINTER
FQ$SEG:	;BLKW			;MAX TRANSMIT SEG SIZE FOR NON-ADJACENT NODES
FQMODE:	;BLKW			;DMC OPERATIONAL MODE
FQ$LIN:	.BLKB			;NUMBER OF LINES ALLOWED ON AT ANY TIME
FQ$STA:	.BLKB			;NSP STATE

FQ$OLV:	;BLKW			;ADDRESS OF OUTPUT LINE VECTOR POINTER
FQ$DAC:	;BLKW			;DEFAULT ACCOUNT PPN
FQ$HOP:	.BLKB			;MAX HOPS
FQ$RXM:	.BLKB			;RETRANSMIT FACTOR (MAX # TRANSMIT RETRIES)

FQ$CTM:	;BLKW			;CIRCUIT COUNTER TIMER
FQ$MTX:	;BLKW			;ADDRESS OF ROUTING MATRIX POINTER
FQ$RPW:	;BLKB	8.		;RECEIVE PASSWORD
FQ$NMX:	.BLKW			;MAX NODE ADDRESS

FQ$RTC:	;BLKW			;ADDRESS OF ROUTING MESSAGE CCB POINTER
FQ$CST:	;BLKW			;MAX COST
FQDEV:	.BLKW			;DEVICE NAME

FQ$TCT:	;BLKW			;ADDRESS OF TRANSPORT ERROR COUNTERS
FQ$RTM:	;BLKW			;ROUTE TIMER
FQDEVN:	.BLKB			;DEVICE UNIT NUMBER
	.BLKB			;UNIT NUMBER FLAG (255.)

FQ$DLL:	;BLKW			;DLL BUFFER SIZE
FQCLUS:	.BLKB			;DMC BUFFER QUOTA (BYTE)
	.BLKB			;RESERVED

FQ$$DS:	.BLKW			;DISPATCH POINTER FOR NSP'S FIP REQUESTS

.ASSUME	. EQ 40

.MCALL	.MDELET
.MDELET	$NETFQB
.ENDM	$NETFQB
.MACRO	$LLTTAB
.DSECT

LL$BUF:	.BLKW			;CONTORTED LLT BUFFER ADDRESS
LL$LMX:	.BLKB			;MAXIMUM NUMBER OF LINKS ALLOWED
LL$LCT:	.BLKB			;NUMBER OF LINKS CURRENTLY IN USE
LL$MSK:	.BLKW			;LLT FREE SLOT MASK
LL$END:	.BLKW			;LLT END ADDRESS (VIRTUAL)
LL$TMU:	.BLKW			;LOGICAL LINK TABLE MMU ADDRESS (KISAR6)
LL$LST:	.BLKW			;LLT ADDRESS OF LAST LINK ALLOCATED
LL$VIR:	.BLKW			;VIRTUAL ADDRESS OF LLT BUFFER
LL$INC:	.BLKW			;INCREMENT FOR LLT SLOT USAGE COUNTERS
LL$LTP:	.BLKW			;HIGHEST LINK COUNT SINCE NSP ON
LL$SIZ=.			;LENGTH OF TABLE

.MCALL	MDELET
.MCALL	.MDELET
.MDELET	$LLTTAB
.ENDM	$LLTTAB
.MACRO $TRNCTR
.DSECT

TR$NNU:	.BLKW			;NODE UNREACHABLE PACKET LOSS
TR$NAP:	.BLKB			;AGED PACKET LOSS (VISIT COUNT)
TR$NNO:	.BLKB			;NODE OUT-OF-RANGE PACKET LOSS
TR$NOP:	.BLKB			;OVERSIZED PACKET LOSS
TR$NPF:	.BLKB			;PACKET FORMAT ERROR
TR$NPR:	.BLKB			;PARTIAL ROUTING UPDATE LOSS
TR$NVR:	.BLKB			;VERIFICATION REJECT COUNT
TR$LOS:	.BLKW			;"NO BUFFER" INCOMING PACKET LOSS
TR$SIZ:				;LENGTH OF TABLE

.MCALL	.MDELET
.MDELET	$TRNCTR
.ENDM	$TRNCTR
.MACRO	$RSNDEF
.DSECT

DR$USR:	.BLKB			;NO ERROR - USER DISCONNECT OR ABORT
DR$RES:	.BLKB			;RESOURCE ALLOCATION FAILURE
DR$NOD:	.BLKB			;DESTINATION NODE NAME DOES NOT EXIST
DR$NSD:	.BLKB			;NODE SHUTTING DOWN
DR$ODE:	.BLKB			;DESTINATION PROCESS (OBJECT) DOESN'T EXIST
DR$NAM:	.BLKB			;INVALID DSTNAME OR SRCNAME FIELD
DR$MQF:	.BLKB			;DESTINATION PROCESS TOO BUSY
DR$ERR:	.BLKB			;UNSPECIFIED ERROR CONDITION (E.G., PROTOCOL)
DR$3RD:	.BLKB			;THIRD PARTY ABORTED LOGICAL LINK
DR$UAB:	.BLKB			;USER LINK ABORT
	.BLKB	22.		;RESERVED
DR$TCN:	.BLKB			;TOO MANY CONNECTS TO NODE (SESSION CTL)
DR$TCP:	.BLKB			;TOO MANY CONNECTS TO DESTINATION PROCESS
DR$ACS:	.BLKB			;ACCESS NOT PERMITTED
	.BLKB			;RESERVED
DR$ACT:	.BLKB			;INVALID ACCOUNTING INFORMATION 
	.BLKB			;RESERVED
DR$ABT:	.BLKB			;USER ABORTED, TIMED OUT, OR CANCELLED LINK
DR$PTH:	.BLKB			;NODE UNREACHABLE
	.BLKB			;RESERVED
DR$NCL:	.BLKB			;NO CURRENT LINK (DSTADDR NOT RECOGNIZED)
DR$CDI:	.BLKB			;CONFIRMATION OF DISCONNECT INITIATE
DR$IMG:	.BLKB	11.		;IMAGE FIELD (IN CI) TOO LONG		;016
DR$SVC:	.BLKB			;Cryptographic service mismatch		;016

.MCALL	.MDELET
.MDELET	$RSNDEF
.ENDM	$RSNDEF
.MACRO	$CDBDEF
.DSECT

C.NODE:	.BLKB	6.		;REMOTE NODE NAME (BLANK FILL TO 6 CHAR.)

C.RNAM:				;REMOTE PROCESS NAME FIELD
C.RFMT:	.BLKB			;REMOTE DESCRIPTOR FORMAT IDENTIFIER
C.ROBJ:	.BLKB			;REMOTE OBJECT TYPE
C.CFMT:	.BLKB			;Connect block format			;000
C.RLEN:	.BLKB			;LENGTH OF REMOTE DESCRIPTOR FIELD (FMT 1,2)
C.RDSC:	.BLKB	16.		;REMOTE DESCRIPTOR (BINARY FIELD)  (FMT 1)

C.LNAM:				;LOCAL PROCESS NAME FIELD
C.LFMT:	.BLKB			;LOCAL DESCRIPTOR FORMAT IDENTIFIER
C.LOBJ:	.BLKB			;LOCAL OBJECT TYPE
	.BLKB			;UNUSED BYTE
C.LLEN:	.BLKB			;LENGTH OF LOCAL DESCRIPTOR FIELD (FMT 1,2)
C.LDSC:	.BLKB	16.		;LOCAL DESCRIPTOR (BINARY FIELD)   (FMT 1)

C.ACTL:				;ACCESS CONTROL FIELDS (format 0)	;000
	.BLKB			;RESERVED
C.QLEN:	.BLKB			;LENGTH OF REQUESTOR ID FIELD
C.RQID:	.BLKB	16.		;REQUESTOR ID (BINARY FIELD)

	.BLKB			;RESERVED
C.PLEN:	.BLKB			;LENGTH OF PASSWORD FIELD
C.PSWD:	.BLKB	8.		;PASSWORD  (BINARY FIELD)

	.BLKB			;RESERVED
C.ALEN:	.BLKB			;LENGTH OF ACCOUNTING FIELD
C.ACNT:	.BLKB	16.		;ACCOUNTING (BINARY FIELD)

C.ADDR:	.BLKW			;NODE ADDRESS OF REMOTE NODE

	.BLKB	26.		;CURRENTLY NOT USED

C.DATA:	.BLKB	MAXOPD		;OFFSET TO OPTIONAL DATA IN SCB
.EVEN

C.SIZE:				;SIZE OF CONNECT DATA BLOCK

; Alternate access control layout (format 1)				;000

.DSECT	C.ACTL								;000
	.BLKB			;RESERVED
C.QLEN:	.BLKB			;LENGTH OF REQUESTOR ID FIELD
C.RQID:	.BLKB	40.		;REQUESTOR ID (BINARY FIELD)		;000

	.BLKB			;RESERVED
C2.PLN:	.BLKB			;LENGTH OF PASSWORD FIELD		;000
C2.PWD:	.BLKB	40.		;PASSWORD  (BINARY FIELD)		;000

	.BLKB			;RESERVED
C2.ALN:	.BLKB			;LENGTH OF ACCOUNTING FIELD		;000
C2.ACT:	.BLKB	40.		;ACCOUNTING (BINARY FIELD)		;000

C2.DAT:	.BLKB	MAXOPD		;OFFSET TO OPTIONAL DATA IN SCB		;000
.EVEN

C2.SIZ:				;SIZE OF CONNECT DATA BLOCK		;000

.DSECT	C.RLEN			;FORMAT 2 ONLY
C.RLEN:	.BLKB			;LENGTH OF REMOTE DESCRIPTOR (UP TO 12)
C.RDSC:	.BLKB	12.		;REMOTE DESCRIPTOR
C.RGRP:	.BLKB	2		;REMOTE GROUP CODE
C.RUSR:	.BLKB	2		;REMOTE USER CODE

.DSECT	C.LLEN			;FORMAT 2 ONLY
C.LLEN:	.BLKB			;LENGTH OF LOCAL DESCRIPTOR (UP TO 12)
C.LDSC:	.BLKB	12.		;LOCAL DESCRIPTOR
C.LGRP:	.BLKB	2		;LOCAL GROUP CODE
C.LUSR:	.BLKB	2		;LOCAL USER CODE

.MCALL	.MDELET
.MDELET	$CDBDEF
.ENDM	$CDBDEF
.MACRO	$OBJTYP
.DSECT

			; FORMAT	PROCESS TYPE
O.USER:	.BLKB		;   1 2		 0 GENERAL TASK, USER PROCESS
O.FAL2:	.BLKB		; 0 1		 1 FILE ACCESS (DAP 1.0)	;000
O.URS:	.BLKB		; 0 1		 2 UNIT RECORD SERVICES
O.ATS:	.BLKB		; 0 1		 3 APPLICATION TERMINAL SERVICES
O.CTS:	.BLKB		; 0 1		 4 COMMAND TERMINAL SERVICES
O.RSX1:	.BLKB		; 0		 5 RSX task control, V1		;000
O.OPSER:.BLKB		; 0		 6 OPERATOR SERVICES INTERFACE
O.NRM:	.BLKB		; 0		 7 NODE RESOURCE MANAGER
O.3270:	.BLKB		; 0		 8 IBM 3270-BISYNC GATEWAY
O.2780:	.BLKB		; 0		 9 IBM 2780-BISYNC GATEWAY
O.3790:	.BLKB		; 0		10 IBM 3790-SDLC GATEWAY
O.TAPP:	.BLKB		;   1 2		11 TPS APPLICATION
O.RTDI:	.BLKB		;   1 2		12 RT-11 DIBOL APPLICATION
O.20TH:	.BLKB		; 0 1		13 TOPS-20 TERMINAL HANDLER
O.20RS:	.BLKB		; 0 1		14 TOPS-20 REMOTE SPOOLER
O.MTCL:	.BLKB		; 0		15 RSX-11M TASK CONTROL
O.LSN:	.BLKB		; 0		16 TLK LISTENER UTILITY
O.FAL:	.BLKB		; 0 1		17 FILE ACCESS (DAP 3.0/4.0)
O.HLD:	.BLKB		; 0		18 RSX-11M REMOTE TASK LOADER (HLD)
O.NML:	.BLKB		; 0		19 PHASE III NICE PROCESS (NML)
O.CPY:	.BLKB		; 0 1		20 RSTS MEDIA XFER PROGRAM (NETCPY)
	.BLKB		; 0		21 Obsolete terminal handler	;000
O.DMLS:	.BLKB		; 0		22 DECMAIL LISTENER
O.RMT:	.BLKB		; 0		23 NETWORK TERMINAL HANDLER V1	;000
O.CONC:	.BLKB		; 0		24 TERMINAL CONCENTRATOR HANDLER
O.MIR:	.BLKB		; 0		25 NET MANAGEMENT LOOPBACK MIRROR
O.EVT:	.BLKB		; 0		26 NET MANAGEMENT EVENT RECEIVER
O.MAIL:	.BLKB		; 0		27 KAWELL-MAIL LISTENER
O.FTS:	.BLKB		; 0		28 FILE TRANSFER SPOOLER
O.PHON:	.BLKB		; 0		29 VMS PHONE UTILITY
O.DDMF:	.BLKB		; 0		30 DISTRIBUTED DATA MANAGEMENT
O.25GW:	.BLKB		; 0		31 X.25 GATEWAY MODULE
O.UETP:	.BLKB		; 0		32 UETP (Provisional)		;000
O.VMSM:	.BLKB		; 0		33 VAX/VMS Mail Utility (Provisional) ;000
O.X29T:	.BLKB		; 0		34 X.29 Terminal Servier (Provisional) ;000
O.RDBS:	.BLKB		; 0		35 Relational Database Server	;000
O.X25G:	.BLKB		; 0		36 X.25 Gateway Access		;000
O.SNAG:	.BLKB		; 0		37 SNA Gateway Access		;000
O.SNAR:	.BLKB		; 0		38 SNA RJE Utility		;000
O.SNGI:	.BLKB		; 0		39 SNA Gateway Initialization Server ;000
O.MTS:	.BLKB		; 0		40 Message Transport System Server ;000
O.ELF:	.BLKB		; 0		41 ELF (Provisional)		;000
O.CTRM:	.BLKB		; 0		42 CTERM/FOUND V2		;000
O.VTEX:	.BLKB		; 0		43 Videotex Server		;000
O.DNST:	.BLKB		; 0		44 DNS Transaction Agent	;000
O.DNSU:	.BLKB		; 0		45 Update Listener		;000
O.SNAH:	.BLKB		; 0		46 DHCF - SNA Distributed Host	;000
	.BLKB	77-.	; 0 1 2		   RESERVED FOR DECNET USE
O.DTR:	.BLKB		; 0 1		63 DECNET NSP TEST RECEIVER
	.BLKB	200-.	; 0 1 2		   RESERVED FOR DECNET CONTROL
	.BLKB	400-.	; 0 1 2		   RESERVED FOR CUSTOMER EXTENSIONS

.MCALL	.MDELET
.MDELET	$OBJTYP
.ENDM	$OBJTYP
.MACRO	$NSPSIZ
; SIZE OF MESSAGE FIELDS FOR DECNET/E IMPLEMENTATION OF NSP 4.0

R.RTHD	=:	21.		;Size of long format route header	;007
EP.SIZ	=:	16.		;Size of ethernet DLL header		;007
D.HDRL	=:	16.		;Size of DDCMP header			;026
F.ROVH	=	R.RTHD+EP.SIZ	;Routing layer overhead			;009
.ASSUME	D.HDRL	LE	EP.SIZ						;009
F.MFLG	=	1		;MESSAGE FLAGS		(MSGFLG FIELD)
F.DADR	=	2		;DESTINATION ADDRESS	(DESTADDR FIELD)
F.SADR	=	2		;SOURCE ADDRESS		(SRCADDR FIELD)
F.LSER	=	1		;LINK SERVICES		(SERVICES FIELD)
F.INFO	=	1		;LINK INFO		(INFO FIELD)
F.DSEG	=	2		;MAX SIZE OF DATA FLD	(SEGSIZE FIELD)
F.DNAM	=	7		;DESTINATION NAME	(FMT,OBJ,DSCLEN,P,PN)
F.SNAM	=	7		;SOURCE NAME		(FMT,OBJ,DSCLEN,P,PN)
F.MENU	=	1		;MENU			(MENU FIELD)
F.LQID	=	1		;REQUESTOR ID		(RQSTRID LEN BYTE)
F.LPSW	=	1		;PASSWORD		(PASSWRD LEN BYTE)
F.LACT	=	1		;ACCOUNTING		(ACCOUNT LEN BYTE)
F.LACS	=	F.LQID+F.LPSW+F.LACT  ;			(ACCESS CONTROL FLAGS)
F.ANUM	=	2		;ACKNOWLEDGEMENT NUMBER	(ACKNUM FIELD)
F.XNUM	=	2		;Cross-channel ack num	(ACKNUM FIELD)	;000
F.SNUM	=	2		;SEGMENT NUMBER		(SEGNUM FIELD)
F.LSFL	=	1		;LINK SERVICE FLAGS	(LSFLAGS FIELD)
F.FCVL	=	1		;FLOW CONTROL VALUE	(FCVAL FIELD)
F.RESN	=	1		;DISCONNECT REASON	(REASON FIELD)

DTAOVH	=	F.MFLG+F.DADR+F.SADR+F.ANUM+F.XNUM+F.SNUM
				;Data message overhead			;012

MAXOPD	=:	16.		;MAX NUMBER OF OPTIONAL DATA BYTES
				;IN CI,CC, AND DI MESSAGES

.MCALL	.MDELET
.MDELET	$NSPSIZ
.ENDM	$NSPSIZ
.MACRO	$TRNSIZ
F.FLAG	=:	1		;Flag byte
F.SRCN	=:	2		;Source node in short format
F.THDR	=:	F.FLAG+F.SRCN	;Short message control header
F.TINF	=:	1		;TIINFO				(NODE INIT)
F.BSIZ	=:	2		;BUFFER SIZE			(NODE INIT)
F.TVER	=:	3		;TRANSPORT VERSION NUMBER	(NODE INIT)
F.HTMR	=:	2		;Hello timer			(Node init) ;000
F.SEED	=:	1+0		;IMAGE FIELD FOR VERIFY SEED	(NODE INIT)
F.PASS	=:	1+8.		;IMAGE FIELD FOR PASSWORD	(VERIFY)
F.TEST	=:	1		;LENGTH BYTE OF TEST DATA IMAGE	(HELLO/TEST)
F.CHEK	=:	2		;CHECKSUM			(ROUTING MSG)
F.ID	=:	6		;System ID field in ethernet messages	;007
F.IINF	=:	1		;IINFO field				;007
F.PRI	=:	1		;Router's priority			;007
F.AREA	=:	1		;Reserved AREA field			;007
F.MPD	=:	1		;Reserved MPD field			;007
F.ELEN	=:	1		;Length of logical ethernet table	;007
F.NAME	=:	7		;Reserved logical ethernet name		;007
F.ELIS	=:	1		;Length of router/state pair list	;007
F.ESED	=:	8.		;Ethernet verification seed		;007
F.MPAD	=:	7.		;Maximum padding if blocksize unknown	;018

; Routing fixed message overhead by message type

P4IOVH	=	F.THDR+F.TINF+F.BSIZ+F.TVER+F.HTMR+F.SEED
P3IOVH	=	F.THDR+F.TINF+F.BSIZ+F.TVER+F.SEED
P3VOVH	=	F.THDR+F.PASS
HIOVH	=	F.THDR+F.TEST
RTMOVH	=	F.THDR+F.CHEK
ERHOVH	=	F.FLAG+F.TVER+F.ID+F.IINF+F.BSIZ+F.PRI+F.AREA+F.HTMR+F.MPD+F.ELEN+F.NAME+F.ELIS
EEHOVH	=	F.FLAG+F.TVER+F.ID+F.IINF+F.BSIZ+F.AREA+F.ESED+F.ID+F.HTMR+F.MPD+F.TEST

.MCALL	.MDELET
.MDELET	$TRNSIZ
.ENDM	$TRNSIZ
.MACRO	$DBSDF

; SOME USEFUL CONSTANTS

.DSECT	-1			;Define the UU.SWP subfunctions

SW.LST:	.BLKB			;UU.SWP List Swapfile function
SW.REM:	.BLKB			;UU.SWP Remove Swapfile function
SW.ADD:	.BLKB			;UU.SWP Add Swapfile function

.DSECT	0			;Define the System File file numbers

	.BLKB			;Swapping file - slot #0
	.BLKB			;Swapping file - slot #1
	.BLKB			;Swapping file - slot #2
	.BLKB			;Swapping file - slot #3
	.BLKB			;Monitor Overlay file
	.BLKB			;Monitor Error file
SY.NSP:	.BLKB			;NSP Volatile Database

; NCP/DBS FILENAME DESCRIPTOR BLOCK (FDB) OFFSETS

.DSECT				;DEFINE OFFSETS INTO THE FDB

FB.CHN:	.BLKW			;Channel number for OPEN
FB.CSZ:	.BLKW			;File size for CREATE
FB.NAM:	.BLKW			;Filename pointer in FDB
FB.NML:	.BLKW			;Filename string length
FB.FUN:	.BLKW			;FIP function value
FB.MOD:	.BLKW			;Open mode for file
FB.CRB:	.BLKW			;R/W word for Current Resident Block
FB.ROM:	.BLKW			;0 if write privs obtained, else non-zero
FB.ERR:	.BLKB			;Returned error code if operatn error (FIRQB)
FB.ERU:	.BLKB			;Routine-unique operation error code
FDBSIZ:				;Size of FDB in bytes

.DSECT				;ERROR CODES

; FDB ERROR CODES (HIBYTE OF FB.ERR) (REFER ONLY TO CHANNEL OWNED BY FDB)

FE.RDE:	.BLKB			;Database Read Error
FE.CRE:	.BLKB			;Error during file create
FE.ZER:	.BLKB			;Error during Database Zero operation
FE.WRT:	.BLKB			;Error during File Write
FE.BOS:	.BLKB			;Bad Database File Size on Open
FE.OPE:	.BLKB			;Error during file Open
FE.NOW:	.BLKB			;No Write privs on open (not an error, but...)
FE.POW:	.BLKB			;Error during Populate of Object Database
FE.CLE:	.BLKB			;Error during Channel close

; DATABASE REQUEST UNIQUE OPERATION ERROR CODES (HIBYTE OF DB.ERR)

DE.BRT:	.BLKB			;Bad Record Type
DE.BFC:	.BLKB			;Bad Function Code
DE.BRN:	.BLKB			;Bad Record Number
DE.BBS:	.BLKB			;Bad User Buffer Size
DE.RDE:	.BLKB			;Error during Read of database file
DE.WRE:	.BLKB			;Error during Write to database file

; DATABASE REQUEST BLOCK (DBRQ)

.DSECT				;DEFINE OFFSETS INTO DATABASE REQUEST BLOCK

DB.FDB:	.BLKW			;Pointer to FDB
DB.TYP:	.BLKB			;Record Type code
DB.FUN:	.BLKB			;1 = Read, 2 = Write (to database)
DB.REC:	.BLKW			;Record number
DB.ADR:	.BLKW			;User's buffer address
DB.BSZ:	.BLKW			;Size of user's buffer in bytes
DB.RSZ:	.BLKW			;Length of record returned (for READS)
DB.ERR:	.BLKB			;Returned error code if operatn error (FIRQB)
DB.ERU:	.BLKB			;Routine-unique operation error code
DBRQSZ:				;Size of Database Request Block in bytes

.DSECT				;PERMANENT DATABASE FUNCTION CODES

	.BLKB			;Function code 0 not defined
PFU.RD:	.BLKB			;Read (from database) function code
PFU.WR:	.BLKB			;Write (to database) function code

.DSECT			;CHANNEL ASSIGNMENTS (CHANNEL NUMBER * 2)

	.BLKW		;Channel #0 reserved for Job Console
TOCNLW:	.BLKW		;Channel #1 reserved for text output file
VDBCNL:	.BLKW		;Channel for R/O access to Volatile Database
PDBCNL:	.BLKW		;Channel for R/O access to Permanent Database
VDBCNW:	.BLKW		;Channel for R/W access to Volatile Database
PDBCNW:	.BLKW		;Channel for R/W access to Permanent Database
NDBCNL:	.BLKW		;Channel for R/W access to NEW database		;002

.MCALL	.MDELET
.MDELET	$DBSDF
.ENDM	$DBSDF
.MACRO	CKPWR2	RECSIZ
 .IIF NE <<RECSIZ&<-RECSIZ>>-RECSIZ> .ERROR ;RECSIZ NOT A POWER OF 2
.ENDM	CKPWR2
.MACRO	$CTLREC
.DSECT	0			;NCP DATABASE CONTROL RECORD DEFINITIONS
NC.VER:	.BLKB			;Version of database			;004
NC.ECO:	.BLKB			;ECO number of database			;004
NC.EXE:	.BLKW			;Database Executor record start block	;011
NC.COF:	.BLKW			;"Max circuit" offset into Exec record	;011
	.BLKW	251.		;Reserved for future use		;011
NC.IDE:	.BLKW			;Must contain a "NET" in RAD-50		;004
NC.XOR:	.BLKW			;XOR of the block contents		;004
NC.SIZ:				;LENGTH OF ENTRY

NC$VER	=:	4		;Current database version number	;004
NC$ECO	=:	3		;Current database ECO number		;029

.ASSUME	NC.SIZ EQ 512.		;NCP DATABASE CONTROL RECORD MUST BE 1 BLOCK

	CKPWR2	NC.SIZ		;MAKE SURE THIS RECORD'S SIZE IS A POWER OF 2

.MCALL	.MDELET
.MDELET	$CTLREC
.ENDM	$CTLREC
.MACRO	$OBJREC
.MCALL	.DSECT,.BSECT,.ASSUME,CKPWR2
.DSECT

O.FLAG:	.BLKB			;FLAG BYTE
	.BLKB			;RESERVED
	.BLKW	2		;RESERVED
FQPPN:	.BLKW			;PPN OF FILE TO RUN
FQNAM1:	.BLKW	2		;FILE NAME TO RUN
FQEXT:	.BLKW			;EXTENSION
O.ONUM:	.BLKB			;OBJECT NUMBER
O.ONAM:	.BLKB	6		;OBJECT NAME
O.P2:	.BLKB	2		;SECOND PARAMETER WORD
	.BLKB			;RESERVED
FQDEV:	.BLKW			;DEVICE NAME OF FILE TO RUN
FQDEVN:	.BLKB	2		;UNIT NUMBER OF FILE TO RUN
	.BLKW			;Part 3 of device if logical name
O.P1:	.BLKW			;FIRST PARAMETER WORD
O.SIZE:				;LENGTH OF ENTRY

.ASSUME	O.SIZE EQ 40		;MUST FIT IN A SMALL BUFFER

	CKPWR2	O.SIZE		;MAKE SURE THIS RECORD'S SIZE IS A POWER OF 2

; OBJECT FLAG BYTE DEFINITIONS

.BSECT

OF.REA:	.BLKB	5		;REAL FLAG (0 IF ENTRY UNUSED)		;000
OF.VAL:	.BLKB	2		;Validation mask			;000
OF.LOG:	.BLKB	.		;Device name is a logical name		;000
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO

; Values for validation mask field of object flags			;000

.DSECT									;000

OV$PRO:	.BLKB	2		;Program does validation (old style)	;000
OV$OFF:	.BLKB	2		;No validation				;000
OV$ON:	.BLKB	2		;Monitor does validation		;000

.MCALL	.MDELET
.MDELET	$OBJREC
.ENDM	$OBJREC
.MACRO	$NOBREC
.MCALL	CKPWR2
.DSECT

ND.FLG:	.BLKB			;FLAG BYTE
	.BLKB			;RESERVED, MUST BE ZERO
ND.NAM:	.BLKB	6		;NODE NAME, ZERO-FILLED
;ND.AKA: .BLKB	6		;alias name, zero-filled		;005
	.BLKB	6		;RESERVED				;005
				;PASSWORDS MUST BE IN SAME ORDER AS EXE RECORD
ND.SPW:				;START OF PASSWORDS
ND.XPO:	.BLKB	8.		;TRANSMIT ORIGINATE PASSWORD
ND.RPO:	.BLKB	8.		;RECEIVE ORIGINATE PASSWORD
ND.XPA:	.BLKB	8.		;TRANSMIT ANSWER PASSWORD
ND.RPA:	.BLKB	8.		;RECEIVE ANSWER PASSWORD
ND.EPW:				;END OF PASSWORDS
ND.CTM:	.BLKW	1		;COUNTER TIMER
ND.NUM:	.BLKW			;Node number
	.BLKW	7		;RESERVED, MUST BE ZERO
ND.SIZ:				;SIZE OF ENTRY

	CKPWR2	ND.SIZ		;MAKE SURE THIS RECORD'S SIZE IS A POWER OF 2

; NODE FLAGS

.BSECT

NF.REA:	.BLKB	.		;REAL FLAG (0 IF ENTRY IS UNUSED)
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
NF.IBP:	.BLKB	.		;Honor proxy on inbound connections	;016
NF.OBP:	.BLKB	.		;Request proxy on outbound connections	;016
NF.NUS:	.BLKB	.		;Entry has never been used (NF.REA clear) ;012

NDF.RS	=	^C<NF.IBP!NF.OBP> ;Only proxy bits are carried to NOB	;016

.MCALL	.MDELET
.MDELET	$NOBREC
.ENDM	$NOBREC
.MACRO	$LINREC
.MCALL	.DSECT,.BSECT
.DSECT	0			;LINE DATABASE RECORD DEFINITIONS
;
; NOTE:	LINE PARAMETER VALUES ARE KEPT IN THE RECORD FOR
;	CIRCUIT 0 ON A MULTIPOINT LINE.  WHEN THE USER DEFINES
;	THE LINE PARAMETERS, THE FLAG 'L1.LIN' WILL BE SET IN
;	THE RECORD FOR THAT LINE (CIRCUIT 0); THE FLAG 'L1.REA' AND
;	DEFAULT PARAMETER VALUES WILL BE SET FOR CIRCUIT 0.
;
;	WHEN A CIRCUIT STATE IS SET TO 'ON', THE LINE PARAMETERS ARE 
;	OBTAINED FROM THE RECORD FOR CIRCUIT 0, AND THE CIRCUIT PARAMETERS
;	ARE OBTAINED FROM THE APPROPRIATE CIRCUIT RECORD.
;	A CIRCUIT CANNOT BE SET TO 'ON' BEFORE LINE PARAMETERS ARE
;	DEFINED; AT LEAST BUFFER QUOTA MUST BE DEFINED FOR THE LINE.
;
LI.FLG:	.BLKW			;GENERAL PURPOSE FLAG WORD
LI.DEV:	.BLKW			;DEVICE NAME (ASCII: XM,XD)
LI.DDM:	.BLKW			;DEVICE NAME (RAD50: DMC,DMP,DMV)
LI.DCN:	.BLKB			;CONTROLLER NUMBER
LI.DCU:	.BLKB			;CONTROLLER LETTERCODE
LI.TRB:	.BLKB			;MULTIPOINT TRIB NUMBER
LI.UNI:	.BLKB			;RSTS UNIT NUMBER

;
; LINE PARAMETERS
;
LI.LCT:	.BLKW			;LINE COUNTER TIMER (NOT YET IMPLEMENTED)
LI.RET:	.BLKW			;RETRANSMIT TIMER (really a circuit param,
				;		needs netmgt spec change)
LI.DDT:	.BLKW			;DEAD TIMER
LI.DLT:	.BLKW			;DELAY TIMER
LI.BFQ:	.BLKW			;RECEIVE BUFFER QUOTA
;
; CIRCUIT PARAMETERS
;
LI.CTM:	.BLKW			;CIRCUIT COUNTER TIMER (NOT YET IMPLEMENTED)
LI.STA:	.BLKB			;STATE FLAGS
LI.TAD:	.BLKB			;TRIBUTARY ADDRESS
LI.CST:	.BLKW			;COST
LI.HTM:	.BLKW			;HELLO TIMER
LI.RBF:	.BLKW			;MAXIMUM RECEIVE BUFFERS (NOT YET IMPLEMENTED)

;
; THE FOLLOWING ARE UNDOCUMENTED MULTIPOINT PARAMETERS
;
; LINE PARAMETERS
;
LI.STT:	.BLKW			;STREAM TIMER
LI.SCT:	.BLKW			;SCHEDULING TIMER
;
; CIRCUIT PARAMETERS
;
;	The order of the following definitions must be maintained;
;	byte parameter changes are written to the device as fullwords,
;	and rely on finding the proper value for the parameter in the
;	other half of the fullword.
;
LI.XMT:	.BLKW			;TRANSMIT TIMER		;tss bytes 60-61
LI.BSA:	.BLKB			;ACTIVE BASE		;tss byte  62
LI.INA:	.BLKB			;ACTIVE INCREMENT	;tss byte  63
LI.BSI:	.BLKB			;INACTIVE BASE		;tss byte  64
LI.INI:	.BLKB			;INACTIVE INCREMENT	;tss byte  65
LI.BSD:	.BLKB			;DYING BASE		;tss byte  66
LI.IND:	.BLKB			;DYING INCREMENT	;tss byte  67
LI.TH3:	.BLKB			;INACTIVE THRESHOLD	;tss byte  70
LI.TH2:	.BLKB			;DYING THRESHOLD	;tss byte  71
LI.TH1:	.BLKB			;DEAD THRESHOLD		;tss byte  72
LI.MXB:	.BLKB	;;maximum blocks belongs here   (not implemented)  73
	.BLKW	;;retransmit timer belongs here		;tss bytes 74-75
LI.BBT:	.BLKW			;BABBLE TIMER		;tss bytes 76-77
;
;	End of ordered set of parameters
;

;
; DO NOT add any more line/circuit record parameters.  If this record	;008
; gets any larger, the starting block numbers (SB$-*-) will change.	;008
;
LI.PRO:	.BLKB	2		;Protocol type in hex (Ethernet)	;008
	.EVEN
LI.QLM:	.BLKW			;Originating queue limit		;000
LI.RTM:	.BLKW			;Recall timer				;000
LI.MRO:	.BLKB			;Maximum Routers (Ethernet)		;005
LI.RPR:	.BLKB			;Router Priority (Ethernet)		;005
LI.RQT:	.BLKB			;Routing queue threshold		;016
	.BLKB	<110-<.-LI.FLG>> ;Reserved for future expansion		;016
LI.SIZ:				;SIZE OF ENTRY

.ASSUME	LI.SIZ	EQ 110		;Make sure it's what we expect		;010

; LI.FLG FLAGS
;
; NOTE:	L1.REA, L1.OWN, AND L1.RES ARE CIRCUIT PARAMETERS;
;	THE REST ARE LINE PARAMETERS.
;
.BSECT

L1.REA:	.BLKB	.		;CIRCUIT REAL FLAG (0 IF ENTRY IS UNUSED)
L1.LIN:	.BLKB	.		;LINE REAL FLAG: 1 = LINE IS DEFINED, 0 = NOT
L1.LOP:	.BLKB	.		;0 = NORMAL CTLR MODE, 1 = LOOPBACK MODE
L1.HDX:	.BLKB	.		;0 = FULL DUPLEX OPERATION, 1 = HALF DUPLEX
L1.ANS:	.BLKB	.		;0 = ORIGINATE MODE, 1 = ANSWER-ONLY MODE
L1.VER:	.BLKB	.		;0 = NO VERIFICATION USED, 1 = VERIFICATION
L1.OWN:	.BLKB	.		;0 = NOT OWNED BY DECNET, 1 = OWNER EXE
L1.RES:	.BLKB	.		;0 = NOT RESTARTABLE, 1 = AUTORESTART

L1.TYD:	.BLKB	.		;1 = DMC-COMPATIBLE MODE
L1.TYP:	.BLKB	.		;1 = DDCMP V4.0+ POINT-TO-POINT
L1.TYC:	.BLKB	.		;1 = CONTROL STATION (MASTER)
L1.TYT:	.BLKB	.		;1 = TRIBUTARY STATION (SLAVE)
	.BLKB	.		;RESERVED, MUST BE ZERO
L1.MPC:	.BLKB	.		;LINE/CIRCUIT IS PART OF A MULTIPOINT DEVICE
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO

; LI.STA FLAGS

.BSECT				;LINE STATE FLAG BIT DEFINITIONS

LS.SOK:	.BLKB	.		;0 = SERVICE NOT ALLOWED, 1 = SERVICE OK
LS.SER:	.BLKB	.		;0 = LINE IN NORMAL STATE, 1 = SERVICE STATE
LS.SON:	.BLKB	.		;0 = LINE OFF AT STARTUP, 1 = ON (OR SERVICE)
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO

.MCALL	.MDELET
.MDELET	$LINREC
.ENDM	$LINREC
.MACRO	$EXEREC
.MCALL	.DSECT,.BSECT,.ASSUME,CKPWR2

.DSECT	0			;First Executor Node record definitions

EX$FLG:	.BLKW			;Primary (First) Flag word
EX$FL1:	.BLKW			;Secondary flag word
EX$DLF:	.BLKB			;Delay Factor
EX$DLW:	.BLKB			;Delay Weight
EX$NSI:	.BLKW			;NSP Inactivity Timer
EX$NAD:	.BLKW			;Local Node Address

EX$LMX:	.BLKB			;Max Links
EX$VIS:	.BLKB			;Max Visits
EX$DTQ:	.BLKB			;Data Queue Limit
EX$LTQ:	.BLKB			;Link-svc/Int message queue limit

EX$LIN:	.BLKB			;Max Lines (1 => non-routing node)
EX$STA:	.BLKB			;Desired NSP State (ON, OFF or RES NSP code)
EX$HOP:	.BLKB			;Max Hops
EX$PTH:	.BLKB			;Maximum equal cost paths to split between ;016
EX$NMX:	.BLKW			;Maximum Node Address
EX$CST:	.BLKW			;Max Cost
EX$RTM:	.BLKW			;Route Timer

EX$DLL:	.BLKW			;Buffer Size (Data Link Layer Segment Size)
EX$DFA:	.BLKW			;Default Account (PPN after FSS)
EX$DAC:	.BLKB	16.		;Default Account (ASCII, userid as input)
EX$SID:	.BLKB	32.		;System-ID string (ASCII)
EX$MBU:	.BLKW			;Maximum Buffers
EX$ITM:	.BLKW			;Incoming Timer
EX$OTM:	.BLKW			;Outgoing Timer
EX$CTM:	.BLKW			;Counter Timer
EX$NAM:	.BLKW	3		;Local Node Name
;EX$AKA: .BLKW	3		;Local Node Alias
	.BLKW	1		;RESERVED				;024
EX$MXS:	.BLKW			; Max Node Number Specified by User	;024
EX$NDS:	.BLKW			;Number of Node Records defined in DB	;005
EX$LCO:	.BLKW	1		;Default Loop Count
EX$LLE:	.BLKW	1		;Default Loop Block Length
EX$LWI:	.BLKB	1		;Default Loop Data (zeroes, ones, mixed)
EX$NST:	.BLKB	1		;Desired NSP state (NICE code)

				;Passwords must be in same order as node record
EX$SPW:				;Start of Password data 
EX$XPO:	.BLKB	8.		;Transmit Originate Password
EX$RPO:	.BLKB	8.		;Receive Originate Password
EX$XPA:	.BLKB	8.		;Transmit Answer Password
EX$RPA:	.BLKB	8.		;Receive Answer Password
EX$EPW:				;End of Password Data

EX$VOL:	.BLKB	28.		;Volatile Parameter file specification
EX$RXM:	.BLKW			;Retransmit Factor
EX$SBS:	.BLKW			;Segment buffer size (for NSP)
EX$MXN:	.BLKW			;Max nodes (size of node record section)
EX$MXA:	.BLKB			;Max area
EX$AMH:	.BLKB			;Area maxhops
EX$AMC:	.BLKW			;Area maxcost
EX$BRT:	.BLKW			;Broadcast routing timer
EX$MBN:	.BLKW			;Max broadcast nonrouters
EX$MBR:	.BLKW			;Max broadcast routers
EX$PBU:	.BLKW			;Permanent buffers
	.BLKW	256.-<<.-EX$FLG>/2> ;Reserved -- must be zero

EXCSIZ:				;Length of entry

.ASSUME	EXCSIZ EQ 512.		;Executor Node records must be <= 1 block long

	CKPWR2	EXCSIZ		;MAKE SURE THIS RECORD'S SIZE IS A POWER OF 2

; First flag word bit definitions

.BSECT

EF$REA:	.BLKB	.		;Real Flag (0 = Entry unused)
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO

; Secondary flag word bit definitions

.BSECT

	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO
	.BLKB	.		;RESERVED, MUST BE ZERO


.MCALL	.MDELET
.MDELET	$EXEREC
.ENDM	$EXEREC
.MACRO	$EVLREC

.MCALL	.DSECT,.BSECT,.EQUATE
.EQUATE	<CON.SZ>, 6		; maximum console name length
.EQUATE	<FIL.SZ>, 28.		; maximum file name length
.EQUATE	<MON.SZ>, 6		; maximum monitor name length
.EQUATE	<CLS.NM>, 17.		; number of event classes supported
.EQUATE	<FLX.SZ>, 17.*2		; maximum filter size per sink (words)
.EQUATE	<SQL.MX>, 12.		; size of source-qual data for class (bytes)
.EQUATE	<REM.MX>, 2		; maximum number of remote sink nodes
.EQUATE	<NOD.MX>, 4		; maximum sink nodes (0=local)
				; (note - left at 4 to preserve offsets, though
				;  4th space now used for source qualifiers.)

.DSECT				; EVENT LOG DATABASE RECORD DEFINITIONS
LG.STA:	.BLKB			; sink.state%,		! logging sink state
	.BLKB			; RESERVED, MUST BE ZERO
	.EVEN			;
LG.CON: .BLKB	CON.SZ+1	; log.console$	=6%,	! logging console
	.EVEN
LG.FIL: .BLKB	FIL.SZ+1	; log.file$	=28%,	! logging file
	.EVEN
LG.MON:	.BLKB	MON.SZ+1	; log.monitor$	=6%,	! logging monitor
	.EVEN
LG.NOA:	.BLKW	NOD.MX		; sink.address%(3%),	! sink node address
	.BLKB	8.*NOD.MX	; sink.name$(3%)=8%,	! sink node name
				;			!    (unused)
LG.FLG:	.BLKW	NOD.MX		; sink.flag%(3%),	! sink logging flag
	.BLKW	256.-<FLX.SZ*6>-<<.-LG.STA>/2> ; RESERVED, MUST BE ZERO

LG.FLX:	.BLKW	<FLX.SZ*3>*<NOD.MX-1>; sink.filter%(305%) ! sink logging filter
				; 			!   0..101  = local
				; 			!    0..33  = console
				; 			!   34..67  = file
				; 			!   68..101 = monitor
				; 			! 102..203  = remote.1
				; 			! 204..305  = remote.2
LG.SQL:	.BLKW	<CLS.NM>*<SQL.MX/2>	;source.qual%(101%) ! source qual data
				;	 (used to be --	! 306..407  = remote.3)
	.BLKW	512.-<<.-LG.STA>/2> ; RESERVED, MUST BE ZERO

LG.SIZ:				; 2000-BYTES FOR LOGGING ENTITY RECORD
;  LG.STA FIELDS
.BSECT
ST.CON:	.BLKB	.		; 			!   1 = console
ST.FIL:	.BLKB	.		; 			!   2 = file
ST.MON:	.BLKB	.		; 			!   4 = monitor
	.BLKB	.		; RESERVED
	.BLKB	.		; RESERVED
	.BLKB	.		; RESERVED
	.BLKB	.		; RESERVED
ST.AUTO:.BLKB	.		; 1 => autostart event logger on "set system",
				; 0 => don't autostart the event logger

; LG.SQL ENTRY LAYOUT
.DSECT
SQ.FIL:	.BLKW	2		;FILTER FOR EVENT CLASS
SQ.ENT:	.BLKB	1		;ENTITY TYPE = NONE, NODE, LINE, OR CIRCUIT
SQ.FLG:	.BLKB	1		;ENTITY FLAGS

SQ.NAD:				;NODE ADDRESS IF ENTITY TYPE = NODE
SQ.DDM:	.BLKW	1		;DEVICE NAME (RAD50) IF ENTITY TYPE = LINE/CIRC
SQ.DEV:	.BLKW	1		;DEVICE NAME (ASCII) 
SQ.CON:	.BLKB	1		;DEVICE CONTROLLER NUMBER
SQ.TRB:	.BLKB	1		;DEVICE TRIBUTARY NUMBER
SQ.SIZ:				;SIZE OF SOURCE-QUALIFIER ENTRY

.ASSUME	SQ.SIZ EQ SQL.MX	;MUST BE SAME AS DATA BASE SIZE ASSUMPTION
.ASSUME SQ.TRB EQ SQ.CON+1	;SO I CAN MOVE WORD NOT BYTES

; SQ.FLG FLAG BIT DEFINITIONS
.BSECT
SQF.CN:	.BLKB	.		; 			!   1 = console
SQF.FL:	.BLKB	.		; 			!   2 = file
SQF.MN:	.BLKB	.		; 			!   4 = monitor
SQF.MP:	.BLKB	.		; 1 => MULTIPOINT LINE/CIRC
SQF.S1:	.BLKB	.		;USE TWO BITS AS INDEX
SQF.S2:	.BLKB	.		; TO SINK NODE LIST 'LG.NOA'
	.BLKB	.		;RESERVED
SQF.RE:	.BLKB	.		;ENTRY REAL FLAG: 1 => DATA VALID

SQF.SN=SQF.S1!SQF.S2		;INDEX TO SINK NODE LIST

.ASSUME SQF.CN EQ ST.CON	;ALWAYS USE SAME BITS FOR SINK TYPES
.ASSUME SQF.FL EQ ST.FIL
.ASSUME SQF.MN EQ ST.MON
;.ASSUME	SQF.MP EQ LP$MPT	;MUST BE SAME AS LINE/CIRC PARSE FLAG
				;(DEFINED IN $LINDF MACRO, IN NMLMAC.MAC)
.MCALL	.MDELET
.MDELET	$EVLREC
.ENDM	$EVLREC
.MACRO	$NCPDB
.MCALL	.DSECT,.EQUATE

; MACRO TO DEFINE DATABASE SECTIONS (STARTING BLOCK IN FILE FOR EACH DATABASE)
.MACRO	DBSBLK	RECSIZ,RECMAX,SYMNAM,ZERO
.NLIST
.IIF	NB	<ZERO>,	$$$SBL=0
.IF	NB	<SYMNAM>
.LIST
.EQUATE	<SYMNAM>, $$$SBL		;STARTING BLOCK OF THIS DATABASE
.NLIST
.ENDC
$$$SBL	= $$$SBL+<<<<RECSIZ/2*RECMAX>+377>/2>&77777/200>
;;.IIF	GT <$$$SBL-255.>,	.ERROR	;STARTING BLOCK OUT OF RANGE
.LIST
.ENDM	DBSBLK

; MAKE SURE THINGS ARE DEFINED

.MCALL	$CTLREC,$OBJREC,$NOBREC,$LINREC,$EXEREC,$EVLREC
.IIF	NDF	NC.SIZ,	$CTLREC
.IIF	NDF	O.SIZE,	$OBJREC
.IIF	NDF	ND.SIZ,	$NOBREC
.IIF	NDF	LI.SIZ,	$LINREC
.IIF	NDF	EXCSIZ,	$EXEREC
.IIF	NDF	LG.SIZ,	$EVLREC

; COUNT OF RECORDS ALLOCATED TO EACH SUBFILE (DATABASE)

.EQUATE	<RC$NCP>, 1.		;1 RECORD ALLOCATED TO NCP DATABASE CONTROL
.EQUATE	<RC$OBJ>, 256.		;256 RECORDS ALLOCATED TO OBJECT DATABASE
.EQUATE	<RC$LIN>, <16.+8.+<16.*32.>+16.> ;552 records allocated to line DB;029
				; (Added 4 UNA/LUA and 4 QNA slots)	;010
.EQUATE	<RC$EXE>, 2.		;2 RECORDS ALLOCATED TO EXECUTOR DATABASE
.EQUATE	<RC$EVL>, 1.		;1 RECORD ALLOCATED TO LOGGING DATABASE
				;maxarea * 4 node pointer blocks	;000
				;(maxnodes * 1.25)/8 node blocks	;000

; ** NOTE **	THE FOLLOWING DEFINITIONS ARE ORDERED....

; STARTING BLOCK NUMBER FOR EACH SUBFILE
; * ORDER OF OCCURRENCE IN THIS TABLE FORCES ORDER OF OCCURRENCE IN DATABASE *

	DBSBLK	NC.SIZ,RC$NCP,SB$NCP,ZERO ;NCP DATABASE CTL BLOCK (BLOCK 0)
	DBSBLK	O.SIZE,RC$OBJ,SB$OBJ	;OBJECT DATABASE STARTING BLOCK
	DBSBLK	LI.SIZ,RC$LIN,SB$LIN	;LINE DATABASE STARTING BLOCK
	DBSBLK	EXCSIZ,RC$EXE,SB$EXE	;EXECUTOR DATABASE STARTING BLOCK
	DBSBLK	LG.SIZ,RC$EVL,SB$EVL	;LOGGING DATABASE STARTING BLOCK
	DBSBLK	2,     0,     SB$NPT	;Node pointer records

; DATABASE RECORD TYPES

; ** NOTE **	THE FOLLOWING DEFINITIONS ARE IN THE SAME ORDER AS THE ABOVE

.DSECT

TYP$NC:	.BLKB			;0 = NCP DATABASE CONTROL RECORD
TYP$OB:	.BLKB			;1 = OBJECT DEFINITION RECORD
TYP$LI:	.BLKB			;2 = LINE DEFINITION RECORD
TYP$EX:	.BLKB			;3 = EXECUTOR DEFINITION
TYP$LG:	.BLKB			;4 = LOGGING DEFINITION RECORD
TYP$NP:	.BLKB			;5 = Node pointer record
TYP$NO:	.BLKB			;6 = NODE DEFINITION RECORD
	.BLKB	-1
MAXRT:				;MAXIMUM ALLOWABLE RECORD TYPE

; ... TO HERE

.MCALL	.MDELET
.MDELET $NCPDB
.ENDM	$NCPDB
.MACRO	$TRAPMB
.DSECT

P$LINK:	.BLKW			;LINK TO NEXT PMB
P$BUFA:	.BLKW			;CONTORTED BUFFER ADDRESS (0 IF NONE)
P$TYPE:	.BLKB			;TYPE CODE (SR$TRA)
T$SEC:	.BLKB			;SECONDS UNTIL MINUTE (TIMSEC)
T$DATE:	.BLKW			;DATE (RSTS FORMAT)
T$TIME:	.BLKW			;TIME (RSTS FORMAT)
P$BREM:	.BLKW			;Bytes remaining in message		;009
T$DDB:	.BLKW			;DDB ADDRESS OF CIRCUIT
T$FC:	.BLKB	2		;CC.FC AND CC.DFL FIELDS
T$CCB:	.BLKW			;Address of message CCB			;009
T$MMU:	.BLKW			;MMU ADDRESS OF DATA
T$ADDR:	.BLKW			;Virtual address of data		;009
T$XHDR:	.BLKB	BUFHDR		;Copy of XBUF buffer header		;009
	.BLKW	2		;Reserved for future use		;009
.ASSUME	. EQ 40

.MCALL	.MDELET
.MDELET	$TRAPMB
.ENDM	$TRAPMB
.MACRO	$EVLPMB
.DSECT

P$LINK:	.BLKW			;LINK TO NEXT PMB
P$BUFA:	.BLKW			;CONTORTED BUFFER ADDRESS (0 IF NONE)
P$TYPE:	.BLKB			;TYPE CODE (SR$EVT)
E$USED:	.BLKB			;NUMBER OF BYTES USED IN FIRQB
E$CODE:	.BLKW			;EVENT CODE
E$DATE:	.BLKW			;DATE (RSTS FORMAT)
E$TIME:	.BLKW			;TIME (RSTS FORMAT)
E$EDAT:				;"LOG AN EVENT" USER CALL HAS ENTITY DATA HERE
E$TICK:	.BLKW			;TIME (RSTS FORMAT SECONDS AND TICKS)
E$ENT:	.BLKB			;ENTITY TYPE
E$DATA:	.BLKB	17.		;ENTITY ID AND/OR EVENT DATA

.ASSUME	. EQ 40

.MCALL	.MDELET
.MDELET	$EVLPMB
.ENDM	$EVLPMB
.MACRO	$NICE
; DATA ID FIELD

.BSECT
	.BLKB	7776
ID.TYP:	.BLKB			;PARAMETER TYPE
ID.MAP:	.BLKB	50000		;BIT MAPPED (COUNTERS ONLY)
ID.WID:	.BLKB	20000		;FIELD WIDTH (COUNTERS ONLY)
;	=	0*20000		;RESERVED
IW$BYT	=	1*20000		;8 BITS (BYTE)
IW$WRD	=	2*20000		;16 BITS (WORD)
IW$LNG	=	3*20000		;32 BITS (LONGWORD)
ID.CNT:	.BLKB	.		;SET FOR COUNTERS

; SPECIAL ENTRIES (INTERNAL ONLY) IN ID FIELD (FOR EVENT LOGGER CODE)

.DSECT	7000
ID$SPC:				;FIRST SPECIAL VALUE
ID$END:	.BLKW			;END OF LOG LIST
ID$PTR:	.BLKW			;NEXT WORD IS POINTER TO SUBLIST
ID$EBF:	.BLKW			;END OF DATA FROM BUFFER, REST IS FROM GLOBALS
ID$PTX:	.BLKW			;Next two words are sublist pointer and	;006
				;new DPAR5 value to get sublist		;006

; DATA TYPE FIELD

.BSECT
	.BLKB	16
TY.LEN:	.BLKB	41		;LENGTH OF FIELD (BINARY OR CODED FIELDS ONLY)
TY.FMT:	.BLKB	20		;TYPE CODE (BINARY FIELDS ONLY)
TF$DU	=	0*20		;UNSIGNED DECIMAL
TF$DS	=	1*20		;SIGNED DECIMAL
TF$HEX	=	2*20		;HEXADECIMAL
TF$OCT	=	3*20		;OCTAL
TY.ASC:				;ASCII FIELD (NON-CODED ONLY)
TY.MUL:	.BLKB	.		;MULTIPLE FIELD (CODED ONLY)
TY.COD:	.BLKB	.		;CODED FIELD

; STANDARD TYPE CODES

TY$AI	=	TY.ASC		;ASCII IMAGE
TY$DU1	=	TF$DU+1		;DECIMAL UNSIGNED BYTE
TY$DU2	=	TF$DU+2		;DECIMAL UNSIGNED WORD
TY$DS1	=	TF$DS+1		;DECIMAL SIGNED BYTE
TY$DS2	=	TF$DS+2		;DECIMAL SIGNED WORD
TY$HXI	=	TF$HEX		;HEX IMAGE FIELD
TY$HX1	=	TF$HEX+1	;HEX BYTE
TY$HX2	=	TF$HEX+2	;HEX WORD
TY$HX6	=	TF$HEX+6	;Six byte hex string			;007
TY$OC1	=	TF$OCT+1	;OCTAL BYTE
TY$OC2	=	TF$OCT+2	;OCTAL WORD
TY$CD1	=	TY.COD+1	;CODED BYTE
TY$CM1	=	TY.COD+TY.MUL+1	;CODED MULTIPLE, 1 ITEM
TY$CM2	=	TY.COD+TY.MUL+2	;CODED MULTIPLE, 2 ITEMS
TY$CM3	=	TY.COD+TY.MUL+3	;CODED MULTIPLE, 3 ITEMS
TY$CM4	=	TY.COD+TY.MUL+4	;CODED MULTIPLE, 4 ITEMS

.MCALL	.MDELET
.MDELET	$NICE
.ENDM	$NICE
.MACRO	$EVLDB
EL.RNG	=: 8.			;NUMBER OF CLASSES TO HANDLE IN EACH RANGE
				;I.E. 0-7 (GENERAL) AND 32-40 (RSTS SPECIFIC)
CLASES	=: 2*EL.RNG+1		;TWO RANGES, PLUS THE USER EVENT CLASS (480)
FLTSIZ	=: <32./8.>*2+2		;SIZE OF A FILTER (PER CLASS)
EL.QMX	=: 10.			;MAXIMUM NUMBER OF REAL EVENTS PER CLASS

; THE EVENT LOGGING DATA BUFFER CONSISTS OF A FIXED PREFIX, FOLLOWED BY
; A SET OF PER-CLASS EVENT QUEUE BLOCKS. THE EVENT QUEUE BLOCKS ALSO CONTAIN
; THE GLOBAL FILTERS FOR THAT CLASS.  THE POINTER TO THE LOGGING BUFFER
; (IN EVTVIR IN THE IMPURE AREA) POINTS TO THE END OF THE PREFIX.

; EVENT LOGGER PREFIX

.DSECT	-2
EL.LST:	.BLKW	-1		;LOST EVENT COUNT
EL.TIC:	.BLKW	-1		;DATE AND TIME OF FIRST LOST EVENT
EL.TIM:	.BLKW	-1
EL.DAT:	.BLKW	-1
	.BLKW			;COMPENSATES FOR -1 ABOVE
EL.PFX	=	-.		;LENGTH OF PREFIX

; EVENT CLASS QUEUE BLOCK

.DSECT
EL.FLT:	.BLKB	32./8.		;FILTERS, 32 CODES/CLASS, 8 BITS/BYTE
EL.QFL:	.BLKB	32./8.		;QUALIFIED FILTERS
EL.QUA:	.BLKW			;QUALIFIER VALUE
.ASSUME	FLTSIZ EQ .
EL.QUE:	.BLKW	2		;PENDING EVENT QUEUE
EL.CNT:	.BLKW			;COUNT OF PENDING EVENTS FOR THIS CLASS
EL.SIZ:				;LENGTH OF EACH ENTRY

EL.TSZ	=	EL.PFX+<EL.SIZ*CLASES> ;TOTAL SIZE OF EVENT LOGGER DATA

.MCALL	.MDELET
.MDELET	$EVLDB
.ENDM	$EVLDB
.MACRO	$EVTCOD
.DSECT
EV$NML:	.BLKB	100		;NETWORK MANAGEMENT
EV$END:	.BLKB	100		;END USER LAYER
EV$SES:	.BLKB	100		;SESSION CONTROL
EV$NSP:	.BLKB	100		;NETWORK SERVICES
EV$TRN:	.BLKB	100		;TRANSPORT LAYER
EV$DLL:	.BLKB	100		;DATA LINK LAYER
EV$PHY:	.BLKB	100		;PHYSICAL LINK LAYER

.DSECT	<32.*100>		;RSTS SPECIFIC EVENT CLASSES
EV$RST:				;STARTING CLASS CODE FOR RSTS SPECIFIC CLASSES
RV$NML:	.BLKB	100		;NETWORK MANAGEMENT
RV$END:	.BLKB	100		;END USER LAYER
RV$SES:	.BLKB	100		;SESSION CONTROL
RV$NSP:	.BLKB	100		;NETWORK SERVICES
RV$TRN:	.BLKB	100		;TRANSPORT LAYER
RV$DLL:	.BLKB	100		;DATA LINK LAYER
RV$PHY:	.BLKB	100		;PHYSICAL LINK LAYER

.ASSUME	. GT 0

.DSECT	<480.*100>		;USER-DEFINABLE EVENT CLASSES
EV$USR:	.BLKB	100		;FOR NOW WE ONLY SUPPORT ONE

EC$PTR	=	-1		;FLAGS THAT POINTER TO ACTUAL CODE IS IN-LINE

; EVENT SUBCODES

.DSECT	EV$NML			;NETWORK MANAGEMENT EVENTS
EN$LST:	.BLKB			;EVENTS LOST
EN$NCT:	.BLKB			;NODE COUNTERS
EN$LCT:	.BLKB			;LINE COUNTERS
EN$SRV:	.BLKB			;AUTOMATIC LINE SERVICE
EN$LC0:	.BLKB			;LINE COUNTERS ZEROED
EN$NC0:	.BLKB			;NODE COUNTERS ZEROED
EN$LOP:	.BLKB			;LOOPBACK
EN$ABT:	.BLKB			;SERVICE REQUEST ABORTED
EN$AUT:	.BLKB			;AUTOMATIC COUNTERS
EN$ZER:	.BLKB			;COUNTERS ZEROED
.ASSUME	.-EV$NML LE EV.SUB

.DSECT	EV$END			;END USER LAYER EVENTS

.DSECT	EV$SES			;SESSION CONTROL EVENTS
ES$STA:	.BLKB			;LOCAL NODE STATE CHANGE
ES$ACS:	.BLKB			;ACCESS CONTROL REJECT
.ASSUME	.-EV$SES LE EV.SUB

.DSECT	EV$NSP			;NSP EVENTS
EN$ILM:	.BLKB			;ILLEGAL MESSAGE
EN$FCV:	.BLKB			;FLOW CONTROL VIOLATION
EN$NOB:	.BLKB			;NODE BLOCK REUSED
.ASSUME	.-EV$NSP LE EV.SUB

.DSECT	EV$TRN			;TRANSPORT EVENTS
ET$AGE:	.BLKB			;PACKET IS TOO OLD
ET$UNR:	.BLKB			;DESTINATION UNREACHABLE
ET$OOR:	.BLKB			;DESTINATION NODE NUMBER OUT OF RANGE
ET$SIZ:	.BLKB			;PACKET TOO LARGE FOR NEIGHBOR
ET$FMT:	.BLKB			;PACKET FORMAT ERROR
ET$RUL:	.BLKB			;PARTIAL ROUTING UPDATE LOSS
ET$VFY:	.BLKB			;VERIFICATION REJECT
ET$LDN:	.BLKB			;CIRCUIT DOWN, CIRCUIT FAULT
ET$SOF:	.BLKB			;CIRCUIT DOWN
ET$OPR:	.BLKB			;CIRCUIT DOWN, OPERATIONAL
ET$LUP:	.BLKB			;CIRCUIT UP
ET$ILN:	.BLKB			;INITIALIZATION FAILURE, CIRCUIT FAULT
ET$ISF:	.BLKB			;INITIALIZATION FAILURE
ET$IOP:	.BLKB			;INITIALIZATION FAILURE, OPERATIONAL
ET$PTH:	.BLKB			;NODE REACHABILITY CHANGE
ET$AUP:	.BLKB			;Adjacency up				;000
ET$ARJ:	.BLKB			;Adjacency rejected			;000
ET$ARC:	.BLKB			;Area reachability change		;012
ET$ADN:	.BLKB			;Adjacency down				;000
ET$AOP:	.BLKB			;Adjacency down, operational		;000
.ASSUME	.-EV$TRN LE EV.SUB

.DSECT	EV$DLL			;DATA LINK LAYER EVENTS
ED$STA:	.BLKB			;STATE CHANGE
ED$RST:	.BLKB			;REMOTELY INITIATED STATE CHANGE
ED$RSM:	.BLKB			;PROTOCOL RESTART WHILE IN MOP MODE
ED$SER:	.BLKB			;SEND ERROR TRESHOLD
ED$RER:	.BLKB			;RECEIVE ERROR TRESHOLD
ED$SLE:	.BLKB			;Select error treshold			;000
ED$BHF:	.BLKB			;Block header format error		;000
ED$ADR:	.BLKB			;Selection address error		;000
ED$STR:	.BLKB			;STREAMING TRIBUTARY
ED$SIZ:	.BLKB			;BUFFER TOO SMALL
ED$RES:	.BLKB			;Restart				;000
ED$XSC:	.BLKB			;X.25 state change			;000
ED$RMX:	.BLKB			;Retransmit maximum exceeded		;000
ED$INI:	.BLKB			;Initialization failure			;000
ED$SNF:	.BLKB			;Send failed				;000
ED$RCF:	.BLKB			;Receive failed				;000
ED$CCF:	.BLKB			;Collision detect check failed		;000
ED$DTU:	.BLKB			;DTE up					;000
ED$DTD:	.BLKB			;DTE down				;000

.ASSUME	.-EV$DLL LE EV.SUB

.DSECT	EV$PHY			;PHYSICAL LINK LAYER EVENTS
EP$DSR:	.BLKB			;DATA SET READY CHANGE
EP$RNG:	.BLKB			;RING CHANGE
EP$CAR:	.BLKB			;CARRIER STATUS CHANGE
.ASSUME	.-EV$PHY LE EV.SUB

.DSECT	RV$NML			;RSTS SPECIFIC NETWORK MANAGEMENT EVENTS
RN$REQ:	.BLKB			;NETWORK MANAGEMENT REQUEST LOGGED
.ASSUME	.-RV$NML LE EV.SUB

.DSECT	RV$END			;RSTS SPECIFIC END USER EVENTS
RE$NFA:	.BLKB			;NETWORK ACCESS TO FILE
.ASSUME	.-RV$END LE EV.SUB

.DSECT	RV$SES			;RSTS SPECIFIC SESSION CONTROL EVENTS
RS$SPW:	.BLKB			;OBJECT SPAWNED
RS$SPF:	.BLKB			;OBJECT SPAWN FAILED
.ASSUME	.-RV$SES LE EV.SUB

.DSECT	RV$NSP			;RSTS SPECIFIC NSP EVENTS

.DSECT	RV$TRN			;RSTS SPECIFIC TRANSPORT EVENTS

.DSECT	RV$DLL			;RSTS SPECIFIC DATA LINK LAYER EVENTS

.DSECT	RV$PHY			;RSTS SPECIFIC PHYSICAL LINK LAYER EVENTS

.MCALL	.MDELET
.MDELET	$EVTCOD
.ENDM	$EVTCOD
.MACRO	$EVTCTR
.MACRO	.CTR	NAME,VALUE,WIDTH
.NLIST
.IF IDN	<WIDTH>,<B>
CT$'NAME	=:	ID.CNT+IW$BYT+VALUE'.
.IFF
.IF IDN	<WIDTH>,<W>
CT$'NAME	=:	ID.CNT+IW$WRD+VALUE'.
.IFF
CT$'NAME	=:	ID.CNT+IW$LNG+VALUE'.
.ENDC
.ENDC
.LIST
.ENDM	.CTR
.IF NDF	ID.CNT
	.MCALL	$NICE
	$NICE
.ENDC

; Circuit counters

;	Name	Value	Size

.CTR	ZTM	0	W	;Seconds since last zeroed
.CTR	LRC	800	L	;Arriving packets received
.CTR	LXM	801	L	;Departing packets sent
.CTR	LCL	802	W	;Arriving congestion loss
.CTR	COR	805	B	;Corruption loss (X.25)
.CTR	TRC	810	L	;Transit packets received
.CTR	TXM	811	L	;Transit packets sent
.CTR	TCL	812	W	;Transit congestion loss
.CTR	DWN	820	B	;Circuit down
.CTR	IFL	821	B	;Initialization failure
.CTR	BRC	1000	L	;Bytes received
.CTR	BXM	1001	L	;Bytes transmitted
.CTR	FRC	1010	L	;Frames received
.CTR	FXM	1011	L	;Frames transmitted
.CTR	DEI	1020	B	;Data errors inbound
.CTR	DEO	1021	B	;Data errors outbound
.CTR	RRT	1030	B	;Remote reply timeouts
.CTR	LRT	1031	B	;Local reply timeouts
.CTR	RBE	1040	B	;Remote buffer errors
.CTR	LBE	1041	B	;Local buffer errors
.CTR	SEL	1050	W	;Selection intervals elapsed
.CTR	SET	1051	B	;Selection timeouts
.CTR	UBU	1065	W	;User buffer unavailable		;000
.CTR	LIR	1240	B	;Locally initiated resets
.CTR	RIR	1241	B	;Remotely initiated resets
.CTR	NIR	1242	B	;Network initiated resets

; Line counters

;	Name	Value	Size

.CTR	ZTM	0	W	;Seconds since last zeroed
.CTR	BRC	1000	L	;Bytes received				;000
.CTR	BXM	1001	L	;Bytes transmitted			;000
.CTR	MBR	1002	L	;Multicast bytes received		;000
.CTR	FRC	1010	L	;Data blocks received			;000
.CTR	FXM	1011	L	;Data blocks transmitted		;000
.CTR	MFR	1012	L	;Multicast frames received		;000
.CTR	XID	1013	L	;Blocks transmitted, initially deferred	;000
.CTR	XSC	1014	L	;Blocks transmitted, single collision	;000
.CTR	XMC	1015	L	;Blocks transmitted, multiple collisions ;000
.CTR	RBE	1040	B	;Remote buffer errors			;000
.CTR	LBE	1041	B	;Local buffer errors			;000
.CTR	XMF	1060	W	;Transmit failure			;000
.CTR	CCF	1061	W	;Collision check failure		;000
.CTR	RCF	1062	W	;Receive failure			;000
.CTR	UFD	1063	W	;Unrecognized frame destination		;000
.CTR	OVR	1064	W	;Data overrun				;000
.CTR	SBL	1065	W	;System buffer unavailable		;000
.CTR	UBL	1066	W	;User buffer unavailable		;000
.CTR	RPE	1100	B	;Remote station errors
.CTR	LPE	1101	B	;Local station errors

; Node counters

;	Name	Value	Size

.CTR	ZTM	0	W	;Seconds since last zeroed
.CTR	BYR	600	L	;User bytes received
.CTR	BYX	601	L	;User bytes sent
.CTR	MSR	602	L	;User messages received			;000
.CTR	MSX	603	L	;User messages received			;000
.CTR	TBR	608	L	;Total bytes received			;000
.CTR	TBX	609	L	;Total bytes sent			;000
.CTR	TMR	610	L	;Total messages received
.CTR	TMX	611	L	;Total messages sent
.CTR	CIR	620	W	;Connects received
.CTR	CIS	621	W	;Connects sent
.CTR	RTM	630	W	;Response timeouts
.CTR	RRE	640	W	;Received connect resource errors
.CTR	LLA	700	W	;Max logical links active
.CTR	APL	900	B	;Aged packet loss
.CTR	UNR	901	W	;Node unreachable packet loss
.CTR	OOR	902	B	;Node out of range packet loss
.CTR	SIZ	903	B	;Oversized packet loss
.CTR	FRM	910	B	;Packet format errors
.CTR	RUL	920	B	;Partial routing update loss
.CTR	VER	930	B	;Verification reject
.CTR	NRN	2200	W	;Number of reachable nodes
.CTR	NRM	2201	W	;Highest number of reachable nodes

.MCALL	.MDELET
.MDELET	$EVTCTR
.ENDM	$EVTCTR
.MACRO	$EVLDEF
.BSECT
	.BLKB	35
EV.DSP:	.BLKB	2		;ACTION DISPATCH
	.BLKB	.		;RESERVED
	.BLKB	.		;RESERVED
EV.ACT:	.BLKB	.		;IF SET, EV.DSP SPECIFIES HOW TO GET DATA
				;IF CLEAR, LOW ORDER BITS CONTAIN DATA
; ACTION CODE VALUES

.DSECT	EV.ACT
EV$INL:	.BLKW			;NEXT WORD IS DATA
EV$PTR:	.BLKW			;NEXT WORD POINTS TO DATA

EVTSIZ	=: 100.			;SIZE OF EVENT BUFFER FOR INTERNAL EVENTS

; EVENT CODE (E$CODE) LAYOUT

.BSECT
	.BLKB	36
EV.SUB:	.BLKB	1		;EVENT SUBCODE
	.BLKB	77640		;RESERVED
EV.CLS:	.BLKB	100		;EVENT CLASS
	.BLKB	.		;RESERVED

EV.RS	=	^C<EV.SUB!EV.CLS> ;RESERVED BITS

; ENTITY TYPE (E$ENT) CODING

.DSECT	-1
EE$NON:	.BLKB			;NONE
EE$NOD:	.BLKB			;NODE
EE$LIN:	.BLKB			;LINE
EE$LOG:	.BLKB			;LOGGING
EE$CIR:	.BLKB			;CIRCUIT
EE$MOD:	.BLKB			;MODULE
EE$ARE:	.BLKB			;Area					;000

.MCALL	.MDELET
.MDELET	$EVLDEF
.ENDM	$EVLDEF
.MACRO	$EVTTYP
.DSECT
EN$STY:	.BLKB			;SERVICE (EN$SRV)
EN$STA:	.BLKB			;STATUS (EN$SRV)
EN$OPR:	.BLKB			;OPERATION (EN$LOP)
EN$RSN:	.BLKB			;REASON (EN$ABT)
EN$NOD:	.BLKB			;Node					;000
EN$DTE:	.BLKB			;DTE					;000
EN$FIL:	.BLKB			;Filespec				;000
EN$SWT:	.BLKB			;Software type				;000

.DSECT				;(EN$SRV)
ENS$LD:	.BLKB			;LOAD
ENS$DM:	.BLKB			;DUMP

.DSECT	-1			;(EN$STA)
ENS$ER:	.BLKB			;FAILED
ENS$RQ:	.BLKB			;REQUESTED
ENS$OK:	.BLKB			;SUCCEEDED

.DSECT				;(EN$OPR)
ENO$IN:	.BLKB			;INITIATED
ENO$TM:	.BLKB			;TERMINATED

.DSECT				;(EN$ABT)
ENA$TM:	.BLKB			;RECEIVE TIMEOUT
ENA$ER:	.BLKB			;RECEIVE ERROR
ENA$LS:	.BLKB			;LINE STATE CHANGE
ENA$UN:	.BLKB			;UNRECOGNIZED REQUEST
ENA$LO:	.BLKB			;LINE OPEN ERROR

; SESSION CONTROL LAYER EVENTS (EV$SES)

.DSECT
ES$RSN:	.BLKB			;REASON (ES$STA)
ES$OST:	.BLKB			;OLD STATE (ES$STA)
ES$NST:	.BLKB			;NEW STATE (ES$NST)
ES$NOD:	.BLKB			;SOURCE NODE (ES$ACS)
ES$SPR:	.BLKB			;SOURCE PROCESS (ES$ACS)
ES$DPR:	.BLKB			;DESTINATION PROCESS (ES$ACS)
ES$USR:	.BLKB			;USER - PPN, UIC, ... (ES$ACS)
ES$PAS:	.BLKB			;PASSWORD (ES$ACS)
ES$ACT:	.BLKB			;ACCOUNT (ES$ACS)

.DSECT				;(ES$RSN)
ESR$OP:	.BLKB			;OPERATOR COMMAND
ESR$NO:	.BLKB			;NORMAL OPERATION

.DSECT				;(ES$OST, ES$NST)
ESS$ON:	.BLKB			;ON
ESS$OF:	.BLKB			;OFF
ESS$SH:	.BLKB			;SHUT
ESS$RS:	.BLKB			;RESTRICTED

; NETWORK SERVICES LAYER EVENTS (EV$NSP)

.DSECT
EN$MSG:	.BLKB			;MESSAGE HEADER (EN$MSG, EN$FLO)
EN$FLO:	.BLKB			;CURRENT FLOW CONTROL VALUE (EN$FLO)
EN$SRC:	.BLKB			;Source node identification (EN$MSG, EN$FLO) ;000

; TRANSPORT LAYER EVENTS (EV$TRN)

.DSECT
ET$HDR:	.BLKB			;TRANSPORT HEADER (MOST EVENTS)
ET$BEG:	.BLKB			;BEGINNING OF PACKET (ET$FMT)
ET$MAX:	.BLKB			;HIGHEST REACHABLE NODE ADDRESS (ET$RUL)
ET$NOD:	.BLKB			;NODE (ET$VFY, ET$LUP)
ET$EXP:	.BLKB			;EXPECTED NODE (ET$OPR)
ET$RSN:	.BLKB			;REASON (LINE DOWN OR INIT FAILURES)
ET$VER:	.BLKB			;RECEIVED VERSION NUMBER (ET$IOP)
ET$STA:	.BLKB			;STATUS (ET$NOD)
ET$ADJ:	.BLKB			;Adjacent node (most events)		;000

.DSECT				;(ET$RSN)
ETR$SY:	.BLKB			;LINE SYNCHRONIZATION LOST
ETR$ER:	.BLKB			;DATA ERRORS
ETR$PT:	.BLKB			;UNEXPECTED PACKET TYPE
ETR$CS:	.BLKB			;ROUTING MESSAGE CHECKSUM ERROR
ETR$ID:	.BLKB			;ADJACENT NODE IDENTITY CHANGE
ETR$VT:	.BLKB			;VERIFICATION TIMEOUT
ETR$VE:	.BLKB			;VERSION SKEW
ETR$OR:	.BLKB			;NODE ADDRESS OUT OF RANGE
ETR$SZ:	.BLKB			;ADJACENT NODE BUFFER SIZE TOO SMALL
ETR$VS:	.BLKB			;BAD VERIFICATION SEED
ETR$LT:	.BLKB			;LISTENER TIMEOUT
ETR$LD:	.BLKB			;LISTENER RECEIVED BAD DATA
ETR$CF:	.BLKB			;Call failed (X.25)			;000
ETR$PW:	.BLKB			;Password required (from phase 3 nodes)	;000
ETR$DA:	.BLKB			;Dropped by adjacent node (Ethernet)	;000

.DSECT				;(ET$STA)
ETS$ON:	.BLKB			;NODE REACHABLE (ON)
ETS$OF:	.BLKB			;NODE UNREACHABLE (OFF)

; Data link layer events (EV$DLL)					;001

.DSECT
	.BLKB			;Old State
	.BLKB			;New State
	.BLKB			;Header
	.BLKB			;Selected Tributary			;014
	.BLKB			;Previous Tributary			;014
	.BLKB			;Tributary Status			;014
	.BLKB			;Received Tributary			;014
	.BLKB			;Block Length
	.BLKB			;Buffer Length
	.BLKB			;DTE
	.BLKB			;Reason
	.BLKB			;Old State (for event 5.12)
	.BLKB			;New State (for event 5.12)
	.BLKB			;Parameter type
	.BLKB			;Cause
	.BLKB			;Diagnostic
ED$FRN:	.BLKB			;Failure Reason
ED$DST:	.BLKB			;Distance
ED$EHD:	.BLKB			;Ethernet Header
	.BLKB			;Hardware Status

.DSECT									;001
EDF$EC:	.BLKB			;Excessive Collisions
EDF$CC:	.BLKB			;Carrier Check Failed
	.BLKB			; (Obsolete)				;014
EDF$SC:	.BLKB			;Short Circuit
EDF$OC:	.BLKB			;Open Circuit
EDF$FL:	.BLKB			;Frame Too Long
EDF$RF:	.BLKB			;Remote failure to defer
EDF$BD:	.BLKB			;Block check error
EDF$FE:	.BLKB			;Framing error
EDF$DO:	.BLKB			;Data overrun
EDF$SB:	.BLKB			;System buffer unavailable
EDF$UB:	.BLKB			;User buffer unavailable
EDF$UF:	.BLKB			;Unrecognized frame destination

; RSTS SPECIFIC NETWORK MANAGEMENT EVENTS (RV$NML)

.DSECT
RN$TYP:	.BLKB			;REQUEST TYPE (RN$REQ)
RN$USR:	.BLKB			;USER (RN$REQ)

.DSECT				;(RN$TYP)
	.BLKB			;?

; RSTS SPECIFIC SESSION CONTROL EVENTS (RV$SES)

.DSECT
RS$RSN:	.BLKB			;REASON (RS$SPF)
				;ALL OTHER PARAMS AS UNDER EV$SES

.DSECT				;(RS$RSN)
RSR$IO:	.BLKB			;I/O ERROR ON OBJECT DATABASE
RSR$SF:	.BLKB			;SPAWN DIRECTIVE (CREJOB) FAILED
RSR$UO:	.BLKB			;UNDEFINED OBJECT ID
RSR$NP:	.BLKB			;Program not found			;000

.MCALL	.MDELET
.MDELET	$EVTTYP
.ENDM	$EVTTYP
.MACRO	$NETDEF
NSPJOB	=: 3			;DUMMY NSP JOB NUMBER
TRNJOB	=: 5			;DUMMY TRANSPORT JOB NUMBER
MINBUF	=: 246.			;MINIMUM RECEIVE BUFFER SIZE IN NODE INIT
MINCNT	=: 2			;NSP BUFFER DISCARD TRESHOLD FOR TRN

MAXSID	=	32.		;MAX SIZE OF SYSTEM ID STRING
INITMR	=	30.		;Initialization timeout
VFYTMR	=	30.		;Verification timer
IDLMUL	=	2.		;Non-broadcast circuit idle time multiplier
BCT3	=	3.		;Broadcast circuit idle time multiplier
DRDEL	=	5.		;Designated router declaration delay
ACKDLY	=	3.		;Number of seconds to delay ACKs

.MCALL	.MDELET
.MDELET	$NETDEF
.ENDM	$NETDEF
.MACRO	$MNUDEF
.BSECT

MNU.AC:	.BLKB	.		;Access control fields present
MNU.UD:	.BLKB	.		;User data field present
MNU.PR:	.BLKB	.		;Proxy login requested			;016
MNU.PU:	.BLKB	.		;If set, use UIC for proxy identifier	;020
	.BLKB	120		;Reserved				;016
MNU.VE:	.BLKB	40		;Version number				;016
	.BLKB	.		;Reserved

.DSECT		;Bit definitions in MNU.VE

MNU$V1:	.BLKB	40		;Session control V1.0			;016
MNU$V2:	.BLKB	40		;Session control V2.0			;016
	.BLKB	40		;Reserved				;016
	.BLKB	40		;Reserved				;016

.MCALL	.MDELET
.MDELET	$MNUDEF
.ENDM	$MNUDEF
.MACRO	$NETDDB 
.DSECT

; STANDARD FIXED ENTRIES

DDIDX:	.BLKB			;DRIVER INDEX
DDSTS:	.BLKB			;STATUS AND ACCESS CONTROL
DDJBNO:	.BLKB			;OWNING JOB NUMBER * 2 (OR 5 FOR TRN)
DDUNT:	.BLKB			;UNIT NUMBER TIMES 1
DDTIME:	.BLKW			;TIME ASSIGNED OR INITED
DDCNT:	.BLKW			;INIT COUNT AND ASSIGNMENT CONTROL

; COMM DEVICE SPECIFIC FIXED ENTRIES

DDFLAG:	.BLKW			;Comm driver circuit flags
L.FLAG:	.BLKW			;Routing layer circuit flags
L.TBUF:	.BLKB			;Count of outstanding transit buffers	;016
L.RQTA:	.BLKB			;Receive buffer quota			;007
L.TIME:	.BLKW			;Circuit timer
L.DDB:	.BLKW			;Address to pass to driver as a DDB	;007
L.INFO:	.BLKW			;POINTER TO INIT/VERIFY INFO BUFFER	;007
; *** ORDER MUST BE PRESERVED
L.ZDAT:	.BLKW			;DATE (RSTS FORMAT) WHEN COUNTERS LAST ZEROED
L.ZTIM:	.BLKW			; AND TIME (SECONDS/2 UNTIL MIDNIGHT)
L.SCNT:				;START OF COUNTERS
L.TREC:	.BLKW	2		;COUNT OF TRANSIT PACKETS RECEIVED
L.TXMT:	.BLKW	2		;COUNT OF TRANSIT PACKETS SENT
L.LREC:	.BLKW	2		;COUNT OF PACKETS RECEIVED FOR LOCAL NSP
L.LXMT:	.BLKW	2		;COUNT OF PACKETS TRANSMITTED FOR LOCAL NSP
L.TCON:	.BLKW			;COUNT OF TRANSIT PACKETS LOST BY CONGESTION
L.LCON:	.BLKW			;COUNT OF LOCAL NSP PACKETS LOST BY CONGESTION
L.DOWN:	.BLKW			;NUMBER OF TIMES CIRCUIT WENT DOWN
L.IERR:	.BLKW			;COUNT OF INITIALIZATION FAILURES
L.ECNT:				;END OF COUNTERS
; *** END OF ORDERED SECTION
L.COST:	.BLKB			;CIRCUIT COST FOR THIS CIRCUIT
L.ANUM:	.BLKB			;Adjacency number for this circuit	;007
L.HTMR:	.BLKW			;HELLO TIMER INTERVAL
L.XBUF:	.BLKB			;COUNT OF TRANSMIT BUFFERS OUTSTANDING
L.NBUF:	.BLKB			;COUNT OF LOCAL NSP TRANSMIT BUFFERS
L.CINT:	.BLKW			;COUNTER TIMER INTERVAL
L.CTMR:	.BLKW			;TIMER FOR COUNTER LOGGING
L.QMAX:	.BLKW			;Originating queue limit		;000
L.RTMR:	.BLKW			;Recall timer				;000
L.MRTR:	.BLKB			;Maximum broadcast routers allowed	;007
L.TRTR:	.BLKB			;Total number of routers		;007
L.DDEP:				;START OF DEVICE DEPENDENT PORTION

; DDFLAG VALUES

.BSECT
	.BLKB	77		;ONLY USED BY DRIVER			;000
LF.V41:	.BLKB	.		;DDCMP V4.1 mode flag (XM only)		;000
LF.DMV:	.BLKB	.		;FLAG SET BY 'INIT.SYS', READ BY NET MGMT ETC
				; FOR XM, FLAG =0 IF DMC, =1 IF DMR
				; FOR XD, FLAG =0 IF DMP, =1 IF DMV

.BSECT	,NOCREF			;In L.FLAG

LF.AEN:				;(Broadcast) Send next hello to all end-nodes ;007
LF.VMW:	.BLKB	5		;(Point) Awaiting verify from remote node
LF.LSS:	.BLKB	22		;Link dependant sublayer state		;007
LF.STA:	.BLKB	10		;(Control) Circuit state
LF.DES:				;(Broadcast) We are designated router	;007
LF.RVM:	.BLKB	.		;(Point) Remote node wants verify msg	;007
LF.VER:	.BLKB	.		;(Point) Verification required on this circuit *
LF.BRO:	.BLKB	.		;Circuit is a broadcast circuit		;007
LF.FIP:	.BLKB	.		;Circuit has FIP request pending	;007
LF.RST:	.BLKB	.		;Circuit is restartable *
LF.ANS:	.BLKB	.		;(Point) Circuit is operating in answer mode *
LF.CON:	.BLKB	.		;Set if ECL message rejected for congestion
LF.RTM:	.BLKB	.		;Routing message pending on this circuit ;000
LF.TRA:	.BLKB	.		;Trace enabled for this circuit *
LF.LSM:	.BLKB	.		;Link sublayer message in progress 	;012
LF.SRM:	.BLKB	.		;SRM summary flag for this circuit	;000

; Note: The *'ed values above must not be changed, as they are passed
; by the "circuit on" network management function.

; Define control sublayer circuit states (LF.STA)

.DSECT	,NOCREF
LS$OFF:	.BLKB	10		;Circuit is off				;007
LS$ON:	.BLKB	10		;Circuit is active			;007
LS$RES:	.BLKB	10		;Circuit is awaiting restart		;007
LS$SER:	.BLKB	10		;Reserved for service state		;007

; Define link dependant sublayer states (LF.LSS) for the DDCMP sublayer.

.DSECT	,NOCREF
LD$RUN:	.BLKB	2		;Circuit is fully up			;007
LD$IN3:	.BLKB	2		;Waiting to send Ph III init message	;020
LD$VFY:	.BLKB	2		;Waiting to send or receive vfy message	;007
LD$INI:				;Waiting for init message		;007
LD$MAX:				;Maximum valid type (times two)		;007

; Define link dependant sublayer states, same order as entires above
.MACRO	$STATES	Z
Z'.IRP	STATE,<RUN,IN3,VFY,INI>
.ENDM	$STATES

; Define ethernet link dependant sublayer states

.DSECT	,NOCREF								;007
LD$RUN:	.BLKB	2		;Circuit is fully up			;007
LD$ADR:	.BLKB	2		;Setting physical address		;007
LD$MUL:	.BLKB	2		;Setting multicast reception		;007
	.BLKB	2		;Reserved				;007

.MCALL	.MDELET
.MDELET	$NETDDB
.ENDM	$NETDDB
.MACRO	$NSPMSG
.BSECT

	.BLKB	13		;LOW TWO BITS = 0,0 FOR NSP MESSAGES
MFL.MT:	.BLKB	144		;MESSAGE TYPE BITS
MFL$DM	=	0*4		;DATA MESSAGE
MFL$AK	=	1*4		;ACKNOWLEDGEMENT MESSAGE
MFL$CM	=	2*4		;CONTROL MESSAGE
;	=	3*4		;RESERVED

MFL.ST:	.BLKB	20		;MESSAGE SUBTYPE BITS

MFL.RS	=	^C<MFL.MT!MFL.ST>

; MESSAGE SUBTYPE FIELD DEFINITIONS (BITS 4,5,6)

; MESSAGE SUBTYPE FIELD DEFINITIONS FOR DATA MESSAGES (MFL$DM)

.BSECT
	.BLKB	17
MDM.LS:	.BLKB	.		;1 --> INT/LS MESSAGE, 0 --> DATA MESSAGE
MDM.BM:				;BEGINNING-OF-MESSAGE SEGMENT (MDM.LS =0 ONLY)
MDM.IN:	.BLKB	.		;INTERRUPT MESSAGE            (MDM.LS =1 ONLY)
MDM.EM:				;END-OF-MESSAGE SEGMENT       (MDM.LS =0 ONLY)
MDM.XX:	.BLKB	.		;RESERVED                     (MDM.LS =1 ONLY)

; MESSAGE SUBTYPE FIELD DEFINITIONS FOR ACKNOWLEDGEMENT MESSAGES (MFL$AK)

.BSECT
	.BLKB	157		;
MFL.ST:	.BLKB	20		;ACK MESSAGE SUBTYPE BITS
MAK$DM	=	0*20		;DATA MESSAGE ACK
MAK$LS	=	1*20		;INT/LS MESSAGE ACK
MAK$CI	=	2*20		;CONNECT ACK
;	=	3*20		;RESERVED
;	=	4*20		;RESERVED
;	=	5*20		;RESERVED
;	=	6*20		;RESERVED
;	=	7*20		;RESERVED

; MESSAGE SUBTYPE FIELD DEFINITIONS FOR CONTROL MESSAGES (MFL$CM)

.BSECT
	.BLKB	157		;
MFL.ST:	.BLKB	20		;CONTROL MESSAGE SUBTYPE BITS
MCM$NO	=	0*20		;NOP MESSAGE
MCM$CI	=	1*20		;CONNECT INITIATE
MCM$CC	=	2*20		;CONNECT CONFIRM MESSAGE
MCM$DI	=	3*20		;DISCONNECT INITIATE
MCM$DC	=	4*20		;DISCONNECT CONFIRM
MCM$NI	=	5*20		;PHASE 2 NODE INIT
MCM$C2	=	6*20		;Retransmitted CI			;000
;	=	7*20		;RESERVED

; LSFLAGS FIELD DEFINITIONS

.BSECT
	.BLKB	2		;
LSF.BP:	.BLKB	11		;BACK PRESSURE SWITCH MODIFICATION BITS
LSF$NC	=	0		;NO CHANGE
LSF$OF	=	1		;STOP DATA  (OFF)
LSF$ON	=	2		;START DATA (ON)
;	=	3		;RESERVED

LSF.NT:	.BLKB	4		;INTERPRETATION OF FCVAL FIELD IN THIS LS MSG
LSF$DR	=	0*4		;DATA SEGMENT REQUEST COUNT
LSF$IR	=	1*4		;INTERRUPT    REQUEST COUNT
;	=	2*4		;RESERVED
;	=	3*4		;RESERVED

	.BLKB	.		;RESERVED
	.BLKB	.		;RESERVED
	.BLKB	.		;RESERVED
	.BLKB	.		;RESERVED

LSF.RS	= ^C<LSF.BP!LSF$IR>

; ACKNUM FIELD DEFINITIONS

.BSECT
	.BLKB	7776		;
AKN.SN:	.BLKB	20001		;Segment number field			;015

AKN.QL:	.BLKB	10000		;Message qualifier bits			;015
AKN.CO:	.BLKB	.		;Congestion encountered			;012
AKN.FL:	.BLKB	.		;Always set for ACKNUM field

; ACK types

AKN$AK	=	0*10000		;ACK
AKN$NK	=	1*10000		;NAK
AKN$CA	=	2*10000		;Cross-subchannel ACK			;000
AKN$CN	=	3*10000		;Cross-subchannel NAK			;000

; SEGNUM FIELD DEFINITIONS

.BSECT
	.BLKB	7776		;
SGN.SN:	.BLKB			;Segment number field			;015
SGN.AD:	.BLKB	50000		;ACK delay flag				;015
SGN.RS:	.BLKB	20000		;Unused bits				;012
SGN.FL:	.BLKB	.		;Always zero for SEGNUM field

; Services field definitions

.BSECT
	.BLKB	.		;Reserved - must be set			;016
	.BLKB	12		;Reserved				;016
SVC.FL:	.BLKB	4		;Flow control options			;016
SVC.CR:	.BLKB	.		;Cryptographic services flag		;016
	.BLKB	.		;Reserved				;016
	.BLKB	.		;Reserved				;016
	.BLKB	.		;Reserved				;016

; INFO FIELD DEFINITIONS

.BSECT
	.BLKB	2		;
NFO.VE:	.BLKB	1		;VERSION CODE

NFO$32	=	0		;NSP VERSION 3.2
NFO$31	=	1		;NSP VERSION 3.1
NFO$40	=	2		;NSP version 4.0			;000
NFO$41	=	3		;NSP version 4.1			;012

	.BLKB	.		;RESERVED
	.BLKB	.		;RESERVED
	.BLKB	.		;RESERVED
	.BLKB	.		;RESERVED
	.BLKB	.		;RESERVED
	.BLKB	.		;RESERVED

NFO.RS	= ^C<NFO.VE>

.MCALL	.MDELET
.MDELET	$NSPMSG
.ENDM	$NSPMSG
.MACRO	$TRNMSG
.BSECT
TRNFLG:	.BLKB	15		;TRANSPORT CONTROL MESSAGE FLAG BIT
CTL.MT:	.BLKB	2		;CONTROL MESSAGE TYPE
CTL$TI	=	0*2		;TRANSPORT INIT
CTL$TV	=	1*2		;TRANSPORT VERIFICATION
CTL$HI	=	2*2		;HELLO/TEST MESSAGE
CTL$RT	=	3*2		;ROUTING MESSAGE
CTL$AR	=	4*2		;Level 2 routing message		;000
CTL$RH	=	5*2		;Router hello message (Ethernet)	;000
CTL$EH	=	6*2		;Endnode hello message (Ethernet)	;000
;	=	7*2		;RESERVED

CTL.RS	=	^C<TRNFLG!CTL.MT> ;RESERVED BITS

; ROUTE MESSAGE LAYOUT

.BSECT
	.BLKB	1776
COST:	.BLKB	74001		;COST FIELD
HOPS:	.BLKB	2000		;HOPS (DISTANCE) FIELD
RM.SRM:				;Extra bit used as SRM flag in matrix	;000
RM.RS:	.BLKB	.		;RESERVED

; OTHER VALUES USED WITH ROUTE MESSAGES

INFCST	=	COST		;INFINITE COST VALUE
INFHOP	=	HOPS		;INFINITE HOPS VALUE (AS STORED IN MSG)
INFIN	=	<INFHOP!INFCST>	;UNREACHABLE MEANS BOTH VALUES ARE SET INFINITE
HOP	=	INFCST+1	;VALUE FOR A SINGLE HOP (AS STORED IN MSG)

; TRANSPORT INIT MESSAGE FLAGS

.BSECT
	.BLKB	2
TI.NTY:	.BLKB	1		;NODE TYPE FIELD
;	=	0		;RESERVED
NTY$AR	=	1		;Area routing node			;000
NTY$FU	=	2		;Level 1 routing node
NTY$SM	=	3		;End node

TI.VFY:	.BLKB	.		;VERIFICATION REQUIRED
TI.BLO:	.BLKB	.		;Blocking requested (X.25)		;000

TI.RS	=	^C<TI.NTY!TI.VFY> ;RESERVED BITS

.MCALL	.MDELET
.MDELET	$TRNMSG
.ENDM	$TRNMSG
.MACRO	$RTEMSC

; Ethernet end-node cache

.DSECT	,NOCREF

EC.NXT:	.BLKW			;Link to next in this list
EC.ADR:	.BLKW			;Node number this entry describes
EC.HOP:	.BLKW			;Adjacent node to send message to
EC.SIZ:				;Size of entry in bytes

.DSECT	140000,NOCREF

	.BLKW			;Size of end-node cache in bytes (BF.SIZ)
ENCCUR:	.BLKW			;Root of current minute entries
ENCOLD:	.BLKW			;Root of last minute entries
ENCIDL:	.BLKW			;Root of unused entries
ENCDAT:				;Cache data begins here

ENCTMR	=:	40.		;Interval at which end-node cache entries
				;are moved from list to list (this value
				;gives an average life of 60 seconds)

; Routing matrix start address

MTXVIR	=:	140002		;Virtual address of routing matrix start

.MCALL	.MDELET
.MDELET $RTEMSC
.ENDM	$RTEMSC
.MACRO	$RTEHDR


.BSECT				;Route flags
TRNFLG:	.BLKB	.		;Zero to indicate non-control message	;009
RTFLG:	.BLKB	.		;Set to indicate routing header
RF.LPF:	.BLKB	.		;Long (Ethernet) packet format		;000
RF.RQR:	.BLKB	.		;Return to sender request
RF.RTS:	.BLKB	.		;Message being returned to sender
RF.LAN:	.BLKB	.		;Message originated on this LAN (long only)
RF.FUT:	.BLKB	.		;Future version message			;009
	.BLKB	.		;Pad flag - must be zero

.DSECT				;Short format route header layout

RTFLGS:	.BLKB			;Flags byte
DSTADR:	.BLKB	2		;Destination address
SRCADR:	.BLKB	2		;Source address
VISITS:	.BLKB			;Visit count and flags
F.RTHD:				;Header size

.DSECT				;Long format route header layout

RTFLGS:	.BLKB			;Flags byte
	.BLKB			;Reserved for destination area
	.BLKB			;Reserved for destination subarea
R.DEST:	.BLKB	6		;Destination address
	.BLKB			;Reserved for source area
	.BLKB			;Reserved for source subarea
R.SRC:	.BLKB	6		;Source address
R.FLG2:	.BLKB			;Secondary flags
R.VIS:	.BLKB			;Visit count
	.BLKB			;Reserved for service class
	.BLKB			;Reserved for protocol type
R.RTHD:				;Size of long-format route header

.BSECT				;Field in address value			;000
	.BLKB	1776							;000
TI.NOD:	.BLKB	174001		;Node number within area		;000
TI.ARE:	.BLKB			;Area number				;000

AREA	=:	TI.NOD+1	;Multiplier for area # to TI.ARE field	;000

.BSECT				;In VISITS
	.BLKB	76							;012
VC.CNT:	.BLKB			;Visit count				;012
	.BLKB	.		;Reserved				;012
VC.CON:	.BLKB	.		;Congestion encountered en-route	;012

.BSECT				;In R.FLG2
F2.CON:	.BLKB	.		;Congestion encountered en-route	;012
	.BLKB	.		;Reserved				;012
	.BLKB	.		;Reserved				;012
	.BLKB	.		;Reserved				;012
	.BLKB	.		;Reserved				;012
	.BLKB	.		;Reserved				;012
	.BLKB	.		;Reserved				;012
	.BLKB	.		;Reserved				;012
	.BLKB	.		;Reserved				;012
	.BLKB	.		;Reserved				;012
	.BLKB	.		;Reserved				;012
	.BLKB	.		;Reserved				;012
	.BLKB	.		;Reserved				;012
	.BLKB	.		;Reserved				;012
	.BLKB	.		;Reserved				;012
	.BLKB	.		;Reserved				;012

.MCALL	.MDELET
.MDELET	$RTEHDR
.ENDM	$RTEHDR
.MACRO	$DMCMOD
.BSECT	HIGH

MX$MNT:	.BLKB	.		;Maintenance Mode if set
MX$HOS:	.BLKB	.		;Hangup on Error or Timeout
MX$HDX:	.BLKB	.		;Set Half Duplex Protocol
MX$SEC:	.BLKB	.		;Set Secondary Station mode
	.BLKB	.		;Reserved, must be zero
	.BLKB	.		;Reserved, must be zero
MX$LUL:	.BLKB	.		;Loop Line unit (Line Unit Loopback)
	.BLKB	.		;Reserved, must be zero

.MCALL	.MDELET
.MDELET	$DMCMOD
.ENDM	$DMCMOD
.MACRO	$DMPMOD
.DSECT

MY$DMC:	.BLKB			;DMC-compatible point-to-point mode
MY$PTP:	.BLKB			;non-DMC-compatible point-to-point mode
MY$MPM:	.BLKB			;Multipoint Master
MY$MPS:	.BLKB			;Multipoint slave
	.BLKB	.		;Reserved, must be zero
	.BLKB	.		;Reserved, must be zero
	.BLKB	.		;Reserved, must be zero
	.BLKB	.		;Reserved, must be zero
	.BLKB	.		;Reserved, must be zero
MY$MNT:
MY$DOG:	.BLKB	.		;Allow diagnostic functions
MY$MOP:	.BLKB	.		;Set Maintenance mode
	.BLKB	.		;Reserved, must be zero
MY$HDX:	.BLKB	.		;Set Half Duplex protocol
	.BLKB	.		;Reserved, must be zero
	.BLKB	.		;Reserved, must be zero
	.BLKB	.		;Reserved, must be zero
MY$LUL:	.BLKB	.		;Loop Line unit (Line Unit Loopback)

.MCALL	.MDELET
.MDELET	$DMPMOD
.ENDM	$DMPMOD
	.MACRO	MANDF$ LST
 
	.IIF 	NB LST.LIST
;
; OBJECT TYPES
;
	MO$NIC	= 19.			;NICE LISTENER
	MO$MIR  = 25.			;LOOPBACK MIRROR
;
; FUNCTION CODES
;
	MF$EVT	= 1.			;EVENT
	MF$CHA	= 19.			;CHANGE PARAMETER
	MF$DUM	= 16.			;REQUEST UP-LINE DUMP
	MF$LOA	= 15.			;REQUEST DOWN-LINE LOAD
	MF$REA	= 20.			;READ INFORMATION
	MF$SYS	= 22.			;SYSTEM-SPECIFIC FUNCTION
	MF$TES	= 18.			;TEST
	MF$TRI	= 17.			;TRIGGER BOOTSTRAP
	MF$ZER	= 21.			;ZERO COUNTERS
 
	MO$OPT	= 1.			;OFFSET TO OPTION-BYTE.
	ML$SYS	= 2.			;PREFIX LNGTH FOR SYS-SPECIFIC MSGS.
;
; SPECIAL FUNCTIONS TO NETWORK MANAGEMENT ACP (RSX)
;
	MF$BYE  = 192.			; LOGOUT USER
	MF$TRA  = 193.			; TRANSLATE
;
; OPTION MASKS
;
;    COMMON TO CHANGE PARAMETER, READ INFORMATION AND ZERO COUNTERS
;
	MO$ENT	= 017			;ENTITY TYPE MASK
;
;    COMMON TO DUMP, LOAD, TEST OR TRIGGER
;
	MO$LIN	= 001			;IDENTIFY TARGET BY LINE ID
	MO$NOD	= 000			;IDENTIFY TARGET BY NODE ID
	MO$CIR	= 003			;IDENTIFY TARGET BY CIRCUIT-ID (IV)
;
;    COMMON TO CHANGE PARAMETER OR READ INFORMATION
;
	MO$VOL	= 000			;VOLATILE PARAMETERS
	MO$PER	= 200			;PERMANENT PARAMETERS
;
;    CHANGE PARAMETER
;
	MO$SET	= 000			;SET PARAMETER
	MO$CLE	= 100			;CLEAR PARAMETER
;
;    LOAD
;
	MO$DPR	= 000			;DEFAULT PROGRAM REQUEST
	MO$PRO	= 002			;PROGRAM REQUEST INCLUDED
;
;    READ INFORMATION
;
	MO$INF	= 160			;INFORMATION TYPE MASK
 
	MO$CHA	= 040			;CHARACTERISTICS
	MO$COU	= 060			;COUNTERS
	MO$EVE	= 100			;EVENTS
	MO$STA	= 020			;STATUS
	MO$SUM	= 000			;SUMMARY
;
;    TEST
;
	MO$DAC	= 000			;DEFAULT ACCESS CONTROL
	MO$ACC	= 200			;ACCESS CONTROL INCLUDED
;
;    TRANSLATE
;
;ads	MO$ALI  = 000			;TRANSLATE ALIAS -> NAME	;4.01
;ads					;No more alias stuff		;4.01
	MO$NAM  = 001			;TRANSLATE NAME -> ADDRESS
	MO$ADD  = 002			;TRANSLATE ADDRESS -> NAME
;
;    ZERO
;
	MO$ZER	= 000			;ZERO ONLY
	MO$REA	= 200			;READ AND ZERO 

;
; SYSTEM TYPES
;
	MS$RST	= 1			;RSTS/E
	MS$RSX	= 2			;RSX FAMILY
	MS$TOP	= 3			;TOPS-20
	MS$VMS	= 4			;VMS
	MS$RT	= 5			;RT-11
	MS$CT	= 6			;CT
	MS$CS	= 7			;COMMUNICATIONS SERVER
;
; ENTITY TYPES
;
	ME$NON	= -1.			;NONE
	ME$NOD	= 0.			;NODE
	ME$LIN	= 1.			;LINE
	ME$LOG	= 2.			;LOGGING
	ME$CIR	= 3.			;CIRCUIT 			
	ME$MOD	= 4.			;MODULE 			
	ME$ARE	= 5.			;AREA	(IV)
 
	ME$LNK	= 6.			;LINK			(RSTS)	;1.56
	ME$OBJ	= 7.			;OBJECT			  (RSTS & RSX)
 
	ME$AL2  = 3.			;ALIAS				(RSX)
	ME$OB2	= 4.			;OBJECT				(RSX)
	ME$PRO	= 5.			;PROCESS			(RSX)
	ME$SYS	= 6.			;SYSTEM				(RSX)
	ME$ALI  = 8.			;ALIAS				(RSX)
 
	ME$EXA	= 0.		;EXECUTOR NODE ADDRESS (BY CONVENTION)
	ME$EXE	= 200		;EXECUTOR INDICATOR FLAG FOR RESPONSE MESSAGES
;
; ENTITY IDENTIFICATION FORMAT TYPES
;
	MF$ACT	= -2.			;ACTIVE
	MF$ALL	= -3.			;ALL
	MF$KNO	= -1.			;KNOWN
	MF$LOO	= -3.			;LOOP
	MF$ADD	= 0.			;NODE ADDRESS, AREA NUMBER
	MF$ADJ	= -4.			;ADJACENT			(IV)
	MF$SIG	= -5.			;SIGNIFICANT			(IV)
;
; LOGGING SINK TYPES
;
	ML$CON	= 1.			;CONSOLE
	ML$FIL	= 2.			;FILE
	ML$MON	= 3.			;MONITOR

;
; COUNTER DATA TYPE VALUES
;
	MC$COU	= 100000		;COUNTER INDICATOR
	MC$WID	= 060000		;WIDTH FIELD MASK
	MC$WIL	= 020000		;WIDTH FIELD LOW BIT
	MC$WIH	= 040000		;WIDTH FIELD HIGH BIT
	MC$MAP	= 010000		;BITMAPPED INDICATOR
	MC$TYP	= 007777		;TYPE MASK
	MC$W08	= 020000		;WIDTH = 8 BITS
	MC$W16	= 040000		;WIDTH = 16 BITS
	MC$W32	= 060000		;WIDTH = 32 BITS
;
; PARAMETER DATA TYPE VALUES
;
	MT$MAX	= 31.			;MAXIMUM FIELDS WITHIN CODED MULTIPLE
 					;(CHANGED FOR IV FROM 15)
	MT$TYP	= 007777		;TYPE MASK
	MT$COD	= 200			;CODED INDICATOR
	MT$MUL	= 100			;CODED MULTIPLE INDICATOR
	MT$CLE	= 077			;CODED LENGTH MASK
	MT$ASC	= 100			;ASCII IMAGE INDICATOR
	MT$USD	= 000			;UNSIGNED DECIMAL NUMBER
	MT$SGD	= 020			;SIGNED DECIMAL NUMBER
	MT$HEX  = 040			;HEXADECIMAL NUMBER
	MT$OCT	= 060			;OCTAL NUMBER
	MT$NLE	= 017			;NUMBER LENGTH MASK
	MT$NTY	= 060			;NUMBER TYPE MASK
;
; LOGGING DATA TYPE VALUES
;
	ML$CLS  = 0*40000		;SINGLE CLASS
	ML$ALL  = 2*40000		;ALL EVENTS FOR CLASS
	ML$KNO  = 3*40000		;KNOWN EVENTS

;
; COUNTER TYPES
;
 
;
; COMMON TO MORE THAN ONE ENTITY
;
	MC$TIM	= 0			;SECONDS SINCE LAST ZEROED
 
;
; DATA LINK LINE COUNTERS
;
	MC$LRP	= 1100.			;REMOTE STATION ERRORS	(DDCMP, LAPB)
	MC$LLP	= 1101.			;LOCAL STATION ERRORS	(DDCMP, LAPB)
;
; CIRCUITS
;
	MC$CAP	= 800.			;TERMINATING PACKETS RECEIVED
	MC$CDP	= 801.			;ORIGINATING PACKETS SENT
	MC$CAC	= 802.			;TERMINATING CONGESTION LOSS
	MC$CCL	= 805.			;CORRUPTION LOSS
	MC$CTR	= 810.			;TRANSIT PACKETS RECEIVED
	MC$CTS	= 811.			;TRANSIT PACKETS SENT
	MC$CTL	= 812.			;TRANSIT CONGESTION LOSS
	MC$CLD	= 820.			;CIRCUIT DOWN
	MC$CIF	= 821.			;INITIALIZATION FAILURE
;
; LINE AND CIRCUIT
; 
	MC$LBR	= 1000.			;BYTES RECEIVED
	MC$LBS	= 1001.			;BYTES SENT
	MC$MBR	= 1002.			;MULTICAST BYTES RECEIVED	(IV)
	MC$LDR	= 1010.			;DATA BLOCKS RECEIVED
	MC$LDS	= 1011.			;DATA BLOCKS SENT
	MC$BLR	= 1012.			;MULTICAST BLOCKS RECEIVED	(IV)
	MC$BID	= 1013.			;BLOCKS SENT, INITIALLY DEFERRED (IV)
	MC$BSC	= 1014.			;BLOCKS SENT, SINGLE COLLISION	 (IV)
	MC$BMC	= 1015.			;BLOCKS SENT, MULTIPLE COLLISIONS (IV)
	MC$LDI	= 1020.			;DATA ERRORS INBOUND
	MC$LDO	= 1021.			;DATA ERRORS OUTBOUND
	MC$LRR	= 1030.			;REMOTE REPLY TIMEOUTS
	MC$LLR	= 1031.			;LOCAL REPLY TIMEOUTS
	MC$LRB	= 1040.			;REMOTE BUFFER ERRORS
	MC$LLB	= 1041.			;LOCAL BUFFER ERRORS
	MC$LSI	= 1050.			;SELECTION INTERVALS ELAPSED
	MC$LST	= 1051.			;SELECTION TIMEOUTS
	MC$ESF	= 1060.			;SEND FAILURE			(IV)
	MC$EDF	= 1061.			;COLLISION DETECT FAILURE	(IV)
	MC$ERF	= 1062.			;RECEIVE FAILURE		(IV)
	MC$EUD	= 1063.			;UNRECOGNIZED FRAME DESTINATION	(IV)
	MC$EDO	= 1064.			;DATA OVERRUN			(IV)
	MC$ESB	= 1065.			;SYSTEM BUFFER UNAVAILABLE	(IV)
	MC$EUB	= 1066.			;USER BUFFER UNAVAILABLE	(IV)
;
; NODES
;
;   GENERAL
;
	MC$NBR	= 600.			;USER BYTES RECEIVED
	MC$NBS	= 601.			;USER BYTES SENT
	MC$UMR	= 602.			;USER MESSAGES RECEIVED
	MC$UMS	= 603.			;USER MESSAGES SENT
	MC$TBR	= 608.			;TOTAL BYTES RECEIVED
	MC$TBS	= 609.			;TOTAL BYTES SENT
	MC$NMR	= 610.			;TOTAL MESSAGES RECEIVED
	MC$NMS	= 611.			;TOTAL MESSAGES SENT
	MC$NCR	= 620.			;CONNECTS RECEIVED
	MC$NCS	= 621.			;CONNECTS SENT
	MC$NRT	= 630.			;RESPONSE TIMEOUTS
	MC$NRE	= 640.			;RECEIVED RESOURCE ERRORS
;
;   EXECUTOR ONLY - TRANSPORT COUNTERS
;
	MC$NML	= 700.			;MAXIMUM LOGICAL LINKS ACTIVE
	MC$NAP	= 900.			;AGED PACKET LOSS
	MC$NNU	= 901.			;NODE UNREACHABLE PACKET LOSS
	MC$NNO	= 902.			;NODE OUT-OF-RANGE PACKET LOSS
	MC$NOP	= 903.			;OVERSIZED PACKET LOSS
	MC$NPF	= 910.			;PACKET FORMAT ERROR
	MC$NPR	= 920.			;PACKET ROUTING UPDATE LOSS
	MC$NVR	= 930.			;VERIFICATION REJECT
 
	MC$NRN	= 2100.+100.		;CURRENT NO. OF REACHABLE NODES (RSTS)
	MC$NRM	= 2100.+101.		;MAX COUNT OF REACHABLE NODES   (RSTS)
;
; MODULES (X25)
;
;   PROTOCOL MODULES
;
	MC$XBR	= 1000.			;BYTES RECEIVED
	MC$XBS	= 1001.			;BYTES SENT
	MC$XDR	= 1010.			;DATA BLOCKS RECEIVED
	MC$XDS	= 1011.			;DATA BLOCKS SENT
	MC$XCR	= 1200.			;CALLS RECEIVED
	MC$XCS	= 1201.			;CALLS SENT
	MC$XFR	= 1210.			;FAST SELECTS RECEIVED
	MC$XFS	= 1211.			;FAST SELECTS SENT
	MC$XMS	= 1220.			;MAXIMUM SWITCHED CIRCUITS ACTIVE
	MC$XMC	= 1221.			;MAXIMUM CHANNELS ACTIVE
	MC$XRC	= 1230.			;RECEIVED CALL RESOURCE ERRORS
	MC$XLR	= 1240.			;LOCALLY INITIATED RESETS
	MC$XRR	= 1241.			;REMOTELY INITIATED RESETS
	MC$XNR	= 1242.			;NETWORK INITIATED RESETS
	MC$XRS	= 1250.			;RESTARTS
;
;   SERVER MODULES
;
	MC$XMA	= 200.			;MAXIMUM CIRCUITS ACTIVE
	MC$XCJ	= 210.			;INCOMING CALLS REJECTED, NO RESOURCES
	MC$XLJ	= 211.			;LOGICAL LINKS REJECTED, NO RESOURCES

;
; PARAMETER TYPES
;
; RSX SPECIFIC PARAMETER RANGE:		2300. TO 2499.
; RT-11 SPECIFIC PARAMETER RANGE:	2900. TO 3099.
; RSTS/E SPECIFIC PARAMETER RANGE:	2100. TO 2299.
;
;    COMMON TO MORE THAN ONE ENTITY
;
	MP$STA	= 0.			;STATE
	MP$SUB	= 1.			;SUBSTATE	(CIRCUITS AND LINES)
	MP$SER	= 100.			;SERVICE	(CIRCUITS AND LINES)
	MP$LCT	= 110.			;COUNTER TIMER	(CIRCUITS AND LINES)
;
;    ALIAS
;
	MP$SCO	= 100.			;SCOPE
	MP$DES	= 110.			;DESTINATION
;
;    LINE
;

	MP$DEV	= 1100.			;DEVICE
	MP$BFQ	= 1105.			;RECEIVE BUFFERS
	MP$CON	= 1110.			;CONTROLLER
	MP$DUP	= 1111.			;DUPLEX			(V2.0)
	MP$PRO	= 1112.			;PROTOCOL TYPE		(V2.0)
	MP$CLK	= 1113.			;CLOCK
	MP$STI	= 1120.			;SERVICE TIMER 		(DDCMP ONLY)
	MP$NTI	= 1121.			;RETRANSMIT TIMER	(V2.1)
	MP$HBT	= 1122.			;HOLDBACK TIMER			(LAPB)
	MP$LMB	= 1130.			;MAXIMUM BLOCK			(LAPB)
 	MP$MRT	= 1131.			;MAXIMUM RETRANSMITS		(LAPB)
	MP$MWN	= 1132.			;MAXIMUM WINDOW			(LAPB)
	MP$SCT	= 1150.			;SCHEDULING TIMER		(V2.2)
	MP$DDT	= 1151.			;DEAD TIMER			(V2.2)
	MP$DLT	= 1152.			;DELAY TIMER			(V2.2)
	MP$STT	= 1153.			;STREAM TIMER			(V2.2)
	MP$HWA	= 1160.			;HARDWARE ADDRESS	(ETHERNET);4.02
	MP$VRF	= 2100.+11.		;VERIFICATION			(RSTS)
 
	MP$OWN  = 2300.			;OWNER				(RSX)
	MP$CCS  = 2300.+10.		;CONTROLLER CSR			(RSX)
	MP$UCS  = 2300.+11.		;UNIT CSR			(RSX)
	MP$VEC  = 2300.+12.		;VECTOR				(RSX)
	MP$PRI  = 2300.+13.		;PRIORITY			(RSX)
	MP$MDE	= 2300.+21.		;DEAD POLLING RATIO		(RSX)
	MP$LLO  = 2300.+30.		;LOCATION			(RSX)
;*D	MP$CCS	= 2900.+10.		;CONTROLLER CSR			(RT)
;*D	MP$UCS	= 2900.+11.		;UNIT CSR			(RT)
;*D	MP$VEC	= 2900.+12.		;VECTOR				(RT)
;*D	MP$PRI	= 2900.+13.		;PRIORITY			(RT)
;
;    CIRCUIT
;
	MP$SPH	= 120.			;SERVICE PHYSICAL ADDRESS (ETHERNET)
	MP$SVS	= 121.			;SERVICE SUBSTATE	  (ETHERNET)
	MP$CND	= 200.			;CONNECTED NODE			(X25)
	MP$COB	= 201.			;CONNECTED OBJECT		(X25)
	MP$LOO	= 400.			;LOOPBACK NAME
	MP$ADJ	= 800.			;ADJACENT NODE
	MP$EDR	= 801.			;DESIGNATED ROUTER	  (ETHERNET)
	MP$BLO	= 810.			;BLOCK SIZE
	MP$OQL	= 811.			;ORIGINATING QUEUE LIMIT	(IV)
	MP$COS	= 900.			;COST
	MP$MRO	= 901.			;MAXIMUM ROUTERS	  (ETHERNET)
	MP$RPR	= 902.			;ROUTER PRIORITY	  (ETHERNET)
	MP$RTM	= 905.			;ROUTING TIMER
	MP$HTM	= 906.			;HELLO TIMER
	MP$LTM	= 907.			;LISTEN TIMER
	MP$BLK	= 910.			;BLOCKING			(X25)
	MP$MXR	= 920.			;MAXIMUM RECALLS		(X25)
	MP$RET	= 921.			;RECALL TIMER		(X25, IV)
	MP$NUM	= 930.			;NUMBER				(X25)
	MP$USR	= 1000.			;USER			(EXE OR X25)
	MP$PST	= 1010.			;POLLING STATE			(V2.2)
	MP$PSS	= 1011.			;POLLING SUBSTATE		(V2.2)
	MP$ONR	= 1100.			;OWNER			(EXE ONLY)
	MP$CLN	= 1110.			;LINE			(DDCMP ONLY)
	MP$CUS	= 1111.			;USAGE				(X25)
	MP$TYP	= 1112.			;TYPE
	MP$DTE	= 1120.			;DTE ADDRESS			(X25)
	MP$CHN	= 1121.			;CHANNEL			(X25)
	MP$CMB	= 1122.			;MAXIMUM DATA			(X25)
	MP$MXW	= 1123.			;MAXIMUM WINDOW			(X25)
	MP$TRI	= 1140.			;TRIBUTARY
	MP$BBT	= 1141.			;BABBLE TIMER			(V2.2)
	MP$XMT	= 1142.			;TRANSMIT TIMER			(V2.2)
	MP$MRB	= 1145.			;MAXIMUM BUFFERS 	(V2.2)
	MP$MXB	= 1146.			;MAXIMUM TRANSMITS		(V2.2)
	MP$BSA	= 1150.			;ACTIVE BASE			(V2.2)
	MP$INA	= 1151.			;ACTIVE INCREMENT		(V2.2)
	MP$BSI	= 1152.			;INACTIVE BASE			(V2.2)
	MP$INI	= 1153.			;INACTIVE INCREMENT		(V2.2)
	MP$TH3	= 1154.			;INACTIVE THRESHOLD		(V2.2)
	MP$BSD	= 1155.			;DYING BASE			(V2.2)
	MP$IND	= 1156.			;DYING INCREMENT		(V2.2)
	MP$TH2	= 1157.			;DYING THRESHOLD		(V2.2)
	MP$TH1	= 1158.			;DEAD THRESHOLD			(V2.2)
 
	MP$RES	= 2100.+10.		;AUTORESTART			(RSTS)
 
	MP$MAC	= 2300.+20.		;MULTIPOINT ACTIVE RATIO	(RSX)
;
;    LOGGING
;
	MP$LNA	= 100.			;NAME
	MP$SIN	= 200.			;SINK NODE
	MP$EVE	= 201.			;EVENTS
	MP$XXX	= 177777		;DUMMY FOR LOGGING SOURCE QUALIFIERS
;
;    MODULE
;
;      X.25-ACCESS MODULE
;
	MP$NET  = 300.			;NETWORK
	MP$NOD  = 310.			;NODE
	MP$AUS  = 320.			;USER
	MP$PAS  = 321.			;PASSWORD
	MP$ACC  = 322.			;ACCOUNT
	MP$ADS  = 2300.+10.		;DESTINATION
	MP$ANB  = 2300.+20.		;NUMBER
	MP$ASC  = 2300.+30.		;SCOPE
;
;      X.25-PROTOCOL MODULE
;
	MP$PCT  = 100.			;COUNTER TIMER
	MP$CAC  = 1000.			;ACTIVE CHANNELS
	MP$CAS  = 1010.			;ACTIVE SWITCHED
	MP$PDT  = 1100.			;DTE
	MP$GRO  = 1101.			;GROUP
	MP$PNT  = 1110.			;NETWORK
	MP$PLN  = 1120.			;LINE
	MP$PCH  = 1130.			;CHANNELS
	MP$CMX  = 1131.			;MAXIMUM CHANNELS
	MP$BDF  = 1140.			;DEFAULT DATA
	MP$WDF  = 1141.			;DEFAULT WINDOW
	MP$BMX  = 1150.			;MAXIMUM DATA
	MP$WMX  = 1151.			;MAXIMUM WINDOW
	MP$LMX  = 1152.			;MAXIMUM CLEARS
	MP$RMX  = 1153.			;MAXIMUM RESETS
	MP$SMX  = 1154.			;MAXIMUM RESTARTS
	MP$CAT  = 1160.			;CALL TIMER
	MP$CLT  = 1161.			;CLEAR TIMER
	MP$RST  = 1162.			;RESET TIMER
	MP$RRT  = 1163.			;RESTART TIMER
	MP$GDT  = 1170.			;DTE (QUALIFIED BY GROUP)
	MP$GNM  = 1171.			;NUMBER (QUALIFIED BY GROUP)
	MP$GTY  = 1172.			;TYPE (QUALIFIED BY GROUP)
	MP$PMC  = 2300.			;MAXIMUM CIRCUITS
;
;      X.25-SERVER MODULE
;
	MP$SCA  = 200.			;ACTIVE CIRCUITS
	MP$DST  = 300.			;DESTINATION
	MP$MXC  = 310.			;MAXIMUM CIRCUITS
	MP$SND  = 320.			;NODE
	MP$SUS  = 330.			;USER
	MP$SPS  = 331.			;PASSWORD
	MP$SAC  = 332.			;ACCOUNT
	MP$SOB  = 340.			;OBJECT
	MP$SPR  = 350.			;PRIORITY
	MP$CMK  = 351.			;CALL MASK
	MP$CVA  = 352.			;CALL VALUE
	MP$GRP  = 353.			;GROUP
	MP$SNU  = 354.			;NUMBER
	MP$SAD  = 355.			;SUBADDRESSES
	MP$5ST  = 2300.+10.		;STATE
;
;    NODE
;
	MP$NPA	= 10.			;PHYSICAL ADDRESS	  (ETHERNET)
	MP$IDE	= 100.			;IDENTIFICATION
	MP$MVE	= 101.			;MANAGEMENT VERSION
	MP$SLI	= 110.			;SERVICE CIRCUIT
	MP$SPA	= 111.			;SERVICE PASSWORD
	MP$SDV	= 112.			;SERVICE DEVICE
	MP$CPU	= 113.			;CPU
	MP$NHA	= 114.			;HARDWARE ADDRESS	  (ETHERNET)
	MP$SNV	= 115.			;SERVICE NODE VERSION	  (ETHERNET)
	MP$LOA	= 120.			;LOAD FILE
	MP$SLO	= 121.			;SECONDARY LOADER
	MP$TLO	= 122.			;TERTIARY LOADER
	MP$NDF	= 123.			;DIAGNOSTIC FILE	  (ETHERNET)
	MP$STY	= 125.			;SOFTWARE TYPE
	MP$SID	= 126.			;SOFTWARE IDENTIFICATION
	MP$DUM	= 130.			;DUMP FILE
	MP$SDU	= 131.			;SECONDARY DUMPER
	MP$DUA	= 135.			;DUMP ADDRESS
	MP$DUC	= 136.			;DUMP COUNT
	MP$OHO	= 140.			;HOST 		(READ ONLY PARAMETER)
	MP$IHO	= 141.			;HOST 		(WRITE ONLY PARAMETER)
	MP$LCO	= 150.			;LOOP COUNT
	MP$LLE	= 151.			;LOOP LENGTH
	MP$LWI	= 152.			;LOOP WITH
	MP$LAA	= 153.			;LOOP ASSISTANT PHYSICAL ADDRESS (IV)
	MP$LHE	= 154.			;LOOP HELP		  (ETHERNET)
	MP$LNO	= 155.			;LOOP NODE		  (ETHERNET)
	MP$LAS	= 156.			;LOOP ASSISTANT NODE	  (ETHERNET)
	MP$NCT	= 160.			;NODE COUNTER TIMER	  (ETHERNET)
	MP$NNA	= 500.			;NAME
	MP$NLI	= 501.			;CIRCUIT
	MP$ADD	= 502.			;ADDRESS
	MP$ITI	= 510.			;INCOMING TIMER
	MP$OTI	= 511.			;OUTGOING TIMER
	MP$NAC	= 600.			;ACTIVE LINKS
	MP$DEL	= 601.			;DELAY
	MP$NVE	= 700.			;NSP VERSION
	MP$MLK	= 710.			;MAXIMUM LINKS
	MP$DFA	= 720.			;DELAY FACTOR
	MP$DWE	= 721.			;DELAY WEIGHT
	MP$IAT	= 722.			;INACTIVITY TIMER
	MP$RFA	= 723.			;RETRANSMIT FACTOR
	MP$DTY	= 810.			;TYPE 			(DESTINATION)
	MP$DCO	= 820.			;COST 			(DESTINATION)
	MP$DHO	= 821.			;HOPS 			(DESTINATION)
	MP$DLI	= 822.			;CIRCUIT		(DESTINATION)
	MP$NND	= 830.			;NEXT NODE (TO DESTINATION)  (ETHERNET)
	MP$RVE	= 900.			;ROUTING VERSION
	MP$ETY	= 901.			;TYPE 			(EXECUTOR)
	MP$RTI	= 910.			;ROUTING TIMER
	MP$NSA  = 911.			;SUBADDRESSES
	MP$BRT	= 912.			;BROADCAST ROUTING TIMER  (ETHERNET)
	MP$MAD	= 920.			;MAXIMUM ADDRESS
	MP$MLN	= 921.			;MAXIMUM CIRCUITS
	MP$MCO	= 922.			;MAXIMUM COST
	MP$MHO	= 923.			;MAXIMUM HOPS
	MP$MVI	= 924.			;MAXIMUM VISITS
	MP$MAR	= 925.			;MAXIMUM AREA		  (ETHERNET)
	MP$MBN	= 926.			;MAXIMUM BROADCAST NONROUTERS (IV)
	MP$MBR	= 927.			;MAXIMUM BROADCAST ROUTERS (ETHERNET) ;4.00
	MP$AMC	= 928.			;AREA MAXIMUM COST	  (ETHERNET)
	MP$AMH	= 929.			;AREA MAXIMUM HOPS	  (ETHERNET)
	MP$MBU	= 930.			;MAXIMUM BUFFERS
	MP$BUS	= 931.			;BUFFER SIZE
	MP$SBS	= 932.			;SEGMENT BUFFER SIZE	 	(IV)
 
	MP$RPA	= 2300.+00.		;RECEIVE PASSWORD		(RSX)
	MP$TPA	= 2300.+01.		;TRANSMIT PASSWORD		(RSX)
	MP$VER	= 2300.+10.		;VERIFICATION STATE		(RSX)
 
;	MP$RPA	= 2900.+00.		;RECEIVE PASSWORD		(RT)
;	MP$TPA	= 2900.+01.		;TRANSMIT PASSWORD		(RT)
;	MP$VER	= 2900.+10.		;VERIFICATION STATE		(RT)
 
	MP$ROP	= 2100.+20.		;RECEIVE PASSWORD ORG		(RSTS)
	MP$RAP	= 2100.+21.		;RECEIVE PASSWORD ANS		(RSTS)
	MP$TOP	= 2100.+22.		;TRANSMIT PASSWORD ORG		(RSTS)
	MP$TAP	= 2100.+23.		;TRANSMIT PASSWORD ANS		(RSTS)
;ads	MP$AKA	= 2100.+24.		;ALIAS NAME		(RSTS)	;4.01
	MP$DAC	= 2100.+25.		;DEFAULT ACCOUNT		(RSTS)
	MP$DTQ	= 2100.+26.		;DATA XMIT QUEUE MAX		(RSTS)
	MP$INQ	= 2100.+27.		;INT/LS XMIT QUEUE MAX		(RSTS)
	MP$VOL	= 2100.+28.		;VOLATILE PARAM FILE NAME	(RSTS)
	MP$MRN	= 2100.+29.		;MAXIMUM NODE RECORD NUMBER  (RSTS,IV)
	MP$BPS	= 2100.+44.		;MAXIMUM BUFFER POOL SIZE    (RSTS,IV)
;
;    AREA	PHASE IV AREA PARAMETERS
;
	MP$ACO	= 820.			;AREA COST			(IV)
	MP$AHO	= 821.			;AREA HOPS			(IV)
	MP$ACI	= 822.			;AREA CIRCUIT			(IV)
	MP$ANN	= 830.			;AREA NEXT NODE (TO DESTINATION)(IV)
;
;    OBJECT
;
	MP$OAN	= 400.			;ACTIVE NAME			(RSX)
	MP$OAC	= 410.			;ACTIVE LINKS			(RSX)
	MP$ONA	= 500.			;NAME			  (RSTS, RSX)
	MP$OCO	= 510.			;COPIES				(RSX)
	MP$OUS	= 511.			;USER				(RSX)
	MP$OVE	= 520.			;VERIFICATION		        (RSX)
 
	MP$FIL	= 2100.+00.		;FILESPEC			(RSTS)
	MP$PA1	= 2100.+01.		;PARAMETER 1			(RSTS)
	MP$PA2	= 2100.+02.		;PARAMETER 2			(RSTS)
	MP$OBT	= 2100.+03.		;TYPE				(RSTS)
	MP$OVA	= 2100.+04.		;Verification			(RSTS)
;
;    LINK	(RSTS)
;
	MP$LLA	= 2100.+30.		;LOCAL LINK ADDRESS		(RSTS)
	MP$RLA	= 2100.+31.		;REMOTE LINK ADDRESS		(RSTS)
	MP$ULA	= 2100.+32.		;USER LINK ADDRESS		(RSTS)
	MP$LST	= 2100.+33.		;LINK STATE			(RSTS)
	MP$LND	= 2100.+34.		;LINK NODE NUMBER & NAME	(RSTS)
	MP$LOB	= 2100.+35.		;LINK OBJECT NAME		(RSTS)
	MP$LJB	= 2100.+36.		;LINK OBJECT JOB NUMBER		(RSTS)
	MP$RIB	= 2100.+37.		;LINK OBJECT RIB NUMBER		(RSTS)
	MP$LFL	= 2100.+38.		;LOCAL FLOW CONTROL OPTION	(RSTS)
	MP$RFL	= 2100.+39.		;REMOTE FLOW CONTROL OPTION	(RSTS)
	MP$LDR	= 2100.+40.		;LOCAL DATA REQUEST COUNT	(RSTS)
	MP$RDR	= 2100.+41.		;REMOTE DATA REQUEST COUNT	(RSTS)
	MP$LIR	= 2100.+42.		;LOCAL INTERRUPT REQ. COUNT	(RSTS)
	MP$RIR	= 2100.+43.		;REMOTE INTERRUPT REQ. COUNT	(RSTS)
;
;    PROCESS	(RSX)
;
	MP$PLO	= 10.			;LOCATION			(RSX)
	MP$PCO	= 20.			;MAXIMUM CONTROLLERS		(RSX)
	MP$PLI	= 21.			;MAXIMUM LINES			(RSX)
	MP$PAR	= 30.			;PARTITION			(RSX)
;
;    SYSTEM	(RSX)
;
	MP$ACB	= 10.			;ACTIVE CONTROL BUFFERS		(RSX)
	MP$ASB	= 20.			;ACTIVE SMALL BUFFERS		(RSX)
	MP$ALB	= 30.			;ACTIVE LARGE BUFFERS		(RSX)
	MP$MCB	= 110.			;MAXIMUM CONTROL BUFFERS	(RSX)
	MP$MSB	= 120.			;MAXIMUM SMALL BUFFERS		(RSX)
	MP$MLB	= 130.			;MAXIMUM LARGE BUFFERS		(RSX)
	MP$LBS	= 131.			;LARGE BUFFER SIZE		(RSX)
	MP$NRB	= 140.			;MINIMUM RECEIVE BUFFERS 	(RSX)
	MP$NPT	= 2900.+00.		;NET POOL: TOTAL BYTES		(RT)
	MP$NPF	= 2900.+10.		;NET POOL: NUMBER OF FRAGMENTS	(RT)
	MP$NPL	= 2900.+20.		;NET POOL: LARGEST FRAGMENT	(RT)
;
;    TRACE (RSX)
;
	MP$TST  = 100.			;STATE				(RSX)
	MP$TFL  = 110.			;FILE				(RSX)
	MP$BUF  = 120.			;BUFFER				(RSX)
	MP$TLN  = 130.			;LINE				(RSX)

;
; PARAMETER VALUES
;
; LOOP TEST BLOCK TYPE VALUES
;
	MB$MIX	= 2.			;MIXED
	MB$ONE	= 1.			;ONES
	MB$ZER	= 0.			;ZEROES
;
;
; LOOP HELP TYPE VALUES
;
	MB$HFL	= 2.			;FULL
	MB$HRC	= 1.			;RECEIVE
	MB$HTR	= 0.			;TRANSMIT
;
; STATE VALUES
;
	MS$ON	= 0.			;ON
	MS$OFF	= 1.			;OFF
;
;    LINE/CIRCUIT/PROCESS SPECIFIC STATE VALUES
;
	MS$SER	= 2.			;SERVICE 		
	MS$CLE	= 3.			;CLEARED
;
;    LOGGING SPECIFIC STATE VALUES
;
	MS$HOL	= 2.			;HOLD
;
;    NODE SPECIFIC STATE VALUES
;
	MS$SHU	= 2.			;SHUT
	MS$RES	= 3.			;RESTRICTED
	MS$REA	= 4.			;REACHABLE
	MS$UNR	= 5.			;UNREACHABLE
	MS$BAD	= 13.			;FAILED (DATABASE CORRUPT)	(RSTS)
;
; ALIAS SCOPE VALUES
;
	MS$GLO	= 0.			;GLOBAL
	MS$TER	= 1.			;TERMINAL
;
; GROUP VALUES
;
	MT$BIL  = 1.			;BILATERAL
;
; LINE DUPLEX VALUES
;
	MD$FUL	= 0.			;FULL
	MD$HAL	= 1.			;HALF
;
; LINE/PARTITION LOCATION VALUES
;
	ML$FIR	= 0.			;FIRSTFIT
	ML$TOP	= 1.			;TOPDOWN
;
; LINE/CIRCUIT SUBSTATE VALUES
;
	MS$STA	= 0.			;STARTING
	MS$REF	= 1.			;REFLECTING
	MS$LOO	= 2.			;LOOPING
	MS$LOA	= 3.			;LOADING
	MS$DUM	= 4.			;DUMPING
	MS$TRI	= 5.			;TRIGGERING
	MS$ASE	= 6.			;AUTOSERVICE
	MS$ALO	= 7.			;AUTOLOADING
	MS$ADU	= 8.			;AUTODUMPING
	MS$ATR	= 9.			;AUTOTRIGGERING
	MS$SYN	= 10.			;SYNCHRONIZING
	MS$FAI	= 11.			;FAILED
;
; CIRCUIT TYPE AND LINE PROTOCOL VALUES
;
	MT$POI	= 0.			;DDCMP POINT
	MT$CON	= 1.			;DDCMP CONTROLLER
	MT$TRI	= 2.			;DDCMP TRIBUTARY
	MT$X25	= 3.			;X25			(CIRCUIT)
	MT$DMC	= 4.			;DDCMP DMC-COMPATIBLE
	MT$LPB	= 5.			;LAPB			(LINE)
	MT$ETN	= 6.			;ETHERNET		(IV)
	MT$CI	= 7.			;CI			(IV)
	MT$QP2	= 8.			;QP2 (DTE 20)		(IV)
	MT$BIS	= 9.			;BISYNC			(CIRCUIT)(IV)

;
; LINE CONTROLLER MODE VALUES
;
	MC$NOR	= 0.			;NORMAL
	MC$LOO	= 1.			;LOOPBACK
;
; LINE CLOCK VALUES
;
	LC$EXT	= 0.			;EXTERNAL
	LC$INT	= 1.			;INTERNAL
;
; LINE VERIFICATION MODE VALUES	(RSTS)
;
	LV$OFF	= 0.			;OFF (NO VERIFICATION)		(RSTS)
	LV$ORG	= 1.			;USE ORIGINATE PASSWORDS	(RSTS)
	LV$ANS	= 2.			;USE ANSWER PASSWORDS		(RSTS)
;
; CIRCUIT BLOCKING/SERVICE VALUES
;
	MB$ENA	= 0.			;ENABLED
	MB$DIS	= 1.			;DISABLED
;
; CIRCUIT USAGE VALUES
;
	MU$PER	= 0.			;PERMANENT
	MU$INC	= 1.			;INCOMING
	MU$OUT	= 2.			;OUTGOING
;
; CIRCUIT POLLING STATE AND SUBSTATE VALUES
;
	MS$AUT	= 0.			;AUTOMATIC 	(NOT VALID AS SUBSTATE)
	MS$ACT	= 1.			;ACTIVE
	MS$INA	= 2.			;INACTIVE
	MS$DIE	= 3.			;DYING
	MS$DED	= 4.			;DEAD
;
; NODE CPU VALUES
;
	MC$P08  = 0.			;PDP-8
	MC$P11  = 1.			;PDP-11
	MC$020	= 2.			;DECSYSTEM 1020
	MC$VAX  = 3.			;VAX
;
; NODE SOFTWARE TYPE VALUES
;
	MT$SEC  = 0.			;SECONDARY LOADER
	MT$TER  = 1.			;TERTIARY LOADER
	MT$SYS  = 2.			;SYSTEM
;
; NODE SERVICE DEVICE TYPE VALUES
;
	MD$DP	= 0.			;DP
	MD$UNA	= 1.			;UNA
	MD$DU	= 2.			;DU
	MD$CNA	= 3.			;CNA
	MD$DL	= 4.			;DL
	MD$QNA	= 5.			;QNA
	MD$DQ	= 6.			;DQ
	MD$CI	= 7.			;CI
	MD$DA	= 8.			;DA
	MD$PCL	= 9.			;PCL
	MD$DUP	= 10.			;DUP
	MD$DMC	= 12.			;DMC
	MD$DN	= 14.			;DN
	MD$DLV	= 16.			;DLV
	MD$DMP	= 18.			;DMP
	MD$DTE	= 20.			;DTE
	MD$DV	= 22.			;DV
	MD$DZ	= 24.			;DZ
	MD$KDP	= 28.			;KDP
	MD$KDZ	= 30.			;KDZ
	MD$KL	= 32.			;KL
	MD$DMV	= 34.			;DMV
	MD$DPV	= 36.			;DPV
	MD$DPF	= 38.			;DMF
	MD$DMR	= 40.			;DMR
	MD$KMY	= 42.			;KMY
	MD$KMX	= 44.			;KMX
;
; NODE TYPE VALUES
;
	MT$ROU	= 0.			;ROUTING III
	MT$NON	= 1.			;NONROUTING III
;	MT$PHA	= 2.			;Reserved		(IV)	;1.55
	MT$ARE	= 3.			;AREA			(IV)
	MT$RIV	= 4.			;ROUTING IV
	MT$NIV	= 5.			;NONROUTING IV
;
; NODE PASSWORD VALUES
;
	MP$SET	= 0.			;PASSWORD SET
;
; OBJECT USER VALUES
;
	MO$DEF	= 0.			;DEFAULT
	MO$LOG	= 1.			;LOGIN
;
; OBJECT VERIFICATION VALUES	-- RSX
;
	MO$ON	= 0.			;ON
	MO$OFF	= 1.			;OFF
	MO$INS	= 2.			;INSPECT
;
; OBJECT VERIFICATION VALUES	-- RSTS
;
	MO$PRG	= 0			;PROGRAM--OLD STYLE
	MO$VOF	= 2			;VERIFICATION OFF
	MO$VON	= 4			;VERIFICATION ON
;
; LINK STATE VALUES	(RSTS)
;
	ML$RES	= 0.			;RESERVED			(RSTS)
	ML$CID	= 1.			;CI DELIVERED			(RSTS)
	ML$CIS	= 2.			;CI SENT			(RSTS)
	ML$CIR	= 3.			;CI RECEIVED			(RSTS)
	ML$CCS	= 4.			;CC SENT			(RSTS)
	ML$RUN	= 5.			;RUN				(RSTS)
	ML$DIP	= 6.			;DI PENDING			(RSTS)
	ML$DIS	= 7.			;DI SENT			(RSTS)
;
; LINK FLOW CONTROL VALUES	(RSTS)
;
	ML$NON	= 0.			;NO FLOW CONTROL		(RSTS)
	ML$SEG	= 1.			;SEGMENT FLOW CONTROL		(RSTS)
	ML$MSG	= 2.			;MESSAGE FLOW CONTROL		(RSTS)
;
; NUMERIC VALUES.
;
	MN$UNL	= 377			;"UNLIMITED"

;
; DEFAULT PARAMETER VALUES
;
; LOOP PARAMETERS
;
	MD$COU	= 1.			;COUNT
	MD$LEN	= 128.			;LENGTH
	MD$WIT	= MB$MIX		;WITH
 
;
; MAXIMUM STRING LENGTHS FOR PARAMETER VALUES.
;
	MX$ACT	= 16.
	MX$CIR	= 16.			;CIRCUIT ID.
	MX$CLN	= 16.			;CIRCUIT LINE-ID
	MX$CNM	= 6.			;CIRCUIT NUMBER
	MX$CON	= 6.			;CONSOLE NAME
	MX$DAC	= 16.			;DEFAULT ACCOUNT ID.
	MX$DTE	= 16.			; ? 
	MX$FIL	= 28.			;FILE SPEC.
	MX$LIN	= 16.			;LINE ID.
	MX$LON	= 6.			;LOOPBACK NAME
	MX$NOD	= 6.			;NODE NAME
	MX$OBJ	= 6.			;OBJECT ID.
	MX$OWN	= 32.			;OWNER ID.
	MX$PAS	= 8.			;PASSWORD			;1.52
	MX$RAC	= 39.			;REMOTE ACCOUNT
	MX$RID	= 39.			;REMOTE ID
	MX$RPS	= 39.			;REMOTE PASSWORD
	MX$SID	= 32.			;SYSTEM-ID
	MX$SNK	= 255.			;SINK NAME
	MX$UID	= 16.			;USER-ID (PPN)
 
;
; OTHER MAXIMA (SIZES, VALUES, ETC).
	MX$LGC	= 511.			;LOGGING CLASS CODE.
	MX$PAR	= 30.			;MAX # OF PARAMETERS PER COMMAND.
	MX$TYP	= 63.			;EVENT-TYPE CODE.
	MX$NAD	= 1023.			;MAXIMUM NODE ADDRESS PER AREA
	MX$ARE	= 63.			;MAXIMUM AREA NUMBER
;
; PARAMETER MASKS	--	IV
;
	MK$ARE	= 176000		;AREA NUMBER IN NODE ADDRESS	;1.53
	MK$NAD	= 001777		;NODE NUMBER IN NODE ADDRESS

;
; RETURN CODES
;
	MS.SUC	= 1.			;SUCCESS
	MS.MOR	= 2.			;SUCCESS, MORE TO COME
	MS.PRT	= 3.			;SUCCESS, PARTIAL REPLY
;
	MS.DON	= -128.			;ALL DONE
	ME.DON	= -128.			;ALL DONE
;
	ME.FUN	= -1.			;UNRECOGNIZED FUNCTION OR OPTION
	ME.FOR	= -2.			;INVALID MESSAGE FORMAT
	ME.PRI	= -3.			;PRIVILEGE VIOLATION
	ME.SIZ	= -4.			;OVERSIZED MANAGEMENT COMMAND MESSAGE
	ME.MPR	= -5.			;NETWORK MANAGEMENT PROGRAM ERROR
	ME.PTY	= -6.			;UNRECOGNIZED PARAMETER TYPE
	ME.MVE	= -7.			;INCOMPATIBLE MANAGEMENT VERSION
	ME.UCO	= -8.			;UNRECOGNIZED COMPONENT
	ME.IID	= -9.			;INVALID IDENTIFICATION FORMAT
	ME.LCO	= -10.			;LINE COMMUNICATION ERROR
	ME.CST	= -11.			;COMPONENT IN WRONG STATE
	ME.FOP	= -13.			;FILE OPEN ERROR
	ME.FCO	= -14.			;INVALID FILE CONTENTS
	ME.RES	= -15.			;RESOURCE ERROR
	ME.PVA	= -16.			;INVALID PARAMETER VALUE
	ME.LPR	= -17.			;LINE PROTOCOL ERROR
	ME.FIO	= -18.			;FILE I/O ERROR
	ME.DIS	= -19.			;LINK DISCONNECTED
	ME.ROO	= -20.			;NO ROOM FOR NEW ENTRY
	ME.CON	= -21.			;CONNECT FAILED
	ME.PNA	= -22.			;PARAMETER NOT APPLICABLE
	ME.PLO	= -23.			;PARAMETER VALUE TOO LONG
	ME.HAR	= -24.			;HARDWARE FAILURE
	ME.OPE	= -25.			;OPERATION FAILURE
	ME.SYS	= -26.			;SYSTEM-SPECIFIC MANAGEMENT FUNCTION NOT SUPPORTED
	ME.GRO	= -27.			;INVALID PARAMETER GROUPING
	ME.BLO	= -28.			;BAD LOOPBACK RESPONSE
	ME.PMI	= -29.			;PARAMETER MISSING
;
; ERROR DETAIL CODES
;
	MD.NON	= -1.			;NONE
;
;    FILE
;
	MD.PER	= 0.			;PERMANENT DATABASE
	MD.LOA	= 1.			;LOAD FILE
	MD.DUM	= 2.			;DUMP FILE
	MD.SLO	= 3.			;SECONDARY LOADER
	MD.TLO	= 4.			;TERTIARY LOADER
	MD.SDU	= 5.			;SECONDARY DUMPER
;	MD.VOL	= 6.			;VOLATILE DATABASE		(IV)
;	MD.DFI	= 7.			;DIAGNOSTIC FILE		(IV)
	MD.VOL	= 7.			;VOLATILE DATABASE (FOR COMPAT. RSTS)
;
;    CONNECT/DISCONNECT
;
	MD.SNA	= 0.			;NO NODE NAME SET
	MD.FNA	= 1.			;INVALID NODE NAME FORMAT
	MD.UNA	= 2.			;UNRECOGNIZED NODE NAME
	MD.UNR	= 3.			;NODE UNREACHABLE
	MD.RES	= 4.			;NETWORK RESOURCES
	MD.ROB	= 5.			;REJECTED BY OBJECT
	MD.FOB	= 6.			;INVALID OBJECT NAME FORMAT
	MD.UOB	= 7.			;UNRECOGNIZED OBJECT
	MD.ACC	= 8.			;ACCESS CONTROL REJECTED
	MD.BOB	= 9.			;OBJECT TOO BUSY
	MD.NOB	= 10.			;NO RESPONSE FROM OBJECT
	MD.RSH	= 11.			;REMOTE NODE SHUT DOWN
	MD.FAI	= 12.			;NODE OR OBJECT FAILED
	MD.DOB	= 13.			;DISCONNECT BY OBJECT
	MD.AOB	= 14.			;ABORT BY OBJECT
	MD.ABO	= 15.			;ABORT BY MANAGEMENT
	MD.LSH	= 16.			;LOCAL NODE SHUT DOWN
;
; MAKE MACRO GO AWAY
;
	.IF NB	LST
	.NLIST
	.IFF
	.MACRO	MANDF$
	.ENDM
	.ENDC
;
	.ENDM	MANDF$
	.MACRO	$NMLDF		;GENERAL PARAMETER DEFINITIONS
;	.SBTTL	GENERAL NML DEFINITIONS
;
; THIS MACRO SHOULD BE CALLED BY ALL NML ROUTINES
; BEFORE CALLING ANY OTHER NML-SPECIFIC MACROS
;
	.DSECT
	.=0
;
; CURRENT NICE VERSION NUMBER (FOR CONNECT REQUEST AND CONNECT ACCEPT)
;
$NCVER= 004				;NICE VERSION NUMBER		;4.01
$NCECO= 000				;NICE DEC ECO NUMBER
$NCUSR= 000				;NICE USER ECO NUMBER
;
; DEFINE THE SIZE OF THE MESSAGE BUFFER.
;
$NMMES=  <1024.>&77776			;SIZE OF BUFFER IN BYTES (MUST BE EVEN)

.IIF	LE,$NMMES,	.ERROR		;$NMMES HAS AN ILLEGAL VALUE
;
; DEFINE THE OFFICIAL NCP/NML BUFFER SIZE FOR NETWORK MESSAGES.
;
$NMGBS=  <300.>&77776			;BUFFER SIZE FOR REMOTE COMPATIBILITY
;
; DEFINE THE OFFSET INTO THE MESSAGE BUFFER FOR
; RECEIVING DATA.  THIS OFFSET EXISTS TO MAKE MESSAGE PARSING EASIER.
; THE DATA MUST BE OFFSET FAR ENOUGH INTO THE BUFFER TO PROVIDE ROOM
; FOR PARSING REQUIREMENTS.  THIS CONSTANT MUST BE GLOBAL.
;
$NMOFF=  <26.>&77776			;OFFSET TO START OF MSG (MUST BE EVEN)

.IIF	LE,$NMOFF,	.ERROR		;$NMOFF HAS AN ILLEGAL VALUE
;
; DEFINE THE SIZE OF THE CONTROL BLOCK CONTEXT AREA.
;
$NMCON=  <350.>&77776			;SIZE OF BUFFER IN BYTES

.IIF	LE,$NMCON,	.ERROR		;$NMCON HAS AN ILLEGAL VALUE
;
; PARAMETERS FOR NETWORK I/O CALLS
;
$NMLRT=60		;NETWORK I/O RETRY MAXIMUM
$NMRTS=2		;SECS TO SLEEP BETWEEN NETWORK I/O RETRIES
;
; PARAMETERS FOR NETWORK MANAGEMENT PARAMETER FILES
;
$NMVOL=6		;SYSTEM FILE NUMBER FOR VOLATILE PARAMETER FILE
$NMLPF=2		;CHANNEL FOR OPENING PERMANENT PARAMETER FILE
$NMLVF=3		;CHANNEL FOR OPENING VOLATILE  PARAMETER FILE
$NMLNF=4		;CHANNEL FOR OPENING NEW-SIZED PARAMETER FILE	;4.03
;
	.MCALL	.MDELET
	.MDELET	$NMLDF
;
.ENDM	$NMLDF
	.MACRO	$OBJDF
;	.SBTTL	OBJECT CONTEXT AREA OFFSET DEFINITIONS

	.LIST
;
; OBJECT CONTEXT AREA OFFSET DEFINITIONS
;
	.DSECT
	.=0
O$FLG:	.BLKW	1			;OPERATION FLAG WORD
O$OPT:	.BLKB	1			;SAVED OPTIONS BYTE
O$FMT:	.BLKB	1			;OBJECT-ID FORMAT TYPE
O$PAR:	.BLKW	1			;CURRENT PARAMETER TYPE
O$MSG:	.BLKW	1			;ERROR MESSAGE STRING POINTER
O$BUF:	.BLKW	1			;SAVED POINTER TO BUFFER
O$TYP:	.BLKB	1			;OBJECT TYPE
O$FLA:	.BLKB	1			;OBJECT FLAGS
O$NAM:	.BLKW	3			;OBJECT NAME, ASCII
O$FIL:	.BLKW	6			;OBJECT FILESPEC DATA (FIRQB FORMAT:
					;FQPPN,FQNAM [2 WD],FQEXT,FQDEV,FQDEVN)
O$P1:	.BLKW	1			;FIRST PARAMETER WORD
O$P2:	.BLKW	1			;SECOND PARAMETER WORD

O$SCR:	.BLKW	50.			;SCRATCH AREA >= LENGTH OF OBJ RECORD
O$LEN:					;LENGTH OF CONTEXT AREA

.ASSUME O$LEN LE $NMCON

;
; FLAGS WORD BIT DEFINITIONS
;
	OF$INI=100			;SUCCESSFUL INITIALIZATION COMPLETED
	OS$NAM=1			;NAME
	OS$FIL=2			;FILESPEC
	OS$OBT=4			;TYPE
	OS$PR1=20			;PARAMETER 1
	OS$PR2=40			;PARAMETER 2
	OS$OVA=200			;VERIFICATION
	OC$ALL=100000			;CLEAR/PURGE ALL
;
; MAKE MACRO GO AWAY
;
	.MCALL	.MDELET
	.MDELET	$OBJDF
;
	.NLIST
.ENDM	$OBJDF
	.MACRO	$LINDF
;	.SBTTL	LINE CONTEXT AREA DEFINITIONS

	.LIST
;
; CONTEXT AREA OFFSET DEFINITIONS
;
	.DSECT
	.=0
L$DDM:	.BLKW	1			;DEVICE NAME (RAD50 OF INPUT DEV)
L$DEV:	.BLKW	1			;RSTS DEVICE NAME (ASCII -- XM,XD)
L$CON:	.BLKB	1			;CONTROLLER NUMBER
L$TRB:	.BLKB	1			;TRIBUTARY NUMBER
L$PFG:	.BLKB	1			;PARSE FLAGS
L$DCU:	.BLKB	1			;RSTS CONTROLLER LETTER-CODE
L$UNI:	.BLKB	1			;RSTS DEVICE UNIT NUMBER FIELD OF FIRQB
L$MXT:	.BLKB	1			;MAXIMUM TRIBUTARIES PER DEVICE
L$CNR:	.BLKW	1			;CONTROLLER RECORD NUMBER (CIRCUIT 0)
L$TRR:	.BLKW	1			;TRIBUTARY RECORD NUMBER

L$BUF:	.BLKW	1			;SAVED BUFFER POINTER
L$OPT:	.BLKB	1			;SAVED OPTIONS BYTE
L$FMT:	.BLKB	1			;LINE-ID FORMAT TYPE
L$PAR:	.BLKW	1			;CURRENT PARAMETER TYPE
L$VOL:	.BLKW	1			;POINTER TO VOLATILE FILE DBRQ
L$PER:	.BLKW	1			;POINTER TO PERMANENT FILE DBRQ
L$OPF:	.BLKW	1			;OPERATION CONTROL FLAGS
L$JAD:	.BLKW	1			;Adjacent Node Addr (0 if Name)	;4.02
L$JNM:	.BLKB	MX$NOD			;Adjacent Node Name (0 if Addr)	;4.02
					;Both 0 if no Adj Nod specified	;4.02

L$FLG:	.BLKW	1			;FLAG WORD (FOR COMMAND)
L$FL1:	.BLKW	1			;FLAG WORD (FOR COMMAND)
L$FL2:	.BLKW	1			;FLAG WORD (FOR COMMAND)
L$FL3:	.BLKW	1			;Flag word (for yet more com's)	;4.06
L$FLC:	.BLKW	1			;FLAG WORD (FOR CURRENT LINE)
L$MSG:	.BLKW	1			;ERROR MESSAGE STRING POINTER

L$STA:	.BLKB	1			;STATE
L$COS:	.BLKB	1			;COST
L$OWN:	.BLKB	1			;OWNER FLAG (=1 IF OWNER EXE)
L$TAD:	.BLKB	1			;TRIBUTARY ADDRESS
L$VER:	.BLKW	1			;VERIFICATION OFF/ORG/ANS (0/1/3)
L$RES:	.BLKW	1			;AUTORESTART ON/OFF (1=OFF)
L$LTY:	.BLKW	1			;PROTOCOL/TYPE DATA

L$BFQ:	.BLKW	1			;BUFFER QUOTA (LINE)
L$RBF:	.BLKW	1			;RECEIVE BUFFER COUNT (CIRCUIT)
L$CTM:	.BLKW	1			;COUNTER TIMER VALUE
L$HTM:	.BLKW	1			;HELLO TIMER VALUE
L$RET:	.BLKW	1			;RETRANSMIT TIMER VALUE
L$RTM:	.BLKW	1			;Recall timer value		;4.00
L$OQL:	.BLKW	1			;Originating queue limit value	;4.00
L$MRO:	.BLKB	1			;Maximum Routers		;4.06
L$RPR:	.BLKB	1			;Router Priority		;4.06

L$DDT:	.BLKW	1			;DMP DEAD TIMER
L$DLT:	.BLKW	1			;DMP DELAY TIMER
L$SCT:	.BLKW	1			;DMP SCHEDULING TIMER
L$STT:	.BLKW	1			;DMP STREAM TIMER
L$BBT:	.BLKW	1			;DMP BABBLE TIMER
L$XMT:	.BLKW	1			;DMP TRANSMIT TIMER
L$BSA:	.BLKB	1			;DMP ACTIVE BASE
L$BSD:	.BLKB	1			;DMP DYING BASE
L$BSI:	.BLKB	1			;DMP INACTIVE BASE
L$INA:	.BLKB	1			;DMP ACTIVE INCREMENT
L$IND:	.BLKB	1			;DMP DYING INCREMENT
L$INI:	.BLKB	1			;DMP INACTIVE INCREMENT
L$TH1:	.BLKB	1			;DMP DEAD THRESHOLD
L$TH2:	.BLKB	1			;DMP DYING THRESHOLD
L$TH3:	.BLKB	1			;DMP INACTIVE THRESHOLD

L$CCT:	.BLKB	1			;CIRCUIT COUNT FOR OUTPUT INFO

L$SCR:	.BLKW	39.			;SCRATCH BUFFER:
					;(MIN SIZE = LINE REC + 8)

L$NAC:	.BLKB	1			;NUMBER OF ACTIVE CIRCUITS

L$WIT:	.BLKB	1		;LOOP WITH data-type			;4.09
L$COU:	.BLKW	1		;LOOP COUNT				;4.09
L$LTH:	.BLKW	1		;LOOP LENGTH				;4.09
L$NOD:	.BLKB	6		;LOOP NODE name				;4.09
L$ASN:	.BLKB	6		;LOOP ASSISTANT NODE name		;4.09
L$PAD:	.BLKB	6		;LOOP PHYSICAL ADDRESS			;4.09
L$APA:	.BLKB	6		;LOOP ASSISTANT PHYSICAL ADDRESS	;4.09

L$MLN:	.BLKW	1		;Length of input message		;4.09
L$HLP:	.BLKB	1		;LOOP HELP type				;4.09
	.BLKB	1		;RESERVED
L$LEN:				;LENGTH OF CONTEXT AREA

.ASSUME	L$LEN LE $NMCON

;	.SBTTL	LINE CONTEXT AREA BIT DEFINITIONS
;
; PARSE FLAG DEFINITIONS (L$PFG)
;
	LP$CON = 1		;CONTROLLER NUMBER FOUND
	LP$TRB = 2		;TRIBUTARY NUMBER FOUND
	LP$ETH = 4		;Circuit is Ethernet			;4.08
	LP$MPT = 10		;LINE IS MULTIPOINT
	LP$WDV = 20		;WILD CARD DEVICE NAME FOUND
	LP$WCN = 40		;WILD CARD CONTROLLER NUMBER FOUND
	LP$WTR = 100		;WILD CARD TRIBUTARY NUMBER FOUND	;4.09
	LP$HLP = 200		;Help was specified			;4.09

	LP$WLD = LP$WDV!LP$WCN!LP$WTR	;WILD CARD FIELD MASK
;
; OPERATION CONTROL FLAG DEFINITIONS (L$OPF)
;
	LO$FDF = 1			;FIND NEXT DEFINED RECORD
	LO$WRT = 2			;WRITE RECORD BACK TO FILE
	LO$FRQ = 4			;ASK TRANSPORT/NSP TO CHANGE LINE
	LO$DID = 10			;COMPLETED ONE LINE OPERATION
	LO$LON = 20			;LINE IS ACTIVE (SOME CIRCUIT ON)
	LO$FNM = 40			;FIND RECORD WITH SPECIFIED NUMBER
	LO$COR = 100			;SAVED CONTROLLER RECORD FOR CIRCUIT
	LO$ACT = 200			;CIRCUIT IS ACTIVE (STATE ON)
	LO$EAJ = 400		;Means we are currently in an Ethernet	;4.12
				; adjacency (partial reply) sequence	;4.12
;
; FLAGS WORD BIT DEFINITIONS (L$FLG)
;
	LS$ALL=1			;SET ALL FUNCTION
	LS$STA=2			;SET STATE
	LS$COS=4			;SET COST
	LS$OWN=10			;SET OWNER
	LS$TAD=20			;SET TRIBUTARY ADDRESS
	LS$VER=40			;SET VERIFICATION
	LS$RST=100			;SET AUTORESTART		;4.10
	LS$CTM=200			;SET COUNTER TIMER

	LS$RBF=400			;SET RECEIVE BUFFERS
	LS$DFL=1000			;SET DUPLEX FULL
	LS$DHF=2000			;SET DUPLEX HALF
	LS$RET=4000			;SET RETRANSMIT TIMER
	LS$LTY=10000			;SET PROTOCOL/TYPE 
	LS$CLP=20000			;SET CONTROLLER LOOPBACK
	LS$CNO=40000			;SET CONTROLLER NORMAL
	LS$HLT=100000			;SET HELLO TIMER

	LS$DUP=LS$DFL!LS$DHF		;SET DUPLEX MASK
	LS$CON=LS$CLP!LS$CNO		;CONTROLLER MODE MASK
;
; FLAGS WORD BIT DEFINITIONS (L$FL1)
;
	LS$BSA=1			;SET ACTIVE BASE
	LS$BSD=2     			;SET DYING BASE
	LS$BSI=4      			;SET INACTIVE BASE
	LS$INA=10    			;SET ACTIVE INCREMENT
	LS$IND=20    			;SET DYING INCREMENT
	LS$IIN=40    			;SET INACTIVE INCREMENT
	LS$TH1=100   			;SET DEAD THRESHOLD
	LS$TH2=200   			;SET DYING THRESHOLD

	LS$TH3=400   			;SET INACTIVE THRESHOLD
	LS$BFQ=1000  			;SET BUFFER QUOTA
	LS$DDT=2000			;SET DEAD TIMER
	LS$DLT=4000			;SET DELAY TIMER
	LS$STT=10000			;SET STREAM TIMER
	LS$BBT=20000			;SET BABBLE TIMER
	LS$SCT=40000			;SET SCHEDULING TIMER
	LS$XMT=100000			;SET TRANSMIT TIMER
;
; FLAGS WORD BIT DEFINITIONS (L$FL2)
;
	LF$REA = 1			;READ COUNTERS OPERATION
	LF$ZER = 2			;ZERO COUNTERS OPERATION
	LF$SKP = 4			;SKIP NEXT "FIND NEXT LINE"
					;THIS IS A INDICATOR TO FORCE A
					;PASS THROUGH FOR A MULTIPOINT LINE
					;CONTROLLER AS WELL AS ALL THE STATIONS
					;ON THAT LINE

	LS$PAU=10			;SET POLLING STATE AUTOMATIC
	LS$PAC=20			;SET POLLING STATE ACTIVE
	LS$PIA=40			;SET POLLING STATE INACTIVE
	LS$PDY=100			;SET POLLING STATE DYING
	LS$PDE=200			;SET POLLING STATE DEAD
	LS$MRO=400			;Set Maximum Routers		;4.06
	LS$RPR=1000			;Set Router Priority		;4.06
	LS$RTM=2000			;Set Recall Timer		;4.00
	LS$OQL=4000			;Set Originating Queue Limit	;4.00

	LC$ALL=10000			;CLEAR ALL FUNCTION
	LC$CTM=20000			;CLEAR COUNTER TIMER
	LC$OWN=40000			;CLEAR OWNER
					;100000 reserved		;4.06

	LS$PST=LS$PAU!LS$PAC!LS$PIA!LS$PDY!LS$PDE	;POLLING STATE MASK
;
	.MCALL	.MDELET
	.MDELET	$LINDF
;
	.NLIST
.ENDM	$LINDF
	.MACRO	$NODDF
;
; NODE CONTEXT AREA OFFSET DEFINITIONS
;
	.DSECT
N$FLG:	.BLKW	1			;SET NODE PARSE FLAG WORD
N$FL2:	.BLKW	1			;CLEAR NODE PARSE FLAG WORD
N$FL3:	.BLKW	1			;SET EXECUTOR PARSE FLAG WORD
N$FL4:	.BLKW	1			;CLEAR EXECUTOR PARSE FLAG WORD
N$FL5:	.BLKW	1			;DEFINE EXEC PARSE FLAG WORD	;4.04
N$FLC:	.BLKW	1			;CURRENT FLAG WORD FOR OPERATION

N$OPT:	.BLKB	1			;SAVED OPTIONS BYTE
N$FMT:	.BLKB	1			;NODE FORMAT TYPE
N$OPF:	.BLKW	1			;OPERATIONS FLAG WORD
N$PAR:	.BLKW	1			;CURRENT PARAMETER TYPE
N$MSG:	.BLKW	1			;ERROR MESSAGE STRING POINTER
N$BUF:	.BLKW	1			;SAVED POINTER TO BUFFER
N$SKR:	.BLKW	1			;SKIP RECORD ON DUPLICATE CHECKS

N$NAM:	.BLKW	3			;NODE NAME FROM NODE-ID
N$ADD:	.BLKW	1			;NODE ADDRESS FROM NODE-ID
N$SNM:	.BLKW	3			;NODE NAME INPUT FOR 'SET NAME'
;N$AKA:	.BLKW	3			;node name input for 'set alias';4.07
	.BLKW	3			;RESERVED space now...		;4.07
N$ADR:	.BLKW	1			;NODE ADDRESS INPUT FOR 'SET ADDRESS'
N$LIN:	.BLKW	20.			;LINE-ID AND CONTEXT FOR LOOPBACK NODE

N$RPO:	.BLKB	8.			;RECEIVE PASSWORD ORIGINATE
N$RPA:	.BLKB	8.			;RECEIVE PASSWORD ANSWER
N$TPO:	.BLKB	8.			;TRANSMIT PASSWORD ORIGINATE
N$TPA:	.BLKB	8.			;TRANSMIT PASSWORD ANSWER

N$DLF:	.BLKB	1			;DELAY FACTOR
N$DLW:	.BLKB	1			;DELAY WEIGHT
N$NSI:	.BLKW	1			;NSP INACTIVITY TIMER
N$LMX:	.BLKW	1			;MAX LINKS
N$VIS:	.BLKB	1			;MAX VISITS
N$DTQ:	.BLKB	1			;DATA QUEUE LIMIT
N$INQ:	.BLKB	1			;LINK-SERVICE/INT MSG QUEUE LIMIT
N$HOP:	.BLKB	1			;MAX HOPS
N$MXL:	.BLKW	1			;MAX LINES

N$STA:	.BLKW	1			;STATE TO SET NODE
N$RXM:	.BLKW	1			;RETRANSMIT FACTOR
N$NMX:	.BLKW	1			;MAXIMUM NODE ADDRESS
N$MXN:	.BLKW	1			;MAXIMUM NODE RECORD NUMBER
N$MAR:	.BLKB	1			;MAXIMUM AREA NUMBER		;4.04
N$AMH:	.BLKB	1			;AREA MAX HOPS			;4.04
N$CST:	.BLKW	1			;MAXIMUM COST
N$RTM:	.BLKW	1			;ROUTE TIMER
N$DLL:	.BLKW	1			;DLL BUFFER SIZE (SEGMENT SIZE)
N$SBS:	.BLKW	1			;ECL SEGMENT BUFFER SIZE
N$BPS:	.BLKW	1			;MAXIMUM BUFFER POOL SIZE
N$DAC:	.BLKB	16.			;DEFAULT ACCOUNT USERID
N$PPN:	.BLKW	1			;PARSED PPN FOR DEFAULT ACCOUNT
N$SID:	.BLKB	32.			;SYSTEM-ID STRING
N$ITM:	.BLKW	1			;INCOMING TIMER
N$OTM:	.BLKW	1			;OUTGOING TIMER
N$CTM:	.BLKW	1			;COUNTER TIMER
N$MBU:	.BLKW	1			;MAXIMUM BUFFERS
N$VFS:	.BLKB	28.			;VOLATILE PARAMETER FILESPEC

N$AMC:	.BLKW	1			;AREA MAX COST			;4.04
N$BRT:	.BLKW	1			;BROADCAST ROUTING TIMER	;4.04
N$MBN:	.BLKW	1			;MAXIMUM BROADCAST NONROUTERS	;4.04
N$MBR:	.BLKW	1			;MAXIMUM BROADCAST ROUTERS	;4.04

N$PER:	.BLKW	1			;POINTER TO PERMANENT DBRQ (FOR SET ALL)
N$VOL:	.BLKW	1			;POINTER TO VOLATILE DBRQ (FOR SET ALL)
N$SCR:	.BLKW	40.			;SCRATCH BUFFER

N$LEN:					;LENGTH OF CONTEXT AREA

.ASSUME N$LEN LE $NMCON

;
; SET FLAGS WORD BIT DEFINITIONS (N$FLG)
;
	NS$ALL=1			;SET ALL
	NS$LIN=2			;SET LINE
	NS$NAM=4			;SET NAME
	NS$RPO=10			;SET RECEIVE PASSWORD ORIGINATE
	NS$RPA=20			;SET RECEIVE PASSWORD ANSWER
	NS$TPO=40			;SET TRANSMIT PASSWORD ORIGINATE
	NS$TPA=100			;SET TRANSMIT PASSWORD ANSWER
;ads	NS$AKA=200			;RESERVED, was 'set alias name'	;4.07
	NS$CTM=400			;SET COUNTER TIMER
	NS$ADD=1000			;SET ADDRESS
	NS$VOL=2000			;SET VOLATILE PARAM FILESPEC
	NS$MBU=4000			;SET MAXIMUM BUFFERS
	NS$SID=10000			;SET SYSTEM ID
	NS$ITM=20000			;SET INCOMING TIMER
	NS$OTM=40000			;SET OUTGOING TIMER

	NS$EXP=NS$MBU!NS$SID!NS$ITM!NS$OTM!NS$VOL
					;EXECUTOR PARAMETERS
;
; CLEAR FLAGS WORD BIT DEFINITIONS (N$FL2)
;
	NC$ALL=1			;CLEAR ALL
	NC$LIN=2			;CLEAR LINE
	NC$NAM=4			;CLEAR NAME
	NC$RPO=10			;CLEAR RECEIVE PASSWORD ORIGINATE
	NC$RPA=20			;CLEAR RECEIVE PASSWORD ANSWER
	NC$TPO=40			;CLEAR TRANSMIT PASSWORD ORIGINATE
	NC$TPA=100			;CLEAR TRANSMIT PASSWORD ANSWER
;ads	NC$AKA=200			;RESERVED, was clear alias name	;4.07
	NC$CTM=400			;CLEAR COUNTER TIMER
	NC$ADD=1000			;CLEAR ADDRESS

	NC$MBU=4000			;CLEAR MAXIMUM BUFFERS
	NC$SID=10000			;CLEAR SYSTEM ID (EXECUTOR)
	NC$ITM=20000			;CLEAR INCOMING TIMER
	NC$OTM=40000			;CLEAR OUTGOING TIMER

	NC$EXP=NC$MBU!NC$SID!NC$ITM!NC$OTM	;EXECUTOR PARAMETERS
;
; SET EXECUTOR FLAGS WORD BIT DEFINITIONS (N$FL3)
;
	NS$DLF=1			;DELAY FACTOR
	NS$DLW=2			;DELAY WEIGHT
	NS$NSI=4			;NSP INACTIVITY TIMER
	NS$LMX=10			;MAX LINKS
	NS$VIS=20			;MAX VISITS
	NS$DTQ=40			;DATA QUEUE LIMIT
	NS$INQ=100			;LNK-SRV/INT MSG QUEUE LIMIT
	NS$MXL=200			;MAX CIRCUITS
	NS$STA=400			;DESIRED STATE
	NS$HOP=1000			;MAX HOPS
	NS$RXM=2000			;RETRANSMIT FACTOR
	NS$NMX=4000			;MAX NODE ADDRESS
	NS$CST=10000			;MAX COST
	NS$RTM=20000			;ROUTE TIMER
	NS$DLL=40000			;DLL BUFFER SIZE
	NS$DAC=100000			;DEFAULT ACCOUNT USERID
;
; CLEAR EXECUTOR FLAGS WORD BIT DEFINITIONS (N$FL4)
;
	NC$DLF=1			;DELAY FACTOR			;4.03+
	NC$DLW=2			;DELAY WEIGHT
	NC$NSI=4			;NSP INACTIVITY TIMER
	NC$LMX=10			;MAX LINKS
	NC$VIS=20			;MAX VISITS
	NC$DTQ=40			;DATA QUEUE LIMIT
	NC$INQ=100			;LNK-SRV/INT MSG QUEUE LIMIT
	NC$MXL=200			;MAX LINES
	NC$STA=400			;DESIRED STATE
	NC$HOP=1000			;MAX HOPS
	NC$RXM=2000			;RETRANSMIT FACTOR
	NC$NMX=4000			;MAX NODE ADDRESS
	NC$CST=10000			;MAX COST
	NC$RTM=20000			;ROUTE TIMER
	NC$DLL=40000			;DLL BUFFER SIZE		;4.03-
	NC$DAC=100000			;CLEAR EXEC DEFAULT ACCOUNT USERID

;
; MORE SET/DEFINE EXECUTOR FLAGS WORD BIT DEFINITIONS (N$FL5)		;4.04+
;
	NS$MNO=1		;DEFINE EXEC MAX NODES (causes DB size change)
	NS$MAR=2		;DEFINE EXEC MAX AREAS (also causes DB change)
	NS$MBR=4		;SET/DEF EXEC MAX BROADCAST ROUTERS	;4.05
	NS$MBN=10		;SET/DEF EXEC MAX BROADCAST NONROUTERS	;4.05
	NS$AMC=20		;SET/DEF EXEC AREA MAX COST		;4.05
	NS$AMH=40		;SET/DEF EXEC AREA MAX HOPS		;4.05
	NS$SBS=100		;SET/DEF EXEC SEGMENT BUFFER SIZE (ECL)	;4.05
	NS$BRT=200		;SET/DEF EXEC BROADCAST ROUTING TIMER	;4.05
				;Lot's		=400
				; of		=1000
				;  room		=2000
				;   left!	=4000
									;4.04-
;
; OPERATION FLAG WORD BIT DEFINITIONS
;
	NO$MNN=1		;MATCH NODE NAME
	NO$DUP=2		;"Checking for Duplicate Name" status	;4.11
	NO$MLN=4		;MATCH LOOPBACK NODE NAME
	NO$DID=10		;PERFORMED ONE OPERATION SUCCESSFULLY
	NO$WRT=20		;WRITE RECORD BACK TO FILE
	NO$NSP=40		;TELL NSP ABOUT CHANGED NODE PARAMETERS
	NO$SKP=100		;SKIP DUPLICATE RECORD CHECK
	NO$ZER=200		;ZERO COUNTERS OPERATION

	NO$REA=400		;READ AND ZERO COUNTERS OPERATION
	NO$FDF=1000		;FIND NEXT DEFINED RECORD
	NO$EXE=2000		;THIS NODE IS THE EXECUTOR
	NO$NEX=4000		;TELL NSP ABOUT CHANGED EXE PARAMETERS
	NO$SCX=10000		;"SET EXE" COMMAND, NOT "SET NODE"
	NO$NUB=20000		;FIND NEXT UNDEFINED RECORD

	NO$NAM=NO$MNN!NO$MLN		;MATCH NAMES
	NO$HAN=NO$NUB!NO$NAM		;FIND NAME OR NEXT CLEAR SLOT FOR ONE

	.MCALL	.MDELET
	.MDELET	$NODDF
	.ENDM	$NODDF
	.MACRO	$LNKDF
;	.SBTTL	LINK CONTEXT AREA OFFSET DEFINITIONS

	.LIST
;
; LINK CONTEXT AREA OFFSET DEFINITIONS
;
	.DSECT
	.=0
K$OPT:	.BLKB	1			;SAVED OPTIONS BYTE
K$FMT:	.BLKB	1			;OBJECT-ID FORMAT TYPE
K$OPF:	.BLKB	1			;OPERATIONS FLAG
	.BLKB	1			;RESERVED
K$PAR:	.BLKW	1			;CURRENT PARAMETER TYPE
K$MSG:	.BLKW	1			;ERROR MESSAGE STRING POINTER
K$BUF:	.BLKW	1			;SAVED POINTER TO BUFFER
K$LLA:	.BLKW	1			;LOCAL LINK ADDRESS
K$LEN:					;LENGTH OF CONTEXT AREA

.ASSUME K$LEN LE $NMCON

;
; OPERATION FLAG WORD BIT DEFINITIONS
;
	KO$DID=1		;COMPLETED ONE OPERATION
	KO$UCO=2		;NO LINKS EXIST FOR ACTIVE/KNOWN REQUEST
;
; MAKE MACRO GO AWAY
;
	.MCALL	.MDELET
	.MDELET	$LNKDF
;
	.NLIST
.ENDM	$LNKDF
	.MACRO	EVLDF$	L,B,LST
	.ASECT
.=0
.IIF NB	LST	.LIST
;
;	EVENT LOGGER PROCESS DEFINITIONS
;
E.LNK:'B'	.BLKW		; EVENT BLOCK LINK POINTER
E.EVT:'B'	.BLKW		; EVENT CLASS AND TYPE
E.TIME:'B'	.BLKW	6	; DATE AND TIME OF EVENT
E.CTL:'B'	.BLKB		; EVENT CONTROL MASK
E.PDV:'B'	.BLKB		; REQUESTING PROCESS PDV INDEX
E.SIZ:'B'	.BLKW		; SIZE OF ADDITIONAL DATA
E.LIN:'B'	.BLKW		; SLN & STATION FOR EVENT
E.PRM:'B'	.BLKW	3	; EVENT PARAMETERS
E.NOD:'B'	.BLKW		; REMOTE NODE ADDRESS
E.MOD:'B'	.BLKW		; MODULE ID
E.PORT:'B'	.BLKW		; X.25 PORT #
E.LCN:'B'	.BLKW		; X.25 LOGICAL CHANNEL #
E.DATA:'B'	.BLKB	150	; SPACE FOR ADDITIONAL DATA
E.LEN:
;
; CONTROL MASK FLAG DEFINITIONS
;
EV.CCB='L'	1		; EVENT HAS CCB RELATED DATA
EV.MAP='L'	2		; LINE-ID REQUIRES MAPPING
EV.LIN='L'	4		; EVENT IS ASSOCIATED WITH A LINE
EV.NOD='L'	10		; EVENT IS ASSOCIATED WITH A REMOTE NODE
EV.CIR='L'	20		; EVENT IS ASSOCIATED WITH A CIRCUIT
EV.MOD='L'	40		; EVENT IS ASSOCIATED WITH A MODULE
EV.LCB='L'	100		; USE LINE-ID FROM CCB
;
; EVENT DESCRIPTOR BLOCK OFFSETS
;
.=0
E$LIN:'B'	.BLKW		; SLN & STATION FOR EVENT
E$PRM:'B'	.BLKW	3	; EVENT PARAMETERS
E$NOD:'B'	.BLKW		; REMOTE NODE ADDRESS
E$MOD:'B'	.BLKW		; MODULE ID
E$PORT:'B'	.BLKW		; X.25 PORT #
E$LCN:'B'	.BLKW		; X.25 LOGICAL CHANNEL #
E$DATA:'B'			; DATA TO FOLLOW
;
; EVENT LOGGER PROCESS DDB OFFSETS
;
.=0
E$EVTS:'B'	.BLKW	2	; LISTHEAD OF EVENTS AWAITING COLLECTION
E$TCB:'B'	.BLKW		; TCB ADDRESS OF EVENT COLLECTOR
;
; CLASS AND TYPE CODE DEFINITIONS
;
; CLASS,TYPE WORD FORMAT
;
;  15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
; !---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!
; !RSV!<-------- C L A S S -------------->!<------ T Y P E ------>!
; !---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!
;
;
;  EVENT CLASSES
;
CL$MAN	=64.*0		;NETWORK MANAGEMENT LAYER
CL$SES	=64.*2		;SESSION CONTROL LAYER
CL$ECL	=64.*3		;ECL LAYER
CL$TRN	=64.*4		;TRANSPORT LAYER
CL$DLL	=64.*5		;DATA LINK LAYER
CL$PLL	=64.*6		;PHYSICAL LINK LAYER
CL$ROU	=64.*64.	;RSX SPECIFIC - ROUTING
CL$MOP	=64.*65.	;RSX SPECIFIC - MOP 
CL$LCC	=64.*66.	;RSX SPECIFIC - LINE COST CHANGE
CL$LMN	=64.*67.	;RSX SPECIFIC - LINE MONITOR EVENT
CL$LDN	=64.*68.	;RSX SPECIFIC - LINE DOWN (OPERATOR INITIATED)
CL$XL3	=64.*94.	;RSX SPECIFIC - X.25 LEVEL 3
CL$XL2	=64.*95.	;RSX SPECIFIC - X.25 LEVEL 2
;
; NETWORK MANAGEMENT EVENTS (CLASS 0)
;
EV$AUC	=CL$MAN+8.	;AUTOMATIC COUNTER LOGGING
EV$COZ	=CL$MAN+9.	;COUNTERS ZEROED
;
; SESSION CONTROL EVENTS (CLASS 2)
;
EV$NSC	=CL$SES+0	;LOCAL NODE STATE CHANGE
EV$ACF	=CL$SES+1	;ACCESS CONTROL FAILURE
;
; SESSION CONTROL REASON CODES
;
RE$OPR	=0		;OPERATOR COMMAND
RE$NML	=1		;NORMAL OPERATION
;
; SESSION CONTROL STATES
;
SC$ON	=0		;ON
SC$OFF	=1		;OFF
SC$SHU	=2		;SHUT
SC$RST	=3		;RESTRICTED
;
; ECL EVENTS (CLASS 3)
;
EV$DBR	=CL$ECL+2	;NODE DATA BASE REUSED
;
; TRANSPORT EVENTS (CLASS 4)
;
;
EV$APL	=CL$TRN+0	;AGED PACKET LOSS
EV$NUL	=CL$TRN+1	;NODE UNREACHABLE PACKET LOSS
EV$NOL	=CL$TRN+2	;NODE OUT OF RANGE PACKET LOSS
EV$OPL	=CL$TRN+3	;OVERSIZED PACKET LOSS
EV$PFE	=CL$TRN+4	;PACKET FORMAT ERROR
EV$RUL	=CL$TRN+5	;PARTIAL ROUTING UPDATE LOSS
EV$NVR	=CL$TRN+6	;NODE VERIFICATION REJECT
EV$LDL	=CL$TRN+7	;LINE DOWN - LINE FAULT
EV$LDS	=CL$TRN+8.	;LINE DOWN - SOFTWARE FAULT 
EV$LDO	=CL$TRN+9.	;LINE DOWN - OPERATOR FAULT
EV$LUP	=CL$TRN+10.	;LINE UP
EV$IFL	=CL$TRN+11.	;LINE INITIALIZATION FAILURE - LINE FAULT
EV$IFS	=CL$TRN+12.	;LINE INITIALIZATION FAILURE - SOFTWARE FAULT
EV$IFO	=CL$TRN+13.	;LINE INITIALIZATION FAILURE - OPERATOR FAULT
;
; TRANSPORT REASON CODES
;
RE$SYN=0		;LINE SYNCHRONIZATION LOST
RE$DAT=1		;DATA ERRORS
RE$UPT=2		;UNEXPECTED PACKET TYPE
RE$SUM=3		;ROUTING UPDATE CHECKSUM ERROR
RE$ADC=4		;ADJACENT NODE ADDRESS CHANGE
RE$VER=5		;VERIFICATION RECEIVE TIMEOUT
RE$SKW=6		;VERSION SKEW
RE$ADR=7		;ADJACENT NODE ADDRESS OUT OF RANGE
RE$BLK=8.		;ADJACENT NODE BLOCK SIZE TOO SMALL
RE$SED=9.		;INVALID VERIFICATION SEED VALUE
RE$LSN=10.		;ADJACENT NODE LISTENER TIMEOUT
RE$LDT=11.		;INVALID ADJACENT NODE TEST DATA
;
; DATA LINK LAYER EVENTS (CLASS 5)
;
EV$LSC	=CL$DLL+0.	;LOCALLY INITIATED LINE STATE CHANGE
EV$RSC	=CL$DLL+1.	;REMOTELY INITIATED LINE STATE CHANGE
EV$HFE	=CL$DLL+6.	;BLOCK HEADER FORMAT ERROR (LINE OR CIRCUIT)
EV$XRS	=CL$DLL+10.	;X.25 PROTOCOL - DTE RESTARTED
EV$XSC	=CL$DLL+11.	;X.25 PROTOCOL - DTE STATE CHANGE
EV$XMX	=CL$DLL+12.	;X.25 PROTOCOL - RETRANSMIT MAX EXCEEDED
;
; DATA LINK STATES
;
DL$ON	=0		;ON	)
DL$OFF	=1		;OFF	) PER LOCAL DTE
DL$SHU	=2		;SHUT	)
DL$HLT	=0		;HALTED	)
DL$IST	=1		;ISTRT	)
DL$AST	=2		;ASTRT	) PER
DL$RUN	=3		;RUNNING)  LINE
DL$MAI	=4		;MAINT	)
DL$SYN	=5		;SYNCH	)
;
; RSX SYSTEM SPECIFIC - ROUTING (CLASS 64)
;
EV$RMR	=CL$ROU+0	;ROUTING MESSAGE RECEIVED
;
; RSX SYSTEM SPECIFIC - MOP (CLASS 65)
;
EV$MOP	=CL$MOP+0	;MOP REQUEST RECEIVED
;
; RSX SYSTEM SPECIFIC - LINE COST CHANGE (CLASS 66)
;
EV$LCC	=CL$LCC+0	;LINE COST CHANGE
;
; RSX SYSTEM SPECIFIC - LINE DOWN (CLASS 68)
;
EV$LDN	=CL$LDN+14.	;LINE DOWN (OPERATOR INITIATED)
.IIF NB	LST	.NLIST
	.PSECT
	.ENDM	EVLDF$
	.MACRO	FLTDF$	L,B,LST
	.ASECT
.=0
.IIF NB	LST	.LIST
;
;	EVENT FILTER CONTROL BLOCKS
;
F.LNK:'B'	.BLKW		; FILTER BLOCK LINK POINTER
F.CLS:'B'	.BLKW		; EVENT CLASS AND BLOCK SIZE
 FF.MSK='L'77			; MASK FOR BLOCK SIZE
F.SEV:'B'			; SET EVENT MASK FOR QUALIFIED EVENTS
F.EVT:'B'	.BLKW	4	; EVENT TYPE MASK BITS
F.FLG:'B'	.BLKW		; FLAGS
F.LEN:
;
; EXTENDED BLOCK OFFSETS
;
F.ADD:'B'	.BLKW		; REMOTE NODE ADDRESS FOR FILTERING
F.LIN:'B'	.BLKW		; SLN & STATION PAIR FOR FILTERING
F.REM:'B'	.BLKW		; REMOTE NODE ADDRESS FOR SINK
F.CEV:'B'	.BLKW	4	; CLEAR EVENT MASK FOR QUALIFIED EVENTS
;
; FILTER CONTROL FLAGS
;
FF.CON='L'	1		; EVENT IS DESTINED FOR CONSOLE
FF.FIL='L'	2		; EVENT IS DESTINED FOR A FILE
FF.MON='L'	4		; EVENT IS DESTINED FOR A LOGGING MONITOR
FF.LIN='L'	10		; EVENT IS QUALIFIED BY A LINE
FF.ADD='L'	20		; EVENT IS QUALIFIED BY A NODE ADDRESS
FF.CIR='L'	40		; EVENT IS QUALIFIED BY A CIRCUIT ID
FF.MOD='L'	100		; EVENT IS QUALIFIED BY A MODULE ID
FF.REM='L'	100000		; EVENT IS TO BE SENT TO A REMOTE NODE
FF.QUL='L'	FF.LIN!FF.ADD!FF.CIR!FF.MOD
.IIF NB	LST	.NLIST
	.PSECT
;
	.MCALL	.MDELET
	.MDELET	$FLTDF$
;
	.ENDM	FLTDF$
	.MACRO	EVTDF$
;	.SBTTL	EVENT LOGGER CONTEXT AREA DEFINITIONS

.DSECT
;
; standard parameters for SET/CLEAR/READ operation
;
G$FLAG:	.BLKW	1			;FLAG WORD
G$ENT:	.BLKB	1			;ENTITY ID
G$OPT:	.BLKB	1			;SAVED OPTIONS WORD
G$BUF:	.BLKW	1			;SAVED ADDRESS OF INPUT BUFFER
G$REC:	.BLKW	1			;SAVED ADDRESS OF EVENT RECORD
G$OPF:	.BLKW	1			;RECORD I/O OPTION FLAG
G$PAR:	.BLKW	1			;CURRENT PARAMETER #
	.BLKB	1			;RESERVED
	.BLKB	1			;RESERVED
G$CLS:	.BLKW	1			;EVENT CLASS OF FILTER BLOCK
G$REM:	.BLKW	1			;REMOTE SINK NODE ADDRESS
G$NAM:	.BLKW	3			;REMOTE SINK NODE NAME
G$FILT:	.BLKW	4			;SAVE AREA FOR FILTER MASK
G$SQEN:	.BLKW	1			;SOURCE QUALIFIER ENTITY TYPE
G$SNIN:	.BLKB	1			;INDEX TO CURRENT SINK NODE
G$STPF:	.BLKB	1			;CURRENT SINK TYPE FLAG
G$ADD:	.BLKW	1			;NODE SOURCE QUALIFIER ADDRESS
G$ENM:	.BLKW	3			;NODE SOURCE QUALIFIER NAME
G$LINP:	.BLKW	8.			;LINE/CIRCUIT SOURCE QUALIFIER INFO
					; (OFFSETS PER $LINDF MACRO)
G$SCR:	.BLKW	5			;SCRATCH AREA
;
; extra parameters for CHANGE operation
;
G$CFG:	.BLKW	1			;CONTROL FLAGS
G$STAT:	.BLKW	1			;NEW LOGGING STATE
;
; The following offsets are in the same order as
; they appear in the $EVLREC database
;
	.EVEN				;
G$CON:	.BLKB				;CONSOLE SINK NAME LENGTH
	.BLKB	CON.SZ			;CONSOLE SINK NAME
	.EVEN				;
G$FIL:	.BLKB				;FILE SINK NAME LENGTH
	.BLKB	FIL.SZ			;FILE SINK NAME 
	.EVEN				;
G$MON:	.BLKB				;MONITOR SINK NAME LENGTH
	.BLKB	MON.SZ			;MONITOR SINK NAME
	.EVEN				;
;
; END OF ORDER DEPENDENT OFFSETS
;
;
; extra parameters for READ operation
;
G$CENT:	.BLKW	1			;CURRENT ENTITY TO OPERATE ON
G$SIZE:	.BLKW	1			;SIZE OF OUTPUT BUFFER
G$SNK:	.BLKW	1			;SAVED KNOWN SINK POINTER
G$COU:	.BLKW	1			;SAVED POINTER TO SINK COUNTS
G$CKSN:	.BLKW	1			;CURRENT SINK NODE BEING PROCESSED
G$PTR:	.BLKW	1			;SAVE AREA FOR POINTER TO FILTER BLOCK
G$COCT:	.BLKW	1			;COUNT OF FILTER BLOCKS IN CONSOLE 
G$FICT:	.BLKW	1			;COUNT OF FILTER BLOCKS IN FILE 
G$MOCT:	.BLKW	1			;COUNT OF FILTER BLOCKS IN MONITOR 
G$LEN:					;LENGTH OF CONTEXT AREA
;
; DEFINE FUNCTION BITS FOR FLAG WORD FOR CHANGE OPERATION
;
LF$DON = 1				;OPERATION COMPLETE
LF$KNO = 2				;OPERATE ON KNOWN EVENTS
LF$REM = 4				;SINK NODE SPECIFIED
LF$STA = 10				;CHANGE LOGGING STATE
LF$NAM = 20				;CHANGE LOGGING SINK NAME
LF$FIL = 40				;CHANGE LOGGING FILTERS
LF$SQL = 100				;EVENT QUALIFIED BY SOURCE-ENTITY
LF$SDN = 400				;SINK NODE MESSAGE DONE
LF$PER = 100000				;MESSAGE PARAMETER IN ERROR
;
; DEFINE BITS FOR LOGGING STATE FLAG
;
LS$ON = 1				;SET LOGGING STATE TO ON
;
; DEFINE ADDITIONAL FLAG BITS FOR READ OPERATION
;
LR$DON = 1				;PARAMETER READ COMPLETE
LR$KSN = 2				;KNOWN SINKS SPECIFIED
LR$REM = 4				;SINK NODE SPECIFIED
LR$CON = 10				;CONSOLE SINK TYPE ENTITY FLAG
LR$FIL = 20				;FILE SINK TYPE ENTITY FLAG
LR$MON = 40				;MONITOR SINK TYPE ENTITY FLAG
LR$STA = 100				;BUILT A STATE PARAMETER
LR$LNA = 200				;BUILT A NAME PARAMETER
LR$SDN = 400				;SINK MESSAGE DONE
LR$SQL = 1000				;DOING SOURCE-QUAL FILTERS
LR$OVR = 100000				;CONTEXT BUFFER OVERFLOWED
;
	.MCALL	.MDELET
	.MDELET	EVTDF$
;
	.ENDM	EVTDF$
.MACRO	ISTAT$	STTBL,KEYTBL,DEBUG,PNAM
	.MCALL	MTRAN$
	.IF	DF	$RONLY
	.PSECT	$STATE,D,RO
	.IFF
	.PSECT	$STATE,D
	.ENDC
STTBL::
	.IF	DF	$RONLY
	.PSECT	$KTAB,D,RO
	.IFF
	.PSECT	$KTAB,D
	.ENDC
KEYTBL::
	.IF	DF	$RONLY
	.PSECT	$KSTR,D,RO
	.IFF
	.PSECT	$KSTR,D
	.ENDC
	.IF	DF	$GPRM
	.IF	DF	$RONLY
	.PSECT	$PRMPT,D,RO
	.IFF
	.PSECT	$PRMPT,D
	.ENDC
	.IF	NDF	$GOFF
	.IF	B	<PNAM>
$$PRMP::
	.IFF
PNAM::
	.ENDC
	.ENDC
	.ASECT
	.=0
	.ENDC
	.IF IDN <DEBUG>,<$DEBUG>
	.MACRO DBGTP$
	.LIST
							.=.
	.NLIST
	.ENDM
	.IFF
	.MACRO DBGTP$
	.ENDM
	.ENDC
	$$$KEY = -1
	$$$FLG = -1
	$EXIT = 0
	$FAIL == -1
	$LAMDA = 300
	$NUMBR = 302
	$STRNG = 304
	$BLANK = 306
	$SUBXP = 310
	$EOS   = 312
	$DNUMB = 314
	$RAD50 = 316
	$ANY   = 320
	$ALPHA = 322
	$DIGIT = 324
	.PSECT
.ENDM	ISTAT$
.MACRO	STATE$	LABEL,PSTR,FLAG
	.PSECT	$STATE
	$$$FLG = $$$FLG!200
	MTRAN$
	.IF	NB	LABEL
LABEL:
	.ENDC
	$$$FLG = -1
	.IF	DF	$GPRM
	.IF	NB	PSTR
	.NCHR	$$$LEN,^@PSTR@
	.PSECT	$STATE
	.BYTE	$EOS,3
	.IRPC	.X.,<PSTR>
	.IF	IDN	<.X.>,<">
	.ASECT
$$$PRM=.
	.BLKB	$$$LEN+1
	.EVEN
	.IF	NDF	$GOFF
	.PSECT	$PRMPT
	.ASCIZ	<15><12>PSTR
	.EVEN
	.ENDC
	.PSECT	$STATE
	.IF	NB	FLAG
	.WORD	$$$PRM!1
	.IFF
	.WORD	$$$PRM
	.ENDC
	.MEXIT
	.ENDC
	.IF	IDN	<.X.>,<&>
	.PSECT	$STATE
	.IF	NB	FLAG
	.WORD	177776'PSTR!1
	.IFF
	.WORD	177776'PSTR
	.ENDC
	.MEXIT
	.ENDC
	.ERROR	"BAD PROMPT STRING"
	.MEXIT
	.ENDM
	.WORD	$PRGCL
	.ENDC
	.ENDC
	.PSECT
.ENDM	STATE$
.MACRO	PRMPT$,PSTR,LABEL,L
	.IF	DF	$GPRM
	.NCHR	$$$LEN,^@PSTR@
	.ASECT
LABEL:'L'	.BLKB	$$$LEN-1
	.EVEN
	.IF	NDF	$GOFF
	.PSECT	$PRMPT
	.ASCIZ	PSTR
	.EVEN
	.ENDC
	.PSECT
	.ENDC
.ENDM	PRMPT$
.MACRO	KEYWD$,KEYWRD,SYMBOL
	.PSECT	$KSTR
$$$TMP=.
	.ASCII	KEYWRD<377>
	.PSECT	$KTAB
	.WORD	$$$TMP
	.PSECT
	$$$KEY = $$$KEY+1
	SYMBOL = $$$KEY
	.ENDM	KEYWD$
.MACRO	TRAN$	TYPE,LABEL,ACTION,MASK,ADDR
	.PSECT	$STATE
	MTRAN$
	DBGTP$
	.IF	NB	ACTION
	$$$FLG = $$$FLG!2
	.MACRO	$$$ACT
	.WORD	ACTION
	.ENDM	$$$ACT
	.ENDC
	.IF	NB	MASK
	$$$FLG = $$$FLG!30
	.IF	B	ADDR
	.ERROR	"MASK ADDRESS NOT PRESENT"
	.ENDC
	.MACRO	$$$BIT
	.WORD	MASK,ADDR
	.ENDM	$$$BIT
	.ENDC
	.IF	NB	LABEL
	$$$FLG = $$$FLG!4
	$$$STA = LABEL
	.ENDC
	.IRPC	.X.,<TYPE>
	.IF	IDN	<.X.>,<">
	.PSECT	$KSTR
	$$$TMP = .
	.ASCII	TYPE<377>
	.PSECT	$KTAB
	.WORD	$$$TMP
	.MACRO	$$$TYP
	.BYTE	$$$KEY!200&277
	.ENDM	$$$TYP
	.IF	GT	$$$KEY-63.
	$$$FLG = $$$FLG!1
	.MACRO	$$$EXT
	.WORD	$$$KEY
	.ENDM	$$$EXT
	.ENDC
	$$$KEY = $$$KEY+1
	.MEXIT
	.ENDC
	.IF	IDN	<.X.>,<!>
	$$$FLG = $$$FLG!1
	.MACRO	$$$EXT
	.WORD	0'TYPE
	.ENDM	$$$EXT
	.MACRO	$$$TYP
	.BYTE	$SUBXP
	.ENDM	$$$TYP
	.MEXIT
	.ENDC
	.IF	IDN	<.X.>,<&>
	.MACRO	$$$TYP
	.BYTE	277'TYPE!200
	.ENDM	$$$TYP
	.IF	GT	177777'TYPE-63.
	$$$FLG = $$$FLG!1
	.MACRO	$$$EXT
	.WORD	177777'TYPE
	.ENDM	$$$EXT
	.ENDC
	.MEXIT
	.ENDC
	.MACRO	$$$TYP
	.BYTE	TYPE
	.ENDM	$$$TYP
	.MEXIT
	.ENDM
	.PSECT
.ENDM	TRAN$
.MACRO	MTRAN$
	.PSECT	$STATE
	.IF	EQ	$$$FLG+1
	$$$FLG = 0
	.MEXIT
	.ENDC
	$$$TYP
	.BYTE	$$$FLG
	.IF	NE	$$$FLG&1
	$$$EXT
	.ENDC
	.IF	NE	$$$FLG&2
	$$$ACT
	.ENDC
	.IF	NE	$$$FLG&10
	$$$BIT
	.ENDC
	.IF	NE	$$$FLG&4
	.WORD	$$$STA
	.IFF
	.IF	EQ	$$$FLG&200
	.ERROR	"BAD DEFAULT TRANSITION"
	.ENDC
	.ENDC
	$$$FLG = 0
.ENDM	MTRAN$
.MACRO	EXIT$S		;RSX EXIT directive
	 MOV	(PC)+,-(SP)	;Move Directive on the stack
	  .BYTE	51.,1
	 EMT	^O<377>		;Then do it
.ENDM	EXIT$S
.MACRO	EXST$S	STACOD	;RSX EXIT with Status directive
	 MOV	STACOD,-(SP)	;Push exit-status code
	 MOV	(PC)+,-(SP)	;Move directive on the stack
	 .BYTE	29.,2
	 EMT	^O<377>		;Then do it
.ENDM	EXST$S
.MACRO	$NCPMC

;.SBTTL	PREFIX FILE MACRO DEFINITIONS

.IIF	NDF	L$$IST,	.NLIST
.IIF	NDF	L$$IST,	.NLIST	CND,BEX,ME,MEB
.IIF	DF	L$$IST,	.LIST	BEX,ME,MEB,MC
.ENABL	LC

.MACRO	SETLOC	VAL,LOC,BYT
 .IF	NB   <VAL>
  .IF	IDN  <VAL>,<#0>
	 CLR'BYT	LOC
  .IFF
	 MOV'BYT	VAL,LOC
  .ENDC
 .ENDC
.ENDM	SETLOC

.MACRO	SETXRB	XRLEN.,XRBC.,XRLOC.,XRCI.,XRBLM.,XRBLK.,XRTIM.,XRMOD.
 SETLOC	<XRLEN.>,XRB+XRLEN
 SETLOC	<XRBC.>,<XRB+XRBC>
 SETLOC	<XRLOC.>,<XRB+XRLOC>
 SETLOC	<XRCI.>,<XRB+XRCI>,<B>
 SETLOC	<XRBLM.>,<XRB+XRBLKM>,<B>
 SETLOC	<XRBLK.>,<XRB+XRBLK>
 SETLOC	<XRTIM.>,<XRB+XRTIME>
 SETLOC	<XRMOD.>,<XRB+XRMOD>
.ENDM	SETXRB

; PROGRAM SECTION MACROS
.MACRO	PURE
	TMPORG	GCLPUR,,PSECT,RO,D,GBL,REL,CON
.ENDM	PURE

.MACRO	LCLPUR
	TMPORG	LCLPUR,,PSECT,RO,I,LCL,REL,CON
.ENDM	LCLPUR

.MACRO	IMPURE
	TMPORG	GCLIMP,,PSECT,RW,D,GBL,REL,CON
.ENDM	IMPURE

.MACRO	LCLIMP
	TMPORG	LCLIMP,,PSECT,RW,D,LCL,REL,CON
.ENDM	LCLIMP

.MACRO	TEXT
	TMPORG	GCLTXT,,PSECT,RO,D,GBL,REL,CON
.ENDM	TEXT

.MACRO	LCLTXT
	TMPORG	LCLTXT,,PSECT,RO,D,LCL,REL,CON
.ENDM	LCLTXT

.MACRO	IMPBYT
	TMPORG	GCLIMB,,PSECT,RW,D,GBL,REL,CON
.ENDM	IMPBYT

.MACRO	LCLIMB
	TMPORG	LCLIMB,,PSECT,RW,D,LCL,REL,CON
.ENDM	LCLIMB

.MACRO	PURBYT
	TMPORG	GCLPUB,,PSECT,RO,D,LCL,REL,CON
.ENDM	PURBYT

.MACRO	LCLPUB
	TMPORG	LCLPUB,,PSECT,RO,D,LCL,REL,CON
.ENDM	LCLPUB

; MESSAGE GENERATION MACROS
.MACRO	MESG	TXT,NOCR	;SETUP A TEXT STRING FOR TERMINAL OUTPUT
 .IF	NB	<TXT>
	 LCLTXT			;PUT TEXT IN THE TEXT AREA
$$$$$$	= .
	 .ASCIZ	TXT		;DA MESSAGE!
	 UNORG			;BACK TO PREVIOUS ORIGIN
	 CALLX	PRTMSG,R5,$$$$$$ ;PRINT THE MESSAGE ON KB:
 .ENDC	;NB	<TXT>
 .IF	B	<NOCR>
	 CALLX	PRNTCR		;RETURN THE CARRIAGE FOR THIS MESSAGE
 .ENDC	;<NOCR>
.ENDM	MESG

.MACRO	WRNMSG	TEXT,NOCR
	MESG	<.%.'TEXT>,NOCR
.ENDM	WRNMSG

.MACRO	FTLMSG	TEXT,NOCR
	MESG	<.?.'TEXT>,NOCR
.ENDM	FTLMSG

.MACRO	FTLERM	TEXT,NOCR
	MOVB	FIRQB,SAVFQE	;SAVE THE ERROR CODE FROM THE FIRQB
	FTLMSG	<TEXT>,NOCR	;ISSUE AN OPTIONAL ERROR MESSAGE
	CALLX	PRTFQS		;PRINT AN ERROR MESSAGE FROM THE SAVED CODE
	GLOBAL	<SAVFQE>
.ENDM	FTLERM

.MACRO	MSGERM	TEXT,NOCR
	MOVB	FIRQB,SAVFQE	;SAVE THE ERROR CODE FROM THE FIRQB
	MESG	<TEXT>,NOCR	;ISSUE AN OPTIONAL ERROR MESSAGE
	CALLX	PRTFQS		;PRINT AN ERROR MESSAGE FROM THE SAVED CODE
	GLOBAL	<SAVFQE>
.ENDM	MSGERM

.MACRO	WRNFQS	TEXT,NOCR
	WRNMSG	<TEXT>,NOCR	;ISSUE AN OPTIONAL ERROR MESSAGE
	CALLX	PRTFQS		;PRINT AN ERROR MESSAGE FROM THE SAVED CODE
.ENDM	WRNFQS

.MACRO	FTLFQS	TEXT,NOCR
	FTLMSG	<TEXT>,NOCR	;ISSUE AN OPTIONAL ERROR MESSAGE
	CALLX	PRTFQS		;PRINT AN ERROR MESSAGE FROM THE SAVED CODE
.ENDM	FTLFQS

.MACRO	MSGFQS	TEXT,NOCR
	MESG	<TEXT>,NOCR	;ISSUE AN OPTIONAL ERROR MESSAGE
	CALLX	PRTFQS		;PRINT AN ERROR MESSAGE FROM THE SAVED CODE
.ENDM	MSGFQS

.MACRO	NMPE	TEXT,NOCR
	CALLX	RETCON		;Return the carriage
	MESG	<"Network Management Program error">
 .IF	NB	<TEXT>
	MESG	<TEXT>,NOCR	;Issue an optional error message
 .ENDC	;NB	<TEXT>
.ENDM	NMPE

.MACRO	ERMVL0	TEXT,NOCR
	MESG	<TEXT>,<NOCAR>	;ISSUE AN OPTIONAL ERROR MESSAGE
	CALLX	PRTNM0		;Print the number in R0 in decimal
	MESG	<"">,NOCR	;Maybe return the carriage
.ENDM	ERMVL0

.MACRO	FMTXGN	NAME,TXT
	LCLTXT
TX$'NAME': .ASCIZ	TXT
TL$'NAME =: .-TX$'NAME-1
	UNORG
.ENDM	FMTXGN

.MACRO	LOGIT	MSGSFX
	.GLOBL	LOGMSG
	PCALL	LOGMSG,#TX$'MSGSFX,#TL$'MSGSFX
.ENDM	LOGIT	;MSGSFX

.MACRO	PCALLX	SUB, A,B,C,D,E,F,G,H,I,J,K,L ,DUM
  .NARG $$NARG
  .IF GT $$NARG-13.
    .ERROR	; More than 13. args to PCALLX.
  .ENDC
	 .GLOBL	SUB
	 PCALL	SUB, A,B,C,D,E,F,G,H,I,J,K,L
.ENDM	PCALLX

.MACRO	PCALL SUB, A,B,C,D,E,F,G,H,I,J,K,L ,DUM
  .NARG $$NARG
  .IF GT $$NARG-13.
    .ERROR	; More than 13. args to PCALL.
  .ENDC

  $$ARG= 13.
  .IRP X,<L,K,J,I,H,G,F,E,D,C,B,A>
    .IF NB X
	 PUSH	X	; X
    .IFF
      .IF LE $$ARG-$$NARG
	 CLR	-(SP)	; (unspecified)
      .ENDC
    .ENDC
    $$ARG= $$ARG-1
  .ENDM

	 JSR	PC,SUB
	 .POPARG \<$$NARG-1>*2
.ENDM PCALL

.MACRO	.POPARG	N
  $$TMP= N
  .IF LE N
    .ERROR ; Bad arg 'N' to PCALL.
  .IFF
    .IF EQ N-2
	 TST	(SP)+
    .IFF
      .IF EQ N-4
	 CMP	(SP)+,(SP)+
      .IFF
	 ADD	#'N,SP
      .ENDC
    .ENDC
  .ENDC
.ENDM .POPARG



.MACRO	.FRAME  A,B,C,D,E,F,G,H,I,J,K,L ,DUM
  GLOBAL CSAV
  .NARG $$NARG
  .IF GT $$NARG-12.
    .ERROR	; More than 12. args to .FRAME.
  .ENDC

	 CSAV
  $$ARG= 12
  .IRP X,<A,B,C,D,E,F,G,H,I,J,K,L>
    .IF NB X
      X=$$ARG
    .ENDC
    $$ARG= $$ARG+2
  .ENDM ;IRP
.ENDM .FRAME

.MACRO	CSAV
	 JSR	2,CSAV
	GLOBAL	<CSAV>
 .NLIST
  .SR5 = 0
  .SR4 = 2
  .SR3 = 4
  .SR2 = 6
  .SRTN = 10
 .LIST

.ENDM CSAV


.IIF	NDF	L$$IST,	.LIST

	.ENDM	$NCPMC
	.MACRO	$NCPDF

;	.SBTTL	GENERAL PARAMETERS

;+
; PREFIX CODE FOR ASSEMBLY WITH ALL NCP PARSING ROUTINES.
;-

;SET UP GENERAL  PARAMETERS AND INITIALIZE

	.ENABL	LC		;Allow mixed case messages
	.DSABL	GBL		;All references must be explicit

;DEFINE CONSTANTS

.EQUATE	OPTWD,	<3*400+0>	;TPARS OPTIONS WORD=IGNORE BLANKS,3-CHAR ABBRS
.EQUATE	CTRMAX,	15.		;MAXIMUM LINE CONTROLLER NUMBER
.EQUATE	NADMAX,	255.		;MAXIMUM NODE ADDRESS
.EQUATE	NNAMAX,	6.		;MAXIMUM LENGTH FOR NODE NAME
.EQUATE	TRBMAX,	32.		;MAXIMUM NUMBER OF TRIBUTARIES FOR A DMP/V
.EQUATE	CSTMAX,	25.		;MAXIMUM LINE COST
.EQUATE	OBJMAX, 255.		;MAXIMUM OBJECT TYPE


;DEFINE MESSAGE FORMATTER ACTION ROUTINE VALUES
.DSECT
AR.NUL:	.BLKW			;Null (no action)
AR.CET:	.BLKW			;Coded Entity Type (Param Skip if not NODE)
AR.LEM:	.BLKW			;Logging Event Mask pre-processing
AR.NNM:	.BLKW			;Node name pre-processing

;DEFINE MESSAGE FORMATTER COMPLETION ROUTINE VALUES
.DSECT
CR.NUL:	.BLKW			;Null (no action)
CR.LEM:	.BLKW			;Logging Event Mask post-processing
CR.NNM:	.BLKW			;Node name post-processing

;DEFINE PARAMETER DESCRIPTOR BLOCK OFFSETS
.DSECT
PB$DTC:	.BLKW			;DATA TYPE CODE
PB$NUM:	.BLKW			;Parameter number
PB$TDP:	.BLKW			;Text Descriptor block pointer
PB$HLR:	.BLKW			;Low/High param count or size Range values
PB$CMP:	.BLKW			;Coded Multiple Descriptor area pointer

;DEFINE MESSAGE FORMATTER DESCRIPTOR BLOCK DATA TYPE CODES
.DSECT
PDT$DE:			; DECIMAL FIELD
PDT$IM:	.BLKB		; IMAGE FIELD
PDT$UD:	.BLKB	.	; UNSIGNED DECIMAL
	.BLKB	.	; RESERVED
	.BLKB	.	; RESERVED
	.BLKB	.	; RESERVED
PDT$SD:	.BLKB	.	; SIGNED DECIMAL
PDT$HX:	.BLKB	.	; HEXADECIMAL
PDT$AS:	.BLKB	.	; ASCII IMAGE
PDT$CO:	.BLKB	.	; CODED PARAM
PDT$MU:	.BLKB	.	; MULTIPLE PARAMS
PDT$BM:	.BLKB	.	; BITMAPPED DATE FIELD


.SBTTL	COMMAND DISPATCH TABLE VALUES

;IN LOW BYTE OF $NCPCM - WHICH COMMAND?
	.DSECT
	.BLKB		;  0 = ILLEGAL COMMAND (NO COMMAND PARSED)
CM.HEL:	.BLKB		;  1 = HELP COMMAND
CM.CLR:	.BLKW		;  2 = CLEAR COMMAND
CM.DEF:	.BLKW		;  4 = DEFINE COMMAND
CM.EXI:	.BLKW		;  6 = EXIT COMMAND
CM.LST:	.BLKW		;  8 = LIST COMMAND
CM.LOP:	.BLKW		; 10 = LOOP COMMAND
CM.PUR:	.BLKW		; 12 = PURGE COMMAND
CM.SET:	.BLKW		; 14 = SET COMMAND
CM.SHO:	.BLKW		; 16 = SHOW COMMAND
CM.ZER:	.BLKW		; 18 = ZERO COMMAND
CM.VER:	.BLKW		; 20 = VERSION COMMAND
CM.DBG:	.BLKW		; 22 = ENABLE DEBUGGING COMMAND
CM.ZZZ:	.BLKW		; 24 = END OF LEGAL COMMANDS
 

;IN $NCPCM - FURTHER DETAIL ABOUT COMMAND.
	.DSECT	400
	.BLKB	400	; (UNUSED)
CM.XON:	.BLKB	400	; EXEC STATE ON
CM.XOF:	.BLKB	400	; EXEC STATE OFF
CM.XSH:	.BLKB	400	; EXEC STATE SHUT
CM.XRS:	.BLKB	400	; EXEC STATE RESTRICTED
CM.SYS:	.BLKW	400	; SYSTEM
CM.PPF:	.BLKW	400	; PERM PARAM FILE
CM.XND:	.BLKW	400	; EXECUTOR NODE

.MCALL	.MDELET
.MDELET	$NCPDF

	.ENDM	$NCPDF
