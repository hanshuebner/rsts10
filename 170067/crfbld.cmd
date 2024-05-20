; 
; TKB COMMAND FILE FOR CRF
; ON A MAPPED RSX-11M SYSTEM
; USING SYSLIB
; CREATED BY SGNBLDBLD.CMD VERSION 1.62
;
SYSTEM:CRF/FP/CP/SL/MM,SYSTEM:CRF/MA/CR/-SP,SYSTEM:CRF=
CRF:CRFBLD/MP
STACK=128
TSKV=$ERVCT:10	; SST VECTOR FOR ERROR REPORTS
UNITS=7
ASG=TI:2		; ERROR MESSAGE OUTPUT DEVICE
ASG=OV:7		; WORKFILE I/O DEVICE
;
; ALLOCATE SPACE FOR TWO OPEN FILES
;
EXTSCT=$$FSR1:2040
;
; FOR 72 COLUMN OUTPUT PRECEDE THE FOLLOWING
; COMMAND WITH A SEMI-COLON.
;
EXTSCT=$$RCB0:204	; LINE LENGTH IN BYTES
;
; DUMMY ENTRY POINT TO RESOLVE UNUSED EDIT MESSAGE REFERENCES
;
GBLDEF=$CDDMG:000001
;
; SPECIFY NUMBER OF LINES PER PAGE
;
GBLDEF=L$NMAX:63
;
; SPECIFY NUMBER OF PAGE BUFFERS REQUIRED FOR
; FAST PAGE SEARCHING
;
GBLDEF=N$MPAG:24
;
; DEFINE SHORT FORMAT LINE LENGTH
;
GBLDEF=S$HFMT:122
;
; SPECIFY EXTENSION INCREMENT FOR WORKFILE
;
GBLDEF=W$KEXT:31
;
; DEFINE WORKFILE LUN
;
GBLDEF=W$KLUN:7
; 
; DELETE READ/WRITE LONG REFERENCE
; 
GBLDEF=..RWLG:0
PAR=GEN:0:70000
TASK=...CRF
;
; SET TO RUN UNDER PRIVELEGED UIC
;
UIC=[10,1]
@CRF:CRFGBL.CMD
//
