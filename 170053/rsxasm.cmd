; RSXASM.CMD
;
; MAC COMMAND FILE TO ASSEMBLE THE RSTS/E RSX (RSX.RTS) RUN-TIME SYSTEM
;
; KCG	15-Mar-84	Removed BP2 stuff
;
SYSTEM:RSXRTS,LST:RSXRTS/CR/-SP=CMN:COMMON,RSXCOM,RSX:RSXPRE,RSXRTS
SYSTEM:RSXIO ,LST:RSXIO /CR/-SP=CMN:COMMON,RSXCOM,RSX:RSXPRE,RSXIO
SYSTEM:RSXAST,LST:RSXAST/CR/-SP=CMN:COMMON,RSXCOM,RSX:RSXPRE,RSXAST
SYSTEM:RSXKBM,LST:RSXKBM/CR/-SP=CMN:COMMON,RSXCOM,RSX:RSXPRE,RSXKBM
SYSTEM:RSXAT ,LST:RSXAT /CR/-SP=CMN:COMMON,RSXCOM,RSX:RSXPRE,RSXAT
SYSTEM:RSXMCR,LST:RSXMCR/CR/-SP=CMN:COMMON,RSXCOM,RSX:RSXPRE,RSXMCR
SYSTEM:RSXHLP,LST:RSXHLP/CR/-SP=CMN:COMMON,RSXCOM,RSX:RSXPRE,RSXHLP
SYSTEM:PATCH ,LST:PATCH /CR/-SP=CMN:COMMON,RSXCOM,RSX:RSXPRE,PATCH
SYSTEM:ODT   ,LST:ODT   /CR/-SP=CMN:COMMON,RSXCOM,RSX:ODTPRE,ODTRSX
SYSTEM:ODTID ,LST:ODTID /CR/-SP=CMN:COMMON,RSXCOM,RSX:ODTPRE,ODTID
SYSTEM:TRACE ,LST:TRACE /CR/-SP=CMN:COMMON,RSXCOM,RSX:ODTPRE,TRACE
