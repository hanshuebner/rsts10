$JOB/CCL/NOLIMIT/PRI:-16 [170,42]
$!
$!	CHKINI.CTL --	Check INIT map for problems in the link
$!
$ASSIGN D: DK
$ASSIGN D: SY
$SWITCH RT11
$SIZE 28
$MUNG D:[170,6]INIT.TEC D:[170,42]INIT.MAP/L
$EOJ
