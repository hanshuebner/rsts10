$ _On ERROR Then _Exit
$ _DEASSIGN/ALL
$ _ASSIGN D: INPUT
$ _ASSIGN D:[1,1] LB
$ _Set Data
$ _RUN D:[1,2]BP2IC2
OLD D:[170,51]REORDR.B2S
COMPILE D:[170,51]REORDR/OBJ/CHAIN/LINE/NOWARN/LIST/CROSS:NOKEY
SCRATCH
EXIT
$ _RUN _SY0:[1,2]TKB
@D:[170,51]REORDR.CMD
$ _Delete D:[170,51]REORDR.OBJ
$ _Exit