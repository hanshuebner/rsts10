$ fil = f$parse(p1,".mac")		! Expand the file spec
$ _if f$search(fil) .eqs. "" then _write 0 "?File ''fil' not found"
$ stb = f$parse(".stb",fil)
$ map = f$parse(".map",fil)
$ msg = f$parse(".msg",fil)
$
$ _copy/nolog/repl 'fil' tmpmsg.mac
$
$ _run $macro
tmpmsg=tmpmsg
$
$ _run $link
tmpmsg/z,tmpmsg,tmpmsg=tmpmsg
$
$ _run $silus
tmpmsg.sil=tmpmsg.sav/t
$
$ _copy/nolog/repl tmpmsg.sil 'msg'
$ _copy/nolog/repl tmpmsg.stb 'stb'
$ _copy/nolog/repl tmpmsg.map 'map'
$
$ _dele/nowarn/nolog tmpmsg.*
$
$ _exit 1
