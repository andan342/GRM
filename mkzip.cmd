call install.cmd

del grmgui.zip

pushd ..\..

zip applications/grmgui/grmgui applications/grmgui/grmgui.cmd applications/grmgui/yy2grm.cmd applications/grmgui/ebnf2grm.cmd  applications/grmgui/grmgui.dmp applications/grmgui/readme.txt applications/grmgui/ex2.lsp applications/grmgui/Java/build/classes/grm/*.class applications/grmgui/Java/build/classes/grm/model/*.class jarlib/swing-worker-1.1.jar jarlib/appframework-1.0.3.jar

popd

