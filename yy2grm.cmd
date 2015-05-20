@echo off

if "%1" == "" goto usage

amos2 grmgui.dmp -l "(yy2grm \"%1.y\" \"%1.lsp\" \"%1\" t)" -l "(quit)"
goto end

:usage
echo USAGE: yy2grm grammar
echo        to convert file .\grammar.y to .\grammar.lsp, where Lisp symbol grammar is defined.

:end