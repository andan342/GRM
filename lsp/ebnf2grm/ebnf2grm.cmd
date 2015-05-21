@echo off

if "%1" == "" goto usage

amos2 -l "(with-directory (concat (getenv \"AMOS_HOME\") \"/lsp/grm/ebnf2grm\") (load \"ebnf2grm.lsp\"))" -l "(ebnf2grm \"%1.ebnf\" \"%1.lsp\" \"%1\")" -l "(quit)"
goto end

:usage
echo USAGE: ebnf2grm grammar
echo        to convert file .\grammar.ebnf to .\grammar.lsp, where Lisp symbol grammar is defined.

:end