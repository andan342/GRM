# GRM
The GRM Project: Visual analysis tool for context-free grammars


## INSTALLATION

1. Download SSDM from 

http://www.it.uu.se/research/group/udbl/SciSPARQL/

and unpack it

2. Set AMOS_HOME environment variable to the root directory of SSDM archive

3. Run install.cmd for both *Java* and *Amos Image* compilation

4. To run, use grmgui.cmd in applications/grmgui


## PREPARING INPUT FILES

Internally, GRM uses it's own lisp-based format for representing grammars.

The converters from Bison/Yacc `.y` files, and from [Extended Bachus-Naur Form](http://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form) in version that disallows spaces in symbol names. Most standard specifications, e.g. ones from W3C (http://www.w3.org/TR/sparql11-query/#sparqlGrammar).

- to convert from Bison/Yacc file, 
The GRM Project includes converters from 

- use `yy2grm grammar` to convert file `.\grammar.y` to `.\grammar.lsp`, where Lisp symbol `grammar` will be defined.

- use `ebnf2grm grammar` to convert file `.\grammar.ebnf` to `.\grammar.lsp`, where Lisp symbol `grammar` will be defined.


