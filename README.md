# GRM
The GRM Project: Visual analysis tool for context-free grammars


## INSTALLATION

GRM is a Java Swing application, that runs on top of SSDM, mainly utilizing its Lisp interpreter and boolean matrix operations library.

*GRM can be run under Windows and Linux, however,`install`, `grmgui` and the converter scripts are currently provided for Windows only. Users are encouraged to port and test these scripts under Linux.*

1. Download SSDM from http://www.it.uu.se/research/group/udbl/SciSPARQL/ and unpack it

2. Set `AMOS_HOME` environment variable to the root directory of the unpacked SSDM archive

3. Run `install` for both *Java* and *Amos Image* compilation

4. To run, use `grmgui` in applications/grmgui


## PREPARING INPUT FILES

Internally, GRM uses its own lisp-based format for representing grammars.

Included are the converters from Bison/Yacc `.y` files, and from [Extended Bachus-Naur Form](http://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form) in version that disallows spaces in symbol names. Most standard specifications, e.g. [ones from W3C](http://www.w3.org/TR/sparql11-query/#sparqlGrammar).

- use `yy2grm grammar` to convert file `.\grammar.y` to `.\grammar.lsp`, where Lisp symbol `grammar` will be defined.

- use `ebnf2grm grammar` to convert file `.\grammar.ebnf` to `.\grammar.lsp`, where Lisp symbol `grammar` will be defined.


