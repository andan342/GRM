;;; GRM translation of file sparql10.ebnf
;;; generated by ebnf2grm (C) 2011, Andrej Andrejev

(defparameter sparql10-grammar '(
                                 (<Query> -> <Prologue> <SelectQuery> 1) ;1 ;[1]
                                 (<Query> -> <Prologue> <ConstructQuery> 1) ;2
                                 (<Query> -> <Prologue> <DescribeQuery> 1) ;3
                                 (<Query> -> <Prologue> <AskQuery> 1) ;4
                                 (<Prologue> -> 1) ;5 ;[2]
                                 (<Prologue> -> <PrefixDecl+> 1) ;6
                                 (<Prologue> -> <BaseDecl> 1) ;7
                                 (<Prologue> -> <BaseDecl> <PrefixDecl+> 1) ;8
                                 (<PrefixDecl+> -> <PrefixDecl> 1) ;9
                                 (<PrefixDecl+> -> <PrefixDecl+> <PrefixDecl> 1) ;10
                                 (<BaseDecl> -> BASE IRI_REF 1) ;11 ;[3]
                                 (<PrefixDecl> -> PREFIX PNAME_NS IRI_REF 1) ;12 ;[4]
                                 (<SelectQuery> -> SELECT <Var+> <WhereClause> <SolutionModifier> 1) ;13 ;[5]
                                 (<SelectQuery> -> SELECT <Var+> <DatasetClause+> <WhereClause> <SolutionModifier> 1) ;14
                                 (<SelectQuery> -> SELECT term1 <WhereClause> <SolutionModifier> 1) ;15
                                 (<SelectQuery> -> SELECT term1 <DatasetClause+> <WhereClause> <SolutionModifier> 1) ;16
                                 (<SelectQuery> -> SELECT DISTINCT <Var+> <WhereClause> <SolutionModifier> 1) ;17
                                 (<SelectQuery> -> SELECT DISTINCT <Var+> <DatasetClause+> <WhereClause> <SolutionModifier> 1) ;18
                                 (<SelectQuery> -> SELECT DISTINCT term1 <WhereClause> <SolutionModifier> 1) ;19
                                 (<SelectQuery> -> SELECT DISTINCT term1 <DatasetClause+> <WhereClause> <SolutionModifier> 1) ;20
                                 (<SelectQuery> -> SELECT REDUCED <Var+> <WhereClause> <SolutionModifier> 1) ;21
                                 (<SelectQuery> -> SELECT REDUCED <Var+> <DatasetClause+> <WhereClause> <SolutionModifier> 1) ;22
                                 (<SelectQuery> -> SELECT REDUCED term1 <WhereClause> <SolutionModifier> 1) ;23
                                 (<SelectQuery> -> SELECT REDUCED term1 <DatasetClause+> <WhereClause> <SolutionModifier> 1) ;24
                                 (<Var+> -> <Var> 1) ;25
                                 (<Var+> -> <Var+> <Var> 1) ;26
                                 (<DatasetClause+> -> <DatasetClause> 1) ;27
                                 (<DatasetClause+> -> <DatasetClause+> <DatasetClause> 1) ;28
                                 (<ConstructQuery> -> CONSTRUCT <ConstructTemplate> <WhereClause> <SolutionModifier> 1) ;29 ;[6]
                                 (<ConstructQuery> -> CONSTRUCT <ConstructTemplate> <DatasetClause+> <WhereClause> <SolutionModifier> 1) ;30
                                 (<DescribeQuery> -> DESCRIBE <VarOrIRIref+> <SolutionModifier> 1) ;31 ;[7]
                                 (<DescribeQuery> -> DESCRIBE <VarOrIRIref+> <WhereClause> <SolutionModifier> 1) ;32
                                 (<DescribeQuery> -> DESCRIBE <VarOrIRIref+> <DatasetClause+> <SolutionModifier> 1) ;33
                                 (<DescribeQuery> -> DESCRIBE <VarOrIRIref+> <DatasetClause+> <WhereClause> <SolutionModifier> 1) ;34
                                 (<DescribeQuery> -> DESCRIBE term1 <SolutionModifier> 1) ;35
                                 (<DescribeQuery> -> DESCRIBE term1 <WhereClause> <SolutionModifier> 1) ;36
                                 (<DescribeQuery> -> DESCRIBE term1 <DatasetClause+> <SolutionModifier> 1) ;37
                                 (<DescribeQuery> -> DESCRIBE term1 <DatasetClause+> <WhereClause> <SolutionModifier> 1) ;38
                                 (<VarOrIRIref+> -> <VarOrIRIref> 1) ;39
                                 (<VarOrIRIref+> -> <VarOrIRIref+> <VarOrIRIref> 1) ;40
                                 (<AskQuery> -> ASK <WhereClause> 1) ;41 ;[8]
                                 (<AskQuery> -> ASK <DatasetClause+> <WhereClause> 1) ;42
                                 (<DatasetClause> -> FROM <DefaultGraphClause> 1) ;43 ;[9]
                                 (<DatasetClause> -> FROM <NamedGraphClause> 1) ;44
                                 (<DefaultGraphClause> -> <SourceSelector> 1) ;45 ;[10]
                                 (<NamedGraphClause> -> NAMED <SourceSelector> 1) ;46 ;[11]
                                 (<SourceSelector> -> <IRIref> 1) ;47 ;[12]
                                 (<WhereClause> -> <GroupGraphPattern> 1) ;48 ;[13]
                                 (<WhereClause> -> WHERE <GroupGraphPattern> 1) ;49
                                 (<SolutionModifier> -> 1) ;50 ;[14]
                                 (<SolutionModifier> -> <LimitOffsetClauses> 1) ;51
                                 (<SolutionModifier> -> <OrderClause> 1) ;52
                                 (<SolutionModifier> -> <OrderClause> <LimitOffsetClauses> 1) ;53
                                 (<LimitOffsetClauses> -> <LimitClause> 1) ;54 ;[15]
                                 (<LimitOffsetClauses> -> <LimitClause> <OffsetClause> 1) ;55
                                 (<LimitOffsetClauses> -> <OffsetClause> 1) ;56
                                 (<LimitOffsetClauses> -> <OffsetClause> <LimitClause> 1) ;57
                                 (<OrderClause> -> ORDER BY <OrderCondition+> 1) ;58 ;[16]
                                 (<OrderCondition+> -> <OrderCondition> 1) ;59
                                 (<OrderCondition+> -> <OrderCondition+> <OrderCondition> 1) ;60
                                 (<OrderCondition> -> ASC <BrackettedExpression> 1) ;61 ;[17]
                                 (<OrderCondition> -> DESC <BrackettedExpression> 1) ;62
                                 (<OrderCondition> -> <Constraint> 1) ;63
                                 (<OrderCondition> -> <Var> 1) ;64
                                 (<LimitClause> -> LIMIT INTEGER 1) ;65 ;[18]
                                 (<OffsetClause> -> OFFSET INTEGER 1) ;66 ;[19]
                                 (<GroupGraphPattern> -> term2 term3 1) ;67 ;[20]
                                 (<GroupGraphPattern> -> term2 <aux1> term3 1) ;68
                                 (<GroupGraphPattern> -> term2 <TriplesBlock> term3 1) ;69
                                 (<GroupGraphPattern> -> term2 <TriplesBlock> <aux1> term3 1) ;70
                                 (<aux1> -> <GraphPatternNotTriples> 1) ;71
                                 (<aux1> -> <GraphPatternNotTriples> <TriplesBlock> 1) ;72
                                 (<aux1> -> <GraphPatternNotTriples> term4 1) ;73
                                 (<aux1> -> <GraphPatternNotTriples> term4 <TriplesBlock> 1) ;74
                                 (<aux1> -> <Filter> 1) ;75
                                 (<aux1> -> <Filter> <TriplesBlock> 1) ;76
                                 (<aux1> -> <Filter> term4 1) ;77
                                 (<aux1> -> <Filter> term4 <TriplesBlock> 1) ;78
                                 (<aux1> -> <aux1> <GraphPatternNotTriples> 1) ;79
                                 (<aux1> -> <aux1> <GraphPatternNotTriples> <TriplesBlock> 1) ;80
                                 (<aux1> -> <aux1> <GraphPatternNotTriples> term4 1) ;81
                                 (<aux1> -> <aux1> <GraphPatternNotTriples> term4 <TriplesBlock> 1) ;82
                                 (<aux1> -> <aux1> <Filter> 1) ;83
                                 (<aux1> -> <aux1> <Filter> <TriplesBlock> 1) ;84
                                 (<aux1> -> <aux1> <Filter> term4 1) ;85
                                 (<aux1> -> <aux1> <Filter> term4 <TriplesBlock> 1) ;86
                                 (<TriplesBlock> -> <TriplesSameSubject> 1) ;87 ;[21]
                                 (<TriplesBlock> -> <TriplesSameSubject> term4 1) ;88
                                 (<TriplesBlock> -> <TriplesSameSubject> term4 <TriplesBlock> 1) ;89
                                 (<GraphPatternNotTriples> -> <OptionalGraphPattern> 1) ;90 ;[22]
                                 (<GraphPatternNotTriples> -> <GroupOrUnionGraphPattern> 1) ;91
                                 (<GraphPatternNotTriples> -> <GraphGraphPattern> 1) ;92
                                 (<OptionalGraphPattern> -> OPTIONAL <GroupGraphPattern> 1) ;93 ;[23]
                                 (<GraphGraphPattern> -> GRAPH <VarOrIRIref> <GroupGraphPattern> 1) ;94 ;[24]
                                 (<GroupOrUnionGraphPattern> -> <GroupGraphPattern> 1) ;95 ;[25]
                                 (<GroupOrUnionGraphPattern> -> <GroupGraphPattern> <aux2> 1) ;96
                                 (<aux2> -> UNION <GroupGraphPattern> 1) ;97
                                 (<aux2> -> <aux2> UNION <GroupGraphPattern> 1) ;98
                                 (<Filter> -> FILTER <Constraint> 1) ;99 ;[26]
                                 (<Constraint> -> <BrackettedExpression> 1) ;100 ;[27]
                                 (<Constraint> -> <BuiltInCall> 1) ;101
                                 (<Constraint> -> <FunctionCall> 1) ;102
                                 (<FunctionCall> -> <IRIref> <ArgList> 1) ;103 ;[28]
                                 (<ArgList> -> NIL 1) ;104 ;[29]
                                 (<ArgList> -> term5 <Expression> term6 1) ;105
                                 (<ArgList> -> term5 <Expression> <aux3> term6 1) ;106
                                 (<aux3> -> term7 <Expression> 1) ;107
                                 (<aux3> -> <aux3> term7 <Expression> 1) ;108
                                 (<ConstructTemplate> -> term2 term3 1) ;109 ;[30]
                                 (<ConstructTemplate> -> term2 <ConstructTriples> term3 1) ;110
                                 (<ConstructTriples> -> <TriplesSameSubject> 1) ;111 ;[31]
                                 (<ConstructTriples> -> <TriplesSameSubject> term4 1) ;112
                                 (<ConstructTriples> -> <TriplesSameSubject> term4 <ConstructTriples> 1) ;113
                                 (<TriplesSameSubject> -> <VarOrTerm> <PropertyListNotEmpty> 1) ;114 ;[32]
                                 (<TriplesSameSubject> -> <TriplesNode> <PropertyList> 1) ;115
                                 (<PropertyListNotEmpty> -> <Verb> <ObjectList> 1) ;116 ;[33]
                                 (<PropertyListNotEmpty> -> <Verb> <ObjectList> <aux4> 1) ;117
                                 (<aux4> -> term8 1) ;118
                                 (<aux4> -> term8 <Verb> <ObjectList> 1) ;119
                                 (<aux4> -> <aux4> term8 1) ;120
                                 (<aux4> -> <aux4> term8 <Verb> <ObjectList> 1) ;121
                                 (<PropertyList> -> 1) ;122 ;[34]
                                 (<PropertyList> -> <PropertyListNotEmpty> 1) ;123
                                 (<ObjectList> -> <Object> 1) ;124 ;[35]
                                 (<ObjectList> -> <Object> <aux5> 1) ;125
                                 (<aux5> -> term7 <Object> 1) ;126
                                 (<aux5> -> <aux5> term7 <Object> 1) ;127
                                 (<Object> -> <GraphNode> 1) ;128 ;[36]
                                 (<Verb> -> <VarOrIRIref> 1) ;129 ;[37]
                                 (<Verb> -> a 1) ;130
                                 (<TriplesNode> -> <Collection> 1) ;131 ;[38]
                                 (<TriplesNode> -> <BlankNodePropertyList> 1) ;132
                                 (<BlankNodePropertyList> -> term9 <PropertyListNotEmpty> term10 1) ;133 ;[39]
                                 (<Collection> -> term5 <GraphNode+> term6 1) ;134 ;[40]
                                 (<GraphNode+> -> <GraphNode> 1) ;135
                                 (<GraphNode+> -> <GraphNode+> <GraphNode> 1) ;136
                                 (<GraphNode> -> <VarOrTerm> 1) ;137 ;[41]
                                 (<GraphNode> -> <TriplesNode> 1) ;138
                                 (<VarOrTerm> -> <Var> 1) ;139 ;[42]
                                 (<VarOrTerm> -> <GraphTerm> 1) ;140
                                 (<VarOrIRIref> -> <Var> 1) ;141 ;[43]
                                 (<VarOrIRIref> -> <IRIref> 1) ;142
                                 (<Var> -> VAR1 1) ;143 ;[44]
                                 (<Var> -> VAR2 1) ;144
                                 (<GraphTerm> -> <IRIref> 1) ;145 ;[45]
                                 (<GraphTerm> -> <RDFLiteral> 1) ;146
                                 (<GraphTerm> -> <NumericLiteral> 1) ;147
                                 (<GraphTerm> -> <BooleanLiteral> 1) ;148
                                 (<GraphTerm> -> <BlankNode> 1) ;149
                                 (<GraphTerm> -> NIL 1) ;150
                                 (<Expression> -> <ConditionalOrExpression> 1) ;151 ;[46]
                                 (<ConditionalOrExpression> -> <ConditionalAndExpression> 1) ;152 ;[47]
                                 (<ConditionalOrExpression> -> <ConditionalAndExpression> <aux6> 1) ;153
                                 (<aux6> -> term11 <ConditionalAndExpression> 1) ;154
                                 (<aux6> -> <aux6> term11 <ConditionalAndExpression> 1) ;155
                                 (<ConditionalAndExpression> -> <ValueLogical> 1) ;156 ;[48]
                                 (<ConditionalAndExpression> -> <ValueLogical> <aux7> 1) ;157
                                 (<aux7> -> term12 <ValueLogical> 1) ;158
                                 (<aux7> -> <aux7> term12 <ValueLogical> 1) ;159
                                 (<ValueLogical> -> <RelationalExpression> 1) ;160 ;[49]
                                 (<RelationalExpression> -> <NumericExpression> 1) ;161 ;[50]
                                 (<RelationalExpression> -> <NumericExpression> term13 <NumericExpression> 1) ;162
                                 (<RelationalExpression> -> <NumericExpression> term14 <NumericExpression> 1) ;163
                                 (<RelationalExpression> -> <NumericExpression> < <NumericExpression> 1) ;164
                                 (<RelationalExpression> -> <NumericExpression> term15 <NumericExpression> 1) ;165
                                 (<RelationalExpression> -> <NumericExpression> term16 <NumericExpression> 1) ;166
                                 (<RelationalExpression> -> <NumericExpression> term17 <NumericExpression> 1) ;167
                                 (<NumericExpression> -> <AdditiveExpression> 1) ;168 ;[51]
                                 (<AdditiveExpression> -> <MultiplicativeExpression> 1) ;169 ;[52]
                                 (<AdditiveExpression> -> <MultiplicativeExpression> <aux8> 1) ;170
                                 (<aux8> -> term18 <MultiplicativeExpression> 1) ;171
                                 (<aux8> -> term19 <MultiplicativeExpression> 1) ;172
                                 (<aux8> -> <NumericLiteralPositive> 1) ;173
                                 (<aux8> -> <NumericLiteralNegative> 1) ;174
                                 (<aux8> -> <aux8> term18 <MultiplicativeExpression> 1) ;175
                                 (<aux8> -> <aux8> term19 <MultiplicativeExpression> 1) ;176
                                 (<aux8> -> <aux8> <NumericLiteralPositive> 1) ;177
                                 (<aux8> -> <aux8> <NumericLiteralNegative> 1) ;178
                                 (<MultiplicativeExpression> -> <UnaryExpression> 1) ;179 ;[53]
                                 (<MultiplicativeExpression> -> <UnaryExpression> <aux9> 1) ;180
                                 (<aux9> -> term1 <UnaryExpression> 1) ;181
                                 (<aux9> -> term20 <UnaryExpression> 1) ;182
                                 (<aux9> -> <aux9> term1 <UnaryExpression> 1) ;183
                                 (<aux9> -> <aux9> term20 <UnaryExpression> 1) ;184
                                 (<UnaryExpression> -> term21 <PrimaryExpression> 1) ;185 ;[54]
                                 (<UnaryExpression> -> term18 <PrimaryExpression> 1) ;186
                                 (<UnaryExpression> -> term19 <PrimaryExpression> 1) ;187
                                 (<UnaryExpression> -> <PrimaryExpression> 1) ;188
                                 (<PrimaryExpression> -> <BrackettedExpression> 1) ;189 ;[55]
                                 (<PrimaryExpression> -> <BuiltInCall> 1) ;190
                                 (<PrimaryExpression> -> <IRIrefOrFunction> 1) ;191
                                 (<PrimaryExpression> -> <RDFLiteral> 1) ;192
                                 (<PrimaryExpression> -> <NumericLiteral> 1) ;193
                                 (<PrimaryExpression> -> <BooleanLiteral> 1) ;194
                                 (<PrimaryExpression> -> <Var> 1) ;195
                                 (<BrackettedExpression> -> term5 <Expression> term6 1) ;196 ;[56]
                                 (<BuiltInCall> -> STR term5 <Expression> term6 1) ;197 ;[57]
                                 (<BuiltInCall> -> LANG term5 <Expression> term6 1) ;198
                                 (<BuiltInCall> -> LANGMATCHES term5 <Expression> term7 <Expression> term6 1) ;199
                                 (<BuiltInCall> -> DATATYPE term5 <Expression> term6 1) ;200
                                 (<BuiltInCall> -> BOUND term5 <Var> term6 1) ;201
                                 (<BuiltInCall> -> sameTerm term5 <Expression> term7 <Expression> term6 1) ;202
                                 (<BuiltInCall> -> isIRI term5 <Expression> term6 1) ;203
                                 (<BuiltInCall> -> isURI term5 <Expression> term6 1) ;204
                                 (<BuiltInCall> -> isBLANK term5 <Expression> term6 1) ;205
                                 (<BuiltInCall> -> isLITERAL term5 <Expression> term6 1) ;206
                                 (<BuiltInCall> -> <RegexExpression> 1) ;207
                                 (<RegexExpression> -> REGEX term5 <Expression> term7 <Expression> term6 1) ;208 ;[58]
                                 (<RegexExpression> -> REGEX term5 <Expression> term7 <Expression> term7 <Expression> term6 1) ;209
                                 (<IRIrefOrFunction> -> <IRIref> 1) ;210 ;[59]
                                 (<IRIrefOrFunction> -> <IRIref> <ArgList> 1) ;211
                                 (<RDFLiteral> -> <String> 1) ;212 ;[60]
                                 (<RDFLiteral> -> <String> LANGTAG 1) ;213
                                 (<RDFLiteral> -> <String> term22 <IRIref> 1) ;214
                                 (<NumericLiteral> -> <NumericLiteralUnsigned> 1) ;215 ;[61]
                                 (<NumericLiteral> -> <NumericLiteralPositive> 1) ;216
                                 (<NumericLiteral> -> <NumericLiteralNegative> 1) ;217
                                 (<NumericLiteralUnsigned> -> INTEGER 1) ;218 ;[62]
                                 (<NumericLiteralUnsigned> -> DECIMAL 1) ;219
                                 (<NumericLiteralUnsigned> -> DOUBLE 1) ;220
                                 (<NumericLiteralPositive> -> INTEGER_POSITIVE 1) ;221 ;[63]
                                 (<NumericLiteralPositive> -> DECIMAL_POSITIVE 1) ;222
                                 (<NumericLiteralPositive> -> DOUBLE_POSITIVE 1) ;223
                                 (<NumericLiteralNegative> -> INTEGER_NEGATIVE 1) ;224 ;[64]
                                 (<NumericLiteralNegative> -> DECIMAL_NEGATIVE 1) ;225
                                 (<NumericLiteralNegative> -> DOUBLE_NEGATIVE 1) ;226
                                 (<BooleanLiteral> -> true 1) ;227 ;[65]
                                 (<BooleanLiteral> -> false 1) ;228
                                 (<String> -> STRING_LITERAL1 1) ;229 ;[66]
                                 (<String> -> STRING_LITERAL2 1) ;230
                                 (<String> -> STRING_LITERAL_LONG1 1) ;231
                                 (<String> -> STRING_LITERAL_LONG2 1) ;232
                                 (<IRIref> -> IRI_REF 1) ;233 ;[67]
                                 (<IRIref> -> <PrefixedName> 1) ;234
                                 (<PrefixedName> -> PNAME_LN 1) ;235 ;[68]
                                 (<PrefixedName> -> PNAME_NS 1) ;236
                                 (<BlankNode> -> BLANK_NODE_LABEL 1) ;237 ;[69]
                                 (<BlankNode> -> ANON 1) ;238
                                 ))

;;; This will generate SLR(1) parser, ascend.lsp should be loaded first
(make-slr1-parser (grammar-from-johnsons sparql10-grammar) nil "sparql10-slr1" "sparql10-slr1.lsp" nil)

;;; The following terminal symbols were substituted:
;; term1 = *
;; term2 = {
;; term3 = }
;; term4 = .
;; term5 = (
;; term6 = )
;; term7 = ,
;; term8 = ;
;; term9 = [
;; term10 = ]
;; term11 = ||
;; term12 = &&
;; term13 = =
;; term14 = !=
;; term15 = >
;; term16 = <=
;; term17 = >=
;; term18 = +
;; term19 = -
;; term20 = /
;; term21 = !
;; term22 = ^^