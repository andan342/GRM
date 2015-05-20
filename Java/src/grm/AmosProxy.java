
package grm;
import callin.*;
import javax.swing.JOptionPane;

public class AmosProxy {
    public static final double REQUIRED_SSDM_VERSION = 16.300;

    public static final String[] STATUS_COLORS = {"", "AAAAAA", "AA00AA", "0000FF", "008888"};

    public Connection conn;
    public Oid prereadFn, loadFn, getRulesFn, getSymbolsFn, getCountsFn, getSetFn, getGroupedSetFn, // getFollowsFn,
               updateGrammarRuleFn, updateGrammarSymbolFn, compileGrammarFn, getKeysFn, buildCTFn,
               getHeaderFn, getPTRowsFn, getCTRowsFn;

    public AmosProxy() {
        try {
            conn = new Connection("");
            Oid getSSDMVersionFn = conn.getFunction("GET_SSDM_VERSION->NUMBER");
            Scan versionScan = conn.callFunction(getSSDMVersionFn, new Tuple(0));
            if (versionScan.eos() || versionScan.getRow().getDoubleElem(0) < REQUIRED_SSDM_VERSION)
                JOptionPane.showMessageDialog(null, "Required SSDM version is " + REQUIRED_SSDM_VERSION);
            else {
                prereadFn = conn.getFunction("charstring.preread_grammar->charstring.integer");
                loadFn = conn.getFunction("charstring.charstring.load_grammar->boolean");
                getRulesFn = conn.getFunction("CHARSTRING.GET_GRAMMAR_RULES->CHARSTRING.VECTOR-CHARSTRING.INTEGER.CHARSTRING");
                getSymbolsFn = conn.getFunction("CHARSTRING.INTEGER.GET_GRAMMAR_SYMBOLS->CHARSTRING.INTEGER.INTEGER.VECTOR-INTEGER");
                getCountsFn = conn.getFunction("CHARSTRING.INTEGER.GET_GRAMMAR_COUNTS->VECTOR-INTEGER");
                getSetFn = conn.getFunction("CHARSTRING.CHARSTRING.INTEGER.GET_GRAMMAR_SET->INTEGER.CHARSTRING.INTEGER"); //obsolete
                getGroupedSetFn = conn.getFunction("CHARSTRING.CHARSTRING.INTEGER.GET_GRAMMAR_GROUPED_SET->VECTOR-INTEGER.CHARSTRING.INTEGER");
//                getFollowsFn = conn.getFunction("CHARSTRING.CHARSTRING.GET_GRAMMAR_FOLLOWS->CHARSTRING");
                updateGrammarRuleFn = conn.getFunction("CHARSTRING.INTEGER.INTEGER.UPDATE_GRAMMAR_RULE->BOOLEAN");
                updateGrammarSymbolFn = conn.getFunction("CHARSTRING.CHARSTRING.INTEGER.UPDATE_GRAMMAR_SYMBOL->BOOLEAN");
                compileGrammarFn = conn.getFunction("CHARSTRING.COMPILE_GRAMMAR->BOOLEAN");
                getKeysFn = conn.getFunction("CHARSTRING.GET_GRAMMAR_KEYS->INTEGER.CHARSTRING.VECTOR-INTEGER.VECTOR-INTEGER");
                buildCTFn = conn.getFunction("CHARSTRING.GRAMMAR_BUILD_CT->INTEGER.VECTOR-INTEGER.CHARSTRING");
                getHeaderFn = conn.getFunction("CHARSTRING.INTEGER.GET_GRAMMAR_TABLE_HEADER->VECTOR-INTEGER");
                getPTRowsFn = conn.getFunction("CHARSTRING.GET_GRAMMAR_PT_ROWS->INTEGER.VECTOR-INTEGER");
                getCTRowsFn = conn.getFunction("CHARSTRING.GET_GRAMMAR_CT_ROWS->INTEGER.VECTOR");
            }
        } catch (AmosException e) { JOptionPane.showMessageDialog(null, e); }
    }

    public static String[] tupleToStrings(Tuple tpl) throws AmosException {
        int size = tpl.getArity();
        String[] res = new String[size];
        for (int i = 0; i < size; i++) res[i] = tpl.getStringElem(i);
        return res;
    }
}
