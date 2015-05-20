
package grm;

import callin.*;
import javax.swing.JList;
import javax.swing.tree.TreePath;

public class GrammarRuleSymbol {
    public int[] rules;
    public String symbol;
    public int kind;
    public boolean nt;    
    public TreePath original = null;
    public JList list;


    public GrammarRuleSymbol(Tuple rulesVector, String symbol, int kind, boolean nt, JList list) throws AmosException {
        rules = new int[rulesVector.getArity()];
        for (int i=0; i<rules.length; i++) rules[i] = rulesVector.getIntElem(i);
        this.symbol = symbol;
        this.kind = kind;
        this.nt = nt;
        this.list = list;
    }

    public String tag2label(int tag) {
        switch (tag) {
            case -2: return "prev: ";
            case 5: return "ew: ";
            case 0: return "begins: ";

            case -1: return "next: ";
            case 4: return "bw: ";
            case 1: return "ends: ";
            default: return "";
        }
    }

    public static boolean member(String symbol, Object[] symbols) {
        for (int i=0; i < symbols.length; i++)
            if (symbol.equals(symbols[i].toString())) {
                return true;
            }
        return false;
    }

    @Override public String toString() {
        
        boolean sel = member(symbol, list.getSelectedValues());

        StringBuffer sb = new StringBuffer("<html>");
        if (sel) sb.append("<font style=\"BACKGROUND-COLOR: yellow\">");
        if (kind == 0 || kind == 1) sb.append("<font color=\"#008000\">"); //paint IDE nodes in green
        sb.append("(" + tag2label(kind));
        for (int i = 0; i < rules.length; i++) {
            if (i > 0) sb.append(" ");
            sb.append(rules[i]);
        }
        sb.append(") ");
        if (original!=null) { //paint duplicate NTs in blue (or bright-green)
            if (kind == 0 || kind == 1) sb.append("<font color=\"#00D000\">");
            else sb.append("<font color=\"#0000FF\">");
        }
        if (!nt) sb.append("<b>");
        SymbolSeqRenderer.writeHTML(symbol, sb);
        if (!nt) sb.append("</b>");
        if (original != null) sb.append("</font>");
        if (kind == 0 || kind == 1) sb.append("</font>");
        if (sel) sb.append("</font>");
        return sb.toString();
    }
}
