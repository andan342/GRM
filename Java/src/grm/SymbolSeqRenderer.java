package grm;

import grm.model.SymbolSeq;
import grm.model.Grammar;
import grm.model.Rule;
import grm.model.Symbol;
import javax.swing.table.DefaultTableCellRenderer;

public class SymbolSeqRenderer extends DefaultTableCellRenderer {

    public MainForm mainForm;

    public SymbolSeqRenderer(MainForm mainForm) {
        super();
        this.mainForm = mainForm;
    }   

    public static void writeHTML(String s, StringBuffer sb) {
        for (int i=0; i<s.length(); i++) {
            if (s.charAt(i)=='<') sb.append("&lt;");
            else if (s.charAt(i)=='>') sb.append("&gt;");
            else sb.append(s.charAt(i));
        }
    }

    @Override public void setValue(Object value) {
        SymbolSeq tss = (SymbolSeq) value;
        StringBuffer sb = new StringBuffer();
        Rule rule = (Rule) tss.tag;
        boolean isDisabledBySymbol;

        if (rule != null && rule.status > 0) sb.append(Grammar.statusToHTMLFontTag(rule.status));
        for (int i = 0; i < tss.strings.length; i++) {
            String symbol = tss.strings[i];

            Symbol gs = mainForm.findSymbolInList(symbol, mainForm.tsTable);
            int pos = tss.strings.length - i;
            boolean isT = (gs != null);
            int selKind; // 1 = manual 2 = conflictKey, 3 = conflictInput, 4 = PTKey, 5 = CTKey, 6 = selT, 7 = selNT
            if (tss.role == SymbolSeq.RHS && mainForm.rulesTable.getSelectedRow() == (rule.no - 1)
                && mainForm.selectedRulePos == i)
                selKind = 1; //manual
            else if  (tss.role != SymbolSeq.INPUTS
                && mainForm.isSymbolInConflictKeySelection(symbol, (tss.role == SymbolSeq.LHS)? -1 : rule.no, pos))
                selKind = 2; //conflictKey
            else if (tss.role == SymbolSeq.RHS && mainForm.isSymbolInConflictInputSelection(symbol))
                selKind = 3; //conflictInput
            else if (mainForm.ptf != null && mainForm.ptf.isVisible()
                && tss.role == SymbolSeq.RHS && mainForm.ptf.selectedKey != null
                && mainForm.ptf.selectedKey.containsOccurence(symbol, rule.no, pos))
                selKind = 4; //PTKey
            else if (mainForm.ctf != null && mainForm.ctf.isVisible()
                && tss.role == SymbolSeq.RHS && mainForm.ctf.selectedKey != null
                && mainForm.ctf.selectedKey.containsOccurence(symbol, rule.no, pos))
                selKind = 5; //CTKey
            else if (mainForm.isSymbolInSelection(symbol, mainForm.tsTable))
                selKind = 6; //selT
            else if (mainForm.isSymbolInSelection(symbol, mainForm.ntsTable))
                selKind = 7; //selNT
            else selKind = 0; //no selection

            
            if (rule != null && rule.status == 4) {
                if (gs == null) gs = mainForm.findSymbolInList(symbol, mainForm.ntsTable);
                isDisabledBySymbol = (gs.status == 1);
            } else isDisabledBySymbol = false;

            if (sb.length() > 0) sb.append(" ");
            if (selKind > 0) sb.append("<font style=\"BACKGROUND-COLOR: ");
            switch (selKind) {
                case 1: sb.append("EEEEEE\">"); break; //silver
                case 2: if (tss.role == SymbolSeq.LHS) sb.append("FFA0A0\">"); //pink
                            else sb.append("FF0000\">"); //red
                        break;
                case 3: sb.append("orange\">"); break;
                case 4: sb.append("FF00FF\">"); break; //purple
                case 5: sb.append("00FF00\">"); break; //green
                case 6: sb.append("yellow\">"); break;
                case 7: sb.append("aqua\">"); break; //#FFA0FF
            }
            if (isT) sb.append("<b>");
            if (isDisabledBySymbol) sb.append("<u>");
            writeHTML(symbol,sb);
            if (isDisabledBySymbol) sb.append("</u>");
            if (isT) sb.append("</b>");
            if (selKind > 0) sb.append("</font>");
        }
        if (rule != null && rule.status > 0) sb.append("</font>");
        setText("<html>" + sb.toString());
    }
}
