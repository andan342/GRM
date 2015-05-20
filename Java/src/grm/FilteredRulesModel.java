/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package grm;

import grm.model.Grammar;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.swing.table.AbstractTableModel;

public class FilteredRulesModel extends AbstractTableModel {

    public List<RuleEntry> entries = new ArrayList<RuleEntry>();

    public Grammar g = null;

    public void addEntry(int ruleNo, int tag) {
        RuleEntry re;
        for (int i = 0; i < entries.size(); i++) {
            re = entries.get(i);
            if (re.rule.no == ruleNo) {
                if (tag < re.tag) re.tag = tag;
                return;
            }
        }
        re = new RuleEntry();
        re.rule = g.rules.get(ruleNo - 1);
        re.tag = tag;
        entries.add(re);
    }

    public void sort() {
        Collections.sort(entries);
    }

    public int getRowCount() {
        return entries.size();
    }
    public int getColumnCount() {
        return 4;
    }

    public Object getValueAt(int row, int column) {
        RuleEntry re = entries.get(row);
        StringBuffer sb = new StringBuffer("<html>");
        if (re.tag == 1) sb.append("<font color=\"#008000\">");
        switch (column) {
            case 0: SymbolSeqRenderer.writeHTML("" + re.rule.no, sb); break;
            case 3: SymbolSeqRenderer.writeHTML(re.rule.comment, sb); break;
            default:
                String[] rs = (column == 1)? re.rule.lhs : re.rule.rhs;
                for (int i = 0; i < rs.length; i++) {
                    if (i > 0) sb.append(" ");
                    SymbolSeqRenderer.writeHTML(rs[i], sb);
                }
        }
        if (re.tag == 1) sb.append("</font>");
        sb.append("</html>");
        return sb.toString();
    }

    @Override public Class getColumnClass(int c) {
        return String.class;
    }

    @Override
    public String getColumnName(int column) {
        switch (column) {
            case 0: return "No";
            case 1: return "Left";
            case 2: return "Right";
            default: return "Comment";
        }
    }
}
