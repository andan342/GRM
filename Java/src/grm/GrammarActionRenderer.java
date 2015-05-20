package grm;

import grm.model.Action;
import javax.swing.table.DefaultTableCellRenderer;

public class GrammarActionRenderer extends DefaultTableCellRenderer  {

    GrammarTableForm ctForm;

    public GrammarActionRenderer(GrammarTableForm ctForm) {
        this.ctForm = ctForm;
    }

    @Override
    public void setValue(Object value) {
        Action a = (Action) value;

        if (a.alternatives.length > 1) {
            StringBuffer sb = new StringBuffer("<html><font color=\"#FF0000\">");
            SymbolSeqRenderer.writeHTML(value.toString(), sb);
            sb.append("</font>");
            setText(sb.toString() );
        } else setText(value.toString());
    }

}