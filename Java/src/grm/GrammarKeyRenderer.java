package grm;

import javax.swing.table.DefaultTableCellRenderer;

public class GrammarKeyRenderer extends DefaultTableCellRenderer  {

    GrammarTableForm gtForm;

    public GrammarKeyRenderer(GrammarTableForm gtForm) {
        this.gtForm = gtForm;
    }

    @Override
    public void setValue(Object value) {
        if (value == null) setText("NULL!"); else //DEBUG
        if (value == gtForm.selectedKey) {
            StringBuffer sb = new StringBuffer("<html><font style=\"BACKGROUND-COLOR: ");
            if (gtForm.purpose == GrammarTableForm.PT) sb.append("FF00FF\">");
            else sb.append("00FF00\">");
            SymbolSeqRenderer.writeHTML(value.toString(), sb);
            sb.append("</font>");
            setText(sb.toString() );
        } else setText(value.toString());
    }
}