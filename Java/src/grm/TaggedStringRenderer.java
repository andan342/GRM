package grm;

import grm.model.Grammar;
import grm.model.Rule;
import grm.model.Symbol;
import javax.swing.table.DefaultTableCellRenderer;

public class TaggedStringRenderer extends DefaultTableCellRenderer  {

    public boolean symbolTag;

     public TaggedStringRenderer(boolean symbolTag) {
        this.symbolTag = symbolTag;
    }

    @Override
    public void setValue(Object value) {
        TaggedString ts = (TaggedString) value;
        int status;
        
        if (symbolTag) status = ((Symbol) ts.tag).status;
        else status = ((Rule) ts.tag).status;

        if (status > 0) {
            StringBuffer sb = new StringBuffer("<html>");
            sb.append(Grammar.statusToHTMLFontTag(status));
            SymbolSeqRenderer.writeHTML(ts.string, sb);
            sb.append("</font>");
            setText(sb.toString() );
        } else setText(ts.string );
    }
}
