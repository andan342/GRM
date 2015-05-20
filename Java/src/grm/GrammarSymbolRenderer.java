
package grm;

import grm.model.Symbol;
import java.awt.Component;
import javax.swing.JCheckBox;
import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

public class GrammarSymbolRenderer implements TableCellRenderer {
    
    public Component getTableCellRendererComponent(
                            JTable table, Object value,
                            boolean isSelected, boolean hasFocus,
                            int row, int column) {
        Symbol gs = (Symbol)value;
        JCheckBox cb = new JCheckBox();
        cb.setSelected(gs.status == 0);
        cb.setEnabled(gs.status <= 1);
        cb.setText(gs.toString());
        return cb;
    }
    
}
