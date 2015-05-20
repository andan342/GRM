
package grm;
import grm.model.Rule;
import java.awt.Component;
import javax.swing.JCheckBox;
import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

public class GrammarRuleRenderer implements TableCellRenderer {
    
    public Component getTableCellRendererComponent(
                            JTable table, Object value,
                            boolean isSelected, boolean hasFocus,
                            int row, int column) {
        Rule gr = (Rule)value;
        JCheckBox cb = new JCheckBox();
        cb.setSelected(gr.status == 0);
        cb.setEnabled(gr.status <= 1);
        cb.setText(gr.toString());
        return cb;
    }
}
