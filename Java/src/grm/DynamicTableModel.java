
package grm;

import javax.swing.table.DefaultTableModel;

public class DynamicTableModel extends DefaultTableModel {

    public DynamicTableModel(Object[] columnNames, int rowCount) {
        super(columnNames, rowCount);
    }

     @Override public Class getColumnClass(int c) {
            return getValueAt(0, c).getClass();
        }
}
