
package grm;

import grm.model.Symbol;
import callin.AmosException;
import callin.Tuple;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.AbstractCellEditor;
import javax.swing.JCheckBox;
import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.table.TableCellEditor;

public class GrammarSymbolEditor extends AbstractCellEditor
                         implements TableCellEditor,
                                    ActionListener {
    JCheckBox cb;
    Symbol gs;
    MainForm mainForm;
    
    public GrammarSymbolEditor(MainForm mainForm) {
        this.mainForm = mainForm;
        cb = new JCheckBox();
        cb.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                grUpdate();
                cb.setText(gs.toString());
                updateStatusAll();
            }
        });
    }
    
    private void grUpdate() {
        if (cb.isSelected()) gs.status = 0; //turn on
        else if (gs.status <= 1) gs.status = 1; //turn off
    }
    
    private void updateStatusAll() {
        try {
            Tuple tpl = new Tuple(3); 
            tpl.setElem(0, mainForm.g.gid);
            tpl.setElem(1, gs.symbol);
            tpl.setElem(2, gs.status);
            mainForm.ap.conn.callFunction(mainForm.ap.updateGrammarSymbolFn, tpl);
            mainForm.updateStatusAll();
        } catch (AmosException ae) {
            JOptionPane.showMessageDialog(null, ae);
        }
    }
    
    public void actionPerformed(ActionEvent e) { //TODO: never called?
        JOptionPane.showMessageDialog(cb, e);
        fireEditingStopped();
    }

    public Object getCellEditorValue() {        
        grUpdate();
        cb.setText(gs.toString());
        cb.updateUI();
        return gs;
    }
    
    public Component getTableCellEditorComponent(JTable table,
                                                 Object value,
                                                 boolean isSelected,
                                                 int row,
                                                 int column) {
        gs = (Symbol)value;
        cb.setSelected(gs.status == 0);
        cb.setEnabled(gs.status <= 1);
        cb.setText(gs.toString());
        return cb;
    }
    
}
