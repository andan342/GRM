
package grm;

import grm.model.Rule;
import callin.*;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.AbstractCellEditor;
import javax.swing.JCheckBox;
import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.table.TableCellEditor;

public class GrammarRuleEditor extends AbstractCellEditor
                         implements TableCellEditor,
                                    ActionListener {
    JCheckBox cb;
    Rule gr;
    MainForm mainForm;
    
    public GrammarRuleEditor(MainForm mainForm) {
        this.mainForm = mainForm;
        cb = new JCheckBox();
        cb.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                grUpdate();
                cb.setText(gr.toString());
                updateStatusAll();
            }
        });
    }
    
    private void grUpdate() {
        if (cb.isSelected()) gr.status = 0; //turn on
        else if (gr.status <= 1) gr.status = 1; //turn off
    }
    
    private void updateStatusAll() {
        try {
            Tuple tpl = new Tuple(3); 
            tpl.setElem(0, mainForm.g.gid);
            tpl.setElem(1, gr.no);
            tpl.setElem(2, gr.status);
            mainForm.ap.conn.callFunction(mainForm.ap.updateGrammarRuleFn, tpl);
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
        cb.setText(gr.toString());
        cb.updateUI();
        return gr;
    }
    
    public Component getTableCellEditorComponent(JTable table,
                                                 Object value,
                                                 boolean isSelected,
                                                 int row,
                                                 int column) {
        gr = (Rule)value;
        cb.setSelected(gr.status == 0);
        cb.setEnabled(gr.status <= 1);
        cb.setText(gr.toString());
        return cb;
        }
}
