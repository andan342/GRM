package grm;

import callin.AmosException;
import callin.Scan;
import callin.Tuple;
import grm.model.Action;
import grm.model.Key;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.WindowConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

public class GrammarTableForm extends JFrame {
    public final static int PT = 0, CT = 1;
    public Key selectedKey = null;

    public MainForm mainForm;
    public int purpose;
    String defaultColumnHeaders[] = {"Key"};
    DynamicTableModel model = new DynamicTableModel(defaultColumnHeaders, 0);
    JTable table = new JTable(model);
    JScrollPane scrollPane = new JScrollPane(table, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
            JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    
    

    public GrammarTableForm(MainForm mainForm, String title, int purpose) {
        setTitle(title);
        this.mainForm = mainForm;
        this.purpose = purpose;
        table.setRowSelectionAllowed(false);
        //ptTable.setAutoscrolls(true);
        //scrollPane.setHorizontalScrollBar(new JScrollBar());
        table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        
        table.setDefaultRenderer(Key.class, new GrammarKeyRenderer(this));
        if (purpose == CT)
            table.setDefaultRenderer(Action.class, new GrammarActionRenderer(this));

        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        getContentPane().add(scrollPane);
        pack();
        
        table.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                tableSelectionChanged();
            }
        });

        table.getColumnModel().getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                tableSelectionChanged();
            }
        });
        
        addWindowListener(new WindowAdapter() {
            @Override public void windowClosed(WindowEvent e) {
                selectedKey = null;
                refreshMainForm();
            }
        });
    }

    public void refreshTable() {
        mainForm.progressBar.setIndeterminate(false);
        mainForm.progressBar.setStringPainted(true);
        mainForm.progressBar.setString("Retrieving " + getTitle() + "...");
        try {
            Tuple args = new Tuple(2);
            args.setElem(0, mainForm.g.gid);
            args.setElem(1, purpose);
            Scan scan = mainForm.ap.conn.callFunction(mainForm.ap.getHeaderFn, args);
            Tuple ptHeader = scan.getRow().getSeqElem(0);
            String[] columnHeaders = mainForm.g.sidsToStrings(ptHeader, 1);
            columnHeaders[0] = defaultColumnHeaders[0];
            model.setColumnIdentifiers(columnHeaders);
            model.setColumnCount(columnHeaders.length);

            args = new Tuple(1);
            args.setElem(0, mainForm.g.gid);
            model.setRowCount(mainForm.g.keyCnt);
            scan = mainForm.ap.conn.callFunction((purpose == PT)? mainForm.ap.getPTRowsFn : mainForm.ap.getCTRowsFn, args);
            int cnt = 0,
                progressStep = mainForm.g.keyCnt / 100;
            if (progressStep == 0) progressStep = 1;

            while (!scan.eos()) {
                Tuple res = scan.getRow();
                int kid = res.getIntElem(0);
                Tuple resRow = res.getSeqElem(1);
                model.setValueAt(mainForm.g.keys.get(new Integer(kid)), kid, 0);
                for (int i = 1; i < columnHeaders.length; i++) {
                    if (purpose == PT) {
                        int eltKid = resRow.getIntElem(i - 1);
                        model.setValueAt((eltKid == 0) ? mainForm.g.noKey : mainForm.g.keys.get(new Integer(eltKid)), kid, i);
                    } else {
                        Action a;
                        if (resRow.isTuple(i - 1)) {
                            Tuple conflictActionTpl = resRow.getSeqElem(i - 1);
                            a = new Action(conflictActionTpl.getArity());
                            for (int j = 0; j < a.alternatives.length; j++)
                                a.alternatives[j] = conflictActionTpl.getIntElem(j);
                        } else if (resRow.isInteger(i - 1)) {
                            a = new Action(1);
                            a.alternatives[0] = resRow.getIntElem(i - 1);
                        } else a = mainForm.g.emptyAction;
                        model.setValueAt(a, kid, i);
                    }
                }
                scan.nextRow();
                cnt++;
                if (cnt % progressStep == 0) 
                    mainForm.progressBar.setValue(100 * cnt / mainForm.g.keyCnt);
            }
            //TODO: should auto-fit columns
/*            for (int i=0; i < columnHeaders.length; i++)
                table.getColumnModel().getColumn(i).setMinWidth(50);*/
        } catch (AmosException e) {
            JOptionPane.showMessageDialog(null, e);
        }

        mainForm.progressBar.setStringPainted(false);
        mainForm.progressBar.setValue(0);
    }

    public void tableSelectionChanged() {
        if (purpose == PT && table.getSelectedRow() >= 0 && table.getSelectedColumn() >= 0)
            selectedKey = (Key) model.getValueAt(table.getSelectedRow(), table.getSelectedColumn());
        else if (purpose == CT && table.getSelectedRow() >= 0) 
            selectedKey = (Key) model.getValueAt(table.getSelectedRow(), 0);
        else selectedKey = null;
        table.repaint();
        mainForm.rulesTable.repaint();
    }
    
    public void refreshMainForm() {
        mainForm.rulesTable.repaint();
    }
}
