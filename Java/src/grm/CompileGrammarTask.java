package grm;

import callin.AmosException;
import callin.Scan;
import callin.Tuple;
import grm.model.Key;
import grm.model.SymbolSeq;
import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

public class CompileGrammarTask extends SwingWorker<Void, Void> {
    MainForm mainForm;
    boolean ptReady = false,
            ctReady = false;
    int conflictCnt = 0;


    public CompileGrammarTask(MainForm mainForm) {
        this.mainForm = mainForm;
    }

    public Void doInBackground() {
        mainForm.progressBar.setStringPainted(true);
        mainForm.progressBar.setString("Compiling PT...");
        mainForm.progressBar.setIndeterminate(true);
        try {
            ptReady = false;
            ctReady = false;
            Tuple arg = new Tuple(1);
            arg.setElem(0, mainForm.g.gid);
            mainForm.ap.conn.callFunction(mainForm.ap.compileGrammarFn, arg);
            ptReady = true;

            mainForm.progressBar.setString("Retrieving keys...");
            mainForm.g.refreshKeys();

            mainForm.progressBar.setString("Compiling CT...");
            //mainForm.buildCT();
            mainForm.conflictsModel.setRowCount(0);
            Scan scan = mainForm.ap.conn.callFunction(mainForm.ap.buildCTFn, arg);
            conflictCnt = 0;
            while (!scan.eos()) {
                Tuple res = scan.getRow();
                Object[] row = new Object[5];
                Key key = mainForm.g.keys.get(new Integer(res.getIntElem(0)));
                row[0] = conflictCnt;
                row[1] = key;
                row[2] = key.occurencesToString();
                row[3] = new SymbolSeq(mainForm.g.sidsToStrings(res.getSeqElem(1), 0), null, SymbolSeq.INPUTS);
                row[4] = res.getStringElem(2);
                mainForm.conflictsModel.addRow(row);
                conflictCnt++;
                mainForm.progressBar.setString("Conflicts: " + conflictCnt);
                scan.nextRow();
            }
            ctReady = true; 
        } catch (AmosException e) { JOptionPane.showMessageDialog(null, e); }

        mainForm.progressBar.setStringPainted(false);
        mainForm.progressBar.setIndeterminate(false);
        return null;
    }

     @Override
     public void done() {
        mainForm.ptButton.setEnabled(ptReady);
        mainForm.ctButton.setEnabled(ctReady);
        mainForm.conflictsScrollPane.setVisible(conflictCnt > 0);
        if (conflictCnt > 0) mainForm.outerSplitPane.setDividerLocation(0.8);
     }

}
