package grm;

import javax.swing.SwingWorker;

public class RetrieveTableTask extends SwingWorker<Void, Void> {
    GrammarTableForm tf;

    public RetrieveTableTask(GrammarTableForm tf) {
        this.tf = tf;
    }

    public Void doInBackground() {
        tf.refreshTable();
        return null;
    }

    @Override
     public void done() {
        tf.setVisible(true);
     }
}