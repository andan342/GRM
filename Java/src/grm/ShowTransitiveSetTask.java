package grm;

import javax.swing.SwingWorker;

public class ShowTransitiveSetTask extends SwingWorker<Void, Void> {
    ListAndTreeForm ltf;
    String symbol;
    int kind;

    public ShowTransitiveSetTask(ListAndTreeForm ltf, String symbol, int kind) {
        this.ltf = ltf;
        this.symbol = symbol;
        this.kind = kind;
    }

    public Void doInBackground() {
        ltf.showTransitiveSet(symbol, kind);
        return null;
    }
    
    @Override
    public void done() {
        ltf.setVisible(true);
        ltf.mainForm.waitingNextButton.setEnabled(true);
    }
}
