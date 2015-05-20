
package grm;
import callin.*;
import javax.swing.JOptionPane;

public class Main {

    public static void main(String[] args) {
        try {
            Connection.initializeAmos(args);
        } catch (AmosException e) { JOptionPane.showMessageDialog(null, "Failed to initialize Amos!"); }
        MainForm mainForm = new MainForm(new AmosProxy());
        mainForm.setVisible(true);
    }

}
