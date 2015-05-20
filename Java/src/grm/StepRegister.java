/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package grm;

public class StepRegister {
    public Object lastFoundObject = null;
    public int idx;

    public boolean reset(Object o, int idx) {
        boolean res = !o.equals(lastFoundObject);
        if (res) { 
            this.idx = idx;
            this.lastFoundObject = o;
        }
        return res;
    }
}
