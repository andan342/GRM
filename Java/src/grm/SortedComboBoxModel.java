/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package grm;

import javax.swing.ComboBoxModel;

public class SortedComboBoxModel extends SortedListModel implements ComboBoxModel {
    public Object selectedItem = null;

    public Object getSelectedItem() {
        return selectedItem;
    }

    public void setSelectedItem(Object anItem) {
        selectedItem = anItem;
    }
}
