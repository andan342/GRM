/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package grm;

import grm.model.Grammar;
import java.awt.Dimension;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTree;
import javax.swing.WindowConstants;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;

public class ReversePathForm extends JFrame {
    DefaultMutableTreeNode top = new DefaultMutableTreeNode();
    DefaultTreeModel reversePathModel = new DefaultTreeModel(top);
    JTree tree = new JTree(reversePathModel);

    FilteredRulesModel rulesModel = new FilteredRulesModel();
    JTable rulesTable = new JTable(rulesModel);

    JScrollPane treeScrollPane = new JScrollPane(tree),
                rulesScrollPane = new JScrollPane(rulesTable);

    JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, treeScrollPane, rulesScrollPane);


    public ReversePathForm(Grammar g, TreeModel forwardTree, DefaultMutableTreeNode origin, String set) {

        GrammarRuleSymbol origSymbol = (GrammarRuleSymbol) origin.getUserObject();
        setTitle("Reverse path: " + origSymbol.symbol + " <- " + set + "(" + ((DefaultMutableTreeNode) forwardTree.getRoot()).getUserObject() + ")");

        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

        rulesModel.g = g;

        rulesTable.getColumnModel().getColumn(0).setMaxWidth(50);
        rulesTable.getColumnModel().getColumn(1).setPreferredWidth(150);
        rulesTable.getColumnModel().getColumn(2).setPreferredWidth(450);
        rulesTable.getColumnModel().getColumn(3).setPreferredWidth(100);

        getContentPane().add(splitPane);
        getContentPane().setPreferredSize(new Dimension(600, 500));

        splitPane.setResizeWeight(0.5);
        pack();
        setLocationRelativeTo(null); //center on screen

        buildReversePath(forwardTree, origin);
        showFilteredRules();
    }

    public void buildReversePath(TreeModel forwardTree, DefaultMutableTreeNode origin) {
        top.setUserObject(origin.getUserObject());
        extendReversePath(top, forwardTree, origin);
        for (int i=0; i<tree.getRowCount(); i++) tree.expandRow(i);
        tree.updateUI();
    }
    
    public void extendReversePath(DefaultMutableTreeNode extendPoint, TreeModel forwardTree, DefaultMutableTreeNode node) {
        if (node.getParent() != null) { //add to filtered rules table
            GrammarRuleSymbol grs = (GrammarRuleSymbol) node.getUserObject();
            for (int i = 0; i < grs.rules.length; i++)
                rulesModel.addEntry(grs.rules[i], (grs.kind == 0 || grs.kind == 1)? 1 : 0);
        }

        DefaultMutableTreeNode par = (DefaultMutableTreeNode) node.getParent();
        if (par != null) {
            DefaultMutableTreeNode y = new DefaultMutableTreeNode(par.getUserObject());
            extendPoint.add(y); //add to tree

            extendReversePath(y, forwardTree, par); //continue this path
            if (par.getParent() != null && ((GrammarRuleSymbol) y.getUserObject()).nt) { //look only for references to NT non-root forward nodes
                addReferences(y, forwardTree, (DefaultMutableTreeNode) forwardTree.getRoot(), par);
            }            
        }
    }
    
    public void addReferences(DefaultMutableTreeNode extendPoint, TreeModel forwardTree, DefaultMutableTreeNode node, DefaultMutableTreeNode curBranch) {
        for (int i = 0; i < node.getChildCount(); i++) {
            DefaultMutableTreeNode child = (DefaultMutableTreeNode) node.getChildAt(i);
            GrammarRuleSymbol cs = (GrammarRuleSymbol) child.getUserObject();
            if (cs == curBranch.getUserObject()) break; //never look into the branch where reverse path originates
            if (cs.original != null) { //if a reference node
                DefaultMutableTreeNode referredNode  = (DefaultMutableTreeNode) cs.original.getLastPathComponent();
                if (referredNode.getUserObject() == extendPoint.getUserObject()) { //and refers to the node copied into the extendPoint
                    extendReversePath(extendPoint, forwardTree, child); //add reverese path to its parent
                }
            } else 
                addReferences(extendPoint, forwardTree, child, curBranch);
        }
    }

    public void showFilteredRules() {
        rulesModel.sort();
        rulesTable.repaint();
    }





}
