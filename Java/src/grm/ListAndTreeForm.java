package grm;

import callin.AmosException;
import callin.Scan;
import callin.Tuple;
import grm.model.Symbol;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Enumeration;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTree;
import javax.swing.WindowConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

public class ListAndTreeForm extends JFrame {

    public MainForm mainForm;
    int primarySetKind;

    public SortedListModel terminalsModel = new SortedListModel(),
                           nonterminalsModel = new SortedListModel(),
                           secondarySet = new SortedListModel(),
                           res_tsModel = new SortedListModel(),
                           res_ntsModel = new SortedListModel();
    public DefaultMutableTreeNode top1 = new DefaultMutableTreeNode(),
                                  top2 = new DefaultMutableTreeNode();
    public DefaultTreeModel tree1Model = new DefaultTreeModel(top1),
                            tree2Model = new DefaultTreeModel(top2);

    public StepRegister tree1SR = new StepRegister(),
                        tree2SR = new StepRegister();

    JList tsList = new JList(terminalsModel),
          ntsList = new JList(nonterminalsModel);
    JTree tree1 = new JTree(tree1Model),
          tree2 = new JTree(tree2Model);

    JScrollPane tsScrollPane = new JScrollPane(tsList),
                ntsScrollPane = new JScrollPane(ntsList),
                tree1ScrollPane = new JScrollPane(tree1),
                tree2ScrollPane = new JScrollPane(tree2);

    JLabel secondarySetLabel = new JLabel("Secondary set: "),
           secondarySymbolLabel = new JLabel(" for ");
    JComboBox secondarySetCB = new JComboBox(),
              secondarySymbolCB = new JComboBox();
    JButton intersectButton = new JButton("Intersect >>"),
            subtractButton = new JButton("Subtract >>"),
            returnToPrimarySetButton = new JButton("<< Return to primary set");

    JPanel leftTreePanel = new JPanel(new BorderLayout()),
           leftButtonsPanel0 = new JPanel(new BorderLayout()),
           leftButtonsPanel = new JPanel(),
           rightTreePanel = new JPanel(new BorderLayout()),
           rightButtonsPanel0 = new JPanel(new BorderLayout()),
           rightButtonsPanel = new JPanel();

    JSplitPane listsSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, tsScrollPane, ntsScrollPane),
               treesSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftTreePanel, rightTreePanel),
               mainSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, listsSplitPane, treesSplitPane);

    public ListAndTreeForm(MainForm mainForm, boolean showTerminals) {
        this.mainForm = mainForm;

        /////////////////////// COMPONENTS

        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

        DefaultComboBoxModel ssetcbModel = new DefaultComboBoxModel(MainForm.ntsRSNames);
        ssetcbModel.insertElementAt("NEXT", 0);
        ssetcbModel.insertElementAt("PREV", 0);
        secondarySetCB.setModel(ssetcbModel);
        secondarySetCB.setSelectedIndex(0);

        SortedComboBoxModel ssymbcbModel = new SortedComboBoxModel();
        for (Symbol s : mainForm.g.ts)
            ssymbcbModel.add(s.symbol);
        for (Symbol s : mainForm.g.nts)
            ssymbcbModel.add(s.symbol);
        secondarySymbolCB.setModel(ssymbcbModel);

        leftButtonsPanel.add(secondarySetLabel);
        leftButtonsPanel.add(secondarySetCB);
        leftButtonsPanel.add(secondarySymbolLabel);
        leftButtonsPanel.add(secondarySymbolCB);
        leftButtonsPanel.add(intersectButton);
        leftButtonsPanel.add(subtractButton);
        leftButtonsPanel0.add(leftButtonsPanel, BorderLayout.EAST);
        leftTreePanel.add(leftButtonsPanel0, BorderLayout.NORTH);
        leftTreePanel.add(tree1ScrollPane, BorderLayout.CENTER);

        rightButtonsPanel.add(returnToPrimarySetButton);
        rightButtonsPanel0.add(rightButtonsPanel, BorderLayout.WEST);
        rightTreePanel.add(rightButtonsPanel0, BorderLayout.NORTH);
        rightTreePanel.add(tree2ScrollPane, BorderLayout.CENTER);
        rightTreePanel.setVisible(false);

        mainSplitPane.setResizeWeight(0.3);
        getContentPane().add(mainSplitPane);
        getContentPane().setPreferredSize(new Dimension(1000, 500));

        /////////////////////// EVENTS

        tsList.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent evt) {
                tree1.repaint();
                if (rightTreePanel.isVisible()) tree2.repaint();
            }
        });

        tsList.addMouseListener(new MouseAdapter() {
            @Override public void mouseClicked(MouseEvent evt) {
                symbolListMouseClicked(evt, tsList);
            }
        });

        ntsList.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent evt) {
                tree1.repaint();
                if (rightTreePanel.isVisible()) tree2.repaint();
            }
        });

        ntsList.addMouseListener(new MouseAdapter() {
            @Override public void mouseClicked(MouseEvent evt) {
                symbolListMouseClicked(evt, ntsList);
            }
        });

        tree1.addMouseListener(new MouseAdapter() {
            @Override public void mouseClicked(MouseEvent evt) {
                treeMouseClicked(evt, tree1);
            }
        });

        tree2.addMouseListener(new MouseAdapter() {
            @Override public void mouseClicked(MouseEvent evt) {
                treeMouseClicked(evt, tree2);
            }
        });

        intersectButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                intersectOrSubtract(true);
            }
        });

        subtractButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                intersectOrSubtract(false);
            }
        });

        returnToPrimarySetButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                returnToPrimarySet();
            }
        });

        //////////////////// INITIAL APPEARENCE

        listsSplitPane.setResizeWeight(0.5);
//        listsSplitPane.setMinimumSize(new Dimension(170, 300));
        tsScrollPane.setVisible(showTerminals);
        pack();
        setLocationRelativeTo(null); //center on screen
        mainSplitPane.setDividerLocation(0.3);
    }
    
    public String setKindToString(int kind) {
        switch (kind) {
            case -2: return "PREV";
            case -1: return "NEXT";
            default: return MainForm.ntsRSNames[kind];
        }
    }

    public void showTransitiveSet(String x, int kind) {
        primarySetKind = kind;
        setTitle(setKindToString(kind) + "(" + x + ")");
        secondarySymbolCB.setSelectedItem(x);
        showTransitiveSet0(x, kind, tree1, top1);
        tsList.updateUI();
        ntsList.updateUI();
    }

    public void intersectOrSubtract(boolean isIntersecting) {
        top2.removeAllChildren();
        tree2Model.reload();
        secondarySet.clear();
        showTransitiveSet0(((String) secondarySymbolCB.getSelectedItem()), secondarySetCB.getSelectedIndex() - 2, tree2, top2);

        res_tsModel.clear();
        res_ntsModel.clear();

        if (isIntersecting)
            for (int i = 0; i < secondarySet.getSize(); i++) {
                Object s = secondarySet.getElementAt(i);
                if (tsList.isVisible() && terminalsModel.contains(s)) res_tsModel.add(s);
                if (nonterminalsModel.contains(s)) res_ntsModel.add(s);
            }
        else {
            if (tsList.isVisible())
                for (int i = 0; i < terminalsModel.getSize(); i++)
                    if (!secondarySet.contains(terminalsModel.getElementAt(i))) 
                        res_tsModel.add(terminalsModel.getElementAt(i));
            for (int i = 0; i < nonterminalsModel.getSize(); i++)
                    if (!secondarySet.contains(nonterminalsModel.getElementAt(i)))
                        res_ntsModel.add(nonterminalsModel.getElementAt(i));
        }

        if (!rightTreePanel.isVisible()) {
            rightTreePanel.setVisible(true);
            treesSplitPane.setDividerLocation(0.7);
            tsList.setModel(res_tsModel);
            ntsList.setModel(res_ntsModel);
        }
        
        tsList.updateUI();
        ntsList.updateUI();
    }

    public void returnToPrimarySet() {
        rightTreePanel.setVisible(false);;
        tsList.setModel(terminalsModel);
        ntsList.setModel(nonterminalsModel);
    }

    public void showTransitiveSet0(String x, int kind, JTree tree, DefaultMutableTreeNode top) {

        mainForm.progressBar.setIndeterminate(true);
        mainForm.progressBar.setStringPainted(true);
        try {
            mainForm.progressBar.setString("Planting " + setKindToString(kind) + " nodes...");
            top.setUserObject(x);
            addNodes(tree, top, null, x, kind, kind >= 0);
            if (kind < 0) { //PREV or NEXT, kind+2 gives BEGINS or ENDS
                mainForm.progressBar.setString("Planting more " + setKindToString(kind + 2) + " nodes...");
                addNodes(tree, top, null, x, kind + 2, false);
            }
            for (int i=0; i<tree.getRowCount(); i++) tree.expandRow(i);
            tree.updateUI();
        } catch (AmosException e) {JOptionPane.showMessageDialog(null, e); }
        mainForm.progressBar.setIndeterminate(false);
        mainForm.progressBar.setStringPainted(false);               
    }

    public void addNodes(JTree tree, DefaultMutableTreeNode par, DefaultMutableTreeNode branchStart, String x, int kind, boolean simple)
            throws AmosException {
        SortedListModel tsModel = (tree == tree1)? terminalsModel : ((tsList.isVisible())? secondarySet : null),
                        ntsModel = (tree == tree1)? nonterminalsModel : secondarySet;
        Tuple arg = new Tuple(3);
        arg.setElem(0, mainForm.g.gid);
        arg.setElem(1, x);
        arg.setElem(2, kind);
        Scan scan = mainForm.ap.conn.callFunction(mainForm.ap.getGroupedSetFn, arg);
        while (!scan.eos()) {
            Tuple res = scan.getRow();
            boolean isNT = (res.getIntElem(2) == 1);
            GrammarRuleSymbol grs = new GrammarRuleSymbol(res.getSeqElem(0),
                    res.getStringElem(1), (simple)? -10 : kind, isNT, (isNT)? ntsList : tsList);

//            if ((grs.kind == 0 || grs.kind == 1) && grs.symbol.equals(x)) continue; //do not add recursive green nodes
//            if (!((kind >= 0) && existsInBranch(grs.symbol, par, branchStart))) { //add only new symbols to branch except when processing NEXT and PREV

            if ((kind < 0 || !existsInBranch(grs.symbol, par, branchStart)) // add only new symbols except for NEXT and PREV nodes
                    && (kind > 1 || !grs.symbol.equals(x))) { // do not add recursive green nodes

                DefaultMutableTreeNode yNode = new DefaultMutableTreeNode(grs);
                par.add(yNode);
                if (grs.nt) {
                    grs.original = findInSubtrees(grs, tree.getPathForRow(0));
                    if (grs.original == null) { //add recursive nodes only for the first occurence of NT in tree
                        if (simple || kind == 4 || kind == 5) { //BDW, EDW
                            ntsModel.add(grs.symbol);
                            addNodes(tree, yNode, (branchStart == null) ? par : branchStart, grs.symbol, kind, simple);
                        //processing PREV
                        } else if (kind == -2) { //PREV
                            ntsModel.add(grs.symbol);
                            addNodes(tree, yNode, null, grs.symbol, 5, false); //add all EDW nodes
                        } else if (kind == 0 ) { //BEGINS: don't add to list
                            addNodes(tree, yNode, null, grs.symbol, -2, false); //add all DF nodes
                            addNodes(tree, yNode, (branchStart == null) ? par : branchStart, grs.symbol, 0, false); //add IDB nodes
                        //processing NEXT
                        } else if (kind == -1) { //NEXT
                            ntsModel.add(grs.symbol);
                            addNodes(tree, yNode, null, grs.symbol, 4, false); //add all BDW nodes
                        } else if (kind == 1) { //ENDS: don't add to list
                            addNodes(tree, yNode, null, grs.symbol, -1, false); //add all IDFB nodes
                            addNodes(tree, yNode, (branchStart == null) ? par : branchStart, grs.symbol, 1, false); //add IDE nodes
                        }
                    }
                } else if ((simple || kind < 0 || kind > 1) && tsModel != null) //Don't add Ts for IDB nodes in PREV, IDE nodes in NEXT
                    tsModel.add(grs.symbol);              
            }
            scan.nextRow();
        }
    }

    public boolean existsInBranch(String x, DefaultMutableTreeNode par, DefaultMutableTreeNode branchStart) {
        DefaultMutableTreeNode node = par;
        if (branchStart!=null) do {
            GrammarRuleSymbol grs = (GrammarRuleSymbol)node.getUserObject();
            if (grs.symbol.equals(x)) return true;
            node = (DefaultMutableTreeNode)node.getParent();
        } while (node!=null && node!=branchStart);
        return false;
    }

    public TreePath findInSubtrees(GrammarRuleSymbol x, TreePath path) {
        TreeNode par = (TreeNode) path.getLastPathComponent();
        for (Enumeration e = par.children(); e.hasMoreElements(); ) {
            DefaultMutableTreeNode node = (DefaultMutableTreeNode) e.nextElement();
            TreePath nodePath =  path.pathByAddingChild(node);
            GrammarRuleSymbol grs = (GrammarRuleSymbol) node.getUserObject();
            if (grs != x && tagsCompatible(x.kind, grs.kind) && x.symbol.equals(grs.symbol)) return nodePath;
            else {
                TreePath recres = findInSubtrees(x, nodePath);
                if (recres!=null) return recres;
            }
        }
        return null;
    }

    public boolean tagsCompatible(int x, int y) { //TODO: support PREV
        return ((x == -1 || x == 4) && (y == -1 || y == 4))
                || (x == y);
    }

    private void treeMouseClicked(MouseEvent evt, JTree tree) {
        if (evt.getClickCount()>1) {
            DefaultMutableTreeNode clickedNode = (DefaultMutableTreeNode)tree.getSelectionPath().getLastPathComponent();
            if (clickedNode!=null) {
                GrammarRuleSymbol grs = (GrammarRuleSymbol)clickedNode.getUserObject();
                if (grs.original != null) { //show original appearance of this symbol in the tree
                    tree.setSelectionPath(grs.original);
                    tree.scrollPathToVisible(grs.original);
                } else { //show reverse path
                    ReversePathForm rpf = new ReversePathForm(mainForm.g, tree.getModel(), clickedNode,
                            (tree == tree1)? setKindToString(primarySetKind) : (String) secondarySetCB.getSelectedItem());
                    rpf.setVisible(true);
                }

            }
        }
    }

    private void symbolListMouseClicked(MouseEvent evt, JList list) {
        if (evt.getClickCount()>1) {
            String selSymbol = (String) list.getSelectedValue();
            selectNextInTree(selSymbol, tree1, tree1SR);
            if (rightTreePanel.isVisible()) selectNextInTree(selSymbol, tree2, tree2SR);
        }
    }

    private void selectNextInTree(String selSymbol, JTree tree, StepRegister sr) {
        sr.reset(selSymbol, 1);
        int i = sr.idx;
        do {
            i++;
            if (i >= tree.getRowCount()) i = 1;
            if (((GrammarRuleSymbol) ((DefaultMutableTreeNode) tree.getPathForRow(i).getLastPathComponent()).getUserObject())
                    .symbol.equals(selSymbol)) {
                tree.setSelectionRow(i);
                tree.scrollRowToVisible(i);
                sr.idx = i;
                break;
            }
        } while (i != sr.idx);
    }
}
