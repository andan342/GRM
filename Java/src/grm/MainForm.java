package grm;

import grm.model.SymbolSeq;
import grm.model.Rule;
import grm.model.Symbol;
import callin.AmosException;
import callin.Scan;
import callin.Tuple;
import grm.model.Grammar;
import grm.model.Key;
import grm.model.Occurence;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.Stack;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.ProgressMonitor;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;

public class MainForm extends JFrame /*implements PropertyChangeListener*/ {
    public AmosProxy ap;
    public Grammar g;
    public int lastFoundRule = -1,
               selectedRulePos = -1;
    Symbol lastFoundSymbol = null;
    Key lastFoundKey = null;
    Stack<Integer> visitedRules = new Stack<Integer>();

    public GrammarTableForm ptf = null, ctf = null;

    ShowTransitiveSetTask showNextTask;
    ProgressMonitor progressMonitor;
    JButton waitingNextButton;

    public static String rulesHeaders[] = {"Status","No","Left","Right","Comment"},
                         tsHeaders[] = {"Status", "Symbol"},
                         ntsHeaders[] = {"Status", "Symbol", "Type"},
                         conflictsHeaders[] = {"#", "Key", "Occurences", "Inputs", "Conflict"},
                         tsRSNames[] = {"BEGINS", "ENDS", "OCCURS-IN", "REDUCES-TO"},
                         ntsRSNames[] = {"BEGINS", "ENDS", "OCCURS-IN", "REDUCES-TO", "BEGINS-WITH", "ENDS-WITH", "CONTAINS", "PRODUCES"};

    DefaultTableModel rulesModel = new DynamicTableModel(rulesHeaders, 0),
                      tsModel = new DynamicTableModel(tsHeaders, 0),
                      ntsModel = new DynamicTableModel(ntsHeaders, 0),
                      conflictsModel = new DynamicTableModel(conflictsHeaders, 0);

    JTable rulesTable = new JTable(rulesModel),
           tsTable = new JTable(tsModel),
           ntsTable = new JTable(ntsModel),
           conflictsTable = new JTable(conflictsModel);

    JScrollPane rulesScrollPane = new JScrollPane(rulesTable),
                tsScrollPane = new JScrollPane(tsTable),
                ntsScrollPane = new JScrollPane(ntsTable),
                conflictsScrollPane = new JScrollPane(conflictsTable);

    JLabel rulesLabel = new JLabel("  Rules"),
           tsLabel = new JLabel("  Terminals"),
           ntsLabel = new JLabel("  Nonterminals");

    JButton loadButton = new JButton("Load Grammar..."),
            compileButton = new JButton("Compile"),
            ptButton = new JButton("PT"),
            ctButton = new JButton("CT"),
            tsPrevButton = new JButton("PREV"),
            ntsPrevButton = new JButton("PREV"),
            tsNextButton = new JButton("NEXT"),
            ntsNextButton = new JButton("NEXT"),
            tsShowRSButton = new JButton("Show"),
            ntsShowRSButton = new JButton("Show");

    JComboBox tsRSCombo = new JComboBox(tsRSNames),
              ntsRSCombo = new JComboBox(ntsRSNames);

    JPanel buttonsPanel = new JPanel(new BorderLayout()),
           progressPanel = new JPanel(new BorderLayout()),
           rulesPanel = new JPanel(new BorderLayout()),
           rulesGapPanel = new JPanel(),
           tsButtonsPanel = new JPanel(),
           ntsButtonsPanel = new JPanel(),
           tsPanel = new JPanel(new BorderLayout()),
           ntsPanel = new JPanel(new BorderLayout()),
           symbolPanel = new JPanel(new BorderLayout());

    JProgressBar progressBar = new JProgressBar(0, 100);

    JSplitPane symbolSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, tsPanel, ntsPanel),
               mainSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, rulesPanel, symbolSplitPane),
               outerSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, mainSplitPane, conflictsScrollPane);

    public MainForm(AmosProxy ap) {
        this.ap = ap;
        g = new Grammar(ap, null);

        /////////////////////// COMPONENTS
        setTitle("GRM");
        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        buttonsPanel.add(rulesLabel, BorderLayout.WEST);
        rulesGapPanel.add(loadButton);
        rulesGapPanel.add(compileButton);
        rulesGapPanel.add(ptButton);
        rulesGapPanel.add(ctButton);
        buttonsPanel.add(rulesGapPanel, BorderLayout.EAST);

        progressPanel.add(progressBar, BorderLayout.EAST);
        progressBar.setPreferredSize(new Dimension(300, 20));

        tsLabel.setPreferredSize(new Dimension(200, 30));
        ntsLabel.setPreferredSize(new Dimension(200, 30));

        rulesPanel.add(buttonsPanel, BorderLayout.NORTH);
        rulesPanel.add(rulesScrollPane, BorderLayout.CENTER);
        rulesPanel.add(progressPanel, BorderLayout.SOUTH);

        tsButtonsPanel.add(tsPrevButton);
        tsButtonsPanel.add(tsNextButton);
        tsButtonsPanel.add(tsRSCombo);
        tsButtonsPanel.add(tsShowRSButton);
        tsPanel.add(tsLabel, BorderLayout.NORTH);
        tsPanel.add(tsScrollPane, BorderLayout.CENTER);
        tsPanel.add(tsButtonsPanel, BorderLayout.SOUTH);

        ntsButtonsPanel.add(ntsPrevButton);
        ntsButtonsPanel.add(ntsNextButton);
        ntsButtonsPanel.add(ntsRSCombo);
        ntsButtonsPanel.add(ntsShowRSButton);
        ntsPanel.add(ntsLabel, BorderLayout.NORTH);
        ntsPanel.add(ntsScrollPane, BorderLayout.CENTER);
        ntsPanel.add(ntsButtonsPanel, BorderLayout.SOUTH);

        symbolSplitPane.setResizeWeight(0.5);
        mainSplitPane.setResizeWeight(0.7);
        outerSplitPane.setResizeWeight(0.85);
        rulesTable.getColumnModel().getColumn(0).setMaxWidth(50);
        rulesTable.getColumnModel().getColumn(1).setMaxWidth(50);
        rulesTable.getColumnModel().getColumn(2).setPreferredWidth(150);
        rulesTable.getColumnModel().getColumn(3).setPreferredWidth(450);
        rulesTable.getColumnModel().getColumn(4).setPreferredWidth(100);

        tsTable.getColumnModel().getColumn(0).setMaxWidth(50);
        tsTable.getColumnModel().getColumn(1).setPreferredWidth(150);
        tsTable.setAutoCreateRowSorter(true);

        ntsTable.getColumnModel().getColumn(0).setMaxWidth(50);
        ntsTable.getColumnModel().getColumn(1).setPreferredWidth(150);
        ntsTable.getColumnModel().getColumn(2).setPreferredWidth(50);
        ntsTable.setAutoCreateRowSorter(true);

        conflictsTable.getColumnModel().getColumn(0).setMaxWidth(30);
        conflictsTable.getColumnModel().getColumn(1).setPreferredWidth(150);
        conflictsTable.getColumnModel().getColumn(2).setPreferredWidth(100);
        conflictsTable.getColumnModel().getColumn(3).setPreferredWidth(400);
        conflictsTable.getColumnModel().getColumn(4).setMinWidth(150);
        conflictsTable.setAutoCreateRowSorter(true);


        getContentPane().add(outerSplitPane);
        getContentPane().setPreferredSize(new Dimension(1200, 600));

        /////////////////// EVENTS

        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                exit(0); //Exit normally on closing by user/OS
            }
        });

        loadButton.addActionListener(new ActionListener() {
           public void actionPerformed(ActionEvent e) {
               openFile();
           }
        });

        compileButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                compileGrammar();
            }
        });

        ptButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                showPTForm();
            }
        });

        ctButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                showCTForm();
            }
        });

        tsPrevButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                waitingNextButton = tsPrevButton;
                showNextSet(tsTable, -2);
            }
        });

        ntsPrevButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                waitingNextButton = ntsPrevButton;
                showNextSet(ntsTable, -2);
            }
        });

        tsNextButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                waitingNextButton = tsNextButton;
                showNextSet(tsTable, -1);
            }
        });

        ntsNextButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                waitingNextButton = ntsNextButton;
                showNextSet(ntsTable, -1);
            }
        });

         tsShowRSButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                waitingNextButton = tsShowRSButton;
                showNextSet(tsTable, tsRSCombo.getSelectedIndex());
            }
        });

        ntsShowRSButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                waitingNextButton = ntsShowRSButton;
                showNextSet(ntsTable, ntsRSCombo.getSelectedIndex());
            }
        });

        tsTable.addMouseListener(new MouseAdapter() {
            @Override public void mouseClicked(MouseEvent evt) {
                symbolsMouseClicked(evt, tsTable);
            }
        });

        ntsTable.addMouseListener(new MouseAdapter() {
            @Override public void mouseClicked(MouseEvent evt) {
                symbolsMouseClicked(evt, ntsTable);
            }
        });

        conflictsTable.addMouseListener(new MouseAdapter() {
            @Override public void mouseClicked(MouseEvent evt) {
                conflictMouseClicked(evt);
            }
        });

        tsTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                updateSymbolButtonEnabled(tsTable);
                rulesTable.repaint();
            }
        });
        
        ntsTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                updateSymbolButtonEnabled(ntsTable);
                rulesTable.repaint();
            }
        });

        conflictsTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                rulesTable.repaint();
            }
        });

        rulesTable.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (rulesTableKeyPressed(e.getKeyCode())) e.consume();
            }
        });

        /////////////////// TABLE BEHAVIOUR

        rulesTable.setDefaultRenderer(TaggedString.class, new TaggedStringRenderer(false));
        rulesTable.setDefaultRenderer(SymbolSeq.class, new SymbolSeqRenderer(this));
        rulesTable.setDefaultRenderer(Rule.class, new GrammarRuleRenderer());

        rulesTable.setDefaultEditor(Rule.class, new GrammarRuleEditor(this));

        tsTable.setDefaultRenderer(TaggedString.class, new TaggedStringRenderer(true));
        tsTable.setDefaultRenderer(Symbol.class, new GrammarSymbolRenderer());
        ntsTable.setDefaultRenderer(TaggedString.class, new TaggedStringRenderer(true));
        ntsTable.setDefaultRenderer(Symbol.class, new GrammarSymbolRenderer());
        
        tsTable.setDefaultEditor(Symbol.class, new GrammarSymbolEditor(this));
        ntsTable.setDefaultEditor(Symbol.class, new GrammarSymbolEditor(this));

        tsTable.setDefaultRenderer(SymbolSeq.class, new SymbolSeqRenderer(this));

        //////////////////// INITIAL APPEARENCE

        compileButton.setEnabled(false);
        ptButton.setEnabled(false);
        ctButton.setEnabled(false);
        tsPrevButton.setEnabled(false);
        ntsPrevButton.setEnabled(false);
        tsNextButton.setEnabled(false);
        ntsNextButton.setEnabled(false);
        tsShowRSButton.setEnabled(false);
        ntsShowRSButton.setEnabled(false);

        pack();
        setLocationRelativeTo(null); //center on screen
        mainSplitPane.setDividerLocation(0.67);
//        outerSplitPane.setDividerLocation(1.0);
        conflictsScrollPane.setVisible(false);
    }

    void exit(Integer ErrCode) {
        try {
            ap.conn.disconnect();
        } catch (AmosException e) {} //ignore all exceptions here
        System.exit(ErrCode);
    }

    public void openFile() {
        JFileChooser fc = new JFileChooser();
        fc.addChoosableFileFilter(new FileNameExtensionFilter("Lisp Files (*.lsp)","lsp"));
        fc.setCurrentDirectory(new File("."));
        if (fc.showOpenDialog(this)==JFileChooser.APPROVE_OPTION) try {
            String filename = fc.getSelectedFile().toString();
            Tuple tpl = new Tuple(1);
            tpl.addElem(0, filename);
            Scan res = ap.conn.callFunction(ap.prereadFn, tpl);
            if (res.eos()) JOptionPane.showMessageDialog(null, "No symbols are assigned in file!");
            else {
                g.gid = res.getRow().getStringElem(0);
                res.nextRow();
                if (res.eos()) loadGrammar(filename);
                else JOptionPane.showMessageDialog(null, "More than one symbol is assigned in file!");
            }
        } catch (AmosException e) { JOptionPane.showMessageDialog(null, e); }
    }

     public void loadGrammar(String filename) throws AmosException {
         // 1. Load grammar
         setTitle("GRM: " + g.gid);
         Tuple tpl = new Tuple(2);
         tpl.setElem(0, filename);
         tpl.setElem(1, g.gid);
         ap.conn.callFunction(ap.loadFn, tpl);

         Tuple arg = new Tuple(1);
         arg.setElem(0, g.gid);

         // 2. Populate rule table
         rulesModel.setRowCount(0);
         int cnt = 0;
         Scan scan = ap.conn.callFunction(ap.getRulesFn, arg);
         while (!scan.eos()) {
             Tuple res = scan.getRow();
             cnt++;
             String[] lpSeq = new String[1]; //Initialize second column with singleton tuples
             lpSeq[0] = res.getStringElem(0); //to use the same HTML renderer
             Rule gr = new Rule(cnt, lpSeq, AmosProxy.tupleToStrings(res.getSeqElem(1)), res.getStringElem(3), res.getIntElem(2));
             g.rules.add(gr);
             Object[] row = new Object[5];
             row[0] = gr;
             row[1] = new TaggedString(Integer.toString(cnt), gr);
             row[2] = new SymbolSeq(gr.lhs, gr, SymbolSeq.LHS);
             row[3] = new SymbolSeq(gr.rhs, gr, SymbolSeq.RHS);
             row[4] = new TaggedString(gr.comment, gr);

             rulesModel.addRow(row);
             scan.nextRow();
         }

         updateSymbolsList(tsTable, 1, false);
         updateSymbolsList(ntsTable, 0, false); //TODO: kinds should be uniform
         updateCounts();
         compileButton.setEnabled(true);
    }

     public void updateSymbolsList(JTable table, int kind, boolean statusOnly) throws AmosException {
         DefaultTableModel model = (DefaultTableModel) table.getModel();
         if (!statusOnly) model.setRowCount(0);
         Tuple arg = new Tuple(2);
         arg.setElem(0, g.gid);
         arg.setElem(1, kind);
         Scan scan = ap.conn.callFunction(ap.getSymbolsFn, arg);
         Symbol gs;
         String typesStr;
         int cnt = 0;
         while (!scan.eos()) {
             Tuple res = scan.getRow();
             if (kind == 0) {
                 Tuple types = res.getSeqElem(3);
                 StringBuffer sbTypes = new StringBuffer();
                 if (types.getIntElem(0) > 0) sbTypes.append("Ø"); //nullable                 
                 if (types.getIntElem(1) > 0) {
                     if (sbTypes.length() > 0) sbTypes.append(", ");
                     sbTypes.append("LR"); //left-recursive: X -> X a
                 }
                 if (types.getIntElem(2) > 0) {
                     if (sbTypes.length() > 0) sbTypes.append(", ");
                     sbTypes.append("RR"); //right-recursive: X -> a X
                 }                 
                 typesStr = sbTypes.toString();
             } else typesStr = "";
             if (statusOnly) {
                 gs = (Symbol) model.getValueAt(cnt, 0); //assume symbols always com in same order
                 gs.status = res.getIntElem(2);
                 if (kind == 0) ((TaggedString) model.getValueAt(cnt, 2)).string = typesStr;
             } else {
                 Object[] row = new Object[3];
                 gs = new Symbol(res.getStringElem(0), res.getIntElem(1), res.getIntElem(2));
                 row[0] = gs;
                 row[1] = new TaggedString(gs.symbol, gs); 
                 if (kind == 0) row[2] = new TaggedString(typesStr, gs);
                 model.addRow(row);
             }             
             scan.nextRow();
             cnt++;
         }

         if (kind == 0) {
             g.ntsCnt = cnt;
             g.nts = new Symbol[cnt];
             for (int i=0; i < cnt; i++)
                g.nts[i] = (Symbol) model.getValueAt(i, 0);
         } else {
             g.tsCnt = cnt;
             g.ts = new Symbol[cnt];
             for (int i=0; i < cnt; i++)
                g.ts[i] = (Symbol) model.getValueAt(i, 0);
         }

     }

     public void updateCounts() throws AmosException {
        rulesLabel.setText(countsToString("  Rules:", 3));
        tsLabel.setText(countsToString("  Terminals:", 1));
        ntsLabel.setText(countsToString("  Nonterminals:", 2));
    }

    public String countsToString(String prefix, int kind) throws AmosException {
        Tuple getCountsTpl = new Tuple(2);
        getCountsTpl.setElem(0, g.gid);

        getCountsTpl.setElem(1, kind);
        Scan scan = ap.conn.callFunction(ap.getCountsFn, getCountsTpl);
        if (scan.eos()) return prefix;
        else {
            Tuple counts = scan.getRow().getSeqElem(0);
            int onCount = counts.getIntElem(0);
            int offCount = counts.getIntElem(1);
            int dCount = counts.getIntElem(2);
            int uCount = counts.getIntElem(3);
            int sCount = counts.getIntElem(4);
            int totalCount = onCount + offCount + dCount + uCount + sCount;
            if (kind == 1) g.tsCnt = totalCount; //store terminal count

            StringBuffer sb = new StringBuffer("<html>&nbsp;&nbsp;" + prefix + " " + Integer.toString(totalCount) +
                                               " On: " + Integer.toString(onCount) + " Off: ");
            if (offCount > 0) sb.append("<b>");
            sb.append(offCount + sCount);
            if (offCount > 0) sb.append("</b>");
            
            sb.append(" Dead: ");
            if (dCount > 0) sb.append(Grammar.statusToHTMLFontTag(2) + "<b>");
            sb.append(dCount);
            if (dCount > 0) sb.append("</b></font>");
            
            sb.append(" Unreachable: ");
            if (uCount > 0) sb.append(Grammar.statusToHTMLFontTag(3) + "<b>");
            sb.append(uCount);
            if (uCount > 0) sb.append("</b></font>");

            return sb.toString();
        }
    }

    public void updateStatusAll() {
        try {
            Tuple arg = new Tuple(1);
            arg.setElem(0, g.gid);
            int cnt = 0;
            Scan scan = ap.conn.callFunction(ap.getRulesFn, arg);

            while (!scan.eos()) {
                Tuple res = scan.getRow();
                Rule gr = (Rule) rulesModel.getValueAt(cnt, 0); //assume rules always come in same order
                gr.status = res.getIntElem(2); //status
                scan.nextRow();
                cnt++;
            }
            updateSymbolsList(tsTable, 1, true);
            updateSymbolsList(ntsTable, 0, true); //TODO: kinds should be uniform
            tsTable.repaint();
            updateSymbolButtonEnabled(tsTable);
            ntsTable.repaint();
            updateSymbolButtonEnabled(ntsTable);
            rulesTable.repaint(); 
            updateCounts();

            compileButton.setEnabled(true);
            ptButton.setEnabled(false);
            ctButton.setEnabled(false);
        } catch (AmosException e) { JOptionPane.showMessageDialog(null, e); }
    }

    public boolean isSymbolInSelection(String symbol, JTable table) {
        TableModel model = table.getModel();
        for (int i : table.getSelectedRows()) {
            Symbol gs = (Symbol) model.getValueAt(table.convertRowIndexToModel(i), 0);
            if (symbol.equals(gs.symbol)) return true;
        }
        return false;
    }

    public boolean isSymbolInConflictKeySelection(String symbol, int ruleno, int pos) {
        for (int i : conflictsTable.getSelectedRows()) {
            Key key = (Key) conflictsModel.getValueAt(conflictsTable.convertRowIndexToModel(i), 1);
            if (symbol.equals(key.symbol)) {
                if (ruleno < 0) return true; //occurence-insensitive check
                else for (Occurence o : key.occurences)
                    if (o.ruleno == ruleno && o.pos == pos) return true; //occurence-sensitive
            }
        }
        return false;
    }

    public boolean isSymbolInConflictInputSelection(String symbol) {
        for (int i :conflictsTable.getSelectedRows()) {
            SymbolSeq inputs = (SymbolSeq) conflictsModel.getValueAt(conflictsTable.convertRowIndexToModel(i), 3);
            for (String input : inputs.strings)
                if (symbol.equals(input)) return true;
        }
        return false;
    }

    public Symbol findSymbolInList(String symbol, JTable table) {
        TableModel model = table.getModel();
        for (int i = 0; i < model.getRowCount(); i++) {
            Symbol gs = (Symbol) model.getValueAt(i, 0);
            if (symbol.equals(gs.symbol)) return gs;
        }
        return null;
    }

    public void updateSymbolButtonEnabled(JTable table) {
        boolean e = table.getSelectedRow() >= 0;
        if (e) {
            Symbol gs = (Symbol) table.getModel().getValueAt(table.convertRowIndexToModel(table.getSelectedRow()), 0);
            e = (gs.status == 0);
        }
        if (table == tsTable) {
            tsPrevButton.setEnabled(e);
            tsNextButton.setEnabled(e);
            tsShowRSButton.setEnabled(e);
        } else {
            ntsPrevButton.setEnabled(e);
            ntsNextButton.setEnabled(e);
            ntsShowRSButton.setEnabled(e);
        }
    }

    public void showNextSet(JTable table, int kind) {
        Symbol gs = null;
        if (table.getSelectedRow() >= 0)
            gs = (Symbol) table.getModel().getValueAt(table.convertRowIndexToModel(table.getSelectedRow()), 0);

        if (gs != null) {
            ListAndTreeForm ltf = new ListAndTreeForm(this, (kind < 0 || kind >= tsRSNames.length));
            showNextTask = new ShowTransitiveSetTask(ltf, gs.symbol, kind);
            showNextTask.execute();
            waitingNextButton.setEnabled(false);
        }
    }
    
    public int findSymbolInRules(String symbol, boolean lhs, boolean rhs) {
        int i = lastFoundRule;
        do {
            i++;
            if (i >= rulesModel.getRowCount()) {
                if (lastFoundRule >=0 ) i = 0;
                else return -1;
            }
            Rule gr = (Rule) rulesModel.getValueAt(i, 0);
            if (lhs) for (String rgs : gr.lhs)
                if (rgs.equals(symbol)) return i;
            if (rhs) for (String rgs : gr.rhs)
                if (rgs.equals(symbol)) return i;
        } while (i != lastFoundRule);
        return -1;                
    }

    public int findRuleInKey(Key key) {
        int i = lastFoundRule;
        do {
            i++;
            if (i >= rulesModel.getRowCount()) i = 0;
            for (Occurence o : key.occurences)
                if (i + 1 == o.ruleno) return i;
        } while (i != lastFoundRule);
        return -1;
    }

    private void symbolsMouseClicked(MouseEvent evt, JTable table) {
         Symbol gs = null;
         if (evt.getClickCount() > 1 && table.getSelectedRow() >= 0) {
            gs = (Symbol) table.getModel().getValueAt(table.convertRowIndexToModel(table.getSelectedRow()), 0);
            evt.consume();
         }

         if (gs != null && rulesModel.getRowCount() > 0)  {
             if (gs != lastFoundSymbol) {
                 lastFoundRule = -1;
                 lastFoundSymbol = gs;
             }
             selectRule(findSymbolInRules(gs.symbol, true, true));
        }
    }

    private void conflictMouseClicked(MouseEvent evt) {
         Key key = null;
         if (evt.getClickCount() > 1 && conflictsTable.getSelectedRow() >= 0) {
            key = (Key) conflictsModel.getValueAt(conflictsTable.convertRowIndexToModel(conflictsTable.getSelectedRow()), 1);
            evt.consume();
         }

         if (key != null && rulesModel.getRowCount() > 0)  {
             if (key != lastFoundKey) {
                 lastFoundRule = -1;
                 lastFoundKey = key;
             }
             selectRule(findRuleInKey(key));
        }
    }

    private void selectRule(int ruleFound) {
        lastFoundRule = ruleFound;
        if (lastFoundRule >= 0) { //scrollToVisible(rulesTable, lastFoundRule, 0);
            if (rulesTable.getSelectedRow() >= 0) visitedRules.push(new Integer(rulesTable.getSelectedRow()));
            rulesTable.getSelectionModel().setSelectionInterval(lastFoundRule, lastFoundRule);
            rulesTable.scrollRectToVisible(new Rectangle(rulesTable.getCellRect(lastFoundRule, 0, true)));
        }
    }

    private void compileGrammar() {     
        CompileGrammarTask task = new CompileGrammarTask(this);
        task.execute();
        compileButton.setEnabled(false);
    }

    private void showPTForm() {
        ptf = new GrammarTableForm(this, "Push Table", GrammarTableForm.PT);
        RetrieveTableTask rtt = new RetrieveTableTask(ptf);
        rtt.execute();
    }

    private void showCTForm() {
        ctf = new GrammarTableForm(this, "Control Table", GrammarTableForm.CT);
        RetrieveTableTask rtt = new RetrieveTableTask(ctf);
        rtt.execute();
    }

    public boolean rulesTableKeyPressed(int keyCode) {
        boolean processed = false;
        if (rulesTable.getSelectedRow() >= 0) {
            SymbolSeq rhs = (SymbolSeq) rulesModel.getValueAt(rulesTable.getSelectedRow(), 3);
            int rhsLen = rhs.strings.length;
            if (keyCode == KeyEvent.VK_LEFT) {
                if (selectedRulePos >= rhsLen) {
                    selectedRulePos = 0;
                    processed = true;
                } else if (selectedRulePos > 0) {
                    selectedRulePos--;
                    processed = true;
                }
            } else if (keyCode == KeyEvent.VK_RIGHT) {
                if (selectedRulePos < (rhsLen - 1)) selectedRulePos++;
                else selectedRulePos = rhsLen - 1;
                processed = true;
            } else if (keyCode == KeyEvent.VK_ENTER && selectedRulePos >= 0 && selectedRulePos < rhsLen) {
                lastFoundRule = -1; //always search from the beginning
                selectRule(findSymbolInRules(rhs.strings[selectedRulePos], true, false));
                return true; //consume event even if not found
            } else if (keyCode == KeyEvent.VK_BACK_SPACE && !visitedRules.isEmpty()) {
                int ruleno = visitedRules.pop().intValue();
                rulesTable.getSelectionModel().setSelectionInterval(ruleno, ruleno);
                rulesTable.scrollRectToVisible(new Rectangle(rulesTable.getCellRect(ruleno, 0, true)));
                return true;
            }
        }
        if (processed) rulesTable.repaint();
        return processed;
    }
}
