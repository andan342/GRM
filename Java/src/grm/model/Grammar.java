package grm.model;

import callin.AmosException;
import callin.Scan;
import callin.Tuple;
import grm.AmosProxy;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Grammar {
    AmosProxy ap;
    public String gid;
    public Symbol[] ts, nts;
    public int tsCnt, ntsCnt, keyCnt;
    public Symbol endMarker = new Symbol("^END^", -1, 0);
    public Key noKey = new Key(-1, "");
    public Action emptyAction = new Action(0);

    public List<Rule> rules = new ArrayList();

    public Map<Integer, Key> keys;

    public Grammar(AmosProxy ap, String id) {
        this.ap = ap;
        this.gid = id;
        keys = new HashMap<Integer, Key>();
    }

    public static String statusToString(int status) {
        switch (status) {
            case 1: return "Off";
            case 2: return "D";
            case 3: return "U";
            case 4: return "S";
            default: return "";
        }
    }

    public static String statusToHTMLFontTag(int status) {
        return "<font color=\"#" + AmosProxy.STATUS_COLORS[status] + "\">";
    }

    public Symbol getSymbol(int sid) {
        if (sid < 0) return endMarker;
        else if (sid < ntsCnt) return nts[sid];
        else return ts[sid-ntsCnt];
    }

    public String[] sidsToStrings(Tuple sids, int reserveLeft) throws AmosException {
        String[] res = new String[reserveLeft + sids.getArity()];
        for (int i = 0; i < sids.getArity(); i++) {
            res[reserveLeft + i] = getSymbol(sids.getIntElem(i)).symbol;
        }
        return res;
    }

    public void refreshKeys() throws AmosException {
        keys.clear();

        Tuple arg = new Tuple(1);
        arg.setElem(0, gid);
        Scan scan = ap.conn.callFunction(ap.getKeysFn, arg);
        keyCnt = 0;

        while (!scan.eos()) {
            Tuple res = scan.getRow();
            Key key = new Key(res.getIntElem(0), res.getStringElem(1));
            Tuple rulenos = res.getSeqElem(2);
            Tuple poss = res.getSeqElem(3);
            for (int i=0; i < rulenos.getArity(); i++)
                key.addOccurence(rulenos.getIntElem(i), poss.getIntElem(i));
            keys.put(new Integer(key.kid), key);
            keyCnt++;
            scan.nextRow();
        }
    }
}
