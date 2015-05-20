package grm.model;

import java.util.LinkedList;
import java.util.List;

public class Key {
    public int kid;
    public String symbol;
    public List<Occurence> occurences;

    public Key(int kid, String symbol) {
        this.kid = kid;
        this.symbol = symbol;
        occurences = new LinkedList<Occurence>();
    }

    @Override
    public String toString() {
        if (kid == -1) return ""; //special noKey value
        else if (kid == 0) return symbol; //bottom marker
        else return symbol + " #" + kid;
    }

    public void addOccurence(int ruleno, int pos) {
        occurences.add(new Occurence(ruleno, pos));
    }

    public String occurencesToString() {
        StringBuffer sb = new StringBuffer();
        for (Occurence o : occurences) {
            if (sb.length() > 0) sb.append(", ");
            sb.append(o.ruleno);
            sb.append(".");
            sb.append(o.pos);
        }
        return sb.toString();
    }

    public boolean containsOccurence(String symbol, int ruleno, int pos) {
        if (!this.symbol.equals(symbol)) return false;
        else for (Occurence o : occurences)
            if (o.ruleno == ruleno && o.pos == pos) return true;
        return false;
    }

}