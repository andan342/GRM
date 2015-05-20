
package grm.model;

public class SymbolSeq {
    public static final int LHS = 0, RHS = 1, INPUTS = 2;

    public String[] strings;
    public Object tag;
    public int role;
    
    public SymbolSeq(String[] strings, Object tag, int role) {
        this.strings = strings;
        this.tag = tag;
        this.role = role;
    }

    @Override public String toString() //Debug only!
    {
        StringBuffer sb = new StringBuffer();

        for (int i = 0; i < strings.length; i++) {
            if (i > 0)  sb.append(", ");
            sb.append(strings[i]);
        }
        return sb.toString();
    }
            
    
}
