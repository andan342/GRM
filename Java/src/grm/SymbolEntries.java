
package grm;

public class SymbolEntries {

    public String s;
    public int i;

    public SymbolEntries(String s, int i) {
        this.s = s;
        this.i = i;
    }

    @Override
    public String toString() {
        return s + " (" + i + " entries)";
    }


}
