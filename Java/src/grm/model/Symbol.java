
package grm.model;

public class Symbol {
    public String symbol;
    public int id, status;

    public Symbol(String symbol, int id, int status) {
        this.symbol = symbol;
        this.id = id;
        this.status = status;
    }

    @Override public String toString() {
        return Grammar.statusToString(status);
    }

}
