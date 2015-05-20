
package grm.model;

public class Rule {
    public int no;
    public String[] lhs;
    public String[] rhs;
    public String comment;
    public int status;
    
    public Rule(int no, String[] lhs, String[] rhs, String comment, int status) {
        this.no = no;
        this.lhs = lhs;
        this.rhs = rhs;
        this.comment = comment;
        this.status = status;
    }    
    
    @Override public String toString() {
        return Grammar.statusToString(status);
    }
}
