
package grm;

public class TaggedString {
    public String string;
    public Object tag;
            
    public TaggedString(String string, Object tag) {
        this.string = string;
        this.tag = tag;
    }
    
    @Override public String toString() {
        return this.string;
    }
}
