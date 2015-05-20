/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package grm;

import grm.model.Rule;

public class RuleEntry implements Comparable<RuleEntry> {
    public Rule rule;
    public int tag;

    public int compareTo(RuleEntry o) {
        if (o.rule.no < rule.no) return 1;
        else if (o.rule.no > rule.no) return -1;
        else return 0;
    }
}
