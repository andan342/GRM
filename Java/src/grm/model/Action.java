package grm.model;

public class Action {
    public int[] alternatives;

    public Action(int altCnt) {
       alternatives = new int[altCnt];
    }

    public String alternativeToString(int a) {
        switch (a) {
            case -1 : return "ACCEPT";
            case 0 : return "SHIFT";
            default : return "REDUCE(" + a + ")";
        }
    }

    @Override
    public String toString() {
        StringBuffer sb = new StringBuffer();
        for (int a : alternatives) {
            if (sb.length() > 0) sb.append(" vs. ");
            sb.append(alternativeToString(a));
        }
        return sb.toString();
    }
}