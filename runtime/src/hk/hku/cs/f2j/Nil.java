package hk.hku.cs.f2j;

public class Nil implements FunctionalList { 
    public Integer head() {
        return null;
    }

    public FunctionalList tail() {
        return null;
    }
}