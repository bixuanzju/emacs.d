package hk.hku.cs.f2j;

public abstract class Closure
{
  public Object x;
  public Object out;
  public abstract void apply ()
  ;
  public Closure clone () {
      return (Closure) ((Object) this.clone());
  }
}
