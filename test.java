public class test {

  public static void f(String s) {
    try {
      fromXml(s);
    } catch (Exception f) {
      System.out.println(f.getMessage());
    }
  }

  public static void g(String s) {
    try {
      fromXml(s);
    } catch (Exception e) {
      System.out.println("MMM, Exceptional");
    }
  }
 
  public class testSubClass {
     public static void h(String s) {
       try {
         fromXml(s);
       } catch (Exception e) {
         System.out.println(e.getMessage());
       }
     }
  }

  public static void main(String[] args) {
    System.out.prinln("Hello world");
  }
}
