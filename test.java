public class test {

  Int local_integer;
  Int init_devl_block = new Int(7+8);

  public static void f(String s) {
    int f_local_integer;
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
