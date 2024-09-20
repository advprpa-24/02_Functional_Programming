public class Impurity {
    static int y = 0; // mutable state

    public static int f(int x) {
        y = y + 1; // side effect
        return x + y;
    }

    public static void main(String[] args) {
        System.out.println(f(3) - f(3));
        System.out.println(2 * f(3));
    }
}