import java.util.function.Supplier;

public class Strict {

    public static<A> A ifThenElse(boolean cond, A _then, A _else) {
        return cond ? _then : _else;
    }

    public static int f(int x) {
        return ifThenElse(x != 0, 100/x, 0);
    } 

    // Simulated lazy evaluation
    public static<A> A ifThenElse_(Supplier<Boolean> cond, Supplier<A> _then, Supplier<A> _else) {
        return cond.get() ? _then.get() : _else.get();
    }

    public static int f_(int x) {
        return ifThenElse_(() -> x != 0, () -> 100/x, () -> 0);
    }

    public static void main(String[] args) {
        System.out.println( f(12) );
        System.out.println( f(0) );
    }
}

class Termination {
    record Pair(int first, int second){}
    
    static int forever() {
        while(true){}
    }

    public static void main(String[] args) {
        System.out.println(new Pair(1, forever()).first);
    }
}