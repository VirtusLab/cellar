package cellar.fixture.java;

/**
 * Declarations whose classfile says less, or something else, than their source.
 */
public class CellarJavaNested {
    public CellarJavaNested(String... names) {}

    public <T> CellarJavaNested(T value, int count) {}

    public static class Mid {
        public static class Inner {}
    }

    public static Mid.Inner inner() {
        return new Mid.Inner();
    }

    public static <C extends AutoCloseable> C closing(C closeable) {
        return closeable;
    }

    protected static class Hidden {}

    public class Bound {
        public Bound(int size) {}
    }

    public interface Callback {
        void call(String message);
    }
}
