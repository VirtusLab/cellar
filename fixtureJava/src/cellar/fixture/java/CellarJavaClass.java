package cellar.fixture.java;

import java.util.ArrayList;
import java.util.List;

/**
 * A concrete generic Java class implementing CellarJavaInterface.
 */
public class CellarJavaClass<T extends Comparable<T>> implements CellarJavaInterface<T> {
    // Non-constant initializer, so javac emits a <clinit> for the filter to hide.
    public static final List<String> LABELS = List.of("cellar");

    private final T defaultValue;

    public CellarJavaClass(T defaultValue) {
        this.defaultValue = defaultValue;
    }

    public static <E extends Comparable<E>> CellarJavaClass<E> of(E value) {
        return new CellarJavaClass<>(value);
    }

    @Override
    public T identity(T value) {
        return value;
    }

    @Override
    public List<T> repeat(T value, int times) {
        List<T> result = new ArrayList<>(times);
        for (int i = 0; i < times; i++) {
            result.add(value);
        }
        return result;
    }

    public T getDefault() {
        return defaultValue;
    }

    public static int staticHelper(int n) {
        return n * 2;
    }

    public String format(int value) {
        return String.valueOf(value);
    }

    public String format(String value) {
        return value;
    }

    public String format(int value, boolean verbose) {
        return verbose ? "value=" + value : String.valueOf(value);
    }
}
