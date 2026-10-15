package cellar.fixture.java;

/**
 * Static classes nested two deep, for qualified type names in signatures.
 */
public class CellarJavaNested {
    public static class Mid {
        public static class Inner {}
    }

    public static Mid.Inner inner() {
        return new Mid.Inner();
    }
}
