package com.apicatalog.tree.io;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.apicatalog.tree.io.java.NativeComposer;
import com.apicatalog.tree.io.java.NativeTraverser;

public final class Tree {

    @SuppressWarnings("unchecked")
    public static <T> T read(TreeParser parser) throws IOException {
        return (T) read(parser, new NativeComposer());
    }

    public static <T> T read(TreeParser parser, TreeComposer<T> composer) throws IOException {
        parser.parse(composer::accept);
        return composer.compose();
    }

    public static <T> void write(T node, TreeEmitter emitter) throws IOException {
        write(new NativeTraverser(node), emitter);
    }

    public static void write(TreeTraverser<?> traverser, TreeEmitter emitter) throws IOException {
        try {
            traverser.traverse(emitter::accept);
        } catch (UncheckedIOException e) {
            throw e.getCause();
        }
    }

    public static void copy(TreeParser parser, TreeEmitter emitter) throws IOException {
        try {
            parser.parse(emitter::accept);
        } catch (UncheckedIOException e) {
            throw e.getCause();
        }
    }

    public static <T> T clone(TreeTraverser<?> traverser, TreeComposer<T> composer) {
        traverser.traverse(composer::accept);
        return composer.compose();
    }

    public static boolean identical(TreeTraverser<?> tree1, TreeTraverser<?> tree2, ScalarEquality scalarEquals) {

        while (tree1.hasNext() && tree2.hasNext()) {

            var event1 = tree1.next();
            var event2 = tree2.next();

            if (!Objects.equals(event1, event2)
                    // cursor
                    || !Objects.equals(tree1.nodeType(), tree2.nodeType())
                    || (tree1.nodeType().isScalar() && !scalarEquals.test(tree1, tree2))) {

                return false;
            }
        }

        return !tree1.hasNext() && !tree2.hasNext();
    }

    // --- Convenience & Type Coercion Methods ---

    /**
     * Returns the node's contents as a universal {@link Iterable}. This is a
     * convenience method that enables uniform iteration logic.
     * <ul>
     * <li>If the node is a collection, returns its elements.</li>
     * <li>If the node is not a collection, wraps it in a single-element
     * iterable.</li>
     * <li>If the node is {@code null}, returns an empty iterable.</li>
     * </ul>
     *
     * @param node the node to convert.
     * @return a non-null {@link Iterable} representing the node's contents.
     */
    @SuppressWarnings({ "rawtypes", "unchecked" })
    public static Iterable<? extends Object> asIterable(Object node) {
        if (node == null) {
            return List.of();
        }
        if (node instanceof Collection) {
            return (Collection) node;
        }
        if (node instanceof Stream) {
            return ((Stream<Object>) node).collect(Collectors.toList());
        }
        return List.of(node);
    }

    /**
     * Returns the node's contents as a universal {@link Stream}.
     *
     * @param node the node to convert.
     * @return a non-null {@link Stream} representing the node's contents.
     */
    public static Stream<? extends Object> asStream(Object node) {
        if (node == null) {
            return Stream.empty();
        }
        if (node instanceof Stream<?> stream) {
            return stream;
        }
        if (node instanceof Collection<?> collection) {
            return collection.stream();
        }
        return Stream.of(node);
    }

    /**
     * Returns a string representation of the node, coercing non-string scalar types
     * where applicable.
     *
     * @param node the node to convert.
     * @return a string representation of the node's value.
     */
    public static String asString(Object node) {
        if (node instanceof String stringValue) {
            return stringValue;
        }
        return Objects.toString(node);
    }

    /**
     * Converts a given node to a {@link BigDecimal}, if possible. This method can
     * be used to treat both numeric and string nodes as decimal values.
     *
     * @param node the node to convert.
     * @return the {@link BigDecimal} representation.
     * @throws NumberFormatException if a string node cannot be parsed into a
     *                               BigDecimal.
     * @throws ClassCastException    if the node is neither a number nor a string.
     */
    public static BigDecimal asDecimal(Object node) {
        if (node instanceof BigDecimal number) {
            return number;
        }
        if (node instanceof Double number) {
            return BigDecimal.valueOf(number);
        }
        if (node instanceof Float number) {
            return BigDecimal.valueOf(number);
        }
        if (node instanceof Integer number) {
            return BigDecimal.valueOf(number);
        }
        if (node instanceof Long number) {
            return BigDecimal.valueOf(number);
        }
        if (node instanceof BigInteger number) {
            return BigDecimal.valueOf(number.longValueExact());
        }
        throw new IllegalArgumentException();
    }

    public static Collection<?> asCollection(Object node) {
        return node instanceof Collection col
                ? col
                : node != null
                        ? List.of(node)
                        : List.of();
    }

    public static boolean isIntegral(Object node) {
        return node != null
                && (node instanceof Integer
                        || node instanceof Long
                        || node instanceof BigInteger);
    }

    public static boolean isNode(Object node) {
        return node == null
                || node instanceof String
                || node instanceof Boolean
                || node instanceof Integer
                || node instanceof Long
                || node instanceof BigInteger
                || node instanceof Double
                || node instanceof BigDecimal
                || node instanceof Float
                || node instanceof Map
                || node instanceof Collection
                || node instanceof byte[];
    }

    public static NodeType type(Object node) {
        if (node == null) {
            return NodeType.NULL;
        }
        if (node instanceof String) {
            return NodeType.STRING;
        }
        if (node instanceof Boolean) {
            return ((boolean) node) ? NodeType.TRUE : NodeType.FALSE;
        }
        if (node instanceof Number) {
            return NodeType.NUMBER;
        }
        if (node instanceof Map) {
            return NodeType.MAP;
        }
        if (node instanceof Collection) {
            return NodeType.SEQUENCE;
        }
        if (node instanceof byte[]) {
            return NodeType.BINARY;
        }

        throw new IllegalArgumentException("Unrecognized node type='" + node.getClass() + ", value=" + node + "'.");
    }

    public enum Event {
        BEGIN_MAP,
        END_MAP,
        BEGIN_SEQUENCE,
        END_SEQUENCE,
        SCALAR;
    }

    // --- ... ---

    @FunctionalInterface
    public interface EventConsumer {
        /**
         * 
         * @param <T>
         * @param event
         * @param cursor
         * @return
         * @throws UncheckedIOException if an I/O error occurs during serialization.
         */
        <T extends TreeCursor> boolean accept(Event event, T cursor);
    }

    @FunctionalInterface
    public interface ScalarEquality {
        boolean test(TreeCursor cursor1, TreeCursor cursor2);
    }

    /**
     * Defines the structural role of the token or value being emitted. Ordinarily
     * originates from the structural state of the source tree during traversal,
     * allowing a stack-free generator to map the incoming node accurately.
     */
    public enum NodeContext {
        /**
         * Indicates the node is the top-level root of the tree structure.
         */
        ROOT,

        /**
         * Indicates the node is an element within an ordered sequence or array.
         */
        ELEMENT,

        LAST_ELEMENT,

        /**
         * Indicates the node functions as a key within a map or object structure.
         */
        ENTRY_KEY,

        /**
         * Indicates the node functions as a value associated with a key within a map or
         * object structure.
         */
        ENTRY_VALUE,

        LAST_ENTRY_VALUE
    }

    public enum NodeType {

        /**
         * Mapping structure of key-value pairs, such as a JSON object or
         * dictionary-like node. Each key is typically a string associated with a nested
         * node.
         */
        MAP(false),

        /**
         * Ordered sequence of elements, such as a JSON array or list. Elements may be
         * scalar or structural nodes.
         */
        SEQUENCE(false),

        /**
         * Textual scalar value. Represents a string node within the tree.
         */
        STRING(true),

        /**
         * Numeric scalar value. Represents an integer or decimal number node.
         */
        NUMBER(true),

        /**
         * Binary scalar value, typically a byte sequence or encoded binary content.
         */
        BINARY(true),

        /**
         * Boolean literal {@code true}.
         */
        TRUE(true),

        /**
         * Boolean literal {@code false}.
         */
        FALSE(true),

        /**
         * Null literal value.
         */
        NULL(true);

        private final boolean scalar;

        NodeType(boolean scalar) {
            this.scalar = scalar;
        }

        /**
         * Returns whether this node type represents a scalar value.
         *
         * @return {@code true} if the node is scalar; {@code false} otherwise
         */
        public boolean isScalar() {
            return scalar;
        }

        /**
         * Returns whether this node type represents a structural container.
         *
         * @return {@code true} if the node is structural (non-scalar)
         */
        public boolean isStructure() {
            return !scalar;
        }
    }

    public static final record Features(
            Set<NodeType> keys,
            Set<NodeType> nodes) {

        public Features {
            keys = keys == null ? Set.of() : Set.copyOf(keys);
            nodes = nodes == null ? Set.of() : Set.copyOf(nodes);
        }

        /**
         * Returns the set of scalar types that are supported as keys in map nodes. For
         * example, a JSON-based adapter would return only {@link NodeType#STRING},
         * whereas a CBOR-based adapter might return multiple scalar types.
         *
         * @return an immutable set of supported key {@link NodeType}s.
         */
        @Override
        public Set<NodeType> keys() {
            return keys;
        }

        /**
         * Returns the complete set of node types that this adapter is capable of
         * representing.
         *
         * @return an immutable set of supported {@link NodeType}s.
         */
        @Override
        public Set<NodeType> nodes() {
            return nodes;
        }
    }
}
