package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.apicatalog.tree.io.java.JavaTreeGenerator;
import com.apicatalog.tree.io.java.JavaTreeTraversal;

public final class Tree {

    public static Object read(TreeParser parser) throws TreeIOException {
        var generator = new JavaTreeGenerator();
        translate(parser, generator);
        return generator.get();
    }

    public static void write(Object node, TreeGenerator generator) throws TreeIOException {
        var traversal = new JavaTreeTraversal();
        traversal.node(node);
        translate(traversal, generator);
    }

    /**
     * A high-level utility method that fully traverses the tree and drives the
     * provided {@link TreeGenerator}. This is the primary method for tree
     * transformation, serialization, or deep cloning. It iterates through every
     * node using {@link TreeParser#next()} and emits a corresponding event to the generator.
     *
     * @param parser
     * @param generator the generator that will receive construction events.
     * @throws TreeIOException       if the generator encounters an I/O error.
     * @throws IllegalStateException if the source tree is malformed (e.g., unclosed
     *                               structures).
     */
    public static void translate(TreeParser parser, TreeGenerator generator) throws TreeIOException {
        while (true) {
            switch (parser.next()) {
            case BEGIN_MAP:
                generator.beginMap(parser.context());
                continue;

            case END_MAP:
                generator.endMap(parser.context());
                continue;

            case BEGIN_SEQUENCE:
                generator.beginSequence(parser.context());
                continue;

            case END_SEQUENCE:
                generator.endSequence(parser.context());
                continue;

            case SCALAR:
                switch (parser.nodeType()) {
                case NULL -> generator.nullValue(parser.context());
                case TRUE -> generator.booleanValue(parser.context(), true);
                case FALSE -> generator.booleanValue(parser.context(), false);
                case STRING -> generator.stringValue(parser.context(), parser.stringValue());
                case NUMBER -> generator.numberValue(parser.context(), parser.numberValue());
                case BINARY -> generator.binaryValue(parser.context(), parser.binaryValue());

                default -> throw new IllegalArgumentException(
                        """
                        Unexpected node type=%s"
                        """.formatted(parser.nodeType()));
                }
                continue;

            case null:
//                if (traversal.dept depth > 0) {
//                    throw new IllegalStateException("The traversed tree is malformed. A map or a collection was not properly closed.");
//                }
                return;
            }
        }

//        if (stack.peek() != NodeContext.ROOT) {
//            throw new IllegalStateException();
//        }
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

        /**
         * Indicates the node functions as a key within a map or object structure.
         */
        ENTRY_KEY,

        /**
         * Indicates the node functions as a value associated with a key within a map or
         * object structure.
         */
        ENTRY_VALUE
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
