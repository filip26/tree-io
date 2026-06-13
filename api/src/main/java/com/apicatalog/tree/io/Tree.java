package com.apicatalog.tree.io;

import java.util.ArrayDeque;
import java.util.Set;

import com.apicatalog.tree.io.TreeGenerator.Context;
import com.apicatalog.tree.io.java.NativeTreeGenerator;
import com.apicatalog.tree.io.java.NativeTreeTraversal;

/**
 * Immutable representation of a tree node where the node and its descendants
 * are accessed through a {@link TreeAdapter}.
 * <p>
 * A {@link Tree} instance binds a node with its adapter, providing a uniform
 * way to traverse or compare trees of arbitrary underlying object models.
 * </p>
 * <p>
 * Pass a {@link Tree} from JSON, YAML, or CBOR into the tree to create
 * polyformic tree composed of various different serializations, libraries, in
 * order to uniformly prosses such a tree.
 * </p>
 *
 */
public class Tree {

    public static Object read(TreeParser parser) throws TreeIOException {
        var generator = new NativeTreeGenerator();
        translate(parser, generator);
        return generator.get();
    }

    public static void write(Object node, TreeGenerator generator) throws TreeIOException {
        var traversal = new NativeTreeTraversal();
        traversal.node(node);
        translate(traversal, generator);
    }

    /**
     * A high-level utility method that fully traverses the tree and drives the
     * provided {@link TreeGenerator}. This is the primary method for tree
     * transformation, serialization, or deep cloning. It iterates through every
     * node using {@link #next()} and emits a corresponding event to the generator.
     *
     * @param node
     * @param generator the generator that will receive construction events.
     * @throws TreeIOException       if the generator encounters an I/O error.
     * @throws IllegalStateException if the source tree is malformed (e.g., unclosed
     *                               structures).
     */
    public static void translate(TreeTraversal traversal, TreeGenerator generator) throws TreeIOException {

        while (true) {
            switch (traversal.next()) {
            case BEGIN_MAP:
                generator.beginMap(traversal.context());
                continue;

            case END_MAP:
                generator.endMap(traversal.context());
                continue;

            case BEGIN_SEQUENCE:
                generator.beginSequence(traversal.context());
                continue;

            case END_SEQUENCE:
                generator.endSequence(traversal.context());
                continue;

            case SCALAR:
                break;

            case END:
//                if (traversal.dept depth > 0) {
//                    throw new IllegalStateException("The traversed tree is malformed. A map or a collection was not properly closed.");
//                }
                return;
            }

            switch (traversal.node()) {
            case null -> generator.nullValue(traversal.context());
            case Boolean bool -> generator.booleanValue(traversal.context(), bool);
            case String string -> generator.stringValue(traversal.context(), string);
            case Number number -> generator.numericValue(traversal.context(), number);
            case byte[] bytes -> generator.binaryValue(traversal.context(), bytes);

            default -> throw new IllegalArgumentException(
                    """
                    Unexpected node type=%s, value=%s"
                    """.formatted(traversal.node().getClass(), traversal.node()));
            }
        }
    }

    public static void translate(TreeParser parser, TreeGenerator generator) throws TreeIOException {

        var stack = new ArrayDeque<Context>();
        stack.push(Context.ROOT);

        var token = parser.nextToken();

        while (token != null) {

            switch (token) {
            case BEGIN_MAP:
                generator.beginMap(stack.peek());
                stack.push(Context.ENTRY_KEY);
                break;

            case END_MAP:
                if (stack.pop() != Context.ENTRY_VALUE) {
                    throw new IllegalStateException();
                }
                generator.endMap(stack.peek());
                if (stack.peek() == Context.ENTRY_KEY) {
                    stack.pop();
                    stack.push(Context.ENTRY_VALUE);
                }
                break;

            case BEGIN_SEQUENCE:
                generator.beginSequence(stack.peek());
                stack.push(Context.ELEMENT);
                break;

            case END_SEQUENCE:
                if (stack.pop() != Context.ELEMENT) {
                    throw new IllegalStateException();
                }
                generator.endSequence(stack.peek());
                if (stack.peek() == Context.ENTRY_KEY) {
                    stack.pop();
                    stack.push(Context.ENTRY_VALUE);
                }
                break;

            case NULL:
                generator.nullValue(stack.peek());
                break;

            case TRUE:
                generator.booleanValue(stack.peek(), true);
                break;

            case FALSE:
                generator.booleanValue(stack.peek(), false);
                break;

            case NUMBER:
                generator.numericValue(stack.peek(), parser.getNumber());
                if (stack.peek() == Context.ENTRY_KEY) {
                    stack.pop();
                    stack.push(Context.ENTRY_VALUE);
                }
                break;

            case STRING:
                generator.stringValue(stack.peek(), parser.getString());
                if (stack.peek() == Context.ENTRY_KEY) {
                    stack.pop();
                    stack.push(Context.ENTRY_VALUE);
                }
                break;

            case BINARY:
                generator.binaryValue(stack.peek(), parser.getBinary());
                if (stack.peek() == Context.ENTRY_KEY) {
                    stack.pop();
                    stack.push(Context.ENTRY_VALUE);
                }
                break;
            }
            ;

            token = parser.nextToken();
        }

        if (stack.peek() != Context.ROOT) {
            throw new IllegalStateException();
        }
    }

    /**
     * Enumeration of supported node types within a {@code PolyMorph} tree
     * structure.
     * <p>
     * A {@code NodeType} describes the semantic kind of a node, distinguishing
     * between scalar values (e.g. string, number, boolean) and structural
     * containers (e.g. map, collection, or polymorphic wrapper).
     * </p>
     * <p>
     * {@link #TREE} represents an ad-hoc, heterogeneous wrapper node that can
     * encapsulate another node originating from a different data model or library.
     * This enables uniform traversal and comparison of mixed-format trees.
     * </p>
     *
     * @see TreeAdapter
     * @see com.apicatalog.tree.io.Tree
     */
    @Deprecated
    public enum NodeType {

        /**
         * Polymorphic wrapper node enabling heterogeneous access across formats.
         * <p>
         * An adapted node acts as an adapter-level bridge between different underlying
         * object models, allowing a mixed tree to be processed uniformly.
         * </p>
         */
        TREE(false),

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
            return !scalar && this != TREE;
        }
    }

    public enum Capability {
        DEEP_OBJECT_EQUALS,
        SCALAR_OBJECT_EQUALS,
    }

    public static final record Features(
            Set<NodeType> keys,
            Set<NodeType> nodes,
            Set<Capability> capabilities) {

        public Features {
            keys = keys == null ? Set.of() : Set.copyOf(keys);
            nodes = nodes == null ? Set.of() : Set.copyOf(nodes);
            capabilities = capabilities == null ? Set.of() : Set.copyOf(capabilities);
        }

        /**
         * Returns the set of scalar types that are supported as keys in map nodes. For
         * example, a JSON-based adapter would return only {@link NodeType#STRING},
         * whereas a CBOR-based adapter might return multiple scalar types.
         *
         * @return an immutable set of supported key {@link NodeType}s.
         */
        public Set<NodeType> keys() {
            return keys;
        }

        /**
         * Returns the complete set of node types that this adapter is capable of
         * representing.
         *
         * @return an immutable set of supported {@link NodeType}s.
         */
        public Set<NodeType> nodes() {
            return nodes;
        }
    }
}
