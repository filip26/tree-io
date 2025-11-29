package com.apicatalog.tree.io;

import java.util.Collection;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Stream;

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
public record Tree(
        Object node,
        TreeAdapter adapter) {

    /**
     * Creates a new immutable tree with the given root node and adapter.
     *
     * @param node    the root node of the tree, must not be {@code null}
     * @param adapter the adapter providing access to node types and values, must
     *                not be {@code null}
     * @throws NullPointerException if {@code root} or {@code adapter} is
     *                              {@code null}
     */
    public Tree {
        node = Objects.requireNonNull(node);
        adapter = Objects.requireNonNull(adapter);
    }

    public Features features() {
        return adapter.features();
    }

    public TreeAdapter adapter() {
        return adapter;
    }

    /**
     * Root node, a structure like Map or Collection.
     * 
     * @return
     */
    public Object node() {
        return node;
    }

    public void traverse(Consumer<TreeTraversal> visitor) {
        (new TreeTraversal()).root(node, adapter).traverse(visitor);
    }

    public void generate(TreeGenerator generator) throws TreeIOException {
        (new TreeTraversal()).root(node, adapter).traverse(generator);
    }

    // exact structural/value match
    public boolean isIsomorphicTo(Tree other) {
        return isIsomorphicTo(other.node, other.adapter);
    }

    public boolean isIsomorphicTo(Tree other, int maxDepth) {
        return isIsomorphicTo(other.node, other.adapter, maxDepth);
    }

    public boolean isIsomorphicTo(Object other, TreeAdapter otherAdapter) {
        return TreeComparison.deepEquals(node, adapter, other, otherAdapter);
    }

    public boolean isIsomorphicTo(Object other, TreeAdapter otherAdapter, int maxDepth) {
        return TreeComparison.deepEquals(node, adapter, other, otherAdapter, maxDepth);
    }

    public boolean isEmptyOrNull() {
        return node == null
                || adapter.isNull(node)
                || adapter.isEmpty(node);
    }

    public boolean isCollection() {
        return node != null && adapter.isCollection(node);
    }

    /**
     * Determines the {@link NodeType} of the given native node.
     *
     * @return the {@link NodeType} corresponding to the node.
     */
    public NodeType type() {
        return adapter.type(node);
    }

    /**
     * Checks if a map or collection node is empty.
     *
     * @return {@code true} if the node contains no entries or elements,
     *         {@code false} otherwise.
     * @throws UnsupportedOperationException if the node is not a map or collection.
     */
    public boolean isEmpty() {
        return node != null && adapter.isEmpty(node);
    }

    // --- Map Operations ---

    /**
     * Checks if the adapted node represents a map (an object with key-value pairs).
     *
     * @return {@code true} if the node represents a map, {@code false} otherwise.
     */
    public boolean isMap() {
        return node != null && adapter.isMap(node);
    }

    /**
     * Returns a collection of the native key objects from a map node. The types of
     * the keys depend on the underlying format (e.g., Strings for JSON, various
     * scalars for CBOR).
     *
     * @return a collection containing the map's native key objects.
     */
    public Collection<?> keys() {
        return adapter.keys(node);
    }

    /**
     * Retrieves the value associated with a given native key object from a map
     * node.
     *
     * @param key the native key object used for the lookup.
     * @return the native value node, or {@code null} if the key is not found.
     */
    public Object property(Object key) {
        return adapter.property(key, node);
    }

    public Object property(Object key, TreeAdapter keyAdapter) {
        return adapter.property(key, keyAdapter, node);
    }

    /**
     * Returns all key-value pairs of a map node as an {@link Iterable}. The entries
     * contain the native key and value objects.
     *
     * @return an {@link Iterable} of its entries.
     * @throws UnsupportedOperationException if the node is not a map.
     */
    public Iterable<Entry<?, ?>> entries() {
        return adapter.entries(node);
    }

    /**
     * Returns all key-value pairs of a map node as a {@link Stream}. This is a
     * convenience method for processing map entries using the Stream API.
     *
     * @return a {@link Stream} of its entries.
     */
    public Stream<Entry<?, ?>> entryStream() {
        return adapter.entryStream(node);
    }

    public boolean isSingleEntry() {
        return adapter.isSingleEntry(node);
    }

    public Entry<?, ?> singleEntry() {
        return adapter.singleEntry(node);
    }

    public Stream<?> keyStream() {
        return adapter.keyStream(node);
    }

    // --- Collection Operations ---

    /**
     * Checks if the adapted node represents a sequence of elements
     *
     * @return {@code true} if the node represents a sequence, {@code false}
     *         otherwise.
     */
    public boolean isSequence() {
        return node != null && adapter.isCollection(node);
    }

    /**
     * Returns the elements of a collection node as an {@link Iterable}.
     *
     * @return an {@link Iterable} of its native element objects.
     */
    public Iterable<?> elements() {
        return adapter.elements(node);
    }

    /**
     * Returns the elements of a collection node as a {@link Stream}.
     *
     * @return a {@link Stream} of its native element objects.
     */
    public Stream<?> elementStream() {
        return adapter.elementStream(node);
    }

    public boolean isSingleElement() {
        return adapter.isSingleElement(node);
    }

    public Object singleElement() {
        return adapter.singleElement(node);
    }

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
     * @return a non-null {@link Iterable} representing the node's contents.
     */
    public Iterable<?> asIterable() {
        return adapter.asIterable(node);
    }

    /**
     * Returns the node's contents as a universal {@link Stream}.
     *
     * @return a non-null {@link Stream} representing the node's contents.
     */
    public Stream<?> asStream() {
        return adapter.asStream(node);
    }

    public boolean isEmptyMap() {
        return node != null && adapter.isEmptyMap(node);
    }

    public boolean isEmptyCollection() {
        return node != null && adapter.isEmptyCollection(node);
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
     * {@link #TREE_IO} represents an ad-hoc, heterogeneous wrapper node that can
     * encapsulate another node originating from a different data model or library.
     * This enables uniform traversal and comparison of mixed-format trees.
     * </p>
     *
     * @see TreeAdapter
     * @see com.apicatalog.tree.io.Tree
     */
    public enum NodeType {

        /**
         * Polymorphic wrapper node enabling heterogeneous access across formats.
         * <p>
         * An adapted node acts as an adapter-level bridge between different underlying
         * object models, allowing a mixed tree to be processed uniformly.
         * </p>
         */
        TREE_IO(false),

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
        COLLECTION(false),

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
            return !scalar && this != TREE_IO;
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

//        public boolean contains(Features features) {
//            return keys.containsAll(features.keys)
//                    && nodes.containsAll(features.nodes);
//        }

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
