package com.apicatalog.tree.io;

import java.util.Collection;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.stream.Stream;

/**
 * Immutable representation of a tree node where the node and its descendants
 * are accessed through a {@link TreeAdapter}.
 * <p>
 * A {@link TreeIO} instance binds a node with its adapter, providing a uniform
 * way to traverse or compare trees of arbitrary underlying object models.
 * </p>
 * <p>
 * Pass a {@link TreeIO} from JSON, YAML, or CBOR into the tree to create
 * polyformic tree composed of various different serializations, libraries, in
 * order to uniformly prosses such a tree.
 * </p>
 *
 */
//TODO rename to plain Tree
public record TreeIO(
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
    public TreeIO {
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

    // TODO
//    boolean isIsomorphic(Tree other);      // exact structural/value match
//    boolean isSubtreeOf(Tree other);     // this ⊆ other
//    boolean isSupertreeOf(Tree other);   // this ⊇ other

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
}
