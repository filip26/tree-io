package com.apicatalog.tree.io;

import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;
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
public class TreeIO {

    protected final TreeAdapter adapter;
    protected final Object node;

    /**
     * Creates a new immutable tree with the given root node and adapter.
     *
     * @param node    the root node of the tree, must not be {@code null}
     * @param adapter the adapter providing access to node types and values, must
     *                not be {@code null}
     * @throws NullPointerException if {@code root} or {@code adapter} is
     *                              {@code null}
     */
    public TreeIO(Object node, TreeAdapter adapter) {
        this.node = Objects.requireNonNull(node);
        this.adapter = Objects.requireNonNull(adapter);
    }

    public TreeAdapter adapter() {
        return adapter;
    }

    /**
     * Root node, can be scalar or a structure like Map or Collection.
     * 
     * @return
     */
    public Object node() {
        return node;
    }

    public void traverse(Consumer<TreeTraversal> visitor) {
        (new TreeTraversal()).root(node, adapter).traverse(visitor);
    }

    public void traverse(TreeGenerator generator) throws TreeIOException {
        (new TreeTraversal()).root(node, adapter).traverse(generator);
    }

    public static final boolean deepEquals(TreeIO left, TreeIO right) {
        if (left == null) {
            return right == null;

        } else if (right == null) {
            return false;
        }
        return deepEquals(left.node, left.adapter, right.node, right.adapter);
    }

    public static final boolean deepEquals(
            final Object left,
            final TreeAdapter leftAdapter,
            final Object right,
            final TreeAdapter rightAdapter) {

        if (leftAdapter.isNull(left)) {
            return rightAdapter.isNull(right);

        } else if (rightAdapter.isNull(right)) {
            return false;
        }

        final NodeType leftType = leftAdapter.type(left);
        final NodeType rightType = rightAdapter.type(right);

//TODO polynode!!
//        if (leftType == NodeType.TREE_IO) {
//            return deepEquals((((TreeIO)leftType), null)
//        }
        
        if (leftType != rightType) {
            return false;
        }

        switch (leftType) {
        case BINARY:
            return Arrays.equals(
                    leftAdapter.binaryValue(left),
                    rightAdapter.binaryValue(right));

        case STRING:
            return Objects.equals(
                    leftAdapter.stringValue(left),
                    rightAdapter.stringValue(right));

        case NUMBER:
            if (leftAdapter.isIntegral(left)) {
                return rightAdapter.isIntegral(right) && Objects.equals(
                        leftAdapter.integerValue(left),
                        rightAdapter.integerValue(right));
            }
            return !rightAdapter.isIntegral(right) && Objects.equals(
                    leftAdapter.decimalValue(left),
                    rightAdapter.decimalValue(right));

        case COLLECTION:
            return deepEqualsCollection(
                    leftAdapter.elements(left),
                    leftAdapter,
                    rightAdapter.elements(right),
                    rightAdapter);

        case MAP:
            final Iterator<Entry<?, ?>> leftEntries = leftAdapter.entryStream(left)
                    .sorted(TreeIO.comparingEntry(e -> leftAdapter.asString(e.getKey())))
                    .iterator();

            final Iterator<Entry<?, ?>> rightEntries = rightAdapter.entryStream(right)
                    .sorted(TreeIO.comparingEntry(e -> rightAdapter.asString(e.getKey())))
                    .iterator();

            while (leftEntries.hasNext() && rightEntries.hasNext()) {

                final Entry<?, ?> leftEntry = leftEntries.next();
                final Entry<?, ?> rightEntry = rightEntries.next();

                if (!deepEquals(leftEntry.getKey(), leftAdapter, rightEntry.getKey(), rightAdapter)
                        || !deepEquals(leftEntry.getValue(), leftAdapter, rightEntry.getValue(), rightAdapter)) {
                    return false;
                }
            }

            return !leftEntries.hasNext() && !rightEntries.hasNext();

        case FALSE:
        case TRUE:
        case NULL:
            return true;

        default:
            return false;
        }
    }

    protected static boolean deepEqualsCollection(
            final Iterable<? extends Object> left, 
            final TreeAdapter leftAdapter, 
            final Iterable<? extends Object> right, 
            final TreeAdapter rightAdapter) {

        final Iterator<? extends Object> leftIterator = left.iterator();
        final Iterator<? extends Object> rightIterator = right.iterator();

        while (leftIterator.hasNext() && rightIterator.hasNext()) {
            if (!deepEquals(
                    leftIterator.next(),
                    leftAdapter,
                    rightIterator.next(),
                    rightAdapter)) {
                return false;
            }
        }

        // Check if both iterators are exhausted
        return !leftIterator.hasNext() && !rightIterator.hasNext();
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    public static Comparator<Entry<?, ?>> comparingEntry(Function<Entry<?, ?>, Comparable> keyExtractor) {
        return (Entry<?, ?> arg0, Entry<?, ?> arg1) -> keyExtractor.apply(arg0).compareTo(keyExtractor.apply(arg1));
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    public static Comparator<?> comparingNode(Function<Object, Comparable> keyExtractor) {
        return (Object arg0, Object arg1) -> keyExtractor.apply(arg0).compareTo(keyExtractor.apply(arg1));
    }

    public static Comparator<Entry<?, ?>> comparingStringKeys(TreeAdapter adapter) {
        return comparingEntry(e -> adapter.asString(e.getKey()));
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    public static Comparator<Object> comparingElement(Function<Object, Comparable> keyExtractor) {
        return (Object arg0, Object arg1) -> keyExtractor.apply(arg0).compareTo(keyExtractor.apply(arg1));
    }

    public boolean isEmptyOrNull() {
        return isEmptyOrNull(this);
    }

    public static final boolean isEmptyOrNull(TreeIO node) {
        return node == null
                || node.node == null
                || node.adapter.isNull(node.node)
                || node.adapter.isEmpty(node.node);
    }

    public boolean isMap() {
        return isMap(this);
    }

    public static final boolean isMap(TreeIO node) {
        return node != null && node.adapter().isMap(node.node);
    }

    public Object property(String key) {
        return property(key, this);
    }

    public static final Object property(Object key, TreeIO node) {
        return node != null
                ? node.adapter().property(key, node.node)
                : null;
    }

    public boolean isCollection() {
        return isCollection(this);
    }

    public static final boolean isCollection(TreeIO node) {
        return node != null && node.adapter().isCollection(node.node);
    }

    public NodeType type() {
        return type(this);
    }

    public static final NodeType type(TreeIO node) {
        return node.adapter().type(node.node);
    }

    public boolean isSingleElement() {
        return isSingleElement(this);
    }

    public boolean isSingleElement(TreeIO node) {
        return node.adapter().isSingleElement(node.node);
    }

    public Object singleElement() {
        return adapter.singleElement(node);
    }

    public Collection<?> keys() {
        return adapter.keys(node);
    }

    public boolean isSingleEntry() {
        return isSingleEntry(this);
    }

    public boolean isSingleEntry(TreeIO node) {
        return node.adapter().isSingleEntry(node.node);
    }

    public Entry<?, ?> singleEntry() {
        return adapter.singleEntry(node);
    }

    public Stream<?> keyStream() {
        return adapter.keyStream(node);
    }

}
