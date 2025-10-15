package com.apicatalog.tree.io;

import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.function.Function;

/**
 * Immutable representation of a tree node accessed through a
 * {@link NodeAdapter}.
 * <p>
 * A {@link PolyNode} instance binds a node with its adapter, providing a
 * uniform way to traverse or compare trees of arbitrary underlying object
 * models.
 * </p>
 * <p>
 * Pass a {@link PolyNode} from JSON, YAML, or CBOR into the tree.
 * </p>
 *
 */
public class PolyNode {

    protected final NodeAdapter adapter;
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
    public PolyNode(Object node, NodeAdapter adapter) {
        this.node = Objects.requireNonNull(node);
        this.adapter = Objects.requireNonNull(adapter);
    }

    public NodeAdapter adapter() {
        return adapter;
    }

    public Object node() {
        return node;
    }

    public static final boolean deepEquals(PolyNode left, PolyNode right) {
        if (left == null) {
            return right == null;

        } else if (right == null) {
            return false;
        }
        return deepEquals(left.node, left.adapter, right.node, right.adapter);
    }

    public static final boolean deepEquals(Object left, NodeAdapter leftAdapter, Object right, NodeAdapter rightAdapter) {

        if (leftAdapter.isNull(left)) {
            return rightAdapter.isNull(right);

        } else if (rightAdapter.isNull(right)) {
            return false;
        }

        NodeType leftType = leftAdapter.type(left);
        NodeType rightType = rightAdapter.type(right);

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
                        leftAdapter.bigIntegerValue(left),
                        rightAdapter.bigIntegerValue(right));
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
                    .sorted(PolyNode.comparingEntry(e -> leftAdapter.asString(e.getKey())))
                    .iterator();

            final Iterator<Entry<?, ?>> rightEntries = rightAdapter.entryStream(right)
                    .sorted(PolyNode.comparingEntry(e -> rightAdapter.asString(e.getKey())))
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

    protected static boolean deepEqualsCollection(Iterable<? extends Object> left, NodeAdapter leftAdapter, Iterable<? extends Object> right, NodeAdapter rightAdapter) {

        Iterator<? extends Object> leftIterator = left.iterator();
        Iterator<? extends Object> rightIterator = right.iterator();

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

    public static Comparator<Entry<?, ?>> comparingStringKeys(NodeAdapter adapter) {
        return comparingEntry(e -> adapter.asString(e.getKey()));
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    public static Comparator<Object> comparingElement(Function<Object, Comparable> keyExtractor) {
        return (Object arg0, Object arg1) -> keyExtractor.apply(arg0).compareTo(keyExtractor.apply(arg1));
    }

}
