package com.apicatalog.tree.io;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Objects;

/**
 * Immutable representation of a tree structure accessed through a
 * {@link NodeAdapter}.
 * <p>
 * A {@code TreeModel} instance binds a root node with its adapter, providing a
 * uniform way to traverse or compare trees of arbitrary underlying object
 * models.
 * </p>
 *
 * @param <T> the type of the root node
 */
public class TreeModel {

    protected final NodeAdapter adapter;
    protected final Object root;

    /**
     * Creates a new immutable tree with the given root node and adapter.
     *
     * @param root    the root node of the tree, must not be {@code null}
     * @param adapter the adapter providing access to node types and values, must
     *                not be {@code null}
     * @throws NullPointerException if {@code root} or {@code adapter} is
     *                              {@code null}
     */
    public TreeModel(Object root, NodeAdapter adapter) {
        Objects.requireNonNull(root);
        Objects.requireNonNull(adapter);

        this.root = root;
        this.adapter = adapter;
    }

    public NodeAdapter adapter() {
        return adapter;
    }

    public Object root() {
        return root;
    }

    static final boolean deepEquals(Object left, NodeAdapter leftAdapter, Object right, NodeAdapter rightAdapter) {

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
            final int leftSize = leftAdapter.size(left);
            final int rightSize = rightAdapter.size(right);

            if (leftSize != rightSize) {
                return false;
            }

            return deepEqualsCollection(
                    leftAdapter.iterable(left),
                    leftAdapter,
                    rightAdapter.iterable(right),
                    rightAdapter);

        case MAP:
            final Collection<? extends Object> leftProps = leftAdapter.keys(left);
            final Collection<? extends Object> rightProps = rightAdapter.keys(right);

            if (leftProps.size() != rightProps.size()) {
                return false;
            }

            // FIXME ----
            // deep compare property names / keys
            if (!deepEqualsCollection(leftProps, leftAdapter, rightProps, rightAdapter)) {
                return false;
            }

            for (final Object property : leftProps) {
                if (!deepEquals(
                        leftAdapter.property(property, rightType),
                        leftAdapter,
                        rightProps,
                        rightAdapter)) {
                    return false;
                }
            }

            return true;

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

}
