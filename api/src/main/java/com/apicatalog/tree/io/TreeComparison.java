package com.apicatalog.tree.io;

import java.util.Arrays;
import java.util.Comparator;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.function.Function;

import com.apicatalog.tree.io.Tree.NodeType;

public class TreeComparison {

    //TODO use stack instead of recursion

    // TODO rename to just isomorphic?
    public static final boolean deepEquals(Tree left, Tree right) {
        if (left == null) {
            return right == null;

        } else if (right == null) {
            return false;
        }
        return deepEquals(left.node(), left.adapter(), right.node(), right.adapter());
    }

    public static final boolean deepEquals(
            final Object left,
            final TreeAdapter leftAdapter,
            final Object right,
            final TreeAdapter rightAdapter) {

        if (left instanceof Tree(Object node, TreeAdapter adapter)) {
            return deepEquals(node, adapter, right, rightAdapter);
        }

        if (right instanceof Tree(Object node, TreeAdapter adapter)) {
            return deepEquals(left, leftAdapter, node, adapter);
        }

        if (leftAdapter.isNull(left)) {
            return rightAdapter.isNull(right);

        } else if (rightAdapter.isNull(right)) {
            return false;
        }

        final NodeType leftType = leftAdapter.type(left);
        final NodeType rightType = rightAdapter.type(right);

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
            final var leftEntries = leftAdapter.entryStream(left)
                    .sorted(TreeComparison.comparingEntry(e -> leftAdapter.asString(e.getKey())))
                    .iterator();

            final var rightEntries = rightAdapter.entryStream(right)
                    .sorted(TreeComparison.comparingEntry(e -> rightAdapter.asString(e.getKey())))
                    .iterator();

            while (leftEntries.hasNext() && rightEntries.hasNext()) {

                final var leftEntry = leftEntries.next();
                final var rightEntry = rightEntries.next();

                if (!deepEquals( // compare keys
                        leftEntry.getKey(),
                        leftAdapter,
                        rightEntry.getKey(),
                        rightAdapter)
                        ||
                        !deepEquals( // compare values
                                leftEntry.getValue(),
                                leftAdapter,
                                rightEntry.getValue(),
                                rightAdapter)) {
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

        final var leftIterator = left.iterator();
        final var rightIterator = right.iterator();

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
}
