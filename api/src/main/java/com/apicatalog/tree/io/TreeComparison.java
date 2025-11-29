package com.apicatalog.tree.io;

import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Deque;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.function.Function;

import com.apicatalog.tree.io.Tree.NodeType;

public interface TreeComparison {

    boolean isomorphic();

    public static TreeComparison of(Tree left, Tree right) {
        return of(
                left.node(),
                left.adapter(),
                right.node(),
                right.adapter());
    }

    public static TreeComparison of(
            Object left,
            TreeAdapter leftAdapter,
            Object right,
            TreeAdapter rightAdapter) {
        return ComparisonStack.of(left, leftAdapter, right, rightAdapter);
    }

    public static boolean deepEquals(Tree left, Tree right) {
        return of(left, right).isomorphic();
    }

    public static boolean deepEquals(
            Object left,
            TreeAdapter leftAdapter,
            Object right,
            TreeAdapter rightAdapter) {
        return of(left, leftAdapter, right, rightAdapter).isomorphic();
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

    static record ComparsionResult(boolean isomorphic) implements TreeComparison {

        static final TreeComparison TRUE = new ComparsionResult(true);
        static final TreeComparison FALSE = new ComparsionResult(false);
    }

    static class ComparisonStack implements TreeComparison {

        static record Structure(
                NodeType type,
                Object left,
                TreeAdapter leftAdapter,
                Object right,
                TreeAdapter rightAdapter) {
        };

        private Deque<Structure> stack;
        private Boolean result;

        protected ComparisonStack(Deque<Structure> stack) {
            this.stack = stack;
            this.result = null;
        }

        public static TreeComparison of(
                Object left,
                TreeAdapter leftAdapter,
                Object right,
                TreeAdapter rightAdapter) {

            if (left == null) {
                return right == null ? ComparsionResult.TRUE : ComparsionResult.FALSE;

            } else if (right == null) {
                return ComparsionResult.FALSE;
            }

            var result = nodeEquals(left, leftAdapter, right, rightAdapter);

            if (result instanceof Structure structure) {
                var stack = new ArrayDeque<Structure>();
                stack.push(structure);
                return new ComparisonStack(stack);

            } else if (result instanceof Boolean status) {
                return status
                        ? ComparsionResult.TRUE
                        : ComparsionResult.FALSE;
            }

            throw new IllegalStateException();
        }

        @Override
        public boolean isomorphic() {
            if (result != null) {
                return result;
            }
            // TODO max depth
            while (!stack.isEmpty()) {

                var structure = stack.peek();

                if (structure.type == NodeType.COLLECTION) {
                    result = arrayEquals();

                } else if (structure.type == NodeType.MAP) {
                    result = mapEquals();

                } else {
                    throw new IllegalStateException();
                }

                if (result != null) {
                    if (!result) {
                        return false;
                    }
                    stack.pop();
                }
            }

            if (result == null) {
                throw new IllegalStateException();
            }

            return result;
        }

        final Boolean mapEquals() {

            var struct = (Structure) stack.peek();
            @SuppressWarnings("unchecked")
            var entries = (Iterator<Map.Entry<?, ?>>) struct.left;

            while (entries.hasNext()) {

                var entry = entries.next();

                var rightValue = struct.rightAdapter.property(
                        entry.getKey(),
                        struct.leftAdapter,
                        struct.right);

                var result = nodeEquals(
                        entry.getValue(),
                        struct.leftAdapter,
                        rightValue,
                        struct.rightAdapter);

                if (result instanceof Structure item) {
                    stack.push(item);
                    return null;
                }

                if (!(Boolean) result) {
                    return false;
                }
            }
            return true;
        }

        final Boolean arrayEquals() {

            var struct = stack.peek();

            var leftIterator = (Iterator<?>) struct.left;
            var rightIterator = (Iterator<?>) struct.right;

            while (leftIterator.hasNext() && rightIterator.hasNext()) {

                var leftElement = leftIterator.next();
                var rightElement = rightIterator.next();

                var result = nodeEquals(
                        leftElement,
                        struct.leftAdapter,
                        rightElement,
                        struct.rightAdapter);

                if (result instanceof Structure item) {
                    stack.push(item);
                    return null;
                }

                if (!(Boolean) result) {
                    return false;
                }
            }

            // Check if both iterators are exhausted
            return !leftIterator.hasNext() && !rightIterator.hasNext();
        }

        static final Object nodeEquals(
                final Object left,
                final TreeAdapter leftAdapter,
                final Object right,
                final TreeAdapter rightAdapter) {

            if (left instanceof Tree(Object node, TreeAdapter adapter)) {
                return nodeEquals(node, adapter, right, rightAdapter);
            }

            if (right instanceof Tree(Object node, TreeAdapter adapter)) {
                return nodeEquals(left, leftAdapter, node, adapter);
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
            case FALSE:
            case TRUE:
            case NULL:
                return true;

            case BINARY:
                return Arrays.equals(leftAdapter.binaryValue(left), rightAdapter.binaryValue(right));

            case STRING:
                return Objects.equals(leftAdapter.stringValue(left), rightAdapter.stringValue(right));

            case NUMBER:
                if (leftAdapter.isIntegral(left)) {
                    return rightAdapter.isIntegral(right)
                            && Objects.equals(
                                    leftAdapter.integerValue(left),
                                    rightAdapter.integerValue(right));
                }
                return !rightAdapter.isIntegral(right)
                        && Objects.equals(
                                leftAdapter.decimalValue(left),
                                rightAdapter.decimalValue(right));

            case COLLECTION:
                if (leftAdapter.isEmptyCollection(left)) {
                    return rightAdapter.isEmptyCollection(right);

                } else if (rightAdapter.isEmptyCollection(right)) {
                    return false;
                }

                return new Structure(
                        NodeType.COLLECTION,
                        leftAdapter.elements(left).iterator(),
                        leftAdapter,
                        rightAdapter.elements(right).iterator(),
                        rightAdapter);

            case MAP:
                var leftKeys = leftAdapter.keys(left);
                var rightKeys = rightAdapter.keys(right);

                if (leftKeys.size() != rightKeys.size()) {
                    return false;
                }

                return new Structure(
                        NodeType.MAP,
                        leftAdapter.entries(left).iterator(),
                        leftAdapter,
                        right,
                        rightAdapter);

            default:
                return false;
            }
        }
    }
}
