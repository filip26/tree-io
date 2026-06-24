package com.apicatalog.tree.io.java;

import java.io.IOException;
import java.lang.reflect.Array;
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Comparator;
import java.util.Deque;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;

import com.apicatalog.tree.io.Tree.Event;
import com.apicatalog.tree.io.Tree.EventConsumer;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeContext;
import com.apicatalog.tree.io.Tree.NodeType;
import com.apicatalog.tree.io.TreeProcessor;
import com.apicatalog.tree.io.TreeTraverser;

/**
 * Provides a stateful, non-recursive, depth-first iterator for arbitrary
 * tree-like structures. This class decouples the traversal algorithm from the
 * tree.
 */
public final class NativeTraverser implements TreeTraverser<Object>, TreeProcessor, Iterator<Event> {

    /** A sentinel value indicating that traversal depth is not limited. */
    public static final int UNLIMITED_DEPTH = -1;

    /**
     * A sentinel value indicating that the number of visited nodes is not limited.
     */
    public static final int UNLIMITED_NODES = -1;

    private final Deque<Object> stack;

    private Comparator<Entry<?, ?>> entryComparator;

    private int maxVisited;
    private int maxDepth;

    private int depth;
    private int visited;

    private Object currentNode;

    private NodeType currentNodeType;
    private NodeContext currentNodeContext;

    public NativeTraverser(Object node) {
        this(node, new ArrayDeque<>(), null);
    }

    public NativeTraverser(Object node, Comparator<Entry<?, ?>> entryComparator) {
        this(node, new ArrayDeque<>(), entryComparator);
    }

    private NativeTraverser(Object node, Deque<Object> stack, Comparator<Entry<?, ?>> entryComparator) {
        this.stack = stack;
        this.entryComparator = entryComparator;
        this.maxVisited = UNLIMITED_NODES;
        this.maxDepth = UNLIMITED_DEPTH;
        this.visited = 0;
        this.depth = 0;
        this.stack.push(node);
        this.currentNode = node;
        this.currentNodeContext = NodeContext.ROOT;
    }

    @Override
    public Features features() {
        return NativeComposer.FEATURES;
    }

    @Override
    public boolean traverse(EventConsumer consumer) throws IOException {
        while (hasNext()) {
            if (!consumer.accept(next(), this)) {
                return false;
            }
        }
        return true;
    }

    @Override
    public boolean hasNext() {
        return !stack.isEmpty();
    }

    @Override
    public Event next() {
        if (stack.isEmpty()) {
            throw new NoSuchElementException();
        }

        if (maxVisited > 0 && maxVisited < visited) {
            throw new IllegalStateException(
                    """
                    The maximum=%d visited nodes has been reached.
                    """.formatted(maxVisited));
        }
        if (maxDepth > 0 && maxDepth < depth) {
            throw new IllegalStateException(
                    """
                    The maximum traversal depth=%d has been reached.
                    """.formatted(maxDepth));
        }

        var item = stack.peek();

        // map or collection iterator
        if (item instanceof Iterator<?> it) {

            if (!it.hasNext()) {
                stack.pop(); // remove iterator
                currentNode = stack.pop();
                currentNodeType = switch (currentNode) {
                case Collection<?> col -> NodeType.SEQUENCE;
                case Map<?, ?> map -> NodeType.MAP;
                default -> throw new IllegalStateException();
                };
                currentNodeContext = (NodeContext) stack.pop();
                depth -= 1;
                return (Event) stack.pop();
            }

            item = it.next();

            if (item instanceof Map.Entry<?, ?> entry) {
                // process map entry
                currentNodeContext = NodeContext.ENTRY_KEY;

                stack.push(entry);

                // process property key
                currentNode = entry.getKey();

            } else {
                // process collection element
                currentNodeContext = it.hasNext() ? NodeContext.ELEMENT : NodeContext.LAST_ELEMENT;
                currentNode = item;
            }

        } else if (item instanceof Map.Entry<?, ?> entry) {
            // process property value
            currentNode = entry.getValue();
            // restore map iterator over entries
            stack.pop();
            var it = (Iterator<?>) stack.peek();
            currentNodeContext = it.hasNext() ? NodeContext.ENTRY_VALUE : NodeContext.LAST_ENTRY_VALUE;

        } else {
            currentNode = item;
            stack.pop();
        }

        visited++;

        return switch (currentNode) {
        case null -> {
            currentNodeType = NodeType.NULL;
            yield Event.SCALAR;
        }

        case Collection<?> col -> {
            stack.push(Event.END_SEQUENCE);
            stack.push(currentNodeContext);
            stack.push(col);
            stack.push(col.iterator());
            depth += 1;
            currentNodeType = NodeType.SEQUENCE;
            yield Event.BEGIN_SEQUENCE;
        }
        case Map<?, ?> map -> {
            stack.push(Event.END_MAP);
            stack.push(currentNodeContext);
            stack.push(map);

            if (entryComparator != null) {
                stack.push(map.entrySet().stream().sorted(entryComparator).iterator());

            } else {
                stack.push(map.entrySet().iterator());
            }

            depth += 1;
            currentNodeType = NodeType.MAP;
            yield Event.BEGIN_MAP;
        }

        case byte[] bytes -> {
            currentNodeType = NodeType.BINARY;
            yield Event.SCALAR;
        }

        // primitive array, i.e. int[], double[], etc.
        case Object primitives when primitives != null && primitives.getClass().isArray() && primitives.getClass().getComponentType().isPrimitive() -> {
            stack.push(Event.END_SEQUENCE);
            stack.push(currentNodeContext);
            stack.push(primitives);
            stack.push(primitiveArrayIterator(primitives));
            depth += 1;
            currentNodeType = NodeType.SEQUENCE;
            yield Event.BEGIN_SEQUENCE;
        }

        // object arrays, i.e. String
        case Object[] array -> {
            stack.push(Event.END_SEQUENCE);
            stack.push(currentNodeContext);
            stack.push(array);
            stack.push(arrayIterator(array));
            depth += 1;
            currentNodeType = NodeType.SEQUENCE;
            yield Event.BEGIN_SEQUENCE;
        }

        case Enum<?> enumeration -> {
            currentNode = enumeration.name();
            currentNodeType = NodeType.STRING;
            yield Event.SCALAR;
        }

        default -> {
            currentNodeType = switch (currentNode) {
            case Boolean bool -> bool ? NodeType.TRUE : NodeType.FALSE;
            case String string -> NodeType.STRING;
            case Number number -> NodeType.NUMBER;

            default -> throw new IllegalStateException(
                    """
                    Unexpected scalar node value=%s"
                    """.formatted(currentNode));
            };
            yield Event.SCALAR;
        }
        };
    }

    public void reset(Object node) {
        this.stack.clear();
        this.stack.push(node);
        this.depth = 0;
        this.visited = 0;
        this.currentNode = node;
        this.currentNodeContext = NodeContext.ROOT;
    }

    /** Gets the total number of nodes visited so far. */
    public long visited() {
        return visited;
    }

    /**
     * Sets the maximum traversal depth. If the traversal reaches this depth, it
     * will not process the children of nodes at that depth.
     *
     * @param maxDepth the maximum depth, or {@link #UNLIMITED_DEPTH} for no limit.
     * @return
     */
    public NativeTraverser maxDepth(int maxDepth) {
        this.maxDepth = maxDepth;
        return this;
    }

    /**
     * Gets the configured maximum traversal depth.
     *
     * @return the maximum depth, or {@link #UNLIMITED_DEPTH} if no limit is set.
     */
    public int maxDepth() {
        return maxDepth;
    }

    /**
     * Sets the maximum number of nodes to visit. The traversal will throw an
     * {@link IllegalStateException} if this limit is exceeded.
     *
     * @param maxVisitedNodes the maximum number of nodes, or
     *                        {@link #UNLIMITED_NODES} for no limit.
     * @return
     */
    public NativeTraverser maxVisited(int maxVisitedNodes) {
        this.maxVisited = maxVisitedNodes;
        return this;
    }

    /**
     * Gets the configured maximum number of nodes to visit.
     *
     * @return the maximum node count, or {@link #UNLIMITED_NODES} if no limit is
     *         set.
     */
    public int maxVisited() {
        return maxVisited;
    }

    public Object node() {
        return currentNode;
    }

    @Override
    public NodeContext context() {
        return currentNodeContext;
    }

    @Override
    public Number numberValue() {
        return (Number) currentNode;
    }

    @Override
    public String stringValue() {
        return (String) currentNode;
    }

    @Override
    public byte[] binaryValue() {
        return (byte[]) currentNode;
    }

    @Override
    public NodeType nodeType() {
        return currentNodeType;
    }

    @Override
    public int structureSize() {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public void comparator(Comparator<Entry<?, ?>> entryComparator) {
        this.entryComparator = entryComparator;
    }

    private static Iterator<Object> primitiveArrayIterator(Object primitiveArray) {
        return new Iterator<Object>() {
            private int index = 0;
            private final int length = Array.getLength(primitiveArray);

            @Override
            public boolean hasNext() {
                return index < length;
            }

            @Override
            public Object next() {
                if (!hasNext()) {
                    throw new NoSuchElementException();
                }
                return Array.get(primitiveArray, index++);
            }
        };
    }

    private static Iterator<Object> arrayIterator(Object[] array) {
        return new Iterator<Object>() {
            private int index = 0;

            @Override
            public boolean hasNext() {
                return index < array.length;
            }

            @Override
            public Object next() {
                if (!hasNext()) {
                    throw new NoSuchElementException();
                }
                return array[index++];
            }
        };
    }
}
