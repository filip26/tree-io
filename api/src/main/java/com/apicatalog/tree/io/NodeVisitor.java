package com.apicatalog.tree.io;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayDeque;
import java.util.Comparator;
import java.util.Deque;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.function.BiConsumer;

/**
 * A non-recursive depth-first traversal utility for arbitrary object trees.
 * <p>
 * This class uses an explicit {@link Deque} as a stack to drive traversal,
 * avoiding recursion. It relies on a {@link NodeAdapter} to abstract access to
 * collections and map-like structures.
 * </p>
 *
 * <p>
 * Traversal rules:
 * </p>
 * <ul>
 * <li>When visiting a {@code Map}-like node, keys are delivered to the consumer
 * before their corresponding values. Values are pushed back onto the stack to
 * be visited later.</li>
 * <li>When visiting a collection/array node, its elements are traversed in
 * iteration order.</li>
 * <li>Other node types are delivered to the consumer directly.</li>
 * </ul>
 *
 * <p>
 * The traversal proceeds step-by-step: each call to {@link #step(BiConsumer)}
 * visits exactly one node and schedules its children (if any). Repeated calls
 * continue traversal until the stack is empty.
 * </p>
 */
public class NodeVisitor {

    /**
     * Indicates the type of node currently being visited during traversal.
     */
    public enum Context {

        ROOT,

        /**
         * Visiting the key/name of a property in a map/object.
         */
        PROPERTY_KEY,

        /**
         * Visiting the value of a property in a map/object.
         */
        PROPERTY_VALUE,

        /**
         * Visiting an element within a collection, array, or list.
         */
        COLLECTION_ELEMENT,

        END,
    }

    public static final int MAX_DEPTH = -1;
    public static final int MAX_NODES = -1;

    protected int maxVisited;
    protected int maxDepth;

    /** Traversal stack; elements may be nodes or iterators of child nodes. */
    protected final Deque<Object> stack;

    protected Comparator<Object> propertyComparator;

    /** Adapter that provides access to node types and their children. */
    protected NodeAdapter adapter;

    protected long depth;
    protected long visited;

    /** Runtime */
    protected Object node;
    protected NodeType nodeType;
    protected Context nodeContext;

    /**
     * Creates a new traversal with the given stack and adapter.
     *
     * @param stack   the stack to use for traversal state
     * @param adapter the adapter providing node access
     */
    protected NodeVisitor(final Deque<Object> stack, final NodeAdapter adapter) {
        this(stack, adapter, (a, b) -> 0);
    }

    protected NodeVisitor(final Deque<Object> stack, final NodeAdapter adapter, Comparator<Object> propertyComparator) {
        this.stack = stack;
        this.adapter = null;
        this.visited = 0;
        this.depth = 0;
        this.propertyComparator = propertyComparator;
        this.maxVisited = MAX_NODES;
        this.maxDepth = MAX_DEPTH;
    }

    public static NodeVisitor of(Object root, NodeAdapter adapter) {
        return of(root, adapter, (a, b) -> 0);
    }

    /**
     * Creates a depth-first traversal starting from the given root node.
     *
     * @param root               the root node, must not be {@code null}
     * @param adapter            the adapter providing node access, must not be
     *                           {@code null}
     * @param propertyComparator
     * @return a new traversal instance positioned at the root
     * @throws NullPointerException if {@code root} or {@code adapter} is
     *                              {@code null}
     */
    public static NodeVisitor of(Object root, NodeAdapter adapter, Comparator<Object> propertyComparator) {
        Objects.requireNonNull(root);
        Objects.requireNonNull(adapter);

        Deque<Object> stack = new ArrayDeque<>();
        stack.push(root);

        return new NodeVisitor(stack, adapter);
    }

    /**
     * Advances the traversal by one step and delivers the current node to the
     * provided consumer.
     *
     * <p>
     * If the current stack element is an {@link Iterator}, its next value is
     * retrieved. If that value is a {@link Map.Entry}, the key is delivered to the
     * consumer immediately and the corresponding value is pushed back onto the
     * stack for later traversal.
     * </p>
     *
     * <p>
     * After delivering a node to the consumer, its children are discovered using
     * the {@link NodeAdapter} and pushed onto the stack if available.
     * </p>
     *
     * @param consumer a consumer to receive the current node, not {@code null}
     * @return {@code true} if a node has been processed, otherwise {@code false} if
     *         traversal is complete
     */
    public boolean step() {

        if (stack.isEmpty()) {
            return false;
        }

        if (maxVisited > 0 && maxVisited <= visited) {
            throw new IllegalStateException();
        }
        if (maxDepth > 0 && maxDepth < depth) {
            throw new IllegalStateException();
        }

        Object item = stack.peek();

        // map or collection
        if (item instanceof Iterator) {

            Iterator<?> it = (Iterator<?>) item;
            if (!it.hasNext()) {
                stack.pop();
                depth -= 1;
                node = stack.pop();
                nodeType = (NodeType) stack.pop();
                nodeContext = Context.END;
                return true;
            }

            item = it.next();

            if (item instanceof Map.Entry) {
                nodeContext = Context.PROPERTY_KEY;
                Map.Entry<?, ?> entry = (Map.Entry<?, ?>) item;

                stack.push(entry);

                // process property key
                node = entry.getKey();
                nodeType = adapter.type(node);

                visited++;

                // process property value
                return true;

            } else {
                // process collection element
                nodeContext = Context.COLLECTION_ELEMENT;
                node = item;
            }

        } else if (item instanceof Map.Entry) {
            // process property value
            nodeContext = Context.PROPERTY_VALUE;
            node = ((Map.Entry<?, ?>) item).getValue();
            stack.pop();

        } else {
            // process root value
            nodeContext = Context.ROOT;
            node = item;
            stack.pop();
        }

        nodeType = adapter.type(node);

        switch (nodeType) {
        case COLLECTION:
            stack.push(NodeType.COLLECTION);
            stack.push(node);
            stack.push(adapter.asIterable(node).iterator());
            depth += 1;
            break;

        case MAP:
            stack.push(NodeType.MAP);
            stack.push(node);
            stack.push(adapter.keys(node)
                    .stream()
                    .sorted(propertyComparator)
                    .map(key -> new SimpleEntry<>(key, adapter.property(key, node)))
                    .iterator());
            depth += 1;
            break;

        default:
            break;
        }

        visited++;
        return true;
    }

    public long visited() {
        return visited;
    }

    public void reset(Object node, NodeAdapter adapter) {
        this.adapter = adapter;
        this.stack.clear();
        this.stack.push(node);
        this.depth = 0;
        this.visited = 0;
    }

    public void keyComparator(Comparator<Object> keyComparator) {
        this.propertyComparator = keyComparator;
    }

    public void maxDepth(int maxDepth) {
        this.maxDepth = maxDepth;
    }

    public int maxDepth() {
        return maxDepth;
    }

    public void maxVisited(int maxVisitedNodes) {
        this.maxVisited = maxVisitedNodes;
    }

    public int maxVisited() {
        return maxVisited;
    }
}
