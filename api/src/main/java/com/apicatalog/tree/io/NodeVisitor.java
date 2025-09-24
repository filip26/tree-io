package com.apicatalog.tree.io;

import java.util.ArrayDeque;
import java.util.Comparator;
import java.util.Deque;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;

/**
 * A non-recursive depth-first traversal for arbitrary trees.
 * <p>
 * Traverses trees in depth-first order, visiting nodes one at a time via
 * {@link #step()}. The traversal distinguishes between the root node, map
 * property keys and values, and collection elements using {@link Context}.
 * </p>
 *
 * <p>
 * Traversal rules:
 * </p>
 * <ul>
 * <li>When visiting a map-like node, keys are visited before their
 * corresponding values.</li>
 * <li>When visiting a collection or array node, elements are visited in
 * insertion order.</li>
 * <li>Other node types are visited directly.</li>
 * </ul>
 *
 * <p>
 * Traversal proceeds until all nodes have been visited or limits (maximum depth
 * or maximum nodes) are reached.
 * </p>
 */
public class NodeVisitor {

    /**
     * Indicates the role of the node during traversal.
     */
    public enum Context {

        /** Node is the root of the tree. */
        ROOT,

        /** Node is a property key in a map. */
        PROPERTY_KEY,

        /** Node is a property value in a map. */
        PROPERTY_VALUE,

        /** Node is an element of a collection or array. */
        COLLECTION_ELEMENT,

        /** Marks the end of a collection or map. */
        END,
    }

    /** Sentinel value indicating no maximum depth limit. */
    public static final int UNLIMITED_DEPTH = -1;

    /** Sentinel value indicating no maximum node visit limit. */
    public static final int UNLIMITED_NODES = -1;

    /** Maximum number of nodes allowed to be visited. */
    protected int maxVisited;

    /** Maximum depth allowed during traversal. */
    protected int maxDepth;

    /** Stack used for traversal. */
    protected final Deque<Object> stack;

    /** Comparator for map keys. */
    protected Comparator<Object> keyComparator;

    /** Adapter providing access to node types and children. */
    protected NodeAdapter adapter;

    /** Current depth. */
    protected long depth;

    /** Number of nodes visited. */
    protected long visited;

    /** Current node. */
    protected Object node;

    /** Type of the current node. */
    protected NodeType nodeType;

    /** Role of the current node. */
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
        this.keyComparator = propertyComparator;
        this.maxVisited = UNLIMITED_NODES;
        this.maxDepth = UNLIMITED_DEPTH;
    }

    /**
     * Creates a new {@code NodeVisitor} starting at the given root node, using the
     * default property key ordering - insertion order.
     *
     * @param root    the root node to start traversal from, must not be
     *                {@code null}
     * @param adapter the adapter providing access to node types and children, must
     *                not be {@code null}
     * @return a new {@code NodeVisitor} instance positioned at the root node
     * @throws NullPointerException if {@code root} or {@code adapter} is
     *                              {@code null}
     */
    public static NodeVisitor of(Object root, NodeAdapter adapter) {
        return of(root, adapter, (a, b) -> 0);
    }

    /**
     * Creates a new {@code NodeVisitor} starting at the given root node, using a
     * custom comparator for ordering map property keys.
     *
     * @param root               the root node to start traversal from, must not be
     *                           {@code null}
     * @param adapter            the adapter providing access to node types and
     *                           children, must not be {@code null}
     * @param propertyComparator comparator used to order map keys during traversal,
     *                           must not be {@code null}
     * @return a new {@code NodeVisitor} instance positioned at the root node
     * @throws NullPointerException if {@code root}, {@code adapter}, or
     *                              {@code propertyComparator} is {@code null}
     */
    public static NodeVisitor of(Object root, NodeAdapter adapter, Comparator<Entry<?, ?>> propertyComparator) {
        Objects.requireNonNull(root);
        Objects.requireNonNull(adapter);

        Deque<Object> stack = new ArrayDeque<>();
        stack.push(root);

        return new NodeVisitor(stack, adapter);
    }

    /**
     * Advances the traversal by one step.
     *
     * <p>
     * Processes exactly one node, updating {@link #node}, {@link #nodeType}, and
     * {@link #nodeContext} to describe it. Subsequent calls continue traversal
     * until the entire tree has been visited.
     * </p>
     * 
     * @return {@code true} if a node was processed, or {@code false} if traversal
     *         is complete
     * @throws IllegalStateException if the traversal exceeds the maximum node count
     *                               or the maximum depth configured via
     *                               {@link #maxVisited(int)} or
     *                               {@link #maxDepth(int)}
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
                nodeType = adapter.typeOf(node);

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

        nodeType = adapter.typeOf(node);

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
            stack.push(adapter.entryStream(node)
                    .sorted(keyComparator)
                    .iterator());
            depth += 1;
            break;

        default:
            break;
        }

        visited++;
        return true;
    }

    /** Returns the number of nodes visited. */
    public long visited() {
        return visited;
    }

    /**
     * Resets the traversal with a new root node and adapter.
     *
     * @param node    the new root node
     * @param adapter the adapter providing access to node types
     */
    public void reset(Object node, NodeAdapter adapter) {
        this.adapter = adapter;
        this.stack.clear();
        this.stack.push(node);
        this.depth = 0;
        this.visited = 0;
    }

    /**
     * Sets the comparator used to order map property keys during traversal.
     *
     * <p>
     * The comparator determines the order in which map keys are visited. By
     * default, keys are visited in insertion order if no comparator is set.
     *
     * @param keyComparator comparator for map keys; must not be null
     */
    public void keyComparator(Comparator<Object> keyComparator) {
        this.keyComparator = keyComparator;
    }

    /**
     * Sets the maximum depth of traversal.
     *
     * <p>
     * If the traversal reaches this depth, further children will not be visited.
     * Use {@link #UNLIMITED_DEPTH} (-1) to indicate no depth limit (default).
     * </p>
     *
     * @param maxDepth maximum depth allowed during traversal
     */
    public void maxDepth(int maxDepth) {
        this.maxDepth = maxDepth;
    }

    /**
     * Returns the maximum depth allowed during traversal.
     *
     * @return maximum depth; -1 if no limit
     */
    public int maxDepth() {
        return maxDepth;
    }

    /**
     * Sets the maximum number of nodes that can be visited during traversal.
     *
     * <p>
     * If the number of visited nodes reaches this limit, {@link #step()} will throw
     * {@link IllegalStateException}. Use {@link #UNLIMITED_NODES} (-1) to indicate
     * no limit (default).
     * </p>
     *
     * @param maxVisitedNodes maximum number of nodes to visit
     */
    public void maxVisited(int maxVisitedNodes) {
        this.maxVisited = maxVisitedNodes;
    }

    /**
     * Returns the maximum number of nodes that can be visited during traversal.
     *
     * @return maximum number of nodes; -1 if no limit
     */
    public int maxVisited() {
        return maxVisited;
    }
}
