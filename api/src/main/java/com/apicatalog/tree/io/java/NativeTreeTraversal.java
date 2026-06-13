package com.apicatalog.tree.io.java;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Comparator;
import java.util.Deque;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Consumer;

import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeGenerator;
import com.apicatalog.tree.io.TreeTraversal;
import com.apicatalog.tree.io.TreeGenerator.Context;
import com.apicatalog.tree.io.TreeParser.Token;
import com.apicatalog.tree.io.TreeTraversal.Event;

/**
 * Provides a stateful, non-recursive, depth-first iterator for arbitrary
 * tree-like structures. This class decouples the traversal algorithm from the
 * concrete representation of the tree by operating on the {@link TreeAdapter}
 * abstraction.
 * <p>
 * It can be used in two primary ways:
 * </p>
 * <ol>
 * <li><b>Manual Iteration:</b> By repeatedly calling the {@link #next()} method
 * in a loop, you can process each node individually, allowing for complex logic
 * like searching, validation, or conditional processing.</li>
 * <li><b>Automated Transformation:</b> The {@link #generate(TreeGenerator)}
 * method provides a high-level utility to walk the entire tree and drive a
 * {@link TreeGenerator}, effectively translating or transforming one tree
 * representation into another.</li>
 * </ol>
 * <p>
 * <b>Traversal Rules:</b>
 * </p>
 * <ul>
 * <li>When visiting a map, keys are visited before their corresponding values.
 * The order of keys can be controlled with a custom {@link Comparator}.</li>
 * <li>When visiting a collection, elements are visited in their natural
 * iteration order.</li>
 * </ul>
 */
public class NativeTreeTraversal implements TreeTraversal {

    /** A sentinel value indicating that traversal depth is not limited. */
    public static final int UNLIMITED_DEPTH = -1;

    /**
     * A sentinel value indicating that the number of visited nodes is not limited.
     */
    public static final int UNLIMITED_NODES = -1;

    protected final Deque<Object> stack;

    protected Comparator<Entry<?, ?>> entryComparator;

    protected int maxVisited;
    protected int maxDepth;

    protected int depth;
    protected int visited;

    protected Object currentNode;

    protected Token currentToken;
    protected Context currentNodeContext;

    public NativeTreeTraversal() {
        this(new ArrayDeque<>(), null);
    }

    public NativeTreeTraversal(Comparator<Entry<?, ?>> entryComparator) {
        this(new ArrayDeque<>(), entryComparator);
    }

    protected NativeTreeTraversal(final Deque<Object> stack, Comparator<Entry<?, ?>> entryComparator) {
        this.stack = stack;
        this.entryComparator = entryComparator;
        this.maxVisited = UNLIMITED_NODES;
        this.maxDepth = UNLIMITED_DEPTH;
        this.visited = 0;
        this.depth = 0;
        this.currentNode = null;
        this.currentNodeContext = null;
    }

    @Override
    public void traverse(Object node, final Consumer<NativeTreeTraversal> consumer) {

        node(node);

        var event = next();

        while (event != Event.END) {
            consumer.accept(this);
            event = next();
        }
    }

    @Override
    public Event next() {

        if (stack.isEmpty()) {
            return Event.END;
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
                currentNodeContext = (Context) stack.pop();
                depth -= 1;
                return (Event) stack.pop();
            }

            item = it.next();

            if (item instanceof Map.Entry<?, ?> entry) {
                // process map entry
                currentNodeContext = Context.ENTRY_KEY;

                stack.push(entry);

                // process property key
                currentNode = entry.getKey();

            } else {
                // process collection element
                currentNodeContext = Context.ELEMENT;
                currentNode = item;
            }

        } else if (item instanceof Map.Entry<?, ?> entry) {
            // process property value
            currentNodeContext = Context.ENTRY_VALUE;
            currentNode = entry.getValue();
            stack.pop();

        } else {
            currentNode = item;
            stack.pop();
        }

        visited++;

        switch (currentNode) {
        case Collection<?> col:
            stack.push(Event.END_SEQUENCE);
            stack.push(currentNodeContext);
            stack.push(col);
            stack.push(col.iterator());
            depth += 1;
            return Event.BEGIN_SEQUENCE;

        case Map<?, ?> map:
            stack.push(Event.END_MAP);
            stack.push(currentNodeContext);
            stack.push(map);

            if (entryComparator != null) {
                stack.push(map.entrySet().stream().sorted(entryComparator).iterator());

            } else {
                stack.push(map.entrySet().iterator());
            }

            depth += 1;
            return Event.BEGIN_MAP;

        default:
            return Event.SCALAR;
        }
    }

    @Override
    public NativeTreeTraversal node(Object node) {
        this.stack.clear();
        this.stack.push(node);
        this.depth = 0;
        this.visited = 0;
        this.currentNode = node;
        this.currentNodeContext = Context.ROOT;
        return this;
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
    public NativeTreeTraversal maxDepth(int maxDepth) {
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
    public NativeTreeTraversal maxVisited(int maxVisitedNodes) {
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

    @Override
    public Object node() {
        return currentNode;
    }

    @Override
    public Context context() {
        return currentNodeContext;
    }
}
