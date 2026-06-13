package com.apicatalog.tree.io;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Comparator;
import java.util.Deque;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Consumer;

import com.apicatalog.tree.io.Tree.NodeType;

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
public class TreeTraversal {

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
    protected NodeContext currentNodeContext;

    public TreeTraversal(Object tree) {
        this(new ArrayDeque<>(), null);
        this.stack.push(tree);
    }

    public TreeTraversal(Object tree, Comparator<Entry<?, ?>> entryComparator) {
        this(new ArrayDeque<>(), entryComparator);
        this.stack.push(tree);
    }

    protected TreeTraversal(final Deque<Object> stack, Comparator<Entry<?, ?>> entryComparator) {
        this.stack = stack;
        this.entryComparator = entryComparator;
        this.maxVisited = UNLIMITED_NODES;
        this.maxDepth = UNLIMITED_DEPTH;
        this.visited = 0;
        this.depth = 0;
        this.currentNode = null;
        this.currentNodeContext = null;
    }

    public void traverse(final Consumer<TreeTraversal> consumer) {
        while (next()) {
            consumer.accept(this);
        }
    }

    /**
     * A high-level utility method that fully traverses the tree and drives the
     * provided {@link TreeGenerator}. This is the primary method for tree
     * transformation, serialization, or deep cloning. It iterates through every
     * node using {@link #next()} and emits a corresponding event to the generator.
     *
     * @param generator the generator that will receive construction events.
     * @throws TreeIOException       if the generator encounters an I/O error.
     * @throws IllegalStateException if the source tree is malformed (e.g., unclosed
     *                               structures).
     */
    public void generate(final TreeGenerator generator) throws TreeIOException {
        while (next()) {

//            if (NodeContext.END == currentNodeContext) {
//                generator.endMap();
//                continue;
//            }
//
//            switch (currentNode) {
//            case Map<?, ?> map -> generator.beginMap();
//            case Collection<?> col -> generator.beginSequence();
//
//            case null -> generator.nullValue();
//            case Boolean bool -> generator.booleanValue(bool);
//            case String string -> generator.stringValue(string);
//            case Number number -> generator.numericValue(number);
//            case byte[] bytes -> generator.binaryValue(bytes);
//
//            default -> throw new IllegalArgumentException(
//                    """
//                    Unexpected node type=%s, value=%s"
//                    """.formatted(currentNode.getClass(), currentNode));
//            }
        }

        if (depth > 0) {
            throw new IllegalStateException("The traversed tree is malformed. A map or a collection was not properly closed.");
        }
    }

    /**
     * Advances the traversal to the next node in the depth-first sequence.
     * <p>
     * Each call to this method processes exactly one node or structural marker.
     * After a successful call, the visitor's state is updated, and the details of
     * the current item can be accessed via {@link #node()}, {@link #nodeType()},
     * and {@link #nodeContext()}.
     * </p>
     *
     * @return {@code true} if the traversal advanced to a new item, or
     *         {@code false} if the traversal is complete.
     * @throws IllegalStateException if the traversal exceeds configured limits
     *                               (e.g., maximum depth or node count).
     */
    public boolean next() {
        return next(NodeContext.ROOT);
    }

    protected boolean next(NodeContext stepContext) {

        if (stack.isEmpty()) {
            return false;
        }

        if (maxVisited > 0 && maxVisited <= visited) {
            throw new IllegalStateException(
                    """
                    The maximum=%d visited nodes has been reached.
                    """.formatted(maxVisited));
        }
        if (maxDepth > 0 && maxDepth < depth) {
            throw new IllegalStateException(
                    """
                    The maximum traversal depth=%d has been reached.                        
                    """.formatted(maxDepth)
                    );
        }

        var item = stack.peek();

//        if (NodeType.TREE.equals(item)) {
//            stack.pop();
//            item = stack.peek();
//        }

        // map or collection
        if (item instanceof Iterator<?> it) {

            if (!it.hasNext()) {
                stack.pop();
                depth -= 1;
                currentNode = stack.pop();
                currentNodeContext = NodeContext.END;
                return true;
            }

            item = it.next();

            if (item instanceof Map.Entry<?, ?> entry) {
                // process map entry
                currentNodeContext = NodeContext.PROPERTY_KEY;

                stack.push(entry);

                // process property key
                currentNode = entry.getKey();

                visited++;

                // process property value
                return true;

            } else {
                // process collection element
                currentNodeContext = NodeContext.ELEMENT;
                currentNode = item;
            }

        } else if (item instanceof Map.Entry<?, ?> entry) {
            // process property value
            currentNodeContext = NodeContext.PROPERTY_VALUE;
            currentNode = entry.getValue();
            stack.pop();

        } else {
            // process root value
            currentNodeContext = stepContext;
            currentNode = item;
            stack.pop();
        }

        switch (currentNode) {
//        case TREE:
//            stack.push(NodeType.TREE);
//            final Tree adaptedNode = (Tree) currentNode;
//            root(adaptedNode.node(), adaptedNode.adapter());
//            return next(currentNodeContext);

        case Collection<?> col:
            stack.push(NodeType.SEQUENCE);
            stack.push(currentNode);
            stack.push(col.iterator());
            depth += 1;
            break;

        case Map<?, ?> map:
            stack.push(NodeType.MAP);
            stack.push(currentNode);

            if (entryComparator != null) {
                stack.push(map.entrySet().stream().sorted(entryComparator).iterator());

            } else {
                stack.push(map.entrySet().iterator());
            }

            depth += 1;
            break;

        default:
            break;
        }

        visited++;
        return true;
    }

    /**
     * Resets the visitor's internal state, clearing the traversal stack and
     * counters. The visitor can be reused after calling this method, but a new root
     * node must be set using {@link #root(Object, TreeAdapter)}.
     * 
     * @param node the new root node.
     * @return this instance, for chaining.
     */
    public TreeTraversal reset(Object node) {
        this.stack.clear();
        this.stack.push(node);
        this.depth = 0;
        this.visited = 0;
        this.currentNode = null;
        this.currentNodeContext = null;
        return this;
    }

//    /**
//     * Sets the root node for the traversal, initializing the visitor's stack.
//     *
//     * @param node the new root node.
//     * @return this instance, for chaining.
//     */
//    public TreeTraversal root(Object node) {
//        this.stack.push(node);
//        return this;
//    }

    /** Gets the total number of nodes visited so far. */
    public long visited() {
        return visited;
    }

    /**
     * Sets the maximum traversal depth. If the traversal reaches this depth, it
     * will not process the children of nodes at that depth.
     *
     * @param maxDepth the maximum depth, or {@link #UNLIMITED_DEPTH} for no limit.
     */
    public void maxDepth(int maxDepth) {
        this.maxDepth = maxDepth;
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
     */
    public void maxVisited(int maxVisitedNodes) {
        this.maxVisited = maxVisitedNodes;
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

    /** Gets the current node being processed. */
    public Object node() {
        return currentNode;
    }

    /** Gets the context of the current node. */
    public NodeContext nodeContext() {
        return currentNodeContext;
    }
}
