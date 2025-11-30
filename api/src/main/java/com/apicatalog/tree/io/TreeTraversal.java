package com.apicatalog.tree.io;

import java.util.ArrayDeque;
import java.util.Comparator;
import java.util.Deque;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Consumer;
import java.util.stream.Stream;

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

    /**
     * Identifies the role of the current node within the tree structure during
     * traversal.
     */
    public enum Context {

        /** The current node is the root of the tree being traversed. */
        ROOT,

        /** The current node is a key within a map structure. */
        PROPERTY_KEY,

        /** The current node is a value associated with a key in a map structure. */
        PROPERTY_VALUE,

        /** The current node is an element within a collection structure. */
        ELEMENT,

        /**
         * A synthetic marker indicating the end of a map or collection has been
         * reached.
         */
        END,
    }

    /** A sentinel value indicating that traversal depth is not limited. */
    public static final int UNLIMITED_DEPTH = -1;

    /**
     * A sentinel value indicating that the number of visited nodes is not limited.
     */
    public static final int UNLIMITED_NODES = -1;

    protected final Deque<TreeAdapter> adapters;
    protected final Deque<Object> stack;

    protected Comparator<Entry<?, ?>> entryComparator;

    protected int maxVisited;
    protected int maxDepth;

    protected int depth;
    protected int visited;

    protected Object currentNode;
    protected NodeType currentNodeType;
    protected Context currentNodeContext;

    public TreeTraversal() {
        this(new ArrayDeque<>(), null);
    }

    public TreeTraversal(final Deque<Object> stack) {
        this(stack, null);
    }

    public TreeTraversal(final Deque<Object> stack, Comparator<Entry<?, ?>> entryComparator) {
        this.stack = stack;
        this.adapters = new ArrayDeque<>(5);
        this.entryComparator = entryComparator;
        this.maxVisited = UNLIMITED_NODES;
        this.maxDepth = UNLIMITED_DEPTH;
        this.visited = 0;
        this.depth = 0;
        this.currentNode = null;
        this.currentNodeContext = null;
        this.currentNodeType = null;
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

            if (Context.END == currentNodeContext) {
                generator.end();
                continue;
            }

            switch (currentNodeType) {
            case MAP:
                generator.beginMap();
                break;

            case SEQUENCE:
                generator.beginSequence();
                break;

            case NULL:
                generator.nullValue();
                break;

            case TRUE:
                generator.booleanValue(true);
                break;

            case FALSE:
                generator.booleanValue(false);
                break;

            case STRING:
                generator.stringValue(adapter().stringValue(currentNode));
                break;

            case BINARY:
                generator.binaryValue(adapter().binaryValue(currentNode));
                break;

            case NUMBER:
                if (adapter().isIntegral(currentNode)) {
                    generator.numericValue(adapter().integerValue(currentNode));
                } else {
                    generator.numericValue(adapter().decimalValue(currentNode));
                }
                break;

            default:
                throw new IllegalStateException("Unexpected node type: " + currentNodeType);
            }
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
        return next(Context.ROOT);
    }

    protected boolean next(Context stepContext) {

        if (stack.isEmpty()) {
            return false;
        }

        if (maxVisited > 0 && maxVisited <= visited) {
            throw new IllegalStateException("The maximum number [" + maxVisited + "] of visited nodes has been reached.");
        }
        if (maxDepth > 0 && maxDepth < depth) {
            throw new IllegalStateException("The maximum traversal depth [" + maxDepth + "] has been reached.");
        }

        var nodeAdapter = adapters.peek();
        var item = stack.peek();

        if (NodeType.TREE.equals(item)) {
            adapters.pop();
            nodeAdapter = adapters.peek();
            stack.pop();
            item = stack.peek();
        }

        // map or collection
        if (item instanceof Iterator) {

            Iterator<?> it = (Iterator<?>) item;
            if (!it.hasNext()) {
                stack.pop();
                depth -= 1;
                currentNode = stack.pop();
                currentNodeType = (NodeType) stack.pop();
                currentNodeContext = Context.END;
                return true;
            }

            item = it.next();

            if (item instanceof Map.Entry) {
                currentNodeContext = Context.PROPERTY_KEY;
                Map.Entry<?, ?> entry = (Map.Entry<?, ?>) item;

                stack.push(entry);

                // process property key
                currentNode = entry.getKey();
                currentNodeType = nodeAdapter.type(currentNode);

                visited++;

                // process property value
                return true;

            } else {
                // process collection element
                currentNodeContext = Context.ELEMENT;
                currentNode = item;
            }

        } else if (item instanceof Map.Entry) {
            // process property value
            currentNodeContext = Context.PROPERTY_VALUE;
            currentNode = ((Map.Entry<?, ?>) item).getValue();
            stack.pop();

        } else {
            // process root value
            currentNodeContext = stepContext;
            currentNode = item;
            stack.pop();
        }

        currentNodeType = nodeAdapter.type(currentNode);

        switch (currentNodeType) {
        case TREE:
            stack.push(NodeType.TREE);
            final Tree adaptedNode = (Tree) currentNode;
            root(adaptedNode.node(), adaptedNode.adapter());
            return next(currentNodeContext);

        case SEQUENCE:
            stack.push(NodeType.SEQUENCE);
            stack.push(currentNode);
            stack.push(nodeAdapter.asIterable(currentNode).iterator());
            depth += 1;
            break;

        case MAP:
            stack.push(NodeType.MAP);
            stack.push(currentNode);
            Stream<Entry<?, ?>> entryStream = nodeAdapter.entryStream(currentNode);
            if (entryComparator != null) {
                entryStream = entryStream.sorted(entryComparator);
            }
            stack.push(entryStream.iterator());
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
     * @return this instance, for chaining.
     */
    public TreeTraversal reset() {
        this.adapters.clear();
        this.stack.clear();
        this.depth = 0;
        this.visited = 0;
        this.currentNode = null;
        this.currentNodeContext = null;
        this.currentNodeType = null;
        return this;
    }

    /**
     * Sets the root node for the traversal, initializing the visitor's stack.
     *
     * @param node    the new root node.
     * @param adapter the adapter for interpreting the new tree structure.
     * @return this instance, for chaining.
     */
    public TreeTraversal root(Object node, TreeAdapter adapter) {
        this.adapters.push(adapter);
        this.stack.push(node);
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

    /** Gets the adapter to process the {@code #currentNode()}. */
    public TreeAdapter adapter() {
        return adapters.peek();
    }

    /** Gets the current node being processed. */
    public Object node() {
        return currentNode;
    }

    /** Gets the type of the current node. */
    public NodeType nodeType() {
        return currentNodeType;
    }

    /** Gets the context of the current node. */
    public Context nodeContext() {
        return currentNodeContext;
    }
}
