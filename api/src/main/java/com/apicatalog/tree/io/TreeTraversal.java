package com.apicatalog.tree.io;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayDeque;
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
 * The traversal proceeds step-by-step: each call to {@link #traverse(BiConsumer)}
 * visits exactly one node and schedules its children (if any). Repeated calls
 * continue traversal until the stack is empty.
 * </p>
 */
public class TreeTraversal {

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

    /** Traversal stack; elements may be nodes or iterators of child nodes. */
    protected final Deque<Object> stack;

    /** Adapter that provides access to node types and their children. */
    protected NodeAdapter adapter;

    protected long depth;
    protected long visited;

    /**
     * Creates a new traversal with the given stack and adapter.
     *
     * @param stack   the stack to use for traversal state
     * @param adapter the adapter providing node access
     */
    protected TreeTraversal(final Deque<Object> stack, final NodeAdapter adapter) {
        this.stack = stack;
        this.adapter = null;
        this.visited = 0;
        this.depth = 0;
    }

    /**
     * Creates a depth-first traversal starting from the given root node.
     *
     * @param root    the root node, must not be {@code null}
     * @param adapter the adapter providing node access, must not be {@code null}
     * @return a new traversal instance positioned at the root
     * @throws NullPointerException if {@code root} or {@code adapter} is
     *                              {@code null}
     */
    public static TreeTraversal of(Object root, NodeAdapter adapter) {
        Objects.requireNonNull(root);
        Objects.requireNonNull(adapter);

        Deque<Object> stack = new ArrayDeque<>();
        stack.push(root);

        return new TreeTraversal(stack, adapter);
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
     * @return {@code true} if there are more nodes to traverse, {@code false} if
     *         traversal is complete
     */
    public boolean traverse(BiConsumer<Context, Object> consumer) {

        if (stack.isEmpty()) {
            return false;
        }

        final Context ctx;
        final Object node;
        Object item = stack.peek();

        if (item instanceof Iterator) {

            Iterator<?> it = (Iterator<?>) item;
            if (!it.hasNext()) {
                stack.pop();
                depth -= 1;
                consumer.accept(Context.END, null);
                return !stack.isEmpty();
            }

            item = it.next();

            if (item instanceof Map.Entry) {
                ctx = Context.PROPERTY_KEY;
                Map.Entry<?, ?> entry = (Map.Entry<?, ?>) item;
                node = entry.getKey();
                stack.push(entry);

            } else {
                ctx = Context.COLLECTION_ELEMENT;
                node = item;
            }

        } else if (item instanceof Map.Entry) {
            ctx = Context.PROPERTY_VALUE;
            node = ((Map.Entry<?, ?>) item).getValue();
            stack.pop();

        } else {
            ctx = Context.ROOT;
            node = item;
            stack.pop();
        }

        switch (adapter.type(node)) {
        case COLLECTION:
            stack.push(adapter.asIterable(node).iterator());
            depth += 1;
            break;

        case MAP:
            stack.push(adapter.properties(node)
                    .stream()
                    .map(prop -> new SimpleEntry<>(prop, adapter.property(prop, node)))
                    .iterator());
            depth += 1;
            break;

        default:
            break;
        }

        consumer.accept(ctx, node);
        visited++;

        return !stack.isEmpty();
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
}
