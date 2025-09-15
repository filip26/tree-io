package com.apicatalog.tree.io;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;

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
 * The traversal proceeds step-by-step: each call to {@link #traverse(Consumer)}
 * visits exactly one node and schedules its children (if any). Repeated calls
 * continue traversal until the stack is empty.
 * </p>
 */
public class DepthFirstTraversal {
    
    /** Traversal stack; elements may be nodes or iterators of child nodes. */
    protected final Deque<Object> stack;

    /** Adapter that provides access to node types and their children. */
    protected final NodeAdapter adapter;

    /**
     * Creates a new traversal with the given stack and adapter.
     *
     * @param stack   the stack to use for traversal state
     * @param adapter the adapter providing node access
     */
    protected DepthFirstTraversal(final Deque<Object> stack, final NodeAdapter adapter) {
        this.stack = stack;
        this.adapter = adapter;
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
    public static DepthFirstTraversal of(Object root, NodeAdapter adapter) {
        Objects.requireNonNull(root);
        Objects.requireNonNull(adapter);

        Deque<Object> stack = new ArrayDeque<>();
        stack.push(root);

        return new DepthFirstTraversal(stack, adapter);
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
    public boolean traverse(Consumer<Object> consumer) {

        if (stack.isEmpty()) {
            return false;
        }

        final Object node;
        Object item = stack.peek();

        if (item instanceof Iterator) {

            Iterator<?> it = (Iterator<?>) item;
            if (!it.hasNext()) {
                stack.pop();
                return !stack.isEmpty() && traverse(consumer); // tail recurse
            }

            item = it.next();

            if (!it.hasNext()) {
                stack.pop();
            }

            if (item instanceof Map.Entry) {
                Map.Entry<?, ?> entry = (Map.Entry<?, ?>) item;
                node = entry.getKey();
                stack.push(entry.getValue());
            } else {
                node = item;
            }
        } else {
            node = item;
            stack.pop(); // consume plain node
        }

        consumer.accept(node);

        switch (adapter.type(node)) {
        case COLLECTION:
            Iterator<?> it2 = adapter.asIterable(node).iterator();
            if (it2.hasNext()) {
                stack.push(it2);
            }
            break;

        case MAP:
            Iterator<?> it3 = adapter.properties(node)
                    .stream()
                    .map(prop -> new SimpleEntry<>(prop, adapter.property(prop, node)))
                    .iterator();

            if (it3.hasNext()) {
                stack.push(it3);
            }
            break;

        default:
        }

        return !stack.isEmpty();
    }

    public int depth() {
        return stack.size();
    }
}
