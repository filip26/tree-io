package com.apicatalog.tree.io;

import java.util.function.Consumer;

import com.apicatalog.tree.io.Tree.Event;
import com.apicatalog.tree.io.Tree.NodeContext;

public interface TreeTraversal {

    void traverse(Object node, final Consumer<TreeTraversal> consumer);
    
    /**
     * Advances the traversal to the next node in the depth-first sequence.
     * <p>
     * Each call to this method processes exactly one node or structural marker.
     * After a successful call, the visitor's state is updated, and the details of
     * the current item can be accessed via {@link #node()}.
     * </p>
     *
     * @return event
     * @throws IllegalStateException if the traversal exceeds configured limits
     *                               (e.g., maximum depth or node count).
     */
    public Event next();

    /**
     * Sets the visitor's internal state, clearing the traversal stack and counters.
     * The visitor can be reused after calling this method, but a new root node must
     * be set using {@link #node(Object)} before calling {@link #next()}
     * 
     * @param node the new root node.
     * @return this instance, for chaining.
     */
    TreeTraversal node(Object node);

    /** Gets the current node being processed. */
    Object node();

    /** Gets the context of the current node. */
    public NodeContext context();
}
