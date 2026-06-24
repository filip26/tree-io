package com.apicatalog.tree.io;

import java.io.IOException;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Map.Entry;

import com.apicatalog.tree.io.Tree.Event;
import com.apicatalog.tree.io.Tree.EventConsumer;
import com.apicatalog.tree.io.Tree.NodeType;

/**
 * Facilitates stateful traversal of a hierarchical tree structure.
 * <p>
 * The traverser maintains a current position within the tree. Accessor methods
 * (e.g., {@link #stringValue()}, {@link #numberValue()}) are state-dependent
 * and must only be invoked when the traverser is positioned on a compatible
 * {@link NodeType}.
 * </p>
 *
 * @param <T> the type of the tree structure being traversed
 */
public interface TreeTraverser<T> extends TreeCursor, Iterator<Event> {

    /**
     * Initiates a traversal of the specified tree structure.
     *
     * @param consumer the visitor logic invoked for each node, receiving the
     *                 current event type and the traverser instance
     * @throws IOException
     */
    boolean traverse(EventConsumer consumer) throws IOException;

    /**
     * Advances the cursor to the next token in the stream and returns its type.
     *
     * @return the next structural or scalar {@link Event} in the sequence, or null
     *         if the end of the input has been reached.
     */
    @Override
    Event next(); 

    void reset(T tree);

    void comparator(Comparator<Entry<?, ?>> entryComparator);

    /**
     * Returns the number of elements contained in the current structural node.
     * <p>
     * This method is only valid when the current {@link NodeType} represents a
     * collection (e.g., MAP or SEQUENCE).
     * </p>
     *
     * @return the number of elements in the current structure
     * @throws IllegalStateException if the current node is not a MAP or SEQUENCE
     */
    int structureSize();

    // TODO skipMap, skipSequence, skipEntryValue

}