package com.apicatalog.tree.io;

import com.apicatalog.tree.io.Tree.Event;
import com.apicatalog.tree.io.Tree.NodeContext;
import com.apicatalog.tree.io.Tree.NodeType;

/**
 * Represents a read-only cursor pointing to the current position within a tree
 * structure.
 * <p>
 * This interface provides a stateful view of the current traversal or parsing
 * position. Accessor methods are valid only when the cursor is positioned on a
 * node type compatible with the requested data.
 * </p>
 */
public interface TreeCursor {

    /**
     * Advances the cursor to the next token in the stream and returns its type.
     *
     * @return the next structural or scalar {@link Event} in the sequence, or null
     *         if the end of the input (EOF) has been reached.
     * @throws TreeIOException
     */
    Event next() throws TreeIOException;
    
    
    /**
     * Retrieves the numeric value at the current cursor position.
     *
     * @return the numeric value as a Number
     * @throws IllegalStateException if the current node is not a NUMBER type
     * @throws TreeIOException       if an I/O error occurs while retrieving the
     *                               value
     */
    Number numberValue() throws TreeIOException;

    /**
     * Retrieves the string value at the current cursor position.
     *
     * @return the string value
     * @throws IllegalStateException if the current node is not a STRING type
     * @throws TreeIOException       if an I/O error occurs while retrieving the
     *                               value
     */
    String stringValue() throws TreeIOException;

    /**
     * Retrieves the binary data at the current cursor position.
     *
     * @return a byte array containing the binary payload
     * @throws IllegalStateException if the current node is not a BINARY type
     * @throws TreeIOException       if an I/O error occurs while retrieving the
     *                               value
     */
    byte[] binaryValue() throws TreeIOException;

    /**
     * Returns the type of the current node.
     *
     * @return the current NodeType
     */
    NodeType nodeType();

    /**
     * Returns the navigational context of the current node.
     *
     * @return the current NodeContext
     */
    NodeContext context();

}