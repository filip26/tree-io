package com.apicatalog.tree.io;

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
     * Retrieves the numeric value at the current cursor position.
     *
     * @return the numeric value as a Number
     * @throws IllegalStateException if the current node is not a NUMBER type
     */
    Number numberValue();

    /**
     * Retrieves the string value at the current cursor position.
     *
     * @return the string value
     * @throws IllegalStateException if the current node is not a STRING type
     */
    String stringValue();

    /**
     * Retrieves the binary data at the current cursor position.
     *
     * @return a byte array containing the binary payload
     * @throws IllegalStateException if the current node is not a BINARY type
     */
    byte[] binaryValue();

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