package com.apicatalog.tree.io;

import java.util.function.Consumer;

import com.apicatalog.tree.io.Tree.NodeContext;
import com.apicatalog.tree.io.Tree.NodeType;

public interface TreeTraverser<T> {

    void traverse(T node, final Consumer<TreeTraverser<T>> consumer) throws TreeIOException;


    /**
     * Returns the numeric value associated with the current token position.
     * <p>
     * This method is intended to be called when the parser is positioned on a
     * NUMBER token.
     * </p>
     *
     * @return the Number mapping to the current numeric token
     * @throws TreeIOException if an I/O error occurs while retrieving the value.
     */
    Number numberValue() throws TreeIOException;

    /**
     * Returns the string value associated with the current token position.
     * <p>
     * This method is intended to be called when the parser is positioned on a
     * STRING token.
     * </p>
     *
     * @return the String mapping to the current text token
     * @throws TreeIOException if an I/O error occurs while retrieving the value.
     */
    String stringValue() throws TreeIOException;

    /**
     * Returns the binary data associated with the current token position.
     * <p>
     * This method is intended to be called when the parser is positioned on a
     * BINARY token.
     * </p>
     *
     * @return a byte array containing the binary payload
     * @throws TreeIOException if an I/O error occurs while retrieving the value.
     */
    byte[] binaryValue() throws TreeIOException;

    /** Gets the node type of the current node. */
    NodeType nodeType();
    
    /** Gets the context of the current node. */
    NodeContext context();
}
