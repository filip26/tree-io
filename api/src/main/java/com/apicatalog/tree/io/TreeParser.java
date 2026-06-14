package com.apicatalog.tree.io;

import com.apicatalog.tree.io.Tree.Event;
import com.apicatalog.tree.io.Tree.NodeContext;
import com.apicatalog.tree.io.Tree.NodeType;

/**
 * Provides a uniform, performant, pull-based streaming abstraction for parsing
 * tree-like data structures. This interface decouples the process of reading a
 * tree from its underlying representation, making it suitable for both
 * deserialization and re-materialization.
 * <p>
 * The parser is forward-only and token-driven, maintaining the internal state
 * of the current position in the input stream. It emits fine-grained structural
 * and scalar tokens. The parser does not track structural roles; the caller is
 * entirely responsible for keeping track of the structural context (such as
 * tracking whether a token represents a map key, a map value, or a sequence
 * element) during traversal.
 * </p>
 */
public interface TreeParser {

    /**
     * Advances the parser to the next token in the stream and returns its type.
     *
     * @return the next structural or scalar {@link Event} in the sequence, or null
     *         if the end of the input (EOF) has been reached.
     * @throws TreeIOException
     */
    Event next() throws TreeIOException;

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
    public NodeContext context();

}