package com.apicatalog.tree.io;

/**
 * Provides a uniform, performant, pull-based streaming abstraction for parsing
 * tree-like data structures. This interface decouples the process of reading a
 * tree from its underlying representation, making it suitable for both
 * deserialization and dematerialization.
 * <p>
 * The parser is forward-only and token-driven, maintaining the internal state
 * of the current position in the input stream. It emits fine-grained structural
 * and scalar tokens. The parser does not track structural roles; the caller is
 * entirely responsible for keeping track of the structural context (such as
 * tracking whether a token represents a map key, a map value, or a sequence
 * element) during traversal.
 * </p>
 */
public interface TreeParser extends TreeProcessor {

    /**
     * Represents the structural and scalar components encountered during the
     * parsing of a tree structure.
     */
    enum Token {
        /**
         * Indicates the start of a map structure.
         */
        BEGIN_MAP,

        /**
         * Indicates the end of a map structure.
         */
        END_MAP,

        /**
         * Indicates the start of an ordered sequence structure.
         */
        BEGIN_SEQUENCE,

        /**
         * Indicates the end of an ordered sequence structure.
         */
        END_SEQUENCE,

        /**
         * Indicates a literal null value.
         */
        NULL,

        /**
         * Indicates a boolean true literal.
         */
        TRUE,

        /**
         * Indicates a boolean false literal.
         */
        FALSE,

        /**
         * Indicates a numeric value.
         */
        NUMBER,

        /**
         * Indicates a text string.
         */
        STRING,

        /**
         * Indicates a binary data payload.
         */
        BINARY
    }

    /**
     * Advances the parser to the next token in the stream and returns its type.
     *
     * @return the next structural or scalar {@link Token} in the sequence, or null
     *         if the end of the input (EOF) has been reached.
     * @throws TreeIOException 
     */
    Token nextToken() throws TreeIOException;

    /**
     * Returns the scalar value associated with the current token position.
     * <p>
     * This method is intended to be called when the parser is positioned on a
     * scalar token (e.g., NUMBER, STRING, BINARY). Calling this method when the
     * parser is positioned on structural boundaries (e.g., BEGIN_MAP, END_MAP)
     * yields a null result.
     * </p>
     *
     * @return the Java Object mapping to the current scalar token value, or null if
     *         the current token holds no scalar data.
     * @throws TreeIOException 
     */
    Object getScalar() throws TreeIOException;
}