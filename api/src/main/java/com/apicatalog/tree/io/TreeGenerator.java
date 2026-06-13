package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Provides a uniform, performant, event-based abstraction for generating
 * tree-like data structures. This interface decouples the process of describing
 * a tree from its final representation, making it suitable for both
 * serialization and materialization.
 * <p>
 * This interface is explicitly designed to enable completely stateless,
 * stack-free generator implementations. To eliminate internal state tracking or
 * stacks within the generator, the responsibility of tracking the structural
 * hierarchy is shifted entirely to the caller. The {@link Context} passed into
 * each method originates from the processing or traversal of a source tree
 * structure (e.g., a tree reader, walker, or visitor). This allows the
 * structural state to be piped directly from the source tracking mechanism into
 * the generator, enabling a zero-allocation, stateless data transformation or
 * rendering pipeline.
 * </p>
 * <p>
 * By leveraging this origin context per event, implementations can immediately
 * determine the correct structural layout, delimiters, or object-graph
 * placement deterministically without maintaining state records or looking up
 * an internal nesting stack.
 * </p>
 */
public interface TreeGenerator extends TreeProcessor {

    /**
     * Defines the structural role of the token or value being emitted. Ordinarily
     * originates from the structural state of the source tree during traversal,
     * allowing a stack-free generator to map the incoming node accurately.
     */
    public enum Context {
        /**
         * Indicates the node is the top-level root of the tree structure.
         */
        ROOT,

        /**
         * Indicates the node is an element within an ordered sequence or array.
         */
        ELEMENT,

        /**
         * Indicates the node functions as a key within a map or object structure.
         */
        ENTRY_KEY,

        /**
         * Indicates the node functions as a value associated with a key within a map or
         * object structure.
         */
        ENTRY_VALUE
    }

    // --- scalars ---

    /**
     * Adds a null value to the current context.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @throws TreeIOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void nullValue(Context context) throws TreeIOException;

    /**
     * Adds a boolean value to the tree.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @param value   the boolean value to add.
     * @throws TreeIOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void booleanValue(Context context, boolean value) throws TreeIOException;

    /**
     * Adds a string value to the tree.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @param value   the non-null string value to add.
     * @throws TreeIOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void stringValue(Context context, String value) throws TreeIOException;

    /**
     * Adds a generic numeric value by routing it to the appropriate typed method.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @param value   the non-null numeric value to add.
     * @throws TreeIOException          if an I/O error occurs during serialization
     *                                  or materialization.
     * @throws IllegalArgumentException if the underlying numeric type is not
     *                                  supported.
     * @throws IllegalStateException
     */
    default void numericValue(Context context, Number value) throws TreeIOException {
        switch (value) {
        case Short s -> numericValue(context, (long) s);
        case Integer i -> numericValue(context, (long) i);
        case Long l -> numericValue(context, l);
        case BigInteger bi -> numericValue(context, bi);

        case Float f -> numericValue(context, (double) f);
        case Double d -> numericValue(context, d);
        case BigDecimal bd -> numericValue(context, bd);

        default -> throw new IllegalArgumentException(
                """
                Unsupported numeric type=%s, value=%s"
                """.formatted(value.getClass(), value));
        }
    }

    /**
     * Adds an arbitrary-precision integer value.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @param value   the non-null BigInteger value to add.
     * @throws TreeIOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void numericValue(Context context, BigInteger value) throws TreeIOException;

    /**
     * Adds a long integer value.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @param value   the long value to add.
     * @throws TreeIOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    default void numericValue(Context context, long value) throws TreeIOException {
        numericValue(context, BigInteger.valueOf(value));
    }

    /**
     * Adds a double-precision floating-point value to the specified context.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @param value   the double value to add.
     * @throws TreeIOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    default void numericValue(Context context, double value) throws TreeIOException {
        numericValue(context, BigDecimal.valueOf(value));
    }

    /**
     * Adds an arbitrary-precision decimal value to the specified context.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @param value   the non-null BigDecimal value to add.
     * @throws TreeIOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void numericValue(Context context, BigDecimal value) throws TreeIOException;

    /**
     * Adds a binary data value to the specified context.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @param value   the non-null byte array to add.
     * @throws TreeIOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void binaryValue(Context context, byte[] value) throws TreeIOException;

    // --- structures --

    /**
     * Begins a new map (or object) structure within the given context. Every call
     * to this method must be matched by a corresponding call to
     * {@link #endMap(Context)}.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @throws TreeIOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void beginMap(Context context) throws TreeIOException;

    /**
     * Ends the current map structure. This call must close the scope opened by the
     * corresponding {@link #beginMap(Context)} call.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @throws TreeIOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void endMap(Context context) throws TreeIOException;

    /**
     * Begins a new list, array, or sequence structure within the given context.
     * Every call to this method must be matched by a corresponding call to
     * {@link #endSequence(Context)}.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @throws TreeIOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void beginSequence(Context context) throws TreeIOException;

    /**
     * Ends the current sequence structure. This call must close the scope opened by
     * the corresponding {@link #beginSequence(Context)} call.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @throws TreeIOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void endSequence(Context context) throws TreeIOException;
}
