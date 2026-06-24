package com.apicatalog.tree.io;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.Function;

import com.apicatalog.tree.io.Tree.Event;
import com.apicatalog.tree.io.Tree.NodeContext;

/**
 * Provides a uniform, performant, event-based abstraction for generating
 * tree-like data structures. This interface decouples the process of describing
 * a tree from its final representation, making it suitable for both
 * serialization and materialization.
 * <p>
 * This interface is explicitly designed to enable completely stateless,
 * stack-free generator implementations. To eliminate internal state tracking or
 * stacks within the generator, the responsibility of tracking the structural
 * hierarchy is shifted entirely to the caller. The {@link NodeContext} passed
 * into each method originates from the processing or traversal of a source tree
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
public interface TreeEmitter /*extends Flushable, Closeable*/ {

    default boolean accept(Event event, TreeCursor cursor) throws IOException {
        switch (event) {
        case BEGIN_MAP:
            beginMap(cursor.context());
            return true;

        case END_MAP:
            endMap(cursor.context());
            return true;

        case BEGIN_SEQUENCE:
            beginSequence(cursor.context());
            return true;

        case END_SEQUENCE:
            endSequence(cursor.context());
            return true;

        case SCALAR:
            switch (cursor.nodeType()) {
            case NULL -> nullValue(cursor.context());
            case TRUE -> booleanValue(cursor.context(), true);
            case FALSE -> booleanValue(cursor.context(), false);
            case STRING -> stringValue(cursor.context(), cursor.stringValue());
            case NUMBER -> numberValue(cursor.context(), cursor.numberValue());
            case BINARY -> binaryValue(cursor.context(), cursor.binaryValue());

            default -> throw new IllegalArgumentException(
                    """
                    Unexpected node type=%s"
                    """.formatted(cursor.nodeType()));
            }
            return true;

        case null:
            throw new IllegalArgumentException();
        }
    }

    
    // --- scalars ---

    /**
     * Adds a null value to the current context.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @throws IOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void nullValue(NodeContext context) throws IOException;

    /**
     * Adds a boolean value to the tree.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @param value   the boolean value to add.
     * @throws IOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void booleanValue(NodeContext context, boolean value) throws IOException;

    /**
     * Adds a string value to the tree.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @param value   the non-null string value to add.
     * @throws IOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void stringValue(NodeContext context, String value) throws IOException;

    /**
     * Adds a generic numeric value by routing it to the appropriate typed method.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @param value   the non-null numeric value to add.
     * @throws IOException          if an I/O error occurs during serialization
     *                                  or materialization.
     * @throws IllegalArgumentException if the underlying numeric type is not
     *                                  supported.
     * @throws IllegalStateException
     */
    default void numberValue(NodeContext context, Number value) throws IOException {
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
     * @throws IOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void numericValue(NodeContext context, BigInteger value) throws IOException;

    /**
     * Adds a long integer value.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @param value   the long value to add.
     * @throws IOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    default void numericValue(NodeContext context, long value) throws IOException {
        numericValue(context, BigInteger.valueOf(value));
    }

    /**
     * Adds a double-precision floating-point value to the specified context.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @param value   the double value to add.
     * @throws IOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    default void numericValue(NodeContext context, double value) throws IOException {
        numericValue(context, BigDecimal.valueOf(value));
    }

    /**
     * Adds an arbitrary-precision decimal value to the specified context.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @param value   the non-null BigDecimal value to add.
     * @throws IOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void numericValue(NodeContext context, BigDecimal value) throws IOException;

    /**
     * Adds a binary data value to the specified context.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @param value   the non-null byte array to add.
     * @throws IOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void binaryValue(NodeContext context, byte[] value) throws IOException;

    // --- structures --

    /**
     * Begins a new map (or object) structure within the given context. Every call
     * to this method must be matched by a corresponding call to
     * {@link #endMap(NodeContext)}.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @throws IOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void beginMap(NodeContext context) throws IOException;

    /**
     * Ends the current map structure. This call must close the scope opened by the
     * corresponding {@link #beginMap(NodeContext)} call.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @throws IOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void endMap(NodeContext context) throws IOException;

    /**
     * Begins a new list, array, or sequence structure within the given context.
     * Every call to this method must be matched by a corresponding call to
     * {@link #endSequence(NodeContext)}.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @throws IOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void beginSequence(NodeContext context) throws IOException;

    /**
     * Ends the current sequence structure. This call must close the scope opened by
     * the corresponding {@link #beginSequence(NodeContext)} call.
     *
     * @param context the structural context originating from the source tree
     *                traversal.
     * @throws IOException       if an I/O error occurs during serialization or
     *                               materialization.
     * @throws IllegalStateException
     */
    void endSequence(NodeContext context) throws IOException;

    default void nullEntry(String key) throws IOException {
        stringValue(NodeContext.ENTRY_KEY, key);
        nullValue(NodeContext.ENTRY_VALUE);
    }

    default void entry(String key, boolean value) throws IOException {
        stringValue(NodeContext.ENTRY_KEY, key);
        booleanValue(NodeContext.ENTRY_VALUE, value);
    }

    default void entry(String key, long value) throws IOException {
        stringValue(NodeContext.ENTRY_KEY, key);
        numericValue(NodeContext.ENTRY_VALUE, value);
    }

    default void entry(String key, int value) throws IOException {
        stringValue(NodeContext.ENTRY_KEY, key);
        numericValue(NodeContext.ENTRY_VALUE, value);
    }

    default void entry(String key, double value) throws IOException {
        stringValue(NodeContext.ENTRY_KEY, key);
        numericValue(NodeContext.ENTRY_VALUE, value);
    }
    
    default void entry(String key, BigDecimal value) throws IOException {
        stringValue(NodeContext.ENTRY_KEY, key);
        numericValue(NodeContext.ENTRY_VALUE, value);
    }

    default void entry(String key, BigInteger value) throws IOException {
        stringValue(NodeContext.ENTRY_KEY, key);
        numericValue(NodeContext.ENTRY_VALUE, value);
    }

    default void entry(String key, String value) throws IOException {
        if (value != null) {
            stringValue(NodeContext.ENTRY_KEY, key);
            stringValue(NodeContext.ENTRY_VALUE, value);
        }
    }

    default <T> void entry(String key, T object, Function<T, String> map) throws IOException {
        if (object != null) {
            stringValue(NodeContext.ENTRY_KEY, key);
            stringValue(NodeContext.ENTRY_VALUE, map.apply(object));
        }
    }
}
