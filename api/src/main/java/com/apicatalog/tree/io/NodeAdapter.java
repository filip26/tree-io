package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.stream.Stream;

/**
 * Interface providing a uniform abstraction for reading tree-like data
 * structures.
 * <p>
 * Implementations can work with any tree representation (JSON, YAML, CBOR,
 * etc.) and any underlying library (Jackson, Gson, Jakarta, etc.). Methods
 * throw {@link IllegalArgumentException}, {@link ClassCastException},
 * {@link NullPointerException}, {@link IllegalStateException} if the node is
 * not of the expected type or processing error.
 */
public interface NodeAdapter {

    static NodeAdapter withNativeTypes(NodeAdapter adapter) {
        return new NativeTypeAdapter(adapter);
    }

    /**
     * Returns the type of the given node.
     *
     * @param node the node to inspect
     * @return the {@link NodeType} of the node
     */
    NodeType typeOf(Object node);

    boolean isNull(Object node);

    boolean isBoolean(Object node);

    // --- Structure operations ---
    int size(Object node);

    boolean isEmpty(Object node);

    // --- Map operations ---
    boolean isMap(Object node);

    /**
     * Returns the collection of property names for a map node.
     *
     * @param node the map node
     * @return collection of property names
     */
    Collection<? extends Object> properties(Object node);

    /**
     * Returns the value of a property from a map node.
     *
     * @param property the property key
     * @param node     the map node containing the property
     * @return the child node associated with the property key
     */
    Object property(Object property, Object node);

    // --- Collection operations ---
    boolean isCollection(Object node);

    /**
     * Returns the collection of items for a collection node.
     *
     * @param node the collection node
     * @return collection of child nodes
     */
    Iterable<? extends Object> iterable(Object node);
    
    Stream<? extends Object> stream(Object node);

    // --- String operations ---
    boolean isString(Object node);

    /**
     * Returns the string value of the node.
     *
     * @param node the string node
     * @return string representation
     */
    String stringValue(Object node);

    // --- Number operations ---
    boolean isNumber(Object node);

    /**
     * Returns false if the number node is a decimal type (floating point or
     * BigDecimal).
     *
     * @param node the number node
     * @return true if node is integral value
     */
    boolean isIntegral(Object node);

    /**
     * Returns the integer value of the number node.
     *
     * @param node the number node
     * @return integer value
     */
    int intValue(Object node);

    /**
     * Returns the long value of the number node.
     *
     * @param node the number node
     * @return long value
     */
    long longValue(Object node);

    /**
     * Returns the BigInteger value of the number node.
     *
     * @param node the number node
     * @return BigInteger value
     */
    BigInteger bigIntegerValue(Object node);

    /**
     * Returns the double value of the number node.
     *
     * @param node the number node
     * @return double value
     */
    double doubleValue(Object node);

    /**
     * Returns the BigDecimal value of the number node.
     *
     * @param node the number node
     * @return BigDecimal value
     */
    BigDecimal decimalValue(Object node);

    // --- Binary operations ---

    /**
     * Returns the binary value of the node as a byte array.
     *
     * @param node the binary node
     * @return byte array content
     */
    byte[] binaryValue(Object node);

    /**
     * If the node is a collection then returns the node, converts the node into an
     * empty collection in a case of null or single item collection.
     * 
     * Allows to iterate over elements without checking a node type.
     * 
     * @param node
     * @return
     */
    Iterable<? extends Object> asIterable(Object node);

    Stream<? extends Object> asStream(Object node);
}
