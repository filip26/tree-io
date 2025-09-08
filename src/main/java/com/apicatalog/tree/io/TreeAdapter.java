package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;

/**
 * Interface providing a uniform abstraction for reading tree-like data
 * structures.
 * <p>
 * Implementations can work with any tree representation (JSON, YAML, CBOR,
 * etc.) and any underlying library (Jackson, Gson, Jakarta, etc.). Methods
 * throw {@link IllegalArgumentException} if the node is not of the expected
 * type.
 */
public interface TreeAdapter {

    /**
     * Supported node types in a tree structure.
     */
    enum NodeType {
        Map,
        Collection,
        String,
        Number,
        Binary,
        True,
        False,
        Null,
    }

    /**
     * Returns the type of the given node.
     *
     * @param node the node to inspect
     * @return the {@link NodeType} of the node
     * @throws IllegalArgumentException if the node is null or not recognized
     */
    NodeType getNodeType(Object node);

    // --- Map operations ---

    /**
     * Returns the collection of property values for a map node.
     *
     * @param node the map node
     * @return collection of child nodes
     * @throws IllegalArgumentException if the node is not of type Map
     */
    Collection<Object> properties(Object node);

    /**
     * Returns the value of a property from a map node.
     *
     * @param property the property key
     * @param node     the map node containing the property
     * @return the child node associated with the property key
     * @throws IllegalArgumentException if the node is not of type Map
     */
    Object node(Object property, Object node);

    // --- Collection operations ---

    /**
     * Returns the collection of items for a collection node.
     *
     * @param node the collection node
     * @return collection of child nodes
     * @throws IllegalArgumentException if the node is not of type Collection
     */
    Collection<Object> items(Object node);

    // --- String operations ---

    /**
     * Returns the string value of the node.
     *
     * @param node the string node
     * @return string representation
     * @throws IllegalArgumentException if the node is not of type String
     */
    String asString(Object node);

    // --- Number operations ---

    /**
     * Returns true if the number node is a decimal type (floating point or
     * BigDecimal).
     *
     * @param node the number node
     * @return true if node is decimal
     * @throws IllegalArgumentException if the node is not of type Number
     */
    boolean isDecimal(Object node);

    /**
     * Returns the integer value of the number node.
     *
     * @param node the number node
     * @return integer value
     * @throws IllegalArgumentException if the node is not a number
     *                                  or cannot be converted to Integer
     */
    Integer asInteger(Object node);

    /**
     * Returns the long value of the number node.
     *
     * @param node the number node
     * @return long value
     * @throws IllegalArgumentException if the node is not a number
     *                                  or cannot be converted to Long
     */
    Long asLong(Object node);

    /**
     * Returns the BigInteger value of the number node.
     *
     * @param node the number node
     * @return BigInteger value
     * @throws IllegalArgumentException if the node is not a number
     *                                  or cannot be converted to BigInteger
     */
    BigInteger asBigInteger(Object node);

    /**
     * Returns the double value of the number node.
     *
     * @param node the number node
     * @return double value
     * @throws IllegalArgumentException if the node is not a number
     */
    Double asDouble(Object node);

    /**
     * Returns the BigDecimal value of the number node.
     *
     * @param node the number node
     * @return BigDecimal value
     * @throws IllegalArgumentException if the node is not a number
     */
    BigDecimal asBigDecimal(Object node);

    // --- Binary operations ---

    /**
     * Returns the binary value of the node as a byte array.
     *
     * @param node the binary node
     * @return byte array content
     * @throws IllegalArgumentException if the node is not of type Binary
     */
    byte[] asByteArray(Object node);
}
