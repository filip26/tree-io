package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Provides a uniform abstraction for reading tree-like data structures.
 * <p>
 * Implementations can support any tree representation (JSON, YAML, CBOR, etc.)
 * and any underlying library (Jackson, Gson, Jakarta, etc.).
 * <p>
 * Each adapter may support only a subset of nodes from a representation.
 * Methods may throw {@link IllegalArgumentException},
 * {@link ClassCastException}, {@link NullPointerException}, or
 * {@link IllegalStateException} if a node cannot be processed or an operation
 * fails.
 */
public interface NodeAdapter {

    // --- Features ---

    /**
     * Checks whether the given object can be processed by this adapter.
     * <p>
     * Returns true if the adapter is capable of handling the node type; false
     * otherwise.
     *
     * @param node the object to check
     * @return true if the node can be adapted by this adapter, false otherwise
     */
    boolean isNode(Object node);

    /**
     * Returns the set of node types supported by this adapter.
     *
     * @return a set of supported node types
     */
    Set<NodeType> nodeTypes();

    /**
     * Returns the set of node types that are supported as keys in map nodes for
     * this adapter.
     *
     * @return a set of supported key node types
     */
    Set<NodeType> keyTypes();

    /**
     * Returns the type of the given node.
     *
     * @param node the node to inspect
     * @return the {@link NodeType} of the node
     */
    NodeType type(Object node);

    /**
     * Checks whether the node represents a null value.
     *
     * @param node the node to check
     * @return true if the node is null, false otherwise
     */
    boolean isNull(Object node);

    /**
     * Checks whether the node represents a boolean value.
     *
     * @param node the node to check
     * @return true if the node is a boolean, false otherwise
     */
    boolean isBoolean(Object node);

    /**
     * Checks whether the node contains binary data.
     *
     * @param node the node to check
     * @return true if the node is binary, false otherwise
     */
    boolean isBinary(Object node);

    // --- Structure operations ---

    /**
     * Returns the number of entries in a map node or items in a collection node.
     * <p>
     * Intended for nodes of type {@link NodeType#MAP} or
     * {@link NodeType#COLLECTION}.
     *
     * @param node the node to inspect
     * @return number of entries (for map) or items (for collection)
     */
    int size(Object node);

    /**
     * Checks whether a map node has no entries or a collection node has no items.
     * <p>
     * This method is intended for nodes of type {@link NodeType#MAP} or
     * {@link NodeType#COLLECTION}.
     *
     * @param node the node to inspect
     * @return true if the map has no entries or the collection has no items, false
     *         otherwise
     */
    boolean isEmpty(Object node);

    // --- Map operations ---

    /**
     * Checks whether the node represents a map (object) structure.
     *
     * @param node the node to check
     * @return true if the node is a map, false otherwise
     */
    boolean isMap(Object node);

    /**
     * Returns the collection of property key nodes for a map node.
     *
     * @param node the map node
     * @return collection of property key nodes
     */
    // TODO deprecate in favor of properties?
    @Deprecated
    Collection<?> keys(Object node);

    /**
     * Returns the property value associated with the specified key node in a map
     * node.
     *
     * @param key  the property key node
     * @param node the map node containing the property
     * @return the property value node corresponding to the property key
     */
    // TODO deprecate in favor of properties?
    @Deprecated
    Object property(Object key, Object node);

    Iterable<Entry<?, ?>> entries(Object node);

    Stream<Entry<?, ?>> streamEntries(Object node);

    // --- Collection operations ---

    /**
     * Checks whether the node represents a collection.
     *
     * @param node the node to check
     * @return true if the node is a collection, false otherwise
     */
    boolean isCollection(Object node);

    /**
     * Preserves insertion order and can contain duplicates.
     * 
     * @param node
     * @return
     */
    boolean isList(Object node);

    /**
     * Contains only unique elements. Is unordered.
     * 
     * @param node
     * @return
     */
    boolean isSet(Object node);

    /**
     * Returns the child nodes of a collection node as an {@link Iterable}.
     *
     * @param node the collection node
     * @return iterable of child nodes
     */
    Iterable<?> items(Object node);

    /**
     * Returns the child nodes of a collection node as a {@link Stream}.
     *
     * @param node the collection node
     * @return stream of child nodes
     */
    Stream<?> streamItems(Object node);

    // --- String operations ---

    /**
     * Checks whether the node represents a string value.
     *
     * @param node the node to check
     * @return true if the node is a string, false otherwise
     */
    boolean isString(Object node);

    /**
     * Returns the string value of the node.
     *
     * @param node the string node
     * @return string representation
     */
    String stringValue(Object node);

    // --- Number operations ---

    /**
     * Checks whether the node represents a numeric value.
     *
     * @param node the node to check
     * @return true if the node is numeric, false otherwise
     */
    boolean isNumber(Object node);

    /**
     * Checks whether a number node represents an integral value.
     * <p>
     * Returns false for floating-point or {@link BigDecimal} nodes.
     *
     * @param node the number node
     * @return true if the node is integral, false otherwise
     */
    boolean isIntegral(Object node);

    /**
     * Returns the integer value of a number node.
     *
     * @param node the number node
     * @return integer value
     */
    int intValue(Object node);

    /**
     * Returns the long value of a number node.
     *
     * @param node the number node
     * @return long value
     */
    long longValue(Object node);

    /**
     * Returns the {@link BigInteger} value of a number node.
     *
     * @param node the number node
     * @return BigInteger value
     */
    BigInteger bigIntegerValue(Object node);

    /**
     * Returns the double value of a number node.
     *
     * @param node the number node
     * @return double value
     */
    double doubleValue(Object node);

    /**
     * Returns the {@link BigDecimal} value of a number node.
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
     * @return byte array representation
     */
    byte[] binaryValue(Object node);

    /**
     * Returns the node as an iterable.
     * <p>
     * If the node is null, returns an empty iterable. If it is a single element,
     * wraps it as a singleton iterable. Useful for iterating without type checks.
     *
     * @param node the node to convert
     * @return iterable of elements
     */
    Iterable<?> asIterable(Object node);

    /**
     * Returns the node as a stream.
     *
     * @param node the node to convert
     * @return stream of elements
     */
    Stream<?> asStream(Object node);

    /**
     * Returns the string representation of the node, converting other types as
     * needed.
     *
     * @param node the node to convert
     * @return string representation
     */
    String asString(Object node);

    BigDecimal asDecimal(Object node);
}
