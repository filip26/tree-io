package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Provides a uniform, read-only abstraction for navigating tree-like data
 * structures. This interface acts as a "wrapper" or "view" over an existing,
 * underlying tree model, decoupling processing logic from any specific data
 * format or library.
 * 
 * <p>
 * It is the conceptual counterpart to {@link NodeGenerator}. Where
 * {@code NodeGenerator} offers a "write-only" API for <em>constructing</em> a
 * tree, {@code NodeAdapter} provides a "read-only" API for <em>inspecting</em>
 * one.
 * </p>
 * <h2>Core Use Cases</h2>
 * <ol>
 * <li><b>Standalone Processing:</b> Use an adapter to traverse and extract data
 * from a native tree representation (e.g., an in-memory JSON object model)
 * using a consistent API. This keeps application logic independent of the
 * underlying data-binding library.</li>
 * <li><b>Data Transformation:</b> Serve as the input for a {@link NodeVisitor},
 * which walks the tree exposed by this adapter and drives a
 * {@link NodeGenerator}. This powerful pattern is the foundation for converting
 * between different data formats (e.g., from a YAML document to a binary CBOR
 * representation).</li>
 * </ol>
 * <p>
 * Implementations are responsible for interpreting the native node objects of a
 * specific format. Methods are expected to throw runtime exceptions such as
 * {@link ClassCastException} or {@link UnsupportedOperationException} if a node
 * is of an unexpected type or an operation is not supported for a given node.
 * </p>
 *
 * @see NodeGenerator
 * @see NodeVisitor
 * @see NodeType
 */
public interface NodeAdapter {

    // --- Adapter Capabilities & Node Introspection ---

    /**
     * Checks if the given object is a native node that this adapter can process.
     * This method is the primary entry point for determining if the adapter is
     * suitable for a given piece of data.
     *
     * @param node the object to check, which may be {@code null}.
     * @return {@code true} if the adapter can handle the object's type,
     *         {@code false} otherwise.
     */
    boolean isNode(Object node);

    /**
     * Returns the complete set of node types that this adapter is capable of
     * representing.
     *
     * @return an immutable set of supported {@link NodeType}s.
     */
    Set<NodeType> nodeTypes();

    /**
     * Returns the set of scalar types that are supported as keys in map nodes. For
     * example, a JSON-based adapter would return only {@link NodeType#STRING},
     * whereas a CBOR-based adapter might return multiple scalar types.
     *
     * @return an immutable set of supported key {@link NodeType}s.
     */
    Set<NodeType> keyTypes();

    /**
     * Determines the {@link NodeType} of the given native node.
     *
     * @param node the node to inspect, which must be a valid node for this adapter.
     * @return the {@link NodeType} corresponding to the node.
     * @throws IllegalArgumentException if the object is not a node this adapter can
     *                                  process.
     */
    NodeType type(Object node);

    /**
     * Checks if the adapted node represents a null value.
     *
     * @param node the node to check.
     * @return {@code true} if the node represents a null value, {@code false}
     *         otherwise.
     */
    boolean isNull(Object node);

    /**
     * Checks if the adapted node represents a boolean value.
     *
     * @param node the node to check.
     * @return {@code true} if the node represents a boolean value, {@code false}
     *         otherwise.
     */
    boolean isBoolean(Object node);

    /**
     * Checks if the adapted node contains binary data.
     *
     * @param node the node to check.
     * @return {@code true} if the node represents binary data, {@code false}
     *         otherwise.
     */
    boolean isBinary(Object node);

    // --- Common Structure Operations ---

    /**
     * Returns the number of entries in a map or elements in a collection. This
     * method is intended for nodes where {@link #isMap(Object)} or
     * {@link #isCollection(Object)} returns {@code true}.
     *
     * @param node the structure node to inspect.
     * @return the number of entries (for a map) or elements (for a collection).
     * @throws UnsupportedOperationException if the node is not a map or collection.
     */
    int size(Object node);

    /**
     * Checks if a map or collection node is empty.
     *
     * @param node the structure node to inspect.
     * @return {@code true} if the node contains no entries or elements,
     *         {@code false} otherwise.
     * @throws UnsupportedOperationException if the node is not a map or collection.
     */
    boolean isEmpty(Object node);

    // --- Map Operations ---

    /**
     * Checks if the adapted node represents a map (an object with key-value pairs).
     *
     * @param node the node to check.
     * @return {@code true} if the node represents a map, {@code false} otherwise.
     */
    boolean isMap(Object node);

    /**
     * Returns a collection of the native key objects from a map node. The types of
     * the keys depend on the underlying format (e.g., Strings for JSON, various
     * scalars for CBOR).
     *
     * @param node the map node to inspect.
     * @return a collection containing the map's native key objects.
     * @throws UnsupportedOperationException if the node is not a map.
     */
    Collection<?> keys(Object node);

    /**
     * Retrieves the value associated with a given native key object from a map
     * node.
     *
     * @param key  the native key object used for the lookup.
     * @param node the map node to query.
     * @return the native value node, or {@code null} if the key is not found.
     * @throws UnsupportedOperationException if the node is not a map.
     */
    Object property(Object key, Object node);

    /**
     * Returns all key-value pairs of a map node as an {@link Iterable}. The entries
     * contain the native key and value objects.
     *
     * @param node the map node to inspect.
     * @return an {@link Iterable} of its entries.
     * @throws UnsupportedOperationException if the node is not a map.
     */
    Iterable<Entry<?, ?>> entries(Object node);

    /**
     * Returns all key-value pairs of a map node as a {@link Stream}. This is a
     * convenience method for processing map entries using the Stream API.
     *
     * @param node the map node to inspect.
     * @return a {@link Stream} of its entries.
     * @throws UnsupportedOperationException if the node is not a map.
     */
    Stream<Entry<?, ?>> entryStream(Object node);

    // --- Collection Operations ---

    /**
     * Checks if the adapted node represents a collection of elements (e.g., an
     * array or list).
     *
     * @param node the node to check.
     * @return {@code true} if the node represents a collection, {@code false}
     *         otherwise.
     */
    boolean isCollection(Object node);

    /**
     * Checks if the adapted collection node is a list (an ordered collection that
     * allows duplicates).
     *
     * @param node the collection node to check.
     * @return {@code true} if the node is a list, {@code false} otherwise.
     */
    boolean isList(Object node);

    /**
     * Checks if the adapted collection node is a set (typically an unordered
     * collection of unique elements).
     *
     * @param node the collection node to check.
     * @return {@code true} if the node is a set, {@code false} otherwise.
     */
    boolean isSet(Object node);

    /**
     * Returns the elements of a collection node as an {@link Iterable}.
     *
     * @param node the collection node to inspect.
     * @return an {@link Iterable} of its native element objects.
     * @throws UnsupportedOperationException if the node is not a collection.
     */
    Iterable<?> elements(Object node);

    /**
     * Returns the elements of a collection node as a {@link Stream}.
     *
     * @param node the collection node to inspect.
     * @return a {@link Stream} of its native element objects.
     * @throws UnsupportedOperationException if the node is not a collection.
     */
    Stream<?> elementStream(Object node);

    // --- Scalar Value Accessors ---

    /**
     * Checks if the adapted node represents a string value.
     *
     * @param node the node to check.
     * @return {@code true} if the node represents a string, {@code false}
     *         otherwise.
     */
    boolean isString(Object node);

    /**
     * Extracts the string value from a string node.
     *
     * @param node the string node.
     * @return the string value.
     * @throws ClassCastException if the node is not a string.
     */
    String stringValue(Object node);

    /**
     * Checks if the adapted node represents any numeric value (integral or
     * floating-point).
     *
     * @param node the node to check.
     * @return {@code true} if the node represents a number, {@code false}
     *         otherwise.
     */
    boolean isNumber(Object node);

    /**
     * Checks if a number node represents an integral value (e.g., an integer or
     * long) as opposed to a floating-point value.
     *
     * @param node the number node to check.
     * @return {@code true} if the node's value is integral, {@code false}
     *         otherwise.
     * @throws ClassCastException if the node is not a number.
     */
    boolean isIntegral(Object node);

    /**
     * Extracts the {@code int} value from a numeric node. This may involve a
     * narrowing primitive conversion.
     *
     * @param node the numeric node.
     * @return the value as an {@code int}.
     * @throws ClassCastException if the node is not a number.
     */
    int intValue(Object node);

    /**
     * Extracts the {@code long} value from a numeric node. This may involve a
     * narrowing primitive conversion.
     *
     * @param node the numeric node.
     * @return the value as a {@code long}.
     * @throws ClassCastException if the node is not a number.
     */
    long longValue(Object node);

    /**
     * Extracts the {@link BigInteger} value from a numeric node.
     *
     * @param node the numeric node.
     * @return the value as a {@link BigInteger}.
     * @throws ClassCastException if the node is not a number.
     */
    BigInteger bigIntegerValue(Object node);

    /**
     * Extracts the {@code double} value from a numeric node.
     *
     * @param node the numeric node.
     * @return the value as a {@code double}.
     * @throws ClassCastException if the node is not a number.
     */
    double doubleValue(Object node);

    /**
     * Extracts the {@link BigDecimal} value from a numeric node.
     *
     * @param node the numeric node.
     * @return the value as a {@link BigDecimal}.
     * @throws ClassCastException if the node is not a number.
     */
    BigDecimal decimalValue(Object node);

    /**
     * Extracts the binary data from a binary node.
     *
     * @param node the binary node.
     * @return the data as a byte array.
     * @throws ClassCastException if the node is not binary.
     */
    byte[] binaryValue(Object node);

    // --- Convenience & Type Coercion Methods ---

    /**
     * Returns the node's contents as a universal {@link Iterable}. This is a
     * convenience method that enables uniform iteration logic.
     * <ul>
     * <li>If the node is a collection, returns its elements.</li>
     * <li>If the node is not a collection, wraps it in a single-element
     * iterable.</li>
     * <li>If the node is {@code null}, returns an empty iterable.</li>
     * </ul>
     *
     * @param node the node to convert.
     * @return a non-null {@link Iterable} representing the node's contents.
     */
    Iterable<?> asIterable(Object node);

    /**
     * Returns the node's contents as a universal {@link Stream}.
     *
     * @param node the node to convert.
     * @return a non-null {@link Stream} representing the node's contents.
     */
    Stream<?> asStream(Object node);

    /**
     * Returns a string representation of the node, coercing non-string scalar types
     * where applicable.
     *
     * @param node the node to convert.
     * @return a string representation of the node's value.
     */
    String asString(Object node);

    /**
     * Converts a given node to a {@link BigDecimal}, if possible. This method can
     * be used to treat both numeric and string nodes as decimal values.
     *
     * @param node the node to convert.
     * @return the {@link BigDecimal} representation.
     * @throws NumberFormatException if a string node cannot be parsed into a
     *                               BigDecimal.
     * @throws ClassCastException    if the node is neither a number nor a string.
     */
    BigDecimal asDecimal(Object node);
}
