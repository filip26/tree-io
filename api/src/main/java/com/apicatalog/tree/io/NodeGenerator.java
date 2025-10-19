package com.apicatalog.tree.io;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;

import com.apicatalog.tree.io.traverse.Visitor;

/**
 * Provides a uniform, event-based abstraction for constructing tree-like data
 * structures. This interface decouples the process of describing a tree from
 * its final representation.
 * <p>
 * It is the conceptual counterpart to {@link NodeAdapter}, which reads tree
 * structures.
 * </p>
 * <p>
 * While this interface can be implemented and driven manually, it is often used
 * as a target for a {@link Visitor}. The visitor traverses a source tree
 * (read via a {@link NodeAdapter}) and calls the appropriate methods on this
 * generator, effectively enabling powerful tree transformation and conversion
 * workflows.
 * </p>
 * <p>
 * Implementations can use this sequence of construction events for two main
 * purposes:
 * </p>
 * <ol>
 * <li><b>Serialization:</b> Writing the structure directly to an output stream
 * in a specific format (e.g., text-based JSON, XML, YAML, or binary formats
 * like CBOR).</li>
 * <li><b>Materialization:</b> Building a concrete, in-memory object model or
 * document tree.</li>
 * </ol>
 * <p>
 * This approach is analogous to streaming parsers like SAX or StAX, but for
 * generation rather than parsing. The generator is stateful and forward-only,
 * expecting methods to be called in a valid sequence.
 * </p>
 */
public interface NodeGenerator {

    Features features();
    
    // --- scalars ---

    /**
     * Adds a null value to the current context.
     *
     * @throws IOException if an I/O error occurs during construction.
     */
    void nullValue() throws IOException;

    /**
     * Adds a boolean value to the current context.
     *
     * @param value the boolean value to add.
     * @throws IOException if an I/O error occurs during construction.
     */
    void booleanValue(boolean value) throws IOException;

    /**
     * Adds a string value. Depending on the context, this could represent a map
     * key, a value associated with a key, or an element in a collection.
     *
     * @param value the non-null string value to add.
     * @throws IOException if an I/O error occurs during construction.
     */
    void stringValue(String value) throws IOException;

    /**
     * Adds a long integer value. Depending on the context, this could represent a
     * map key (in formats like CBOR), a value, or a collection element.
     *
     * @param value the long value to add.
     * @throws IOException if an I/O error occurs during construction.
     */
    void numericValue(long value) throws IOException;

    /**
     * Adds an arbitrary-precision integer value. Depending on the context, this
     * could represent a map key, a value, or a collection element.
     *
     * @param value the non-null BigInteger value to add.
     * @throws IOException if an I/O error occurs during construction.
     */
    void numericValue(BigInteger value) throws IOException;

    /**
     * Adds a double-precision floating-point value to the current context.
     *
     * @param value the double value to add.
     * @throws IOException if an I/O error occurs during construction.
     */
    void numericValue(double value) throws IOException;

    /**
     * Adds an arbitrary-precision decimal value to the current context.
     *
     * @param value the non-null BigDecimal value to add.
     * @throws IOException if an I/O error occurs during construction.
     */
    void numericValue(BigDecimal value) throws IOException;

    /**
     * Adds a binary data value to the current context. The specific encoding (e.g.,
     * Base64 for JSON, or direct bytes for CBOR) is determined by the underlying
     * implementation.
     *
     * @param value the non-null byte array to add.
     * @throws IOException if an I/O error occurs during construction.
     */
    void binaryValue(byte[] value) throws IOException;

    // --- structures --

    /**
     * Begins a new map (or object) structure. After this call, the generator
     * expects a sequence of key-value pairs.
     * <p>
     * The type of a key is determined by the target format. For formats like JSON,
     * keys are strings added via {@link #stringValue(String)}. For others, like
     * CBOR, keys can also be other types.
     * </p>
     * Every call to this method must be matched by a corresponding call to
     * {@link #end()}.
     *
     * @throws IOException if an I/O error occurs during construction.
     */
    void beginMap() throws IOException;

    /**
     * Begins a new list (or array) structure. An ordered collection that allows
     * duplicates. After this call, subsequent calls are expected to be the elements
     * of the list. Every call to this method must be matched by a corresponding
     * call to {@link #end()}.
     *
     * @throws IOException if an I/O error occurs during construction.
     */
    void beginList() throws IOException;

    /**
     * Begins a new set structure. An unordered collection of unique elements. After
     * this call, subsequent calls are expected to be the elements of the set. Every
     * call to this method must be matched by a corresponding call to
     * {@link #end()}.
     *
     * @throws IOException if an I/O error occurs during construction.
     */
    void beginSet() throws IOException;

    /**
     * Ends the current map or collection structure. This call must match a
     * preceding {@link #beginMap()} or {@link #beginList()}.
     *
     * @throws IOException if an I/O error occurs during construction.
     */
    void end() throws IOException;
}
