package com.apicatalog.tree.io;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.function.Function;

import jakarta.json.stream.JsonGenerator;

/**
 * A specialized class that serializes any tree-like source to a JSON document
 * using the Jakarta JSON-P streaming API ({@link JsonGenerator}).
 * <p>
 * This class implements both {@link DepthFirstTraversal} and {@link NodeGenerator},
 * enabling it to function as a self-contained serialization engine. It
 * traverses a source structure (via its {@code NodeVisitor} parent) and
 * consumes its own traversal events (via its {@code NodeGenerator}
 * implementation) to write directly to the provided {@code JsonGenerator}.
 * </p>
 * <p>
 * This class is stateful and intended for a single serialization task, as it
 * operates on a forward-only stream writer. Since standard JSON does not
 * support a native binary type, binary data can only be processed if an encoder
 * (e.g., for Base64) is supplied during construction.
 * </p>
 */
public class JakartaWriter extends DepthFirstTraversal implements NodeGenerator {

    protected final JsonGenerator writer;
    protected final Function<byte[], String> encoder;

    /**
     * Constructs a new writer that will output to the given {@link JsonGenerator}.
     * Binary data is not supported with this constructor.
     *
     * @param writer the Jakarta JSON generator to write to, must not be
     *               {@code null}
     */
    public JakartaWriter(JsonGenerator writer) {
        this(writer, null);
    }

    /**
     * Constructs a new writer that will output to the given {@link JsonGenerator}
     * with custom handling for binary data.
     *
     * @param writer  the Jakarta JSON generator to write to, must not be
     *                {@code null}
     * @param encoder a function to convert byte arrays into a {@link String}
     *                representation (e.g., Base64); if {@code null}, binary data is
     *                not supported
     */
    public JakartaWriter(JsonGenerator writer, Function<byte[], String> encoder) {
        super(new ArrayDeque<>(), null);
        this.writer = writer;
        this.encoder = encoder;
    }

    /**
     * The primary entry point for serialization. Traverses the given source node
     * and writes the corresponding JSON structure to the underlying
     * {@link JsonGenerator}.
     *
     * @param node    the source root node to traverse
     * @param adapter the adapter for interpreting the source node's structure
     * @return the underlying {@link JsonGenerator} for further use if needed
     * @throws IOException if an error occurs during writing
     */
    public JsonGenerator node(Object node, NodeAdapter adapter) throws IOException {
        root(node, adapter).traverse(this);
        return writer;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON {@code null} literal.
     * </p>
     */
    @Override
    public void nullValue() throws IOException {
        writer.writeNull();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON boolean literal ({@code true} or {@code false}).
     * </p>
     */
    @Override
    public void booleanValue(boolean node) throws IOException {
        writer.write(node);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON object key if the current context is a {@code PROPERTY_KEY},
     * otherwise writes a JSON string value.
     * </p>
     */
    @Override
    public void stringValue(String node) throws IOException {
        if (currentNodeContext == Context.PROPERTY_KEY) {
            writer.writeKey(adapter().asString(node));
            return;
        }
        writer.write(node);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(long node) throws IOException {
        writer.write(node);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(BigInteger node) throws IOException {
        writer.write(node);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(double node) throws IOException {
        writer.write(node);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(BigDecimal node) throws IOException {
        writer.write(node);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Encodes the byte array as a string using the provided encoder function and
     * writes it as a JSON string.
     * </p>
     * 
     * @throws UnsupportedOperationException if no encoder was provided during
     *                                       construction
     */
    @Override
    public void binaryValue(byte[] node) throws IOException {
        if (encoder == null) {
            throw new UnsupportedOperationException("Binary values are not supported without a configured encoder.");
        }
        writer.write(encoder.apply(node));
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON start-object token (<code>{</code>).
     * </p>
     */
    @Override
    public void beginMap() throws IOException {
        writer.writeStartObject();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON start-array token ({@code '['}).
     * </p>
     */
    @Override
    public void beginList() throws IOException {
        writer.writeStartArray();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void beginSet() throws IOException {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON end token (<code>}</code> or <code>]</code>) to close the
     * current object or array context.
     * </p>
     */
    @Override
    public void end() throws IOException {
        writer.writeEnd();
    }
}
