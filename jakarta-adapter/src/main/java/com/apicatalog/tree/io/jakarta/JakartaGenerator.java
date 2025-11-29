package com.apicatalog.tree.io.jakarta;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.function.Function;

import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeGenerator;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeTraversal;

import jakarta.json.JsonException;
import jakarta.json.stream.JsonGenerator;

/**
 * A specialized class that serializes any tree-like source to a JSON document
 * using the Jakarta JSON-P streaming API ({@link JsonGenerator}).
 * <p>
 * This class implements both {@link TreeTraversal} and {@link TreeGenerator},
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
public class JakartaGenerator extends TreeTraversal implements TreeGenerator {

    protected final JsonGenerator writer;
    protected final Function<byte[], String> encoder;

    /**
     * Constructs a new writer that will output to the given {@link JsonGenerator}.
     * Binary data is not supported with this constructor.
     *
     * @param writer the Jakarta JSON generator to write to, must not be
     *               {@code null}
     */
    public JakartaGenerator(JsonGenerator writer) {
        this(writer, null);
    }

    @Override
    public Features features() {
        return JakartaAdapter.FEATURES;
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
    public JakartaGenerator(JsonGenerator writer, Function<byte[], String> encoder) {
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
     * @throws TreeIOException if an error occurs during writing
     */
    public JsonGenerator node(Object node, TreeAdapter adapter) throws TreeIOException {
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
    public void nullValue() throws TreeIOException {
        try {
            writer.writeNull();
        } catch (JsonException e) {
            throw new TreeIOException(e);
        }

    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON boolean literal ({@code true} or {@code false}).
     * </p>
     */
    @Override
    public void booleanValue(boolean node) throws TreeIOException {
        try {
            writer.write(node);
        } catch (JsonException e) {
            throw new TreeIOException(e);
        }

    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON object key if the current context is a {@code PROPERTY_KEY},
     * otherwise writes a JSON string value.
     * </p>
     */
    @Override
    public void stringValue(String node) throws TreeIOException {
        try {
            if (currentNodeContext == Context.PROPERTY_KEY) {
                writer.writeKey(node);
                return;
            }
            writer.write(node);
        } catch (JsonException e) {
            throw new TreeIOException(e);
        }

    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(long node) throws TreeIOException {
        try {
            writer.write(node);
        } catch (JsonException e) {
            throw new TreeIOException(e);
        }

    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(BigInteger node) throws TreeIOException {
        try {
            writer.write(node);
        } catch (JsonException e) {
            throw new TreeIOException(e);
        }

    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(double node) throws TreeIOException {
        try {
            writer.write(node);
        } catch (JsonException e) {
            throw new TreeIOException(e);
        }

    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(BigDecimal node) throws TreeIOException {
        try {
            writer.write(node);
        } catch (JsonException e) {
            throw new TreeIOException(e);
        }
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
    public void binaryValue(byte[] node) throws TreeIOException {
        if (encoder == null) {
            throw new UnsupportedOperationException("Binary values are not supported without a configured encoder.");
        }
        try {
            writer.write(encoder.apply(node));
        } catch (JsonException e) {
            throw new TreeIOException(e);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON start-object token (<code>{</code>).
     * </p>
     */
    @Override
    public void beginMap() throws TreeIOException {
        try {
            writer.writeStartObject();
        } catch (JsonException e) {
            throw new TreeIOException(e);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON start-array token ({@code '['}).
     * </p>
     * @throws TreeIOException 
     */
    @Override
    public void beginList() throws TreeIOException {
        try {
            writer.writeStartArray();
        } catch (JsonException e) {
            throw new TreeIOException(e);
        }
    }

    /**
     * {@inheritDoc}
     * @throws TreeIOException 
     */
    @Override
    public void beginSet() throws TreeIOException {
        try {
            writer.writeStartArray();
        } catch (JsonException e) {
            throw new TreeIOException(e);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON end token (<code>}</code> or <code>]</code>) to close the
     * current object or array context.
     * </p>
     * 
     * @throws TreeIOException
     */
    @Override
    public void end() throws TreeIOException {
        try {
            writer.writeEnd();
        } catch (JsonException e) {
            throw new TreeIOException(e);
        }
    }
}
