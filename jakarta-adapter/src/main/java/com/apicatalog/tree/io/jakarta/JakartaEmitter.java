package com.apicatalog.tree.io.jakarta;

import java.io.Closeable;
import java.io.Flushable;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.Function;

import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeContext;
import com.apicatalog.tree.io.TreeEmitter;
import com.apicatalog.tree.io.TreeProcessor;
import com.apicatalog.tree.io.java.NativeTraverser;

import jakarta.json.stream.JsonGenerator;
import jakarta.json.stream.JsonGeneratorFactory;

/**
 * A specialized class that serializes any tree-like source to a JSON document
 * using the Jakarta JSON-P streaming API ({@link JsonGenerator}).
 * <p>
 * This class implements both {@link NativeTraverser} and {@link TreeEmitter},
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
public class JakartaEmitter implements TreeEmitter, TreeProcessor, Flushable, Closeable {

    protected final JsonGenerator writer;
    protected final Function<byte[], String> encoder;

    /**
     * Constructs a new writer that will output to the given {@link JsonGenerator}.
     * Binary data is not supported with this constructor.
     *
     * @param writer the Jakarta JSON generator to write to, must not be
     *               {@code null}
     */
    public JakartaEmitter(JsonGenerator writer) {
        this(writer, null);
    }

//    public static final JakartaEmitter createEmitter()

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
    public JakartaEmitter(JsonGenerator writer, Function<byte[], String> encoder) {
        this.writer = writer;
        this.encoder = encoder;
    }

    public static JakartaEmitter createEmitter(OutputStream os, JsonGeneratorFactory factory) {
        return new JakartaEmitter(factory.createGenerator(os));
    }

    @Override
    public Features features() {
        return JakartaAdapter.FEATURES;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON {@code null} literal.
     * </p>
     */
    @Override
    public void nullValue(NodeContext context) {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }
        writer.writeNull();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON boolean literal ({@code true} or {@code false}).
     * </p>
     */
    @Override
    public void booleanValue(NodeContext context, boolean node) {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }
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
    public void stringValue(NodeContext context, String node) {
        if (context == NodeContext.ENTRY_KEY) {
            writer.writeKey(node);
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
    public void numericValue(NodeContext context, long node) {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
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
    public void numericValue(NodeContext context, BigInteger node) {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
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
    public void numericValue(NodeContext context, double node) {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
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
    public void numericValue(NodeContext context, BigDecimal node) {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }
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
    public void binaryValue(NodeContext context, byte[] node) {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }
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
    public void beginMap(NodeContext context) {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }
        writer.writeStartObject();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON start-array token ({@code '['}).
     * </p>
     * 
     */
    @Override
    public void beginSequence(NodeContext context) {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }
        writer.writeStartArray();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON end token (<code>}</code> or <code>]</code>) to close the
     * current object or array context.
     * </p>
     * 
     */
    @Override
    public void endMap(NodeContext context) {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }
        writer.writeEnd();
    }

    @Override
    public void endSequence(NodeContext context) {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }
        writer.writeEnd();
    }

    @Override
    public void close() throws IOException {
        writer.close();
    }

    @Override
    public void flush() throws IOException {
        writer.flush();
    }
}
