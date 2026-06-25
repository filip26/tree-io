package com.apicatalog.tree.io.jakcson;

import java.io.Closeable;
import java.io.Flushable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.math.BigInteger;

import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeContext;
import com.apicatalog.tree.io.TreeEmitter;
import com.apicatalog.tree.io.TreeProcessor;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;

/**
 * A specialized class that serializes any tree-like source to a JSON document
 * using the Jackson 2 streaming API ({@link JsonGenerator}).
 * <p>
 * This class implements {@link TreeEmitter}, enabling it to function as a
 * self-contained serialization engine. to write directly to the provided
 * {@code JsonGenerator}.
 * </p>
 * <p>
 * This class is stateful and intended for a single serialization task, as it
 * operates on a forward-only stream writer.
 * </p>
 */
public final class Jackson2Emitter implements TreeEmitter, TreeProcessor, Flushable, Closeable {

    private final JsonGenerator generator;

    /**
     * Constructs a new writer that will output to the given {@link JsonGenerator}.
     *
     * @param writer the Jackson JSON generator to write to, must not be
     *               {@code null}
     */
    public Jackson2Emitter(JsonGenerator writer) {
        this.generator = writer;
    }

    public static Jackson2Emitter createEmitter(OutputStream os, JsonFactory factory) throws IOException {
        return new Jackson2Emitter(factory.createGenerator(os));
    }

    @Override
    public Features features() {
        return Jackson2Adapter.FEATURES;
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
        try {
            generator.writeStartObject();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON start-array token ({@code '['}).
     * </p>
     */
    @Override
    public void beginSequence(NodeContext context) {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }
        try {
            generator.writeStartArray();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }

    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON end-object ({@code '}'}) or end-array ({@code ']'}) token based
     * on the current traversal context.
     * </p>
     */
    @Override
    public void endMap(NodeContext context) {
        try {
            generator.writeEndObject();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }

    }

    @Override
    public void endSequence(NodeContext context) {
        try {
            generator.writeEndArray();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }

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
        try {
            generator.writeNull();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON boolean literal ({@code true} or {@code false}).
     * </p>
     */
    @Override
    public void booleanValue(NodeContext context, boolean value) {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }
        try {
            generator.writeBoolean(value);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON field name if the current context is a {@code PROPERTY_KEY},
     * otherwise writes a JSON string value.
     * </p>
     */
    @Override
    public void stringValue(NodeContext context, String value) {
        try {
            if (context == NodeContext.ENTRY_KEY) {
                generator.writeFieldName(value);
                return;
            }
            generator.writeString(value);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(NodeContext context, long value) {
        try {
            if (context == NodeContext.ENTRY_KEY) {
                generator.writeFieldId(value);
                return;
            }
            generator.writeNumber(value);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(NodeContext context, BigInteger value) {
        try {
            if (context == NodeContext.ENTRY_KEY) {
                generator.writeFieldId(value.longValueExact());
                return;
            }
            generator.writeNumber(value);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }

    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(NodeContext context, double value) {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }
        try {
            generator.writeNumber(value);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(NodeContext context, BigDecimal value) {
        try {
            if (context == NodeContext.ENTRY_KEY) {
                generator.writeFieldId(value.longValueExact());
                return;
            }
            generator.writeNumber(value);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * This operation is not supported, as standard JSON does not have a native
     * binary type.
     * </p>
     * 
     * @throws UnsupportedOperationException always
     */
    @Override
    public void binaryValue(NodeContext context, byte[] value) {
        throw new UnsupportedOperationException("JSON does not support a native binary type.");
    }

    @Override
    public void close() throws IOException {
        generator.close();
    }

    @Override
    public void flush() throws IOException {
        generator.flush();
    }
}
