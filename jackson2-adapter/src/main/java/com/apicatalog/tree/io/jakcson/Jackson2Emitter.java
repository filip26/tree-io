package com.apicatalog.tree.io.jakcson;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;

import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeContext;
import com.apicatalog.tree.io.TreeEmitter;
import com.apicatalog.tree.io.TreeProcessor;
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
public final class Jackson2Emitter implements TreeEmitter, TreeProcessor {

    private final JsonGenerator writer;

    /**
     * Constructs a new writer that will output to the given {@link JsonGenerator}.
     *
     * @param writer the Jackson JSON generator to write to, must not be
     *               {@code null}
     */
    public Jackson2Emitter(JsonGenerator writer) {
        this.writer = writer;
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
     * 
     * @throws IOException
     */
    @Override
    public void beginMap(NodeContext context) throws IOException {
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
     */
    @Override
    public void beginSequence(NodeContext context) throws IOException {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }        
            writer.writeStartArray();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON end-object ({@code '}'}) or end-array ({@code ']'}) token based
     * on the current traversal context.
     * </p>
     */
    @Override
    public void endMap(NodeContext context) throws IOException {
            writer.writeEndObject();
    }
    
    @Override
    public void endSequence(NodeContext context) throws IOException {
            writer.writeEndArray();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON {@code null} literal.
     * </p>
     */
    @Override
    public void nullValue(NodeContext context) throws IOException {
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
    public void booleanValue(NodeContext context, boolean value) throws IOException {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }
            writer.writeBoolean(value);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON field name if the current context is a {@code PROPERTY_KEY},
     * otherwise writes a JSON string value.
     * </p>
     */
    @Override
    public void stringValue(NodeContext context, String value) throws IOException {
            if (context == NodeContext.ENTRY_KEY) {
                writer.writeFieldName(value);
                return;
            }
            writer.writeString(value);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(NodeContext context, long value) throws IOException {
            if (context == NodeContext.ENTRY_KEY) {
                writer.writeFieldId(value);
                return;
            }
            writer.writeNumber(value);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(NodeContext context, BigInteger value) throws IOException {
            if (context == NodeContext.ENTRY_KEY) {
                writer.writeFieldId(value.longValueExact());
                return;
            }
            writer.writeNumber(value);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(NodeContext context, double value) throws IOException {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }
            writer.writeNumber(value);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(NodeContext context, BigDecimal value) throws IOException {
            if (context == NodeContext.ENTRY_KEY) {
                writer.writeFieldId(value.longValueExact());
                return;
            }
            writer.writeNumber(value);
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
}
