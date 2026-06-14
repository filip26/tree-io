package com.apicatalog.tree.io.jakcson;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;

import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeContext;
import com.apicatalog.tree.io.TreeGenerator;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeProcessor;
import com.fasterxml.jackson.core.JsonGenerator;

/**
 * A specialized class that serializes any tree-like source to a JSON document
 * using the Jackson 2 streaming API ({@link JsonGenerator}).
 * <p>
 * This class implements {@link TreeGenerator}, enabling it to function as a
 * self-contained serialization engine. to write directly to the provided
 * {@code JsonGenerator}.
 * </p>
 * <p>
 * This class is stateful and intended for a single serialization task, as it
 * operates on a forward-only stream writer.
 * </p>
 */
public final class Jackson2Generator implements TreeGenerator, TreeProcessor {

    private final JsonGenerator writer;

    /**
     * Constructs a new writer that will output to the given {@link JsonGenerator}.
     *
     * @param writer the Jackson JSON generator to write to, must not be
     *               {@code null}
     */
    public Jackson2Generator(JsonGenerator writer) {
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
     * @throws TreeIOException
     */
    @Override
    public void beginMap(NodeContext context) throws TreeIOException {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }
        try {
            writer.writeStartObject();
        } catch (IOException e) {
            throw new TreeIOException(e);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON start-array token ({@code '['}).
     * </p>
     */
    @Override
    public void beginSequence(NodeContext context) throws TreeIOException {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }        
        try {
            writer.writeStartArray();
        } catch (IOException e) {
            throw new TreeIOException(e);
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
    public void endMap(NodeContext context) throws TreeIOException {
        try {
            writer.writeEndObject();
        } catch (IOException e) {
            throw new TreeIOException(e);
        }
    }
    
    @Override
    public void endSequence(NodeContext context) throws TreeIOException {
        try {
            writer.writeEndArray();
        } catch (IOException e) {
            throw new TreeIOException(e);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON {@code null} literal.
     * </p>
     */
    @Override
    public void nullValue(NodeContext context) throws TreeIOException {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }
        try {
            writer.writeNull();
        } catch (IOException e) {
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
    public void booleanValue(NodeContext context, boolean value) throws TreeIOException {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }
        try {
            writer.writeBoolean(value);
        } catch (IOException e) {
            throw new TreeIOException(e);
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
    public void stringValue(NodeContext context, String value) throws TreeIOException {
        try {
            if (context == NodeContext.ENTRY_KEY) {
                writer.writeFieldName(value);
                return;
            }
            writer.writeString(value);
        } catch (IOException e) {
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
    public void numericValue(NodeContext context, long value) throws TreeIOException {
        try {
            if (context == NodeContext.ENTRY_KEY) {
                writer.writeFieldId(value);
                return;
            }
            writer.writeNumber(value);
        } catch (IOException e) {
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
    public void numericValue(NodeContext context, BigInteger value) throws TreeIOException {
        try {
            if (context == NodeContext.ENTRY_KEY) {
                writer.writeFieldId(value.longValueExact());
                return;
            }
            writer.writeNumber(value);
        } catch (IOException e) {
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
    public void numericValue(NodeContext context, double value) throws TreeIOException {
        if (context == NodeContext.ENTRY_KEY) {
            throw new IllegalStateException();
        }
        try {
            writer.writeNumber(value);
        } catch (IOException e) {
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
    public void numericValue(NodeContext context, BigDecimal value) throws TreeIOException {
        try {
            if (context == NodeContext.ENTRY_KEY) {
                writer.writeFieldId(value.longValueExact());
                return;
            }

            writer.writeNumber(value);
        } catch (IOException e) {
            throw new TreeIOException(e);
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
    public void binaryValue(NodeContext context, byte[] value) throws TreeIOException {
        throw new UnsupportedOperationException("JSON does not support a native binary type.");
    }
}
