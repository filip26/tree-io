package com.apicatalog.tree.io.jakcson;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.Deque;

import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.TreeGenerator;
import com.apicatalog.tree.io.TreeIOException;
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
public class Jackson2Generator implements TreeGenerator {

    protected final Deque<Boolean> stack;
    protected final JsonGenerator writer;

    /**
     * Constructs a new writer that will output to the given {@link JsonGenerator}.
     *
     * @param writer the Jackson JSON generator to write to, must not be
     *               {@code null}
     */
    public Jackson2Generator(JsonGenerator writer) {
        this.stack = new ArrayDeque<>();
        this.writer = writer;
    }

    @Override
    public Features features() {
        return Jackson2Adapter.FEATURES;
    }
//
//    /**
//     * The primary entry point for serialization. Traverses the given source node
//     * and writes the corresponding JSON structure to the underlying
//     * {@link JsonGenerator}.
//     *
//     * @param node    the source root node to traverse
//     * @param adapter the adapter for interpreting the source node's structure
//     * @return the underlying {@link JsonGenerator} for further use if needed
//     * @throws TreeIOException if an error occurs during writing
//     */
//    public JsonGenerator node(Object node, TreeAdapter adapter) throws TreeIOException {
//        root(node, adapter).generate(this);
//        return writer;
//    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON start-object token (<code>{</code>).
     * </p>
     * 
     * @throws TreeIOException
     */
    @Override
    public void beginMap(Context context) throws TreeIOException {
        if (context == Context.ENTRY_KEY) {
            throw new IllegalStateException();
        }
        try {
            writer.writeStartObject();
            stack.push(true);
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
    public void beginSequence(Context context) throws TreeIOException {
        if (context == Context.ENTRY_KEY) {
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
    public void endMap(Context context) throws TreeIOException {
        try {
            writer.writeEndObject();
        } catch (IOException e) {
            throw new TreeIOException(e);
        }
    }
    
    @Override
    public void endSequence(Context context) throws TreeIOException {
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
    public void nullValue() throws TreeIOException {
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
    public void booleanValue(Context context, boolean value) throws TreeIOException {
        if (context == Context.ENTRY_KEY) {
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
    public void stringValue(Context context, String value) throws TreeIOException {
        try {
            if (context == Context.ENTRY_KEY) {
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
    public void numericValue(Context context, long value) throws TreeIOException {
        try {
            if (context == Context.ENTRY_KEY) {
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
    public void numericValue(Context context, BigInteger value) throws TreeIOException {
        try {
            if (context == Context.ENTRY_KEY) {
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
    public void numericValue(Context context, double value) throws TreeIOException {
        if (context == Context.ENTRY_KEY) {
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
    public void numericValue(Context context, BigDecimal value) throws TreeIOException {
        try {
            if (context == Context.ENTRY_KEY) {
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
    public void binaryValue(Context context, byte[] value) throws TreeIOException {
        throw new UnsupportedOperationException("JSON does not support a native binary type.");
    }
}
