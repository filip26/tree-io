package com.apicatalog.tree.io.jakcson;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayDeque;

import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeGenerator;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeType;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeTraversal;
import com.fasterxml.jackson.core.JsonGenerator;

/**
 * A specialized class that serializes any tree-like source to a JSON document
 * using the Jackson 2 streaming API ({@link JsonGenerator}).
 * <p>
 * This class implements both {@link TreeTraversal} and {@link TreeGenerator},
 * enabling it to function as a self-contained serialization engine. It
 * traverses a source structure (via its {@code NodeVisitor} parent) and
 * consumes its own traversal events (via its {@code NodeGenerator}
 * implementation) to write directly to the provided {@code JsonGenerator}.
 * </p>
 * <p>
 * This class is stateful and intended for a single serialization task, as it
 * operates on a forward-only stream writer.
 * </p>
 */
public class Jackson2Generator extends TreeTraversal implements TreeGenerator {

    protected final JsonGenerator writer;

    /**
     * Constructs a new writer that will output to the given {@link JsonGenerator}.
     *
     * @param writer the Jackson JSON generator to write to, must not be
     *               {@code null}
     */
    public Jackson2Generator(JsonGenerator writer) {
        super(new ArrayDeque<>(), null);
        this.writer = writer;
    }

    @Override
    public Features features() {
        return Jackson2Adapter.FEATURES;
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
        root(node, adapter).generate(this);
        return writer;
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
    public void beginMap() throws TreeIOException {
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
    public void beginSequence() throws TreeIOException {
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
    public void end() throws TreeIOException {
        try {
            if (NodeType.MAP == currentNodeType) {
                writer.writeEndObject();
                return;
            }
            if (NodeType.SEQUENCE == currentNodeType) {
                writer.writeEndArray();
                return;
            }
        } catch (IOException e) {
            throw new TreeIOException(e);
        }

        throw new IllegalStateException("The end() method was called in an invalid context.");
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
    public void booleanValue(boolean value) throws TreeIOException {
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
    public void stringValue(String value) throws TreeIOException {
        try {
            if (currentNodeContext == Context.PROPERTY_KEY) {
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
    public void numericValue(long value) throws TreeIOException {
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
    public void numericValue(BigInteger value) throws TreeIOException {
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
    public void numericValue(double value) throws TreeIOException {
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
    public void numericValue(BigDecimal value) throws TreeIOException {
        try {
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
    public void binaryValue(byte[] value) throws TreeIOException {
        throw new UnsupportedOperationException("JSON does not support a native binary type.");
    }
}
