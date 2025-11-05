package com.apicatalog.tree.io.jakcson;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayDeque;

import com.apicatalog.tree.io.Features;
import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeGenerator;
import com.apicatalog.tree.io.NodeType;
import com.apicatalog.tree.io.traverse.Visitor;
import com.fasterxml.jackson.core.JsonGenerator;

/**
 * A specialized class that serializes any tree-like source to a JSON document
 * using the Jackson 2 streaming API ({@link JsonGenerator}).
 * <p>
 * This class implements both {@link Visitor} and {@link TreeGenerator},
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
public class Jackson2Generator extends Visitor implements TreeGenerator {

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
     * @throws IOException if an error occurs during writing
     */
    public JsonGenerator node(Object node, TreeAdapter adapter) throws IOException {
        root(node, adapter).traverse(this);
        return writer;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON start-object token (<code>{</code>).
     * </p>
     * @throws IOException 
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
     * Writes a JSON end-object ({@code '}'}) or end-array ({@code ']'}) token based
     * on the current traversal context.
     * </p>
     */
    @Override
    public void end() throws IOException {
        if (NodeType.MAP == currentNodeType) {
            writer.writeEndObject();
            return;
        }
        if (NodeType.COLLECTION == currentNodeType) {
            writer.writeEndArray();
            return;
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
    public void booleanValue(boolean value) throws IOException {
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
    public void stringValue(String value) throws IOException {
        if (currentNodeContext == Context.PROPERTY_KEY) {
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
    public void numericValue(long value) throws IOException {
        writer.writeNumber(value);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(BigInteger value) throws IOException {
        writer.writeNumber(value);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(double value) throws IOException {
        writer.writeNumber(value);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Writes a JSON number.
     * </p>
     */
    @Override
    public void numericValue(BigDecimal value) throws IOException {
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
    public void binaryValue(byte[] value) throws IOException {
        throw new UnsupportedOperationException("JSON does not support a native binary type.");
    }
}
