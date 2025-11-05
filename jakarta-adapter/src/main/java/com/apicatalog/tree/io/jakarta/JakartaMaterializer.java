package com.apicatalog.tree.io.jakarta;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.Deque;

import com.apicatalog.tree.io.Features;
import com.apicatalog.tree.io.TreeIOAdapter;
import com.apicatalog.tree.io.TreeIOGenerator;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.traverse.Visitor;

import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;
import jakarta.json.spi.JsonProvider;

/**
 * A specialized class that builds a {@code jakarta.json.JsonValue} object model
 * from any tree-like source.
 * <p>
 * This class implements both {@link Visitor} and {@link TreeIOGenerator},
 * allowing it to act as a self-contained transformation engine. It traverses a
 * source structure using its {@code NodeVisitor} capabilities and consumes its
 * own traversal events via its {@code NodeGenerator} implementation to
 * construct a {@link JsonValue} tree in memory.
 * </p>
 * <p>
 * The class is stateful and designed for a single transformation. It can be
 * reused by calling the {@link #reset()} method.
 * </p>
 */
public class JakartaMaterializer extends Visitor implements TreeIOGenerator {

    protected final JsonProvider provider;
    protected final Deque<Object> builders;

    protected JsonValue json;

    /**
     * Constructs a new materializer using the default {@link JsonProvider}.
     */
    public JakartaMaterializer() {
        this(JsonProvider.provider());
    }

    @Override
    public Features features() {
        return JakartaAdapter.FEATURES;
    }

    /**
     * Constructs a new materializer using the specified {@link JsonProvider}.
     *
     * @param provider the JSON-P provider to use for creating JSON values and
     *                 builders
     */
    public JakartaMaterializer(JsonProvider provider) {
        super(new ArrayDeque<>(), null);

        this.provider = provider;
        this.builders = new ArrayDeque<>();
        this.json = null;
    }

    public JsonValue node(TreeIO node) throws IOException {
        return node(node.node(), node.adapter());
    }
    
    /**
     * The primary entry point for materialization. Traverses the given source node
     * and returns the resulting Jakarta {@link JsonValue}.
     *
     * @param node    the source root node to traverse
     * @param adapter the adapter for interpreting the source node's structure
     * @return the fully materialized {@link JsonValue}
     * @throws IOException if an error occurs during generation
     */
    public JsonValue node(Object node, TreeIOAdapter adapter) throws IOException {
        root(node, adapter).traverse(this);
        return json;
    }

    /**
     * Returns the fully materialized {@link JsonValue} after a successful
     * traversal.
     *
     * @return the resulting {@link JsonValue}, or {@code null} if traversal has not
     *         completed
     */
    public JsonValue json() {
        return json;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Clears the partially built JSON structure and the internal builder stack,
     * allowing the instance to be reused for a new materialization.
     * </p>
     */
    @Override
    public Visitor reset() {
        this.builders.clear();
        this.json = null;
        return super.reset();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Creates a {@link JsonValue#NULL}.
     * </p>
     */
    @Override
    public void nullValue() throws IOException {
        json(JsonValue.NULL);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Creates a {@link JsonValue#TRUE} or {@link JsonValue#FALSE}.
     * </p>
     */
    @Override
    public void booleanValue(boolean node) throws IOException {
        json(node ? JsonValue.TRUE : JsonValue.FALSE);
    }

    /**
     * {@inheritDoc}
     * <p>
     * If the current context is a {@code PROPERTY_KEY}, the string is pushed onto
     * the builder stack to be used as a key. Otherwise, it creates a
     * {@code JsonString} value.
     * </p>
     */
    @Override
    public void stringValue(String node) throws IOException {
        if (currentNodeContext == Context.PROPERTY_KEY) {
            builders.push(node);
            return;
        }
        json(provider.createValue(node));
    }

    /**
     * {@inheritDoc}
     * <p>
     * Creates a {@code JsonNumber}.
     * </p>
     */
    @Override
    public void numericValue(long node) throws IOException {
        json(provider.createValue(node));
    }

    /**
     * {@inheritDoc}
     * <p>
     * Creates a {@code JsonNumber}.
     * </p>
     */
    @Override
    public void numericValue(BigInteger node) throws IOException {
        json(provider.createValue(node));
    }

    /**
     * {@inheritDoc}
     * <p>
     * Creates a {@code JsonNumber}.
     * </p>
     */
    @Override
    public void numericValue(double node) throws IOException {
        json(provider.createValue(node));
    }

    /**
     * {@inheritDoc}
     * <p>
     * Creates a {@code JsonNumber}.
     * </p>
     */
    @Override
    public void numericValue(BigDecimal node) throws IOException {
        json(provider.createValue(node));
    }

    /**
     * {@inheritDoc}
     * <p>
     * This operation is not supported, as the Jakarta JSON-P API does not provide a
     * native binary type.
     * </p>
     * 
     * @throws UnsupportedOperationException always
     */
    @Override
    public void binaryValue(byte[] node) throws IOException {
        throw new UnsupportedOperationException("Jakarta JSON-P does not support a native binary type.");
    }

    /**
     * {@inheritDoc}
     * <p>
     * Starts a new {@link JsonObjectBuilder} and places it on the internal builder
     * stack.
     * </p>
     */
    @Override
    public void beginMap() throws IOException {
        builders.push(provider.createObjectBuilder());
    }

    /**
     * {@inheritDoc}
     * <p>
     * Starts a new {@link JsonArrayBuilder} and places it on the internal builder
     * stack.
     * </p>
     */
    @Override
    public void beginList() throws IOException {
        builders.push(provider.createArrayBuilder());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void beginSet() throws IOException {
        throw new UnsupportedOperationException();
    }

    /**
     * 
     * {@inheritDoc}
     * <p>
     * Finalizes the current {@code JsonObjectBuilder} or {@code JsonArrayBuilder},
     * pops it from the stack, builds the final {@link JsonValue}, and attaches it
     * to its parent structure if one exists.
     * </p>
     */
    @Override
    public void end() throws IOException {

        final Object builder = builders.pop();

        if (builder instanceof JsonArrayBuilder) {
            json = ((JsonArrayBuilder) builder).build();

        } else if (builder instanceof JsonObjectBuilder) {
            json = ((JsonObjectBuilder) builder).build();

        } else if (builder instanceof JsonValue) {
            json = (JsonValue) builder;

        } else {
            throw new IllegalStateException("Internal builder stack is in an inconsistent state [" + builder + "].");
        }

        if (!builders.isEmpty()) {
            if (builders.peek() instanceof String) {
                String key = (String) builders.pop();
                ((JsonObjectBuilder) builders.peek()).add(key, json);
            } else if (builders.peek() instanceof JsonArrayBuilder) {
                ((JsonArrayBuilder) builders.peek()).add(json);
            }
        }
    }

    /**
     * Internal dispatcher that places a newly created {@link JsonValue} into the
     * correct position within the JSON structure being built.
     *
     * @param value the {@link JsonValue} to place
     */
    protected void json(final JsonValue value) {
        switch (currentNodeContext) {
        case PROPERTY_VALUE:
            String key = (String) builders.pop();
            ((JsonObjectBuilder) builders.peek()).add(key, value);
            return;

        case COLLECTION_ELEMENT:
            ((JsonArrayBuilder) builders.peek()).add(value);
            return;

        case ROOT:
            json = value;
            return;

        default:
            throw new IllegalStateException("Cannot add a JsonValue in the current context: " + currentNodeContext);
        }
    }
}
