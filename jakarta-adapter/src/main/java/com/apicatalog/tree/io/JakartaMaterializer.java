package com.apicatalog.tree.io;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.Deque;

import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;
import jakarta.json.spi.JsonProvider;

public class JakartaMaterializer extends NodeVisitor implements NodeGenerator {

    protected final JsonProvider provider;
    protected final Deque<Object> builders;

    protected JsonValue json;

    public JakartaMaterializer() {
        this(JsonProvider.provider());
    }

    public JakartaMaterializer(JsonProvider provider) {
        super(new ArrayDeque<>(), null);

        this.provider = provider;
        this.builders = new ArrayDeque<>();
        this.json = null;
    }

    public JsonValue node(Object node, NodeAdapter adapter) throws IOException {
        root(node, adapter).traverse(this);
        return json;
    }

    public JsonValue json() {
        return json;
    }

    @Override
    public NodeVisitor reset() {
        this.builders.clear();
        this.json = null;
        return super.reset();
    }

    @Override
    public void nullValue() throws IOException {
        json(JsonValue.NULL);
    }

    @Override
    public void booleanValue(boolean node) throws IOException {
        json(node ? JsonValue.TRUE : JsonValue.FALSE);
    }

    @Override
    public void stringValue(String node) throws IOException {
        if (currentNodeContext == Context.PROPERTY_KEY) {
            builders.push(node);
            return;
        }
        json(provider.createValue(node));
    }

    @Override
    public void numericValue(long node) throws IOException {
        json(provider.createValue(node));
    }

    @Override
    public void numericValue(BigInteger node) throws IOException {
        json(provider.createValue(node));
    }

    @Override
    public void numericValue(double node) throws IOException {
        json(provider.createValue(node));
    }

    @Override
    public void numericValue(BigDecimal node) throws IOException {
        json(provider.createValue(node));
    }

    @Override
    public void binaryValue(byte[] node) throws IOException {
        throw new UnsupportedOperationException();
    }

    @Override
    public void beginMap() throws IOException {
        builders.push(provider.createObjectBuilder());
    }

    @Override
    public void beginCollection() throws IOException {
        builders.push(provider.createArrayBuilder());
    }

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
            throw new IllegalStateException();
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

    public void json(final JsonValue value) {
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
            throw new IllegalStateException();
        }
    }
}
