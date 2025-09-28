package com.apicatalog.tree.io;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayDeque;

import com.fasterxml.jackson.core.JsonGenerator;

public class Jackson2Writer extends NodeVisitor implements NodeGenerator {

    protected final JsonGenerator writer;

    public Jackson2Writer(JsonGenerator writer) {
        super(new ArrayDeque<>(), null);
        this.writer = writer;
    }

    public JsonGenerator node(Object node, NodeAdapter adapter) throws IOException {
        root(node, adapter).traverse(this);
        return writer;
    }

    @Override
    public void beginMap() throws IOException {
        writer.writeStartObject();
    }

    @Override
    public void beginCollection() throws IOException {
        writer.writeStartArray();
    }

    @Override
    public void end() throws IOException {
        if (NodeType.MAP == nodeType) {
            writer.writeEndObject();
            return;
        }
        if (NodeType.COLLECTION == nodeType) {
            writer.writeEndArray();
            return;
        }
        throw new IllegalStateException();
    }

    @Override
    public void nullValue() throws IOException {
        writer.writeNull();
    }

    @Override
    public void booleanValue(boolean value) throws IOException {
        writer.writeBoolean(value);
    }

    @Override
    public void stringValue(String value) throws IOException {
        if (nodeContext == Context.PROPERTY_KEY) {
            writer.writeFieldName(value);
            return;
        }
        writer.writeString(value);
    }

    @Override
    public void numericValue(long value) throws IOException {
        writer.writeNumber(value);

    }

    @Override
    public void numericValue(BigInteger value) throws IOException {
        writer.writeNumber(value);
    }

    @Override
    public void numericValue(double value) throws IOException {
        writer.writeNumber(value);
    }

    @Override
    public void numericValue(BigDecimal value) throws IOException {
        writer.writeNumber(value);
    }

    @Override
    public void binaryValue(byte[] value) throws IOException {
        throw new UnsupportedOperationException();
    }
}
