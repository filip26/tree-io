package com.apicatalog.tree.io;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.function.Function;

import jakarta.json.stream.JsonGenerator;

public class JakartaWriter extends NodeVisitor implements NodeGenerator {

    protected final JsonGenerator writer;
    protected final Function<byte[], String> encoder;

    public JakartaWriter(JsonGenerator writer) {
        this(writer, null);
    }

    public JakartaWriter(JsonGenerator writer, Function<byte[], String> encoder) {
        super(new ArrayDeque<>(), null);
        this.writer = writer;
        this.encoder = encoder;
    }

    public JsonGenerator node(Object node, NodeAdapter adapter) throws IOException {
        root(node, adapter).traverse(this);
        return writer;
    }

    @Override
    public void nullValue() throws IOException {
        writer.writeNull();        
    }

    @Override
    public void booleanValue(boolean node) throws IOException {
        writer.write(node);        
    }

    @Override
    public void stringValue(String node) throws IOException {
        if (currentNodeContext == Context.PROPERTY_KEY) {
            writer.writeKey(nodeAdapter.asString(node));
            return;
        }
        writer.write(node);
    }

    @Override
    public void numericValue(long node) throws IOException {
        writer.write(node);
    }

    @Override
    public void numericValue(BigInteger node) throws IOException {
        writer.write(node);
    }

    @Override
    public void numericValue(double node) throws IOException {
        writer.write(node);        
    }

    @Override
    public void numericValue(BigDecimal node) throws IOException {
        writer.write(node);
    }

    @Override
    public void binaryValue(byte[] node) throws IOException {
        if (encoder == null) {
            throw new UnsupportedOperationException();
        }
        writer.write(encoder.apply(node));
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
        writer.writeEnd();        
    }
}
