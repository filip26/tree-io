package com.apicatalog.tree.io;

import java.io.IOException;
import java.util.ArrayDeque;
import java.util.Deque;

import com.fasterxml.jackson.core.JsonGenerator;

public class Jackson2Writer extends NodeGenerator {

    protected final JsonGenerator writer;

    public Jackson2Writer(JsonGenerator writer) {
        this(writer, new ArrayDeque<>());
    }

    protected Jackson2Writer(JsonGenerator writer, Deque<Object> stack) {
        super(stack, PropertyKeyPolicy.StringOnly);
        this.writer = writer;
    }

    @Override
    protected void scalar(Object node) throws IOException {
        if (nodeContext == Context.PROPERTY_KEY) {
            writer.writeFieldName(adapter.asString(node));
            return;
        }

        switch (adapter.typeOf(node)) {
        case FALSE:
            writer.writeBoolean(false);
            return;

        case TRUE:
            writer.writeBoolean(true);
            return;

        case NULL:
            writer.writeNull();
            return;

        case STRING:
            writer.writeString(adapter.asString(node));
            return;

        case NUMBER:
            if (adapter.isIntegral(node)) {
                writer.writeNumber(adapter.bigIntegerValue(node));
            } else {
                writer.writeNumber(adapter.decimalValue(node));
            }
            return;

        default:
            throw new IllegalStateException();
        }
    }

    @Override
    protected void beginMap() throws IOException {
        if (nodeContext == Context.PROPERTY_KEY) {
            throw new IllegalStateException();
        }
        writer.writeStartObject();
    }

    @Override
    protected void beginCollection() throws IOException {
        if (nodeContext == Context.PROPERTY_KEY) {
            throw new IllegalStateException();
        }
        writer.writeStartArray();
    }

    @Override
    protected void end() throws IOException {
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

}
