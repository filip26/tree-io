package com.apicatalog.tree.io;

import java.io.IOException;
import java.util.ArrayDeque;
import java.util.Deque;

import jakarta.json.stream.JsonGenerator;

public class JakartaWriter extends AbstractGenerator {

    protected final JsonGenerator writer;

    public JakartaWriter(JsonGenerator writer) {
        this(writer, new ArrayDeque<>());
    }

    protected JakartaWriter(JsonGenerator writer, Deque<Object> stack) {
        super(stack);
        this.writer = writer;
    }

    @Override
    protected void scalar(Context ctx, Object node) throws IOException {
        if (ctx == Context.PROPERTY_KEY) {
            writer.writeKey(adapter.asString(node));
            return;
        }

        switch (adapter.type(node)) {
        case FALSE:
            writer.write(false);
            return;

        case TRUE:
            writer.write(true);
            return;

        case NULL:
            writer.writeNull();
            return;

        case STRING:
            writer.write(adapter.asString(node));
            return;

        case NUMBER:
            if (adapter.isIntegral(node)) {
                writer.write(adapter.bigIntegerValue(node));
            } else {
                writer.write(adapter.decimalValue(node));
            }
            return;

        default:
            throw new IllegalStateException();
        }
    }

    @Override
    protected void beginMap(Context ctx) throws IOException {
        if (ctx == Context.PROPERTY_KEY) {
            throw new IllegalStateException();
        }
        writer.writeStartObject();
    }

    @Override
    protected void beginCollection(Context ctx) throws IOException {
        if (ctx == Context.PROPERTY_KEY) {
            throw new IllegalStateException();
        }
        writer.writeStartArray();
    }

    @Override
    protected void end() throws IOException {
        writer.writeEnd();
    }

}
