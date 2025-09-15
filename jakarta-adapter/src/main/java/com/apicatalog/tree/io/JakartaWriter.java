package com.apicatalog.tree.io;

import java.io.IOException;
import java.util.ArrayDeque;

import jakarta.json.stream.JsonGenerator;

public class JakartaWriter extends BaseNodeConsumer {

    protected final JsonGenerator writer;

    protected JakartaWriter(JsonGenerator writer) {
        super(new ArrayDeque<>(), JakartaAdapter.INSTANCE);
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
            throw new IllegalStateException("TODO " + adapter.type(node));
        }
    }

    @Override
    protected void beginMap() throws IOException {
        writer.writeStartObject();
    }

    @Override
    protected void beginCollection() throws IOException {
        writer.writeStartArray();
    }

    @Override
    protected void end() throws IOException {
        writer.writeEnd();
    }

}
