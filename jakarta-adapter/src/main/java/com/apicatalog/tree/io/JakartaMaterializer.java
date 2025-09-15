package com.apicatalog.tree.io;

import java.io.IOException;
import java.util.ArrayDeque;
import java.util.Deque;

import jakarta.json.Json;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;

public class JakartaMaterializer extends AbstractGenerator {

    protected JsonValue value;
    protected final Deque<Object> builders;

    public JakartaMaterializer() {
        super(new ArrayDeque<>());
        this.value = null;
        this.builders = new ArrayDeque<>();
    }

    public void accept(Object node, NodeAdapter adapter) {
        // reset
        this.value = null;
        this.builders.clear();

        try {
            super.accept(node, adapter);
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }

    }

    @Override
    protected void scalar(Context ctx, Object node) {

        switch (ctx) {
        case PROPERTY_KEY:
            builders.push(adapter.asString(node));
            return;

        case PROPERTY_VALUE:
            String key = (String) builders.pop();
            ((JsonObjectBuilder) builders.peek()).add(key, toJsonValue(node));
            return;

        case COLLECTION_ELEMENT:
            ((JsonArrayBuilder) builders.peek()).add(toJsonValue(node));
            return;

        case ROOT:
            value = toJsonValue(node);
            return;

        default:
            throw new IllegalStateException();
        }
    }

    protected JsonValue toJsonValue(Object node) {
        switch (adapter.type(node)) {
        case STRING:
            return Json.createValue(adapter.stringValue(node));

        case NULL:
            return JsonValue.NULL;

        case TRUE:
            return JsonValue.TRUE;

        case FALSE:
            return JsonValue.FALSE;

        case NUMBER:
            return adapter.isIntegral(node)
                    ? Json.createValue(adapter.bigIntegerValue(node))
                    : Json.createValue(adapter.decimalValue(node));

        default:
            throw new IllegalStateException();
        }
    }

    @Override
    protected void beginMap(Context ctx) {
        builders.push(Json.createObjectBuilder());
    }

    @Override
    protected void beginCollection(Context ctx) {
        builders.push(Json.createArrayBuilder());
    }

    @Override
    protected void end() {
        Object builder = builders.pop();
        if (builder instanceof JsonArrayBuilder) {
            value = ((JsonArrayBuilder) builder).build();
        }
        if (builder instanceof JsonObjectBuilder) {
            value = ((JsonObjectBuilder) builder).build();
        }

        if (!builders.isEmpty()) {
            if (builders.peek() instanceof String) {
                String key = (String) builders.pop();
                ((JsonObjectBuilder) builders.peek()).add(key, value);
            }
            if (builders.peek() instanceof JsonArrayBuilder) {
                ((JsonArrayBuilder) builders.peek()).add(value);
            }
        }
    }

    public JsonValue value() {
        return value;
    }
}
