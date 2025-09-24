package com.apicatalog.tree.io;

import java.io.IOException;
import java.util.ArrayDeque;
import java.util.Deque;

import jakarta.json.Json;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;

public class JakartaMaterializer extends NodeGenerator {

    protected JsonValue json;
    protected final Deque<Object> builders;

    public JakartaMaterializer() {
        super(new ArrayDeque<>(), PropertyKeyPolicy.StringOnly);
        this.json = null;
        this.builders = new ArrayDeque<>();
    }

    public void node(Object node, NodeAdapter adapter) {
        // reset
        this.json = null;
        this.builders.clear();

        try {
            super.node(node, adapter);
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }

    }

    @Override
    protected void scalar(Object node) {

        switch (nodeContext) {
        case PROPERTY_KEY:
            builders.push(adapter.asString(node));
            return;

        case PROPERTY_VALUE:
            String key = (String) builders.pop();
            if (builders.peek() instanceof JsonObject) {
                builders.pop();
                builders.push(Json.createObjectBuilder());
            }
            ((JsonObjectBuilder) builders.peek()).add(key, toJsonValue(node));
            return;

        case COLLECTION_ELEMENT:
            ((JsonArrayBuilder) builders.peek()).add(toJsonValue(node));
            return;

        case ROOT:
            json = toJsonValue(node);
            return;

        default:
            throw new IllegalStateException();
        }
    }

    protected JsonValue toJsonValue(Object node) {
        switch (adapter.typeOf(node)) {
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
    protected void beginMap() {
        builders.push(JsonValue.EMPTY_JSON_OBJECT);
    }

    @Override
    protected void beginCollection() {
        builders.push(Json.createArrayBuilder());
    }

    @Override
    protected void end() {
        Object builder = builders.pop();

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
                if (builders.peek() instanceof JsonObject) {
                    builders.pop();
                    builders.push(Json.createObjectBuilder());
                }
                ((JsonObjectBuilder) builders.peek()).add(key, json);
            } else if (builders.peek() instanceof JsonArrayBuilder) {
                ((JsonArrayBuilder) builders.peek()).add(json);
            }
        }
    }

    public JsonValue json() {
        return json;
    }
}
