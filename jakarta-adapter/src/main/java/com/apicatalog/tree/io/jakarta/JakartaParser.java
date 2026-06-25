package com.apicatalog.tree.io.jakarta;

import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayDeque;
import java.util.Deque;

import com.apicatalog.tree.io.Tree.Event;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeContext;
import com.apicatalog.tree.io.Tree.NodeType;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.tree.io.TreeProcessor;

import jakarta.json.stream.JsonParser;
import jakarta.json.stream.JsonParserFactory;

public final class JakartaParser implements TreeParser, TreeProcessor, Closeable {

    private final JsonParser parser;
    private final Deque<NodeContext> contexts;

    private NodeType nodeType;
    private NodeContext context;

    public JakartaParser(JsonParser parser) {
        this.parser = parser;
        this.contexts = new ArrayDeque<NodeContext>();
        this.nodeType = null;
        contexts.push(NodeContext.ROOT);
    }

    public static JakartaParser createParser(InputStream is, JsonParserFactory factory) {
        return new JakartaParser(factory.createParser(is));
    }

    @Override
    public Features features() {
        return JakartaAdapter.FEATURES;
    }

    @Override
    public Event next() {

        if (!parser.hasNext()) {
            nodeType = null;
            context = contexts.peek();
            return null;
        }

        this.context = contexts.peek();

        var lastEvent = parser.next();
        return switch (lastEvent) {
        case START_OBJECT -> {
            contexts.push(NodeContext.ENTRY_KEY);
            nodeType = NodeType.MAP;
            yield Event.BEGIN_MAP;
        }
        case END_OBJECT -> {
            contexts.pop();
            this.context = contexts.peek();
            switchMapContext();
            nodeType = NodeType.MAP;
            yield Event.END_MAP;
        }
        case START_ARRAY -> {
            contexts.push(NodeContext.ELEMENT);
            nodeType = NodeType.SEQUENCE;
            yield Event.BEGIN_SEQUENCE;
        }
        case END_ARRAY -> {
            contexts.pop();
            this.context = contexts.peek();
            switchMapContext();
            nodeType = NodeType.SEQUENCE;
            yield Event.END_SEQUENCE;
        }
        case VALUE_NULL -> {
            switchMapContext();
            nodeType = NodeType.NULL;
            yield Event.SCALAR;
        }
        case VALUE_TRUE -> {
            switchMapContext();
            nodeType = NodeType.TRUE;
            yield Event.SCALAR;
        }
        case VALUE_FALSE -> {
            switchMapContext();
            nodeType = NodeType.FALSE;
            yield Event.SCALAR;
        }
        case VALUE_NUMBER -> {
            switchMapContext();
            nodeType = NodeType.NUMBER;
            yield Event.SCALAR;
        }
        case KEY_NAME -> {
            if (NodeContext.ENTRY_KEY != contexts.pop()) {
                throw new IllegalStateException();
            };
            contexts.push(NodeContext.ENTRY_VALUE);
            nodeType = NodeType.STRING;
            yield Event.SCALAR;
        }
        case VALUE_STRING -> {
            switchMapContext();
            nodeType = NodeType.STRING;
            yield Event.SCALAR;
        }
        };

    }

    @Override
    public Number numberValue() {
        return parser.isIntegralNumber()
                ? parser.getLong()
                : parser.getBigDecimal();
    }

    @Override
    public String stringValue() {
        return parser.getString();
    }

    @Override
    public byte[] binaryValue() {
        throw new UnsupportedOperationException();
    }

    @Override
    public NodeType nodeType() {
        return nodeType;
    }

    @Override
    public NodeContext context() {
        return context;
    }

    private void switchMapContext() {
        if (contexts.peek() == NodeContext.ENTRY_KEY) {
            contexts.pop();
            contexts.push(NodeContext.ENTRY_VALUE);
        } else if (contexts.peek() == NodeContext.ENTRY_VALUE) {
            contexts.pop();
            contexts.push(NodeContext.ENTRY_KEY);
        }
    }

    @Override
    public String toString() {
        return JakartaParser.class.getSimpleName() + "[context=" + context + ", type=" + nodeType + "]";
    }

    @Override
    public void close() throws IOException {
        parser.close();
    }
}
