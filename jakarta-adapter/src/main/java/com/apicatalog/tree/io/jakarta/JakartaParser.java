package com.apicatalog.tree.io.jakarta;

import com.apicatalog.tree.io.Tree.Event;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeContext;
import com.apicatalog.tree.io.Tree.NodeType;

import java.util.ArrayDeque;
import java.util.Deque;

import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.tree.io.TreeProcessor;

import jakarta.json.JsonException;
import jakarta.json.stream.JsonParser;

public final class JakartaParser implements TreeParser, TreeProcessor {

    private final JsonParser parser;
    private final Deque<NodeContext> stack;

    private NodeType nodeType;
    private NodeContext context;

    public JakartaParser(JsonParser parser) {
        this.parser = parser;
        this.stack = new ArrayDeque<NodeContext>();
        this.nodeType = null;
        stack.push(NodeContext.ROOT);
    }

    @Override
    public Features features() {
        return JakartaAdapter.FEATURES;
    }

    @Override
    public Event next() throws TreeIOException {

        if (!parser.hasNext()) {
            nodeType = null;
            context = stack.peek();
            return null;
        }

        this.context = stack.peek();

        try {
            var lastEvent = parser.next();
            return switch (lastEvent) {
            case START_OBJECT -> {
                stack.push(NodeContext.ENTRY_KEY);
                nodeType = NodeType.MAP;
                yield Event.BEGIN_MAP;
            }
            case END_OBJECT -> {
                stack.pop();
                this.context = stack.peek();
                switchMapContext();
                nodeType = NodeType.MAP;
                yield Event.END_MAP;
            }
            case START_ARRAY -> {
                stack.push(NodeContext.ELEMENT);
                nodeType = NodeType.SEQUENCE;
                yield Event.BEGIN_SEQUENCE;
            }
            case END_ARRAY -> {
                stack.pop();
                this.context = stack.peek();
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
            case KEY_NAME, VALUE_STRING -> {
                switchMapContext();
                nodeType = NodeType.STRING;
                yield Event.SCALAR;
            }
            };

        } catch (JsonException e) {
            throw new TreeIOException(e);
        }
    }

    @Override
    public Number numberValue() throws TreeIOException {
        return parser.isIntegralNumber()
                ? parser.getLong()
                : parser.getBigDecimal();
    }

    @Override
    public String stringValue() throws TreeIOException {
        return parser.getString();
    }

    @Override
    public byte[] binaryValue() throws TreeIOException {
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
        if (stack.peek() == NodeContext.ENTRY_KEY) {
            stack.pop();
            stack.push(NodeContext.ENTRY_VALUE);
        } else if (stack.peek() == NodeContext.ENTRY_VALUE) {
            stack.pop();
            stack.push(NodeContext.ENTRY_KEY);
        }
    }
    
    @Override
    public String toString() {
        return JakartaParser.class.getSimpleName() + "[context=" + context + ", type=" + nodeType + "]";
    }
}
