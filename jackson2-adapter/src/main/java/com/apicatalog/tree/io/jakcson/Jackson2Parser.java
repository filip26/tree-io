package com.apicatalog.tree.io.jakcson;

import java.io.IOException;
import java.util.ArrayDeque;
import java.util.Deque;

import com.apicatalog.tree.io.Tree.Event;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeContext;
import com.apicatalog.tree.io.Tree.NodeType;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.tree.io.TreeProcessor;
import com.fasterxml.jackson.core.JsonParser;

public final class Jackson2Parser implements TreeParser, TreeProcessor {

    private final JsonParser parser;
    private final Deque<NodeContext> contexts;

    private NodeType nodeType;
    private NodeContext context;

    private Object scalar;

    public Jackson2Parser(final JsonParser parser) {
        this(parser, new ArrayDeque<NodeContext>());
    }

    public Jackson2Parser(final JsonParser parser, final Deque<NodeContext> contexts) {
        this.parser = parser;
        this.contexts = contexts;
        this.nodeType = null;
        this.context = null;
        this.scalar = null;
        contexts.push(NodeContext.ROOT);
    }

    @Override
    public Features features() {
        return Jackson2Adapter.FEATURES;
    }

    @Override
    public Event next() throws IOException {

        this.context = contexts.peek();
        this.scalar = null;

        return switch (parser.nextToken()) {
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
        case FIELD_NAME -> {
            switchMapContext();
            nodeType = NodeType.STRING;
            scalar = parser.getText();
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
        case VALUE_NUMBER_FLOAT, VALUE_NUMBER_INT -> {
            switchMapContext();
            nodeType = NodeType.NUMBER;
            scalar = switch (parser.currentToken()) {
            case VALUE_NUMBER_FLOAT -> parser.getDecimalValue();
            case VALUE_NUMBER_INT -> parser.getLongValue();
            default -> throw new IllegalStateException(
                    """
                    Expected number token, but have=%s
                    """.formatted(parser.currentToken()));
            };
            yield Event.SCALAR;
        }
        case VALUE_STRING -> {
            switchMapContext();
            nodeType = NodeType.STRING;
            scalar = parser.getText();
            yield Event.SCALAR;
        }
        case null -> {
            nodeType = null;
            context = contexts.peek();
            yield null;
        }
        default -> throw new IllegalStateException(
                """
                Unsupported token=%s
                """.formatted(parser.currentToken()));

        };
    }

    @Override
    public Number numberValue() {
        return (Number) scalar;
    }

    @Override
    public String stringValue() {
        return (String) scalar;
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
        return Jackson2Parser.class.getSimpleName() + "[context=" + context + ", type=" + nodeType + "]";
    }
}
