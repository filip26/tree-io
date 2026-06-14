package com.apicatalog.tree.io.jakarta;

import com.apicatalog.tree.io.Tree.Event;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeType;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.tree.io.TreeProcessor;

import jakarta.json.JsonException;
import jakarta.json.stream.JsonParser;

public final class JakartaParser implements TreeParser, TreeProcessor {

    private final JsonParser parser;
    private NodeType nodeType;

    public JakartaParser(JsonParser parser) {
        this.parser = parser;
        this.nodeType = null;
    }

    @Override
    public Features features() {
        return JakartaAdapter.FEATURES;
    }

    @Override
    public Event next() throws TreeIOException {

        if (!parser.hasNext()) {
            return null;
        }

        try {
            var lastEvent = parser.next();

            return switch (lastEvent) {
            case START_OBJECT -> {
                nodeType = NodeType.MAP;
                yield Event.BEGIN_MAP;
            }
            case END_OBJECT -> {
                nodeType = NodeType.MAP;
                yield Event.END_MAP;
            }
            case START_ARRAY -> {
                nodeType = NodeType.SEQUENCE;
                yield Event.BEGIN_SEQUENCE;
            }
            case END_ARRAY -> {
                nodeType = NodeType.SEQUENCE;
                yield Event.END_SEQUENCE;
            }
            case KEY_NAME -> {
                nodeType = NodeType.STRING;
                yield Event.SCALAR;
            }
            case VALUE_NULL -> {
                nodeType = NodeType.NULL;
                yield Event.SCALAR;
            }
            case VALUE_TRUE -> {
                nodeType = NodeType.TRUE;
                yield Event.SCALAR;
            }
            case VALUE_FALSE -> {
                nodeType = NodeType.FALSE;
                yield Event.SCALAR;
            }
            case VALUE_NUMBER -> {
                nodeType = NodeType.NUMBER;
                yield Event.SCALAR;
            }
            case VALUE_STRING -> {
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
}
