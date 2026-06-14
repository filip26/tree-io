package com.apicatalog.tree.io.jakcson;

import java.io.IOException;

import com.apicatalog.tree.io.Tree.Event;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeType;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.tree.io.TreeProcessor;
import com.fasterxml.jackson.core.JsonParser;

public final class Jackson2Parser implements TreeParser, TreeProcessor {

    private final JsonParser parser;
    private NodeType nodeType;

    public Jackson2Parser(final JsonParser parser) {
        this.parser = parser;
        this.nodeType = null;
    }

    @Override
    public Features features() {
        return Jackson2Adapter.FEATURES;
    }

    @Override
    public Event next() throws TreeIOException {
        try {
            return switch (parser.nextToken()) {
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
            case VALUE_NULL -> {
                nodeType = NodeType.NULL;
                yield Event.SCALAR;
            }
            case FIELD_NAME -> {
                nodeType = NodeType.STRING;
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
            case VALUE_NUMBER_FLOAT, VALUE_NUMBER_INT -> {
                nodeType = NodeType.NUMBER;
                yield Event.SCALAR;
            }
            case VALUE_STRING -> {
                nodeType = NodeType.STRING;
                yield Event.SCALAR;
            }
            default -> throw new IllegalStateException(
                    """
                    Unsupported token=%s
                    """.formatted(parser.currentToken()));

            };
        } catch (IOException e) {
            throw new TreeIOException(e);
        }
    }

    @Override
    public Number numberValue() throws TreeIOException {
        try {
            return switch (parser.currentToken()) {
            case VALUE_NUMBER_FLOAT -> parser.getDecimalValue();
            case VALUE_NUMBER_INT -> parser.getLongValue();
            default -> throw new IllegalStateException(
                    """
                    Expected number token, but have=%s
                    """.formatted(parser.currentToken()));
            };

        } catch (IOException e) {
            throw new TreeIOException(e);
        }
    }

    @Override
    public String stringValue() throws TreeIOException {
        try {
            return parser.getText();
        } catch (IOException e) {
            throw new TreeIOException(e);
        }
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
