package com.apicatalog.tree.io.jakcson;

import java.io.IOException;
import java.io.InputStream;

import com.apicatalog.tree.io.NodeContext;
import com.apicatalog.tree.io.Tree;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeParser;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

public final class Jackson2Parser implements TreeParser {

    private final JsonParser parser;

    public Jackson2Parser(final JsonParser parser) {
        this.parser = parser;
    }

    @Override
    public Features features() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Token nextToken() throws TreeIOException {
        try {
            return switch (parser.nextToken()) {
            case START_OBJECT -> Token.BEGIN_MAP;
            case END_OBJECT -> Token.END_MAP;
            case START_ARRAY -> Token.BEGIN_SEQUENCE;
            case END_ARRAY -> Token.END_SEQUENCE;
            case VALUE_NULL -> Token.NULL;
            case FIELD_NAME -> nextToken();
            case VALUE_TRUE -> Token.TRUE;
            case VALUE_FALSE -> Token.FALSE;
            case VALUE_NUMBER_FLOAT, VALUE_NUMBER_INT -> Token.NUMBER;
            case VALUE_STRING -> Token.STRING;
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
    public Object getScalar() throws TreeIOException {
        try {
            return switch (parser.currentToken()) {
            case VALUE_STRING -> parser.getText();
            case VALUE_NUMBER_FLOAT -> parser.getDecimalValue();
            case VALUE_NUMBER_INT -> parser.getLongValue();
            case VALUE_TRUE -> Boolean.TRUE;
            case VALUE_FALSE -> Boolean.FALSE;
            case VALUE_NULL -> null;

            default -> throw new IllegalStateException(
                    """
                    Expected scalar, but have=%s
                    """.formatted(parser.currentToken()));
            };
        } catch (IOException e) {
            throw new TreeIOException(e);
        }
    }
}
