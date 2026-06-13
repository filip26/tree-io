package com.apicatalog.tree.io.jakcson;

import java.io.IOException;

import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.tree.io.TreeProcessor;
import com.fasterxml.jackson.core.JsonParser;

public final class Jackson2Parser implements TreeParser, TreeProcessor {

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
    public Number getNumber() throws TreeIOException {
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
    public String getString() throws TreeIOException {
        try {
            return parser.getText();
        } catch (IOException e) {
            throw new TreeIOException(e);
        }
    }

    @Override
    public byte[] getBinary() throws TreeIOException {
        throw new UnsupportedOperationException();
    }
}
