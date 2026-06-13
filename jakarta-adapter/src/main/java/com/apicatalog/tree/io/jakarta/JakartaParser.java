package com.apicatalog.tree.io.jakarta;

import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeParser;

import jakarta.json.JsonException;
import jakarta.json.stream.JsonParser;
import jakarta.json.stream.JsonParser.Event;

public final class JakartaParser implements TreeParser {

    private final JsonParser parser;
    private Event lastEvent;

    public JakartaParser(JsonParser parser) {
        this.parser = parser;
    }

//    @Override
//    public Tree parse(InputStream is) throws TreeIOException {
//        try (final var reader = factory.createReader(is, Charset.defaultCharset())) {
//            return new Tree(reader.readValue(), JakartaAdapter.instance());
//        } catch (JsonException | IllegalStateException e) {
//            throw new TreeIOException(e);
//        }
//    }

    @Override
    public Features features() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Token nextToken() throws TreeIOException {

        try {
            this.lastEvent = parser.next();

            return switch (lastEvent) {
            case START_OBJECT -> Token.BEGIN_MAP;
            case END_OBJECT -> Token.END_MAP;
            case START_ARRAY -> Token.BEGIN_SEQUENCE;
            case END_ARRAY -> Token.END_SEQUENCE;
            case KEY_NAME -> nextToken();
            case VALUE_NULL -> Token.NULL;
            case VALUE_TRUE -> Token.TRUE;
            case VALUE_FALSE -> Token.FALSE;
            case VALUE_NUMBER -> Token.NUMBER;
            case VALUE_STRING -> Token.STRING;
            };

        } catch (JsonException e) {
            throw new TreeIOException(e);
        }
    }

    @Override
    public Object getScalar() {
        return switch (lastEvent) {
        case VALUE_STRING -> parser.getString();
        case VALUE_NUMBER -> parser.isIntegralNumber()
                ? parser.getLong()
                : parser.getBigDecimal();
        case VALUE_TRUE -> Boolean.TRUE;
        case VALUE_FALSE -> Boolean.FALSE;
        case VALUE_NULL -> null;

        default -> throw new IllegalStateException(
                """
                Expected scalar, but have=%s
                """.formatted(lastEvent));
        };
    }

}
