package com.apicatalog.tree.io.jakarta;

import java.io.InputStream;
import java.io.Reader;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.Map;

import com.apicatalog.tree.io.TreeReader;

import jakarta.json.Json;
import jakarta.json.stream.JsonParser;
import jakarta.json.stream.JsonParserFactory;

public final class JakartaReader implements TreeReader {

    private final JsonParserFactory factory;

    public JakartaReader() {
        this(Json.createParserFactory(Map.of()));
    }

    public JakartaReader(JsonParserFactory factory) {
        this.factory = factory;
    }

    public Object readNode(InputStream is) {
        return readNode(factory.createParser(is));
    }

    @Override
    public Object read(Reader reader) {
        return readNode(factory.createParser(reader));
    }

    protected static Object readNode(JsonParser parser) {
        if (parser.hasNext()) {
            var structures = new ArrayDeque<Object>();

            return processEvent(parser, parser.next());
        }
        return null;
    }

    protected static Object processEvent(JsonParser parser, JsonParser.Event event) {
        return switch (event) {
        case START_OBJECT -> {
            var map = new LinkedHashMap<String, Object>();
            while (parser.hasNext()) {
                var next = parser.next();
                if (next == JsonParser.Event.END_OBJECT) {
                    break;
                }
                // In OBJECT context, next is always KEY_NAME
                if (next != JsonParser.Event.KEY_NAME) {
                    throw new IllegalArgumentException(
                            """
                            Unexpected JSON parse event=%s, expected=%s
                            """.formatted(event, JsonParser.Event.KEY_NAME));
                }

                var key = parser.getString();

                map.put(key, processEvent(parser, parser.next()));
            }
            yield map;
        }
        case START_ARRAY -> {
            var list = new ArrayList<>();
            while (parser.hasNext()) {
                var next = parser.next();
                if (next == JsonParser.Event.END_ARRAY) {
                    break;
                }
                list.add(processEvent(parser, next));
            }
            yield list;
        }
        case VALUE_STRING -> parser.getString();

        case VALUE_NUMBER -> parser.isIntegralNumber()
                ? parser.getLong()
                : parser.getBigDecimal();

        case VALUE_TRUE -> Boolean.TRUE;
        case VALUE_FALSE -> Boolean.FALSE;
        case VALUE_NULL -> null;

        default -> throw new IllegalStateException(
                """
                Unexpected JSON parse event=%s
                """.formatted(event));
        };
    }
}
