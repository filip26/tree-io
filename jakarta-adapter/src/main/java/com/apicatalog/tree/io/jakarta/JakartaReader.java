package com.apicatalog.tree.io.jakarta;

import java.util.ArrayList;
import java.util.LinkedHashMap;

import jakarta.json.stream.JsonParser;

public class JakartaReader {

    public Object read(JsonParser parser) {

        if (parser.hasNext()) {
            return processEvent(parser, parser.next());
        }

        return null;
    }

    private static Object processEvent(JsonParser parser, JsonParser.Event event) {
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
                            "Unexpected JSON parse event "
                                    + event
                                    + ", expected "
                                    + JsonParser.Event.KEY_NAME);
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
                "Unexpected JSON parse event " + event);
        };
    }
}
