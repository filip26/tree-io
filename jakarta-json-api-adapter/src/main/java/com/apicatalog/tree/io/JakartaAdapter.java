package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JakartaAdapter implements NodeAdapter {

    static final JakartaAdapter INSTANCE = new JakartaAdapter();

    public static final JakartaAdapter instance() {
        return  INSTANCE;
    }

    @Override
    public NodeType typeOf(Object node) {
        switch (((JsonValue) node).getValueType()) {
        case NULL:
            return NodeType.NULL;
        case TRUE:
            return NodeType.TRUE;
        case FALSE:
            return NodeType.FALSE;
        case STRING:
            return NodeType.STRING;
        case NUMBER:
            return NodeType.NUMBER;
        case ARRAY:
            return NodeType.COLLECTION;
        case OBJECT:
            return NodeType.MAP;
        }

        throw new IllegalStateException();
    }

    @Override
    public Set<String> properties(Object node) {
        return ((JsonObject) node).keySet();
    }

    @Override
    public Object propertyValue(Object property, Object node) {
        return ((JsonObject) node).get(property);
    }

    @Override
    public Collection<? extends Object> items(Object node) {
        return (JsonArray) node;
    }

    @Override
    public String stringValue(Object node) {
        return ((JsonString) node).getString();
    }

    @Override
    public int intValue(Object node) {
        return ((JsonNumber) node).intValueExact();
    }

    @Override
    public long longValue(Object node) {
        return ((JsonNumber) node).longValueExact();
    }

    @Override
    public BigInteger bigIntegerValue(Object node) {
        return ((JsonNumber) node).bigIntegerValueExact();
    }

    @Override
    public double doubleValue(Object node) {
        return ((JsonNumber) node).doubleValue();
    }

    @Override
    public BigDecimal decimalValue(Object node) {
        return ((JsonNumber) node).bigDecimalValue();
    }

    @Override
    public byte[] binaryValue(Object node) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Collection<? extends Object> asCollection(Object node) {
        if (node == null) {
            return Collections.emptyList();
        }

        if (JsonValue.ValueType.ARRAY.equals(((JsonValue) node).getValueType())) {
            return ((JsonArray) node);
        }
        return Collections.singletonList(node);
    }

    @Override
    public Stream<? extends Object> asStream(Object node) {
        if (node == null) {
            return Stream.empty();
        }

        if (JsonValue.ValueType.ARRAY.equals(((JsonValue) node).getValueType())) {
            return ((JsonArray) node).stream();
        }
        return Stream.of(node);
    }

    @Override
    public boolean isNull(Object node) {
        return node == null
                || ValueType.NULL.equals(((JsonValue) node).getValueType());
    }

    @Override
    public boolean isBoolean(Object node) {
        return node != null && (ValueType.TRUE.equals(((JsonValue) node).getValueType())
                || ValueType.FALSE.equals(((JsonValue) node).getValueType()));
    }

    @Override
    public boolean isMap(Object node) {
        return node != null && ValueType.OBJECT.equals(((JsonValue) node).getValueType());
    }

    @Override
    public boolean isCollection(Object node) {
        return node != null && ValueType.ARRAY.equals(((JsonValue) node).getValueType());
    }

    @Override
    public boolean isString(Object node) {
        return node != null && ValueType.STRING.equals(((JsonValue) node).getValueType());
    }

    @Override
    public boolean isNumber(Object node) {
        return node != null && ValueType.NUMBER.equals(((JsonValue) node).getValueType());
    }

    @Override
    public boolean isIntegral(Object node) {
        return isNumber(node) && ((JsonNumber) node).isIntegral();
    }

    @Override
    public boolean isEmpty(Object node) {
        if (isMap(node)) {
            return ((JsonObject) node).isEmpty();
        }
        if (isCollection(node)) {
            return ((JsonArray) node).isEmpty();
        }
        throw new ClassCastException();
    }

    @Override
    public int size(Object node) {
        if (isMap(node)) {
            return ((JsonObject) node).size();
        }
        if (isCollection(node)) {
            return ((JsonArray) node).size();
        }
        throw new ClassCastException();
    }

    public static final JsonValue adapt(Object value, NodeAdapter adapter) {
        
        if (value == null) {
            return null;
//            return JsonValue.NULL;
        }

        final NodeType dataType = adapter.typeOf(value);

        switch (dataType) {

        case STRING:
            return Json.createValue(adapter.stringValue(value));

        case NUMBER:
            return adapter.isIntegral(value)
                    ? Json.createValue(adapter.bigIntegerValue(value))
                    : Json.createValue(adapter.decimalValue(value));

        case TRUE:
            return JsonValue.TRUE;

        case FALSE:
            return JsonValue.FALSE;

        case COLLECTION:
            if (adapter.isEmpty(value)) {
                return JsonValue.EMPTY_JSON_ARRAY;
            }

            final JsonArrayBuilder array = Json.createArrayBuilder();

            adapter.items(value)
                    .stream()
                    .map(item -> adapt(item, adapter))
                    .forEach(array::add);

            return array.build();

        case MAP:
            if (adapter.isEmpty(value)) {
                return JsonValue.EMPTY_JSON_OBJECT;
            }

            final JsonObjectBuilder map = Json.createObjectBuilder();

            for (final String key : adapter.properties(value)
                    .stream()
                    .map(String.class::cast)
                    .collect(Collectors.toSet())) {

                Object entry = adapter.propertyValue(key, value);
                map.add(key, adapt(entry, adapter));
            }

            return map.build();

        case NULL:
            return JsonValue.NULL;
            
        default:
            break;
        }

        throw new IllegalStateException("An unsupported data type '" + dataType + "'.");
    }
}
