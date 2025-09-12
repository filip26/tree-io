package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Stream;

import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JakartaJsonTreeAdapter implements NodeAdapter {

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
    public Object node(Object property, Object node) {
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
    public boolean isEmptyMap(Object node) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean isCollection(Object node) {
        return node != null && ValueType.ARRAY.equals(((JsonValue) node).getValueType());
    }

    @Override
    public boolean isEmptyCollection(Object node) {
        // TODO Auto-generated method stub
        return false;
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
}
