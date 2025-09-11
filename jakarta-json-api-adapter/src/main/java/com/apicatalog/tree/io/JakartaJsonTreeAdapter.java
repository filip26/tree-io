package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Set;

import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public class JakartaJsonTreeAdapter implements TreeAdapter {

    @Override
    public NodeType typeOf(Object node) {
        switch (((JsonValue) node).getValueType()) {
        case NULL:
            return NodeType.Null;
        case TRUE:
            return NodeType.True;
        case FALSE:
            return NodeType.False;
        case STRING:
            return NodeType.String;
        case NUMBER:
            return NodeType.Number;
        case ARRAY:
            return NodeType.Collection;
        case OBJECT:
            return NodeType.Map;
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
    public String asString(Object node) {
        return ((JsonString) node).getString();
    }

    @Override
    public boolean isDecimal(Object node) {
        return !((JsonNumber) node).isIntegral();
    }

    @Override
    public int asInteger(Object node) {
        return ((JsonNumber) node).intValueExact();
    }

    @Override
    public long asLong(Object node) {
        return ((JsonNumber) node).longValueExact();
    }

    @Override
    public BigInteger asBigInteger(Object node) {
        return ((JsonNumber) node).bigIntegerValueExact();
    }

    @Override
    public double asDouble(Object node) {
        return ((JsonNumber) node).doubleValue();
    }

    @Override
    public BigDecimal asBigDecimal(Object node) {
        return ((JsonNumber) node).bigDecimalValue();
    }

    @Override
    public byte[] asByteArray(Object node) {
        throw new UnsupportedOperationException();
    }
}
