package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Stream;

import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JakartaAdapter implements NodeAdapter {

    static final Set<NodeType> VALUES = new HashSet<>(Arrays.asList(
            NodeType.COLLECTION,
            NodeType.MAP,
            NodeType.NUMBER,
            NodeType.STRING,
            NodeType.FALSE,
            NodeType.TRUE,
            NodeType.NULL));

    static final Set<NodeType> KEYS = Collections.singleton(NodeType.STRING);

    static final JakartaAdapter INSTANCE = new JakartaAdapter();

    public static final JakartaAdapter instance() {
        return INSTANCE;
    }

    @Override
    public boolean isNode(Object node) {
        return node != null && (node instanceof JsonValue // values
                || node instanceof String // keys
        );
    }

    @Override
    public Set<NodeType> nodeTypes() {
        return VALUES;
    }

    @Override
    public NodeType type(Object node) {

        // property keys are strings
        if (node instanceof String) {
            return NodeType.STRING;
        }

        // all other values
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

        throw new IllegalArgumentException();
    }

    @Override
    public Set<NodeType> keyTypes() {
        return KEYS;
    }

    @Override
    public Set<String> keys(Object node) {
        return ((JsonObject) node).keySet();
    }

    @Override
    public Object property(Object property, Object node) {
        return ((JsonObject) node).get(property);
    }
    
    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Iterable<Entry<?, ?>> properties(Object node) {
        return (Iterable)((JsonObject) node).entrySet();
    }

    public Iterable<Entry<?, ?>> properties(Object node, Comparator<Map.Entry<?, ?>> comparator) {
        final ArrayList<Entry<?, ?>> sorted = new ArrayList<>(((JsonObject) node).entrySet());
        Collections.sort(sorted, comparator);
        return sorted;
    }

    @Override
    public Collection<?> iterable(Object node) {
        return (JsonArray) node;
    }

    @Override
    public Stream<?> stream(Object node) {
        return ((JsonArray) node).stream();
    }

    @Override
    public String stringValue(Object node) {
        // keys
        if (node instanceof String) {
            return (String) node;
        }
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
    public Collection<?> asIterable(Object node) {
        if (node == null) {
            return Collections.emptyList();
        }

        if (JsonValue.ValueType.ARRAY.equals(((JsonValue) node).getValueType())) {
            return ((JsonArray) node);
        }
        return Collections.singletonList(node);
    }

    @Override
    public Stream<?> asStream(Object node) {
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
                || ((node instanceof JsonValue)
                        && ValueType.NULL.equals(((JsonValue) node).getValueType()));
    }

    @Override
    public boolean isBoolean(Object node) {
        return node != null
                && (node instanceof JsonValue)
                && (ValueType.TRUE.equals(((JsonValue) node).getValueType())
                        || ValueType.FALSE.equals(((JsonValue) node).getValueType()));
    }

    @Override
    public boolean isMap(Object node) {
        return node != null && (node instanceof JsonObject);
    }

    @Override
    public boolean isCollection(Object node) {
        return node != null && (node instanceof JsonArray);
    }

    @Override
    public boolean isString(Object node) {
        return node != null && (node instanceof String || node instanceof JsonString);
    }

    @Override
    public boolean isNumber(Object node) {
        return node != null && (node instanceof JsonNumber);
    }

    @Override
    public boolean isIntegral(Object node) {
        return isNumber(node) && ((JsonNumber) node).isIntegral();
    }

    @Override
    public boolean isBinary(Object node) {
        return false;
    }

    @Override
    public boolean isEmpty(Object node) {
        if (node instanceof JsonObject) {
            return ((JsonObject) node).isEmpty();
        }
        if (node instanceof JsonArray) {
            return ((JsonArray) node).isEmpty();
        }
        throw new ClassCastException();
    }

    @Override
    public int size(Object node) {
        if (node instanceof JsonObject) {
            return ((JsonObject) node).size();
        }
        if (node instanceof JsonArray) {
            return ((JsonArray) node).size();
        }
        throw new ClassCastException();
    }

    @Override
    public String asString(Object node) {
        if (node instanceof String) {
            return (String) node;
        }
        if (node instanceof JsonString) {
            return ((JsonString) node).getString();
        }
        return node.toString();
    }
    
    @Override
    public BigDecimal asDecimal(Object node) {
        if (node instanceof JsonNumber) {
            return ((JsonNumber)node).bigDecimalValue();
        }
        throw new IllegalArgumentException();
    }
}
