package com.apicatalog.tree.io;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeType;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class Jackson2Adapter implements NodeAdapter {

    static final Jackson2Adapter INSTANCE = new Jackson2Adapter();

    public static final Jackson2Adapter instance() {
        return INSTANCE;
    }

    @Override
    public NodeType typeOf(Object node) {
        if (node instanceof Map) {
            return NodeType.MAP;
        }
        if (node instanceof Collection) {
            return NodeType.COLLECTION;
        }

        switch (((JsonNode) node).getNodeType()) {
        case NULL:
        case MISSING:
            return NodeType.NULL;
        case BOOLEAN:
            return ((JsonNode) node).asBoolean() ? NodeType.TRUE : NodeType.FALSE;
        case STRING:
            return NodeType.STRING;
        case NUMBER:
            return NodeType.NUMBER;
        case ARRAY:
            return NodeType.COLLECTION;
        case OBJECT:
            return NodeType.MAP;
        case BINARY:
            return NodeType.BINARY;
        default:
        }

        throw new IllegalStateException();
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Set<String> properties(Object node) {
        if (node instanceof Map) {
            return ((Map) node).keySet();
        }

        Set<String> set = new HashSet<>();
        ((ObjectNode) node).fieldNames().forEachRemaining(set::add);
        return set;
    }

    @SuppressWarnings({ "rawtypes" })
    @Override
    public Object propertyValue(Object property, Object node) {

        if (property instanceof String) {
            throw new IllegalArgumentException();
        }

        Object value = null;

        if (node instanceof Map) {
            value = ((Map) node).get(property);
        } else {
            value = ((ObjectNode) node).get((String) property);
        }

        if (value instanceof ObjectNode) {
            return ((ObjectNode) value).propertyStream().collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        }
        if (value instanceof ArrayNode) {
            return ((ArrayNode) node).valueStream().collect(Collectors.toList());
        }

        return value;
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    @Override
    public Collection<Object> items(Object node) {
        if (node instanceof Collection) {
            return (Collection) node;
        }
        return ((ArrayNode) node).valueStream().collect(Collectors.toList());
    }

    @Override
    public String stringValue(Object node) {
        return ((JsonNode) node).textValue();
    }

    @Override
    public int intValue(Object node) {
        return ((JsonNode) node).intValue();
    }

    @Override
    public long longValue(Object node) {
        return ((JsonNode) node).longValue();
    }

    @Override
    public BigInteger bigIntegerValue(Object node) {
        return ((JsonNode) node).bigIntegerValue();
    }

    @Override
    public double doubleValue(Object node) {
        return ((JsonNode) node).doubleValue();
    }

    @Override
    public BigDecimal decimalValue(Object node) {
        return ((JsonNode) node).decimalValue();
    }

    @Override
    public byte[] binaryValue(Object node) {
        try {
            return ((JsonNode) node).binaryValue();
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    @Override
    public Collection<Object> asCollection(Object node) {
        if (node == null) {
            return Collections.emptySet();
        }
        if (node instanceof Collection) {
            return (Collection) node;
        }
        if (node instanceof ArrayNode) {
            return ((ArrayNode) node).valueStream().collect(Collectors.toList());
        }
        return Collections.singleton(node);
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    @Override
    public Stream<? extends Object> asStream(Object node) {
        if (node == null) {
            return Stream.empty();
        }
        if (node instanceof Collection) {
            return ((Collection) node).stream();
        }
        if (node instanceof ArrayNode) {
            return ((ArrayNode) node).valueStream();
        }
        return Stream.of(node);
    }

    @Override
    public boolean isNull(Object node) {
        return node == null
                || JsonNodeType.NULL.equals(((JsonNode) node).getNodeType())
                || JsonNodeType.MISSING.equals(((JsonNode) node).getNodeType());
    }

    @Override
    public boolean isBoolean(Object node) {
        return node != null && JsonNodeType.BOOLEAN.equals(((JsonNode) node).getNodeType());
    }

    @Override
    public boolean isMap(Object node) {
        return node != null
                && (node instanceof Map
                        || JsonNodeType.OBJECT.equals(((JsonNode) node).getNodeType()));
    }

    @Override
    public boolean isCollection(Object node) {
        return node != null
                && (node instanceof Collection
                        || JsonNodeType.ARRAY.equals(((JsonNode) node).getNodeType()));
    }

    @Override
    public boolean isString(Object node) {
        return node != null && JsonNodeType.STRING.equals(((JsonNode) node).getNodeType());
    }

    @Override
    public boolean isNumber(Object node) {
        return node != null && JsonNodeType.NUMBER.equals(((JsonNode) node).getNodeType());
    }

    @Override
    public boolean isIntegral(Object node) {
        return isNumber(node) && ((JsonNode) node).isIntegralNumber();
    }

    @Override
    public boolean isEmpty(Object node) {
        if (isMap(node)) {
            return ((ObjectNode) node).isEmpty();
        }
        if (isCollection(node)) {
            return ((ArrayNode) node).isEmpty();
        }
        throw new ClassCastException();
    }

    @Override
    public int size(Object node) {
        if (isMap(node)) {
            return ((ObjectNode) node).size();
        }
        if (isCollection(node)) {
            return ((ArrayNode) node).size();
        }
        throw new ClassCastException();
    }
}
