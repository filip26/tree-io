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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class Jackson2TreeAdapter implements TreeAdapter {

    @Override
    public NodeType typeOf(Object node) {
        if (node instanceof Map) {
            return NodeType.Map;
        }
        if (node instanceof Collection) {
            return NodeType.Collection;
        }
        
        switch (((JsonNode) node).getNodeType()) {
        case NULL:
        case MISSING:
            return NodeType.Null;
        case BOOLEAN:
            return ((JsonNode) node).asBoolean() ? NodeType.True : NodeType.False;
        case STRING:
            return NodeType.String;
        case NUMBER:
            return NodeType.Number;
        case ARRAY:
            return NodeType.Collection;
        case OBJECT:
            return NodeType.Map;
        case BINARY:
            return NodeType.Binary;
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
    public Object node(Object property, Object node) {

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
            return (Collection)node;
        }
        return ((ArrayNode) node).valueStream().collect(Collectors.toList());
    }

    @Override
    public String textValue(Object node) {
        return ((JsonNode) node).textValue();
    }

    @Override
    public boolean isIntegral(Object node) {
        return ((JsonNode) node).isIntegralNumber();
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
            return (Collection)node;
        }
        if (node instanceof ArrayNode) {
            return ((ArrayNode) node).valueStream().collect(Collectors.toList());
        }
        return Collections.singleton(node);
    }
}
