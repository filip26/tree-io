package com.apicatalog.tree.io;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
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
    public boolean isNode(Object node) {
        return node != null && node instanceof JsonNode;
    }

    @Override
    public NodeType type(Object node) {

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

        throw new IllegalArgumentException();
    }

    @Override
    public Set<String> properties(Object node) {
        return ((ObjectNode) node).propertyStream().map(Map.Entry::getKey).collect(Collectors.toSet());
    }

    @Override
    public JsonNode property(Object property, Object node) {
        return ((ObjectNode) node).get((String) property);
    }

    @Override
    public Collection<JsonNode> iterable(Object node) {
        return ((ArrayNode) node).valueStream().collect(Collectors.toList());
    }

    @Override
    public Stream<JsonNode> stream(Object node) {
        return ((ArrayNode) node).valueStream();
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
    public Collection<Object> asIterable(Object node) {
        if (node == null) {
            return Collections.emptySet();
        }
        if (node instanceof Collection) {
            return (Collection) node;
        }
        if (node instanceof ArrayNode) {
            return ((ArrayNode) node).valueStream().collect(Collectors.toList());
        }
        if (node instanceof Stream) {
            return ((Stream<Object>) node).collect(Collectors.toList());
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
        if (node instanceof Stream) {
            return (Stream) node;
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
        return node != null && JsonNodeType.OBJECT.equals(((JsonNode) node).getNodeType());
    }

    @Override
    public boolean isCollection(Object node) {
        return node != null && JsonNodeType.ARRAY.equals(((JsonNode) node).getNodeType());
    }

    @Override
    public boolean isString(Object node) {
        return node != null
                && (node instanceof String
                        || JsonNodeType.STRING.equals(((JsonNode) node).getNodeType()));
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
    public boolean isBinary(Object node) {
        return node != null && JsonNodeType.BINARY.equals(((JsonNode) node).getNodeType());
    }

    @Override
    public boolean isEmpty(Object node) {
        Objects.requireNonNull(node);

        if (node instanceof ObjectNode) {
            return ((ObjectNode) node).isEmpty();
        }
        if (node instanceof ArrayNode) {
            return ((ArrayNode) node).isEmpty();
        }
        throw new ClassCastException();
    }

    @Override
    public int size(Object node) {
        Objects.requireNonNull(node);

        if (node instanceof ObjectNode) {
            return ((ObjectNode) node).size();
        }
        if (node instanceof ArrayNode) {
            return ((ArrayNode) node).size();
        }
        throw new ClassCastException();
    }

    @Override
    public String asString(Object node) {
        if (node instanceof String) {
            return (String) node;
        }
        if (node instanceof JsonNode) {
            return ((JsonNode) node).asText();
        }
        return node.toString();
    }
}
