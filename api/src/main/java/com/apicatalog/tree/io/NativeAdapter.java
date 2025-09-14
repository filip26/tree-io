package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class NativeAdapter implements NodeAdapter {

    protected static final NativeAdapter INSTANCE = new NativeAdapter();

    public static final NativeAdapter instance() {
        return INSTANCE;
    }

    @Override
    public NodeType type(Object node) {
        if (node == null) {
            return NodeType.NULL;
        }
        if (node instanceof String) {
            return NodeType.STRING;
        }
        if (node instanceof Boolean) {
            return ((boolean) node) ? NodeType.TRUE : NodeType.FALSE;
        }
        if (node instanceof Integer
                || node instanceof Long
                || node instanceof BigInteger
                || node instanceof Double
                || node instanceof BigDecimal
                || node instanceof Float) {
            return NodeType.NUMBER;
        }
        if (node instanceof Map) {
            return NodeType.MAP;
        }
        if (node instanceof Collection) {
            return NodeType.COLLECTION;
        }
        if (node instanceof byte[]) {
            return NodeType.BINARY;
        }

        throw new IllegalArgumentException();
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Collection<? extends Object> properties(Object node) {
        return ((Map) node).keySet();
    }

    @SuppressWarnings("rawtypes")
    @Override
    public Object property(Object property, Object node) {
        return ((Map) node).get(property);
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Iterable<? extends Object> iterable(Object node) {
        return ((Iterable) node);
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Stream<? extends Object> stream(Object node) {
        return ((Collection) node).stream();
    }

    @Override
    public String stringValue(Object node) {
        return (String) node;
    }

    @Override
    public int intValue(Object node) {
        return (Integer) node;
    }

    @Override
    public long longValue(Object node) {
        if (node instanceof Long) {
            return (Long) node;
        }
        if (node instanceof Integer) {
            return (Integer) node;
        }
        throw new IllegalArgumentException();
    }

    @Override
    public BigInteger bigIntegerValue(Object node) {
        if (node instanceof Long) {
            return BigInteger.valueOf((Long) node);
        }
        if (node instanceof Integer) {
            return BigInteger.valueOf((Integer) node);
        }
        if (node instanceof BigInteger) {
            return (BigInteger) node;
        }
        throw new IllegalArgumentException();
    }

    @Override
    public double doubleValue(Object node) {
        if (node instanceof Double) {
            return (Double) node;
        }
        if (node instanceof Float) {
            return (Float) node;
        }
        throw new IllegalArgumentException();
    }

    @Override
    public BigDecimal decimalValue(Object node) {
        if (node instanceof Double) {
            return BigDecimal.valueOf((Double) node);
        }
        if (node instanceof Float) {
            return BigDecimal.valueOf((Float) node);
        }
        throw new IllegalArgumentException();
    }

    @Override
    public byte[] binaryValue(Object node) {
        return (byte[]) node;
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    @Override
    public Iterable<? extends Object> asIterable(Object node) {
        if (node == null) {
            return Collections.emptyList();
        }
        if (node instanceof Collection) {
            return (Collection) node;
        }
        if (node instanceof Stream) {
            return ((Stream<Object>) node).collect(Collectors.toList());
        }
        throw new IllegalArgumentException();
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    @Override
    public Stream<? extends Object> asStream(Object node) {
        if (node == null) {
            return Stream.empty();
        }
        if (node instanceof Stream) {
            return (Stream) node;
        }
        if (node instanceof Collection) {
            return ((Collection) node).stream();
        }
        throw new IllegalArgumentException();
    }

    @SuppressWarnings("rawtypes")
    @Override
    public boolean isNull(Object node) {
        return node == null || ((node instanceof Optional) && !((Optional) node).isPresent());
    }

    @Override
    public boolean isBoolean(Object node) {
        return node != null && node instanceof Boolean;
    }

    @Override
    public boolean isMap(Object node) {
        return node != null && node instanceof Map;
    }

    @Override
    public boolean isCollection(Object node) {
        return node != null && node instanceof Collection;
    }

    @Override
    public boolean isString(Object node) {
        return node != null && node instanceof String;
    }

    @Override
    public boolean isNumber(Object node) {
        return node != null
                && (node instanceof Integer
                        || node instanceof Long
                        || node instanceof BigInteger
                        || node instanceof Double
                        || node instanceof BigDecimal
                        || node instanceof Float);
    }

    @Override
    public boolean isIntegral(Object node) {
        return node != null
                && (node instanceof Integer
                        || node instanceof Long
                        || node instanceof BigInteger);
    }

    @Override
    public boolean isBinary(Object node) {
        return node != null && node instanceof byte[];
    }

    @SuppressWarnings("rawtypes")
    @Override
    public boolean isEmpty(Object node) {
        if (node instanceof Map) {
            return ((Map) node).isEmpty();
        }
        if (node instanceof Collection) {
            return ((Collection) node).isEmpty();
        }
        throw new IllegalArgumentException();
    }

    @SuppressWarnings("rawtypes")
    @Override
    public int size(Object node) {
        if (node instanceof Map) {
            return ((Map) node).size();
        }
        if (node instanceof Collection) {
            return ((Collection) node).size();
        }
        throw new IllegalArgumentException();
    }

    @Override
    public String asString(Object node) {
        if (node instanceof String) {
            return (String) node;
        }
        return Objects.toString(node);
    }

    public static final Object adapt(Object value, NodeAdapter adapter) {

        if (value == null) {
            return null;
        }

        final NodeType dataType = adapter.type(value);

        switch (dataType) {

        case STRING:
            return adapter.stringValue(value);

        case NUMBER:
            return adapter.isIntegral(value)
                    ? adapter.bigIntegerValue(value)
                    : adapter.decimalValue(value);

        case TRUE:
            return true;

        case FALSE:
            return false;

        case COLLECTION:
            if (adapter.isEmpty(value)) {
                return Collections.emptyList();
            }

            return adapter.stream(value)
                    .map(item -> adapt(item, adapter))
                    .collect(Collectors.toList());

        case MAP:
            if (adapter.isEmpty(value)) {
                return Collections.emptyMap();
            }

            return adapter.properties(value)
                    .stream()
                    .reduce(new LinkedHashMap<>(adapter.size(value)),
                            (map, key) -> {
                                Object entry = adapter.property(key, value);
                                map.put(key, adapt(entry, adapter));
                                return map;
                            },
                            (map1, map2) -> { // combiner (parallel streams)
                                map1.putAll(map2);
                                return map1;
                            });

        case BINARY:
            return adapter.binaryValue(value);

        case NULL:
            return null;

        default:
            break;
        }

        throw new IllegalStateException("An unsupported data type '" + dataType + "'.");
    }
}
