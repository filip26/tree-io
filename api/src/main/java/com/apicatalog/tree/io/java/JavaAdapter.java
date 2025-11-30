package com.apicatalog.tree.io.java;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.apicatalog.tree.io.Tree;
import com.apicatalog.tree.io.Tree.Capability;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeType;
import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeIOException;

public final class JavaAdapter implements TreeAdapter {

    static final Features FEATURES = new Features(
            // keys
            Set.of(
                    NodeType.COLLECTION,
                    NodeType.MAP,
                    NodeType.NUMBER,
                    NodeType.STRING),
            // nodes
            Set.of(
                    NodeType.COLLECTION,
                    NodeType.MAP,
                    NodeType.NUMBER,
                    NodeType.STRING,
                    NodeType.BINARY,
                    NodeType.FALSE,
                    NodeType.TRUE,
                    NodeType.NULL,
                    NodeType.TREE_IO),
            // capabilities
            Set.of(Capability.SCALAR_OBJECT_EQUALS));

    private static final JavaAdapter INSTANCE = new JavaAdapter();

    private JavaAdapter() {
        // static
    }
    
    public static final JavaAdapter instance() {
        return INSTANCE;
    }

    @Override
    public Features features() {
        return FEATURES;
    }

    @Override
    public boolean isNode(Object node) {
        return node == null
                || node instanceof String
                || node instanceof Boolean
                || node instanceof Integer
                || node instanceof Long
                || node instanceof BigInteger
                || node instanceof Double
                || node instanceof BigDecimal
                || node instanceof Float
                || node instanceof Map
                || node instanceof Collection
                || node instanceof byte[]
                || node instanceof Tree;
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
        if (node instanceof Tree) {
            return NodeType.TREE_IO;
        }

        throw new IllegalArgumentException("Unrecognized node type='" + node.getClass() + ", value=" + node + "'.");
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Collection<? extends Object> keys(Object node) {
        return ((Map) node).keySet();
    }

    @SuppressWarnings("rawtypes")
    @Override
    public Object property(Object property, Object node) {
        return ((Map) node).get(property);
    }

    @SuppressWarnings("rawtypes")
    @Override
    public Object property(Object key, TreeAdapter keyAdapter, Object node) {
        try {
            return ((Map) node).get(JavaMaterializer.node(key, keyAdapter));

        } catch (TreeIOException e) {
            throw new IllegalStateException(e);
        }
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Iterable<Entry<?, ?>> entries(Object node) {
        return ((Map) node).entrySet();
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Stream<Entry<?, ?>> entryStream(Object node) {
        return ((Map) node).entrySet().stream();
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Iterable<? extends Object> elements(Object node) {
        return ((Iterable) node);
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Stream<? extends Object> elementStream(Object node) {
        return ((Collection) node).stream();
    }

    @Override
    public String stringValue(Object node) {
        return (String) node;
    }

    @Override
    public int intValue(Object node) {
        if (node instanceof Long number) {
            return Math.toIntExact(number);
        }
        if (node instanceof Integer number) {
            return number;
        }
        if (node instanceof BigInteger number) {
            return number.intValueExact();
        }
        return (int) node;
    }

    @Override
    public long longValue(Object node) {
        if (node instanceof Long number) {
            return number;
        }
        if (node instanceof Integer number) {
            return number;
        }
        if (node instanceof BigInteger number) {
            return number.longValueExact();
        }
        return (long) node;
    }

    @Override
    public BigInteger integerValue(Object node) {
        if (node instanceof Long number) {
            return BigInteger.valueOf(number);
        }
        if (node instanceof Integer number) {
            return BigInteger.valueOf(number);
        }
        if (node instanceof BigInteger number) {
            return number;
        }
        throw new IllegalArgumentException();
    }

    @Override
    public double doubleValue(Object node) {
        if (node instanceof BigDecimal number) {
            return number.doubleValue();
        }
        if (node instanceof Double number) {
            return number;
        }
        if (node instanceof Float number) {
            return number;
        }
        throw new IllegalArgumentException();
    }

    @Override
    public BigDecimal decimalValue(Object node) {
        if (node instanceof BigDecimal) {
            return (BigDecimal) node;
        }
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
            return List.of();
        }
        if (node instanceof Collection) {
            return (Collection) node;
        }
        if (node instanceof Stream) {
            return ((Stream<Object>) node).collect(Collectors.toList());
        }
        return List.of(node);
    }

    @Override
    public Stream<? extends Object> asStream(Object node) {
        if (node == null) {
            return Stream.empty();
        }
        if (node instanceof Stream<?> stream) {
            return stream;
        }
        if (node instanceof Collection<?> collection) {
            return collection.stream();
        }
        return Stream.of(node);
    }

    @Override
    public boolean isNull(Object node) {
        return node == null || (node instanceof Optional opt && opt.isEmpty());
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
    public boolean isList(Object node) {
        return node != null && node instanceof List;
    }

    @Override
    public boolean isSet(Object node) {
        return node != null && node instanceof Set;
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

    @Override
    public boolean isEmpty(Object node) {
        if (node instanceof Map map) {
            return map.isEmpty();
        }
        if (node instanceof Collection col) {
            return col.isEmpty();
        }
        throw new IllegalArgumentException();
    }

    @Override
    public int size(Object node) {
        if (node instanceof Map map) {
            return map.size();
        }
        if (node instanceof Collection col) {
            return col.size();
        }
        throw new IllegalArgumentException();
    }

    @Override
    public String asString(Object node) {
        if (node instanceof String stringValue) {
            return stringValue;
        }
        return Objects.toString(node);
    }

    @Override
    public BigDecimal asDecimal(Object node) {
        if (node instanceof BigDecimal number) {
            return number;
        }
        if (node instanceof Double number) {
            return BigDecimal.valueOf(number);
        }
        if (node instanceof Float number) {
            return BigDecimal.valueOf(number);
        }
        if (node instanceof Integer number) {
            return BigDecimal.valueOf(number);
        }
        if (node instanceof Long number) {
            return BigDecimal.valueOf(number);
        }
        if (node instanceof BigInteger number) {
            return BigDecimal.valueOf(number.longValueExact());
        }
        throw new IllegalArgumentException();
    }

    @Deprecated
    public static final Object adapt(Object value, TreeAdapter adapter) {

        if (value == null) {
            return null;
        }

        final NodeType dataType = adapter.type(value);

        switch (dataType) {

        case STRING:
            return adapter.stringValue(value);

        case NUMBER:
            return adapter.isIntegral(value)
                    ? adapter.integerValue(value)
                    : adapter.decimalValue(value);

        case TRUE:
            return true;

        case FALSE:
            return false;

        case COLLECTION:
            if (adapter.isEmpty(value)) {
                return List.of();
            }

            return adapter
                    .elementStream(value)
                    .map(item -> adapt(item, adapter))
                    .collect(Collectors.toList());

        case MAP:
            if (adapter.isEmpty(value)) {
                return Map.of();
            }

            return adapter
                    .entryStream(value)
                    .reduce(
                            new LinkedHashMap<>(),
                            (map, entry) -> {
                                map.put(entry.getKey(), adapt(entry.getValue(), adapter));
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

    public static Collection<?> asCollection(Object node) {
        return node instanceof Collection col
                ? col
                : node != null
                        ? List.of(node)
                        : List.of();
    }
}
