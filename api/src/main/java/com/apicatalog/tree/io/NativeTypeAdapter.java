package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class NativeTypeAdapter implements NodeAdapter {

    final NodeAdapter adapter;

    NativeTypeAdapter(final NodeAdapter adapter) {
        this.adapter = adapter;
    }
    
    @Override
    public NodeType typeOf(Object node) {
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

        return adapter.typeOf(node);
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Collection<? extends Object> properties(Object node) {
        if (node instanceof Map) {
            return ((Map) node).keySet();
        }
        return adapter.properties(node);
    }

    @SuppressWarnings("rawtypes")
    @Override
    public Object propertyValue(Object property, Object node) {
        if (node instanceof Map) {
            return ((Map) node).get(property);
        }
        return adapter.propertyValue(property, node);
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Collection<? extends Object> items(Object node) {
        if (node instanceof Collection) {
            return ((Collection) node);
        }
        return adapter.items(node);
    }

    @Override
    public String stringValue(Object node) {
        if (node instanceof String) {
            return (String) node;
        }

        return adapter.stringValue(node);
    }

    @Override
    public int intValue(Object node) {
        if (node instanceof Integer) {
            return (Integer) node;
        }
        return adapter.intValue(node);
    }

    @Override
    public long longValue(Object node) {
        if (node instanceof Long) {
            return (Long) node;
        }
        if (node instanceof Integer) {
            return (Integer) node;
        }
        return adapter.longValue(node);
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
        return adapter.bigIntegerValue(node);
    }

    @Override
    public double doubleValue(Object node) {
        if (node instanceof Double) {
            return (Double) node;
        }
        if (node instanceof Float) {
            return (Float) node;
        }
        return adapter.doubleValue(node);
    }

    @Override
    public BigDecimal decimalValue(Object node) {
        if (node instanceof Double) {
            return BigDecimal.valueOf((Double) node);
        }
        if (node instanceof Float) {
            return BigDecimal.valueOf((Float) node);
        }
        return adapter.decimalValue(node);
    }

    @Override
    public byte[] binaryValue(Object node) {
        if (node instanceof byte[]) {
            return (byte[]) node;
        }
        return adapter.binaryValue(node);
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    @Override
    public Collection<? extends Object> asCollection(Object node) {
        if (node == null) {
            return Collections.emptyList();
        }
        if (node instanceof Collection) {
            return (Collection) node;
        }
        if (node instanceof Stream) {
            return ((Stream<Object>) node).collect(Collectors.toList());
        }        
        return adapter.asCollection(node);
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
        return adapter.asStream(node);
    }

    @Override
    public boolean isNull(Object node) {
        return node == null || adapter.isNull(node);
    }

    @Override
    public boolean isBoolean(Object node) {
        return node != null && (node instanceof Boolean || adapter.isBoolean(node));
    }

    @Override
    public boolean isMap(Object node) {
        return node != null && (node instanceof Map || adapter.isMap(node));
    }

    @Override
    public boolean isCollection(Object node) {
        return node != null && (node instanceof Collection || adapter.isCollection(node));
    }

    @Override
    public boolean isString(Object node) {
        return node != null && (node instanceof String || adapter.isString(node));
    }

    @Override
    public boolean isNumber(Object node) {
        return node != null
                && (node instanceof Integer
                        || node instanceof Long
                        || node instanceof BigInteger
                        || node instanceof Double
                        || node instanceof BigDecimal
                        || node instanceof Float
                        || adapter.isNumber(node));
    }

    @Override
    public boolean isIntegral(Object node) {
        return node != null
                && (node instanceof Integer
                        || node instanceof Long
                        || node instanceof BigInteger
                        || adapter.isIntegral(node));
    }
    
    @SuppressWarnings("rawtypes")
    @Override
    public boolean isEmpty(Object node) {
        if (node == null) {
            return false;
        }
        if (node instanceof Map) {
            return ((Map) node).isEmpty();
        }
        if (node instanceof Collection) {
            return ((Collection) node).isEmpty();
        }
        return adapter.isEmpty(node);
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
        return adapter.size(node);
    }

}
