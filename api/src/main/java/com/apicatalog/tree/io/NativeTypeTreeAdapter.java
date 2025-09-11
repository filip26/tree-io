package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;

class NativeTypeTreeAdapter implements TreeAdapter {

    final TreeAdapter adapter;

    NativeTypeTreeAdapter(final TreeAdapter adapter) {
        this.adapter = adapter;
    }

    @Override
    public NodeType typeOf(Object node) {
        if (node == null) {
            return NodeType.Null;
        }
        if (node instanceof String) {
            return NodeType.String;
        }
        if (node instanceof Boolean) {
            return ((boolean) node) ? NodeType.True : NodeType.False;
        }
        if (node instanceof Integer
                || node instanceof Long
                || node instanceof BigInteger
                || node instanceof Double
                || node instanceof BigDecimal
                || node instanceof Float) {
            return NodeType.Number;
        }
        if (node instanceof Map) {
            return NodeType.Map;
        }
        if (node instanceof Collection) {
            return NodeType.Collection;
        }
        if (node instanceof byte[]) {
            return NodeType.Binary;
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
    public Object node(Object property, Object node) {
        if (node instanceof Map) {
            return ((Map) node).get(property);
        }
        return adapter.node(property, node);
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
    public boolean isIntegral(Object node) {
        return node instanceof Integer
                || node instanceof Long
                || node instanceof BigInteger
                || adapter.isIntegral(node);
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
        return adapter.asCollection(node);
    }

}
