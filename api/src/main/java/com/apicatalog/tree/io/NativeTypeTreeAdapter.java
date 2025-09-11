package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

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
    public Set<? extends Object> properties(Object node) {
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
    public String asString(Object node) {
        if (node instanceof String) {
            return (String) node;
        }

        return adapter.asString(node);
    }

    @Override
    public boolean isDecimal(Object node) {
        return node instanceof Double
                || node instanceof Float
                || node instanceof BigDecimal
                || adapter.isDecimal(node);
    }

    @Override
    public int asInteger(Object node) {
        if (node instanceof Integer) {
            return (Integer) node;
        }
        return adapter.asInteger(node);
    }

    @Override
    public long asLong(Object node) {
        if (node instanceof Long) {
            return (Long) node;
        }
        if (node instanceof Integer) {
            return (Integer) node;
        }
        return adapter.asLong(node);
    }

    @Override
    public BigInteger asBigInteger(Object node) {
        if (node instanceof Long) {
            return BigInteger.valueOf((Long) node);
        }
        if (node instanceof Integer) {
            return BigInteger.valueOf((Integer) node);
        }
        if (node instanceof BigInteger) {
            return (BigInteger) node;
        }
        return adapter.asBigInteger(node);
    }

    @Override
    public double asDouble(Object node) {
        if (node instanceof Double) {
            return (Double) node;
        }
        if (node instanceof Float) {
            return (Float) node;
        }
        return adapter.asDouble(node);
    }

    @Override
    public BigDecimal asBigDecimal(Object node) {
        if (node instanceof Double) {
            return BigDecimal.valueOf((Double) node);
        }
        if (node instanceof Float) {
            return BigDecimal.valueOf((Float) node);
        }
        return adapter.asBigDecimal(node);
    }

    @Override
    public byte[] asByteArray(Object node) {
        if (node instanceof byte[]) {
            return (byte[])node;
        }
        return adapter.asByteArray(node);
    }

}
