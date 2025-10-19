package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.stream.Stream;

public class PolyAdapter implements NodeAdapter {

    protected final NodeAdapter adapter;
    protected Collection<NodeAdapter> polyAdapters;

    public PolyAdapter(NodeAdapter adapter) {
        this.adapter = adapter;
        this.polyAdapters = null;
    }

    @Override
    public Features features() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public boolean isNode(Object node) {
        return apply(node, a -> a.isNode(node));
    }

    @Override
    public NodeType type(Object node) {
        return apply(node, a -> a.type(node));
    }

    @Override
    public boolean isBoolean(Object node) {
        return apply(node, a -> a.isBoolean(node));
    }

    @Override
    public boolean isBinary(Object node) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public int size(Object node) {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public boolean isMap(Object node) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public Collection<?> keys(Object node) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Object property(Object key, Object node) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Iterable<Entry<?, ?>> entries(Object node) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Stream<Entry<?, ?>> entryStream(Object node) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public boolean isCollection(Object node) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean isList(Object node) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean isSet(Object node) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public Iterable<?> elements(Object node) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Stream<?> elementStream(Object node) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public boolean isString(Object node) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public String stringValue(Object node) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public boolean isNumber(Object node) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean isIntegral(Object node) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public int intValue(Object node) {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public long longValue(Object node) {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public BigInteger bigIntegerValue(Object node) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public double doubleValue(Object node) {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public BigDecimal decimalValue(Object node) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public byte[] binaryValue(Object node) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Iterable<?> asIterable(Object node) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Stream<?> asStream(Object node) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String asString(Object node) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public BigDecimal asDecimal(Object node) {
        // TODO Auto-generated method stub
        return null;
    }

    <T> T apply(Object node, Function<NodeAdapter, T> fnc) {
        return adapter.isNode(node)
                ? fnc.apply(adapter)
                : polyAdapters != null
                        ? polyAdapters.stream().filter(a -> a.isNode(node)).findFirst()
                                .map(fnc)
                                .orElse(fnc.apply(adapter))
                        : fnc.apply(adapter);
    }

    @Override
    public Object property(Object key, NodeAdapter keyAdapter, Object node) {
        // TODO Auto-generated method stub
        return null;
    }
}
