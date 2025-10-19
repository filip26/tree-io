package com.apicatalog.tree.io.java;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.apicatalog.tree.io.Features;
import com.apicatalog.tree.io.NodeFactory;

public class NativeMaterializer2 implements NodeFactory<Object, Object> {

    @Override
    public Features features() {
        return NativeAdapter.FEATURES;
    }

    @Override
    public Object node(String text) {
        return text;
    }

    @Override
    public Object node(BigDecimal number) {
        return number;
    }

    @Override
    public Object node(double number) {
        return number;
    }

    @Override
    public Object node(float number) {
        return number;
    }

    @Override
    public Object node(BigInteger number) {
        return number;
    }

    @Override
    public Object node(long number) {
        return number;
    }

    @Override
    public Object node(int number) {
        return number;
    }

    @Override
    public Object node(boolean value) {
        return value;
    }

    @Override
    public Object node(byte[] binary) {
        return binary;
    }

    @Override
    public Object mapNode(Iterator<Entry<Object, Object>> entries) {

        if (entries == null || !entries.hasNext()) {
            return Collections.emptyMap();
        }

        final Map<Object, Object> map = new LinkedHashMap<>();

        while (entries.hasNext()) {
            final Entry<Object, Object> entry = entries.next();
            map.put(entry.getKey(), entry.getValue());
        }

        return map;
    }

    @Override
    public Object arrayNode(Iterator<Object> elements) {

        if (elements == null || !elements.hasNext()) {
            return Collections.emptyList();
        }

        final List<Object> list = new ArrayList<>();

        while (elements.hasNext()) {
            list.add(elements.next());
        }

        return list;
    }
}
