package com.apicatalog.tree.io.morph;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Map.Entry;
import java.util.stream.Stream;

import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeType;
import com.apicatalog.tree.io.TreeAdapter;

public class MorphAdapter implements TreeAdapter {

    protected final TreeAdapter base;
    
    public MorphAdapter(TreeAdapter base) {
        this.base = base;
    }

    @Override
    public boolean isEqualTo(TreeAdapter adapter) {
        return adapter instanceof MorphAdapter morph
                && base.isEqualTo(morph.base);
    }

    @Override
    public Features features() {
        return base.features();
    }

    @Override
    public boolean isNode(Object node) {
        return node instanceof MapOverlay || base.isNode(node);
    }

    @Override
    public NodeType type(Object node) {
        if (node instanceof MapOverlay) {
            return NodeType.MAP;
        }
        return base.type(node);
    }

    @Override
    public boolean isBoolean(Object node) {
        return base.isBoolean(node);
    }

    @Override
    public boolean isBinary(Object node) {
        return base.isBinary(node);
    }

    @Override
    public int size(Object node) {
        if (node instanceof MapOverlay map) {
            return map.keys(base).size();
        }
        return base.size(node);
    }

    @Override
    public boolean isMap(Object node) {
        return node instanceof MapOverlay || base.isMap(node);
    }

    @Override
    public Collection<?> keys(Object node) {
        if (node instanceof MapOverlay map) {
            return map.keys(base);
        }
        return base.keys(node);
    }
    
    @Override
    public Stream<?> keyStream(Object node) {
        if (node instanceof MapOverlay map) {
            return map.keyStream(base);
        }
        return base.keyStream(node);
    }

    @Override
    public Object property(Object key, Object node) {
        if (node instanceof MapOverlay map) {
            return map.property(key, base);
        }
        return base.property(key, node);
    }

    @Override
    public Object property(Object key, TreeAdapter keyAdapter, Object node) {
        if (node instanceof MapOverlay map) {
            return map.property(key, keyAdapter, base);
        }
        return base.property(key, keyAdapter, node);
    }

    @Override
    public Iterable<Entry<?, ?>> entries(Object node) {
        if (node instanceof MapOverlay map) {
            return map.entries(base);
        }
        return base.entries(node);
    }

    @Override
    public Stream<Entry<?, ?>> entryStream(Object node) {
        if (node instanceof MapOverlay map) {
            return map.entryStream(base);
        }
        return base.entryStream(node);
    }

    @Override
    public boolean isSequence(Object node) {
        return base.isSequence(node);
    }

    @Override
    public Iterable<?> elements(Object node) {
        return base.elements(node);
    }

    @Override
    public Stream<?> elementStream(Object node) {
        return base.elementStream(node);
    }

    @Override
    public boolean isString(Object node) {
        return base.isString(node);
    }

    @Override
    public String stringValue(Object node) {
        return base.stringValue(node);
    }

    @Override
    public boolean isNumber(Object node) {
        return base.isNumber(node);
    }

    @Override
    public boolean isIntegral(Object node) {
        return base.isIntegral(node);
    }

    @Override
    public int intValue(Object node) {
        return base.intValue(node);
    }

    @Override
    public long longValue(Object node) {
        return base.longValue(node);
    }

    @Override
    public BigInteger integerValue(Object node) {
        return base.integerValue(node);
    }

    @Override
    public double doubleValue(Object node) {
        return base.doubleValue(node);
    }

    @Override
    public BigDecimal decimalValue(Object node) {
        return base.decimalValue(node);
    }

    @Override
    public byte[] binaryValue(Object node) {
        return base.binaryValue(node);
    }

    @Override
    public Iterable<?> asIterable(Object node) {
        return base.asIterable(node);
    }

    @Override
    public Stream<?> asStream(Object node) {
        return base.asStream(node);
    }

    @Override
    public String asString(Object node) {
        return base.asString(node);
    }

    @Override
    public BigDecimal asDecimal(Object node) {
        return base.asDecimal(node);
    }
}
