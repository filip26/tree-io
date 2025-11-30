package com.apicatalog.tree.io.cbor;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.AbstractMap.SimpleEntry;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Stream;

import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeType;
import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeIOException;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.DoublePrecisionFloat;
import co.nstant.in.cbor.model.HalfPrecisionFloat;
import co.nstant.in.cbor.model.Map;
import co.nstant.in.cbor.model.NegativeInteger;
import co.nstant.in.cbor.model.SimpleValue;
import co.nstant.in.cbor.model.SinglePrecisionFloat;
import co.nstant.in.cbor.model.Special;
import co.nstant.in.cbor.model.SpecialType;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;

/**
 * A {@link TreeAdapter} implementation for the
 * {@code co.nstant.in.cbor.model.DataItem} object model, which represents CBOR
 * (Concise Binary Object Representation) data.
 * <p>
 * This adapter allows the generic tree processing framework to read and
 * interpret CBOR structures. It is implemented as a singleton and can be
 * accessed via the {@link #instance()} method.
 * </p>
 * <p>
 * As CBOR supports a wider range of key types than JSON, this adapter correctly
 * reports support for non-string keys in map structures.
 * </p>
 */
public class CborAdapter implements TreeAdapter {

    static final Features FEATURES = new Features(
            // keys
            Set.of(
                    NodeType.SEQUENCE,
                    NodeType.MAP,
                    NodeType.NUMBER,
                    NodeType.STRING,
                    NodeType.BINARY),
            // nodes
            Set.of(
                    NodeType.SEQUENCE,
                    NodeType.MAP,
                    NodeType.NUMBER,
                    NodeType.STRING,
                    NodeType.BINARY,
                    NodeType.FALSE,
                    NodeType.TRUE,
                    NodeType.NULL),
            // capabilities
            null);

    static final CborAdapter INSTANCE = new CborAdapter();

    /**
     * Provides the singleton instance of the {@code CborAdapter}.
     * 
     * @return the singleton instance
     */
    public static CborAdapter instance() {
        return INSTANCE;
    }

    @Override
    public Features features() {
        return FEATURES;
    }

    /**
     * {@inheritDoc}
     * <p>
     * This implementation checks if the node is an instance of {@link DataItem}.
     * </p>
     */
    @Override
    public boolean isNode(Object node) {
        return node != null && (node instanceof DataItem || node instanceof List);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Determines the {@link NodeType} based on the CBOR {@code MajorType} and
     * {@code SpecialType} of the given {@link DataItem}.
     * </p>
     */
    @Override
    public NodeType type(Object node) {

        // root List
        if (node instanceof List) {
            return NodeType.SEQUENCE;
        }

        switch (((DataItem) node).getMajorType()) {

        case MAP:
            return NodeType.MAP;

        case ARRAY:
            return NodeType.SEQUENCE;

        case BYTE_STRING:
            return NodeType.BINARY;

        case UNICODE_STRING:
            return NodeType.STRING;

        case UNSIGNED_INTEGER:
        case NEGATIVE_INTEGER:
            return NodeType.NUMBER;

        case SPECIAL:
            if (SpecialType.SIMPLE_VALUE.equals(((Special) node).getSpecialType())) {
                if (SimpleValue.TRUE.equals(node)) {
                    return NodeType.TRUE;
                }
                if (SimpleValue.FALSE.equals(node)) {
                    return NodeType.FALSE;
                }
                if (SimpleValue.NULL.equals(node)) {
                    return NodeType.NULL;
                }

            } else if (SpecialType.IEEE_754_DOUBLE_PRECISION_FLOAT.equals(((Special) node).getSpecialType())
                    || SpecialType.IEEE_754_HALF_PRECISION_FLOAT.equals(((Special) node).getSpecialType())
                    || SpecialType.IEEE_754_SINGLE_PRECISION_FLOAT.equals(((Special) node).getSpecialType())) {
                return NodeType.NUMBER;
            }

        default:
        }

        throw new IllegalStateException("Unknown or unsupported CBOR data item type, node=" + node);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<DataItem> keys(Object node) {
        return ((Map) node).getKeys();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DataItem property(Object property, Object node) {
        return ((Map) node).get((DataItem) property);
    }

    @Override
    public Object property(Object key, TreeAdapter keyAdapter, Object node) {
        try {
            return ((Map) node).get(CborMaterializer.node(key, keyAdapter));
        } catch (TreeIOException e) {
            throw new IllegalArgumentException(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Iterable<Entry<?, ?>> entries(Object node) {
        return new Iterable<Entry<?, ?>>() {

            final Collection<DataItem> keys = keys(node);

            @Override
            public Iterator<Entry<?, ?>> iterator() {
                return new Iterator<Entry<?, ?>>() {

                    final Iterator<DataItem> kit = keys.iterator();

                    @Override
                    public Entry<?, ?> next() {
                        final DataItem key = kit.next();
                        return new SimpleEntry<>(key, property(key, node));
                    }

                    @Override
                    public boolean hasNext() {
                        return kit.hasNext();
                    }
                };
            }
        };
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Stream<Entry<?, ?>> entryStream(Object node) {
        return keys(node).stream().map(key -> java.util.Map.entry(key, property(key, node)));
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings("unchecked")
    @Override
    public Collection<DataItem> elements(Object node) {
        if (node instanceof List root) {
            return (List<DataItem>) root;
        }
        return ((Array) node).getDataItems();
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings("unchecked")
    @Override
    public Stream<DataItem> elementStream(Object node) {
        if (node instanceof List root) {
            return ((List<DataItem>) root).stream();
        }
        return ((Array) node).getDataItems().stream();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String stringValue(Object node) {
        return ((UnicodeString) node).getString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int intValue(Object node) {
        return ((co.nstant.in.cbor.model.Number) node).getValue().intValueExact();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long longValue(Object node) {
        return ((co.nstant.in.cbor.model.Number) node).getValue().longValueExact();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BigInteger integerValue(Object node) {
        return ((co.nstant.in.cbor.model.Number) node).getValue();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double doubleValue(Object node) {
        if (node instanceof HalfPrecisionFloat halfFloat) {
            return halfFloat.getValue();
        }
        if (node instanceof SinglePrecisionFloat singleFloat) {
            return singleFloat.getValue();
        }
        return ((DoublePrecisionFloat) node).getValue();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BigDecimal decimalValue(Object node) {
        return BigDecimal.valueOf(doubleValue(node));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public byte[] binaryValue(Object node) {
        return ((ByteString) node).getBytes();
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings("unchecked")
    @Override
    public Collection<DataItem> asIterable(Object node) {
        if (node == null) {
            return List.of();
        }
        if (node instanceof Array array) {
            return array.getDataItems();
        }
        if (node instanceof List list) {
            return (List<DataItem>)list;
        }
        return List.of((DataItem) node);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Stream<DataItem> asStream(Object node) {
        if (node == null) {
            return Stream.empty();
        }
        if (node instanceof Array array) {
            return array.getDataItems().stream();
        }
        return Stream.of((DataItem) node);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isNull(Object node) {
        return node instanceof Special special
                && SpecialType.SIMPLE_VALUE == special.getSpecialType()
                && SimpleValue.NULL.equals(node);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isBoolean(Object node) {
        return node instanceof Special special
                && SpecialType.SIMPLE_VALUE == special.getSpecialType()
                && (SimpleValue.TRUE.equals(node) || SimpleValue.FALSE.equals(node));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isMap(Object node) {
        return node instanceof Map;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSequence(Object node) {
        return node instanceof Array || node instanceof List;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isString(Object node) {
        return node instanceof UnicodeString;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isNumber(Object node) {
        return ((node instanceof UnsignedInteger)
                || (node instanceof NegativeInteger)
                || ((node instanceof Special special)
                        && (SpecialType.IEEE_754_DOUBLE_PRECISION_FLOAT == special.getSpecialType()
                                || SpecialType.IEEE_754_HALF_PRECISION_FLOAT == special.getSpecialType()
                                || SpecialType.IEEE_754_SINGLE_PRECISION_FLOAT == special.getSpecialType())));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isIntegral(Object node) {
        return ((node instanceof UnsignedInteger)
                || (node instanceof NegativeInteger));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isBinary(Object node) {
        return node instanceof ByteString;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isEmpty(Object node) {
        if (node instanceof Map map) {
            return map.getKeys().isEmpty();
        }
        if (node instanceof Array array) {
            return array.getDataItems().isEmpty();
        }
        if (node instanceof List list) {
            return list.isEmpty();
        }
        throw new ClassCastException("Node must be a Map or an Array, node=" + node);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int size(Object node) {
        if (node instanceof Map map) {
            return map.getKeys().size();
        }
        if (node instanceof Array array) {
            return array.getDataItems().size();
        }
        if (node instanceof List list) {
            return list.size();
        }        
        throw new ClassCastException("Node must be a Map or an Array, node=" + node);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String asString(Object node) {
        if (node instanceof String stringValue) {
            return stringValue;
        }
        if (node instanceof UnicodeString unicode) {
            return unicode.getString();
        }
        return Objects.toString(node);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BigDecimal asDecimal(Object node) {
        if (isIntegral(node)) {
            return new BigDecimal(integerValue(node));
        }
        return decimalValue(node);
    }
}
