package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.DoublePrecisionFloat;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.Map;
import co.nstant.in.cbor.model.SimpleValue;
import co.nstant.in.cbor.model.Special;
import co.nstant.in.cbor.model.SpecialType;
import co.nstant.in.cbor.model.UnicodeString;

public class CborAdapter implements NodeAdapter {

    static final CborAdapter INSTANCE = new CborAdapter();

    public static final CborAdapter instance() {
        return INSTANCE;
    }

    @Override
    public NodeType type(Object node) {

        switch (((DataItem) node).getMajorType()) {

        case MAP:
            return NodeType.MAP;

        case ARRAY:
            return NodeType.COLLECTION;

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

            } else if (SpecialType.IEEE_754_DOUBLE_PRECISION_FLOAT.equals(((Special) node).getSpecialType())) {
                return NodeType.NUMBER;
            }

        default:

        }

        throw new IllegalStateException();
    }

    @Override
    public Collection<DataItem> properties(Object node) {
        return ((Map) node).getKeys();
    }

    @Override
    public DataItem property(Object property, Object node) {
        if (property instanceof DataItem) {
            return ((Map) node).get((DataItem) property);
        }
        if (property instanceof String) {
            return ((Map) node).get(new UnicodeString((String) property));
        }
        throw new ClassCastException();
    }

    @Override
    public List<DataItem> iterable(Object node) {
        return ((Array) node).getDataItems();
    }

    @Override
    public Stream<DataItem> stream(Object node) {
        return ((Array) node).getDataItems().stream();
    }

    @Override
    public String stringValue(Object node) {
        return ((UnicodeString) node).getString();
    }

    @Override
    public int intValue(Object node) {
        return ((co.nstant.in.cbor.model.Number) node).getValue().intValueExact();
    }

    @Override
    public long longValue(Object node) {
        return ((co.nstant.in.cbor.model.Number) node).getValue().longValueExact();
    }

    @Override
    public BigInteger bigIntegerValue(Object node) {
        return ((co.nstant.in.cbor.model.Number) node).getValue();
    }

    @Override
    public double doubleValue(Object node) {
        return ((DoublePrecisionFloat) node).getValue();
    }

    @Override
    public BigDecimal decimalValue(Object node) {
        return BigDecimal.valueOf(((DoublePrecisionFloat) node).getValue());
    }

    @Override
    public byte[] binaryValue(Object node) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Collection<? extends Object> asIterable(Object node) {
        if (node == null) {
            return Collections.emptyList();
        }
        if (MajorType.ARRAY.equals(((DataItem) node).getMajorType())) {
            return ((Array) node).getDataItems();
        }
        return Collections.singletonList(node);
    }

    @Override
    public Stream<? extends Object> asStream(Object node) {
        if (node == null) {
            return Stream.empty();
        }
        if (MajorType.ARRAY.equals(((DataItem) node).getMajorType())) {
            return ((Array) node).getDataItems().stream();
        }
        return Stream.of(node);
    }

    @Override
    public boolean isNull(Object node) {
        return node == null
                || (SpecialType.SIMPLE_VALUE.equals(((Special) node).getSpecialType())
                        && SimpleValue.NULL.equals(node));
    }

    @Override
    public boolean isBoolean(Object node) {
        return node != null
                && SpecialType.SIMPLE_VALUE.equals(((Special) node).getSpecialType())
                && (SimpleValue.TRUE.equals(node)
                        || SimpleValue.FALSE.equals(node));
    }

    @Override
    public boolean isMap(Object node) {
        return node != null && MajorType.MAP == ((DataItem) node).getMajorType();
    }

    @Override
    public boolean isCollection(Object node) {
        return node != null && MajorType.ARRAY == ((DataItem) node).getMajorType();
    }

    @Override
    public boolean isString(Object node) {
        return node != null && MajorType.UNICODE_STRING == ((DataItem) node).getMajorType();
    }

    @Override
    public boolean isNumber(Object node) {
        return node != null
                && (MajorType.UNSIGNED_INTEGER == ((DataItem) node).getMajorType()
                        || MajorType.NEGATIVE_INTEGER == ((DataItem) node).getMajorType()
                        || (SpecialType.SIMPLE_VALUE.equals(((Special) node).getSpecialType())
                                && SpecialType.IEEE_754_DOUBLE_PRECISION_FLOAT.equals(((Special) node).getSpecialType())));
    }

    @Override
    public boolean isIntegral(Object node) {
        return node != null
                && (((DataItem) node).getMajorType().equals(MajorType.UNSIGNED_INTEGER)
                        || ((DataItem) node).getMajorType().equals(MajorType.NEGATIVE_INTEGER));
    }

    @Override
    public boolean isEmpty(Object node) {
        if (isMap(node)) {
            return ((Map) node).getKeys().isEmpty();
        }
        if (isCollection(node)) {
            return ((Array) node).getDataItems().isEmpty();
        }
        throw new ClassCastException();
    }

    @Override
    public int size(Object node) {
        if (isMap(node)) {
            return ((Map) node).getKeys().size();
        }
        if (isCollection(node)) {
            return ((Array) node).getDataItems().size();
        }
        throw new ClassCastException();
    }

}
