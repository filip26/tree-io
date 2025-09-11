package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.DoublePrecisionFloat;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.Map;
import co.nstant.in.cbor.model.SimpleValue;
import co.nstant.in.cbor.model.Special;
import co.nstant.in.cbor.model.SpecialType;
import co.nstant.in.cbor.model.UnicodeString;

public class CborTreeAdapter implements TreeAdapter {

    @Override
    public NodeType typeOf(Object node) {

        switch (((DataItem) node).getMajorType()) {

        case MAP:
            return NodeType.Map;

        case ARRAY:
            return NodeType.Collection;

        case BYTE_STRING:
            return NodeType.Binary;

        case UNICODE_STRING:
            return NodeType.String;

        case UNSIGNED_INTEGER:
        case NEGATIVE_INTEGER:
            return NodeType.Number;

        case SPECIAL:
            if (SpecialType.SIMPLE_VALUE.equals(((Special) node).getSpecialType())) {
                if (SimpleValue.TRUE.equals(node)) {
                    return NodeType.True;
                }
                if (SimpleValue.FALSE.equals(node)) {
                    return NodeType.False;
                }
                if (SimpleValue.NULL.equals(node)) {
                    return NodeType.Null;
                }

            } else if (SpecialType.IEEE_754_DOUBLE_PRECISION_FLOAT.equals(((Special) node).getSpecialType())) {
                return NodeType.Number;
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
    public DataItem node(Object property, Object node) {
        return ((Map) node).get((DataItem) property);
    }

    @Override
    public List<DataItem> items(Object node) {
        return ((Array) node).getDataItems();
    }

    @Override
    public String stringValue(Object node) {
        return ((UnicodeString) node).getString();
    }

    @Override
    public boolean isIntegral(Object node) {
        return ((DataItem) node).getMajorType().equals(MajorType.UNSIGNED_INTEGER)
                || ((DataItem) node).getMajorType().equals(MajorType.NEGATIVE_INTEGER);
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
    public Collection<? extends Object> asCollection(Object node) {
        if (node == null) {
            return Collections.emptyList();
        }

        if (MajorType.ARRAY.equals(((DataItem) node).getMajorType())) {
            return ((Array) node).getDataItems();
        }

        return Collections.singletonList(node);
    }
}
