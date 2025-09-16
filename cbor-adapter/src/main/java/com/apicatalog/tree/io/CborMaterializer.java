package com.apicatalog.tree.io;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.Deque;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.DoublePrecisionFloat;
import co.nstant.in.cbor.model.Map;
import co.nstant.in.cbor.model.NegativeInteger;
import co.nstant.in.cbor.model.SimpleValue;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;

public class CborMaterializer extends NodeGenerator {

    protected DataItem value;
    protected final Deque<Object> builders;

    public CborMaterializer() {
        super(new ArrayDeque<>(), PropertyKeyPolicy.Any);
        this.value = null;
        this.builders = new ArrayDeque<>();
    }

    public void node(Object node, NodeAdapter adapter) {
        // reset
        this.value = null;
        this.builders.clear();

        try {
            super.node(node, adapter);
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }

    @Override
    protected void scalar(Object node) {

        switch (nodeContext) {
        case PROPERTY_KEY:
            builders.push(toScalar(node));
            return;

        case PROPERTY_VALUE:
            DataItem key = (DataItem) builders.pop();
            ((Map) builders.peek()).put(key, toScalar(node));
            return;

        case COLLECTION_ELEMENT:
            ((Array) builders.peek()).add(toScalar(node));
            return;

        case ROOT:
            value = toScalar(node);
            return;

        default:
            throw new IllegalStateException();
        }
    }

    protected DataItem toScalar(Object value) {
        switch (adapter.type(value)) {
        case STRING:
            return new UnicodeString(adapter.stringValue(value));

        case NUMBER:
            if (adapter.isIntegral(value)) {
                final BigInteger integer = adapter.bigIntegerValue(value);
                switch (integer.signum()) {
                case -1:
                    return new NegativeInteger(integer);
                case 0:
                    return new UnsignedInteger(BigInteger.ZERO);
                case 1:
                    return new UnsignedInteger(integer);
                }
            }
            return new DoublePrecisionFloat(adapter.doubleValue(value));

        case TRUE:
            return SimpleValue.TRUE;

        case FALSE:
            return SimpleValue.FALSE;

        case BINARY:
            return new ByteString(adapter.binaryValue(value));

        case NULL:
            return SimpleValue.NULL;

        default:
            throw new IllegalStateException();
        }
    }

    @Override
    protected void beginMap() {
        builders.push(new Map());
    }

    @Override
    protected void beginCollection() {
        builders.push(new Array());
    }

    @Override
    protected void end() {
        Object builder = builders.pop();

        if (builder instanceof Map) {
            value = (Map) builder;

        } else if (builder instanceof Array) {
            value = (Array) builder;

        } else {
            throw new IllegalStateException();
        }

        if (!builders.isEmpty()) {
            if (builders.peek() instanceof DataItem) {
                DataItem key = (DataItem) builders.pop();
                ((Map) builders.peek()).put(key, value);

            } else if (builders.peek() instanceof Array) {
                ((Array) builders.peek()).add(value);

            } else if (builders.peek() instanceof Map) {
                builders.push(value);

            } else {
                throw new IllegalStateException();
            }
        }
    }

    public DataItem value() {
        return value;
    }
}
