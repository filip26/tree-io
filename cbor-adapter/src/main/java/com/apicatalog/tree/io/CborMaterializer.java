package com.apicatalog.tree.io;

import java.io.IOException;
import java.math.BigDecimal;
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

public class CborMaterializer extends NodeVisitor implements NodeGenerator {

    protected DataItem cbor;
    protected final Deque<Object> builders;

    public CborMaterializer() {
        super(new ArrayDeque<>(), null);
        this.builders = new ArrayDeque<>();
        this.cbor = null;
    }

    public DataItem node(Object node, NodeAdapter adapter) throws IOException {
        root(node, adapter).traverse(this);
        return cbor;
    }

    @Override
    public NodeVisitor reset() {
        this.cbor = null;
        this.builders.clear();

        return super.reset();
    }

    @Override
    public void beginMap() {
        builders.push(new Map());
    }

    @Override
    public void beginCollection() {
        builders.push(new Array());
    }

    @Override
    public void end() {
        Object builder = builders.pop();

        if (builder instanceof Map) {
            cbor = (Map) builder;

        } else if (builder instanceof Array) {
            cbor = (Array) builder;

        } else {
            throw new IllegalStateException();
        }

        if (!builders.isEmpty()) {
            if (builders.peek() instanceof Array) {
                ((Array) builders.peek()).add(cbor);

            } else if (builders.peek() instanceof Map) {
                builders.push(cbor);

            } else if (builders.peek() instanceof DataItem) {
                DataItem key = (DataItem) builders.pop();
                ((Map) builders.peek()).put(key, cbor);

            } else {
                throw new IllegalStateException();
            }
        }

    }

    public DataItem cbor() {
        return cbor;
    }

    @Override
    public void nullValue() throws IOException {
        cbor(SimpleValue.NULL);
    }

    @Override
    public void booleanValue(boolean node) throws IOException {
        cbor(node ? SimpleValue.TRUE : SimpleValue.FALSE);
    }

    @Override
    public void stringValue(String node) throws IOException {
        cbor(new UnicodeString(node));
    }

    @Override
    public void numericValue(long node) throws IOException {
        if (node == 0) {
            cbor(new UnsignedInteger(BigInteger.ZERO));
            return;
        }
        if (node < 0) {
            cbor(new NegativeInteger(node));
            return;
        }
        cbor(new UnsignedInteger(node));
    }

    @Override
    public void numericValue(BigInteger node) throws IOException {
        switch (node.signum()) {
        case -1:
            cbor(new NegativeInteger(node));
            break;
        case 0:
            cbor(new UnsignedInteger(BigInteger.ZERO));
            break;
        case 1:
            cbor(new UnsignedInteger(node));
            break;
        }
    }

    @Override
    public void numericValue(double node) throws IOException {
        cbor(new DoublePrecisionFloat(node));
    }

    @Override
    public void numericValue(BigDecimal node) throws IOException {
        cbor(new DoublePrecisionFloat(node.doubleValue()));
    }

    @Override
    public void binaryValue(byte[] node) throws IOException {
        cbor(new ByteString(node));

    }

    public void cbor(DataItem node) {
        switch (currentNodeContext) {
        case PROPERTY_KEY:
            builders.push(node);
            return;

        case PROPERTY_VALUE:
            DataItem key = (DataItem) builders.pop();
            ((Map) builders.peek()).put(key, node);
            return;

        case COLLECTION_ELEMENT:
            ((Array) builders.peek()).add(node);
            return;

        case ROOT:
            cbor = node;
            return;

        default:
            throw new IllegalStateException();
        }
    }
}
