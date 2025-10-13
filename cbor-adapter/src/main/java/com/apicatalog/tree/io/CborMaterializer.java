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

/**
 * A specialized class that builds a {@code co.nstant.in.cbor.model.DataItem}
 * object model from any tree-like source.
 * <p>
 * This class implements both {@link DepthFirstTraversal} and {@link NodeGenerator},
 * allowing it to act as a self-contained transformation engine. It traverses a
 * source structure using its {@code NodeVisitor} capabilities and consumes its
 * own traversal events via its {@code NodeGenerator} implementation to
 * construct a CBOR {@link DataItem} tree in memory.
 * </p>
 * <p>
 * The class is stateful and designed for a single transformation. It can be
 * reused by calling the {@link #reset()} method.
 * </p>
 */
public class CborMaterializer extends DepthFirstTraversal implements NodeGenerator {

    protected DataItem cbor;
    protected final Deque<Object> builders;

    /**
     * Constructs a new, empty {@code CborMaterializer}.
     */
    public CborMaterializer() {
        super(new ArrayDeque<>(), null);
        this.builders = new ArrayDeque<>();
        this.cbor = null;
    }

    /**
     * The primary entry point for materialization. Traverses the given source node
     * and returns the resulting CBOR {@link DataItem}.
     *
     * @param node    the source root node to traverse
     * @param adapter the adapter for interpreting the source node's structure
     * @return the fully materialized {@link DataItem}
     * @throws IOException if an error occurs during generation
     */
    public DataItem node(Object node, NodeAdapter adapter) throws IOException {
        root(node, adapter).traverse(this);
        return cbor;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Clears the partially built CBOR structure and the internal builder stack,
     * allowing the instance to be reused for a new materialization.
     * </p>
     */
    @Override
    public DepthFirstTraversal reset() {
        this.cbor = null;
        this.builders.clear();
        return super.reset();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Starts a new CBOR {@link Map} and places it on the internal builder stack.
     * </p>
     */
    @Override
    public void beginMap() {
        builders.push(new Map());
    }

    /**
     * {@inheritDoc}
     * <p>
     * Starts a new CBOR {@link Array} and places it on the internal builder stack.
     * </p>
     */
    @Override
    public void beginList() {
        builders.push(new Array());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void beginSet() throws IOException {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Finalizes the current CBOR {@link Map} or {@link Array}, pops it from the
     * builder stack, and attaches it to its parent structure.
     * </p>
     */
    @Override
    public void end() {
        Object builder = builders.pop();

        if (builder instanceof Map) {
            cbor = (Map) builder;

        } else if (builder instanceof Array) {
            cbor = (Array) builder;

        } else {
            throw new IllegalStateException("Internal builder stack is in an inconsistent state.");
        }

        if (!builders.isEmpty()) {
            Object parent = builders.peek();
            if (parent instanceof Array) {
                ((Array) parent).add(cbor);
            } else if (parent instanceof Map) {
                builders.push(cbor); // Pushes value to be paired with a key later
            } else if (parent instanceof DataItem) {
                DataItem key = (DataItem) builders.pop();
                ((Map) builders.peek()).put(key, cbor);
            } else {
                throw new IllegalStateException("Internal builder stack contains an unexpected type.");
            }
        }
    }

    /**
     * Returns the fully materialized CBOR {@link DataItem} after a successful
     * traversal.
     *
     * @return the resulting {@link DataItem}, or {@code null} if traversal has not
     *         completed
     */
    public DataItem cbor() {
        return cbor;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void nullValue() throws IOException {
        cbor(SimpleValue.NULL);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void booleanValue(boolean node) throws IOException {
        cbor(node ? SimpleValue.TRUE : SimpleValue.FALSE);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void stringValue(String node) throws IOException {
        cbor(new UnicodeString(node));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void numericValue(long node) throws IOException {
        if (node < 0) {
            cbor(new NegativeInteger(node));
        } else {
            cbor(new UnsignedInteger(node));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void numericValue(BigInteger node) throws IOException {
        if (node.signum() < 0) {
            cbor(new NegativeInteger(node));
        } else {
            cbor(new UnsignedInteger(node));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void numericValue(double node) throws IOException {
        cbor(new DoublePrecisionFloat(node));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void numericValue(BigDecimal node) throws IOException {
        cbor(new DoublePrecisionFloat(node.doubleValue()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void binaryValue(byte[] node) throws IOException {
        cbor(new ByteString(node));
    }

    /**
     * Internal dispatcher that places a newly created {@link DataItem} into the
     * correct position within the CBOR structure being built.
     *
     * @param node the {@link DataItem} to place
     */
    protected void cbor(DataItem node) {
        switch (currentNodeContext) {
        case PROPERTY_KEY:
            builders.push(node);
            break;

        case PROPERTY_VALUE:
            DataItem key = (DataItem) builders.pop();
            ((Map) builders.peek()).put(key, node);
            break;

        case COLLECTION_ELEMENT:
            ((Array) builders.peek()).add(node);
            break;

        case ROOT:
            cbor = node;
            break;

        default:
            throw new IllegalStateException("Cannot add a CBOR DataItem in the current context: " + currentNodeContext);
        }
    }
}
