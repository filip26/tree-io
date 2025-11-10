package com.apicatalog.tree.io.java;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import com.apicatalog.tree.io.Features;
import com.apicatalog.tree.io.NodeType;
import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeGenerator;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeTraversal;

/**
 * A specialized class that builds a native object model from any tree-like
 * source.
 * <p>
 * This class implements both {@link TreeTraversal} and {@link TreeGenerator},
 * allowing it to act as a self-contained transformation engine. It traverses a
 * source structure using its {@code NodeVisitor} capabilities and consumes its
 * own traversal events via its {@code NodeGenerator} implementation to
 * construct a {@link Object} tree in memory.
 * </p>
 * <p>
 * The class is stateful and designed for a single transformation. It can be
 * reused by calling the {@link #reset()} method.
 * </p>
 */
public class NativeMaterializer extends TreeTraversal implements TreeGenerator {

    protected final Deque<Object> structures;

    protected Object object;

    public NativeMaterializer() {
        super(new ArrayDeque<>(), null);
        this.structures = new ArrayDeque<>();
        this.object = null;
    }

    @Override
    public Features features() {
        return NativeAdapter.FEATURES;
    }

    public static Object node(TreeIO node) throws TreeIOException {
        return node(node.node(), node.adapter());
    }

    public static Object node(Object node, TreeAdapter adapter) throws TreeIOException {

        if (NativeAdapter.instance().isCompatibleWith(adapter)) {
            return node;
        }

        final NodeType type = adapter.type(node);

        if (type.isScalar()) {
            return scalar(node, adapter);
        }

        return new NativeMaterializer().structure(node, adapter);
    }

    public static Object scalar(Object node, TreeAdapter adapter) throws TreeIOException {
        final NodeType type = adapter.type(node);

        switch (type) {
        case NULL:
            return null;
        case TRUE:
            return true;
        case FALSE:
            return false;
        case BINARY:
            return adapter.binaryValue(node);
        case STRING:
            return adapter.stringValue(node);
        case NUMBER:
            return adapter.isIntegral(node)
                    ? adapter.integerValue(node)
                    : adapter.decimalValue(node);
        default:
            throw new IllegalArgumentException();
        }
    }

    /**
     * The primary entry point for materialization. Traverses the given source node.
     *
     * @param node    the source root node to traverse
     * @param adapter the adapter for interpreting the source node's structure
     * @return the fully materialized object
     * @throws TreeIOException if an error occurs during generation
     */
    public Object structure(Object node, TreeAdapter adapter) throws TreeIOException {
        root(node, adapter).traverse(this);
        return object;
    }

    /**
     * Returns the fully materialized {@link Object} after a successful traversal.
     *
     * @return the resulting {@link Object}, or {@code null} if traversal has not
     *         completed
     */
    public Object object() {
        return object;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Clears the partially built structure and the internal builder stack, allowing
     * the instance to be reused for a new materialization.
     * </p>
     */
    @Override
    public NativeMaterializer reset() {
        this.structures.clear();
        this.object = null;
        super.reset();
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void nullValue() throws TreeIOException {
        value(null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void booleanValue(boolean node) throws TreeIOException {
        value(node);
    }

    /**
     * {@inheritDoc}
     * <p>
     * If the current context is a {@code PROPERTY_KEY}, the string is pushed onto
     * the builder stack to be used as a key. Otherwise, it creates a {@code String}
     * value.
     * </p>
     */
    @Override
    public void stringValue(String node) throws TreeIOException {
        if (currentNodeContext == Context.PROPERTY_KEY) {
            structures.push(node);
            return;
        }
        value(node);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void numericValue(long node) throws TreeIOException {
        value(node);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void numericValue(BigInteger node) throws TreeIOException {
        value(node);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void numericValue(double node) throws TreeIOException {
        value(node);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void numericValue(BigDecimal node) throws TreeIOException {
        value(node);
    }

    /**
     * {@inheritDoc}
     * 
     * @throws UnsupportedOperationException always
     */
    @Override
    public void binaryValue(byte[] node) throws TreeIOException {
        value(node);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void beginMap() throws TreeIOException {
        structures.push(new LinkedHashMap<>());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void beginList() throws TreeIOException {
        structures.push(new ArrayList<>());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void beginSet() throws TreeIOException {
        structures.push(new LinkedHashSet<>());
    }

    /**
     * 
     * {@inheritDoc}
     * <p>
     * Finalizes the current {@code Map} or {@code List}, pops it from the stack,
     * builds the final {@link Object}, and attaches it to its parent structure if
     * one exists.
     * </p>
     */
    @Override
    public void end() throws TreeIOException {

        object = structures.pop();

        if (!structures.isEmpty()) {
            if (structures.peek() instanceof String) {
                String key = (String) structures.pop();
                ((Map) structures.peek()).put(key, object);
            } else if (structures.peek() instanceof List) {
                ((List) structures.peek()).add(object);
            }
        }
    }

    /**
     * Internal dispatcher that places a newly created {@link Object} into the
     * correct position within the structure being built.
     *
     * @param value the {@link Object} to place
     */
    protected void value(final Object value) {

        switch (currentNodeContext) {
        case PROPERTY_VALUE:
            String key = (String) structures.pop();
            ((Map) structures.peek()).put(key, value);
            return;

        case COLLECTION_ELEMENT:
            ((List) structures.peek()).add(value);
            return;

        case ROOT:
            object = value;
            return;

        default:
            throw new IllegalStateException("Cannot add a value in the current context: " + currentNodeContext + ", value: " + value);
        }
    }
}
