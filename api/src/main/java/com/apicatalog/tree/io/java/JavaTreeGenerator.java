package com.apicatalog.tree.io.java;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import com.apicatalog.tree.io.Tree.Capability;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeContext;
import com.apicatalog.tree.io.Tree.NodeType;
import com.apicatalog.tree.io.TreeGenerator;
import com.apicatalog.tree.io.TreeIOException;

public class JavaTreeGenerator implements TreeGenerator {

    Deque<Object> stack;

    static final Features FEATURES = new Features(
            // keys
            Set.of(
                    NodeType.SEQUENCE,
                    NodeType.MAP,
                    NodeType.NUMBER,
                    NodeType.STRING),
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
            Set.of(Capability.SCALAR_OBJECT_EQUALS));

    public JavaTreeGenerator() {
        this.stack = new ArrayDeque<>();
    }

    @Override
    public Features features() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void nullValue(NodeContext context) {
        // root
        if (stack.isEmpty()) {
            return;
        }
        switch (context) {
        case ELEMENT:
            ((Collection) stack.peek()).add(null);
            return;

        case ENTRY_VALUE:
            var key = stack.pop();
            ((Map) stack.peek()).put(key, null);
            return;

        case ROOT:
            return;

        case ENTRY_KEY:
            throw new IllegalStateException();
        }
    }

    @Override
    public void booleanValue(NodeContext context, boolean value) throws TreeIOException {
        stack.push(value);
        next(context);
    }

    @Override
    public void stringValue(NodeContext context, String value) throws TreeIOException {
        stack.push(value);
        next(context);
    }

    @Override
    public void numericValue(NodeContext context, BigInteger value) throws TreeIOException {
        stack.push(value);
        next(context);
    }

    @Override
    public void numericValue(NodeContext context, BigDecimal value) throws TreeIOException {
        stack.push(value);
        next(context);
    }

    @Override
    public void binaryValue(NodeContext context, byte[] value) throws TreeIOException {
        stack.push(value);
        next(context);
    }

    @Override
    public void beginMap(NodeContext context) {
        stack.push(new LinkedHashMap<>());
    }

    @Override
    public void endMap(NodeContext context) {
        if (stack.peek() instanceof Map) {
            next(context);
            return;
        }
        throw new IllegalStateException();
    }

    @Override
    public void beginSequence(NodeContext context) throws TreeIOException {
        stack.push(new ArrayList<>());
    }

    @Override
    public void endSequence(NodeContext context) throws TreeIOException {
        if (stack.peek() instanceof Collection) {
            next(context);
            return;
        }
        throw new IllegalStateException();
    }

    public Object get() {
        if (stack.size() > 1) {
            throw new IllegalStateException();
        }
        if (stack.isEmpty()) {
            return null;
        }
        return stack.peek();
    }

    private final void next(NodeContext context) {
        switch (context) {
        case ELEMENT:
            var element = stack.pop();
            ((Collection) stack.peek()).add(element);
            return;

        case ENTRY_VALUE:
            var value = stack.pop();
            var key = stack.pop();
            ((Map) stack.peek()).put(key, value);
            return;

        case ENTRY_KEY:
            return;

        case ROOT:
            return;
        }
    }

}
