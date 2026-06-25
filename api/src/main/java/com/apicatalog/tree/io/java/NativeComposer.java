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

import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeContext;
import com.apicatalog.tree.io.Tree.NodeType;
import com.apicatalog.tree.io.TreeComposer;
import com.apicatalog.tree.io.TreeProcessor;

public final class NativeComposer implements TreeComposer<Object>, TreeProcessor {

    private final Deque<Object> stack;

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
                    NodeType.NULL));

    public NativeComposer() {
        this.stack = new ArrayDeque<>();
    }

    @Override
    public Features features() {
        return FEATURES;
    }

    @SuppressWarnings("unchecked")
    public void nullValue(NodeContext context) {
        // root
        if (stack.isEmpty()) {
            return;
        }
        switch (context) {
        case ELEMENT, LAST_ELEMENT:
            ((Collection<?>) stack.peek()).add(null);
            return;

        case ENTRY_VALUE, LAST_ENTRY_VALUE:
            var key = stack.pop();
            ((Map<Object, ?>) stack.peek()).put(key, null);
            return;

        case ROOT:
            return;

        case ENTRY_KEY:
            throw new IllegalStateException();
        }
    }

    @Override
    public Object compose() {
        if (stack.size() > 1) {
            throw new IllegalStateException();
        }
        if (stack.isEmpty()) {
            return null;
        }
        return stack.peek();
    }

    @Override
    public void booleanValue(NodeContext context, boolean value) {
        stack.push(value);
        next(context);
    }

    @Override
    public void stringValue(NodeContext context, String value) {
        stack.push(value);
        next(context);
    }

    @Override
    public void numericValue(NodeContext context, BigInteger value) {
        stack.push(value);
        next(context);
    }

    @Override
    public void numericValue(NodeContext context, BigDecimal value) {
        stack.push(value);
        next(context);
    }

    @Override
    public void binaryValue(NodeContext context, byte[] value) {
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
    public void beginSequence(NodeContext context) {
        stack.push(new ArrayList<>());
    }

    @Override
    public void endSequence(NodeContext context) {
        if (stack.peek() instanceof Collection) {
            next(context);
            return;
        }
        throw new IllegalStateException();
    }

    @SuppressWarnings("unchecked")
    private final void next(NodeContext context) {
        switch (context) {
        case ELEMENT, LAST_ELEMENT:
            var element = stack.pop();
            ((Collection<Object>) stack.peek()).add(element);
            return;

        case ENTRY_VALUE, LAST_ENTRY_VALUE:
            var value = stack.pop();
            var key = stack.pop();
            ((Map<Object, Object>) stack.peek()).put(key, value);
            return;

        case ENTRY_KEY:
            return;

        case ROOT:
            return;
        }
    }
}
