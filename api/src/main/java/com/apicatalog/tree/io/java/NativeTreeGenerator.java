package com.apicatalog.tree.io.java;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.apicatalog.tree.io.Tree.Capability;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeType;
import com.apicatalog.tree.io.TreeGenerator;
import com.apicatalog.tree.io.TreeIOException;

public class NativeTreeGenerator implements TreeGenerator {

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
                    NodeType.NULL,
                    NodeType.TREE),
            // capabilities
            Set.of(Capability.SCALAR_OBJECT_EQUALS));

    
    @Override
    public Features features() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void nullValue(Context context) {
        // root
        if (stack.isEmpty()) {
            stack.push(Optional.empty());
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
    public void booleanValue(Context context, boolean value) throws TreeIOException {
        stack.push(value);
        next(context);
    }

    @Override
    public void stringValue(Context context, String value) throws TreeIOException {
        stack.push(value);
        next(context);
    }

    @Override
    public void numericValue(Context context, BigInteger value) throws TreeIOException {
        stack.push(value);
        next(context);
    }

    @Override
    public void numericValue(Context context, BigDecimal value) throws TreeIOException {
        stack.push(value);
        next(context);
    }

    @Override
    public void binaryValue(Context context, byte[] value) throws TreeIOException {
        stack.push(value);
        next(context);
    }

    @Override
    public void beginMap(Context context) {
        stack.push(new LinkedHashMap<>());
    }

    @Override
    public void endMap(Context context) {
        if (stack.peek() instanceof Map) {
            next(context);
        }
        throw new IllegalStateException();
    }

    @Override
    public void beginSequence(Context context) throws TreeIOException {
        stack.push(new ArrayList<>());
    }

    @Override
    public void endSequence(Context context) throws TreeIOException {
        if (stack.peek() instanceof Collection) {
            next(context);
        }
        throw new IllegalStateException();
    }

    public Object get() {
        if (stack.size() != 1) {
            throw new IllegalStateException();
        }
        if (stack.peek() instanceof Optional) {
            return null;
        }
        return stack.peek();
    }

    private final void next(Context context) {
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

        case ROOT, ENTRY_KEY:
            return;
        }
    }

}
