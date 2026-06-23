package com.apicatalog.tree.io.java;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import com.apicatalog.tree.io.Tree.Event;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeContext;
import com.apicatalog.tree.io.Tree.NodeType;
import com.apicatalog.tree.io.TreeComposer;
import com.apicatalog.tree.io.TreeCursor;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeProcessor;

public class NativeComposer implements TreeComposer<Object>, TreeProcessor {

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
    public boolean accept(Event event, TreeCursor cursor) throws TreeIOException {
        switch (event) {
        case BEGIN_MAP:
            stack.push(new LinkedHashMap<>());
            return true;

        case END_MAP:
            if (stack.peek() instanceof Map) {
                next(cursor.context());
                return true;
            }
            throw new IllegalStateException();

        case BEGIN_SEQUENCE:
            stack.push(new ArrayList<>());
            return true;

        case END_SEQUENCE:
            if (stack.peek() instanceof Collection) {
                next(cursor.context());
                return true;
            }
            throw new IllegalStateException();

        case SCALAR:
            switch (cursor.nodeType()) {
            case NULL:
                nullValue(cursor.context());
                return true;

            case TRUE:
                stack.push(true);
                next(cursor.context());
                return true;

            case FALSE:
                stack.push(false);
                next(cursor.context());
                return true;

            case BINARY:
                stack.push(cursor.binaryValue());
                next(cursor.context());
                return true;

            case NUMBER:
                stack.push(cursor.numberValue());
                next(cursor.context());
                return true;

            case STRING:
                stack.push(cursor.stringValue());
                next(cursor.context());
                return true;

            default:
                throw new IllegalStateException();
            }

        case null:
            throw new IllegalArgumentException();
        }
    }

    @Override
    public Object compose() throws TreeIOException {
        if (stack.size() > 1) {
            throw new IllegalStateException();
        }
        if (stack.isEmpty()) {
            return null;
        }
        return stack.peek();
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
