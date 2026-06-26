package com.apicatalog.tree.io.util;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.function.Function;

import com.apicatalog.tree.io.Tree.NodeContext;
import com.apicatalog.tree.io.TreeEmitter;

public final class PropertyTreeEmitter {

    private final TreeEmitter emitter;
    private final Deque<NodeContext> contexts;

    public PropertyTreeEmitter(TreeEmitter emitter) {
        this.emitter = emitter;
        this.contexts = new ArrayDeque<>();
        this.contexts.push(NodeContext.ROOT);
    }

    public void beginMap() {
        emitter.beginMap(contexts.peek());
        contexts.push(NodeContext.FIRST_ENTRY_KEY);
    }

    public void beginMap(String key) {
        emitter.stringValue(contexts.peek(), key);
        if (NodeContext.ENTRY_KEY == contexts.peek() || NodeContext.FIRST_ENTRY_KEY == contexts.peek()) {
            contexts.pop();
            contexts.push(NodeContext.ENTRY_VALUE);
        }
        beginMap();
    }

    public void endMap() {
        if (NodeContext.ENTRY_KEY != contexts.peek() && NodeContext.FIRST_ENTRY_KEY != contexts.peek()) {
            throw new IllegalStateException();
        }
        contexts.pop();
        emitter.endMap(contexts.peek());
    }
    

    public <T> void entry(String key, T object, Function<T, String> map) {
        if (object == null) {
            return;
        }
        var value = map.apply(object);
        if (value == null) {
            return;
        }
        entry(key, value);
    }

    public void entry(String key, String value) {
        if (value == null) {
            return;
        }
        emitter.stringValue(contexts.pop(), key);
        emitter.stringValue(NodeContext.ENTRY_VALUE, value);
        contexts.push(NodeContext.ENTRY_KEY);
    }

    public void entry(String key, boolean value) {
        emitter.stringValue(contexts.pop(), key);
        emitter.booleanValue(NodeContext.ENTRY_VALUE, value);
        contexts.push(NodeContext.ENTRY_KEY);
    }

    public void entry(String key, long value) {
        emitter.stringValue(contexts.pop(), key);
        emitter.numericValue(NodeContext.ENTRY_VALUE, value);
        contexts.push(NodeContext.ENTRY_KEY);
    }

    public void beginSequence() {
        emitter.beginMap(contexts.peek());
        contexts.push(NodeContext.FIRST_ELEMENT);
    }

    public void beginSequence(String key) {
        emitter.stringValue(contexts.peek(), key);
        if (NodeContext.FIRST_ELEMENT == contexts.peek()) {
            contexts.pop();
            contexts.push(NodeContext.ELEMENT);
        }
        beginSequence();
    }

    public void endSequence() {
        if (NodeContext.ELEMENT != contexts.peek() && NodeContext.FIRST_ELEMENT != contexts.peek()) {
            throw new IllegalStateException();
        }
        contexts.pop();
        emitter.endMap(contexts.peek());
    }

    public void element(String element) {
        emitter.stringValue(contexts.peek(), element);
    }

    public void element(long element) {
        emitter.numericValue(contexts.peek(), element);
    }

    public void element(boolean element) {
        emitter.booleanValue(contexts.peek(), element);
    }
}
