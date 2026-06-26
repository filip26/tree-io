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

    public PropertyTreeEmitter beginMap() {
        emitter.beginMap(contexts.peek());
        contexts.push(NodeContext.FIRST_ENTRY_KEY);
        return this;
    }

    public PropertyTreeEmitter beginMap(String key) {
        if (NodeContext.ENTRY_KEY != contexts.peek() && NodeContext.FIRST_ENTRY_KEY != contexts.peek()) {
            throw new IllegalStateException();
        }
        emitter.stringValue(contexts.pop(), key);
        contexts.push(NodeContext.ENTRY_VALUE);
        beginMap();
        return this;
    }

    public PropertyTreeEmitter endMap() {
        if (NodeContext.ENTRY_KEY != contexts.peek() && NodeContext.FIRST_ENTRY_KEY != contexts.peek()) {
            throw new IllegalStateException();
        }
        contexts.pop();
        emitter.endMap(contexts.peek());
        if (NodeContext.ENTRY_VALUE == contexts.peek()) {
            contexts.pop();
            contexts.push(NodeContext.ENTRY_KEY);
        } else if (NodeContext.FIRST_ELEMENT == contexts.peek()) {
            contexts.pop();
            contexts.push(NodeContext.ELEMENT);
        }
        return this;
    }

    public <T> PropertyTreeEmitter entry(String key, T object, Function<T, String> map) {
        if (object == null) {
            return this;
        }
        var value = map.apply(object);
        if (value == null) {
            return this;
        }
        entry(key, value);
        return this;
    }

    public PropertyTreeEmitter entry(String key, String value) {
        if (value == null) {
            return this;
        }
        emitter.stringValue(contexts.pop(), key);
        emitter.stringValue(NodeContext.ENTRY_VALUE, value);
        contexts.push(NodeContext.ENTRY_KEY);
        return this;
    }

    public PropertyTreeEmitter entry(String key, boolean value) {
        emitter.stringValue(contexts.pop(), key);
        emitter.booleanValue(NodeContext.ENTRY_VALUE, value);
        contexts.push(NodeContext.ENTRY_KEY);
        return this;
    }

    public PropertyTreeEmitter entry(String key, long value) {
        emitter.stringValue(contexts.pop(), key);
        emitter.numericValue(NodeContext.ENTRY_VALUE, value);
        contexts.push(NodeContext.ENTRY_KEY);
        return this;
    }

    public PropertyTreeEmitter beginSequence() {
        emitter.beginSequence(contexts.peek());
        contexts.push(NodeContext.FIRST_ELEMENT);
        return this;
    }

    public PropertyTreeEmitter beginSequence(String key) {
        if (NodeContext.ENTRY_KEY != contexts.peek() && NodeContext.FIRST_ENTRY_KEY != contexts.peek()) {
            throw new IllegalStateException();
        }
        emitter.stringValue(contexts.pop(), key);
        contexts.push(NodeContext.ENTRY_VALUE);
        beginSequence();
        return this;
    }

    public PropertyTreeEmitter endSequence() {
        if (NodeContext.ELEMENT != contexts.peek() && NodeContext.FIRST_ELEMENT != contexts.peek()) {
            throw new IllegalStateException();
        }
        contexts.pop();
        emitter.endMap(contexts.peek());
        if (NodeContext.ENTRY_VALUE == contexts.peek()) {
            contexts.pop();
            contexts.push(NodeContext.ENTRY_KEY);
        } else if (NodeContext.FIRST_ELEMENT == contexts.peek()) {
            contexts.pop();
            contexts.push(NodeContext.ELEMENT);
        }
        return this;
    }

    public PropertyTreeEmitter element(String element) {
        emitter.stringValue(contexts.peek(), element);
        if (NodeContext.FIRST_ELEMENT == contexts.peek()) {
            contexts.pop();
            contexts.push(NodeContext.ELEMENT);
        }
        return this;
    }

    public PropertyTreeEmitter element(long element) {
        emitter.numericValue(contexts.peek(), element);
        if (NodeContext.FIRST_ELEMENT == contexts.peek()) {
            contexts.pop();
            contexts.push(NodeContext.ELEMENT);
        }
        return this;
    }

    public PropertyTreeEmitter element(boolean element) {
        emitter.booleanValue(contexts.peek(), element);
        if (NodeContext.FIRST_ELEMENT == contexts.peek()) {
            contexts.pop();
            contexts.push(NodeContext.ELEMENT);
        }
        return this;
    }
}
