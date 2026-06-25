package com.apicatalog.tree.io.utils;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Map.Entry;
import java.util.function.Function;

import com.apicatalog.tree.io.Tree.NodeContext;
import com.apicatalog.tree.io.TreeEmitter;

public final class PropertyTreeEmitter {

    private final TreeEmitter emitter;
    private final Deque<NodeContext> context;
    
    private Entry<String, Object> lastEntry;
    private Object lastElement;
    
    public PropertyTreeEmitter(TreeEmitter emitter) {
        this.emitter = emitter;
        this.context = new ArrayDeque<>();
        this.context.push(NodeContext.ROOT);
    }

    public void beginMap() {
        emitter.beginMap(context.peek());
    }

    public void beginMap(String key) {
        emitter.beginMap(context.peek());
    }


    public void endMap() {
        emitter.endMap(context.peek());
    }
    
    public void entry(String key, String value) {
//        if (lastEntry != null) {
//            entry(lastEntry.)
//        }
        
        
//        emitter.stringValue(null, value);
    }

    public void entry(String key, boolean value) {
        
    }
    
    public void entry(String key, long value) {
        
    }
    
    public void beginSequence() {
        
    }

    public void beginSequence(String key) {
        
    }

    public void endSequence() {
        
    }
    
    public void element(String element) {
        
    }
    
    public void element(long value) {
        
    }
    
    public void element(boolean value) {
        
    }


    public <T> void entry(String key, T value, Function<T, Object> map) {
        // TODO Auto-generated method stub
        
    }

}
