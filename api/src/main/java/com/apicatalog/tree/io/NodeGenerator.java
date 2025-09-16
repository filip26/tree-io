package com.apicatalog.tree.io;

import java.io.IOException;
import java.util.Deque;

public abstract class NodeGenerator extends NodeVisitor {

    public enum PropertyKeyPolicy {
        StringOnly,
        ScalarOnly,
        Any
    }

    protected final PropertyKeyPolicy policy;
    
    protected NodeGenerator(Deque<Object> stack, PropertyKeyPolicy policy) {
        super(stack, null);
        this.policy = policy;
    }

    protected abstract void scalar(Object node) throws IOException;

    protected abstract void beginMap() throws IOException;

    protected abstract void beginCollection() throws IOException;

    protected abstract void end() throws IOException;

    public void node(Object node, NodeAdapter adapter) throws IOException {

        reset(node, adapter);

        while (step()) {
            node(node);
        }

        if (depth > 0) {
            throw new IllegalStateException();
        }
    }

    protected void node(Object value) throws IOException {

        if (nodeCtx == Context.END) {
            end();
            return;            
        }
        
        if (nodeCtx == Context.PROPERTY_KEY) {
            switch (policy) {
            case ScalarOnly:
                if (nodeType != null && !nodeType.isScalar()) {
                    throw new IllegalStateException();
                }
                
                break;
            case StringOnly:
                if (NodeType.STRING != nodeType) {
                    throw new IllegalStateException();
                }
                break;
                
            default:
                break;
            }            
        }

        if (adapter.isMap(value)) {
            beginMap();
            return;
        }

        if (adapter.isCollection(value)) {
            beginCollection();
            return;
        }

        scalar(value);
    }
}
