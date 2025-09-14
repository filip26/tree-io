package com.apicatalog.tree.io;

public class TreeIO {

    protected final NodeAdapter adapter;
    protected final Object root;

    public TreeIO(Object root, NodeAdapter adapter) {
        this.root = root;
        this.adapter = adapter;
    }

    public NodeAdapter adapter() {
        return adapter;
    }

    public Object root() {
        return root;
    }
}
