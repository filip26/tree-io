package com.apicatalog.tree.io.java;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.Tree;
import com.apicatalog.tree.io.TreeIOException;

public final class JavaTree {

    private JavaTree() {
        // protected
    }

    public static TreeAdapter adapter() {
        return JavaAdapter.instance();
    }

    public static Tree of(Map<?, ?> map) {
        return new Tree(map, JavaAdapter.instance());
    }

    public static Tree of(Collection<?> collection) {
        return new Tree(collection, JavaAdapter.instance());
    }

    public static Tree of(Tree node) throws TreeIOException {
        if (JavaAdapter.instance().isEqualTo(node.adapter())) {
            return node;
        }
        return new Tree(adapt(node.node(), node.adapter()), JavaAdapter.instance());
    }

    public static Tree of(Object node, TreeAdapter adapter) throws TreeIOException {
        if (JavaAdapter.instance().isEqualTo(adapter)) {
            return new Tree(node, JavaAdapter.instance());
        }
        return new Tree(adapt(node, adapter), JavaAdapter.instance());
    }

    public static Object adapt(Tree node) throws TreeIOException {
        if (JavaAdapter.instance().isEqualTo(node.adapter())) {
            return node.node();
        }
        return JavaMaterializer.node(node);
    }

    public static Object adapt(Object node, TreeAdapter adapter) throws TreeIOException {
        if (JavaAdapter.instance().isEqualTo(adapter)) {
            return node;
        }
        return JavaMaterializer.node(node, adapter);
    }
}
