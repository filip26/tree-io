package com.apicatalog.tree.io.java;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.TreeIOException;

public class JavaNode {

    public static TreeAdapter adapter() {
        return NativeAdapter.instance();
    }

    public static TreeIO of(Map<?, ?> map) {
        return new TreeIO(map, NativeAdapter.instance());
    }

    public static TreeIO of(Collection<?> collection) {
        return new TreeIO(collection, NativeAdapter.instance());
    }

    public static TreeIO of(TreeIO node) throws TreeIOException {
        if (NativeAdapter.instance().isCompatibleWith(node.adapter())) {
            return node;
        }
        return new TreeIO(adapt(node.node(), node.adapter()), NativeAdapter.instance());
    }

    public static Object of(Object node, TreeAdapter adapter) throws TreeIOException {
        if (NativeAdapter.instance().isCompatibleWith(adapter)) {
            return new TreeIO(node, NativeAdapter.instance());
        }
        return new TreeIO(adapt(node, adapter), NativeAdapter.instance());
    }

    public static Object adapt(TreeIO node) throws TreeIOException {
        if (NativeAdapter.instance().isCompatibleWith(node.adapter())) {
            return node.node();
        }
        return NativeMaterializer.node(node);
    }

    public static Object adapt(Object node, TreeAdapter adapter) throws TreeIOException {
        if (NativeAdapter.instance().isCompatibleWith(adapter)) {
            return node;
        }
        return NativeMaterializer.node(node, adapter);
    }
}
