package com.apicatalog.tree.io.java;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeIO;

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

    public static Object adapt(Object node, TreeAdapter adapter) {
        return NativeAdapter.adapt(node, adapter);
    }

    //TODO ?!?
    public static <K, V> Map<K, V> cast(Map<?, ?> map) {
        return (Map<K, V>)map;
    }
    
}
