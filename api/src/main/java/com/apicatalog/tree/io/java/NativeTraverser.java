package com.apicatalog.tree.io.java;

import java.util.ArrayDeque;
import java.util.Comparator;
import java.util.Map.Entry;
import java.util.function.Consumer;

import com.apicatalog.tree.io.TreeTraverser;

/**
 * Provides a stateful, non-recursive, depth-first iterator for arbitrary
 * tree-like structures. This class decouples the traversal algorithm from the
 * tree.
 */
public class NativeTraverser extends NativeParser implements TreeTraverser<Object> {

    public NativeTraverser() {
        super(new ArrayDeque<>(), null);
    }

    public NativeTraverser(Comparator<Entry<?, ?>> entryComparator) {
        super(new ArrayDeque<>(), entryComparator);
    }

    @Override
    public void traverse(Object node, final Consumer<TreeTraverser<Object>> consumer) {

        node(node);

        var event = next();

        while (event != null) {
            consumer.accept(this);
            event = next();
        }
    }
}
