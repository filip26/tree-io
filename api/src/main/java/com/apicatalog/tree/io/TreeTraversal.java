package com.apicatalog.tree.io;

import java.util.function.Consumer;

@FunctionalInterface
public interface TreeTraversal {

    void traverse(Object node, final Consumer<TreeTraversal> consumer) throws TreeIOException;

}
