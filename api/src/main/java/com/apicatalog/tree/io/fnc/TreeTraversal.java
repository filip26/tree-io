package com.apicatalog.tree.io.fnc;

import java.util.function.Consumer;

import com.apicatalog.tree.io.TreeIOException;

@FunctionalInterface
public interface TreeTraversal {

    void traverse(Object node, final Consumer<TreeTraversal> consumer) throws TreeIOException;

}
