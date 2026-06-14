package com.apicatalog.tree.io.fnc;

import java.util.function.Consumer;

import com.apicatalog.tree.io.TreeIOException;

@FunctionalInterface
public interface TreeTraverser {

    void traverse(Object node, final Consumer<TreeTraverser> consumer) throws TreeIOException;

}
