package com.apicatalog.tree.io;

import java.io.OutputStream;

@FunctionalInterface
public interface TreeRenderer {

    default void render(Tree tree, OutputStream os) throws TreeIOException {
        render(tree.node(), tree.adapter(), os);
    }

    void render(Object node, TreeAdapter adapter, OutputStream os) throws TreeIOException;

}
