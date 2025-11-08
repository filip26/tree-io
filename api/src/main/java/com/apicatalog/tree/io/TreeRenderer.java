package com.apicatalog.tree.io;

import java.io.OutputStream;

@FunctionalInterface
public interface TreeRenderer {

    void render(Object node, TreeAdapter adapter, OutputStream os) throws TreeIOException;

}
