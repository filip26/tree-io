package com.apicatalog.tree.io;

import com.apicatalog.tree.io.Tree.Event;

public interface TreeComposer<T> {

    boolean accept(Event event, TreeCursor cursor) throws TreeIOException;

    T compose() throws TreeIOException;

}

