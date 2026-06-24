package com.apicatalog.tree.io;

import java.io.IOException;
import java.io.InputStream;

/**
 * Provides a uniform abstraction for de-serializing tree-like data structures.
 * This interface decouples the process of describing a tree from its final
 * representation.
 * <p>
 * It is the conceptual counterpart to {@link TreeWriter}, which writes tree
 * structures.
 * </p>
 */
public interface TreeReader<T> {

    T read(InputStream is) throws IOException;

//    <C> C read(InputStream is, TreeComposer<C> consumer);
}
