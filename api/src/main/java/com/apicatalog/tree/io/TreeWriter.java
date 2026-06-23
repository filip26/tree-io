package com.apicatalog.tree.io;

import java.io.OutputStream;

/**
 * Provides a uniform abstraction for serializing tree-like data structures.
 * This interface decouples the process of describing a tree from its final
 * representation.
 * <p>
 * It is the conceptual counterpart to {@link TreeReader}, which reads tree
 * structures.
 * </p>
 */
@FunctionalInterface
public interface TreeWriter<T> {

    void write(T node, OutputStream os) throws TreeIOException;

}
