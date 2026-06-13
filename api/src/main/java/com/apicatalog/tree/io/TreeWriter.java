package com.apicatalog.tree.io;

import java.io.Writer;

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
public interface TreeWriter {

    void write(Writer writer, Object node) throws TreeIOException;

}
