package com.apicatalog.tree.io.fnc;

import java.io.OutputStream;

import com.apicatalog.tree.io.TreeIOException;

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

    void write(Object node, OutputStream os) throws TreeIOException;

}
