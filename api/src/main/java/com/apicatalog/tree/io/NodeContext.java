package com.apicatalog.tree.io;

/**
 * Identifies the role of the current node within the tree structure during tree
 * traversal, parsing, and generation.
 */
public enum NodeContext {

    /** The current node is the root of the tree being traversed. */
    ROOT,

    /** The current node is a key within a map structure. */
    PROPERTY_KEY,

    /** The current node is a value associated with a key in a map structure. */
    PROPERTY_VALUE,

    /** The current node is an element within a collection structure. */
    ELEMENT,

    /**
     * A synthetic marker indicating the end of a map or collection has been
     * reached.
     */
    END,
}