package com.apicatalog.tree.io;

/**
 * Enumeration of supported node types within a {@code PolyMorph} tree
 * structure.
 * <p>
 * A {@code NodeType} describes the semantic kind of a node, distinguishing
 * between scalar values (e.g. string, number, boolean) and structural
 * containers (e.g. map, collection, or polymorphic wrapper).
 * </p>
 * <p>
 * {@link #ADAPTED} represents an ad-hoc, heterogeneous wrapper node that can
 * encapsulate another node originating from a different data model or library.
 * This enables uniform traversal and comparison of mixed-format trees.
 * </p>
 *
 * @see NodeAdapter
 * @see com.apicatalog.tree.io.AdaptedNode
 */
public enum NodeType {

    /**
     * Polymorphic wrapper node enabling heterogeneous access across formats.
     * <p>
     * An adapted node acts as an adapter-level bridge between different
     * underlying object models, allowing a mixed tree to be processed uniformly.
     * </p>
     */
    ADAPTED(false),

    /**
     * Mapping structure of key-value pairs, such as a JSON object or
     * dictionary-like node. Each key is typically a string associated with a nested
     * node.
     */
    MAP(false),

    /**
     * Ordered sequence of elements, such as a JSON array or list. Elements may be
     * scalar or structural nodes.
     */
    COLLECTION(false),

    /**
     * Textual scalar value. Represents a string node within the tree.
     */
    STRING(true),

    /**
     * Numeric scalar value. Represents an integer or decimal number node.
     */
    NUMBER(true),

    /**
     * Binary scalar value, typically a byte sequence or encoded binary content.
     */
    BINARY(true),

    /**
     * Boolean literal {@code true}.
     */
    TRUE(true),

    /**
     * Boolean literal {@code false}.
     */
    FALSE(true),

    /**
     * Null literal value.
     */
    NULL(true);

    private final boolean scalar;

    NodeType(boolean scalar) {
        this.scalar = scalar;
    }

    /**
     * Returns whether this node type represents a scalar value.
     *
     * @return {@code true} if the node is scalar; {@code false} otherwise
     */
    public boolean isScalar() {
        return scalar;
    }

    /**
     * Returns whether this node type represents a structural container.
     *
     * @return {@code true} if the node is structural (non-scalar)
     */
    public boolean isStructure() {
        return !scalar;
    }
}
