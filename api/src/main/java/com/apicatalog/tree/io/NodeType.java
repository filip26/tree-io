package com.apicatalog.tree.io;

/**
 * Supported node types in a tree structure.
 */
public enum NodeType {
    MAP(false),
    COLLECTION(false),
    STRING(true),
    NUMBER(true),
    BINARY(true),
    TRUE(true),
    FALSE(true),
    NULL(true);

    private final boolean scalar;

    NodeType(boolean scalar) {
        this.scalar = scalar;
    }

    public boolean isScalar() {
        return scalar;
    }
}
