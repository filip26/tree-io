package com.apicatalog.tree.io;

import java.util.Set;

public class Features {

    final Set<NodeType> nodes;
    final Set<NodeType> keys;

    public Features(Set<NodeType> nodes, Set<NodeType> keys) {
        this.nodes = nodes;
        this.keys = keys;
    }

    public boolean isCompatibleWith(Features features) {
        return keys.containsAll(features.keys)
                && nodes.containsAll(features.nodes);
    }

    /**
     * Returns the complete set of node types that this adapter is capable of
     * representing.
     *
     * @return an immutable set of supported {@link NodeType}s.
     */
    public Set<NodeType> nodes() {
        return nodes;
    }

    /**
     * Returns the set of scalar types that are supported as keys in map nodes. For
     * example, a JSON-based adapter would return only {@link NodeType#STRING},
     * whereas a CBOR-based adapter might return multiple scalar types.
     *
     * @return an immutable set of supported key {@link NodeType}s.
     */
    public Set<NodeType> keys() {
        return keys;
    }
}
