package com.apicatalog.tree.io;

import java.util.Arrays;
import java.util.Collection;
import java.util.Optional;

public class NodeAdapterProvider {

    protected final Collection<NodeAdapter> adapters;

    protected NodeAdapterProvider(final Collection<NodeAdapter> adapters) {
        this.adapters = adapters;
    }

    public static final NodeAdapterProvider of(NodeAdapter... adapters) {
        return new NodeAdapterProvider(Arrays.asList(adapters));
    }

    public Optional<NodeAdapter> findAdapter(Object node) {
        return adapters.stream().filter(a -> a.isNode(node)).findAny();
    }

    //TODO findAdapters by features predicate ...
    
    public Collection<NodeAdapter> adapters() {
        return adapters;
    }
}
