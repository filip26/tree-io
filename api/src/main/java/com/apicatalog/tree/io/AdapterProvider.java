package com.apicatalog.tree.io;

import java.util.Arrays;
import java.util.Collection;
import java.util.Optional;

class AdapterProvider {
    
    protected final Collection<NodeAdapter> adapters;

    protected AdapterProvider(final Collection<NodeAdapter> adapters) {
        this.adapters = adapters;
    }

    public static final AdapterProvider of(NodeAdapter... adapters) {
        return new AdapterProvider(Arrays.asList(adapters));
    }

    public Optional<NodeAdapter> findAdapter(Object node) {
        return adapters.stream().filter(a -> a.isNode(node)).findAny();
    }

    //TODO findAdapters by features predicate ...
    
    public Collection<NodeAdapter> adapters() {
        return adapters;
    }
}
