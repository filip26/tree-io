package com.apicatalog.tree.io.morph;

import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Stream;

import com.apicatalog.tree.io.Tree;
import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.java.JavaAdapter;

public class MapOverlay {

    private final Object base;
    private final TreeAdapter baseAdapter;

    private Collection<Object> keys;
    private Map<Object, Object> overlay;

    private MapOverlay(Object base, TreeAdapter baseAdapter) {
        this.base = base;
        this.baseAdapter = baseAdapter;
    }

    public static MapOverlay with(Tree tree) {
        return with(tree.node(), tree.adapter());
    }

    public static MapOverlay with(Map<Object, Object> map) {
        return with(map, JavaAdapter.instance());
    }

    public static MapOverlay with(Object map, TreeAdapter adapter) {
        return null;
    }

    public Collection<?> keys() {
        return keys;
    }

    public Object property(Object key) {
        if (overlay.containsKey(key)) {
            return overlay.get(key);
        }
        return baseAdapter.property(key, base);
    }

    public Object property(Object key, TreeAdapter keyAdapter) {
        if (overlay.containsKey(key)) {
            return overlay.get(key);
        }
        return baseAdapter.property(key, keyAdapter, base);
    }

    public Iterable<Entry<?, ?>> entries() {
        return entryStream().toList();
    }

    public Stream<Entry<?, ?>> entryStream() {
        return Stream.concat(
                baseAdapter.entryStream(base)
                        .filter(entry -> !overlay.containsKey(entry.getKey()) && keys.contains(entry.getKey())),
                overlay.entrySet().stream());
    }

}
