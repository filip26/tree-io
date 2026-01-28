package com.apicatalog.tree.io.morph;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.stream.Stream;

import com.apicatalog.tree.io.Tree;
import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.java.JavaAdapter;

public class MapOverlay {

    static final Function<Object, Object> REMOVED = Function.identity();

    private final Object base;

    private final Map<Object, Object> overlay;

    private MapOverlay(Object base, Map<Object, Object> overlay) {
        this.base = base;
        this.overlay = overlay;
    }

    public Collection<?> keys(TreeAdapter adapter) {
        return keyStream(adapter).toList();
    }

    public Stream<?> keyStream(TreeAdapter adapter) {
        return Stream.concat(
                adapter.keyStream(base)
                        .filter(key -> !overlay.containsKey(key)),
                overlay.entrySet().stream()
                        .filter(entry -> !REMOVED.equals(entry.getValue()))
                        .map(Entry::getKey));
    }

    public Object property(Object key, TreeAdapter adapter) {
        if (overlay.containsKey(key)) {
            final var value = overlay.get(key);
            if (REMOVED.equals(value)) {
                return null;
            }
            return value;
        }
        return adapter.property(key, base);
    }

    public Object property(Object key, TreeAdapter keyAdapter, TreeAdapter adapter) {
        if (overlay.containsKey(key)) {
            final var value = overlay.get(key);
            if (REMOVED.equals(value)) {
                return null;
            }
            return value;
        }
        return adapter.property(key, keyAdapter, base);
    }

    public Iterable<Entry<?, ?>> entries(TreeAdapter adapter) {
        return entryStream(adapter).toList();
    }

    public Stream<Entry<?, ?>> entryStream(TreeAdapter adapter) {
        return Stream.concat(
                adapter.entryStream(base)
                        .filter(entry -> !overlay.containsKey(entry.getKey())),
                overlay.entrySet().stream()
                        .filter(entry -> !REMOVED.equals(entry.getValue())));
    }

    public static Builder newBuilder(Tree tree) {
        return newBuilder(tree.node(), tree.adapter());
    }

    public static Builder newBuilder(Map<Object, Object> map) {
        return new Builder(map, JavaAdapter.instance(), new LinkedHashMap<>());
    }

    public static Builder newBuilder(Object map, TreeAdapter adapter) {
        if (map instanceof MapOverlay overlay) {
            return new Builder(overlay.base,
                    adapter instanceof MorphAdapter morph
                            ? morph.base
                            : adapter,
                    new LinkedHashMap<>(overlay.overlay));
        }
        return new Builder(map,
                adapter instanceof MorphAdapter morph
                        ? morph.base
                        : adapter,
                new LinkedHashMap<>());
    }

    public static class Builder {

        private final Object base;
        private final TreeAdapter baseAdapter;

        private final Map<Object, Object> overlay;

        private Builder(Object base, TreeAdapter baseAdapter, Map<Object, Object> overlay) {
            this.base = base;
            this.baseAdapter = baseAdapter;
            this.overlay = overlay;
        }

        public Builder remove(Object key) {
            overlay.put(key, REMOVED);
            return this;
        }

        public Builder put(Object key, Object node, TreeAdapter nodeAdapter) {
            if (node instanceof Tree tree) {
                return put(key, tree);
            }
            if (nodeAdapter instanceof MorphAdapter morph
                    && baseAdapter.isEqualTo(morph.base)
                    || baseAdapter.isEqualTo(nodeAdapter)) {
                overlay.put(key, node);
                return this;
            }

            overlay.put(key, new Tree(node, nodeAdapter));
            return this;
        }

        public Builder put(Object key, Tree tree) {
            if (tree.adapter() instanceof MorphAdapter morph
                    && baseAdapter.isEqualTo(morph.base)
                    || baseAdapter.isEqualTo(tree.adapter())) {
                overlay.put(key, tree.node());
                return this;
            }
            overlay.put(key, tree);
            return this;
        }

        public MapOverlay build() {
            return new MapOverlay(
                    base,
                    Map.copyOf(overlay));
        }

        public Tree buildTree() {
            return new Tree(build(), new MorphAdapter(baseAdapter));
        }
    }

}
