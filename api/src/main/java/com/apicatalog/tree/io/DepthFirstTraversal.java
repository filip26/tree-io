package com.apicatalog.tree.io;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;

public class DepthFirstTraversal {

    protected final Deque<Object> stack;
    protected final NodeAdapter adapter;

    protected DepthFirstTraversal(final Deque<Object> stack, final NodeAdapter adapter) {
        this.stack = stack;
        this.adapter = adapter;
    }

    public static DepthFirstTraversal of(Object root, NodeAdapter adapter) {
        Objects.requireNonNull(root);
        Objects.requireNonNull(adapter);

        Deque<Object> stack = new ArrayDeque<>();
        stack.push(root);

        return new DepthFirstTraversal(stack, adapter);
    }

    public boolean traverse(Consumer<Object> consumer) {

        if (stack.isEmpty()) {
            return false;
        }

        Object item = stack.peek();

        if (item instanceof Iterator) {

            Iterator<? extends Object> it = (Iterator<? extends Object>) item;
            item = it.next();

            if (!it.hasNext()) {
                stack.pop();
            }

            if (item instanceof Map.Entry) {
                Map.Entry<? extends Object, ? extends Object> entry = (Map.Entry<? extends Object, ? extends Object>) item;
                consumer.accept(entry.getKey());
                item = entry.getValue();
            }
        }

        consumer.accept(item);

        switch (adapter.type(item)) {
        case COLLECTION:
            Iterator<? extends Object> it2 = adapter.asIterable(item).iterator();
            if (it2.hasNext()) {
                stack.push(it2);
            }
            break;

        case MAP:

        default:
        }

        return !stack.isEmpty();
    }

    protected void accept(Object node, NodeAdapter adapter, Consumer<Object> consumer) {
        consumer.accept(node);

        switch (adapter.type(node)) {
        case COLLECTION:
            Iterator<? extends Object> it2 = adapter.asIterable(node).iterator();
            if (it2.hasNext()) {
                stack.push(it2);
            }
            break;

        case MAP:

        default:
        }
    }

}
