package com.apicatalog.tree.io;

import java.io.IOException;
import java.util.Deque;

public abstract class AbstractNodeConsumer extends DepthFirstTraversal {

    public static final int MAX_DEPTH = 16;
    public static final int MAX_NODES = -1;

    protected int maxVisited;
    protected int maxDepth;

    protected AbstractNodeConsumer(Deque<Object> stack) {
        super(stack, null);
        this.maxVisited = -1;
        this.maxDepth = -1;
    }

    protected abstract void scalar(Context ctx, Object node) throws IOException;

    protected abstract void beginMap(Context ctx) throws IOException;

    protected abstract void beginCollection(Context ctx) throws IOException;

    protected abstract void end() throws IOException;

    public void accept(Object node, NodeAdapter adapter) throws IOException {

        reset(node, adapter);

        final IOException[] exception = new IOException[1]; // mutable holder
        exception[0] = null;

        while (exception[0] == null
                && traverse((ctx, t) -> {
                    try {
                        accept(ctx, t);
                    } catch (IOException e) {
                        exception[0] = e;
                    }
                }))
            ;

        if (exception[0] != null) {
            throw exception[0];
        }

        if (depth > 0) {
            throw new IllegalStateException();
        }
    }

    protected void accept(Context ctx, Object value) throws IOException {

        if (maxVisited > 0 && maxVisited <= visited) {
            throw new IllegalStateException();
        }
        if (maxDepth > 0 && maxDepth < depth) {
            throw new IllegalStateException();
        }

        if (Context.END == ctx) {
            end();
            return;
        }

        switch (adapter.type(value)) {
        case MAP:
            beginMap(ctx);
            return;

        case COLLECTION:
            beginCollection(ctx);
            return;

        default:
            scalar(ctx, value);
            break;
        }
    }

    public void setMaxDepth(int maxDepth) {
        this.maxDepth = maxDepth;
    }

    public int getMaxDepth() {
        return maxDepth;
    }

    public void setMaxVisited(int maxVisitedNodes) {
        this.maxVisited = maxVisitedNodes;
    }

    public int getMaxVisited() {
        return maxVisited;
    }
}
