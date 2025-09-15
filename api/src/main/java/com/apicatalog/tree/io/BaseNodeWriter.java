package com.apicatalog.tree.io;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.Deque;

public abstract class BaseNodeWriter extends DepthFirstTraversal {

    /**
     * Indicates the type of node currently being visited during traversal.
     */
    public enum Context {

        /**
         * Visiting the key/name of a property in a map/object.
         */
        PROPERTY_KEY,

        /**
         * Visiting the value of a property in a map/object.
         */
        PROPERTY_VALUE,

        /**
         * Visiting an element within a collection, array, or list.
         */
        COLLECTION_ELEMENT,
//
//        /**
//         * Visiting a primitive/scalar node (string, number, boolean, etc.).
//         */
//        SCALAR
    }

    protected final Deque<Context> context;

    protected BaseNodeWriter(Deque<Object> stack, NodeAdapter adapter) {
        super(stack, adapter);
        this.context = new ArrayDeque<>();
    }

    protected abstract void writeNull() throws IOException;

    protected abstract void writeString(String value) throws IOException;

    protected abstract void writeNumber(BigInteger value) throws IOException;

    protected abstract void writeNumber(BigDecimal value) throws IOException;

    protected abstract void writeNumber(long value) throws IOException;

    protected abstract void writeNumber(double value) throws IOException;

    protected abstract void writeBoolean(boolean value) throws IOException;

    protected abstract void writeByteArray(byte[] value) throws IOException;

    protected abstract void beginMap() throws IOException;

    protected abstract void endMap() throws IOException;

    protected abstract void beginCollection() throws IOException;

    protected abstract void endCollection() throws IOException;

    public void write() throws IOException {

        final IOException[] exception = new IOException[1]; // mutable holder
        exception[0] = null;

        while (exception[0] != null
                && traverse(t -> {
                    try {
                        write(t);
                    } catch (IOException e) {
                        exception[0] = e;
                    }
                }))
            ;

        if (exception[0] != null) {
            throw exception[0];
        }

        if (!context.isEmpty()) {
            throw new IllegalStateException();
        }
    }

    protected void write(Object value) throws IOException {

        if (adapter.isNull(value)) {
            writeNull();
            if (Context.PROPERTY_KEY == context.peek()) {
                context.pop();
                context.push(Context.PROPERTY_VALUE);
            }
            return;
        }

        if (context.size() > depth()) {
            Context previous = context.pop();

            if (Context.PROPERTY_VALUE == previous) {
                endMap();

            } else if (Context.COLLECTION_ELEMENT == previous) {
                endCollection();

            } else {
                throw new IllegalStateException();
            }
        }

        switch (adapter.type(value)) {
        case MAP:
            beginMap();
            if (Context.PROPERTY_KEY == context.peek()) {
                context.pop();
                context.push(Context.PROPERTY_VALUE);
            }
            if (context.size() < depth()) {
                context.push(Context.PROPERTY_KEY);
            } else {
                endMap();
            }
            return;

        case COLLECTION:
            beginCollection();
            if (Context.PROPERTY_KEY == context.peek()) {
                context.pop();
                context.push(Context.PROPERTY_VALUE);
            }
            if (context.size() < depth()) {
                context.push(Context.COLLECTION_ELEMENT);
            } else {
                endCollection();
            }
            return;

        case STRING:
            writeString(adapter.stringValue(value));
            break;

        case BINARY:
            writeByteArray(adapter.binaryValue(value));
            break;

        case NUMBER:
            // TODO
            break;

        case FALSE:
            writeBoolean(false);
            break;

        case TRUE:
            writeBoolean(true);
            break;

        case NULL:
            writeNull();
            break;
        }
        if (Context.PROPERTY_KEY == context.peek()) {
            context.pop();
            context.push(Context.PROPERTY_VALUE);
        }
    }
}
