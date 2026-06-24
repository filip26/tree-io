package com.apicatalog.tree.io;

import java.io.IOException;

import com.apicatalog.tree.io.Tree.Event;
import com.apicatalog.tree.io.Tree.EventConsumer;

/**
 * Provides a uniform, performant, pull-based streaming abstraction for parsing
 * tree-like data structures. This interface decouples the process of reading a
 * tree from its underlying representation, making it suitable for both
 * deserialization and re-materialization.
 * <p>
 * The parser is forward-only and token-driven, maintaining the internal state
 * of the current position in the input stream. It emits fine-grained structural
 * and scalar tokens. The parser keeps track structural roles.
 * </p>
 */
public interface TreeParser extends TreeCursor {

    default boolean parse(EventConsumer consumer) throws IOException {
        var event = next();
        while (event != null) {
            if (!consumer.accept(event, this)) {
                return false;
            }
            event = next();
        }
        return true;
    }

    /**
     * Advances the cursor to the next token in the stream and returns its type.
     *
     * @return the next structural or scalar {@link Event} in the sequence, or null
     *         if the end of the input (EOF) has been reached.
     * @throws IOException
     */
    Event next() throws IOException;
}