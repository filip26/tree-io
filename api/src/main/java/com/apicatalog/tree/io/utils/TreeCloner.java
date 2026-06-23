package com.apicatalog.tree.io.utils;

import com.apicatalog.tree.io.TreeGenerator;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeParser;

class TreeCloner {

    //TODO maxEvents, etc.
    
    /**
     * A high-level utility method that fully traverses the tree and drives the
     * provided {@link TreeGenerator}. This is the primary method for tree
     * transformation, serialization, or deep cloning. It iterates through every
     * node using {@link TreeParser#next()} and emits a corresponding event to the generator.
     *
     * @param parser
     * @param generator the generator that will receive construction events.
     * @throws TreeIOException       if the generator encounters an I/O error.
     * @throws IllegalStateException if the source tree is malformed (e.g., unclosed
     *                               structures).
     */
    public static void copy(TreeParser parser, TreeGenerator generator) throws TreeIOException {
        while (true) {
            switch (parser.next()) {
            case BEGIN_MAP:
                generator.beginMap(parser.context());
                continue;

            case END_MAP:
                generator.endMap(parser.context());
                continue;

            case BEGIN_SEQUENCE:
                generator.beginSequence(parser.context());
                continue;

            case END_SEQUENCE:
                generator.endSequence(parser.context());
                continue;

            case SCALAR:
                switch (parser.nodeType()) {
                case NULL -> generator.nullValue(parser.context());
                case TRUE -> generator.booleanValue(parser.context(), true);
                case FALSE -> generator.booleanValue(parser.context(), false);
                case STRING -> generator.stringValue(parser.context(), parser.stringValue());
                case NUMBER -> generator.numberValue(parser.context(), parser.numberValue());
                case BINARY -> generator.binaryValue(parser.context(), parser.binaryValue());

                default -> throw new IllegalArgumentException(
                        """
                        Unexpected node type=%s"
                        """.formatted(parser.nodeType()));
                }
                continue;

            case null:
//                if (traversal.dept depth > 0) {
//                    throw new IllegalStateException("The traversed tree is malformed. A map or a collection was not properly closed.");
//                }
                return;
            }
        }

//        if (stack.peek() != NodeContext.ROOT) {
//            throw new IllegalStateException();
//        }
    }
}
