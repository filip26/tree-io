package com.apicatalog.tree.io.traverse;

import com.apicatalog.tree.io.NodeGenerator;

/**
 * A specialized class that builds a native object model from any tree-like
 * source.
 * <p>
 * This class implements both {@link Visitor} and
 * {@link NodeGenerator}, allowing it to act as a self-contained transformation
 * engine. It traverses a source structure using its {@code NodeVisitor}
 * capabilities and consumes its own traversal events via its
 * {@code NodeGenerator} implementation to construct a {@link Object} tree in
 * memory.
 * </p>
 * <p>
 * The class is stateful and designed for a single transformation. It can be
 * reused by calling the {@link #reset()} method.
 * </p>
 */
public class Materializer extends Visitor {

//    protected final Deque<Object> stack;
    
    /**
     * The primary entry point for materialization. Traverses the given source node.
     *
     * @param node    the source root node to traverse
     * @param adapter the adapter for interpreting the source node's structure
     * @return the fully materialized object
     * @throws IOException if an error occurs during generation
     */
//    public Object adapt(NodeFactory<?, ?> target, Object node, NodeAdapter adapter) throws IOException {
//        root(node, adapter);
//        
//        while (next()) {
//
//            if (Context.END == currentNodeContext) {
//                generator.end();
//                continue;
//            }
//
//            switch (currentNodeType) {
//            case MAP:
//                generator.beginMap();
//                break;
//
//            case COLLECTION:
//                generator.beginList();
//                break;
//
//            case NULL:
//                generator.nullValue();
//                break;
//
//            case TRUE:
//                generator.booleanValue(true);
//                break;
//
//            case FALSE:
//                generator.booleanValue(false);
//                break;
//
//            case STRING:
//                generator.stringValue(adapter().stringValue(currentNode));
//                break;
//
//            case BINARY:
//                generator.binaryValue(adapter().binaryValue(currentNode));
//                break;
//
//            case NUMBER:
//                if (adapter().isIntegral(currentNode)) {
//                    generator.numericValue(adapter().bigIntegerValue(currentNode));
//                } else {
//                    generator.numericValue(adapter().asDecimal(currentNode));
//                }
//                break;
//                
//            default:
//                throw new IllegalStateException("Unexpected node type: " + currentNodeType);
//            }
//        }
//
//        return object;
//    }
    
}
