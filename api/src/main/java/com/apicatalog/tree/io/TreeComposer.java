package com.apicatalog.tree.io;

public interface TreeComposer<T> extends TreeEmitter {

    T compose();
    
    //TODO
//    void reset();
//
//    boolean isComplete();
}
