package com.apicatalog.tree.io.cbor;

import com.apicatalog.tree.io.Tree.Event;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeType;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeParser;
import com.apicatalog.tree.io.TreeProcessor;

import co.nstant.in.cbor.CborDecoder;

public class CborParser implements TreeParser, TreeProcessor {

    CborDecoder decoder;
    
//    @Override
//    public Tree parse(InputStream is) throws TreeIOException {
//        try {
//            return new Tree(
//                    CborDecoder.decode(is.readAllBytes()),
//                    CborAdapter.instance());
//        } catch (CborException | IOException e) {
//            throw new TreeIOException(e);
//        }
//    }

    @Override
    public Features features() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Event next() throws TreeIOException {
        
        
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Number numberValue() throws TreeIOException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String stringValue() throws TreeIOException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public byte[] binaryValue() throws TreeIOException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public NodeType nodeType() {
        // TODO Auto-generated method stub
        return null;
    }
}
