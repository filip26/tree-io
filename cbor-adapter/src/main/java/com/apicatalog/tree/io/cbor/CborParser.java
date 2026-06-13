package com.apicatalog.tree.io.cbor;

import java.io.IOException;
import java.io.InputStream;

import com.apicatalog.tree.io.Tree;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeParser;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;

public class CborParser implements TreeParser {

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
    public Token nextToken() throws TreeIOException {
        
        
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Object getScalar() throws TreeIOException {
        // TODO Auto-generated method stub
        return null;
    }

}
