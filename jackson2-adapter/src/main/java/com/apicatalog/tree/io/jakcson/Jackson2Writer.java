package com.apicatalog.tree.io.jakcson;

import java.io.IOException;
import java.io.OutputStream;

import com.apicatalog.tree.io.Tree;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeProcessor;
import com.apicatalog.tree.io.fnc.TreeWriter;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;

public final class Jackson2Writer implements TreeWriter, TreeProcessor {

    private final JsonFactory factory;
    
    public Jackson2Writer(JsonFactory factory) {
        this.factory = factory;
    }
    
    @Override
    public Features features() {
        return Jackson2Adapter.FEATURES;
    }

    @Override
    public void write(Object node, OutputStream os) throws TreeIOException {
        try (JsonGenerator generator = factory.createGenerator(os)) {
            Jackson2Generator writer = new Jackson2Generator(generator);
            Tree.write(node, writer);
            
        } catch (IOException e) {
            throw new TreeIOException(e);
        }        
    }

}
