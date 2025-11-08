package com.apicatalog.tree.io.jakcson;

import java.io.IOException;
import java.io.OutputStream;

import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeRenderer;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;

public class Jackson2Renderer implements TreeRenderer {

    private final JsonFactory factory;

    public Jackson2Renderer(final JsonFactory factory) {
        this.factory = factory;
    }

    @Override
    public void render(Object node, TreeAdapter adapter, OutputStream os) throws TreeIOException {
        try (JsonGenerator generator = factory.createGenerator(os)) {
            (new Jackson2Generator(generator)).node(node, adapter);
        } catch (IOException e) {
            throw new TreeIOException(e);
        }
    }
}
