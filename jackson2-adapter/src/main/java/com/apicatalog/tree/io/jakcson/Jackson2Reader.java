package com.apicatalog.tree.io.jakcson;

import java.io.IOException;
import java.io.InputStream;

import com.apicatalog.tree.io.Tree;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.TreeProcessor;
import com.apicatalog.tree.io.TreeReader;
import com.fasterxml.jackson.core.JsonFactory;

public final class Jackson2Reader implements TreeReader<Object>, TreeProcessor {

    private final JsonFactory factory;

    public Jackson2Reader(JsonFactory factory) {
        this.factory = factory;
    }

    @Override
    public Features features() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Object read(InputStream is) throws IOException {
        try (var jsonParser = factory.createParser(is)) {
            var parser = new Jackson2Parser(jsonParser);
            return Tree.read(parser);
        }
    }

}
