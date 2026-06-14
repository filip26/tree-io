package com.apicatalog.tree.io.jakarta;

import java.io.InputStream;
import java.io.Reader;
import java.util.Map;

import com.apicatalog.tree.io.Tree;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeProcessor;
import com.apicatalog.tree.io.TreeReader;

import jakarta.json.Json;
import jakarta.json.stream.JsonParserFactory;

public final class JakartaReader implements TreeReader, TreeProcessor {

    private final JsonParserFactory factory;

    public JakartaReader() {
        this(Json.createParserFactory(Map.of()));
    }

    public JakartaReader(JsonParserFactory factory) {
        this.factory = factory;
    }

    @Override
    public Features features() {
        return JakartaAdapter.FEATURES;
    }

    @Override
    public Object read(InputStream is) throws TreeIOException {
        return Tree.read(new JakartaParser(factory.createParser(is)));
    }
    
    public Object read(Reader reader) throws TreeIOException {
        return Tree.read(new JakartaParser(factory.createParser(reader)));
    }

}
