package com.apicatalog.tree.io.jakarta;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.Map;

import com.apicatalog.tree.io.Tree;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.TreeProcessor;
import com.apicatalog.tree.io.TreeReader;

import jakarta.json.Json;
import jakarta.json.stream.JsonParserFactory;

public final class JakartaReader implements TreeReader<Object>, TreeProcessor {

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
    public Object read(InputStream is) throws IOException {
        try (var parser = factory.createParser(is)) {
            return Tree.read(new JakartaParser(parser));
        }
    }

    public Object read(Reader reader) throws IOException {
        try (var parser = factory.createParser(reader)) {
            return Tree.read(new JakartaParser(parser));
        }
    }
}
