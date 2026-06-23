package com.apicatalog.tree.io.jakarta;

import java.io.OutputStream;
import java.io.Writer;
import java.util.Collections;

import com.apicatalog.tree.io.Tree;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeProcessor;
import com.apicatalog.tree.io.TreeWriter;
import com.apicatalog.tree.io.Tree.Features;

import jakarta.json.Json;
import jakarta.json.stream.JsonGeneratorFactory;

public class JakartaWriter implements TreeWriter<Object>, TreeProcessor {

    private final JsonGeneratorFactory factory;

    public JakartaWriter() {
        this(Json.createGeneratorFactory(Collections.emptyMap()));
    }

    public JakartaWriter(JsonGeneratorFactory factory) {
        this.factory = factory;
    }
    
    @Override
    public Features features() {
        return JakartaAdapter.FEATURES;
    }

    @Override
    public void write(Object node, OutputStream os) throws TreeIOException {
        try (final var generator = factory.createGenerator(os)) {
            var jsonGenerator = new JakartaGenerator(generator);
            Tree.write(node, jsonGenerator);
        }
    }

    public void write(Object node, Writer writer) throws TreeIOException {
        try (final var generator = factory.createGenerator(writer)) {
            var jsonGenerator = new JakartaGenerator(generator);
            Tree.write(node, jsonGenerator);
        }
    }
}
