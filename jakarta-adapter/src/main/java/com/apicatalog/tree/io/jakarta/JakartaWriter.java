package com.apicatalog.tree.io.jakarta;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Map;

import com.apicatalog.tree.io.Tree;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.TreeProcessor;
import com.apicatalog.tree.io.TreeWriter;

import jakarta.json.Json;
import jakarta.json.stream.JsonGeneratorFactory;

public class JakartaWriter implements TreeWriter<Object>, TreeProcessor {

    private final JsonGeneratorFactory factory;

    public JakartaWriter() {
        this(Json.createGeneratorFactory(Map.of()));
    }

    public JakartaWriter(JsonGeneratorFactory factory) {
        this.factory = factory;
    }

    @Override
    public Features features() {
        return JakartaAdapter.FEATURES;
    }

    @Override
    public void write(Object node, OutputStream os) throws IOException {
        try (final var generator = factory.createGenerator(os)) {
            var jsonGenerator = new JakartaEmitter(generator);
            Tree.write(node, jsonGenerator);
        }
    }

    public void write(Object node, Writer writer) throws IOException {
        try (final var generator = factory.createGenerator(writer)) {
            var jsonGenerator = new JakartaEmitter(generator);
            Tree.write(node, jsonGenerator);
        }
    }
}
