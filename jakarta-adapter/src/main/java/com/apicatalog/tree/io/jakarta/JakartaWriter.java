package com.apicatalog.tree.io.jakarta;

import java.io.OutputStream;
import java.io.Writer;
import java.util.Collections;

import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeTraversal;

import jakarta.json.Json;
import jakarta.json.stream.JsonGeneratorFactory;

public class JakartaWriter {

    private final JsonGeneratorFactory factory;

    public JakartaWriter() {
        this(Json.createGeneratorFactory(Collections.emptyMap()));
    }

    public JakartaWriter(JsonGeneratorFactory factory) {
        this.factory = factory;
    }

    public void write(Object node, OutputStream os) throws TreeIOException {
        try (final var generator = factory.createGenerator(os)) {
            var jsonGenerator = new JakartaGenerator(generator);
            (new TreeTraversal(node)).generate(jsonGenerator);
        }
    }

    public void write(Object node, Writer writer) throws TreeIOException {
        try (final var generator = factory.createGenerator(writer)) {
            var jsonGenerator = new JakartaGenerator(generator);
            (new TreeTraversal(node)).generate(jsonGenerator);
        }
    }
}
