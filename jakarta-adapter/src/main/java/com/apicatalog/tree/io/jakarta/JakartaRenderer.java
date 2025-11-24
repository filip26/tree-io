package com.apicatalog.tree.io.jakarta;

import java.io.OutputStream;
import java.util.Collections;

import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeRenderer;

import jakarta.json.Json;
import jakarta.json.stream.JsonGenerator;
import jakarta.json.stream.JsonGeneratorFactory;

public class JakartaRenderer implements TreeRenderer {

    private final JsonGeneratorFactory factory;

    public JakartaRenderer() {
        this(Json.createGeneratorFactory(Collections.emptyMap()));
    }

    public JakartaRenderer(JsonGeneratorFactory factory) {
        this.factory = factory;
    }

    @Override
    public void render(Object node, TreeAdapter adapter, OutputStream os) throws TreeIOException {
        try (final var generator = factory.createGenerator(os)) {
            (new JakartaGenerator(generator)).node(node, adapter);
        }
    }
}
