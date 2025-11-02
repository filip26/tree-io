package com.apicatalog.tree.io.jakarta;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.Collections;

import com.apicatalog.tree.io.NodeParser;
import com.apicatalog.tree.io.PolyNode;

import jakarta.json.Json;
import jakarta.json.JsonReader;
import jakarta.json.JsonReaderFactory;

public class JakartaParser implements NodeParser {

    protected final JsonReaderFactory factory;

    public JakartaParser() {
        this(Json.createReaderFactory(Collections.emptyMap()));
    }
    
    public JakartaParser(JsonReaderFactory factory) {
        this.factory = factory;
    }

    @Override
    public PolyNode read(InputStream is) throws IOException {
        try (final JsonReader reader = factory.createReader(is, Charset.defaultCharset())) {
            return new PolyNode(reader.readValue(), JakartaAdapter.instance());
        }
    }

}
