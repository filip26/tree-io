package com.apicatalog.tree.io.jakarta;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.Collections;

import com.apicatalog.tree.io.NodeReader;
import com.apicatalog.tree.io.PolyNode;

import jakarta.json.Json;
import jakarta.json.JsonReader;
import jakarta.json.JsonReaderFactory;

public class JakartaReader implements NodeReader {

    protected final JsonReaderFactory factory;

    public JakartaReader() {
        this(Json.createReaderFactory(Collections.emptyMap()));
    }
    
    public JakartaReader(JsonReaderFactory factory) {
        this.factory = factory;
    }

    @Override
    public PolyNode read(InputStream is) throws IOException {
        try (final JsonReader reader = factory.createReader(is, Charset.defaultCharset())) {
            return new PolyNode(reader.readValue(), JakartaAdapter.instance());
        }
    }

}
