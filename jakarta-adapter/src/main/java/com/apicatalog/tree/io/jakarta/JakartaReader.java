package com.apicatalog.tree.io.jakarta;

import java.io.IOException;
import java.io.InputStream;

import com.apicatalog.tree.io.NodeReader;
import com.apicatalog.tree.io.PolyNode;

import jakarta.json.JsonReader;
import jakarta.json.JsonReaderFactory;

public class JakartaReader implements NodeReader {

    protected final JsonReaderFactory factory;

    public JakartaReader(JsonReaderFactory factory) {
        this.factory = factory;
    }

    @Override
    public PolyNode read(InputStream is) throws IOException {
        try (final JsonReader reader = factory.createReader(is)) {
            return new PolyNode(reader.read(), JakartaAdapter.instance());
        }
    }

}
