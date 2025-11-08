package com.apicatalog.tree.io.jakarta;

import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.Collections;

import com.apicatalog.tree.io.TreeIO;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeParser;

import jakarta.json.Json;
import jakarta.json.JsonException;
import jakarta.json.JsonReader;
import jakarta.json.JsonReaderFactory;

public final class JakartaParser implements TreeParser {

    private final JsonReaderFactory factory;

    public JakartaParser() {
        this(Json.createReaderFactory(Collections.emptyMap()));
    }

    public JakartaParser(JsonReaderFactory factory) {
        this.factory = factory;
    }

    @Override
    public TreeIO parse(InputStream is) throws TreeIOException {
        try (final JsonReader reader = factory.createReader(is, Charset.defaultCharset())) {
            return new TreeIO(reader.readValue(), JakartaAdapter.instance());
        } catch (JsonException | IllegalStateException e) {
            throw new TreeIOException(e);
        }
    }
}
