package com.apicatalog.tree.io.jakcson;

import java.io.IOException;
import java.io.InputStream;

import com.apicatalog.tree.io.Tree;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeParser;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

public final class Jackson2Parser implements TreeParser {

    private final ObjectMapper mapper;

    public Jackson2Parser(final ObjectMapper mapper) {
        this.mapper = mapper;
    }

    @Override
    public Tree parse(InputStream is) throws TreeIOException {
        try {
            final JsonNode tree = mapper.readTree(is);
            return new Tree(tree, Jackson2Adapter.instance());
        } catch (IOException e) {
            throw new TreeIOException(e);
        }
    }
}
