package com.apicatalog.tree.io.jakcson;

import java.io.IOException;
import java.io.InputStream;

import com.apicatalog.tree.io.NodeParser;
import com.apicatalog.tree.io.PolyNode;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

public final class Jackson2Parser implements NodeParser {

    private final ObjectMapper mapper;

    public Jackson2Parser(final ObjectMapper mapper) {
        this.mapper = mapper;
    }

    @Override
    public PolyNode parse(InputStream is) throws IOException {

        final JsonNode tree = mapper.readTree(is);

        return new PolyNode(tree, Jackson2Adapter.instance());
    }

}
