package com.apicatalog.tree.io;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.tree.io.jakcson.Jackson2Adapter;
import com.apicatalog.tree.io.jakcson.Jackson2Writer;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

@TestMethodOrder(OrderAnnotation.class)
class Jackson2Test {

    final static ObjectMapper MAPPER = new ObjectMapper();
    final static JsonFactory FACTORY = JsonFactory.builder().build();

    @ParameterizedTest
    @MethodSource({ "resources" })
    @Order(0)
    void testWrite(String name) throws IOException {

        ByteArrayOutputStream bos = new ByteArrayOutputStream();

        try (JsonGenerator generator = FACTORY.createGenerator(bos)) {
            Jackson2Writer writer = new Jackson2Writer(generator);
            writer.node(getJsonResource(name), Jackson2Adapter.instance());
        }
        assertEquals(getJsonResource(name), getJson(bos.toString()));
    }

    static final Stream<String> resources() throws IOException {
        URL url = Jackson2Test.class.getResource("");

        return Stream.of(new File(url.getPath()).listFiles())
                .filter(File::isFile)
                .map(File::getName)
                .filter(name -> name.endsWith(".json"));
    }

    static String getResource(String name) throws IOException {
        try (BufferedInputStream is = new BufferedInputStream(Jackson2Test.class.getResourceAsStream(name))) {
            return new BufferedReader(
                    new InputStreamReader(is, StandardCharsets.UTF_8))
                    .lines()
                    .collect(Collectors.joining("\n"));
        }
    }

    static JsonNode getJsonResource(String name) throws IOException {
        return MAPPER.readTree(Jackson2Test.class.getResourceAsStream(name));
    }

    static JsonNode getJson(String json) throws IOException {
        return MAPPER.readTree(new StringReader(json));
    }
}
