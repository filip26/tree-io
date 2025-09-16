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
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import jakarta.json.Json;
import jakarta.json.JsonReader;
import jakarta.json.JsonValue;
import jakarta.json.stream.JsonGenerator;
import jakarta.json.stream.JsonGeneratorFactory;

@TestMethodOrder(OrderAnnotation.class)
class JakartaTest {

    // Enable pretty printing
    final static Map<String, Object> CONFIG = new HashMap<>();

    static {
        CONFIG.put(JsonGenerator.PRETTY_PRINTING, true);
    }

    final static JsonGeneratorFactory FACTORY = Json.createGeneratorFactory(CONFIG);

    final static JakartaMaterializer MATERIALIZER = new JakartaMaterializer();

    @ParameterizedTest
    @MethodSource({ "resources" })
    @Order(0)
    void testWrite(String name) throws IOException {

        ByteArrayOutputStream bos = new ByteArrayOutputStream();

        try (JsonGenerator generator = FACTORY.createGenerator(bos)) {
            JakartaWriter writer = new JakartaWriter(generator);
            writer.node(getJsonResource(name), JakartaAdapter.instance());
        }
        assertEquals(getJsonResource(name), getJson(bos.toString()));
    }

    @ParameterizedTest
    @MethodSource({ "resources" })
    @Order(1)
    void testMaterialize(String name) throws IOException {
        MATERIALIZER.node(getJsonResource(name), JakartaAdapter.instance());
        assertEquals(getJsonResource(name), MATERIALIZER.json());
    }

    static final Stream<String> resources() throws IOException {
        URL url = JakartaTest.class.getResource("");

        return Stream.of(new File(url.getPath()).listFiles())
                .filter(File::isFile)
                .map(File::getName)
                .filter(name -> name.endsWith(".json"));
    }

    static String getResource(String name) throws IOException {
        try (BufferedInputStream is = new BufferedInputStream(JakartaTest.class.getResourceAsStream(name))) {
            return new BufferedReader(
                    new InputStreamReader(is, StandardCharsets.UTF_8))
                    .lines()
                    .collect(Collectors.joining("\n"));
        }
    }

    static JsonValue getJsonResource(String name) {
        try (JsonReader reader = Json.createReader(
                JakartaTest.class.getResourceAsStream(name))) {
            return reader.read();
        }
    }

    static JsonValue getJson(String json) {
        try (JsonReader reader = Json.createReader(new StringReader(json))) {
            return reader.read();
        }
    }

}
