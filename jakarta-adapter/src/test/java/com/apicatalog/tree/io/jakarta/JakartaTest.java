package com.apicatalog.tree.io.jakarta;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.tree.io.Tree;
import com.apicatalog.tree.io.TreeIOException;

import jakarta.json.Json;
import jakarta.json.JsonReader;
import jakarta.json.JsonValue;
import jakarta.json.stream.JsonGenerator;
import jakarta.json.stream.JsonGeneratorFactory;

@TestMethodOrder(OrderAnnotation.class)
class JakartaTest {

    final static JsonGeneratorFactory FACTORY = Json.createGeneratorFactory(
            Map.of(JsonGenerator.PRETTY_PRINTING, true));

    @ParameterizedTest
    @MethodSource({ "resources" })
    void testReadWrite(String name) throws TreeIOException, IOException {
        var parser = new JakartaParser(Json.createParser(new StringReader(getResource(name))));
        var result = Tree.read(parser);

        var writer = new StringWriter();

        try (JsonGenerator generator = FACTORY.createGenerator(writer)) {
            Tree.write(result, new JakartaGenerator(generator));
        }

        assertEquals(getResource(name), writer.toString());
    }

    static final Stream<String> resources() throws TreeIOException {
        return Stream.of(new File(JakartaTest.class.getResource("").getPath()).listFiles())
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
