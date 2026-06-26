package com.apicatalog.tree.io;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.tree.io.jakcson.Jackson2Emitter;
import com.apicatalog.tree.io.jakcson.Jackson2Parser;
import com.fasterxml.jackson.core.JsonFactory;

@TestMethodOrder(OrderAnnotation.class)
class Jackson2Test {

    final static JsonFactory FACTORY = JsonFactory.builder().build();

    @ParameterizedTest
    @MethodSource({ "resources" })
    void testReadWrite(String name) throws IOException {

        Object tree = null;

        try (var parser = Jackson2Parser.createParser(new ByteArrayInputStream(getResource(name).getBytes()),
                FACTORY)) {
            tree = Tree.read(parser);
        }

        ByteArrayOutputStream bos = new ByteArrayOutputStream();

        try (var emitter = Jackson2Emitter.createEmitter(bos, FACTORY)) {
            Tree.write(tree, emitter);
        }

        assertEquals(getResource(name), bos.toString());
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
}
