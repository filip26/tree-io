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

import com.apicatalog.tree.io.jakcson.Jackson2Reader;
import com.apicatalog.tree.io.jakcson.Jackson2Writer;
import com.fasterxml.jackson.core.JsonFactory;

@TestMethodOrder(OrderAnnotation.class)
class Jackson2Test {

    final static JsonFactory FACTORY = JsonFactory.builder().build();

    final static Jackson2Reader READER = new Jackson2Reader(FACTORY);
    final static Jackson2Writer WRITER = new Jackson2Writer(FACTORY);
    
    @ParameterizedTest
    @MethodSource({ "resources" })
    void testReadWrite(String name) throws IOException {

        var tree = READER.read(new ByteArrayInputStream(getResource(name).getBytes()));
        
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        WRITER.write(tree, bos);

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
