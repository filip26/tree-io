package com.apicatalog.tree.io;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.tree.io.cbor.CborAdapter;
import com.apicatalog.tree.io.cbor.CborMaterializer;
import com.apicatalog.tree.io.cbor.CborParser;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.DataItem;

@TestMethodOrder(OrderAnnotation.class)
class CborTest {

    final static CborParser PARSER = new CborParser();

    @ParameterizedTest
    @MethodSource({ "resources" })
    @Order(10)
    void testRender(String name) throws IOException, CborException, TreeIOException {

        var materializer = new CborMaterializer();

        materializer.structure(getCborResource(name).iterator().next(), CborAdapter.instance());

        assertEquals(getCborResource(name).iterator().next(), materializer.cbor());
    }

    @ParameterizedTest
    @MethodSource({ "resources" })
    @Order(20)
    void testParse(String name) throws IOException, CborException, TreeIOException {

        var tree = PARSER.parse(CborTest.class.getResourceAsStream(name));

        var match = TreeComparison.deepEquals(
                new Tree(
                        getCborResource(name),
                        CborAdapter.instance()),
                tree);

        assertTrue(match);
    }

    static final Stream<String> resources() throws IOException {
        URL url = CborTest.class.getResource("");

        return Stream.of(new File(url.getPath()).listFiles())
                .filter(File::isFile)
                .map(File::getName)
                .filter(name -> name.endsWith(".cbor"));
    }

    static List<DataItem> getCborResource(String name) throws CborException, IOException {
        return CborDecoder.decode(CborTest.class.getResourceAsStream(name).readAllBytes());
    }
}
