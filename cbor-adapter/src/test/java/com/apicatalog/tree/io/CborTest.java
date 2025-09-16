package com.apicatalog.tree.io;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.DataItem;

@TestMethodOrder(OrderAnnotation.class)
class CborTest {


//    final static JsonGeneratorFactory FACTORY = Json.createGeneratorFactory(CONFIG);
//
    final static CborMaterializer MATERIALIZER = new CborMaterializer();

    @ParameterizedTest
    @MethodSource({ "resources" })
    @Order(1)
    void testMaterialize(String name) throws IOException, CborException {
        MATERIALIZER.accept(getCborResource(name), CborAdapter.instance());
        assertEquals(getCborResource(name), MATERIALIZER.value());
    }

    static final Stream<String> resources() throws IOException {
        URL url = CborTest.class.getResource("");

        return Stream.of(new File(url.getPath()).listFiles())
                .filter(File::isFile)
                .map(File::getName)
                .filter(name -> name.endsWith(".cbor"));
    }

    static String getResource(String name) throws IOException {
        try (BufferedInputStream is = new BufferedInputStream(CborTest.class.getResourceAsStream(name))) {
            return new BufferedReader(
                    new InputStreamReader(is, StandardCharsets.UTF_8))
                    .lines()
                    .collect(Collectors.joining("\n"));
        }
    }

    static List<DataItem> getCborResource(String name) throws CborException, IOException {
        return CborDecoder.decode(toByteArray(CborTest.class.getResourceAsStream(name)));
    }

    static List<DataItem> getCbor(String cbor) throws CborException {
        return CborDecoder.decode(cbor.getBytes(StandardCharsets.UTF_8));        
    }
    
    static byte[] toByteArray(InputStream in) throws IOException {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        byte[] data = new byte[8192]; // 8 KB buffer
        int nRead;
        while ((nRead = in.read(data, 0, data.length)) != -1) {
            buffer.write(data, 0, nRead);
        }
        return buffer.toByteArray();
    }

}
