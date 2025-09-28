package com.apicatalog.tree.io;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.List;
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

    final static CborMaterializer MATERIALIZER = new CborMaterializer();

    @ParameterizedTest
    @MethodSource({ "resources" })
    @Order(1)
    void testMaterialize(String name) throws IOException, CborException {
        MATERIALIZER.node(getCborResource(name).iterator().next(), CborAdapter.instance());
        assertEquals(getCborResource(name).iterator().next(), MATERIALIZER.cbor());
    }

    static final Stream<String> resources() throws IOException {
        URL url = CborTest.class.getResource("");

        return Stream.of(new File(url.getPath()).listFiles())
                .filter(File::isFile)
                .map(File::getName)
                .filter(name -> name.endsWith(".cbor"));
    }

    static List<DataItem> getCborResource(String name) throws CborException, IOException {
        return CborDecoder.decode(toByteArray(CborTest.class.getResourceAsStream(name)));
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
