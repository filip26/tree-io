package com.apicatalog.tree.io;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Provides a uniform abstraction for writing tree-like data structures.
 */
public interface NodeGenerator {

    // --- scalars ---

    void nullValue() throws IOException;

    void booleanValue(boolean node) throws IOException;

    void stringValue(String node) throws IOException;

    void numericValue(long node) throws IOException;
    void numericValue(BigInteger node) throws IOException;

    void numericValue(double node) throws IOException;
    void numericValue(BigDecimal node) throws IOException;

    void binaryValue(byte[] node) throws IOException;

    // --- structures --

    void beginMap() throws IOException;

    void beginCollection() throws IOException;

    void end() throws IOException;
}
