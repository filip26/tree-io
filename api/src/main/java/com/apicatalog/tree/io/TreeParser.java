package com.apicatalog.tree.io;

import java.io.InputStream;

@FunctionalInterface
public interface TreeParser {

    Tree parse(InputStream is) throws TreeIOException;
}
