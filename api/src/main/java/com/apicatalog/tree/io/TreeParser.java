package com.apicatalog.tree.io;

import java.io.IOException;
import java.io.InputStream;

@FunctionalInterface
public interface TreeParser {

    TreeIO parse(InputStream is) throws IOException;
}
