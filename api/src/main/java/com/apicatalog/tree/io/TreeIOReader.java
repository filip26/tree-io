package com.apicatalog.tree.io;

import java.io.IOException;
import java.io.InputStream;

public interface TreeIOReader {

    TreeIO parse(InputStream is) throws IOException;

}
