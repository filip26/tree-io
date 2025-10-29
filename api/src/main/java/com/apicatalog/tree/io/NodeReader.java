package com.apicatalog.tree.io;

import java.io.IOException;
import java.io.InputStream;

public interface NodeReader {

    PolyNode read(InputStream reader) throws IOException;
        
}
