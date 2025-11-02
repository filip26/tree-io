package com.apicatalog.tree.io;

import java.io.IOException;
import java.io.InputStream;

public interface NodeParser {

    PolyNode read(InputStream is) throws IOException;
        
}
