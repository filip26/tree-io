package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Iterator;
import java.util.Map.Entry;

public interface NodeFactory<K, V> {

    Features features();
    
    V node(String text);

    V node(BigDecimal number);

    V node(double number);

    V node(float number);

    V node(BigInteger number);

    V node(long number);

    V node(int number);

    V node(boolean value);
    
    V node(byte[] binary);

    V mapNode(Iterator<Entry<K, V>> entries);
    
    V arrayNode(Iterator<V> elements);
}
