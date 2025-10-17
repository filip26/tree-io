package com.apicatalog.tree.io;

import java.math.BigDecimal;
import java.math.BigInteger;

public interface NodeMaterializer<T> {

    T node(String text);
    
    T node(BigDecimal number);
    T node(double number);
    T node(float number);
    
    T node(BigInteger number);
    T node(long number);
    T node(int number);
    
    T node(boolean value);
    
//    T node()
}
