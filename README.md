# tree-io

**Uniform API to read/write tree data models — cross-format, cross-library.**

**tree-io** provides a consistent abstraction for working with **tree-like data structures**.  It is **format-agnostic** (JSON, YAML, CBOR, …) and **library-agnostic** (Jackson, Gson, Jakarta, …),  allowing you to **read, manipulate, and write trees uniformly** without depending on a specific parser or serializer.

## Features

- Uniform, library-agnostic API for tree data processing
- Supports multiple formats: JSON, YAML, CBOR
- Works with Jackson, Gson, Jakarta, and other libraries
- Extensible adapter model for adding new formats or libraries

## Use Cases

- Uniform processing of tree-structured data
- Building library-agnostic processors and pipelines

## Implementations

 Adapter               | Version | Documentation
-----------------------|---------|---------------
 Jakarta JSON API      | [![Maven Central](https://img.shields.io/maven-central/v/com.apicatalog/tree-io-jakarta.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:com.apicatalog%20AND%20a:tree-io-jakarta) | [![javadoc](https://javadoc.io/badge2/com.apicatalog/tree-io-jakarta/javadoc.svg)](https://javadoc.io/doc/com.apicatalog/tree-io-jakarta) 
 Jackson 2 Tree Model  | [![Maven Central](https://img.shields.io/maven-central/v/com.apicatalog/tree-io-jackson.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:com.apicatalog%20AND%20a:tree-io-jackson) | [![javadoc](https://javadoc.io/badge2/com.apicatalog/tree-io-jackson/javadoc.svg)](https://javadoc.io/doc/com.apicatalog/tree-io-jackson) 
 CBOR                  | [![Maven Central](https://img.shields.io/maven-central/v/com.apicatalog/tree-io-cbor.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:com.apicatalog%20AND%20a:tree-io-cbor) | [![javadoc](https://javadoc.io/badge2/com.apicatalog/tree-io-cbor/javadoc.svg)](https://javadoc.io/doc/com.apicatalog/tree-io-cbor) 

## Artifacts

```xml
<!-- Core API: uniform interfaces implemented by all adapters -->
<dependency>
    <groupId>com.apicatalog</groupId>
    <artifactId>tree-io-api</artifactId>
    <version>${tree-io.version}</version>
</dependency>

<!-- Available adapters: include the one(s) matching your use case -->

<!-- Jakarta JSON API adapter -->
<dependency>
    <groupId>com.apicatalog</groupId>
    <artifactId>tree-io-jakarta</artifactId>
    <version>${tree-io.version}</version>
</dependency>

<!-- Jackson 2 Tree Model adapter -->
<dependency>
    <groupId>com.apicatalog</groupId>
    <artifactId>tree-io-jackson2</artifactId>
    <version>${tree-io.version}</version>
</dependency>

<!-- CBOR adapter -->
<dependency>
    <groupId>com.apicatalog</groupId>
    <artifactId>tree-io-cbor</artifactId>
    <version>${tree-io.version}</version>
</dependency>
```

## Contributing

Contributions are welcome! Please submit a pull request.

### Building

Fork and clone the repository, then build with Maven:

```bash
> cd tree-io
> mvn package
```

## Commercial Support

Commercial support and consulting are available.  
For inquiries, please contact: filip26@gmail.com
