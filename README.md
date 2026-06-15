# 🌳 Tree I/O

**Uniform API to read/write heterogeneous tree data models - cross-format, cross-library.**

**tree-io** [tri-yo]  provides a consistent and lightweight abstraction for working with heterogeneous hierarchical data structures.  It is format-agnostic (JSON, YAML, CBOR, …) and library-agnostic,  allowing you to read, manipulate, and write trees uniformly without depending on a specific parser or generator.

## ✨ Features

- 🌳 Uniform Tree Processing - handle any hierarchical and heterogeneous data structure consistently across formats and libraries  
- 🌐 Library-agnostic API for tree data processing - Parser, Traverser, Generator
- 🗂️ Multi-format support
- 🛠️ Extensible model for adding new formats and libraries

## 🏗️ Use Cases

- Uniform processing of heterogeneous tree models
- Building library-agnostic processors and pipelines
- Manipulating heterogeneous hierarchical data in a consistent way

## 🔌 Implementations of Tree I/O API

 Artifact               | Version | Javadoc
-----------------------|---------|---------------
**Tree I/O API**           | [![Maven Central](https://img.shields.io/maven-central/v/com.apicatalog/tree-io-api.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:com.apicatalog%20AND%20a:tree-io-api) | [![javadoc](https://javadoc.io/badge2/com.apicatalog/tree-io-api/javadoc.svg)](https://javadoc.io/doc/com.apicatalog/tree-io-api) 
 Jakarta JSON API      | [![Maven Central](https://img.shields.io/maven-central/v/com.apicatalog/tree-io-jakarta.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:com.apicatalog%20AND%20a:tree-io-jakarta) | [![javadoc](https://javadoc.io/badge2/com.apicatalog/tree-io-jakarta/javadoc.svg)](https://javadoc.io/doc/com.apicatalog/tree-io-jakarta) 
 Jackson 2 Tree Model  | [![Maven Central](https://img.shields.io/maven-central/v/com.apicatalog/tree-io-jackson2.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:com.apicatalog%20AND%20a:tree-io-jackson2) | [![javadoc](https://javadoc.io/badge2/com.apicatalog/tree-io-jackson2/javadoc.svg)](https://javadoc.io/doc/com.apicatalog/tree-io-jackson2) 
 CBOR                  | [![Maven Central](https://img.shields.io/maven-central/v/com.apicatalog/tree-io-cbor.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:com.apicatalog%20AND%20a:tree-io-cbor) | [![javadoc](https://javadoc.io/badge2/com.apicatalog/tree-io-cbor/javadoc.svg)](https://javadoc.io/doc/com.apicatalog/tree-io-cbor) 

## 📦 Artifacts

```xml
<!-- Core API: uniform interfaces implemented by all adapters -->
<dependency>
    <groupId>com.apicatalog</groupId>
    <artifactId>tree-io-api</artifactId>
    <version>${tree-io.version}</version>
</dependency>

<!-- Available adapters: include the one(s) matching your use case -->

<!-- Jakarta JSON adapter -->
<dependency>
    <groupId>com.apicatalog</groupId>
    <artifactId>tree-io-jakarta</artifactId>
    <version>${tree-io.version}</version>
</dependency>

<!-- Jackson 2 adapter -->
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

## 🤝 Contributing

Contributions are welcome! Please submit a pull request.

### Building

Fork and clone the repository, then build with Maven:

```bash
> cd tree-io
> mvn package
```

## 💼 Commercial Support

Commercial support and consulting are available.  
For inquiries, please contact: filip26@gmail.com
