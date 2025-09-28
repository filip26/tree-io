# 🌳 Tree I/O

**Uniform API to read/write tree data models — cross-format, cross-library.**

**tree-io** provides a consistent abstraction for working with **tree-like data structures**.  It is **format-agnostic** (JSON, YAML, CBOR, …) and **library-agnostic** (Jackson, Gson, Jakarta, …),  allowing you to **read, manipulate, and write trees uniformly** without depending on a specific parser or serializer.

## ✨ Features

- 🌐 Uniform, library-agnostic API for tree data processing
- 🗂️ Supports multiple formats: JSON, YAML, CBOR
- 🔌 Works with Jackson, Gson, Jakarta, and other libraries
- 🛠️ Extensible adapter model for adding new formats or libraries

## 🏗️ Use Cases

- Uniform processing of tree-structured data
- Building library-agnostic processors and pipelines
- Manipulating hierarchical data in a consistent way

## 🔌 Implementations of Tree I/O API

 Adapter               | Version | Documentation
-----------------------|---------|---------------
 Jakarta JSON API      | [![Maven Central](https://img.shields.io/maven-central/v/com.apicatalog/tree-io-jakarta.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:com.apicatalog%20AND%20a:tree-io-jakarta) | [![javadoc](https://javadoc.io/badge2/com.apicatalog/tree-io-jakarta/javadoc.svg)](https://javadoc.io/doc/com.apicatalog/tree-io-jakarta) 
 Jackson 2 Tree Model  | [![Maven Central](https://img.shields.io/maven-central/v/com.apicatalog/tree-io-jackson2.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:com.apicatalog%20AND%20a:tree-io-jackson2) | [![javadoc](https://javadoc.io/badge2/com.apicatalog/tree-io-jackson2/javadoc.svg)](https://javadoc.io/doc/com.apicatalog/tree-io-jackson2) 
 CBOR                  | [![Maven Central](https://img.shields.io/maven-central/v/com.apicatalog/tree-io-cbor.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:com.apicatalog%20AND%20a:tree-io-cbor) | [![javadoc](https://javadoc.io/badge2/com.apicatalog/tree-io-cbor/javadoc.svg)](https://javadoc.io/doc/com.apicatalog/tree-io-cbor) 

## Examples 

### High-Level Transformation
The most common use case is a full transformation from a source to a destination. The writer classes are designed to handle this with a single method call.

```javascript
// Have a source object (e.g., a Map) and a destination (e.g., a JsonGenerator)
Map<String, Object> source = Map.of("hello", "world");
JsonGenerator destination = ... ;

// Create an adapter for the source and a writer for the destination
NodeAdapter adapter = new NativeAdapter();
Jackson2Writer writer = new Jackson2Writer(destination);

// Run the transformation with a single call
writer.node(source, adapter);
```

### Direct Node Inspection using NodeAdapter

Use a NodeAdapter directly when you need to read, inspect, or extract specific values from a tree structure without traversing the entire thing.

```javascript
// Given any 'node' object and a suitable 'adapter'...

// To inspect and process it safely:

// 1. Check for structural types first.
if (adapter.isMap(node)) {
    // If it's a map, iterate its entries.
    for (Entry<?, ?> entry : adapter.entries(node)) {
        // ... process key and value recursively
    }

} else if (adapter.isCollection(node)) {
    // If it's a collection, iterate its elements.
    for (Object element : adapter.elements(node)) {
        // ... process element recursively
    }

// 2. Then, check for scalar types.
} else if (adapter.isString(node)) {
    String value = adapter.stringValue(node);

} else if (adapter.isNumber(node)) {
    // For numbers, check for the specific type before extracting.
    if (adapter.isIntegral(node)) {
        long intValue = adapter.longValue(node);
    } else {
        BigDecimal decimalValue = adapter.decimalValue(node);
    }

} else if (adapter.isNull(node)) {
    // It's a null value.
}
```

### Manual Traversal using NodeVisitor
For complex processing like searching or validation, you can manually iterate through the tree using the step() method of a NodeVisitor.

```javascript
// Given a source object and a suitable adapter...
Object source = ... ;
NodeAdapter adapter = ... ;

// 1. Create the visitor instance.
NodeVisitor visitor = NodeVisitor.of(source, adapter);

// 2. Loop step-by-step through every node in the tree.
while (visitor.step()) {

    // 3. Inspect the visitor's state after each step.
    Object node = visitor.currentNode();
    NodeType type = visitor.currentNodeType();
    Context context = visitor.currentNodeContext();

    // 4. Perform actions based on the node's role in the tree.
    switch (context) {

        case ROOT:
            // Process the top-level node.
            break;

        case PROPERTY_KEY:
            // The current node is a key in a map.
            // The next step() will move to its value.
            break;

        case PROPERTY_VALUE:
            // The current node is a value in a map.
            break;

        case COLLECTION_ELEMENT:
            // The current node is an element in a collection.
            break;

        case END:
            // A synthetic marker showing a map or collection has ended.
            break;
    }
}
```

## 📦 Artifacts

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
