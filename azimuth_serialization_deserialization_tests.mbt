// Azimuth High-Quality Serialization and Deserialization Tests
// This file contains comprehensive test cases for serialization and deserialization

// Test 1: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  let json_serializer = JsonSerializer::new()
  
  // Test basic object serialization
  let test_object = TestObject::new()
    .with_id(123)
    .with_name("Test Object")
    .with_active(true)
    .with_created_at(DateTime::new(2024, 1, 15, 10, 30, 0))
    .with_tags(["tag1", "tag2", "tag3"])
    .with_metadata({"key1": "value1", "key2": "value2"})
  
  let serialized_json = json_serializer.serialize(test_object)
  assert_true(serialized_json.length() > 0)
  
  // Verify JSON structure
  assert_true(serialized_json.contains("\"id\":123"))
  assert_true(serialized_json.contains("\"name\":\"Test Object\""))
  assert_true(serialized_json.contains("\"active\":true"))
  assert_true(serialized_json.contains("\"created_at\""))
  assert_true(serialized_json.contains("\"tags\":["))
  assert_true(serialized_json.contains("\"metadata\":{"))
  
  // Test basic object deserialization
  let deserialized_object = json_serializer.deserialize::<TestObject>(serialized_json)
  assert_true(deserialized_object.is_ok())
  
  let obj = deserialized_object.unwrap()
  assert_eq(obj.id, 123)
  assert_eq(obj.name, "Test Object")
  assert_true(obj.active)
  assert_eq(obj.created_at, DateTime::new(2024, 1, 15, 10, 30, 0))
  assert_eq(obj.tags, ["tag1", "tag2", "tag3"])
  assert_eq(obj.metadata.get("key1"), "value1")
  assert_eq(obj.metadata.get("key2"), "value2")
  
  // Test array serialization
  let test_array = [1, 2, 3, 4, 5]
  let serialized_array = json_serializer.serialize(test_array)
  assert_eq(serialized_array, "[1,2,3,4,5]")
  
  let deserialized_array = json_serializer.deserialize::<Array<Int>>(serialized_array)
  assert_true(deserialized_array.is_ok())
  assert_eq(deserialized_array.unwrap(), [1, 2, 3, 4, 5])
  
  // Test complex nested object serialization
  let nested_object = ComplexObject::new()
    .with_user(User::new("john_doe", "John Doe", 30))
    .with_address(Address::new("123 Main St", "Anytown", "CA", "12345"))
    .with_orders([
      Order::new(1, "Product A", 19.99, 2),
      Order::new(2, "Product B", 29.99, 1)
    ])
    .with_preferences(Preferences::new(true, "en-US", ["notifications", "newsletter"]))
  
  let serialized_nested = json_serializer.serialize(nested_object)
  assert_true(serialized_nested.length() > 0)
  
  let deserialized_nested = json_serializer.deserialize::<ComplexObject>(serialized_nested)
  assert_true(deserialized_nested.is_ok())
  
  let nested = deserialized_nested.unwrap()
  assert_eq(nested.user.username, "john_doe")
  assert_eq(nested.user.name, "John Doe")
  assert_eq(nested.user.age, 30)
  assert_eq(nested.address.street, "123 Main St")
  assert_eq(nested.address.city, "Anytown")
  assert_eq(nested.address.state, "CA")
  assert_eq(nested.address.zip_code, "12345")
  assert_eq(nested.orders.length(), 2)
  assert_eq(nested.orders[0].id, 1)
  assert_eq(nested.orders[0].product_name, "Product A")
  assert_eq(nested.orders[0].price, 19.99)
  assert_eq(nested.orders[0].quantity, 2)
  assert_true(nested.preferences.email_notifications)
  assert_eq(nested.preferences.language, "en-US")
  assert_eq(nested.preferences.features, ["notifications", "newsletter"])
  
  // Test serialization with custom options
  let options = JsonSerializerOptions::new()
    .with_pretty_print(true)
    .with_indent(2)
    .with_sort_keys(true)
  
  let pretty_serialized = json_serializer.serialize_with_options(test_object, options)
  assert_true(pretty_serialized.contains("\n"))
  assert_true(pretty_serialized.contains("  "))
  
  // Test serialization error handling
  let circular_object = CircularObject::new()
  circular_object.set_reference(circular_object.clone()) // Create circular reference
  
  let circular_result = json_serializer.serialize(circular_object)
  assert_true(circular_result.is_error())
  
  // Test deserialization error handling
  let invalid_json = "{\"id\":123,\"name\":\"Test Object\",\"active\":true\"invalid\"}"
  let invalid_result = json_serializer.deserialize::<TestObject>(invalid_json)
  assert_true(invalid_result.is_error())
  
  // Test partial deserialization
  let partial_json = "{\"id\":123,\"name\":\"Test Object\"}"
  let partial_result = json_serializer.deserialize::<TestObject>(partial_json)
  assert_true(partial_result.is_ok())
  
  let partial_obj = partial_result.unwrap()
  assert_eq(partial_obj.id, 123)
  assert_eq(partial_obj.name, "Test Object")
  assert_eq(partial_obj.active, false) // Default value
  assert_eq(partial_obj.tags, []) // Default value
}

// Test 2: Binary Serialization and Deserialization
test "binary serialization and deserialization" {
  let binary_serializer = BinarySerializer::new()
  
  // Test basic type serialization
  let int_value = 42
  let serialized_int = binary_serializer.serialize(int_value)
  assert_eq(serialized_int.length(), 4) // 4 bytes for int
  
  let deserialized_int = binary_serializer.deserialize::<Int>(serialized_int)
  assert_true(deserialized_int.is_ok())
  assert_eq(deserialized_int.unwrap(), 42)
  
  // Test float serialization
  let float_value = 3.14159
  let serialized_float = binary_serializer.serialize(float_value)
  assert_eq(serialized_float.length(), 8) // 8 bytes for double
  
  let deserialized_float = binary_serializer.deserialize::<Float>(serialized_float)
  assert_true(deserialized_float.is_ok())
  assert_true(abs(deserialized_float.unwrap() - 3.14159) < 0.00001)
  
  // Test string serialization
  let string_value = "Hello, World!"
  let serialized_string = binary_serializer.serialize(string_value)
  assert_true(serialized_string.length() > 4) // Length prefix + string data
  
  let deserialized_string = binary_serializer.deserialize::<String>(serialized_string)
  assert_true(deserialized_string.is_ok())
  assert_eq(deserialized_string.unwrap(), "Hello, World!")
  
  // Test array serialization
  let array_value = [1, 2, 3, 4, 5]
  let serialized_array = binary_serializer.serialize(array_value)
  assert_true(serialized_array.length() > 4) // Length prefix + array data
  
  let deserialized_array = binary_serializer.deserialize::<Array<Int>>(serialized_array)
  assert_true(deserialized_array.is_ok())
  assert_eq(deserialized_array.unwrap(), [1, 2, 3, 4, 5])
  
  // Test object serialization
  let test_object = BinaryTestObject::new()
    .with_id(123)
    .with_name("Test Object")
    .with_value(456.789)
    .with_active(true)
    .with_data([1, 2, 3, 4, 5])
  
  let serialized_object = binary_serializer.serialize(test_object)
  assert_true(serialized_object.length() > 0)
  
  let deserialized_object = binary_serializer.deserialize::<BinaryTestObject>(serialized_object)
  assert_true(deserialized_object.is_ok())
  
  let obj = deserialized_object.unwrap()
  assert_eq(obj.id, 123)
  assert_eq(obj.name, "Test Object")
  assert_true(abs(obj.value - 456.789) < 0.001)
  assert_true(obj.active)
  assert_eq(obj.data, [1, 2, 3, 4, 5])
  
  // Test binary format versioning
  let versioned_serializer = BinarySerializer::new()
    .with_version(1)
    .with_compression(true)
    .with_checksum(true)
  
  let versioned_object = VersionedObject::new()
    .with_version(1)
    .with_data("test data")
    .with_timestamp(DateTime::now())
  
  let versioned_serialized = versioned_serializer.serialize(versioned_object)
  assert_true(versioned_serialized.length() > 0)
  
  // Verify version header
  let version_header = versioned_serialized.slice(0, 4)
  let version = version_header.to_int()
  assert_eq(version, 1)
  
  // Test endianness handling
  let little_endian_serializer = BinarySerializer::new()
    .with_endianness(Endianness::Little)
  
  let big_endian_serializer = BinarySerializer::new()
    .with_endianness(Endianness::Big)
  
  let test_int = 0x12345678
  
  let little_endian_serialized = little_endian_serializer.serialize(test_int)
  let big_endian_serialized = big_endian_serializer.serialize(test_int)
  
  // The bytes should be in different order
  assert_ne(little_endian_serialized, big_endian_serialized)
  
  // Test deserialization with correct endianness
  let little_endian_deserialized = little_endian_serializer.deserialize::<Int>(little_endian_serialized)
  assert_true(little_endian_deserialized.is_ok())
  assert_eq(little_endian_deserialized.unwrap(), test_int)
  
  let big_endian_deserialized = big_endian_serializer.deserialize::<Int>(big_endian_serialized)
  assert_true(big_endian_deserialized.is_ok())
  assert_eq(big_endian_deserialized.unwrap(), test_int)
  
  // Test compression
  let compressed_serializer = BinarySerializer::new()
    .with_compression(true)
    .with_compression_algorithm(CompressionAlgorithm::Gzip)
  
  let large_data = "x".repeat(10000) // Large repetitive data
  let compressed_serialized = compressed_serializer.serialize(large_data)
  
  // Compressed data should be smaller than original
  assert_true(compressed_serialized.length() < large_data.length())
  
  let compressed_deserialized = compressed_serializer.deserialize::<String>(compressed_serialized)
  assert_true(compressed_deserialized.is_ok())
  assert_eq(compressed_deserialized.unwrap(), large_data)
  
  // Test checksum verification
  let checksum_serializer = BinarySerializer::new()
    .with_checksum(true)
    .with_checksum_algorithm(ChecksumAlgorithm::CRC32)
  
  let checksum_serialized = checksum_serializer.serialize(test_object)
  assert_true(checksum_serialized.length() > 0)
  
  // Verify checksum is present (4 bytes for CRC32)
  assert_true(checksum_serialized.length() >= 4)
  
  // Test corruption detection
  let mut corrupted_data = checksum_serialized.to_array()
  corrupted_data[corrupted_data.length() - 5] = corrupted_data[corrupted_data.length() - 5] + 1 // Corrupt one byte
  
  let corrupted_result = checksum_serializer.deserialize::<BinaryTestObject>(corrupted_data)
  assert_true(corrupted_result.is_error())
}

// Test 3: Protocol Buffers Serialization
test "protocol buffers serialization" {
  let protobuf_serializer = ProtobufSerializer::new()
  
  // Test basic message serialization
  let proto_message = TestMessage::new()
    .with_id(123)
    .with_name("Test Message")
    .with_value(456.789)
    .with_active(true)
    .with_timestamp(Timestamp::new(2024, 1, 15, 10, 30, 0))
  
  let serialized_proto = protobuf_serializer.serialize(proto_message)
  assert_true(serialized_proto.length() > 0)
  
  // Test basic message deserialization
  let deserialized_proto = protobuf_serializer.deserialize::<TestMessage>(serialized_proto)
  assert_true(deserialized_proto.is_ok())
  
  let message = deserialized_proto.unwrap()
  assert_eq(message.id, 123)
  assert_eq(message.name, "Test Message")
  assert_true(abs(message.value - 456.789) < 0.001)
  assert_true(message.active)
  assert_eq(message.timestamp, Timestamp::new(2024, 1, 15, 10, 30, 0))
  
  // Test repeated field serialization
  let repeated_message = RepeatedMessage::new()
    .with_ids([1, 2, 3, 4, 5])
    .with_names(["Name1", "Name2", "Name3"])
    .with_values([1.1, 2.2, 3.3])
  
  let serialized_repeated = protobuf_serializer.serialize(repeated_message)
  assert_true(serialized_repeated.length() > 0)
  
  let deserialized_repeated = protobuf_serializer.deserialize::<RepeatedMessage>(serialized_repeated)
  assert_true(deserialized_repeated.is_ok())
  
  let repeated = deserialized_repeated.unwrap()
  assert_eq(repeated.ids, [1, 2, 3, 4, 5])
  assert_eq(repeated.names, ["Name1", "Name2", "Name3"])
  assert_eq(repeated.values.length(), 3)
  assert_true(abs(repeated.values[0] - 1.1) < 0.001)
  assert_true(abs(repeated.values[1] - 2.2) < 0.001)
  assert_true(abs(repeated.values[2] - 3.3) < 0.001)
  
  // Test nested message serialization
  let nested_message = NestedMessage::new()
    .with_user(UserMessage::new()
      .with_id(123)
      .with_name("John Doe")
      .with_email("john@example.com"))
    .with_address(AddressMessage::new()
      .with_street("123 Main St")
      .with_city("Anytown")
      .with_state("CA")
      .with_zip("12345"))
  
  let serialized_nested = protobuf_serializer.serialize(nested_message)
  assert_true(serialized_nested.length() > 0)
  
  let deserialized_nested = protobuf_serializer.deserialize::<NestedMessage>(serialized_nested)
  assert_true(deserialized_nested.is_ok())
  
  let nested = deserialized_nested.unwrap()
  assert_eq(nested.user.id, 123)
  assert_eq(nested.user.name, "John Doe")
  assert_eq(nested.user.email, "john@example.com")
  assert_eq(nested.address.street, "123 Main St")
  assert_eq(nested.address.city, "Anytown")
  assert_eq(nested.address.state, "CA")
  assert_eq(nested.address.zip, "12345")
  
  // Test map field serialization
  let map_message = MapMessage::new()
    .with_string_map({"key1": "value1", "key2": "value2", "key3": "value3"})
    .with_int_map({1: "one", 2: "two", 3: "three"})
  
  let serialized_map = protobuf_serializer.serialize(map_message)
  assert_true(serialized_map.length() > 0)
  
  let deserialized_map = protobuf_serializer.deserialize::<MapMessage>(serialized_map)
  assert_true(deserialized_map.is_ok())
  
  let map = deserialized_map.unwrap()
  assert_eq(map.string_map.get("key1"), "value1")
  assert_eq(map.string_map.get("key2"), "value2")
  assert_eq(map.string_map.get("key3"), "value3")
  assert_eq(map.int_map.get(1), "one")
  assert_eq(map.int_map.get(2), "two")
  assert_eq(map.int_map.get(3), "three")
  
  // Test oneof field serialization
  let oneof_message = OneofMessage::new()
    .with_string_value("string value")
  
  let serialized_oneof = protobuf_serializer.serialize(oneof_message)
  assert_true(serialized_oneof.length() > 0)
  
  let deserialized_oneof = protobuf_serializer.deserialize::<OneofMessage>(serialized_oneof)
  assert_true(deserialized_oneof.is_ok())
  
  let oneof = deserialized_oneof.unwrap()
  assert_eq(oneof.string_value, "string value")
  assert_eq(oneof.int_value, 0) // Default value
  assert_eq(oneof.bool_value, false) // Default value
  
  // Test enum field serialization
  let enum_message = EnumMessage::new()
    .with_status(Status::Active)
    .with_priority(Priority::High)
    .with_category(Category::Electronics)
  
  let serialized_enum = protobuf_serializer.serialize(enum_message)
  assert_true(serialized_enum.length() > 0)
  
  let deserialized_enum = protobuf_serializer.deserialize::<EnumMessage>(serialized_enum)
  assert_true(deserialized_enum.is_ok())
  
  let enum_msg = deserialized_enum.unwrap()
  assert_eq(enum_msg.status, Status::Active)
  assert_eq(enum_msg.priority, Priority::High)
  assert_eq(enum_msg.category, Category::Electronics)
  
  // Test message compatibility across versions
  let v1_message = VersionedMessage::new()
    .with_id(123)
    .with_name("V1 Message")
    .with_value(456.789)
  
  let v1_serialized = protobuf_serializer.serialize(v1_message)
  
  // Simulate deserializing with a newer schema that has additional fields
  let v2_deserialized = protobuf_serializer.deserialize::<VersionedMessageV2>(v1_serialized)
  assert_true(v2_deserialized.is_ok())
  
  let v2_message = v2_deserialized.unwrap()
  assert_eq(v2_message.id, 123)
  assert_eq(v2_message.name, "V1 Message")
  assert_true(abs(v2_message.value - 456.789) < 0.001)
  assert_eq(v2_message.description, "") // Default value for new field
  
  // Test backward compatibility
  let v2_message_full = VersionedMessageV2::new()
    .with_id(123)
    .with_name("V2 Message")
    .with_value(456.789)
    .with_description("V2 Description")
  
  let v2_serialized = protobuf_serializer.serialize(v2_message_full)
  
  let v1_deserialized = protobuf_serializer.deserialize::<VersionedMessage>(v2_serialized)
  assert_true(v1_deserialized.is_ok())
  
  let v1_message = v1_deserialized.unwrap()
  assert_eq(v1_message.id, 123)
  assert_eq(v1_message.name, "V2 Message")
  assert_true(abs(v1_message.value - 456.789) < 0.001)
}

// Test 4: XML Serialization and Deserialization
test "xml serialization and deserialization" {
  let xml_serializer = XmlSerializer::new()
  
  // Test basic object serialization
  let test_object = XmlTestObject::new()
    .with_id(123)
    .with_name("Test Object")
    .with_active(true)
    .with_created_at(DateTime::new(2024, 1, 15, 10, 30, 0))
    .with_tags(["tag1", "tag2", "tag3"])
    .with_metadata({"key1": "value1", "key2": "value2"})
  
  let serialized_xml = xml_serializer.serialize(test_object)
  assert_true(serialized_xml.length() > 0)
  
  // Verify XML structure
  assert_true(serialized_xml.contains("<XmlTestObject>"))
  assert_true(serialized_xml.contains("<id>123</id>"))
  assert_true(serialized_xml.contains("<name>Test Object</name>"))
  assert_true(serialized_xml.contains("<active>true</active>"))
  assert_true(serialized_xml.contains("<created_at>"))
  assert_true(serialized_xml.contains("<tags>"))
  assert_true(serialized_xml.contains("<tag>tag1</tag>"))
  assert_true(serialized_xml.contains("<metadata>"))
  assert_true(serialized_xml.contains("<key1>value1</key1>"))
  assert_true(serialized_xml.contains("</XmlTestObject>"))
  
  // Test basic object deserialization
  let deserialized_object = xml_serializer.deserialize::<XmlTestObject>(serialized_xml)
  assert_true(deserialized_object.is_ok())
  
  let obj = deserialized_object.unwrap()
  assert_eq(obj.id, 123)
  assert_eq(obj.name, "Test Object")
  assert_true(obj.active)
  assert_eq(obj.created_at, DateTime::new(2024, 1, 15, 10, 30, 0))
  assert_eq(obj.tags, ["tag1", "tag2", "tag3"])
  assert_eq(obj.metadata.get("key1"), "value1")
  assert_eq(obj.metadata.get("key2"), "value2")
  
  // Test XML with attributes
  let attributed_object = AttributedObject::new()
    .with_id(123)
    .with_name("Attributed Object")
    .with_attributes({"type": "test", "version": "1.0"})
  
  let attributed_serializer = XmlSerializer::new()
    .with_serialize_attributes(true)
  
  let serialized_attributed = attributed_serializer.serialize(attributed_object)
  assert_true(serialized_attributed.contains("type=\"test\""))
  assert_true(serialized_attributed.contains("version=\"1.0\""))
  
  let deserialized_attributed = attributed_serializer.deserialize::<AttributedObject>(serialized_attributed)
  assert_true(deserialized_attributed.is_ok())
  
  let attributed = deserialized_attributed.unwrap()
  assert_eq(attributed.id, 123)
  assert_eq(attributed.name, "Attributed Object")
  assert_eq(attributed.attributes.get("type"), "test")
  assert_eq(attributed.attributes.get("version"), "1.0")
  
  // Test XML namespace support
  let namespaced_object = NamespacedObject::new()
    .with_id(123)
    .with_name("Namespaced Object")
    .with_namespace("http://example.com/schema")
  
  let namespaced_serializer = XmlSerializer::new()
    .with_namespace_support(true)
  
  let serialized_namespaced = namespaced_serializer.serialize(namespaced_object)
  assert_true(serialized_namespaced.contains("xmlns=\"http://example.com/schema\""))
  
  let deserialized_namespaced = namespaced_serializer.deserialize::<NamespacedObject>(serialized_namespaced)
  assert_true(deserialized_namespaced.is_ok())
  
  let namespaced = deserialized_namespaced.unwrap()
  assert_eq(namespaced.id, 123)
  assert_eq(namespaced.name, "Namespaced Object")
  assert_eq(namespaced.namespace, "http://example.com/schema")
  
  // Test XML with CDATA sections
  let cdata_object = CDataObject::new()
    .with_id(123)
    .with_content("This is <special> & content that needs CDATA")
  
  let cdata_serializer = XmlSerializer::new()
    .with_use_cdata_for_special_chars(true)
  
  let serialized_cdata = cdata_serializer.serialize(cdata_object)
  assert_true(serialized_cdata.contains("<![CDATA["))
  assert_true(serialized_cdata.contains("]]>"))
  
  let deserialized_cdata = cdata_serializer.deserialize::<CDataObject>(serialized_cdata)
  assert_true(deserialized_cdata.is_ok())
  
  let cdata = deserialized_cdata.unwrap()
  assert_eq(cdata.id, 123)
  assert_eq(cdata.content, "This is <special> & content that needs CDATA")
  
  // Test XML with pretty printing
  let pretty_serializer = XmlSerializer::new()
    .with_pretty_print(true)
    .with_indent(2)
  
  let pretty_serialized = pretty_serializer.serialize(test_object)
  assert_true(pretty_serialized.contains("\n"))
  assert_true(pretty_serialized.contains("  "))
  
  // Test XML schema validation
  let schema_validator = XmlSchemaValidator::new()
  schema_validator.load_schema("test_schema.xsd")
  
  let valid_xml = "<ValidObject><id>123</id><name>Test</name></ValidObject>"
  let validation_result = schema_validator.validate(valid_xml)
  assert_true(validation_result.is_valid)
  
  let invalid_xml = "<InvalidObject><id>abc</id></InvalidObject>" // id should be integer
  let invalid_validation_result = schema_validator.validate(invalid_xml)
  assert_false(invalid_validation_result.is_valid)
  assert_true(invalid_validation_result.errors.length() > 0)
  
  // Test XPath queries
  let xpath_processor = XPathProcessor::new()
  
  let xml_doc = "<root><person><name>John</name><age>30</age></person><person><name>Jane</name><age>25</age></person></root>"
  
  let names = xpath_processor.query(xml_doc, "//person/name/text()")
  assert_eq(names, ["John", "Jane"])
  
  let ages = xpath_processor.query(xml_doc, "//person/age/text()")
  assert_eq(ages, ["30", "25"])
  
  let person_over_25 = xpath_processor.query(xml_doc, "//person[age > 25]/name/text()")
  assert_eq(person_over_25, ["John"])
  
  // Test XSLT transformation
  let xslt_transformer = XsltTransformer::new()
  xslt_transformer.load_stylesheet("transform.xsl")
  
  let source_xml = "<people><person><name>John</name><age>30</age></person></people>"
  let transformed_result = xslt_transformer.transform(source_xml)
  assert_true(transformed_result.is_success())
  
  let transformed_xml = transformed_result.unwrap()
  assert_true(transformed_xml.length() > 0)
}

// Test 5: Custom Serialization Formats
test "custom serialization formats" {
  // Test CSV serialization
  let csv_serializer = CsvSerializer::new()
    .with_delimiter(",")
    .with_header(true)
    .with_quote("\"")
  
  let csv_data = [
    CsvRow::new(["ID", "Name", "Age", "Active"]),
    CsvRow::new(["1", "John Doe", "30", "true"]),
    CsvRow::new(["2", "Jane Smith", "25", "false"]),
    CsvRow::new(["3", "Bob Johnson", "35", "true"])
  ]
  
  let serialized_csv = csv_serializer.serialize(csv_data)
  assert_true(serialized_csv.contains("ID,Name,Age,Active"))
  assert_true(serialized_csv.contains("1,John Doe,30,true"))
  assert_true(serialized_csv.contains("2,Jane Smith,25,false"))
  assert_true(serialized_csv.contains("3,Bob Johnson,35,true"))
  
  let deserialized_csv = csv_serializer.deserialize::<Array<CsvRow>>(serialized_csv)
  assert_true(deserialized_csv.is_ok())
  
  let csv_rows = deserialized_csv.unwrap()
  assert_eq(csv_rows.length(), 4)
  assert_eq(csv_rows[0].fields, ["ID", "Name", "Age", "Active"])
  assert_eq(csv_rows[1].fields, ["1", "John Doe", "30", "true"])
  assert_eq(csv_rows[2].fields, ["2", "Jane Smith", "25", "false"])
  assert_eq(csv_rows[3].fields, ["3", "Bob Johnson", "35", "true"])
  
  // Test CSV with special characters
  let special_csv_data = [
    CsvRow::new(["ID", "Name", "Description"]),
    CsvRow::new(["1", "John Doe", "Has a comma, in name"]),
    CsvRow::new(["2", "Jane \"Smith\"", "Has quotes"]),
    CsvRow::new(["3", "Bob Johnson", "Has\nnew\nlines"])
  ]
  
  let serialized_special_csv = csv_serializer.serialize(special_csv_data)
  assert_true(serialized_special_csv.contains("\"Has a comma, in name\""))
  assert_true(serialized_special_csv.contains("\"Jane \"\"Smith\"\"\""))
  assert_true(serialized_special_csv.contains("\"Has\nnew\nlines\""))
  
  // Test YAML serialization
  let yaml_serializer = YamlSerializer::new()
    .with_indent(2)
    .with_inline_arrays(false)
  
  let yaml_object = YamlTestObject::new()
    .with_id(123)
    .with_name("Test Object")
    .with_active(true)
    .with_tags(["tag1", "tag2", "tag3"])
    .with_metadata({"key1": "value1", "key2": "value2"})
  
  let serialized_yaml = yaml_serializer.serialize(yaml_object)
  assert_true(serialized_yaml.contains("id: 123"))
  assert_true(serialized_yaml.contains("name: Test Object"))
  assert_true(serialized_yaml.contains("active: true"))
  assert_true(serialized_yaml.contains("tags:"))
  assert_true(serialized_yaml.contains("- tag1"))
  assert_true(serialized_yaml.contains("metadata:"))
  assert_true(serialized_yaml.contains("key1: value1"))
  
  let deserialized_yaml = yaml_serializer.deserialize::<YamlTestObject>(serialized_yaml)
  assert_true(deserialized_yaml.is_ok())
  
  let yaml_obj = deserialized_yaml.unwrap()
  assert_eq(yaml_obj.id, 123)
  assert_eq(yaml_obj.name, "Test Object")
  assert_true(yaml_obj.active)
  assert_eq(yaml_obj.tags, ["tag1", "tag2", "tag3"])
  assert_eq(yaml_obj.metadata.get("key1"), "value1")
  assert_eq(yaml_obj.metadata.get("key2"), "value2")
  
  // Test TOML serialization
  let toml_serializer = TomlSerializer::new()
  
  let toml_object = TomlTestObject::new()
    .with_title("Test Configuration")
    .with_database(DatabaseConfig::new("localhost", 5432, "azimuth", "user", "password"))
    .with_server(ServerConfig::new("0.0.0.0", 8080, true))
    .with_features(["telemetry", "analytics", "monitoring"])
  
  let serialized_toml = toml_serializer.serialize(toml_object)
  assert_true(serialized_toml.contains("title = \"Test Configuration\""))
  assert_true(serialized_toml.contains("[database]"))
  assert_true(serialized_toml.contains("host = \"localhost\""))
  assert_true(serialized_toml.contains("port = 5432"))
  assert_true(serialized_toml.contains("[server]"))
  assert_true(serialized_toml.contains("bind_address = \"0.0.0.0\""))
  assert_true(serialized_toml.contains("port = 8080"))
  assert_true(serialized_toml.contains("ssl = true"))
  assert_true(serialized_toml.contains("features = ["))
  
  let deserialized_toml = toml_serializer.deserialize::<TomlTestObject>(serialized_toml)
  assert_true(deserialized_toml.is_ok())
  
  let toml_obj = deserialized_toml.unwrap()
  assert_eq(toml_obj.title, "Test Configuration")
  assert_eq(toml_obj.database.host, "localhost")
  assert_eq(toml_obj.database.port, 5432)
  assert_eq(toml_obj.database.name, "azimuth")
  assert_eq(toml_obj.database.user, "user")
  assert_eq(toml_obj.database.password, "password")
  assert_eq(toml_obj.server.bind_address, "0.0.0.0")
  assert_eq(toml_obj.server.port, 8080)
  assert_true(toml_obj.server.ssl)
  assert_eq(toml_obj.features, ["telemetry", "analytics", "monitoring"])
  
  // Test INI file serialization
  let ini_serializer = IniSerializer::new()
  
  let ini_object = IniTestObject::new()
    .with_section("general", {"debug": "true", "log_level": "info"})
    .with_section("database", {"host": "localhost", "port": "5432"})
    .with_section("server", {"host": "0.0.0.0", "port": "8080"})
  
  let serialized_ini = ini_serializer.serialize(ini_object)
  assert_true(serialized_ini.contains("[general]"))
  assert_true(serialized_ini.contains("debug = true"))
  assert_true(serialized_ini.contains("log_level = info"))
  assert_true(serialized_ini.contains("[database]"))
  assert_true(serialized_ini.contains("host = localhost"))
  assert_true(serialized_ini.contains("port = 5432"))
  assert_true(serialized_ini.contains("[server]"))
  assert_true(serialized_ini.contains("host = 0.0.0.0"))
  assert_true(serialized_ini.contains("port = 8080"))
  
  let deserialized_ini = ini_serializer.deserialize::<IniTestObject>(serialized_ini)
  assert_true(deserialized_ini.is_ok())
  
  let ini_obj = deserialized_ini.unwrap()
  assert_eq(ini_obj.sections.get("general").get("debug"), "true")
  assert_eq(ini_obj.sections.get("general").get("log_level"), "info")
  assert_eq(ini_obj.sections.get("database").get("host"), "localhost")
  assert_eq(ini_obj.sections.get("database").get("port"), "5432")
  assert_eq(ini_obj.sections.get("server").get("host"), "0.0.0.0")
  assert_eq(ini_obj.sections.get("server").get("port"), "8080")
}

// Test 6: Serialization Performance and Optimization
test "serialization performance and optimization" {
  // Test JSON serialization performance
  let json_serializer = JsonSerializer::new()
  let large_object = create_large_test_object(10000) // Object with 10000 properties
  
  let json_serialize_start = get_current_timestamp()
  let json_serialized = json_serializer.serialize(large_object.clone())
  let json_serialize_end = get_current_timestamp()
  let json_serialize_time = json_serialize_end - json_serialize_start
  
  assert_true(json_serialize_time.to_millis() < 1000) // Should serialize in less than 1 second
  
  let json_deserialize_start = get_current_timestamp()
  let json_deserialized = json_serializer.deserialize::<LargeTestObject>(json_serialized)
  let json_deserialize_end = get_current_timestamp()
  let json_deserialize_time = json_deserialize_end - json_deserialize_start
  
  assert_true(json_deserialize_time.to_millis() < 1000) // Should deserialize in less than 1 second
  
  // Test binary serialization performance
  let binary_serializer = BinarySerializer::new()
  
  let binary_serialize_start = get_current_timestamp()
  let binary_serialized = binary_serializer.serialize(large_object.clone())
  let binary_serialize_end = get_current_timestamp()
  let binary_serialize_time = binary_serialize_end - binary_serialize_start
  
  assert_true(binary_serialize_time.to_millis() < 500) // Should be faster than JSON
  
  let binary_deserialize_start = get_current_timestamp()
  let binary_deserialized = binary_serializer.deserialize::<LargeTestObject>(binary_serialized)
  let binary_deserialize_end = get_current_timestamp()
  let binary_deserialize_time = binary_deserialize_end - binary_deserialize_start
  
  assert_true(binary_deserialize_time.to_millis() < 500) // Should be faster than JSON
  
  // Compare serialization sizes
  assert_true(binary_serialized.length() < json_serialized.length()) // Binary should be smaller
  
  // Test serialization with pooling
  let pooled_json_serializer = JsonSerializer::new()
    .with_object_pooling(true)
    .with_string_interning(true)
  
  let pool_serialize_start = get_current_timestamp()
  for i in 0..=100 {
    let _ = pooled_json_serializer.serialize(large_object.clone())
  }
  let pool_serialize_end = get_current_timestamp()
  let pool_serialize_time = pool_serialize_end - pool_serialize_start
  
  // Pooled serialization should be faster for multiple objects
  let regular_serialize_start = get_current_timestamp()
  for i in 0..=100 {
    let _ = json_serializer.serialize(large_object.clone())
  }
  let regular_serialize_end = get_current_timestamp()
  let regular_serialize_time = regular_serialize_end - regular_serialize_start
  
  assert_true(pool_serialize_time.to_millis() < regular_serialize_time.to_millis())
  
  // Test serialization with streaming
  let streaming_serializer = StreamingSerializer::new()
  
  let stream_start = get_current_timestamp()
  let stream_result = streaming_serializer.serialize_stream(large_object.clone(), |chunk| {
    // Process chunk
    assert_true(chunk.length() > 0)
  })
  let stream_end = get_current_timestamp()
  let stream_time = stream_end - stream_start
  
  assert_true(stream_result.is_success())
  assert_true(stream_time.to_millis() < 1000) // Should stream in less than 1 second
  
  // Test deserialization with streaming
  let streaming_deserializer = StreamingDeserializer::new()
  
  let stream_deserialize_start = get_current_timestamp()
  let stream_deserialize_result = streaming_deserializer.deserialize_stream(json_serialized, |chunk| {
    // Process chunk
    assert_true(chunk.length() > 0)
  })
  let stream_deserialize_end = get_current_timestamp()
  let stream_deserialize_time = stream_deserialize_end - stream_deserialize_start
  
  assert_true(stream_deserialize_result.is_success())
  assert_true(stream_deserialize_time.to_millis() < 1000) // Should stream in less than 1 second
  
  // Test serialization caching
  let cached_serializer = JsonSerializer::new()
    .with_serialization_cache(true)
    .with_cache_size(1000)
  
  let cache_serialize_start = get_current_timestamp()
  for i in 0..=100 {
    let _ = cached_serializer.serialize(large_object.clone()) // Same object multiple times
  }
  let cache_serialize_end = get_current_timestamp()
  let cache_serialize_time = cache_serialize_end - cache_serialize_start
  
  // Cached serialization should be much faster for repeated objects
  assert_true(cache_serialize_time.to_millis() < pool_serialize_time.to_millis() / 2)
  
  // Test memory usage during serialization
  let memory_before = get_memory_usage()
  
  let memory_test_objects = []
  for i in 0..=100 {
    memory_test_objects.push(create_large_test_object(1000))
  }
  
  let memory_serialize_start = get_current_timestamp()
  for obj in memory_test_objects {
    let _ = json_serializer.serialize(obj)
  }
  let memory_serialize_end = get_current_timestamp()
  
  let memory_after = get_memory_usage()
  let memory_used = memory_after - memory_before
  
  // Memory usage should be reasonable
  assert_true(memory_used < 500 * 1024 * 1024) // Should use less than 500MB
  
  // Test serialization metrics
  let json_metrics = json_serializer.get_metrics()
  assert_true(json_metrics.total_serializations > 0)
  assert_true(json_metrics.total_deserializations > 0)
  assert_true(json_metrics.average_serialization_time_ms > 0)
  assert_true(json_metrics.average_deserialization_time_ms > 0)
  assert_true(json_metrics.peak_memory_usage > 0)
  
  let binary_metrics = binary_serializer.get_metrics()
  assert_true(binary_metrics.total_serializations > 0)
  assert_true(binary_metrics.total_deserializations > 0)
  assert_true(binary_metrics.average_serialization_time_ms > 0)
  assert_true(binary_metrics.average_deserialization_time_ms > 0)
  assert_true(binary_metrics.peak_memory_usage > 0)
}

// Test 7: Serialization Security and Validation
test "serialization security and validation" {
  let secure_serializer = SecureSerializer::new()
  
  // Test serialization with encryption
  let encryption_key = generate_encryption_key()
  secure_serializer.set_encryption_key(encryption_key)
  
  let sensitive_object = SensitiveObject::new()
    .with_id(123)
    .with_name("Sensitive Object")
    .with_secret("secret_data")
    .with_token("access_token_123")
    .with_api_key("api_key_456")
  
  let encrypted_serialized = secure_serializer.serialize_with_encryption(sensitive_object)
  assert_true(encrypted_serialized.length() > 0)
  
  // Verify data is encrypted (not plain text)
  assert_false(encrypted_serialized.contains("secret_data"))
  assert_false(encrypted_serialized.contains("access_token_123"))
  assert_false(encrypted_serialized.contains("api_key_456"))
  
  // Test decryption
  let decrypted_result = secure_serializer.deserialize_with_decryption::<SensitiveObject>(encrypted_serialized)
  assert_true(decrypted_result.is_ok())
  
  let decrypted = decrypted_result.unwrap()
  assert_eq(decrypted.id, 123)
  assert_eq(decrypted.name, "Sensitive Object")
  assert_eq(decrypted.secret, "secret_data")
  assert_eq(decrypted.token, "access_token_123")
  assert_eq(decrypted.api_key, "api_key_456")
  
  // Test serialization with digital signatures
  let signing_key = generate_signing_key()
  secure_serializer.set_signing_key(signing_key)
  
  let signed_serialized = secure_serializer.serialize_with_signature(sensitive_object)
  assert_true(signed_serialized.length() > 0)
  
  // Verify signature
  let signature_valid = secure_serializer.verify_signature(signed_serialized)
  assert_true(signature_valid)
  
  // Test deserialization with signature verification
  let verified_result = secure_serializer.deserialize_with_signature_verification::<SensitiveObject>(signed_serialized)
  assert_true(verified_result.is_ok())
  
  // Test tampering detection
  let mut tampered_data = signed_serialized.to_array()
  tampered_data[tampered_data.length() - 1] = tampered_data[tampered_data.length() - 1] + 1 // Tamper with data
  
  let tampered_result = secure_serializer.deserialize_with_signature_verification::<SensitiveObject>(tampered_data)
  assert_true(tampered_result.is_error())
  
  // Test input validation
  let validator = SerializationValidator::new()
  
  // Add validation rules
  validator.add_rule("id", ValidationRule::Required)
  validator.add_rule("id", ValidationRule::IntegerRange(1, 1000))
  validator.add_rule("name", ValidationRule::Required)
  validator.add_rule("name", ValidationRule::StringLength(1, 100))
  validator.add_rule("secret", ValidationRule::Required)
  validator.add_rule("secret", ValidationRule::PatternMatches("^[a-zA-Z0-9_]+$"))
  
  secure_serializer.set_validator(validator)
  
  // Test valid object
  let valid_result = secure_serializer.serialize_with_validation(sensitive_object)
  assert_true(valid_result.is_ok())
  
  // Test invalid object
  let invalid_object = SensitiveObject::new()
    .with_id(0) // Below range
    .with_name("") // Empty string
    .with_secret("invalid-secret!") // Contains invalid character
  
  let invalid_result = secure_serializer.serialize_with_validation(invalid_object)
  assert_true(invalid_result.is_error())
  
  let validation_errors = invalid_result.unwrap_error()
  assert_true(validation_errors.length() >= 3)
  
  // Test deserialization validation
  let malicious_json = "{\"id\":123,\"name\":\"Malicious Object\",\"secret\":\"secret_data\",\"token\":\"<script>alert('xss')</script>\"}"
  
  let sanitized_result = secure_serializer.deserialize_with_sanitization::<SensitiveObject>(malicious_json)
  assert_true(sanitized_result.is_ok())
  
  let sanitized = sanitized_result.unwrap()
  assert_eq(sanitized.id, 123)
  assert_eq(sanitized.name, "Malicious Object")
  assert_eq(sanitized.secret, "secret_data")
  assert_ne(sanitized.token, "<script>alert('xss')</script>") // Should be sanitized
  
  // Test serialization with access control
  let access_controller = AccessController::new()
  
  // Define access policies
  access_controller.add_policy("admin", AccessPolicy::new().with_allow_all())
  access_controller.add_policy("user", AccessPolicy::new()
    .with_allow_fields(["id", "name"])
    .with_deny_fields(["secret", "token", "api_key"]))
  access_controller.add_policy("guest", AccessPolicy::new()
    .with_allow_fields(["id"])
    .with_deny_fields(["name", "secret", "token", "api_key"]))
  
  secure_serializer.set_access_controller(access_controller)
  
  // Test admin access
  let admin_context = AccessContext::new("admin")
  let admin_result = secure_serializer.serialize_with_access_control(sensitive_object, admin_context)
  assert_true(admin_result.is_ok())
  
  let admin_serialized = admin_result.unwrap()
  assert_true(admin_serialized.contains("secret_data")) // Admin can see all fields
  
  // Test user access
  let user_context = AccessContext::new("user")
  let user_result = secure_serializer.serialize_with_access_control(sensitive_object, user_context)
  assert_true(user_result.is_ok())
  
  let user_serialized = user_result.unwrap()
  assert_true(user_serialized.contains("\"id\":123"))
  assert_true(user_serialized.contains("\"name\":\"Sensitive Object\""))
  assert_false(user_serialized.contains("secret_data")) // User cannot see secret fields
  
  // Test guest access
  let guest_context = AccessContext::new("guest")
  let guest_result = secure_serializer.serialize_with_access_control(sensitive_object, guest_context)
  assert_true(guest_result.is_ok())
  
  let guest_serialized = guest_result.unwrap()
  assert_true(guest_serialized.contains("\"id\":123"))
  assert_false(guest_serialized.contains("\"name\":\"Sensitive Object\"")) // Guest cannot see name
  assert_false(guest_serialized.contains("secret_data")) // Guest cannot see secret fields
  
  // Test serialization audit logging
  let audit_logger = AuditLogger::new()
  secure_serializer.set_audit_logger(audit_logger)
  
  // Perform some operations
  let _ = secure_serializer.serialize_with_validation(sensitive_object);
  let _ = secure_serializer.deserialize_with_decryption::<SensitiveObject>(encrypted_serialized);
  
  // Check audit logs
  let audit_logs = secure_serializer.get_audit_logs()
  assert_true(audit_logs.length() > 0)
  
  for log in audit_logs {
    assert_true(log.timestamp > 0)
    assert_true(log.operation.length() > 0)
    assert_true(log.user_id.length() > 0)
  }
}

// Test 8: Cross-Platform Serialization Compatibility
test "cross platform serialization compatibility" {
  // Test JSON compatibility across platforms
  let json_serializer = JsonSerializer::new()
  let json_compatibility = JsonCompatibilityTester::new()
  
  // Generate test data
  let test_data = CrossPlatformTestData::new()
    .with_primitives(42, 3.14159, true, "test string")
    .with_arrays([1, 2, 3], ["a", "b", "c"])
    .with_datetime(DateTime::new(2024, 1, 15, 10, 30, 0))
    .with_uuid("550e8400-e29b-41d4-a716-446655440000")
    .with_nested_object(NestedObject::new("nested", 123))
  
  // Serialize to JSON
  let json_data = json_serializer.serialize(test_data)
  
  // Test compatibility with different JSON parsers
  let parsers = ["rapidjson", "nlohmann_json", "jsoncpp", "simdjson"]
  
  for parser in parsers {
    let compatibility_result = json_compatibility.test_with_parser(json_data.clone(), parser)
    assert_true(compatibility_result.is_compatible)
    assert_true(compatibility_result.parsing_time_ms < 100)
  }
  
  // Test Protocol Buffers compatibility across versions
  let protobuf_serializer = ProtobufSerializer::new()
  let protobuf_compatibility = ProtobufCompatibilityTester::new()
  
  let proto_message = CrossPlatformProtoMessage::new()
    .with_id(123)
    .with_name("Cross Platform Test")
    .with_timestamp(Timestamp::now())
    .with_data([1, 2, 3, 4, 5])
  
  let proto_data = protobuf_serializer.serialize(proto_message)
  
  // Test compatibility with different protobuf versions
  let versions = ["2.6.1", "3.15.8", "3.21.0"]
  
  for version in versions {
    let compatibility_result = protobuf_compatibility.test_with_version(proto_data.clone(), version)
    assert_true(compatibility_result.is_compatible)
    assert_true(compatibility_result.serialization_time_ms < 50)
    assert_true(compatibility_result.deserialization_time_ms < 50)
  }
  
  // Test binary format compatibility across architectures
  let binary_serializer = BinarySerializer::new()
  let binary_compatibility = BinaryCompatibilityTester::new()
  
  let binary_data = binary_serializer.serialize(test_data)
  
  // Test compatibility with different architectures
  let architectures = ["x86_64", "arm64", "ppc64le", "s390x"]
  
  for arch in architectures {
    let compatibility_result = binary_compatibility.test_with_architecture(binary_data.clone(), arch)
    assert_true(compatibility_result.is_compatible)
    assert_true(compatibility_result.endianness_handled)
  }
  
  // Test XML compatibility across parsers
  let xml_serializer = XmlSerializer::new()
  let xml_compatibility = XmlCompatibilityTester::new()
  
  let xml_object = XmlCrossPlatformObject::new()
    .with_id(123)
    .with_name("XML Cross Platform Test")
    .with_attributes({"type": "test", "version": "1.0"})
    .with_elements(["elem1", "elem2", "elem3"])
  
  let xml_data = xml_serializer.serialize(xml_object)
  
  // Test compatibility with different XML parsers
  let xml_parsers = ["libxml2", "xerces", "expat", "pugixml"]
  
  for parser in xml_parsers {
    let compatibility_result = xml_compatibility.test_with_parser(xml_data.clone(), parser)
    assert_true(compatibility_result.is_compatible)
    assert_true(compatibility_result.parsing_time_ms < 100)
    assert_true(compatibility_result.validation_passed)
  }
  
  // Test serialization format migration
  let migration_manager = SerializationMigrationManager::new()
  
  // Define migration from v1 to v2
  let v1_to_v2_migration = Migration::new("v1", "v2")
    .add_field_mapping("old_field", "new_field")
    .add_field_renaming("old_name", "new_name")
    .add_field_addition("new_field", "default_value")
    .add_field_removal("deprecated_field")
  
  migration_manager.add_migration(v1_to_v2_migration)
  
  // Test v1 data
  let v1_data = V1TestData::new()
    .with_old_field("old_value")
    .with_old_name("old_name_value")
    .with_deprecated_field("deprecated_value")
  
  let v1_serialized = json_serializer.serialize(v1_data)
  
  // Migrate to v2
  let migrated_result = migration_manager.migrate(v1_serialized, "json", "v1", "v2")
  assert_true(migrated_result.is_success())
  
  let migrated_data = migrated_result.unwrap()
  assert_true(migrated_data.contains("new_field"))
  assert_true(migrated_data.contains("new_name"))
  assert_false(migrated_data.contains("deprecated_field"))
  
  // Test cross-language serialization
  let cross_language_tester = CrossLanguageTester::new()
  
  // Test with different languages
  let languages = ["python", "java", "csharp", "go", "rust"]
  
  for language in languages {
    let compatibility_result = cross_language_tester.test_serialization(json_data.clone(), "json", language)
    assert_true(compatibility_result.is_compatible)
    assert_true(compatibility_result.round_trip_successful)
  }
  
  // Test platform-specific optimizations
  let platform_optimizer = PlatformOptimizer::new()
  
  // Detect current platform
  let current_platform = platform_optimizer.detect_platform()
  assert_true(current_platform.os.length() > 0)
  assert_true(current_platform.arch.length() > 0)
  
  // Apply platform-specific optimizations
  let optimized_serializer = platform_optimizer.optimize_serializer(json_serializer, current_platform)
  
  let optimized_serialize_start = get_current_timestamp()
  let _ = optimized_serializer.serialize(test_data)
  let optimized_serialize_end = get_current_timestamp()
  let optimized_serialize_time = optimized_serialize_end - optimized_serialize_start
  
  let standard_serialize_start = get_current_timestamp()
  let _ = json_serializer.serialize(test_data)
  let standard_serialize_end = get_current_timestamp()
  let standard_serialize_time = standard_serialize_end - standard_serialize_start
  
  // Optimized serializer should be faster or equal
  assert_true(optimized_serialize_time.to_millis() <= standard_serialize_time.to_millis())
  
  // Test serialization format versioning
  let version_manager = SerializationVersionManager::new()
  
  // Register format versions
  version_manager.register_version("json", "1.0", DateTime::new(2020, 1, 1, 0, 0, 0))
  version_manager.register_version("json", "1.1", DateTime::new(2021, 1, 1, 0, 0, 0))
  version_manager.register_version("json", "1.2", DateTime::new(2022, 1, 1, 0, 0, 0))
  version_manager.register_version("json", "2.0", DateTime::new(2023, 1, 1, 0, 0, 0))
  
  // Check version compatibility
  let compatibility_matrix = version_manager.get_compatibility_matrix("json")
  
  assert_true(compatibility_matrix.is_compatible("1.0", "1.1"))
  assert_true(compatibility_matrix.is_compatible("1.1", "1.2"))
  assert_false(compatibility_matrix.is_compatible("1.2", "2.0")) // Breaking change
  assert_true(compatibility_matrix.is_compatible("2.0", "2.0"))
  
  // Test format feature detection
  let feature_detector = FormatFeatureDetector::new()
  
  let json_features = feature_detector.detect_features("json")
  assert_true(json_features.supports_comments == false)
  assert_true(json_features.supports_references == false)
  assert_true(json_features.supports_metadata == false)
  assert_true(json_features.human_readable == true)
  
  let yaml_features = feature_detector.detect_features("yaml")
  assert_true(yaml_features.supports_comments == true)
  assert_true(yaml_features.supports_references == true)
  assert_true(yaml_features.supports_metadata == true)
  assert_true(yaml_features.human_readable == true)
  
  let binary_features = feature_detector.detect_features("binary")
  assert_true(binary_features.supports_comments == false)
  assert_true(binary_features.supports_references == false)
  assert_true(binary_features.supports_metadata == false)
  assert_true(binary_features.human_readable == false)
}

// Test 9: Serialization Error Handling and Recovery
test "serialization error handling and recovery" {
  let error_handling_serializer = ErrorHandlingSerializer::new()
  
  // Test circular reference detection
  let circular_object = CircularObject::new()
  circular_object.set_self_reference()
  
  let circular_result = error_handling_serializer.serialize(circular_object)
  assert_true(circular_result.is_error())
  
  let circular_error = circular_result.unwrap_error()
  assert_eq(circular_error.error_type, SerializationErrorType::CircularReference)
  assert_true(circular_error.message.contains("circular"))
  
  // Test data too large error
  let large_object = create_very_large_object(1000000) // 1GB object
  let large_result = error_handling_serializer.serialize(large_object)
  assert_true(large_result.is_error())
  
  let large_error = large_result.unwrap_error()
  assert_eq(large_error.error_type, SerializationErrorType::DataTooLarge)
  
  // Test invalid format error
  let invalid_format_data = "This is not valid JSON {"
  let invalid_result = error_handling_serializer.deserialize::<TestObject>(invalid_format_data)
  assert_true(invalid_result.is_error())
  
  let invalid_error = invalid_result.unwrap_error()
  assert_eq(invalid_error.error_type, SerializationErrorType::InvalidFormat)
  
  // Test missing field error
  let missing_field_data = "{\"id\":123}" // Missing required name field
  let missing_result = error_handling_serializer.deserialize::<TestObject>(missing_field_data)
  assert_true(missing_result.is_error())
  
  let missing_error = missing_result.unwrap_error()
  assert_eq(missing_error.error_type, SerializationErrorType::MissingField)
  assert_true(missing_error.message.contains("name"))
  
  // Test type mismatch error
  let type_mismatch_data = "{\"id\":\"not_an_integer\",\"name\":\"Test\"}"
  let type_result = error_handling_serializer.deserialize::<TestObject>(type_mismatch_data)
  assert_true(type_result.is_error())
  
  let type_error = type_result.unwrap_error()
  assert_eq(type_error.error_type, SerializationErrorType::TypeMismatch)
  assert_true(type_error.message.contains("id"))
  
  // Test out of range error
  let out_of_range_data = "{\"id\":9999999999999999999999999999,\"name\":\"Test\"}"
  let range_result = error_handling_serializer.deserialize::<TestObject>(out_of_range_data)
  assert_true(range_result.is_error())
  
  let range_error = range_result.unwrap_error()
  assert_eq(range_error.error_type, SerializationErrorType::OutOfRange)
  
  // Test error recovery strategies
  let recovery_serializer = RecoverySerializer::new()
  
  // Set recovery strategies
  recovery_serializer.set_circular_reference_strategy(RecoveryStrategy::ReplaceWithNull)
  recovery_serializer.set_missing_field_strategy(RecoveryStrategy::UseDefaultValue)
  recovery_serializer.set_type_mismatch_strategy(RecoveryStrategy::ConvertIfPossible)
  
  // Test circular reference recovery
  let circular_recovery_result = recovery_serializer.serialize(circular_object)
  assert_true(circular_recovery_result.is_ok())
  
  // Test missing field recovery
  let missing_recovery_result = recovery_serializer.deserialize::<TestObject>(missing_field_data)
  assert_true(missing_recovery_result.is_ok())
  
  let recovered_object = missing_recovery_result.unwrap()
  assert_eq(recovered_object.id, 123)
  assert_eq(recovered_object.name, "") // Default value
  
  // Test type mismatch recovery
  let type_recovery_result = recovery_serializer.deserialize::<TestObject>(type_mismatch_data)
  assert_true(type_recovery_result.is_ok()) // If conversion is possible
  
  // Test error logging and monitoring
  let error_logger = SerializationErrorLogger::new()
  error_handling_serializer.set_error_logger(error_logger)
  
  // Generate some errors
  let _ = error_handling_serializer.serialize(circular_object);
  let _ = error_handling_serializer.deserialize::<TestObject>(invalid_format_data);
  let _ = error_handling_serializer.deserialize::<TestObject>(missing_field_data);
  
  // Check error logs
  let error_logs = error_handling_serializer.get_error_logs()
  assert_true(error_logs.length() >= 3)
  
  for log in error_logs {
    assert_true(log.timestamp > 0)
    assert_true(log.error_type.length() > 0)
    assert_true(log.message.length() > 0)
  }
  
  // Test error statistics
  let error_stats = error_handling_serializer.get_error_statistics()
  assert_true(error_stats.total_errors >= 3)
  assert_true(error_stats.error_counts.get(SerializationErrorType::CircularReference) > 0)
  assert_true(error_stats.error_counts.get(SerializationErrorType::InvalidFormat) > 0)
  assert_true(error_stats.error_counts.get(SerializationErrorType::MissingField) > 0)
  
  // Test error retry mechanism
  let retry_serializer = RetrySerializer::new()
  retry_serializer.set_max_retries(3)
  retry_serializer.set_retry_delay(Duration::from_millis(100))
  
  // Create a flaky serializer that fails sometimes
  let flaky_serializer = FlakySerializer::new(0.5) // 50% failure rate
  
  let retry_result = retry_serializer.serialize_with_retry(|| {
    flaky_serializer.serialize(test_data)
  })
  
  // Should eventually succeed with retries
  assert_true(retry_result.is_ok())
  
  // Test fallback serializers
  let fallback_manager = FallbackSerializerManager::new()
  
  // Add serializers in order of preference
  fallback_manager.add_serializer("json", JsonSerializer::new())
  fallback_manager.add_serializer("binary", BinarySerializer::new())
  fallback_manager.add_serializer("xml", XmlSerializer::new())
  
  // Test fallback when primary fails
  let failing_json_serializer = FailingSerializer::new()
  fallback_manager.add_serializer("failing_json", failing_json_serializer, 0) // Highest priority but fails
  
  let fallback_result = fallback_manager.serialize_with_fallback(test_data)
  assert_true(fallback_result.is_ok())
  
  let fallback_info = fallback_result.unwrap()
  assert_eq(fallback_info.used_format, "binary") // Should fall back to binary
  assert_eq(fallback_info.attempts, 2) // Tried failing_json and binary
  
  // Test partial serialization recovery
  let partial_serializer = PartialSerializer::new()
  
  let complex_object = ComplexObject::new()
    .with_required_field("required_value")
    .with_optional_field("optional_value")
    .with_nested_object(NestedObject::new("nested", 123))
  
  // Make nested object fail to serialize
  let failing_nested = FailingNestedObject::new()
  complex_object.set_nested_object(failing_nested)
  
  let partial_result = partial_serializer.serialize_partial(complex_object)
  assert_true(partial_result.is_ok())
  
  let partial_data = partial_result.unwrap()
  assert_true(partial_data.contains("required_field"))
  assert_true(partial_data.contains("optional_field"))
  assert_false(partial_data.contains("nested_object")) // Should be excluded due to failure
  
  // Test error context preservation
  let context_serializer = ContextPreservingSerializer::new()
  
  let context_result = context_serializer.deserialize_with_context::<TestObject>(missing_field_data, "user_profile_load")
  assert_true(context_result.is_error())
  
  let context_error = context_result.unwrap_error()
  assert_eq(context_error.context, "user_profile_load")
  assert_true(context_error.error_chain.length() > 0)
}

// Test 10: Advanced Serialization Patterns
test "advanced serialization patterns" {
  // Test lazy serialization
  let lazy_serializer = LazySerializer::new()
  
  let lazy_object = LazyObject::new()
    .with_id(123)
    .with_lazy_field(|| {
      // Expensive computation
      sleep(Duration::from_millis(100))
      "computed_value"
    })
  
  let lazy_serialize_start = get_current_timestamp()
  let lazy_serialized = lazy_serializer.serialize(lazy_object)
  let lazy_serialize_end = get_current_timestamp()
  let lazy_serialize_time = lazy_serialize_end - lazy_serialize_start
  
  // Lazy serialization should be fast initially
  assert_true(lazy_serialize_time.to_millis() < 50)
  
  // Force lazy field computation
  let _ = lazy_object.get_lazy_field();
  
  let lazy_serialize_start_2 = get_current_timestamp()
  let _ = lazy_serializer.serialize(lazy_object)
  let lazy_serialize_end_2 = get_current_timestamp()
  let lazy_serialize_time_2 = lazy_serialize_end_2 - lazy_serialize_start_2
  
  // Second serialization should be slower due to lazy field computation
  assert_true(lazy_serialize_time_2.to_millis() > 50)
  
  // Test proxy serialization
  let proxy_serializer = ProxySerializer::new()
  
  let proxy_object = ProxyObject::new()
    .with_id(123)
    .with_large_data("x".repeat(1000000)) // 1MB of data
  
  let proxy_serialized = proxy_serializer.serialize(proxy_object)
  
  // Proxy should serialize reference instead of actual data
  assert_true(proxy_serialized.length() < 1000) // Should be much smaller than 1MB
  assert_true(proxy_serialized.contains("proxy_reference"))
  
  // Test virtual object serialization
  let virtual_serializer = VirtualSerializer::new()
  
  let virtual_object = VirtualObject::new()
    .with_virtual_properties({
      "computed_property": || "computed_value",
      "dynamic_property": || get_current_time().to_string()
    })
  
  let virtual_serialized = virtual_serializer.serialize(virtual_object)
  assert_true(virtual_serialized.contains("computed_property"))
  assert_true(virtual_serialized.contains("dynamic_property"))
  
  // Test differential serialization
  let diff_serializer = DifferentialSerializer::new()
  
  let original_object = LargeObject::new()
    .with_id(123)
    .with_name("Original Name")
    .with_data("x".repeat(1000))
  
  let modified_object = LargeObject::new()
    .with_id(123) // Same ID
    .with_name("Modified Name") // Changed
    .with_data("x".repeat(1000)) // Same data
  
  // Serialize original
  let original_serialized = diff_serializer.serialize(original_object)
  
  // Serialize difference
  let diff_serialized = diff_serializer.serialize_difference(modified_object, original_object)
  
  // Difference should be smaller than full serialization
  assert_true(diff_serialized.length() < original_serialized.length())
  
  // Test patch application
  let patch_result = diff_serializer.apply_patch(original_serialized, diff_serialized)
  assert_true(patch_result.is_success())
  
  let patched_object = diff_serializer.deserialize::<LargeObject>(patch_result.unwrap())
  assert_true(patched_object.is_ok())
  
  let patched = patched_object.unwrap()
  assert_eq(patched.id, 123)
  assert_eq(patched.name, "Modified Name")
  
  // Test incremental serialization
  let incremental_serializer = IncrementalSerializer::new()
  
  let incremental_object = IncrementalObject::new()
  
  // Serialize initial state
  let initial_serialized = incremental_serializer.serialize_initial(incremental_object)
  
  // Add changes incrementally
  incremental_object.add_change("field1", "value1");
  let incremental_1 = incremental_serializer.serialize_incremental();
  
  incremental_object.add_change("field2", "value2");
  incremental_object.add_change("field3", "value3");
  let incremental_2 = incremental_serializer.serialize_incremental();
  
  // Apply increments to reconstruct
  let reconstructed = incremental_serializer.apply_increments(initial_serialized, [incremental_1, incremental_2])
  assert_true(reconstructed.is_success())
  
  // Test conditional serialization
  let conditional_serializer = ConditionalSerializer::new()
  
  let conditional_object = ConditionalObject::new()
    .with_id(123)
    .with_public_field("public_value")
    .with_private_field("private_value")
    .with_debug_field("debug_value")
  
  // Serialize based on user role
  let admin_serialized = conditional_serializer.serialize_with_condition(conditional_object, "role", "admin");
  assert_true(admin_serialized.contains("private_value")) // Admin can see private
  assert_true(admin_serialized.contains("debug_value")) // Admin can see debug
  
  let user_serialized = conditional_serializer.serialize_with_condition(conditional_object, "role", "user");
  assert_false(user_serialized.contains("private_value")) // User cannot see private
  assert_false(user_serialized.contains("debug_value")) // User cannot see debug
  
  let debug_serialized = conditional_serializer.serialize_with_condition(conditional_object, "mode", "debug");
  assert_true(debug_serialized.contains("debug_value")) // Debug mode shows debug field
  
  // Test streaming serialization
  let streaming_serializer = StreamingSerializer::new()
  
  let streaming_object = StreamingObject::new()
    .with_large_data("x".repeat(1000000)) // 1MB of data
  
  let stream_result = streaming_serializer.serialize_to_stream(streaming_object, |chunk| {
    // Process chunk
    assert_true(chunk.length() > 0)
    assert_true(chunk.length() <= 65536) // Max chunk size
  })
  
  assert_true(stream_result.is_success())
  
  // Test memoization serialization
  let memoized_serializer = MemoizedSerializer::new()
  
  let memoized_object = MemoizedObject::new()
    .with_id(123)
    .with_expensive_field(|| {
      // Expensive computation
      sleep(Duration::from_millis(100))
      "expensive_value"
    })
  
  // First serialization
  let memoized_start_1 = get_current_timestamp()
  let _ = memoized_serializer.serialize(memoized_object.clone())
  let memoized_end_1 = get_current_timestamp()
  let memoized_time_1 = memoized_end_1 - memoized_start_1
  
  // Second serialization (should use memoized result)
  let memoized_start_2 = get_current_timestamp()
  let _ = memoized_serializer.serialize(memoized_object.clone())
  let memoized_end_2 = get_current_timestamp()
  let memoized_time_2 = memoized_end_2 - memoized_start_2
  
  // Second serialization should be faster
  assert_true(memoized_time_2.to_millis() < memoized_time_1.to_millis() / 2)
  
  // Test adaptive serialization
  let adaptive_serializer = AdaptiveSerializer::new()
  
  let adaptive_object = AdaptiveObject::new()
    .with_small_data("small")
    .with_large_data("x".repeat(100000))
  
  // Adaptive serializer should choose best format based on data characteristics
  let adaptive_result = adaptive_serializer.serialize_adaptive(adaptive_object)
  assert_true(adaptive_result.is_success())
  
  let adaptive_info = adaptive_result.unwrap()
  assert_true(adaptive_info.chosen_format.length() > 0)
  assert_true(adaptive_info.compression_ratio > 0.0)
  assert_true(adaptive_info.serialization_time_ms > 0)
}