// Azimuth Telemetry System - Data Processing and Conversion Tests
// This file contains comprehensive test cases for data processing and conversion functionality

// Test 1: Data Type Conversion Tests
test "data type conversion operations" {
  // Test string to int conversion
  let int_str = "42"
  let int_result = int_str.to_int()
  match int_result {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  // Test string to float conversion
  let float_str = "3.14159"
  let float_result = float_str.to_float()
  match float_result {
    Some(value) => assert_eq(value, 3.14159)
    None => assert_true(false)
  }
  
  // Test int to string conversion
  let int_val = 123
  let int_str_result = int_val.to_string()
  assert_eq(int_str_result, "123")
  
  // Test float to string conversion
  let float_val = 2.71828
  let float_str_result = float_val.to_string()
  assert_eq(float_str_result, "2.71828")
  
  // Test boolean conversion
  let bool_true = true
  let bool_false = false
  assert_eq(bool_true.to_string(), "true")
  assert_eq(bool_false.to_string(), "false")
  
  // Test invalid conversion handling
  let invalid_int_str = "not_a_number"
  let invalid_result = invalid_int_str.to_int()
  match invalid_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Data Format Conversion Tests
test "data format conversion operations" {
  // Test JSON serialization and deserialization
  let test_data = [
    ("name", StringValue("test_service")),
    ("version", StringValue("1.0.0")),
    ("port", IntValue(8080)),
    ("enabled", BoolValue(true)),
    ("tags", ArrayStringValue(["azimuth", "telemetry", "test"]))
  ]
  
  // Test JSON serialization
  let json_str = JsonUtil::serialize(test_data)
  assert_true(json_str.contains("\"name\":\"test_service\""))
  assert_true(json_str.contains("\"version\":\"1.0.0\""))
  assert_true(json_str.contains("\"port\":8080"))
  assert_true(json_str.contains("\"enabled\":true"))
  
  // Test JSON deserialization
  let deserialized_data = JsonUtil::deserialize(json_str)
  match deserialized_data {
    Some(data) => {
      let name_value = MapUtil::get(data, "name")
      match name_value {
        Some(StringValue(name)) => assert_eq(name, "test_service")
        _ => assert_true(false)
      }
      
      let port_value = MapUtil::get(data, "port")
      match port_value {
        Some(IntValue(port)) => assert_eq(port, 8080)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test XML conversion
  let xml_str = XmlUtil::to_xml(test_data)
  assert_true(xml_str.contains("<name>test_service</name>"))
  assert_true(xml_str.contains("<version>1.0.0</version>"))
  assert_true(xml_str.contains("<port>8080</port>"))
  
  // Test CSV conversion
  let csv_data = [
    ["timestamp", "metric_name", "value", "tags"],
    ["2023-01-01T00:00:00Z", "cpu_usage", "75.5", "region:us-east"],
    ["2023-01-01T00:01:00Z", "memory_usage", "60.2", "region:us-west"]
  ]
  let csv_str = CsvUtil::serialize(csv_data)
  assert_true(csv_str.contains("timestamp,metric_name,value,tags"))
  assert_true(csv_str.contains("2023-01-01T00:00:00Z,cpu_usage,75.5,region:us-east"))
}

// Test 3: Data Aggregation Tests
test "data aggregation operations" {
  // Test numeric aggregation
  let numbers = [1.0, 2.5, 3.0, 4.5, 5.0]
  
  // Test sum
  let sum = AggregationUtil::sum(numbers)
  assert_eq(sum, 16.0)
  
  // Test average
  let avg = AggregationUtil::average(numbers)
  assert_eq(avg, 3.2)
  
  // Test min
  let min = AggregationUtil::min(numbers)
  assert_eq(min, 1.0)
  
  // Test max
  let max = AggregationUtil::max(numbers)
  assert_eq(max, 5.0)
  
  // Test median
  let median = AggregationUtil::median(numbers)
  assert_eq(median, 3.0)
  
  // Test percentile
  let p95 = AggregationUtil::percentile(numbers, 95.0)
  assert_eq(p95, 5.0)
  
  // Test empty array handling
  let empty_numbers = []
  let empty_sum = AggregationUtil::sum(empty_numbers)
  assert_eq(empty_sum, 0.0)
  
  let empty_avg = AggregationUtil::average(empty_numbers)
  assert_eq(empty_avg, 0.0)
}

// Test 4: Data Filtering and Transformation Tests
test "data filtering and transformation operations" {
  // Test data filtering
  let test_records = [
    ("service1", "region1", 100.0, "success"),
    ("service2", "region1", 200.0, "error"),
    ("service3", "region2", 150.0, "success"),
    ("service4", "region2", 50.0, "error"),
    ("service5", "region1", 300.0, "success")
  ]
  
  // Filter by region
  let region1_records = FilterUtil::filter_by_region(test_records, "region1")
  assert_eq(region1_records.length(), 3)
  
  // Filter by status
  let success_records = FilterUtil::filter_by_status(test_records, "success")
  assert_eq(success_records.length(), 3)
  
  // Filter by value threshold
  let high_value_records = FilterUtil::filter_by_value_threshold(test_records, 150.0)
  assert_eq(high_value_records.length(), 2)
  
  // Test data transformation
  let transformed_records = TransformUtil::normalize_values(test_records)
  for record in transformed_records {
    match record {
      (_, _, value, _) => assert_true(value >= 0.0 && value <= 1.0)
    }
  }
  
  // Test data grouping
  let grouped_by_region = TransformUtil::group_by_region(test_records)
  assert_true(MapUtil::contains_key(grouped_by_region, "region1"))
  assert_true(MapUtil::contains_key(grouped_by_region, "region2"))
  
  let region1_group = MapUtil::get(grouped_by_region, "region1")
  match region1_group {
    Some(records) => assert_eq(records.length(), 3)
    None => assert_true(false)
  }
}

// Test 5: Data Validation Tests
test "data validation operations" {
  // Test required field validation
  let valid_record = [
    ("timestamp", StringValue("2023-01-01T00:00:00Z")),
    ("service_name", StringValue("test_service")),
    ("trace_id", StringValue("0af7651916cd43dd8448eb211c80319c"))
  ]
  
  let validation_result = ValidationUtil::validate_required_fields(valid_record, ["timestamp", "service_name", "trace_id"])
  assert_true(validation_result.is_valid)
  
  // Test missing required field
  let invalid_record = [
    ("timestamp", StringValue("2023-01-01T00:00:00Z")),
    ("service_name", StringValue("test_service"))
    // Missing trace_id
  ]
  
  let invalid_validation_result = ValidationUtil::validate_required_fields(invalid_record, ["timestamp", "service_name", "trace_id"])
  assert_false(invalid_validation_result.is_valid)
  assert_true(invalid_validation_result.errors.length() > 0)
  
  // Test data type validation
  let type_validation_result = ValidationUtil::validate_field_types(valid_record, [
    ("timestamp", "string"),
    ("service_name", "string"),
    ("trace_id", "string")
  ])
  assert_true(type_validation_result.is_valid)
  
  // Test invalid data type
  let type_invalid_record = [
    ("timestamp", StringValue("2023-01-01T00:00:00Z")),
    ("service_name", IntValue(123)),  // Should be string
    ("trace_id", StringValue("0af7651916cd43dd8448eb211c80319c"))
  ]
  
  let type_invalid_result = ValidationUtil::validate_field_types(type_invalid_record, [
    ("timestamp", "string"),
    ("service_name", "string"),
    ("trace_id", "string")
  ])
  assert_false(type_invalid_result.is_valid)
  
  // Test range validation
  let range_record = [
    ("value", IntValue(50)),
    ("percentage", FloatValue(0.75))
  ]
  
  let range_validation_result = ValidationUtil::validate_ranges(range_record, [
    ("value", 0, 100),
    ("percentage", 0.0, 1.0)
  ])
  assert_true(range_validation_result.is_valid)
  
  // Test out of range
  let out_of_range_record = [
    ("value", IntValue(150)),
    ("percentage", FloatValue(1.5))
  ]
  
  let out_of_range_result = ValidationUtil::validate_ranges(out_of_range_record, [
    ("value", 0, 100),
    ("percentage", 0.0, 1.0)
  ])
  assert_false(out_of_range_result.is_valid)
}

// Test 6: Data Compression Tests
test "data compression operations" {
  // Test string compression
  let original_string = "This is a test string for compression. It contains repetitive text that should compress well. This is a test string for compression."
  let compressed_string = CompressionUtil::compress(original_string)
  
  assert_true(compressed_string.length() < original_string.length())
  
  // Test decompression
  let decompressed_string = CompressionUtil::decompress(compressed_string)
  assert_eq(decompressed_string, original_string)
  
  // Test binary data compression
  let binary_data = [0x01, 0x02, 0x03, 0x04, 0x05, 0x01, 0x02, 0x03, 0x04, 0x05]
  let compressed_binary = CompressionUtil::compress_binary(binary_data)
  let decompressed_binary = CompressionUtil::decompress_binary(compressed_binary)
  
  assert_eq(binary_data.length(), decompressed_binary.length())
  for i in 0..binary_data.length() {
    assert_eq(binary_data[i], decompressed_binary[i])
  }
  
  // Test compression level
  let high_compression = CompressionUtil::compress_with_level(original_string, 9)
  let low_compression = CompressionUtil::compress_with_level(original_string, 1)
  
  assert_true(high_compression.length() <= low_compression.length())
}

// Test 7: Data Encoding Tests
test "data encoding operations" {
  // Test Base64 encoding
  let original_data = "Hello, World!"
  let base64_encoded = EncodingUtil::base64_encode(original_data)
  
  // Test Base64 decoding
  let base64_decoded = EncodingUtil::base64_decode(base64_encoded)
  assert_eq(base64_decoded, original_data)
  
  // Test URL encoding
  let url_string = "https://example.com/path?param=value&other=value with spaces"
  let url_encoded = EncodingUtil::url_encode(url_string)
  
  // Test URL decoding
  let url_decoded = EncodingUtil::url_decode(url_encoded)
  assert_eq(url_decoded, url_string)
  
  // Test HTML encoding
  let html_string = "<div>Hello & \"World\"</div>"
  let html_encoded = EncodingUtil::html_encode(html_string)
  assert_true(html_encoded.contains("&lt;div&gt;"))
  assert_true(html_encoded.contains("&amp;"))
  assert_true(html_encoded.contains("&quot;"))
  
  // Test HTML decoding
  let html_decoded = EncodingUtil::html_decode(html_encoded)
  assert_eq(html_decoded, html_string)
  
  // Test Hex encoding
  let hex_data = [0x48, 0x65, 0x6C, 0x6C, 0x6F]  // "Hello" in ASCII
  let hex_encoded = EncodingUtil::hex_encode(hex_data)
  assert_eq(hex_encoded, "48656C6C6F")
  
  // Test Hex decoding
  let hex_decoded = EncodingUtil::hex_decode(hex_encoded)
  assert_eq(hex_data.length(), hex_decoded.length())
  for i in 0..hex_data.length() {
    assert_eq(hex_data[i], hex_decoded[i])
  }
}

// Test 8: Data Deduplication Tests
test "data deduplication operations" {
  // Test array deduplication
  let duplicate_array = [1, 2, 3, 2, 4, 1, 5, 3, 6, 4]
  let deduplicated_array = DedupUtil::deduplicate_array(duplicate_array)
  
  assert_eq(deduplicated_array.length(), 6)
  assert_true(deduplicated_array.contains(1))
  assert_true(deduplicated_array.contains(2))
  assert_true(deduplicated_array.contains(3))
  assert_true(deduplicated_array.contains(4))
  assert_true(deduplicated_array.contains(5))
  assert_true(deduplicated_array.contains(6))
  
  // Test string deduplication
  let duplicate_strings = ["apple", "banana", "apple", "cherry", "banana", "date"]
  let deduplicated_strings = DedupUtil::deduplicate_strings(duplicate_strings)
  
  assert_eq(deduplicated_strings.length(), 4)
  assert_true(deduplicated_strings.contains("apple"))
  assert_true(deduplicated_strings.contains("banana"))
  assert_true(deduplicated_strings.contains("cherry"))
  assert_true(deduplicated_strings.contains("date"))
  
  // Test record deduplication
  let duplicate_records = [
    ("trace1", "span1", "service1", 100L),
    ("trace2", "span2", "service2", 200L),
    ("trace1", "span1", "service1", 100L),  // Duplicate
    ("trace3", "span3", "service3", 300L),
    ("trace2", "span2", "service2", 200L)   // Duplicate
  ]
  
  let deduplicated_records = DedupUtil::deduplicate_records(duplicate_records)
  assert_eq(deduplicated_records.length(), 3)
  
  // Test fuzzy deduplication (similar but not identical)
  let similar_strings = ["test@example.com", "test@example.com ", "Test@Example.com", "test@example.com"]
  let fuzzy_deduplicated = DedupUtil::fuzzy_deduplicate_strings(similar_strings)
  assert_eq(fuzzy_deduplicated.length(), 2)  // Should group similar emails
}

// Test 9: Data Normalization Tests
test "data normalization operations" {
  // Test numeric normalization
  let numeric_data = [10.0, 20.0, 30.0, 40.0, 50.0]
  let min_max_normalized = NormalizationUtil::min_max_normalize(numeric_data)
  
  assert_eq(min_max_normalized[0], 0.0)    // (10-10)/(50-10) = 0
  assert_eq(min_max_normalized[4], 1.0)    // (50-10)/(50-10) = 1
  assert_eq(min_max_normalized[2], 0.5)    // (30-10)/(50-10) = 0.5
  
  // Test z-score normalization
  let z_normalized = NormalizationUtil::z_score_normalize(numeric_data)
  let z_mean = AggregationUtil::sum(z_normalized) / z_normalized.length()
  assert_true(z_mean >= -0.1 && z_mean <= 0.1)  // Mean should be close to 0
  
  // Test string normalization
  let messy_strings = ["  Hello World  ", "HELLO WORLD", "hello world", "Hello World!"]
  let normalized_strings = NormalizationUtil::normalize_strings(messy_strings)
  
  for normalized_str in normalized_strings {
    assert_eq(normalized_str, "hello world")
  }
  
  // Test timestamp normalization
  let timestamps = [
    "2023-01-01T00:00:00Z",
    "2023-01-01 01:00:00",
    "01/01/2023 02:00:00",
    "2023/01/01 03:00:00"
  ]
  
  let normalized_timestamps = NormalizationUtil::normalize_timestamps(timestamps)
  for normalized_ts in normalized_timestamps {
    assert_true(normalized_ts.length() == 20)  // ISO 8601 format length
    assert_true(normalized_ts.contains("2023-01-01T"))
  }
  
  // Test address normalization
  let addresses = [
    "123 Main St, New York, NY 10001",
    "123 Main Street, New York, New York 10001",
    "123 Main St., New York, NY 10001"
  ]
  
  let normalized_addresses = NormalizationUtil::normalize_addresses(addresses)
  for normalized_addr in normalized_addresses {
    assert_eq(normalized_addr, "123 main st new york ny 10001")
  }
}

// Test 10: Data Migration Tests
test "data migration operations" {
  // Test schema migration
  let old_schema = [
    ("service", StringValue("test_service")),
    ("version", StringValue("1.0.0")),
    ("host", StringValue("localhost"))
  ]
  
  let migration_rules = [
    ("service", "service_name"),  // Rename field
    ("version", "service_version"),
    ("host", "service_host")
  ]
  
  let migrated_schema = MigrationUtil::migrate_schema(old_schema, migration_rules)
  
  // Verify old fields are gone
  assert_false(MapUtil::contains_key(migrated_schema, "service"))
  assert_false(MapUtil::contains_key(migrated_schema, "version"))
  assert_false(MapUtil::contains_key(migrated_schema, "host"))
  
  // Verify new fields exist
  assert_true(MapUtil::contains_key(migrated_schema, "service_name"))
  assert_true(MapUtil::contains_key(migrated_schema, "service_version"))
  assert_true(MapUtil::contains_key(migrated_schema, "service_host"))
  
  // Test data type migration
  let type_migration_rules = [
    ("port", "string", "int"),  // Convert string to int
    ("enabled", "bool", "string")  // Convert bool to string
  ]
  
  let old_data = [
    ("port", StringValue("8080")),
    ("enabled", BoolValue(true)),
    ("name", StringValue("test"))
  ]
  
  let type_migrated_data = MigrationUtil::migrate_data_types(old_data, type_migration_rules)
  
  let port_value = MapUtil::get(type_migrated_data, "port")
  match port_value {
    Some(IntValue(port)) => assert_eq(port, 8080)
    _ => assert_true(false)
  }
  
  let enabled_value = MapUtil::get(type_migrated_data, "enabled")
  match enabled_value {
    Some(StringValue(enabled)) => assert_eq(enabled, "true")
    _ => assert_true(false)
  }
  
  // Test version migration
  let version1_data = [
    ("service_name", StringValue("test")),
    ("metrics", ArrayIntValue([1, 2, 3]))
  ]
  
  let version2_data = MigrationUtil::migrate_version(version1_data, "1.0.0", "2.0.0")
  
  // Verify version-specific transformations
  assert_true(MapUtil::contains_key(version2_data, "service_name"))
  assert_true(MapUtil::contains_key(version2_data, "metrics"))
  assert_true(MapUtil::contains_key(version2_data, "schema_version"))
  
  let schema_version = MapUtil::get(version2_data, "schema_version")
  match schema_version {
    Some(StringValue(version)) => assert_eq(version, "2.0.0")
    _ => assert_true(false)
  }
}