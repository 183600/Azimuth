// Azimuth Data Processing and Transformation Test Suite
// This file contains test cases for data processing and transformation operations

// Test 1: Data Type Conversion Operations
test "data type conversion operations" {
  // Test string to numeric conversions
  let string_int = "42"
  let string_float = "3.14159"
  let string_bool = "true"
  
  let converted_int = DataConverter::string_to_int(string_int)
  match converted_int {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  let converted_float = DataConverter::string_to_float(string_float)
  match converted_float {
    Some(value) => assert_true(value > 3.14 && value < 3.15)
    None => assert_true(false)
  }
  
  let converted_bool = DataConverter::string_to_bool(string_bool)
  match converted_bool {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  // Test numeric to string conversions
  let int_value = 123
  let float_value = 45.678
  let bool_value = false
  
  let int_to_string = DataConverter::int_to_string(int_value)
  assert_eq(int_to_string, "123")
  
  let float_to_string = DataConverter::float_to_string(float_value)
  assert_true(float_to_string.contains("45.678"))
  
  let bool_to_string = DataConverter::bool_to_string(bool_value)
  assert_eq(bool_to_string, "false")
  
  // Test invalid conversions
  let invalid_int = "not_a_number"
  let invalid_int_conversion = DataConverter::string_to_int(invalid_int)
  assert_eq(invalid_int_conversion, None)
  
  let invalid_float = "not_a_float"
  let invalid_float_conversion = DataConverter::string_to_float(invalid_float)
  assert_eq(invalid_float_conversion, None)
  
  let invalid_bool = "not_a_bool"
  let invalid_bool_conversion = DataConverter::string_to_bool(invalid_bool)
  assert_eq(invalid_bool_conversion, None)
}

// Test 2: Data Format Transformation
test "data format transformation" {
  // Test JSON transformation
  let json_data = "{\"name\":\"test\",\"value\":42,\"active\":true}"
  let transformed_data = DataTransformer::json_to_attributes(json_data)
  
  match transformed_data {
    Some(attrs) => {
      let name_attr = AttributeCollection::get(attrs, "name")
      match name_attr {
        Some(StringValue(value)) => assert_eq(value, "test")
        _ => assert_true(false)
      }
      
      let value_attr = AttributeCollection::get(attrs, "value")
      match value_attr {
        Some(IntValue(value)) => assert_eq(value, 42)
        _ => assert_true(false)
      }
      
      let active_attr = AttributeCollection::get(attrs, "active")
      match active_attr {
        Some(BoolValue(value)) => assert_true(value)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test attributes to JSON transformation
  let attributes = AttributeCollection::new()
  let with_name = AttributeCollection::add(attributes, Attribute::create("name", "test"))
  let with_value = AttributeCollection::add(with_name, Attribute::create("value", 42))
  let with_active = AttributeCollection::add(with_value, Attribute::create("active", true))
  
  let json_output = DataTransformer::attributes_to_json(with_active)
  assert_true(json_output.contains("\"name\":\"test\""))
  assert_true(json_output.contains("\"value\":42"))
  assert_true(json_output.contains("\"active\":true"))
  
  // Test CSV transformation
  let csv_data = "name,age,city\nJohn,30,New York\nJane,25,Los Angeles\nBob,35,Chicago"
  let csv_transformed = DataTransformer::csv_to_records(csv_data)
  
  assert_eq(csv_transformed.length(), 3)
  
  let first_record = csv_transformed[0]
  assert_eq(Record::get_field(first_record, "name"), "John")
  assert_eq(Record::get_field(first_record, "age"), "30")
  assert_eq(Record::get_field(first_record, "city"), "New York")
  
  // Test records to CSV transformation
  let csv_output = DataTransformer::records_to_csv(csv_transformed)
  assert_true(csv_output.contains("name,age,city"))
  assert_true(csv_output.contains("John,30,New York"))
}

// Test 3: Data Aggregation Operations
test "data aggregation operations" {
  // Test numeric aggregation
  let numeric_data = [10, 20, 30, 40, 50]
  
  let sum = DataAggregator::sum(numeric_data)
  assert_eq(sum, 150)
  
  let average = DataAggregator::average(numeric_data)
  assert_eq(average, 30.0)
  
  let min = DataAggregator::min(numeric_data)
  assert_eq(min, 10)
  
  let max = DataAggregator::max(numeric_data)
  assert_eq(max, 50)
  
  let median = DataAggregator::median(numeric_data)
  assert_eq(median, 30)
  
  // Test float data aggregation
  let float_data = [1.5, 2.5, 3.5, 4.5, 5.5]
  
  let float_sum = DataAggregator::sum_float(float_data)
  assert_true(float_sum > 17.4 && float_sum < 17.6)
  
  let float_average = DataAggregator::average_float(float_data)
  assert_true(float_average > 3.4 && float_average < 3.6)
  
  // Test grouped aggregation
  let grouped_data = [
    ("category1", 10),
    ("category2", 20),
    ("category1", 15),
    ("category2", 25),
    ("category1", 5)
  ]
  
  let grouped_sum = DataAggregator::group_by_sum(grouped_data)
  
  let category1_sum = Map::get(grouped_sum, "category1")
  match category1_sum {
    Some(value) => assert_eq(value, 30)
    None => assert_true(false)
  }
  
  let category2_sum = Map::get(grouped_sum, "category2")
  match category2_sum {
    Some(value) => assert_eq(value, 45)
    None => assert_true(false)
  }
  
  // Test time-based aggregation
  let base_time = 1640995200000L // 2022-01-01 00:00:00 UTC
  let time_series_data = [
    (base_time, 10.0),
    (base_time + 60000L, 20.0), // +1 minute
    (base_time + 120000L, 15.0), // +2 minutes
    (base_time + 180000L, 25.0), // +3 minutes
    (base_time + 240000L, 30.0)  // +4 minutes
  ]
  
  let time_windowed_avg = DataAggregator::time_window_average(time_series_data, 120000L) // 2-minute windows
  
  assert_eq(time_windowed_avg.length(), 2)
  
  // First window (0-2 minutes): average of 10.0 and 20.0
  assert_true(time_windowed_avg[0].1 > 14.9 && time_windowed_avg[0].1 < 15.1)
  
  // Second window (2-4 minutes): average of 15.0, 25.0, and 30.0
  assert_true(time_windowed_avg[1].1 > 23.2 && time_windowed_avg[1].1 < 23.4)
}

// Test 4: Data Filtering and Selection
test "data filtering and selection" {
  // Test numeric filtering
  let numeric_data = [5, 10, 15, 20, 25, 30]
  
  let greater_than_15 = DataFilter::filter_numeric(numeric_data, |x| x > 15)
  assert_eq(greater_than_15.length(), 3)
  assert_eq(greater_than_15[0], 20)
  assert_eq(greater_than_15[2], 30)
  
  let even_numbers = DataFilter::filter_numeric(numeric_data, |x| x % 2 == 0)
  assert_eq(even_numbers.length(), 3)
  assert_eq(even_numbers[0], 10)
  assert_eq(even_numbers[2], 30)
  
  // Test string filtering
  let string_data = ["apple", "banana", "cherry", "date", "elderberry"]
  
  let starts_with_a = DataFilter::filter_string(string_data, |s| String::starts_with(s, "a"))
  assert_eq(starts_with_a.length(), 2)
  assert_eq(starts_with_a[0], "apple")
  
  let length_gt_5 = DataFilter::filter_string(string_data, |s| String::length(s) > 5)
  assert_eq(length_gt_5.length(), 3)
  assert_eq(length_gt_5[0], "banana")
  
  // Test record filtering
  let records = [
    Record::new([("name", "John"), ("age", "30"), ("city", "New York")]),
    Record::new([("name", "Jane"), ("age", "25"), ("city", "Los Angeles")]),
    Record::new([("name", "Bob"), ("age", "35"), ("city", "Chicago")])
  ]
  
  let age_gt_30 = DataFilter::filter_records(records, |record| {
    match Record::get_field_as_int(record, "age") {
      Some(age) => age > 30,
      None => false
    }
  })
  
  assert_eq(age_gt_30.length(), 1)
  assert_eq(Record::get_field(age_gt_30[0], "name"), "Bob")
  
  // Test attribute filtering
  let attributes = AttributeCollection::new()
  let with_attrs = AttributeCollection::add(attributes, Attribute::create("name", "test"))
  let with_attrs2 = AttributeCollection::add(with_attrs, Attribute::create("value", 42))
  let with_attrs3 = AttributeCollection::add(with_attrs2, Attribute::create("active", true))
  
  let int_attributes = DataFilter::filter_attributes(with_attrs3, |attr| {
    match Attribute::value_type(attr) {
      IntType => true,
      _ => false
    }
  })
  
  assert_eq(int_attributes.length(), 1)
  assert_eq(Attribute::key(int_attributes[0]), "value")
}

// Test 5: Data Validation Operations
test "data validation operations" {
  // Test numeric validation
  let validator = DataValidator::numeric()
  assert_true(DataValidator::validate(validator, 42))
  assert_true(DataValidator::validate(validator, 3.14))
  assert_false(DataValidator::validate(validator, "not_numeric"))
  
  // Test range validation
  let range_validator = DataValidator::range(10, 100)
  assert_true(DataValidator::validate(range_validator, 50))
  assert_false(DataValidator::validate(range_validator, 5))
  assert_false(DataValidator::validate(range_validator, 150))
  
  // Test string validation
  let string_validator = DataValidator::string()
  assert_true(DataValidator::validate(string_validator, "test"))
  assert_false(DataValidator::validate(string_validator, 42))
  
  // Test regex validation
  let email_validator = DataValidator::regex("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")
  assert_true(DataValidator::validate(email_validator, "test@example.com"))
  assert_false(DataValidator::validate(email_validator, "invalid_email"))
  
  // Test required validation
  let required_validator = DataValidator::required()
  assert_true(DataValidator::validate(required_validator, "value"))
  assert_false(DataValidator::validate(required_validator, ""))
  assert_false(DataValidator::validate(required_validator, None))
  
  // Test composite validation
  let composite_validator = DataValidator::all([
    DataValidator::required(),
    DataValidator::string(),
    DataValidator::min_length(5)
  ])
  
  assert_true(DataValidator::validate(composite_validator, "valid_string"))
  assert_false(DataValidator::validate(composite_validator, "short"))
  assert_false(DataValidator::validate(composite_validator, 123))
  
  // Test record validation
  let record_schema = DataValidator::schema([
    ("name", DataValidator::required()),
    ("age", DataValidator::all([DataValidator::required(), DataValidator::range(0, 150)])),
    ("email", DataValidator::regex("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"))
  ])
  
  let valid_record = Record::new([
    ("name", "John Doe"),
    ("age", "30"),
    ("email", "john@example.com")
  ])
  
  let invalid_record = Record::new([
    ("name", ""),
    ("age", "200"),
    ("email", "invalid_email")
  ])
  
  assert_true(DataValidator::validate_record(record_schema, valid_record))
  assert_false(DataValidator::validate_record(record_schema, invalid_record))
}

// Test 6: Data Normalization Operations
test "data normalization operations" {
  // Test numeric normalization (min-max)
  let numeric_data = [10, 20, 30, 40, 50]
  let normalized_data = DataNormalizer::min_max(numeric_data)
  
  assert_eq(normalized_data.length(), 5)
  assert_eq(normalized_data[0], 0.0) // Min value
  assert_eq(normalized_data[4], 1.0) // Max value
  assert_eq(normalized_data[2], 0.5) // Middle value
  
  // Test z-score normalization
  let z_score_data = DataNormalizer::z_score(numeric_data)
  
  assert_eq(z_score_data.length(), 5)
  // Mean should be 0 after z-score normalization
  let z_score_sum = z_score_data.reduce(|acc, val| acc + val, 0.0)
  assert_true(z_score_sum > -0.1 && z_score_sum < 0.1)
  
  // Test string normalization
  let string_data = ["  Hello World  ", "  TEST STRING  ", "  Mixed Case  "]
  let normalized_strings = DataNormalizer::strings(string_data)
  
  assert_eq(normalized_strings[0], "hello world")
  assert_eq(normalized_strings[1], "test string")
  assert_eq(normalized_strings[2], "mixed case")
  
  // Test date normalization
  let date_strings = ["2022-01-01", "01/02/2022", "2022.03.01"]
  let normalized_dates = DataNormalizer::dates(date_strings, "YYYY-MM-DD")
  
  assert_eq(normalized_dates[0], "2022-01-01")
  assert_eq(normalized_dates[1], "2022-01-02")
  assert_eq(normalized_dates[2], "2022-03-01")
  
  // Test record normalization
  let records = [
    Record::new([("name", "  John  "), ("age", " 30 "), ("active", " true ")]),
    Record::new([("name", "  Jane  "), ("age", " 25 "), ("active", " false ")]),
    Record::new([("name", "  Bob  "), ("age", " 35 "), ("active", " true ")])
  ]
  
  let normalized_records = DataNormalizer::records(records, [
    ("name", StringNormalizer::trim()),
    ("age", NumericNormalizer::parse_int()),
    ("active", BooleanNormalizer::parse())
  ])
  
  assert_eq(Record::get_field(normalized_records[0], "name"), "John")
  assert_eq(Record::get_field(normalized_records[0], "age"), "30")
  assert_eq(Record::get_field(normalized_records[0], "active"), "true")
}

// Test 7: Data Enrichment Operations
test "data enrichment operations" {
  // Test attribute enrichment
  let base_attributes = AttributeCollection::new()
  let with_name = AttributeCollection::add(base_attributes, Attribute::create("name", "test"))
  
  let enriched_attributes = DataEnricher::add_timestamp(with_name)
  let enriched_with_host = DataEnricher::add_host_info(enriched_attributes)
  let enriched_with_version = DataEnricher::add_service_info(enriched_with_host, "test-service", "1.0.0")
  
  // Verify enrichment
  let timestamp = AttributeCollection::get(enriched_with_version, "timestamp")
  match timestamp {
    Some(IntValue(_)) => assert_true(true)
    _ => assert_true(false)
  }
  
  let host_name = AttributeCollection::get(enriched_with_version, "host.name")
  match host_name {
    Some(StringValue(_)) => assert_true(true)
    _ => assert_true(false)
  }
  
  let service_name = AttributeCollection::get(enriched_with_version, "service.name")
  match service_name {
    Some(StringValue(value)) => assert_eq(value, "test-service")
    _ => assert_true(false)
  }
  
  let service_version = AttributeCollection::get(enriched_with_version, "service.version")
  match service_version {
    Some(StringValue(value)) => assert_eq(value, "1.0.0")
    _ => assert_true(false)
  }
  
  // Test record enrichment
  let base_record = Record::new([("user_id", "12345"), ("action", "login")])
  
  let enriched_record = DataEnricher::enrich_record(base_record, [
    ("timestamp", Time::now().to_string()),
    ("session_id", "sess_abcdef123456"),
    ("ip_address", "192.168.1.100")
  ])
  
  assert_eq(Record::get_field(enriched_record, "user_id"), "12345")
  assert_eq(Record::get_field(enriched_record, "action"), "login")
  assert_eq(Record::get_field(enriched_record, "session_id"), "sess_abcdef123456")
  assert_eq(Record::get_field(enriched_record, "ip_address"), "192.168.1.100")
  
  // Test calculated enrichment
  let calculated_record = Record::new([
    ("width", "10"),
    ("height", "20"),
    ("depth", "5")
  ])
  
  let with_calculated = DataEnricher::add_calculated_field(
    calculated_record,
    "volume",
    |record| {
      match (Record::get_field_as_int(record, "width"), 
             Record::get_field_as_int(record, "height"), 
             Record::get_field_as_int(record, "depth")) {
        (Some(w), Some(h), Some(d)) => (w * h * d).to_string(),
        _ => "0"
      }
    }
  )
  
  assert_eq(Record::get_field(with_calculated, "volume"), "1000")
}

// Test 8: Data Deduplication Operations
test "data deduplication operations" {
  // Test array deduplication
  let array_with_duplicates = [1, 2, 3, 2, 4, 1, 5, 3]
  let deduplicated_array = DataDeduplicator::array(array_with_duplicates)
  
  assert_eq(deduplicated_array.length(), 5)
  assert_eq(deduplicated_array[0], 1)
  assert_eq(deduplicated_array[1], 2)
  assert_eq(deduplicated_array[2], 3)
  assert_eq(deduplicated_array[3], 4)
  assert_eq(deduplicated_array[4], 5)
  
  // Test string array deduplication
  let string_duplicates = ["apple", "banana", "apple", "cherry", "banana", "date"]
  let deduplicated_strings = DataDeduplicator::string_array(string_duplicates)
  
  assert_eq(deduplicated_strings.length(), 4)
  assert_eq(deduplicated_strings[0], "apple")
  assert_eq(deduplicated_strings[1], "banana")
  assert_eq(deduplicated_strings[2], "cherry")
  assert_eq(deduplicated_strings[3], "date")
  
  // Test record deduplication by key
  let records = [
    Record::new([("id", "1"), ("name", "John"), ("age", "30")]),
    Record::new([("id", "2"), ("name", "Jane"), ("age", "25")]),
    Record::new([("id", "1"), ("name", "Johnny"), ("age", "31")]), // Duplicate ID
    Record::new([("id", "3"), ("name", "Bob"), ("age", "35")]),
    Record::new([("id", "2"), ("name", "Janey"), ("age", "26")])  // Duplicate ID
  ]
  
  let deduplicated_records = DataDeduplicator::records_by_key(records, "id")
  
  assert_eq(deduplicated_records.length(), 3)
  
  // Verify we kept the first occurrence of each ID
  let record_ids = deduplicated_records.map(|r| Record::get_field(r, "id"))
  assert_eq(record_ids[0], "1")
  assert_eq(record_ids[1], "2")
  assert_eq(record_ids[2], "3")
  
  // Test attribute deduplication
  let attributes = AttributeCollection::new()
  let with_attr1 = AttributeCollection::add(attributes, Attribute::create("key1", "value1"))
  let with_attr2 = AttributeCollection::add(with_attr1, Attribute::create("key2", "value2"))
  let with_attr3 = AttributeCollection::add(with_attr2, Attribute::create("key1", "value3")) // Duplicate key
  let with_attr4 = AttributeCollection::add(with_attr3, Attribute::create("key3", "value3"))
  
  let deduplicated_attributes = DataDeduplicator::attributes(with_attr4)
  
  // Verify we have 3 unique keys
  let attr_keys = AttributeCollection::keys(deduplicated_attributes)
  assert_eq(attr_keys.length(), 3)
  
  // Verify the last value for key1 was kept
  let key1_value = AttributeCollection::get(deduplicated_attributes, "key1")
  match key1_value {
    Some(StringValue(value)) => assert_eq(value, "value3")
    _ => assert_true(false)
  }
}

// Test 9: Data Compression Operations
test "data compression operations" {
  // Test string compression
  let long_string = "This is a very long string that contains repetitive text. ".repeat(100)
  
  let compressed = DataCompressor::compress_string(long_string)
  let decompressed = DataCompressor::decompress_string(compressed)
  
  assert_true(compressed.length() < long_string.length())
  assert_eq(decompressed, long_string)
  
  // Test array compression
  let large_array = []
  for i = 0; i < 10000; i = i + 1 {
    large_array.push(i % 100) // Create repetitive pattern
  }
  
  let compressed_array = DataCompressor::compress_array(large_array)
  let decompressed_array = DataCompressor::decompress_array(compressed_array)
  
  assert_true(compressed_array.length() < large_array.length())
  assert_eq(decompressed_array.length(), large_array.length())
  
  // Test record compression
  let records = []
  for i = 0; i < 1000; i = i + 1 {
    let record = Record::new([
      ("id", i.to_string()),
      ("name", "user_" + i.to_string()),
      ("type", "standard_user"),
      ("status", "active")
    ])
    records.push(record)
  }
  
  let compressed_records = DataCompressor::compress_records(records)
  let decompressed_records = DataCompressor::decompress_records(compressed_records)
  
  assert_true(compressed_records.length() < records.length() * 100) // Rough estimate
  assert_eq(decompressed_records.length(), records.length())
  
  // Test attribute compression
  let attributes = AttributeCollection::new()
  let mut with_attrs = attributes
  
  for i = 0; i < 500; i = i + 1 {
    let attr = Attribute::create("attr_" + i.to_string(), "value_" + (i % 10).to_string())
    with_attrs = AttributeCollection::add(with_attrs, attr)
  }
  
  let compressed_attrs = DataCompressor::compress_attributes(with_attrs)
  let decompressed_attrs = DataCompressor::decompress_attributes(compressed_attrs)
  
  assert_true(compressed_attrs.length() > 0)
  assert_eq(AttributeCollection::size(decompressed_attrs), 500)
}

// Test 10: Data Serialization and Deserialization
test "data serialization and deserialization" {
  // Test JSON serialization
  let data = [
    ("name", "test"),
    ("value", 42),
    ("active", true),
    ("tags", ["tag1", "tag2", "tag3"])
  ]
  
  let json_serialized = DataSerializer::to_json(data)
  assert_true(json_serialized.contains("\"name\":\"test\""))
  assert_true(json_serialized.contains("\"value\":42"))
  assert_true(json_serialized.contains("\"active\":true"))
  assert_true(json_serialized.contains("\"tags\""))
  
  let json_deserialized = DataDeserializer::from_json(json_serialized)
  match json_deserialized {
    Some(map) => {
      let name = Map::get(map, "name")
      match name {
        Some(StringValue(value)) => assert_eq(value, "test"),
        _ => assert_true(false)
      }
      
      let value = Map::get(map, "value")
      match value {
        Some(IntValue(v)) => assert_eq(v, 42),
        _ => assert_true(false)
      }
      
      let active = Map::get(map, "active")
      match active {
        Some(BoolValue(v)) => assert_true(v),
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test binary serialization
  let binary_data = [1, 2, 3, 4, 5]
  let binary_serialized = DataSerializer::to_binary(binary_data)
  let binary_deserialized = DataDeserializer::from_binary(binary_serialized)
  
  assert_eq(binary_deserialized.length(), 5)
  assert_eq(binary_deserialized[0], 1)
  assert_eq(binary_deserialized[4], 5)
  
  // Test record serialization
  let record = Record::new([
    ("id", "12345"),
    ("name", "Test Record"),
    ("timestamp", "1640995200000"),
    ("metadata", "{\"key\":\"value\"}")
  ])
  
  let record_serialized = DataSerializer::record_to_string(record)
  let record_deserialized = DataDeserializer::record_from_string(record_serialized)
  
  assert_eq(Record::get_field(record_deserialized, "id"), "12345")
  assert_eq(Record::get_field(record_deserialized, "name"), "Test Record")
  assert_eq(Record::get_field(record_deserialized, "timestamp"), "1640995200000")
  assert_eq(Record::get_field(record_deserialized, "metadata"), "{\"key\":\"value\"}")
  
  // Test attribute collection serialization
  let attributes = AttributeCollection::new()
  let with_attr1 = AttributeCollection::add(attributes, Attribute::create("string.key", "string.value"))
  let with_attr2 = AttributeCollection::add(with_attr1, Attribute::create("int.key", 42))
  let with_attr3 = AttributeCollection::add(with_attr2, Attribute::create("bool.key", true))
  
  let attrs_serialized = DataSerializer::attributes_to_string(with_attr3)
  let attrs_deserialized = DataDeserializer::attributes_from_string(attrs_serialized)
  
  let string_value = AttributeCollection::get(attrs_deserialized, "string.key")
  match string_value {
    Some(StringValue(value)) => assert_eq(value, "string.value"),
    _ => assert_true(false)
  }
  
  let int_value = AttributeCollection::get(attrs_deserialized, "int.key")
  match int_value {
    Some(IntValue(value)) => assert_eq(value, 42),
    _ => assert_true(false)
  }
  
  let bool_value = AttributeCollection::get(attrs_deserialized, "bool.key")
  match bool_value {
    Some(BoolValue(value)) => assert_true(value),
    _ => assert_true(false)
  }
}