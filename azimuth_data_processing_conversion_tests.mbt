// Azimuth Data Processing and Conversion Test Suite
// This file contains comprehensive test cases for data processing and conversion functionality

// Test 1: Attribute Value Type Conversion
test "attribute value type conversion" {
  // Test string to int conversion
  let string_attr = StringValue("42")
  let int_result = AttributeValue::to_int(string_attr)
  match int_result {
    Ok(value) => assert_eq(value, 42)
    Err(_) => assert_true(false)
  }
  
  // Test int to string conversion
  let int_attr = IntValue(42)
  let string_result = AttributeValue::to_string(int_attr)
  match string_result {
    Ok(value) => assert_eq(value, "42")
    Err(_) => assert_true(false)
  }
  
  // Test string to float conversion
  let float_string = StringValue("3.14")
  let float_result = AttributeValue::to_float(float_string)
  match float_result {
    Ok(value) => assert_eq(value, 3.14)
    Err(_) => assert_true(false)
  }
  
  // Test float to string conversion
  let float_attr = FloatValue(3.14)
  let float_string_result = AttributeValue::to_string(float_attr)
  match float_string_result {
    Ok(value) => assert_eq(value, "3.14")
    Err(_) => assert_true(false)
  }
  
  // Test boolean conversion
  let bool_attr = BoolValue(true)
  let bool_string_result = AttributeValue::to_string(bool_attr)
  match bool_string_result {
    Ok(value) => assert_eq(value, "true")
    Err(_) => assert_true(false)
  }
  
  // Test array conversion
  let array_int = ArrayIntValue([1, 2, 3])
  let array_string_result = AttributeValue::to_string(array_int)
  match array_string_result {
    Ok(value) => assert_true(value.contains("[1, 2, 3]"))
    Err(_) => assert_true(false)
  }
}

// Test 2: Data Structure Transformations
test "data structure transformations" {
  // Test map to array transformation
  let data_map = [
    ("key1", StringValue("value1")),
    ("key2", IntValue(42)),
    ("key3", BoolValue(true))
  ]
  
  let transformed_array = DataProcessor::map_to_array(data_map)
  assert_eq(transformed_array.length(), 3)
  
  // Test array filtering
  let mixed_values = [
    StringValue("test"),
    IntValue(42),
    FloatValue(3.14),
    BoolValue(true)
  ]
  
  let string_values = DataProcessor::filter_by_type(mixed_values, "string")
  assert_eq(string_values.length(), 1)
  
  let numeric_values = DataProcessor::filter_numeric(mixed_values)
  assert_eq(numeric_values.length(), 2)
  
  // Test data aggregation
  let numbers = [IntValue(1), IntValue(2), IntValue(3), IntValue(4), IntValue(5)]
  let sum = DataProcessor::aggregate_ints(numbers, "sum")
  match sum {
    IntValue(total) => assert_eq(total, 15)
    _ => assert_true(false)
  }
  
  let average = DataProcessor::aggregate_ints(numbers, "average")
  match average {
    FloatValue(avg) => assert_eq(avg, 3.0)
    _ => assert_true(false)
  }
}

// Test 3: Time Series Data Processing
test "time series data processing" {
  // Create time series data points
  let data_points = [
    TimeSeriesPoint::new(1000L, 10.5),
    TimeSeriesPoint::new(2000L, 15.2),
    TimeSeriesPoint::new(3000L, 12.8),
    TimeSeriesPoint::new(4000L, 18.3),
    TimeSeriesPoint::new(5000L, 20.1)
  ]
  
  // Test time series aggregation
  let aggregated = TimeSeriesProcessor::aggregate(data_points, 2000L, "average")
  assert_eq(aggregated.length(), 3)
  
  // Test time series filtering
  let filtered = TimeSeriesProcessor::filter_by_time_range(data_points, 2000L, 4000L)
  assert_eq(filtered.length(), 3)
  
  // Test time series interpolation
  let interpolated = TimeSeriesProcessor::interpolate(data_points, 2500L)
  match interpolated {
    Some(point) => {
      assert_eq(TimeSeriesPoint::timestamp(point), 2500L)
      assert_true(TimeSeriesPoint::value(point) > 12.8 && TimeSeriesPoint::value(point) < 15.2)
    }
    None => assert_true(false)
  }
  
  // Test time series smoothing
  let smoothed = TimeSeriesProcessor::smooth(data_points, "moving_average", 3)
  assert_eq(smoothed.length(), 5)
  assert_true(TimeSeriesPoint::value(smoothed[1]) > 12.0 && TimeSeriesPoint::value(smoothed[1]) < 16.0)
}

// Test 4: Data Validation and Sanitization
test "data validation and sanitization" {
  // Test string validation
  let valid_string = "valid_string_123"
  let invalid_string = "invalid@string#with$special%chars"
  
  assert_true(DataValidator::is_valid_string(valid_string))
  assert_false(DataValidator::is_valid_string(invalid_string))
  
  // Test numeric validation
  let valid_int = "42"
  let invalid_int = "not_a_number"
  
  assert_true(DataValidator::is_valid_int(valid_int))
  assert_false(DataValidator::is_valid_int(invalid_int))
  
  // Test email validation
  let valid_email = "test@example.com"
  let invalid_email = "invalid.email"
  
  assert_true(DataValidator::is_valid_email(valid_email))
  assert_false(DataValidator::is_valid_email(invalid_email))
  
  // Test data sanitization
  let dirty_string = "<script>alert('xss')</script>"
  let clean_string = DataSanitizer::sanitize_html(dirty_string)
  assert_false(clean_string.contains("<script>"))
  assert_false(clean_string.contains("alert"))
  
  // Test SQL injection prevention
  let sql_input = "'; DROP TABLE users; --"
  let safe_input = DataSanitizer::sanitize_sql(sql_input)
  assert_false(safe_input.contains("DROP TABLE"))
  assert_false(safe_input.contains("--"))
}

// Test 5: Data Format Conversion
test "data format conversion" {
  // Test JSON to attributes conversion
  let json_string = "{\"name\":\"test\",\"value\":42,\"active\":true}"
  let attributes = DataConverter::json_to_attributes(json_string)
  
  let name_value = Attributes::get(attributes, "name")
  match name_value {
    Some(StringValue(v)) => assert_eq(v, "test")
    _ => assert_true(false)
  }
  
  let value_attr = Attributes::get(attributes, "value")
  match value_attr {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  let active_attr = Attributes::get(attributes, "active")
  match active_attr {
    Some(BoolValue(v)) => assert_true(v)
    _ => assert_true(false)
  }
  
  // Test attributes to JSON conversion
  let attrs = Attributes::new()
  Attributes::set(attrs, "string_key", StringValue("test_value"))
  Attributes::set(attrs, "int_key", IntValue(123))
  Attributes::set(attrs, "bool_key", BoolValue(false))
  
  let json_result = DataConverter::attributes_to_json(attrs)
  assert_true(json_result.contains("string_key"))
  assert_true(json_result.contains("test_value"))
  assert_true(json_result.contains("int_key"))
  assert_true(json_result.contains("123"))
  assert_true(json_result.contains("bool_key"))
  assert_true(json_result.contains("false"))
  
  // Test CSV parsing
  let csv_data = "name,age,city\nJohn,30,New York\nJane,25,Los Angeles"
  let parsed_data = DataConverter::csv_to_array(csv_data)
  assert_eq(parsed_data.length(), 3) // Header + 2 rows
  assert_eq(parsed_data[0].length(), 3) // 3 columns
}

// Test 6: Data Compression and Decompression
test "data compression and decompression" {
  // Test string compression
  let original_string = "This is a test string that should be compressed to reduce its size"
  let compressed = DataCompressor::compress_string(original_string, "gzip")
  assert_true(compressed.length() < original_string.length())
  
  // Test string decompression
  let decompressed = DataCompressor::decompress_string(compressed, "gzip")
  assert_eq(decompressed, original_string)
  
  // Test binary data compression
  let binary_data = [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08]
  let compressed_binary = DataCompressor::compress_binary(binary_data, "lz4")
  assert_true(compressed_binary.length() <= binary_data.length())
  
  // Test binary data decompression
  let decompressed_binary = DataCompressor::decompress_binary(compressed_binary, "lz4")
  assert_eq(decompressed_binary, binary_data)
  
  // Test compression ratio calculation
  let ratio = DataCompressor::calculate_compression_ratio(original_string.length(), compressed.length())
  assert_true(ratio > 0.0 && ratio < 1.0)
}

// Test 7: Data Encoding and Decoding
test "data encoding and decoding" {
  // Test base64 encoding
  let original_data = "Hello, World!"
  let encoded = DataEncoder::base64_encode(original_data)
  assert_eq(encoded, "SGVsbG8sIFdvcmxkIQ==")
  
  // Test base64 decoding
  let decoded = DataEncoder::base64_decode(encoded)
  assert_eq(decoded, original_data)
  
  // Test URL encoding
  let url_string = "hello world?param=value&other=test"
  let url_encoded = DataEncoder::url_encode(url_string)
  assert_true(url_encoded.contains("%20"))
  assert_true(url_encoded.contains("%3F"))
  assert_true(url_encoded.contains("%3D"))
  assert_true(url_encoded.contains("%26"))
  
  // Test URL decoding
  let url_decoded = DataEncoder::url_decode(url_encoded)
  assert_eq(url_decoded, url_string)
  
  // Test hex encoding
  let hex_data = [0x48, 0x65, 0x6C, 0x6C, 0x6F] // "Hello" in ASCII
  let hex_encoded = DataEncoder::hex_encode(hex_data)
  assert_eq(hex_encoded, "48656C6C6F")
  
  // Test hex decoding
  let hex_decoded = DataEncoder::hex_decode(hex_encoded)
  assert_eq(hex_decoded, hex_data)
}

// Test 8: Data Aggregation and Statistics
test "data aggregation and statistics" {
  // Test numeric statistics
  let numbers = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
  
  let mean = Statistics::calculate_mean(numbers)
  assert_eq(mean, 5.5)
  
  let median = Statistics::calculate_median(numbers)
  assert_eq(median, 5.5)
  
  let mode = Statistics::calculate_mode([1, 2, 2, 3, 3, 3, 4, 4, 5])
  assert_eq(mode, 3)
  
  let std_dev = Statistics::calculate_std_dev(numbers)
  assert_true(std_dev > 2.8 && std_dev < 3.2)
  
  let variance = Statistics::calculate_variance(numbers)
  assert_true(variance > 8.0 && variance < 10.0)
  
  // Test percentile calculation
  let p90 = Statistics::calculate_percentile(numbers, 90.0)
  assert_eq(p90, 9.0)
  
  let p25 = Statistics::calculate_percentile(numbers, 25.0)
  assert_eq(p25, 3.0)
  
  // Test histogram generation
  let histogram = Statistics::generate_histogram(numbers, 5)
  assert_eq(histogram.length(), 5)
  assert_true(histogram[0].count > 0)
}

// Test 9: Data Stream Processing
test "data stream processing" {
  // Create a data stream
  let stream = DataStream::new()
  
  // Test stream filtering
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let filtered_stream = DataStream::filter(stream, numbers, fn(x) { x % 2 == 0 })
  let filtered_result = DataStream::collect(filtered_stream)
  assert_eq(filtered_result, [2, 4, 6, 8, 10])
  
  // Test stream mapping
  let mapped_stream = DataStream::map(stream, numbers, fn(x) { x * 2 })
  let mapped_result = DataStream::collect(mapped_stream)
  assert_eq(mapped_result, [2, 4, 6, 8, 10, 12, 14, 16, 18, 20])
  
  // Test stream reduction
  let sum = DataStream::reduce(stream, numbers, fn(acc, x) { acc + x }, 0)
  assert_eq(sum, 55)
  
  // Test stream windowing
  let windowed = DataStream::window(stream, numbers, 3)
  assert_eq(windowed.length(), 8) // 10 elements with window size 3
  assert_eq(windowed[0], [1, 2, 3])
  assert_eq(windowed[7], [8, 9, 10])
  
  // Test stream grouping
  let grouped = DataStream::group_by(stream, numbers, fn(x) { x % 3 })
  assert_eq(grouped.length(), 3)
  assert_eq(grouped[1], [1, 4, 7, 10])
  assert_eq(grouped[2], [2, 5, 8])
  assert_eq(grouped[0], [3, 6, 9])
}

// Test 10: Data Transformation Pipeline
test "data transformation pipeline" {
  // Create a transformation pipeline
  let pipeline = DataPipeline::new()
  
  // Add pipeline stages
  DataPipeline::add_stage(pipeline, "validation", fn(data) {
    match data {
      StringValue(s) => DataValidator::is_valid_string(s) ? Some(data) : None
      _ => Some(data)
    }
  })
  
  DataPipeline::add_stage(pipeline, "normalization", fn(data) {
    match data {
      StringValue(s) => Some(StringValue(s.to_lowercase()))
      IntValue(i) => Some(IntValue(i * 2))
      _ => Some(data)
    }
  })
  
  DataPipeline::add_stage(pipeline, "enrichment", fn(data) {
    match data {
      StringValue(s) => {
        let enriched = Attributes::new()
        Attributes::set(enriched, "original", data)
        Attributes::set(enriched, "length", IntValue(s.length()))
        Some(StringValue(s + "_enriched"))
      }
      _ => Some(data)
    }
  })
  
  // Test pipeline with valid data
  let valid_data = StringValue("ValidData")
  let result = DataPipeline::process(pipeline, valid_data)
  match result {
    Some(StringValue(processed)) => {
      assert_eq(processed, "validdata_enriched")
    }
    _ => assert_true(false)
  }
  
  // Test pipeline with invalid data
  let invalid_data = StringValue("Invalid@Data!")
  let invalid_result = DataPipeline::process(pipeline, invalid_data)
  match invalid_result {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Test pipeline with numeric data
  let numeric_data = IntValue(21)
  let numeric_result = DataPipeline::process(pipeline, numeric_data)
  match numeric_result {
    Some(IntValue(processed)) => assert_eq(processed, 42)
    _ => assert_true(false)
  }
}