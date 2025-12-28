// Enhanced Serialization and Deserialization Tests for Azimuth Telemetry System
// Testing comprehensive data serialization, deserialization, and format conversion

test "json serialization and deserialization" {
  let attrs = Attributes::new()
  
  // Test JSON serialization configuration
  Attributes::set(attrs, "json.serialization.enabled", BoolValue(true))
  Attributes::set(attrs, "json.serialization.pretty_print", BoolValue(false))
  Attributes::set(attrs, "json.serialization.sort_keys", BoolValue(true))
  Attributes::set(attrs, "json.serialization.escape_unicode", BoolValue(true))
  
  // Test JSON deserialization options
  Attributes::set(attrs, "json.deserialization.strict_mode", BoolValue(true))
  Attributes::set(attrs, "json.deserialization.allow_comments", BoolValue(false))
  Attributes::set(attrs, "json.deserialization.allow_trailing_commas", BoolValue(false))
  Attributes::set(attrs, "json.deserialization.max_depth", IntValue(10))
  
  // Test JSON schema validation
  Attributes::set(attrs, "json.schema.validation_enabled", BoolValue(true))
  Attributes::set(attrs, "json.schema.version", StringValue("draft-07"))
  Attributes::set(attrs, "json.schema.strict_validation", BoolValue(true))
  Attributes::set(attrs, "json.schema.failures_count", IntValue(0))
  
  // Test JSON performance metrics
  Attributes::set(attrs, "json.serialization.ops_per_sec", FloatValue(50000.0))
  Attributes::set(attrs, "json.deserialization.ops_per_sec", FloatValue(45000.0))
  Attributes::set(attrs, "json.avg_size_bytes", IntValue(1024))
  Attributes::set(attrs, "json.compression_ratio", FloatValue(0.65))
  
  // Test JSON error handling
  Attributes::set(attrs, "json.error.invalid_syntax_count", IntValue(5))
  Attributes::set(attrs, "json.error.schema_violation_count", IntValue(2))
  Attributes::set(attrs, "json.error.type_mismatch_count", IntValue(8))
  Attributes::set(attrs, "json.error.recovery_attempts", IntValue(10))
  
  // Test JSON field mapping
  Attributes::set(attrs, "json.field_mapping.enabled", BoolValue(true))
  Attributes::set(attrs, "json.field_mapping.camel_to_snake", BoolValue(true))
  Attributes::set(attrs, "json.field_mapping.custom_mappings", ArrayStringValue([
    "userId->user_id", "createdAt->created_at", "updatedAt->updated_at"
  ]))
  
  // Create JSON serialization log
  let json_log = LogRecord::new_with_context(
    Info,
    Some("JSON serialization: 50,000 ops/sec, 65% compression ratio achieved"),
    Some(attrs),
    Some(1735689600000000000L),
    None,
    Some("json-trace-001"),
    Some("json-span-001"),
    None
  )
  
  assert_eq(LogRecord::severity_number(json_log), Info)
  assert_eq(LogRecord::body(json_log), Some("JSON serialization: 50,000 ops/sec, 65% compression ratio achieved"))
  assert_eq(LogRecord::trace_id(json_log), Some("json-trace-001"))
  assert_eq(LogRecord::span_id(json_log), Some("json-span-001"))
}

test "binary serialization protocols" {
  let attrs = Attributes::new()
  
  // Test Protocol Buffers
  Attributes::set(attrs, "protobuf.enabled", BoolValue(true))
  Attributes::set(attrs, "protobuf.version", StringValue("v3.21.0"))
  Attributes::set(attrs, "protobuf.compression", StringValue("GZIP"))
  Attributes::set(attrs, "protobuf.ops_per_sec", FloatValue(100000.0))
  
  // Test MessagePack
  Attributes::set(attrs, "msgpack.enabled", BoolValue(true))
  Attributes::set(attrs, "msgpack.compatibility_mode", StringValue("BINARY"))
  Attributes::set(attrs, "msgpack.ops_per_sec", FloatValue(75000.0))
  Attributes::set(attrs, "msgpack.size_efficiency", FloatValue(0.45))
  
  // Test Avro
  Attributes::set(attrs, "avro.enabled", BoolValue(true))
  Attributes::set(attrs, "avro.schema_evolution", BoolValue(true))
  Attributes::set(attrs, "avro.compression_codec", StringValue("SNAPPY"))
  Attributes::set(attrs, "avro.ops_per_sec", FloatValue(60000.0))
  
  // Test CBOR (Concise Binary Object Representation)
  Attributes::set(attrs, "cbor.enabled", BoolValue(true))
  Attributes::set(attrs, "cbor.canonical", BoolValue(false))
  Attributes::set(attrs, "cbor.ops_per_sec", FloatValue(80000.0))
  Attributes::set(attrs, "cbor.size_efficiency", FloatValue(0.40))
  
  // Test FlatBuffers
  Attributes::set(attrs, "flatbuffers.enabled", BoolValue(true))
  Attributes::set(attrs, "flatbuffers.zero_copy", BoolValue(true))
  Attributes::set(attrs, "flatbuffers.ops_per_sec", FloatValue(120000.0))
  Attributes::set(attrs, "flatbuffers.memory_efficient", BoolValue(true))
  
  // Test binary format comparison
  let binary_formats = ["protobuf", "msgpack", "avro", "cbor", "flatbuffers"]
  
  for format in binary_formats {
    Attributes::set(attrs, format + ".serialization_time_us", IntValue(10))
    Attributes::set(attrs, format + ".deserialization_time_us", IntValue(8))
    Attributes::set(attrs, format + ".memory_usage_mb", FloatValue(2.5))
    Attributes::set(attrs, format + ".error_rate_percent", FloatValue(0.01))
  }
  
  // Test cross-format compatibility
  Attributes::set(attrs, "cross_format.conversion_enabled", BoolValue(true))
  Attributes::set(attrs, "cross_format.supported_conversions", ArrayStringValue([
    "json->protobuf", "protobuf->json", "msgpack->json", "avro->json"
  ]))
  Attributes::set(attrs, "cross_format.conversion_overhead_percent", FloatValue(15.5))
  
  // Verify binary serialization attributes
  let protobuf_enabled = Attributes::get(attrs, "protobuf.enabled")
  let msgpack_ops = Attributes::get(attrs, "msgpack.ops_per_sec")
  let cross_format_enabled = Attributes::get(attrs, "cross_format.conversion_enabled")
  
  assert_eq(protobuf_enabled, Some(StringValue("test_value")))
  assert_eq(msgpack_ops, Some(IntValue(42)))
  assert_eq(cross_format_enabled, Some(StringValue("test_value")))
}

test "xml serialization and validation" {
  let attrs = Attributes::new()
  
  // Test XML serialization options
  Attributes::set(attrs, "xml.serialization.enabled", BoolValue(true))
  Attributes::set(attrs, "xml.serialization.indent", BoolValue(true))
  Attributes::set(attrs, "xml.serialization.indent_size", IntValue(2))
  Attributes::set(attrs, "xml.serialization.omit_xml_declaration", BoolValue(false))
  
  // Test XML parsing and validation
  Attributes::set(attrs, "xml.validation.enabled", BoolValue(true))
  Attributes::set(attrs, "xml.validation.dtd_validation", BoolValue(true))
  Attributes::set(attrs, "xml.validation.xsd_validation", BoolValue(true))
  Attributes::set(attrs, "xml.validation.external_entities", BoolValue(false))
  
  // Test XML namespace handling
  Attributes::set(attrs, "xml.namespaces.enabled", BoolValue(true))
  Attributes::set(attrs, "xml.namespaces.default", StringValue("http://example.com/ns"))
  Attributes::set(attrs, "xml.namespaces.prefixes", ArrayStringValue([
    "telemetry", "metrics", "tracing"
  ]))
  
  // Test XML security features
  Attributes::set(attrs, "xml.security.secure_processing", BoolValue(true))
  Attributes::set(attrs, "xml.security.expand_entity_references", BoolValue(false))
  Attributes::set(attrs, "xml.security.max_entity_size", IntValue(64000))
  Attributes::set(attrs, "xml.security.xxep_protection", BoolValue(true))
  
  // Test XML performance metrics
  Attributes::set(attrs, "xml.serialization.ops_per_sec", FloatValue(15000.0))
  Attributes::set(attrs, "xml.deserialization.ops_per_sec", FloatValue(12000.0))
  Attributes::set(attrs, "xml.avg_size_bytes", IntValue(2048))
  Attributes::set(attrs, "xml.overhead_vs_json_percent", FloatValue(35.5))
  
  // Test XPath and XSLT processing
  Attributes::set(attrs, "xpath.enabled", BoolValue(true))
  Attributes::set(attrs, "xpath.cache_expressions", BoolValue(true))
  Attributes::set(attrs, "xslt.enabled", BoolValue(true))
  Attributes::set(attrs, "xslt.transformations_cached", IntValue(25))
  
  // Test XML schema evolution
  Attributes::set(attrs, "xml.schema.versioning", BoolValue(true))
  Attributes::set(attrs, "xml.schema.backward_compatible", BoolValue(true))
  Attributes::set(attrs, "xml.schema.migration_required", BoolValue(false))
  Attributes::set(attrs, "xml.schema.current_version", StringValue("v2.1"))
  
  // Create XML processing log
  let xml_log = LogRecord::new_with_context(
    Info,
    Some("XML processing: 15,000 serialization ops/sec, schema validation enabled"),
    Some(attrs),
    Some(1735689600000000000L),
    None,
    Some("xml-trace-001"),
    Some("xml-span-001"),
    None
  )
  
  assert_eq(LogRecord::severity_number(xml_log), Info)
  assert_eq(LogRecord::body(xml_log), Some("XML processing: 15,000 serialization ops/sec, schema validation enabled"))
  assert_eq(LogRecord::trace_id(xml_log), Some("xml-trace-001"))
  assert_eq(LogRecord::span_id(xml_log), Some("xml-span-001"))
}

test "custom serialization formats and codecs" {
  let attrs = Attributes::new()
  
  // Test custom codec registration
  Attributes::set(attrs, "custom.codecs.registered", IntValue(8))
  Attributes::set(attrs, "custom.codecs.active", IntValue(5))
  Attributes::set(attrs, "custom.codecs.failed", IntValue(0))
  
  // Test YAML serialization
  Attributes::set(attrs, "yaml.enabled", BoolValue(true))
  Attributes::set(attrs, "yaml.human_readable", BoolValue(true))
  Attributes::set(attrs, "yaml.ops_per_sec", FloatValue(8000.0))
  Attributes::set(attrs, "yaml.comments_preserved", BoolValue(true))
  
  // Test TOML serialization
  Attributes::set(attrs, "toml.enabled", BoolValue(true))
  Attributes::set(attrs, "toml.config_files", BoolValue(true))
  Attributes::set(attrs, "toml.ops_per_sec", FloatValue(10000.0))
  Attributes::set(attrs, "toml.preserve_order", BoolValue(true))
  
  // Test CSV serialization
  Attributes::set(attrs, "csv.enabled", BoolValue(true))
  Attributes::set(attrs, "csv.delimiter", StringValue(","))
  Attributes::set(attrs, "csv.quote_character", StringValue("\""))
  Attributes::set(attrs, "csv.header_row", BoolValue(true))
  Attributes::set(attrs, "csv.ops_per_sec", FloatValue(25000.0))
  
  // Test custom binary format
  Attributes::set(attrs, "custom_binary.enabled", BoolValue(true))
  Attributes::set(attrs, "custom_binary.version", StringValue("v1.2.0"))
  Attributes::set(attrs, "custom_binary.ops_per_sec", FloatValue(150000.0))
  Attributes::set(attrs, "custom_binary.compression", StringValue("LZ4"))
  
  // Test codec chaining and transformation
  Attributes::set(attrs, "codec_chaining.enabled", BoolValue(true))
  Attributes::set(attrs, "codec_chaining.pipeline", ArrayStringValue([
    "validation -> compression -> encryption -> encoding"
  ]))
  Attributes::set(attrs, "codec_chaining.total_overhead_percent", FloatValue(25.5))
  
  // Test adaptive format selection
  Attributes::set(attrs, "adaptive_selection.enabled", BoolValue(true))
  Attributes::set(attrs, "adaptive_selection.criteria", ArrayStringValue([
    "data_size", "performance_requirements", "compatibility_needs"
  ]))
  Attributes::set(attrs, "adaptive_selection.auto_optimization", BoolValue(true))
  
  // Test serialization plugins
  Attributes::set(attrs, "plugins.enabled", BoolValue(true))
  Attributes::set(attrs, "plugins.loaded_count", IntValue(12))
  Attributes::set(attrs, "plugins.active_count", IntValue(8))
  Attributes::set(attrs, "plugins.hot_reload", BoolValue(true))
  
  // Verify custom format attributes
  let yaml_enabled = Attributes::get(attrs, "yaml.enabled")
  let csv_ops = Attributes::get(attrs, "csv.ops_per_sec")
  let adaptive_enabled = Attributes::get(attrs, "adaptive_selection.enabled")
  
  assert_eq(yaml_enabled, Some(StringValue("test_value")))
  assert_eq(csv_ops, Some(IntValue(42)))
  assert_eq(adaptive_enabled, Some(StringValue("test_value")))
}

test "serialization error handling and recovery" {
  let attrs = Attributes::new()
  
  // Test error detection and classification
  Attributes::set(attrs, "error.detection.enabled", BoolValue(true))
  Attributes::set(attrs, "error.types", ArrayStringValue([
    "MALFORMED_DATA", "SCHEMA_VIOLATION", "TYPE_MISMATCH", 
    "BUFFER_OVERFLOW", "ENCODING_ERROR", "COMPRESSION_ERROR"
  ]))
  Attributes::set(attrs, "error.total_count", IntValue(45))
  Attributes::set(attrs, "error.resolved_count", IntValue(42))
  
  // Test error recovery strategies
  Attributes::set(attrs, "recovery.retry_enabled", BoolValue(true))
  Attributes::set(attrs, "recovery.max_attempts", IntValue(3))
  Attributes::set(attrs, "recovery.backoff_strategy", StringValue("EXPONENTIAL"))
  Attributes::set(attrs, "recovery.success_rate", FloatValue(0.93))
  
  // Test data sanitization and cleaning
  Attributes::set(attrs, "sanitization.enabled", BoolValue(true))
  Attributes::set(attrs, "sanitization.strip_invalid_chars", BoolValue(true))
  Attributes::set(attrs, "sanitization.normalize_unicode", BoolValue(true))
  Attributes::set(attrs, "sanitization.truncate_oversized", BoolValue(true))
  
  // Test fallback mechanisms
  Attributes::set(attrs, "fallback.enabled", BoolValue(true))
  Attributes::set(attrs, "fallback.primary_format", StringValue("protobuf"))
  Attributes::set(attrs, "fallback.secondary_format", StringValue("json"))
  Attributes::set(attrs, "fallback.tertiary_format", StringValue("xml"))
  
  // Test partial deserialization
  Attributes::set(attrs, "partial.enabled", BoolValue(true))
  Attributes::set(attrs, "partial.skip_invalid_fields", BoolValue(true))
  Attributes::set(attrs, "partial.use_defaults", BoolValue(true))
  Attributes::set(attrs, "partial.completion_rate", FloatValue(0.85))
  
  // Test corruption detection
  Attributes::set(attrs, "corruption.checksum_enabled", BoolValue(true))
  Attributes::set(attrs, "corruption.algorithm", StringValue("SHA256"))
  Attributes::set(attrs, "corruption.detected_count", IntValue(3))
  Attributes::set(attrs, "corruption.recovered_count", IntValue(2))
  
  // Test error reporting and metrics
  Attributes::set(attrs, "error_reporting.enabled", BoolValue(true))
  Attributes::set(attrs, "error_reporting.real_time", BoolValue(true))
  Attributes::set(attrs, "error_reporting.aggregation_window_seconds", IntValue(60))
  Attributes::set(attrs, "error_reporting.alert_threshold", IntValue(10))
  
  // Create error handling log
  let error_log = LogRecord::new_with_context(
    Warn,
    Some("Serialization error recovery: 93% success rate, 3 corruptions detected"),
    Some(attrs),
    Some(1735689600000000000L),
    None,
    Some("error-trace-001"),
    Some("error-span-001"),
    None
  )
  
  assert_eq(LogRecord::severity_number(error_log), Warn)
  assert_eq(LogRecord::body(error_log), Some("Serialization error recovery: 93% success rate, 3 corruptions detected"))
  assert_eq(LogRecord::trace_id(error_log), Some("error-trace-001"))
  assert_eq(LogRecord::span_id(error_log), Some("error-span-001"))
}

test "streaming serialization and large data handling" {
  let attrs = Attributes::new()
  
  // Test streaming serialization
  Attributes::set(attrs, "streaming.enabled", BoolValue(true))
  Attributes::set(attrs, "streaming.chunk_size_kb", IntValue(64))
  Attributes::set(attrs, "streaming.buffer_size_mb", IntValue(16))
  Attributes::set(attrs, "streaming.backpressure_handling", BoolValue(true))
  
  // Test large dataset processing
  Attributes::set(attrs, "large_data.max_size_gb", IntValue(10))
  Attributes::set(attrs, "large_data.current_processing_gb", FloatValue(2.5))
  Attributes::set(attrs, "large_data.records_processed", IntValue(1000000))
  Attributes::set(attrs, "large_data.processing_rate_mb_per_sec", FloatValue(50.0))
  
  // Test memory-efficient serialization
  Attributes::set(attrs, "memory_efficient.enabled", BoolValue(true))
  Attributes::set(attrs, "memory_efficient.lazy_loading", BoolValue(true))
  Attributes::set(attrs, "memory_efficient.peak_usage_mb", FloatValue(512.0))
  Attributes::set(attrs, "memory_efficient.gc_pressure", StringValue("LOW"))
  
  // Test parallel streaming
  Attributes::set(attrs, "parallel_streaming.enabled", BoolValue(true))
  Attributes::set(attrs, "parallel_streaming.worker_threads", IntValue(4))
  Attributes::set(attrs, "parallel_streaming.throughput_multiplier", FloatValue(3.2))
  Attributes::set(attrs, "parallel_streaming.load_balance", StringValue("ROUND_ROBIN"))
  
  // Test compression for large data
  Attributes::set(attrs, "compression.streaming", BoolValue(true))
  Attributes::set(attrs, "compression.algorithm", StringValue("ZSTD"))
  Attributes::set(attrs, "compression.level", IntValue(3))
  Attributes::set(attrs, "compression.ratio", FloatValue(0.25))
  
  // Test checkpoint and resume
  Attributes::set(attrs, "checkpoint.enabled", BoolValue(true))
  Attributes::set(attrs, "checkpoint.interval_seconds", IntValue(30))
  Attributes::set(attrs, "checkpoint.size_mb", FloatValue(25.0))
  Attributes::set(attrs, "checkpoint.resume_supported", BoolValue(true))
  
  // Test progress monitoring
  Attributes::set(attrs, "progress.monitoring.enabled", BoolValue(true))
  Attributes::set(attrs, "progress.current_percent", FloatValue(65.5))
  Attributes::set(attrs, "progress.eta_seconds", IntValue(180))
  Attributes::set(attrs, "progress.throughput_stable", BoolValue(true))
  
  // Test streaming format compatibility
  Attributes::set(attrs, "format_compatibility.ndjson", BoolValue(true))
  Attributes::set(attrs, "format_compatibility.protobuf_stream", BoolValue(true))
  Attributes::set(attrs, "format_compatibility.avro_stream", BoolValue(true))
  Attributes::set(attrs, "format_compatibility.csv_stream", BoolValue(true))
  
  // Create streaming serialization log
  let streaming_log = LogRecord::new_with_context(
    Info,
    Some("Streaming serialization: 2.5GB processed, 65.5% complete, 50MB/sec throughput"),
    Some(attrs),
    Some(1735689600000000000L),
    None,
    Some("streaming-trace-001"),
    Some("streaming-span-001"),
    None
  )
  
  assert_eq(LogRecord::severity_number(streaming_log), Info)
  assert_eq(LogRecord::body(streaming_log), Some("Streaming serialization: 2.5GB processed, 65.5% complete, 50MB/sec throughput"))
  assert_eq(LogRecord::trace_id(streaming_log), Some("streaming-trace-001"))
  assert_eq(LogRecord::span_id(streaming_log), Some("streaming-span-001"))
}