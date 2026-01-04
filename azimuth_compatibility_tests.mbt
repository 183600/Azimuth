// Azimuth Compatibility Tests
// This file contains test cases for compatibility with different systems and versions

test "version compatibility" {
  // Test compatibility with different versions
  let current_version = Version::parse("1.2.3")
  let compatible_version = Version::parse("1.2.0")
  let incompatible_version = Version::parse("2.0.0")
  
  // Verify version compatibility
  assert_true(Version::is_compatible(current_version, compatible_version))
  assert_false(Version::is_compatible(current_version, incompatible_version))
  
  // Test telemetry data version compatibility
  let telemetry_data = TelemetryData::with_version(current_version)
  
  // Serialize with current version
  let serialized = TelemetryData::serialize(telemetry_data)
  
  // Try to deserialize with compatible version
  let compatible_result = TelemetryData::deserialize_with_version(serialized, compatible_version)
  match compatible_result {
    Success(_) => assert_true(true)
    _ => assert_true(false)
  }
  
  // Try to deserialize with incompatible version
  let incompatible_result = TelemetryData::deserialize_with_version(serialized, incompatible_version)
  match incompatible_result {
    VersionError(_) => assert_true(true)
    _ => assert_true(false)
  }
}

test "cross-platform compatibility" {
  // Create telemetry data
  let span_ctx = SpanContext::new("cross_platform_trace", "cross_platform_span", true, "")
  let span = Span::new("cross_platform_test", Internal, span_ctx)
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "platform.test", StringValue("cross_platform_value"))
  Attributes::set(attrs, "platform.os", StringValue(System::get_os()))
  Attributes::set(attrs, "platform.arch", StringValue(System::get_architecture()))
  
  Span::add_event(span, "cross_platform_event", Some(attrs))
  
  // Serialize in platform-independent format
  let platform_independent_serialized = CrossPlatformSerializer::serialize(span)
  
  // Simulate different platforms
  let platforms = ["linux", "windows", "macos", "freebsd"]
  
  for platform in platforms {
    // Deserialize on different platform
    let deserialized = CrossPlatformSerializer::deserialize(platform_independent_serialized, platform)
    
    // Verify compatibility
    assert_true(Span::is_valid(deserialized))
    assert_eq(Span::name(deserialized), "cross_platform_test")
    
    let ctx = Span::span_context(deserialized)
    assert_eq(SpanContext::trace_id(ctx), "cross_platform_trace")
    assert_eq(SpanContext::span_id(ctx), "cross_platform_span")
    
    // Verify platform-specific attributes are handled correctly
    let platform_attr = Attributes::get(Span::attributes(deserialized), "platform.os")
    match platform_attr {
      StringValue(os) => assert_eq(os, System::get_os()) // Should be original OS
      _ => assert_true(false)
    }
  }
}

test "format compatibility" {
  // Create test data
  let span_ctx = SpanContext::new("format_trace", "format_span", true, "")
  let span = Span::new("format_test", Internal, span_ctx)
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "format.test", StringValue("format_value"))
  Attributes::set(attrs, "format.number", IntValue(42))
  Attributes::set(attrs, "format.float", FloatValue(3.14))
  
  Span::add_event(span, "format_event", Some(attrs))
  
  // Test JSON format compatibility
  let json_serialized = JsonSerializer::serialize(span)
  let json_deserialized = JsonSerializer::deserialize(json_serialized)
  
  assert_true(Span::is_valid(json_deserialized))
  assert_eq(Span::name(json_deserialized), "format_test")
  
  // Test XML format compatibility
  let xml_serialized = XmlSerializer::serialize(span)
  let xml_deserialized = XmlSerializer::deserialize(xml_serialized)
  
  assert_true(Span::is_valid(xml_deserialized))
  assert_eq(Span::name(xml_deserialized), "format_test")
  
  // Test binary format compatibility
  let binary_serialized = BinarySerializer::serialize(span)
  let binary_deserialized = BinarySerializer::deserialize(binary_serialized)
  
  assert_true(Span::is_valid(binary_deserialized))
  assert_eq(Span::name(binary_deserialized), "format_test")
  
  // Verify all formats produce equivalent results
  assert_eq(Span::name(json_deserialized), Span::name(xml_deserialized))
  assert_eq(Span::name(xml_deserialized), Span::name(binary_deserialized))
  
  let json_ctx = Span::span_context(json_deserialized)
  let xml_ctx = Span::span_context(xml_deserialized)
  let binary_ctx = Span::span_context(binary_deserialized)
  
  assert_eq(SpanContext::trace_id(json_ctx), SpanContext::trace_id(xml_ctx))
  assert_eq(SpanContext::trace_id(xml_ctx), SpanContext::trace_id(binary_ctx))
}

test "protocol compatibility" {
  // Create telemetry data for different protocols
  let span_ctx = SpanContext::new("protocol_trace", "protocol_span", true, "")
  let span = Span::new("protocol_test", Internal, span_ctx)
  
  // Test HTTP/HTTPS protocol compatibility
  let http_data = HttpProtocolAdapter::adapt(span)
  assert_true(HttpProtocolAdapter::is_valid(http_data))
  
  // Test gRPC protocol compatibility
  let grpc_data = GrpcProtocolAdapter::adapt(span)
  assert_true(GrpcProtocolAdapter::is_valid(grpc_data))
  
  // Test WebSocket protocol compatibility
  let websocket_data = WebSocketProtocolAdapter::adapt(span)
  assert_true(WebSocketProtocolAdapter::is_valid(websocket_data))
  
  // Test MQTT protocol compatibility
  let mqtt_data = MqttProtocolAdapter::adapt(span)
  assert_true(MqttProtocolAdapter::is_valid(mqtt_data))
  
  // Convert between protocols and verify compatibility
  let http_to_grpc = GrpcProtocolAdapter::from_http(http_data)
  assert_true(GrpcProtocolAdapter::is_valid(http_to_grpc))
  
  let grpc_to_websocket = WebSocketProtocolAdapter::from_grpc(grpc_data)
  assert_true(WebSocketProtocolAdapter::is_valid(grpc_to_websocket))
  
  let websocket_to_mqtt = MqttProtocolAdapter::from_websocket(websocket_data)
  assert_true(MqttProtocolAdapter::is_valid(websocket_to_mqtt))
  
  // Verify data preservation across protocol conversions
  let http_span = HttpProtocolAdapter::to_span(http_data)
  let grpc_span = GrpcProtocolAdapter::to_span(grpc_data)
  let websocket_span = WebSocketProtocolAdapter::to_span(websocket_data)
  let mqtt_span = MqttProtocolAdapter::to_span(mqtt_data)
  
  assert_eq(Span::name(http_span), Span::name(grpc_span))
  assert_eq(Span::name(grpc_span), Span::name(websocket_span))
  assert_eq(Span::name(websocket_span), Span::name(mqtt_span))
}

test "third-party system compatibility" {
  // Test compatibility with OpenTelemetry
  let span_ctx = SpanContext::new("otel_trace", "otel_span", true, "")
  let span = Span::new("otel_test", Internal, span_ctx)
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "otel.test", StringValue("otel_value"))
  Span::add_event(span, "otel_event", Some(attrs))
  
  // Convert to OpenTelemetry format
  let otel_span = OpenTelemetryAdapter::to_otel_span(span)
  assert_true(OpenTelemetryAdapter::is_otel_span_valid(otel_span))
  
  // Convert back from OpenTelemetry format
  let converted_span = OpenTelemetryAdapter::from_otel_span(otel_span)
  assert_true(Span::is_valid(converted_span))
  
  // Verify data preservation
  assert_eq(Span::name(span), Span::name(converted_span))
  
  // Test compatibility with Prometheus
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "prometheus_meter")
  let counter = Meter::create_counter(meter, "prometheus_counter", Some("Prometheus counter"), Some("count"))
  
  Counter::add(counter, 42.0)
  
  // Convert to Prometheus format
  let prometheus_metric = PrometheusAdapter::to_prometheus_metric(counter)
  assert_true(PrometheusAdapter::is_prometheus_metric_valid(prometheus_metric))
  
  // Convert back from Prometheus format
  let converted_counter = PrometheusAdapter::from_prometheus_metric(prometheus_metric)
  assert_true(Counter::is_valid(converted_counter))
  
  // Test compatibility with Jaeger
  let jaeger_span = JaegerAdapter::to_jaeger_span(span)
  assert_true(JaegerAdapter::is_jaeger_span_valid(jaeger_span))
  
  let converted_jaeger_span = JaegerAdapter::from_jaeger_span(jaeger_span)
  assert_true(Span::is_valid(converted_jaeger_span))
}

test "backward compatibility" {
  // Create old format data (simulating v1.0)
  let old_format_data = """
  {
    "version": "1.0",
    "trace_id": "backward_trace",
    "span_id": "backward_span",
    "name": "backward_test",
    "events": [
      {
        "name": "old_event",
        "attributes": {
          "old_attr": "old_value"
        }
      }
    ]
  }
  """
  
  // Try to load old format data with current version
  let loaded_data = BackwardCompatibilityAdapter::load_old_format(old_format_data)
  
  match loaded_data {
    Success(span) => {
      // Verify data was loaded correctly
      assert_true(Span::is_valid(span))
      assert_eq(Span::name(span), "backward_test")
      
      let ctx = Span::span_context(span)
      assert_eq(SpanContext::trace_id(ctx), "backward_trace")
      assert_eq(SpanContext::span_id(ctx), "backward_span")
    }
    BackwardCompatibilityError(_) => assert_true(false)
  }
  
  // Test migration of old format to new format
  let migrated_data = BackwardCompatibilityAdapter::migrate_to_current_format(old_format_data)
  assert_true(migrated_data.contains("\"version\": \"1.2\"")) // Assuming current version is 1.2
  
  // Verify migrated data can be loaded by current version
  let migrated_span = CurrentFormatLoader::load(migrated_data)
  assert_true(Span::is_valid(migrated_span))
}

test "forward compatibility" {
  // Create current format data
  let span_ctx = SpanContext::new("forward_trace", "forward_span", true, "")
  let span = Span::new("forward_test", Internal, span_ctx)
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "forward.test", StringValue("forward_value"))
  Attributes::set(attrs, "future.feature", StringValue("future_value")) // Simulate future feature
  Span::add_event(span, "forward_event", Some(attrs))
  
  // Serialize with forward compatibility markers
  let forward_compatible_data = ForwardCompatibilityAdapter::serialize_with_markers(span)
  
  // Simulate loading with older version (ignoring unknown fields)
  let loaded_by_old_version = ForwardCompatibilityAdapter::load_by_old_version(forward_compatible_data)
  
  match loaded_by_old_version {
    Success(old_span) => {
      // Verify known fields are preserved
      assert_true(Span::is_valid(old_span))
      assert_eq(Span::name(old_span), "forward_test")
      
      let ctx = Span::span_context(old_span)
      assert_eq(SpanContext::trace_id(ctx), "forward_trace")
      assert_eq(SpanContext::span_id(ctx), "forward_span")
      
      // Unknown fields should be ignored
      let old_attrs = Span::attributes(old_span)
      let future_attr = Attributes::get(old_attrs, "future.feature")
      match future_attr {
        Some(_) => assert_true(false) // Should be ignored by old version
        None => assert_true(true)
      }
    }
    _ => assert_true(false)
  }
  
  // Verify current version can still load the data
  let loaded_by_current_version = CurrentFormatLoader::load(forward_compatible_data)
  assert_true(Span::is_valid(loaded_by_current_version))
  
  // Verify future feature is preserved for current version
  let current_attrs = Span::attributes(loaded_by_current_version)
  let future_attr = Attributes::get(current_attrs, "future.feature")
  match future_attr {
    StringValue(v) => assert_eq(v, "future_value")
    _ => assert_true(false)
  }
}

test "api compatibility" {
  // Test API compatibility with different versions
  
  // Test v1.0 API
  let v1_api = ApiV1::new()
  let v1_result = ApiV1::create_span(v1_api, "api_v1_test")
  assert_true(ApiV1::is_span_valid(v1_result))
  
  // Test v1.1 API (backward compatible)
  let v1_1_api = ApiV1_1::new()
  let v1_1_result = ApiV1_1::create_span(v1_1_api, "api_v1_1_test")
  assert_true(ApiV1_1::is_span_valid(v1_1_result))
  
  // Test v2.0 API (breaking changes)
  let v2_api = ApiV2::new()
  let v2_result = ApiV2::create_span(v2_api, "api_v2_test", SpanOptions::default())
  assert_true(ApiV2::is_span_valid(v2_result))
  
  // Test API compatibility adapters
  let v1_to_v1_1 = ApiCompatibilityAdapter::v1_to_v1_1(v1_result)
  assert_true(ApiV1_1::is_span_valid(v1_to_v1_1))
  
  let v1_1_to_v2 = ApiCompatibilityAdapter::v1_1_to_v2(v1_1_result)
  assert_true(ApiV2::is_span_valid(v1_1_to_v2))
  
  let v2_to_v1 = ApiCompatibilityAdapter::v2_to_v1(v2_result)
  assert_true(ApiV1::is_span_valid(v2_to_v1))
  
  // Verify data preservation across API versions
  assert_eq(ApiV1::span_name(v1_result), ApiV1_1::span_name(v1_to_v1_1))
  assert_eq(ApiV1_1::span_name(v1_1_result), ApiV2::span_name(v1_1_to_v2))
  assert_eq(ApiV2::span_name(v2_result), ApiV1::span_name(v2_to_v1))
}

test "configuration compatibility" {
  // Create configuration with different versions
  let v1_config = Configuration::new()
  Configuration::set(v1_config, "service.name", "v1_service")
  Configuration::set(v1_config, "telemetry.enabled", true)
  Configuration::set(v1_config, "telemetry.sampling_rate", 0.5)
  
  // Export v1 configuration
  let v1_config_exported = Configuration::export(v1_config)
  
  // Try to load v1 configuration with v2 format
  let v2_config = ConfigurationV2::import(v1_config_exported)
  
  // Verify configuration was loaded with defaults for new fields
  let service_name = ConfigurationV2::get_string(v2_config, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "v1_service")
    None => assert_true(false)
  }
  
  let telemetry_enabled = ConfigurationV2::get_bool(v2_config, "telemetry.enabled")
  match telemetry_enabled {
    Some(enabled) => assert_true(enabled)
    None => assert_true(false)
  }
  
  let sampling_rate = ConfigurationV2::get_float(v2_config, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.5)
    None => assert_true(false)
  }
  
  // Verify new fields have default values
  let new_field = ConfigurationV2::get_string(v2_config, "telemetry.new_field")
  match new_field {
    Some(value) => assert_eq(value, "default_value") // Should have default
    None => assert_true(false)
  }
  
  // Export v2 configuration
  let v2_config_exported = ConfigurationV2::export(v2_config)
  
  // Verify v1 can load v2 configuration (ignoring unknown fields)
  let v1_config_from_v2 = Configuration::import(v2_config_exported)
  
  let v1_service_name = Configuration::get_string(v1_config_from_v2, "service.name")
  match v1_service_name {
    Some(name) => assert_eq(name, "v1_service")
    None => assert_true(false)
  }
  
  // Unknown fields should be ignored
  let v1_unknown_field = Configuration::get_string(v1_config_from_v2, "telemetry.new_field")
  match v1_unknown_field {
    Some(_) => assert_true(false) // Should be ignored
    None => assert_true(true)
  }
}

test "data model compatibility" {
  // Create data with v1 model
  let v1_model = DataModelV1::new()
  DataModelV1::add_attribute(v1_model, "v1_attr", "v1_value")
  DataModelV1::add_metric(v1_model, "v1_metric", 42.0)
  
  // Convert to v2 model
  let v2_model = DataModelAdapter::v1_to_v2(v1_model)
  
  // Verify v2 model has v1 data
  let v1_attr = DataModelV2::get_attribute(v2_model, "v1_attr")
  match v1_attr {
    StringValue(v) => assert_eq(v, "v1_value")
    _ => assert_true(false)
  }
  
  let v1_metric = DataModelV2::get_metric(v2_model, "v1_metric")
  match v1_metric {
    FloatValue(v) => assert_eq(v, 42.0)
    _ => assert_true(false)
  }
  
  // Add v2-specific data
  DataModelV2::add_event(v2_model, "v2_event", DataModelV2::timestamp())
  
  // Convert back to v1 model (v2 data should be filtered out)
  let v1_model_from_v2 = DataModelAdapter::v2_to_v1(v2_model)
  
  // Verify v1 data is preserved
  let v1_attr_back = DataModelV1::get_attribute(v1_model_from_v2, "v1_attr")
  match v1_attr_back {
    StringValue(v) => assert_eq(v, "v1_value")
    _ => assert_true(false)
  }
  
  let v1_metric_back = DataModelV1::get_metric(v1_model_from_v2, "v1_metric")
  match v1_metric_back {
    FloatValue(v) => assert_eq(v, 42.0)
    _ => assert_true(false)
  }
  
  // Verify v2 data is not present in v1 model
  let v2_event_count = DataModelV1::event_count(v1_model_from_v2)
  assert_eq(v2_event_count, 0) // v2 events should be filtered out
}