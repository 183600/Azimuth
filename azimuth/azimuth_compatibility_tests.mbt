// Azimuth Compatibility Test Suite
// This file contains compatibility test cases for the Azimuth telemetry system

// Test 1: Version compatibility testing
pub test "compatibility version testing" {
  // Test with different version formats
  let provider_v1 = azimuth::MeterProvider::default()
  let meter_v1 = azimuth::MeterProvider::get_meter(provider_v1, "v1.0.0")
  
  let provider_v2 = azimuth::MeterProvider::default()
  let meter_v2 = azimuth::MeterProvider::get_meter(provider_v2, "v2.0.0")
  
  let provider_v3 = azimuth::MeterProvider::default()
  let meter_v3 = azimuth::MeterProvider::get_meter(provider_v3, "v2.1.0")
  
  // Create metrics with different version compatibility
  let v1_counter = azimuth::Meter::create_counter(meter_v1, "legacy.counter")
  let v2_counter = azimuth::Meter::create_counter(meter_v2, "modern.counter")
  let v3_counter = azimuth::Meter::create_counter(meter_v3, "latest.counter")
  
  // Test with semantic versioning
  let provider_semantic = azimuth::MeterProvider::default()
  let meter_semantic = azimuth::MeterProvider::get_meter(provider_semantic, "1.2.3-beta.1+build.123")
  
  let semantic_counter = azimuth::Meter::create_counter(meter_semantic, "semantic.version.counter")
  
  // Test with pre-release versions
  let provider_prerelease = azimuth::MeterProvider::default()
  let meter_prerelease = azimuth::MeterProvider::get_meter(provider_prerelease, "1.0.0-alpha")
  
  let prerelease_counter = azimuth::Meter::create_counter(meter_prerelease, "prerelease.counter")
  
  // Perform operations on all versioned metrics
  azimuth::Counter::add(v1_counter, 1.0)
  azimuth::Counter::add(v2_counter, 1.0)
  azimuth::Counter::add(v3_counter, 1.0)
  azimuth::Counter::add(semantic_counter, 1.0)
  azimuth::Counter::add(prerelease_counter, 1.0)
  
  // Verify all metrics work regardless of version
  assert_eq(v1_counter.name, "legacy.counter")
  assert_eq(v2_counter.name, "modern.counter")
  assert_eq(v3_counter.name, "latest.counter")
  assert_eq(semantic_counter.name, "semantic.version.counter")
  assert_eq(prerelease_counter.name, "prerelease.counter")
}

// Test 2: Platform compatibility testing
pub test "compatibility platform testing" {
  // Test platform-specific resource attributes
  let resource = azimuth::Resource::new()
  
  // Windows platform attributes
  let windows_attributes = [
    ("os.type", azimuth::StringValue("windows")),
    ("os.version", azimuth::StringValue("10.0.19042")),
    ("os.description", azimuth::StringValue("Windows 10 Pro")),
    ("host.arch", azimuth::StringValue("amd64")),
    ("host.name", azimuth::StringValue("WIN-1234567890"))
  ]
  
  let windows_resource = azimuth::Resource::with_attributes(resource, windows_attributes)
  
  // Linux platform attributes
  let linux_attributes = [
    ("os.type", azimuth::StringValue("linux")),
    ("os.version", azimuth::StringValue("5.15.0")),
    ("os.description", azimuth::StringValue("Ubuntu 22.04 LTS")),
    ("host.arch", azimuth::StringValue("amd64")),
    ("host.name", azimuth::StringValue("linux-host-001"))
  ]
  
  let linux_resource = azimuth::Resource::with_attributes(resource, linux_attributes)
  
  // macOS platform attributes
  let macos_attributes = [
    ("os.type", azimuth::StringValue("darwin")),
    ("os.version", azimuth::StringValue("21.6.0")),
    ("os.description", azimuth::StringValue("macOS Monterey 12.5")),
    ("host.arch", azimuth::StringValue("arm64")),
    ("host.name", azimuth::StringValue("macbook-pro-001"))
  ]
  
  let macos_resource = azimuth::Resource::with_attributes(resource, macos_attributes)
  
  // Container platform attributes
  let container_attributes = [
    ("os.type", azimuth::StringValue("linux")),
    ("os.version", azimuth::StringValue("5.10.0")),
    ("container.name", azimuth::StringValue("azimuth-service")),
    ("container.image", azimuth::StringValue("azimuth:1.2.3")),
    ("container.id", azimuth::StringValue("container-123456"))
  ]
  
  let container_resource = azimuth::Resource::with_attributes(resource, container_attributes)
  
  // Kubernetes platform attributes
  let k8s_attributes = [
    ("os.type", azimuth::StringValue("linux")),
    ("k8s.pod.name", azimuth::StringValue("azimuth-pod-12345")),
    ("k8s.namespace", azimuth::StringValue("production")),
    ("k8s.deployment.name", azimuth::StringValue("azimuth-deployment")),
    ("k8s.node.name", azimuth::StringValue("worker-node-001"))
  ]
  
  let k8s_resource = azimuth::Resource::with_attributes(resource, k8s_attributes)
  
  // Verify platform-specific attributes
  assert_eq(azimuth::Resource::get_attribute(windows_resource, "os.type"), Some(azimuth::StringValue("windows")))
  assert_eq(azimuth::Resource::get_attribute(windows_resource, "os.version"), Some(azimuth::StringValue("10.0.19042")))
  assert_eq(azimuth::Resource::get_attribute(windows_resource, "host.arch"), Some(azimuth::StringValue("amd64")))
  
  assert_eq(azimuth::Resource::get_attribute(linux_resource, "os.type"), Some(azimuth::StringValue("linux")))
  assert_eq(azimuth::Resource::get_attribute(linux_resource, "os.version"), Some(azimuth::StringValue("5.15.0")))
  assert_eq(azimuth::Resource::get_attribute(linux_resource, "host.arch"), Some(azimuth::StringValue("amd64")))
  
  assert_eq(azimuth::Resource::get_attribute(macos_resource, "os.type"), Some(azimuth::StringValue("darwin")))
  assert_eq(azimuth::Resource::get_attribute(macos_resource, "os.version"), Some(azimuth::StringValue("21.6.0")))
  assert_eq(azimuth::Resource::get_attribute(macos_resource, "host.arch"), Some(azimuth::StringValue("arm64")))
  
  assert_eq(azimuth::Resource::get_attribute(container_resource, "container.name"), Some(azimuth::StringValue("azimuth-service")))
  assert_eq(azimuth::Resource::get_attribute(container_resource, "container.image"), Some(azimuth::StringValue("azimuth:1.2.3")))
  
  assert_eq(azimuth::Resource::get_attribute(k8s_resource, "k8s.pod.name"), Some(azimuth::StringValue("azimuth-pod-12345")))
  assert_eq(azimuth::Resource::get_attribute(k8s_resource, "k8s.namespace"), Some(azimuth::StringValue("production")))
}

// Test 3: Configuration compatibility testing
pub test "compatibility configuration testing" {
  // Test with different configuration scenarios
  
  // Minimal configuration
  let minimal_provider = azimuth::MeterProvider::default()
  let minimal_meter = azimuth::MeterProvider::get_meter(minimal_provider, "minimal-service")
  let minimal_counter = azimuth::Meter::create_counter(minimal_meter, "minimal.counter")
  
  // Full configuration
  let full_provider = azimuth::MeterProvider::default()
  let full_meter = azimuth::MeterProvider::get_meter(full_provider, "full-service")
  let full_counter = azimuth::Meter::create_counter(full_meter, "full.counter", Some("Full description"), Some("requests"))
  
  // Configuration with special characters
  let special_provider = azimuth::MeterProvider::default()
  let special_meter = azimuth::MeterProvider::get_meter(special_provider, "service-with-special-chars_123")
  let special_counter = azimuth::Meter::create_counter(special_meter, "counter.with-special-chars_123")
  
  // Configuration with Unicode
  let unicode_provider = azimuth::MeterProvider::default()
  let unicode_meter = azimuth::MeterProvider::get_meter(unicode_provider, "服务-unicode")
  let unicode_counter = azimuth::Meter::create_counter(unicode_meter, "计数器-unicode")
  
  // Configuration with very long names
  let long_provider = azimuth::MeterProvider::default()
  let long_meter = azimuth::MeterProvider::get_meter(long_provider, "this.is.a.very.long.service.name.that.tests.boundary.conditions")
  let long_counter = azimuth::Meter::create_counter(long_meter, "this.is.a.very.long.metric.name.that.tests.boundary.conditions")
  
  // Perform operations on all configured metrics
  azimuth::Counter::add(minimal_counter, 1.0)
  azimuth::Counter::add(full_counter, 1.0)
  azimuth::Counter::add(special_counter, 1.0)
  azimuth::Counter::add(unicode_counter, 1.0)
  azimuth::Counter::add(long_counter, 1.0)
  
  // Verify all configurations work
  assert_eq(minimal_counter.name, "minimal.counter")
  assert_eq(minimal_counter.description, None)
  assert_eq(minimal_counter.unit, None)
  
  assert_eq(full_counter.name, "full.counter")
  assert_eq(full_counter.description, Some("Full description"))
  assert_eq(full_counter.unit, Some("requests"))
  
  assert_eq(special_counter.name, "counter.with-special-chars_123")
  assert_eq(unicode_counter.name, "计数器-unicode")
  assert_eq(long_counter.name, "this.is.a.very.long.metric.name.that.tests.boundary.conditions")
}

// Test 4: Protocol compatibility testing
pub test "compatibility protocol testing" {
  // Test with different protocol versions and formats
  
  // HTTP/1.1 compatibility
  let http11_provider = azimuth::MeterProvider::default()
  let http11_meter = azimuth::MeterProvider::get_meter(http11_provider, "http11-service")
  let http11_counter = azimuth::Meter::create_counter(http11_meter, "http11.requests")
  
  // HTTP/2 compatibility
  let http2_provider = azimuth::MeterProvider::default()
  let http2_meter = azimuth::MeterProvider::get_meter(http2_provider, "http2-service")
  let http2_counter = azimuth::Meter::create_counter(http2_meter, "http2.requests")
  
  // gRPC compatibility
  let grpc_provider = azimuth::MeterProvider::default()
  let grpc_meter = azimuth::MeterProvider::get_meter(grpc_provider, "grpc-service")
  let grpc_counter = azimuth::Meter::create_counter(grpc_meter, "grpc.requests")
  
  // WebSocket compatibility
  let websocket_provider = azimuth::MeterProvider::default()
  let websocket_meter = azimuth::MeterProvider::get_meter(websocket_provider, "websocket-service")
  let websocket_counter = azimuth::Meter::create_counter(websocket_meter, "websocket.connections")
  
  // Create protocol-specific spans
  let http11_span_ctx = azimuth::SpanContext::new("http11-trace", "http11-span", true, "")
  let http11_span = azimuth::Span::new("http11.request", azimuth::Server, http11_span_ctx)
  
  let http2_span_ctx = azimuth::SpanContext::new("http2-trace", "http2-span", true, "")
  let http2_span = azimuth::Span::new("http2.request", azimuth::Server, http2_span_ctx)
  
  let grpc_span_ctx = azimuth::SpanContext::new("grpc-trace", "grpc-span", true, "")
  let grpc_span = azimuth::Span::new("grpc.request", azimuth::Server, grpc_span_ctx)
  
  let websocket_span_ctx = azimuth::SpanContext::new("websocket-trace", "websocket-span", true, "")
  let websocket_span = azimuth::Span::new("websocket.connection", azimuth::Server, websocket_span_ctx)
  
  // Perform operations on protocol-specific metrics and spans
  azimuth::Counter::add(http11_counter, 1.0)
  azimuth::Counter::add(http2_counter, 1.0)
  azimuth::Counter::add(grpc_counter, 1.0)
  azimuth::Counter::add(websocket_counter, 1.0)
  
  azimuth::Span::add_event(http11_span, "request.received", [])
  azimuth::Span::add_event(http2_span, "request.received", [])
  azimuth::Span::add_event(grpc_span, "request.received", [])
  azimuth::Span::add_event(websocket_span, "connection.established", [])
  
  // Verify protocol-specific operations
  assert_eq(http11_counter.name, "http11.requests")
  assert_eq(http2_counter.name, "http2.requests")
  assert_eq(grpc_counter.name, "grpc.requests")
  assert_eq(websocket_counter.name, "websocket.connections")
  
  assert_eq(azimuth::Span::name(http11_span), "http11.request")
  assert_eq(azimuth::Span::name(http2_span), "http2.request")
  assert_eq(azimuth::Span::name(grpc_span), "grpc.request")
  assert_eq(azimuth::Span::name(websocket_span), "websocket.connection")
}

// Test 5: Data format compatibility testing
pub test "compatibility data format testing" {
  // Test with different data formats and serializations
  
  // JSON format compatibility
  let json_provider = azimuth::MeterProvider::default()
  let json_meter = azimuth::MeterProvider::get_meter(json_provider, "json-service")
  let json_counter = azimuth::Meter::create_counter(json_meter, "json.requests")
  
  // Protocol Buffers format compatibility
  let protobuf_provider = azimuth::MeterProvider::default()
  let protobuf_meter = azimuth::MeterProvider::get_meter(protobuf_provider, "protobuf-service")
  let protobuf_counter = azimuth::Meter::create_counter(protobuf_meter, "protobuf.requests")
  
  // XML format compatibility
  let xml_provider = azimuth::MeterProvider::default()
  let xml_meter = azimuth::MeterProvider::get_meter(xml_provider, "xml-service")
  let xml_counter = azimuth::Meter::create_counter(xml_meter, "xml.requests")
  
  // CSV format compatibility
  let csv_provider = azimuth::MeterProvider::default()
  let csv_meter = azimuth::MeterProvider::get_meter(csv_provider, "csv-service")
  let csv_counter = azimuth::Meter::create_counter(csv_meter, "csv.requests")
  
  // Create attributes with different data types
  let attrs = azimuth::Attributes::new()
  
  // String attributes
  azimuth::Attributes::set(attrs, "string.value", azimuth::StringValue("test string"))
  
  // Integer attributes
  azimuth::Attributes::set(attrs, "int.value", azimuth::IntValue(42))
  
  // Float attributes
  azimuth::Attributes::set(attrs, "float.value", azimuth::FloatValue(3.14))
  
  // Boolean attributes
  azimuth::Attributes::set(attrs, "bool.value", azimuth::BoolValue(true))
  
  // Array attributes
  azimuth::Attributes::set(attrs, "string.array", azimuth::ArrayStringValue(["a", "b", "c"]))
  azimuth::Attributes::set(attrs, "int.array", azimuth::ArrayIntValue([1, 2, 3]))
  
  // Perform operations on format-specific metrics
  azimuth::Counter::add(json_counter, 1.0)
  azimuth::Counter::add(protobuf_counter, 1.0)
  azimuth::Counter::add(xml_counter, 1.0)
  azimuth::Counter::add(csv_counter, 1.0)
  
  // Verify format-specific operations
  assert_eq(json_counter.name, "json.requests")
  assert_eq(protobuf_counter.name, "protobuf.requests")
  assert_eq(xml_counter.name, "xml.requests")
  assert_eq(csv_counter.name, "csv.requests")
  
  // Verify attribute compatibility
  assert_eq(azimuth::Attributes::get(attrs, "string.value"), Some(azimuth::StringValue("test string")))
  assert_eq(azimuth::Attributes::get(attrs, "int.value"), Some(azimuth::IntValue(42)))
  assert_eq(azimuth::Attributes::get(attrs, "float.value"), Some(azimuth::FloatValue(3.14)))
  assert_eq(azimuth::Attributes::get(attrs, "bool.value"), Some(azimuth::BoolValue(true)))
  assert_eq(azimuth::Attributes::get(attrs, "string.array"), Some(azimuth::ArrayStringValue(["a", "b", "c"])))
  assert_eq(azimuth::Attributes::get(attrs, "int.array"), Some(azimuth::ArrayIntValue([1, 2, 3])))
}

// Test 6: Time zone and timestamp compatibility testing
pub test "compatibility timezone testing" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "timezone-logger")
  
  // Test with different timestamp formats
  let utc_timestamp = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  let est_timestamp = 1735675200000000000L  // 2024-12-31 19:00:00 EST (UTC-5)
  let pst_timestamp = 1735668000000000000L  // 2024-12-31 16:00:00 PST (UTC-8)
  let jst_timestamp = 1735716400000000000L  // 2025-01-01 09:00:00 JST (UTC+9)
  
  // Create log records with different timestamps
  let utc_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("UTC timestamp log"),
    None,
    Some(utc_timestamp),
    Some(utc_timestamp + 1000000L),
    Some("timezone-trace-utc"),
    Some("timezone-span-utc"),
    None
  )
  
  let est_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("EST timestamp log"),
    None,
    Some(est_timestamp),
    Some(est_timestamp + 1000000L),
    Some("timezone-trace-est"),
    Some("timezone-span-est"),
    None
  )
  
  let pst_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("PST timestamp log"),
    None,
    Some(pst_timestamp),
    Some(pst_timestamp + 1000000L),
    Some("timezone-trace-pst"),
    Some("timezone-span-pst"),
    None
  )
  
  let jst_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("JST timestamp log"),
    None,
    Some(jst_timestamp),
    Some(jst_timestamp + 1000000L),
    Some("timezone-trace-jst"),
    Some("timezone-span-jst"),
    None
  )
  
  // Create spans with different timestamps
  let utc_span_ctx = azimuth::SpanContext::new("timezone-trace-utc", "timezone-span-utc", true, "")
  let utc_span = azimuth::Span::new("utc.operation", azimuth::Internal, utc_span_ctx)
  
  let est_span_ctx = azimuth::SpanContext::new("timezone-trace-est", "timezone-span-est", true, "")
  let est_span = azimuth::Span::new("est.operation", azimuth::Internal, est_span_ctx)
  
  let pst_span_ctx = azimuth::SpanContext::new("timezone-trace-pst", "timezone-span-pst", true, "")
  let pst_span = azimuth::Span::new("pst.operation", azimuth::Internal, pst_span_ctx)
  
  let jst_span_ctx = azimuth::SpanContext::new("timezone-trace-jst", "timezone-span-jst", true, "")
  let jst_span = azimuth::Span::new("jst.operation", azimuth::Internal, jst_span_ctx)
  
  // Emit all log records
  azimuth::Logger::emit(logger, utc_log)
  azimuth::Logger::emit(logger, est_log)
  azimuth::Logger::emit(logger, pst_log)
  azimuth::Logger::emit(logger, jst_log)
  
  // Verify timestamp compatibility
  assert_eq(azimuth::LogRecord::body(utc_log), Some("UTC timestamp log"))
  assert_eq(azimuth::LogRecord::body(est_log), Some("EST timestamp log"))
  assert_eq(azimuth::LogRecord::body(pst_log), Some("PST timestamp log"))
  assert_eq(azimuth::LogRecord::body(jst_log), Some("JST timestamp log"))
  
  assert_eq(azimuth::LogRecord::trace_id(utc_log), Some("timezone-trace-utc"))
  assert_eq(azimuth::LogRecord::trace_id(est_log), Some("timezone-trace-est"))
  assert_eq(azimuth::LogRecord::trace_id(pst_log), Some("timezone-trace-pst"))
  assert_eq(azimuth::LogRecord::trace_id(jst_log), Some("timezone-trace-jst"))
  
  assert_eq(azimuth::Span::name(utc_span), "utc.operation")
  assert_eq(azimuth::Span::name(est_span), "est.operation")
  assert_eq(azimuth::Span::name(pst_span), "pst.operation")
  assert_eq(azimuth::Span::name(jst_span), "jst.operation")
}

// Test 7: Browser and runtime compatibility testing
pub test "compatibility browser_runtime_testing" {
  // Test with different runtime environments
  
  // Node.js environment
  let nodejs_provider = azimuth::MeterProvider::default()
  let nodejs_meter = azimuth::MeterProvider::get_meter(nodejs_provider, "nodejs-service")
  let nodejs_counter = azimuth::Meter::create_counter(nodejs_meter, "nodejs.requests")
  
  // Browser environment
  let browser_provider = azimuth::MeterProvider::default()
  let browser_meter = azimuth::MeterProvider::get_meter(browser_provider, "browser-service")
  let browser_counter = azimuth::Meter::create_counter(browser_meter, "browser.requests")
  
  // Deno environment
  let deno_provider = azimuth::MeterProvider::default()
  let deno_meter = azimuth::MeterProvider::get_meter(deno_provider, "deno-service")
  let deno_counter = azimuth::Meter::create_counter(deno_meter, "deno.requests")
  
  // WebAssembly environment
  let wasm_provider = azimuth::MeterProvider::default()
  let wasm_meter = azimuth::MeterProvider::get_meter(wasm_provider, "wasm-service")
  let wasm_counter = azimuth::Meter::create_counter(wasm_meter, "wasm.requests")
  
  // Create runtime-specific resources
  let nodejs_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("runtime.name", azimuth::StringValue("nodejs")),
    ("runtime.version", azimuth::StringValue("18.17.0")),
    ("runtime.arch", azimuth::StringValue("x64"))
  ])
  
  let browser_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("runtime.name", azimuth::StringValue("browser")),
    ("runtime.version", azimuth::StringValue("Chrome/120.0.0.0")),
    ("runtime.arch", azimuth::StringValue("x64"))
  ])
  
  let deno_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("runtime.name", azimuth::StringValue("deno")),
    ("runtime.version", azimuth::StringValue("1.38.0")),
    ("runtime.arch", azimuth::StringValue("x64"))
  ])
  
  let wasm_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("runtime.name", azimuth::StringValue("wasm")),
    ("runtime.version", azimuth::StringValue("1.0.0")),
    ("runtime.arch", azimuth::StringValue("wasm32"))
  ])
  
  // Perform operations on runtime-specific metrics
  azimuth::Counter::add(nodejs_counter, 1.0)
  azimuth::Counter::add(browser_counter, 1.0)
  azimuth::Counter::add(deno_counter, 1.0)
  azimuth::Counter::add(wasm_counter, 1.0)
  
  // Verify runtime-specific operations
  assert_eq(nodejs_counter.name, "nodejs.requests")
  assert_eq(browser_counter.name, "browser.requests")
  assert_eq(deno_counter.name, "deno.requests")
  assert_eq(wasm_counter.name, "wasm.requests")
  
  // Verify runtime-specific resources
  assert_eq(azimuth::Resource::get_attribute(nodejs_resource, "runtime.name"), Some(azimuth::StringValue("nodejs")))
  assert_eq(azimuth::Resource::get_attribute(browser_resource, "runtime.name"), Some(azimuth::StringValue("browser")))
  assert_eq(azimuth::Resource::get_attribute(deno_resource, "runtime.name"), Some(azimuth::StringValue("deno")))
  assert_eq(azimuth::Resource::get_attribute(wasm_resource, "runtime.name"), Some(azimuth::StringValue("wasm")))
}

// Test 8: Dependency and library compatibility testing
pub test "compatibility dependency_testing" {
  // Test with different dependency versions
  
  // OpenTelemetry compatibility
  let otel_provider = azimuth::MeterProvider::default()
  let otel_meter = azimuth::MeterProvider::get_meter(otel_provider, "otel-compatible-service")
  let otel_counter = azimuth::Meter::create_counter(otel_meter, "otel.requests")
  
  // Prometheus compatibility
  let prometheus_provider = azimuth::MeterProvider::default()
  let prometheus_meter = azimuth::MeterProvider::get_meter(prometheus_provider, "prometheus-compatible-service")
  let prometheus_counter = azimuth::Meter::create_counter(prometheus_meter, "prometheus.requests")
  
  // Jaeger compatibility
  let jaeger_provider = azimuth::MeterProvider::default()
  let jaeger_meter = azimuth::MeterProvider::get_meter(jaeger_provider, "jaeger-compatible-service")
  let jaeger_counter = azimuth::Meter::create_counter(jaeger_meter, "jaeger.requests")
  
  // Zipkin compatibility
  let zipkin_provider = azimuth::MeterProvider::default()
  let zipkin_meter = azimuth::MeterProvider::get_meter(zipkin_provider, "zipkin-compatible-service")
  let zipkin_counter = azimuth::Meter::create_counter(zipkin_meter, "zipkin.requests")
  
  // Create dependency-specific spans
  let otel_span_ctx = azimuth::SpanContext::new("otel-trace", "otel-span", true, "")
  let otel_span = azimuth::Span::new("otel.operation", azimuth::Internal, otel_span_ctx)
  
  let prometheus_span_ctx = azimuth::SpanContext::new("prometheus-trace", "prometheus-span", true, "")
  let prometheus_span = azimuth::Span::new("prometheus.operation", azimuth::Internal, prometheus_span_ctx)
  
  let jaeger_span_ctx = azimuth::SpanContext::new("jaeger-trace", "jaeger-span", true, "")
  let jaeger_span = azimuth::Span::new("jaeger.operation", azimuth::Internal, jaeger_span_ctx)
  
  let zipkin_span_ctx = azimuth::SpanContext::new("zipkin-trace", "zipkin-span", true, "")
  let zipkin_span = azimuth::Span::new("zipkin.operation", azimuth::Internal, zipkin_span_ctx)
  
  // Create dependency-specific propagators
  let otel_propagator = azimuth::W3CTraceContextPropagator::new()
  let otel_propagators = [otel_propagator]
  let otel_composite = azimuth::CompositePropagator::new(otel_propagators)
  
  let prometheus_propagator = azimuth::W3CTraceContextPropagator::new()
  let prometheus_propagators = [prometheus_propagator]
  let prometheus_composite = azimuth::CompositePropagator::new(prometheus_propagators)
  
  // Perform operations on dependency-specific metrics and spans
  azimuth::Counter::add(otel_counter, 1.0)
  azimuth::Counter::add(prometheus_counter, 1.0)
  azimuth::Counter::add(jaeger_counter, 1.0)
  azimuth::Counter::add(zipkin_counter, 1.0)
  
  azimuth::Span::add_event(otel_span, "otel.event", [])
  azimuth::Span::add_event(prometheus_span, "prometheus.event", [])
  azimuth::Span::add_event(jaeger_span, "jaeger.event", [])
  azimuth::Span::add_event(zipkin_span, "zipkin.event", [])
  
  // Test propagator operations
  let otel_carrier = azimuth::TextMapCarrier::new()
  let otel_ctx = azimuth::Context::root()
  azimuth::CompositePropagator::inject(otel_composite, otel_ctx, otel_carrier)
  
  let prometheus_carrier = azimuth::TextMapCarrier::new()
  let prometheus_ctx = azimuth::Context::root()
  azimuth::CompositePropagator::inject(prometheus_composite, prometheus_ctx, prometheus_carrier)
  
  // Verify dependency-specific operations
  assert_eq(otel_counter.name, "otel.requests")
  assert_eq(prometheus_counter.name, "prometheus.requests")
  assert_eq(jaeger_counter.name, "jaeger.requests")
  assert_eq(zipkin_counter.name, "zipkin.requests")
  
  assert_eq(azimuth::Span::name(otel_span), "otel.operation")
  assert_eq(azimuth::Span::name(prometheus_span), "prometheus.operation")
  assert_eq(azimuth::Span::name(jaeger_span), "jaeger.operation")
  assert_eq(azimuth::Span::name(zipkin_span), "zipkin.operation")
}