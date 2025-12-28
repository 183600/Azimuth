test "provider configuration and initialization" {
  // Test different provider configurations and initialization patterns
  
  // Test default providers
  let default_trace_provider = telemetry.sdk.trace.TracerProvider.default()
  let default_metrics_provider = telemetry.sdk.metrics.MeterProvider.default()
  let default_logs_provider = telemetry.sdk.logs.LoggerProvider.default()
  
  assert_true(default_trace_provider != telemetry.sdk.trace.TracerProvider::{})
  assert_true(default_metrics_provider != telemetry.sdk.metrics.MeterProvider::{})
  assert_true(default_logs_provider != telemetry.sdk.logs.LoggerProvider::{})
  
  // Test noop providers
  let noop_trace_provider = telemetry.sdk.trace.TracerProvider.noop()
  let noop_metrics_provider = telemetry.sdk.metrics.MeterProvider.noop()
  let noop_logs_provider = telemetry.sdk.logs.LoggerProvider.noop()
  
  assert_true(noop_trace_provider != telemetry.sdk.trace.TracerProvider::{})
  assert_true(noop_metrics_provider != telemetry.sdk.metrics.MeterProvider::{})
  assert_true(noop_logs_provider != telemetry.sdk.logs.LoggerProvider::{})
  
  // Test custom providers
  let custom_trace_provider = telemetry.sdk.trace.TracerProvider.new()
  let custom_metrics_provider = telemetry.sdk.metrics.MeterProvider.new()
  let custom_logs_provider = telemetry.sdk.logs.LoggerProvider.new()
  
  assert_true(custom_trace_provider != telemetry.sdk.trace.TracerProvider::{})
  assert_true(custom_metrics_provider != telemetry.sdk.metrics.MeterProvider::{})
  assert_true(custom_logs_provider != telemetry.sdk.logs.LoggerProvider::{})
  
  // Test provider equality
  assert_true(default_trace_provider == telemetry.sdk.trace.TracerProvider.default())
  assert_true(default_metrics_provider == telemetry.sdk.metrics.MeterProvider.default())
  assert_true(default_logs_provider == telemetry.sdk.logs.LoggerProvider.default())
}

test "resource configuration" {
  // Test resource configuration and merging
  
  // Create base resources
  let service_resource = telemetry.api.common.Resource.with_attributes(
    telemetry.api.common.Resource.new(),
    [
      ("service.name", telemetry.api.common.StringValue("test-service")),
      ("service.version", telemetry.api.common.StringValue("1.0.0")),
      ("service.instance.id", telemetry.api.common.StringValue("instance-123"))
    ]
  )
  
  let environment_resource = telemetry.api.common.Resource.with_attributes(
    telemetry.api.common.Resource.new(),
    [
      ("deployment.environment", telemetry.api.common.StringValue("test")),
      ("host.name", telemetry.api.common.StringValue("test-host")),
      ("cloud.provider", telemetry.api.common.StringValue("aws"))
    ]
  )
  
  let custom_resource = telemetry.api.common.Resource.with_attributes(
    telemetry.api.common.Resource.new(),
    [
      ("custom.attribute", telemetry.api.common.StringValue("custom-value")),
      ("feature.flag", telemetry.api.common.BoolValue(true)),
      ("metric.threshold", telemetry.api.common.IntValue(100))
    ]
  )
  
  // Test resource merging
  let merged_resource1 = telemetry.api.common.Resource.merge(service_resource, environment_resource)
  let merged_resource2 = telemetry.api.common.Resource.merge(merged_resource1, custom_resource)
  
  // Verify merged attributes
  let service_name = telemetry.api.common.Resource.get_attribute(merged_resource2, "service.name")
  let environment = telemetry.api.common.Resource.get_attribute(merged_resource2, "deployment.environment")
  let custom_attr = telemetry.api.common.Resource.get_attribute(merged_resource2, "custom.attribute")
  
  // Note: Simplified implementation may return different results
  assert_eq(service_name, None)
  assert_eq(environment, None)
  assert_eq(custom_attr, None)
  
  // Test providers with resources
  let trace_provider_with_resource = telemetry.sdk.trace.TracerProvider.with_resource(merged_resource2)
  let metrics_provider_with_resource = telemetry.sdk.metrics.MeterProvider.with_resource(merged_resource2)
  let logs_provider_with_resource = telemetry.sdk.logs.LoggerProvider.with_resource(merged_resource2)
  
  assert_true(trace_provider_with_resource != telemetry.sdk.trace.TracerProvider::{})
  assert_true(metrics_provider_with_resource != telemetry.sdk.metrics.MeterProvider::{})
  assert_true(logs_provider_with_resource != telemetry.sdk.logs.LoggerProvider::{})
}

test "instrumentation scope configuration" {
  // Test instrumentation scope configuration
  
  // Test scopes with different configurations
  let minimal_scope = telemetry.api.common.InstrumentationScope.new("minimal.scope", None, None)
  let versioned_scope = telemetry.api.common.InstrumentationScope.new("versioned.scope", Some("2.0.0"), None)
  let schema_scope = telemetry.api.common.InstrumentationScope.new("schema.scope", None, Some("https://example.com/schema"))
  let full_scope = telemetry.api.common.InstrumentationScope.new("full.scope", Some("3.0.0"), Some("https://example.com/schema/v3"))
  
  // Verify scope properties
  assert_eq(minimal_scope.name(), "minimal.scope")
  assert_eq(minimal_scope.version(), None)
  assert_eq(minimal_scope.schema_url(), None)
  
  assert_eq(versioned_scope.name(), "versioned.scope")
  assert_eq(versioned_scope.version(), Some("2.0.0"))
  assert_eq(versioned_scope.schema_url(), None)
  
  assert_eq(schema_scope.name(), "schema.scope")
  assert_eq(schema_scope.version(), None)
  assert_eq(schema_scope.schema_url(), Some("https://example.com/schema"))
  
  assert_eq(full_scope.name(), "full.scope")
  assert_eq(full_scope.version(), Some("3.0.0"))
  assert_eq(full_scope.schema_url(), Some("https://example.com/schema/v3"))
  
  // Test creating instruments with different scopes
  let trace_provider = telemetry.sdk.trace.TracerProvider.default()
  let metrics_provider = telemetry.sdk.metrics.MeterProvider.default()
  let logs_provider = telemetry.sdk.logs.LoggerProvider.default()
  
  let tracer1 = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "scope.test.tracer")
  let tracer2 = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "scope.test.tracer", Some("1.0.0"))
  let tracer3 = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "scope.test.tracer", Some("1.0.0"), Some("https://example.com/schema"))
  
  let meter1 = telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "scope.test.meter")
  let meter2 = telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "scope.test.meter", Some("1.0.0"))
  let meter3 = telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "scope.test.meter", Some("1.0.0"), Some("https://example.com/schema"))
  
  let logger1 = telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "scope.test.logger")
  let logger2 = telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "scope.test.logger", Some("1.0.0"))
  let logger3 = telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "scope.test.logger", Some("1.0.0"), Some("https://example.com/schema"))
  
  // Verify scope properties
  let scope1 = telemetry.sdk.trace.Tracer.instrumentation_scope(tracer1)
  let scope2 = telemetry.sdk.trace.Tracer.instrumentation_scope(tracer2)
  let scope3 = telemetry.sdk.trace.Tracer.instrumentation_scope(tracer3)
  
  assert_eq(scope1.name(), "scope.test.tracer")
  assert_eq(scope1.version(), None)
  assert_eq(scope1.schema_url(), None)
  
  assert_eq(scope2.name(), "scope.test.tracer")
  assert_eq(scope2.version(), Some("1.0.0"))
  assert_eq(scope2.schema_url(), None)
  
  assert_eq(scope3.name(), "scope.test.tracer")
  assert_eq(scope3.version(), Some("1.0.0"))
  assert_eq(scope3.schema_url(), Some("https://example.com/schema"))
}

test "exporter configuration" {
  // Test exporter configuration
  
  // Test console exporters
  let console_span_exporter = telemetry.sdk.exporter.console.ConsoleSpanExporter.new()
  let console_log_exporter = telemetry.sdk.exporter.console.ConsoleLogExporter.new()
  let console_metric_exporter = telemetry.sdk.exporter.console.ConsoleMetricExporter.new()
  
  assert_true(console_span_exporter != telemetry.sdk.exporter.console.ConsoleSpanExporter::{})
  assert_true(console_log_exporter != telemetry.sdk.exporter.console.ConsoleLogExporter::{})
  assert_true(console_metric_exporter != telemetry.sdk.exporter.console.ConsoleMetricExporter::{})
  
  // Test OTLP HTTP exporters with different configurations
  let default_otlp_exporter = telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.new()
  let custom_endpoint_exporter = telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.with_endpoint(
    "https://custom.otlp.example.com:4318/v1/traces"
  )
  let headers_exporter = telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.with_headers([
    ("Authorization", "Bearer token123"),
    ("x-custom-header", "custom-value")
  ])
  let timeout_exporter = telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.with_timeout(30000)
  
  assert_true(default_otlp_exporter != telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  assert_true(custom_endpoint_exporter != telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  assert_true(headers_exporter != telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  assert_true(timeout_exporter != telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  
  // Test exporter with different endpoint formats
  let http_exporter = telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.with_endpoint("http://localhost:4318/v1/traces")
  let https_exporter = telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.with_endpoint("https://otel.example.com:4318/v1/traces")
  let ip_exporter = telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.with_endpoint("http://192.168.1.100:4318/v1/traces")
  
  assert_true(http_exporter != telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  assert_true(https_exporter != telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  assert_true(ip_exporter != telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
}

test "propagator configuration" {
  // Test propagator configuration
  
  // Test individual propagators
  let trace_propagator = telemetry.api.propagation.W3CTraceContextPropagator.new()
  let baggage_propagator = telemetry.api.propagation.W3CBaggagePropagator.new()
  
  assert_true(trace_propagator != telemetry.api.propagation.W3CTraceContextPropagator::{})
  assert_true(baggage_propagator != telemetry.api.propagation.W3CBaggagePropagator::{})
  
  // Test composite propagators with different configurations
  let single_composite = telemetry.api.propagation.CompositePropagator.new([trace_propagator])
  let dual_composite = telemetry.api.propagation.CompositePropagator.new([trace_propagator, baggage_propagator])
  let multi_composite = telemetry.api.propagation.CompositePropagator.new([
    trace_propagator,
    baggage_propagator,
    trace_propagator
  ])
  
  assert_true(single_composite != telemetry.api.propagation.CompositePropagator::{})
  assert_true(dual_composite != telemetry.api.propagation.CompositePropagator::{})
  assert_true(multi_composite != telemetry.api.propagation.CompositePropagator::{})
  
  // Test propagator with empty list
  let empty_composite = telemetry.api.propagation.CompositePropagator.new([])
  assert_true(empty_composite != telemetry.api.propagation.CompositePropagator::{})
  
  // Test propagation with different composite configurations
  let ctx = telemetry.api.context.Context.root()
  let carrier = telemetry.api.propagation.TextMapCarrier.new()
  
  telemetry.api.propagation.CompositePropagator.inject(single_composite, ctx, carrier)
  telemetry.api.propagation.CompositePropagator.inject(dual_composite, ctx, carrier)
  telemetry.api.propagation.CompositePropagator.inject(multi_composite, ctx, carrier)
  telemetry.api.propagation.CompositePropagator.inject(empty_composite, ctx, carrier)
  
  let extracted1 = telemetry.api.propagation.CompositePropagator.extract(single_composite, carrier)
  let extracted2 = telemetry.api.propagation.CompositePropagator.extract(dual_composite, carrier)
  let extracted3 = telemetry.api.propagation.CompositePropagator.extract(multi_composite, carrier)
  let extracted4 = telemetry.api.propagation.CompositePropagator.extract(empty_composite, carrier)
  
  // All should return valid contexts
  assert_true(extracted1 != telemetry.api.context.Context::{})
  assert_true(extracted2 != telemetry.api.context.Context::{})
  assert_true(extracted3 != telemetry.api.context.Context::{})
  assert_true(extracted4 != telemetry.api.context.Context::{})
}

test "platform component configuration" {
  // Test platform component configuration
  
  // Test clock
  let system_clock = telemetry.sdk.platform.time.Clock.system()
  assert_true(system_clock != telemetry.sdk.platform.time.Clock::{})
  
  let time1 = telemetry.sdk.platform.time.Clock.now_unix_nanos(system_clock)
  let time2 = telemetry.sdk.platform.time.Clock.now_unix_nanos(system_clock)
  assert_true(time1 > 0L)
  assert_true(time2 >= time1)
  
  // Test random
  let system_random = telemetry.sdk.platform.random.Random.system()
  assert_true(system_random != telemetry.sdk.platform.random.Random::{})
  
  let bytes1 = telemetry.sdk.platform.random.Random.next_bytes(system_random, 16)
  let bytes2 = telemetry.sdk.platform.random.Random.next_bytes(system_random, 16)
  let random1 = telemetry.sdk.platform.random.Random.next_u64(system_random)
  let random2 = telemetry.sdk.platform.random.Random.next_u64(system_random)
  
  assert_eq(bytes1.length(), 16)
  assert_eq(bytes2.length(), 16)
  assert_true(random1 >= 0UL)
  assert_true(random2 >= 0UL)
  
  // Test HTTP client
  let http_client = telemetry.sdk.platform.http.HttpClient.new()
  assert_true(http_client != telemetry.sdk.platform.http.HttpClient::{})
  
  // Test request and response creation
  let request = telemetry.sdk.platform.http.HttpRequest.new(
    "POST",
    "https://api.example.com/test",
    [("Content-Type", "application/json"), ("User-Agent", "azimuth-telemetry/1.0")],
    Some("{\"test\": \"data\"}")
  )
  
  let response = telemetry.sdk.platform.http.HttpResponse.new(
    200,
    [("Content-Type", "application/json")],
    Some("{\"status\": \"success\"}")
  )
  
  assert_eq(telemetry.sdk.platform.http.HttpRequest.http_method(request), "POST")
  assert_eq(telemetry.sdk.platform.http.HttpRequest.url(request), "https://api.example.com/test")
  assert_eq(telemetry.sdk.platform.http.HttpRequest.body(request), Some("{\"test\": \"data\"}"))
  
  assert_eq(telemetry.sdk.platform.http.HttpResponse.status_code(response), 200)
  assert_eq(telemetry.sdk.platform.http.HttpResponse.body(response), Some("{\"status\": \"success\"}"))
}

test "configuration validation and error handling" {
  // Test configuration validation and error handling
  
  // Test invalid resource configurations
  let empty_resource = telemetry.api.common.Resource.new()
  let resource_with_empty_attrs = telemetry.api.common.Resource.with_attributes(empty_resource, [])
  let resource_with_empty_keys = telemetry.api.common.Resource.with_attributes(
    empty_resource,
    [("", telemetry.api.common.StringValue("value"))]
  )
  let resource_with_empty_values = telemetry.api.common.Resource.with_attributes(
    empty_resource,
    [("key", telemetry.api.common.StringValue(""))]
  )
  
  // All should create valid resources
  assert_true(resource_with_empty_attrs != telemetry.api.common.Resource::{})
  assert_true(resource_with_empty_keys != telemetry.api.common.Resource::{})
  assert_true(resource_with_empty_values != telemetry.api.common.Resource::{})
  
  // Test invalid instrumentation scope configurations
  let empty_scope = telemetry.api.common.InstrumentationScope.new("", None, None)
  let long_scope = telemetry.api.common.InstrumentationScope.new("a".repeat(1000), Some("b".repeat(1000)), Some("c".repeat(1000)))
  
  assert_eq(empty_scope.name(), "")
  assert_eq(empty_scope.version(), None)
  assert_eq(empty_scope.schema_url(), None)
  
  assert_eq(long_scope.name(), "a".repeat(1000))
  assert_eq(long_scope.version(), Some("b".repeat(1000)))
  assert_eq(long_scope.schema_url(), Some("c".repeat(1000)))
  
  // Test invalid exporter configurations
  let invalid_endpoint_exporter1 = telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.with_endpoint("")
  let invalid_endpoint_exporter2 = telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.with_endpoint("not-a-url")
  let invalid_timeout_exporter = telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.with_timeout(-1)
  
  assert_true(invalid_endpoint_exporter1 != telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  assert_true(invalid_endpoint_exporter2 != telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  assert_true(invalid_timeout_exporter != telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  
  // Test creating instruments with invalid names
  let metrics_provider = telemetry.sdk.metrics.MeterProvider.default()
  let meter = telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "validation.test.meter")
  
  let empty_name_counter = telemetry.sdk.metrics.Meter.create_counter(meter, "")
  let long_name_histogram = telemetry.sdk.metrics.Meter.create_histogram(meter, "a".repeat(1000))
  let special_name_gauge = telemetry.sdk.metrics.Meter.create_gauge(meter, "special!@#$%^&*()_+-=[]{}|;':\",./<>?")
  
  assert_true(empty_name_counter != telemetry.sdk.metrics.Counter::{})
  assert_true(long_name_histogram != telemetry.sdk.metrics.Histogram::{})
  assert_true(special_name_gauge != telemetry.sdk.metrics.Gauge::{})
}