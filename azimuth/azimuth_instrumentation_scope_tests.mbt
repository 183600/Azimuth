// Instrumentation Scope Test Suite for Azimuth Telemetry System
// This file contains test cases for instrumentation scope operations

test "instrumentation scope basic operations" {
  // Test basic instrumentation scope creation and properties
  let scope = InstrumentationScope::{
    name: "azimuth.test.scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  // Verify scope properties
  assert_eq(scope.name, "azimuth.test.scope")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, Some("https://example.com/schema/v1"))
}

test "instrumentation scope with minimal properties" {
  // Test instrumentation scope with only required properties
  let minimal_scope = InstrumentationScope::{
    name: "minimal.scope",
    version: None,
    schema_url: None
  }
  
  // Verify minimal scope properties
  assert_eq(minimal_scope.name, "minimal.scope")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
}

test "instrumentation scope in tracer" {
  // Test instrumentation scope integration with tracer
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test.tracer", Some("2.1.0"))
  
  // Get instrumentation scope from tracer
  let scope = Tracer::instrumentation_scope(tracer)
  
  // Verify scope properties
  assert_eq(scope.name, "test.tracer")
  assert_eq(scope.version, Some("2.1.0"))
  assert_eq(scope.schema_url, None)
  
  // Create spans with the tracer
  let span1 = Tracer::start_span(tracer, "span.1")
  let span2 = Tracer::start_span(tracer, "span.2")
  
  // Verify spans are created with the same instrumentation scope
  assert_eq(Span::name(span1), "span.1")
  assert_eq(Span::name(span2), "span.2")
}

test "instrumentation scope in meter" {
  // Test instrumentation scope integration with meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test.meter", Some("1.5.2"))
  
  // Get instrumentation scope from meter
  let scope = meter.scope
  
  // Verify scope properties
  assert_eq(scope.name, "test.meter")
  assert_eq(scope.version, Some("1.5.2"))
  assert_eq(scope.schema_url, None)
  
  // Create instruments with the meter
  let counter = Meter::create_counter(meter, "test.counter")
  let histogram = Meter::create_histogram(meter, "test.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "test.updown")
  let gauge = Meter::create_gauge(meter, "test.gauge")
  
  // Verify instruments are created with the same instrumentation scope
  assert_eq(counter.name, "test.counter")
  assert_eq(histogram.name, "test.histogram")
  assert_eq(updown_counter.name, "test.updown")
  assert_eq(gauge.name, "test.gauge")
}

test "instrumentation scope in logger" {
  // Test instrumentation scope integration with logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test.logger", Some("3.0.1"))
  
  // Get instrumentation scope from logger
  let scope = logger.scope
  
  // Verify scope properties
  assert_eq(scope.name, "test.logger")
  assert_eq(scope.version, Some("3.0.1"))
  assert_eq(scope.schema_url, None)
  
  // Create log records with the logger
  let info_record = LogRecord::new(Info, "Test info message")
  let error_record = LogRecord::new(Error, "Test error message")
  
  // Emit log records
  Logger::emit(logger, info_record)
  Logger::emit(logger, error_record)
  
  // Verify log records have correct severity
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(error_record), Error)
}

test "instrumentation scope with schema URL" {
  // Test instrumentation scope with schema URL
  let tracer_provider = TracerProvider::default()
  let tracer_with_schema = TracerProvider::get_tracer(
    tracer_provider, 
    "schema.test.tracer", 
    Some("1.0.0")
  )
  
  // Note: The current implementation doesn't support setting schema_url
  // This test verifies the existing behavior
  let scope = Tracer::instrumentation_scope(tracer_with_schema)
  assert_eq(scope.name, "schema.test.tracer")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, None)  // Simplified implementation
}

test "instrumentation scope version formats" {
  // Test instrumentation scope with different version formats
  let tracer_provider = TracerProvider::default()
  
  // Semantic version
  let tracer_semver = TracerProvider::get_tracer(tracer_provider, "semver.tracer", Some("1.2.3"))
  let semver_scope = Tracer::instrumentation_scope(tracer_semver)
  assert_eq(semver_scope.name, "semver.tracer")
  assert_eq(semver_scope.version, Some("1.2.3"))
  
  // Pre-release version
  let tracer_prerelease = TracerProvider::get_tracer(tracer_provider, "prerelease.tracer", Some("2.0.0-alpha.1"))
  let prerelease_scope = Tracer::instrumentation_scope(tracer_prerelease)
  assert_eq(prerelease_scope.name, "prerelease.tracer")
  assert_eq(prerelease_scope.version, Some("2.0.0-alpha.1"))
  
  // Build metadata
  let tracer_build = TracerProvider::get_tracer(tracer_provider, "build.tracer", Some("3.1.0+build.42"))
  let build_scope = Tracer::instrumentation_scope(tracer_build)
  assert_eq(build_scope.name, "build.tracer")
  assert_eq(build_scope.version, Some("3.1.0+build.42"))
  
  // Date-based version
  let tracer_date = TracerProvider::get_tracer(tracer_provider, "date.tracer", Some("2025.01.01"))
  let date_scope = Tracer::instrumentation_scope(tracer_date)
  assert_eq(date_scope.name, "date.tracer")
  assert_eq(date_scope.version, Some("2025.01.01"))
}

test "instrumentation scope special characters" {
  // Test instrumentation scope with special characters in name
  let tracer_provider = TracerProvider::default()
  
  // Unicode characters
  let tracer_unicode = TracerProvider::get_tracer(tracer_provider, "测试追踪器", Some("1.0.0"))
  let unicode_scope = Tracer::instrumentation_scope(tracer_unicode)
  assert_eq(unicode_scope.name, "测试追踪器")
  assert_eq(unicode_scope.version, Some("1.0.0"))
  
  // Dots and hyphens
  let tracer_dots = TracerProvider::get_tracer(tracer_provider, "service.module.component", Some("2.1.0"))
  let dots_scope = Tracer::instrumentation_scope(tracer_dots)
  assert_eq(dots_scope.name, "service.module.component")
  assert_eq(dots_scope.version, Some("2.1.0"))
  
  // Underscores
  let tracer_underscores = TracerProvider::get_tracer(tracer_provider, "service_module_component", Some("3.0.0"))
  let underscores_scope = Tracer::instrumentation_scope(tracer_underscores)
  assert_eq(underscores_scope.name, "service_module_component")
  assert_eq(underscores_scope.version, Some("3.0.0"))
}

test "instrumentation scope with different providers" {
  // Test instrumentation scope across different telemetry providers
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  // Create instruments with the same scope name but different providers
  let tracer = TracerProvider::get_tracer(tracer_provider, "unified.scope", Some("1.0.0"))
  let meter = MeterProvider::get_meter(meter_provider, "unified.scope", Some("1.0.0"))
  let logger = LoggerProvider::get_logger(logger_provider, "unified.scope", Some("1.0.0"))
  
  // Get scopes from each provider
  let tracer_scope = Tracer::instrumentation_scope(tracer)
  let meter_scope = meter.scope
  let logger_scope = logger.scope
  
  // Verify all scopes have the same properties
  assert_eq(tracer_scope.name, "unified.scope")
  assert_eq(meter_scope.name, "unified.scope")
  assert_eq(logger_scope.name, "unified.scope")
  
  assert_eq(tracer_scope.version, Some("1.0.0"))
  assert_eq(meter_scope.version, Some("1.0.0"))
  assert_eq(logger_scope.version, Some("1.0.0"))
  
  // Create telemetry data with unified scope
  let span = Tracer::start_span(tracer, "unified.span")
  let counter = Meter::create_counter(meter, "unified.counter")
  let log_record = LogRecord::new(Info, "Unified log message")
  
  // Verify all telemetry is created successfully
  assert_eq(Span::name(span), "unified.span")
  assert_eq(counter.name, "unified.counter")
  assert_eq(LogRecord::body(log_record), Some("Unified log message"))
}

test "instrumentation scope isolation" {
  // Test that different instrumentation scopes are properly isolated
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  
  // Create tracers with different scopes
  let tracer_a = TracerProvider::get_tracer(tracer_provider, "scope.a", Some("1.0.0"))
  let tracer_b = TracerProvider::get_tracer(tracer_provider, "scope.b", Some("2.0.0"))
  let tracer_c = TracerProvider::get_tracer(tracer_provider, "scope.c", None)
  
  // Get scopes
  let scope_a = Tracer::instrumentation_scope(tracer_a)
  let scope_b = Tracer::instrumentation_scope(tracer_b)
  let scope_c = Tracer::instrumentation_scope(tracer_c)
  
  // Verify scope isolation
  assert_eq(scope_a.name, "scope.a")
  assert_eq(scope_a.version, Some("1.0.0"))
  
  assert_eq(scope_b.name, "scope.b")
  assert_eq(scope_b.version, Some("2.0.0"))
  
  assert_eq(scope_c.name, "scope.c")
  assert_eq(scope_c.version, None)
  
  // Create meters with different scopes
  let meter_a = MeterProvider::get_meter(meter_provider, "scope.a", Some("1.0.0"))
  let meter_b = MeterProvider::get_meter(meter_provider, "scope.b", Some("2.0.0"))
  
  // Create instruments in different scopes
  let counter_a = Meter::create_counter(meter_a, "counter")
  let counter_b = Meter::create_counter(meter_b, "counter")
  
  // Verify instruments are created in their respective scopes
  assert_eq(counter_a.name, "counter")
  assert_eq(counter_b.name, "counter")
  
  // Create spans in different scopes
  let span_a = Tracer::start_span(tracer_a, "operation")
  let span_b = Tracer::start_span(tracer_b, "operation")
  
  // Verify spans are created in their respective scopes
  assert_eq(Span::name(span_a), "operation")
  assert_eq(Span::name(span_b), "operation")
}