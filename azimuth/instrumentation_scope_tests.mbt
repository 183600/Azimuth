// Instrumentation Scope Tests for Azimuth Telemetry System
// Comprehensive testing of instrumentation scope with version and schema URL handling

test "instrumentation_scope_basic_operations" {
  // Test instrumentation scope with minimal information
  let minimal_scope = InstrumentationScope::{
    name: "test.instrument",
    version: None,
    schema_url: None
  }
  
  assert_eq(minimal_scope.name, "test.instrument")
  assert_true(minimal_scope.version is None)
  assert_true(minimal_scope.schema_url is None)
  
  // Test instrumentation scope with version only
  let version_scope = InstrumentationScope::{
    name: "versioned.instrument",
    version: Some("1.0.0"),
    schema_url: None
  }
  
  assert_eq(version_scope.name, "versioned.instrument")
  match version_scope.version {
    Some(v) => assert_eq(v, "1.0.0")
    _ => assert_true(false)
  }
  assert_true(version_scope.schema_url is None)
  
  // Test instrumentation scope with schema URL only
  let schema_scope = InstrumentationScope::{
    name: "schema.instrument",
    version: None,
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  assert_eq(schema_scope.name, "schema.instrument")
  assert_true(schema_scope.version is None)
  match schema_scope.schema_url {
    Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.20.0")
    _ => assert_true(false)
  }
  
  // Test instrumentation scope with both version and schema URL
  let full_scope = InstrumentationScope::{
    name: "full.instrument",
    version: Some("2.1.3"),
    schema_url: Some("https://opentelemetry.io/schemas/1.21.0")
  }
  
  assert_eq(full_scope.name, "full.instrument")
  match full_scope.version {
    Some(v) => assert_eq(v, "2.1.3")
    _ => assert_true(false)
  }
  match full_scope.schema_url {
    Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.21.0")
    _ => assert_true(false)
  }
}

test "instrumentation_scope_version_formats" {
  // Test different version formats
  let version_formats = [
    ("1.0.0", "Semantic version"),
    ("v2.1.3", "Version with v prefix"),
    ("2023.12.01", "Date-based version"),
    ("1.0.0-alpha", "Pre-release version"),
    ("1.0.0-beta.2", "Pre-release with build"),
    ("1.0.0+build.123", "Build metadata"),
    ("latest", "Latest version"),
    ("dev", "Development version"),
    ("", "Empty version string")
  ]
  
  for (version, description) in version_formats {
    let scope = InstrumentationScope::{
      name: "version.test.instrument",
      version: Some(version),
      schema_url: None
    }
    
    match scope.version {
      Some(v) => assert_eq(v, version)
      _ => assert_true(false)
    }
  }
}

test "instrumentation_scope_schema_url_formats" {
  // Test different schema URL formats
  let schema_urls = [
    ("https://opentelemetry.io/schemas/1.20.0", "Official OpenTelemetry schema"),
    ("http://localhost:8080/schemas/custom/v1", "Local custom schema"),
    ("https://example.com/telemetry/schema.json", "Custom JSON schema"),
    ("https://api.company.com/schemas/v2.1", "Company API schema"),
    ("urn:example:telemetry:schema:1.0", "URN-based schema"),
    ("", "Empty schema URL"),
    ("file:///etc/telemetry/schema.json", "Local file schema"),
    ("https://schemas.example.com/telemetry/v1.0.0", "Versioned schema")
  ]
  
  for (schema_url, description) in schema_urls {
    let scope = InstrumentationScope::{
      name: "schema.test.instrument",
      version: None,
      schema_url: Some(schema_url)
    }
    
    match scope.schema_url {
      Some(url) => assert_eq(url, schema_url)
      _ => assert_true(false)
    }
  }
}

test "instrumentation_scope_with_providers" {
  // Test instrumentation scope with tracer provider
  let tracer_provider = TracerProvider::default()
  let tracer_scope = InstrumentationScope::{
    name: "azimuth.tracer",
    version: Some("1.2.3"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  let tracer = Tracer::{ scope: tracer_scope }
  
  assert_eq(Tracer::instrumentation_scope(tracer).name, "azimuth.tracer")
  match Tracer::instrumentation_scope(tracer).version {
    Some(v) => assert_eq(v, "1.2.3")
    _ => assert_true(false)
  }
  match Tracer::instrumentation_scope(tracer).schema_url {
    Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.20.0")
    _ => assert_true(false)
  }
  
  // Test instrumentation scope with meter provider
  let meter_provider = MeterProvider::default()
  let meter_scope = InstrumentationScope::{
    name: "azimuth.meter",
    version: Some("2.1.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.21.0")
  }
  let meter = Meter::{ scope: meter_scope }
  
  assert_eq(meter.scope.name, "azimuth.meter")
  match meter.scope.version {
    Some(v) => assert_eq(v, "2.1.0")
    _ => assert_true(false)
  }
  match meter.scope.schema_url {
    Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.21.0")
    _ => assert_true(false)
  }
  
  // Test instrumentation scope with logger provider
  let logger_provider = LoggerProvider::noop()
  let logger_scope = InstrumentationScope::{
    name: "azimuth.logger",
    version: Some("0.9.5"),
    schema_url: Some("https://opentelemetry.io/schemas/1.19.0")
  }
  let logger = Logger::{ scope: logger_scope }
  
  assert_eq(logger.scope.name, "azimuth.logger")
  match logger.scope.version {
    Some(v) => assert_eq(v, "0.9.5")
    _ => assert_true(false)
  }
  match logger.scope.schema_url {
    Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.19.0")
    _ => assert_true(false)
  }
}

test "instrumentation_scope_special_characters" {
  // Test instrumentation scope names with special characters
  let special_names = [
    ("azimuth-service", "Hyphen in name"),
    ("azimuth_service", "Underscore in name"),
    ("azimuth.service", "Dot in name"),
    ("azimuth/service", "Slash in name"),
    ("azimuth.service.v1", "Version in name"),
    ("com.example.azimuth", "Reverse domain name"),
    ("azimuthService", "Camel case name"),
    ("azimuth_service_v2", "Snake case with version"),
    ("AZIMUTH_SERVICE", "Upper case name"),
    ("azimuth-123", "Name with numbers")
  ]
  
  for (name, description) in special_names {
    let scope = InstrumentationScope::{
      name: name,
      version: Some("1.0.0"),
      schema_url: Some("https://example.com/schema")
    }
    
    assert_eq(scope.name, name)
    match scope.version {
      Some(v) => assert_eq(v, "1.0.0")
      _ => assert_true(false)
    }
    match scope.schema_url {
      Some(url) => assert_eq(url, "https://example.com/schema")
      _ => assert_true(false)
    }
  }
}

test "instrumentation_scope_edge_cases" {
  // Test empty name
  let empty_name_scope = InstrumentationScope::{
    name: "",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  assert_eq(empty_name_scope.name, "")
  
  // Test very long name
  let long_name = "very.long.instrumentation.name.that.exceeds.normal.length.expectations.and.tests.system.boundaries" * 2
  let long_name_scope = InstrumentationScope::{
    name: long_name,
    version: Some("1.0.0"),
    schema_url: None
  }
  assert_eq(long_name_scope.name, long_name)
  
  // Test name with Unicode characters
  let unicode_name = "遥测仪器.测试.中文"
  let unicode_scope = InstrumentationScope::{
    name: unicode_name,
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  assert_eq(unicode_scope.name, unicode_name)
  
  // Test name with emojis
  let emoji_name = "azimuth.instrument.🚀.📊.🔍"
  let emoji_scope = InstrumentationScope::{
    name: emoji_name,
    version: Some("1.0.0"),
    schema_url: None
  }
  assert_eq(emoji_scope.name, emoji_name)
  
  // Test very long version
  let long_version = "1.0.0-alpha.1+build.1234567890.abcdef.very.long.version.string"
  let long_version_scope = InstrumentationScope::{
    name: "long.version.test",
    version: Some(long_version),
    schema_url: None
  }
  match long_version_scope.version {
    Some(v) => assert_eq(v, long_version)
    _ => assert_true(false)
  }
  
  // Test very long schema URL
  let long_schema = "https://very.long.schema.url.example.com/that/exceeds/normal/length/expectations/and/tests/system/boundaries/with/many/path/segments/v1.0.0/schema.json"
  let long_schema_scope = InstrumentationScope::{
    name: "long.schema.test",
    version: None,
    schema_url: Some(long_schema)
  }
  match long_schema_scope.schema_url {
    Some(url) => assert_eq(url, long_schema)
    _ => assert_true(false)
  }
}

test "instrumentation_scope_provider_integration" {
  // Test creating scopes through different providers
  let tracer_provider = TracerProvider::default()
  let tracer1 = TracerProvider::get_tracer(tracer_provider, "basic.tracer")
  let tracer2 = TracerProvider::get_tracer(tracer_provider, "versioned.tracer", Some("2.1.0"))
  let tracer3 = TracerProvider::get_tracer(tracer_provider, "full.tracer", Some("3.0.0"))
  
  assert_eq(tracer1.scope.name, "basic.tracer")
  assert_true(tracer1.scope.version is None)
  assert_true(tracer1.scope.schema_url is None)
  
  assert_eq(tracer2.scope.name, "versioned.tracer")
  match tracer2.scope.version {
    Some(v) => assert_eq(v, "2.1.0")
    _ => assert_true(false)
  }
  assert_true(tracer2.scope.schema_url is None)
  
  assert_eq(tracer3.scope.name, "full.tracer")
  match tracer3.scope.version {
    Some(v) => assert_eq(v, "3.0.0")
    _ => assert_true(false)
  }
  assert_true(tracer3.scope.schema_url is None)
  
  // Test meter provider
  let meter_provider = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(meter_provider, "basic.meter")
  let meter2 = MeterProvider::get_meter(meter_provider, "scoped.meter")
  
  assert_eq(meter1.scope.name, "basic.meter")
  assert_true(meter1.scope.version is None)
  assert_true(meter1.scope.schema_url is None)
  
  assert_eq(meter2.scope.name, "scoped.meter")
  assert_true(meter2.scope.version is None)
  assert_true(meter2.scope.schema_url is None)
  
  // Test logger provider
  let logger_provider = LoggerProvider::noop()
  let logger1 = LoggerProvider::get_logger(logger_provider, "basic.logger")
  let logger2 = LoggerProvider::get_logger(logger_provider, "scoped.logger")
  
  assert_eq(logger1.scope.name, "basic.logger")
  assert_true(logger1.scope.version is None)
  assert_true(logger1.scope.schema_url is None)
  
  assert_eq(logger2.scope.name, "scoped.logger")
  assert_true(logger2.scope.version is None)
  assert_true(logger2.scope.schema_url is None)
}