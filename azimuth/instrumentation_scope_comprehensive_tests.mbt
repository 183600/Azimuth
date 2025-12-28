// Instrumentation Scope Test Cases for Azimuth Telemetry System
// Testing instrumentation scope functionality and scope management

test "instrumentation scope creation and properties" {
  // Test basic scope creation
  let basic_scope = InstrumentationScope::{ 
    name: "test.instrument", 
    version: None, 
    schema_url: None 
  }
  
  assert_eq(basic_scope.name, "test.instrument")
  assert_eq(basic_scope.version, None)
  assert_eq(basic_scope.schema_url, None)
}

test "instrumentation scope with version and schema" {
  // Test scope with all fields populated
  let full_scope = InstrumentationScope::{ 
    name: "library.name", 
    version: Some("1.2.3"), 
    schema_url: Some("https://example.com/schema/v1") 
  }
  
  assert_eq(full_scope.name, "library.name")
  assert_eq(full_scope.version, Some("1.2.3"))
  assert_eq(full_scope.schema_url, Some("https://example.com/schema/v1"))
}

test "instrumentation scope in tracer" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "my.tracer", Some("2.0.0"))
  let scope = Tracer::instrumentation_scope(tracer)
  
  assert_eq(scope.name, "my.tracer")
  assert_eq(scope.version, Some("2.0.0"))
  assert_eq(scope.schema_url, None)
}

test "instrumentation scope in meter" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "metrics.library", Some("3.1.4"))
  let scope = meter.scope
  
  assert_eq(scope.name, "metrics.library")
  assert_eq(scope.version, Some("3.1.4"))
  assert_eq(scope.schema_url, None)
}

test "instrumentation scope in logger" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "logging.component", Some("0.9.0"))
  let scope = logger.scope
  
  assert_eq(scope.name, "logging.component")
  assert_eq(scope.version, Some("0.9.0"))
  assert_eq(scope.schema_url, None)
}

test "instrumentation scope version comparisons" {
  let scope_v1 = InstrumentationScope::{ 
    name: "same.name", 
    version: Some("1.0.0"), 
    schema_url: None 
  }
  
  let scope_v2 = InstrumentationScope::{ 
    name: "same.name", 
    version: Some("2.0.0"), 
    schema_url: None 
  }
  
  let scope_no_version = InstrumentationScope::{ 
    name: "same.name", 
    version: None, 
    schema_url: None 
  }
  
  // Test that scopes with different versions are different
  assert_eq(scope_v1.name, scope_v2.name)
  assert_eq(scope_v1.name, scope_no_version.name)
  assert_true(scope_v1.version != scope_v2.version)
  assert_true(scope_v2.version != scope_no_version.version)
}

test "instrumentation scope schema URL handling" {
  let scope_with_schema = InstrumentationScope::{ 
    name: "schema.test", 
    version: Some("1.0.0"), 
    schema_url: Some("https://opentelemetry.io/schemas/v1.1.0") 
  }
  
  let scope_without_schema = InstrumentationScope::{ 
    name: "schema.test", 
    version: Some("1.0.0"), 
    schema_url: None 
  }
  
  // Test schema URL differences
  assert_eq(scope_with_schema.name, scope_without_schema.name)
  assert_eq(scope_with_schema.version, scope_without_schema.version)
  assert_true(scope_with_schema.schema_url != scope_without_schema.schema_url)
  
  // Test schema URL content
  assert_eq(scope_with_schema.schema_url, Some("https://opentelemetry.io/schemas/v1.1.0"))
  assert_eq(scope_without_schema.schema_url, None)
}

test "instrumentation scope with special characters" {
  // Test scope names with special characters and formats
  let scopes = [
    InstrumentationScope::{ name: "com.example.app", version: Some("1.0.0"), schema_url: None },
    InstrumentationScope::{ name: "my-lib_v2", version: Some("2.1.0"), schema_url: None },
    InstrumentationScope::{ name: "service.name", version: Some("0.1.0-alpha"), schema_url: None },
    InstrumentationScope::{ name: "underscore_name", version: Some("1.0.0-beta.1"), schema_url: None }
  ]
  
  // Verify all scopes are created successfully
  for scope in scopes {
    assert_true(scope.name.length() > 0)
    assert_true(scope.version != None)
  }
  
  assert_eq(scopes.length(), 4)
  assert_eq(scopes[0].name, "com.example.app")
  assert_eq(scopes[1].name, "my-lib_v2")
  assert_eq(scopes[2].name, "service.name")
  assert_eq(scopes[3].name, "underscore_name")
}