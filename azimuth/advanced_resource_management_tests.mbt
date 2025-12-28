// Advanced Resource Management Tests for Azimuth Telemetry System
// Testing advanced resource lifecycle, attribute handling, and edge cases

test "resource_attribute_inheritance_and_overriding" {
  // Test resource attribute inheritance chain
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.namespace", StringValue("default")),
    ("service.version", StringValue("1.0.0"))
  ]
  
  let inherited_resource = Resource::with_attributes(base_resource, base_attrs)
  
  // Test attribute overriding
  let override_attrs = [
    ("service.version", StringValue("2.0.0")),
    ("deployment.environment", StringValue("production"))
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(inherited_resource, override_resource)
  
  // Verify inheritance and overriding behavior
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("base-service")))
  assert_eq(Resource::get_attribute(merged_resource, "service.namespace"), Some(StringValue("default")))
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), Some(StringValue("2.0.0")))
  assert_eq(Resource::get_attribute(merged_resource, "deployment.environment"), Some(StringValue("production")))
}

test "resource_attribute_type_consistency" {
  // Test resource attribute type consistency across operations
  let resource = Resource::new()
  
  // Add attributes of different types
  let mixed_attrs = [
    ("string.attr", StringValue("test")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14)),
    ("bool.attr", BoolValue(true)),
    ("array.string", ArrayStringValue(["a", "b", "c"])),
    ("array.int", ArrayIntValue([1, 2, 3]))
  ]
  
  let resource_with_mixed = Resource::with_attributes(resource, mixed_attrs)
  
  // Verify type consistency after merge operations
  let additional_attrs = [
    ("new.string", StringValue("new_test")),
    ("new.int", IntValue(100))
  ]
  
  let additional_resource = Resource::with_attributes(Resource::new(), additional_attrs)
  let final_resource = Resource::merge(resource_with_mixed, additional_resource)
  
  // Verify all original attributes maintain their types
  assert_eq(Resource::get_attribute(final_resource, "string.attr"), Some(StringValue("test")))
  assert_eq(Resource::get_attribute(final_resource, "int.attr"), Some(IntValue(42)))
  assert_eq(Resource::get_attribute(final_resource, "float.attr"), Some(FloatValue(3.14)))
  assert_eq(Resource::get_attribute(final_resource, "bool.attr"), Some(BoolValue(true)))
  
  // Verify new attributes are added correctly
  assert_eq(Resource::get_attribute(final_resource, "new.string"), Some(StringValue("new_test")))
  assert_eq(Resource::get_attribute(final_resource, "new.int"), Some(IntValue(100)))
}

test "resource_lifecycle_with_instrumentation_scopes" {
  // Test resource lifecycle with different instrumentation scopes
  let resource = Resource::new()
  let service_attrs = [
    ("service.name", StringValue("lifecycle-test-service")),
    ("service.instance.id", StringValue("instance-12345"))
  ]
  
  let service_resource = Resource::with_attributes(resource, service_attrs)
  
  // Create tracer with resource-based scope
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle-tracer", Some("1.2.3"))
  let span = Tracer::start_span(tracer, "lifecycle-test-span")
  
  // Create meter with resource-based scope
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "lifecycle-meter")
  let counter = Meter::create_counter(meter, "lifecycle.counter")
  
  // Create logger with resource-based scope
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "lifecycle-logger")
  
  // Verify all components work with the same resource context
  assert_eq(Span::name(span), "lifecycle-test-span")
  assert_eq(Instrument::name(Counter(counter.name, counter.description, counter.unit)), "lifecycle.counter")
  
  // Test resource cleanup simulation
  Span::end(span)
  
  // Verify resource attributes are still accessible after span end
  assert_eq(Resource::get_attribute(service_resource, "service.name"), Some(StringValue("lifecycle-test-service")))
  assert_eq(Resource::get_attribute(service_resource, "service.instance.id"), Some(StringValue("instance-12345")))
}

test "resource_attribute_deep_nesting" {
  // Test resource with deeply nested attribute structures
  let resource = Resource::new()
  
  // Create nested attribute structures
  let nested_attrs = [
    ("level1.level2.level3.string", StringValue("deeply.nested.value")),
    ("level1.level2.level3.int", IntValue(999)),
    ("level1.level2.level3.float", FloatValue(9.99)),
    ("level1.level2.level3.bool", BoolValue(false))
  ]
  
  let nested_resource = Resource::with_attributes(resource, nested_attrs)
  
  // Test retrieving deeply nested attributes
  assert_eq(Resource::get_attribute(nested_resource, "level1.level2.level3.string"), Some(StringValue("deeply.nested.value")))
  assert_eq(Resource::get_attribute(nested_resource, "level1.level2.level3.int"), Some(IntValue(999)))
  assert_eq(Resource::get_attribute(nested_resource, "level1.level2.level3.float"), Some(FloatValue(9.99)))
  assert_eq(Resource::get_attribute(nested_resource, "level1.level2.level3.bool"), Some(BoolValue(false)))
  
  // Test partial path matching (should return None)
  assert_eq(Resource::get_attribute(nested_resource, "level1.level2"), None)
  assert_eq(Resource::get_attribute(nested_resource, "level1.level2.level3.nonexistent"), None)
  
  // Test merging resources with nested attributes
  let additional_nested = [
    ("level1.level2.level3.new", StringValue("new.nested.value")),
    ("level1.level2.different", StringValue("different.level.value"))
  ]
  
  let additional_resource = Resource::with_attributes(Resource::new(), additional_nested)
  let merged_nested = Resource::merge(nested_resource, additional_resource)
  
  // Verify merge preserves all nested attributes
  assert_eq(Resource::get_attribute(merged_nested, "level1.level2.level3.string"), Some(StringValue("deeply.nested.value")))
  assert_eq(Resource::get_attribute(merged_nested, "level1.level2.level3.new"), Some(StringValue("new.nested.value")))
  assert_eq(Resource::get_attribute(merged_nested, "level1.level2.different"), Some(StringValue("different.level.value")))
}

test "resource_attribute_validation_and_sanitization" {
  // Test resource attribute validation and sanitization
  let resource = Resource::new()
  
  // Test attributes with special characters
  let special_char_attrs = [
    ("key.with.dots", StringValue("value.with.dots")),
    ("key_with_underscores", StringValue("value_with_underscores")),
    ("key-with-dashes", StringValue("value-with-dashes")),
    ("key_with spaces", StringValue("value with spaces")),
    ("key/with/slashes", StringValue("value/with/slashes")),
    ("key\\with\\backslashes", StringValue("value\\with\\backslashes")),
    ("key.with.symbols!@#$%^&*()", StringValue("value.with.symbols!@#$%^&*()"))
  ]
  
  let special_resource = Resource::with_attributes(resource, special_char_attrs)
  
  // Verify special characters are preserved
  assert_eq(Resource::get_attribute(special_resource, "key.with.dots"), Some(StringValue("value.with.dots")))
  assert_eq(Resource::get_attribute(special_resource, "key_with_underscores"), Some(StringValue("value_with_underscores")))
  assert_eq(Resource::get_attribute(special_resource, "key-with-dashes"), Some(StringValue("value-with-dashes")))
  assert_eq(Resource::get_attribute(special_resource, "key_with spaces"), Some(StringValue("value with spaces")))
  assert_eq(Resource::get_attribute(special_resource, "key/with/slashes"), Some(StringValue("value/with/slashes")))
  
  // Test attributes with Unicode characters
  let unicode_attrs = [
    ("clé.avec.accents", StringValue("valeur.avec.accents")),
    ("ключ.на.русском", StringValue("значение.на.русском")),
    ("キー.日本語", StringValue("値.日本語")),
    ("키.한국어", StringValue("값.한국어")),
    ("مفتاح.عربي", StringValue("قيمة.عربي"))
  ]
  
  let unicode_resource = Resource::with_attributes(Resource::new(), unicode_attrs)
  
  // Verify Unicode characters are preserved
  assert_eq(Resource::get_attribute(unicode_resource, "clé.avec.accents"), Some(StringValue("valeur.avec.accents")))
  assert_eq(Resource::get_attribute(unicode_resource, "ключ.на.русском"), Some(StringValue("значение.на.русском")))
  assert_eq(Resource::get_attribute(unicode_resource, "キー.日本語"), Some(StringValue("値.日本語")))
  assert_eq(Resource::get_attribute(unicode_resource, "키.한국어"), Some(StringValue("값.한국어")))
  assert_eq(Resource::get_attribute(unicode_resource, "مفتاح.عربي"), Some(StringValue("قيمة.عربي")))
}

test "resource_performance_with_large_attribute_sets" {
  // Test resource performance with large attribute sets
  let resource = Resource::new()
  
  // Create a large number of attributes
  let large_attrs = [
    ("attr." + @string.to_string(i), StringValue("value." + @string.to_string(i)))
    for i in range(0, 1000)
  ]
  
  let large_resource = Resource::with_attributes(resource, large_attrs)
  
  // Test retrieval of first, middle, and last attributes
  assert_eq(Resource::get_attribute(large_resource, "attr.0"), Some(StringValue("value.0")))
  assert_eq(Resource::get_attribute(large_resource, "attr.500"), Some(StringValue("value.500")))
  assert_eq(Resource::get_attribute(large_resource, "attr.999"), Some(StringValue("value.999")))
  
  // Test non-existent attribute in large set
  assert_eq(Resource::get_attribute(large_resource, "attr.1000"), None)
  assert_eq(Resource::get_attribute(large_resource, "nonexistent"), None)
  
  // Test merging large resource with another resource
  let additional_attrs = [
    ("new.attr." + @string.to_string(i), StringValue("new.value." + @string.to_string(i)))
    for i in range(0, 100)
  ]
  
  let additional_resource = Resource::with_attributes(Resource::new(), additional_attrs)
  let merged_large = Resource::merge(large_resource, additional_resource)
  
  // Verify merge preserves attributes from both resources
  assert_eq(Resource::get_attribute(merged_large, "attr.0"), Some(StringValue("value.0")))
  assert_eq(Resource::get_attribute(merged_large, "attr.999"), Some(StringValue("value.999")))
  assert_eq(Resource::get_attribute(merged_large, "new.attr.0"), Some(StringValue("new.value.0")))
  assert_eq(Resource::get_attribute(merged_large, "new.attr.99"), Some(StringValue("new.value.99")))
}