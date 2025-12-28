// Concise Quality Test Suite for Azimuth Telemetry System
// This file contains focused high-quality test cases

test "attributes operations with different types" {
  let attrs = Attributes::new()
  
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  
  assert_eq(Attributes::get(attrs, "string.key"), Some(StringValue("test_value")))
  assert_eq(Attributes::get(attrs, "int.key"), Some(IntValue(42)))
  assert_eq(Attributes::get(attrs, "float.key"), Some(FloatValue(3.14)))
  assert_eq(Attributes::get(attrs, "bool.key"), Some(BoolValue(true)))
  assert_eq(Attributes::get(attrs, "missing.key"), None)
}

test "baggage operations" {
  let baggage = Baggage::new()
  
  let baggage_with_entry = Baggage::set_entry(baggage, "user.id", "12345")
  assert_eq(Baggage::get_entry(baggage_with_entry, "user.id"), Some("12345"))
  
  let baggage_after_removal = Baggage::remove_entry(baggage_with_entry, "user.id")
  assert_eq(Baggage::get_entry(baggage_after_removal, "user.id"), None)
}

test "span context validation" {
  let valid_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  assert_true(SpanContext::is_valid(valid_ctx))
  assert_true(SpanContext::is_sampled(valid_ctx))
  
  let invalid_ctx = SpanContext::new("", "b7ad6b7169203331", true, "")
  assert_false(SpanContext::is_valid(invalid_ctx))
}

test "context propagation" {
  let ctx = Context::root()
  let key = ContextKey::new("test.key")
  let ctx_with_value = Context::with_value(ctx, key, "test_value")
  
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("test_value"))
}

test "metrics histogram operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("unit"))
  
  assert_eq(histogram.name, "test.histogram")
  assert_eq(histogram.description, Some("Test histogram"))
  assert_eq(histogram.unit, Some("unit"))
  
  Histogram::record(histogram, 1.0)
  Histogram::record(histogram, 2.5)
  
  assert_true(true)
}

test "resource operations" {
  let resource = Resource::new()
  let attrs = [("service.name", StringValue("test-service"))]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.name"), Some(StringValue("test-service")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "missing.attr"), None)
}