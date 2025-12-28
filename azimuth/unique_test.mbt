// Simple Advanced Test Suite for Azimuth Telemetry System

test "unique_advanced_test_12345" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "test.key", StringValue("test_value"))
  let value = Attributes::get(attrs, "test.key")
  assert_eq(value, Some(StringValue("test_value")))
}