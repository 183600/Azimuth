test "common attributes creation" {
  // Test basic attribute creation and manipulation
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "string.key", @azimuth.telemetry.api.common.StringValue("test_value"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "int.key", @azimuth.telemetry.api.common.IntValue(42))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "bool.key", @azimuth.telemetry.api.common.BoolValue(true))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "float.key", @azimuth.telemetry.api.common.FloatValue(3.14))
  
  let string_val = @azimuth.telemetry.api.common.Attributes.get(attrs, "string.key")
  let int_val = @azimuth.telemetry.api.common.Attributes.get(attrs, "int.key")
  let bool_val = @azimuth.telemetry.api.common.Attributes.get(attrs, "bool.key")
  let float_val = @azimuth.telemetry.api.common.Attributes.get(attrs, "float.key")
  
  assert_eq(string_val, Some(@azimuth.telemetry.api.common.StringValue("test_value")))
  assert_eq(int_val, Some(@azimuth.telemetry.api.common.IntValue(42)))
  assert_eq(bool_val, Some(@azimuth.telemetry.api.common.BoolValue(true)))
  assert_eq(float_val, Some(@azimuth.telemetry.api.common.FloatValue(3.14)))
}

test "common resource creation" {
  // Test resource creation with default and custom attributes
  let resource = @azimuth.telemetry.api.common.Resource.new()
  
  // Check default service.name is set
  let service_name = @azimuth.telemetry.api.common.Resource.get_attribute(resource, "service.name")
  assert_true(service_name != None)
  
  // Add custom attributes
  let custom_resource = @azimuth.telemetry.api.common.Resource.with_attributes(resource, [
    ("custom.attribute", @azimuth.telemetry.api.common.StringValue("custom_value")),
    ("version", @azimuth.telemetry.api.common.StringValue("1.0.0"))
  ])
  
  let custom_attr = @azimuth.telemetry.api.common.Resource.get_attribute(custom_resource, "custom.attribute")
  assert_eq(custom_attr, Some(@azimuth.telemetry.api.common.StringValue("custom_value")))
}

test "common instrumentation scope" {
  // Test instrumentation scope creation and properties
  let scope = @azimuth.telemetry.api.common.InstrumentationScope.new(
    "test.instrumentation", 
    Some("1.0.0"), 
    Some("https://example.com/schema")
  )
  
  assert_eq(scope.name(), "test.instrumentation")
  assert_eq(scope.version(), Some("1.0.0"))
  assert_eq(scope.schema_url(), Some("https://example.com/schema"))
}

test "common attribute array handling" {
  // Test array attribute values
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.string", @azimuth.telemetry.api.common.ArrayStringValue(["a", "b", "c"]))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.int", @azimuth.telemetry.api.common.ArrayIntValue([1, 2, 3]))
  
  let string_array = @azimuth.telemetry.api.common.Attributes.get(attrs, "array.string")
  let int_array = @azimuth.telemetry.api.common.Attributes.get(attrs, "array.int")
  
  assert_eq(string_array, Some(@azimuth.telemetry.api.common.ArrayStringValue(["a", "b", "c"])))
  assert_eq(int_array, Some(@azimuth.telemetry.api.common.ArrayIntValue([1, 2, 3])))
}

test "resource merge functionality" {
  // Test resource merging with different attributes
  let base_resource = @azimuth.telemetry.api.common.Resource.with_attributes(
    @azimuth.telemetry.api.common.Resource.new(),
    [
      ("base.attr1", @azimuth.telemetry.api.common.StringValue("base_value1")),
      ("common.attr", @azimuth.telemetry.api.common.StringValue("base_common"))
    ]
  )
  
  let override_resource = @azimuth.telemetry.api.common.Resource.with_attributes(
    @azimuth.telemetry.api.common.Resource.new(),
    [
      ("override.attr1", @azimuth.telemetry.api.common.StringValue("override_value1")),
      ("common.attr", @azimuth.telemetry.api.common.StringValue("override_common"))
    ]
  )
  
  let merged_resource = @azimuth.telemetry.api.common.Resource.merge(base_resource, override_resource)
  
  // After merge, we should get the override resource's attributes
  let override_attr = @azimuth.telemetry.api.common.Resource.get_attribute(merged_resource, "override.attr1")
  let common_attr = @azimuth.telemetry.api.common.Resource.get_attribute(merged_resource, "common.attr")
  let base_attr = @azimuth.telemetry.api.common.Resource.get_attribute(merged_resource, "base.attr1")
  
  assert_eq(override_attr, Some(@azimuth.telemetry.api.common.StringValue("override_value1")))
  assert_eq(common_attr, Some(@azimuth.telemetry.api.common.StringValue("override_common")))
  // Base attribute should not exist in simplified merge implementation
  assert_eq(base_attr, None)
}

test "instrumentation scope edge cases" {
  // Test instrumentation scope with optional values
  let scope_with_minimal = @azimuth.telemetry.api.common.InstrumentationScope.new("minimal.scope", None, None)
  assert_eq(scope_with_minimal.name(), "minimal.scope")
  assert_eq(scope_with_minimal.version(), None)
  assert_eq(scope_with_minimal.schema_url(), None)
  
  let scope_with_version_only = @azimuth.telemetry.api.common.InstrumentationScope.new("version.scope", Some("2.0.0"), None)
  assert_eq(scope_with_version_only.name(), "version.scope")
  assert_eq(scope_with_version_only.version(), Some("2.0.0"))
  assert_eq(scope_with_version_only.schema_url(), None)
  
  let scope_with_schema_only = @azimuth.telemetry.api.common.InstrumentationScope.new("schema.scope", None, Some("https://example.com/schema/v1"))
  assert_eq(scope_with_schema_only.name(), "schema.scope")
  assert_eq(scope_with_schema_only.version(), None)
  assert_eq(scope_with_schema_only.schema_url(), Some("https://example.com/schema/v1"))
}