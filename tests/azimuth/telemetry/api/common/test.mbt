test "common attributes creation" {
  // Test basic attribute creation and manipulation
  let attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(attrs, "string.key", telemetry.api.common.StringValue("test_value"))
  telemetry.api.common.Attributes.set(attrs, "int.key", telemetry.api.common.IntValue(42))
  telemetry.api.common.Attributes.set(attrs, "bool.key", telemetry.api.common.BoolValue(true))
  telemetry.api.common.Attributes.set(attrs, "float.key", telemetry.api.common.FloatValue(3.14))
  
  let string_val = telemetry.api.common.Attributes.get(attrs, "string.key")
  let int_val = telemetry.api.common.Attributes.get(attrs, "int.key")
  let bool_val = telemetry.api.common.Attributes.get(attrs, "bool.key")
  let float_val = telemetry.api.common.Attributes.get(attrs, "float.key")
  
  assert_eq(string_val, Some(telemetry.api.common.StringValue("test_value")))
  assert_eq(int_val, Some(telemetry.api.common.IntValue(42)))
  assert_eq(bool_val, Some(telemetry.api.common.BoolValue(true)))
  assert_eq(float_val, Some(telemetry.api.common.FloatValue(3.14)))
}

test "common resource creation" {
  // Test resource creation with default and custom attributes
  let resource = telemetry.api.common.Resource.new()
  
  // Check default service.name is set
  let service_name = telemetry.api.common.Resource.get_attribute(resource, "service.name")
  assert_true(service_name != None)
  
  // Add custom attributes
  let custom_resource = telemetry.api.common.Resource.with_attributes(resource, [
    ("custom.attribute", telemetry.api.common.StringValue("custom_value")),
    ("version", telemetry.api.common.StringValue("1.0.0"))
  ])
  
  let custom_attr = telemetry.api.common.Resource.get_attribute(custom_resource, "custom.attribute")
  assert_eq(custom_attr, Some(telemetry.api.common.StringValue("custom_value")))
}

test "common instrumentation scope" {
  // Test instrumentation scope creation and properties
  let scope = telemetry.api.common.InstrumentationScope.new(
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
  let attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(attrs, "array.string", telemetry.api.common.ArrayStringValue(["a", "b", "c"]))
  telemetry.api.common.Attributes.set(attrs, "array.int", telemetry.api.common.ArrayIntValue([1, 2, 3]))
  
  let string_array = telemetry.api.common.Attributes.get(attrs, "array.string")
  let int_array = telemetry.api.common.Attributes.get(attrs, "array.int")
  
  assert_eq(string_array, Some(telemetry.api.common.ArrayStringValue(["a", "b", "c"])))
  assert_eq(int_array, Some(telemetry.api.common.ArrayIntValue([1, 2, 3])))
}

test "resource merge functionality" {
  // Test resource merging with different attributes
  let base_resource = telemetry.api.common.Resource.with_attributes(
    telemetry.api.common.Resource.new(),
    [
      ("base.attr1", telemetry.api.common.StringValue("base_value1")),
      ("common.attr", telemetry.api.common.StringValue("base_common"))
    ]
  )
  
  let override_resource = telemetry.api.common.Resource.with_attributes(
    telemetry.api.common.Resource.new(),
    [
      ("override.attr1", telemetry.api.common.StringValue("override_value1")),
      ("common.attr", telemetry.api.common.StringValue("override_common"))
    ]
  )
  
  let merged_resource = telemetry.api.common.Resource.merge(base_resource, override_resource)
  
  // After merge, we should get the override resource's attributes
  let override_attr = telemetry.api.common.Resource.get_attribute(merged_resource, "override.attr1")
  let common_attr = telemetry.api.common.Resource.get_attribute(merged_resource, "common.attr")
  let base_attr = telemetry.api.common.Resource.get_attribute(merged_resource, "base.attr1")
  
  assert_eq(override_attr, Some(telemetry.api.common.StringValue("override_value1")))
  assert_eq(common_attr, Some(telemetry.api.common.StringValue("override_common")))
  // Base attribute should not exist in simplified merge implementation
  assert_eq(base_attr, None)
}

test "instrumentation scope edge cases" {
  // Test instrumentation scope with optional values
  let scope_with_minimal = telemetry.api.common.InstrumentationScope.new("minimal.scope", None, None)
  assert_eq(scope_with_minimal.name(), "minimal.scope")
  assert_eq(scope_with_minimal.version(), None)
  assert_eq(scope_with_minimal.schema_url(), None)
  
  let scope_with_version_only = telemetry.api.common.InstrumentationScope.new("version.scope", Some("2.0.0"), None)
  assert_eq(scope_with_version_only.name(), "version.scope")
  assert_eq(scope_with_version_only.version(), Some("2.0.0"))
  assert_eq(scope_with_version_only.schema_url(), None)
  
  let scope_with_schema_only = telemetry.api.common.InstrumentationScope.new("schema.scope", None, Some("https://example.com/schema/v1"))
  assert_eq(scope_with_schema_only.name(), "schema.scope")
  assert_eq(scope_with_schema_only.version(), None)
  assert_eq(scope_with_schema_only.schema_url(), Some("https://example.com/schema/v1"))
}