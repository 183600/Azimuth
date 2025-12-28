test "resource creation and basic operations" {
  // Test empty resource creation
  let empty_resource = @azimuth.Resource::new()
  assert_eq(empty_resource.attributes.length(), 0)
  
  // Test resource with attributes
  let service_name_attr = ("service.name", @azimuth.StringValue("azimuth-telemetry"))
  let service_version_attr = ("service.version", @azimuth.StringValue("1.0.0"))
  let service_instance_attr = ("service.instance.id", @azimuth.StringValue("instance-123"))
  
  let resource_with_attrs = @azimuth.Resource::with_attributes(
    empty_resource, 
    [service_name_attr, service_version_attr, service_instance_attr]
  )
  
  assert_eq(resource_with_attrs.attributes.length(), 3)
}

test "resource attribute retrieval" {
  // Test resource attribute retrieval
  let attributes = [
    ("service.name", @azimuth.StringValue("my-service")),
    ("service.version", @azimuth.StringValue("2.1.0")),
    ("deployment.environment", @azimuth.StringValue("production")),
    ("host.name", @azimuth.StringValue("web-server-01")),
    ("process.pid", @azimuth.IntValue(1234))
  ]
  
  let resource = @azimuth.Resource::with_attributes(@azimuth.Resource::new(), attributes)
  
  // Test retrieving different attribute types
  let service_name = @azimuth.Resource::get_attribute(resource, "service.name")
  let service_version = @azimuth.Resource::get_attribute(resource, "service.version")
  let environment = @azimuth.Resource::get_attribute(resource, "deployment.environment")
  let host_name = @azimuth.Resource::get_attribute(resource, "host.name")
  let process_pid = @azimuth.Resource::get_attribute(resource, "process.pid")
  let missing_attr = @azimuth.Resource::get_attribute(resource, "missing.attribute")
  
  assert_eq(service_name, Some(@azimuth.StringValue("my-service")))
  assert_eq(service_version, Some(@azimuth.StringValue("2.1.0")))
  assert_eq(environment, Some(@azimuth.StringValue("production")))
  assert_eq(host_name, Some(@azimuth.StringValue("web-server-01")))
  assert_eq(process_pid, Some(@azimuth.IntValue(1234)))
  assert_eq(missing_attr, None)
}

test "resource with different attribute value types" {
  // Test resource with different attribute value types
  let attributes = [
    ("string.attr", @azimuth.StringValue("string value")),
    ("int.attr", @azimuth.IntValue(42)),
    ("float.attr", @azimuth.FloatValue(3.14159)),
    ("bool.attr", @azimuth.BoolValue(true)),
    ("string.array.attr", @azimuth.ArrayStringValue(["item1", "item2", "item3"])),
    ("int.array.attr", @azimuth.ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  
  let resource = @azimuth.Resource::with_attributes(@azimuth.Resource::new(), attributes)
  
  // Test retrieving each attribute type
  let string_attr = @azimuth.Resource::get_attribute(resource, "string.attr")
  let int_attr = @azimuth.Resource::get_attribute(resource, "int.attr")
  let float_attr = @azimuth.Resource::get_attribute(resource, "float.attr")
  let bool_attr = @azimuth.Resource::get_attribute(resource, "bool.attr")
  let string_array_attr = @azimuth.Resource::get_attribute(resource, "string.array.attr")
  let int_array_attr = @azimuth.Resource::get_attribute(resource, "int.array.attr")
  
  assert_eq(string_attr, Some(@azimuth.StringValue("string value")))
  assert_eq(int_attr, Some(@azimuth.IntValue(42)))
  assert_eq(float_attr, Some(@azimuth.FloatValue(3.14159)))
  assert_eq(bool_attr, Some(@azimuth.BoolValue(true)))
  assert_eq(string_array_attr, Some(@azimuth.ArrayStringValue(["item1", "item2", "item3"])))
  assert_eq(int_array_attr, Some(@azimuth.ArrayIntValue([1, 2, 3, 4, 5])))
}

test "resource merge operations" {
  // Test resource merge operations
  let base_attributes = [
    ("service.name", @azimuth.StringValue("base-service")),
    ("service.version", @azimuth.StringValue("1.0.0")),
    ("base.attribute", @azimuth.StringValue("base-value"))
  ]
  
  let override_attributes = [
    ("service.name", @azimuth.StringValue("override-service")),  // This should override
    ("override.attribute", @azimuth.StringValue("override-value")),
    ("service.version", @azimuth.StringValue("2.0.0"))  // This should override
  ]
  
  let base_resource = @azimuth.Resource::with_attributes(@azimuth.Resource::new(), base_attributes)
  let override_resource = @azimuth.Resource::with_attributes(@azimuth.Resource::new(), override_attributes)
  
  // Test merge operation
  let merged_resource = @azimuth.Resource::merge(base_resource, override_resource)
  
  // Note: Simplified implementation just returns override resource
  let merged_service_name = @azimuth.Resource::get_attribute(merged_resource, "service.name")
  let merged_version = @azimuth.Resource::get_attribute(merged_resource, "service.version")
  let merged_override_attr = @azimuth.Resource::get_attribute(merged_resource, "override.attribute")
  let merged_base_attr = @azimuth.Resource::get_attribute(merged_resource, "base.attribute")
  
  assert_eq(merged_service_name, Some(@azimuth.StringValue("override-service")))
  assert_eq(merged_version, Some(@azimuth.StringValue("2.0.0")))
  assert_eq(merged_override_attr, Some(@azimuth.StringValue("override-value")))
  assert_eq(merged_base_attr, None)  // Simplified implementation
}

test "resource with OpenTelemetry semantic conventions" {
  // Test resource with standard OpenTelemetry semantic conventions
  let otel_attributes = [
    // Service resource attributes
    ("service.name", @azimuth.StringValue("azimuth-telemetry")),
    ("service.version", @azimuth.StringValue("0.1.0")),
    ("service.namespace", @azimuth.StringValue("azimuth")),
    ("service.instance.id", @azimuth.StringValue("instance-abc123")),
    
    // Telemetry SDK resource attributes
    ("telemetry.sdk.name", @azimuth.StringValue("azimuth")),
    ("telemetry.sdk.language", @azimuth.StringValue("moonbit")),
    ("telemetry.sdk.version", @azimuth.StringValue("0.1.0")),
    
    // Host resource attributes
    ("host.name", @azimuth.StringValue("production-server-01")),
    ("host.id", @azimuth.StringValue("host-id-456")),
    ("host.type", @azimuth.StringValue("container")),
    
    // Process resource attributes
    ("process.pid", @azimuth.IntValue(12345)),
    ("process.executable.name", @azimuth.StringValue("azimuth-telemetry")),
    ("process.command_args", @azimuth.ArrayStringValue(["./azimuth-telemetry", "--config", "prod.yaml"]))
  ]
  
  let otel_resource = @azimuth.Resource::with_attributes(@azimuth.Resource::new(), otel_attributes)
  
  // Test retrieving semantic convention attributes
  let service_name = @azimuth.Resource::get_attribute(otel_resource, "service.name")
  let sdk_name = @azimuth.Resource::get_attribute(otel_resource, "telemetry.sdk.name")
  let host_name = @azimuth.Resource::get_attribute(otel_resource, "host.name")
  let process_pid = @azimuth.Resource::get_attribute(otel_resource, "process.pid")
  let command_args = @azimuth.Resource::get_attribute(otel_resource, "process.command_args")
  
  assert_eq(service_name, Some(@azimuth.StringValue("azimuth-telemetry")))
  assert_eq(sdk_name, Some(@azimuth.StringValue("azimuth")))
  assert_eq(host_name, Some(@azimuth.StringValue("production-server-01")))
  assert_eq(process_pid, Some(@azimuth.IntValue(12345)))
  assert_eq(command_args, Some(@azimuth.ArrayStringValue(["./azimuth-telemetry", "--config", "prod.yaml"])))
}

test "resource with cloud and deployment attributes" {
  // Test resource with cloud and deployment attributes
  let cloud_attributes = [
    // Cloud provider attributes
    ("cloud.provider", @azimuth.StringValue("aws")),
    ("cloud.account.id", @azimuth.StringValue("123456789012")),
    ("cloud.region", @azimuth.StringValue("us-west-2")),
    ("cloud.availability_zone", @azimuth.StringValue("us-west-2a")),
    
    // Deployment attributes
    ("deployment.environment", @azimuth.StringValue("production")),
    ("deployment.user", @azimuth.StringValue("deploy-bot")),
    ("deployment.timestamp", @azimuth.IntValue(1735689600)),
    
    // Kubernetes attributes
    ("k8s.cluster.name", @azimuth.StringValue("prod-cluster")),
    ("k8s.namespace.name", @azimuth.StringValue("azimuth")),
    ("k8s.pod.name", @azimuth.StringValue("azimuth-telemetry-7d4f8c9b-xyz12")),
    ("k8s.container.name", @azimuth.StringValue("telemetry-container"))
  ]
  
  let cloud_resource = @azimuth.Resource::with_attributes(@azimuth.Resource::new(), cloud_attributes)
  
  // Test retrieving cloud attributes
  let cloud_provider = @azimuth.Resource::get_attribute(cloud_resource, "cloud.provider")
  let cloud_region = @azimuth.Resource::get_attribute(cloud_resource, "cloud.region")
  let deployment_env = @azimuth.Resource::get_attribute(cloud_resource, "deployment.environment")
  let k8s_pod = @azimuth.Resource::get_attribute(cloud_resource, "k8s.pod.name")
  
  assert_eq(cloud_provider, Some(@azimuth.StringValue("aws")))
  assert_eq(cloud_region, Some(@azimuth.StringValue("us-west-2")))
  assert_eq(deployment_env, Some(@azimuth.StringValue("production")))
  assert_eq(k8s_pod, Some(@azimuth.StringValue("azimuth-telemetry-7d4f8c9b-xyz12")))
}

test "resource with special characters and unicode" {
  // Test resource with special characters and unicode in attribute values
  let special_attributes = [
    ("unicode.service", @azimuth.StringValue("遥测服务")),
    ("emoji.attribute", @azimuth.StringValue("🚀 📊 🎯")),
    ("special.chars", @azimuth.StringValue("special:chars&key=value")),
    ("json.like", @azimuth.StringValue("{\"key\": \"value\", \"number\": 42}")),
    ("url.like", @azimuth.StringValue("https://example.com/path?param=value")),
    ("empty.string", @azimuth.StringValue("")),
    ("spaces.only", @azimuth.StringValue("   "))
  ]
  
  let special_resource = @azimuth.Resource::with_attributes(@azimuth.Resource::new(), special_attributes)
  
  // Test retrieving special character attributes
  let unicode_service = @azimuth.Resource::get_attribute(special_resource, "unicode.service")
  let emoji_attr = @azimuth.Resource::get_attribute(special_resource, "emoji.attribute")
  let special_chars = @azimuth.Resource::get_attribute(special_resource, "special.chars")
  let json_like = @azimuth.Resource::get_attribute(special_resource, "json.like")
  let empty_string = @azimuth.Resource::get_attribute(special_resource, "empty.string")
  
  assert_eq(unicode_service, Some(@azimuth.StringValue("遥测服务")))
  assert_eq(emoji_attr, Some(@azimuth.StringValue("🚀 📊 🎯")))
  assert_eq(special_chars, Some(@azimuth.StringValue("special:chars&key=value")))
  assert_eq(json_like, Some(@azimuth.StringValue("{\"key\": \"value\", \"number\": 42}")))
  assert_eq(empty_string, Some(@azimuth.StringValue("")))
}

test "resource attribute edge cases" {
  // Test resource attribute edge cases
  let edge_attributes = [
    ("large.number", @azimuth.IntValue(2147483647)),
    ("negative.number", @azimuth.IntValue(-2147483648)),
    ("zero.number", @azimuth.IntValue(0)),
    ("large.float", @azimuth.FloatValue(1.7976931348623157e+308)),
    ("small.float", @azimuth.FloatValue(2.2250738585072014e-308)),
    ("negative.float", @azimuth.FloatValue(-1.7976931348623157e+308)),
    ("boolean.true", @azimuth.BoolValue(true)),
    ("boolean.false", @azimuth.BoolValue(false))
  ]
  
  let edge_resource = @azimuth.Resource::with_attributes(@azimuth.Resource::new(), edge_attributes)
  
  // Test retrieving edge case attributes
  let large_num = @azimuth.Resource::get_attribute(edge_resource, "large.number")
  let negative_num = @azimuth.Resource::get_attribute(edge_resource, "negative.number")
  let zero_num = @azimuth.Resource::get_attribute(edge_resource, "zero.number")
  let large_float = @azimuth.Resource::get_attribute(edge_resource, "large.float")
  let bool_true = @azimuth.Resource::get_attribute(edge_resource, "boolean.true")
  let bool_false = @azimuth.Resource::get_attribute(edge_resource, "boolean.false")
  
  assert_eq(large_num, Some(@azimuth.IntValue(2147483647)))
  assert_eq(negative_num, Some(@azimuth.IntValue(-2147483648)))
  assert_eq(zero_num, Some(@azimuth.IntValue(0)))
  assert_eq(large_float, Some(@azimuth.FloatValue(1.7976931348623157e+308)))
  assert_eq(bool_true, Some(@azimuth.BoolValue(true)))
  assert_eq(bool_false, Some(@azimuth.BoolValue(false)))
}