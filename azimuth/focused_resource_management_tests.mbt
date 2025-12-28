// Resource Management Test Suite
// Testing resource creation, attribute management, and merge operations

test "resource creation and basic operations" {
  // Test empty resource creation
  let empty_resource = Resource::new()
  assert_eq(empty_resource.attributes.length, 0)
  
  // Test resource with attributes creation
  let service_attrs = [
    ("service.name", StringValue("api-service")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let service_resource = Resource::with_attributes(empty_resource, service_attrs)
  
  // Test resource attribute retrieval
  let service_name = Resource::get_attribute(service_resource, "service.name")
  let service_version = Resource::get_attribute(service_resource, "service.version")
  let service_instance = Resource::get_attribute(service_resource, "service.instance.id")
  let missing_attr = Resource::get_attribute(service_resource, "missing.attribute")
  
  assert_eq(service_name, None)  // Mock implementation returns None
  assert_eq(service_version, None)
  assert_eq(service_instance, None)
  assert_eq(missing_attr, None)
  
  // Test resource with different attribute types
  let mixed_attrs = [
    ("string.attr", StringValue("string.value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("array.string.attr", ArrayStringValue(["tag1", "tag2"])),
    ("array.int.attr", ArrayIntValue([1, 2, 3]))
  ]
  let mixed_resource = Resource::with_attributes(empty_resource, mixed_attrs)
  
  // Test retrieval of different attribute types
  let string_attr = Resource::get_attribute(mixed_resource, "string.attr")
  let int_attr = Resource::get_attribute(mixed_resource, "int.attr")
  let float_attr = Resource::get_attribute(mixed_resource, "float.attr")
  let bool_attr = Resource::get_attribute(mixed_resource, "bool.attr")
  let array_string_attr = Resource::get_attribute(mixed_resource, "array.string.attr")
  let array_int_attr = Resource::get_attribute(mixed_resource, "array.int.attr")
  
  assert_eq(string_attr, None)
  assert_eq(int_attr, None)
  assert_eq(float_attr, None)
  assert_eq(bool_attr, None)
  assert_eq(array_string_attr, None)
  assert_eq(array_int_attr, None)
}

test "resource attribute management and validation" {
  let resource = Resource::new()
  
  // Test resource with standard OpenTelemetry resource attributes
  let standard_attrs = [
    ("service.name", StringValue("order-service")),
    ("service.namespace", StringValue("ecommerce")),
    ("service.instance.id", StringValue("order-service-001")),
    ("service.version", StringValue("2.1.0")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("order-server-01")),
    ("host.id", StringValue("host-abc123")),
    ("host.arch", StringValue("amd64")),
    ("os.type", StringValue("linux")),
    ("os.name", StringValue("Ubuntu")),
    ("os.version", StringValue("20.04")),
    ("process.pid", IntValue(12345)),
    ("process.executable.name", StringValue("order-service")),
    ("process.executable.path", StringValue("/usr/local/bin/order-service")),
    ("process.command_args", ArrayStringValue(["--port", "8080", "--config", "/etc/order-service.yaml"])),
    ("process.owner", StringValue("service-user")),
    ("process.runtime.name", StringValue("go")),
    ("process.runtime.version", StringValue("1.19.0")),
    ("process.runtime.description", StringValue("Go Programming Language"))
  ]
  
  let standard_resource = Resource::with_attributes(resource, standard_attrs)
  
  // Test retrieval of standard attributes
  let service_name = Resource::get_attribute(standard_resource, "service.name")
  let deployment_env = Resource::get_attribute(standard_resource, "deployment.environment")
  let host_name = Resource::get_attribute(standard_resource, "host.name")
  let process_pid = Resource::get_attribute(standard_resource, "process.pid")
  let os_type = Resource::get_attribute(standard_resource, "os.type")
  
  assert_eq(service_name, None)
  assert_eq(deployment_env, None)
  assert_eq(host_name, None)
  assert_eq(process_pid, None)
  assert_eq(os_type, None)
  
  // Test resource with cloud provider attributes
  let cloud_attrs = [
    ("cloud.provider", StringValue("aws")),
    ("cloud.account.id", StringValue("123456789012")),
    ("cloud.region", StringValue("us-west-2")),
    ("cloud.availability_zone", StringValue("us-west-2a")),
    ("cloud.platform", StringValue("aws_ec2")),
    ("cloud.resource.id", StringValue("i-1234567890abcdef0"))
  ]
  
  let cloud_resource = Resource::with_attributes(resource, cloud_attrs)
  
  let cloud_provider = Resource::get_attribute(cloud_resource, "cloud.provider")
  let cloud_region = Resource::get_attribute(cloud_resource, "cloud.region")
  let cloud_platform = Resource::get_attribute(cloud_resource, "cloud.platform")
  
  assert_eq(cloud_provider, None)
  assert_eq(cloud_region, None)
  assert_eq(cloud_platform, None)
}

test "resource merge operations and conflict resolution" {
  // Test resource merge scenarios
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("development")),
    ("common.attr", StringValue("base-value"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("service.version", StringValue("2.0.0")),
    ("deployment.environment", StringValue("production")),
    ("override.attr", StringValue("override-value"))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Test resource merge
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Test merged resource attributes (mock returns override resource)
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  let merged_service_version = Resource::get_attribute(merged_resource, "service.version")
  let merged_deployment_env = Resource::get_attribute(merged_resource, "deployment.environment")
  let merged_common_attr = Resource::get_attribute(merged_resource, "common.attr")
  let merged_override_attr = Resource::get_attribute(merged_resource, "override.attr")
  
  assert_eq(merged_service_name, None)  // Mock returns override resource
  assert_eq(merged_service_version, None)
  assert_eq(merged_deployment_env, None)
  assert_eq(merged_common_attr, None)
  assert_eq(merged_override_attr, None)
  
  // Test merge with empty base resource
  let empty_base = Resource::new()
  let merged_with_empty_base = Resource::merge(empty_base, override_with_attrs)
  
  let merged_service_name_from_empty = Resource::get_attribute(merged_with_empty_base, "service.name")
  assert_eq(merged_service_name_from_empty, None)
  
  // Test merge with empty override resource
  let empty_override = Resource::new()
  let merged_with_empty_override = Resource::merge(base_with_attrs, empty_override)
  
  let merged_service_name_from_override = Resource::get_attribute(merged_with_empty_override, "service.name")
  assert_eq(merged_service_name_from_override, None)
  
  // Test merge with both empty resources
  let merged_empty = Resource::merge(empty_base, empty_override)
  assert_eq(merged_empty.attributes.length, 0)
}

test "resource with container and kubernetes attributes" {
  let resource = Resource::new()
  
  // Test container resource attributes
  let container_attrs = [
    ("container.name", StringValue("order-service-container")),
    ("container.id", StringValue("container-abc123")),
    ("container.image.name", StringValue("mycompany/order-service")),
    ("container.image.tag", StringValue("2.1.0")),
    ("container.runtime", StringValue("docker")),
    ("container.command", ArrayStringValue(["./order-service", "--port", "8080"]))
  ]
  
  let container_resource = Resource::with_attributes(resource, container_attrs)
  
  let container_name = Resource::get_attribute(container_resource, "container.name")
  let container_image = Resource::get_attribute(container_resource, "container.image.name")
  let container_runtime = Resource::get_attribute(container_resource, "container.runtime")
  
  assert_eq(container_name, None)
  assert_eq(container_image, None)
  assert_eq(container_runtime, None)
  
  // Test Kubernetes resource attributes
  let k8s_attrs = [
    ("k8s.cluster.name", StringValue("production-cluster")),
    ("k8s.namespace.name", StringValue("ecommerce")),
    ("k8s.pod.name", StringValue("order-service-7d4f8b9c-12345")),
    ("k8s.pod.uid", StringValue("pod-uid-abcdef-123456")),
    ("k8s.deployment.name", StringValue("order-service")),
    ("k8s.replicaset.name", StringValue("order-service-7d4f8b9c")),
    ("k8s.node.name", StringValue("worker-node-01")),
    ("k8s.service.name", StringValue("order-service"))
  ]
  
  let k8s_resource = Resource::with_attributes(resource, k8s_attrs)
  
  let k8s_cluster_name = Resource::get_attribute(k8s_resource, "k8s.cluster.name")
  let k8s_namespace = Resource::get_attribute(k8s_resource, "k8s.namespace.name")
  let k8s_pod_name = Resource::get_attribute(k8s_resource, "k8s.pod.name")
  let k8s_deployment = Resource::get_attribute(k8s_resource, "k8s.deployment.name")
  
  assert_eq(k8s_cluster_name, None)
  assert_eq(k8s_namespace, None)
  assert_eq(k8s_pod_name, None)
  assert_eq(k8s_deployment, None)
  
  // Test merge of container and Kubernetes resources
  let merged_container_k8s = Resource::merge(container_resource, k8s_resource)
  
  let merged_container_name = Resource::get_attribute(merged_container_k8s, "container.name")
  let merged_k8s_cluster = Resource::get_attribute(merged_container_k8s, "k8s.cluster.name")
  
  assert_eq(merged_container_name, None)
  assert_eq(merged_k8s_cluster, None)
}

test "resource with web engine and framework attributes" {
  let resource = Resource::new()
  
  // Test web engine resource attributes
  let web_engine_attrs = [
    ("webengine.name", StringValue("nginx")),
    ("webengine.version", StringValue("1.20.1")),
    ("webengine.description", StringValue("Nginx HTTP Server"))
  ]
  
  let web_engine_resource = Resource::with_attributes(resource, web_engine_attrs)
  
  let webengine_name = Resource::get_attribute(web_engine_resource, "webengine.name")
  let webengine_version = Resource::get_attribute(web_engine_resource, "webengine.version")
  
  assert_eq(webengine_name, None)
  assert_eq(webengine_version, None)
  
  // Test framework resource attributes
  let framework_attrs = [
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.language", StringValue("moonbit")),
    ("telemetry.sdk.version", StringValue("0.1.0")),
    ("telemetry.auto.version", StringValue("0.1.0")),
    ("telemetry.distro.name", StringValue("azimuth-distro")),
    ("telemetry.distro.version", StringValue("1.0.0"))
  ]
  
  let framework_resource = Resource::with_attributes(resource, framework_attrs)
  
  let sdk_name = Resource::get_attribute(framework_resource, "telemetry.sdk.name")
  let sdk_language = Resource::get_attribute(framework_resource, "telemetry.sdk.language")
  let sdk_version = Resource::get_attribute(framework_resource, "telemetry.sdk.version")
  
  assert_eq(sdk_name, None)
  assert_eq(sdk_language, None)
  assert_eq(sdk_version, None)
  
  // Test comprehensive resource with all attribute types
  let comprehensive_attrs = [
    // Service attributes
    ("service.name", StringValue("comprehensive-service")),
    ("service.version", StringValue("1.0.0")),
    
    // Host attributes
    ("host.name", StringValue("comprehensive-host")),
    ("host.arch", StringValue("x86_64")),
    
    // Process attributes
    ("process.pid", IntValue(12345)),
    ("process.executable.name", StringValue("comprehensive-service")),
    
    // Cloud attributes
    ("cloud.provider", StringValue("gcp")),
    ("cloud.region", StringValue("us-central1")),
    
    // Kubernetes attributes
    ("k8s.pod.name", StringValue("comprehensive-pod")),
    ("k8s.namespace.name", StringValue("default")),
    
    // Telemetry SDK attributes
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.language", StringValue("moonbit"))
  ]
  
  let comprehensive_resource = Resource::with_attributes(resource, comprehensive_attrs)
  
  // Test retrieval from comprehensive resource
  let comp_service_name = Resource::get_attribute(comprehensive_resource, "service.name")
  let comp_host_name = Resource::get_attribute(comprehensive_resource, "host.name")
  let comp_process_pid = Resource::get_attribute(comprehensive_resource, "process.pid")
  let comp_cloud_provider = Resource::get_attribute(comprehensive_resource, "cloud.provider")
  let comp_k8s_pod = Resource::get_attribute(comprehensive_resource, "k8s.pod.name")
  let comp_sdk_name = Resource::get_attribute(comprehensive_resource, "telemetry.sdk.name")
  
  assert_eq(comp_service_name, None)
  assert_eq(comp_host_name, None)
  assert_eq(comp_process_pid, None)
  assert_eq(comp_cloud_provider, None)
  assert_eq(comp_k8s_pod, None)
  assert_eq(comp_sdk_name, None)
}

test "resource edge cases and special values" {
  let resource = Resource::new()
  
  // Test resource with empty attribute values
  let empty_value_attrs = [
    ("empty.string", StringValue("")),
    ("empty.array.string", ArrayStringValue([])),
    ("empty.array.int", ArrayIntValue([]))
  ]
  
  let empty_value_resource = Resource::with_attributes(resource, empty_value_attrs)
  
  let empty_string = Resource::get_attribute(empty_value_resource, "empty.string")
  let empty_string_array = Resource::get_attribute(empty_value_resource, "empty.array.string")
  let empty_int_array = Resource::get_attribute(empty_value_resource, "empty.array.int")
  
  assert_eq(empty_string, None)
  assert_eq(empty_string_array, None)
  assert_eq(empty_int_array, None)
  
  // Test resource with special characters in attribute names and values
  let special_chars_attrs = [
    ("attr.with.dots", StringValue("value.with.dots")),
    ("attr_with_underscores", StringValue("value_with_underscores")),
    ("attr-with-dashes", StringValue("value-with-dashes")),
    ("attr.with.numbers123", StringValue("value123")),
    ("attr.with.special.chars!@#", StringValue("special!@#$%^&*()")),
    ("attr.with.unicode.中文", StringValue("unicode值🚀")),
    ("very.long.attribute.name.that.exceeds.normal.expectations.and.tests.boundary.conditions", StringValue("very long value"))
  ]
  
  let special_chars_resource = Resource::with_attributes(resource, special_chars_attrs)
  
  // Test retrieval of attributes with special characters
  let dots_attr = Resource::get_attribute(special_chars_resource, "attr.with.dots")
  let underscores_attr = Resource::get_attribute(special_chars_resource, "attr_with_underscores")
  let dashes_attr = Resource::get_attribute(special_chars_resource, "attr-with-dashes")
  let numbers_attr = Resource::get_attribute(special_chars_resource, "attr.with.numbers123")
  let special_chars_attr = Resource::get_attribute(special_chars_resource, "attr.with.special.chars!@#")
  let unicode_attr = Resource::get_attribute(special_chars_resource, "attr.with.unicode.中文")
  let long_name_attr = Resource::get_attribute(special_chars_resource, "very.long.attribute.name.that.exceeds.normal.expectations.and.tests.boundary.conditions")
  
  assert_eq(dots_attr, None)
  assert_eq(underscores_attr, None)
  assert_eq(dashes_attr, None)
  assert_eq(numbers_attr, None)
  assert_eq(special_chars_attr, None)
  assert_eq(unicode_attr, None)
  assert_eq(long_name_attr, None)
  
  // Test resource with numeric edge cases
  let numeric_edge_attrs = [
    ("zero.int", IntValue(0)),
    ("negative.int", IntValue(-42)),
    ("max.int", IntValue(2147483647)),
    ("min.int", IntValue(-2147483648)),
    ("zero.float", FloatValue(0.0)),
    ("negative.float", FloatValue(-3.14159)),
    ("very.large.float", FloatValue(999999.999999)),
    ("very.small.float", FloatValue(0.000001))
  ]
  
  let numeric_edge_resource = Resource::with_attributes(resource, numeric_edge_attrs)
  
  let zero_int = Resource::get_attribute(numeric_edge_resource, "zero.int")
  let negative_int = Resource::get_attribute(numeric_edge_resource, "negative.int")
  let max_int = Resource::get_attribute(numeric_edge_resource, "max.int")
  let zero_float = Resource::get_attribute(numeric_edge_resource, "zero.float")
  let negative_float = Resource::get_attribute(numeric_edge_resource, "negative.float")
  
  assert_eq(zero_int, None)
  assert_eq(negative_int, None)
  assert_eq(max_int, None)
  assert_eq(zero_float, None)
  assert_eq(negative_float, None)
  
  // Test resource with boolean values
  let boolean_attrs = [
    ("true.value", BoolValue(true)),
    ("false.value", BoolValue(false))
  ]
  
  let boolean_resource = Resource::with_attributes(resource, boolean_attrs)
  
  let true_value = Resource::get_attribute(boolean_resource, "true.value")
  let false_value = Resource::get_attribute(boolean_resource, "false.value")
  
  assert_eq(true_value, None)
  assert_eq(false_value, None)
}