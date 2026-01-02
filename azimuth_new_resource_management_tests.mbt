// Azimuth Telemetry System - Resource Management Tests
// This file contains test cases for resource management functionality

// Test 1: Basic resource creation and attributes
test "basic resource creation and attributes" {
  // Create empty resource
  let empty_resource = Resource::new()
  assert_eq(empty_resource.attributes.length(), 0)
  
  // Create resource with attributes
  let attrs = [
    ("service.name", AttributeValue::StringValue("auth-service")),
    ("service.version", AttributeValue::StringValue("1.2.3")),
    ("service.instance.id", AttributeValue::StringValue("instance-12345"))
  ]
  let resource_with_attrs = Resource::with_attributes(empty_resource, attrs)
  
  // Verify resource attributes
  assert_eq(resource_with_attrs.attributes.length(), 3)
  
  // Get specific attributes
  match Resource::get_attribute(resource_with_attrs, "service.name") {
    Some(AttributeValue::StringValue(name)) => assert_eq(name, "auth-service")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_attrs, "service.version") {
    Some(AttributeValue::StringValue(version)) => assert_eq(version, "1.2.3")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_attrs, "service.instance.id") {
    Some(AttributeValue::StringValue(instance_id)) => assert_eq(instance_id, "instance-12345")
    _ => assert_true(false)
  }
  
  // Test nonexistent attribute
  match Resource::get_attribute(resource_with_attrs, "nonexistent") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Resource with different attribute types
test "resource with different attribute types" {
  // Create resource with mixed attribute types
  let attrs = [
    ("string.attr", AttributeValue::StringValue("string_value")),
    ("int.attr", AttributeValue::IntValue(42)),
    ("float.attr", AttributeValue::FloatValue(3.14159)),
    ("bool.attr", AttributeValue::BoolValue(true)),
    ("string.array.attr", AttributeValue::ArrayStringValue(["item1", "item2", "item3"])),
    ("int.array.attr", AttributeValue::ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attrs)
  assert_eq(resource.attributes.length(), 6)
  
  // Test string attribute
  match Resource::get_attribute(resource, "string.attr") {
    Some(AttributeValue::StringValue(value)) => assert_eq(value, "string_value")
    _ => assert_true(false)
  }
  
  // Test int attribute
  match Resource::get_attribute(resource, "int.attr") {
    Some(AttributeValue::IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  // Test float attribute
  match Resource::get_attribute(resource, "float.attr") {
    Some(AttributeValue::FloatValue(value)) => assert_eq(value, 3.14159)
    _ => assert_true(false)
  }
  
  // Test bool attribute
  match Resource::get_attribute(resource, "bool.attr") {
    Some(AttributeValue::BoolValue(value)) => assert_true(value)
    _ => assert_true(false)
  }
  
  // Test string array attribute
  match Resource::get_attribute(resource, "string.array.attr") {
    Some(AttributeValue::ArrayStringValue(arr)) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "item1")
      assert_eq(arr[1], "item2")
      assert_eq(arr[2], "item3")
    }
    _ => assert_true(false)
  }
  
  // Test int array attribute
  match Resource::get_attribute(resource, "int.array.attr") {
    Some(AttributeValue::ArrayIntValue(arr)) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 5)
      
      // Test array sum
      let mut sum = 0
      for i in arr {
        sum = sum + i
      }
      assert_eq(sum, 15)
    }
    _ => assert_true(false)
  }
}

// Test 3: Resource merging operations
test "resource merging operations" {
  // Create base resource
  let base_attrs = [
    ("service.name", AttributeValue::StringValue("base-service")),
    ("service.version", AttributeValue::StringValue("1.0.0")),
    ("deployment.region", AttributeValue::StringValue("us-west-1"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  // Create override resource
  let override_attrs = [
    ("service.version", AttributeValue::StringValue("2.0.0")),  // Override
    ("service.instance.id", AttributeValue::StringValue("instance-67890")),  // New
    ("host.name", AttributeValue::StringValue("host-123"))  // New
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // Merge resources
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // Verify merged resource has override attributes
  match Resource::get_attribute(merged_resource, "service.name") {
    Some(AttributeValue::StringValue(name)) => assert_eq(name, "base-service")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(merged_resource, "service.version") {
    Some(AttributeValue::StringValue(version)) => assert_eq(version, "2.0.0")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(merged_resource, "service.instance.id") {
    Some(AttributeValue::StringValue(instance_id)) => assert_eq(instance_id, "instance-67890")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(merged_resource, "host.name") {
    Some(AttributeValue::StringValue(host_name)) => assert_eq(host_name, "host-123")
    _ => assert_true(false)
  }
}

// Test 4: Resource with service information
test "resource with service information" {
  // Create resource with comprehensive service information
  let service_attrs = [
    ("service.name", AttributeValue::StringValue("payment-service")),
    ("service.version", AttributeValue::StringValue("3.2.1")),
    ("service.namespace", AttributeValue::StringValue("production")),
    ("service.instance.id", AttributeValue::StringValue("payment-pod-12345")),
    ("service.node.name", AttributeValue::StringValue("worker-node-6"))
  ]
  
  let service_resource = Resource::with_attributes(Resource::new(), service_attrs)
  assert_eq(service_resource.attributes.length(), 5)
  
  // Verify all service attributes
  let expected_attrs = [
    ("service.name", "payment-service"),
    ("service.version", "3.2.1"),
    ("service.namespace", "production"),
    ("service.instance.id", "payment-pod-12345"),
    ("service.node.name", "worker-node-6")
  ]
  
  for (key, expected_value) in expected_attrs {
    match Resource::get_attribute(service_resource, key) {
      Some(AttributeValue::StringValue(actual_value)) => assert_eq(actual_value, expected_value)
      _ => assert_true(false)
    }
  }
}

// Test 5: Resource with host and process information
test "resource with host and process information" {
  // Create resource with host and process information
  let host_process_attrs = [
    ("host.name", AttributeValue::StringValue("prod-server-01")),
    ("host.id", AttributeValue::StringValue("host-id-123456789")),
    ("host.type", AttributeValue::StringValue("virtual_machine")),
    ("host.arch", AttributeValue::StringValue("amd64")),
    ("os.type", AttributeValue::StringValue("linux")),
    ("os.version", AttributeValue::StringValue("5.15.0-1020-aws")),
    ("process.pid", AttributeValue::IntValue(12345)),
    ("process.executable.name", AttributeValue::StringValue("payment-service")),
    ("process.executable.path", AttributeValue::StringValue("/usr/local/bin/payment-service")),
    ("process.command_args", AttributeValue::ArrayStringValue(["--config", "/etc/payment/config.yaml", "--port", "8080"]))
  ]
  
  let host_process_resource = Resource::with_attributes(Resource::new(), host_process_attrs)
  assert_eq(host_process_resource.attributes.length(), 10)
  
  // Verify host information
  match Resource::get_attribute(host_process_resource, "host.name") {
    Some(AttributeValue::StringValue(host_name)) => assert_eq(host_name, "prod-server-01")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(host_process_resource, "host.arch") {
    Some(AttributeValue::StringValue(arch)) => assert_eq(arch, "amd64")
    _ => assert_true(false)
  }
  
  // Verify process information
  match Resource::get_attribute(host_process_resource, "process.pid") {
    Some(AttributeValue::IntValue(pid)) => assert_eq(pid, 12345)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(host_process_resource, "process.command_args") {
    Some(AttributeValue::ArrayStringValue(args)) => {
      assert_eq(args.length(), 4)
      assert_eq(args[0], "--config")
      assert_eq(args[1], "/etc/payment/config.yaml")
      assert_eq(args[2], "--port")
      assert_eq(args[3], "8080")
    }
    _ => assert_true(false)
  }
}

// Test 6: Resource with cloud provider information
test "resource with cloud provider information" {
  // Create resource with cloud provider information
  let cloud_attrs = [
    ("cloud.provider", AttributeValue::StringValue("aws")),
    ("cloud.account.id", AttributeValue::StringValue("123456789012")),
    ("cloud.region", AttributeValue::StringValue("us-west-2")),
    ("cloud.availability_zone", AttributeValue::StringValue("us-west-2a")),
    ("cloud.platform", AttributeValue::StringValue("aws_ec2")),
    ("aws.ec2.instance.id", AttributeValue::StringValue("i-1234567890abcdef0")),
    ("aws.ec2.instance.type", AttributeValue::StringValue("t3.medium")),
    ("aws.ec2.instance.image.id", AttributeValue::StringValue("ami-1234567890abcdef0"))
  ]
  
  let cloud_resource = Resource::with_attributes(Resource::new(), cloud_attrs)
  assert_eq(cloud_resource.attributes.length(), 8)
  
  // Verify cloud provider information
  match Resource::get_attribute(cloud_resource, "cloud.provider") {
    Some(AttributeValue::StringValue(provider)) => assert_eq(provider, "aws")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(cloud_resource, "cloud.account.id") {
    Some(AttributeValue::StringValue(account_id)) => assert_eq(account_id, "123456789012")
    _ => assert_true(false)
  }
  
  // Verify AWS-specific information
  match Resource::get_attribute(cloud_resource, "aws.ec2.instance.id") {
    Some(AttributeValue::StringValue(instance_id)) => assert_eq(instance_id, "i-1234567890abcdef0")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(cloud_resource, "aws.ec2.instance.type") {
    Some(AttributeValue::StringValue(instance_type)) => assert_eq(instance_type, "t3.medium")
    _ => assert_true(false)
  }
}

// Test 7: Resource with container information
test "resource with container information" {
  // Create resource with container information
  let container_attrs = [
    ("container.name", AttributeValue::StringValue("payment-service-container")),
    ("container.id", AttributeValue::StringValue("container-id-1234567890abcdef")),
    ("container.image.name", AttributeValue::StringValue("payment-service:3.2.1")),
    ("container.image.tag", AttributeValue::StringValue("3.2.1")),
    ("container.runtime", AttributeValue::StringValue("docker")),
    ("k8s.pod.name", AttributeValue::StringValue("payment-service-7d4f8b9c-12345")),
    ("k8s.namespace.name", AttributeValue::StringValue("production")),
    ("k8s.deployment.name", AttributeValue::StringValue("payment-service")),
    ("k8s.node.name", AttributeValue::StringValue("worker-node-6"))
  ]
  
  let container_resource = Resource::with_attributes(Resource::new(), container_attrs)
  assert_eq(container_resource.attributes.length(), 9)
  
  // Verify container information
  match Resource::get_attribute(container_resource, "container.name") {
    Some(AttributeValue::StringValue(name)) => assert_eq(name, "payment-service-container")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(container_resource, "container.image.name") {
    Some(AttributeValue::StringValue(image_name)) => assert_eq(image_name, "payment-service:3.2.1")
    _ => assert_true(false)
  }
  
  // Verify Kubernetes information
  match Resource::get_attribute(container_resource, "k8s.pod.name") {
    Some(AttributeValue::StringValue(pod_name)) => assert_eq(pod_name, "payment-service-7d4f8b9c-12345")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(container_resource, "k8s.namespace.name") {
    Some(AttributeValue::StringValue(namespace)) => assert_eq(namespace, "production")
    _ => assert_true(false)
  }
}

// Test 8: Resource with telemetry SDK information
test "resource with telemetry sdk information" {
  // Create resource with telemetry SDK information
  let telemetry_attrs = [
    ("telemetry.sdk.name", AttributeValue::StringValue("azimuth")),
    ("telemetry.sdk.language", AttributeValue::StringValue("moonbit")),
    ("telemetry.sdk.version", AttributeValue::StringValue("0.1.0")),
    ("telemetry.auto.version", AttributeValue::StringValue("1.0.0")),
    ("service.name", AttributeValue::StringValue("telemetry-test-service")),
    ("service.version", AttributeValue::StringValue("1.0.0-test"))
  ]
  
  let telemetry_resource = Resource::with_attributes(Resource::new(), telemetry_attrs)
  assert_eq(telemetry_resource.attributes.length(), 6)
  
  // Verify SDK information
  match Resource::get_attribute(telemetry_resource, "telemetry.sdk.name") {
    Some(AttributeValue::StringValue(sdk_name)) => assert_eq(sdk_name, "azimuth")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(telemetry_resource, "telemetry.sdk.language") {
    Some(AttributeValue::StringValue(language)) => assert_eq(language, "moonbit")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(telemetry_resource, "telemetry.sdk.version") {
    Some(AttributeValue::StringValue(version)) => assert_eq(version, "0.1.0")
    _ => assert_true(false)
  }
  
  // Verify service information
  match Resource::get_attribute(telemetry_resource, "service.name") {
    Some(AttributeValue::StringValue(service_name)) => assert_eq(service_name, "telemetry-test-service")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(telemetry_resource, "service.version") {
    Some(AttributeValue::StringValue(service_version)) => assert_eq(service_version, "1.0.0-test")
    _ => assert_true(false)
  }
}