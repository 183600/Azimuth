// Comprehensive Resource Management Test Suite
// Tests for resource creation, attribute management, and merging

test "resource creation and basic operations" {
  let resource = Resource::new()
  
  // Test that new resource has no attributes
  let missing_attr = Resource::get_attribute(resource, "service.name")
  assert_eq(missing_attr, None)
}

test "resource with attributes" {
  let attributes = [
    ("service.name", StringValue("azimuth-telemetry")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attributes)
  
  // Test getting existing attributes
  let service_name = Resource::get_attribute(resource, "service.name")
  assert_eq(service_name, Some(StringValue("azimuth-telemetry")))
  
  let service_version = Resource::get_attribute(resource, "service.version")
  assert_eq(service_version, Some(StringValue("1.0.0")))
  
  let instance_id = Resource::get_attribute(resource, "service.instance.id")
  assert_eq(instance_id, Some(StringValue("instance-123")))
  
  // Test getting non-existent attribute
  let missing_attr = Resource::get_attribute(resource, "missing.attr")
  assert_eq(missing_attr, None)
}

test "resource with different attribute types" {
  let attributes = [
    ("service.name", StringValue("azimuth")),
    ("service.port", IntValue(8080)),
    ("cpu.usage", FloatValue(0.75)),
    ("service.enabled", BoolValue(true)),
    ("service.tags", ArrayStringValue(["telemetry", "moonbit", "opentelemetry"]))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attributes)
  
  // Test string attribute
  let name = Resource::get_attribute(resource, "service.name")
  assert_eq(name, Some(StringValue("azimuth")))
  
  // Test int attribute
  let port = Resource::get_attribute(resource, "service.port")
  assert_eq(port, Some(IntValue(8080)))
  
  // Test float attribute
  let cpu = Resource::get_attribute(resource, "cpu.usage")
  assert_eq(cpu, Some(FloatValue(0.75)))
  
  // Test boolean attribute
  let enabled = Resource::get_attribute(resource, "service.enabled")
  assert_eq(enabled, Some(BoolValue(true)))
  
  // Test array attribute
  let tags = Resource::get_attribute(resource, "service.tags")
  assert_eq(tags, Some(ArrayStringValue(["telemetry", "moonbit", "opentelemetry"])))
}

test "resource merging behavior" {
  let base_attributes = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("common.attr", StringValue("base-value"))
  ]
  
  let override_attributes = [
    ("service.name", StringValue("override-service")),  // This should override
    ("service.instance.id", StringValue("override-instance")),  // New attribute
    ("common.attr", StringValue("override-value"))  // This should override
  ]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attributes)
  let override_resource = Resource::with_attributes(Resource::new(), override_attributes)
  
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // Test that override values are used
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  assert_eq(service_name, Some(StringValue("override-service")))
  
  let common_attr = Resource::get_attribute(merged_resource, "common.attr")
  assert_eq(common_attr, Some(StringValue("override-value")))
  
  // Test new attribute from override
  let instance_id = Resource::get_attribute(merged_resource, "service.instance.id")
  assert_eq(instance_id, Some(StringValue("override-instance")))
}

test "resource with host and process information" {
  let attributes = [
    ("host.name", StringValue("localhost")),
    ("host.arch", StringValue("x86_64")),
    ("os.type", StringValue("linux")),
    ("process.pid", IntValue(12345)),
    ("process.executable.name", StringValue("azimuth")),
    ("process.command_args", ArrayStringValue(["azimuth", "serve", "--port", "8080"]))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attributes)
  
  // Test host information
  let host_name = Resource::get_attribute(resource, "host.name")
  assert_eq(host_name, Some(StringValue("localhost")))
  
  let host_arch = Resource::get_attribute(resource, "host.arch")
  assert_eq(host_arch, Some(StringValue("x86_64")))
  
  // Test OS information
  let os_type = Resource::get_attribute(resource, "os.type")
  assert_eq(os_type, Some(StringValue("linux")))
  
  // Test process information
  let process_pid = Resource::get_attribute(resource, "process.pid")
  assert_eq(process_pid, Some(IntValue(12345)))
  
  let executable_name = Resource::get_attribute(resource, "process.executable.name")
  assert_eq(executable_name, Some(StringValue("azimuth")))
  
  let command_args = Resource::get_attribute(resource, "process.command_args")
  assert_eq(command_args, Some(ArrayStringValue(["azimuth", "serve", "--port", "8080"])))
}

test "resource with telemetry SDK information" {
  let attributes = [
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.language", StringValue("moonbit")),
    ("telemetry.sdk.version", StringValue("0.1.0")),
    ("telemetry.auto.version", StringValue("0.1.0"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attributes)
  
  // Test SDK information
  let sdk_name = Resource::get_attribute(resource, "telemetry.sdk.name")
  assert_eq(sdk_name, Some(StringValue("azimuth")))
  
  let sdk_language = Resource::get_attribute(resource, "telemetry.sdk.language")
  assert_eq(sdk_language, Some(StringValue("moonbit")))
  
  let sdk_version = Resource::get_attribute(resource, "telemetry.sdk.version")
  assert_eq(sdk_version, Some(StringValue("0.1.0")))
  
  let auto_version = Resource::get_attribute(resource, "telemetry.auto.version")
  assert_eq(auto_version, Some(StringValue("0.1.0")))
}

test "resource with service and deployment information" {
  let attributes = [
    ("service.name", StringValue("payment-service")),
    ("service.namespace", StringValue("production")),
    ("service.instance.id", StringValue("instance-abc123")),
    ("service.version", StringValue("2.1.0")),
    ("deployment.environment", StringValue("production")),
    ("deployment.namespace", StringValue("default")),
    ("k8s.pod.name", StringValue("payment-service-7d4f8c9b-xyz")),
    ("k8s.namespace.name", StringValue("production"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attributes)
  
  // Test service information
  let service_name = Resource::get_attribute(resource, "service.name")
  assert_eq(service_name, Some(StringValue("payment-service")))
  
  let service_namespace = Resource::get_attribute(resource, "service.namespace")
  assert_eq(service_namespace, Some(StringValue("production")))
  
  let service_instance = Resource::get_attribute(resource, "service.instance.id")
  assert_eq(service_instance, Some(StringValue("instance-abc123")))
  
  // Test deployment information
  let deployment_env = Resource::get_attribute(resource, "deployment.environment")
  assert_eq(deployment_env, Some(StringValue("production")))
  
  // Test Kubernetes information
  let pod_name = Resource::get_attribute(resource, "k8s.pod.name")
  assert_eq(pod_name, Some(StringValue("payment-service-7d4f8c9b-xyz")))
}

test "resource with cloud provider information" {
  let attributes = [
    ("cloud.provider", StringValue("aws")),
    ("cloud.account.id", StringValue("123456789012")),
    ("cloud.region", StringValue("us-west-2")),
    ("cloud.availability_zone", StringValue("us-west-2a")),
    ("aws.ecs.container.arn", StringValue("arn:aws:ecs:us-west-2:123456789012:container/abc123")),
    ("aws.ecs.task.arn", StringValue("arn:aws:ecs:us-west-2:123456789012:task/def456"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attributes)
  
  // Test cloud provider information
  let cloud_provider = Resource::get_attribute(resource, "cloud.provider")
  assert_eq(cloud_provider, Some(StringValue("aws")))
  
  let cloud_account = Resource::get_attribute(resource, "cloud.account.id")
  assert_eq(cloud_account, Some(StringValue("123456789012")))
  
  let cloud_region = Resource::get_attribute(resource, "cloud.region")
  assert_eq(cloud_region, Some(StringValue("us-west-2")))
  
  // Test AWS-specific information
  let ecs_container_arn = Resource::get_attribute(resource, "aws.ecs.container.arn")
  assert_eq(ecs_container_arn, Some(StringValue("arn:aws:ecs:us-west-2:123456789012:container/abc123")))
}