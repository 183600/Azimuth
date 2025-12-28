// Resource Attributes Operations Tests
// Tests for resource creation, attribute management, and resource merging

test "resource creation and basic operations" {
  let resource = Resource::new()
  
  // Test empty resource
  assert_eq(resource.attributes.length(), 0)
  
  // Test resource with attributes
  let attrs = [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("instance-abc123"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  assert_eq(resource_with_attrs.attributes.length(), 3)
}

test "resource attribute retrieval and management" {
  let attrs = [
    ("service.name", StringValue("auth-service")),
    ("service.version", StringValue("2.0.0")),
    ("service.namespace", StringValue("production")),
    ("host.name", StringValue("auth-server-01")),
    ("host.arch", StringValue("amd64")),
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("20.04")),
    ("process.pid", IntValue(12345)),
    ("process.executable.name", StringValue("auth-service")),
    ("process.command_args", ArrayStringValue(["./auth-service", "--port=8080"]))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attrs)
  
  // Test attribute retrieval
  let service_name = Resource::get_attribute(resource, "service.name")
  let service_version = Resource::get_attribute(resource, "service.version")
  let host_name = Resource::get_attribute(resource, "host.name")
  let process_pid = Resource::get_attribute(resource, "process.pid")
  let missing_attr = Resource::get_attribute(resource, "missing.attribute")
  
  assert_eq(match service_name { Some(StringValue(v)) => v, _ => "" }, "auth-service")
  assert_eq(match service_version { Some(StringValue(v)) => v, _ => "" }, "2.0.0")
  assert_eq(match host_name { Some(StringValue(v)) => v, _ => "" }, "auth-server-01")
  assert_eq(match process_pid { Some(IntValue(v)) => v, _ => 0 }, 12345)
  assert_eq(missing_attr, None)
}

test "resource merging operations" {
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("development"))
  ]
  
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("service.instance.id", StringValue("instance-xyz789")),
    ("host.name", StringValue("override-host"))
  ]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // Test resource merging
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // In simplified implementation, override resource replaces base
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  let merged_instance_id = Resource::get_attribute(merged_resource, "service.instance.id")
  let merged_host_name = Resource::get_attribute(merged_resource, "host.name")
  
  assert_eq(match merged_service_name { Some(StringValue(v)) => v, _ => "" }, "override-service")
  assert_eq(match merged_instance_id { Some(StringValue(v)) => v, _ => "" }, "instance-xyz789")
  assert_eq(match merged_host_name { Some(StringValue(v)) => v, _ => "" }, "override-host")
}

test "resource with cloud provider attributes" {
  let cloud_attrs = [
    ("cloud.provider", StringValue("aws")),
    ("cloud.account.id", StringValue("123456789012")),
    ("cloud.region", StringValue("us-west-2")),
    ("cloud.availability_zone", StringValue("us-west-2a")),
    ("cloud.platform", StringValue("aws_ec2")),
    ("aws.ec2.instance.id", StringValue("i-1234567890abcdef0")),
    ("aws.ec2.instance.type", StringValue("t3.micro")),
    ("aws.ec2.instance.image.id", StringValue("ami-12345678"))
  ]
  
  let cloud_resource = Resource::with_attributes(Resource::new(), cloud_attrs)
  
  // Test cloud attribute retrieval
  let cloud_provider = Resource::get_attribute(cloud_resource, "cloud.provider")
  let cloud_region = Resource::get_attribute(cloud_resource, "cloud.region")
  let ec2_instance_id = Resource::get_attribute(cloud_resource, "aws.ec2.instance.id")
  let ec2_instance_type = Resource::get_attribute(cloud_resource, "aws.ec2.instance.type")
  
  assert_eq(match cloud_provider { Some(StringValue(v)) => v, _ => "" }, "aws")
  assert_eq(match cloud_region { Some(StringValue(v)) => v, _ => "" }, "us-west-2")
  assert_eq(match ec2_instance_id { Some(StringValue(v)) => v, _ => "" }, "i-1234567890abcdef0")
  assert_eq(match ec2_instance_type { Some(StringValue(v)) => v, _ => "" }, "t3.micro")
}

test "resource with container attributes" {
  let container_attrs = [
    ("container.name", StringValue("payment-api")),
    ("container.id", StringValue("container-abc123def456")),
    ("container.image.name", StringValue("payment-service:latest")),
    ("container.image.tag", StringValue("latest")),
    ("container.runtime", StringValue("docker")),
    ("kubernetes.pod.name", StringValue("payment-api-7d4f8b9c-12345")),
    ("kubernetes.namespace.name", StringValue("payment")),
    ("kubernetes.deployment.name", StringValue("payment-api")),
    ("kubernetes.node.name", StringValue("worker-node-01"))
  ]
  
  let container_resource = Resource::with_attributes(Resource::new(), container_attrs)
  
  // Test container attribute retrieval
  let container_name = Resource::get_attribute(container_resource, "container.name")
  let container_image = Resource::get_attribute(container_resource, "container.image.name")
  let k8s_pod_name = Resource::get_attribute(container_resource, "kubernetes.pod.name")
  let k8s_namespace = Resource::get_attribute(container_resource, "kubernetes.namespace.name")
  
  assert_eq(match container_name { Some(StringValue(v)) => v, _ => "" }, "payment-api")
  assert_eq(match container_image { Some(StringValue(v)) => v, _ => "" }, "payment-service:latest")
  assert_eq(match k8s_pod_name { Some(StringValue(v)) => v, _ => "" }, "payment-api-7d4f8b9c-12345")
  assert_eq(match k8s_namespace { Some(StringValue(v)) => v, _ => "" }, "payment")
}

test "resource with telemetry sdk attributes" {
  let telemetry_attrs = [
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.language", StringValue("moonbit")),
    ("telemetry.sdk.version", StringValue("0.1.0")),
    ("telemetry.auto.version", StringValue("0.1.0")),
    ("service.name", StringValue("order-service")),
    ("service.version", StringValue("3.2.1")),
    ("service.instance.id", StringValue("order-service-001"))
  ]
  
  let telemetry_resource = Resource::with_attributes(Resource::new(), telemetry_attrs)
  
  // Test telemetry SDK attribute retrieval
  let sdk_name = Resource::get_attribute(telemetry_resource, "telemetry.sdk.name")
  let sdk_language = Resource::get_attribute(telemetry_resource, "telemetry.sdk.language")
  let sdk_version = Resource::get_attribute(telemetry_resource, "telemetry.sdk.version")
  let service_name = Resource::get_attribute(telemetry_resource, "service.name")
  
  assert_eq(match sdk_name { Some(StringValue(v)) => v, _ => "" }, "azimuth")
  assert_eq(match sdk_language { Some(StringValue(v)) => v, _ => "" }, "moonbit")
  assert_eq(match sdk_version { Some(StringValue(v)) => v, _ => "" }, "0.1.0")
  assert_eq(match service_name { Some(StringValue(v)) => v, _ => "" }, "order-service")
}

test "resource with web engine attributes" {
  let web_attrs = [
    ("service.name", StringValue("web-frontend")),
    ("webengine.name", StringValue("nginx")),
    ("webengine.version", StringValue("1.20.2")),
    ("webengine.description", StringValue("Nginx web server")),
    ("host.name", StringValue("web-server-01")),
    ("http.port", IntValue(80)),
    ("https.port", IntValue(443))
  ]
  
  let web_resource = Resource::with_attributes(Resource::new(), web_attrs)
  
  // Test web engine attribute retrieval
  let webengine_name = Resource::get_attribute(web_resource, "webengine.name")
  let webengine_version = Resource::get_attribute(web_resource, "webengine.version")
  let http_port = Resource::get_attribute(web_resource, "http.port")
  let https_port = Resource::get_attribute(web_resource, "https.port")
  
  assert_eq(match webengine_name { Some(StringValue(v)) => v, _ => "" }, "nginx")
  assert_eq(match webengine_version { Some(StringValue(v)) => v, _ => "" }, "1.20.2")
  assert_eq(match http_port { Some(IntValue(v)) => v, _ => 0 }, 80)
  assert_eq(match https_port { Some(IntValue(v)) => v, _ => 0 }, 443)
}

test "resource with database attributes" {
  let db_attrs = [
    ("service.name", StringValue("database-service")),
    ("db.system", StringValue("postgresql")),
    ("db.name", StringValue("production_db")),
    ("db.connection_string", StringValue("postgresql://user:pass@localhost:5432/prod_db")),
    ("db.statement", StringValue("SELECT * FROM users WHERE id = $1")),
    ("db.user", StringValue("app_user")),
    ("net.peer.name", StringValue("db-primary.example.com")),
    ("net.peer.port", IntValue(5432))
  ]
  
  let db_resource = Resource::with_attributes(Resource::new(), db_attrs)
  
  // Test database attribute retrieval
  let db_system = Resource::get_attribute(db_resource, "db.system")
  let db_name = Resource::get_attribute(db_resource, "db.name")
  let db_user = Resource::get_attribute(db_resource, "db.user")
  let net_peer_port = Resource::get_attribute(db_resource, "net.peer.port")
  
  assert_eq(match db_system { Some(StringValue(v)) => v, _ => "" }, "postgresql")
  assert_eq(match db_name { Some(StringValue(v)) => v, _ => "" }, "production_db")
  assert_eq(match db_user { Some(StringValue(v)) => v, _ => "" }, "app_user")
  assert_eq(match net_peer_port { Some(IntValue(v)) => v, _ => 0 }, 5432)
}

test "resource with complex attribute types" {
  let complex_attrs = [
    ("service.name", StringValue("complex-service")),
    ("service.tags", ArrayStringValue(["api", "v2", "production"])),
    ("feature.flags", ArrayStringValue(["feature-a", "feature-b", "feature-c"])),
    ("server.ports", ArrayIntValue([8080, 8443, 9090])),
    ("process.threads", IntValue(16)),
    ("memory.limit", IntValue(2147483648)),
    ("cpu.cores", FloatValue(4.0)),
    ("disk.usage", FloatValue(75.5)),
    ("service.enabled", BoolValue(true)),
    ("debug.mode", BoolValue(false))
  ]
  
  let complex_resource = Resource::with_attributes(Resource::new(), complex_attrs)
  
  // Test complex attribute retrieval
  let service_name = Resource::get_attribute(complex_resource, "service.name")
  let service_tags = Resource::get_attribute(complex_resource, "service.tags")
  let server_ports = Resource::get_attribute(complex_resource, "server.ports")
  let cpu_cores = Resource::get_attribute(complex_resource, "cpu.cores")
  let service_enabled = Resource::get_attribute(complex_resource, "service.enabled")
  
  assert_eq(match service_name { Some(StringValue(v)) => v, _ => "" }, "complex-service")
  assert_eq(match service_tags { Some(ArrayStringValue(arr)) => arr.length(), _ => 0 }, 3)
  assert_eq(match server_ports { Some(ArrayIntValue(arr)) => arr.length(), _ => 0 }, 3)
  assert_eq(match cpu_cores { Some(FloatValue(v)) => v, _ => 0.0 }, 4.0)
  assert_eq(match service_enabled { Some(BoolValue(v)) => v, _ => false }, true)
}

test "resource with custom business attributes" {
  let business_attrs = [
    ("business.unit", StringValue("payments")),
    ("business.domain", StringValue("financial-services")),
    ("business.region", StringValue("emea")),
    ("business.tier", StringValue("premium")),
    ("cost.center", StringValue("cc-12345")),
    ("compliance.level", StringValue("pci-dss")),
    ("data.classification", StringValue("sensitive")),
    ("sla.tier", StringValue("gold")),
    ("monitoring.enabled", BoolValue(true)),
    ("backup.required", BoolValue(true))
  ]
  
  let business_resource = Resource::with_attributes(Resource::new(), business_attrs)
  
  // Test business attribute retrieval
  let business_unit = Resource::get_attribute(business_resource, "business.unit")
  let business_domain = Resource::get_attribute(business_resource, "business.domain")
  let compliance_level = Resource::get_attribute(business_resource, "compliance.level")
  let monitoring_enabled = Resource::get_attribute(business_resource, "monitoring.enabled")
  
  assert_eq(match business_unit { Some(StringValue(v)) => v, _ => "" }, "payments")
  assert_eq(match business_domain { Some(StringValue(v)) => v, _ => "" }, "financial-services")
  assert_eq(match compliance_level { Some(StringValue(v)) => v, _ => "" }, "pci-dss")
  assert_eq(match monitoring_enabled { Some(BoolValue(v)) => v, _ => false }, true)
}