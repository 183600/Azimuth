// 遥测资源检测测试用例

test "basic_resource_detection" {
  // 测试基本资源检测
  
  let detector = azimuth::telemetry::api::resource::ResourceDetector::new()
  
  // 检测资源
  let resource = detector.detect()
  
  // 验证基本资源属性
  assert_eq(resource.get_attributes().size() > 0, true)
  
  // 验证服务名称
  let service_name = resource.get_attribute("service.name")
  assert_eq(service_name.is_some(), true)
  assert_eq(service_name.unwrap().to_string().length() > 0, true)
  
  // 验证主机名
  let hostname = resource.get_attribute("host.name")
  assert_eq(hostname.is_some(), true)
  assert_eq(hostname.unwrap().to_string().length() > 0, true)
}

test "environment_resource_detection" {
  // 测试环境变量资源检测
  
  // 设置环境变量
  azimuth::telemetry::api::common::env::set("OTEL_SERVICE_NAME", "test-service")
  azimuth::telemetry::api::common::env::set("OTEL_SERVICE_VERSION", "1.0.0")
  azimuth::telemetry::api::common::env::set("OTEL_RESOURCE_ATTRIBUTES", "deployment.environment=testing,team=backend")
  
  let detector = azimuth::telemetry::api::resource::EnvironmentResourceDetector::new()
  let resource = detector.detect()
  
  // 验证环境变量设置的资源属性
  assert_eq(resource.get_attribute("service.name").unwrap().to_string(), "test-service")
  assert_eq(resource.get_attribute("service.version").unwrap().to_string(), "1.0.0")
  assert_eq(resource.get_attribute("deployment.environment").unwrap().to_string(), "testing")
  assert_eq(resource.get_attribute("team").unwrap().to_string(), "backend")
  
  // 清理环境变量
  azimuth::telemetry::api::common::env::unset("OTEL_SERVICE_NAME")
  azimuth::telemetry::api::common::env::unset("OTEL_SERVICE_VERSION")
  azimuth::telemetry::api::common::env::unset("OTEL_RESOURCE_ATTRIBUTES")
}

test "host_resource_detection" {
  // 测试主机资源检测
  
  let detector = azimuth::telemetry::api::resource::HostResourceDetector::new()
  let resource = detector.detect()
  
  // 验证主机相关属性
  let host_name = resource.get_attribute("host.name")
  assert_eq(host_name.is_some(), true)
  assert_eq(host_name.unwrap().to_string().length() > 0, true)
  
  let host_arch = resource.get_attribute("host.arch")
  assert_eq(host_arch.is_some(), true)
  assert_eq(host_arch.unwrap().to_string().length() > 0, true)
  
  let host_ip = resource.get_attribute("host.ip")
  assert_eq(host_ip.is_some(), true)
  
  let os_type = resource.get_attribute("os.type")
  assert_eq(os_type.is_some(), true)
  assert_eq(["linux", "windows", "darwin"].contains(os_type.unwrap().to_string()), true)
}

test "process_resource_detection" {
  // 测试进程资源检测
  
  let detector = azimuth::telemetry::api::resource::ProcessResourceDetector::new()
  let resource = detector.detect()
  
  // 验证进程相关属性
  let process_pid = resource.get_attribute("process.pid")
  assert_eq(process_pid.is_some(), true)
  assert_eq(process_pid.unwrap().to_string().to_int() > 0, true)
  
  let process_executable_name = resource.get_attribute("process.executable.name")
  assert_eq(process_executable_name.is_some(), true)
  assert_eq(process_executable_name.unwrap().to_string().length() > 0, true)
  
  let process_command_args = resource.get_attribute("process.command_args")
  assert_eq(process_command_args.is_some(), true)
  
  let process_runtime_name = resource.get_attribute("process.runtime.name")
  assert_eq(process_runtime_name.is_some(), true)
  assert_eq(process_runtime_name.unwrap().to_string(), "moonbit")
}

test "container_resource_detection" {
  // 测试容器资源检测
  
  let detector = azimuth::telemetry::api::resource::ContainerResourceDetector::new()
  let resource = detector.detect()
  
  // 如果在容器环境中运行，应该检测到容器相关属性
  let container_id = resource.get_attribute("container.id")
  let container_name = resource.get_attribute("container.name")
  let container_image = resource.get_attribute("container.image.name")
  
  // 验证容器属性（如果在容器中）
  if container_id.is_some() {
    assert_eq(container_id.unwrap().to_string().length() > 0, true)
  }
  
  if container_name.is_some() {
    assert_eq(container_name.unwrap().to_string().length() > 0, true)
  }
  
  if container_image.is_some() {
    assert_eq(container_image.unwrap().to_string().length() > 0, true)
  }
}

test "cloud_resource_detection" {
  // 测试云资源检测
  
  let detector = azimuth::telemetry::api::resource::CloudResourceDetector::new()
  let resource = detector.detect()
  
  // 验证云相关属性（如果在云环境中）
  let cloud_provider = resource.get_attribute("cloud.provider")
  let cloud_account_id = resource.get_attribute("cloud.account.id")
  let cloud_region = resource.get_attribute("cloud.region")
  let cloud_availability_zone = resource.get_attribute("cloud.availability_zone")
  
  // 如果检测到云环境，验证属性
  if cloud_provider.is_some() {
    let provider = cloud_provider.unwrap().to_string()
    assert_eq(["aws", "gcp", "azure", "alibaba_cloud"].contains(provider), true)
    
    if cloud_account_id.is_some() {
      assert_eq(cloud_account_id.unwrap().to_string().length() > 0, true)
    }
    
    if cloud_region.is_some() {
      assert_eq(cloud_region.unwrap().to_string().length() > 0, true)
    }
  }
}

test "kubernetes_resource_detection" {
  // 测试Kubernetes资源检测
  
  let detector = azimuth::telemetry::api::resource::KubernetesResourceDetector::new()
  let resource = detector.detect()
  
  // 验证Kubernetes相关属性（如果在K8s环境中）
  let k8s_pod_name = resource.get_attribute("k8s.pod.name")
  let k8s_pod_uid = resource.get_attribute("k8s.pod.uid")
  let k8s_namespace_name = resource.get_attribute("k8s.namespace.name")
  let k8s_deployment_name = resource.get_attribute("k8s.deployment.name")
  let k8s_node_name = resource.get_attribute("k8s.node.name")
  
  // 如果在K8s环境中，验证属性
  if k8s_pod_name.is_some() {
    assert_eq(k8s_pod_name.unwrap().to_string().length() > 0, true)
  }
  
  if k8s_pod_uid.is_some() {
    assert_eq(k8s_pod_uid.unwrap().to_string().length() > 0, true)
  }
  
  if k8s_namespace_name.is_some() {
    assert_eq(k8s_namespace_name.unwrap().to_string().length() > 0, true)
  }
}

test "composite_resource_detection" {
  // 测试复合资源检测
  
  // 创建多个检测器
  let detectors = [
    azimuth::telemetry::api::resource::EnvironmentResourceDetector::new(),
    azimuth::telemetry::api::resource::HostResourceDetector::new(),
    azimuth::telemetry::api::resource::ProcessResourceDetector::new()
  ]
  
  let composite_detector = azimuth::telemetry::api::resource::CompositeResourceDetector::new(detectors)
  let resource = composite_detector.detect()
  
  // 验证复合检测器包含了所有检测器的结果
  assert_eq(resource.get_attribute("host.name").is_some(), true)
  assert_eq(resource.get_attribute("process.pid").is_some(), true)
  
  // 验证属性数量
  assert_eq(resource.get_attributes().size() > 5, true)
}

test "resource_merging_and_override" {
  // 测试资源合并和覆盖
  
  // 创建第一个资源
  let resource1 = azimuth::telemetry::api::resource::Resource::empty()
    .with("service.name", "service-A")
    .with("service.version", "1.0.0")
    .with("environment", "development")
  
  // 创建第二个资源
  let resource2 = azimuth::telemetry::api::resource::Resource::empty()
    .with("service.name", "service-B")  // 覆盖service.name
    .with("deployment.region", "us-west-2")
    .with("team", "backend")
  
  // 合并资源
  let merged_resource = resource1.merge(resource2)
  
  // 验证合并结果
  assert_eq(merged_resource.get_attribute("service.name").unwrap().to_string(), "service-B")  // 被覆盖
  assert_eq(merged_resource.get_attribute("service.version").unwrap().to_string(), "1.0.0")  // 保留
  assert_eq(merged_resource.get_attribute("environment").unwrap().to_string(), "development")  // 保留
  assert_eq(merged_resource.get_attribute("deployment.region").unwrap().to_string(), "us-west-2")  // 新增
  assert_eq(merged_resource.get_attribute("team").unwrap().to_string(), "backend")  // 新增
  
  assert_eq(merged_resource.get_attributes().size(), 5)
}

test "resource_serialization_and_export" {
  // 测试资源序列化和导出
  
  let resource = azimuth::telemetry::api::resource::Resource::empty()
    .with("service.name", "test-service")
    .with("service.version", "2.1.0")
    .with("environment", "production")
    .with("host.name", "test-host")
    .with("process.pid", "12345")
  
  // 序列化为JSON
  let json_export = resource.to_json()
  assert_eq(json_export.length() > 0, true)
  assert_eq(json_export.contains("service.name"), true)
  assert_eq(json_export.contains("test-service"), true)
  assert_eq(json_export.contains("environment"), true)
  assert_eq(json_export.contains("production"), true)
  
  // 从JSON恢复
  let restored_resource = azimuth::telemetry::api::resource::Resource::from_json(json_export)
  assert_eq(restored_resource.get_attribute("service.name").unwrap().to_string(), "test-service")
  assert_eq(restored_resource.get_attribute("service.version").unwrap().to_string(), "2.1.0")
  assert_eq(restored_resource.get_attribute("environment").unwrap().to_string(), "production")
  assert_eq(restored_resource.get_attribute("host.name").unwrap().to_string(), "test-host")
  assert_eq(restored_resource.get_attribute("process.pid").unwrap().to_string(), "12345")
}

test "resource_validation_and_edge_cases" {
  // 测试资源验证和边界情况
  
  // 测试空资源
  let empty_resource = azimuth::telemetry::api::resource::Resource::empty()
  assert_eq(empty_resource.get_attributes().size(), 0)
  
  // 测试包含特殊字符的属性值
  let special_resource = azimuth::telemetry::api::resource::Resource::empty()
    .with("special.chars", "value@with#special$chars")
    .with("unicode.value", "值.包含.中文.和.🚀.emoji")
    .with("empty.value", "")
    .with("long.value", "this.is.a.very.long.value.that.tests.system.behavior.with.extremely.long.strings")
  
  assert_eq(special_resource.get_attributes().size(), 4)
  assert_eq(special_resource.get_attribute("special.chars").unwrap().to_string(), "value@with#special$chars")
  assert_eq(special_resource.get_attribute("unicode.value").unwrap().to_string(), "值.包含.中文.和.🚀.emoji")
  assert_eq(special_resource.get_attribute("empty.value").unwrap().to_string(), "")
  assert_eq(special_resource.get_attribute("long.value").unwrap().to_string().length() > 50, true)
  
  // 测试数值属性
  let numeric_resource = azimuth::telemetry::api::resource::Resource::empty()
    .with("int.value", azimuth::telemetry::api::common::AttributeValue::Int64(42))
    .with("float.value", azimuth::telemetry::api::common::AttributeValue::Float64(3.14159))
    .with("bool.value", azimuth::telemetry::api::common::AttributeValue::Bool(true))
  
  assert_eq(numeric_resource.get_attribute("int.value").unwrap().to_string(), "42")
  assert_eq(numeric_resource.get_attribute("float.value").unwrap().to_string(), "3.14159")
  assert_eq(numeric_resource.get_attribute("bool.value").unwrap().to_string(), "true")
}