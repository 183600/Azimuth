// Resource配置验证测试
// 测试Resource的配置验证，包括各种配置场景下的正确性和完整性

test "resource_default_configuration_validation" {
  // 测试Resource默认配置的验证
  
  // 创建默认resource
  let default_resource = Resource::default("test-service")
  
  // 验证默认配置
  assert_eq(default_resource.service_name, "test-service")
  match default_resource.service_version {
    Some(_) => @test.fail("Test failed")  // 默认情况下版本应该为None
    None => assert_eq(true, true)
  }
  assert_eq(default_resource.telemetry_sdk_name, "azimuth")
  assert_eq(default_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(default_resource.attributes.length(), 0)
}

test "resource_complete_configuration_validation" {
  // 测试Resource完整配置的验证
  
  // 创建完整的resource
  let complete_attributes = [
    ("service.instance.id", AttributeValue::string("instance-123")),
    ("service.namespace", AttributeValue::string("production")),
    ("deployment.environment", AttributeValue::string("prod")),
    ("host.name", AttributeValue::string("web-server-01")),
    ("host.id", AttributeValue::string("host-456")),
    ("os.type", AttributeValue::string("linux")),
    ("os.version", AttributeValue::string("5.4.0-42-generic")),
    ("process.pid", AttributeValue::int(12345L)),
    ("process.executable.name", AttributeValue::string("my-app")),
    ("process.command_line", AttributeValue::string("/usr/bin/my-app --config prod.yaml"))
  ]
  
  let complete_resource = Resource::{
    service_name: "payment-service",
    service_version: Some("2.1.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: complete_attributes
  }
  
  // 验证完整配置
  assert_eq(complete_resource.service_name, "payment-service")
  match complete_resource.service_version {
    Some(version) => assert_eq(version, "2.1.0")
    None => @test.fail("Test failed")
  }
  assert_eq(complete_resource.telemetry_sdk_name, "azimuth")
  assert_eq(complete_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(complete_resource.attributes.length(), 10)
  
  // 验证特定属性
  let mut found_service_instance_id = false
  let mut found_deployment_environment = false
  let mut found_process_pid = false
  
  let mut i = 0
  while i < complete_resource.attributes.length() {
    let (key, value) = complete_resource.attributes[i]
    
    match key {
      "service.instance.id" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "instance-123")
            found_service_instance_id = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "deployment.environment" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "prod")
            found_deployment_environment = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "process.pid" => {
        match value {
          IntValue(v) => {
            assert_eq(v, 12345L)
            found_process_pid = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => ()  // 忽略其他属性
    }
    
    i = i + 1
  }
  
  assert_eq(found_service_instance_id, true)
  assert_eq(found_deployment_environment, true)
  assert_eq(found_process_pid, true)
}

test "resource_edge_cases_configuration" {
  // 测试Resource边界情况的配置
  
  // 边界情况1: 空服务名
  let empty_service_resource = Resource::default("")
  assert_eq(empty_service_resource.service_name, "")
  
  // 边界情况2: 长服务名
  let long_service_name = "this-is-a-very-long-service-name-that-exceeds-normal-expectations-and-tests-boundary-conditions"
  let long_service_resource = Resource::default(long_service_name)
  assert_eq(long_service_resource.service_name, long_service_name)
  
  // 边界情况3: 包含特殊字符的服务名
  let special_service_name = "service!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_service_resource = Resource::default(special_service_name)
  assert_eq(special_service_resource.service_name, special_service_name)
  
  // 边界情况4: Unicode服务名
  let unicode_service_name = "服务名称 🌍 ñáéíóú"
  let unicode_service_resource = Resource::default(unicode_service_name)
  assert_eq(unicode_service_resource.service_name, unicode_service_name)
  
  // 边界情况5: 空版本字符串
  let empty_version_resource = Resource::{
    service_name: "test-service",
    service_version: Some(""),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: []
  }
  match empty_version_resource.service_version {
    Some(version) => assert_eq(version, "")
    None => @test.fail("Test failed")
  }
  
  // 边界情况6: 长版本字符串
  let long_version = "1.2.3-alpha.1+build.1234567890.abcdef.very-long-version-string"
  let long_version_resource = Resource::{
    service_name: "test-service",
    service_version: Some(long_version),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: []
  }
  match long_version_resource.service_version {
    Some(version) => assert_eq(version, long_version)
    None => @test.fail("Test failed")
  }
}

test "resource_complex_attributes_validation" {
  // 测试Resource复杂属性的验证
  
  // 创建包含各种类型属性的resource
  let complex_attributes = [
    // 基本类型
    ("string.basic", AttributeValue::string("basic_value")),
    ("int.basic", AttributeValue::int(42L)),
    ("float.basic", AttributeValue::float(3.14159)),
    ("bool.basic", AttributeValue::bool(true)),
    
    // 数组类型
    ("string.array", AttributeValue::array_string(["item1", "item2", "item3"])),
    ("int.array", AttributeValue::array_int([1L, 2L, 3L, 4L, 5L])),
    ("float.array", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool.array", AttributeValue::array_bool([true, false, true])),
    
    // 特殊值
    ("empty.string", AttributeValue::string("")),
    ("zero.int", AttributeValue::int(0L)),
    ("zero.float", AttributeValue::float(0.0)),
    ("false.bool", AttributeValue::bool(false)),
    ("empty.array", AttributeValue::array_string([])),
    
    // 边界值
    ("max.int", AttributeValue::int(9223372036854775807L)),
    ("min.int", AttributeValue::int(-9223372036854775808L)),
    ("tiny.float", AttributeValue::float(1.0e-10)),
    ("large.float", AttributeValue::float(1.0e10)),
    
    // Unicode和特殊字符
    ("unicode.value", AttributeValue::string("Hello 世界 🌍")),
    ("special.chars", AttributeValue::string("!@#$%^&*()_+-=[]{}|;':\",./<>?")),
    ("multiline.value", AttributeValue::string("Line 1\nLine 2\nLine 3")),
    ("json.value", AttributeValue::string("{\"key\":\"value\",\"number\":42}")),
    ("base64.value", AttributeValue::string("SGVsbG8gV29ybGQ="))
  ]
  
  let complex_resource = Resource::{
    service_name: "complex-attributes-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: complex_attributes
  }
  
  // 验证所有属性都正确设置
  assert_eq(complex_resource.service_name, "complex-attributes-service")
  match complex_resource.service_version {
    Some(version) => assert_eq(version, "1.0.0")
    None => @test.fail("Test failed")
  }
  assert_eq(complex_resource.attributes.length(), 22)
  
  // 验证特定属性
  let mut found_string_basic = false
  let mut found_int_basic = false
  let mut found_float_basic = false
  let mut found_bool_basic = false
  let mut found_array_string = false
  let mut found_array_int = false
  let mut found_unicode = false
  let mut found_json = false
  
  let mut i = 0
  while i < complex_resource.attributes.length() {
    let (key, value) = complex_resource.attributes[i]
    
    match key {
      "string.basic" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "basic_value")
            found_string_basic = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "int.basic" => {
        match value {
          IntValue(v) => {
            assert_eq(v, 42L)
            found_int_basic = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "float.basic" => {
        match value {
          FloatValue(v) => {
            assert_eq(v, 3.14159)
            found_float_basic = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "bool.basic" => {
        match value {
          BoolValue(v) => {
            assert_eq(v, true)
            found_bool_basic = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "string.array" => {
        match value {
          ArrayStringValue(v) => {
            assert_eq(v.length(), 3)
            found_array_string = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "int.array" => {
        match value {
          ArrayIntValue(v) => {
            assert_eq(v.length(), 5)
            found_array_int = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "unicode.value" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "Hello 世界 🌍")
            found_unicode = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "json.value" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "{\"key\":\"value\",\"number\":42}")
            found_json = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => ()  // 忽略其他属性
    }
    
    i = i + 1
  }
  
  assert_eq(found_string_basic, true)
  assert_eq(found_int_basic, true)
  assert_eq(found_float_basic, true)
  assert_eq(found_bool_basic, true)
  assert_eq(found_array_string, true)
  assert_eq(found_array_int, true)
  assert_eq(found_unicode, true)
  assert_eq(found_json, true)
}

test "resource_cloud_environment_configuration" {
  // 测试云环境下的Resource配置
  
  // AWS环境配置
  let aws_attributes = [
    ("cloud.provider", AttributeValue::string("aws")),
    ("cloud.region", AttributeValue::string("us-west-2")),
    ("cloud.account.id", AttributeValue::string("123456789012")),
    ("cloud.platform", AttributeValue::string("aws_ec2")),
    ("aws.ec2.instance.id", AttributeValue::string("i-1234567890abcdef0")),
    ("aws.ec2.instance.type", AttributeValue::string("t3.medium")),
    ("aws.availability.zone", AttributeValue::string("us-west-2a"))
  ]
  
  let aws_resource = Resource::{
    service_name: "web-service",
    service_version: Some("1.2.3"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: aws_attributes
  }
  
  // 验证AWS配置
  assert_eq(aws_resource.service_name, "web-service")
  assert_eq(aws_resource.attributes.length(), 7)
  
  let mut found_cloud_provider = false
  let mut found_cloud_region = false
  let mut found_ec2_instance_id = false
  
  let mut i = 0
  while i < aws_resource.attributes.length() {
    let (key, value) = aws_resource.attributes[i]
    
    match key {
      "cloud.provider" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "aws")
            found_cloud_provider = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "cloud.region" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "us-west-2")
            found_cloud_region = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "aws.ec2.instance.id" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "i-1234567890abcdef0")
            found_ec2_instance_id = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => ()  // 忽略其他属性
    }
    
    i = i + 1
  }
  
  assert_eq(found_cloud_provider, true)
  assert_eq(found_cloud_region, true)
  assert_eq(found_ec2_instance_id, true)
  
  // Kubernetes环境配置
  let k8s_attributes = [
    ("k8s.cluster.name", AttributeValue::string("production-cluster")),
    ("k8s.namespace.name", AttributeValue::string("web-services")),
    ("k8s.pod.name", AttributeValue::string("web-service-7d4b8c9f5-abcde")),
    ("k8s.pod.uid", AttributeValue::string("12345678-1234-1234-1234-123456789012")),
    ("k8s.container.name", AttributeValue::string("web-service")),
    ("k8s.deployment.name", AttributeValue::string("web-service-deployment")),
    ("k8s.replicaset.name", AttributeValue::string("web-service-7d4b8c9f5"))
  ]
  
  let k8s_resource = Resource::{
    service_name: "api-service",
    service_version: Some("2.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: k8s_attributes
  }
  
  // 验证Kubernetes配置
  assert_eq(k8s_resource.service_name, "api-service")
  assert_eq(k8s_resource.attributes.length(), 7)
  
  let mut found_cluster_name = false
  let mut found_pod_name = false
  let mut found_container_name = false
  
  i = 0
  while i < k8s_resource.attributes.length() {
    let (key, value) = k8s_attributes[i]
    
    match key {
      "k8s.cluster.name" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "production-cluster")
            found_cluster_name = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "k8s.pod.name" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "web-service-7d4b8c9f5-abcde")
            found_pod_name = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "k8s.container.name" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "web-service")
            found_container_name = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => ()  // 忽略其他属性
    }
    
    i = i + 1
  }
  
  assert_eq(found_cluster_name, true)
  assert_eq(found_pod_name, true)
  assert_eq(found_container_name, true)
}

test "resource_configuration_validation_with_service_mesh" {
  // 测试Service Mesh环境下的Resource配置
  
  // Istio配置
  let istio_attributes = [
    ("servicemesh.workload.name", AttributeValue::string("productpage-v1")),
    ("servicemesh.workload.namespace", AttributeValue::string("default")),
    ("servicemesh.istio.canonical_service", AttributeValue::string("productpage.default")),
    ("servicemesh.istio.canonical_service_revision", AttributeValue::string("v1")),
    ("servicemesh.istio.canonical_service_version", AttributeValue::string("latest")),
    ("servicemesh.istio.io/revision", AttributeValue::string("1-12-0"))
  ]
  
  let istio_resource = Resource::{
    service_name: "productpage",
    service_version: Some("v1"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: istio_attributes
  }
  
  // 验证Istio配置
  assert_eq(istio_resource.service_name, "productpage")
  match istio_resource.service_version {
    Some(version) => assert_eq(version, "v1")
    None => @test.fail("Test failed")
  }
  assert_eq(istio_resource.attributes.length(), 6)
  
  // Linkerd配置
  let linkerd_attributes = [
    ("servicemesh.linkerd.namespace", AttributeValue::string("emojivoto")),
    ("servicemesh.linkerd.deployment", AttributeValue::string("web-svc")),
    ("servicemesh.linkerd.replicaset", AttributeValue::string("web-svc-7d4b8c9f5")),
    ("servicemesh.linkerd.pod", AttributeValue::string("web-svc-7d4b8c9f5-abcde")),
    ("servicemesh.linkerd.container", AttributeValue::string("web-svc"))
  ]
  
  let linkerd_resource = Resource::{
    service_name: "web-svc",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: linkerd_attributes
  }
  
  // 验证Linkerd配置
  assert_eq(linkerd_resource.service_name, "web-svc")
  assert_eq(linkerd_resource.attributes.length(), 5)
}

test "resource_configuration_performance_validation" {
  // 测试Resource配置的性能验证
  
  // 创建包含大量属性的resource
  let mut large_attributes = []
  let mut i = 0
  while i < 1000 {
    let key = "perf.key_" + i.to_string()
    let value = AttributeValue::string("perf.value_" + i.to_string())
    large_attributes.push((key, value))
    i = i + 1
  }
  
  let large_resource = Resource::{
    service_name: "performance-test-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: large_attributes
  }
  
  // 验证大量属性的正确性
  assert_eq(large_resource.service_name, "performance-test-service")
  assert_eq(large_resource.attributes.length(), 1000)
  
  // 验证随机属性
  let test_indices = [0, 99, 500, 999, 123, 777]
  i = 0
  while i < test_indices.length() {
    let index = test_indices[i]
    let expected_key = "perf.key_" + index.to_string()
    let expected_value = "perf.value_" + index.to_string()
    
    let mut found = false
    let mut j = 0
    while j < large_resource.attributes.length() {
      let (key, value) = large_resource.attributes[j]
      if key == expected_key {
        match value {
          StringValue(v) => {
            assert_eq(v, expected_value)
            found = true
          }
          _ => @test.fail("Test failed")
        }
        break
      }
      j = j + 1
    }
    
    assert_eq(found, true)
    i = i + 1
  }
}

test "resource_configuration_error_handling" {
  // 测试Resource配置的错误处理
  
  // 测试空属性数组
  let empty_attributes_resource = Resource::{
    service_name: "empty-attributes-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: []
  }
  
  assert_eq(empty_attributes_resource.service_name, "empty-attributes-service")
  assert_eq(empty_attributes_resource.attributes.length(), 0)
  
  // 测试包含空键的属性
  let empty_key_attributes = [
    ("", AttributeValue::string("empty_key_value")),
    ("normal.key", AttributeValue::string("normal_value"))
  ]
  
  let empty_key_resource = Resource::{
    service_name: "empty-key-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: empty_key_attributes
  }
  
  assert_eq(empty_key_resource.service_name, "empty-key-service")
  assert_eq(empty_key_resource.attributes.length(), 2)
  
  // 测试包含空值的属性
  let empty_value_attributes = [
    ("empty.value", AttributeValue::string("")),
    ("normal.value", AttributeValue::string("normal_value"))
  ]
  
  let empty_value_resource = Resource::{
    service_name: "empty-value-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: empty_value_attributes
  }
  
  assert_eq(empty_value_resource.service_name, "empty-value-service")
  assert_eq(empty_value_resource.attributes.length(), 2)
}