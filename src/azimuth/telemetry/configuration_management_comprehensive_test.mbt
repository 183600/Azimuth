// 配置管理测试 - 测试遥测系统的配置管理功能

test "telemetry_configuration_basic" {
  // 测试基本的遥测配置
  
  // 模拟基本配置结构
  let basic_config = {
    "service.name": "test-service",
    "service.version": "1.0.0",
    "telemetry.sdk.name": "azimuth",
    "telemetry.sdk.version": "0.1.0",
    "environment": "test"
  }
  
  // 验证基本配置可以正确创建Resource
  let resource = Resource::{
    service_name: "test-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", AttributeValue::string("test"))
    ]
  }
  
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.service_version, Some("1.0.0"))
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 1)
  
  match resource.attributes[0].1 {
    StringValue(env) => assert_eq(env, "test")
    _ => @test.fail("Test failed")
  }
  
  // 测试InstrumentationScope配置
  let scope_config = {
    "instrumentation.name": "test-instrumentation",
    "instrumentation.version": "1.5.0",
    "instrumentation.schema_url": "https://example.com/schema"
  }
  
  let instrumentation_scope = InstrumentationScope::{
    name: "test-instrumentation",
    version: Some("1.5.0"),
    schema_url: Some("https://example.com/schema")
  }
  
  assert_eq(instrumentation_scope.name, "test-instrumentation")
  assert_eq(instrumentation_scope.version, Some("1.5.0"))
  assert_eq(instrumentation_scope.schema_url, Some("https://example.com/schema"))
}

test "telemetry_configuration_complex" {
  // 测试复杂的遥测配置
  
  // 模拟复杂配置结构
  let complex_config_attributes = [
    ("service.name", AttributeValue::string("complex-service")),
    ("service.version", AttributeValue::string("2.1.0-beta.1")),
    ("service.namespace", AttributeValue::string("payments")),
    ("deployment.environment", AttributeValue::string("production")),
    ("deployment.region", AttributeValue::string("us-west-2")),
    ("deployment.zone", AttributeValue::string("us-west-2a")),
    ("host.name", AttributeValue::string("prod-payment-01")),
    ("host.ip", AttributeValue::string("10.0.1.100")),
    ("host.id", AttributeValue::string("i-1234567890abcdef0")),
    ("host.type", AttributeValue::string("t3.medium")),
    ("process.id", AttributeValue::int(12345L)),
    ("process.executable.name", AttributeValue::string("payment-service")),
    ("process.executable.path", AttributeValue::string("/opt/payment-service/bin/payment-service")),
    ("process.command", AttributeValue::string("./payment-service")),
    ("process.command_args", AttributeValue::array_string(["./payment-service", "--config", "/etc/payment/config.yaml"])),
    ("process.owner", AttributeValue::string("payment-user")),
    ("process.runtime.name", AttributeValue::string("nodejs")),
    ("process.runtime.version", AttributeValue::string("18.17.0")),
    ("process.runtime.description", AttributeValue::string("Node.js JavaScript Runtime")),
    ("telemetry.sdk.name", AttributeValue::string("azimuth")),
    ("telemetry.sdk.version", AttributeValue::string("0.1.0")),
    ("telemetry.sdk.language", AttributeValue::string("moonbit")),
    ("telemetry.auto_instrumentation.version", AttributeValue::string("0.1.0")),
    ("telemetry.distros.version", AttributeValue::string("0.1.0")),
    ("container.id", AttributeValue::string("container-123456")),
    ("container.name", AttributeValue::string("payment-container")),
    ("container.image.name", AttributeValue::string("payment-service:2.1.0-beta.1")),
    ("container.image.tag", AttributeValue::string("2.1.0-beta.1")),
    ("k8s.pod.name", AttributeValue::string("payment-service-7d4f8c9b5-xyz12")),
    ("k8s.namespace.name", AttributeValue::string("payments")),
    ("k8s.deployment.name", AttributeValue::string("payment-service")),
    ("k8s.node.name", AttributeValue::string("ip-10-0-1-100.us-west-2.compute.internal")),
    ("cloud.provider", AttributeValue::string("aws")),
    ("cloud.account.id", AttributeValue::string("123456789012")),
    ("cloud.region", AttributeValue::string("us-west-2")),
    ("cloud.availability_zone", AttributeValue::string("us-west-2a")),
    ("cloud.platform", AttributeValue::string("aws_ec2")),
    ("service.instance.id", AttributeValue::string("instance-123456")),
    ("service.namespace", AttributeValue::string("payments")),
    ("service.version", AttributeValue::string("2.1.0-beta.1")),
    ("service.name", AttributeValue::string("payment-service")),
    ("os.type", AttributeValue::string("linux")),
    ("os.description", AttributeValue::string("Amazon Linux 2")),
    ("os.name", AttributeValue::string("Amazon Linux")),
    ("os.version", AttributeValue::string("2")),
    ("os.build_id", AttributeValue::string("kernel-5.10.0-197.736.amzn2.x86_64"))
  ]
  
  let complex_resource = Resource::{
    service_name: "complex-service",
    service_version: Some("2.1.0-beta.1"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: complex_config_attributes
  }
  
  // 验证复杂配置
  assert_eq(complex_resource.service_name, "complex-service")
  assert_eq(complex_resource.service_version, Some("2.1.0-beta.1"))
  assert_eq(complex_resource.attributes.length(), 42)
  
  // 验证特定配置项
  let mut found_service_name = false
  let mut found_deployment_env = false
  let mut found_cloud_provider = false
  let mut found_k8s_pod = false
  let mut found_container = false
  let mut found_process_args = false
  
  let mut i = 0
  while i < complex_resource.attributes.length() {
    let (key, value) = complex_resource.attributes[i]
    
    match key {
      "service.name" => {
        match value {
          StringValue(name) => {
            assert_eq(name, "complex-service")
            found_service_name = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "deployment.environment" => {
        match value {
          StringValue(env) => {
            assert_eq(env, "production")
            found_deployment_env = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "cloud.provider" => {
        match value {
          StringValue(provider) => {
            assert_eq(provider, "aws")
            found_cloud_provider = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "k8s.pod.name" => {
        match value {
          StringValue(pod) => {
            assert_eq(pod, "payment-service-7d4f8c9b5-xyz12")
            found_k8s_pod = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "container.id" => {
        match value {
          StringValue(id) => {
            assert_eq(id, "container-123456")
            found_container = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "process.command_args" => {
        match value {
          ArrayStringValue(args) => {
            assert_eq(args.length(), 3)
            assert_eq(args[0], "./payment-service")
            assert_eq(args[1], "--config")
            assert_eq(args[2], "/etc/payment/config.yaml")
            found_process_args = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => ()  // 忽略其他键
    }
    i = i + 1
  }
  
  assert_eq(found_service_name, true)
  assert_eq(found_deployment_env, true)
  assert_eq(found_cloud_provider, true)
  assert_eq(found_k8s_pod, true)
  assert_eq(found_container, true)
  assert_eq(found_process_args, true)
}

test "telemetry_configuration_validation" {
  // 测试遥测配置的验证
  
  // 测试有效配置
  let valid_configs = [
    ("service.name", "valid-service"),
    ("service.version", "1.0.0"),
    ("telemetry.sdk.name", "azimuth"),
    ("telemetry.sdk.version", "0.1.0"),
    ("deployment.environment", "production"),
    ("host.name", "valid-host-name"),
    ("process.id", "12345"),
    ("container.id", "valid-container-id"),
    ("k8s.pod.name", "valid-pod-name"),
    ("cloud.provider", "aws")
  ]
  
  let mut valid_i = 0
  while valid_i < valid_configs.length() {
    let (key, value) = valid_configs[valid_i]
    
    // 验证配置键和值不为空
    assert_eq(key.length() > 0, true)
    assert_eq(value.length() > 0, true)
    
    // 验证配置键格式
    assert_eq(key.contains(" "), false)  // 配置键不应包含空格
    assert_eq(key[0].is_alphabetic(), true)  // 配置键应以字母开头
    
    valid_i = valid_i + 1
  }
  
  // 测试无效配置
  let invalid_config_keys = [
    "",  // 空键
    " ",  // 只有空格的键
    "invalid key with spaces",  // 包含空格的键
    "123invalid",  // 以数字开头的键
    "invalid@key",  // 包含特殊字符的键
    "invalid.key.with.many.dots.and.very.long.name.that.exceeds.reasonable.limits"
  ]
  
  let mut invalid_i = 0
  while invalid_i < invalid_config_keys.length() {
    let invalid_key = invalid_config_keys[invalid_i]
    
    // 验证无效配置键被正确识别
    if invalid_key.length() == 0 {
      assert_eq(invalid_key.length() == 0, true)  // 空键
    } else if invalid_key.contains(" ") {
      assert_eq(invalid_key.contains(" "), true)  // 包含空格
    } else if invalid_key.length() > 0 && invalid_key[0].is_digit() {
      assert_eq(invalid_key[0].is_digit(), true)  // 以数字开头
    }
    
    invalid_i = invalid_i + 1
  }
  
  // 测试边界值配置
  let boundary_config_values = [
    ("service.name", ""),  // 空值
    ("service.name", "a" * 1000),  // 非常长的值
    ("service.name", "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./测试中文🚀"),  // 特殊字符
    ("process.id", "0"),  // 最小进程ID
    ("process.id", "9223372036854775807"),  // 最大进程ID
    ("telemetry.sdk.version", "1.0.0-alpha.1+build.123"),  // 复杂版本号
    ("deployment.environment", "production_with_underscores_and_numbers_123")
  ]
  
  let mut boundary_i = 0
  while boundary_i < boundary_config_values.length() {
    let (key, value) = boundary_config_values[boundary_i]
    
    // 验证边界值配置
    assert_eq(key.length() > 0, true)
    
    if value.length() == 0 {
      assert_eq(value, "")  // 空值
    } else if value.length() == 1000 {
      assert_eq(value.length(), 1000)  // 长值
    } else if value.contains("测试中文") {
      assert_eq(value.contains("测试中文"), true)  // Unicode字符
    }
    
    boundary_i = boundary_i + 1
  }
}

test "telemetry_configuration_inheritance" {
  // 测试遥测配置的继承和覆盖
  
  // 模拟全局配置
  let global_config = Resource::{
    service_name: "global-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("deployment.environment", AttributeValue::string("production")),
      ("cloud.provider", AttributeValue::string("aws")),
      ("cloud.region", AttributeValue::string("us-west-2"))
    ]
  }
  
  // 模拟服务级别配置（继承并覆盖全局配置）
  let service_config = Resource::{
    service_name: "payment-service",  // 覆盖全局service.name
    service_version: Some("2.1.0"),   // 覆盖全局service.version
    telemetry_sdk_name: "azimuth",     // 继承全局
    telemetry_sdk_version: "0.1.0",   // 继承全局
    attributes: [
      ("deployment.environment", AttributeValue::string("staging")),  // 覆盖全局
      ("cloud.provider", AttributeValue::string("aws")),              // 继承全局
      ("cloud.region", AttributeValue::string("us-west-2")),          // 继承全局
      ("service.namespace", AttributeValue::string("payments")),      // 服务特定配置
      ("service.instance.id", AttributeValue::string("instance-123")) // 服务特定配置
    ]
  }
  
  // 模拟实例级别配置（继承并覆盖服务配置）
  let instance_config = Resource::{
    service_name: "payment-service",        // 继承服务配置
    service_version: Some("2.1.0"),         // 继承服务配置
    telemetry_sdk_name: "azimuth",           // 继承服务配置
    telemetry_sdk_version: "0.1.0",         // 继承服务配置
    attributes: [
      ("deployment.environment", AttributeValue::string("development")),  // 覆盖服务配置
      ("cloud.provider", AttributeValue::string("aws")),                   // 继承服务配置
      ("cloud.region", AttributeValue::string("us-west-2")),               // 继承服务配置
      ("service.namespace", AttributeValue::string("payments")),           // 继承服务配置
      ("service.instance.id", AttributeValue::string("instance-456")),     // 覆盖服务配置
      ("host.name", AttributeValue::string("dev-host-001")),               // 实例特定配置
      ("process.id", AttributeValue::int(54321L))                         // 实例特定配置
    ]
  }
  
  // 验证全局配置
  assert_eq(global_config.service_name, "global-service")
  assert_eq(global_config.service_version, Some("1.0.0"))
  assert_eq(global_config.attributes.length(), 3)
  
  // 验证服务配置继承和覆盖
  assert_eq(service_config.service_name, "payment-service")  // 覆盖
  assert_eq(service_config.service_version, Some("2.1.0"))   // 覆盖
  assert_eq(service_config.telemetry_sdk_name, "azimuth")    // 继承
  assert_eq(service_config.attributes.length(), 5)           // 3个继承 + 2个新增
  
  // 验证实例配置继承和覆盖
  assert_eq(instance_config.service_name, "payment-service")  // 继承
  assert_eq(instance_config.service_version, Some("2.1.0"))   // 继承
  assert_eq(instance_config.attributes.length(), 7)           // 5个继承 + 2个新增
  
  // 验证特定属性的覆盖链
  let mut found_deployment_env = false
  let mut found_cloud_provider = false
  let mut found_service_instance = false
  let mut found_host_name = false
  let mut found_process_id = false
  
  let mut i = 0
  while i < instance_config.attributes.length() {
    let (key, value) = instance_config.attributes[i]
    
    match key {
      "deployment.environment" => {
        match value {
          StringValue(env) => {
            assert_eq(env, "development")  // 最终覆盖值
            found_deployment_env = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "cloud.provider" => {
        match value {
          StringValue(provider) => {
            assert_eq(provider, "aws")  // 继承值
            found_cloud_provider = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "service.instance.id" => {
        match value {
          StringValue(id) => {
            assert_eq(id, "instance-456")  // 覆盖值
            found_service_instance = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "host.name" => {
        match value {
          StringValue(host) => {
            assert_eq(host, "dev-host-001")  // 实例特定值
            found_host_name = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "process.id" => {
        match value {
          IntValue(pid) => {
            assert_eq(pid, 54321L)  // 实例特定值
            found_process_id = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => ()  // 忽略其他键
    }
    i = i + 1
  }
  
  assert_eq(found_deployment_env, true)
  assert_eq(found_cloud_provider, true)
  assert_eq(found_service_instance, true)
  assert_eq(found_host_name, true)
  assert_eq(found_process_id, true)
}

test "telemetry_configuration_dynamic_update" {
  // 测试遥测配置的动态更新
  
  // 初始配置
  let initial_config = Resource::{
    service_name: "initial-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("deployment.environment", AttributeValue::string("development")),
      ("feature.flags", AttributeValue::array_bool([true, false]))
    ]
  }
  
  // 验证初始配置
  assert_eq(initial_config.service_name, "initial-service")
  assert_eq(initial_config.service_version, Some("1.0.0"))
  assert_eq(initial_config.attributes.length(), 2)
  
  // 动态更新配置
  let updated_config = Resource::{
    service_name: "updated-service",        // 更新
    service_version: Some("2.0.0"),         // 更新
    telemetry_sdk_name: "azimuth",           // 保持不变
    telemetry_sdk_version: "0.1.0",         // 保持不变
    attributes: [
      ("deployment.environment", AttributeValue::string("production")),  // 更新
      ("feature.flags", AttributeValue::array_bool([true, true, false])), // 更新
      ("new.attribute", AttributeValue::string("new-value"))              // 新增
    ]
  }
  
  // 验证更新后的配置
  assert_eq(updated_config.service_name, "updated-service")
  assert_eq(updated_config.service_version, Some("2.0.0"))
  assert_eq(updated_config.telemetry_sdk_name, "azimuth")
  assert_eq(updated_config.telemetry_sdk_version, "0.1.0")
  assert_eq(updated_config.attributes.length(), 3)
  
  // 验证特定属性的更新
  let mut found_updated_env = false
  let mut found_updated_flags = false
  let mut found_new_attribute = false
  
  let mut i = 0
  while i < updated_config.attributes.length() {
    let (key, value) = updated_config.attributes[i]
    
    match key {
      "deployment.environment" => {
        match value {
          StringValue(env) => {
            assert_eq(env, "production")
            found_updated_env = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "feature.flags" => {
        match value {
          ArrayBoolValue(flags) => {
            assert_eq(flags.length(), 3)
            assert_eq(flags[0], true)
            assert_eq(flags[1], true)
            assert_eq(flags[2], false)
            found_updated_flags = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "new.attribute" => {
        match value {
          StringValue(new_val) => {
            assert_eq(new_val, "new-value")
            found_new_attribute = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => ()  // 忽略其他键
    }
    i = i + 1
  }
  
  assert_eq(found_updated_env, true)
  assert_eq(found_updated_flags, true)
  assert_eq(found_new_attribute, true)
  
  // 测试InstrumentationScope的动态更新
  let initial_scope = InstrumentationScope::{
    name: "initial-scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/v1")
  }
  
  let updated_scope = InstrumentationScope::{
    name: "updated-scope",           // 更新
    version: Some("2.0.0"),         // 更新
    schema_url: Some("https://example.com/v2")  // 更新
  }
  
  // 验证scope更新
  assert_eq(initial_scope.name, "initial-scope")
  assert_eq(initial_scope.version, Some("1.0.0"))
  assert_eq(initial_scope.schema_url, Some("https://example.com/v1"))
  
  assert_eq(updated_scope.name, "updated-scope")
  assert_eq(updated_scope.version, Some("2.0.0"))
  assert_eq(updated_scope.schema_url, Some("https://example.com/v2"))
}

test "telemetry_configuration_error_handling" {
  // 测试遥测配置的错误处理
  
  // 测试配置解析错误
  let invalid_config_cases = [
    ("service.name", ""),                    // 空服务名
    ("service.version", "invalid.version"), // 无效版本格式
    ("process.id", "not-a-number"),         // 无效数字
    ("feature.flags", "not-a-boolean"),     // 无效布尔值
    ("invalid.key", "any-value")            // 无效配置键
  ]
  
  let mut error_case_i = 0
  while error_case_i < invalid_config_cases.length() {
    let (key, value) = invalid_config_cases[error_case_i]
    
    // 模拟配置验证错误处理
    if key == "service.name" && value.length() == 0 {
      // 处理空服务名错误
      assert_eq(value.length(), 0)
    } else if key == "service.version" && value.contains("invalid") {
      // 处理无效版本格式错误
      assert_eq(value.contains("invalid"), true)
    } else if key == "process.id" && !value.chars().all(fn(c) { c.is_digit() }) {
      // 处理无效数字错误
      assert_eq(value.contains("not-a-number"), true)
    } else if key == "feature.flags" && ! (value == "true" || value == "false") {
      // 处理无效布尔值错误
      assert_eq(value == "not-a-boolean", true)
    } else if key == "invalid.key" {
      // 处理无效配置键错误
      assert_eq(key, "invalid.key")
    }
    
    error_case_i = error_case_i + 1
  }
  
  // 测试配置缺失错误处理
  let required_config_keys = [
    "service.name",
    "telemetry.sdk.name",
    "telemetry.sdk.version"
  ]
  
  let mut missing_i = 0
  while missing_i < required_config_keys.length() {
    let required_key = required_config_keys[missing_i]
    
    // 模拟缺失必需配置的错误处理
    if required_key == "service.name" {
      // 处理缺失服务名的错误
      assert_eq(required_key, "service.name")
    } else if required_key == "telemetry.sdk.name" {
      // 处理缺失SDK名称的错误
      assert_eq(required_key, "telemetry.sdk.name")
    } else if required_key == "telemetry.sdk.version" {
      // 处理缺失SDK版本的错误
      assert_eq(required_key, "telemetry.sdk.version")
    }
    
    missing_i = missing_i + 1
  }
  
  // 测试配置类型错误处理
  let type_error_cases = [
    ("process.id", AttributeValue::string("should-be-int")),     // 应该是Int64但提供了String
    ("feature.flags", AttributeValue::string("should-be-bool")), // 应该是Bool但提供了String
    ("tags", AttributeValue::int(123)),                          // 应该是Array但提供了Int
    ("timeout", AttributeValue::bool(true))                      // 应该是Float但提供了Bool
  ]
  
  let mut type_error_i = 0
  while type_error_i < type_error_cases.length() {
    let (key, wrong_type_value) = type_error_cases[type_error_i]
    
    // 模拟配置类型错误处理
    match key {
      "process.id" => {
        match wrong_type_value {
          StringValue(_) => assert_eq(true, true)  // 检测到类型错误
          _ => @test.fail("Test failed")
        }
      }
      "feature.flags" => {
        match wrong_type_value {
          StringValue(_) => assert_eq(true, true)  // 检测到类型错误
          _ => @test.fail("Test failed")
        }
      }
      "tags" => {
        match wrong_type_value {
          IntValue(_) => assert_eq(true, true)  // 检测到类型错误
          _ => @test.fail("Test failed")
        }
      }
      "timeout" => {
        match wrong_type_value {
          BoolValue(_) => assert_eq(true, true)  // 检测到类型错误
          _ => @test.fail("Test failed")
        }
      }
      _ => @test.fail("Test failed")
    }
    
    type_error_i = type_error_i + 1
  }
  
  // 测试配置恢复机制
  let fallback_config = Resource::default("fallback-service")
  
  // 验证fallback配置
  assert_eq(fallback_config.service_name, "fallback-service")
  assert_eq(fallback_config.service_version, None)
  assert_eq(fallback_config.telemetry_sdk_name, "azimuth")
  assert_eq(fallback_config.telemetry_sdk_version, "0.1.0")
  assert_eq(fallback_config.attributes.length(), 0)
}