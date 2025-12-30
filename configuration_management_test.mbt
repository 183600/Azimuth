// 配置管理测试
// 测试遥测系统的配置管理和动态更新

use azimuth.telemetry.api.trace
use azimuth.telemetry.api.context
use azimuth.telemetry.api.common

// 配置结构定义
pub struct TelemetryConfig {
  service_name : String
  service_version : String?
  sampling_rate : Double
  batch_size : Int
  export_timeout_ms : Int64
  enabled_features : Array[String]
  custom_attributes : Array[(String, common::AttributeValue)]
}

pub fn TelemetryConfig::default(service_name : String) -> TelemetryConfig {
  TelemetryConfig::{
    service_name,
    service_version: None,
    sampling_rate: 1.0,
    batch_size: 512,
    export_timeout_ms: 30000L,
    enabled_features: ["tracing", "metrics", "logging"],
    custom_attributes: []
  }
}

test "default_configuration" {
  // 测试默认配置
  let config = TelemetryConfig::default("test-service")
  
  assert_eq(config.service_name, "test-service")
  assert_eq(config.service_version, None)
  assert_eq(config.sampling_rate, 1.0)
  assert_eq(config.batch_size, 512)
  assert_eq(config.export_timeout_ms, 30000L)
  assert_eq(config.enabled_features.length(), 3)
  assert_eq(config.custom_attributes.length(), 0)
  
  // 验证默认启用的功能
  assert_eq(config.enabled_features.contains("tracing"), true)
  assert_eq(config.enabled_features.contains("metrics"), true)
  assert_eq(config.enabled_features.contains("logging"), true)
}

test "configuration_validation" {
  // 测试配置验证
  let mut config = TelemetryConfig::default("validation-test")
  
  // 测试有效配置
  config.sampling_rate = 0.5
  config.batch_size = 1024
  config.export_timeout_ms = 60000L
  config.service_version = Some("2.0.0")
  
  assert_eq(config.sampling_rate, 0.5)
  assert_eq(config.batch_size, 1024)
  assert_eq(config.export_timeout_ms, 60000L)
  assert_eq(config.service_version, Some("2.0.0"))
  
  // 测试边界值
  config.sampling_rate = 0.0  // 最小采样率
  assert_eq(config.sampling_rate, 0.0)
  
  config.sampling_rate = 1.0  // 最大采样率
  assert_eq(config.sampling_rate, 1.0)
  
  config.batch_size = 1  // 最小批量大小
  assert_eq(config.batch_size, 1)
  
  config.batch_size = 10000  // 大批量大小
  assert_eq(config.batch_size, 10000)
}

test "feature_flag_management" {
  // 测试功能标志管理
  let mut config = TelemetryConfig::default("feature-test")
  
  // 添加功能
  config.enabled_features = config.enabled_features.push(["advanced-tracing", "custom-metrics"])
  assert_eq(config.enabled_features.length(), 5)
  
  // 移除功能
  config.enabled_features = config.enabled_features.filter(fn(feature) {
    feature != "logging"
  })
  assert_eq(config.enabled_features.length(), 4)
  assert_eq(config.enabled_features.contains("logging"), false)
  
  // 检查特定功能
  assert_eq(config.enabled_features.contains("tracing"), true)
  assert_eq(config.enabled_features.contains("advanced-tracing"), true)
  assert_eq(config.enabled_features.contains("custom-metrics"), true)
  
  // 清空所有功能
  config.enabled_features = []
  assert_eq(config.enabled_features.length(), 0)
}

test "custom_attributes_configuration" {
  // 测试自定义属性配置
  let mut config = TelemetryConfig::default("attributes-test")
  
  // 添加自定义属性
  config.custom_attributes = [
    ("environment", common::AttributeValue::string("production")),
    ("region", common::AttributeValue::string("us-west-2")),
    ("instance_count", common::AttributeValue::int(10L)),
    ("debug_mode", common::AttributeValue::bool(false)),
    ("version_tags", common::AttributeValue::array_string(["v1.0", "stable"]))
  ]
  
  assert_eq(config.custom_attributes.length(), 5)
  
  // 验证属性类型
  let env_attr = config.custom_attributes.find(fn(attr) { attr.0 == "environment" })
  let region_attr = config.custom_attributes.find(fn(attr) { attr.0 == "region" })
  let instance_attr = config.custom_attributes.find(fn(attr) { attr.0 == "instance_count" })
  let debug_attr = config.custom_attributes.find(fn(attr) { attr.0 == "debug_mode" })
  let tags_attr = config.custom_attributes.find(fn(attr) { attr.0 == "version_tags" })
  
  match env_attr {
    Some((_, common::StringValue(env))) => assert_eq(env, "production")
    _ => assert_eq(false, true, "Environment attribute not found or wrong type")
  }
  
  match instance_attr {
    Some((_, common::IntValue(count))) => assert_eq(count, 10L)
    _ => assert_eq(false, true, "Instance count attribute not found or wrong type")
  }
  
  match debug_attr {
    Some((_, common::BoolValue(debug))) => assert_eq(debug, false)
    _ => assert_eq(false, true, "Debug mode attribute not found or wrong type")
  }
  
  match tags_attr {
    Some((_, common::ArrayStringValue(tags))) => {
      assert_eq(tags.length(), 2)
      assert_eq(tags.contains("v1.0"), true)
      assert_eq(tags.contains("stable"), true)
    }
    _ => assert_eq(false, true, "Version tags attribute not found or wrong type")
  }
}

test "configuration_merge" {
  // 测试配置合并
  let base_config = TelemetryConfig::default("merge-test")
  let override_config = TelemetryConfig::{
    service_name: "override-service",
    service_version: Some("3.0.0"),
    sampling_rate: 0.1,
    batch_size: 2048,
    export_timeout_ms: 120000L,
    enabled_features: ["tracing"],  // 覆盖原有功能
    custom_attributes: [("override", common::AttributeValue::string("true"))]
  }
  
  // 模拟配置合并逻辑
  let merged_config = {
    service_name: override_config.service_name,
    service_version: override_config.service_version,
    sampling_rate: override_config.sampling_rate,
    batch_size: override_config.batch_size,
    export_timeout_ms: override_config.export_timeout_ms,
    enabled_features: override_config.enabled_features,
    custom_attributes: base_config.custom_attributes + override_config.custom_attributes
  }
  
  // 验证合并结果
  assert_eq(merged_config.service_name, "override-service")
  assert_eq(merged_config.service_version, Some("3.0.0"))
  assert_eq(merged_config.sampling_rate, 0.1)
  assert_eq(merged_config.batch_size, 2048)
  assert_eq(merged_config.export_timeout_ms, 120000L)
  assert_eq(merged_config.enabled_features.length(), 1)
  assert_eq(merged_config.enabled_features[0], "tracing")
  assert_eq(merged_config.custom_attributes.length(), 1)
}

test "configuration_environment_override" {
  // 测试环境变量覆盖配置
  let mut config = TelemetryConfig::default("env-test")
  
  // 模拟环境变量覆盖
  let env_service_name = @sys.get_env("TELEMETRY_SERVICE_NAME")
  let env_sampling_rate = @sys.get_env("TELEMETRY_SAMPLING_RATE")
  let env_batch_size = @sys.get_env("TELEMETRY_BATCH_SIZE")
  
  // 如果环境变量存在，则覆盖配置
  match env_service_name {
    Some(name) => config.service_name = name
    None => ()  // 保持原有值
  }
  
  match env_sampling_rate {
    Some(rate_str) => {
      match rate_str.parse_double() {
        Some(rate) => config.sampling_rate = rate
        None => ()  // 解析失败，保持原有值
      }
    }
    None => ()
  }
  
  match env_batch_size {
    Some(size_str) => {
      match size_str.parse_int() {
        Some(size) => config.batch_size = size
        None => ()  // 解析失败，保持原有值
      }
    }
    None => ()
  }
  
  // 验证配置（环境变量可能不存在，所以验证默认值）
  assert_eq(config.service_name.length() > 0, true)
  assert_eq(config.sampling_rate >= 0.0 && config.sampling_rate <= 1.0, true)
  assert_eq(config.batch_size > 0, true)
}

test "configuration_serialization" {
  // 测试配置序列化
  let config = TelemetryConfig::{
    service_name: "serialization-test",
    service_version: Some("1.5.0"),
    sampling_rate: 0.75,
    batch_size: 1024,
    export_timeout_ms: 45000L,
    enabled_features: ["tracing", "metrics"],
    custom_attributes: [
      ("test", common::AttributeValue::string("serialization")),
      ("number", common::AttributeValue::int(42L))
    ]
  }
  
  // 序列化为JSON字符串（模拟）
  let config_str = "TelemetryConfig{" +
    "service_name=" + config.service_name + ", " +
    "service_version=" + config.service_version.to_string() + ", " +
    "sampling_rate=" + config.sampling_rate.to_string() + ", " +
    "batch_size=" + config.batch_size.to_string() + ", " +
    "export_timeout_ms=" + config.export_timeout_ms.to_string() + ", " +
    "enabled_features=[" + config.enabled_features.join(",") + "], " +
    "custom_attributes=[" + config.custom_attributes.length().to_string() + "items]" +
    "}"
  
  // 验证序列化字符串包含关键信息
  assert_eq(config_str.contains("serialization-test"), true)
  assert_eq(config_str.contains("1.5.0"), true)
  assert_eq(config_str.contains("0.75"), true)
  assert_eq(config_str.contains("1024"), true)
  assert_eq(config_str.contains("45000"), true)
  assert_eq(config_str.contains("tracing,metrics"), true)
  assert_eq(config_str.contains("2items"), true)
}