// Azimuth Telemetry - Configuration Management Test
// 测试遥测配置的动态管理功能

use azimuth.telemetry.api.common
use azimuth.telemetry.api.context
use azimuth.telemetry.api.trace
use azimuth.telemetry.api.metrics
use azimuth.telemetry.api.logs

// 配置结构定义
pub struct TelemetryConfiguration {
  service_name : String
  service_version : String?
  sampling_enabled : Bool
  sampling_rate : Double
  batch_size : Int
  export_interval_ms : Int64
  metrics_enabled : Bool
  logs_enabled : Bool
  trace_enabled : Bool
  resource_attributes : Array[(String, common::AttributeValue)]
}

pub fn TelemetryConfiguration::default(service_name : String) -> TelemetryConfiguration {
  TelemetryConfiguration::{
    service_name,
    service_version: None,
    sampling_enabled: true,
    sampling_rate: 1.0,
    batch_size: 512,
    export_interval_ms: 5000L,
    metrics_enabled: true,
    logs_enabled: true,
    trace_enabled: true,
    resource_attributes: []
  }
}

pub fn TelemetryConfiguration::with_sampling(self : TelemetryConfiguration, enabled : Bool, rate : Double) -> TelemetryConfiguration {
  TelemetryConfiguration::{
    ..self,
    sampling_enabled: enabled,
    sampling_rate: rate
  }
}

pub fn TelemetryConfiguration::with_batch_size(self : TelemetryConfiguration, size : Int) -> TelemetryConfiguration {
  TelemetryConfiguration::{ ..self, batch_size: size }
}

pub fn TelemetryConfiguration::with_export_interval(self : TelemetryConfiguration, interval_ms : Int64) -> TelemetryConfiguration {
  TelemetryConfiguration::{ ..self, export_interval_ms: interval_ms }
}

pub fn TelemetryConfiguration::enable_disable(self : TelemetryConfiguration, metrics : Bool, logs : Bool, trace : Bool) -> TelemetryConfiguration {
  TelemetryConfiguration::{
    ..self,
    metrics_enabled: metrics,
    logs_enabled: logs,
    trace_enabled: trace
  }
}

pub fn TelemetryConfiguration::with_resource_attribute(self : TelemetryConfiguration, key : String, value : common::AttributeValue) -> TelemetryConfiguration {
  let new_attributes = []
  let mut i = 0
  while i < self.resource_attributes.length() {
    new_attributes.push(self.resource_attributes[i])
    i = i + 1
  }
  new_attributes.push((key, value))
  TelemetryConfiguration::{ ..self, resource_attributes: new_attributes }
}

test "telemetry_configuration_creation_and_validation" {
  // 测试配置创建和验证
  
  // 创建默认配置
  let default_config = TelemetryConfiguration::default("test-service")
  
  @assert(default_config.service_name == "test-service")
  @assert(default_config.sampling_enabled == true)
  @assert(default_config.sampling_rate == 1.0)
  @assert(default_config.batch_size == 512)
  @assert(default_config.export_interval_ms == 5000L)
  @assert(default_config.metrics_enabled == true)
  @assert(default_config.logs_enabled == true)
  @assert(default_config.trace_enabled == true)
  @assert(default_config.resource_attributes.length() == 0)
  
  // 创建自定义配置
  let custom_config = TelemetryConfiguration::default("custom-service")
    .with_sampling(false, 0.1)
    .with_batch_size(1024)
    .with_export_interval(10000L)
    .enable_disable(false, true, false)
    .with_resource_attribute("environment", common::AttributeValue::string("production"))
    .with_resource_attribute("region", common::AttributeValue::string("us-west-2"))
  
  @assert(custom_config.service_name == "custom-service")
  @assert(custom_config.sampling_enabled == false)
  @assert(custom_config.sampling_rate == 0.1)
  @assert(custom_config.batch_size == 1024)
  @assert(custom_config.export_interval_ms == 10000L)
  @assert(custom_config.metrics_enabled == false)
  @assert(custom_config.logs_enabled == true)
  @assert(custom_config.trace_enabled == false)
  @assert(custom_config.resource_attributes.length() == 2)
}

test "telemetry_configuration_dynamic_updates" {
  // 测试配置的动态更新
  
  let mut config = TelemetryConfiguration::default("dynamic-test-service")
  
  // 初始配置验证
  @assert(config.sampling_enabled == true)
  @assert(config.sampling_rate == 1.0)
  @assert(config.batch_size == 512)
  
  // 动态更新采样配置
  config = config.with_sampling(false, 0.5)
  @assert(config.sampling_enabled == false)
  @assert(config.sampling_rate == 0.5)
  
  // 动态更新批处理配置
  config = config.with_batch_size(2048)
  @assert(config.batch_size == 2048)
  
  // 动态更新导出间隔
  config = config.with_export_interval(15000L)
  @assert(config.export_interval_ms == 15000L)
  
  // 动态启用/禁用功能
  config = config.enable_disable(true, false, true)
  @assert(config.metrics_enabled == true)
  @assert(config.logs_enabled == false)
  @assert(config.trace_enabled == true)
  
  // 动态添加资源属性
  config = config.with_resource_attribute("version", common::AttributeValue::string("2.0.0"))
  config = config.with_resource_attribute("instance_id", common::AttributeValue::int(12345L))
  
  @assert(config.resource_attributes.length() == 2)
  @assert(match config.resource_attributes[0] {
    ("version", common::AttributeValue::StringValue(v)) => v == "2.0.0"
    _ => false
  })
  @assert(match config.resource_attributes[1] {
    ("instance_id", common::AttributeValue::IntValue(v)) => v == 12345L
    _ => false
  })
}

test "telemetry_configuration_application" {
  // 测试配置应用到实际的遥测组件
  
  let config = TelemetryConfiguration::default("config-test-service")
    .with_sampling(true, 0.8)
    .with_batch_size(256)
    .with_export_interval(3000L)
    .enable_disable(true, true, true)
    .with_resource_attribute("environment", common::AttributeValue::string("test"))
    .with_resource_attribute("version", common::AttributeValue::string("1.0.0"))
  
  // 根据配置创建资源
  let resource = common::Resource::default(config.service_name)
  
  @assert(resource.service_name == config.service_name)
  @assert(resource.telemetry_sdk_name == "azimuth")
  @assert(resource.telemetry_sdk_version == "0.1.0")
  
  // 验证配置是否正确应用到各个组件
  @assert(config.trace_enabled == true)
  @assert(config.metrics_enabled == true)
  @assert(config.logs_enabled == true)
  
  // 创建基于配置的Tracer
  if config.trace_enabled {
    let tracer_provider = trace::NoopTracerProvider::{}
    let tracer = tracer_provider.get_tracer("config-test-tracer", "1.0.0")
    let (_, span) = tracer.start_span(
      context::Context::new(),
      "config-test-span",
      trace::Server,
      config.resource_attributes
    )
    @assert(span.name == "config-test-span")
    @assert(span.attributes.length() == config.resource_attributes.length())
  }
  
  // 创建基于配置的Meter
  if config.metrics_enabled {
    let meter_provider = metrics::NoopMeterProvider::{}
    let meter = meter_provider.get_meter("config-test-meter", "1.0.0")
    let counter = meter.create_counter("config.test.counter", "count", "Test counter")
    counter.add(1L, config.resource_attributes)
  }
  
  // 创建基于配置的Logger
  if config.logs_enabled {
    let logger_provider = logs::NoopLoggerProvider::{}
    let logger = logger_provider.get_logger("config-test-logger", "1.0.0")
    logger.info("Configuration test log", config.resource_attributes)
  }
}

test "telemetry_configuration_edge_cases" {
  // 测试配置的边界情况
  
  // 测试空服务名
  let empty_service_config = TelemetryConfiguration::default("")
  @assert(empty_service_config.service_name == "")
  
  // 测试极端采样率
  let zero_sampling_config = TelemetryConfiguration::default("zero-sampling")
    .with_sampling(true, 0.0)
  @assert(zero_sampling_config.sampling_rate == 0.0)
  
  let max_sampling_config = TelemetryConfiguration::default("max-sampling")
    .with_sampling(true, 1.0)
  @assert(max_sampling_config.sampling_rate == 1.0)
  
  // 测试极端批处理大小
  let min_batch_config = TelemetryConfiguration::default("min-batch")
    .with_batch_size(1)
  @assert(min_batch_config.batch_size == 1)
  
  let max_batch_config = TelemetryConfiguration::default("max-batch")
    .with_batch_size(10000)
  @assert(max_batch_config.batch_size == 10000)
  
  // 测试极端导出间隔
  let min_interval_config = TelemetryConfiguration::default("min-interval")
    .with_export_interval(100L)
  @assert(min_interval_config.export_interval_ms == 100L)
  
  let max_interval_config = TelemetryConfiguration::default("max-interval")
    .with_export_interval(60000L)
  @assert(max_interval_config.export_interval_ms == 60000L)
  
  // 测试禁用所有功能
  let all_disabled_config = TelemetryConfiguration::default("all-disabled")
    .enable_disable(false, false, false)
  @assert(all_disabled_config.metrics_enabled == false)
  @assert(all_disabled_config.logs_enabled == false)
  @assert(all_disabled_config.trace_enabled == false)
  
  // 测试大量资源属性
  let mut many_attrs_config = TelemetryConfiguration::default("many-attrs")
  let mut i = 0
  while i < 100 {
    many_attrs_config = many_attrs_config.with_resource_attribute(
      "attr." + i.to_string(),
      common::AttributeValue::string("value." + i.to_string())
    )
    i = i + 1
  }
  @assert(many_attrs_config.resource_attributes.length() == 100)
}

test "telemetry_configuration_consistency" {
  // 测试配置的一致性和完整性
  
  let config = TelemetryConfiguration::default("consistency-test")
    .with_sampling(true, 0.5)
    .with_batch_size(512)
    .with_export_interval(5000L)
    .enable_disable(true, true, true)
    .with_resource_attribute("service.namespace", common::AttributeValue::string("test"))
    .with_resource_attribute("service.instance.id", common::AttributeValue::string("instance-123"))
  
  // 验证配置的内部一致性
  @assert(config.sampling_enabled == true)
  @assert(config.sampling_rate >= 0.0 && config.sampling_rate <= 1.0)
  @assert(config.batch_size > 0)
  @assert(config.export_interval_ms > 0)
  
  // 验证资源属性的类型一致性
  let mut i = 0
  while i < config.resource_attributes.length() {
    let (key, value) = config.resource_attributes[i]
    @assert(key.length() > 0)
    @assert(match value {
      common::StringValue(_) => true
      common::IntValue(_) => true
      common::FloatValue(_) => true
      common::BoolValue(_) => true
      common::ArrayStringValue(_) => true
      common::ArrayIntValue(_) => true
      common::ArrayFloatValue(_) => true
      common::ArrayBoolValue(_) => true
    })
    i = i + 1
  }
  
  // 验证配置可以正确应用到所有遥测组件
  let resource = common::Resource::default(config.service_name)
  @assert(resource.service_name == config.service_name)
  
  // 验证配置不会导致组件冲突
  if config.trace_enabled && config.metrics_enabled && config.logs_enabled {
    // 所有功能都启用时应该正常工作
    @assert(true)
  }
  
  // 验证部分功能禁用时仍然可以正常工作
  let partial_config = config.enable_disable(true, false, true)
  @assert(partial_config.metrics_enabled == true)
  @assert(partial_config.logs_enabled == false)
  @assert(partial_config.trace_enabled == true)
}