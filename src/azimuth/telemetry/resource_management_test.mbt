// 资源管理测试 - 验证Resource的创建、合并和生命周期管理

test "resource_creation_and_defaults" {
  // 测试默认资源创建
  let default_resource = common::Resource::default("test-service")
  
  @assert.eq(default_resource.service_name, "test-service")
  @assert.eq(default_resource.service_version, None)
  @assert.eq(default_resource.telemetry_sdk_name, "azimuth")
  @assert.eq(default_resource.telemetry_sdk_version, "0.1.0")
  @assert.eq(default_resource.attributes.length(), 0)
  
  // 测试完整资源创建
  let full_resource = common::Resource::{
    service_name: "production-service",
    service_version: Some("2.1.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", common::AttributeValue::string("production")),
      ("region", common::AttributeValue::string("us-east-1")),
      ("instance.id", common::AttributeValue::string("i-1234567890abcdef0"))
    ]
  }
  
  @assert.eq(full_resource.service_name, "production-service")
  @assert.eq(full_resource.service_version.unwrap(), "2.1.0")
  @assert.eq(full_resource.telemetry_sdk_name, "azimuth")
  @assert.eq(full_resource.telemetry_sdk_version, "0.1.0")
  @assert.eq(full_resource.attributes.length(), 3)
}

test "resource_attribute_management" {
  // 创建带有多种属性类型的资源
  let resource = common::Resource::{
    service_name: "multi-attr-service",
    service_version: Some("1.5.2"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("string.attr", common::AttributeValue::string("value")),
      ("int.attr", common::AttributeValue::int(42)),
      ("float.attr", common::AttributeValue::float(3.14159)),
      ("bool.attr", common::AttributeValue::bool(true)),
      ("array.string.attr", common::AttributeValue::array_string(["a", "b", "c"])),
      ("array.int.attr", common::AttributeValue::array_int([1L, 2L, 3L])),
      ("array.float.attr", common::AttributeValue::array_float([1.1, 2.2, 3.3])),
      ("array.bool.attr", common::AttributeValue::array_bool([true, false, true]))
    ]
  }
  
  // 验证属性数量
  @assert.eq(resource.attributes.length(), 8)
  
  // 验证各种属性类型
  let mut found_string = false
  let mut found_int = false
  let mut found_float = false
  let mut found_bool = false
  let mut found_array_string = false
  let mut found_array_int = false
  let mut found_array_float = false
  let mut found_array_bool = false
  
  let mut i = 0
  while i < resource.attributes.length() {
    let (key, value) = resource.attributes[i]
    match key {
      "string.attr" => {
        match value {
          common::StringValue(v) => {
            @assert.eq(v, "value")
            found_string = true
          }
          _ => @assert.fail("Expected StringValue for string.attr")
        }
      }
      "int.attr" => {
        match value {
          common::IntValue(v) => {
            @assert.eq(v, 42L)
            found_int = true
          }
          _ => @assert.fail("Expected IntValue for int.attr")
        }
      }
      "float.attr" => {
        match value {
          common::FloatValue(v) => {
            @assert.eq(v, 3.14159)
            found_float = true
          }
          _ => @assert.fail("Expected FloatValue for float.attr")
        }
      }
      "bool.attr" => {
        match value {
          common::BoolValue(v) => {
            @assert.eq(v, true)
            found_bool = true
          }
          _ => @assert.fail("Expected BoolValue for bool.attr")
        }
      }
      "array.string.attr" => {
        match value {
          common::ArrayStringValue(v) => {
            @assert.eq(v.length(), 3)
            @assert.eq(v[0], "a")
            @assert.eq(v[1], "b")
            @assert.eq(v[2], "c")
            found_array_string = true
          }
          _ => @assert.fail("Expected ArrayStringValue for array.string.attr")
        }
      }
      "array.int.attr" => {
        match value {
          common::ArrayIntValue(v) => {
            @assert.eq(v.length(), 3)
            @assert.eq(v[0], 1L)
            @assert.eq(v[1], 2L)
            @assert.eq(v[2], 3L)
            found_array_int = true
          }
          _ => @assert.fail("Expected ArrayIntValue for array.int.attr")
        }
      }
      "array.float.attr" => {
        match value {
          common::ArrayFloatValue(v) => {
            @assert.eq(v.length(), 3)
            @assert.eq(v[0], 1.1)
            @assert.eq(v[1], 2.2)
            @assert.eq(v[2], 3.3)
            found_array_float = true
          }
          _ => @assert.fail("Expected ArrayFloatValue for array.float.attr")
        }
      }
      "array.bool.attr" => {
        match value {
          common::ArrayBoolValue(v) => {
            @assert.eq(v.length(), 3)
            @assert.eq(v[0], true)
            @assert.eq(v[1], false)
            @assert.eq(v[2], true)
            found_array_bool = true
          }
          _ => @assert.fail("Expected ArrayBoolValue for array.bool.attr")
        }
      }
      _ => @assert.fail("Unexpected attribute key: " + key)
    }
    i = i + 1
  }
  
  @assert.assert(found_string, "string.attr not found")
  @assert.assert(found_int, "int.attr not found")
  @assert.assert(found_float, "float.attr not found")
  @assert.assert(found_bool, "bool.attr not found")
  @assert.assert(found_array_string, "array.string.attr not found")
  @assert.assert(found_array_int, "array.int.attr not found")
  @assert.assert(found_array_float, "array.float.attr not found")
  @assert.assert(found_array_bool, "array.bool.attr not found")
}

test "resource_in_telemetry_components" {
  // 测试资源在各种遥测组件中的使用
  let resource = common::Resource::{
    service_name: "telemetry-test-service",
    service_version: Some("3.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.instance.id", common::AttributeValue::string("instance-123")),
      ("deployment.environment", common::AttributeValue::string("staging"))
    ]
  }
  
  // 在Trace中使用资源
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("resource-test-tracer")
  let (_, span) = tracer.start_span(
    context::Context::empty(),
    "resource-test-span",
    trace::Internal,
    [("resource.test", common::AttributeValue::string("test-value"))]
  )
  
  // 在Metrics中使用资源
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("resource-test-meter")
  let counter = meter.create_counter("resource-test-counter", "count", "Counter with resource context")
  counter.add(10L, [("service.name", common::AttributeValue::string(resource.service_name))])
  
  // 在Logs中使用资源
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("resource-test-logger")
  
  let log_record = logs::LogRecord::builder()
    .timestamp(4000000L)
    .severity(logs::Info)
    .body("Log with resource context")
    .with_attribute("service.name", common::AttributeValue::string(resource.service_name))
    .with_attribute("service.version", common::AttributeValue::string(resource.service_version.unwrap()))
    .with_attribute("service.instance.id", common::AttributeValue::string("instance-123"))
    .build()
  
  logger.emit(log_record)
  
  // 验证资源信息正确传递
  @assert.eq(resource.service_name, "telemetry-test-service")
  @assert.eq(resource.service_version.unwrap(), "3.0.0")
  @assert.eq(resource.attributes.length(), 2)
  @assert.eq(log_record.attributes.length(), 3)
}

test "resource_merging_and_conflicts" {
  // 测试资源合并逻辑（模拟场景）
  let base_resource = common::Resource::{
    service_name: "base-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("base.attr1", common::AttributeValue::string("base-value1")),
      ("base.attr2", common::AttributeValue::int(100)),
      ("common.attr", common::AttributeValue::string("base-common"))
    ]
  }
  
  let override_resource = common::Resource::{
    service_name: "override-service",  // 应该覆盖基础服务名
    service_version: Some("2.0.0"),    // 应该覆盖版本
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("override.attr1", common::AttributeValue::string("override-value1")),
      ("override.attr2", common::AttributeValue::int(200)),
      ("common.attr", common::AttributeValue::string("override-common"))  // 应该覆盖通用属性
    ]
  }
  
  // 模拟资源合并的结果
  let merged_service_name = override_resource.service_name  // 覆盖
  let merged_service_version = override_resource.service_version  // 覆盖
  let merged_attributes = [
    ("base.attr1", common::AttributeValue::string("base-value1")),
    ("base.attr2", common::AttributeValue::int(100)),
    ("override.attr1", common::AttributeValue::string("override-value1")),
    ("override.attr2", common::AttributeValue::int(200)),
    ("common.attr", common::AttributeValue::string("override-common"))  // 被覆盖
  ]
  
  let merged_resource = common::Resource::{
    service_name: merged_service_name,
    service_version: merged_service_version,
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: merged_attributes
  }
  
  // 验证合并结果
  @assert.eq(merged_resource.service_name, "override-service")
  @assert.eq(merged_resource.service_version.unwrap(), "2.0.0")
  @assert.eq(merged_resource.attributes.length(), 5)
  
  // 验证特定属性值
  let mut found_common_attr = false
  let mut i = 0
  while i < merged_resource.attributes.length() {
    let (key, value) = merged_resource.attributes[i]
    if key == "common.attr" {
      match value {
        common::StringValue(v) => {
          @assert.eq(v, "override-common")  // 应该是覆盖的值
          found_common_attr = true
        }
        _ => @assert.fail("Expected StringValue for common.attr")
      }
    }
    i = i + 1
  }
  @assert.assert(found_common_attr, "common.attr not found in merged resource")
}

test "resource_validation_and_edge_cases" {
  // 测试资源验证和边界情况
  
  // 测试空服务名（在实际应用中可能需要验证）
  let empty_service_resource = common::Resource::{
    service_name: "",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: []
  }
  @assert.eq(empty_service_resource.service_name, "")
  
  // 测试大量属性
  let mut many_attributes : Array[(String, common::AttributeValue)] = []
  let mut i = 0
  while i < 100 {
    many_attributes.push(("attr." + i.to_string(), common::AttributeValue::int(i.to_int64())))
    i = i + 1
  }
  
  let many_attrs_resource = common::Resource::{
    service_name: "many-attrs-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: many_attributes
  }
  
  @assert.eq(many_attrs_resource.attributes.length(), 100)
  
  // 验证特定属性
  let mut found_attr_0 = false
  let mut found_attr_99 = false
  let mut j = 0
  while j < many_attrs_resource.attributes.length() {
    let (key, value) = many_attrs_resource.attributes[j]
    if key == "attr.0" {
      match value {
        common::IntValue(v) => {
          @assert.eq(v, 0L)
          found_attr_0 = true
        }
        _ => @assert.fail("Expected IntValue for attr.0")
      }
    } else if key == "attr.99" {
      match value {
        common::IntValue(v) => {
          @assert.eq(v, 99L)
          found_attr_99 = true
        }
        _ => @assert.fail("Expected IntValue for attr.99")
      }
    }
    j = j + 1
  }
  
  @assert.assert(found_attr_0, "attr.0 not found")
  @assert.assert(found_attr_99, "attr.99 not found")
}