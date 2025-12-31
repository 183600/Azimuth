// 遥测数据质量验证测试
// 测试数据准确性、完整性、一致性

// 数据质量验证结果
struct DataQualityResult {
  is_valid : Bool
  accuracy_score : Double  // 0.0 - 1.0
  completeness_score : Double  // 0.0 - 1.0
  consistency_score : Double  // 0.0 - 1.0
  issues : Array[String]
}

fn DataQualityResult::new() -> DataQualityResult {
  DataQualityResult::{
    is_valid: true,
    accuracy_score: 1.0,
    completeness_score: 1.0,
    consistency_score: 1.0,
    issues: []
  }
}

fn DataQualityResult::add_issue(self : DataQualityResult, issue : String) -> DataQualityResult {
  let new_issues = []
  let mut i = 0
  while i < self.issues.length() {
    new_issues.push(self.issues[i])
    i = i + 1
  }
  new_issues.push(issue)
  DataQualityResult::{ ..self, issues: new_issues, is_valid: false }
}

fn DataQualityResult::with_accuracy(self : DataQualityResult, score : Double) -> DataQualityResult {
  DataQualityResult::{ ..self, accuracy_score: score }
}

fn DataQualityResult::with_completeness(self : DataQualityResult, score : Double) -> DataQualityResult {
  DataQualityResult::{ ..self, completeness_score: score }
}

fn DataQualityResult::with_consistency(self : DataQualityResult, score : Double) -> DataQualityResult {
  DataQualityResult::{ ..self, consistency_score: score }
}

// 数据验证函数
fn validate_trace_id(trace_id : Array[Byte]) -> DataQualityResult {
  let mut result = DataQualityResult::new()
  
  // 检查trace ID长度（应该是16字节）
  if trace_id.length() != 16 {
    result = result.add_issue("Trace ID length should be 16 bytes, got " + trace_id.length().to_string())
  }
  
  // 检查trace ID是否全为零（无效的trace ID）
  let mut all_zero = true
  let mut i = 0
  while i < trace_id.length() {
    if trace_id[i] != 0_byte {
      all_zero = false
      break
    }
    i = i + 1
  }
  
  if all_zero {
    result = result.add_issue("Trace ID should not be all zeros")
  }
  
  // 计算准确性分数
  let accuracy = if trace_id.length() == 16 and not all_zero { 1.0 } else { 0.0 }
  result = result.with_accuracy(accuracy)
  
  result
}

fn validate_span_id(span_id : Array[Byte]) -> DataQualityResult {
  let mut result = DataQualityResult::new()
  
  // 检查span ID长度（应该是8字节）
  if span_id.length() != 8 {
    result = result.add_issue("Span ID length should be 8 bytes, got " + span_id.length().to_string())
  }
  
  // 检查span ID是否全为零
  let mut all_zero = true
  let mut i = 0
  while i < span_id.length() {
    if span_id[i] != 0_byte {
      all_zero = false
      break
    }
    i = i + 1
  }
  
  if all_zero {
    result = result.add_issue("Span ID should not be all zeros")
  }
  
  // 计算准确性分数
  let accuracy = if span_id.length() == 8 and not all_zero { 1.0 } else { 0.0 }
  result = result.with_accuracy(accuracy)
  
  result
}

fn validate_timestamp(timestamp : Int64) -> DataQualityResult {
  let mut result = DataQualityResult::new()
  
  // 检查时间戳是否为正数
  if timestamp <= 0L {
    result = result.add_issue("Timestamp should be positive")
  }
  
  // 检查时间戳是否在合理范围内（1970年之后，2100年之前）
  let min_timestamp = 0L  // 1970-01-01 00:00:00 UTC
  let max_timestamp = 4102444800000000000L  // 2100-01-01 00:00:00 UTC
  
  if timestamp < min_timestamp or timestamp > max_timestamp {
    result = result.add_issue("Timestamp is outside reasonable range")
  }
  
  // 计算准确性分数
  let accuracy = if timestamp > 0L and timestamp >= min_timestamp and timestamp <= max_timestamp { 1.0 } else { 0.0 }
  result = result.with_accuracy(accuracy)
  
  result
}

fn validate_attribute_name(name : String) -> DataQualityResult {
  let mut result = DataQualityResult::new()
  
  // 检查属性名是否为空
  if name.length() == 0 {
    result = result.add_issue("Attribute name should not be empty")
  }
  
  // 检查属性名长度（建议不超过255字符）
  if name.length() > 255 {
    result = result.add_issue("Attribute name too long (max 255 characters)")
  }
  
  // 检查属性名是否包含无效字符
  let invalid_chars = [" ", "\t", "\n", "=", "}", "{"]
  let mut i = 0
  while i < invalid_chars.length() {
    if name.includes(invalid_chars[i]) {
      result = result.add_issue("Attribute name contains invalid character: " + invalid_chars[i])
    }
    i = i + 1
  }
  
  // 计算准确性分数
  let accuracy = if name.length() > 0 and name.length() <= 255 { 1.0 } else { 0.0 }
  result = result.with_accuracy(accuracy)
  
  result
}

fn validate_attribute_value(value : common::AttributeValue) -> DataQualityResult {
  let mut result = DataQualityResult::new()
  
  match value {
    common::StringValue(s) => {
      // 检查字符串长度
      if s.length() > 10000 {
        result = result.add_issue("String attribute value too long (max 10000 characters)")
      }
    }
    common::ArrayStringValue(arr) => {
      // 检查数组长度
      if arr.length() > 1000 {
        result = result.add_issue("String array too long (max 1000 elements)")
      }
      
      // 检查数组元素
      let mut i = 0
      while i < arr.length() {
        if arr[i].length() > 1000 {
          result = result.add_issue("Array element too long (max 1000 characters)")
          break
        }
        i = i + 1
      }
    }
    common::ArrayIntValue(arr) => {
      if arr.length() > 1000 {
        result = result.add_issue("Int array too long (max 1000 elements)")
      }
    }
    common::ArrayFloatValue(arr) => {
      if arr.length() > 1000 {
        result = result.add_issue("Float array too long (max 1000 elements)")
      }
    }
    common::ArrayBoolValue(arr) => {
      if arr.length() > 1000 {
        result = result.add_issue("Bool array too long (max 1000 elements)")
      }
    }
    _ => {
      // IntValue, FloatValue, BoolValue 通常不需要特殊验证
    }
  }
  
  // 计算准确性分数
  let accuracy = if result.issues.length() == 0 { 1.0 } else { 0.5 }
  result = result.with_accuracy(accuracy)
  
  result
}

test "span_data_quality_validation" {
  // 测试Span数据质量验证
  
  // 创建高质量的Span
  let valid_trace_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte]
  let valid_span_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]
  
  let valid_span = trace::Span::{
    name: "valid-operation",
    context: trace::SpanContext::{
      trace_id: valid_trace_id,
      span_id: valid_span_id,
      trace_flags: 1_byte,
      trace_state: "test=1"
    },
    kind: trace::Server,
    parent_span_id: Some([1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200000001000L),
    status: trace::Ok,
    status_description: Some("Success"),
    attributes: [
      ("operation.name", common::AttributeValue::string("valid-operation")),
      ("service.version", common::AttributeValue::string("1.0.0")),
      ("user.id", common::AttributeValue::int(12345L))
    ],
    events: [],
    links: []
  }
  
  // 验证高质量Span
  let trace_id_result = validate_trace_id(valid_span.context.trace_id)
  @assertion.assert_eq(trace_id_result.is_valid, true)
  @assertion.assert_eq(trace_id_result.accuracy_score, 1.0)
  
  let span_id_result = validate_span_id(valid_span.context.span_id)
  @assertion.assert_eq(span_id_result.is_valid, true)
  @assertion.assert_eq(span_id_result.accuracy_score, 1.0)
  
  let start_time_result = validate_timestamp(valid_span.start_time_unix_nanos)
  @assertion.assert_eq(start_time_result.is_valid, true)
  @assertion.assert_eq(start_time_result.accuracy_score, 1.0)
  
  let end_time_result = validate_timestamp(valid_span.end_time_unix_nanos?)
  @assertion.assert_eq(end_time_result.is_valid, true)
  @assertion.assert_eq(end_time_result.accuracy_score, 1.0)
  
  // 验证属性
  let mut attr_validation = DataQualityResult::new()
  let mut i = 0
  while i < valid_span.attributes.length() {
    let name_result = validate_attribute_name(valid_span.attributes[i].0)
    let value_result = validate_attribute_value(valid_span.attributes[i].1)
    
    if not name_result.is_valid {
      attr_validation = attr_validation.add_issue("Invalid attribute name: " + name_result.issues.join(","))
    }
    
    if not value_result.is_valid {
      attr_validation = attr_validation.add_issue("Invalid attribute value: " + value_result.issues.join(","))
    }
    
    i = i + 1
  }
  
  @assertion.assert_eq(attr_validation.is_valid, true)
  @assertion.assert_eq(attr_validation.accuracy_score, 1.0)
  
  // 创建低质量的Span进行对比测试
  let invalid_trace_id = [0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte]
  let invalid_span_id = [0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte]
  
  let invalid_span = trace::Span::{
    name: "",  // 空名称
    context: trace::SpanContext::{
      trace_id: invalid_trace_id,
      span_id: invalid_span_id,
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: -1L,  // 无效时间戳
    end_time_unix_nanos: Some(-1L),
    status: trace::Error,
    status_description: None,
    attributes: [
      ("", common::StringValue("")),  // 空属性名和值
      ("very_long_attribute_name_that_exceeds_the_recommended_limit_of_255_characters_and_should_be_flagged_as_a_quality_issue", common::StringValue("test"))
    ],
    events: [],
    links: []
  }
  
  // 验证低质量Span
  let invalid_trace_id_result = validate_trace_id(invalid_span.context.trace_id)
  @assertion.assert_eq(invalid_trace_id_result.is_valid, false)
  @assertion.assert_eq(invalid_trace_id_result.accuracy_score, 0.0)
  @assertion.assert_true(invalid_trace_id_result.issues.length() > 0)
  
  let invalid_span_id_result = validate_span_id(invalid_span.context.span_id)
  @assertion.assert_eq(invalid_span_id_result.is_valid, false)
  @assertion.assert_eq(invalid_span_id_result.accuracy_score, 0.0)
  @assertion.assert_true(invalid_span_id_result.issues.length() > 0)
  
  let invalid_start_time_result = validate_timestamp(invalid_span.start_time_unix_nanos)
  @assertion.assert_eq(invalid_start_time_result.is_valid, false)
  @assertion.assert_eq(invalid_start_time_result.accuracy_score, 0.0)
  @assertion.assert_true(invalid_start_time_result.issues.length() > 0)
}

test "log_data_quality_validation" {
  // 测试Log数据质量验证
  
  // 创建高质量的LogRecord
  let valid_log = logs::LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000001L),
    severity_number: logs::Info,
    severity_text: Some("INFO"),
    body: Some("Application started successfully"),
    attributes: [
      ("logger.name", common::AttributeValue::string("application.logger")),
      ("thread.id", common::AttributeValue::int(1L)),
      ("session.id", common::AttributeValue::string("abc123-def456-ghi789"))
    ],
    trace_id: Some([1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte]),
    span_id: Some([1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]),
    trace_flags: Some(1_byte),
    resource: Some(common::Resource::default("test-service")),
    instrumentation_scope: Some(common::InstrumentationScope::{
      name: "test-logger",
      version: Some("1.0.0"),
      schema_url: Some("https://example.com/schema")
    })
  }
  
  // 验证高质量LogRecord
  let timestamp_result = validate_timestamp(valid_log.timestamp_unix_nanos)
  @assertion.assert_eq(timestamp_result.is_valid, true)
  @assertion.assert_eq(timestamp_result.accuracy_score, 1.0)
  
  if let Some(observed_timestamp) = valid_log.observed_timestamp_unix_nanos {
    let observed_result = validate_timestamp(observed_timestamp)
    @assertion.assert_eq(observed_result.is_valid, true)
    @assertion.assert_eq(observed_result.accuracy_score, 1.0)
    
    // 验证observed_timestamp应该大于等于timestamp
    @assertion.assert_true(observed_timestamp >= valid_log.timestamp_unix_nanos)
  }
  
  // 验证severity和body的一致性
  @assertion.assert_eq(valid_log.severity_text?, "INFO")
  @assertion.assert_eq(valid_log.severity_number, logs::Info)
  @assertion.assert_true(valid_log.body?.length() > 0)
  
  // 验证trace关联的完整性
  @assertion.assert_true(valid_log.trace_id?.length() == 16)
  @assertion.assert_true(valid_log.span_id?.length() == 8)
  
  // 创建低质量的LogRecord
  let invalid_log = logs::LogRecord::{
    timestamp_unix_nanos: -100L,  // 无效时间戳
    observed_timestamp_unix_nanos: Some(-50L),
    severity_number: logs::Error,
    severity_text: Some("CRITICAL"),  // 不匹配的severity text
    body: None,  // 缺少body
    attributes: [
      ("", common::StringValue("invalid")),  // 空属性名
      ("valid.name", common::StringValue(""))  // 空字符串值
    ],
    trace_id: Some([0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte]),
    span_id: Some([0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte, 0_byte]),
    trace_flags: Some(0_byte),
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证低质量LogRecord
  let invalid_timestamp_result = validate_timestamp(invalid_log.timestamp_unix_nanos)
  @assertion.assert_eq(invalid_timestamp_result.is_valid, false)
  @assertion.assert_eq(invalid_timestamp_result.accuracy_score, 0.0)
  
  // 验证severity不匹配
  @assertion.assert_eq(invalid_log.severity_text?, "CRITICAL")
  @assertion.assert_eq(invalid_log.severity_number, logs::Error)
  // 注意：CRITICAL通常对应Fatal，这里存在不一致
  
  // 验证缺少body
  @assertion.assert_eq(invalid_log.body, None)
  
  // 验证无效的trace ID
  let invalid_trace_result = validate_trace_id(invalid_log.trace_id?)
  @assertion.assert_eq(invalid_trace_result.is_valid, false)
  @assertion.assert_eq(invalid_trace_result.accuracy_score, 0.0)
}

test "metric_data_quality_validation" {
  // 测试Metric数据质量验证
  
  // 创建高质量的Measurement
  let valid_measurement = metrics::Measurement::{
    value: 123.456,
    attributes: [
      ("metric.name", common::AttributeValue::string("request.duration")),
      ("unit", common::AttributeValue::string("milliseconds")),
      ("status.code", common::AttributeValue::int(200)),
      ("endpoint", common::AttributeValue::string("/api/users"))
    ]
  }
  
  // 验证高质量Measurement
  @assertion.assert_true(valid_measurement.value >= 0.0)  // 假设这是持续时间指标
  @assertion.assert_true(valid_measurement.value < 3600000.0)  // 不超过1小时
  
  let mut attr_score_sum = 0.0
  let mut i = 0
  while i < valid_measurement.attributes.length() {
    let name_result = validate_attribute_name(valid_measurement.attributes[i].0)
    let value_result = validate_attribute_value(valid_measurement.attributes[i].1)
    
    attr_score_sum = attr_score_sum + name_result.accuracy_score + value_result.accuracy_score
    i = i + 1
  }
  
  let avg_attr_score = attr_score_sum / (valid_measurement.attributes.length() * 2.0)
  @assertion.assert_eq(avg_attr_score, 1.0)
  
  // 创建边界情况的Measurement
  let edge_measurement = metrics::Measurement::{
    value: -1.0,  // 负值（对于某些指标可能是有效的）
    attributes: [
      ("metric.name", common::AttributeValue::string("temperature.change")),
      ("unit", common::AttributeValue::string("celsius")),
      ("tags", common::AttributeValue::array_string(["tag1", "tag2", "tag3"])),
      ("measurements", common::ArrayFloatValue([1.1, 2.2, 3.3, 4.4, 5.5]))
    ]
  }
  
  // 验证边界情况
  @assertion.assert_true(edge_measurement.value < 0.0)  // 温度变化可以是负的
  
  // 验证数组属性
  let mut i = 0
  while i < edge_measurement.attributes.length() {
    let value_result = validate_attribute_value(edge_measurement.attributes[i].1)
    @assertion.assert_eq(value_result.is_valid, true)
    @assertion.assert_eq(value_result.accuracy_score, 1.0)
    i = i + 1
  }
  
  // 创建低质量的Measurement
  let invalid_measurement = metrics::Measurement::{
    value: 0.0 / 0.0,  // NaN（如果支持的话）
    attributes: [
      ("", common::StringValue("empty name")),
      ("valid.name", common::ArrayStringValue([])),  // 空数组
      ("large.array", common::ArrayIntValue([for i = 0; i < 2000; i = i + 1].map(fn(_) { 1L })))  // 过大的数组
    ]
  }
  
  // 验证低质量Measurement
  let mut invalid_count = 0
  let mut i = 0
  while i < invalid_measurement.attributes.length() {
    let name_result = validate_attribute_name(invalid_measurement.attributes[i].0)
    let value_result = validate_attribute_value(invalid_measurement.attributes[i].1)
    
    if not name_result.is_valid or not value_result.is_valid {
      invalid_count = invalid_count + 1
    }
    i = i + 1
  }
  
  @assertion.assert_true(invalid_count > 0)
}

test "data_completeness_validation" {
  // 测试数据完整性验证
  
  // 创建完整的Span
  let complete_span = trace::Span::{
    name: "complete-operation",
    context: trace::SpanContext::{
      trace_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte],
      span_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte],
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Server,
    parent_span_id: Some([1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200000001000L),
    status: trace::Ok,
    status_description: Some("Operation completed successfully"),
    attributes: [
      ("operation.type", common::AttributeValue::string("complete")),
      ("service.name", common::AttributeValue::string("test-service"))
    ],
    events: [
      trace::SpanEvent::{
        name: "operation.started",
        timestamp_unix_nanos: 1640995200000000000L,
        attributes: [("event.type", common::AttributeValue::string("start"))]
      }
    ],
    links: []
  }
  
  // 验证完整性
  let mut completeness_score = 1.0
  
  // 检查必需字段
  if complete_span.name.length() == 0 {
    completeness_score = completeness_score - 0.2
  }
  
  if complete_span.end_time_unix_nanos == None {
    completeness_score = completeness_score - 0.2
  }
  
  if complete_span.status_description == None {
    completeness_score = completeness_score - 0.1
  }
  
  if complete_span.attributes.length() == 0 {
    completeness_score = completeness_score - 0.1
  }
  
  @assertion.assert_eq(completeness_score, 1.0)
  
  // 创建不完整的Span
  let incomplete_span = trace::Span::{
    name: "",  // 缺少名称
    context: trace::SpanContext::{
      trace_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte],
      span_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte],
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: None,  // 缺少结束时间
    status: trace::Unset,
    status_description: None,  // 缺少状态描述
    attributes: [],  // 缺少属性
    events: [],  // 缺少事件
    links: []
  }
  
  // 验证不完整性
  completeness_score = 1.0
  
  if incomplete_span.name.length() == 0 {
    completeness_score = completeness_score - 0.2
  }
  
  if incomplete_span.end_time_unix_nanos == None {
    completeness_score = completeness_score - 0.2
  }
  
  if incomplete_span.status_description == None {
    completeness_score = completeness_score - 0.1
  }
  
  if incomplete_span.attributes.length() == 0 {
    completeness_score = completeness_score - 0.1
  }
  
  @assertion.assert_eq(completeness_score, 0.4)
}

test "data_consistency_validation" {
  // 测试数据一致性验证
  
  // 创建一致的数据集
  let consistent_trace_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte]
  let consistent_span_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]
  let consistent_timestamp = 1640995200000000000L
  
  let consistent_span = trace::Span::{
    name: "consistent-operation",
    context: trace::SpanContext::{
      trace_id: consistent_trace_id,
      span_id: consistent_span_id,
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: consistent_timestamp,
    end_time_unix_nanos: Some(consistent_timestamp + 1000000L),
    status: trace::Ok,
    status_description: Some("Success"),
    attributes: [
      ("operation.name", common::AttributeValue::string("consistent-operation")),
      ("trace.id", common::AttributeValue::array_string([consistent_trace_id.map(fn(b) { b.to_string() }).join("")]))
    ],
    events: [],
    links: []
  }
  
  let consistent_log = logs::LogRecord::{
    timestamp_unix_nanos: consistent_timestamp + 500000L,
    observed_timestamp_unix_nanos: None,
    severity_number: logs::Info,
    severity_text: Some("INFO"),
    body: Some("Consistent operation log"),
    attributes: [
      ("operation.name", common::AttributeValue::string("consistent-operation")),
      ("trace.id", common::AttributeValue::array_string([consistent_trace_id.map(fn(b) { b.to_string() }).join("")]))
    ],
    trace_id: Some(consistent_trace_id),
    span_id: Some(consistent_span_id),
    trace_flags: Some(1_byte),
    resource: Some(common::Resource::default("consistent-service")),
    instrumentation_scope: Some(common::InstrumentationScope::{
      name: "consistent-logger",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  let consistent_measurement = metrics::Measurement::{
    value: 100.0,
    attributes: [
      ("operation.name", common::AttributeValue::string("consistent-operation")),
      ("trace.id", common::AttributeValue::array_string([consistent_trace_id.map(fn(b) { b.to_string() }).join("")]))
    ]
  }
  
  // 验证一致性
  let mut consistency_score = 1.0
  
  // 检查trace ID一致性
  let span_trace_id = consistent_span.context.trace_id.map(fn(b) { b.to_string() }).join("")
  let log_trace_id = consistent_log.trace_id?.map(fn(b) { b.to_string() }).join("")
  
  if span_trace_id != log_trace_id {
    consistency_score = consistency_score - 0.3
  }
  
  // 检查操作名称一致性
  let span_op_name = match consistent_span.attributes[0].1 {
    common::StringValue(s) => s
    _ => ""
  }
  
  let log_op_name = match consistent_log.attributes[0].1 {
    common::StringValue(s) => s
    _ => ""
  }
  
  let metric_op_name = match consistent_measurement.attributes[0].1 {
    common::StringValue(s) => s
    _ => ""
  }
  
  if span_op_name != log_op_name or span_op_name != metric_op_name {
    consistency_score = consistency_score - 0.3
  }
  
  // 检查时间顺序一致性
  if not (consistent_span.start_time_unix_nanos <= consistent_log.timestamp_unix_nanos and consistent_log.timestamp_unix_nanos <= consistent_span.end_time_unix_nanos?) {
    consistency_score = consistency_score - 0.2
  }
  
  // 检查资源一致性
  if consistent_log.resource?.service_name != "consistent-service" {
    consistency_score = consistency_score - 0.1
  }
  
  @assertion.assert_eq(consistency_score, 1.0)
  
  // 创建不一致的数据集
  let inconsistent_span = trace::Span::{
    name: "inconsistent-operation",
    context: trace::SpanContext::{
      trace_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte],
      span_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte],
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200000001000L),
    status: trace::Ok,
    status_description: Some("Success"),
    attributes: [
      ("operation.name", common::AttributeValue::string("different-operation"))  // 不一致的操作名
    ],
    events: [],
    links: []
  }
  
  let inconsistent_log = logs::LogRecord::{
    timestamp_unix_nanos: 1640995190000000000L,  // 早于span开始时间
    observed_timestamp_unix_nanos: None,
    severity_number: logs::Info,
    severity_text: Some("INFO"),
    body: Some("Inconsistent operation log"),
    attributes: [
      ("operation.name", common::AttributeValue::string("another-operation"))  // 另一个不一致的操作名
    ],
    trace_id: Some([2_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte]),  // 不同的trace ID
    span_id: Some([1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]),
    trace_flags: Some(1_byte),
    resource: Some(common::Resource::default("different-service")),  // 不同的服务名
    instrumentation_scope: None
  }
  
  // 验证不一致性
  consistency_score = 1.0
  
  // 检查trace ID不一致
  let span_trace_id = inconsistent_span.context.trace_id.map(fn(b) { b.to_string() }).join("")
  let log_trace_id = inconsistent_log.trace_id?.map(fn(b) { b.to_string() }).join("")
  
  if span_trace_id != log_trace_id {
    consistency_score = consistency_score - 0.3
  }
  
  // 检查操作名称不一致
  let span_op_name = match inconsistent_span.attributes[0].1 {
    common::StringValue(s) => s
    _ => ""
  }
  
  let log_op_name = match inconsistent_log.attributes[0].1 {
    common::StringValue(s) => s
    _ => ""
  }
  
  if span_op_name != log_op_name {
    consistency_score = consistency_score - 0.3
  }
  
  // 检查时间顺序不一致
  if not (inconsistent_span.start_time_unix_nanos <= inconsistent_log.timestamp_unix_nanos and inconsistent_log.timestamp_unix_nanos <= inconsistent_span.end_time_unix_nanos?) {
    consistency_score = consistency_score - 0.2
  }
  
  // 检查资源不一致
  if inconsistent_log.resource?.service_name != "test-service" {  // 假设期望的服务名
    consistency_score = consistency_score - 0.1
  }
  
  @assertion.assert_eq(consistency_score, 0.1)
}

test "cross_module_data_quality_correlation" {
  // 测试跨模块数据质量关联
  
  // 创建相关的trace、metric、log数据
  let base_trace_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte]
  let base_span_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]
  let base_timestamp = 1640995200000000000L
  
  let correlated_span = trace::Span::{
    name: "correlated-operation",
    context: trace::SpanContext::{
      trace_id: base_trace_id,
      span_id: base_span_id,
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: base_timestamp,
    end_time_unix_nanos: Some(base_timestamp + 1000000L),
    status: trace::Ok,
    status_description: Some("Success"),
    attributes: [
      ("operation.id", common::AttributeValue::string("op-123")),
      ("user.id", common::AttributeValue::int(456))
    ],
    events: [],
    links: []
  }
  
  let correlated_log = logs::LogRecord::{
    timestamp_unix_nanos: base_timestamp + 500000L,
    observed_timestamp_unix_nanos: None,
    severity_number: logs::Info,
    severity_text: Some("INFO"),
    body: Some("Correlated operation completed"),
    attributes: [
      ("operation.id", common::AttributeValue::string("op-123")),
      ("user.id", common::AttributeValue::int(456))
    ],
    trace_id: Some(base_trace_id),
    span_id: Some(base_span_id),
    trace_flags: Some(1_byte),
    resource: Some(common::Resource::default("correlated-service")),
    instrumentation_scope: Some(common::InstrumentationScope::{
      name: "correlated-logger",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  let correlated_measurement = metrics::Measurement::{
    value: 150.0,
    attributes: [
      ("operation.id", common::AttributeValue::string("op-123")),
      ("user.id", common::AttributeValue::int(456))
    ]
  }
  
  // 验证跨模块关联质量
  let mut correlation_score = 1.0
  
  // 检查trace关联
  if correlated_log.trace_id? != correlated_span.context.trace_id {
    correlation_score = correlation_score - 0.4
  }
  
  if correlated_log.span_id? != correlated_span.context.span_id {
    correlation_score = correlation_score - 0.2
  }
  
  // 检查属性关联
  let span_op_id = match correlated_span.attributes[0].1 {
    common::StringValue(s) => s
    _ => ""
  }
  
  let log_op_id = match correlated_log.attributes[0].1 {
    common::StringValue(s) => s
    _ => ""
  }
  
  let metric_op_id = match correlated_measurement.attributes[0].1 {
    common::StringValue(s) => s
    _ => ""
  }
  
  if span_op_id != log_op_id or span_op_id != metric_op_id {
    correlation_score = correlation_score - 0.2
  }
  
  // 检查时间关联
  if not (correlated_span.start_time_unix_nanos <= correlated_log.timestamp_unix_nanos and correlated_log.timestamp_unix_nanos <= correlated_span.end_time_unix_nanos?) {
    correlation_score = correlation_score - 0.1
  }
  
  // 检查资源关联
  if correlated_log.resource?.service_name != "correlated-service" {
    correlation_score = correlation_score - 0.1
  }
  
  @assertion.assert_eq(correlation_score, 1.0)
  
  // 计算整体数据质量分数
  let span_quality = 0.95  // 假设span质量分数
  let log_quality = 0.90   // 假设log质量分数
  let metric_quality = 0.88 // 假设metric质量分数
  
  let overall_quality = (span_quality + log_quality + metric_quality) / 3.0 * correlation_score
  @assertion.assert_true(overall_quality >= 0.85)
}