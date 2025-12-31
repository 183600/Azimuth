// 遥测API向后兼容性测试
// 测试API变更对现有代码的影响

// API版本信息
struct APIVersion {
  major : Int
  minor : Int
  patch : Int
}

fn APIVersion::new(major : Int, minor : Int, patch : Int) -> APIVersion {
  APIVersion::{ major, minor, patch }
}

fn APIVersion::to_string(self : APIVersion) -> String {
  self.major.to_string() + "." + self.minor.to_string() + "." + self.patch.to_string()
}

fn APIVersion::is_compatible(self : APIVersion, other : APIVersion) -> Bool {
  // 主版本号相同认为兼容，或者新版本的主版本号更大但保持向后兼容
  self.major == other.major or (self.major > other.major and self.minor >= other.minor)
}

// 兼容性测试结果
struct CompatibilityResult {
  is_compatible : Bool
  breaking_changes : Array[String]
  deprecated_features : Array[String]
  new_features : Array[String]
  migration_notes : Array[String]
}

fn CompatibilityResult::new() -> CompatibilityResult {
  CompatibilityResult::{
    is_compatible: true,
    breaking_changes: [],
    deprecated_features: [],
    new_features: [],
    migration_notes: []
  }
}

fn CompatibilityResult::add_breaking_change(self : CompatibilityResult, change : String) -> CompatibilityResult {
  let new_changes = []
  let mut i = 0
  while i < self.breaking_changes.length() {
    new_changes.push(self.breaking_changes[i])
    i = i + 1
  }
  new_changes.push(change)
  CompatibilityResult::{ ..self, breaking_changes: new_changes, is_compatible: false }
}

fn CompatibilityResult::add_deprecated_feature(self : CompatibilityResult, feature : String) -> CompatibilityResult {
  let new_features = []
  let mut i = 0
  while i < self.deprecated_features.length() {
    new_features.push(self.deprecated_features[i])
    i = i + 1
  }
  new_features.push(feature)
  CompatibilityResult::{ ..self, deprecated_features: new_features }
}

// 模拟不同版本的API
// v1.0.0 - 初始版本
struct V1_0_0_Span {
  name : String
  trace_id : Array[Byte]
  span_id : Array[Byte]
  start_time : Int64
  end_time : Int64?
}

// v1.1.0 - 添加status字段
struct V1_1_0_Span {
  name : String
  trace_id : Array[Byte]
  span_id : Array[Byte]
  start_time : Int64
  end_time : Int64?
  status : String  // 新字段
}

// v1.2.0 - 重命名字段并添加新字段
struct V1_2_0_Span {
  name : String
  trace_id : Array[Byte]
  span_id : Array[Byte]
  start_time_unix_nanos : Int64  // 重命名字段
  end_time_unix_nanos : Int64?   // 重命名字段
  status : String
  kind : String  // 新字段
  parent_span_id : Array[Byte]?  // 新字段
}

// v2.0.0 - 破坏性变更：改变字段类型
struct V2_0_0_Span {
  span_name : String  // 重命名字段
  trace_context : String  // 改变类型：从Array[Byte]到String
  span_context : String   // 改变类型：从Array[Byte]到String
  start_timestamp : Int64  // 重命名字段
  end_timestamp : Int64?   // 重命名字段
  span_status : Int        // 改变类型：从String到Int
  span_kind : String
  parent_context : String?  // 改变类型
}

test "trace_api_backward_compatibility" {
  // 测试Trace API向后兼容性
  
  let v1_0_0 = APIVersion::new(1, 0, 0)
  let v1_1_0 = APIVersion::new(1, 1, 0)
  let v1_2_0 = APIVersion::new(1, 2, 0)
  let v2_0_0 = APIVersion::new(2, 0, 0)
  
  let mut result = CompatibilityResult::new()
  
  // 测试v1.0.0到v1.1.0的兼容性（添加新字段）
  @assertion.assert_true(v1_1_0.is_compatible(v1_0_0))
  
  // 模拟v1.0.0的代码使用v1.1.0的API
  let v1_0_span = V1_0_0_Span::{
    name: "test-span",
    trace_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte],
    span_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte],
    start_time: 1640995200000000000L,
    end_time: Some(1640995200000001000L)
  }
  
  // 转换为v1.1.0格式（新字段有默认值）
  let v1_1_span = V1_1_0_Span::{
    name: v1_0_span.name,
    trace_id: v1_0_span.trace_id,
    span_id: v1_0_span.span_id,
    start_time: v1_0_span.start_time,
    end_time: v1_0_span.end_time,
    status: "OK"  // 默认值
  }
  
  @assertion.assert_eq(v1_0_span.name, v1_1_span.name)
  @assertion.assert_eq(v1_0_span.trace_id, v1_1_span.trace_id)
  @assertion.assert_eq(v1_0_span.span_id, v1_1_span.span_id)
  @assertion.assert_eq(v1_1_span.status, "OK")
  
  // 测试v1.1.0到v1.2.0的兼容性（重命名字段和添加新字段）
  @assertion.assert_true(v1_2_0.is_compatible(v1_1_0))
  
  // 转换为v1.2.0格式
  let v1_2_span = V1_2_0_Span::{
    name: v1_1_span.name,
    trace_id: v1_1_span.trace_id,
    span_id: v1_1_span.span_id,
    start_time_unix_nanos: v1_1_span.start_time,  // 重命名字段
    end_time_unix_nanos: v1_1_span.end_time,       // 重命名字段
    status: v1_1_span.status,
    kind: "INTERNAL",  // 新字段，默认值
    parent_span_id: None  // 新字段，默认值
  }
  
  @assertion.assert_eq(v1_1_span.name, v1_2_span.name)
  @assertion.assert_eq(v1_1_span.start_time, v1_2_span.start_time_unix_nanos)
  @assertion.assert_eq(v1_2_span.kind, "INTERNAL")
  @assertion.assert_eq(v1_2_span.parent_span_id, None)
  
  // 测试v1.2.0到v2.0.0的兼容性（破坏性变更）
  @assertion.assert_false(v2_0_0.is_compatible(v1_2_0))
  
  result = result.add_breaking_change("Field type changed: trace_id from Array[Byte] to String")
  result = result.add_breaking_change("Field type changed: span_id from Array[Byte] to String")
  result = result.add_breaking_change("Field type changed: status from String to Int")
  result = result.add_breaking_change("Field renamed: name to span_name")
  
  @assertion.assert_eq(result.is_compatible, false)
  @assertion.assert_eq(result.breaking_changes.length(), 4)
}

test "metrics_api_backward_compatibility" {
  // 测试Metrics API向后兼容性
  
  let v1_0_0 = APIVersion::new(1, 0, 0)
  let v1_1_0 = APIVersion::new(1, 1, 0)
  let v1_2_0 = APIVersion::new(1, 2, 0)
  
  let mut result = CompatibilityResult::new()
  
  // 模拟metrics API的演进
  
  // v1.0.0 - 基础指标
  struct V1_0_0_Metric {
    name : String
    value : Double
    tags : Array[(String, String)]
  }
  
  // v1.1.0 - 添加单位
  struct V1_1_0_Metric {
    name : String
    value : Double
    unit : String  // 新字段
    tags : Array[(String, String)]
  }
  
  // v1.2.0 - 添加描述和类型
  struct V1_2_0_Metric {
    name : String
    value : Double
    unit : String
    description : String?  // 新字段
    metric_type : String   // 新字段
    tags : Array[(String, String)]
  }
  
  // 测试v1.0.0到v1.1.0的兼容性
  @assertion.assert_true(v1_1_0.is_compatible(v1_0_0))
  
  let v1_0_metric = V1_0_0_Metric::{
    name: "request.count",
    value: 100.0,
    tags: [("method", "GET"), ("status", "200")]
  }
  
  // 转换为v1.1.0格式
  let v1_1_metric = V1_1_0_Metric::{
    name: v1_0_metric.name,
    value: v1_0_metric.value,
    unit: "",  // 默认空单位
    tags: v1_0_metric.tags
  }
  
  @assertion.assert_eq(v1_0_metric.name, v1_1_metric.name)
  @assertion.assert_eq(v1_0_metric.value, v1_1_metric.value)
  @assertion.assert_eq(v1_1_metric.unit, "")
  
  // 测试v1.1.0到v1.2.0的兼容性
  @assertion.assert_true(v1_2_0.is_compatible(v1_1_0))
  
  // 转换为v1.2.0格式
  let v1_2_metric = V1_2_0_Metric::{
    name: v1_1_metric.name,
    value: v1_1_metric.value,
    unit: v1_1_metric.unit,
    description: None,  // 默认无描述
    metric_type: "unknown",  // 默认类型
    tags: v1_1_metric.tags
  }
  
  @assertion.assert_eq(v1_1_metric.name, v1_2_metric.name)
  @assertion.assert_eq(v1_2_metric.description, None)
  @assertion.assert_eq(v1_2_metric.metric_type, "unknown")
  
  // 验证向后兼容的迁移路径
  fn migrate_v1_0_to_v1_2(old_metric : V1_0_0_Metric) -> V1_2_0_Metric {
    V1_2_0_Metric::{
      name: old_metric.name,
      value: old_metric.value,
      unit: "",  // 默认单位
      description: None,
      metric_type: "unknown",
      tags: old_metric.tags
    }
  }
  
  let migrated_metric = migrate_v1_0_to_v1_2(v1_0_metric)
  @assertion.assert_eq(migrated_metric.name, "request.count")
  @assertion.assert_eq(migrated_metric.value, 100.0)
  @assertion.assert_eq(migrated_metric.unit, "")
  @assertion.assert_eq(migrated_metric.tags.length(), 2)
}

test "logs_api_backward_compatibility" {
  // 测试Logs API向后兼容性
  
  let v1_0_0 = APIVersion::new(1, 0, 0)
  let v1_1_0 = APIVersion::new(1, 1, 0)
  
  let mut result = CompatibilityResult::new()
  
  // 模拟logs API的演进
  
  // v1.0.0 - 基础日志
  struct V1_0_0_Log {
    timestamp : Int64
    level : String
    message : String
    fields : Array[(String, String)]
  }
  
  // v1.1.0 - 添加trace关联
  struct V1_1_0_Log {
    timestamp : Int64
    level : String
    message : String
    fields : Array[(String, String)]
    trace_id : Array[Byte]?  // 新字段
    span_id : Array[Byte]?   // 新字段
  }
  
  // 测试v1.0.0到v1.1.0的兼容性
  @assertion.assert_true(v1_1_0.is_compatible(v1_0_0))
  
  let v1_0_log = V1_0_0_Log::{
    timestamp: 1640995200000000000L,
    level: "INFO",
    message: "Application started",
    fields: [("service", "api-server"), ("version", "1.0.0")]
  }
  
  // 转换为v1.1.0格式
  let v1_1_log = V1_1_0_Log::{
    timestamp: v1_0_log.timestamp,
    level: v1_0_log.level,
    message: v1_0_log.message,
    fields: v1_0_log.fields,
    trace_id: None,  // 默认无trace关联
    span_id: None    // 默认无span关联
  }
  
  @assertion.assert_eq(v1_0_log.timestamp, v1_1_log.timestamp)
  @assertion.assert_eq(v1_0_log.level, v1_1_log.level)
  @assertion.assert_eq(v1_0_log.message, v1_1_log.message)
  @assertion.assert_eq(v1_1_log.trace_id, None)
  @assertion.assert_eq(v1_1_log.span_id, None)
  
  // 测试带有trace关联的日志
  let v1_1_log_with_trace = V1_1_0_Log::{
    timestamp: 1640995200000000000L,
    level: "ERROR",
    message: "Request failed",
    fields: [("error", "timeout"), ("url", "/api/users")],
    trace_id: Some([1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte]),
    span_id: Some([1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte])
  }
  
  // 验证新功能
  @assertion.assert_true(v1_1_log_with_trace.trace_id?.length() == 16)
  @assertion.assert_true(v1_1_log_with_trace.span_id?.length() == 8)
  
  // 测试向后兼容的降级处理
  fn downgrade_v1_1_to_v1_0(new_log : V1_1_0_Log) -> V1_0_0_Log {
    V1_0_0_Log::{
      timestamp: new_log.timestamp,
      level: new_log.level,
      message: new_log.message,
      fields: new_log.fields  // 丢弃trace_id和span_id
    }
  }
  
  let downgraded_log = downgrade_v1_1_to_v1_0(v1_1_log_with_trace)
  @assertion.assert_eq(downgraded_log.timestamp, v1_1_log_with_trace.timestamp)
  @assertion.assert_eq(downgraded_log.level, v1_1_log_with_trace.level)
  @assertion.assert_eq(downgraded_log.message, v1_1_log_with_trace.message)
  @assertion.assert_eq(downgraded_log.fields.length(), 2)
}

test "attribute_value_compatibility" {
  // 测试AttributeValue的向后兼容性
  
  let v1_0_0 = APIVersion::new(1, 0, 0)
  let v1_1_0 = APIVersion::new(1, 1, 0)
  
  let mut result = CompatibilityResult::new()
  
  // 模拟AttributeValue的演进
  
  // v1.0.0 - 基础类型
  enum V1_0_0_AttributeValue {
    V1_String(String)
    V1_Int(Int64)
    V1_Float(Double)
    V1_Bool(Bool)
  }
  
  // v1.1.0 - 添加数组类型
  enum V1_1_0_AttributeValue {
    V1_String(String)
    V1_Int(Int64)
    V1_Float(Double)
    V1_Bool(Bool)
    V1_ArrayString(Array[String])  // 新类型
    V1_ArrayInt(Array[Int64])     // 新类型
    V1_ArrayFloat(Array[Double])  // 新类型
    V1_ArrayBool(Array[Bool])     // 新类型
  }
  
  // 测试v1.0.0到v1.1.0的兼容性
  @assertion.assert_true(v1_1_0.is_compatible(v1_0_0))
  
  // 转换函数
  fn upgrade_attribute_value(old_value : V1_0_0_AttributeValue) -> V1_1_0_AttributeValue {
    match old_value {
      V1_0_0_AttributeValue::V1_String(s) => V1_1_0_AttributeValue::V1_String(s)
      V1_0_0_AttributeValue::V1_Int(i) => V1_1_0_AttributeValue::V1_Int(i)
      V1_0_0_AttributeValue::V1_Float(f) => V1_1_0_AttributeValue::V1_Float(f)
      V1_0_0_AttributeValue::V1_Bool(b) => V1_1_0_AttributeValue::V1_Bool(b)
    }
  }
  
  // 测试转换
  let old_string = V1_0_0_AttributeValue::V1_String("test")
  let upgraded_string = upgrade_attribute_value(old_string)
  match upgraded_string {
    V1_1_0_AttributeValue::V1_String(s) => @assertion.assert_eq(s, "test")
    _ => @assertion.assert_fail("Expected String value")
  }
  
  let old_int = V1_0_0_AttributeValue::V1_Int(42L)
  let upgraded_int = upgrade_attribute_value(old_int)
  match upgraded_int {
    V1_1_0_AttributeValue::V1_Int(i) => @assertion.assert_eq(i, 42L)
    _ => @assertion.assert_fail("Expected Int value")
  }
  
  // 测试降级函数
  fn downgrade_attribute_value(new_value : V1_1_0_AttributeValue) -> V1_0_0_AttributeValue? {
    match new_value {
      V1_1_0_AttributeValue::V1_String(s) => Some(V1_0_0_AttributeValue::V1_String(s))
      V1_1_0_AttributeValue::V1_Int(i) => Some(V1_0_0_AttributeValue::V1_Int(i))
      V1_1_0_AttributeValue::V1_Float(f) => Some(V1_0_0_AttributeValue::V1_Float(f))
      V1_1_0_AttributeValue::V1_Bool(b) => Some(V1_0_0_AttributeValue::V1_Bool(b))
      _ => None  // 数组类型无法降级
    }
  }
  
  // 测试可降级的值
  let new_string = V1_1_0_AttributeValue::V1_String("test")
  let downgraded_string = downgrade_attribute_value(new_string)
  @assertion.assert_true(downgraded_string != None)
  
  // 测试不可降级的值
  let new_array = V1_1_0_AttributeValue::V1_ArrayString(["a", "b", "c"])
  let downgraded_array = downgrade_attribute_value(new_array)
  @assertion.assert_true(downgraded_array == None)
  
  // 记录兼容性信息
  result = result.add_deprecated_feature("Array attribute types not supported in v1.0.0")
  
  @assertion.assert_eq(result.deprecated_features.length(), 1)
}

test "api_method_compatibility" {
  // 测试API方法的向后兼容性
  
  let v1_0_0 = APIVersion::new(1, 0, 0)
  let v1_1_0 = APIVersion::new(1, 1, 0)
  let v1_2_0 = APIVersion::new(1, 2, 0)
  
  let mut result = CompatibilityResult::new()
  
  // 模拟Tracer接口的演进
  
  // v1.0.0 - 基础接口
  trait V1_0_0_Tracer {
    start_span(name : String) -> trace::Span
  }
  
  // v1.1.0 - 添加可选参数
  trait V1_1_0_Tracer {
    start_span(name : String, attributes? : Array[(String, common::AttributeValue)]) -> trace::Span
  }
  
  // v1.2_0 - 添加更多参数和返回上下文
  trait V1_2_0_Tracer {
    start_span(
      name : String, 
      attributes? : Array[(String, common::AttributeValue)], 
      start_time? : Int64
    ) -> (context::Context, trace::Span)
  }
  
  // 测试方法签名兼容性
  
  // v1.0.0的代码调用v1.1.0的API
  fn v1_0_0_caller_v1_1_0(tracer : V1_1_0_Tracer) -> trace::Span {
    // 旧代码只传递必需参数
    tracer.start_span("test-span")
  }
  
  // v1.0.0的代码调用v1.2.0的API（需要适配器）
  fn adapt_v1_2_0_for_v1_0_0(tracer : V1_2_0_Tracer) -> trace::Span {
    let (ctx, span) = tracer.start_span("test-span", None, None)
    span  // 旧代码只关心span，忽略上下文
  }
  
  // 测试v1.1.0的代码调用v1.2.0的API
  fn v1_1_0_caller_v1_2_0(tracer : V1_2_0_Tracer) -> trace::Span {
    let attributes = [("test", common::AttributeValue::string("value"))]
    let (ctx, span) = tracer.start_span("test-span", Some(attributes), None)
    span
  }
  
  // 验证兼容性
  @assertion.assert_true(v1_1_0.is_compatible(v1_0_0))
  @assertion.assert_true(v1_2_0.is_compatible(v1_1_0))
  
  // 测试破坏性变更：如果v2.0.0改变了方法签名
  trait V2_0_0_Tracer {
    create_span(name : String, options : SpanOptions) -> trace::Span  // 破坏性变更
  }
  
  struct SpanOptions {
    attributes : Array[(String, common::AttributeValue)]
    start_time : Int64?
    kind : trace::SpanKind
  }
  
  let v2_0_0 = APIVersion::new(2, 0, 0)
  @assertion.assert_false(v2_0_0.is_compatible(v1_2_0))
  
  result = result.add_breaking_change("Method signature changed: start_span to create_span")
  result = result.add_breaking_change("Parameter consolidation: multiple parameters to options struct")
  
  @assertion.assert_eq(result.is_compatible, false)
  @assertion.assert_eq(result.breaking_changes.length(), 2)
}

test "configuration_compatibility" {
  // 测试配置的向后兼容性
  
  let v1_0_0 = APIVersion::new(1, 0, 0)
  let v1_1_0 = APIVersion::new(1, 1, 0)
  
  let mut result = CompatibilityResult::new()
  
  // 模拟配置的演进
  
  // v1.0.0 - 基础配置
  struct V1_0_0_Config {
    service_name : String
    enabled : Bool
    endpoint : String
  }
  
  // v1.1.0 - 添加新配置项
  struct V1_1_0_Config {
    service_name : String
    enabled : Bool
    endpoint : String
    batch_size : Int        // 新字段
    timeout_seconds : Int   // 新字段
    headers : Array[(String, String)]  // 新字段
  }
  
  // 测试配置迁移
  fn migrate_config_v1_0_to_v1_1(old_config : V1_0_0_Config) -> V1_1_0_Config {
    V1_1_0_Config::{
      service_name: old_config.service_name,
      enabled: old_config.enabled,
      endpoint: old_config.endpoint,
      batch_size: 100,        // 默认值
      timeout_seconds: 30,    // 默认值
      headers: []             // 默认值
    }
  }
  
  let old_config = V1_0_0_Config::{
    service_name: "test-service",
    enabled: true,
    endpoint: "http://localhost:4317"
  }
  
  let new_config = migrate_config_v1_0_to_v1_1(old_config)
  @assertion.assert_eq(new_config.service_name, "test-service")
  @assertion.assert_eq(new_config.enabled, true)
  @assertion.assert_eq(new_config.endpoint, "http://localhost:4317")
  @assertion.assert_eq(new_config.batch_size, 100)
  @assertion.assert_eq(new_config.timeout_seconds, 30)
  @assertion.assert_eq(new_config.headers.length(), 0)
  
  // 测试配置降级（移除新字段）
  fn downgrade_config_v1_1_to_v1_0(new_config : V1_1_0_Config) -> V1_0_0_Config {
    V1_0_0_Config::{
      service_name: new_config.service_name,
      enabled: new_config.enabled,
      endpoint: new_config.endpoint
      // 丢失新配置项
    }
  }
  
  let downgraded_config = downgrade_config_v1_1_to_v1_0(new_config)
  @assertion.assert_eq(downgraded_config.service_name, "test-service")
  @assertion.assert_eq(downgraded_config.enabled, true)
  @assertion.assert_eq(downgraded_config.endpoint, "http://localhost:4317")
  
  // 验证配置兼容性
  @assertion.assert_true(v1_1_0.is_compatible(v1_0_0))
  
  // 测试配置验证
  fn validate_config_compatibility(old_config : V1_0_0_Config, new_config : V1_1_0_Config) -> Bool {
    // 检查核心字段是否保持兼容
    old_config.service_name == new_config.service_name and
    old_config.enabled == new_config.enabled and
    old_config.endpoint == new_config.endpoint
  }
  
  @assertion.assert_true(validate_config_compatibility(old_config, new_config))
}

test "serialization_format_compatibility" {
  // 测试序列化格式的向后兼容性
  
  let v1_0_0 = APIVersion::new(1, 0, 0)
  let v1_1_0 = APIVersion::new(1, 1, 0)
  
  let mut result = CompatibilityResult::new()
  
  // 模拟序列化格式的演进
  
  // v1.0.0 - JSON格式
  fn serialize_v1_0_0(span : trace::Span) -> String {
    "{"
      + "\"name\":\"" + span.name + "\","
      + "\"trace_id\":\"" + span.context.trace_id.map(fn(b) { b.to_string() }).join("") + "\","
      + "\"span_id\":\"" + span.context.span_id.map(fn(b) { b.to_string() }).join("") + "\","
      + "\"start_time\":" + span.start_time_unix_nanos.to_string() + ","
      + "\"end_time\":" + span.end_time_unix_nanos?.to_string()
      + "}"
  }
  
  // v1.1.0 - 添加新字段但保持格式兼容
  fn serialize_v1_1_0(span : trace::Span) -> String {
    "{"
      + "\"name\":\"" + span.name + "\","
      + "\"trace_id\":\"" + span.context.trace_id.map(fn(b) { b.to_string() }).join("") + "\","
      + "\"span_id\":\"" + span.context.span_id.map(fn(b) { b.to_string() }).join("") + "\","
      + "\"start_time\":" + span.start_time_unix_nanos.to_string() + ","
      + "\"end_time\":" + span.end_time_unix_nanos?.to_string() + ","
      + "\"status\":\"" + (match span.status { trace::Ok => "OK", trace::Error => "ERROR", _ => "UNSET" }) + "\","
      + "\"kind\":\"" + (match span.kind { trace::Server => "SERVER", trace::Client => "CLIENT", _ => "INTERNAL" }) + "\""
      + "}"
  }
  
  // 测试序列化兼容性
  let test_span = trace::Span::{
    name: "test-span",
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
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  let v1_0_0_json = serialize_v1_0_0(test_span)
  let v1_1_0_json = serialize_v1_1_0(test_span)
  
  // 验证v1.0.0解析器能处理v1.1.0的输出（忽略新字段）
  @assertion.assert_true(v1_0_0_json.includes("\"name\":\"test-span\""))
  @assertion.assert_true(v1_0_0_json.includes("\"trace_id\":\"0102030405060708090a0b0c0d0e0f10\""))
  
  @assertion.assert_true(v1_1_0_json.includes("\"name\":\"test-span\""))
  @assertion.assert_true(v1_1_0_json.includes("\"status\":\"OK\""))
  @assertion.assert_true(v1_1_0_json.includes("\"kind\":\"SERVER\""))
  
  // 验证核心字段在两个版本中都存在
  @assertion.assert_true(v1_0_0_json.includes("\"name\""))
  @assertion.assert_true(v1_0_0_json.includes("\"trace_id\""))
  @assertion.assert_true(v1_0_0_json.includes("\"span_id\""))
  @assertion.assert_true(v1_0_0_json.includes("\"start_time\""))
  @assertion.assert_true(v1_0_0_json.includes("\"end_time\""))
  
  @assertion.assert_true(v1_1_0_json.includes("\"name\""))
  @assertion.assert_true(v1_1_0_json.includes("\"trace_id\""))
  @assertion.assert_true(v1_1_0_json.includes("\"span_id\""))
  @assertion.assert_true(v1_1_0_json.includes("\"start_time\""))
  @assertion.assert_true(v1_1_0_json.includes("\"end_time\""))
  
  // 测试解析兼容性
  fn can_parse_v1_0_0_with_v1_1_0_parser(json : String) -> Bool {
    // v1.1.0解析器应该能处理v1.0.0格式（缺少新字段）
    json.includes("\"name\"") and json.includes("\"trace_id\"") and json.includes("\"span_id\"")
  }
  
  fn can_parse_v1_1_0_with_v1_0_0_parser(json : String) -> Bool {
    // v1.0.0解析器应该能处理v1.1.0格式（忽略新字段）
    json.includes("\"name\"") and json.includes("\"trace_id\"") and json.includes("\"span_id\"")
  }
  
  @assertion.assert_true(can_parse_v1_0_0_with_v1_1_0_parser(v1_0_0_json))
  @assertion.assert_true(can_parse_v1_1_0_with_v1_0_0_parser(v1_1_0_json))
  
  // 验证格式兼容性
  @assertion.assert_true(v1_1_0.is_compatible(v1_0_0))
  
  result = result.add_new_feature("Added status and kind fields to JSON serialization")
  
  @assertion.assert_eq(result.new_features.length(), 1)
}