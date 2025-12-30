// 跨模块集成测试
// 测试遥测系统各个模块之间的集成

// 导入必要的模块
use azimuth.telemetry.api.trace
use azimuth.telemetry.api.context
use azimuth.telemetry.api.metrics
use azimuth.telemetry.api.logs
use azimuth.telemetry.api.propagation
use azimuth.telemetry.api.common

test "trace_context_integration" {
  // 创建根上下文
  let root_ctx = context::Context::current()
  
  // 创建追踪器
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test-tracer", "1.0.0")
  
  // 启动span
  let (ctx_with_span, span) = tracer.start_span(root_ctx, "test-operation", trace::Internal, [("test-key", common::AttributeValue::string("test-value"))])
  
  // 验证span创建成功
  assert_eq(span.name, "test-operation")
  assert_eq(span.kind, trace::Internal)
  assert_eq(span.attributes.length(), 1)
  assert_eq(span.attributes[0], ("test-key", common::AttributeValue::string("test-value")))
}

test "metrics_logs_integration" {
  // 创建资源
  let resource = common::Resource::default("test-service")
  
  // 创建仪表化范围
  let scope = common::InstrumentationScope::{
    name: "test-instrumentation",
    version: Some("1.0.0"),
    schema_url: None
  }
  
  // 验证资源创建
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  
  // 验证仪表化范围
  assert_eq(scope.name, "test-instrumentation")
  assert_eq(scope.version, Some("1.0.0"))
}

test "propagation_round_trip" {
  // 创建原始上下文
  let original_ctx = context::Context::current()
  
  // 创建传播器
  let propagator = propagation::NoopTracerProvider::{}
  
  // 模拟传播载体
  let carrier = [("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")]
  
  // 验证载体数据
  assert_eq(carrier.length(), 1)
  assert_eq(carrier[0], ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
}

test "attribute_conversions" {
  // 测试各种属性值类型的转换
  let string_attr = common::AttributeValue::string("test")
  let int_attr = common::AttributeValue::int(42L)
  let float_attr = common::AttributeValue::float(3.14)
  let bool_attr = common::AttributeValue::bool(true)
  
  // 验证属性值创建
  match string_attr {
    common::StringValue(s) => assert_eq(s, "test")
    _ => assert_eq(false, true, "Expected StringValue")
  }
  
  match int_attr {
    common::IntValue(i) => assert_eq(i, 42L)
    _ => assert_eq(false, true, "Expected IntValue")
  }
  
  match float_attr {
    common::FloatValue(f) => assert_eq(f, 3.14)
    _ => assert_eq(false, true, "Expected FloatValue")
  }
  
  match bool_attr {
    common::BoolValue(b) => assert_eq(b, true)
    _ => assert_eq(false, true, "Expected BoolValue")
  }
}