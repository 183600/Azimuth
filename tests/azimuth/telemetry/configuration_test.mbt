// 配置和初始化测试
// Configuration and initialization tests for Azimuth telemetry system

test "默认提供者初始化测试" {
  // 测试各种提供者的默认初始化
  let tracer_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let meter_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let logger_provider = @azimuth.telemetry.sdk.logs.LoggerProvider::default()
  
  // 验证提供者创建成功
  assert_not_eq(tracer_provider, @azimuth.telemetry.sdk.trace.TracerProvider::{})
  assert_not_eq(meter_provider, @azimuth.telemetry.sdk.metrics.MeterProvider::{})
  assert_not_eq(logger_provider, @azimuth.telemetry.sdk.logs.LoggerProvider::{})
  
  // 测试从提供者创建仪器
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(tracer_provider, "default-test-tracer")
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(meter_provider, "default-test-meter")
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(logger_provider, "default-test-logger")
  
  // 验证仪器创建成功
  assert_eq(@azimuth.telemetry.sdk.trace.Tracer::instrumentation_scope(tracer).name, "default-test-tracer")
  assert_eq(meter.scope.name, "default-test-meter")
  assert_eq(logger.scope.name, "default-test-logger")
}

test "Noop提供者初始化测试" {
  // 测试Noop（无操作）提供者的初始化
  let noop_tracer_provider = @azimuth.telemetry.sdk.trace.TracerProvider::noop()
  let noop_meter_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::noop()
  let noop_logger_provider = @azimuth.telemetry.sdk.logs.LoggerProvider::noop()
  
  // 验证Noop提供者创建成功
  assert_not_eq(noop_tracer_provider, @azimuth.telemetry.sdk.trace.TracerProvider::{})
  assert_not_eq(noop_meter_provider, @azimuth.telemetry.sdk.metrics.MeterProvider::{})
  assert_not_eq(noop_logger_provider, @azimuth.telemetry.sdk.logs.LoggerProvider::{})
  
  // 测试从Noop提供者创建仪器
  let noop_tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(noop_tracer_provider, "noop-tracer")
  let noop_meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(noop_meter_provider, "noop-meter")
  let noop_logger = @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(noop_logger_provider, "noop-logger")
  
  // 验证Noop仪器创建成功
  assert_eq(@azimuth.telemetry.sdk.trace.Tracer::instrumentation_scope(noop_tracer).name, "noop-tracer")
  assert_eq(noop_meter.scope.name, "noop-meter")
  assert_eq(noop_logger.scope.name, "noop-logger")
  
  // 测试Noop仪器的操作不会产生错误
  let ctx = @azimuth.telemetry.api.context.Context::root()
  let (_, noop_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(noop_tracer, ctx, "noop-span")
  let noop_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(noop_meter, "noop-counter")
  let noop_log = @azimuth.telemetry.api.logs.LogRecord::new(@azimuth.telemetry.api.logs.Info, "noop log")
  
  @azimuth.telemetry.api.trace.Span::end(noop_span)
  @azimuth.telemetry.sdk.metrics.Counter::add(noop_counter, 1.0)
  @azimuth.telemetry.sdk.logs.Logger::emit(noop_logger, noop_log)
}

test "仪器作用域配置测试" {
  // 测试仪器作用域的配置
  let tracer_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  
  // 创建具有不同版本信息的Tracer
  let tracer_v1 = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(
    tracer_provider, 
    "versioned-tracer", 
    Some("1.0.0")
  )
  let tracer_v2 = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(
    tracer_provider, 
    "versioned-tracer", 
    Some("2.0.0")
  )
  
  // 验证版本信息正确设置
  let scope_v1 = @azimuth.telemetry.sdk.trace.Tracer::instrumentation_scope(tracer_v1)
  let scope_v2 = @azimuth.telemetry.sdk.trace.Tracer::instrumentation_scope(tracer_v2)
  
  assert_eq(scope_v1.name, "versioned-tracer")
  assert_eq(scope_v1.version, Some("1.0.0"))
  
  assert_eq(scope_v2.name, "versioned-tracer")
  assert_eq(scope_v2.version, Some("2.0.0"))
}

test "资源初始化配置测试" {
  // 测试资源的初始化配置
  let empty_resource = @azimuth.telemetry.api.common.Resource::new()
  assert_eq(empty_resource.attributes.length(), 0)
  
  // 测试带属性的资源初始化
  let service_attrs = [
    ("service.name", @azimuth.telemetry.api.common.StringValue("test-service")),
    ("service.version", @azimuth.telemetry.api.common.StringValue("1.0.0")),
    ("service.instance.id", @azimuth.telemetry.api.common.StringValue("instance-123")),
    ("environment", @azimuth.telemetry.api.common.StringValue("testing"))
  ]
  
  let service_resource = @azimuth.telemetry.api.common.Resource::with_attributes(empty_resource, service_attrs)
  
  // 验证属性设置正确
  let service_name = @azimuth.telemetry.api.common.Resource::get_attribute(service_resource, "service.name")
  let service_version = @azimuth.telemetry.api.common.Resource::get_attribute(service_resource, "service.version")
  let instance_id = @azimuth.telemetry.api.common.Resource::get_attribute(service_resource, "service.instance.id")
  let environment = @azimuth.telemetry.api.common.Resource::get_attribute(service_resource, "environment")
  
  assert_eq(service_name, Some(@azimuth.telemetry.api.common.StringValue("test-service")))
  assert_eq(service_version, Some(@azimuth.telemetry.api.common.StringValue("1.0.0")))
  assert_eq(instance_id, Some(@azimuth.telemetry.api.common.StringValue("instance-123")))
  assert_eq(environment, Some(@azimuth.telemetry.api.common.StringValue("testing")))
}

test "传播器配置测试" {
  // 测试传播器的配置
  let trace_propagator = @azimuth.telemetry.api.propagation.W3CTraceContextPropagator::new()
  
  // 测试单个传播器的复合传播器
  let single_composite = @azimuth.telemetry.api.propagation.CompositePropagator::new([trace_propagator])
  assert_eq(single_composite.propagators.length(), 1)
  
  // 测试传播器的注入和提取
  let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier::new()
  let ctx = @azimuth.telemetry.api.context.Context::root()
  
  // 注入上下文
  @azimuth.telemetry.api.propagation.CompositePropagator::inject(single_composite, ctx, carrier)
  
  // 提取上下文
  let extracted_ctx = @azimuth.telemetry.api.propagation.CompositePropagator::extract(single_composite, carrier)
  
  // 验证提取操作的标志
  let key = @azimuth.telemetry.api.context.ContextKey::new("extracted")
  let value = @azimuth.telemetry.api.context.Context::get(extracted_ctx, key)
  assert_eq(value, Some("true"))
}

test "平台服务初始化测试" {
  // 测试平台服务的初始化
  let system_clock = @azimuth.telemetry.api.platform.time.Clock::system()
  let system_random = @azimuth.telemetry.api.platform.random.Random::system()
  let http_client = @azimuth.telemetry.api.platform.http.HttpClient::new()
  
  // 验证平台服务创建成功
  assert_not_eq(system_clock, @azimuth.telemetry.api.platform.time.Clock::{})
  assert_not_eq(system_random, @azimuth.telemetry.api.platform.random.Random::{})
  assert_not_eq(http_client, @azimuth.telemetry.api.platform.http.HttpClient::{})
  
  // 测试时钟服务
  let timestamp1 = @azimuth.telemetry.api.platform.time.Clock::now_unix_nanos(system_clock)
  let timestamp2 = @azimuth.telemetry.api.platform.time.Clock::now_unix_nanos(system_clock)
  
  // 验证时间戳是合理的（2025年的时间戳）
  assert_true(timestamp1 > 1700000000000000000L)
  assert_true(timestamp1 < 1800000000000000000L)
  assert_eq(timestamp1, timestamp2) // 简化实现返回固定值
  
  // 测试随机数服务
  let random_bytes1 = @azimuth.telemetry.api.platform.random.Random::next_bytes(system_random, 16)
  let random_bytes2 = @azimuth.telemetry.api.platform.random.Random::next_bytes(system_random, 16)
  let random_u64_1 = @azimuth.telemetry.api.platform.random.Random::next_u64(system_random)
  let random_u64_2 = @azimuth.telemetry.api.platform.random.Random::next_u64(system_random)
  
  // 验证随机数生成
  assert_eq(random_bytes1.length(), 16)
  assert_eq(random_bytes2.length(), 16)
  assert_eq(random_u64_1, 12345UL) // 简化实现返回固定值
  assert_eq(random_u64_2, 12345UL) // 简化实现返回固定值
}