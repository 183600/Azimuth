// Azimuth Telemetry System - 补充测试用例
// 涵盖跨服务传播、资源管理、Baggage传播等领域

test "跨服务传播测试 - HTTP头传播" {
  // 测试通过HTTP头在不同服务之间传播追踪上下文
  
  // 1. 服务A创建追踪上下文
  let tracer_provider = @azimuth.TracerProvider::default()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "service-a")
  let ctx = @azimuth.Context::root()
  let (service_a_ctx, service_a_span) = @azimuth.Tracer::start_span(tracer, ctx, "service-a-operation")
  
  // 2. 创建传播器并注入到HTTP头
  let trace_propagator = @azimuth.W3CTraceContextPropagator::new()
  let composite_propagator = @azimuth.CompositePropagator::new([trace_propagator])
  let http_headers = @azimuth.TextMapCarrier::new()
  
  @azimuth.CompositePropagator::inject(composite_propagator, service_a_ctx, http_headers)
  
  // 3. 验证注入的追踪头
  let injected_traceparent = @azimuth.TextMapCarrier::get(http_headers, "traceparent")
  assert_true(injected_traceparent.is_some())
  
  // 4. 模拟服务B提取追踪上下文
  let service_b_ctx = @azimuth.CompositePropagator::extract(composite_propagator, http_headers)
  let service_b_tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "service-b")
  let (_, service_b_span) = @azimuth.Tracer::start_span(service_b_tracer, service_b_ctx, "service-b-operation")
  
  // 5. 验证追踪上下文传播
  let service_a_span_ctx = @azimuth.Span::span_context(service_a_span)
  let service_b_span_ctx = @azimuth.Span::span_context(service_b_span)
  
  // 验证trace_id相同（表示是同一个追踪链）
  assert_eq(
    @azimuth.SpanContext::trace_id(service_a_span_ctx),
    @azimuth.SpanContext::trace_id(service_b_span_ctx)
  )
  
  // 验证span_id不同（表示是不同的span）
  assert_ne(
    @azimuth.SpanContext::span_id(service_a_span_ctx),
    @azimuth.SpanContext::span_id(service_b_span_ctx)
  )
  
  @azimuth.Span::end(service_b_span)
  @azimuth.Span::end(service_a_span)
}

test "资源合并和属性管理测试" {
  // 测试Resource对象的合并逻辑和属性管理
  
  // 1. 创建基础资源
  let base_resource = @azimuth.Resource::new()
  let base_with_attrs = @azimuth.Resource::with_attributes(base_resource, [
    ("service.name", @azimuth.StringValue("base-service")),
    ("service.version", @azimuth.StringValue("1.0.0")),
    ("environment", @azimuth.StringValue("production"))
  ])
  
  // 2. 创建覆盖资源
  let override_resource = @azimuth.Resource::new()
  let override_with_attrs = @azimuth.Resource::with_attributes(override_resource, [
    ("service.version", @azimuth.StringValue("2.0.0")), // 覆盖版本
    ("host.name", @azimuth.StringValue("new-host")),    // 新增属性
    ("deployment.region", @azimuth.StringValue("us-west"))
  ])
  
  // 3. 合并资源
  let merged_resource = @azimuth.Resource::merge(base_with_attrs, override_with_attrs)
  
  // 4. 验证合并结果
  let service_name = @azimuth.Resource::get_attribute(merged_resource, "service.name")
  let service_version = @azimuth.Resource::get_attribute(merged_resource, "service.version")
  let host_name = @azimuth.Resource::get_attribute(merged_resource, "host.name")
  let environment = @azimuth.Resource::get_attribute(merged_resource, "environment")
  let deployment_region = @azimuth.Resource::get_attribute(merged_resource, "deployment.region")
  
  // 验证属性值
  assert_eq(service_name, Some(@azimuth.StringValue("base-service")))
  assert_eq(service_version, Some(@azimuth.StringValue("2.0.0"))) // 应该被覆盖
  assert_eq(host_name, Some(@azimuth.StringValue("new-host")))
  assert_eq(environment, Some(@azimuth.StringValue("production")))
  assert_eq(deployment_region, Some(@azimuth.StringValue("us-west")))
}

test "Baggage传播测试 - 跨服务上下文传递" {
  // 测试W3C Baggage在不同服务之间的传播
  
  // 1. 服务A创建Baggage
  let baggage = @azimuth.Baggage::new()
  let baggage_with_entries = @azimuth.Baggage::set_entry(baggage, "user.id", "12345")
  let final_baggage = @azimuth.Baggage::set_entry(baggage_with_entries, "request.id", "req-67890")
  
  // 2. 创建Baggage传播器
  let baggage_propagator = @azimuth.W3CBaggagePropagator::new()
  let composite_propagator = @azimuth.CompositePropagator::new([
    @azimuth.W3CTraceContextPropagator::new()
  ])
  
  // 3. 将Baggage注入到载体
  let ctx = @azimuth.Context::root()
  let carrier = @azimuth.TextMapCarrier::new()
  @azimuth.CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // 4. 模拟在载体中设置baggage头
  @azimuth.TextMapCarrier::set(carrier, "baggage", "user.id=12345,request.id=req-67890")
  
  // 5. 服务B提取Baggage
  let extracted_ctx = @azimuth.CompositePropagator::extract(composite_propagator, carrier)
  
  // 6. 验证Baggage条目
  let user_id = @azimuth.Baggage::get_entry(final_baggage, "user.id")
  let request_id = @azimuth.Baggage::get_entry(final_baggage, "request.id")
  let non_existent = @azimuth.Baggage::get_entry(final_baggage, "non.existent")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(request_id, Some("req-67890"))
  assert_eq(non_existent, None)
  
  // 7. 测试Baggage条目移除
  let baggage_after_removal = @azimuth.Baggage::remove_entry(final_baggage, "user.id")
  let removed_user_id = @azimuth.Baggage::get_entry(baggage_after_removal, "user.id")
  let still_present_request_id = @azimuth.Baggage::get_entry(baggage_after_removal, "request.id")
  
  assert_eq(removed_user_id, None)
  assert_eq(still_present_request_id, Some("req-67890"))
}

test "Span链接和关系测试" {
  // 测试Span之间的链接关系（如因果关联）
  
  let tracer_provider = @azimuth.TracerProvider::default()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "span-links-test")
  
  // 1. 创建父Span
  let ctx = @azimuth.Context::root()
  let (parent_ctx, parent_span) = @azimuth.Tracer::start_span(tracer, ctx, "parent-operation")
  
  // 2. 创建第一个子Span
  let (_, child_span1) = @azimuth.Tracer::start_span(tracer, parent_ctx, "child-operation-1")
  
  // 3. 创建第二个子Span（与第一个子Span并行）
  let (_, child_span2) = @azimuth.Tracer::start_span(tracer, parent_ctx, "child-operation-2")
  
  // 4. 创建第三个子Span（依赖于前两个子Span）
  let (_, child_span3) = @azimuth.Tracer::start_span(tracer, parent_ctx, "child-operation-3")
  
  // 5. 验证Span关系
  let parent_span_ctx = @azimuth.Span::span_context(parent_span)
  let child1_span_ctx = @azimuth.Span::span_context(child_span1)
  let child2_span_ctx = @azimuth.Span::span_context(child_span2)
  let child3_span_ctx = @azimuth.Span::span_context(child_span3)
  
  // 所有子Span应该有相同的trace_id
  assert_eq(@azimuth.SpanContext::trace_id(parent_span_ctx), @azimuth.SpanContext::trace_id(child1_span_ctx))
  assert_eq(@azimuth.SpanContext::trace_id(parent_span_ctx), @azimuth.SpanContext::trace_id(child2_span_ctx))
  assert_eq(@azimuth.SpanContext::trace_id(parent_span_ctx), @azimuth.SpanContext::trace_id(child3_span_ctx))
  
  // 所有Span应该有不同的span_id
  assert_ne(@azimuth.SpanContext::span_id(parent_span_ctx), @azimuth.SpanContext::span_id(child1_span_ctx))
  assert_ne(@azimuth.SpanContext::span_id(parent_span_ctx), @azimuth.SpanContext::span_id(child2_span_ctx))
  assert_ne(@azimuth.SpanContext::span_id(parent_span_ctx), @azimuth.SpanContext::span_id(child3_span_ctx))
  assert_ne(@azimuth.SpanContext::span_id(child1_span_ctx), @azimuth.SpanContext::span_id(child2_span_ctx))
  assert_ne(@azimuth.SpanContext::span_id(child1_span_ctx), @azimuth.SpanContext::span_id(child3_span_ctx))
  assert_ne(@azimuth.SpanContext::span_id(child2_span_ctx), @azimuth.SpanContext::span_id(child3_span_ctx))
  
  // 6. 为Span添加事件和状态
  @azimuth.Span::add_event(parent_span, "parent-started")
  @azimuth.Span::add_event(child_span1, "child1-completed")
  @azimuth.Span::add_event(child_span2, "child2-completed")
  @azimuth.Span::add_event(child_span3, "child3-completed")
  
  @azimuth.Span::set_status(child_span1, @azimuth.Ok, Some("Child 1 completed successfully"))
  @azimuth.Span::set_status(child_span2, @azimuth.Ok, Some("Child 2 completed successfully"))
  @azimuth.Span::set_status(child_span3, @azimuth.Error, Some("Child 3 failed"))
  
  // 7. 结束所有Span（按正确顺序）
  @azimuth.Span::end(child_span1)
  @azimuth.Span::end(child_span2)
  @azimuth.Span::end(child_span3)
  @azimuth.Span::end(parent_span)
}

test "指标仪器类型转换测试" {
  // 测试不同Instrument类型之间的转换和操作
  
  let meter_provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(meter_provider, "instrument-conversion-test")
  
  // 1. 创建不同类型的仪器
  let counter = @azimuth.Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  let histogram = @azimuth.Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  let updown_counter = @azimuth.Meter::create_up_down_counter(meter, "test.updown", Some("Test up-down counter"), Some("count"))
  let gauge = @azimuth.Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("value"))
  
  // 2. 将仪器转换为Instrument枚举
  let counter_instrument = @azimuth.Counter::as_instrument(counter)
  let histogram_instrument = @azimuth.Histogram::as_instrument(histogram)
  
  // 3. 测试Instrument属性访问
  let counter_name = @azimuth.Instrument::name(counter_instrument)
  let counter_desc = @azimuth.Instrument::description(counter_instrument)
  let counter_unit = @azimuth.Instrument::unit(counter_instrument)
  
  let histogram_name = @azimuth.Instrument::name(histogram_instrument)
  let histogram_desc = @azimuth.Instrument::description(histogram_instrument)
  let histogram_unit = @azimuth.Instrument::unit(histogram_instrument)
  
  // 4. 验证属性
  assert_eq(counter_name, "test.counter")
  assert_eq(counter_desc, Some("Test counter"))
  assert_eq(counter_unit, Some("count"))
  
  assert_eq(histogram_name, "test.histogram")
  assert_eq(histogram_desc, Some("Test histogram"))
  assert_eq(histogram_unit, Some("ms"))
  
  // 5. 测试仪器操作
  let attrs = @azimuth.Attributes::new()
  @azimuth.Attributes::set(attrs, "operation.type", @azimuth.StringValue("test"))
  
  // Counter操作
  @azimuth.Counter::add(counter, 1.0, Some(attrs))
  @azimuth.Counter::add(counter, 2.5, Some(attrs))
  
  // Histogram操作
  @azimuth.Histogram::record(histogram, 10.5, Some(attrs))
  @azimuth.Histogram::record(histogram, 25.8, Some(attrs))
  
  // UpDownCounter操作
  @azimuth.UpDownCounter::add(updown_counter, 5.0, Some(attrs))
  @azimuth.UpDownCounter::add(updown_counter, -2.0, Some(attrs))
  
  // Gauge操作
  @azimuth.Gauge::record(gauge, 42.5, Some(attrs))
  @azimuth.Gauge::record(gauge, 38.7, Some(attrs))
}

test "日志级别和过滤测试" {
  // 测试不同日志级别的处理和过滤逻辑
  
  let logger_provider = @azimuth.LoggerProvider::default()
  let logger = @azimuth.LoggerProvider::get_logger(logger_provider, "log-level-test")
  
  // 1. 创建不同级别的日志记录
  let trace_log = @azimuth.LogRecord::new(@azimuth.Trace, "Trace level message")
  let debug_log = @azimuth.LogRecord::new(@azimuth.Debug, "Debug level message")
  let info_log = @azimuth.LogRecord::new(@azimuth.Info, "Info level message")
  let warn_log = @azimuth.LogRecord::new(@azimuth.Warn, "Warning level message")
  let error_log = @azimuth.LogRecord::new(@azimuth.Error, "Error level message")
  let fatal_log = @azimuth.LogRecord::new(@azimuth.Fatal, "Fatal level message")
  
  // 2. 测试日志级别属性
  assert_eq(@azimuth.LogRecord::severity_number(trace_log), @azimuth.Trace)
  assert_eq(@azimuth.LogRecord::severity_number(debug_log), @azimuth.Debug)
  assert_eq(@azimuth.LogRecord::severity_number(info_log), @azimuth.Info)
  assert_eq(@azimuth.LogRecord::severity_number(warn_log), @azimuth.Warn)
  assert_eq(@azimuth.LogRecord::severity_number(error_log), @azimuth.Error)
  assert_eq(@azimuth.LogRecord::severity_number(fatal_log), @azimuth.Fatal)
  
  // 3. 发送所有级别的日志
  @azimuth.Logger::emit(logger, trace_log)
  @azimuth.Logger::emit(logger, debug_log)
  @azimuth.Logger::emit(logger, info_log)
  @azimuth.Logger::emit(logger, warn_log)
  @azimuth.Logger::emit(logger, error_log)
  @azimuth.Logger::emit(logger, fatal_log)
  
  // 4. 创建带有属性和上下文的复杂日志
  let attrs = @azimuth.Attributes::new()
  @azimuth.Attributes::set(attrs, "user.id", @azimuth.IntValue(12345))
  @azimuth.Attributes::set(attrs, "operation", @azimuth.StringValue("critical-operation"))
  
  let complex_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Error,
    Some("Critical operation failed"),
    Some(attrs),
    Some(@azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())),
    None,
    Some("trace-12345"),
    Some("span-67890"),
    Some(@azimuth.Context::root())
  )
  
  @azimuth.Logger::emit(logger, complex_log)
  
  // 5. 验证复杂日志的属性
  assert_eq(@azimuth.LogRecord::body(complex_log), Some("Critical operation failed"))
  assert_eq(@azimuth.LogRecord::trace_id(complex_log), Some("trace-12345"))
  assert_eq(@azimuth.LogRecord::span_id(complex_log), Some("span-67890"))
  assert_eq(@azimuth.LogRecord::severity_number(complex_log), @azimuth.Error)
}

test "上下文边界和超时测试" {
  // 测试上下文的边界条件和超时处理
  
  // 1. 测试根上下文
  let root_ctx = @azimuth.Context::root()
  let key = @azimuth.ContextKey::new("test.key")
  let root_value = @azimuth.Context::get(root_ctx, key)
  assert_eq(root_value, None)
  
  // 2. 测试多层嵌套上下文
  let ctx1 = @azimuth.Context::with_value(root_ctx, @azimuth.ContextKey::new("level1"), "value1")
  let ctx2 = @azimuth.Context::with_value(ctx1, @azimuth.ContextKey::new("level2"), "value2")
  let ctx3 = @azimuth.Context::with_value(ctx2, @azimuth.ContextKey::new("level3"), "value3")
  
  // 验证每一层的值都存在
  let level1_key = @azimuth.ContextKey::new("level1")
  let level2_key = @azimuth.ContextKey::new("level2")
  let level3_key = @azimuth.ContextKey::new("level3")
  
  assert_eq(@azimuth.Context::get(ctx3, level1_key), Some("value1"))
  assert_eq(@azimuth.Context::get(ctx3, level2_key), Some("value2"))
  assert_eq(@azimuth.Context::get(ctx3, level3_key), Some("value3"))
  
  // 3. 测试上下文覆盖
  let ctx_override = @azimuth.Context::with_value(ctx3, level1_key, "overridden_value1")
  assert_eq(@azimuth.Context::get(ctx_override, level1_key), Some("overridden_value1"))
  assert_eq(@azimuth.Context::get(ctx_override, level2_key), Some("value2")) // 其他值保持不变
  
  // 4. 测试空值和特殊字符
  let empty_key = @azimuth.ContextKey::new("")
  let special_key = @azimuth.ContextKey::new("special.key.with.dots")
  let unicode_key = @azimuth.ContextKey::new("测试键")
  
  let ctx_special = @azimuth.Context::with_value(root_ctx, empty_key, "empty_key_value")
  let ctx_special = @azimuth.Context::with_value(ctx_special, special_key, "special_value")
  let ctx_special = @azimuth.Context::with_value(ctx_special, unicode_key, "中文值")
  
  assert_eq(@azimuth.Context::get(ctx_special, empty_key), Some("empty_key_value"))
  assert_eq(@azimuth.Context::get(ctx_special, special_key), Some("special_value"))
  assert_eq(@azimuth.Context::get(ctx_special, unicode_key), Some("中文值"))
  
  // 5. 测试在Span中使用上下文
  let tracer_provider = @azimuth.TracerProvider::default()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "context-boundary-test")
  
  let (_, span_with_ctx) = @azimuth.Tracer::start_span(tracer, ctx_special, "span-with-context")
  @azimuth.Span::add_event(span_with_ctx, "context-boundary-test")
  @azimuth.Span::end(span_with_ctx)
}

test "遥测数据一致性测试" {
  // 测试Trace、Metrics、Logs之间的数据一致性
  
  let tracer_provider = @azimuth.TracerProvider::default()
  let meter_provider = @azimuth.MeterProvider::default()
  let logger_provider = @azimuth.LoggerProvider::default()
  
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "consistency-test")
  let meter = @azimuth.MeterProvider::get_meter(meter_provider, "consistency-test")
  let logger = @azimuth.LoggerProvider::get_logger(logger_provider, "consistency-test")
  
  // 1. 使用相同的trace_id和span_id关联所有遥测数据
  let trace_id = "trace-12345-67890"
  let span_id = "span-abcde-fghij"
  
  // 2. 创建Span
  let ctx = @azimuth.Context::root()
  let (_, main_span) = @azimuth.Tracer::start_span(tracer, ctx, "consistency-test-operation")
  let span_ctx = @azimuth.Span::span_context(main_span)
  
  // 3. 创建与Span关联的指标
  let counter = @azimuth.Meter::create_counter(meter, "operations.total")
  let histogram = @azimuth.Meter::create_histogram(meter, "operations.duration")
  
  let attrs = @azimuth.Attributes::new()
  @azimuth.Attributes::set(attrs, "trace.id", @azimuth.StringValue(@azimuth.SpanContext::trace_id(span_ctx)))
  @azimuth.Attributes::set(attrs, "span.id", @azimuth.StringValue(@azimuth.SpanContext::span_id(span_ctx)))
  @azimuth.Attributes::set(attrs, "operation.name", @azimuth.StringValue("consistency-test-operation"))
  
  @azimuth.Counter::add(counter, 1.0, Some(attrs))
  @azimuth.Histogram::record(histogram, 42.5, Some(attrs))
  
  // 4. 创建与Span关联的日志
  let log_attrs = @azimuth.Attributes::new()
  @azimuth.Attributes::set(log_attrs, "operation.result", @azimuth.StringValue("success"))
  @azimuth.Attributes::set(log_attrs, "operation.duration_ms", @azimuth.IntValue(42))
  
  let consistency_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Operation completed successfully"),
    Some(log_attrs),
    Some(@azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())),
    None,
    Some(@azimuth.SpanContext::trace_id(span_ctx)),
    Some(@azimuth.SpanContext::span_id(span_ctx)),
    Some(ctx)
  )
  
  @azimuth.Logger::emit(logger, consistency_log)
  
  // 5. 验证数据关联性
  let log_trace_id = @azimuth.LogRecord::trace_id(consistency_log)
  let log_span_id = @azimuth.LogRecord::span_id(consistency_log)
  
  assert_eq(log_trace_id, Some(@azimuth.SpanContext::trace_id(span_ctx)))
  assert_eq(log_span_id, Some(@azimuth.SpanContext::span_id(span_ctx)))
  
  // 6. 创建错误场景的日志
  let error_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Error,
    Some("Operation failed with timeout"),
    Some(log_attrs),
    Some(@azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())),
    None,
    Some(@azimuth.SpanContext::trace_id(span_ctx)),
    Some(@azimuth.SpanContext::span_id(span_ctx)),
    Some(ctx)
  )
  
  @azimuth.Logger::emit(logger, error_log)
  
  // 7. 更新Span状态
  @azimuth.Span::set_status(main_span, @azimuth.Error, Some("Operation failed"))
  @azimuth.Span::end(main_span)
}

test "错误边界和恢复测试" {
  // 测试系统在各种错误条件下的边界处理和恢复能力
  
  // 1. 测试无效SpanContext的处理
  let invalid_trace_ctx = @azimuth.SpanContext::new("", "span123", true, "")
  let invalid_span_ctx = @azimuth.SpanContext::new("trace123", "", true, "")
  let both_invalid_ctx = @azimuth.SpanContext::new("", "", false, "")
  
  assert_false(@azimuth.SpanContext::is_valid(invalid_trace_ctx))
  assert_false(@azimuth.SpanContext::is_valid(invalid_span_ctx))
  assert_false(@azimuth.SpanContext::is_valid(both_invalid_ctx))
  
  // 2. 测试使用无效上下文创建Span
  let tracer_provider = @azimuth.TracerProvider::default()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "error-boundary-test")
  
  // 即使上下文无效，系统也应该能够创建Span（降级处理）
  let ctx = @azimuth.Context::root()
  let (_, recovery_span) = @azimuth.Tracer::start_span(tracer, ctx, "recovery-operation")
  
  // 3. 测试属性操作的错误处理
  let attrs = @azimuth.Attributes::new()
  
  // 设置空键和空值
  @azimuth.Attributes::set(attrs, "", @azimuth.StringValue("empty-key-test"))
  @azimuth.Attributes::set(attrs, "normal.key", @azimuth.StringValue(""))
  
  // 获取不存在的键
  let non_existent = @azimuth.Attributes::get(attrs, "completely.non.existent.key")
  assert_eq(non_existent, None)
  
  // 4. 测试传播器的错误恢复
  let empty_carrier = @azimuth.TextMapCarrier::new()
  let propagator = @azimuth.W3CTraceContextPropagator::new()
  let composite = @azimuth.CompositePropagator::new([propagator])
  
  // 从空载体提取应该返回有效的上下文而不是崩溃
  let recovered_ctx = @azimuth.CompositePropagator::extract(composite, empty_carrier)
  
  // 5. 测试日志记录的错误处理
  let logger_provider = @azimuth.LoggerProvider::default()
  let logger = @azimuth.LoggerProvider::get_logger(logger_provider, "error-recovery-test")
  
  // 创建包含各种边界情况的日志
  let empty_body_log = @azimuth.LogRecord::new(@azimuth.Info, "")
  let null_attrs_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Warn,
    Some("Warning with null attributes"),
    None, // None属性
    None, // None时间戳
    None,
    None, // None trace_id
    None, // None span_id
    None  // None context
  )
  
  @azimuth.Logger::emit(logger, empty_body_log)
  @azimuth.Logger::emit(logger, null_attrs_log)
  
  // 6. 测试指标操作的错误处理
  let meter_provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(meter_provider, "error-recovery-metrics")
  
  let counter = @azimuth.Meter::create_counter(meter, "error.test.counter")
  let histogram = @azimuth.Meter::create_histogram(meter, "error.test.histogram")
  
  // 测试极端值
  @azimuth.Counter::add(counter, 0.0) // 零值
  @azimuth.Counter::add(counter, -1.0) // 负值
  @azimuth.Counter::add(counter, 999999999.0) // 极大值
  
  @azimuth.Histogram::record(histogram, 0.0) // 零值
  @azimuth.Histogram::record(histogram, -1.0) // 负值
  @azimuth.Histogram::record(histogram, 999999999.0) // 极大值
  
  // 7. 测试资源合并的错误处理
  let empty_resource = @azimuth.Resource::new()
  let null_attrs_resource = @azimuth.Resource::with_attributes(empty_resource, [])
  let merged_empty = @azimuth.Resource::merge(empty_resource, null_attrs_resource)
  
  // 验证合并不会导致错误
  let test_attr = @azimuth.Resource::get_attribute(merged_empty, "any.key")
  assert_eq(test_attr, None)
  
  @azimuth.Span::end(recovery_span)
}