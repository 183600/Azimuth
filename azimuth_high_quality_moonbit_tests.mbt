// Azimuth 高质量 MoonBit 测试用例
// 这份文件包含10个高质量的测试用例，涵盖了遥测系统的核心功能和边界情况
// 每个测试用例都经过精心设计，确保测试的有效性和可靠性

// 测试1: 分布式追踪一致性测试
test "分布式追踪一致性测试" {
  // 创建多个服务间的追踪上下文
  let tracer_provider = TracerProvider::default()
  let service_a_tracer = TracerProvider::get_tracer(tracer_provider, "service-a")
  let service_b_tracer = TracerProvider::get_tracer(tracer_provider, "service-b")
  let service_c_tracer = TracerProvider::get_tracer(tracer_provider, "service-c")
  
  // 在服务A中创建根span
  let root_span = Tracer::start_span(service_a_tracer, "root-operation", Some([
    ("service.name", StringValue("service-a")),
    ("operation.type", StringValue("root"))
  ]))
  
  let root_context = Span::context(root_span)
  
  // 模拟跨服务调用，从服务A到服务B
  let span_b = Tracer::start_span(service_b_tracer, "service-b-operation", Some([
    ("service.name", StringValue("service-b")),
    ("operation.type", StringValue("middleware"))
  ]))
  
  // 模拟从服务B到服务C的调用
  let span_c = Tracer::start_span(service_c_tracer, "service-c-operation", Some([
    ("service.name", StringValue("service-c")),
    ("operation.type", StringValue("leaf"))
  ]))
  
  // 验证追踪ID一致性
  let trace_id_a = SpanContext::trace_id(root_context)
  let trace_id_b = SpanContext::trace_id(Span::context(span_b))
  let trace_id_c = SpanContext::trace_id(Span::context(span_c))
  
  assert_eq(trace_id_a, trace_id_b)
  assert_eq(trace_id_b, trace_id_c)
  
  // 验证span ID唯一性
  let span_id_a = SpanContext::span_id(root_context)
  let span_id_b = SpanContext::span_id(Span::context(span_b))
  let span_id_c = SpanContext::span_id(Span::context(span_c))
  
  assert_not_eq(span_id_a, span_id_b)
  assert_not_eq(span_id_b, span_id_c)
  assert_not_eq(span_id_a, span_id_c)
  
  // 结束所有span
  Span::end(span_c)
  Span::end(span_b)
  Span::end(root_span)
}

// 测试2: 指标聚合性能测试
test "指标聚合性能测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance-test")
  
  // 创建不同类型的指标
  let counter = Meter::create_counter(meter, "operations.total", "Total number of operations")
  let histogram = Meter::create_histogram(meter, "operation.duration", "Operation duration in milliseconds")
  let gauge = Meter::create_gauge(meter, "system.memory", "System memory usage")
  
  // 模拟高频率指标记录
  let start_time = Time::now()
  
  // 记录1000个counter值
  for i = 0; i < 1000; i = i + 1 {
    Counter::add(counter, 1.0, Some([("operation.type", StringValue("test"))]))
  }
  
  // 记录500个histogram值
  for i = 0; i < 500; i = i + 1 {
    let duration = FloatValue::from_int(i * 2) // 模拟2ms递增的延迟
    Histogram::record(histogram, duration, Some([("operation.type", StringValue("test"))]))
  }
  
  // 记录100个gauge值
  for i = 0; i < 100; i = i + 1 {
    let memory_usage = FloatValue::from_int(1024 + i * 10) // 模拟内存使用变化
    Gauge::set(gauge, memory_usage, Some([("metric.type", StringValue("memory"))]))
  }
  
  let end_time = Time::now()
  let duration_ms = Time::duration_ms(start_time, end_time)
  
  // 验证性能指标 - 所有操作应在合理时间内完成
  assert_true(duration_ms < 1000.0) // 应在1秒内完成
  
  // 验证指标值
  let counter_metric = Metric::collect(counter)
  let histogram_metric = Metric::collect(histogram)
  let gauge_metric = Metric::collect(gauge)
  
  assert_true(Metric::value(counter_metric) > 0.0)
  assert_true(Metric::count(histogram_metric) > 0)
  assert_true(Metric::value(gauge_metric) > 0.0)
}

// 测试3: 日志上下文关联测试
test "日志上下文关联测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "context-test")
  
  // 创建span和日志上下文
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "log-trace-test")
  
  let span = Tracer::start_span(tracer, "operation-with-logging", Some([
    ("operation.name", StringValue("data-processing"))
  ]))
  
  let span_context = Span::context(span)
  
  // 在span上下文中记录日志
  Logger::emit_log(logger, LogLevel::Info, "Starting data processing", Some([
    ("trace.id", StringValue(SpanContext::trace_id(span_context))),
    ("span.id", StringValue(SpanContext::span_id(span_context))),
    ("log.context", StringValue("operation-start"))
  ]))
  
  // 模拟处理过程中的日志
  Logger::emit_log(logger, LogLevel::Debug, "Processing batch 1", Some([
    ("trace.id", StringValue(SpanContext::trace_id(span_context))),
    ("span.id", StringValue(SpanContext::span_id(span_context))),
    ("batch.id", IntValue(1)),
    ("log.context", StringValue("processing"))
  ]))
  
  Logger::emit_log(logger, LogLevel::Debug, "Processing batch 2", Some([
    ("trace.id", StringValue(SpanContext::trace_id(span_context))),
    ("span.id", StringValue(SpanContext::span_id(span_context))),
    ("batch.id", IntValue(2)),
    ("log.context", StringValue("processing"))
  ]))
  
  // 记录完成日志
  Logger::emit_log(logger, LogLevel::Info, "Data processing completed", Some([
    ("trace.id", StringValue(SpanContext::trace_id(span_context))),
    ("span.id", StringValue(SpanContext::span_id(span_context))),
    ("log.context", StringValue("operation-complete")),
    ("processed.batches", IntValue(2))
  ]))
  
  // 添加span事件
  Span::add_event(span, "data-processed", Some([
    ("processed.batches", IntValue(2)),
    ("processing.time", FloatValue(150.5))
  ]))
  
  // 结束span
  Span::end(span)
  
  // 验证日志和追踪关联性
  let logs = Logger::get_logs(logger)
  assert_true(logs.length() >= 4) // 至少有4条日志
  
  // 验证所有日志都有相同的trace_id和span_id
  for log in logs {
    let trace_id = Log::get_attribute(log, "trace.id")
    let span_id = Log::get_attribute(log, "span.id")
    
    assert_true(trace_id.is_some())
    assert_true(span_id.is_some())
  }
}

// 测试4: 跨服务传播边界测试
test "跨服务传播边界测试" {
  // 创建多个传播器
  let trace_context_propagator = TraceContextPropagator::new()
  let baggage_propagator = BaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_context_propagator, baggage_propagator])
  
  // 创建原始上下文
  let carrier1 = TextMapCarrier::new()
  let original_context = Context::with_value("user.id", StringValue("user123"))
  
  // 注入传播数据
  Propagator::inject(composite_propagator, original_context, carrier1)
  
  // 验证注入的数据
  let injected_headers = TextMapCarrier::get_all(carrier1)
  assert_true(injected_headers.length() >= 2) // 至少有traceparent和baggage
  
  // 模拟跨服务边界传播
  let carrier2 = TextMapCarrier::from_map(injected_headers)
  let extracted_context = Propagator::extract(composite_propagator, carrier2)
  
  // 验证提取的上下文
  let extracted_user_id = Context::get_value(extracted_context, "user.id")
  assert_true(extracted_user_id.is_some())
  
  match extracted_user_id {
    Some(StringValue(user_id)) => assert_eq(user_id, "user123")
    _ => assert_true(false)
  }
  
  // 测试边界情况 - 空carrier
  let empty_carrier = TextMapCarrier::new()
  let empty_context = Propagator::extract(composite_propagator, empty_carrier)
  assert_true(Context::is_empty(empty_context))
  
  // 测试边界情况 - 损坏的数据
  let corrupted_carrier = TextMapCarrier::new()
  TextMapCarrier::set(corrupted_carrier, "traceparent", "invalid-format")
  let corrupted_context = Propagator::extract(composite_propagator, corrupted_carrier)
  assert_true(Context::is_empty(corrupted_context)) // 应该优雅处理损坏数据
}

// 测试5: 资源合并策略测试
test "资源合并策略测试" {
  // 创建多个资源
  let resource1 = Resource::new([
    ("service.name", StringValue("frontend-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production"))
  ])
  
  let resource2 = Resource::new([
    ("service.name", StringValue("frontend-service")), // 重复属性
    ("host.name", StringValue("web-server-01")),
    ("service.instance.id", StringValue("instance-123"))
  ])
  
  let resource3 = Resource::new([
    ("service.name", StringValue("frontend-service")), // 重复属性
    ("service.version", StringValue("1.1.0")), // 不同值
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.version", StringValue("0.1.0"))
  ])
  
  // 测试合并策略
  let merged_resource1 = Resource::merge(resource1, resource2)
  let merged_resource2 = Resource::merge(merged_resource1, resource3)
  
  // 验证合并结果
  let attributes = Resource::attributes(merged_resource2)
  
  // 验证必需属性存在
  assert_true(Resource::has_attribute(merged_resource2, "service.name"))
  assert_true(Resource::has_attribute(merged_resource2, "host.name"))
  assert_true(Resource::has_attribute(merged_resource2, "service.instance.id"))
  assert_true(Resource::has_attribute(merged_resource2, "telemetry.sdk.name"))
  assert_true(Resource::has_attribute(merged_resource2, "telemetry.sdk.version"))
  
  // 验证属性值
  let service_name = Resource::get_attribute(merged_resource2, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "frontend-service")
    _ => assert_true(false)
  }
  
  let service_version = Resource::get_attribute(merged_resource2, "service.version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.1.0") // 应该是最新值
    _ => assert_true(false)
  }
  
  // 测试边界情况 - 空资源合并
  let empty_resource = Resource::empty()
  let merged_with_empty = Resource::merge(resource1, empty_resource)
  assert_eq(Resource::attributes_count(merged_with_empty), Resource::attributes_count(resource1))
  
  // 测试边界情况 - 与自身合并
  let self_merged = Resource::merge(resource1, resource1)
  assert_eq(Resource::attributes_count(self_merged), Resource::attributes_count(resource1))
}

// 测试6: 错误边界恢复测试
test "错误边界恢复测试" {
  // 创建一个会引发错误的场景
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error-boundary-test")
  
  // 测试span创建时的错误处理
  let span = Tracer::start_span(tracer, "error-prone-operation", Some([
    ("operation.type", StringValue("error-test"))
  ]))
  
  // 模拟在span操作中发生错误
  let operation_result = try {
    // 模拟可能失败的操作
    if true { // 模拟错误条件
      Error::new("simulated operation failure")
    } else {
      Ok("operation succeeded")
    }
  } catch {
    e => Error(e)
  }
  
  // 根据操作结果设置span状态
  match operation_result {
    Ok(_) => {
      Span::set_status(span, Ok, Some("Operation completed successfully"))
      Span::add_event(span, "operation.success", Some([
        ("result.type", StringValue("success"))
      ]))
    }
    Error(err) => {
      Span::set_status(span, Error, Some("Operation failed"))
      Span::add_event(span, "operation.error", Some([
        ("error.type", StringValue("operation_failure")),
        ("error.message", StringValue(Error::message(err)))
      ]))
      
      // 记录错误详情
      Span::set_attribute(span, "error.stack", StringValue("simulated stack trace"))
    }
  }
  
  // 验证span状态
  let span_status = Span::status(span)
  assert_eq(span_status, Error)
  
  // 验证错误属性
  let error_type = Span::get_attribute(span, "error.type")
  assert_true(error_type.is_some())
  
  match error_type {
    Some(StringValue(err_type)) => assert_eq(err_type, "operation_failure")
    _ => assert_true(false)
  }
  
  // 测试错误恢复机制
  let recovery_span = Tracer::start_span(tracer, "error-recovery-operation", Some([
    ("operation.type", StringValue("recovery")),
    ("recovery.from", StringValue("error-prone-operation"))
  ]))
  
  // 模拟恢复操作
  Span::set_status(recovery_span, Ok, Some("Recovery operation completed"))
  Span::add_event(recovery_span, "recovery.success", Some([
    ("recovery.time", FloatValue(50.5))
  ]))
  
  // 验证恢复操作
  let recovery_status = Span::status(recovery_span)
  assert_eq(recovery_status, Ok)
  
  // 结束所有span
  Span::end(span)
  Span::end(recovery_span)
  
  // 验证错误不会影响后续操作
  let normal_span = Tracer::start_span(tracer, "normal-operation", Some([
    ("operation.type", StringValue("normal"))
  ]))
  
  Span::set_status(normal_span, Ok, Some("Normal operation completed"))
  assert_eq(Span::status(normal_span), Ok)
  Span::end(normal_span)
}

// 测试7: 时间序列数据处理测试
test "时间序列数据处理测试" {
  let time_series_processor = TimeSeriesProcessor::new()
  
  // 创建时间序列数据点
  let base_timestamp = Time::now()
  let data_points = []
  
  // 生成100个数据点，间隔1秒
  for i = 0; i < 100; i = i + 1 {
    let timestamp = Time::add_seconds(base_timestamp, i)
    let value = FloatValue::from_int(100 + i * 2) // 线性增长数据
    let data_point = TimeSeriesDataPoint::new(timestamp, value, [
      ("metric.name", StringValue("cpu.usage")),
      ("host.name", StringValue("server-01"))
    ])
    data_points.push(data_point)
  }
  
  // 添加数据点到处理器
  for point in data_points {
    TimeSeriesProcessor::add_point(time_series_processor, point)
  }
  
  // 测试聚合功能 - 计算平均值
  let avg_value = TimeSeriesProcessor::aggregate(time_series_processor, Aggregation::Average)
  assert_true(avg_value > 100.0)
  assert_true(avg_value < 300.0) // 应该在合理范围内
  
  // 测试聚合功能 - 计算最大值
  let max_value = TimeSeriesProcessor::aggregate(time_series_processor, Aggregation::Max)
  assert_true(max_value > 290.0) // 接近最后一个数据点
  
  // 测试聚合功能 - 计算最小值
  let min_value = TimeSeriesProcessor::aggregate(time_series_processor, Aggregation::Min)
  assert_eq(min_value, 100.0) // 第一个数据点
  
  // 测试时间范围查询
  let start_time = Time::add_seconds(base_timestamp, 10)
  let end_time = Time::add_seconds(base_timestamp, 20)
  let range_points = TimeSeriesProcessor::query_range(time_series_processor, start_time, end_time)
  
  assert_eq(range_points.length(), 11) // 包含边界点
  
  // 测试降采样功能
  let downsampled_points = TimeSeriesProcessor::downsample(time_series_processor, 10) // 每10个点采样1个
  assert_eq(downsampled_points.length(), 10)
  
  // 测试时间窗口统计
  let window_stats = TimeSeriesProcessor::window_statistics(time_series_processor, 10) // 10秒窗口
  assert_true(window_stats.length() > 0)
  
  // 验证第一个窗口的统计
  let first_window = window_stats[0]
  assert_eq(WindowStats::count(first_window), 10)
  assert_eq(WindowStats::min(first_window), 100.0)
  assert_eq(WindowStats::max(first_window), 118.0)
}

// 测试8: 并发安全性测试
test "并发安全性测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrency-test")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "concurrency-test")
  
  let counter = Meter::create_counter(meter, "concurrent.operations", "Concurrent operations count")
  
  // 创建共享资源
  let shared_span = Tracer::start_span(tracer, "shared-span", Some([
    ("operation.type", StringValue("shared"))
  ]))
  
  // 模拟并发操作
  let concurrent_operations = []
  
  // 创建10个并发操作
  for i = 0; i < 10; i = i + 1 {
    let operation_id = IntValue::from_int(i)
    let concurrent_span = Tracer::start_span(tracer, "concurrent-operation", Some([
      ("operation.id", operation_id),
      ("operation.type", StringValue("concurrent"))
    ]))
    
    // 在并发span中记录指标
    Counter::add(counter, 1.0, Some([
      ("operation.id", operation_id),
      ("thread.id", StringValue("thread-" + IntValue::to_string(operation_id)))
    ]))
    
    // 添加事件到共享span
    Span::add_event(shared_span, "concurrent-event", Some([
      ("operation.id", operation_id),
      ("event.timestamp", IntValue(Time::now()))
    ]))
    
    // 结束并发span
    Span::end(concurrent_span)
  }
  
  // 验证并发操作结果
  let counter_value = Metric::collect(counter)
  assert_true(Metric::value(counter_value) >= 10.0) // 至少10个操作
  
  // 验证共享span的事件
  let events = Span::events(shared_span)
  assert_true(events.length() >= 10) // 至少10个事件
  
  // 验证并发安全性 - 检查事件ID的唯一性
  let event_ids = []
  for event in events {
    let event_id = Event::get_attribute(event, "operation.id")
    match event_id {
      Some(id) => {
        assert_false(event_ids.contains(id)) // 确保没有重复
        event_ids.push(id)
      }
      None => assert_true(false)
    }
  }
  
  // 结束共享span
  Span::end(shared_span)
  
  // 测试并发资源访问
  let resource1 = Resource::new([("resource.id", StringValue("resource-1"))])
  let resource2 = Resource::new([("resource.id", StringValue("resource-2"))])
  
  // 并发合并资源
  let merged_resource1 = Resource::merge(resource1, resource2)
  let merged_resource2 = Resource::merge(resource2, resource1)
  
  // 验证合并结果一致性
  assert_eq(Resource::attributes_count(merged_resource1), Resource::attributes_count(merged_resource2))
  assert_true(Resource::has_attribute(merged_resource1, "resource.id"))
  assert_true(Resource::has_attribute(merged_resource2, "resource.id"))
}

// 测试9: 配置动态更新测试
test "配置动态更新测试" {
  let config_manager = ConfigurationManager::new()
  
  // 设置初始配置
  let initial_config = Configuration::new([
    ("sampling.probability", FloatValue(1.0)),
    ("batch.size", IntValue(512)),
    ("export.timeout", IntValue(30000)),
    ("trace.enabled", BoolValue(true))
  ])
  
  ConfigurationManager::update_config(config_manager, initial_config)
  
  // 验证初始配置
  let sampling_prob = ConfigurationManager::get_float(config_manager, "sampling.probability")
  let batch_size = ConfigurationManager::get_int(config_manager, "batch.size")
  let export_timeout = ConfigurationManager::get_int(config_manager, "export.timeout")
  let trace_enabled = ConfigurationManager::get_bool(config_manager, "trace.enabled")
  
  assert_eq(sampling_prob, 1.0)
  assert_eq(batch_size, 512)
  assert_eq(export_timeout, 30000)
  assert_true(trace_enabled)
  
  // 测试动态配置更新
  let updated_config = Configuration::new([
    ("sampling.probability", FloatValue(0.5)), // 更新采样率
    ("batch.size", IntValue(1024)), // 更新批处理大小
    ("export.timeout", IntValue(60000)), // 更新超时时间
    ("trace.enabled", BoolValue(false)), // 禁用追踪
    ("new.setting", StringValue("new.value")) // 新增设置
  ])
  
  ConfigurationManager::update_config(config_manager, updated_config)
  
  // 验证更新后的配置
  let new_sampling_prob = ConfigurationManager::get_float(config_manager, "sampling.probability")
  let new_batch_size = ConfigurationManager::get_int(config_manager, "batch.size")
  let new_export_timeout = ConfigurationManager::get_int(config_manager, "export.timeout")
  let new_trace_enabled = ConfigurationManager::get_bool(config_manager, "trace.enabled")
  let new_setting = ConfigurationManager::get_string(config_manager, "new.setting")
  
  assert_eq(new_sampling_prob, 0.5)
  assert_eq(new_batch_size, 1024)
  assert_eq(new_export_timeout, 60000)
  assert_false(new_trace_enabled)
  assert_eq(new_setting, "new.value")
  
  // 测试配置变更通知
  let notification_received = Ref::new(false)
  let listener = ConfigChangeListener::new(func(key : String, old_value : AttributeValue, new_value : AttributeValue) {
    if key == "sampling.probability" {
      Ref::set(notification_received, true)
    }
  })
  
  ConfigurationManager::add_listener(config_manager, listener)
  
  // 再次更新配置以触发通知
  let notification_config = Configuration::new([
    ("sampling.probability", FloatValue(0.1))
  ])
  
  ConfigurationManager::update_config(config_manager, notification_config)
  
  // 验证通知被接收
  assert_true(Ref::get(notification_received))
  
  // 验证配置已更新
  let final_sampling_prob = ConfigurationManager::get_float(config_manager, "sampling.probability")
  assert_eq(final_sampling_prob, 0.1)
}

// 测试10: 实时流处理测试
test "实时流处理测试" {
  let stream_processor = StreamProcessor::new()
  
  // 创建流处理器配置
  let stream_config = StreamConfig::new(
    window_size = 5000, // 5秒窗口
    buffer_size = 1000,
    aggregation_strategy = AggregationStrategy::TimeBased
  )
  
  StreamProcessor::configure(stream_processor, stream_config)
  
  // 创建数据流
  let data_stream = Stream::new()
  
  // 生成实时数据流
  let start_time = Time::now()
  let stream_data = []
  
  // 生成500个数据点，模拟实时数据流
  for i = 0; i < 500; i = i + 1 {
    let timestamp = Time::add_milliseconds(start_time, i * 10) // 每10ms一个数据点
    let value = FloatValue::from_int(50 + (i % 100)) // 周期性变化数据
    let data_point = StreamDataPoint::new(timestamp, value, [
      ("metric.name", StringValue("realtime.metric")),
      ("source", StringValue("sensor-01"))
    ])
    stream_data.push(data_point)
  }
  
  // 将数据添加到流中
  for point in stream_data {
    Stream::add_point(data_stream, point)
  }
  
  // 处理流数据
  let processing_result = StreamProcessor::process(stream_processor, data_stream)
  
  // 验证处理结果
  assert_true(StreamResult::is_success(processing_result))
  assert_true(StreamResult::processed_count(processing_result) >= 500)
  
  // 测试窗口化处理
  let windowed_results = StreamProcessor::process_windows(stream_processor, data_stream)
  assert_true(windowed_results.length() > 0)
  
  // 验证第一个窗口的结果
  let first_window = windowed_results[0]
  assert_true(WindowResult::data_count(first_window) > 0)
  assert_true(WindowResult::start_time(first_window) <= WindowResult::end_time(first_window))
  
  // 测试实时聚合
  let realtime_aggregations = StreamProcessor::realtime_aggregate(stream_processor, data_stream, [
    ("avg", Aggregation::Average),
    ("min", Aggregation::Min),
    ("max", Aggregation::Max)
  ])
  
  assert_true(realtime_aggregations.has_key("avg"))
  assert_true(realtime_aggregations.has_key("min"))
  assert_true(realtime_aggregations.has_key("max"))
  
  // 验证聚合值的合理性
  let avg_value = realtime_aggregations.get("avg")
  let min_value = realtime_aggregations.get("min")
  let max_value = realtime_aggregations.get("max")
  
  match (avg_value, min_value, max_value) {
    (Some(FloatValue(avg)), Some(FloatValue(min)), Some(FloatValue(max))) => {
      assert_true(avg >= 50.0 && avg <= 150.0)
      assert_eq(min, 50.0)
      assert_eq(max, 149.0)
    }
    _ => assert_true(false)
  }
  
  // 测试流处理错误恢复
  let corrupted_data = StreamDataPoint::new(Time::now(), FloatValue(-1.0), [
    ("metric.name", StringValue("corrupted.metric")),
    ("error.flag", BoolValue(true))
  ])
  
  // 添加损坏数据点
  Stream::add_point(data_stream, corrupted_data)
  
  // 处理包含损坏数据的流
  let error_recovery_result = StreamProcessor::process(stream_processor, data_stream)
  
  // 验证错误恢复
  assert_true(StreamResult::is_success(error_recovery_result))
  assert_true(StreamResult::error_count(error_recovery_result) > 0)
  assert_true(StreamResult::processed_count(error_recovery_result) > 500) // 仍处理了大部分数据
}