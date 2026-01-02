// Azimuth Comprehensive Quality Tests
// 综合质量测试用例 - 覆盖遥测系统的关键功能和边界情况

// 测试1: 遥测数据序列化和反序列化
test "遥测数据序列化反序列化测试" {
  let tracer_provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(tracer_provider, "serialization-test")
  
  // 创建包含多种数据类型的Span
  let span = Tracer::start_span(tracer, "serialization-span")
  
  // 设置各种类型的属性
  Span::set_attribute(span, "string.value", StringValue("test-string"))
  Span::set_attribute(span, "int.value", IntValue(42))
  Span::set_attribute(span, "float.value", FloatValue(3.14))
  Span::set_attribute(span, "bool.value", BoolValue(true))
  Span::set_attribute(span, "array.value", ArrayStringValue([StringValue("item1"), StringValue("item2")]))
  
  // 添加事件
  Span::add_event(span, "test-event", Some([("event.data", StringValue("event-data"))]))
  
  // 序列化Span数据
  let serialized_data = Span::serialize(span)
  assert_true(serialized_data.length() > 0)
  
  // 反序列化Span数据
  let deserialized_span = Span::deserialize(serialized_data)
  
  // 验证反序列化后的数据
  assert_eq(Span::get_name(deserialized_span), "serialization-span")
  match Span::get_attribute(deserialized_span, "string.value") {
    Some(StringValue(value)) => assert_eq(value, "test-string")
    _ => assert_true(false)
  }
  match Span::get_attribute(deserialized_span, "int.value") {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  match Span::get_attribute(deserialized_span, "float.value") {
    Some(FloatValue(value)) => assert_eq(value, 3.14)
    _ => assert_true(false)
  }
  match Span::get_attribute(deserialized_span, "bool.value") {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false)
  }
  
  Span::end(span)
  Span::end(deserialized_span)
}

// 测试2: 跨服务传播上下文测试
test "跨服务传播上下文测试" {
  let tracer_provider1 = azimuth::TracerProvider::default()
  let tracer1 = azimuth::TracerProvider::get_tracer(tracer_provider1, "service-a")
  
  // 在服务A中创建Span
  let parent_span = azimuth::Tracer::start_span(tracer1, "service-a-operation")
  azimuth::Span::set_attribute(parent_span, "service.name", "service-a")
  
  // 提取传播上下文
  let context = azimuth::Span::get_context(parent_span)
  let propagation_headers = azimuth::Propagator::inject(context)
  
  // 验证传播头部包含必要信息
  assert_true(propagation_headers.contains("traceparent"))
  assert_true(propagation_headers.contains("tracestate"))
  
  // 模拟服务B接收上下文
  let tracer_provider2 = azimuth::TracerProvider::default()
  let tracer2 = azimuth::TracerProvider::get_tracer(tracer_provider2, "service-b")
  
  let extracted_context = azimuth::Propagator::extract(propagation_headers)
  let child_span = azimuth::Tracer::start_span_with_context(tracer2, "service-b-operation", extracted_context)
  
  // 验证父子关系
  assert_eq(azimuth::Span::get_trace_id(child_span), azimuth::Span::get_trace_id(parent_span))
  assert_eq(azimuth::Span::get_parent_span_id(child_span), azimuth::Span::get_span_id(parent_span))
  
  azimuth::Span::set_attribute(child_span, "service.name", "service-b")
  
  azimuth::Span::end(parent_span)
  azimuth::Span::end(child_span)
}

// 测试3: 性能指标收集和聚合测试
test "性能指标收集和聚合测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "performance-test")
  
  // 创建不同类型的指标
  let counter = azimuth::Meter::create_counter(meter, "request.count", "requests", "Total number of requests")
  let histogram = azimuth::Meter::create_histogram(meter, "request.duration", "ms", "Request duration distribution")
  let gauge = azimuth::Meter::create_gauge(meter, "memory.usage", "MB", "Current memory usage")
  
  // 模拟记录指标数据
  for i in 0..100 {
    azimuth::Counter::add(counter, 1, Some([("endpoint", azimuth::StringValue("/api/data")), ("method", azimuth::StringValue("GET"))]))
    
    let duration = 50.0 + (@random_float() * 200.0)  // 50-250ms随机延迟
    azimuth::Histogram::record(histogram, duration, Some([("endpoint", azimuth::StringValue("/api/data"))]))
  }
  
  // 记录当前内存使用情况
  azimuth::Gauge::set(gauge, 128.5, Some([("instance", azimuth::StringValue("server-01"))]))
  
  // 获取聚合数据
  let counter_data = azimuth::Counter::get_aggregated_data(counter)
  let histogram_data = azimuth::Histogram::get_aggregated_data(histogram)
  let gauge_data = azimuth::Gauge::get_current_value(gauge)
  
  // 验证聚合数据
  assert_eq(counter_data.value, 100)
  assert_true(histogram_data.count == 100)
  assert_true(histogram_data.sum > 5000.0)  // 至少5000ms总时间
  assert_true(histogram_data.min >= 50.0)
  assert_true(histogram_data.max <= 250.0)
  assert_eq(gauge_data.value, 128.5)
}

// 测试4: 错误处理和恢复测试
test "错误处理和恢复测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "error-handling-test")
  
  // 创建一个会经历错误的Span
  let span = azimuth::Tracer::start_span(tracer, "error-prone-operation")
  
  // 模拟操作中的错误
  azimuth::Span::add_event(span, "operation.started", Some([("input.data", azimuth::StringValue("test-input"))]))
  
  // 设置错误状态
  azimuth::Span::set_status(span, azimuth::Error)
  azimuth::Span::set_attribute(span, "error.type", azimuth::StringValue("ValidationError"))
  azimuth::Span::set_attribute(span, "error.message", azimuth::StringValue("Invalid input parameter"))
  
  // 记录错误事件
  azimuth::Span::add_event(span, "error.occurred", Some([
    ("error.code", azimuth::StringValue("ERR_001")),
    ("error.stack", azimuth::StringValue("at processInput (line 42)"))
  ]))
  
  // 模拟恢复操作
  azimuth::Span::add_event(span, "recovery.attempted", Some([("retry.count", azimuth::IntValue(3))]))
  
  // 成功恢复
  azimuth::Span::set_status(span, azimuth::Ok)
  azimuth::Span::add_event(span, "recovery.successful", Some([("final.state", azimuth::StringValue("success"))]))
  
  // 验证错误处理流程
  let events = azimuth::Span::get_events(span)
  assert_eq(events.length(), 4)
  assert_eq(events[0].name, "operation.started")
  assert_eq(events[1].name, "error.occurred")
  assert_eq(events[2].name, "recovery.attempted")
  assert_eq(events[3].name, "recovery.successful")
  
  // 验证最终状态
  assert_eq(azimuth::Span::get_status(span), azimuth::Ok)
  
  azimuth::Span::end(span)
}

// 测试5: 动态配置更新测试
test "动态配置更新测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "config-test")
  
  // 创建配置管理器
  let config_manager = azimuth::ConfigManager::new()
  
  // 设置初始配置
  azimuth::ConfigManager::set_sampling_rate(config_manager, 0.1)  // 10%采样率
  azimuth::ConfigManager::set_max_spans(config_manager, 100)
  azimuth::ConfigManager::set_batch_size(config_manager, 50)
  
  // 验证初始配置
  assert_eq(azimuth::ConfigManager::get_sampling_rate(config_manager), 0.1)
  assert_eq(azimuth::ConfigManager::get_max_spans(config_manager), 100)
  assert_eq(azimuth::ConfigManager::get_batch_size(config_manager), 50)
  
  // 创建Span并应用配置
  let span1 = azimuth::Tracer::start_span_with_config(tracer, "config-test-span-1", config_manager)
  azimuth::Span::end(span1)
  
  // 动态更新配置
  azimuth::ConfigManager::set_sampling_rate(config_manager, 0.5)  // 提高到50%采样率
  azimuth::ConfigManager::set_max_spans(config_manager, 200)
  azimuth::ConfigManager::set_batch_size(config_manager, 100)
  
  // 验证配置更新
  assert_eq(azimuth::ConfigManager::get_sampling_rate(config_manager), 0.5)
  assert_eq(azimuth::ConfigManager::get_max_spans(config_manager), 200)
  assert_eq(azimuth::ConfigManager::get_batch_size(config_manager), 100)
  
  // 创建新Span并验证新配置生效
  let span2 = azimuth::Tracer::start_span_with_config(tracer, "config-test-span-2", config_manager)
  azimuth::Span::end(span2)
  
  // 验证配置历史记录
  let config_history = azimuth::ConfigManager::get_history(config_manager)
  assert_true(config_history.length() >= 2)
}

// 测试6: 时间序列数据处理测试
test "时间序列数据处理测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "timeseries-test")
  
  // 创建时间序列指标
  let timeseries_counter = azimuth::Meter::create_timeseries_counter(meter, "cpu.usage", "percent", "CPU usage over time")
  
  // 模拟时间点数据记录
  let base_time = @current_time_millis()
  for i in 0..60 {
    let timestamp = base_time + (i * 1000)  // 每秒一个数据点
    let value = 20.0 + (@random_float() * 60.0)  // 20-80%随机CPU使用率
    
    azimuth::TimeSeriesCounter::record_at_time(timeseries_counter, value, timestamp, 
      Some([("core", azimuth::StringValue("core-" + (i % 4).to_string()))]))
  }
  
  // 获取时间序列数据
  let timeseries_data = azimuth::TimeSeriesCounter::get_timeseries_data(timeseries_counter)
  
  // 验证时间序列数据
  assert_eq(timeseries_data.points.length(), 60)
  assert_eq(timeseries_data.points[0].timestamp, base_time)
  assert_eq(timeseries_data.points[59].timestamp, base_time + 59000)
  
  // 计算统计信息
  let stats = azimuth::TimeSeriesCounter::calculate_statistics(timeseries_data)
  assert_true(stats.count == 60)
  assert_true(stats.min >= 20.0)
  assert_true(stats.max <= 80.0)
  assert_true(stats.average > 20.0 && stats.average < 80.0)
  
  // 测试时间范围查询
  let start_time = base_time + 10000  // 10秒后
  let end_time = base_time + 30000    // 30秒后
  let filtered_data = azimuth::TimeSeriesCounter::query_by_time_range(timeseries_data, start_time, end_time)
  
  assert_eq(filtered_data.points.length(), 20)  // 10-30秒之间的数据点
}

// 测试7: 采样策略测试
test "采样策略测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "sampling-test")
  
  // 创建不同采样策略
  let always_on_sampler = azimuth::Sampler::always_on()
  let always_off_sampler = azimuth::Sampler::always_off()
  let probability_sampler = azimuth::Sampler::probability(0.5)  // 50%概率采样
  
  // 测试AlwaysOn采样器
  let sampled_count = 0
  for i in 0..100 {
    let sampling_decision = azimuth::Sampler::should_sample(always_on_sampler, "trace-" + i.to_string(), "test-span")
    if sampling_decision.decision == azimuth::RecordAndSample {
      sampled_count += 1
    }
  }
  assert_eq(sampled_count, 100)  // 所有Span都应该被采样
  
  // 测试AlwaysOff采样器
  sampled_count = 0
  for i in 0..100 {
    let sampling_decision = azimuth::Sampler::should_sample(always_off_sampler, "trace-" + i.to_string(), "test-span")
    if sampling_decision.decision == azimuth::RecordAndSample {
      sampled_count += 1
    }
  }
  assert_eq(sampled_count, 0)  // 没有Span应该被采样
  
  // 测试概率采样器
  sampled_count = 0
  for i in 0..1000 {
    let sampling_decision = azimuth::Sampler::should_sample(probability_sampler, "trace-" + i.to_string(), "test-span")
    if sampling_decision.decision == azimuth::RecordAndSample {
      sampled_count += 1
    }
  }
  // 应该大约50%被采样，允许一定误差
  assert_true(sampled_count > 400 && sampled_count < 600)
  
  // 测试基于属性的采样
  let attribute_based_sampler = azimuth::Sampler::attribute_based([
    ("service.name", azimuth::StringValue("critical-service")),
    ("sample.flag", azimuth::BoolValue(true))
  ])
  
  let critical_sampling_decision = azimuth::Sampler::should_sample_with_attributes(
    attribute_based_sampler, "trace-001", "critical-span",
    [("service.name", azimuth::StringValue("critical-service"))]
  )
  assert_eq(critical_sampling_decision.decision, azimuth::RecordAndSample)
  
  let normal_sampling_decision = azimuth::Sampler::should_sample_with_attributes(
    attribute_based_sampler, "trace-002", "normal-span",
    [("service.name", azimuth::StringValue("normal-service"))]
  )
  assert_eq(normal_sampling_decision.decision, azimuth::Drop)
}

// 测试8: 批量操作性能测试
test "批量操作性能测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "batch-test")
  
  // 创建批量处理器
  let batch_processor = azimuth::BatchSpanProcessor::new(100, 5000)  // 100个Span或5秒超时
  
  // 测试批量创建Span
  let start_time = @current_time_millis()
  let spans = []
  
  for i in 0..500 {
    let span = azimuth::Tracer::start_span(tracer, "batch-span-" + i.to_string())
    azimuth::Span::set_attribute(span, "batch.id", azimuth::IntValue(i / 50))  // 50个Span一批
    azimuth::Span::set_attribute(span, "index", azimuth::IntValue(i))
    spans.push(span)
  }
  
  // 批量结束Span
  for batch_start in range_step(0, spans.length(), 50) {
    let batch_end = @min(batch_start + 50, spans.length())
    let batch = spans.slice(batch_start, batch_end)
    
    for span in batch {
      azimuth::Span::end_with_processor(span, batch_processor)
    }
  }
  
  let end_time = @current_time_millis()
  let total_time = end_time - start_time
  
  // 验证批量处理性能
  assert_true(total_time < 5000)  // 应该在5秒内完成
  assert_eq(spans.length(), 500)
  
  // 验证批次处理
  let processed_batches = azimuth::BatchSpanProcessor::get_processed_batches(batch_processor)
  assert_eq(processed_batches.length(), 10)  // 500个Span，每批50个，应该有10批
  
  // 验证每批的完整性
  for batch in processed_batches {
    assert_eq(batch.spans.length(), 50)
    assert_true(batch.processing_time > 0)
  }
}

// 测试9: 资源限制和清理测试
test "资源限制和清理测试" {
  let tracer_provider = azimuth::TracerProvider::with_limits(100, 1000)  // 最多100个活跃Span，1000个总Span
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "resource-limit-test")
  
  // 创建资源管理器
  let resource_manager = azimuth::ResourceManager::new()
  azimuth::ResourceManager::set_memory_limit(resource_manager, 10 * 1024 * 1024)  // 10MB限制
  azimuth::ResourceManager::set_span_limit(resource_manager, 100)
  
  // 测试Span数量限制
  let active_spans = []
  for i in 0..150 {
    let span = azimuth::Tracer::start_span(tracer, "limit-test-span-" + i.to_string())
    
    // 检查是否达到限制
    if azimuth::ResourceManager::is_span_limit_reached(resource_manager) {
      break
    }
    
    active_spans.push(span)
  }
  
  // 应该只创建100个Span（限制值）
  assert_eq(active_spans.length(), 100)
  
  // 测试内存限制
  let large_attribute_value = "x" * 1024 * 1024  // 1MB字符串
  for i in 0..active_spans.length() {
    let span = active_spans[i]
    
    // 尝试添加大属性，直到达到内存限制
    if azimuth::ResourceManager::is_memory_limit_reached(resource_manager) {
      break
    }
    
    azimuth::Span::set_attribute(span, "large.data." + i.to_string(), azimuth::StringValue(large_attribute_value))
  }
  
  // 验证内存限制被触发
  assert_true(azimuth::ResourceManager::is_memory_limit_reached(resource_manager))
  
  // 测试资源清理
  let cleanup_count = azimuth::ResourceManager::cleanup_old_spans(resource_manager, 1000)  // 清理1秒前的Span
  assert_true(cleanup_count > 0)
  
  // 验证清理后的资源状态
  assert_true(azimuth::ResourceManager::get_active_span_count(resource_manager) < 100)
  assert_true(azimuth::ResourceManager::get_memory_usage(resource_manager) < 10 * 1024 * 1024)
  
  // 结束剩余的Span
  for span in active_spans {
    azimuth::Span::end(span)
  }
}

// 测试10: 数据完整性验证测试
test "数据完整性验证测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "integrity-test")
  
  // 创建验证器
  let integrity_validator = azimuth::IntegrityValidator::new()
  
  // 创建包含复杂数据的Span
  let span = azimuth::Tracer::start_span(tracer, "integrity-test-span")
  
  // 设置各种类型的属性
  azimuth::Span::set_attribute(span, "string.value", azimuth::StringValue("test-string-with-special-characters-中文-🚀"))
  azimuth::Span::set_attribute(span, "int.value", azimuth::IntValue(42))
  azimuth::Span::set_attribute(span, "float.value", azimuth::FloatValue(3.14159265359))
  azimuth::Span::set_attribute(span, "bool.value", azimuth::BoolValue(true))
  azimuth::Span::set_attribute(span, "null.value", azimuth::NullValue())
  
  // 添加嵌套属性
  let nested_attributes = [
    ("nested.string", azimuth::StringValue("nested-value")),
    ("nested.int", azimuth::IntValue(100)),
    ("nested.array", azimuth::ArrayValue([
      azimuth::StringValue("item1"),
      azimuth::IntValue(2),
      azimuth::BoolValue(false)
    ]))
  ]
  
  for (key, value) in nested_attributes {
    azimuth::Span::set_attribute(span, key, value)
  }
  
  // 添加事件
  azimuth::Span::add_event(span, "test-event-1", Some([("event.data", azimuth::StringValue("event-data-1"))]))
  azimuth::Span::add_event(span, "test-event-2", Some([("event.data", azimuth::StringValue("event-data-2"))]))
  
  // 设置链接
  let linked_context = azimuth::Context::with_trace_id("linked-trace-id")
  azimuth::Span::add_link(span, linked_context, Some([("link.type", azimuth::StringValue("follows-from"))]))
  
  // 验证数据完整性
  let validation_result = azimuth::IntegrityValidator::validate_span(integrity_validator, span)
  
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // 验证所有属性都被正确保存
  assert_eq(azimuth::Span::get_attribute(span, "string.value"), "test-string-with-special-characters-中文-🚀")
  assert_eq(azimuth::Span::get_attribute(span, "int.value"), 42)
  assert_eq(azimuth::Span::get_attribute(span, "float.value"), 3.14159265359)
  assert_eq(azimuth::Span::get_attribute(span, "bool.value"), true)
  assert_eq(azimuth::Span::get_attribute(span, "null.value"), azimuth::NullValue())
  
  // 验证嵌套属性
  assert_eq(azimuth::Span::get_attribute(span, "nested.string"), "nested-value")
  assert_eq(azimuth::Span::get_attribute(span, "nested.int"), 100)
  
  let nested_array = azimuth::Span::get_attribute(span, "nested.array")
  assert_true(azimuth::is_array_value(nested_array))
  assert_eq(azimuth::array_value_length(nested_array), 3)
  
  // 验证事件
  let events = azimuth::Span::get_events(span)
  assert_eq(events.length(), 2)
  assert_eq(events[0].name, "test-event-1")
  assert_eq(events[1].name, "test-event-2")
  
  // 验证链接
  let links = azimuth::Span::get_links(span)
  assert_eq(links.length(), 1)
  assert_eq(links[0].trace_id, "linked-trace-id")
  
  // 序列化和反序列化验证
  let serialized = azimuth::Span::serialize(span)
  let deserialized = azimuth::Span::deserialize(serialized)
  
  let deserialized_validation = azimuth::IntegrityValidator::validate_span(integrity_validator, deserialized)
  assert_true(deserialized_validation.is_valid)
  
  // 比较原始和反序列化的数据
  assert_eq(azimuth::Span::get_attribute(span, "string.value"), 
            azimuth::Span::get_attribute(deserialized, "string.value"))
  assert_eq(azimuth::Span::get_events(span).length(), 
            azimuth::Span::get_events(deserialized).length())
  assert_eq(azimuth::Span::get_links(span).length(), 
            azimuth::Span::get_links(deserialized).length())
  
  azimuth::Span::end(span)
  azimuth::Span::end(deserialized)
}