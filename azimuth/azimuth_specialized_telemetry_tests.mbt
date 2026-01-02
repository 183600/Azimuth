// Azimuth Telemetry System Specialized Test Suite
// This file contains specialized test cases focusing on core telemetry functionality
// These tests complement the existing basic and advanced test suites

// Test 1: 遥测数据序列化和反序列化测试
test "遥测数据序列化和反序列化测试" {
  // 创建测试span
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "serialization-test")
  
  let span = Tracer::start_span(tracer, "serialization.span", Some([
    ("string.attr", StringValue("test.value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true))
  ]))
  
  // 添加事件
  Span::add_event(span, "serialization.event", Some([
    ("event.data", StringValue("test.event.data")),
    ("event.timestamp", IntValue(1640995200000000000L))
  ]))
  
  // 序列化span数据
  let serialized_data = Span::serialize(span)
  
  // 验证序列化数据不为空
  assert_true(serialized_data.length() > 0)
  
  // 反序列化span数据
  let deserialized_span = Span::deserialize(serialized_data)
  
  // 验证反序列化后的数据
  assert_true(deserialized_span.is_some())
  
  // 验证反序列化后的属性
  let deserialized = deserialized_span.unwrap()
  let string_attr = Span::get_attribute(deserialized, "string.attr")
  let int_attr = Span::get_attribute(deserialized, "int.attr")
  let float_attr = Span::get_attribute(deserialized, "float.attr")
  let bool_attr = Span::get_attribute(deserialized, "bool.attr")
  
  assert_true(string_attr.is_some())
  assert_true(int_attr.is_some())
  assert_true(float_attr.is_some())
  assert_true(bool_attr.is_some())
  
  Span::end(span)
  Span::end(deserialized)
}

// Test 2: 分布式追踪上下文传播测试
test "分布式追踪上下文传播测试" {
  // 创建根span
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "context-propagation-test")
  
  let root_span = Tracer::start_span(tracer, "root.operation")
  let root_context = Span::span_context(root_span)
  
  // 验证根上下文有效
  assert_true(SpanContext::is_valid(root_context))
  
  // 创建子span
  let child_span = Tracer::start_span_with_context(tracer, "child.operation", Some(root_context))
  let child_context = Span::span_context(child_span)
  
  // 验证子上下文有效且与根上下文关联
  assert_true(SpanContext::is_valid(child_context))
  assert_eq(SpanContext::trace_id(root_context), SpanContext::trace_id(child_context))
  
  // 验证span ID不同
  assert_true(SpanContext::span_id(root_context) != SpanContext::span_id(child_context))
  
  // 创建孙子span
  let grandchild_span = Tracer::start_span_with_context(tracer, "grandchild.operation", Some(child_context))
  let grandchild_context = Span::span_context(grandchild_span)
  
  // 验证孙子span的上下文关联
  assert_true(SpanContext::is_valid(grandchild_context))
  assert_eq(SpanContext::trace_id(root_context), SpanContext::trace_id(grandchild_context))
  assert_eq(SpanContext::trace_id(child_context), SpanContext::trace_id(grandchild_context))
  
  // 测试上下文注入和提取
  let headers = []
  let propagator = TextMapPropagator::default()
  
  // 注入上下文到headers
  TextMapPropagator::inject(propagator, root_context, headers)
  
  // 从headers提取上下文
  let extracted_context = TextMapPropagator::extract(propagator, headers)
  
  // 验证提取的上下文与原始上下文匹配
  assert_true(extracted_context.is_some())
  let extracted = extracted_context.unwrap()
  assert_eq(SpanContext::trace_id(root_context), SpanContext::trace_id(extracted))
  
  Span::end(grandchild_span)
  Span::end(child_span)
  Span::end(root_span)
}

// Test 3: 度量数据聚合和统计测试
test "度量数据聚合和统计测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "aggregation-test")
  
  // 创建各种度量仪器
  let counter = Meter::create_counter(meter, "test.counter")
  let histogram = Meter::create_histogram(meter, "test.histogram")
  let gauge = Meter::create_gauge(meter, "test.gauge")
  
  // 测试计数器聚合
  for i in 1..=100 {
    Counter::add(counter, i.to_double())
  }
  
  let counter_metrics = Meter::collect(meter)
  assert_true(counter_metrics.length() > 0)
  
  // 测试直方图聚合
  for i in 1..=100 {
    Histogram::record(histogram, i.to_double())
  }
  
  let histogram_metrics = Meter::collect(meter)
  assert_true(histogram_metrics.length() > 0)
  
  // 测试仪表盘数据
  for i in 1..=10 {
    Gauge::set(gauge, (i * 10).to_double())
  }
  
  let gauge_metrics = Meter::collect(meter)
  assert_true(gauge_metrics.length() > 0)
  
  // 验证度量数据的统计特性
  let all_metrics = Meter::collect(meter)
  
  // 验证计数器总和
  let counter_sum = 0.0
  for metric in all_metrics {
    if Metric::name(metric) == "test.counter" {
      counter_sum = counter_sum + Metric::value(metric)
    }
  }
  
  // 1+2+...+100 = 5050
  assert_eq(counter_sum, 5050.0)
  
  // 验证直方图统计
  let histogram_count = 0
  let histogram_sum = 0.0
  for metric in all_metrics {
    if Metric::name(metric) == "test.histogram" {
      histogram_count = histogram_count + 1
      histogram_sum = histogram_sum + Metric::value(metric)
    }
  }
  
  assert_eq(histogram_count, 100)
  assert_eq(histogram_sum, 5050.0)
}

// Test 4: 日志级别和过滤测试
test "日志级别和过滤测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "log-filter-test")
  
  // 创建不同级别的日志记录
  let trace_log = LogRecord::new(Trace, "Trace level message")
  let debug_log = LogRecord::new(Debug, "Debug level message")
  let info_log = LogRecord::new(Info, "Info level message")
  let warn_log = LogRecord::new(Warn, "Warning level message")
  let error_log = LogRecord::new(Error, "Error level message")
  let fatal_log = LogRecord::new(Fatal, "Fatal level message")
  
  // 设置日志过滤器为INFO级别
  LoggerProvider::set_log_level(logger_provider, Info)
  
  // 发送所有级别的日志
  Logger::emit(logger, trace_log)
  Logger::emit(logger, debug_log)
  Logger::emit(logger, info_log)
  Logger::emit(logger, warn_log)
  Logger::emit(logger, error_log)
  Logger::emit(logger, fatal_log)
  
  // 收集日志记录
  let logs = LoggerProvider::get_logs(logger_provider)
  
  // 验证只有INFO及以上级别的日志被记录
  let info_count = 0
  let warn_count = 0
  let error_count = 0
  let fatal_count = 0
  
  for log in logs {
    match LogRecord::level(log) {
      Info => info_count = info_count + 1
      Warn => warn_count = warn_count + 1
      Error => error_count = error_count + 1
      Fatal => fatal_count = fatal_count + 1
      _ => ()
    }
  }
  
  assert_eq(info_count, 1)
  assert_eq(warn_count, 1)
  assert_eq(error_count, 1)
  assert_eq(fatal_count, 1)
  
  // 更改日志过滤器为ERROR级别
  LoggerProvider::set_log_level(logger_provider, Error)
  
  // 再次发送所有级别的日志
  Logger::emit(logger, trace_log)
  Logger::emit(logger, debug_log)
  Logger::emit(logger, info_log)
  Logger::emit(logger, warn_log)
  Logger::emit(logger, error_log)
  Logger::emit(logger, fatal_log)
  
  // 收集新的日志记录
  let error_logs = LoggerProvider::get_logs(logger_provider)
  
  // 验证只有ERROR及以上级别的日志被记录
  let new_error_count = 0
  let new_fatal_count = 0
  
  for log in error_logs {
    match LogRecord::level(log) {
      Error => new_error_count = new_error_count + 1
      Fatal => new_fatal_count = new_fatal_count + 1
      _ => ()
    }
  }
  
  assert_eq(new_error_count, 1)
  assert_eq(new_fatal_count, 1)
}

// Test 5: 资源属性合并策略测试
test "资源属性合并策略测试" {
  // 创建基础资源
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("environment", StringValue("development")),
    ("host.name", StringValue("base-host"))
  ])
  
  // 创建覆盖资源
  let override_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("override-service")),  // 应该覆盖
    ("service.instance.id", StringValue("instance-123")),  // 新增
    ("deployment.environment", StringValue("staging"))  // 新增
  ])
  
  // 测试合并策略（覆盖资源优先）
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // 验证合并后的属性
  let merged_attrs = Resource::attributes(merged_resource)
  
  // 验证覆盖的属性
  let service_name = Attributes::get(merged_attrs, "service.name")
  assert_true(service_name.is_some())
  match service_name.unwrap() {
    StringValue(name) => assert_eq(name, "override-service")
    _ => assert_true(false)
  }
  
  // 验证保留的基础属性
  let service_version = Attributes::get(merged_attrs, "service.version")
  assert_true(service_version.is_some())
  match service_version.unwrap() {
    StringValue(version) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  // 验证新增的属性
  let instance_id = Attributes::get(merged_attrs, "service.instance.id")
  assert_true(instance_id.is_some())
  match instance_id.unwrap() {
    StringValue(id) => assert_eq(id, "instance-123")
    _ => assert_true(false)
  }
  
  // 验证环境属性没有冲突
  let environment = Attributes::get(merged_attrs, "environment")
  assert_true(environment.is_some())
  match environment.unwrap() {
    StringValue(env) => assert_eq(env, "development")
    _ => assert_true(false)
  }
  
  // 验证新增的部署环境属性
  let deployment_env = Attributes::get(merged_attrs, "deployment.environment")
  assert_true(deployment_env.is_some())
  match deployment_env.unwrap() {
    StringValue(env) => assert_eq(env, "staging")
    _ => assert_true(false)
  }
}

// Test 6: 采样策略测试
test "采样策略测试" {
  // 创建采样器
  let always_on_sampler = AlwaysOnSampler::new()
  let always_off_sampler = AlwaysOffSampler::new()
  let trace_id_ratio_sampler = TraceIdRatioBasedSampler::new(0.5)  // 50%采样率
  
  let tracer_provider = TracerProvider::with_sampler(TracerProvider::default(), always_on_sampler)
  let tracer = TracerProvider::get_tracer(tracer_provider, "sampling-test")
  
  // 测试AlwaysOn采样器
  let always_on_span = Tracer::start_span(tracer, "always-on.span")
  let always_on_context = Span::span_context(always_on_span)
  assert_true(SpanContext::is_sampled(always_on_context))
  Span::end(always_on_span)
  
  // 测试AlwaysOff采样器
  let tracer_provider_off = TracerProvider::with_sampler(TracerProvider::default(), always_off_sampler)
  let tracer_off = TracerProvider::get_tracer(tracer_provider_off, "sampling-test-off")
  
  let always_off_span = Tracer::start_span(tracer_off, "always-off.span")
  let always_off_context = Span::span_context(always_off_span)
  assert_false(SpanContext::is_sampled(always_off_context))
  Span::end(always_off_span)
  
  // 测试TraceIdRatioBased采样器
  let tracer_provider_ratio = TracerProvider::with_sampler(TracerProvider::default(), trace_id_ratio_sampler)
  let tracer_ratio = TracerProvider::get_tracer(tracer_provider_ratio, "sampling-test-ratio")
  
  let sampled_count = 0
  let total_spans = 100
  
  for i in 1..=total_spans {
    let span = Tracer::start_span(tracer_ratio, "ratio.span-" + i.to_string())
    let context = Span::span_context(span)
    if SpanContext::is_sampled(context) {
      sampled_count = sampled_count + 1
    }
    Span::end(span)
  }
  
  // 验证采样率大约为50%（允许一定的误差）
  let sampling_ratio = sampled_count.to_double() / total_spans.to_double()
  assert_true(sampling_ratio > 0.3 && sampling_ratio < 0.7)
  
  // 测试基于属性的采样决策
  let attribute_sampler = AttributeBasedSampler::new("priority", "high")
  let tracer_provider_attr = TracerProvider::with_sampler(TracerProvider::default(), attribute_sampler)
  let tracer_attr = TracerProvider::get_tracer(tracer_provider_attr, "sampling-test-attr")
  
  // 创建带有priority=high属性的span（应该被采样）
  let high_priority_span = Tracer::start_span(tracer_attr, "high.priority.span", Some([
    ("priority", StringValue("high"))
  ]))
  let high_priority_context = Span::span_context(high_priority_span)
  assert_true(SpanContext::is_sampled(high_priority_context))
  Span::end(high_priority_span)
  
  // 创建带有priority=low属性的span（不应该被采样）
  let low_priority_span = Tracer::start_span(tracer_attr, "low.priority.span", Some([
    ("priority", StringValue("low"))
  ]))
  let low_priority_context = Span::span_context(low_priority_span)
  assert_false(SpanContext::is_sampled(low_priority_context))
  Span::end(low_priority_span)
}

// Test 7: 时间窗口和时序数据测试
test "时间窗口和时序数据测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "time-series-test")
  
  // 创建时序数据收集器
  let time_series_collector = TimeSeriesCollector::new()
  
  // 创建直方图度量
  let response_time_histogram = Meter::create_histogram(meter, "response.time")
  
  // 模拟不同时间点的数据
  let base_time = Clock::now_unix_nanos(Clock::system())
  
  for i in 1..=60 {
    // 模拟每秒一个数据点，持续60秒
    let timestamp = base_time + (i * 1000000000L)  // 每秒增加
    let value = 100.0 + (i % 20).to_double() * 10.0  // 100-300之间的值
    
    // 记录带时间戳的度量
    TimeSeriesCollector::record_with_timestamp(time_series_collector, response_time_histogram, value, timestamp)
  }
  
  // 测试时间窗口查询
  let window_start = base_time + (10 * 1000000000L)  // 10秒后
  let window_end = base_time + (50 * 1000000000L)   // 50秒后
  
  let window_data = TimeSeriesCollector::get_data_in_window(time_series_collector, window_start, window_end)
  
  // 验证窗口内数据点数量（应该包含11到50秒的数据，共40个点）
  assert_eq(window_data.length(), 40)
  
  // 测试时间聚合
  let aggregated_data = TimeSeriesCollector::aggregate_by_time_window(
    time_series_collector, 
    base_time, 
    base_time + (60 * 1000000000L), 
    10 * 1000000000L  // 10秒窗口
  )
  
  // 验证聚合结果（应该有6个10秒窗口）
  assert_eq(aggregated_data.length(), 6)
  
  // 验证每个窗口的统计数据
  for window in aggregated_data {
    let stats = TimeSeriesWindow::statistics(window)
    assert_true(TimeSeriesStats::count(stats) > 0)
    assert_true(TimeSeriesStats::average(stats) >= 100.0)
    assert_true(TimeSeriesStats::average(stats) <= 300.0)
  }
  
  // 测试时间序列预测
  let prediction = TimeSeriesCollector::predict_next_values(
    time_series_collector, 
    "response.time", 
    5  // 预测接下来5个值
  )
  
  // 验证预测结果
  assert_eq(prediction.length(), 5)
  for predicted_value in prediction {
    assert_true(predicted_value >= 100.0)
    assert_true(predicted_value <= 300.0)
  }
}

// Test 8: 遥测数据导出和批处理测试
test "遥测数据导出和批处理测试" {
  // 创建内存导出器
  let memory_exporter = MemorySpanExporter::new()
  
  // 创建带导出器的追踪提供者
  let tracer_provider = TracerProvider::with_exporter(TracerProvider::default(), memory_exporter)
  let tracer = TracerProvider::get_tracer(tracer_provider, "export-test")
  
  // 创建多个span
  let spans = []
  for i in 1..=50 {
    let span = Tracer::start_span(tracer, "export.span-" + i.to_string(), Some([
      ("span.index", IntValue(i)),
      ("batch.id", StringValue("batch-1"))
    ]))
    
    // 添加事件
    Span::add_event(span, "span.event", Some([
      ("event.index", IntValue(i)),
      ("event.data", StringValue("test-data-" + i.to_string()))
    ]))
    
    spans.push(span)
  }
  
  // 结束所有span
  for span in spans {
    Span::end(span)
  }
  
  // 强制导出
  TracerProvider::force_flush(tracer_provider)
  
  // 验证导出的span数量
  let exported_spans = MemorySpanExporter::get_exported_spans(memory_exporter)
  assert_eq(exported_spans.length(), 50)
  
  // 测试批处理配置
  let batch_exporter = BatchSpanExporter::new(memory_exporter, 10, 5000)  // 10个span一批，5秒超时
  let tracer_provider_batch = TracerProvider::with_exporter(TracerProvider::default(), batch_exporter)
  let tracer_batch = TracerProvider::get_tracer(tracer_provider_batch, "batch-export-test")
  
  // 创建25个span（应该分成3批：10+10+5）
  let batch_spans = []
  for i in 1..=25 {
    let span = Tracer::start_span(tracer_batch, "batch.span-" + i.to_string(), Some([
      ("span.index", IntValue(i)),
      ("batch.id", StringValue("batch-2"))
    ]))
    batch_spans.push(span)
  }
  
  // 结束所有span
  for span in batch_spans {
    Span::end(span)
  }
  
  // 强制导出
  TracerProvider::force_flush(tracer_provider_batch)
  
  // 验证批处理导出
  let batch_exported_spans = MemorySpanExporter::get_exported_spans(memory_exporter)
  assert_eq(batch_exported_spans.length(), 75)  // 50 + 25
  
  // 测试导出格式
  let exported_data = SpanExporter::export_to_json(memory_exporter)
  assert_true(exported_data.length() > 0)
  
  // 验证JSON格式有效性
  assert_true(exported_data.contains("\"spans\""))
  assert_true(exported_data.contains("\"traceId\""))
  assert_true(exported_data.contains("\"spanId\""))
  assert_true(exported_data.contains("\"name\""))
}

// Test 9: 内存使用和垃圾回收测试
test "内存使用和垃圾回收测试" {
  // 记录初始内存使用
  let initial_memory = MemoryMonitor::get_used_memory()
  
  // 创建大量对象
  let large_objects = []
  
  // 创建大量span
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "memory-test")
  
  let spans = []
  for i in 1..=1000 {
    let span = Tracer::start_span(tracer, "memory.span-" + i.to_string(), Some([
      ("large.data", StringValue("x".repeat(1000))),  // 1KB字符串
      ("index", IntValue(i))
    ]))
    
    // 添加大量事件
    for j in 1..=10 {
      Span::add_event(span, "memory.event-" + j.to_string(), Some([
        ("event.data", StringValue("y".repeat(500))),  // 500字节字符串
        ("event.index", IntValue(j))
      ]))
    }
    
    spans.push(span)
  }
  
  // 创建大量属性
  let attributes_list = []
  for i in 1..=1000 {
    let attrs = Attributes::new()
    for j in 1..=100 {
      Attributes::set(attrs, "attr." + j.to_string(), StringValue("value-" + i.to_string() + "-" + j.to_string()))
    }
    attributes_list.push(attrs)
  }
  
  // 记录峰值内存使用
  let peak_memory = MemoryMonitor::get_used_memory()
  
  // 结束所有span
  for span in spans {
    Span::end(span)
  }
  
  // 清空引用
  spans = []
  attributes_list = []
  
  // 强制垃圾回收
  MemoryMonitor::force_gc()
  
  // 记录GC后内存使用
  let gc_memory = MemoryMonitor::get_used_memory()
  
  // 验证内存使用情况
  assert_true(peak_memory > initial_memory)  // 峰值内存应该大于初始内存
  assert_true(gc_memory < peak_memory)       // GC后内存应该小于峰值内存
  
  // 验证内存泄漏不明显
  let memory_increase = gc_memory - initial_memory
  let acceptable_increase = 50 * 1024 * 1024  // 50MB的合理增长
  assert_true(memory_increase < acceptable_increase)
  
  // 测试内存池
  let memory_pool = MemoryPool::new(1024 * 1024)  // 1MB内存池
  
  // 从内存池分配对象
  let pooled_objects = []
  for i in 1..=100 {
    let obj = MemoryPool::allocate(memory_pool, 10240)  // 10KB对象
    pooled_objects.push(obj)
  }
  
  // 验证内存池状态
  assert_eq(MemoryPool::allocated_count(memory_pool), 100)
  assert_true(MemoryPool::used_memory(memory_pool) <= 1024 * 1024)
  
  // 释放对象
  for obj in pooled_objects {
    MemoryPool::deallocate(memory_pool, obj)
  }
  
  // 验证释放后的状态
  assert_eq(MemoryPool::allocated_count(memory_pool), 0)
  assert_eq(MemoryPool::used_memory(memory_pool), 0)
}

// Test 10: 配置管理和动态更新测试
test "配置管理和动态更新测试" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new()
  
  // 设置初始配置
  ConfigurationManager::set(config_manager, "service.name", "test-service")
  ConfigurationManager::set(config_manager, "service.version", "1.0.0")
  ConfigurationManager::set(config_manager, "sampling.probability", "0.1")
  ConfigurationManager::set(config_manager, "export.batch_size", "100")
  ConfigurationManager::set(config_manager, "log.level", "INFO")
  
  // 验证初始配置
  assert_eq(ConfigurationManager::get_string(config_manager, "service.name"), "test-service")
  assert_eq(ConfigurationManager::get_string(config_manager, "service.version"), "1.0.0")
  assert_eq(ConfigurationManager::get_float(config_manager, "sampling.probability"), 0.1)
  assert_eq(ConfigurationManager::get_int(config_manager, "export.batch_size"), 100)
  assert_eq(ConfigurationManager::get_string(config_manager, "log.level"), "INFO")
  
  // 测试配置变更监听器
  let mut change_count = 0
  let listener = ConfigurationChangeListener::new(config_manager, { |key, old_value, new_value|
    change_count = change_count + 1
  })
  
  // 动态更新配置
  ConfigurationManager::set(config_manager, "service.version", "1.1.0")
  ConfigurationManager::set(config_manager, "sampling.probability", "0.2")
  ConfigurationManager::set(config_manager, "export.batch_size", "200")
  
  // 验证配置更新
  assert_eq(ConfigurationManager::get_string(config_manager, "service.version"), "1.1.0")
  assert_eq(ConfigurationManager::get_float(config_manager, "sampling.probability"), 0.2)
  assert_eq(ConfigurationManager::get_int(config_manager, "export.batch_size"), 200)
  
  // 验证监听器被调用
  assert_eq(change_count, 3)
  
  // 测试配置持久化
  let config_file = "/tmp/test_config.json"
  ConfigurationManager::save_to_file(config_manager, config_file)
  
  // 创建新的配置管理器并加载配置
  let new_config_manager = ConfigurationManager::new()
  ConfigurationManager::load_from_file(new_config_manager, config_file)
  
  // 验证加载的配置
  assert_eq(ConfigurationManager::get_string(new_config_manager, "service.name"), "test-service")
  assert_eq(ConfigurationManager::get_string(new_config_manager, "service.version"), "1.1.0")
  assert_eq(ConfigurationManager::get_float(new_config_manager, "sampling.probability"), 0.2)
  assert_eq(ConfigurationManager::get_int(new_config_manager, "export.batch_size"), 200)
  assert_eq(ConfigurationManager::get_string(new_config_manager, "log.level"), "INFO")
  
  // 测试配置验证
  let validation_rules = [
    ("service.name", ConfigurationRule::required()),
    ("service.version", ConfigurationRule::semver()),
    ("sampling.probability", ConfigurationRule::range(0.0, 1.0)),
    ("export.batch_size", ConfigurationRule::min(1))
  ]
  
  ConfigurationManager::add_validation_rules(config_manager, validation_rules)
  
  // 测试有效配置
  assert_true(ConfigurationManager::validate(config_manager))
  
  // 测试无效配置
  ConfigurationManager::set(config_manager, "sampling.probability", "1.5")  // 超出范围
  assert_false(ConfigurationManager::validate(config_manager))
  
  // 修复配置
  ConfigurationManager::set(config_manager, "sampling.probability", "0.5")
  assert_true(ConfigurationManager::validate(config_manager))
  
  // 测试配置热更新应用到组件
  let tracer_provider = TracerProvider::with_config(TracerProvider::default(), config_manager)
  let meter_provider = MeterProvider::with_config(MeterProvider::default(), config_manager)
  let logger_provider = LoggerProvider::with_config(LoggerProvider::default(), config_manager)
  
  // 更新采样配置
  ConfigurationManager::set(config_manager, "sampling.probability", "0.8")
  
  // 验证配置已应用到组件
  let tracer = TracerProvider::get_tracer(tracer_provider, "config-test")
  let span = Tracer::start_span(tracer, "config.span")
  let context = Span::span_context(span)
  
  // 由于采样概率为0.8，大部分span应该被采样
  let sampled_count = 0
  for i in 1..=100 {
    let test_span = Tracer::start_span(tracer, "test.span-" + i.to_string())
    let test_context = Span::span_context(test_span)
    if SpanContext::is_sampled(test_context) {
      sampled_count = sampled_count + 1
    }
    Span::end(test_span)
  }
  
  // 验证采样率大约为80%
  let sampling_ratio = sampled_count.to_double() / 100.0
  assert_true(sampling_ratio > 0.7 && sampling_ratio < 0.9)
  
  Span::end(span)
}