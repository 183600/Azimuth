// Azimuth New Feature Test Suite
// 新功能测试用例，涵盖遥测系统的各个方面

// 测试1: 遥测数据序列化与反序列化
pub test "遥测数据序列化与反序列化测试" {
  // 创建Span并设置属性
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "serialization-test")
  let span = azimuth::Tracer::start_span(tracer, "serialization-span")
  
  // 设置各种类型的属性
  azimuth::Span::set_attribute(span, "string.attr", azimuth::StringValue("test-value"))
  azimuth::Span::set_attribute(span, "int.attr", azimuth::IntValue(42))
  azimuth::Span::set_attribute(span, "bool.attr", azimuth::BoolValue(true))
  azimuth::Span::set_attribute(span, "double.attr", azimuth::DoubleValue(3.14))
  
  // 添加事件
  azimuth::Span::add_event(span, "test-event", Some([("event.key", azimuth::StringValue("event-value"))]))
  
  // 序列化Span数据
  let serialized = azimuth::Span::serialize(span)
  assert_true(serialized.length() > 0)
  
  // 反序列化Span数据
  let deserialized_span = azimuth::Span::deserialize(serialized)
  assert_true(deserialized_span != span) // 确保是新的Span实例
  
  // 验证反序列化的数据
  assert_eq(azimuth::Span::name(deserialized_span), azimuth::Span::name(span))
  
  // 结束原始Span
  azimuth::Span::end(span)
  azimuth::Span::end(deserialized_span)
}

// 测试2: 跨服务上下文传播
pub test "跨服务上下文传播测试" {
  // 创建父服务上下文
  let parent_tracer_provider = azimuth::TracerProvider::default()
  let parent_tracer = azimuth::TracerProvider::get_tracer(parent_tracer_provider, "parent-service")
  let parent_span = azimuth::Tracer::start_span(parent_tracer, "parent-operation")
  
  // 设置父Span属性
  azimuth::Span::set_attribute(parent_span, "service.name", azimuth::StringValue("parent-service"))
  azimuth::Span::set_attribute(parent_span, "user.id", azimuth::StringValue("user-123"))
  
  // 提取上下文
  let context = azimuth::Span::get_context(parent_span)
  let serialized_context = azimuth::Context::serialize(context)
  
  // 模拟跨服务传播
  let child_tracer_provider = azimuth::TracerProvider::default()
  let child_tracer = azimuth::TracerProvider::get_tracer(child_tracer_provider, "child-service")
  
  // 反序列化上下文
  let propagated_context = azimuth::Context::deserialize(serialized_context)
  let child_span = azimuth::Tracer::start_span_with_context(child_tracer, "child-operation", propagated_context)
  
  // 验证父子关系
  assert_eq(azimuth::Span::parent_id(child_span), azimuth::Span::span_id(parent_span))
  assert_eq(azimuth::Span::trace_id(child_span), azimuth::Span::trace_id(parent_span))
  
  // 设置子Span属性
  azimuth::Span::set_attribute(child_span, "service.name", azimuth::StringValue("child-service"))
  azimuth::Span::set_attribute(child_span, "operation.type", azimuth::StringValue("database-query"))
  
  // 结束Span
  azimuth::Span::end(child_span)
  azimuth::Span::end(parent_span)
}

// 测试3: 度量数据聚合
pub test "度量数据聚合测试" {
  // 创建Meter
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "aggregation-test")
  
  // 创建Counter并记录数据
  let counter = azimuth::Meter::create_counter(meter, "request.count", "请求总数", "count")
  azimuth::Counter::add(counter, 10.0, [("endpoint", azimuth::StringValue("/api/users"))])
  azimuth::Counter::add(counter, 5.0, [("endpoint", azimuth::StringValue("/api/products"))])
  azimuth::Counter::add(counter, 15.0, [("endpoint", azimuth::StringValue("/api/users"))])
  
  // 创建Histogram并记录数据
  let histogram = azimuth::Meter::create_histogram(meter, "response.time", "响应时间", "ms")
  azimuth::Histogram::record(histogram, 100.0, [("status", azimuth::StringValue("200"))])
  azimuth::Histogram::record(histogram, 200.0, [("status", azimuth::StringValue("200"))])
  azimuth::Histogram::record(histogram, 500.0, [("status", azimuth::StringValue("500"))])
  azimuth::Histogram::record(histogram, 150.0, [("status", azimuth::StringValue("200"))])
  
  // 聚合数据
  let counter_data = azimuth::Counter::get_data(counter)
  let histogram_data = azimuth::Histogram::get_data(histogram)
  
  // 验证Counter聚合结果
  assert_eq(counter_data.length(), 2) // 两个不同的endpoint
  for data_point in counter_data {
    if data_point.attributes["endpoint"] == azimuth::StringValue("/api/users") {
      assert_eq(data_point.value, 25.0) // 10 + 15
    } else if data_point.attributes["endpoint"] == azimuth::StringValue("/api/products") {
      assert_eq(data_point.value, 5.0)
    }
  }
  
  // 验证Histogram聚合结果
  assert_eq(histogram_data.length(), 2) // 两个不同的status
  for data_point in histogram_data {
    if data_point.attributes["status"] == azimuth::StringValue("200") {
      assert_eq(data_point.count, 3) // 3个200状态码的响应
      assert_eq(data_point.sum, 450.0) // 100 + 200 + 150
    } else if data_point.attributes["status"] == azimuth::StringValue("500") {
      assert_eq(data_point.count, 1) // 1个500状态码的响应
      assert_eq(data_point.sum, 500.0)
    }
  }
}

// 测试4: 日志记录与关联
pub test "日志记录与关联测试" {
  // 创建Logger
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "correlation-test")
  
  // 创建Span用于关联
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "trace-test")
  let span = azimuth::Tracer::start_span(tracer, "operation-with-logs")
  
  // 获取Span上下文
  let context = azimuth::Span::get_context(span)
  let trace_id = azimuth::Context::trace_id(context)
  let span_id = azimuth::Context::span_id(context)
  
  // 记录关联的日志
  azimuth::Logger::emit_log(logger, azimuth::INFO, "操作开始", [
    ("trace.id", azimuth::StringValue(trace_id)),
    ("span.id", azimuth::StringValue(span_id)),
    ("operation", azimuth::StringValue("database-query"))
  ])
  
  // 在Span中添加事件
  azimuth::Span::add_event(span, "database-query-start", Some([
    ("query", azimuth::StringValue("SELECT * FROM users")),
    ("timestamp", azimuth::StringValue("2023-01-01T00:00:00Z"))
  ]))
  
  // 记录更多日志
  azimuth::Logger::emit_log(logger, azimuth::DEBUG, "查询执行中", [
    ("trace.id", azimuth::StringValue(trace_id)),
    ("span.id", azimuth::StringValue(span_id)),
    ("query.time", azimuth::IntValue(150))
  ])
  
  // 记录操作完成
  azimuth::Logger::emit_log(logger, azimuth::INFO, "操作完成", [
    ("trace.id", azimuth::StringValue(trace_id)),
    ("span.id", azimuth::StringValue(span_id)),
    ("result.count", azimuth::IntValue(42))
  ])
  
  // 结束Span
  azimuth::Span::end(span)
  
  // 验证日志与Span的关联
  let logs = azimuth::Logger::get_logs(logger)
  assert_true(logs.length() >= 3)
  
  for log in logs {
    assert_eq(log.attributes["trace.id"], azimuth::StringValue(trace_id))
    assert_eq(log.attributes["span.id"], azimuth::StringValue(span_id))
  }
}

// 测试5: 性能基准测试
pub test "性能基准测试" {
  // 创建Tracer
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "performance-test")
  
  // 测试Span创建和结束的性能
  let start_time = azimuth::time::current_timestamp()
  let span_count = 1000
  
  for i in 0..span_count {
    let span = azimuth::Tracer::start_span(tracer, "perf-span-" + i.to_string())
    azimuth::Span::set_attribute(span, "index", azimuth::IntValue(i))
    azimuth::Span::add_event(span, "test-event", None)
    azimuth::Span::end(span)
  }
  
  let end_time = azimuth::time::current_timestamp()
  let duration = end_time - start_time
  
  // 验证性能指标（每个Span操作应该在合理时间内完成）
  let avg_duration_per_span = duration.to_float() / span_count.to_float()
  assert_true(avg_duration_per_span < 1.0) // 每个Span平均处理时间应小于1ms
  
  // 测试度量记录的性能
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "perf-meter")
  let counter = azimuth::Meter::create_counter(meter, "perf.counter", "性能测试计数器", "count")
  
  let metric_start_time = azimuth::time::current_timestamp()
  let metric_count = 10000
  
  for i in 0..metric_count {
    azimuth::Counter::add(counter, 1.0, [("batch", azimuth::StringValue((i / 100).to_string()))])
  }
  
  let metric_end_time = azimuth::time::current_timestamp()
  let metric_duration = metric_end_time - metric_start_time
  let avg_duration_per_metric = metric_duration.to_float() / metric_count.to_float()
  assert_true(avg_duration_per_metric < 0.1) // 每个度量记录平均时间应小于0.1ms
}

// 测试6: 错误处理与恢复
pub test "错误处理与恢复测试" {
  // 创建Tracer
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "error-handling-test")
  
  // 测试正常操作
  let normal_span = azimuth::Tracer::start_span(tracer, "normal-operation")
  azimuth::Span::set_status(normal_span, azimuth::Ok, "操作成功")
  azimuth::Span::end(normal_span)
  
  // 测试错误处理
  let error_span = azimuth::Tracer::start_span(tracer, "error-operation")
  
  // 模拟错误情况
  azimuth::Span::set_status(error_span, azimuth::Error, "模拟错误")
  azimuth::Span::record_exception(error_span, "模拟异常", Some([
    ("error.code", azimuth::StringValue("E001")),
    ("error.message", azimuth::StringValue("操作失败"))
  ]))
  
  // 添加错误事件
  azimuth::Span::add_event(error_span, "error-occurred", Some([
    ("error.type", azimuth::StringValue("ValidationError")),
    ("recoverable", azimuth::BoolValue(true))
  ]))
  
  // 结束错误Span
  azimuth::Span::end(error_span)
  
  // 测试恢复操作
  let recovery_span = azimuth::Tracer::start_span(tracer, "recovery-operation")
  
  // 设置恢复上下文
  azimuth::Span::set_attribute(recovery_span, "recovery.from", azimuth::StringValue("error-operation"))
  azimuth::Span::set_attribute(recovery_span, "recovery.strategy", azimuth::StringValue("retry"))
  
  // 模拟恢复成功
  azimuth::Span::set_status(recovery_span, azimuth::Ok, "恢复成功")
  azimuth::Span::end(recovery_span)
  
  // 验证错误和恢复状态
  assert_eq(azimuth::Span::status(error_span), azimuth::Error)
  assert_eq(azimuth::Span::status(recovery_span), azimuth::Ok)
}

// 测试7: 配置动态更新
pub test "配置动态更新测试" {
  // 创建初始配置
  let initial_config = azimuth::Configuration::new()
  azimuth::Configuration::set_sampling_rate(initial_config, 0.1) // 10%采样率
  azimuth::Configuration::set_max_attributes(initial_config, 128)
  azimuth::Configuration::set_max_events(initial_config, 100)
  
  // 创建使用初始配置的TracerProvider
  let tracer_provider = azimuth::TracerProvider::with_config(initial_config)
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "config-test")
  
  // 创建Span并验证配置应用
  let span1 = azimuth::Tracer::start_span(tracer, "config-test-span-1")
  let config1 = azimuth::Span::get_configuration(span1)
  assert_eq(azimuth::Configuration::sampling_rate(config1), 0.1)
  assert_eq(azimuth::Configuration::max_attributes(config1), 128)
  assert_eq(azimuth::Configuration::max_events(config1), 100)
  azimuth::Span::end(span1)
  
  // 动态更新配置
  azimuth::Configuration::set_sampling_rate(initial_config, 0.5) // 50%采样率
  azimuth::Configuration::set_max_attributes(initial_config, 256)
  azimuth::Configuration::set_max_events(initial_config, 200)
  
  // 通知TracerProvider配置更新
  azimuth::TracerProvider::update_configuration(tracer_provider, initial_config)
  
  // 创建新Span并验证配置更新
  let span2 = azimuth::Tracer::start_span(tracer, "config-test-span-2")
  let config2 = azimuth::Span::get_configuration(span2)
  assert_eq(azimuth::Configuration::sampling_rate(config2), 0.5)
  assert_eq(azimuth::Configuration::max_attributes(config2), 256)
  assert_eq(azimuth::Configuration::max_events(config2), 200)
  azimuth::Span::end(span2)
  
  // 验证配置更新不影响已存在的Span
  let span3 = azimuth::Tracer::start_span(tracer, "config-test-span-3")
  let config3 = azimuth::Span::get_configuration(span3)
  assert_eq(azimuth::Configuration::sampling_rate(config3), 0.5)
  azimuth::Span::end(span3)
}

// 测试8: 资源管理
pub test "资源管理测试" {
  // 创建基础资源
  let base_resource = azimuth::Resource::new()
  azimuth::Resource::set_attribute(base_resource, "service.name", azimuth::StringValue("test-service"))
  azimuth::Resource::set_attribute(base_resource, "service.version", azimuth::StringValue("1.0.0"))
  azimuth::Resource::set_attribute(base_resource, "host.name", azimuth::StringValue("test-host"))
  
  // 创建使用资源的TracerProvider
  let tracer_provider = azimuth::TracerProvider::with_resource(base_resource)
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "resource-test")
  
  // 创建Span并验证资源属性
  let span = azimuth::Tracer::start_span(tracer, "resource-test-span")
  let span_resource = azimuth::Span::get_resource(span)
  
  // 验证资源属性继承
  assert_eq(azimuth::Resource::get_attribute(span_resource, "service.name"), 
           Some(azimuth::StringValue("test-service")))
  assert_eq(azimuth::Resource::get_attribute(span_resource, "service.version"), 
           Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(span_resource, "host.name"), 
           Some(azimuth::StringValue("test-host")))
  
  // 添加Span特定属性
  azimuth::Span::set_attribute(span, "operation.name", azimuth::StringValue("database-query"))
  azimuth::Span::set_attribute(span, "user.id", azimuth::StringValue("user-123"))
  
  // 验证Span属性不影响资源
  let span_resource_after = azimuth::Span::get_resource(span)
  assert_eq(azimuth::Resource::get_attribute(span_resource_after, "operation.name"), None)
  assert_eq(azimuth::Resource::get_attribute(span_resource_after, "user.id"), None)
  
  // 验证资源合并
  let additional_resource = azimuth::Resource::new()
  azimuth::Resource::set_attribute(additional_resource, "service.version", azimuth::StringValue("2.0.0"))
  azimuth::Resource::set_attribute(additional_resource, "deployment.region", azimuth::StringValue("us-west-1"))
  
  let merged_resource = azimuth::Resource::merge(base_resource, additional_resource)
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), 
           Some(azimuth::StringValue("test-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.version"), 
           Some(azimuth::StringValue("2.0.0"))) // 覆盖
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "host.name"), 
           Some(azimuth::StringValue("test-host")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "deployment.region"), 
           Some(azimuth::StringValue("us-west-1"))) // 新增
  
  azimuth::Span::end(span)
}

// 测试9: 时间序列数据处理
pub test "时间序列数据处理测试" {
  // 创建Meter
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "time-series-test")
  
  // 创建时间序列度量
  let time_series_counter = azimuth::Meter::create_time_series_counter(meter, 
    "requests.over.time", "请求时间序列", "count", azimuth::TimeSeries::minute)
  
  let base_time = azimuth::time::current_timestamp()
  
  // 模拟不同时间点的数据
  for i in 0..60 {
    let timestamp = base_time + azimuth::time::Duration::from_seconds(i)
    azimuth::TimeSeriesCounter::record_at(time_series_counter, 5.0 + i.to_float() * 0.1, timestamp, 
      [("endpoint", azimuth::StringValue("/api/data"))])
  }
  
  // 创建另一个时间序列
  let time_series_gauge = azimuth::Meter::create_time_series_gauge(meter,
    "memory.usage", "内存使用时间序列", "bytes", azimuth::TimeSeries::second)
  
  // 模拟内存使用数据
  for i in 0..30 {
    let timestamp = base_time + azimuth::time::Duration::from_seconds(i * 2)
    let memory_usage = 1000000.0 + (i * 100000).to_float() + (azimuth::math::random() * 50000.0)
    azimuth::TimeSeriesGauge::record_at(time_series_gauge, memory_usage, timestamp,
      [("component", azimuth::StringValue("cache"))])
  }
  
  // 获取时间序列数据
  let counter_series = azimuth::TimeSeriesCounter::get_series(time_series_counter)
  let gauge_series = azimuth::TimeSeriesGauge::get_series(time_series_gauge)
  
  // 验证时间序列数据
  assert_eq(counter_series.length(), 60)
  assert_eq(gauge_series.length(), 30)
  
  // 验证时间序列排序
  for i in 1..counter_series.length() {
    assert_true(counter_series[i].timestamp >= counter_series[i-1].timestamp)
  }
  
  for i in 1..gauge_series.length() {
    assert_true(gauge_series[i].timestamp >= gauge_series[i-1].timestamp)
  }
  
  // 验证时间序列聚合
  let counter_aggregation = azimuth::TimeSeries::aggregate(counter_series, 
    azimuth::TimeSeries::minute, azimuth::Aggregation::sum)
  
  let gauge_aggregation = azimuth::TimeSeries::aggregate(gauge_series,
    azimuth::TimeSeries::minute, azimuth::Aggregation::avg)
  
  assert_true(counter_aggregation.length() > 0)
  assert_true(gauge_aggregation.length() > 0)
}

// 测试10: 平台兼容性测试
pub test "平台兼容性测试" {
  // 获取当前平台信息
  let platform_info = azimuth::platform::get_info()
  let platform_name = azimuth::platform::name(platform_info)
  let platform_version = azimuth::platform::version(platform_info)
  let architecture = azimuth::platform::architecture(platform_info)
  
  // 创建带平台信息的资源
  let resource = azimuth::Resource::new()
  azimuth::Resource::set_attribute(resource, "platform.name", azimuth::StringValue(platform_name))
  azimuth::Resource::set_attribute(resource, "platform.version", azimuth::StringValue(platform_version))
  azimuth::Resource::set_attribute(resource, "platform.architecture", azimuth::StringValue(architecture))
  
  // 创建TracerProvider
  let tracer_provider = azimuth::TracerProvider::with_resource(resource)
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "platform-test")
  
  // 测试平台特定功能
  let span = azimuth::Tracer::start_span(tracer, "platform-compatibility-test")
  
  // 获取平台特定的性能计数器
  let platform_metrics = azimuth::platform::get_metrics()
  for metric in platform_metrics {
    azimuth::Span::set_attribute(span, metric.name, metric.value)
  }
  
  // 测试平台特定的时间精度
  let time_precision = azimuth::platform::time_precision()
  let timestamp = azimuth::time::current_timestamp()
  azimuth::Span::set_attribute(span, "time.precision", azimuth::IntValue(time_precision))
  azimuth::Span::set_attribute(span, "test.timestamp", azimuth::StringValue(timestamp.to_string()))
  
  // 测试平台特定的序列化格式
  let serialization_format = azimuth::platform::preferred_serialization_format()
  azimuth::Span::set_attribute(span, "serialization.format", azimuth::StringValue(serialization_format))
  
  // 序列化Span数据
  let serialized_data = azimuth::Span::serialize_with_format(span, serialization_format)
  assert_true(serialized_data.length() > 0)
  
  // 反序列化并验证
  let deserialized_span = azimuth::Span::deserialize_with_format(serialized_data, serialization_format)
  assert_true(deserialized_span != span)
  
  // 验证平台特定属性
  let deserialized_resource = azimuth::Span::get_resource(deserialized_span)
  assert_eq(azimuth::Resource::get_attribute(deserialized_resource, "platform.name"),
           Some(azimuth::StringValue(platform_name)))
  assert_eq(azimuth::Resource::get_attribute(deserialized_resource, "platform.version"),
           Some(azimuth::StringValue(platform_version)))
  assert_eq(azimuth::Resource::get_attribute(deserialized_resource, "platform.architecture"),
           Some(azimuth::StringValue(architecture)))
  
  azimuth::Span::end(span)
  azimuth::Span::end(deserialized_span)
}