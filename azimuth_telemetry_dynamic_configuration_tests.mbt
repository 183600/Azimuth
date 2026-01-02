// Azimuth 遥测动态配置测试用例
// 专注于遥测系统运行时配置的动态更新和热重载能力

// 测试1: 采样率动态调整测试
test "采样率动态调整测试" {
  let config_manager = ConfigurationManager::new()
  let tracer_provider = TracerProvider::with_config_manager(config_manager)
  let tracer = TracerProvider::get_tracer(tracer_provider, "dynamic.sampling.test")
  
  // 初始配置：1%采样率
  ConfigurationManager::set_sampling_rate(config_manager, 0.01)
  let initial_config = ConfigurationManager::get_current_config(config_manager)
  assert_eq(Config::sampling_rate(initial_config), 0.01)
  
  // 生成追踪数据验证初始采样率
  let initial_sampled = Array::empty()
  for i = 0; i < 1000; i = i + 1 {
    let span = Tracer::start_span(tracer, "initial.sampling.test." + i.to_string())
    if Span::is_sampled(span) {
      Array::push(initial_sampled, span)
    }
    Span::end(span)
  }
  
  let initial_sample_ratio = initial_sampled.length() / 1000.0
  assert_true(initial_sample_ratio > 0.005 && initial_sample_ratio < 0.015) // 约1%
  
  // 动态调整采样率到50%
  ConfigurationManager::update_sampling_rate(config_manager, 0.5)
  let updated_config = ConfigurationManager::get_current_config(config_manager)
  assert_eq(Config::sampling_rate(updated_config), 0.5)
  
  // 验证配置变更立即生效
  let updated_sampled = Array::empty()
  for i = 0; i < 1000; i = i + 1 {
    let span = Tracer::start_span(tracer, "updated.sampling.test." + i.to_string())
    if Span::is_sampled(span) {
      Array::push(updated_sampled, span)
    }
    Span::end(span)
  }
  
  let updated_sample_ratio = updated_sampled.length() / 1000.0
  assert_true(updated_sample_ratio > 0.45 && updated_sample_ratio < 0.55) // 约50%
  
  // 验证配置变更历史
  let change_history = ConfigurationManager::get_config_history(config_manager)
  assert_eq(change_history.length(), 2)
  assert_eq(ConfigChange::old_value(change_history[1]), 0.01)
  assert_eq(ConfigChange::new_value(change_history[1]), 0.5)
}

// 测试2: 指标收集器动态配置测试
test "指标收集器动态配置测试" {
  let meter_provider = MeterProvider::new()
  let config_manager = MeterProvider::get_config_manager(meter_provider)
  let meter = MeterProvider::get_meter(meter_provider, "dynamic.metric.test")
  
  // 初始配置：收集所有指标
  ConfigurationManager::set_metric_filter(config_manager, MetricFilter::accept_all())
  let counter1 = Meter::create_counter(meter, "test.counter.1")
  let counter2 = Meter::create_counter(meter, "test.counter.2")
  let gauge1 = Meter::create_gauge(meter, "test.gauge.1")
  
  // 生成指标数据
  Counter::add(counter1, 100.0)
  Counter::add(counter2, 200.0)
  Gauge::set(gauge1, 50.0)
  
  // 验证所有指标都被收集
  let initial_metrics = MeterProvider::get_collected_metrics(meter_provider)
  assert_eq(initial_metrics.length(), 3)
  
  // 动态调整：只收集counter类型指标
  let counter_filter = MetricFilter::by_type("counter")
  ConfigurationManager::update_metric_filter(config_manager, counter_filter)
  
  // 生成新指标
  let counter3 = Meter::create_counter(meter, "test.counter.3")
  let gauge2 = Meter::create_gauge(meter, "test.gauge.2")
  
  Counter::add(counter3, 300.0)
  Gauge::set(gauge2, 75.0)
  
  // 验证只有counter类型指标被收集
  let updated_metrics = MeterProvider::get_collected_metrics(meter_provider)
  let counter_metrics = Array::filter(updated_metrics, fn(m) { Metric::type(m) == "counter" })
  let gauge_metrics = Array::filter(updated_metrics, fn(m) { Metric::type(m) == "gauge" })
  
  assert_eq(counter_metrics.length(), 3) // counter1, counter2, counter3
  assert_eq(gauge_metrics.length(), 0) // gauge类型被过滤
  
  // 动态调整：按名称模式过滤
  let pattern_filter = MetricFilter::by_pattern("test.counter.*")
  ConfigurationManager::update_metric_filter(config_manager, pattern_filter)
  
  // 验证模式过滤效果
  let pattern_metrics = MeterProvider::get_collected_metrics(meter_provider)
  for metric in pattern_metrics {
    assert_true(String::starts_with(Metric::name(metric), "test.counter."))
  }
}

// 测试3: 导出器动态切换测试
test "导出器动态切换测试" {
  let telemetry_system = TelemetrySystem::new()
  let config_manager = TelemetrySystem::get_config_manager(telemetry_system)
  
  // 初始配置：使用内存导出器
  let memory_exporter = MemoryExporter::new()
  ConfigurationManager::set_exporter(config_manager, memory_exporter)
  
  let tracer = TelemetrySystem::get_tracer(telemetry_system, "exporter.switch.test")
  let span = Tracer::start_span(tracer, "memory.exporter.test")
  Span::set_attribute(span, "exporter.type", StringValue("memory"))
  Span::end(span)
  
  // 验证内存导出器接收数据
  let memory_spans = MemoryExporter::get_exported_spans(memory_exporter)
  assert_eq(memory_spans.length(), 1)
  assert_eq(Span::get_attribute(memory_spans[0], "exporter.type"), Some(StringValue("memory")))
  
  // 动态切换到文件导出器
  let file_exporter = FileExporter::new("/tmp/telemetry_export.json")
  ConfigurationManager::switch_exporter(config_manager, file_exporter)
  
  let span2 = Tracer::start_span(tracer, "file.exporter.test")
  Span::set_attribute(span2, "exporter.type", StringValue("file"))
  Span::end(span2)
  
  // 验证文件导出器接收新数据
  Thread::sleep(Duration::from_ms(100)) // 等待文件写入
  let file_spans = FileExporter::get_exported_spans(file_exporter)
  assert_eq(file_spans.length(), 1)
  assert_eq(Span::get_attribute(file_spans[0], "exporter.type"), Some(StringValue("file")))
  
  // 验证内存导出器不再接收新数据
  let updated_memory_spans = MemoryExporter::get_exported_spans(memory_exporter)
  assert_eq(updated_memory_spans.length(), 1) // 仍然是原来的1个
  
  // 动态切换到网络导出器
  let network_exporter = NetworkExporter::new("http://telemetry.collector:4317")
  ConfigurationManager::switch_exporter(config_manager, network_exporter)
  
  // 验证导出器切换历史
  let exporter_history = ConfigurationManager::get_exporter_history(config_manager)
  assert_eq(exporter_history.length(), 3)
  assert_eq(ExporterHistory::exporter_type(exporter_history[0]), "memory")
  assert_eq(ExporterHistory::exporter_type(exporter_history[1]), "file")
  assert_eq(ExporterHistory::exporter_type(exporter_history[2]), "network")
}

// 测试4: 批处理配置动态调整测试
test "批处理配置动态调整测试" {
  let batch_processor = BatchProcessor::new()
  let config_manager = BatchProcessor::get_config_manager(batch_processor)
  
  // 初始配置：小批次，短延迟
  ConfigurationManager::set_batch_config(config_manager, {
    "max_batch_size" => IntValue(10),
    "max_delay_ms" => IntValue(100),
    "max_export_timeout_ms" => IntValue(5000)
  })
  
  let test_data = Array::empty()
  for i = 0; i < 25; i = i + 1 {
    let metric = TelemetryMetric::new_counter("batch.test.metric", 1.0, Attributes::from([("index", IntValue(i))]))
    Array::push(test_data, metric)
  }
  
  // 添加数据到批处理器
  for metric in test_data {
    BatchProcessor::add(batch_processor, metric)
  }
  
  // 等待批次处理完成
  Thread::sleep(Duration::from_ms(200))
  
  // 验证批次处理结果（应该是3个批次：10, 10, 5）
  let batch_results = BatchProcessor::get_batch_results(batch_processor)
  assert_eq(batch_results.length(), 3)
  assert_eq(BatchResult::size(batch_results[0]), 10)
  assert_eq(BatchResult::size(batch_results[1]), 10)
  assert_eq(BatchResult::size(batch_results[2]), 5)
  
  // 动态调整：大批次，长延迟
  ConfigurationManager::update_batch_config(config_manager, {
    "max_batch_size" => IntValue(50),
    "max_delay_ms" => IntValue(1000),
    "max_export_timeout_ms" => IntValue(10000)
  })
  
  // 添加更多测试数据
  let additional_data = Array::empty()
  for i = 25; i < 75; i = i + 1 {
    let metric = TelemetryMetric::new_counter("batch.test.metric", 1.0, Attributes::from([("index", IntValue(i))]))
    Array::push(additional_data, metric)
  }
  
  for metric in additional_data {
    BatchProcessor::add(batch_processor, metric)
  }
  
  // 等待批次处理完成
  Thread::sleep(Duration::from_ms(200))
  
  // 验证新的批次处理结果（应该是1个批次：50）
  let new_batch_results = BatchProcessor::get_recent_batch_results(batch_processor)
  assert_eq(new_batch_results.length(), 1)
  assert_eq(BatchResult::size(new_batch_results[0]), 50)
  
  // 验证配置变更统计
  let config_stats = ConfigurationManager::get_config_stats(config_manager)
  assert_eq(ConfigStats::batch_config_changes(config_stats), 2)
  assert_true(ConfigStats::avg_batch_size(config_stats) > 20) // 平均批次大小应该增加
}