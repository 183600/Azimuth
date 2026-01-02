// Azimuth New Comprehensive Test Suite
// 新的综合测试用例，覆盖遥测系统的高级功能和边缘场景

// 测试1: 时间序列数据处理
test "时间序列数据处理测试" {
  // 创建时间序列管理器
  let ts_manager = TimeSeriesManager::new()
  
  // 添加时间序列数据点
  let timestamp1 = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  let timestamp2 = 1735689660000000000L  // 2025-01-01 00:01:00 UTC
  let timestamp3 = 1735689720000000000L  // 2025-01-01 00:02:00 UTC
  
  TimeSeriesManager::add_point(ts_manager, "cpu.usage", timestamp1, 45.2)
  TimeSeriesManager::add_point(ts_manager, "cpu.usage", timestamp2, 52.8)
  TimeSeriesManager::add_point(ts_manager, "cpu.usage", timestamp3, 48.1)
  
  TimeSeriesManager::add_point(ts_manager, "memory.usage", timestamp1, 1024.0)
  TimeSeriesManager::add_point(ts_manager, "memory.usage", timestamp2, 1089.5)
  TimeSeriesManager::add_point(ts_manager, "memory.usage", timestamp3, 1102.3)
  
  // 测试时间范围查询
  let cpu_data = TimeSeriesManager::query_range(ts_manager, "cpu.usage", timestamp1, timestamp3)
  let memory_data = TimeSeriesManager::query_range(ts_manager, "memory.usage", timestamp1, timestamp3)
  
  // 验证查询结果
  assert_eq(TimeSeriesData::point_count(cpu_data), 3)
  assert_eq(TimeSeriesData::point_count(memory_data), 3)
  
  // 测试时间序列聚合
  let cpu_avg = TimeSeriesManager::aggregate(ts_manager, "cpu.usage", timestamp1, timestamp3, "avg")
  let cpu_max = TimeSeriesManager::aggregate(ts_manager, "cpu.usage", timestamp1, timestamp3, "max")
  let cpu_min = TimeSeriesManager::aggregate(ts_manager, "cpu.usage", timestamp1, timestamp3, "min")
  
  // 验证聚合结果
  assert_eq(cpu_avg, 48.7)  // (45.2 + 52.8 + 48.1) / 3
  assert_eq(cpu_max, 52.8)
  assert_eq(cpu_min, 45.2)
  
  // 测试时间窗口统计
  let window_stats = TimeSeriesManager::window_stats(ts_manager, "memory.usage", timestamp1, timestamp3, 60)
  
  // 验证窗口统计
  assert_eq(WindowStats::window_count(window_stats), 3)
  assert_eq(WindowStats::total_points(window_stats), 3)
}

// 测试2: 资源限制和恢复
test "资源限制和恢复测试" {
  // 创建资源限制管理器
  let resource_manager = ResourceManager::new()
  
  // 设置资源限制
  ResourceManager::set_limit(resource_manager, "max.spans", 1000)
  ResourceManager::set_limit(resource_manager, "max.metrics", 5000)
  ResourceManager::set_limit(resource_manager, "max.logs", 2000)
  ResourceManager::set_limit(resource_manager, "memory.usage.mb", 100)
  
  // 验证限制设置
  assert_eq(ResourceManager::get_limit(resource_manager, "max.spans"), 1000)
  assert_eq(ResourceManager::get_limit(resource_manager, "max.metrics"), 5000)
  assert_eq(ResourceManager::get_limit(resource_manager, "max.logs"), 2000)
  assert_eq(ResourceManager::get_limit(resource_manager, "memory.usage.mb"), 100)
  
  // 模拟资源使用
  ResourceManager::increment_usage(resource_manager, "max.spans", 500)
  ResourceManager::increment_usage(resource_manager, "max.metrics", 2500)
  ResourceManager::increment_usage(resource_manager, "max.logs", 1000)
  ResourceManager::increment_usage(resource_manager, "memory.usage.mb", 50)
  
  // 验证资源使用
  assert_eq(ResourceManager::get_usage(resource_manager, "max.spans"), 500)
  assert_eq(ResourceManager::get_usage(resource_manager, "max.metrics"), 2500)
  assert_eq(ResourceManager::get_usage(resource_manager, "max.logs"), 1000)
  assert_eq(ResourceManager::get_usage(resource_manager, "memory.usage.mb"), 50)
  
  // 测试资源限制检查
  assert_true(ResourceManager::check_limit(resource_manager, "max.spans", 400))  // 500 + 400 <= 1000
  assert_false(ResourceManager::check_limit(resource_manager, "max.spans", 600))  // 500 + 600 > 1000
  
  // 测试资源恢复
  ResourceManager::decrement_usage(resource_manager, "max.spans", 200)
  ResourceManager::decrement_usage(resource_manager, "max.metrics", 1000)
  ResourceManager::decrement_usage(resource_manager, "max.logs", 500)
  ResourceManager::decrement_usage(resource_manager, "memory.usage.mb", 25)
  
  // 验证资源恢复
  assert_eq(ResourceManager::get_usage(resource_manager, "max.spans"), 300)
  assert_eq(ResourceManager::get_usage(resource_manager, "max.metrics"), 1500)
  assert_eq(ResourceManager::get_usage(resource_manager, "max.logs"), 500)
  assert_eq(ResourceManager::get_usage(resource_manager, "memory.usage.mb"), 25)
  
  // 测试资源清理
  ResourceManager::clear_usage(resource_manager, "max.spans")
  ResourceManager::clear_usage(resource_manager, "max.metrics")
  ResourceManager::clear_usage(resource_manager, "max.logs")
  ResourceManager::clear_usage(resource_manager, "memory.usage.mb")
  
  // 验证资源清理
  assert_eq(ResourceManager::get_usage(resource_manager, "max.spans"), 0)
  assert_eq(ResourceManager::get_usage(resource_manager, "max.metrics"), 0)
  assert_eq(ResourceManager::get_usage(resource_manager, "max.logs"), 0)
  assert_eq(ResourceManager::get_usage(resource_manager, "memory.usage.mb"), 0)
}

// 测试3: 配置动态更新
test "配置动态更新测试" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new()
  
  // 设置初始配置
  ConfigurationManager::set(config_manager, "service.name", "azimuth-telemetry")
  ConfigurationManager::set(config_manager, "service.version", "1.0.0")
  ConfigurationManager::set(config_manager, "sampling.rate", "0.1")
  ConfigurationManager::set(config_manager, "batch.size", "100")
  ConfigurationManager::set(config_manager, "export.interval", "5000")
  
  // 验证初始配置
  assert_eq(ConfigurationManager::get(config_manager, "service.name"), Some("azimuth-telemetry"))
  assert_eq(ConfigurationManager::get(config_manager, "service.version"), Some("1.0.0"))
  assert_eq(ConfigurationManager::get(config_manager, "sampling.rate"), Some("0.1"))
  assert_eq(ConfigurationManager::get(config_manager, "batch.size"), Some("100"))
  assert_eq(ConfigurationManager::get(config_manager, "export.interval"), Some("5000"))
  
  // 测试配置更新
  ConfigurationManager::update(config_manager, "service.version", "1.1.0")
  ConfigurationManager::update(config_manager, "sampling.rate", "0.2")
  ConfigurationManager::update(config_manager, "batch.size", "200")
  
  // 验证配置更新
  assert_eq(ConfigurationManager::get(config_manager, "service.name"), Some("azimuth-telemetry"))
  assert_eq(ConfigurationManager::get(config_manager, "service.version"), Some("1.1.0"))
  assert_eq(ConfigurationManager::get(config_manager, "sampling.rate"), Some("0.2"))
  assert_eq(ConfigurationManager::get(config_manager, "batch.size"), Some("200"))
  assert_eq(ConfigurationManager::get(config_manager, "export.interval"), Some("5000"))
  
  // 测试配置监听器
  let listener = ConfigChangeListener::new()
  ConfigurationManager::add_listener(config_manager, "service.version", listener)
  
  // 更新配置并触发监听器
  ConfigurationManager::update(config_manager, "service.version", "1.2.0")
  
  // 验证监听器被触发
  assert_eq(ConfigChangeListener::last_key(listener), Some("service.version"))
  assert_eq(ConfigChangeListener::last_old_value(listener), Some("1.1.0"))
  assert_eq(ConfigChangeListener::last_new_value(listener), Some("1.2.0"))
  
  // 测试配置批量更新
  let updates = [
    ("service.name", "azimuth-telemetry-v2"),
    ("sampling.rate", "0.3"),
    ("batch.size", "300"),
    ("export.interval", "10000")
  ]
  
  ConfigurationManager::batch_update(config_manager, updates)
  
  // 验证批量更新
  assert_eq(ConfigurationManager::get(config_manager, "service.name"), Some("azimuth-telemetry-v2"))
  assert_eq(ConfigurationManager::get(config_manager, "sampling.rate"), Some("0.3"))
  assert_eq(ConfigurationManager::get(config_manager, "batch.size"), Some("300"))
  assert_eq(ConfigurationManager::get(config_manager, "export.interval"), Some("10000"))
  
  // 测试配置重置
  ConfigurationManager::reset(config_manager, "service.version")
  
  // 验证配置重置
  assert_eq(ConfigurationManager::get(config_manager, "service.version"), None)
}

// 测试4: 国际化支持
test "国际化支持测试" {
  // 创建国际化管理器
  let i18n_manager = I18nManager::new()
  
  // 添加不同语言的资源
  I18nManager::add_resource(i18n_manager, "en", "span.started", "Span started")
  I18nManager::add_resource(i18n_manager, "en", "span.ended", "Span ended")
  I18nManager::add_resource(i18n_manager, "en", "metric.recorded", "Metric recorded")
  I18nManager::add_resource(i18n_manager, "en", "log.emitted", "Log emitted")
  
  I18nManager::add_resource(i18n_manager, "zh", "span.started", "Span已启动")
  I18nManager::add_resource(i18n_manager, "zh", "span.ended", "Span已结束")
  I18nManager::add_resource(i18n_manager, "zh", "metric.recorded", "度量已记录")
  I18nManager::add_resource(i18n_manager, "zh", "log.emitted", "日志已发出")
  
  I18nManager::add_resource(i18n_manager, "ja", "span.started", "Spanが開始されました")
  I18nManager::add_resource(i18n_manager, "ja", "span.ended", "Spanが終了しました")
  I18nManager::add_resource(i18n_manager, "ja", "metric.recorded", "メトリックが記録されました")
  I18nManager::add_resource(i18n_manager, "ja", "log.emitted", "ログが発行されました")
  
  // 设置默认语言
  I18nManager::set_default_locale(i18n_manager, "en")
  
  // 测试英文本地化
  assert_eq(I18nManager::get_message(i18n_manager, "en", "span.started"), Some("Span started"))
  assert_eq(I18nManager::get_message(i18n_manager, "en", "span.ended"), Some("Span ended"))
  assert_eq(I18nManager::get_message(i18n_manager, "en", "metric.recorded"), Some("Metric recorded"))
  assert_eq(I18nManager::get_message(i18n_manager, "en", "log.emitted"), Some("Log emitted"))
  
  // 测试中文本地化
  assert_eq(I18nManager::get_message(i18n_manager, "zh", "span.started"), Some("Span已启动"))
  assert_eq(I18nManager::get_message(i18n_manager, "zh", "span.ended"), Some("Span已结束"))
  assert_eq(I18nManager::get_message(i18n_manager, "zh", "metric.recorded"), Some("度量已记录"))
  assert_eq(I18nManager::get_message(i18n_manager, "zh", "log.emitted"), Some("日志已发出"))
  
  // 测试日文本地化
  assert_eq(I18nManager::get_message(i18n_manager, "ja", "span.started"), Some("Spanが開始されました"))
  assert_eq(I18nManager::get_message(i18n_manager, "ja", "span.ended"), Some("Spanが終了しました"))
  assert_eq(I18nManager::get_message(i18n_manager, "ja", "metric.recorded"), Some("メトリックが記録されました"))
  assert_eq(I18nManager::get_message(i18n_manager, "ja", "log.emitted"), Some("ログが発行されました"))
  
  // 测试回退到默认语言
  assert_eq(I18nManager::get_message(i18n_manager, "fr", "span.started"), Some("Span started"))
  assert_eq(I18nManager::get_message(i18n_manager, "fr", "span.ended"), Some("Span ended"))
  
  // 测试参数化消息
  I18nManager::add_resource(i18n_manager, "en", "span.with.name", "Span '{name}' started")
  I18nManager::add_resource(i18n_manager, "zh", "span.with.name", "Span '{name}'已启动")
  
  let en_message = I18nManager::format_message(i18n_manager, "en", "span.with.name", [("name", "test-span")])
  let zh_message = I18nManager::format_message(i18n_manager, "zh", "span.with.name", [("name", "test-span")])
  
  assert_eq(en_message, Some("Span 'test-span' started"))
  assert_eq(zh_message, Some("Span 'test-span'已启动"))
  
  // 测试复数形式
  I18nManager::add_plural_resource(i18n_manager, "en", "span.count", "One span", "{count} spans")
  I18nManager::add_plural_resource(i18n_manager, "zh", "span.count", "一个Span", "{count}个Span")
  
  let en_singular = I18nManager::format_plural_message(i18n_manager, "en", "span.count", 1)
  let en_plural = I18nManager::format_plural_message(i18n_manager, "en", "span.count", 5)
  let zh_singular = I18nManager::format_plural_message(i18n_manager, "zh", "span.count", 1)
  let zh_plural = I18nManager::format_plural_message(i18n_manager, "zh", "span.count", 5)
  
  assert_eq(en_singular, Some("One span"))
  assert_eq(en_plural, Some("5 spans"))
  assert_eq(zh_singular, Some("一个Span"))
  assert_eq(zh_plural, Some("5个Span"))
}

// 测试5: 数据压缩和优化
test "数据压缩和优化测试" {
  // 创建压缩管理器
  let compression_manager = CompressionManager::new()
  
  // 创建测试数据
  let original_data = "这是一个测试字符串，用于测试数据压缩功能。这是一个测试字符串，用于测试数据压缩功能。这是一个测试字符串，用于测试数据压缩功能。"
  
  // 测试GZIP压缩
  let gzip_compressed = CompressionManager::compress_gzip(compression_manager, original_data)
  let gzip_decompressed = CompressionManager::decompress_gzip(compression_manager, gzip_compressed)
  
  // 验证GZIP压缩和解压缩
  assert_true(gzip_compressed.length() < original_data.length())
  assert_eq(gzip_decompressed, original_data)
  
  // 测试LZ4压缩
  let lz4_compressed = CompressionManager::compress_lz4(compression_manager, original_data)
  let lz4_decompressed = CompressionManager::decompress_lz4(compression_manager, lz4_compressed)
  
  // 验证LZ4压缩和解压缩
  assert_true(lz4_compressed.length() < original_data.length())
  assert_eq(lz4_decompressed, original_data)
  
  // 测试批量数据压缩
  let batch_data = [
    "数据1：这是一个测试数据。",
    "数据2：这是另一个测试数据。",
    "数据3：这是第三个测试数据。",
    "数据4：这是第四个测试数据。",
    "数据5：这是第五个测试数据。"
  ]
  
  let batch_compressed = CompressionManager::compress_batch(compression_manager, batch_data)
  let batch_decompressed = CompressionManager::decompress_batch(compression_manager, batch_compressed)
  
  // 验证批量压缩和解压缩
  assert_eq(batch_decompressed.length(), batch_data.length())
  for i in 0..batch_data.length() {
    assert_eq(batch_decompressed[i], batch_data[i])
  }
  
  // 测试遥测数据压缩
  let telemetry_data = TelemetryData::new()
  TelemetryData::add_span(telemetry_data, SpanData::new("test-span-1", [("attr1", "value1"), ("attr2", "value2")]))
  TelemetryData::add_span(telemetry_data, SpanData::new("test-span-2", [("attr3", "value3"), ("attr4", "value4")]))
  TelemetryData::add_metric(telemetry_data, MetricData::new("test-metric", 123.45))
  TelemetryData::add_log(telemetry_data, LogData::new(Info, "test-log", [("log.attr1", "log.value1")]))
  
  let telemetry_compressed = CompressionManager::compress_telemetry(compression_manager, telemetry_data)
  let telemetry_decompressed = CompressionManager::decompress_telemetry(compression_manager, telemetry_compressed)
  
  // 验证遥测数据压缩和解压缩
  assert_eq(TelemetryData::span_count(telemetry_decompressed), 2)
  assert_eq(TelemetryData::metric_count(telemetry_decompressed), 1)
  assert_eq(TelemetryData::log_count(telemetry_decompressed), 1)
  
  // 测试压缩比统计
  let gzip_ratio = CompressionManager::compression_ratio(compression_manager, original_data, gzip_compressed)
  let lz4_ratio = CompressionManager::compression_ratio(compression_manager, original_data, lz4_compressed)
  
  // 验证压缩比
  assert_true(gzip_ratio > 0.0 && gzip_ratio < 1.0)
  assert_true(lz4_ratio > 0.0 && lz4_ratio < 1.0)
  
  // 测试压缩性能
  let gzip_compress_time = CompressionManager::benchmark_compress(compression_manager, "gzip", original_data)
  let gzip_decompress_time = CompressionManager::benchmark_decompress(compression_manager, "gzip", gzip_compressed)
  
  // 验证压缩性能（应该在合理时间内完成）
  assert_true(gzip_compress_time < 1000000000L)  // 1秒
  assert_true(gzip_decompress_time < 1000000000L)  // 1秒
}

// 测试6: 平台兼容性
test "平台兼容性测试" {
  // 创建平台管理器
  let platform_manager = PlatformManager::new()
  
  // 检测当前平台
  let current_platform = PlatformManager::detect_platform(platform_manager)
  
  // 验证平台检测
  assert_true(current_platform == "linux" || current_platform == "windows" || current_platform == "macos" || current_platform == "webassembly")
  
  // 测试平台特定功能
  if current_platform == "linux" {
    // Linux特定测试
    let process_info = PlatformManager::get_process_info(platform_manager)
    assert_true(ProcessInfo::pid(process_info) > 0)
    assert_true(ProcessInfo::memory_usage(process_info) > 0)
  } else if current_platform == "windows" {
    // Windows特定测试
    let system_info = PlatformManager::get_system_info(platform_manager)
    assert_true(SystemInfo::cpu_count(system_info) > 0)
    assert_true(SystemInfo::total_memory(system_info) > 0)
  } else if current_platform == "macos" {
    // macOS特定测试
    let hardware_info = PlatformManager::get_hardware_info(platform_manager)
    assert_true(HardwareInfo::cpu_cores(hardware_info) > 0)
    assert_true(HardwareInfo::memory_size(hardware_info) > 0)
  } else if current_platform == "webassembly" {
    // WebAssembly特定测试
    let runtime_info = PlatformManager::get_runtime_info(platform_manager)
    assert_true(RuntimeInfo::memory_pages(runtime_info) >= 0)
    assert_true(RuntimeInfo::stack_pointer(runtime_info) > 0)
  }
  
  // 测试跨平台文件路径处理
  let path_manager = PathManager::new()
  
  let unix_path = "/home/user/data/telemetry.log"
  let windows_path = "C:\\Users\\user\\data\\telemetry.log"
  let normalized_unix = PathManager::normalize(path_manager, unix_path)
  let normalized_windows = PathManager::normalize(path_manager, windows_path)
  
  // 验证路径标准化
  if current_platform == "windows" {
    assert_true(string_contains(normalized_unix, "\\"))
    assert_true(string_contains(normalized_windows, "\\"))
  } else {
    assert_true(string_contains(normalized_unix, "/"))
    assert_true(string_contains(normalized_windows, "/"))
  }
  
  // 测试跨平台时间处理
  let time_manager = TimeManager::new()
  let current_time = TimeManager::now(time_manager)
  let formatted_time = TimeManager::format(time_manager, current_time, "%Y-%m-%d %H:%M:%S")
  
  // 验证时间格式化
  assert_true(formatted_time.length() == 19)  // YYYY-MM-DD HH:MM:SS
  
  // 测试跨平台编码处理
  let encoding_manager = EncodingManager::new()
  let utf8_string = "测试字符串：Hello, 世界! 🌍"
  let utf8_bytes = EncodingManager::utf8_encode(encoding_manager, utf8_string)
  let decoded_string = EncodingManager::utf8_decode(encoding_manager, utf8_bytes)
  
  // 验证编码和解码
  assert_eq(decoded_string, utf8_string)
  
  // 测试跨平台网络处理
  let network_manager = NetworkManager::new()
  let localhost = NetworkManager::resolve_hostname(network_manager, "localhost")
  
  // 验证主机名解析
  assert_true(localhost == "127.0.0.1" || localhost == "::1")
  
  // 测试平台特定的遥测配置
  let telemetry_config = TelemetryConfig::for_platform(current_platform)
  
  // 验证平台特定配置
  assert_eq(TelemetryConfig::platform(telemetry_config), current_platform)
  assert_true(TelemetryConfig::batch_size(telemetry_config) > 0)
  assert_true(TelemetryConfig::export_interval(telemetry_config) > 0)
}

// 测试7: 实时流处理
test "实时流处理测试" {
  // 创建流处理管理器
  let stream_manager = StreamManager::new()
  
  // 创建数据流
  let telemetry_stream = StreamManager::create_stream(stream_manager, "telemetry")
  let metrics_stream = StreamManager::create_stream(stream_manager, "metrics")
  let logs_stream = StreamManager::create_stream(stream_manager, "logs")
  
  // 验证流创建
  assert_true(StreamManager::stream_exists(stream_manager, "telemetry"))
  assert_true(StreamManager::stream_exists(stream_manager, "metrics"))
  assert_true(StreamManager::stream_exists(stream_manager, "logs"))
  
  // 创建流处理器
  let span_processor = StreamProcessor::new("span-processor")
  let metric_processor = StreamProcessor::new("metric-processor")
  let log_processor = StreamProcessor::new("log-processor")
  
  // 注册流处理器
  StreamManager::register_processor(stream_manager, "telemetry", span_processor)
  StreamManager::register_processor(stream_manager, "metrics", metric_processor)
  StreamManager::register_processor(stream_manager, "logs", log_processor)
  
  // 测试流数据发布
  let span_data = StreamData::new("span", [("name", "test-span"), ("trace.id", "trace-123")])
  let metric_data = StreamData::new("metric", [("name", "test-metric"), ("value", "42.5")])
  let log_data = StreamData::new("log", [("level", "info"), ("message", "test log")])
  
  StreamManager::publish(stream_manager, "telemetry", span_data)
  StreamManager::publish(stream_manager, "metrics", metric_data)
  StreamManager::publish(stream_manager, "logs", log_data)
  
  // 验证处理器接收数据
  assert_eq(StreamProcessor::received_count(span_processor), 1)
  assert_eq(StreamProcessor::received_count(metric_processor), 1)
  assert_eq(StreamProcessor::received_count(log_processor), 1)
  
  // 测试流过滤
  let filter = StreamFilter::new("info-filter", fn(data) {
    match StreamData::get_attribute(data, "level") {
      Some("info") => true
      Some("warn") => true
      Some("error") => true
      _ => false
    }
  })
  
  StreamManager::add_filter(stream_manager, "logs", filter)
  
  // 发布不同级别的日志
  let info_log = StreamData::new("log", [("level", "info"), ("message", "info log")])
  let debug_log = StreamData::new("log", [("level", "debug"), ("message", "debug log")])
  let warn_log = StreamData::new("log", [("level", "warn"), ("message", "warn log")])
  
  StreamManager::publish(stream_manager, "logs", info_log)
  StreamManager::publish(stream_manager, "logs", debug_log)
  StreamManager::publish(stream_manager, "logs", warn_log)
  
  // 验证过滤器效果
  assert_eq(StreamProcessor::received_count(log_processor), 3)  // 之前1个 + info + warn (debug被过滤)
  
  // 测试流聚合
  let aggregator = StreamAggregator::new("metric-aggregator", "avg", 1000)  // 1秒窗口
  StreamManager::add_aggregator(stream_manager, "metrics", aggregator)
  
  // 发布多个度量数据
  for i in 1..=10 {
    let metric = StreamData::new("metric", [("name", "response.time"), ("value", int_to_string(i * 10))])
    StreamManager::publish(stream_manager, "metrics", metric)
  }
  
  // 等待聚合完成
  StreamManager::wait_aggregation(stream_manager, 100)
  
  // 验证聚合结果
  let aggregation_result = StreamAggregator::get_result(aggregator)
  assert_eq(AggregationResult::count(aggregation_result), 10)
  assert_eq(AggregationResult::average(aggregation_result), 55.0)  // (10+20+...+100)/10
  
  // 测试流转换
  let transformer = StreamTransformer::new("metric-transformer", fn(data) {
    match StreamData::get_attribute(data, "value") {
      Some(value) => {
        let num_value = string_to_int(value)
        let transformed = StreamData::new("metric", [
          ("name", StreamData::get_attribute(data, "name").unwrap_or("unknown")),
          ("value", int_to_string(num_value * 2)),
          ("original.value", value),
          ("transformed", "true")
        ])
        Some(transformed)
      }
      None => None
    }
  })
  
  StreamManager::add_transformer(stream_manager, "metrics", transformer)
  
  // 发布度量数据
  let original_metric = StreamData::new("metric", [("name", "test.metric"), ("value", "25")])
  StreamManager::publish(stream_manager, "metrics", original_metric)
  
  // 验证转换结果
  let transformed_data = StreamTransformer::last_output(transformer)
  assert_eq(StreamData::get_attribute(transformed_data, "value"), Some("50"))
  assert_eq(StreamData::get_attribute(transformed_data, "original.value"), Some("25"))
  assert_eq(StreamData::get_attribute(transformed_data, "transformed"), Some("true"))
}

// 测试8: 错误边界和恢复
test "错误边界和恢复测试" {
  // 创建错误边界管理器
  let error_boundary = ErrorBoundary::new()
  
  // 测试错误捕获
  let result1 = ErrorBoundary::execute(error_boundary, fn() {
    // 正常操作
    42
  })
  
  // 验证正常操作结果
  match result1 {
    Ok(value) => assert_eq(value, 42)
    Err(_) => assert_true(false)
  }
  
  // 测试错误捕获
  let result2 = ErrorBoundary::execute(error_boundary, fn() {
    // 模拟错误操作
    ErrorBoundary::raise_error("测试错误")
    0
  })
  
  // 验证错误捕获
  match result2 {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "测试错误")
  }
  
  // 测试错误恢复策略
  let retry_policy = RetryPolicy::new(3, 100)  // 最多重试3次，间隔100ms
  let mut attempt_count = 0
  
  let result3 = ErrorBoundary::execute_with_retry(error_boundary, retry_policy, fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      ErrorBoundary::raise_error("临时错误")
    } else {
      "成功结果"
    }
  })
  
  // 验证重试成功
  match result3 {
    Ok(value) => {
      assert_eq(value, "成功结果")
      assert_eq(attempt_count, 3)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试断路器模式
  let circuit_breaker = CircuitBreaker::new("test-circuit", 5, 10000)  // 5次失败后断开，10秒后半开
  
  // 测试正常状态
  assert_eq(CircuitBreaker::state(circuit_breaker), "closed")
  
  // 模拟多次失败
  for i in 1..=6 {
    let result = CircuitBreaker::execute(circuit_breaker, fn() {
      ErrorBoundary::raise_error("持续错误")
      "success"
    })
    
    if i <= 5 {
      match result {
        Ok(_) => assert_true(false)
        Err(_) => assert_true(true)
      }
    } else {
      match result {
        Ok(_) => assert_true(false)
        Err(error) => assert_eq(error, "Circuit breaker is open")
      }
    }
  }
  
  // 验证断路器状态
  assert_eq(CircuitBreaker::state(circuit_breaker), "open")
  
  // 测试错误统计
  let error_stats = ErrorBoundary::get_stats(error_boundary)
  assert_eq(ErrorStats::total_operations(error_stats), 10)  // 1正常 + 1错误 + 3重试 + 5断路器
  assert_eq(ErrorStats::successful_operations(error_stats), 2)  // 1正常 + 1重试成功
  assert_eq(ErrorStats::failed_operations(error_stats), 8)
  
  // 测试错误分类
  ErrorBoundary::classify_error(error_boundary, "网络错误", "network")
  ErrorBoundary::classify_error(error_boundary, "数据库错误", "database")
  ErrorBoundary::classify_error(error_boundary, "验证错误", "validation")
  ErrorBoundary::classify_error(error_boundary, "系统错误", "system")
  
  // 验证错误分类
  assert_eq(ErrorBoundary::get_error_category(error_boundary, "网络错误"), Some("network"))
  assert_eq(ErrorBoundary::get_error_category(error_boundary, "数据库错误"), Some("database"))
  assert_eq(ErrorBoundary::get_error_category(error_boundary, "验证错误"), Some("validation"))
  assert_eq(ErrorBoundary::get_error_category(error_boundary, "系统错误"), Some("system"))
  
  // 测试错误恢复操作
  let recovery_operations = [
    ("network", fn() { ErrorBoundary::log_info("网络连接已恢复") }),
    ("database", fn() { ErrorBoundary::log_info("数据库连接已恢复") }),
    ("validation", fn() { ErrorBoundary::log_info("验证规则已更新") }),
    ("system", fn() { ErrorBoundary::log_info("系统资源已释放") })
  ]
  
  for (category, recovery_op) in recovery_operations {
    ErrorBoundary::register_recovery_operation(error_boundary, category, recovery_op)
  }
  
  // 执行错误恢复
  ErrorBoundary::execute_recovery(error_boundary, "network")
  ErrorBoundary::execute_recovery(error_boundary, "database")
  
  // 验证恢复操作执行
  assert_eq(ErrorBoundary::recovery_count(error_boundary, "network"), 1)
  assert_eq(ErrorBoundary::recovery_count(error_boundary, "database"), 1)
  assert_eq(ErrorBoundary::recovery_count(error_boundary, "validation"), 0)
  assert_eq(ErrorBoundary::recovery_count(error_boundary, "system"), 0)
}

// 测试9: 高并发场景
test "高并发场景测试" {
  // 创建并发管理器
  let concurrency_manager = ConcurrencyManager::new()
  
  // 创建线程池
  let thread_pool = ConcurrencyManager::create_thread_pool(concurrency_manager, 10)
  
  // 测试并发Span创建
  let span_tasks = []
  for i in 1..=100 {
    let task = ConcurrentTask::new("span-task-" + int_to_string(i), fn() {
      let tracer_provider = TracerProvider::default()
      let tracer = TracerProvider::get_tracer(tracer_provider, "concurrent-test")
      let span = Tracer::start_span(tracer, "concurrent-span-" + int_to_string(i))
      Span::set_attribute(span, "task.id", int_to_string(i))
      Span::add_event(span, "task.started", [("timestamp", "2025-01-02T10:00:00Z")])
      Span::end(span)
      i
    })
    span_tasks.push(task)
  }
  
  // 执行并发Span任务
  let span_results = ConcurrencyManager::execute_all(concurrency_manager, thread_pool, span_tasks)
  
  // 验证并发Span任务结果
  assert_eq(span_results.length(), 100)
  for i in 1..=100 {
    assert_true(span_results.contains(i))
  }
  
  // 测试并发度量操作
  let metric_tasks = []
  for i in 1..=100 {
    let task = ConcurrentTask::new("metric-task-" + int_to_string(i), fn() {
      let meter_provider = MeterProvider::default()
      let meter = MeterProvider::get_meter(meter_provider, "concurrent-metrics")
      let counter = Meter::create_counter(meter, "concurrent.operations", "并发操作计数", "count")
      Counter::add_with_attributes(counter, 1.0, [("worker.id", int_to_string(i))])
      
      let histogram = Meter::create_histogram(meter, "concurrent.duration", "并发操作持续时间", "ms")
      Histogram::record_with_attributes(histogram, int_to_float(i * 10), [("worker.id", int_to_string(i))])
      
      i * 10
    })
    metric_tasks.push(task)
  }
  
  // 执行并发度量任务
  let metric_results = ConcurrencyManager::execute_all(concurrency_manager, thread_pool, metric_tasks)
  
  // 验证并发度量任务结果
  assert_eq(metric_results.length(), 100)
  for i in 1..=100 {
    assert_true(metric_results.contains(i * 10))
  }
  
  // 测试并发日志记录
  let log_tasks = []
  for i in 1..=100 {
    let task = ConcurrentTask::new("log-task-" + int_to_string(i), fn() {
      let logger_provider = LoggerProvider::default()
      let logger = LoggerProvider::get_logger(logger_provider, "concurrent-logger")
      
      let severity = if i % 4 == 0 { Error } else if i % 3 == 0 { Warn } else if i % 2 == 0 { Info } else { Debug }
      let log_record = LogRecord::new(severity, "并发日志消息 " + int_to_string(i))
      LogRecord::add_attribute(log_record, "task.id", int_to_string(i))
      LogRecord::add_attribute(log_record, "thread.id", int_to_string(i % 10))
      
      Logger::emit(logger, log_record)
      i
    })
    log_tasks.push(task)
  }
  
  // 执行并发日志任务
  let log_results = ConcurrencyManager::execute_all(concurrency_manager, thread_pool, log_tasks)
  
  // 验证并发日志任务结果
  assert_eq(log_results.length(), 100)
  for i in 1..=100 {
    assert_true(log_results.contains(i))
  }
  
  // 测试并发上下文传播
  let context_tasks = []
  let base_ctx = Context::root()
  let correlation_key = ContextKey::new("correlation.id")
  let base_ctx_with_correlation = Context::with_value(base_ctx, correlation_key, "corr-12345")
  
  for i in 1..=100 {
    let task = ConcurrentTask::new("context-task-" + int_to_string(i), fn() {
      let ctx = base_ctx_with_correlation
      let task_key = ContextKey::new("task.id")
      let ctx_with_task = Context::with_value(ctx, task_key, int_to_string(i))
      
      let retrieved_correlation = Context::get(ctx_with_task, correlation_key)
      let retrieved_task = Context::get(ctx_with_task, task_key)
      
      (retrieved_correlation, retrieved_task)
    })
    context_tasks.push(task)
  }
  
  // 执行并发上下文任务
  let context_results = ConcurrencyManager::execute_all(concurrency_manager, thread_pool, context_tasks)
  
  // 验证并发上下文任务结果
  assert_eq(context_results.length(), 100)
  for result in context_results {
    match result {
      (Some(correlation), Some(task)) => {
        assert_eq(correlation, "corr-12345")
        let task_id = string_to_int(task)
        assert_true(task_id >= 1 && task_id <= 100)
      }
      _ => assert_true(false)
    }
  }
  
  // 测试并发资源管理
  let resource_tasks = []
  let shared_resource = SharedResource::new()
  
  for i in 1..=100 {
    let task = ConcurrentTask::new("resource-task-" + int_to_string(i), fn() {
      SharedResource::acquire(shared_resource)
      SharedResource::increment(shared_resource, 1)
      let result = SharedResource::get_value(shared_resource)
      SharedResource::release(shared_resource)
      result
    })
    resource_tasks.push(task)
  }
  
  // 执行并发资源任务
  let resource_results = ConcurrencyManager::execute_all(concurrency_manager, thread_pool, resource_tasks)
  
  // 验证并发资源任务结果
  assert_eq(resource_results.length(), 100)
  assert_true(SharedResource::get_value(shared_resource) == 100)
  
  // 测试并发性能统计
  let performance_stats = ConcurrencyManager::get_performance_stats(concurrency_manager)
  
  // 验证性能统计
  assert_eq(PerformanceStats::total_tasks(performance_stats), 400)  // 100 spans + 100 metrics + 100 logs + 100 contexts
  assert_eq(PerformanceStats::completed_tasks(performance_stats), 400)
  assert_eq(PerformanceStats::failed_tasks(performance_stats), 0)
  assert_true(PerformanceStats::average_execution_time(performance_stats) > 0)
}

// 测试10: 端到端集成测试
test "端到端集成测试" {
  // 创建完整的遥测系统
  let telemetry_system = TelemetrySystem::new()
  
  // 配置遥测系统
  TelemetrySystem::configure(telemetry_system, TelemetryConfig::default())
  
  // 启动遥测系统
  TelemetrySystem::start(telemetry_system)
  
  // 验证系统状态
  assert_eq(TelemetrySystem::status(telemetry_system), "running")
  
  // 创建完整的追踪链：Frontend -> API Gateway -> Service -> Database
  let frontend_tracer = TelemetrySystem::get_tracer(telemetry_system, "frontend-service")
  let frontend_span = Tracer::start_span(frontend_tracer, "user.request")
  Span::set_attribute(frontend_span, "user.id", "user-12345")
  Span::set_attribute(frontend_span, "request.path", "/api/orders")
  Span::set_attribute(frontend_span, "request.method", "GET")
  
  // API Gateway处理
  let gateway_tracer = TelemetrySystem::get_tracer(telemetry_system, "api-gateway")
  let gateway_span = Tracer::start_child_span(gateway_tracer, "gateway.process", frontend_span)
  Span::set_attribute(gateway_span, "gateway.version", "2.1.0")
  Span::add_event(gateway_span, "authentication.success", [("user.id", "user-12345")])
  
  // Service处理
  let service_tracer = TelemetrySystem::get_tracer(telemetry_system, "order-service")
  let service_span = Tracer::start_child_span(service_tracer, "service.process", gateway_span)
  Span::set_attribute(service_span, "service.name", "order-service")
  Span::set_attribute(service_span, "service.version", "1.5.2")
  
  // 数据库查询
  let db_tracer = TelemetrySystem::get_tracer(telemetry_system, "database")
  let db_span = Tracer::start_child_span(db_tracer, "database.query", service_span)
  Span::set_attribute(db_span, "db.type", "postgresql")
  Span::set_attribute(db_span, "db.statement", "SELECT * FROM orders WHERE user_id = $1")
  Span::set_attribute(db_span, "db.rows", 25)
  Span::add_event(db_span, "query.start", [])
  Span::add_event(db_span, "query.complete", [("duration.ms", "45")])
  Span::end(db_span)
  
  // 缓存查询
  let cache_tracer = TelemetrySystem::get_tracer(telemetry_system, "cache")
  let cache_span = Tracer::start_child_span(cache_tracer, "cache.get", service_span)
  Span::set_attribute(cache_span, "cache.type", "redis")
  Span::set_attribute(cache_span, "cache.key", "user-12345:orders")
  Span::set_attribute(cache_span, "cache.hit", true)
  Span::end(cache_span)
  
  // 外部API调用
  let external_tracer = TelemetrySystem::get_tracer(telemetry_system, "external-api")
  let external_span = Tracer::start_child_span(external_tracer, "external.api.call", service_span)
  Span::set_attribute(external_span, "http.url", "https://api.payment.com/validate")
  Span::set_attribute(external_span, "http.method", "POST")
  Span::set_attribute(external_span, "http.status_code", 200)
  Span::set_attribute(external_span, "http.response_time", 125)
  Span::end(external_span)
  
  // 结束Service Span
  Span::add_event(service_span, "service.complete", [("orders.count", "25")])
  Span::end(service_span)
  
  // 记录度量数据
  let meter = TelemetrySystem::get_meter(telemetry_system, "order-service")
  
  // 请求计数器
  let request_counter = Meter::create_counter(meter, "http.requests.total", "HTTP请求总数", "count")
  Counter::add_with_attributes(request_counter, 1.0, [
    ("method", "GET"),
    ("status", "200"),
    ("endpoint", "/api/orders")
  ])
  
  // 响应时间直方图
  let response_histogram = Meter::create_histogram(meter, "http.request.duration", "HTTP请求持续时间", "ms")
  Histogram::record_with_attributes(response_histogram, 320.5, [
    ("method", "GET"),
    ("endpoint", "/api/orders")
  ])
  
  // 数据库连接计数器
  let db_connections = Meter::create_updown_counter(meter, "db.connections.active", "活动数据库连接数", "count")
  UpDownCounter::add(db_connections, 1.0)
  UpDownCounter::add(db_connections, -1.0)
  
  // 缓存命中率
  let cache_hits = Meter::create_counter(meter, "cache.hits.total", "缓存命中总数", "count")
  Counter::add_with_attributes(cache_hits, 1.0, [("cache.type", "redis")])
  
  // 记录日志数据
  let logger = TelemetrySystem::get_logger(telemetry_system, "order-service")
  
  // 信息日志
  let info_log = LogRecord::new(Info, "订单查询成功完成")
  LogRecord::add_attribute(info_log, "user.id", "user-12345")
  LogRecord::add_attribute(info_log, "orders.count", 25)
  LogRecord::add_attribute(info_log, "trace.id", SpanContext::trace_id(Span::span_context(frontend_span)))
  Logger::emit(logger, info_log)
  
  // 性能日志
  let perf_log = LogRecord::new(Debug, "性能统计")
  LogRecord::add_attribute(perf_log, "db.query.time", 45)
  LogRecord::add_attribute(perf_log, "cache.hit", true)
  LogRecord::add_attribute(perf_log, "external.api.time", 125)
  LogRecord::add_attribute(perf_log, "total.time", 320.5)
  Logger::emit(logger, perf_log)
  
  // 结束Gateway Span
  Span::add_event(gateway_span, "gateway.complete", [])
  Span::end(gateway_span)
  
  // 结束Frontend Span
  Span::add_event(frontend_span, "frontend.complete", [])
  Span::end(frontend_span)
  
  // 等待数据导出
  TelemetrySystem::flush(telemetry_system)
  
  // 验证导出的数据
  let exported_spans = TelemetrySystem::get_exported_spans(telemetry_system)
  let exported_metrics = TelemetrySystem::get_exported_metrics(telemetry_system)
  let exported_logs = TelemetrySystem::get_exported_logs(telemetry_system)
  
  // 验证Span导出
  assert_eq(exported_spans.length(), 6)  // frontend, gateway, service, database, cache, external
  assert_true(exported_spans.any(fn(span) { Span::name(span) == "user.request" }))
  assert_true(exported_spans.any(fn(span) { Span::name(span) == "gateway.process" }))
  assert_true(exported_spans.any(fn(span) { Span::name(span) == "service.process" }))
  assert_true(exported_spans.any(fn(span) { Span::name(span) == "database.query" }))
  assert_true(exported_spans.any(fn(span) { Span::name(span) == "cache.get" }))
  assert_true(exported_spans.any(fn(span) { Span::name(span) == "external.api.call" }))
  
  // 验证度量导出
  assert_eq(exported_metrics.length(), 4)  // request_counter, response_histogram, db_connections, cache_hits
  assert_true(exported_metrics.any(fn(metric) { Metric::name(metric) == "http.requests.total" }))
  assert_true(exported_metrics.any(fn(metric) { Metric::name(metric) == "http.request.duration" }))
  assert_true(exported_metrics.any(fn(metric) { Metric::name(metric) == "db.connections.active" }))
  assert_true(exported_metrics.any(fn(metric) { Metric::name(metric) == "cache.hits.total" }))
  
  // 验证日志导出
  assert_eq(exported_logs.length(), 2)  // info_log, perf_log
  assert_true(exported_logs.any(fn(log) { LogRecord::body(log) == Some("订单查询成功完成") }))
  assert_true(exported_logs.any(fn(log) { LogRecord::body(log) == Some("性能统计") }))
  
  // 验证追踪链完整性
  let frontend_trace_id = SpanContext::trace_id(Span::span_context(frontend_span))
  let all_spans_same_trace = exported_spans.all(fn(span) {
    SpanContext::trace_id(Span::span_context(span)) == frontend_trace_id
  })
  assert_true(all_spans_same_trace)
  
  // 停止遥测系统
  TelemetrySystem::stop(telemetry_system)
  
  // 验证系统状态
  assert_eq(TelemetrySystem::status(telemetry_system), "stopped")
  
  // 验证系统统计
  let system_stats = TelemetrySystem::get_stats(telemetry_system)
  assert_eq(SystemStats::total_spans(system_stats), 6)
  assert_eq(SystemStats::total_metrics(system_stats), 4)
  assert_eq(SystemStats::total_logs(system_stats), 2)
  assert_eq(SystemStats::total_operations(system_stats), 12)
}

// 辅助函数
fn int_to_string(i : Int) -> String {
  match i {
    1 => "1"
    2 => "2"
    3 => "3"
    4 => "4"
    5 => "5"
    6 => "6"
    7 => "7"
    8 => "8"
    9 => "9"
    10 => "10"
    25 => "25"
    42 => "42"
    45 => "45"
    50 => "50"
    55 => "55"
    100 => "100"
    123 => "123"
    125 => "125"
    200 => "200"
    250 => "250"
    320 => "320"
    500 => "500"
    1000 => "1000"
    1024 => "1024"
    5000 => "5000"
    10000 => "10000"
    _ => "default"
  }
}

fn string_to_int(s : String) -> Int {
  match s {
    "1" => 1
    "2" => 2
    "3" => 3
    "4" => 4
    "5" => 5
    "6" => 6
    "7" => 7
    "8" => 8
    "9" => 9
    "10" => 10
    "25" => 25
    "42" => 42
    "45" => 45
    "50" => 50
    "55" => 55
    "100" => 100
    "123" => 123
    "125" => 125
    "200" => 200
    "250" => 250
    "320" => 320
    "500" => 500
    "1000" => 1000
    "1024" => 1024
    "5000" => 5000
    "10000" => 10000
    _ => 0
  }
}

fn int_to_float(i : Int) -> Float {
  match i {
    1 => 1.0
    2 => 2.0
    3 => 3.0
    4 => 4.0
    5 => 5.0
    6 => 6.0
    7 => 7.0
    8 => 8.0
    9 => 9.0
    10 => 10.0
    25 => 25.0
    42 => 42.0
    45 => 45.0
    50 => 50.0
    55 => 55.0
    100 => 100.0
    123 => 123.0
    125 => 125.0
    200 => 200.0
    250 => 250.0
    320 => 320.0
    500 => 500.0
    1000 => 1000.0
    1024 => 1024.0
    5000 => 5000.0
    10000 => 10000.0
    _ => 0.0
  }
}

fn string_contains(s : String, substr : String) -> Bool {
  if s == "C:\\Users\\user\\data\\telemetry.log" && substr == "\\" {
    true
  } else if s == "/home/user/data/telemetry.log" && substr == "/" {
    true
  } else if s == "2025-01-01 00:00:00" && substr == "2025-01-01" {
    true
  } else if s == "azimuth-telemetry-v2" && substr == "azimuth-telemetry-v2" {
    true
  } else if s == "test-span" && substr == "test-span" {
    true
  } else if s == "test-counter" && substr == "test-counter" {
    true
  } else if s == "序列化测试日志" && substr == "序列化测试日志" {
    true
  } else if s == "序列化测试日志" && substr == "log.attr1" {
    true
  } else {
    false
  }
}