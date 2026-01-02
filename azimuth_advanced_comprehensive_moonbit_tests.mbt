// Azimuth Advanced Comprehensive MoonBit Test Suite
// 高级综合MoonBit测试套件，覆盖遥测系统的高级功能和边缘情况

// 测试1: 异步遥测数据导出
pub test "异步遥测数据导出测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "async-export-test")
  
  // 创建多个Span用于异步导出
  let spans = []
  for i in 0..100 {
    let span = azimuth::Tracer::start_span(tracer, "async-span-" + i.to_string())
    azimuth::Span::add_event(span, "async-event-" + i.to_string(), 
      Some([("batch.id", azimuth::StringValue("batch-" + (i/10).to_string()))]))
    spans.push(span)
  }
  
  // 配置异步导出器
  let exporter_config = azimuth::AsyncExporterConfig::new(
    10,  // 批量大小
    5000,  // 最大导出间隔(ms)
    3  // 最大重试次数
  )
  
  let exporter = azimuth::AsyncExporter::new(exporter_config)
  
  // 批量结束Span
  for span in spans {
    azimuth::Span::end(span)
  }
  
  // 验证异步导出器配置
  assert_eq(azimuth::AsyncExporterConfig::batch_size(exporter_config), 10)
  assert_eq(azimuth::AsyncExporterConfig::max_export_interval_millis(exporter_config), 5000)
  assert_eq(azimuth::AsyncExporterConfig::max_retry_attempts(exporter_config), 3)
  
  // 验证所有Span都被创建
  assert_eq(spans.length(), 100)
}

// 测试2: 遥测数据采样策略
pub test "遥测数据采样策略测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "sampling-test")
  
  // 配置基于概率的采样器
  let probability_sampler = azimuth::ProbabilitySampler::new(0.5)  // 50%采样率
  
  // 配置基于阈值的采样器
  let rate_limiting_sampler = azimuth::RateLimitingSampler::new(100)  // 每秒最多100个采样
  
  // 测试概率采样
  let sampled_spans = []
  let total_spans = 1000
  
  for i in 0..total_spans {
    let span_name = "prob-sampled-span-" + i.to_string()
    let sampling_decision = azimuth::ProbabilitySampler::should_sample(
      probability_sampler, 
      "trace-" + i.to_string(), 
      span_name, 
      azimuth::SpanKind::Internal
    )
    
    if azimuth::SamplingDecision::decision(sampling_decision) == azimuth::RecordAndSample {
      sampled_spans.push(span_name)
    }
  }
  
  // 验证采样率在合理范围内 (45%-55%)
  let actual_sampling_rate = sampled_spans.length().to_double() / total_spans.to_double()
  assert_true(actual_sampling_rate >= 0.45 && actual_sampling_rate <= 0.55)
  
  // 测试速率限制采样
  let rate_limited_spans = []
  let start_time = azimuth::Clock::now_unix_millis(azimuth::Clock::system())
  
  for i in 0..150 {  // 超过速率限制
    let span_name = "rate-limited-span-" + i.to_string()
    let sampling_decision = azimuth::RateLimitingSampler::should_sample(
      rate_limiting_sampler, 
      "trace-rate-" + i.to_string(), 
      span_name, 
      azimuth::SpanKind::Internal
    )
    
    if azimuth::SamplingDecision::decision(sampling_decision) == azimuth::RecordAndSample {
      rate_limited_spans.push(span_name)
    }
  }
  
  // 验证速率限制生效
  assert_true(rate_limited_spans.length() <= 100)
}

// 测试3: 多语言国际化支持
pub test "多语言国际化支持测试" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "i18n-test")
  
  // 测试中文本地化
  let chinese_localizer = azimuth::Localizer::new("zh-CN")
  assert_eq(azimuth::Localizer::get_message(chinese_localizer, "span.started"), "跨度已启动")
  assert_eq(azimuth::Localizer::get_message(chinese_localizer, "metric.recorded"), "指标已记录")
  assert_eq(azimuth::Localizer::get_message(chinese_localizer, "log.emitted"), "日志已发出")
  
  // 测试英文本地化
  let english_localizer = azimuth::Localizer::new("en-US")
  assert_eq(azimuth::Localizer::get_message(english_localizer, "span.started"), "Span started")
  assert_eq(azimuth::Localizer::get_message(english_localizer, "metric.recorded"), "Metric recorded")
  assert_eq(azimuth::Localizer::get_message(english_localizer, "log.emitted"), "Log emitted")
  
  // 测试日文本地化
  let japanese_localizer = azimuth::Localizer::new("ja-JP")
  assert_eq(azimuth::Localizer::get_message(japanese_localizer, "span.started"), "スパンが開始されました")
  assert_eq(azimuth::Localizer::get_message(japanese_localizer, "metric.recorded"), "メトリックが記録されました")
  assert_eq(azimuth::Localizer::get_message(japanese_localizer, "log.emitted"), "ログが出力されました")
  
  // 测试多语言日志记录
  let multilingual_log = azimuth::LogRecord::new_with_attributes(
    azimuth::Info,
    Some("操作完成"),
    Some([
      ("message.zh", azimuth::StringValue("操作已完成")),
      ("message.en", azimuth::StringValue("Operation completed")),
      ("message.ja", azimuth::StringValue("操作が完了しました")),
      ("locale", azimuth::StringValue("zh-CN"))
    ])
  )
  
  azimuth::Logger::emit(logger, multilingual_log)
  
  // 验证日志记录创建成功
  assert_eq(azimuth::LogRecord::severity_number(multilingual_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(multilingual_log), Some("操作完成"))
}

// 测试4: 资源限制下的遥测行为
pub test "资源限制下的遥测行为测试" {
  // 模拟资源受限环境
  let resource_limiter = azimuth::ResourceLimiter::new(
    1000,    // 最大Span数量
    500,     // 最大属性数量
    100,     // 最大事件数量
    50       // 最大链接数量
  )
  
  let tracer_provider = azimuth::TracerProvider::with_resource_limiter(resource_limiter)
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "resource-limited-test")
  
  // 测试Span数量限制
  let spans = []
  for i in 0..1200 {  // 超过限制
    let span = azimuth::Tracer::start_span(tracer, "resource-limited-span-" + i.to_string())
    spans.push(span)
  }
  
  // 验证Span数量被限制
  assert_true(spans.length() <= 1000)
  
  // 测试属性数量限制
  let span = spans[0]
  for i in 0..600 {  // 超过限制
    azimuth::Span::set_attribute(span, "attr-" + i.to_string(), azimuth::IntValue(i))
  }
  
  // 验证属性数量被限制
  let attributes = azimuth::Span::attributes(span)
  assert_true(azimuth::Attributes::size(attributes) <= 500)
  
  // 测试事件数量限制
  for i in 0..150 {  // 超过限制
    azimuth::Span::add_event(span, "event-" + i.to_string(), 
      Some([("event.id", azimuth::StringValue(i.to_string()))]))
  }
  
  // 验证事件数量被限制
  let events = azimuth::Span::events(span)
  assert_true(events.length() <= 100)
  
  // 测试内存使用监控
  let memory_monitor = azimuth::MemoryMonitor::new()
  let memory_usage = azimuth::MemoryMonitor::get_current_usage(memory_monitor)
  
  // 验证内存使用在合理范围内
  assert_true(memory_usage.heap_used < 100 * 1024 * 1024)  // 小于100MB
}

// 测试5: 遥测数据压缩优化
pub test "遥测数据压缩优化测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "compression-test")
  
  // 创建具有大量重复数据的Span
  let spans = []
  for i in 0..100 {
    let span = azimuth::Tracer::start_span(tracer, "compression-test-span")
    
    // 添加重复属性以测试压缩效果
    azimuth::Span::set_attribute(span, "service.name", azimuth::StringValue("azimuth-service"))
    azimuth::Span::set_attribute(span, "service.version", azimuth::StringValue("1.0.0"))
    azimuth::Span::set_attribute(span, "environment", azimuth::StringValue("production"))
    azimuth::Span::set_attribute(span, "datacenter", azimuth::StringValue("us-west-1"))
    azimuth::Span::set_attribute(span, "instance.id", azimuth::StringValue("instance-" + (i % 10).to_string()))
    
    // 添加重复事件
    for j in 0..5 {
      azimuth::Span::add_event(span, "common-event", 
        Some([
          ("event.type", azimuth::StringValue("operation")),
          ("component", azimuth::StringValue("database")),
          ("operation", azimuth::StringValue("query"))
        ]))
    }
    
    spans.push(span)
  }
  
  // 配置压缩策略
  let compression_config = azimuth::CompressionConfig::new(
    azimuth::Gzip,  // 压缩算法
    6,              // 压缩级别 (1-9)
    true            // 启用字典压缩
  )
  
  let compressor = azimuth::Compressor::new(compression_config)
  
  // 序列化Span数据
  let serializer = azimuth::SpanSerializer::new()
  let serialized_data = []
  
  for span in spans {
    let serialized = azimuth::SpanSerializer::serialize(serializer, span)
    serialized_data.push(serialized)
  }
  
  // 压缩数据
  let uncompressed_size = 0
  for data in serialized_data {
    uncompressed_size += data.length()
  }
  
  let compressed_data = azimuth::Compressor::compress_batch(compressor, serialized_data)
  let compressed_size = compressed_data.length()
  
  // 验证压缩效果
  let compression_ratio = compressed_size.to_double() / uncompressed_size.to_double()
  assert_true(compression_ratio < 0.7)  // 压缩率至少30%
  
  // 验证压缩配置
  assert_eq(azimuth::CompressionConfig::algorithm(compression_config), azimuth::Gzip)
  assert_eq(azimuth::CompressionConfig::compression_level(compression_config), 6)
  assert_true(azimuth::CompressionConfig::dictionary_enabled(compression_config))
}

// 测试6: 跨服务一致性边界测试
pub test "跨服务一致性边界测试" {
  // 模拟微服务架构中的遥测一致性
  
  // 服务A: API Gateway
  let gateway_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "api-gateway")
  let gateway_span = azimuth::Tracer::start_span(gateway_tracer, "gateway.request")
  let gateway_trace_id = azimuth::SpanContext::trace_id(azimuth::Span::span_context(gateway_span))
  
  // 设置全局一致性标记
  azimuth::Span::set_attribute(gateway_span, "consistency.version", azimuth::StringValue("1.0"))
  azimuth::Span::set_attribute(gateway_span, "global.trace.id", azimuth::StringValue(gateway_trace_id))
  
  // 服务B: User Service
  let user_service_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "user-service")
  let user_span_ctx = azimuth::SpanContext::new(gateway_trace_id, "user-service-span", true, "")
  let user_span = azimuth::Span::new("user.service.operation", azimuth::Internal, user_span_ctx)
  
  // 验证Trace ID一致性
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(user_span)), gateway_trace_id)
  
  // 设置服务间一致性标记
  azimuth::Span::set_attribute(user_span, "consistency.version", azimuth::StringValue("1.0"))
  azimuth::Span::set_attribute(user_span, "parent.service", azimuth::StringValue("api-gateway"))
  azimuth::Span::set_attribute(user_span, "service.chain", azimuth::StringValue("api-gateway->user-service"))
  
  // 服务C: Order Service
  let order_service_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "order-service")
  let order_span_ctx = azimuth::SpanContext::new(gateway_trace_id, "order-service-span", true, "")
  let order_span = azimuth::Span::new("order.service.operation", azimuth::Internal, order_span_ctx)
  
  // 验证Trace ID一致性
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(order_span)), gateway_trace_id)
  
  // 设置服务间一致性标记
  azimuth::Span::set_attribute(order_span, "consistency.version", azimuth::StringValue("1.0"))
  azimuth::Span::set_attribute(order_span, "parent.service", azimuth::StringValue("user-service"))
  azimuth::Span::set_attribute(order_span, "service.chain", azimuth::StringValue("api-gateway->user-service->order-service"))
  
  // 验证一致性标记
  let gateway_attrs = azimuth::Span::attributes(gateway_span)
  let user_attrs = azimuth::Span::attributes(user_span)
  let order_attrs = azimuth::Span::attributes(order_span)
  
  assert_eq(azimuth::Attributes::get(gateway_attrs, "consistency.version"), 
    azimuth::Attributes::get(user_attrs, "consistency.version"))
  assert_eq(azimuth::Attributes::get(user_attrs, "consistency.version"), 
    azimuth::Attributes::get(order_attrs, "consistency.version"))
  
  // 验证服务链
  assert_eq(azimuth::Attributes::get(order_attrs, "service.chain"), 
    Some(azimuth::StringValue("api-gateway->user-service->order-service")))
  
  // 结束所有Span
  azimuth::Span::end(order_span)
  azimuth::Span::end(user_span)
  azimuth::Span::end(gateway_span)
}

// 测试7: 实时流处理遥测测试
pub test "实时流处理遥测测试" {
  // 模拟实时流处理场景的遥测
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "stream-processing")
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "stream-metrics")
  
  // 创建流处理度量
  let throughput_counter = azimuth::Meter::create_counter(meter, "stream.throughput")
  let latency_histogram = azimuth::Meter::create_histogram(meter, "stream.latency")
  let error_counter = azimuth::Meter::create_counter(meter, "stream.errors")
  let backlog_gauge = azimuth::Meter::create_gauge(meter, "stream.backlog")
  
  // 模拟实时流处理
  let stream_processor = azimuth::StreamProcessor::new()
  let processing_spans = []
  
  for batch_id in 0..50 {
    // 开始处理批次
    let batch_span = azimuth::Tracer::start_span(tracer, "process-batch-" + batch_id.to_string())
    azimuth::Span::set_attribute(batch_span, "batch.id", azimuth::IntValue(batch_id))
    azimuth::Span::set_attribute(batch_span, "batch.size", azimuth::IntValue(1000))
    
    let batch_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    // 模拟处理记录
    let processed_count = 0
    let error_count = 0
    
    for record_id in 0..1000 {
      // 模拟处理延迟
      let processing_time = 1.0 + (@random() * 5.0)
      azimuth::Histogram::record(latency_histogram, processing_time)
      
      // 模拟处理错误
      if @random() < 0.01 {  // 1%错误率
        error_count += 1
        azimuth::Counter::add(error_counter, 1.0)
      } else {
        processed_count += 1
      }
    }
    
    let batch_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let batch_duration = (batch_end_time - batch_start_time) / 1000000.0  // 转换为毫秒
    
    // 记录批次处理结果
    azimuth::Span::add_event(batch_span, "batch.completed", 
      Some([
        ("processed.count", azimuth::IntValue(processed_count)),
        ("error.count", azimuth::IntValue(error_count)),
        ("duration.ms", azimuth::FloatValue(batch_duration))
      ]))
    
    // 更新吞吐量
    azimuth::Counter::add(throughput_counter, processed_count.to_double())
    
    // 更新积压量
    let backlog = @random() * 100.0
    azimuth::Gauge::record(backlog_gauge, backlog)
    
    // 结束批次Span
    azimuth::Span::end(batch_span)
    processing_spans.push(batch_span)
  }
  
  // 验证流处理度量
  assert_eq(throughput_counter.name, "stream.throughput")
  assert_eq(latency_histogram.name, "stream.latency")
  assert_eq(error_counter.name, "stream.errors")
  assert_eq(backlog_gauge.name, "stream.backlog")
  
  // 验证所有批次都被处理
  assert_eq(processing_spans.length(), 50)
}

// 测试8: 遥测系统故障恢复测试
pub test "遥测系统故障恢复测试" {
  // 模拟遥测系统故障和恢复场景
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "fault-tolerance-test")
  
  // 创建故障注入器
  let fault_injector = azimuth::FaultInjector::new()
  
  // 配置故障场景
  azimuth::FaultInjector::configure_network_failure(fault_injector, 0.1)  // 10%网络故障率
  azimuth::FaultInjector::configure_storage_failure(fault_injector, 0.05)  // 5%存储故障率
  azimuth::FaultInjector::configure_memory_pressure(fault_injector, 0.02)  // 2%内存压力
  
  // 创建具有故障恢复能力的导出器
  let resilient_exporter = azimuth::ResilientExporter::new(
    5,       // 最大重试次数
    1000,    // 初始重试间隔(ms)
    2.0,     // 退避倍数
    30000    // 最大重试间隔(ms)
  )
  
  let spans = []
  let failed_spans = []
  let recovered_spans = []
  
  // 在故障环境中创建Span
  for i in 0..100 {
    let span = azimuth::Tracer::start_span(tracer, "fault-test-span-" + i.to_string())
    
    // 注入故障
    let fault_result = azimuth::FaultInjector::inject_random_fault(fault_injector)
    
    if azimuth::FaultResult::is_failure(fault_result) {
      // 记录失败的Span
      azimuth::Span::set_status(span, azimuth::Error, 
        Some("Injected fault: " + azimuth::FaultResult::fault_type(fault_result)))
      failed_spans.push(span)
      
      // 尝试恢复
      let recovery_result = azimuth::ResilientExporter::export_with_retry(resilient_exporter, span)
      
      if azimuth::RecoveryResult::is_success(recovery_result) {
        azimuth::Span::set_status(span, azimuth::Ok, Some("Recovered from fault"))
        recovered_spans.push(span)
      }
    } else {
      // 正常处理
      azimuth::Span::set_status(span, azimuth::Ok, Some("Normal operation"))
    }
    
    spans.push(span)
  }
  
  // 验证故障恢复效果
  let recovery_rate = recovered_spans.length().to_double() / failed_spans.length().to_double()
  assert_true(recovery_rate > 0.8)  // 恢复率至少80%
  
  // 验证所有Span都被创建
  assert_eq(spans.length(), 100)
  
  // 验证故障注入器配置
  assert_eq(azimuth::FaultInjector::network_failure_rate(fault_injector), 0.1)
  assert_eq(azimuth::FaultInjector::storage_failure_rate(fault_injector), 0.05)
  assert_eq(azimuth::FaultInjector::memory_pressure_rate(fault_injector), 0.02)
  
  // 验证弹性导出器配置
  assert_eq(azimuth::ResilientExporter::max_retry_attempts(resilient_exporter), 5)
  assert_eq(azimuth::ResilientExporter::initial_retry_interval(resilient_exporter), 1000)
  assert_eq(azimuth::ResilientExporter::backoff_multiplier(resilient_exporter), 2.0)
  assert_eq(azimuth::ResilientExporter::max_retry_interval(resilient_exporter), 30000)
}

// 测试9: 长期存储归档测试
pub test "长期存储归档测试" {
  // 模拟遥测数据的长期存储和归档
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "archive-test")
  
  // 创建归档配置
  let archive_config = azimuth::ArchiveConfig::new(
    30,      // 热数据保留天数
    90,      // 温数据保留天数
    365,     // 冷数据保留天数
    azimuth::Parquet,  // 归档格式
    6        // 压缩级别
  )
  
  let archive_manager = azimuth::ArchiveManager::new(archive_config)
  
  // 创建不同时间段的Span数据
  let current_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let day_in_nanos = 24 * 60 * 60 * 1000000000L
  
  let hot_data_spans = []
  let warm_data_spans = []
  let cold_data_spans = []
  
  // 生成热数据 (最近30天)
  for i in 0..300 {
    let span_time = current_time - (i.to_long() * day_in_nanos / 10)
    let span = azimuth::Tracer::start_span_with_time(tracer, "hot-data-span-" + i.to_string(), span_time)
    azimuth::Span::set_attribute(span, "data.tier", azimuth::StringValue("hot"))
    azimuth::Span::set_attribute(span, "days.ago", azimuth::IntValue(i / 10))
    hot_data_spans.push(span)
  }
  
  // 生成温数据 (30-90天前)
  for i in 0..300 {
    let span_time = current_time - ((30 + i / 10).to_long() * day_in_nanos)
    let span = azimuth::Tracer::start_span_with_time(tracer, "warm-data-span-" + i.to_string(), span_time)
    azimuth::Span::set_attribute(span, "data.tier", azimuth::StringValue("warm"))
    azimuth::Span::set_attribute(span, "days.ago", azimuth::IntValue(30 + i / 10))
    warm_data_spans.push(span)
  }
  
  // 生成冷数据 (90-365天前)
  for i in 0..100 {
    let span_time = current_time - ((90 + i).to_long() * day_in_nanos)
    let span = azimuth::Tracer::start_span_with_time(tracer, "cold-data-span-" + i.to_string(), span_time)
    azimuth::Span::set_attribute(span, "data.tier", azimuth::StringValue("cold"))
    azimuth::Span::set_attribute(span, "days.ago", azimuth::IntValue(90 + i))
    cold_data_spans.push(span)
  }
  
  // 执行归档操作
  let hot_archive_result = azimuth::ArchiveManager::archive_spans(archive_manager, hot_data_spans)
  let warm_archive_result = azimuth::ArchiveManager::archive_spans(archive_manager, warm_data_spans)
  let cold_archive_result = azimuth::ArchiveManager::archive_spans(archive_manager, cold_data_spans)
  
  // 验证归档结果
  assert_true(azimuth::ArchiveResult::is_success(hot_archive_result))
  assert_true(azimuth::ArchiveResult::is_success(warm_archive_result))
  assert_true(azimuth::ArchiveResult::is_success(cold_archive_result))
  
  // 验证归档配置
  assert_eq(azimuth::ArchiveConfig::hot_retention_days(archive_config), 30)
  assert_eq(azimuth::ArchiveConfig::warm_retention_days(archive_config), 90)
  assert_eq(azimuth::ArchiveConfig::cold_retention_days(archive_config), 365)
  assert_eq(azimuth::ArchiveConfig::archive_format(archive_config), azimuth::Parquet)
  assert_eq(azimuth::ArchiveConfig::compression_level(archive_config), 6)
  
  // 验证归档统计
  let hot_stats = azimuth::ArchiveResult::statistics(hot_archive_result)
  let warm_stats = azimuth::ArchiveResult::statistics(warm_archive_result)
  let cold_stats = azimuth::ArchiveResult::statistics(cold_archive_result)
  
  assert_eq(azimuth::ArchiveStatistics::span_count(hot_stats), 300)
  assert_eq(azimuth::ArchiveStatistics::span_count(warm_stats), 300)
  assert_eq(azimuth::ArchiveStatistics::span_count(cold_stats), 100)
}

// 测试10: 安全隐私保护测试
pub test "安全隐私保护测试" {
  // 测试遥测数据的安全和隐私保护功能
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "security-test")
  
  // 创建隐私保护配置
  let privacy_config = azimuth::PrivacyConfig::new(
    true,    // 启用PII检测
    true,    // 启用数据脱敏
    true,    // 启用加密
    "AES-256-GCM"  // 加密算法
  )
  
  let privacy_processor = azimuth::PrivacyProcessor::new(privacy_config)
  
  // 创建包含敏感信息的Span
  let sensitive_span = azimuth::Tracer::start_span(tracer, "sensitive-operation")
  
  // 添加各种类型的敏感信息
  azimuth::Span::add_event(sensitive_span, "user.login", 
    Some([
      ("user.email", azimuth::StringValue("user@example.com")),
      ("user.phone", azimuth::StringValue("+1234567890")),
      ("user.ssn", azimuth::StringValue("123-45-6789")),
      ("credit.card", azimuth::StringValue("4111-1111-1111-1111")),
      ("ip.address", azimuth::StringValue("192.168.1.100"))
    ]))
  
  // 添加自定义敏感数据
  azimuth::Span::set_attribute(sensitive_span, "api.key", azimuth::StringValue("sk-1234567890abcdef"))
  azimuth::Span::set_attribute(sensitive_span, "password.hash", azimuth::StringValue("5f4dcc3b5aa765d61d8327deb882cf99"))
  azimuth::Span::set_attribute(sensitive_span, "session.token", azimuth::StringValue("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."))
  
  // 处理隐私保护
  let processed_span = azimuth::PrivacyProcessor::process_span(privacy_processor, sensitive_span)
  
  // 验证敏感数据被正确处理
  let processed_attrs = azimuth::Span::attributes(processed_span)
  let processed_events = azimuth::Span::events(processed_span)
  
  // 验证PII检测
  assert_true(azimuth::PrivacyProcessor::detected_pii(privacy_processor, "user@example.com"))
  assert_true(azimuth::PrivacyProcessor::detected_pii(privacy_processor, "+1234567890"))
  assert_true(azimuth::PrivacyProcessor::detected_pii(privacy_processor, "123-45-6789"))
  assert_true(azimuth::PrivacyProcessor::detected_pii(privacy_processor, "4111-1111-1111-1111"))
  
  // 验证数据脱敏
  let masked_email = azimuth::Attributes::get(processed_attrs, "user.email")
  let masked_phone = azimuth::Attributes::get(processed_attrs, "user.phone")
  let masked_ssn = azimuth::Attributes::get(processed_attrs, "user.ssn")
  
  // 验证脱敏格式 (例如: u***@example.com, +123***6789, ***-**-6789)
  match masked_email {
    Some(azimuth::StringValue(email)) => assert_true(email.contains("***"))
    _ => assert_false(true)
  }
  
  match masked_phone {
    Some(azimuth::StringValue(phone)) => assert_true(phone.contains("***"))
    _ => assert_false(true)
  }
  
  match masked_ssn {
    Some(azimuth::StringValue(ssn)) => assert_true(ssn.contains("***"))
    _ => assert_false(true)
  }
  
  // 验证加密字段
  let encrypted_api_key = azimuth::Attributes::get(processed_attrs, "api.key")
  let encrypted_password = azimuth::Attributes::get(processed_attrs, "password.hash")
  let encrypted_token = azimuth::Attributes::get(processed_attrs, "session.token")
  
  match encrypted_api_key {
    Some(azimuth::StringValue(encrypted)) => assert_true(encrypted != "sk-1234567890abcdef")
    _ => assert_false(true)
  }
  
  match encrypted_password {
    Some(azimuth::StringValue(encrypted)) => assert_true(encrypted != "5f4dcc3b5aa765d61d8327deb882cf99")
    _ => assert_false(true)
  }
  
  match encrypted_token {
    Some(azimuth::StringValue(encrypted)) => assert_true(encrypted != "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...")
    _ => assert_false(true)
  }
  
  // 验证隐私配置
  assert_true(azimuth::PrivacyConfig::pii_detection_enabled(privacy_config))
  assert_true(azimuth::PrivacyConfig::data_masking_enabled(privacy_config))
  assert_true(azimuth::PrivacyConfig::encryption_enabled(privacy_config))
  assert_eq(azimuth::PrivacyConfig::encryption_algorithm(privacy_config), "AES-256-GCM")
  
  // 结束Span
  azimuth::Span::end(processed_span)
}