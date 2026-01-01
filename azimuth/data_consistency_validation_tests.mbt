// 数据一致性验证测试
// 测试遥测系统在各种场景下的数据一致性和完整性

import "azimuth/azimuth"

pub test "Span数据一致性测试" {
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "consistency-tracer")
  
  // 创建一致性验证器
  let validator = azimuth::DataConsistencyValidator::new()
  
  // 测试Span创建和修改的一致性
  let span = azimuth::Tracer::start_span(tracer, "consistency-test-span")
  
  // 记录初始状态
  let initial_state = azimuth::DataConsistencyValidator::capture_span_state(validator, span)
  
  // 添加事件和属性
  azimuth::Span::add_event(span, "test.event1", Some([
    ("event.id", azimuth::IntValue(1)),
    ("event.data", azimuth::StringValue("test-data-1"))
  ]))
  
  azimuth::Span::set_attribute(span, "test.attribute", azimuth::StringValue("test-value"))
  
  // 记录修改后状态
  let modified_state = azimuth::DataConsistencyValidator::capture_span_state(validator, span)
  
  // 验证状态变更的一致性
  let consistency_check = azimuth::DataConsistencyValidator::verify_span_consistency(
    validator, 
    initial_state, 
    modified_state
  )
  
  assert_true(azimuth::ConsistencyCheck::is_consistent(consistency_check))
  
  // 验证事件计数
  let event_count = azimuth::Span::event_count(span)
  assert_eq(event_count, 1)
  
  // 验证属性存在
  let attribute_value = azimuth::Span::get_attribute(span, "test.attribute")
  assert_eq(attribute_value, Some(azimuth::StringValue("test-value")))
  
  // 结束Span并验证最终状态
  azimuth::Span::end(span)
  
  let final_state = azimuth::DataConsistencyValidator::capture_span_state(validator, span)
  let final_consistency = azimuth::DataConsistencyValidator::verify_span_finalization(
    validator, 
    modified_state, 
    final_state
  )
  
  assert_true(azimuth::ConsistencyCheck::is_consistent(final_consistency))
}

pub test "度量数据一致性测试" {
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "consistency-meter")
  
  let counter = azimuth::Meter::create_counter(meter, "consistency.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "consistency.histogram")
  let gauge = azimuth::Meter::create_gauge(meter, "consistency.gauge")
  
  let validator = azimuth::DataConsistencyValidator::new()
  
  // 记录度量初始状态
  let initial_metrics_state = azimuth::DataConsistencyValidator::capture_metrics_state(validator, [counter, histogram, gauge])
  
  // 执行度量操作
  for i in 0..100 {
    azimuth::Counter::add(counter, 1.0)
    azimuth::Histogram::record(histogram, i.to_double())
    
    if (i % 10 == 0) {
      azimuth::Gauge::set(gauge, i.to_double())
    }
  }
  
  // 记录度量更新后状态
  let updated_metrics_state = azimuth::DataConsistencyValidator::capture_metrics_state(validator, [counter, histogram, gauge])
  
  // 验证度量数据一致性
  let metrics_consistency = azimuth::DataConsistencyValidator::verify_metrics_consistency(
    validator, 
    initial_metrics_state, 
    updated_metrics_state
  )
  
  assert_true(azimuth::ConsistencyCheck::is_consistent(metrics_consistency))
  
  // 验证计数器值
  let counter_value = azimuth::Counter::value(counter)
  assert_eq(counter_value, 100.0)
  
  // 验证直方图统计
  let histogram_stats = azimuth::Histogram::stats(histogram)
  assert_eq(azimuth::HistogramStats::count(histogram_stats), 100)
  assert_eq(azimuth::HistogramStats::sum(histogram_stats), 4950.0)  // 0+1+...+99
  
  // 验证仪表值
  let gauge_value = azimuth::Gauge::value(gauge)
  assert_eq(gauge_value, 90.0)  // 最后设置的值
}

pub test "日志数据一致性测试" {
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "consistency-logger")
  
  let validator = azimuth::DataConsistencyValidator::new()
  
  // 记录日志初始状态
  let initial_logs_state = azimuth::DataConsistencyValidator::capture_logs_state(validator, logger)
  
  // 创建不同严重程度的日志记录
  let log_records = []
  let severities = [azimuth::Debug, azimuth::Info, azimuth::Warn, azimuth::Error]
  
  for i in 0..50 {
    let severity = severities[i % severities.length()]
    let log_record = azimuth::LogRecord::new_with_context(
      severity,
      Some("Consistency test log " + i.to_string()),
      Some(azimuth::Attributes::new()),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some("consistency-trace"),
      Some("consistency-span"),
      Some(azimuth::Context::root())
    )
    
    azimuth::Logger::emit(logger, log_record)
    log_records.push(log_record)
  }
  
  // 记录日志更新后状态
  let updated_logs_state = azimuth::DataConsistencyValidator::capture_logs_state(validator, logger)
  
  // 验证日志数据一致性
  let logs_consistency = azimuth::DataConsistencyValidator::verify_logs_consistency(
    validator, 
    initial_logs_state, 
    updated_logs_state
  )
  
  assert_true(azimuth::ConsistencyCheck::is_consistent(logs_consistency))
  
  // 验证日志计数
  let log_count = azimuth::Logger::emitted_count(logger)
  assert_eq(log_count, 50)
  
  // 验证日志严重程度分布
  let severity_counts = azimuth::Logger::severity_counts(logger)
  assert_eq(severity_counts[azimuth::Debug], 13)
  assert_eq(severity_counts[azimuth::Info], 13)
  assert_eq(severity_counts[azimuth::Warn], 12)
  assert_eq(severity_counts[azimuth::Error], 12)
}

pub test "上下文传播一致性测试" {
  let validator = azimuth::DataConsistencyValidator::new()
  
  // 创建传播器
  let propagator = azimuth::CompositePropagator::new([
    azimuth::W3CTraceContextPropagator::new(),
    azimuth::W3CBaggagePropagator::new()
  ])
  
  // 创建原始上下文
  let original_ctx = azimuth::Context::root()
  let key1 = azimuth::ContextKey::new("consistency.key1")
  let key2 = azimuth::ContextKey::new("consistency.key2")
  let key3 = azimuth::ContextKey::new("consistency.key3")
  
  let enriched_ctx = azimuth::Context::with_value(
    azimuth::Context::with_value(
      azimuth::Context::with_value(original_ctx, key1, "value1"),
      key2, 
      "value2"
    ),
    key3,
    "value3"
  )
  
  // 记录上下文初始状态
  let initial_context_state = azimuth::DataConsistencyValidator::capture_context_state(validator, enriched_ctx)
  
  // 注入上下文到载体
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(propagator, enriched_ctx, carrier)
  
  // 从载体提取上下文
  let extracted_ctx = azimuth::CompositePropagator::extract(propagator, carrier)
  
  // 记录提取后的上下文状态
  let extracted_context_state = azimuth::DataConsistencyValidator::capture_context_state(validator, extracted_ctx)
  
  // 验证上下文传播一致性
  let context_consistency = azimuth::DataConsistencyValidator::verify_context_consistency(
    validator, 
    initial_context_state, 
    extracted_context_state
  )
  
  assert_true(azimuth::ConsistencyCheck::is_consistent(context_consistency))
  
  // 验证上下文值
  let extracted_value1 = azimuth::Context::get(extracted_ctx, key1)
  let extracted_value2 = azimuth::Context::get(extracted_ctx, key2)
  let extracted_value3 = azimuth::Context::get(extracted_ctx, key3)
  
  assert_eq(extracted_value1, Some("value1"))
  assert_eq(extracted_value2, Some("value2"))
  assert_eq(extracted_value3, Some("value3"))
}

pub test "资源数据一致性测试" {
  let validator = azimuth::DataConsistencyValidator::new()
  
  // 创建资源
  let resource1 = azimuth::Resource::new()
  let resource2 = azimuth::Resource::new()
  
  // 添加属性到资源1
  let resource1_attrs = [
    ("service.name", azimuth::StringValue("test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-123"))
  ]
  
  let resource1_with_attrs = azimuth::Resource::with_attributes(resource1, resource1_attrs)
  
  // 添加属性到资源2
  let resource2_attrs = [
    ("host.name", azimuth::StringValue("test-host")),
    ("os.type", azimuth::StringValue("linux")),
    ("os.version", azimuth::StringValue("5.4.0"))
  ]
  
  let resource2_with_attrs = azimuth::Resource::with_attributes(resource2, resource2_attrs)
  
  // 记录资源初始状态
  let initial_resources_state = azimuth::DataConsistencyValidator::capture_resources_state(
    validator, 
    [resource1_with_attrs, resource2_with_attrs]
  )
  
  // 合并资源
  let merged_resource = azimuth::Resource::merge(resource1_with_attrs, resource2_with_attrs)
  
  // 记录合并后状态
  let merged_resources_state = azimuth::DataConsistencyValidator::capture_resources_state(
    validator, 
    [merged_resource]
  )
  
  // 验证资源合并一致性
  let resources_consistency = azimuth::DataConsistencyValidator::verify_resources_consistency(
    validator, 
    initial_resources_state, 
    merged_resources_state
  )
  
  assert_true(azimuth::ConsistencyCheck::is_consistent(resources_consistency))
  
  // 验证合并后的资源属性
  let service_name = azimuth::Resource::get_attribute(merged_resource, "service.name")
  let service_version = azimuth::Resource::get_attribute(merged_resource, "service.version")
  let host_name = azimuth::Resource::get_attribute(merged_resource, "host.name")
  let os_type = azimuth::Resource::get_attribute(merged_resource, "os.type")
  
  assert_eq(service_name, Some(azimuth::StringValue("test-service")))
  assert_eq(service_version, Some(azimuth::StringValue("1.0.0")))
  assert_eq(host_name, Some(azimuth::StringValue("test-host")))
  assert_eq(os_type, Some(azimuth::StringValue("linux")))
}

pub test "跨组件数据一致性测试" {
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "cross-component-tracer")
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "cross-component-meter")
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "cross-component-logger")
  
  let validator = azimuth::DataConsistencyValidator::new()
  
  // 记录系统初始状态
  let initial_system_state = azimuth::DataConsistencyValidator::capture_system_state(
    validator, 
    tracer, 
    meter, 
    logger
  )
  
  // 执行跨组件操作
  let span = azimuth::Tracer::start_span(tracer, "cross-component-operation")
  
  // 在Span上下文中记录度量
  let counter = azimuth::Meter::create_counter(meter, "cross-component.counter")
  azimuth::Counter::add(counter, 1.0)
  
  // 在Span上下文中记录日志
  let log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Cross-component operation"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(span))),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(logger, log_record)
  
  // 结束Span
  azimuth::Span::end(span)
  
  // 记录系统最终状态
  let final_system_state = azimuth::DataConsistencyValidator::capture_system_state(
    validator, 
    tracer, 
    meter, 
    logger
  )
  
  // 验证跨组件一致性
  let cross_component_consistency = azimuth::DataConsistencyValidator::verify_cross_component_consistency(
    validator, 
    initial_system_state, 
    final_system_state
  )
  
  assert_true(azimuth::ConsistencyCheck::is_consistent(cross_component_consistency))
  
  // 验证组件间的关联性
  let span_id = azimuth::SpanContext::span_id(azimuth::Span::span_context(span))
  let trace_id = azimuth::SpanContext::trace_id(azimuth::Span::span_context(span))
  
  // 验证日志与Span的关联
  let log_trace_id = azimuth::LogRecord::trace_id(log_record)
  let log_span_id = azimuth::LogRecord::span_id(log_record)
  
  assert_eq(log_trace_id, Some(trace_id))
  assert_eq(log_span_id, Some(span_id))
}

pub test "并发操作数据一致性测试" {
  let validator = azimuth::DataConsistencyValidator::new()
  
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "concurrent-tracer")
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "concurrent-meter")
  
  let counter = azimuth::Meter::create_counter(meter, "concurrent.counter")
  
  // 记录并发操作前的状态
  let pre_concurrent_state = azimuth::DataConsistencyValidator::capture_concurrent_state(
    validator, 
    tracer, 
    meter
  )
  
  // 创建并发任务
  let concurrent_tasks = []
  for thread_id in 0..10 {
    let task = azimuth::ConcurrentTask::new(fn() {
      let spans = []
      
      // 每个任务创建10个Span
      for i in 0..10 {
        let span = azimuth::Tracer::start_span(tracer, "concurrent-span-" + thread_id.to_string() + "-" + i.to_string())
        
        // 每个Span执行5次计数器操作
        for j in 0..5 {
          azimuth::Counter::add(counter, 1.0)
        }
        
        spans.push(span)
      }
      
      // 结束所有Span
      for span in spans {
        azimuth::Span::end(span)
      }
      
      spans.length()
    })
    
    concurrent_tasks.push(task)
  }
  
  // 启动所有并发任务
  for task in concurrent_tasks {
    azimuth::ConcurrentTask::start(task)
  }
  
  // 等待所有任务完成
  let total_spans_created = 0
  for task in concurrent_tasks {
    let result = azimuth::ConcurrentTask::wait_for_completion(task)
    total_spans_created = total_spans_created + result
  }
  
  // 记录并发操作后的状态
  let post_concurrent_state = azimuth::DataConsistencyValidator::capture_concurrent_state(
    validator, 
    tracer, 
    meter
  )
  
  // 验证并发操作一致性
  let concurrent_consistency = azimuth::DataConsistencyValidator::verify_concurrent_consistency(
    validator, 
    pre_concurrent_state, 
    post_concurrent_state
  )
  
  assert_true(azimuth::ConsistencyCheck::is_consistent(concurrent_consistency))
  
  // 验证并发操作结果
  assert_eq(total_spans_created, 100)  // 10个线程 × 10个Span
  
  // 验证计数器值（10个线程 × 10个Span × 5次操作 = 500）
  let counter_value = azimuth::Counter::value(counter)
  assert_eq(counter_value, 500.0)
}

pub test "数据完整性验证测试" {
  let validator = azimuth::DataConsistencyValidator::new()
  
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "integrity-tracer")
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "integrity-meter")
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "integrity-logger")
  
  // 创建完整性校验器
  let integrity_checker = azimuth::IntegrityChecker::new()
  
  // 执行一系列操作并记录校验和
  let spans = []
  let checksums = []
  
  for i in 0..20 {
    let span = azimuth::Tracer::start_span(tracer, "integrity-span-" + i.to_string())
    
    // 添加事件和属性
    azimuth::Span::add_event(span, "integrity.event", Some([
      ("span.index", azimuth::IntValue(i)),
      ("timestamp", azimuth::IntValue(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_int()))
    ]))
    
    azimuth::Span::set_attribute(span, "span.index", azimuth::IntValue(i))
    
    // 计算Span数据的校验和
    let span_checksum = azimuth::IntegrityChecker::calculate_span_checksum(integrity_checker, span)
    checksums.push(span_checksum)
    
    spans.push(span)
  }
  
  // 验证所有Span的完整性
  let integrity_violations = []
  for i in 0..spans.length() {
    let current_checksum = azimuth::IntegrityChecker::calculate_span_checksum(integrity_checker, spans[i])
    let expected_checksum = checksums[i]
    
    if (current_checksum != expected_checksum) {
      integrity_violations.push(i)
    }
  }
  
  // 验证没有完整性违规
  assert_eq(integrity_violations.length(), 0)
  
  // 结束所有Span并重新验证
  for span in spans {
    azimuth::Span::end(span)
  }
  
  // 验证结束后的完整性
  let post_end_violations = []
  for i in 0..spans.length() {
    let current_checksum = azimuth::IntegrityChecker::calculate_span_checksum(integrity_checker, spans[i])
    
    // 结束后的校验和可能会改变，但应该保持一致
    let new_checksum = azimuth::IntegrityChecker::calculate_span_checksum(integrity_checker, spans[i])
    
    if (current_checksum != new_checksum) {
      post_end_violations.push(i)
    }
  }
  
  assert_eq(post_end_violations.length(), 0)
  
  // 生成完整性报告
  let integrity_report = azimuth::DataConsistencyValidator::generate_integrity_report(validator)
  assert_true(azimuth::IntegrityReport::is_valid(integrity_report))
  assert_eq(azimuth::IntegrityReport::verified_spans(integrity_report), 20)
  assert_eq(azimuth::IntegrityReport::integrity_violations(integrity_report), 0)
}