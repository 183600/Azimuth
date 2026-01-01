// Azimuth Memory Management and Resource Cleanup Tests
// 内存管理和资源清理测试用例
// 测试内存泄漏预防、资源自动清理和生命周期管理

import "azimuth/azimuth"

// Test 1: 内存泄漏预防测试
pub test "内存泄漏预防测试" {
  // 创建大量Span并验证它们能被正确清理
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "memory-leak-test")
  
  // 创建大量Span但不显式结束，测试自动清理
  let spans = []
  for i in 0..1000 {
    let span = azimuth::Tracer::start_span(tracer, "memory-test-span-" + i.to_string())
    spans.push(span)
    
    // 添加属性和事件
    azimuth::Span::add_event(span, "memory.test.event", Some([
      ("iteration", azimuth::IntValue(i)),
      "memory.usage", azimuth::FloatValue(i.to_double() * 0.1)
    ]))
    
    // 设置状态
    if i % 10 == 0 {
      azimuth::Span::set_status(span, azimuth::Error)
    } else {
      azimuth::Span::set_status(span, azimuth::Ok)
    }
  }
  
  // 显式结束部分Span
  for i in 0..500 {
    azimuth::Span::end(spans[i])
  }
  
  // 验证内存使用在合理范围内
  let memory_usage = azimuth::SystemMonitor::get_memory_usage()
  assert_true(memory_usage < 100 * 1024 * 1024)  // 小于100MB
  
  // 强制垃圾回收
  azimuth::SystemMonitor::force_gc()
  
  // 验证内存已释放
  let memory_after_gc = azimuth::SystemMonitor::get_memory_usage()
  assert_true(memory_after_gc < memory_usage * 1.1)  // 内存应该显著减少
}

// Test 2: 资源自动清理测试
pub test "资源自动清理测试" {
  // 测试各种资源的自动清理机制
  let resources = []
  
  // 创建多个资源提供者
  for i in 0..100 {
    let tracer_provider = azimuth::TracerProvider::default()
    let meter_provider = azimuth::MeterProvider::default()
    let logger_provider = azimuth::LoggerProvider::default()
    
    resources.push((tracer_provider, meter_provider, logger_provider))
  }
  
  // 使用资源创建遥测对象
  let telemetry_objects = []
  for (tracer_provider, meter_provider, logger_provider) in resources {
    let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "auto-cleanup-test")
    let meter = azimuth::MeterProvider::get_meter(meter_provider, "auto-cleanup-test")
    let logger = azimuth::LoggerProvider::get_logger(logger_provider, "auto-cleanup-test")
    
    telemetry_objects.push((tracer, meter, logger))
  }
  
  // 创建大量遥测数据
  for (tracer, meter, logger) in telemetry_objects {
    // 创建Span
    let span = azimuth::Tracer::start_span(tracer, "auto-cleanup-span")
    
    // 创建度量
    let counter = azimuth::Meter::create_counter(meter, "auto.cleanup.counter")
    azimuth::Counter::add(counter, 1.0)
    
    // 创建日志
    let log_record = azimuth::LogRecord::new(azimuth::Info, "Auto cleanup test log")
    azimuth::Logger::emit(logger, log_record)
    
    // 不显式结束Span，测试自动清理
  }
  
  // 验证资源计数器
  let active_resources = azimuth::ResourceMonitor::get_active_resources()
  assert_true(active_resources < 50)  // 大部分资源应该已被自动清理
}

// Test 3: 上下文生命周期管理测试
pub test "上下文生命周期管理测试" {
  // 测试上下文的创建、传播和清理
  let contexts = []
  
  // 创建上下文链
  let root_ctx = azimuth::Context::root()
  contexts.push(root_ctx)
  
  // 创建多层嵌套上下文
  let current_ctx = root_ctx
  for i in 0..50 {
    let key = azimuth::ContextKey::new("level." + i.to_string())
    current_ctx = azimuth::Context::with_value(current_ctx, key, "value." + i.to_string())
    contexts.push(current_ctx)
  }
  
  // 验证上下文链的正确性
  for i in 0..50 {
    let key = azimuth::ContextKey::new("level." + i.to_string())
    let value = azimuth::Context::get(contexts[i + 1], key)
    assert_eq(value, Some("value." + i.to_string()))
  }
  
  // 测试上下文的Baggage传播
  let baggage = azimuth::Baggage::new()
  let updated_baggage = azimuth::Baggage::set_entry(baggage, "test.key", "test.value")
  
  // 创建带有Baggage的上下文
  let ctx_with_baggage = azimuth::Context::with_value(current_ctx, 
    azimuth::ContextKey::new("baggage"), updated_baggage)
  
  // 验证Baggage在上下文中的传播
  let retrieved_baggage = azimuth::Context::get(ctx_with_baggage, azimuth::ContextKey::new("baggage"))
  assert_true(retrieved_baggage.is_some())
  
  // 清理上下文引用
  contexts.clear()
  
  // 验证上下文已被清理
  let active_contexts = azimuth::ContextMonitor::get_active_contexts()
  assert_true(active_contexts < 10)  // 大部分上下文应该已被清理
}

// Test 4: 度量数据生命周期测试
pub test "度量数据生命周期测试" {
  // 测试度量数据的创建、聚合和清理
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "lifecycle-test")
  
  // 创建多种类型的度量
  let instruments = []
  
  // 创建Counters
  for i in 0..20 {
    let counter = azimuth::Meter::create_counter(meter, "lifecycle.counter." + i.to_string())
    instruments.push(counter)
  }
  
  // 创建Histograms
  for i in 0..20 {
    let histogram = azimuth::Meter::create_histogram(meter, "lifecycle.histogram." + i.to_string())
    instruments.push(histogram)
  }
  
  // 创建Gauges
  for i in 0..20 {
    let gauge = azimuth::Meter::create_gauge(meter, "lifecycle.gauge." + i.to_string())
    instruments.push(gauge)
  }
  
  // 创建UpDownCounters
  for i in 0..20 {
    let updown_counter = azimuth::Meter::create_updown_counter(meter, "lifecycle.updown." + i.to_string())
    instruments.push(updown_counter)
  }
  
  // 生成大量度量数据
  for instrument in instruments {
    match instrument {
      azimuth::Counter(counter) => {
        for i in 0..100 {
          azimuth::Counter::add(counter, i.to_double())
        }
      }
      azimuth::Histogram(histogram) => {
        for i in 0..100 {
          azimuth::Histogram::record(histogram, i.to_double())
        }
      }
      azimuth::Gauge(gauge) => {
        for i in 0..100 {
          // 模拟Gauge值更新
          azimuth::Gauge::record(gauge, i.to_double())
        }
      }
      azimuth::UpDownCounter(updown_counter) => {
        for i in 0..100 {
          if i % 2 == 0 {
            azimuth::UpDownCounter::add(updown_counter, 1.0)
          } else {
            azimuth::UpDownCounter::add(updown_counter, -1.0)
          }
        }
      }
      _ => {}
    }
  }
  
  // 验证度量数据聚合
  let aggregated_metrics = azimuth::MeterProvider::get_aggregated_metrics(meter_provider)
  assert_true(aggregated_metrics.length() > 0)
  
  // 清理度量引用
  instruments.clear()
  
  // 验证度量数据已被清理
  let metric_memory_usage = azimuth::MetricMonitor::get_memory_usage()
  assert_true(metric_memory_usage < 50 * 1024 * 1024)  // 小于50MB
}

// Test 5: 日志记录生命周期测试
pub test "日志记录生命周期测试" {
  // 测试日志记录的创建、缓冲和清理
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "lifecycle-logger")
  
  // 创建大量日志记录
  let log_records = []
  
  for i in 0..1000 {
    let severity = match i % 6 {
      0 => azimuth::Trace
      1 => azimuth::Debug
      2 => azimuth::Info
      3 => azimuth::Warn
      4 => azimuth::Error
      _ => azimuth::Fatal
    }
    
    let log_record = azimuth::LogRecord::new_with_context(
      severity,
      Some("Lifecycle test log " + i.to_string()),
      Some([("log.index", azimuth::IntValue(i))]),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some("lifecycle-trace-" + (i % 10).to_string()),
      Some("lifecycle-span-" + (i % 100).to_string()),
      Some(azimuth::Context::root())
    )
    
    log_records.push(log_record)
    azimuth::Logger::emit(logger, log_record)
  }
  
  // 验证日志缓冲区大小
  let buffer_size = azimuth::Logger::get_buffer_size(logger)
  assert_true(buffer_size >= 1000)
  
  // 强制刷新日志缓冲区
  azimuth::Logger::flush(logger)
  
  // 验证日志已被处理
  let processed_logs = azimuth::Logger::get_processed_log_count(logger)
  assert_true(processed_logs >= 1000)
  
  // 清理日志记录引用
  log_records.clear()
  
  // 验证日志内存已被清理
  let log_memory_usage = azimuth::LogMonitor::get_memory_usage()
  assert_true(log_memory_usage < 30 * 1024 * 1024)  // 小于30MB
}

// Test 6: 资源限制和监控测试
pub test "资源限制和监控测试" {
  // 测试资源限制和监控机制
  let resource_monitor = azimuth::ResourceMonitor::new()
  
  // 设置资源限制
  azimuth::ResourceMonitor::set_memory_limit(resource_monitor, 200 * 1024 * 1024)  // 200MB
  azimuth::ResourceMonitor::set_span_limit(resource_monitor, 5000)  // 5000个Span
  azimuth::ResourceMonitor::set_metric_limit(resource_monitor, 10000)  // 10000个度量点
  
  // 验证资源限制设置
  assert_eq(azimuth::ResourceMonitor::get_memory_limit(resource_monitor), 200 * 1024 * 1024)
  assert_eq(azimuth::ResourceMonitor::get_span_limit(resource_monitor), 5000)
  assert_eq(azimuth::ResourceMonitor::get_metric_limit(resource_monitor), 10000)
  
  // 创建资源直到接近限制
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "resource-limit-test")
  
  let spans = []
  for i in 0..4500 {  // 接近但不超过Span限制
    let span = azimuth::Tracer::start_span(tracer, "resource-limit-span-" + i.to_string())
    spans.push(span)
  }
  
  // 验证资源使用情况
  let current_span_count = azimuth::ResourceMonitor::get_current_span_count(resource_monitor)
  assert_true(current_span_count <= 4500)
  
  let current_memory_usage = azimuth::ResourceMonitor::get_current_memory_usage(resource_monitor)
  assert_true(current_memory_usage < 200 * 1024 * 1024)  // 不超过内存限制
  
  // 尝试超过资源限制
  let exceeded = false
  for i in 0..1000 {  // 尝试创建更多Span
    let span = azimuth::Tracer::start_span(tracer, "exceed-limit-span-" + i.to_string())
    if azimuth::ResourceMonitor::is_memory_limit_exceeded(resource_monitor) ||
       azimuth::ResourceMonitor::is_span_limit_exceeded(resource_monitor) {
      exceeded = true
      break
    }
    spans.push(span)
  }
  
  // 验证资源限制生效
  assert_true(exceeded)
  
  // 清理资源
  for span in spans {
    azimuth::Span::end(span)
  }
  
  // 验证资源已被释放
  let final_span_count = azimuth::ResourceMonitor::get_current_span_count(resource_monitor)
  assert_true(final_span_count < 1000)  // 大部分Span应该已被清理
}

// Test 7: 异常情况下的资源清理测试
pub test "异常情况下的资源清理测试" {
  // 测试异常情况下的资源清理机制
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "exception-test")
  
  // 创建资源
  let spans = []
  for i in 0..100 {
    let span = azimuth::Tracer::start_span(tracer, "exception-span-" + i.to_string())
    spans.push(span)
  }
  
  // 模拟异常情况
  let exception_occurred = true
  
  // 在异常情况下清理资源
  if exception_occurred {
    // 应该清理所有已创建的资源
    for span in spans {
      try {
        azimuth::Span::end(span)
      } catch {
        _ => {}  // 忽略清理过程中的异常
      }
    }
  }
  
  // 验证资源已被清理
  let active_spans = azimuth::SpanMonitor::get_active_spans()
  assert_true(active_spans < 10)  // 大部分Span应该已被清理
  
  // 测试异常恢复后的资源创建
  let recovery_span = azimuth::Tracer::start_span(tracer, "recovery-span")
  assert_true(azimuth::Span::is_recording(recovery_span))
  azimuth::Span::end(recovery_span)
  
  // 验证系统恢复正常
  let system_health = azimuth::SystemMonitor::get_health_status()
  assert_eq(system_health, azimuth::Healthy)
}

// Test 8: 长期运行稳定性测试
pub test "长期运行稳定性测试" {
  // 测试长期运行情况下的资源管理稳定性
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let duration_minutes = 1  // 1分钟测试（实际应用中可能是几小时或几天）
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "stability-test")
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "stability-test")
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "stability-test")
  
  // 长期运行循环
  let iteration = 0
  while azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - start_time < duration_minutes * 60 * 1000000000L {
    iteration = iteration + 1
    
    // 创建和处理遥测数据
    let span = azimuth::Tracer::start_span(tracer, "stability-span-" + iteration.to_string())
    
    let counter = azimuth::Meter::create_counter(meter, "stability.counter")
    azimuth::Counter::add(counter, 1.0)
    
    let log_record = azimuth::LogRecord::new(azimuth::Info, "Stability test iteration " + iteration.to_string())
    azimuth::Logger::emit(logger, log_record)
    
    azimuth::Span::end(span)
    
    // 定期检查资源使用情况
    if iteration % 100 == 0 {
      let memory_usage = azimuth::SystemMonitor::get_memory_usage()
      assert_true(memory_usage < 150 * 1024 * 1024)  // 内存使用应该在合理范围内
      
      let active_resources = azimuth::ResourceMonitor::get_active_resources()
      assert_true(active_resources < 1000)  // 活跃资源数量应该在合理范围内
    }
    
    // 短暂休眠以避免过度消耗CPU
    azimuth::Clock::sleep(10)  // 10毫秒
  }
  
  // 验证长期运行后的系统状态
  let final_memory_usage = azimuth::SystemMonitor::get_memory_usage()
  assert_true(final_memory_usage < 150 * 1024 * 1024)  // 内存使用应该在合理范围内
  
  let final_active_resources = azimuth::ResourceMonitor::get_active_resources()
  assert_true(final_active_resources < 1000)  // 活跃资源数量应该在合理范围内
  
  let system_health = azimuth::SystemMonitor::get_health_status()
  assert_eq(system_health, azimuth::Healthy)
  
  // 验证遥测数据完整性
  let total_iterations = iteration
  assert_true(total_iterations > 0)
  
  let metric_count = azimuth::MeterProvider::get_metric_count(meter_provider)
  assert_true(metric_count >= total_iterations)
  
  let log_count = azimuth::LoggerProvider::get_log_count(logger_provider)
  assert_true(log_count >= total_iterations)
}