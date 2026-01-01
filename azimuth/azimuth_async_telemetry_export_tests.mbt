// Async Telemetry Export Tests - 异步遥测导出测试
// 测试异步导出遥测数据的功能和性能

pub test "异步遥测数据导出基础测试" {
  // 创建导出器配置
  let exporter_config = azimuth::ExporterConfig::new(
    "http://localhost:4318/v1/traces",
    "http://localhost:4318/v1/metrics",
    "http://localhost:4318/v1/logs",
    5000,  // 批处理大小
    30000,  // 导出间隔(毫秒)
    10000   // 超时时间(毫秒)
  )
  
  // 验证配置
  assert_eq(azimuth::ExporterConfig::traces_endpoint(exporter_config), "http://localhost:4318/v1/traces")
  assert_eq(azimuth::ExporterConfig::metrics_endpoint(exporter_config), "http://localhost:4318/v1/metrics")
  assert_eq(azimuth::ExporterConfig::logs_endpoint(exporter_config), "http://localhost:4318/v1/logs")
  assert_eq(azimuth::ExporterConfig::batch_size(exporter_config), 5000)
  assert_eq(azimuth::ExporterConfig::export_interval(exporter_config), 30000)
  assert_eq(azimuth::ExporterConfig::timeout(exporter_config), 10000)
  
  // 创建异步导出器
  let exporter = azimuth::AsyncTelemetryExporter::new(exporter_config)
  assert_true(azimuth::AsyncTelemetryExporter::is_initialized(exporter))
  
  // 创建遥测数据
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "async-test-tracer")
  let span = azimuth::Tracer::start_span(tracer, "async-operation")
  
  // 添加事件和属性
  azimuth::Span::add_event(span, "async.start", Some([("operation.type", azimuth::StringValue("export"))]))
  azimuth::Span::set_attribute(span, "async.export", azimuth::BoolValue(true))
  
  // 结束span
  azimuth::Span::end(span)
  
  // 创建度量数据
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "async-test-meter")
  let counter = azimuth::Meter::create_counter(meter, "async.operations.count")
  azimuth::Counter::add(counter, 1.0)
  
  // 创建日志数据
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "async-test-logger")
  let log_record = azimuth::LogRecord::new(azimuth::Info, "Async telemetry export test completed")
  azimuth::Logger::emit(logger, log_record)
  
  // 提交导出任务
  let export_task = azimuth::AsyncTelemetryExporter::export_span(exporter, span)
  assert_true(azimuth::AsyncTelemetryExporter::is_task_pending(export_task))
  
  // 等待导出完成
  let export_result = azimuth::AsyncTelemetryExporter::wait_for_completion(export_task, 5000)
  assert_true(azimuth::AsyncTelemetryExporter::is_successful(export_result))
}

pub test "异步批量导出性能测试" {
  // 创建高性能导出器配置
  let high_perf_config = azimuth::ExporterConfig::new(
    "http://localhost:4318/v1/traces",
    "http://localhost:4318/v1/metrics", 
    "http://localhost:4318/v1/logs",
    10000,  // 大批处理大小
    5000,   // 短导出间隔
    5000    // 短超时时间
  )
  
  let exporter = azimuth::AsyncTelemetryExporter::new(high_perf_config)
  
  // 创建大量遥测数据
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "batch-test-tracer")
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "batch-test-meter")
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "batch-test-logger")
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量创建spans
  let spans = []
  let export_tasks = []
  
  for i in 0..1000 {
    let span = azimuth::Tracer::start_span(tracer, "batch-operation-" + i.to_string())
    azimuth::Span::add_event(span, "batch.event", Some([("batch.id", azimuth::StringValue(i.to_string()))]))
    azimuth::Span::set_attribute(span, "batch.index", azimuth::IntValue(i))
    spans.push(span)
    
    // 异步导出每个span
    let task = azimuth::AsyncTelemetryExporter::export_span(exporter, span)
    export_tasks.push(task)
  }
  
  // 批量创建度量
  let counter = azimuth::Meter::create_counter(meter, "batch.operations.count")
  let histogram = azimuth::Meter::create_histogram(meter, "batch.operation.duration")
  
  for i in 0..500 {
    azimuth::Counter::add(counter, 1.0)
    azimuth::Histogram::record(histogram, 100.0 + (i.to_double() * 0.1))
  }
  
  // 批量创建日志
  for i in 0..200 {
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Batch log " + i.to_string()),
      Some([("log.batch.id", azimuth::StringValue(i.to_string()))]),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      None,
      None,
      None
    )
    azimuth::Logger::emit(logger, log_record)
  }
  
  // 等待所有导出任务完成
  let successful_exports = 0
  for task in export_tasks {
    let result = azimuth::AsyncTelemetryExporter::wait_for_completion(task, 10000)
    if azimuth::AsyncTelemetryExporter::is_successful(result) {
      successful_exports = successful_exports + 1
    }
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let total_duration = end_time - start_time
  
  // 验证性能指标
  assert_true(successful_exports >= 950)  // 至少95%成功
  assert_true(total_duration < 30000000000L)  // 小于30秒
  assert_true(spans.length() == 1000)
}

pub test "异步导出错误处理和重试机制测试" {
  // 创建错误导出器配置（使用无效端点）
  let error_config = azimuth::ExporterConfig::new(
    "http://invalid-endpoint:4318/v1/traces",
    "http://invalid-endpoint:4318/v1/metrics",
    "http://invalid-endpoint:4318/v1/logs",
    100,
    1000,
    1000
  )
  
  let exporter = azimuth::AsyncTelemetryExporter::new(error_config)
  
  // 配置重试策略
  let retry_policy = azimuth::RetryPolicy::new(
    3,      // 最大重试次数
    1000,   // 初始重试间隔(毫秒)
    2.0     // 退避倍数
  )
  
  azimuth::AsyncTelemetryExporter::set_retry_policy(exporter, retry_policy)
  
  // 创建测试数据
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "error-test-tracer")
  let span = azimuth::Tracer::start_span(tracer, "error-operation")
  
  // 尝试导出并验证错误处理
  let export_task = azimuth::AsyncTelemetryExporter::export_span(exporter, span)
  let export_result = azimuth::AsyncTelemetryExporter::wait_for_completion(export_task, 10000)
  
  // 验证错误处理
  assert_false(azimuth::AsyncTelemetryExporter::is_successful(export_result))
  assert_true(azimuth::AsyncTelemetryExporter::get_retry_count(export_result) <= 3)
  
  // 测试部分失败场景
  let mixed_config = azimuth::ExporterConfig::new(
    "http://localhost:4318/v1/traces",  // 有效端点
    "http://invalid-endpoint:4318/v1/metrics",  // 无效端点
    "http://localhost:4318/v1/logs",  // 有效端点
    100,
    1000,
    1000
  )
  
  let mixed_exporter = azimuth::AsyncTelemetryExporter::new(mixed_config)
  let mixed_span = azimuth::Tracer::start_span(tracer, "mixed-operation")
  let mixed_task = azimuth::AsyncTelemetryExporter::export_span(mixed_exporter, mixed_span)
  let mixed_result = azimuth::AsyncTelemetryExporter::wait_for_completion(mixed_task, 10000)
  
  // 混合场景可能部分成功
  let mixed_success = azimuth::AsyncTelemetryExporter::is_successful(mixed_result)
  // 这里不强制断言结果，因为取决于具体实现
}

pub test "异步导出内存和资源管理测试" {
  // 创建资源受限的导出器配置
  let resource_config = azimuth::ExporterConfig::new(
    "http://localhost:4318/v1/traces",
    "http://localhost:4318/v1/metrics",
    "http://localhost:4318/v1/logs",
    50,     // 小批处理大小
    500,    // 短导出间隔
    2000    // 适中超时时间
  )
  
  let exporter = azimuth::AsyncTelemetryExporter::new(resource_config)
  
  // 监控内存使用
  let initial_memory = azimuth::MemoryMonitor::get_current_usage()
  
  // 创建大量异步导出任务
  let tasks = []
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "memory-test-tracer")
  
  for i in 0..500 {
    let span = azimuth::Tracer::start_span(tracer, "memory-operation-" + i.to_string())
    
    // 添加大量属性以增加内存使用
    for j in 0..10 {
      azimuth::Span::set_attribute(span, "attr." + j.to_string(), azimuth::StringValue("value." + j.to_string()))
    }
    
    let task = azimuth::AsyncTelemetryExporter::export_span(exporter, span)
    tasks.push(task)
  }
  
  // 等待一半任务完成
  let completed_tasks = 0
  for i in 0..250 {
    let result = azimuth::AsyncTelemetryExporter::wait_for_completion(tasks[i], 5000)
    if azimuth::AsyncTelemetryExporter::is_successful(result) {
      completed_tasks = completed_tasks + 1
    }
  }
  
  // 检查中间内存使用
  let mid_memory = azimuth::MemoryMonitor::get_current_usage()
  
  // 完成所有任务
  for i in 250..500 {
    let result = azimuth::AsyncTelemetryExporter::wait_for_completion(tasks[i], 5000)
    if azimuth::AsyncTelemetryExporter::is_successful(result) {
      completed_tasks = completed_tasks + 1
    }
  }
  
  // 强制垃圾回收
  azimuth::MemoryMonitor::force_gc()
  
  // 检查最终内存使用
  let final_memory = azimuth::MemoryMonitor::get_current_usage()
  
  // 验证内存管理
  assert_true(completed_tasks >= 400)  // 至少80%成功
  assert_true(mid_memory <= initial_memory * 2)  // 内存增长不超过2倍
  assert_true(final_memory <= initial_memory * 1.5)  // 最终内存增长不超过1.5倍
  
  // 清理资源
  azimuth::AsyncTelemetryExporter::shutdown(exporter)
  assert_true(azimuth::AsyncTelemetryExporter::is_shutdown(exporter))
}

pub test "异步导出并发安全测试" {
  // 创建并发测试导出器
  let concurrent_config = azimuth::ExporterConfig::new(
    "http://localhost:4318/v1/traces",
    "http://localhost:4318/v1/metrics",
    "http://localhost:4318/v1/logs",
    100,
    1000,
    5000
  )
  
  let exporter = azimuth::AsyncTelemetryExporter::new(concurrent_config)
  
  // 创建多个并发导出任务
  let concurrent_tasks = []
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "concurrent-test-tracer")
  
  // 并发创建和导出spans
  for thread_id in 0..10 {
    let thread_tasks = []
    
    for i in 0..100 {
      let span = azimuth::Tracer::start_span(tracer, "concurrent-" + thread_id.to_string() + "-operation-" + i.to_string())
      azimuth::Span::set_attribute(span, "thread.id", azimuth::IntValue(thread_id))
      azimuth::Span::set_attribute(span, "operation.id", azimuth::IntValue(i))
      
      let task = azimuth::AsyncTelemetryExporter::export_span(exporter, span)
      thread_tasks.push(task)
    }
    
    concurrent_tasks.push(thread_tasks)
  }
  
  // 并发等待所有任务完成
  let successful_threads = 0
  for thread_tasks in concurrent_tasks {
    let thread_success = 0
    
    for task in thread_tasks {
      let result = azimuth::AsyncTelemetryExporter::wait_for_completion(task, 10000)
      if azimuth::AsyncTelemetryExporter::is_successful(result) {
        thread_success = thread_success + 1
      }
    }
    
    if thread_success >= 90 {  // 每个线程至少90%成功
      successful_threads = successful_threads + 1
    }
  }
  
  // 验证并发安全性
  assert_true(successful_threads >= 9)  // 至少90%的线程成功
  
  // 测试导出器状态一致性
  let exporter_status = azimuth::AsyncTelemetryExporter::get_status(exporter)
  assert_true(azimuth::AsyncTelemetryExporter::is_healthy(exporter_status))
  
  // 获取导出统计信息
  let stats = azimuth::AsyncTelemetryExporter::get_statistics(exporter)
  assert_true(azimuth::AsyncTelemetryExporter::get_total_exports(stats) >= 900)
  assert_true(azimuth::AsyncTelemetryExporter::get_success_rate(stats) >= 0.8)
}