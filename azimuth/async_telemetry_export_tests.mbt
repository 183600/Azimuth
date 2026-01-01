// 异步遥测导出测试
// 测试异步导出遥测数据的功能和性能

import "azimuth/azimuth"

pub test "异步遥测导出基础功能测试" {
  // 创建异步导出器
  let exporter = azimuth::AsyncTelemetryExporter::new()
  
  // 创建测试数据
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "async-test")
  let span = azimuth::Tracer::start_span(tracer, "async-operation")
  
  // 设置Span属性和事件
  azimuth::Span::add_event(span, "async.start", Some([("operation", azimuth::StringValue("test"))]))
  azimuth::Span::set_status(span, azimuth::Ok)
  
  // 创建度量数据
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "async-meter")
  let counter = azimuth::Meter::create_counter(meter, "async.operations")
  azimuth::Counter::add(counter, 1.0)
  
  // 创建日志数据
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "async-logger")
  let log_record = azimuth::LogRecord::new(azimuth::Info, "Async telemetry export test")
  
  // 异步导出数据
  let export_result = azimuth::AsyncTelemetryExporter::export(exporter, span, counter, log_record)
  
  // 验证导出结果
  assert_true(azimuth::AsyncTelemetryExporter::is_success(export_result))
  assert_true(azimuth::AsyncTelemetryExporter::export_count(exporter) > 0)
}

pub test "异步导出并发性能测试" {
  let exporter = azimuth::AsyncTelemetryExporter::new()
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "concurrent-test")
  
  // 测试并发导出性能
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 创建多个并发导出任务
  let export_tasks = []
  for i in 0..50 {
    let span = azimuth::Tracer::start_span(tracer, "concurrent-span-" + i.to_string())
    let task = azimuth::AsyncTelemetryExporter::create_export_task(exporter, span)
    export_tasks.push(task)
  }
  
  // 等待所有导出任务完成
  for task in export_tasks {
    azimuth::AsyncTelemetryExporter::wait_for_task(task)
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let duration = end_time - start_time
  
  // 验证性能指标
  assert_true(duration < 5000000000L)  // 小于5秒
  assert_eq(azimuth::AsyncTelemetryExporter::export_count(exporter), 50)
}

pub test "异步导出错误处理和重试机制测试" {
  let exporter = azimuth::AsyncTelemetryExporter::new()
  
  // 模拟网络错误场景
  let failing_exporter = azimuth::AsyncTelemetryExporter::with_failure_simulation(0.3)  // 30%失败率
  
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "retry-test")
  let successful_exports = 0
  let failed_exports = 0
  
  // 执行多次导出，测试重试机制
  for i in 0..20 {
    let span = azimuth::Tracer::start_span(tracer, "retry-span-" + i.to_string())
    let export_result = azimuth::AsyncTelemetryExporter::export_with_retry(failing_exporter, span, 3)  // 最多重试3次
    
    if (azimuth::AsyncTelemetryExporter::is_success(export_result)) {
      successful_exports = successful_exports + 1
    } else {
      failed_exports = failed_exports + 1
    }
  }
  
  // 验证重试机制有效性
  assert_true(successful_exports > 15)  // 至少75%成功
  assert_true(failed_exports < 5)       // 最多25%失败
}

pub test "异步导出内存管理测试" {
  let exporter = azimuth::AsyncTelemetryExporter::new()
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "memory-test")
  
  // 记录初始内存使用
  let initial_memory = azimuth::AsyncTelemetryExporter::memory_usage(exporter)
  
  // 创建大量导出任务
  let large_dataset = []
  for i in 0..1000 {
    let span = azimuth::Tracer::start_span(tracer, "memory-span-" + i.to_string())
    
    // 添加大量属性和事件
    for j in 0..10 {
      azimuth::Span::add_event(span, "event-" + j.to_string(), Some([
        ("iteration", azimuth::StringValue(i.to_string())),
        ("event.index", azimuth::StringValue(j.to_string())),
        ("payload", azimuth::StringValue("large-payload-data-for-memory-testing-" + i.to_string() + "-" + j.to_string()))
      ]))
    }
    
    large_dataset.push(span)
  }
  
  // 批量导出
  for span in large_dataset {
    azimuth::AsyncTelemetryExporter::export(exporter, span)
  }
  
  // 等待所有导出完成并清理内存
  azimuth::AsyncTelemetryExporter::flush_and_cleanup(exporter)
  
  // 验证内存使用在合理范围内
  let final_memory = azimuth::AsyncTelemetryExporter::memory_usage(exporter)
  let memory_increase = final_memory - initial_memory
  
  // 内存增长不应超过初始内存的50%
  assert_true(memory_increase < initial_memory / 2)
}

pub test "异步导出配置和批处理测试" {
  // 创建不同配置的导出器
  let batch_exporter = azimuth::AsyncTelemetryExporter::with_batch_config(100, 5000)  // 批大小100，超时5秒
  let realtime_exporter = azimuth::AsyncTelemetryExporter::with_realtime_config()     // 实时导出
  
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "config-test")
  
  // 测试批处理导出
  let batch_spans = []
  for i in 0..150 {
    let span = azimuth::Tracer::start_span(tracer, "batch-span-" + i.to_string())
    batch_spans.push(span)
  }
  
  let batch_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量导出
  for span in batch_spans {
    azimuth::AsyncTelemetryExporter::export(batch_exporter, span)
  }
  
  // 等待批处理完成
  azimuth::AsyncTelemetryExporter::wait_for_batch_completion(batch_exporter)
  
  let batch_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let batch_duration = batch_end - batch_start
  
  // 测试实时导出
  let realtime_spans = []
  for i in 0..10 {
    let span = azimuth::Tracer::start_span(tracer, "realtime-span-" + i.to_string())
    realtime_spans.push(span)
  }
  
  let realtime_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 实时导出
  for span in realtime_spans {
    azimuth::AsyncTelemetryExporter::export(realtime_exporter, span)
    azimuth::AsyncTelemetryExporter::wait_for_completion(realtime_exporter)  // 等待每个导出完成
  }
  
  let realtime_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let realtime_duration = realtime_end - realtime_start
  
  // 验证配置效果
  assert_true(batch_duration < realtime_duration * 2)  // 批处理应该比实时处理更高效
  assert_eq(azimuth::AsyncTelemetryExporter::export_count(batch_exporter), 150)
  assert_eq(azimuth::AsyncTelemetryExporter::export_count(realtime_exporter), 10)
}