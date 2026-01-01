// 并发资源管理测试
// 测试遥测系统在高并发场景下的资源管理能力

import "azimuth/azimuth"

pub test "多线程Span创建和销毁测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "concurrent-span-test")
  
  let initial_resources = azimuth::ResourceManager::active_resources()
  
  // 创建多个并发任务
  let concurrent_tasks = []
  for thread_id in 0..10 {
    let task = azimuth::ConcurrentTask::new(fn() {
      // 每个任务创建100个Span
      let spans = []
      for i in 0..100 {
        let span_name = "thread-" + thread_id.to_string() + "-span-" + i.to_string()
        let span = azimuth::Tracer::start_span(tracer, span_name)
        
        // 添加属性和事件
        azimuth::Span::add_event(span, "concurrent.event", Some([
          ("thread.id", azimuth::IntValue(thread_id)),
          ("span.index", azimuth::IntValue(i))
        ]))
        
        spans.push(span)
      }
      
      // 随机结束一些Span
      for i in 0..50 {
        let span_index = i * 2  // 结束偶数索引的Span
        if (span_index < spans.length()) {
          azimuth::Span::end(spans[span_index])
        }
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
  
  let peak_resources = azimuth::ResourceManager::peak_resources()
  let final_resources = azimuth::ResourceManager::active_resources()
  
  // 验证并发执行结果
  assert_eq(total_spans_created, 1000)  // 10个线程 × 100个Span
  assert_true(peak_resources > initial_resources)
  assert_true(final_resources <= peak_resources)
  
  // 清理剩余Span
  azimuth::ResourceManager::cleanup_all_spans()
}

pub test "并发度量操作测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "concurrent-metrics")
  
  let counter = azimuth::Meter::create_counter(meter, "concurrent.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "concurrent.histogram")
  let gauge = azimuth::Meter::create_gauge(meter, "concurrent.gauge")
  
  let initial_metrics = azimuth::MetricsCollector::total_metrics()
  
  // 创建多个并发度量任务
  let metric_tasks = []
  for thread_id in 0..20 {
    let counter_task = azimuth::ConcurrentTask::new(fn() {
      for i in 0..500 {
        azimuth::Counter::add(counter, 1.0)
      }
      500
    })
    
    let histogram_task = azimuth::ConcurrentTask::new(fn() {
      for i in 0..500 {
        azimuth::Histogram::record(histogram, i.to_double())
      }
      500
    })
    
    let gauge_task = azimuth::ConcurrentTask::new(fn() {
      for i in 0..100 {
        azimuth::Gauge::set(gauge, i.to_double())
        azimuth::Clock::sleep(1)  // 短暂延迟
      }
      100
    })
    
    metric_tasks.push(counter_task)
    metric_tasks.push(histogram_task)
    metric_tasks.push(gauge_task)
  }
  
  // 启动所有度量任务
  for task in metric_tasks {
    azimuth::ConcurrentTask::start(task)
  }
  
  // 等待所有任务完成
  let total_counter_operations = 0
  let total_histogram_operations = 0
  let total_gauge_operations = 0
  
  for i in 0..metric_tasks.length() {
    let task = metric_tasks[i]
    let result = azimuth::ConcurrentTask::wait_for_completion(task)
    
    if (i % 3 == 0) {
      total_counter_operations = total_counter_operations + result
    } else if (i % 3 == 1) {
      total_histogram_operations = total_histogram_operations + result
    } else {
      total_gauge_operations = total_gauge_operations + result
    }
  }
  
  let final_metrics = azimuth::MetricsCollector::total_metrics()
  
  // 验证并发度量操作结果
  assert_eq(total_counter_operations, 10000)     // 20个线程 × 500次操作
  assert_eq(total_histogram_operations, 10000)   // 20个线程 × 500次操作
  assert_eq(total_gauge_operations, 2000)        // 20个线程 × 100次操作
  assert_true(final_metrics > initial_metrics)
}

pub test "并发日志记录测试" {
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "concurrent-logger")
  
  let initial_log_count = azimuth::LogBuffer::total_logs()
  
  // 创建多个并发日志任务
  let log_tasks = []
  for thread_id in 0..15 {
    let task = azimuth::ConcurrentTask::new(fn() {
      for i in 0..200 {
        let severity = if (i % 4 == 0) azimuth::Error 
                      else if (i % 4 == 1) azimuth::Warn 
                      else if (i % 4 == 2) azimuth::Info 
                      else azimuth::Debug
        
        let log_record = azimuth::LogRecord::new_with_context(
          severity,
          Some("Concurrent log " + thread_id.to_string() + "-" + i.to_string()),
          Some(azimuth::Attributes::new()),
          Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
          None,
          Some("concurrent-trace-" + thread_id.to_string()),
          Some("concurrent-span-" + i.to_string()),
          Some(azimuth::Context::root())
        )
        
        azimuth::Logger::emit(logger, log_record)
      }
      200
    })
    
    log_tasks.push(task)
  }
  
  // 启动所有日志任务
  for task in log_tasks {
    azimuth::ConcurrentTask::start(task)
  }
  
  // 等待所有任务完成
  let total_logs = 0
  for task in log_tasks {
    let result = azimuth::ConcurrentTask::wait_for_completion(task)
    total_logs = total_logs + result
  }
  
  let final_log_count = azimuth::LogBuffer::total_logs()
  
  // 验证并发日志记录结果
  assert_eq(total_logs, 3000)  // 15个线程 × 200条日志
  assert_eq(final_log_count - initial_log_count, 3000)
  
  // 验证日志缓冲区完整性
  let log_buffer = azimuth::LogBuffer::get_all_logs()
  assert_eq(log_buffer.length(), 3000)
}

pub test "并发上下文传播测试" {
  let initial_contexts = azimuth::ContextManager::active_contexts()
  
  // 创建多个并发上下文传播任务
  let context_tasks = []
  for thread_id in 0..8 {
    let task = azimuth::ConcurrentTask::new(fn() {
      let root_ctx = azimuth::Context::root()
      
      // 创建上下文链
      let ctx1 = azimuth::Context::with_value(root_ctx, azimuth::ContextKey::new("key1"), "value1-" + thread_id.to_string())
      let ctx2 = azimuth::Context::with_value(ctx1, azimuth::ContextKey::new("key2"), "value2-" + thread_id.to_string())
      let ctx3 = azimuth::Context::with_value(ctx2, azimuth::ContextKey::new("key3"), "value3-" + thread_id.to_string())
      
      // 创建Baggage
      let baggage = azimuth::Baggage::new()
      let baggage1 = azimuth::Baggage::set_entry(baggage, "baggage1", "baggage-value1-" + thread_id.to_string())
      let baggage2 = azimuth::Baggage::set_entry(baggage1, "baggage2", "baggage-value2-" + thread_id.to_string())
      
      // 验证上下文和Baggage
      let key1 = azimuth::ContextKey::new("key1")
      let key2 = azimuth::ContextKey::new("key2")
      let key3 = azimuth::ContextKey::new("key3")
      
      let value1 = azimuth::Context::get(ctx3, key1)
      let value2 = azimuth::Context::get(ctx3, key2)
      let value3 = azimuth::Context::get(ctx3, key3)
      
      let baggage_value1 = azimuth::Baggage::get_entry(baggage2, "baggage1")
      let baggage_value2 = azimuth::Baggage::get_entry(baggage2, "baggage2")
      
      // 返回验证结果
      (value1, value2, value3, baggage_value1, baggage_value2)
    })
    
    context_tasks.push(task)
  }
  
  // 启动所有上下文任务
  for task in context_tasks {
    azimuth::ConcurrentTask::start(task)
  }
  
  // 等待所有任务完成并验证结果
  for thread_id in 0..context_tasks.length() {
    let task = context_tasks[thread_id]
    let (value1, value2, value3, baggage_value1, baggage_value2) = azimuth::ConcurrentTask::wait_for_completion(task)
    
    assert_eq(value1, Some("value1-" + thread_id.to_string()))
    assert_eq(value2, Some("value2-" + thread_id.to_string()))
    assert_eq(value3, Some("value3-" + thread_id.to_string()))
    assert_eq(baggage_value1, Some("baggage-value1-" + thread_id.to_string()))
    assert_eq(baggage_value2, Some("baggage-value2-" + thread_id.to_string()))
  }
  
  let final_contexts = azimuth::ContextManager::active_contexts()
  
  // 验证上下文清理
  assert_true(final_contexts <= initial_contexts + context_tasks.length())
}

pub test "并发传播器操作测试" {
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let composite_propagator = azimuth::CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 创建多个并发传播任务
  let propagation_tasks = []
  for thread_id in 0..12 {
    let task = azimuth::ConcurrentTask::new(fn() {
      let carrier = azimuth::TextMapCarrier::new()
      let ctx = azimuth::Context::root()
      
      // 每个任务执行多次注入和提取操作
      for i in 0..50 {
        azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
        let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
        
        // 验证提取的上下文
        let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
        assert_true(trace_header.is_some())
      }
      
      50
    })
    
    propagation_tasks.push(task)
  }
  
  // 启动所有传播任务
  for task in propagation_tasks {
    azimuth::ConcurrentTask::start(task)
  }
  
  // 等待所有任务完成
  let total_operations = 0
  for task in propagation_tasks {
    let result = azimuth::ConcurrentTask::wait_for_completion(task)
    total_operations = total_operations + result
  }
  
  // 验证并发传播操作结果
  assert_eq(total_operations, 600)  // 12个线程 × 50次操作
}

pub test "并发资源竞争和死锁检测测试" {
  let resource_manager = azimuth::ResourceManager::new()
  
  // 创建多个并发任务竞争资源
  let competing_tasks = []
  for thread_id in 0..6 {
    let task = azimuth::ConcurrentTask::new(fn() {
      // 每个任务尝试获取多个资源
      let resource1 = azimuth::ResourceManager::acquire_resource(resource_manager, "resource1", 1000)
      let resource2 = azimuth::ResourceManager::acquire_resource(resource_manager, "resource2", 1000)
      
      // 模拟资源使用
      azimuth::Clock::sleep(10)
      
      // 释放资源
      azimuth::ResourceManager::release_resource(resource_manager, resource1)
      azimuth::ResourceManager::release_resource(resource_manager, resource2)
      
      // 尝试获取另一个资源组合
      let resource3 = azimuth::ResourceManager::acquire_resource(resource_manager, "resource3", 1000)
      let resource4 = azimuth::ResourceManager::acquire_resource(resource_manager, "resource4", 1000)
      
      azimuth::Clock::sleep(10)
      
      azimuth::ResourceManager::release_resource(resource_manager, resource3)
      azimuth::ResourceManager::release_resource(resource_manager, resource4)
      
      true
    })
    
    competing_tasks.push(task)
  }
  
  // 启动所有竞争任务
  for task in competing_tasks {
    azimuth::ConcurrentTask::start(task)
  }
  
  // 设置死锁检测超时
  let deadlock_detector = azimuth::DeadlockDetector::new()
  azimuth::DeadlockDetector::start_monitoring(deadlock_detector, 30000)  // 30秒超时
  
  // 等待所有任务完成
  let successful_tasks = 0
  for task in competing_tasks {
    let result = azimuth::ConcurrentTask::wait_for_completion(task)
    if (result) {
      successful_tasks = successful_tasks + 1
    }
  }
  
  // 停止死锁检测
  azimuth::DeadlockDetector::stop_monitoring(deadlock_detector)
  
  // 验证结果
  assert_eq(successful_tasks, 6)  // 所有任务都应该成功完成
  assert_false(azimuth::DeadlockDetector::detected_deadlock(deadlock_detector))
  
  // 验证资源清理
  let locked_resources = azimuth::ResourceManager::locked_resources(resource_manager)
  assert_eq(locked_resources.length(), 0)  // 所有资源都应该被释放
}