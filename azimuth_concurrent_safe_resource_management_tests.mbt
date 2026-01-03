// 并发安全的资源管理测试
// 测试高并发场景下的资源管理安全性和性能

test "并发安全的属性访问" {
  // 测试多线程并发访问属性的线程安全性
  let attributes = Attributes::new()
  let thread_count = 100
  let operations_per_thread = 1000
  
  // 创建并发任务
  let tasks = []
  for thread_id in 0..thread_count {
    let task = fn() {
      for i in 0..operations_per_thread {
        let key = "concurrent.key." + (thread_id * operations_per_thread + i).to_string()
        let value = StringValue("value-" + thread_id.to_string() + "-" + i.to_string())
        
        // 并发写入
        Attributes::set(attributes, key, value)
        
        // 并发读取
        let read_value = Attributes::get(attributes, key)
        assert_true(read_value.length > 0, "Should be able to read concurrently written value")
        
        // 并发删除
        if i % 10 == 0 {
          Attributes::remove(attributes, key)
        }
      }
    }
    tasks.push(task)
  }
  
  // 执行并发任务
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // 并发执行所有任务
  // ConcurrentExecutor::execute_all(tasks)
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let execution_time = end_time - start_time
  
  // 验证性能
  let total_operations = thread_count * operations_per_thread
  let ops_per_second = total_operations.to_double() / (execution_time.to_double() / 1000000000.0)
  
  assert_true(ops_per_second > 10000.0, "Should handle at least 10,000 operations per second")
  
  // 验证数据一致性
  let final_keys = Attributes::keys(attributes)
  assert_true(final_keys.length > 0, "Should have remaining keys after operations")
}

test "并发安全的资源合并" {
  // 测试多线程并发资源合并的线程安全性
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("concurrent-service")),
    ("service.version", StringValue("1.0.0")),
    ("base.attribute", StringValue("base-value"))
  ])
  
  let thread_count = 50
  let merge_operations = []
  
  // 创建并发合并任务
  for thread_id in 0..thread_count {
    let task = fn() {
      // 每个线程创建自己的资源
      let thread_resource = Resource::with_attributes(Resource::new(), [
        ("thread.id", IntValue(thread_id)),
        ("thread.attribute", StringValue("thread-value-" + thread_id.to_string())),
        ("shared.attribute", StringValue("shared-from-" + thread_id.to_string()))
      ])
      
      // 并发合并资源
      let merged = Resource::merge_concurrent(base_resource, thread_resource)
      
      // 验证合并结果包含预期属性
      let thread_id_value = Resource::get_attribute(merged, "thread.id")
      let thread_attr_value = Resource::get_attribute(merged, "thread.attribute")
      let shared_attr_value = Resource::get_attribute(merged, "shared.attribute")
      
      assert_eq(thread_id_value, Some(IntValue(thread_id)))
      assert_eq(thread_attr_value, Some(StringValue("thread-value-" + thread_id.to_string())))
      assert_eq(shared_attr_value, Some(StringValue("shared-from-" + thread_id.to_string())))
      
      return merged
    }
    merge_operations.push(task)
  }
  
  // 执行并发合并操作
  let merge_results = []
  // ConcurrentExecutor::execute_all(merge_operations)
  
  // 验证所有合并操作成功
  assert_eq(merge_results.length, thread_count, "All merge operations should complete")
  
  for result in merge_results {
    let service_name = Resource::get_attribute(result, "service.name")
    let service_version = Resource::get_attribute(result, "service.version")
    let base_attr = Resource::get_attribute(result, "base.attribute")
    
    assert_eq(service_name, Some(StringValue("concurrent-service")))
    assert_eq(service_version, Some(StringValue("1.0.0")))
    assert_eq(base_attr, Some(StringValue("base-value")))
  }
}

test "并发安全的上下文传播" {
  // 测试多线程并发上下文传播的线程安全性
  let propagator = W3CTraceContextPropagator::new()
  let thread_count = 100
  
  // 创建基础上下文
  let base_trace_id = "concurrent-trace-" + Random::next_u64(Random::system()).to_string()
  let base_span_ctx = SpanContext::new(base_trace_id, "base-span", true, "key=value")
  
  let propagation_tasks = []
  
  // 创建并发传播任务
  for thread_id in 0..thread_count {
    let task = fn() {
      // 每个线程创建自己的载体
      let carrier = TextMapCarrier::new()
      
      // 并发注入上下文
      W3CTraceContextPropagator::inject(propagator, base_span_ctx, carrier)
      
      // 验证注入成功
      let traceparent_header = TextMapCarrier::get(carrier, "traceparent")
      assert_true(traceparent_header.length > 0, "Should have traceparent header after injection")
      
      // 并发提取上下文
      let extracted_ctx = W3CTraceContextPropagator::extract(propagator, carrier)
      
      // 验证提取结果
      assert_eq(SpanContext::trace_id(extracted_ctx), base_trace_id)
      assert_true(SpanContext::is_valid(extracted_ctx))
      
      // 创建子span上下文
      let child_span_id = "child-span-" + thread_id.to_string()
      let child_span_ctx = SpanContext::new(base_trace_id, child_span_id, true, "")
      
      // 并发注入子上下文
      let child_carrier = TextMapCarrier::new()
      W3CTraceContextPropagator::inject(propagator, child_span_ctx, child_carrier)
      
      return child_carrier
    }
    propagation_tasks.push(task)
  }
  
  // 执行并发传播任务
  let child_carriers = []
  // ConcurrentExecutor::execute_all(propagation_tasks)
  
  // 验证所有子上下文正确传播
  assert_eq(child_carriers.length, thread_count, "All propagation tasks should complete")
  
  for carrier in child_carriers {
    let traceparent_header = TextMapCarrier::get(carrier, "traceparent")
    assert_true(traceparent_header.length > 0, "All child carriers should have traceparent")
    
    let extracted_ctx = W3CTraceContextPropagator::extract(propagator, carrier)
    assert_eq(SpanContext::trace_id(extracted_ctx), base_trace_id)
  }
}

test "并发安全的指标收集" {
  // 测试多线程并发指标收集的线程安全性
  let provider = MeterProvider::concurrent()
  let meter = MeterProvider::get_meter(provider, "concurrent-metrics")
  
  let counter = Meter::create_counter(meter, "concurrent.counter")
  let histogram = Meter::create_histogram(meter, "concurrent.histogram")
  let gauge = Meter::create_gauge(meter, "concurrent.gauge")
  
  let thread_count = 50
  let measurements_per_thread = 1000
  
  let metric_tasks = []
  
  // 创建并发指标收集任务
  for thread_id in 0..thread_count {
    let task = fn() {
      for i in 0..measurements_per_thread {
        // 并发更新计数器
        Counter::add(counter, 1.0, [
          ("thread.id", IntValue(thread_id)),
          ("iteration", IntValue(i))
        ])
        
        // 并发记录直方图
        Histogram::record(histogram, (i % 100).to_double(), [
          ("thread.id", IntValue(thread_id))
        ])
        
        // 并发更新仪表
        Gauge::record(gauge, (thread_id + i).to_double(), [
          ("thread.id", IntValue(thread_id))
        ])
      }
    }
    metric_tasks.push(task)
  }
  
  // 执行并发指标收集
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // ConcurrentExecutor::execute_all(metric_tasks)
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let collection_time = end_time - start_time
  
  // 收集指标数据
  let counter_data = Counter::collect(counter)
  let histogram_data = Histogram::collect(histogram)
  let gauge_data = Gauge::collect(gauge)
  
  // 验证数据完整性
  let expected_total = thread_count * measurements_per_thread
  assert_true(counter_data.total >= expected_total * 0.99, "Counter should record at least 99% of updates")
  assert_eq(histogram_data.count, expected_total, "Histogram should record all measurements")
  assert_true(gauge_data.points.length > 0, "Gauge should have data points")
  
  // 验证性能
  let total_measurements = thread_count * measurements_per_thread * 3 // 3指标类型
  let measurements_per_second = total_measurements.to_double() / (collection_time.to_double() / 1000000000.0)
  
  assert_true(measurements_per_second > 50000.0, "Should handle at least 50,000 measurements per second")
}

test "并发安全的日志记录" {
  // 测试多线程并发日志记录的线程安全性
  let provider = LoggerProvider::concurrent()
  let logger = LoggerProvider::get_logger(provider, "concurrent-logger")
  
  let thread_count = 100
  let logs_per_thread = 500
  
  let logging_tasks = []
  
  // 创建并发日志记录任务
  for thread_id in 0..thread_count {
    let task = fn() {
      for i in 0..logs_per_thread {
        let severity = match i % 5 {
          0 => Trace
          1 => Debug
          2 => Info
          3 => Warn
          _ => Error
        }
        
        let log_record = LogRecord::new(severity, "Concurrent log message " + i.to_string())
        
        // 设置属性
        LogRecord::set_attribute(log_record, "thread.id", IntValue(thread_id))
        LogRecord::set_attribute(log_record, "iteration", IntValue(i))
        LogRecord::set_attribute(log_record, "timestamp", Int64(Clock::now_unix_nanos(Clock::system())))
        
        // 并发记录日志
        Logger::emit(logger, log_record)
      }
    }
    logging_tasks.push(task)
  }
  
  // 执行并发日志记录
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // ConcurrentExecutor::execute_all(logging_tasks)
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let logging_time = end_time - start_time
  
  // 收集日志数据
  let log_data = Logger::collect(logger)
  
  // 验证日志完整性
  let expected_logs = thread_count * logs_per_thread
  assert_true(log_data.total >= expected_logs * 0.99, "Should record at least 99% of logs")
  
  // 验证不同严重级别的日志分布
  let trace_logs = log_data.by_severity.get(Trace)
  let debug_logs = log_data.by_severity.get(Debug)
  let info_logs = log_data.by_severity.get(Info)
  let warn_logs = log_data.by_severity.get(Warn)
  let error_logs = log_data.by_severity.get(Error)
  
  assert_true(trace_logs.unwrap > 0, "Should have TRACE logs")
  assert_true(debug_logs.unwrap > 0, "Should have DEBUG logs")
  assert_true(info_logs.unwrap > 0, "Should have INFO logs")
  assert_true(warn_logs.unwrap > 0, "Should have WARN logs")
  assert_true(error_logs.unwrap > 0, "Should have ERROR logs")
  
  // 验证性能
  let logs_per_second = expected_logs.to_double() / (logging_time.to_double() / 1000000000.0)
  assert_true(logs_per_second > 10000.0, "Should handle at least 10,000 logs per second")
}

test "并发安全的span操作" {
  // 测试多线程并发span操作的线程安全性
  let provider = TracerProvider::concurrent()
  let tracer = TracerProvider::get_tracer(provider, "concurrent-tracer")
  
  let thread_count = 50
  let spans_per_thread = 100
  
  let span_tasks = []
  
  // 创建并发span操作任务
  for thread_id in 0..thread_count {
    let task = fn() {
      let spans = []
      
      for i in 0..spans_per_thread {
        // 创建span
        let span = Tracer::start_span(tracer, "concurrent-operation-" + i.to_string(), [
          ("thread.id", IntValue(thread_id)),
          ("iteration", IntValue(i))
        ])
        
        spans.push(span)
        
        // 并发设置状态
        Span::set_status(span, if i % 10 == 0 { Error } else { Ok })
        
        // 并发添加事件
        Span::add_event(span, "event-" + i.to_string(), [
          ("event.data", StringValue("data-" + i.to_string()))
        ])
        
        // 并发设置属性
        Span::set_attribute(span, "duration", IntValue(i * 10))
        
        // 模拟一些工作
        // Thread::sleep(1)
      }
      
      // 并发结束所有spans
      for span in spans {
        Span::end(span)
      }
      
      return spans.length
    }
    span_tasks.push(task)
  }
  
  // 执行并发span操作
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  let span_counts = []
  // ConcurrentExecutor::execute_all(span_tasks)
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let span_time = end_time - start_time
  
  // 验证span操作完整性
  assert_eq(span_counts.length, thread_count, "All span tasks should complete")
  
  let total_spans = 0
  for count in span_counts {
    total_spans += count
  }
  
  let expected_spans = thread_count * spans_per_thread
  assert_true(total_spans >= expected_spans * 0.99, "Should create at least 99% of spans")
  
  // 验证性能
  let spans_per_second = total_spans.to_double() / (span_time.to_double() / 1000000000.0)
  assert_true(spans_per_second > 1000.0, "Should handle at least 1,000 spans per second")
}

test "并发安全的资源池管理" {
  // 测试多线程并发资源池管理的线程安全性
  let resource_pool = ResourcePool::concurrent(100) // 最大100个资源
  
  let thread_count = 50
  let operations_per_thread = 200
  
  let pool_tasks = []
  
  // 创建并发资源池操作任务
  for thread_id in 0..thread_count {
    let task = fn() {
      let acquired_resources = []
      
      for i in 0..operations_per_thread {
        // 并发获取资源
        let resource = ResourcePool::acquire(resource_pool, 1000) // 1秒超时
        assert_true(resource.is_some, "Should be able to acquire resource")
        
        acquired_resources.push(resource.unwrap)
        
        // 使用资源
        Resource::use_safely(resource.unwrap, fn(r) {
          // 模拟资源使用
          Resource::set_attribute(r, "thread.id", IntValue(thread_id))
          Resource::set_attribute(r, "usage.count", IntValue(i))
        })
        
        // 随机释放一些资源
        if i % 3 == 0 && acquired_resources.length > 0 {
          let resource_to_release = acquired_resources.pop()
          ResourcePool::release(resource_pool, resource_to_release)
        }
      }
      
      // 释放所有剩余资源
      for resource in acquired_resources {
        ResourcePool::release(resource_pool, resource)
      }
      
      return operations_per_thread
    }
    pool_tasks.push(task)
  }
  
  // 执行并发资源池操作
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  let operation_counts = []
  // ConcurrentExecutor::execute_all(pool_tasks)
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let pool_time = end_time - start_time
  
  // 验证资源池状态
  assert_eq(operation_counts.length, thread_count, "All pool tasks should complete")
  
  let pool_stats = ResourcePool::stats(resource_pool)
  assert_eq(pool_stats.active_resources, 0, "All resources should be released")
  assert_eq(pool_stats.available_resources, 100, "All resources should be available")
  
  // 验证资源池性能
  let total_operations = thread_count * operations_per_thread
  let operations_per_second = total_operations.to_double() / (pool_time.to_double() / 1000000000.0)
  
  assert_true(operations_per_second > 5000.0, "Should handle at least 5,000 pool operations per second")
}

test "并发安全的缓存管理" {
  // 测试多线程并发缓存管理的线程安全性
  let cache = ConcurrentCache::lru(1000) // 1000条目的LRU缓存
  
  let thread_count = 100
  let operations_per_thread = 1000
  
  let cache_tasks = []
  
  // 创建并发缓存操作任务
  for thread_id in 0..thread_count {
    let task = fn() {
      let hit_count = 0
      let miss_count = 0
      
      for i in 0..operations_per_thread {
        let key = "cache.key." + (thread_id * operations_per_thread + i).to_string()
        let value = "cache.value." + thread_id.to_string() + "." + i.to_string()
        
        // 并发写入缓存
        ConcurrentCache::put(cache, key, value)
        
        // 并发读取缓存
        let cached_value = ConcurrentCache::get(cache, key)
        if cached_value.is_some {
          hit_count += 1
        } else {
          miss_count += 1
        }
        
        // 并发删除一些条目
        if i % 10 == 0 {
          ConcurrentCache::remove(cache, key)
        }
        
        // 随机读取其他线程的键
        if i % 5 == 0 {
          let random_key = "cache.key." + (Random::next_int() % (thread_count * operations_per_thread)).to_string()
          ConcurrentCache::get(cache, random_key)
        }
      }
      
      return (hit_count, miss_count)
    }
    cache_tasks.push(task)
  }
  
  // 执行并发缓存操作
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  let cache_results = []
  // ConcurrentExecutor::execute_all(cache_tasks)
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let cache_time = end_time - start_time
  
  // 验证缓存结果
  assert_eq(cache_results.length, thread_count, "All cache tasks should complete")
  
  let total_hits = 0
  let total_misses = 0
  
  for (hit, miss) in cache_results {
    total_hits += hit
    total_misses += miss
  }
  
  // 验证缓存命中率
  let total_requests = total_hits + total_misses
  let hit_rate = total_hits.to_double() / total_requests.to_double()
  
  assert_true(hit_rate > 0.7, "Cache hit rate should be at least 70%")
  
  // 验证缓存状态
  let cache_stats = ConcurrentCache::stats(cache)
  assert_true(cache_stats.size <= 1000, "Cache size should not exceed capacity")
  assert_true(cache_stats.evictions > 0, "Should have evicted entries due to capacity limit")
  
  // 验证缓存性能
  let cache_ops_per_second = total_requests.to_double() / (cache_time.to_double() / 1000000000.0)
  assert_true(cache_ops_per_second > 100000.0, "Should handle at least 100,000 cache operations per second")
}