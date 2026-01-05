// Azimuth Concurrent Safety and Thread Safety Tests
// 并发安全性和线程安全测试用例 - 测试系统在高并发环境下的安全性和线程安全性

// Test 1: 并发属性操作线程安全性
test "concurrent attributes operations thread safety" {
  let shared_attributes = ThreadSafeAttributes::new()
  let num_threads = 10
  let operations_per_thread = 100
  
  // 创建多个线程并发操作共享属性
  let threads = []
  for thread_id in 1..=num_threads {
    let thread = Thread::spawn(|| {
      for i in 1..=operations_per_thread {
        let key = "thread_" + thread_id.to_string() + "_key_" + i.to_string()
        let value = "thread_" + thread_id.to_string() + "_value_" + i.to_string()
        
        // 并发设置属性
        ThreadSafeAttributes::set(shared_attributes, key, StringValue(value))
        
        // 并发获取属性
        let retrieved = ThreadSafeAttributes::get(shared_attributes, key)
        match retrieved {
          Some(StringValue(v)) => assert_eq(v, value)
          _ => assert_true(false)
        }
        
        // 随机删除一些属性
        if i % 10 == 0 {
          ThreadSafeAttributes::remove(shared_attributes, key)
        }
      }
    })
    threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证最终状态一致性
  let final_keys = ThreadSafeAttributes::get_all_keys(shared_attributes)
  let expected_min_keys = (num_threads * operations_per_thread) - (num_threads * (operations_per_thread / 10))
  assert_true(final_keys.length() >= expected_min_keys * 0.9) // 允许10%的误差
  
  // 验证属性完整性
  for key in final_keys {
    let value = ThreadSafeAttributes::get(shared_attributes, key)
    match value {
      Some(_) => assert_true(true) // 值存在
      None => assert_true(false)    // 值不应该丢失
    }
  }
}

// Test 2: 并发跨度操作线程安全性
test "concurrent span operations thread safety" {
  let span_registry = ThreadSafeSpanRegistry::new()
  let num_threads = 8
  let spans_per_thread = 50
  
  // 创建多个线程并发创建和操作跨度
  let threads = []
  for thread_id in 1..=num_threads {
    let thread = Thread::spawn(|| {
      for i in 1..=spans_per_thread {
        let trace_id = "trace_" + thread_id.to_string()
        let span_id = "span_" + thread_id.to_string() + "_" + i.to_string()
        let span_ctx = SpanContext::new(trace_id, span_id, true, "")
        
        // 并发创建跨度
        let span = Span::new("concurrent_span", Internal, span_ctx)
        
        // 注册跨度到共享注册表
        ThreadSafeSpanRegistry::register(span_registry, span_id, span)
        
        // 并发添加事件
        Span::add_event(span, "event_" + i.to_string(), Some([
          ("thread_id", IntValue(thread_id)),
          ("event_id", IntValue(i))
        ]))
        
        // 并发设置状态
        if i % 5 == 0 {
          Span::set_status(span, Ok, Some("Periodic success"))
        }
        
        // 随机结束一些跨度
        if i % 7 == 0 {
          Span::end(span)
        }
      }
    })
    threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证跨度注册表完整性
  let registered_spans = ThreadSafeSpanRegistry::get_all_spans(span_registry)
  assert_true(registered_spans.length() > 0)
  
  // 验证每个跨度的完整性
  for (span_id, span) in registered_spans {
    assert_eq(Span::name(span), "concurrent_span")
    assert_true(Span::span_context(span).span_id == span_id)
  }
}

// Test 3: 并发指标操作线程安全性
test "concurrent metrics operations thread safety" {
  let provider = ThreadSafeMeterProvider::new()
  let meter = ThreadSafeMeterProvider::get_meter(provider, "concurrent_test_meter")
  let counter = ThreadSafeMeter::create_counter(meter, "concurrent_counter", Some("Concurrent counter"), Some("count"))
  
  let num_threads = 12
  let increments_per_thread = 1000
  
  // 创建多个线程并发增加计数器
  let threads = []
  for thread_id in 1..=num_threads {
    let thread = Thread::spawn(|| {
      for i in 1..=increments_per_thread {
        let attrs = Attributes::new()
        Attributes::set(attrs, "thread_id", IntValue(thread_id))
        Attributes::set(attrs, "iteration", IntValue(i))
        
        // 并发增加计数器
        ThreadSafeCounter::add(counter, 1.0, Some(attrs))
        
        // 偶尔使用批量操作
        if i % 100 == 0 {
          ThreadSafeCounter::add(counter, 10.0, Some(attrs))
        }
      }
    })
    threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证最终计数器值
  let expected_min_value = (num_threads * increments_per_thread).to_float()
  let expected_max_value = expected_min_value + (num_threads * (increments_per_thread / 100) * 10.0).to_float()
  let actual_value = ThreadSafeCounter::get_value(counter)
  
  assert_true(actual_value >= expected_min_value)
  assert_true(actual_value <= expected_max_value)
  
  // 验证属性一致性
  let all_attributes = ThreadSafeCounter::get_all_attributes(counter)
  assert_true(all_attributes.length() > 0)
  
  // 验证每个线程的贡献都被记录
  for thread_id in 1..=num_threads {
    let thread_attrs = ThreadSafeCounter::get_attributes_by_key(counter, "thread_id", thread_id.to_string())
    assert_true(thread_attrs.length() > 0)
  }
}

// Test 4: 并发日志记录线程安全性
test "concurrent logging thread safety" {
  let logger_provider = ThreadSafeLoggerProvider::new()
  let logger = ThreadSafeLoggerProvider::get_logger(logger_provider, "concurrent_test_logger")
  let log_sink = ThreadSafeLogSink::new()
  
  let num_threads = 15
  let logs_per_thread = 200
  
  // 创建多个线程并发记录日志
  let threads = []
  for thread_id in 1..=num_threads {
    let thread = Thread::spawn(|| {
      for i in 1..=logs_per_thread {
        let severity = match i % 4 {
          0 => Debug
          1 => Info
          2 => Warn
          _ => Error
        }
        
        let message = "Thread " + thread_id.to_string() + " log message " + i.to_string()
        let attrs = Attributes::new()
        Attributes::set(attrs, "thread_id", IntValue(thread_id))
        Attributes::set(attrs, "message_id", IntValue(i))
        
        // 并发记录日志
        let log_record = LogRecord::new(severity, message)
        LogRecord::add_attributes(log_record, attrs)
        ThreadSafeLogger::emit_to_sink(logger, log_record, log_sink)
      }
    })
    threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证日志完整性
  let all_logs = ThreadSafeLogSink::get_all_logs(log_sink)
  let expected_log_count = num_threads * logs_per_thread
  assert_eq(all_logs.length(), expected_log_count)
  
  // 验证日志顺序性（按时间戳）
  let sorted_logs = ThreadSafeLogSink::get_logs_sorted_by_timestamp(log_sink)
  for i in 1..=sorted_logs.length() - 1 {
    assert_true(sorted_logs[i-1].timestamp <= sorted_logs[i].timestamp)
  }
  
  // 验证每个线程的日志都被记录
  for thread_id in 1..=num_threads {
    let thread_logs = ThreadSafeLogSink::get_logs_by_attribute(log_sink, "thread_id", thread_id)
    assert_eq(thread_logs.length(), logs_per_thread)
  }
  
  // 验证日志级别分布
  let severity_distribution = ThreadSafeLogSink::get_severity_distribution(log_sink)
  assert_true(severity_distribution.contains(Debug))
  assert_true(severity_distribution.contains(Info))
  assert_true(severity_distribution.contains(Warn))
  assert_true(severity_distribution.contains(Error))
}

// Test 5: 并发上下文传播线程安全性
test "concurrent context propagation thread safety" {
  let context_manager = ThreadSafeContextManager::new()
  let num_threads = 10
  let context_operations_per_thread = 50
  
  // 创建多个线程并发操作上下文
  let threads = []
  for thread_id in 1..=num_threads {
    let thread = Thread::spawn(|| {
      let root_ctx = Context::root()
      
      for i in 1..=context_operations_per_thread {
        // 创建带值的上下文
        let key = ContextKey::new("thread_" + thread_id.to_string() + "_key_" + i.to_string())
        let value = "thread_" + thread_id.to_string() + "_value_" + i.to_string()
        let ctx_with_value = Context::with_value(root_ctx, key, value)
        
        // 线程安全地存储上下文
        ThreadSafeContextManager::store_context(context_manager, thread_id.to_string() + "_" + i.to_string(), ctx_with_value)
        
        // 线程安全地检索上下文
        let retrieved_ctx = ThreadSafeContextManager::get_context(context_manager, thread_id.to_string() + "_" + i.to_string())
        match retrieved_ctx {
          Some(ctx) => {
            let retrieved_value = Context::get(ctx, key)
            match retrieved_value {
              Some(v) => assert_eq(v, value)
              None => assert_true(false)
            }
          }
          None => assert_true(false)
        }
        
        // 随机删除一些上下文
        if i % 7 == 0 {
          ThreadSafeContextManager::remove_context(context_manager, thread_id.to_string() + "_" + i.to_string())
        }
      }
    })
    threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证上下文管理器完整性
  let all_contexts = ThreadSafeContextManager::get_all_contexts(context_manager)
  let expected_min_contexts = (num_threads * context_operations_per_thread) - (num_threads * (context_operations_per_thread / 7))
  assert_true(all_contexts.length() >= expected_min_contexts * 0.9) // 允许10%的误差
  
  // 验证上下文内容完整性
  for (context_id, ctx) in all_contexts {
    let parts = String::split(context_id, "_")
    if parts.length() >= 3 {
      let thread_id = parts[1]
      let operation_id = parts[2]
      let key = ContextKey::new("thread_" + thread_id + "_key_" + operation_id)
      let value = "thread_" + thread_id + "_value_" + operation_id
      
      let retrieved_value = Context::get(ctx, key)
      match retrieved_value {
        Some(v) => assert_eq(v, value)
        None => assert_true(false)
      }
    }
  }
}

// Test 6: 并发资源管理线程安全性
test "concurrent resource management thread safety" {
  let resource_pool = ThreadSafeResourcePool::new(100) // 100个资源的池
  let num_threads = 20
  let operations_per_thread = 50
  
  // 创建多个线程并发使用资源池
  let threads = []
  for thread_id in 1..=num_threads {
    let thread = Thread::spawn(|| {
      for i in 1..=operations_per_thread {
        // 线程安全地获取资源
        let resource = ThreadSafeResourcePool::acquire(resource_pool)
        match resource {
          Some(res) => {
            // 使用资源
            Resource::use_resource(res, "thread_" + thread_id.to_string() + "_operation_" + i.to_string())
            
            // 模拟一些处理时间
            Time::sleep(1)
            
            // 线程安全地释放资源
            ThreadSafeResourcePool::release(resource_pool, res)
          }
          None => {
            // 资源池耗尽，这是可能的
            assert_true(true)
          }
        }
      }
    })
    threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证资源池状态
  let pool_stats = ThreadSafeResourcePool::get_statistics(resource_pool)
  assert_eq(pool_stats.total_resources, 100)
  assert_eq(pool_stats.available_resources, 100) // 所有资源应该被释放
  
  // 验证资源使用历史
  let usage_history = ThreadSafeResourcePool::get_usage_history(resource_pool)
  assert_true(usage_history.length() > 0)
  
  // 验证没有资源泄漏
  let leaked_resources = ThreadSafeResourcePool::detect_leaked_resources(resource_pool)
  assert_eq(leaked_resources.length(), 0)
}

// Test 7: 并发行李操作线程安全性
test "concurrent baggage operations thread safety" {
  let baggage_manager = ThreadSafeBaggageManager::new()
  let num_threads = 8
  let baggage_operations_per_thread = 30
  
  // 创建多个线程并发操作行李
  let threads = []
  for thread_id in 1..=num_threads {
    let thread = Thread::spawn(|| {
      let baggage = Baggage::new()
      
      for i in 1..=baggage_operations_per_thread {
        let key = "thread_" + thread_id.to_string() + "_key_" + i.to_string()
        let value = "thread_" + thread_id.to_string() + "_value_" + i.to_string()
        
        // 线程安全地设置行李条目
        let updated_baggage = ThreadSafeBaggageManager::set_entry(baggage_manager, baggage, key, value)
        
        // 线程安全地获取行李条目
        let retrieved_value = ThreadSafeBaggageManager::get_entry(baggage_manager, updated_baggage, key)
        match retrieved_value {
          Some(v) => assert_eq(v, value)
          None => assert_true(false)
        }
        
        // 随机删除一些条目
        if i % 5 == 0 {
          let baggage_after_removal = ThreadSafeBaggageManager::remove_entry(baggage_manager, updated_baggage, key)
          let value_after_removal = ThreadSafeBaggageManager::get_entry(baggage_manager, baggage_after_removal, key)
          match value_after_removal {
            Some(_) => assert_true(false) // 应该被删除
            None => assert_true(true)
          }
        }
      }
    })
    threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证行李管理器状态
  let baggage_stats = ThreadSafeBaggageManager::get_statistics(baggage_manager)
  assert_true(baggage_stats.total_operations > 0)
  assert_true(baggage_stats.successful_operations > 0)
  
  // 验证行李内容完整性
  let all_baggage_entries = ThreadSafeBaggageManager::get_all_entries(baggage_manager)
  for (key, value) in all_baggage_entries {
    let parts = String::split(key, "_")
    if parts.length() >= 4 {
      let thread_id = parts[1]
      let operation_id = parts[3]
      let expected_value = "thread_" + thread_id + "_value_" + operation_id
      assert_eq(value, expected_value)
    }
  }
}

// Test 8: 并发追踪传播线程安全性
test "concurrent trace propagation thread safety" {
  let propagator = ThreadSafeTracePropagator::new()
  let num_threads = 6
  let propagations_per_thread = 20
  
  // 创建多个线程并发进行追踪传播
  let threads = []
  for thread_id in 1..=num_threads {
    let thread = Thread::spawn(|| {
      for i in 1..=propagations_per_thread {
        // 创建追踪上下文
        let trace_id = TraceId::generate()
        let span_id = SpanId::generate()
        let span_ctx = SpanContext::new(trace_id, span_id, true, "thread_" + thread_id.to_string())
        
        // 线程安全地注入到头部
        let headers = ThreadSafeTracePropagator::inject_to_headers(propagator, span_ctx)
        assert_true(headers.contains("traceparent"))
        
        // 线程安全地从头部提取
        let extracted_ctx = ThreadSafeTracePropagator::extract_from_headers(propagator, headers)
        assert_true(SpanContext::is_valid(extracted_ctx))
        assert_eq(SpanContext::trace_id(extracted_ctx), trace_id)
        assert_eq(SpanContext::span_id(extracted_ctx), span_id)
        
        // 线程安全地存储追踪上下文
        ThreadSafeTracePropagator::store_trace_context(propagator, trace_id, span_ctx)
        
        // 线程安全地检索追踪上下文
        let retrieved_ctx = ThreadSafeTracePropagator::get_trace_context(propagator, trace_id)
        match retrieved_ctx {
          Some(ctx) => {
            assert_eq(SpanContext::trace_id(ctx), trace_id)
            assert_eq(SpanContext::span_id(ctx), span_id)
          }
          None => assert_true(false)
        }
      }
    })
    threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证传播器状态
  let propagator_stats = ThreadSafeTracePropagator::get_statistics(propagator)
  assert_true(propagator_stats.total_injections >= num_threads * propagations_per_thread)
  assert_true(propagator_stats.total_extractions >= num_threads * propagations_per_thread)
  assert_eq(propagator_stats.successful_injections, propagator_stats.total_injections)
  assert_eq(propagator_stats.successful_extractions, propagator_stats.total_extractions)
  
  // 验证存储的追踪上下文完整性
  let all_stored_contexts = ThreadSafeTracePropagator::get_all_stored_contexts(propagator)
  for (trace_id, span_ctx) in all_stored_contexts {
    assert_eq(SpanContext::trace_id(span_ctx), trace_id)
    assert_true(SpanContext::is_valid(span_ctx))
  }
}