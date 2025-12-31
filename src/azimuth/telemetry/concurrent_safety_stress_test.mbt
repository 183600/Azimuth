// 并发安全性压力测试
// 测试高并发环境下遥测系统的线程安全性和数据一致性

test "concurrent_span_creation_and_modification" {
  // 测试并发span创建和修改的线程安全性
  
  let tracer = create_concurrent_safe_tracer()
  let concurrent_threads = 50
  let operations_per_thread = 100
  
  // 创建并发任务
  let tasks = []
  let created_spans = ConcurrentVector::new()
  
  for thread_id = 0; thread_id < concurrent_threads; thread_id = thread_id + 1 {
    let task = ConcurrentTask::spawn(fn() {
      let thread_spans = []
      
      for i = 0; i < operations_per_thread; i = i + 1 {
        let span_name = "span-" + thread_id.to_string() + "-" + i.to_string()
        let (ctx, span) = tracer.start_span(Context::empty(), span_name)
        
        // 并发修改span属性
        span.set_attribute("thread.id", thread_id.to_int64())
        span.set_attribute("operation.id", i.to_int64())
        span.set_attribute("start.time", get_current_timestamp())
        
        // 模拟一些工作
        @thread.sleep(1) // 1ms
        
        span.set_attribute("end.time", get_current_timestamp())
        span.set_status(Ok)
        
        thread_spans.push(span)
      }
      
      // 将线程创建的spans添加到共享集合
      for span in thread_spans {
        created_spans.push(span)
      }
      
      thread_spans.length()
    })
    
    tasks.push(task)
  }
  
  // 等待所有任务完成
  let total_spans_created = 0L
  for task in tasks {
    let thread_result = task.join()
    total_spans_created = total_spans_created + thread_result.to_int64()
  }
  
  // 验证并发安全性
  assert_eq(total_spans_created, (concurrent_threads * operations_per_thread).to_int64())
  assert_eq(created_spans.length(), concurrent_threads * operations_per_thread)
  
  // 验证数据完整性
  for span in created_spans.to_array() {
    assert_eq(span.has_attribute("thread.id"), true)
    assert_eq(span.has_attribute("operation.id"), true)
    assert_eq(span.has_attribute("start.time"), true)
    assert_eq(span.has_attribute("end.time"), true)
    assert_eq(span.status, Ok)
  }
  
  // 验证没有数据竞争
  let race_conditions_detected = detect_race_conditions(created_spans.to_array())
  assert_eq(race_conditions_detected, 0)
}

test "concurrent_metric_aggregation" {
  // 测试并发指标聚合的线程安全性
  
  let metric_aggregator = ConcurrentMetricAggregator::new()
  let concurrent_producers = 20
  let metrics_per_producer = 500
  
  // 创建多个并发的指标生产者
  let producer_tasks = []
  
  for producer_id = 0; producer_id < concurrent_producers; producer_id = producer_id + 1 {
    let task = ConcurrentTask::spawn(fn() {
      for i = 0; i < metrics_per_producer; i = i + 1 {
        let metric = MetricPoint{
          name: "cpu_usage",
          value: @random.float(100.0),
          timestamp: get_current_timestamp(),
          labels: [
            ("host", "server-" + producer_id.to_string()),
            ("instance", i.to_string())
          ]
        }
        
        // 并发添加指标到聚合器
        metric_aggregator.add_metric(metric)
        
        // 偶尔触发聚合计算
        if i % 50 == 0 {
          let _ = metric_aggregator.get_aggregated_metrics("cpu_usage")
        }
      }
      
      metrics_per_producer
    })
    
    producer_tasks.push(task)
  }
  
  // 创建并发的指标消费者
  let consumer_tasks = []
  let consumed_aggregations = ConcurrentVector::new()
  
  for consumer_id = 0; consumer_id < 5; consumer_id = consumer_id + 1 {
    let task = ConcurrentTask::spawn(fn() {
      let local_aggregations = []
      
      // 持续消费聚合结果
      for i = 0; i < 100; i = i + 1 {
        @thread.sleep(10) // 10ms
        
        let aggregated = metric_aggregator.get_aggregated_metrics("cpu_usage")
        match aggregated {
          Some(result) => {
            local_aggregations.push(result)
          }
          None => {}
        }
      }
      
      // 将本地聚合结果添加到共享集合
      for agg in local_aggregations {
        consumed_aggregations.push(agg)
      }
      
      local_aggregations.length()
    })
    
    consumer_tasks.push(task)
  }
  
  // 等待所有生产者完成
  let total_metrics_produced = 0L
  for task in producer_tasks {
    let result = task.join()
    total_metrics_produced = total_metrics_produced + result.to_int64()
  }
  
  // 等待所有消费者完成
  let total_aggregations_consumed = 0L
  for task in consumer_tasks {
    let result = task.join()
    total_aggregations_consumed = total_aggregations_consumed + result.to_int64()
  }
  
  // 验证并发安全性
  assert_eq(total_metrics_produced, (concurrent_producers * metrics_per_producer).to_int64())
  assert_eq(total_aggregations_consumed > 0, true)
  
  // 验证最终聚合结果的正确性
  let final_aggregation = metric_aggregator.get_aggregated_metrics("cpu_usage")
  match final_aggregation {
    Some(result) => {
      assert_eq(result.count, total_metrics_produced)
      assert_eq(result.avg >= 0.0 && result.avg <= 100.0, true)
      assert_eq(result.min >= 0.0, true)
      assert_eq(result.max <= 100.0, true)
    }
    None => @test.fail("Final aggregation result should be available")
  }
  
  // 验证聚合器内部状态一致性
  let internal_stats = metric_aggregator.get_internal_statistics()
  assert_eq(internal_stats.total_metrics_added, total_metrics_produced)
  assert_eq(internal_stats.concurrent_operations_count > 0, true)
  assert_eq(internal_stats.data_corruption_detected, false)
}

test "concurrent_log_writing" {
  // 测试并发日志写入的线程安全性
  
  let log_writer = ConcurrentLogWriter::new()
  let concurrent_writers = 30
  let logs_per_writer = 200
  
  // 创建多个并发日志写入器
  let writer_tasks = []
  let written_logs = ConcurrentVector::new()
  
  for writer_id = 0; writer_id < concurrent_writers; writer_id = writer_id + 1 {
    let task = ConcurrentTask::spawn(fn() {
      let writer_logs = []
      
      for i = 0; i < logs_per_writer; i = i + 1 {
        let log_record = LogRecord{
          timestamp: get_current_timestamp(),
          level: ["DEBUG", "INFO", "WARN", "ERROR"][i % 4],
          message: "Log from writer " + writer_id.to_string() + " entry " + i.to_string(),
          attributes: [
            ("writer.id", writer_id.to_string()),
            ("entry.id", i.to_string()),
            ("thread.name", @thread.get_current_name())
          ]
        }
        
        // 并发写入日志
        let write_result = log_writer.write_log(log_record)
        match write_result {
          Ok(log_id) => {
            writer_logs.push((log_id, log_record))
          }
          Err(error) => @test.fail("Log write failed: " + error.message)
        }
        
        // 偶尔触发日志刷新
        if i % 25 == 0 {
          log_writer.flush()
        }
      }
      
      // 将写入的日志添加到共享集合
      for log_pair in writer_logs {
        written_logs.push(log_pair)
      }
      
      writer_logs.length()
    })
    
    writer_tasks.push(task)
  }
  
  // 创建并发的日志读取器
  let reader_tasks = []
  let read_logs = ConcurrentVector::new()
  
  for reader_id = 0; reader_id < 3; reader_id = reader_id + 1 {
    let task = ConcurrentTask::spawn(fn() {
      let local_read_logs = []
      
      // 持续读取日志
      for i = 0; i < 50; i = i + 1 {
        @thread.sleep(20) // 20ms
        
        let recent_logs = log_writer.get_recent_logs(100)
        for log in recent_logs {
          local_read_logs.push(log)
        }
      }
      
      // 将读取的日志添加到共享集合
      for log in local_read_logs {
        read_logs.push(log)
      }
      
      local_read_logs.length()
    })
    
    reader_tasks.push(task)
  }
  
  // 等待所有写入器完成
  let total_logs_written = 0L
  for task in writer_tasks {
    let result = task.join()
    total_logs_written = total_logs_written + result.to_int64()
  }
  
  // 等待所有读取器完成
  let total_logs_read = 0L
  for task in reader_tasks {
    let result = task.join()
    total_logs_read = total_logs_read + result.to_int64()
  }
  
  // 最终刷新确保所有日志都写入
  log_writer.flush()
  
  // 验证并发安全性
  assert_eq(total_logs_written, (concurrent_writers * logs_per_writer).to_int64())
  assert_eq(written_logs.length(), concurrent_writers * logs_per_writer)
  assert_eq(total_logs_read > 0, true)
  
  // 验证日志完整性
  let all_written_logs = written_logs.to_array()
  for (log_id, log_record) in all_written_logs {
    assert_eq(log_record.has_attribute("writer.id"), true)
    assert_eq(log_record.has_attribute("entry.id"), true)
    assert_eq(log_record.has_attribute("thread.name"), true)
  }
  
  // 验证没有日志丢失或损坏
  let log_integrity_report = log_writer.verify_integrity()
  assert_eq(log_integrity_report.total_logs, total_logs_written)
  assert_eq(log_integrity_report.corrupted_logs, 0)
  assert_eq(log_integrity_report.duplicate_logs, 0)
}

test "concurrent_context_propagation" {
  // 测试并发上下文传播的线程安全性
  
  let context_manager = ConcurrentContextManager::new()
  let concurrent_operations = 40
  
  // 创建根上下文
  let root_context = context_manager.create_context()
  root_context.set_attribute("trace.id", "trace-12345")
  root_context.set_attribute("operation.type", "concurrent_test")
  
  // 创建并发任务，每个都传播上下文
  let operation_tasks = []
  let propagated_contexts = ConcurrentVector::new()
  
  for operation_id = 0; operation_id < concurrent_operations; operation_id = operation_id + 1 {
    let task = ConcurrentTask::spawn_with_context(root_context, fn(parent_ctx) {
      // 从父上下文创建子上下文
      let child_context = context_manager.create_child_context(parent_ctx)
      child_context.set_attribute("operation.id", operation_id.to_string())
      child_context.set_attribute("thread.id", @thread.get_current_id().to_string())
      
      // 模拟嵌套操作
      for nested_id = 0; nested_id < 5; nested_id = nested_id + 1 {
        let nested_context = context_manager.create_child_context(child_context)
        nested_context.set_attribute("nested.id", nested_id.to_string())
        nested_context.set_attribute("nested.timestamp", get_current_timestamp())
        
        // 验证上下文继承
        assert_eq(nested_context.get_attribute("trace.id"), Some("trace-12345"))
        assert_eq(nested_context.get_attribute("operation.type"), Some("concurrent_test"))
        assert_eq(nested_context.get_attribute("operation.id"), Some(operation_id.to_string()))
        
        // 将上下文添加到共享集合
        propagated_contexts.push(nested_context)
      }
      
      child_context
    })
    
    operation_tasks.push(task)
  }
  
  // 等待所有操作完成
  let completed_operations = []
  for task in operation_tasks {
    let result = task.join()
    completed_operations.push(result)
  }
  
  // 验证并发上下文传播
  assert_eq(completed_operations.length(), concurrent_operations)
  assert_eq(propagated_contexts.length(), concurrent_operations * 5)
  
  // 验证上下文一致性
  let all_contexts = propagated_contexts.to_array()
  for context in all_contexts {
    assert_eq(context.get_attribute("trace.id"), Some("trace-12345"))
    assert_eq(context.get_attribute("operation.type"), Some("concurrent_test"))
    assert_eq(context.has_attribute("operation.id"), true)
    assert_eq(context.has_attribute("nested.id"), true)
    assert_eq(context.has_attribute("nested.timestamp"), true)
  }
  
  // 验证上下文管理器内部状态
  let manager_stats = context_manager.get_statistics()
  assert_eq(manager_stats.total_contexts_created >= (concurrent_operations * 6 + 1), true) // 根上下文 + 操作上下文 + 嵌套上下文
  assert_eq(manager_stats.active_contexts, 0) // 所有上下文应该已清理
  assert_eq(manager_stats.context_propagation_errors, 0)
  assert_eq(manager_stats.memory_leaks_detected, false)
}

test "stress_test_with_resource_contention" {
  // 测试资源竞争条件下的压力测试
  
  let shared_resources = SharedResourceManager::new()
  let stress_threads = 100
  let operations_per_thread = 50
  
  // 创建高竞争的共享资源
  let shared_counter = AtomicCounter::new(0)
  let shared_lock = ReentrantLock::new()
  let shared_data = ConcurrentHashMap::new()
  
  // 创建压力测试任务
  let stress_tasks = []
  
  for thread_id = 0; thread_id < stress_threads; thread_id = thread_id + 1 {
    let task = ConcurrentTask::spawn(fn() {
      let local_operations = 0
      
      for i = 0; i < operations_per_thread; i = i + 1 {
        // 操作1: 原子计数器递增
        shared_counter.increment()
        
        // 操作2: 获取锁并修改共享数据
        shared_lock.lock()
        let current_value = shared_data.get("shared_key").unwrap_or(0)
        shared_data.put("shared_key", current_value + 1)
        shared_lock.unlock()
        
        // 操作3: 复杂的遥测操作
        let span = shared_resources.create_span("stress-operation")
        span.set_attribute("thread.id", thread_id.to_string())
        span.set_attribute("operation.id", i.to_string())
        
        // 模拟一些工作
        @thread.sleep(@random.int(5)) // 0-5ms随机延迟
        
        span.end()
        shared_resources.record_span(span)
        
        local_operations = local_operations + 1
      }
      
      local_operations
    })
    
    stress_tasks.push(task)
  }
  
  // 等待所有压力测试任务完成
  let total_operations = 0L
  for task in stress_tasks {
    let result = task.join()
    total_operations = total_operations + result.to_int64()
  }
  
  // 验证压力测试结果
  assert_eq(total_operations, (stress_threads * operations_per_thread).to_int64())
  
  // 验证共享资源的最终状态
  let final_counter_value = shared_counter.get()
  assert_eq(final_counter_value, total_operations)
  
  let final_shared_value = shared_data.get("shared_key").unwrap_or(0)
  assert_eq(final_shared_value, total_operations)
  
  // 验证遥测数据完整性
  let telemetry_stats = shared_resources.get_statistics()
  assert_eq(telemetry_stats.total_spans_created, total_operations)
  assert_eq(telemetry_stats.total_spans_recorded, total_operations)
  assert_eq(telemetry_stats.data_corruption_events, 0)
  assert_eq(telemetry_stats.concurrent_access_errors, 0)
  
  // 验证性能指标
  let performance_metrics = shared_resources.get_performance_metrics()
  assert_eq(performance_metrics.average_operation_time_ms < 10.0, true) // 平均操作时间应小于10ms
  assert_eq(performance_metrics.max_contention_time_ms < 100.0, true) // 最大竞争时间应小于100ms
  assert_eq(performance_metrics.throughput_operations_per_second > 1000, true) // 吞吐量应大于1000 ops/s
}