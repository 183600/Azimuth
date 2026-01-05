// Azimuth Concurrent Safety and Thread Synchronization Test Suite
// 并发安全和线程同步测试套件，验证遥测系统在高并发场景下的稳定性和数据一致性

// 测试1: 并发属性访问安全性
test "并发属性访问安全性测试" {
  // 创建共享属性集合
  let shared_attrs = azimuth::ConcurrentAttributes::new()
  
  // 模拟多个线程同时访问属性
  let num_threads = 10
  let operations_per_thread = 100
  
  // 线程1: 设置属性
  let thread1_operations = azimuth::ConcurrentOperations::new()
  for i in 0..operations_per_thread {
    let key = "thread1.key." + i.to_string()
    let value = azimuth::AttributeValue::StringValue("thread1.value." + i.to_string())
    thread1_operations = azimuth::ConcurrentOperations::add_set_operation(thread1_operations, key, value)
  }
  
  // 线程2: 设置属性
  let thread2_operations = azimuth::ConcurrentOperations::new()
  for i in 0..operations_per_thread {
    let key = "thread2.key." + i.to_string()
    let value = azimuth::AttributeValue::IntValue(i)
    thread2_operations = azimuth::ConcurrentOperations::add_set_operation(thread2_operations, key, value)
  }
  
  // 线程3: 读取属性
  let thread3_operations = azimuth::ConcurrentOperations::new()
  for i in 0..operations_per_thread {
    let key1 = "thread1.key." + i.to_string()
    let key2 = "thread2.key." + i.to_string()
    thread3_operations = azimuth::ConcurrentOperations::add_get_operation(thread3_operations, key1)
    thread3_operations = azimuth::ConcurrentOperations::add_get_operation(thread3_operations, key2)
  }
  
  // 执行并发操作
  let results = azimuth::ConcurrentExecutor::execute_operations([
    ("thread1", thread1_operations),
    ("thread2", thread2_operations),
    ("thread3", thread3_operations)
  ], shared_attrs)
  
  // 验证操作结果
  assert_eq(results.get("thread1.set_success"), Some(operations_per_thread))
  assert_eq(results.get("thread2.set_success"), Some(operations_per_thread))
  
  // 验证属性设置正确
  for i in 0..operations_per_thread {
    let key1 = "thread1.key." + i.to_string()
    let value1 = azimuth::ConcurrentAttributes::get(shared_attrs, key1)
    match value1 {
      Some(azimuth::AttributeValue::StringValue(v)) => assert_eq(v, "thread1.value." + i.to_string())
      _ => assert_true(false)
    }
    
    let key2 = "thread2.key." + i.to_string()
    let value2 = azimuth::ConcurrentAttributes::get(shared_attrs, key2)
    match value2 {
      Some(azimuth::AttributeValue::IntValue(v)) => assert_eq(v, i)
      _ => assert_true(false)
    }
  }
}

// 测试2: 并发跨度操作安全性
test "并发跨度操作安全性测试" {
  // 创建跨度处理器
  let span_processor = azimuth::ConcurrentSpanProcessor::new()
  
  // 模拟多个线程同时创建和处理跨度
  let num_threads = 8
  let spans_per_thread = 50
  
  // 创建并发跨度创建操作
  let mut all_span_operations = []
  for thread_id in 0..num_threads {
    let thread_operations = azimuth::ConcurrentOperations::new()
    for span_id in 0..spans_per_thread {
      let span_name = "span-" + thread_id.to_string() + "-" + span_id.to_string()
      let trace_id = "trace-" + thread_id.to_string()
      let parent_span_id = if span_id > 0 { 
        Some("span-" + thread_id.to_string() + "-" + (span_id - 1).to_string()) 
      } else { 
        None 
      }
      
      thread_operations = azimuth::ConcurrentOperations::add_create_span_operation(
        thread_operations, span_name, trace_id, parent_span_id
      )
      
      // 添加一些属性和事件
      thread_operations = azimuth::ConcurrentOperations::add_set_attribute_operation(
        thread_operations, "thread.id", azimuth::AttributeValue::IntValue(thread_id)
      )
      thread_operations = azimuth::ConcurrentOperations::add_set_attribute_operation(
        thread_operations, "span.index", azimuth::AttributeValue::IntValue(span_id)
      )
      thread_operations = azimuth::ConcurrentOperations::add_add_event_operation(
        thread_operations, "span-created", Some([("creation.time", azimuth::AttributeValue::IntValue(1234567890))])
      )
    }
    all_span_operations = all_span_operations + [("thread-" + thread_id.to_string(), thread_operations)]
  }
  
  // 执行并发跨度操作
  let results = azimuth::ConcurrentSpanProcessor::process_spans(span_processor, all_span_operations)
  
  // 验证所有跨度都被成功创建
  assert_eq(results.get("total_spans_created"), Some(num_threads * spans_per_thread))
  
  // 验证跨度关系正确
  let all_spans = azimuth::ConcurrentSpanProcessor::get_all_spans(span_processor)
  assert_eq(all_spans.length(), num_threads * spans_per_thread)
  
  // 验证父子关系
  for thread_id in 0..num_threads {
    for span_id in 1..spans_per_thread {
      let span_name = "span-" + thread_id.to_string() + "-" + span_id.to_string()
      let span = azimuth::ConcurrentSpanProcessor::get_span_by_name(span_processor, span_name)
      match span {
        Some(s) => {
          let parent_span_id = azimuth::Span::parent_span_id(s)
          match parent_span_id {
            Some(parent) => assert_eq(parent, "span-" + thread_id.to_string() + "-" + (span_id - 1).to_string())
            None => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
  }
}

// 测试3: 并发指标收集安全性
test "并发指标收集安全性测试" {
  // 创建并发指标收集器
  let metric_collector = azimuth::ConcurrentMetricCollector::new()
  
  // 模拟多个线程同时记录指标
  let num_threads = 12
  let metrics_per_thread = 200
  
  // 创建并发指标记录操作
  let mut all_metric_operations = []
  for thread_id in 0..num_threads {
    let thread_operations = azimuth::ConcurrentOperations::new()
    for metric_id in 0..metrics_per_thread {
      let metric_name = "metric-" + thread_id.to_string()
      let metric_value = metric_id.to_float() * 1.5
      let attributes = [
        ("thread.id", azimuth::AttributeValue::IntValue(thread_id)),
        ("metric.index", azimuth::AttributeValue::IntValue(metric_id))
      ]
      
      thread_operations = azimuth::ConcurrentOperations::add_record_metric_operation(
        thread_operations, metric_name, metric_value, attributes
      )
    }
    all_metric_operations = all_metric_operations + [("thread-" + thread_id.to_string(), thread_operations)]
  }
  
  // 执行并发指标记录
  let results = azimuth::ConcurrentMetricCollector::record_metrics(metric_collector, all_metric_operations)
  
  // 验证所有指标都被成功记录
  assert_eq(results.get("total_metrics_recorded"), Some(num_threads * metrics_per_thread))
  
  // 验证指标聚合结果
  for thread_id in 0..num_threads {
    let metric_name = "metric-" + thread_id.to_string()
    let aggregation = azimuth::ConcurrentMetricCollector::get_aggregation(metric_collector, metric_name)
    
    match aggregation {
      Some(agg) => {
        assert_eq(agg.count, metrics_per_thread)
        
        // 计算期望的总和
        let expected_sum = 0.0 + 1.5 + 3.0 + ... + (metrics_per_thread - 1).to_float() * 1.5
        assert_eq(agg.sum, expected_sum)
        assert_eq(agg.min, 0.0)
        assert_eq(agg.max, (metrics_per_thread - 1).to_float() * 1.5)
        assert_eq(agg.average, expected_sum / metrics_per_thread.to_float())
      }
      None => assert_true(false)
    }
  }
}

// 测试4: 并发日志记录安全性
test "并发日志记录安全性测试" {
  // 创建并发日志记录器
  let logger = azimuth::ConcurrentLogger::new()
  
  // 模拟多个线程同时记录日志
  let num_threads = 16
  let logs_per_thread = 100
  
  // 创建并发日志记录操作
  let mut all_log_operations = []
  for thread_id in 0..num_threads {
    let thread_operations = azimuth::ConcurrentOperations::new()
    for log_id in 0..logs_per_thread {
      let severity = if log_id % 4 == 0 { 
        azimuth::LogLevel::Error 
      } else if log_id % 4 == 1 { 
        azimuth::LogLevel::Warn 
      } else if log_id % 4 == 2 { 
        azimuth::LogLevel::Info 
      } else { 
        azimuth::LogLevel::Debug 
      }
      
      let message = "Log message from thread " + thread_id.to_string() + " entry " + log_id.to_string()
      let attributes = [
        ("thread.id", azimuth::AttributeValue::IntValue(thread_id)),
        ("log.index", azimuth::AttributeValue::IntValue(log_id)),
        ("log.severity", azimuth::AttributeValue::StringValue(severity.to_string()))
      ]
      
      thread_operations = azimuth::ConcurrentOperations::add_log_operation(
        thread_operations, severity, message, attributes
      )
    }
    all_log_operations = all_log_operations + [("thread-" + thread_id.to_string(), thread_operations)]
  }
  
  // 执行并发日志记录
  let results = azimuth::ConcurrentLogger::process_logs(logger, all_log_operations)
  
  // 验证所有日志都被成功记录
  assert_eq(results.get("total_logs_recorded"), Some(num_threads * logs_per_thread))
  
  // 验证日志按严重级别分类正确
  let error_logs = azimuth::ConcurrentLogger::get_logs_by_severity(logger, azimuth::LogLevel::Error)
  let warn_logs = azimuth::ConcurrentLogger::get_logs_by_severity(logger, azimuth::LogLevel::Warn)
  let info_logs = azimuth::ConcurrentLogger::get_logs_by_severity(logger, azimuth::LogLevel::Info)
  let debug_logs = azimuth::ConcurrentLogger::get_logs_by_severity(logger, azimuth::LogLevel::Debug)
  
  // 每种严重级别应该有大约1/4的日志
  let expected_per_severity = (num_threads * logs_per_thread) / 4
  assert_eq(error_logs.length(), expected_per_severity)
  assert_eq(warn_logs.length(), expected_per_severity)
  assert_eq(info_logs.length(), expected_per_severity)
  assert_eq(debug_logs.length(), expected_per_severity)
}

// 测试5: 并发上下文传播安全性
test "并发上下文传播安全性测试" {
  // 创建并发上下文管理器
  let context_manager = azimuth::ConcurrentContextManager::new()
  
  // 模拟多个线程同时进行上下文传播
  let num_threads = 10
  let contexts_per_thread = 20
  
  // 创建并发上下文操作
  let mut all_context_operations = []
  for thread_id in 0..num_threads {
    let thread_operations = azimuth::ConcurrentOperations::new()
    for context_id in 0..contexts_per_thread {
      let trace_id = "trace-" + thread_id.to_string() + "-" + context_id.to_string()
      let span_id = "span-" + thread_id.to_string() + "-" + context_id.to_string()
      
      // 创建根上下文
      thread_operations = azimuth::ConcurrentOperations::add_create_context_operation(
        thread_operations, trace_id, span_id
      )
      
      // 添加上下文值
      thread_operations = azimuth::ConcurrentOperations::add_set_context_value_operation(
        thread_operations, "thread.id", thread_id
      )
      thread_operations = azimuth::ConcurrentOperations::add_set_context_value_operation(
        thread_operations, "context.index", context_id
      )
      
      // 如果不是第一个上下文，创建子上下文
      if context_id > 0 {
        let parent_span_id = "span-" + thread_id.to_string() + "-" + (context_id - 1).to_string()
        thread_operations = azimuth::ConcurrentOperations::add_create_child_context_operation(
          thread_operations, trace_id, span_id, parent_span_id
        )
      }
    }
    all_context_operations = all_context_operations + [("thread-" + thread_id.to_string(), thread_operations)]
  }
  
  // 执行并发上下文操作
  let results = azimuth::ConcurrentContextManager::process_contexts(context_manager, all_context_operations)
  
  // 验证所有上下文都被成功创建
  assert_eq(results.get("total_contexts_created"), Some(num_threads * contexts_per_thread))
  
  // 验证上下文层次结构
  for thread_id in 0..num_threads {
    for context_id in 1..contexts_per_thread {
      let span_id = "span-" + thread_id.to_string() + "-" + context_id.to_string()
      let context = azimuth::ConcurrentContextManager::get_context_by_span_id(context_manager, span_id)
      match context {
        Some(ctx) => {
          let parent_span_id = azimuth::Context::parent_span_id(ctx)
          match parent_span_id {
            Some(parent) => assert_eq(parent, "span-" + thread_id.to_string() + "-" + (context_id - 1).to_string())
            None => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
  }
}

// 测试6: 并发资源管理安全性
test "并发资源管理安全性测试" {
  // 创建并发资源管理器
  let resource_manager = azimuth::ConcurrentResourceManager::new()
  
  // 模拟多个线程同时进行资源分配和释放
  let num_threads = 8
  let resources_per_thread = 50
  
  // 创建并发资源操作
  let mut all_resource_operations = []
  for thread_id in 0..num_threads {
    let thread_operations = azimuth::ConcurrentOperations::new()
    for resource_id in 0..resources_per_thread {
      let resource_name = "resource-" + thread_id.to_string() + "-" + resource_id.to_string()
      let resource_type = if resource_id % 3 == 0 { "database" } else if resource_id % 3 == 1 { "cache" } else { "queue" }
      
      // 分配资源
      thread_operations = azimuth::ConcurrentOperations::add_allocate_resource_operation(
        thread_operations, resource_name, resource_type
      )
      
      // 使用资源
      thread_operations = azimuth::ConcurrentOperations::add_use_resource_operation(
        thread_operations, resource_name
      )
      
      // 释放资源
      thread_operations = azimuth::ConcurrentOperations::add_release_resource_operation(
        thread_operations, resource_name
      )
    }
    all_resource_operations = all_resource_operations + [("thread-" + thread_id.to_string(), thread_operations)]
  }
  
  // 执行并发资源操作
  let results = azimuth::ConcurrentResourceManager::process_resources(resource_manager, all_resource_operations)
  
  // 验证所有资源都被成功处理
  assert_eq(results.get("total_resources_allocated"), Some(num_threads * resources_per_thread))
  assert_eq(results.get("total_resources_used"), Some(num_threads * resources_per_thread))
  assert_eq(results.get("total_resources_released"), Some(num_threads * resources_per_thread))
  
  // 验证没有资源泄漏
  let leaked_resources = azimuth::ConcurrentResourceManager::get_leaked_resources(resource_manager)
  assert_eq(leaked_resources.length(), 0)
  
  // 验证资源使用统计
  let resource_stats = azimuth::ConcurrentResourceManager::get_resource_statistics(resource_manager)
  assert_eq(resource_stats.get("database"), Some(num_threads * resources_per_thread / 3))
  assert_eq(resource_stats.get("cache"), Some(num_threads * resources_per_thread / 3))
  assert_eq(resource_stats.get("queue"), Some(num_threads * resources_per_thread / 3))
}

// 测试7: 并发行李传播安全性
test "并发行李传播安全性测试" {
  // 创建并发行李管理器
  let baggage_manager = azimuth::ConcurrentBaggageManager::new()
  
  // 模拟多个线程同时进行行李传播
  let num_threads = 6
  let baggage_entries_per_thread = 30
  
  // 创建并发行李操作
  let mut all_baggage_operations = []
  for thread_id in 0..num_threads {
    let thread_operations = azimuth::ConcurrentOperations::new()
    for entry_id in 0..baggage_entries_per_thread {
      let key = "baggage-key-" + thread_id.to_string() + "-" + entry_id.to_string()
      let value = "baggage-value-" + thread_id.to_string() + "-" + entry_id.to_string()
      
      // 设置行李条目
      thread_operations = azimuth::ConcurrentOperations::add_set_baggage_entry_operation(
        thread_operations, key, value
      )
      
      // 传播行李到下一个上下文
      if entry_id < baggage_entries_per_thread - 1 {
        let next_key = "baggage-key-" + thread_id.to_string() + "-" + (entry_id + 1).to_string()
        thread_operations = azimuth::ConcurrentOperations::add_propagate_baggage_operation(
          thread_operations, key, next_key
        )
      }
    }
    all_baggage_operations = all_baggage_operations + [("thread-" + thread_id.to_string(), thread_operations)]
  }
  
  // 执行并发行李操作
  let results = azimuth::ConcurrentBaggageManager::process_baggage(baggage_manager, all_baggage_operations)
  
  // 验证所有行李条目都被成功设置
  assert_eq(results.get("total_baggage_entries_set"), Some(num_threads * baggage_entries_per_thread))
  
  // 验证行李传播正确
  for thread_id in 0..num_threads {
    for entry_id in 0..baggage_entries_per_thread {
      let key = "baggage-key-" + thread_id.to_string() + "-" + entry_id.to_string()
      let expected_value = "baggage-value-" + thread_id.to_string() + "-" + entry_id.to_string()
      
      let baggage_value = azimuth::ConcurrentBaggageManager::get_entry(baggage_manager, key)
      match baggage_value {
        Some(value) => assert_eq(value, expected_value)
        None => assert_true(false)
      }
    }
  }
  
  // 验证行李合并功能
  let merged_baggage = azimuth::ConcurrentBaggageManager::merge_all_baggage(baggage_manager)
  assert_eq(merged_baggage.entries.length(), num_threads * baggage_entries_per_thread)
}

// 测试8: 并发追踪ID生成安全性
test "并发追踪ID生成安全性测试" {
  // 创建并发ID生成器
  let id_generator = azimuth::ConcurrentIDGenerator::new()
  
  // 模拟多个线程同时生成ID
  let num_threads = 20
  let ids_per_thread = 100
  
  // 创建并发ID生成操作
  let mut all_id_operations = []
  for thread_id in 0..num_threads {
    let thread_operations = azimuth::ConcurrentOperations::new()
    for id_id in 0..ids_per_thread {
      thread_operations = azimuth::ConcurrentOperations::add_generate_trace_id_operation(thread_operations)
      thread_operations = azimuth::ConcurrentOperations::add_generate_span_id_operation(thread_operations)
    }
    all_id_operations = all_id_operations + [("thread-" + thread_id.to_string(), thread_operations)]
  }
  
  // 执行并发ID生成
  let results = azimuth::ConcurrentIDGenerator::generate_ids(id_generator, all_id_operations)
  
  // 验证所有ID都被成功生成
  assert_eq(results.get("total_trace_ids_generated"), Some(num_threads * ids_per_thread))
  assert_eq(results.get("total_span_ids_generated"), Some(num_threads * ids_per_thread))
  
  // 验证ID唯一性
  let all_trace_ids = azimuth::ConcurrentIDGenerator::get_all_trace_ids(id_generator)
  let all_span_ids = azimuth::ConcurrentIDGenerator::get_all_span_ids(id_generator)
  
  assert_eq(all_trace_ids.length(), num_threads * ids_per_thread)
  assert_eq(all_span_ids.length(), num_threads * ids_per_thread)
  
  // 验证没有重复的追踪ID
  let unique_trace_ids = azimuth::ConcurrentIDGenerator::get_unique_trace_ids(id_generator)
  assert_eq(unique_trace_ids.length(), all_trace_ids.length())
  
  // 验证没有重复的跨度ID
  let unique_span_ids = azimuth::ConcurrentIDGenerator::get_unique_span_ids(id_generator)
  assert_eq(unique_span_ids.length(), all_span_ids.length())
  
  // 验证ID格式正确
  for trace_id in all_trace_ids {
    assert_eq(trace_id.length(), 32) // 128位十六进制字符串
    assert_true(azimuth::ConcurrentIDGenerator::is_valid_hex_string(trace_id))
  }
  
  for span_id in all_span_ids {
    assert_eq(span_id.length(), 16) // 64位十六进制字符串
    assert_true(azimuth::ConcurrentIDGenerator::is_valid_hex_string(span_id))
  }
}

// 测试9: 并发采样决策一致性
test "并发采样决策一致性测试" {
  // 创建并发采样管理器
  let sampling_manager = azimuth::ConcurrentSamplingManager::new(0.1) // 10%采样率
  
  // 模拟多个线程同时进行采样决策
  let num_threads = 15
  let decisions_per_thread = 200
  
  // 创建并发采样操作
  let mut all_sampling_operations = []
  for thread_id in 0..num_threads {
    let thread_operations = azimuth::ConcurrentOperations::new()
    for decision_id in 0..decisions_per_thread {
      let trace_id = "trace-" + thread_id.to_string() + "-" + decision_id.to_string()
      
      // 进行采样决策
      thread_operations = azimuth::ConcurrentOperations::add_sampling_decision_operation(
        thread_operations, trace_id
      )
      
      // 再次进行相同trace_id的采样决策（应该得到相同结果）
      thread_operations = azimuth::ConcurrentOperations::add_sampling_decision_operation(
        thread_operations, trace_id
      )
    }
    all_sampling_operations = all_sampling_operations + [("thread-" + thread_id.to_string(), thread_operations)]
  }
  
  // 执行并发采样决策
  let results = azimuth::ConcurrentSamplingManager::process_sampling_decisions(sampling_manager, all_sampling_operations)
  
  // 验证所有采样决策都被处理
  assert_eq(results.get("total_sampling_decisions"), Some(num_threads * decisions_per_thread * 2))
  
  // 验证采样决策一致性（相同trace_id应该得到相同决策）
  for thread_id in 0..num_threads {
    for decision_id in 0..decisions_per_thread {
      let trace_id = "trace-" + thread_id.to_string() + "-" + decision_id.to_string()
      let decisions = azimuth::ConcurrentSamplingManager::get_decisions_for_trace_id(sampling_manager, trace_id)
      
      match decisions {
        Some(decision_list) => {
          assert_eq(decision_list.length(), 2) // 每个trace_id应该有两个决策记录
          assert_eq(decision_list[0], decision_list[1]) // 两个决策应该相同
        }
        None => assert_true(false)
      }
    }
  }
  
  // 验证采样率接近期望值
  let all_decisions = azimuth::ConcurrentSamplingManager::get_all_decisions(sampling_manager)
  let sampled_count = azimuth::ConcurrentSamplingManager::get_sampled_count(sampling_manager)
  let actual_rate = sampled_count.to_float() / all_decisions.length().to_float()
  
  // 允许一定误差范围
  assert_true(actual_rate > 0.08 && actual_rate < 0.12)
}

// 测试10: 并发序列化和反序列化安全性
test "并发序列化和反序列化安全性测试" {
  // 创建并发序列化管理器
  let serialization_manager = azimuth::ConcurrentSerializationManager::new()
  
  // 模拟多个线程同时进行序列化和反序列化
  let num_threads = 10
  let objects_per_thread = 50
  
  // 创建并发序列化操作
  let mut all_serialization_operations = []
  for thread_id in 0..num_threads {
    let thread_operations = azimuth::ConcurrentOperations::new()
    for object_id in 0..objects_per_thread {
      // 创建测试对象
      let span_name = "span-" + thread_id.to_string() + "-" + object_id.to_string()
      let trace_id = "trace-" + thread_id.to_string() + "-" + object_id.to_string()
      let span_id = "span-id-" + thread_id.to_string() + "-" + object_id.to_string()
      
      let span = azimuth::TestSpan::new(span_name, trace_id, span_id)
      
      // 序列化对象
      thread_operations = azimuth::ConcurrentOperations::add_serialize_operation(
        thread_operations, span_name, span
      )
      
      // 反序列化对象
      thread_operations = azimuth::ConcurrentOperations::add_deserialize_operation(
        thread_operations, span_name
      )
    }
    all_serialization_operations = all_serialization_operations + [("thread-" + thread_id.to_string(), thread_operations)]
  }
  
  // 执行并发序列化和反序列化
  let results = azimuth::ConcurrentSerializationManager::process_serialization(serialization_manager, all_serialization_operations)
  
  // 验证所有序列化和反序列化操作都被处理
  assert_eq(results.get("total_serializations"), Some(num_threads * objects_per_thread))
  assert_eq(results.get("total_deserializations"), Some(num_threads * objects_per_thread))
  
  // 验证序列化和反序列化结果正确
  for thread_id in 0..num_threads {
    for object_id in 0..objects_per_thread {
      let span_name = "span-" + thread_id.to_string() + "-" + object_id.to_string()
      
      // 获取序列化结果
      let serialized_data = azimuth::ConcurrentSerializationManager::get_serialized_data(serialization_manager, span_name)
      match serialized_data {
        Some(data) => {
          // 验证序列化数据包含预期内容
          assert_true(data.contains("\"span_name\":\"" + span_name + "\""))
          assert_true(data.contains("\"trace_id\":\"trace-" + thread_id.to_string() + "-" + object_id.to_string() + "\""))
          assert_true(data.contains("\"span_id\":\"span-id-" + thread_id.to_string() + "-" + object_id.to_string() + "\""))
        }
        None => assert_true(false)
      }
      
      // 获取反序列化结果
      let deserialized_span = azimuth::ConcurrentSerializationManager::get_deserialized_object(serialization_manager, span_name)
      match deserialized_span {
        Some(span) => {
          assert_eq(azimuth::TestSpan::name(span), span_name)
          assert_eq(azimuth::TestSpan::trace_id(span), "trace-" + thread_id.to_string() + "-" + object_id.to_string())
          assert_eq(azimuth::TestSpan::span_id(span), "span-id-" + thread_id.to_string() + "-" + object_id.to_string())
        }
        None => assert_true(false)
      }
    }
  }
  
  // 验证序列化数据完整性
  let all_serialized_data = azimuth::ConcurrentSerializationManager::get_all_serialized_data(serialization_manager)
  assert_eq(all_serialized_data.length(), num_threads * objects_per_thread)
  
  // 验证反序列化对象完整性
  let all_deserialized_objects = azimuth::ConcurrentSerializationManager::get_all_deserialized_objects(serialization_manager)
  assert_eq(all_deserialized_objects.length(), num_threads * objects_per_thread)
}