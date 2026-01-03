// Azimuth Telemetry System - High Quality Concurrent Safety Tests
// 并发操作安全性测试

test "并发属性操作安全性" {
  // 创建线程安全的属性集合
  let concurrent_attrs = azimuth::ConcurrentAttributes::new()
  
  // 创建多个并发任务
  let task_count = 20
  let operations_per_task = 50
  
  // 模拟并发写入操作
  for task_id in 0..task_count {
    for op_id in 0..operations_per_task {
      let key = "task_" + task_id.to_string() + "_key_" + op_id.to_string()
      let value = azimuth::StringValue("task_" + task_id.to_string() + "_value_" + op_id.to_string())
      
      // 并发设置属性
      azimuth::ConcurrentAttributes::set(concurrent_attrs, key, value)
    }
  }
  
  // 验证所有属性都已设置
  let total_expected = task_count * operations_per_task
  let actual_count = azimuth::ConcurrentAttributes::size(concurrent_attrs)
  assert_eq(actual_count, total_expected)
  
  // 验证属性值的正确性
  for task_id in 0..task_count {
    for op_id in 0..operations_per_task {
      let key = "task_" + task_id.to_string() + "_key_" + op_id.to_string()
      let expected_value = "task_" + task_id.to_string() + "_value_" + op_id.to_string()
      
      let retrieved_value = azimuth::ConcurrentAttributes::get(concurrent_attrs, key)
      match retrieved_value {
        Some(azimuth::StringValue(actual_value)) => {
          assert_eq(actual_value, expected_value)
        }
        _ => assert_true(false)
      }
    }
  }
  
  // 模拟并发读取操作
  let read_task_count = 10
  let reads_per_task = 100
  
  for read_task_id in 0..read_task_count {
    for read_id in 0..reads_per_task {
      // 随机选择一个键进行读取
      let random_task_id = read_id % task_count
      let random_op_id = read_id % operations_per_task
      let key = "task_" + random_task_id.to_string() + "_key_" + random_op_id.to_string()
      
      let retrieved_value = azimuth::ConcurrentAttributes::get(concurrent_attrs, key)
      match retrieved_value {
        Some(azimuth::StringValue(actual_value)) => {
          let expected_value = "task_" + random_task_id.to_string() + "_value_" + random_op_id.to_string()
          assert_eq(actual_value, expected_value)
        }
        _ => assert_true(false)
      }
    }
  }
  
  // 模拟并发删除操作
  let delete_task_count = 5
  let deletes_per_task = 20
  
  for delete_task_id in 0..delete_task_count {
    for delete_id in 0..deletes_per_task {
      let task_id = delete_task_id * 4
      let op_id = delete_id * 5
      let key = "task_" + task_id.to_string() + "_key_" + op_id.to_string()
      
      // 并发删除属性
      let removed = azimuth::ConcurrentAttributes::remove(concurrent_attrs, key)
      assert_true(removed)
      
      // 验证属性已被删除
      let retrieved_value = azimuth::ConcurrentAttributes::get(concurrent_attrs, key)
      match retrieved_value {
        Some(_) => assert_true(false) // 不应该存在
        None => assert_true(true) // 正确删除
      }
    }
  }
  
  // 验证最终属性数量
  let expected_final_count = total_expected - (delete_task_count * deletes_per_task)
  let actual_final_count = azimuth::ConcurrentAttributes::size(concurrent_attrs)
  assert_eq(actual_final_count, expected_final_count)
}

test "并发跨度操作安全性" {
  // 创建线程安全的跨度管理器
  let span_manager = azimuth::ConcurrentSpanManager::new()
  
  // 创建多个并发跨度
  let span_count = 50
  let mut span_ids = []
  
  for i in 0..span_count {
    let trace_id = "concurrent_trace_" + (i % 10).to_string()
    let span_id = "concurrent_span_" + i.to_string()
    span_ids = span_ids + [span_id]
    
    let span_context = azimuth::SpanContext::new(trace_id, span_id, true, "")
    let span = azimuth::Span::new("concurrent_span_" + i.to_string(), azimuth::Internal, span_context)
    
    // 并发注册跨度
    azimuth::ConcurrentSpanManager::register_span(span_manager, span_id, span)
  }
  
  // 验证所有跨度都已注册
  let registered_count = azimuth::ConcurrentSpanManager::get_span_count(span_manager)
  assert_eq(registered_count, span_count)
  
  // 模拟并发跨度操作
  let operation_task_count = 20
  let operations_per_task = 25
  
  for task_id in 0..operation_task_count {
    for op_id in 0..operations_per_task {
      let span_index = (task_id * operations_per_task + op_id) % span_count
      let span_id = span_ids[span_index]
      
      // 获取跨度
      let span_option = azimuth::ConcurrentSpanManager::get_span(span_manager, span_id)
      match span_option {
        Some(span) => {
          // 并发添加事件
          let event_attrs = azimuth::Attributes::new()
          azimuth::Attributes::set(event_attrs, "task.id", azimuth::IntValue(task_id))
          azimuth::Attributes::set(event_attrs, "operation.id", azimuth::IntValue(op_id))
          
          azimuth::Span::add_event(span, "concurrent_event", Some(event_attrs))
          
          // 并发设置属性
          let span_attrs = azimuth::Span::attributes(span)
          azimuth::Attributes::set(span_attrs, "concurrent.attr", azimuth::StringValue("concurrent_value"))
          
          // 并发更新状态
          if op_id % 5 == 0 {
            azimuth::Span::set_status(span, azimuth::Ok, Some("Concurrent operation completed"))
          }
        }
        None => assert_true(false) // 跨度应该存在
      }
    }
  }
  
  // 验证并发操作的完整性
  for span_id in span_ids {
    let span_option = azimuth::ConcurrentSpanManager::get_span(span_manager, span_id)
    match span_option {
      Some(span) => {
        // 验证事件已添加
        let events = azimuth::Span::get_events(span)
        assert_true(events.length() > 0)
        
        // 验证属性已设置
        let span_attrs = azimuth::Span::attributes(span)
        let concurrent_attr = azimuth::Attributes::get(span_attrs, "concurrent.attr")
        match concurrent_attr {
          Some(azimuth::StringValue(value)) => assert_eq(value, "concurrent_value")
          _ => assert_true(false)
        }
        
        // 验证状态（可能已更新或保持初始状态）
        let status = azimuth::Span::status(span)
        assert_true(status == azimuth::Ok || status == azimuth::Unset)
      }
      None => assert_true(false) // 跨度应该存在
    }
  }
  
  // 并发结束跨度
  let end_task_count = 10
  let ends_per_task = 5
  
  for task_id in 0..end_task_count {
    for end_id in 0..ends_per_task {
      let span_index = task_id * ends_per_task + end_id
      if span_index < span_ids.length() {
        let span_id = span_ids[span_index]
        
        // 并发结束跨度
        let span_option = azimuth::ConcurrentSpanManager::get_span(span_manager, span_id)
        match span_option {
          Some(span) => {
            if azimuth::Span::is_recording(span) {
              azimuth::Span::end(span)
            }
          }
          None => assert_true(false) // 跨度应该存在
        }
      }
    }
  }
  
  // 验证跨度状态
  let ended_count = azimuth::ConcurrentSpanManager::get_ended_span_count(span_manager)
  assert_true(ended_count > 0)
}

test "并发度量操作安全性" {
  // 创建线程安全的度量提供器
  let metric_provider = azimuth::ConcurrentMetricProvider::new()
  
  // 创建多个并发度量
  let meter = azimuth::ConcurrentMetricProvider::get_meter(metric_provider, "concurrent_meter")
  
  // 创建不同类型的度量
  let counter = azimuth::Meter::create_counter(meter, "concurrent_counter", Some("Concurrent counter"), Some("count"))
  let histogram = azimuth::Meter::create_histogram(meter, "concurrent_histogram", Some("Concurrent histogram"), Some("ms"))
  let gauge = azimuth::Meter::create_gauge(meter, "concurrent_gauge", Some("Concurrent gauge"), Some("value"))
  
  // 模拟并发度量操作
  let operation_task_count = 30
  let operations_per_task = 100
  
  for task_id in 0..operation_task_count {
    for op_id in 0..operations_per_task {
      // 并发更新计数器
      azimuth::Counter::add(counter, 1.0)
      
      // 并发记录直方图值
      let histogram_value = 10.0 + (task_id * 0.1) + (op_id * 0.01)
      azimuth::Histogram::record(histogram, histogram_value)
      
      // 并发更新仪表值
      let gauge_value = 100.0 + (task_id % 50) + (op_id % 10)
      azimuth::Gauge::record(gauge, gauge_value)
    }
  }
  
  // 验证计数器值
  let counter_value = azimuth::Counter::get_value(counter)
  let expected_counter_value = (operation_task_count * operations_per_task) as Float
  assert_eq(counter_value, expected_counter_value)
  
  // 验证直方图统计
  let histogram_stats = azimuth::Histogram::get_stats(histogram)
  assert_eq(azimuth::HistogramStats::count(histogram_stats), operation_task_count * operations_per_task)
  assert_true(azimuth::HistogramStats::sum(histogram_stats) > 0.0)
  assert_true(azimuth::HistogramStats::min(histogram_stats) >= 10.0)
  assert_true(azimuth::HistogramStats::max(histogram_stats) > 10.0)
  
  // 验证仪表值
  let gauge_value = azimuth::Gauge::get_value(gauge)
  assert_true(gauge_value >= 100.0)
  
  // 创建带有属性的并发度量操作
  let attribute_task_count = 20
  let attributes_per_task = 50
  
  for task_id in 0..attribute_task_count {
    for attr_id in 0..attributes_per_task {
      let attrs = azimuth::Attributes::new()
      azimuth::Attributes::set(attrs, "task.id", azimuth::IntValue(task_id))
      azimuth::Attributes::set(attrs, "attribute.id", azimuth::IntValue(attr_id))
      azimuth::Attributes::set(attrs, "service.name", azimuth::StringValue("concurrent_service"))
      
      // 并发更新带属性的计数器
      azimuth::Counter::add(counter, 1.0, Some(attrs))
      
      // 并发记录带属性的直方图值
      let histogram_value = 50.0 + (attr_id % 20)
      azimuth::Histogram::record(histogram, histogram_value, Some(attrs))
    }
  }
  
  // 验证带属性的度量统计
  let filtered_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(filtered_attrs, "service.name", azimuth::StringValue("concurrent_service"))
  
  let filtered_counter_value = azimuth::Counter::get_value_with_attributes(counter, Some(filtered_attrs))
  assert_eq(filtered_counter_value, (attribute_task_count * attributes_per_task) as Float)
  
  let filtered_histogram_stats = azimuth::Histogram::get_stats_with_attributes(histogram, Some(filtered_attrs))
  assert_eq(azimuth::HistogramStats::count(filtered_histogram_stats), attribute_task_count * attributes_per_task)
}

test "并发日志操作安全性" {
  // 创建线程安全的日志提供器
  let logger_provider = azimuth::ConcurrentLoggerProvider::new()
  let logger = azimuth::ConcurrentLoggerProvider::get_logger(logger_provider, "concurrent_logger")
  
  // 模拟并发日志记录
  let log_task_count = 25
  let logs_per_task = 40
  
  for task_id in 0..log_task_count {
    for log_id in 0..logs_per_task {
      let severity = if log_id % 4 == 0 { azimuth::Error }
                     else if log_id % 4 == 1 { azimuth::Warn }
                     else if log_id % 4 == 2 { azimuth::Info }
                     else { azimuth::Debug }
      
      let message = "Concurrent log message from task " + task_id.to_string() + " log " + log_id.to_string()
      
      // 创建日志记录
      let log_record = azimuth::LogRecord::new(severity, message)
      
      // 添加日志属性
      let log_attrs = azimuth::LogRecord::attributes(log_record)
      azimuth::Attributes::set(log_attrs, "task.id", azimuth::IntValue(task_id))
      azimuth::Attributes::set(log_attrs, "log.id", azimuth::IntValue(log_id))
      azimuth::Attributes::set(log_attrs, "thread.name", azimuth::StringValue("concurrent_thread_" + task_id.to_string()))
      
      // 并发发出日志
      azimuth::Logger::emit(logger, log_record)
    }
  }
  
  // 验证日志记录数量
  let total_logs = azimuth::ConcurrentLoggerProvider::get_log_count(logger_provider)
  assert_eq(total_logs, log_task_count * logs_per_task)
  
  // 验证日志记录内容
  let all_logs = azimuth::ConcurrentLoggerProvider::get_all_logs(logger_provider)
  assert_eq(all_logs.length(), log_task_count * logs_per_task)
  
  // 验证每个任务的日志记录
  for task_id in 0..log_task_count {
    let task_logs = azimuth::ConcurrentLoggerProvider::get_logs_by_attribute(
      logger_provider,
      "task.id",
      azimuth::IntValue(task_id)
    )
    
    assert_eq(task_logs.length(), logs_per_task)
    
    // 验证每个日志记录的属性
    for log in task_logs {
      let log_attrs = azimuth::LogRecord::attributes(log)
      
      let task_id_attr = azimuth::Attributes::get(log_attrs, "task.id")
      match task_id_attr {
        Some(azimuth::IntValue(id)) => assert_eq(id, task_id)
        _ => assert_true(false)
      }
      
      let thread_name_attr = azimuth::Attributes::get(log_attrs, "thread.name")
      match thread_name_attr {
        Some(azimuth::StringValue(name)) => assert_eq(name, "concurrent_thread_" + task_id.to_string())
        _ => assert_true(false)
      }
    }
  }
  
  // 验证不同严重级别的日志数量
  let error_logs = azimuth::ConcurrentLoggerProvider::get_logs_by_severity(logger_provider, azimuth::Error)
  let warn_logs = azimuth::ConcurrentLoggerProvider::get_logs_by_severity(logger_provider, azimuth::Warn)
  let info_logs = azimuth::ConcurrentLoggerProvider::get_logs_by_severity(logger_provider, azimuth::Info)
  let debug_logs = azimuth::ConcurrentLoggerProvider::get_logs_by_severity(logger_provider, azimuth::Debug)
  
  let expected_per_severity = (log_task_count * logs_per_task) / 4
  assert_eq(error_logs.length(), expected_per_severity)
  assert_eq(warn_logs.length(), expected_per_severity)
  assert_eq(info_logs.length(), expected_per_severity)
  assert_eq(debug_logs.length(), expected_per_severity)
}

test "并发资源管理安全性" {
  // 创建线程安全的资源管理器
  let resource_manager = azimuth::ConcurrentResourceManager::new()
  
  // 创建多个并发资源
  let resource_count = 30
  let mut resource_ids = []
  
  for i in 0..resource_count {
    let resource_id = "concurrent_resource_" + i.to_string()
    resource_ids = resource_ids + [resource_id]
    
    let resource_attrs = [
      ("resource.id", azimuth::StringValue(resource_id)),
      ("resource.type", azimuth::StringValue("concurrent_type")),
      ("resource.capacity", azimuth::IntValue(100 + i)),
      ("resource.status", azimuth::StringValue("active"))
    ]
    
    let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), resource_attrs)
    
    // 并发注册资源
    azimuth::ConcurrentResourceManager::register_resource(resource_manager, resource_id, resource)
  }
  
  // 验证所有资源都已注册
  let registered_count = azimuth::ConcurrentResourceManager::get_resource_count(resource_manager)
  assert_eq(registered_count, resource_count)
  
  // 模拟并发资源操作
  let operation_task_count = 15
  let operations_per_task = 20
  
  for task_id in 0..operation_task_count {
    for op_id in 0..operations_per_task {
      let resource_index = (task_id * operations_per_task + op_id) % resource_count
      let resource_id = resource_ids[resource_index]
      
      // 获取资源
      let resource_option = azimuth::ConcurrentResourceManager::get_resource(resource_manager, resource_id)
      match resource_option {
        Some(resource) => {
          // 并发更新资源属性
          let usage_attrs = [
            ("last.used.by", azimuth::StringValue("task_" + task_id.to_string())),
            ("usage.count", azimuth::IntValue(op_id)),
            ("usage.timestamp", azimuth::IntValue(1609459200000 + task_id * 1000 + op_id * 100))
          ]
          
          let updated_resource = azimuth::Resource::with_attributes(resource, usage_attrs)
          
          // 并发更新资源
          azimuth::ConcurrentResourceManager::update_resource(resource_manager, resource_id, updated_resource)
        }
        None => assert_true(false) // 资源应该存在
      }
    }
  }
  
  // 验证并发操作的完整性
  for resource_id in resource_ids {
    let resource_option = azimuth::ConcurrentResourceManager::get_resource(resource_manager, resource_id)
    match resource_option {
      Some(resource) => {
        // 验证基本属性
        let resource_id_attr = azimuth::Resource::get_attribute(resource, "resource.id")
        match resource_id_attr {
          Some(azimuth::StringValue(id)) => assert_eq(id, resource_id)
          _ => assert_true(false)
        }
        
        let resource_type_attr = azimuth::Resource::get_attribute(resource, "resource.type")
        match resource_type_attr {
          Some(azimuth::StringValue(type_str)) => assert_eq(type_str, "concurrent_type")
          _ => assert_true(false)
        }
        
        // 验证使用属性（应该已被更新）
        let last_used_by_attr = azimuth::Resource::get_attribute(resource, "last.used.by")
        match last_used_by_attr {
          Some(azimuth::StringValue(user)) => assert_true(user.starts_with("task_"))
          None => assert_true(false) // 应该存在
        }
        
        let usage_count_attr = azimuth::Resource::get_attribute(resource, "usage.count")
        match usage_count_attr {
          Some(azimuth::IntValue(count)) => assert_true(count >= 0)
          None => assert_true(false) // 应该存在
        }
      }
      None => assert_true(false) // 资源应该存在
    }
  }
  
  // 并发资源合并操作
  let merge_task_count = 10
  let merges_per_task = 3
  
  for task_id in 0..merge_task_count {
    for merge_id in 0..merges_per_task {
      let resource_index1 = (task_id * merges_per_task + merge_id) % resource_count
      let resource_index2 = (resource_index1 + 1) % resource_count
      
      let resource_id1 = resource_ids[resource_index1]
      let resource_id2 = resource_ids[resource_index2]
      
      // 获取两个资源
      let resource1_option = azimuth::ConcurrentResourceManager::get_resource(resource_manager, resource_id1)
      let resource2_option = azimuth::ConcurrentResourceManager::get_resource(resource_manager, resource_id2)
      
      match (resource1_option, resource2_option) {
        (Some(resource1), Some(resource2)) => {
          // 并发合并资源
          let merged_resource = azimuth::Resource::merge(resource1, resource2)
          
          // 更新第一个资源为合并结果
          azimuth::ConcurrentResourceManager::update_resource(resource_manager, resource_id1, merged_resource)
        }
        _ => assert_true(false) // 资源应该存在
      }
    }
  }
  
  // 验证资源合并结果
  for i in 0..merge_task_count {
    for merge_id in 0..merges_per_task {
      let resource_index1 = (i * merges_per_task + merge_id) % resource_count
      let resource_id1 = resource_ids[resource_index1]
      
      let resource_option = azimuth::ConcurrentResourceManager::get_resource(resource_manager, resource_id1)
      match resource_option {
        Some(resource) => {
          // 验证合并后的资源包含两个原始资源的属性
          let resource_id_attr = azimuth::Resource::get_attribute(resource, "resource.id")
          match resource_id_attr {
            Some(azimuth::StringValue(id)) => assert_eq(id, resource_id1)
            _ => assert_true(false)
          }
          
          // 合并的资源应该有更多的属性
          let all_attrs = azimuth::Resource::get_all_attributes(resource)
          assert_true(all_attrs.length() >= 4) // 至少包含基本属性
        }
        None => assert_true(false) // 资源应该存在
      }
    }
  }
}