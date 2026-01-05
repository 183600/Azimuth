// Azimuth Memory Management and Resource Cleanup Tests
// 内存管理和资源清理测试用例 - 测试系统的内存管理和资源清理功能，包括内存泄漏检测、资源池管理、垃圾回收等

// Test 1: 跨度生命周期内存管理
test "span lifecycle memory management" {
  let memory_tracker = MemoryTracker::new()
  let initial_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // 创建大量跨度
  let spans = []
  for i in 1..=10000 {
    let trace_id = "trace_" + i.to_string()
    let span_id = "span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("memory_test_span", Internal, span_ctx)
    
    // 添加事件和属性
    for j in 1..=10 {
      Span::add_event(span, "event_" + j.to_string(), Some([
        ("iteration", IntValue(j)),
        ("data", StringValue("test_data_" + j.to_string()))
      ]))
    }
    
    spans.push(span)
  }
  
  let after_creation_memory = MemoryTracker::get_current_usage(memory_tracker)
  let memory_used = after_creation_memory - initial_memory
  
  // 验证内存使用合理
  assert_true(memory_used > 0)
  let memory_per_span = memory_used.to_float() / 10000.0
  assert_true(memory_per_span < 5120.0) // 每个跨度应该使用少于5KB内存
  
  // 结束所有跨度
  for span in spans {
    Span::end(span)
  }
  
  // 清理引用
  spans.clear()
  
  // 强制垃圾回收
  MemoryTracker::force_gc(memory_tracker)
  
  let after_cleanup_memory = MemoryTracker::get_current_usage(memory_tracker)
  let memory_recovered = after_creation_memory - after_cleanup_memory
  
  // 验证内存回收
  assert_true(memory_recovered > memory_used * 0.8) // 至少回收80%的内存
  
  // 验证没有内存泄漏
  let memory_leak_detected = MemoryTracker::detect_leak(memory_tracker, "span")
  assert_false(memory_leak_detected)
}

// Test 2: 属性集合内存管理
test "attributes collection memory management" {
  let memory_tracker = MemoryTracker::new()
  let initial_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // 创建大量属性集合
  let attributes_list = []
  for i in 1..=5000 {
    let attrs = Attributes::new()
    
    // 添加多个属性
    for j in 1..=20 {
      Attributes::set(attrs, "key_" + j.to_string(), StringValue("value_" + j.to_string()))
    }
    
    attributes_list.push(attrs)
  }
  
  let after_creation_memory = MemoryTracker::get_current_usage(memory_tracker)
  let memory_used = after_creation_memory - initial_memory
  
  // 验证内存使用合理
  assert_true(memory_used > 0)
  let memory_per_attrs = memory_used.to_float() / 5000.0
  assert_true(memory_per_attrs < 2048.0) // 每个属性集应该使用少于2KB内存
  
  // 清理引用
  attributes_list.clear()
  
  // 强制垃圾回收
  MemoryTracker::force_gc(memory_tracker)
  
  let after_cleanup_memory = MemoryTracker::get_current_usage(memory_tracker)
  let memory_recovered = after_creation_memory - after_cleanup_memory
  
  // 验证内存回收
  assert_true(memory_recovered > memory_used * 0.8) // 至少回收80%的内存
  
  // 验证没有内存泄漏
  let memory_leak_detected = MemoryTracker::detect_leak(memory_tracker, "attributes")
  assert_false(memory_leak_detected)
}

// Test 3: 指标数据内存管理
test "metrics data memory management" {
  let memory_tracker = MemoryTracker::new()
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "memory_test_meter")
  let initial_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // 创建大量指标
  let metrics = []
  for i in 1..=1000 {
    let counter = Meter::create_counter(meter, "counter_" + i.to_string(), Some("Test counter"), Some("count"))
    let histogram = Meter::create_histogram(meter, "histogram_" + i.to_string(), Some("Test histogram"), Some("ms"))
    let gauge = Meter::create_gauge(meter, "gauge_" + i.to_string(), Some("Test gauge"), Some("value"))
    
    // 添加数据点
    for j in 1..=100 {
      Counter::add(counter, j.to_float())
      Histogram::record(histogram, j.to_float())
      Gauge::record(gauge, j.to_float())
    }
    
    metrics.push(counter)
    metrics.push(histogram)
    metrics.push(gauge)
  }
  
  let after_creation_memory = MemoryTracker::get_current_usage(memory_tracker)
  let memory_used = after_creation_memory - initial_memory
  
  // 验证内存使用合理
  assert_true(memory_used > 0)
  let memory_per_metric = memory_used.to_float() / 3000.0
  assert_true(memory_per_metric < 1024.0) // 每个指标应该使用少于1KB内存
  
  // 清理引用
  metrics.clear()
  
  // 强制垃圾回收
  MemoryTracker::force_gc(memory_tracker)
  
  let after_cleanup_memory = MemoryTracker::get_current_usage(memory_tracker)
  let memory_recovered = after_creation_memory - after_cleanup_memory
  
  // 验证内存回收
  assert_true(memory_recovered > memory_used * 0.8) // 至少回收80%的内存
  
  // 验证没有内存泄漏
  let memory_leak_detected = MemoryTracker::detect_leak(memory_tracker, "metrics")
  assert_false(memory_leak_detected)
}

// Test 4: 资源池管理
test "resource pool management" {
  let resource_pool = ResourcePool::new(100) // 最多100个资源
  let memory_tracker = MemoryTracker::new()
  let initial_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // 获取和释放资源
  let resources = []
  for i in 1..=100 {
    let resource = ResourcePool::acquire(resource_pool)
    match resource {
      Some(res) => {
        Resource::initialize(res, "resource_" + i.to_string())
        resources.push(res)
      }
      None => assert_true(false)
    }
  }
  
  // 验证资源池已满
  let full_pool_resource = ResourcePool::acquire(resource_pool)
  match full_pool_resource {
    Some(_) => assert_true(false) // 不应该获取到资源
    None => assert_true(true)      // 应该获取失败
  }
  
  let after_acquisition_memory = MemoryTracker::get_current_usage(memory_tracker)
  let memory_used = after_acquisition_memory - initial_memory
  
  // 验证内存使用合理
  assert_true(memory_used > 0)
  let memory_per_resource = memory_used.to_float() / 100.0
  assert_true(memory_per_resource < 512.0) // 每个资源应该使用少于512B内存
  
  // 释放一半资源
  for i in 1..=50 {
    let resource = resources[i - 1]
    ResourcePool::release(resource_pool, resource)
  }
  
  // 验证可以重新获取资源
  let new_resources = []
  for i in 1..=50 {
    let resource = ResourcePool::acquire(resource_pool)
    match resource {
      Some(res) => {
        new_resources.push(res)
      }
      None => assert_true(false)
    }
  }
  
  // 验证资源池仍然满
  let still_full_pool_resource = ResourcePool::acquire(resource_pool)
  match still_full_pool_resource {
    Some(_) => assert_true(false) // 不应该获取到资源
    None => assert_true(true)      // 应该获取失败
  }
  
  // 释放所有资源
  for i in 51..=100 {
    let resource = resources[i - 1]
    ResourcePool::release(resource_pool, resource)
  }
  
  for resource in new_resources {
    ResourcePool::release(resource_pool, resource)
  }
  
  // 强制垃圾回收
  MemoryTracker::force_gc(memory_tracker)
  
  let after_cleanup_memory = MemoryTracker::get_current_usage(memory_tracker)
  let memory_recovered = after_acquisition_memory - after_cleanup_memory
  
  // 验证内存回收
  assert_true(memory_recovered > memory_used * 0.8) // 至少回收80%的内存
  
  // 验证资源池统计
  let pool_stats = ResourcePool::get_statistics(resource_pool)
  assert_eq(pool_stats.total_resources, 100)
  assert_eq(pool_stats.available_resources, 100)
  assert_eq(pool_stats.acquired_count, 150)
  assert_eq(pool_stats.released_count, 150)
  
  // 验证没有资源泄漏
  let resource_leak_detected = ResourcePool::detect_leaks(resource_pool)
  assert_false(resource_leak_detected)
}

// Test 5: 缓存内存管理
test "cache memory management" {
  let memory_tracker = MemoryTracker::new()
  let cache = MemoryCache::new(10 * 1024 * 1024) // 10MB缓存
  let initial_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // 添加大量缓存项
  for i in 1..=10000 {
    let key = "key_" + i.to_string()
    let value = "value_" + i.to_string() + "_with_additional_data_to_increase_size"
    MemoryCache::put(cache, key, value)
  }
  
  let after_population_memory = MemoryTracker::get_current_usage(memory_tracker)
  let memory_used = after_population_memory - initial_memory
  
  // 验证内存使用合理
  assert_true(memory_used > 0)
  assert_true(memory_used < 10 * 1024 * 1024) // 不应该超过10MB限制
  
  // 验证缓存大小限制生效
  let cache_size = MemoryCache::size(cache)
  assert_true(cache_size < 10000) // 应该有部分项被淘汰
  
  // 测试缓存命中率
  let hits = 0
  for i in 1..=1000 {
    let key = "key_" + i.to_string()
    let value = MemoryCache::get(cache, key)
    match value {
      Some(_) => hits = hits + 1
      None => ()
    }
  }
  
  let hit_rate = hits.to_float() / 1000.0
  assert_true(hit_rate > 0.5) // 命中率应该大于50%
  
  // 清空缓存
  MemoryCache::clear(cache)
  
  // 强制垃圾回收
  MemoryTracker::force_gc(memory_tracker)
  
  let after_cleanup_memory = MemoryTracker::get_current_usage(memory_tracker)
  let memory_recovered = after_population_memory - after_cleanup_memory
  
  // 验证内存回收
  assert_true(memory_recovered > memory_used * 0.8) // 至少回收80%的内存
  
  // 验证缓存统计
  let cache_stats = MemoryCache::get_statistics(cache)
  assert_true(cache_stats.put_count > 0)
  assert_true(cache_stats.get_count > 0)
  assert_true(cache_stats.eviction_count > 0)
  assert_eq(cache_stats.size, 0)
  
  // 验证没有缓存泄漏
  let cache_leak_detected = MemoryCache::detect_leaks(cache)
  assert_false(cache_leak_detected)
}

// Test 6: 事件队列内存管理
test "event queue memory management" {
  let memory_tracker = MemoryTracker::new()
  let event_queue = EventQueue::new(10000) // 最多10000个事件
  let initial_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // 添加大量事件
  for i in 1..=10000 {
    let event = Event::new("event_type_" + (i % 10).to_string(), Some([
      ("event_id", IntValue(i)),
      ("timestamp", IntValue(Time::now())),
      ("data", StringValue("event_data_" + i.to_string()))
    ]))
    
    let enqueue_result = EventQueue::enqueue(event_queue, event)
    if i <= 10000 {
      assert_true(enqueue_result) // 前10000个事件应该成功入队
    }
  }
  
  // 尝试添加超出队列容量的事件
  let overflow_event = Event::new("overflow_event", None)
  let overflow_result = EventQueue::enqueue(event_queue, overflow_event)
  assert_false(overflow_result) // 应该失败
  
  let after_population_memory = MemoryTracker::get_current_usage(memory_tracker)
  let memory_used = after_population_memory - initial_memory
  
  // 验证内存使用合理
  assert_true(memory_used > 0)
  let memory_per_event = memory_used.to_float() / 10000.0
  assert_true(memory_per_event < 256.0) // 每个事件应该使用少于256B内存
  
  // 处理事件
  let processed_count = 0
  while EventQueue::has_more(event_queue) {
    let event = EventQueue::dequeue(event_queue)
    match event {
      Some(e) => {
        // 处理事件
        processed_count = processed_count + 1
      }
      None => break
    }
  }
  
  // 验证所有事件都被处理
  assert_eq(processed_count, 10000)
  
  // 强制垃圾回收
  MemoryTracker::force_gc(memory_tracker)
  
  let after_processing_memory = MemoryTracker::get_current_usage(memory_tracker)
  let memory_recovered = after_population_memory - after_processing_memory
  
  // 验证内存回收
  assert_true(memory_recovered > memory_used * 0.8) // 至少回收80%的内存
  
  // 验证队列统计
  let queue_stats = EventQueue::get_statistics(event_queue)
  assert_eq(queue_stats.enqueued_count, 10000)
  assert_eq(queue_stats.dequeued_count, 10000)
  assert_eq(queue_stats.dropped_count, 1) // 一个溢出事件
  assert_eq(queue_stats.current_size, 0)
  
  // 验证没有队列泄漏
  let queue_leak_detected = EventQueue::detect_leaks(event_queue)
  assert_false(queue_leak_detected)
}

// Test 7: 序列化缓冲区内存管理
test "serialization buffer memory management" {
  let memory_tracker = MemoryTracker::new()
  let buffer_pool = BufferPool::new(100, 1024) // 100个1KB缓冲区
  let initial_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // 准备测试数据
  let trace_id = "trace_serialization_test"
  let span_id = "span_serialization_test"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("serialization_test_span", Internal, span_ctx)
  
  // 添加大量事件
  for i in 1..=100 {
    Span::add_event(span, "event_" + i.to_string(), Some([
      ("iteration", IntValue(i)),
      ("data", StringValue("serialization_test_data_" + i.to_string()))
    ]))
  }
  
  // 序列化测试
  let serialized_data = []
  for i in 1..=100 {
    let buffer = BufferPool::acquire(buffer_pool)
    match buffer {
      Some(buf) => {
        let serializer = JsonSerializer::new()
        let serialized = JsonSerializer::serialize_to_buffer(serializer, span, buf)
        serialized_data.push(serialized)
        BufferPool::release(buffer_pool, buf)
      }
      None => assert_true(false)
    }
  }
  
  let after_serialization_memory = MemoryTracker::get_current_usage(memory_tracker)
  let memory_used = after_serialization_memory - initial_memory
  
  // 验证内存使用合理
  assert_true(memory_used > 0)
  
  // 反序列化测试
  let deserializer = SpanDeserializer::new()
  for serialized in serialized_data {
    let deserialized = SpanDeserializer::deserialize_from_buffer(deserializer, serialized)
    assert_eq(Span::name(deserialized), "serialization_test_span")
  }
  
  // 清理序列化数据
  serialized_data.clear()
  
  // 强制垃圾回收
  MemoryTracker::force_gc(memory_tracker)
  
  let after_cleanup_memory = MemoryTracker::get_current_usage(memory_tracker)
  let memory_recovered = after_serialization_memory - after_cleanup_memory
  
  // 验证内存回收
  assert_true(memory_recovered > memory_used * 0.8) // 至少回收80%的内存
  
  // 验证缓冲区池统计
  let pool_stats = BufferPool::get_statistics(buffer_pool)
  assert_eq(pool_stats.total_buffers, 100)
  assert_eq(pool_stats.available_buffers, 100)
  assert_eq(pool_stats.acquired_count, 100)
  assert_eq(pool_stats.released_count, 100)
  
  // 验证没有缓冲区泄漏
  let buffer_leak_detected = BufferPool::detect_leaks(buffer_pool)
  assert_false(buffer_leak_detected)
}

// Test 8: 长时间运行内存稳定性
test "long-running memory stability" {
  let memory_tracker = MemoryTracker::new()
  let initial_memory = MemoryTracker::get_current_usage(memory_tracker)
  let memory_samples = []
  
  // 模拟长时间运行的操作
  for cycle in 1..=100 {
    // 创建和销毁大量对象
    let spans = []
    for i in 1..=1000 {
      let trace_id = "trace_" + cycle.to_string() + "_" + i.to_string()
      let span_id = "span_" + cycle.to_string() + "_" + i.to_string()
      let span_ctx = SpanContext::new(trace_id, span_id, true, "")
      let span = Span::new("long_running_span", Internal, span_ctx)
      
      // 添加事件
      for j in 1..=10 {
        Span::add_event(span, "event_" + j.to_string(), Some([
          ("cycle", IntValue(cycle)),
          ("iteration", IntValue(i)),
          ("event_id", IntValue(j))
        ]))
      }
      
      spans.push(span)
    }
    
    // 处理跨度
    for span in spans {
      Span::set_status(span, Ok, Some("Processed"))
      Span::end(span)
    }
    
    // 清理引用
    spans.clear()
    
    // 每10个周期记录一次内存使用情况
    if cycle % 10 == 0 {
      let current_memory = MemoryTracker::get_current_usage(memory_tracker)
      memory_samples.push(current_memory)
      
      // 偶尔强制垃圾回收
      if cycle % 30 == 0 {
        MemoryTracker::force_gc(memory_tracker)
      }
    }
  }
  
  // 最终垃圾回收
  MemoryTracker::force_gc(memory_tracker)
  
  let final_memory = MemoryTracker::get_current_usage(memory_tracker)
  let total_memory_growth = final_memory - initial_memory
  
  // 验证内存增长在合理范围内
  assert_true(total_memory_growth < 10 * 1024 * 1024) // 内存增长应该小于10MB
  
  // 验证内存使用稳定（没有持续增长）
  let memory_variance = MemoryTracker::calculate_variance(memory_samples)
  assert_true(memory_variance < 5 * 1024 * 1024) // 内存变化方差应该小于5MB
  
  // 检查内存泄漏趋势
  let leak_trend = MemoryTracker::analyze_leak_trend(memory_samples)
  assert_false(leak_trend.has_leak) // 不应该有内存泄漏趋势
  
  // 验证内存统计
  let memory_stats = MemoryTracker::get_comprehensive_statistics(memory_tracker)
  assert_true(memory_stats.total_allocations > 0)
  assert_true(memory_stats.total_deallocations > 0)
  assert_true(memory_stats.peak_usage > 0)
  assert_true(memory_stats.average_usage > 0)
  
  // 验证最终内存状态
  let final_memory_state = MemoryTracker::get_current_state(memory_tracker)
  assert_eq(final_memory_state.active_objects, 0) // 应该没有活跃对象
  assert_eq(final_memory_state.leaked_objects, 0) // 应该没有泄漏对象
  
  // 生成内存使用报告
  let memory_report = MemoryTracker::generate_report(memory_tracker)
  assert_true(memory_report.contains("memory_usage"))
  assert_true(memory_report.contains("allocations"))
  assert_true(memory_report.contains("deallocations"))
  assert_true(memory_report.contains("leak_analysis"))
}