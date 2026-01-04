// Azimuth 内存泄漏和资源管理测试用例
// 专注于内存泄漏检测和资源管理验证

// 测试1: Span对象内存泄漏检测
test "Span对象内存泄漏检测测试" {
  // 记录初始内存使用
  let initial_memory = Memory::used()
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "memory.leak.test")
  
  // 创建大量Span对象
  let span_count = 10000
  let spans = []
  
  for i in 0..span_count {
    let span = Tracer::start_span(tracer, "memory.leak.span." + i.to_string())
    Span::set_attribute(span, "iteration", IntValue(i))
    Span::set_attribute(span, "test.data", StringValue("test data " + i.to_string()))
    spans.push(span)
  }
  
  // 记录峰值内存使用
  let peak_memory = Memory::used()
  let memory_increase = peak_memory - initial_memory
  
  // 结束所有Span
  for span in spans {
    Span::end(span)
  }
  
  // 清空引用
  let spans = []
  
  // 强制垃圾回收
  Memory::gc()
  Memory::gc()  // 调用两次确保完整回收
  
  // 记录回收后内存使用
  let final_memory = Memory::used()
  let memory_reclaimed = peak_memory - final_memory
  let memory_leaked = final_memory - initial_memory
  
  // 内存泄漏验证
  let reclamation_ratio = memory_reclaimed.to_float() / memory_increase.to_float()
  assert_true(reclamation_ratio > 0.9)  // 至少回收90%的内存
  
  // 允许少量内存泄漏（由于缓存等原因）
  let leak_ratio = memory_leaked.to_float() / memory_increase.to_float()
  assert_true(leak_ratio < 0.1)  // 泄漏应该少于10%
  
  // 检查Span对象是否正确清理
  let active_spans = TracerProvider::get_active_span_count(tracer_provider)
  assert_eq(active_spans, 0)  // 应该没有活跃的Span
}

// 测试2: 指标对象内存泄漏检测
test "指标对象内存泄漏检测测试" {
  // 记录初始内存使用
  let initial_memory = Memory::used()
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "memory.leak.test")
  
  // 创建大量指标对象
  let metric_count = 5000
  let counters = []
  let histograms = []
  let gauges = []
  
  for i in 0..metric_count {
    // 创建计数器
    let counter = Meter::create_counter(meter, "memory.leak.counter." + i.to_string())
    Counter::add(counter, i.to_float())
    counters.push(counter)
    
    // 创建直方图
    if i % 2 == 0 {
      let histogram = Meter::create_histogram(meter, "memory.leak.histogram." + i.to_string())
      Histogram::record(histogram, i.to_float())
      histograms.push(histogram)
    }
    
    // 创建仪表
    if i % 3 == 0 {
      let gauge = Meter::create_gauge(meter, "memory.leak.gauge." + i.to_string())
      Gauge::set(gauge, i.to_float())
      gauges.push(gauge)
    }
  }
  
  // 记录峰值内存使用
  let peak_memory = Memory::used()
  let memory_increase = peak_memory - initial_memory
  
  // 清空引用
  let counters = []
  let histograms = []
  let gauges = []
  
  // 强制垃圾回收
  Memory::gc()
  Memory::gc()
  
  // 记录回收后内存使用
  let final_memory = Memory::used()
  let memory_reclaimed = peak_memory - final_memory
  let memory_leaked = final_memory - initial_memory
  
  // 内存泄漏验证
  let reclamation_ratio = memory_reclaimed.to_float() / memory_increase.to_float()
  assert_true(reclamation_ratio > 0.85)  // 至少回收85%的内存
  
  // 允许少量内存泄漏（由于指标缓存等原因）
  let leak_ratio = memory_leaked.to_float() / memory_increase.to_float()
  assert_true(leak_ratio < 0.15)  // 泄漏应该少于15%
  
  // 检查指标对象是否正确清理
  let active_metrics = MeterProvider::get_active_metric_count(meter_provider)
  assert_true(active_metrics < metric_count / 2)  // 大部分指标应该被清理
}

// 测试3: 日志对象内存泄漏检测
test "日志对象内存泄漏检测测试" {
  // 记录初始内存使用
  let initial_memory = Memory::used()
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "memory.leak.test")
  
  // 创建大量日志对象
  let log_count = 10000
  let log_records = []
  
  for i in 0..log_count {
    let log_attrs = Attributes::new()
    Attributes::set(log_attrs, "iteration", IntValue(i))
    Attributes::set(log_attrs, "test.data", StringValue("test log data " + i.to_string()))
    
    // 为部分日志添加更多属性
    if i % 10 == 0 {
      for j in 0..10 {
        Attributes::set(log_attrs, "extra.attr." + j.to_string(), StringValue("extra value " + j.to_string()))
      }
    }
    
    let log_record = LogRecord::new(
      if i % 4 == 0 { Info } else if i % 4 == 1 { Warn } else if i % 4 == 2 { Error } else { Debug },
      Some("Memory leak test log " + i.to_string()),
      Some(log_attrs),
      Some(Time::now()),
      Some(Time::now() + 1),
      Some("memory-leak-trace-" + i.to_string()),
      Some("memory-leak-span-" + i.to_string())
    )
    
    log_records.push(log_record)
    Logger::emit(logger, log_record)
  }
  
  // 记录峰值内存使用
  let peak_memory = Memory::used()
  let memory_increase = peak_memory - initial_memory
  
  // 清空引用
  let log_records = []
  
  // 强制垃圾回收
  Memory::gc()
  Memory::gc()
  
  // 记录回收后内存使用
  let final_memory = Memory::used()
  let memory_reclaimed = peak_memory - final_memory
  let memory_leaked = final_memory - initial_memory
  
  // 内存泄漏验证
  let reclamation_ratio = memory_reclaimed.to_float() / memory_increase.to_float()
  assert_true(reclamation_ratio > 0.8)  // 至少回收80%的内存
  
  // 允许少量内存泄漏（由于日志缓冲等原因）
  let leak_ratio = memory_leaked.to_float() / memory_increase.to_float()
  assert_true(leak_ratio < 0.2)  // 泄漏应该少于20%
}

// 测试4: 属性对象内存泄漏检测
test "属性对象内存泄漏检测测试" {
  // 记录初始内存使用
  let initial_memory = Memory::used()
  
  // 创建大量属性对象
  let attr_count = 5000
  let attributes_list = []
  
  for i in 0..attr_count {
    let attrs = Attributes::new()
    
    // 添加基本属性
    Attributes::set(attrs, "iteration", IntValue(i))
    Attributes::set(attrs, "string.value", StringValue("string " + i.to_string()))
    Attributes::set(attrs, "float.value", FloatValue(i.to_float()))
    Attributes::set(attrs, "bool.value", BoolValue(i % 2 == 0))
    
    // 添加数组属性
    if i % 5 == 0 {
      let string_array = []
      for j in 0..5 {
        string_array = string_array.push("array item " + j.to_string())
      }
      Attributes::set(attrs, "string.array", ArrayStringValue(string_array))
      
      let int_array = []
      for j in 0..5 {
        int_array = int_array.push(i * 10 + j)
      }
      Attributes::set(attrs, "int.array", ArrayIntValue(int_array))
    }
    
    attributes_list.push(attrs)
  }
  
  // 记录峰值内存使用
  let peak_memory = Memory::used()
  let memory_increase = peak_memory - initial_memory
  
  // 清空引用
  let attributes_list = []
  
  // 强制垃圾回收
  Memory::gc()
  Memory::gc()
  
  // 记录回收后内存使用
  let final_memory = Memory::used()
  let memory_reclaimed = peak_memory - final_memory
  let memory_leaked = final_memory - initial_memory
  
  // 内存泄漏验证
  let reclamation_ratio = memory_reclaimed.to_float() / memory_increase.to_float()
  assert_true(reclamation_ratio > 0.85)  // 至少回收85%的内存
  
  // 允许少量内存泄漏
  let leak_ratio = memory_leaked.to_float() / memory_increase.to_float()
  assert_true(leak_ratio < 0.15)  // 泄漏应该少于15%
}

// 测试5: 上下文对象内存泄漏检测
test "上下文对象内存泄漏检测测试" {
  // 记录初始内存使用
  let initial_memory = Memory::used()
  
  // 创建大量嵌套上下文
  let context_count = 2000
  let contexts = []
  
  for i in 0..context_count {
    let mut context = Context::root()
    
    // 创建嵌套上下文
    for j in 0..10 {
      let key = ContextKey::new("context.key." + j.to_string())
      let value = "context.value." + i.to_string() + "." + j.to_string()
      context = Context::with_value(context, key, value)
    }
    
    contexts.push(context)
  }
  
  // 记录峰值内存使用
  let peak_memory = Memory::used()
  let memory_increase = peak_memory - initial_memory
  
  // 清空引用
  let contexts = []
  
  // 强制垃圾回收
  Memory::gc()
  Memory::gc()
  
  // 记录回收后内存使用
  let final_memory = Memory::used()
  let memory_reclaimed = peak_memory - final_memory
  let memory_leaked = final_memory - initial_memory
  
  // 内存泄漏验证
  let reclamation_ratio = memory_reclaimed.to_float() / memory_increase.to_float()
  assert_true(reclamation_ratio > 0.9)  // 至少回收90%的内存
  
  // 允许少量内存泄漏
  let leak_ratio = memory_leaked.to_float() / memory_increase.to_float()
  assert_true(leak_ratio < 0.1)  // 泄漏应该少于10%
}

// 测试6: 资源对象内存泄漏检测
test "资源对象内存泄漏检测测试" {
  // 记录初始内存使用
  let initial_memory = Memory::used()
  
  // 创建大量资源对象
  let resource_count = 1000
  let resources = []
  
  for i in 0..resource_count {
    let resource = Resource::new()
    
    // 添加大量属性
    for j in 0..20 {
      let key = "resource.attr." + j.to_string()
      let value = "resource.value." + i.to_string() + "." + j.to_string()
      Resource::set_attribute(resource, key, StringValue(value))
    }
    
    resources.push(resource)
  }
  
  // 记录峰值内存使用
  let peak_memory = Memory::used()
  let memory_increase = peak_memory - initial_memory
  
  // 清空引用
  let resources = []
  
  // 强制垃圾回收
  Memory::gc()
  Memory::gc()
  
  // 记录回收后内存使用
  let final_memory = Memory::used()
  let memory_reclaimed = peak_memory - final_memory
  let memory_leaked = final_memory - initial_memory
  
  // 内存泄漏验证
  let reclamation_ratio = memory_reclaimed.to_float() / memory_increase.to_float()
  assert_true(reclamation_ratio > 0.85)  // 至少回收85%的内存
  
  // 允许少量内存泄漏
  let leak_ratio = memory_leaked.to_float() / memory_increase.to_float()
  assert_true(leak_ratio < 0.15)  // 泄漏应该少于15%
}

// 测试7: 长时间运行内存泄漏检测
test "长时间运行内存泄漏检测测试" {
  // 记录初始内存使用
  let initial_memory = Memory::used()
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "long.running.leak.test")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "long.running.leak.test")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "long.running.leak.test")
  
  // 模拟长时间运行场景
  let iterations = 100
  let memory_snapshots = []
  
  for iteration in 0..iterations {
    // 创建和销毁对象
    for i in 0..100 {
      // 创建Span
      let span = Tracer::start_span(tracer, "long.running.span." + i.to_string())
      Span::set_attribute(span, "iteration", IntValue(iteration))
      Span::set_attribute(span, "item", IntValue(i))
      Span::end(span)
      
      // 创建指标
      if i % 10 == 0 {
        let counter = Meter::create_counter(meter, "iteration." + iteration.to_string() + ".counter")
        Counter::add(counter, 1.0)
      }
      
      // 创建日志
      if i % 5 == 0 {
        let log_attrs = Attributes::new()
        Attributes::set(log_attrs, "iteration", IntValue(iteration))
        Attributes::set(log_attrs, "item", IntValue(i))
        
        let log_record = LogRecord::new(
          Info,
          Some("Long running log " + i.to_string()),
          Some(log_attrs),
          Some(Time::now()),
          Some(Time::now() + 1),
          Some("long-running-trace-" + iteration.to_string() + "-" + i.to_string()),
          Some("long-running-span-" + iteration.to_string() + "-" + i.to_string())
        )
        Logger::emit(logger, log_record)
      }
    }
    
    // 定期记录内存使用情况
    if iteration % 10 == 0 {
      Memory::gc()
      let current_memory = Memory::used()
      memory_snapshots.push((iteration, current_memory))
    }
  }
  
  // 最终垃圾回收
  Memory::gc()
  Memory::gc()
  
  // 记录最终内存使用
  let final_memory = Memory::used()
  let total_increase = final_memory - initial_memory
  
  // 分析内存增长趋势
  let memory_growth_rate = if memory_snapshots.length() >= 2 {
    let (first_iter, first_memory) = memory_snapshots[0]
    let (last_iter, last_memory) = memory_snapshots[memory_snapshots.length() - 1]
    
    let iter_diff = last_iter - first_iter
    let memory_diff = last_memory - first_memory
    
    if iter_diff > 0 {
      memory_diff.to_float() / iter_diff.to_float()
    } else {
      0.0
    }
  } else {
    0.0
  }
  
  // 长时间运行内存泄漏验证
  assert_true(memory_growth_rate < 10000.0)  // 每次迭代内存增长应小于10KB
  
  // 总内存增长应该在合理范围内
  let reasonable_increase = 50 * 1024 * 1024  // 50MB
  assert_true(total_increase < reasonable_increase)
  
  // 检查内存增长是否线性（可能是内存泄漏）
  if memory_snapshots.length() >= 3 {
    let mut is_linear_growth = true
    let mut prev_growth = 0
    
    for i in 1..memory_snapshots.length() {
      let (curr_iter, curr_memory) = memory_snapshots[i]
      let (prev_iter, prev_memory) = memory_snapshots[i - 1]
      
      let iter_diff = curr_iter - prev_iter
      let memory_diff = curr_memory - prev_memory
      let current_growth = if iter_diff > 0 { memory_diff / iter_diff } else { 0 }
      
      if i > 1 && current_growth > prev_growth * 1.5 {
        is_linear_growth = false
      }
      
      prev_growth = current_growth
    }
    
    // 内存增长应该是线性的或亚线性的
    assert_true(is_linear_growth)
  }
}

// 测试8: 并发场景内存泄漏检测
test "并发场景内存泄漏检测测试" {
  // 记录初始内存使用
  let initial_memory = Memory::used()
  
  // 创建并发任务
  let thread_count = 10
  let operations_per_thread = 1000
  
  let concurrent_tasks = []
  
  for thread_id in 0..thread_count {
    let task = fn() {
      let tracer_provider = TracerProvider::default()
      let tracer = TracerProvider::get_tracer(tracer_provider, "concurrent.leak.test." + thread_id.to_string())
      
      let meter_provider = MeterProvider::default()
      let meter = MeterProvider::get_meter(meter_provider, "concurrent.leak.test." + thread_id.to_string())
      
      let logger_provider = LoggerProvider::default()
      let logger = LoggerProvider::get_logger(logger_provider, "concurrent.leak.test." + thread_id.to_string())
      
      // 每个线程创建和销毁对象
      for i in 0..operations_per_thread {
        // 创建Span
        let span = Tracer::start_span(tracer, "concurrent.span." + i.to_string())
        Span::set_attribute(span, "thread_id", IntValue(thread_id))
        Span::set_attribute(span, "operation_id", IntValue(i))
        Span::end(span)
        
        // 创建指标
        if i % 50 == 0 {
          let counter = Meter::create_counter(meter, "thread." + thread_id.to_string() + ".counter")
          Counter::add(counter, 1.0)
        }
        
        // 创建日志
        if i % 25 == 0 {
          let log_attrs = Attributes::new()
          Attributes::set(log_attrs, "thread_id", IntValue(thread_id))
          Attributes::set(log_attrs, "operation_id", IntValue(i))
          
          let log_record = LogRecord::new(
            Info,
            Some("Concurrent log " + i.to_string()),
            Some(log_attrs),
            Some(Time::now()),
            Some(Time::now() + 1),
            Some("concurrent-trace-" + thread_id.to_string() + "-" + i.to_string()),
            Some("concurrent-span-" + thread_id.to_string() + "-" + i.to_string())
          )
          Logger::emit(logger, log_record)
        }
      }
    }
    
    concurrent_tasks.push(task)
  }
  
  // 执行并发任务
  let _ = Concurrent::execute_all(concurrent_tasks)
  
  // 强制垃圾回收
  Memory::gc()
  Memory::gc()
  
  // 记录最终内存使用
  let final_memory = Memory::used()
  let memory_increase = final_memory - initial_memory
  
  // 并发场景内存泄漏验证
  let total_operations = thread_count * operations_per_thread
  let memory_per_operation = memory_increase.to_float() / total_operations.to_float()
  
  assert_true(memory_per_operation < 1024.0)  // 每个操作内存开销应小于1KB
  
  // 总内存增长应该在合理范围内
  let reasonable_increase = 100 * 1024 * 1024  // 100MB
  assert_true(memory_increase < reasonable_increase)
}

// 测试9: 资源池泄漏检测
test "资源池泄漏检测测试" {
  // 记录初始内存使用
  let initial_memory = Memory::used()
  
  // 创建资源池
  let pool_size = 100
  let resource_pool = ResourcePool::new(pool_size)
  
  // 获取资源但不释放（模拟泄漏）
  let leaked_resources = []
  
  for i in 0..pool_size {
    let resource = ResourcePool::acquire(resource_pool)
    leaked_resources.push(resource)
  }
  
  // 尝试获取更多资源（应该失败）
  let extra_resource = ResourcePool::try_acquire(resource_pool)
  assert_true(extra_resource.is_none())  // 应该无法获取额外资源
  
  // 记录峰值内存使用
  let peak_memory = Memory::used()
  
  // 释放部分资源
  for i in 0..(pool_size / 2) {
    ResourcePool::release(resource_pool, leaked_resources[i])
  }
  
  // 现在应该能够获取资源
  let available_resource = ResourcePool::try_acquire(resource_pool)
  assert_true(available_resource.is_some())  // 应该能够获取资源
  
  // 释放剩余资源
  for i in (pool_size / 2)..pool_size {
    ResourcePool::release(resource_pool, leaked_resources[i])
  }
  
  // 强制垃圾回收
  Memory::gc()
  Memory::gc()
  
  // 记录最终内存使用
  let final_memory = Memory::used()
  let memory_increase = final_memory - initial_memory
  
  // 资源池泄漏验证
  let reasonable_increase = 10 * 1024 * 1024  // 10MB
  assert_true(memory_increase < reasonable_increase)
  
  // 验证资源池状态
  let available_count = ResourcePool::available_count(resource_pool)
  assert_eq(available_count, pool_size)  // 所有资源应该可用
}

// 测试10: 循环引用内存泄漏检测
test "循环引用内存泄漏检测测试" {
  // 记录初始内存使用
  let initial_memory = Memory::used()
  
  // 创建可能导致循环引用的对象结构
  let circular_objects = []
  
  for i in 0..1000 {
    // 创建父对象
    let parent_attrs = Attributes::new()
    Attributes::set(parent_attrs, "type", StringValue("parent"))
    Attributes::set(parent_attrs, "id", IntValue(i))
    
    // 创建子对象
    let child_attrs = Attributes::new()
    Attributes::set(child_attrs, "type", StringValue("child"))
    Attributes::set(child_attrs, "parent_id", IntValue(i))
    
    // 创建相互引用（模拟循环引用）
    Attributes::set(parent_attrs, "child_ref", StringValue("child_" + i.to_string()))
    Attributes::set(child_attrs, "parent_ref", StringValue("parent_" + i.to_string()))
    
    circular_objects.push((parent_attrs, child_attrs))
  }
  
  // 记录峰值内存使用
  let peak_memory = Memory::used()
  let memory_increase = peak_memory - initial_memory
  
  // 清空引用
  let circular_objects = []
  
  // 强制垃圾回收
  Memory::gc()
  Memory::gc()
  
  // 记录最终内存使用
  let final_memory = Memory::used()
  let memory_reclaimed = peak_memory - final_memory
  let memory_leaked = final_memory - initial_memory
  
  // 循环引用内存泄漏验证
  let reclamation_ratio = memory_reclaimed.to_float() / memory_increase.to_float()
  assert_true(reclamation_ratio > 0.8)  // 至少回收80%的内存
  
  // 允许少量内存泄漏
  let leak_ratio = memory_leaked.to_float() / memory_increase.to_float()
  assert_true(leak_ratio < 0.2)  // 泄漏应该少于20%
}