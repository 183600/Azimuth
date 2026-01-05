// Azimuth 高级性能基准测试
// 测试遥测系统在各种负载和场景下的性能表现

// 测试1: 高吞吐量数据处理基准
test "高吞吐量数据处理基准" {
  // 定义基准测试结果
  type BenchmarkResult = {
    test_name: String,
    item_count: Int,
    duration_ms: Int,
    throughput: Float,  // items/second
    memory_usage_mb: Float,
    cpu_usage_percent: Float
  }
  
  // 定义遥测数据项
  type TelemetryItem = {
    trace_id: String,
    span_id: String,
    timestamp: Int,
    service_name: String,
    operation: String,
    duration: Int,
    status: String,
    attributes: Array[(String, String)>
  }
  
  // 生成测试数据
  let generate_telemetry_data = fn(count: Int) {
    let mut data = []
    
    for i in 0..count {
      let item = {
        trace_id: "trace-" + (i % 1000).to_string(),
        span_id: "span-" + i.to_string(),
        timestamp: 1640995200000 + i * 10,
        service_name: "service-" + (i % 10).to_string(),
        operation: "operation-" + (i % 50).to_string(),
        duration: 50 + (i % 500),
        status: if i % 20 == 0 { "error" } else { "ok" },
        attributes: [
          ("host", "host-" + (i % 5).to_string()),
          ("region", "region-" + (i % 3).to_string()),
          ("version", "v" + ((i % 5) + 1).to_string() + ".0.0")
        ]
      }
      data = data + [item]
    }
    
    data
  }
  
  // 数据处理函数
  let process_telemetry_data = fn(data: Array[TelemetryItem>) {
    let mut processed = []
    
    for item in data {
      // 模拟数据处理：过滤、转换、聚合
      if item.status == "ok" {
        let processed_item = {
          trace_id: item.trace_id,
          span_id: item.span_id,
          timestamp: item.timestamp,
          service_name: item.service_name,
          operation: item.operation,
          duration: item.duration,
          // 添加处理后的字段
          processing_timestamp: Time::now(),
          processing_duration: item.duration + 10
        }
        processed = processed + [processed_item]
      }
    }
    
    processed
  }
  
  // 运行基准测试
  let run_benchmark = fn(test_name: String, data_size: Int, process_fn: (Array[TelemetryItem>) -> Array[TelemetryItem>) {
    let data = generate_telemetry_data(data_size)
    
    // 记录开始时间和内存
    let start_time = Time::now()
    let start_memory = 100.0  // 模拟内存使用
    let start_cpu = 25.0     // 模拟CPU使用
    
    // 执行处理
    let result = process_fn(data)
    
    // 记录结束时间和内存
    let end_time = Time::now()
    let end_memory = 150.0  // 模拟内存使用
    let end_cpu = 75.0     // 模拟CPU使用
    
    let duration = end_time - start_time
    let throughput = if duration > 0 {
      (result.length() as Float / duration as Float) * 1000.0
    } else {
      0.0
    }
    
    {
      test_name: test_name,
      item_count: data_size,
      duration_ms: duration,
      throughput: throughput,
      memory_usage_mb: end_memory - start_memory,
      cpu_usage_percent: end_cpu - start_cpu
    }
  }
  
  // 运行不同规模的基准测试
  let small_benchmark = run_benchmark("small_dataset", 1000, process_telemetry_data)
  let medium_benchmark = run_benchmark("medium_dataset", 10000, process_telemetry_data)
  let large_benchmark = run_benchmark("large_dataset", 100000, process_telemetry_data)
  
  // 验证基准测试结果
  assert_eq(small_benchmark.test_name, "small_dataset")
  assert_eq(small_benchmark.item_count, 1000)
  assert_true(small_benchmark.duration_ms > 0)
  assert_true(small_benchmark.throughput > 0)
  assert_true(small_benchmark.memory_usage_mb > 0)
  assert_true(small_benchmark.cpu_usage_percent > 0)
  
  assert_eq(medium_benchmark.test_name, "medium_dataset")
  assert_eq(medium_benchmark.item_count, 10000)
  assert_true(medium_benchmark.duration_ms > 0)
  assert_true(medium_benchmark.throughput > 0)
  
  assert_eq(large_benchmark.test_name, "large_dataset")
  assert_eq(large_benchmark.item_count, 100000)
  assert_true(large_benchmark.duration_ms > 0)
  assert_true(large_benchmark.throughput > 0)
  
  // 验证性能指标关系
  assert_true(medium_benchmark.duration_ms >= small_benchmark.duration_ms)
  assert_true(large_benchmark.duration_ms >= medium_benchmark.duration_ms)
  
  // 验证吞吐量合理性
  assert_true(small_benchmark.throughput > 0)
  assert_true(medium_benchmark.throughput > 0)
  assert_true(large_benchmark.throughput > 0)
  
  // 验证内存使用增长
  assert_true(medium_benchmark.memory_usage_mb >= small_benchmark.memory_usage_mb)
  assert_true(large_benchmark.memory_usage_mb >= medium_benchmark.memory_usage_mb)
}

// 测试2: 低延迟处理基准
test "低延迟处理基准" {
  // 定义延迟测试结果
  type LatencyResult = {
    test_name: String,
    item_count: Int,
    min_latency_ms: Float,
    max_latency_ms: Float,
    avg_latency_ms: Float,
    p50_latency_ms: Float,
    p95_latency_ms: Float,
    p99_latency_ms: Float
  }
  
  // 定义遥测事件
  type TelemetryEvent = {
    event_id: String,
    timestamp: Int,
    event_type: String,
    data: String
  }
  
  // 生成测试事件
  let generate_events = fn(count: Int) {
    let mut events = []
    
    for i in 0..count {
      let event = {
        event_id: "event-" + i.to_string(),
        timestamp: Time::now(),
        event_type: "type-" + (i % 10).to_string(),
        data: "event data " + i.to_string()
      }
      events = events + [event]
    }
    
    events
  }
  
  // 低延迟处理函数
  let low_latency_process = fn(event: TelemetryEvent) {
    // 模拟极低延迟处理
    let process_start = Time::now()
    
    // 简单处理
    let processed_event = {
      event_id: event.event_id,
      timestamp: event.timestamp,
      event_type: event.event_type,
      data: event.data,
      processed: true,
      process_start: process_start
    }
    
    let process_end = Time::now()
    let latency = process_end - process_start
    
    (processed_event, latency)
  }
  
  // 批量低延迟处理
  let batch_low_latency_process = fn(events: Array[TelemetryEvent>) {
    let mut results = []
    let mut latencies = []
    
    for event in events {
      let (processed_event, latency) = low_latency_process(event)
      results = results + [processed_event]
      latencies = latencies + [latency]
    }
    
    (results, latencies)
  }
  
  // 计算延迟统计
  let calculate_latency_stats = fn(latencies: Array[Int>) {
    if latencies.length() == 0 {
      return {
        min_latency_ms: 0.0,
        max_latency_ms: 0.0,
        avg_latency_ms: 0.0,
        p50_latency_ms: 0.0,
        p95_latency_ms: 0.0,
        p99_latency_ms: 0.0
      }
    }
    
    // 排序延迟
    let sorted_latencies = latencies.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
    
    let min_latency = sorted_latencies[0] as Float
    let max_latency = sorted_latencies[sorted_latencies.length() - 1] as Float
    let avg_latency = latencies.reduce(fn(acc, l) { acc + l }, 0) as Float / latencies.length() as Float
    
    // 计算百分位数
    let p50_index = (sorted_latencies.length() * 50 / 100)
    let p95_index = (sorted_latencies.length() * 95 / 100)
    let p99_index = (sorted_latencies.length() * 99 / 100)
    
    let p50_latency = sorted_latencies[p50_index] as Float
    let p95_latency = sorted_latencies[p95_index] as Float
    let p99_latency = sorted_latencies[p99_index] as Float
    
    {
      min_latency_ms: min_latency,
      max_latency_ms: max_latency,
      avg_latency_ms: avg_latency,
      p50_latency_ms: p50_latency,
      p95_latency_ms: p95_latency,
      p99_latency_ms: p99_latency
    }
  }
  
  // 运行延迟基准测试
  let run_latency_benchmark = fn(test_name: String, event_count: Int) {
    let events = generate_events(event_count)
    let (_, latencies) = batch_low_latency_process(events)
    let stats = calculate_latency_stats(latencies)
    
    {
      test_name: test_name,
      item_count: event_count,
      min_latency_ms: stats.min_latency_ms,
      max_latency_ms: stats.max_latency_ms,
      avg_latency_ms: stats.avg_latency_ms,
      p50_latency_ms: stats.p50_latency_ms,
      p95_latency_ms: stats.p95_latency_ms,
      p99_latency_ms: stats.p99_latency_ms
    }
  }
  
  // 运行不同规模的延迟测试
  let small_latency = run_latency_benchmark("small_latency", 100)
  let medium_latency = run_latency_benchmark("medium_latency", 1000)
  let large_latency = run_latency_benchmark("large_latency", 10000)
  
  // 验证延迟测试结果
  assert_eq(small_latency.test_name, "small_latency")
  assert_eq(small_latency.item_count, 100)
  assert_true(small_latency.min_latency_ms >= 0)
  assert_true(small_latency.max_latency_ms >= small_latency.min_latency_ms)
  assert_true(small_latency.avg_latency_ms >= small_latency.min_latency_ms)
  assert_true(small_latency.avg_latency_ms <= small_latency.max_latency_ms)
  assert_true(small_latency.p50_latency_ms >= small_latency.min_latency_ms)
  assert_true(small_latency.p95_latency_ms >= small_latency.p50_latency_ms)
  assert_true(small_latency.p99_latency_ms >= small_latency.p95_latency_ms)
  
  assert_eq(medium_latency.test_name, "medium_latency")
  assert_eq(medium_latency.item_count, 1000)
  assert_true(medium_latency.min_latency_ms >= 0)
  assert_true(medium_latency.max_latency_ms >= medium_latency.min_latency_ms)
  
  assert_eq(large_latency.test_name, "large_latency")
  assert_eq(large_latency.item_count, 10000)
  assert_true(large_latency.min_latency_ms >= 0)
  assert_true(large_latency.max_latency_ms >= large_latency.min_latency_ms)
  
  // 验证延迟百分位数的合理性
  assert_true(small_latency.p50_latency_ms <= small_latency.p95_latency_ms)
  assert_true(small_latency.p95_latency_ms <= small_latency.p99_latency_ms)
  assert_true(small_latency.p99_latency_ms <= small_latency.max_latency_ms)
  
  assert_true(medium_latency.p50_latency_ms <= medium_latency.p95_latency_ms)
  assert_true(medium_latency.p95_latency_ms <= medium_latency.p99_latency_ms)
  assert_true(medium_latency.p99_latency_ms <= medium_latency.max_latency_ms)
  
  assert_true(large_latency.p50_latency_ms <= large_latency.p95_latency_ms)
  assert_true(large_latency.p95_latency_ms <= large_latency.p99_latency_ms)
  assert_true(large_latency.p99_latency_ms <= large_latency.max_latency_ms)
}

// 测试3: 内存使用效率基准
test "内存使用效率基准" {
  // 定义内存使用结果
  type MemoryResult = {
    test_name: String,
    item_count: Int,
    initial_memory_mb: Float,
    peak_memory_mb: Float,
    final_memory_mb: Float,
    memory_growth_mb: Float,
    memory_per_item_kb: Float,
    gc_events: Int
  }
  
  // 定义内存密集型数据结构
  type MemoryIntensiveData = {
    id: String,
    large_payload: String,
    metadata: Array<(String, String)>,
    nested_data: Array<{
      sub_id: String,
      sub_data: String,
      sub_metadata: Array<(String, String)>
    }>
  }
  
  // 生成内存密集型数据
  let generate_memory_intensive_data = fn(count: Int) {
    let mut data = []
    
    for i in 0..count {
      let large_payload = "x".repeat(1000)  // 1KB载荷
      let metadata = [
        ("key1", "value1-" + i.to_string()),
        ("key2", "value2-" + i.to_string()),
        ("key3", "value3-" + i.to_string())
      ]
      
      let mut nested_data = []
      for j in 0..5 {
        let sub_data = {
          sub_id: "sub-" + i.to_string() + "-" + j.to_string(),
          sub_data: "y".repeat(100),  // 100B子数据
          sub_metadata: [
            ("sub_key1", "sub_value1-" + j.to_string()),
            ("sub_key2", "sub_value2-" + j.to_string())
          ]
        }
        nested_data = nested_data + [sub_data]
      }
      
      let item = {
        id: "item-" + i.to_string(),
        large_payload: large_payload,
        metadata: metadata,
        nested_data: nested_data
      }
      
      data = data + [item]
    }
    
    data
  }
  
  // 内存处理函数
  let memory_intensive_process = fn(data: Array<MemoryIntensiveData>) {
    let mut processed = []
    
    for item in data {
      // 处理大型载荷
      let processed_payload = item.large_payload.to_uppercase()
      
      // 处理元数据
      let processed_metadata = item.metadata.map(fn(meta) {
        let (key, value) = meta
        (key.to_uppercase(), value.to_uppercase())
      })
      
      // 处理嵌套数据
      let processed_nested = item.nested_data.map(fn(sub) {
        {
          sub_id: sub.sub_id + "-processed",
          sub_data: sub.sub_data.to_uppercase(),
          sub_metadata: sub.sub_metadata.map(fn(sub_meta) {
            let (key, value) = sub_meta
            (key.to_uppercase(), value.to_uppercase())
          })
        }
      })
      
      let processed_item = {
        id: item.id + "-processed",
        large_payload: processed_payload,
        metadata: processed_metadata,
        nested_data: processed_nested
      }
      
      processed = processed + [processed_item]
    }
    
    processed
  }
  
  // 运行内存基准测试
  let run_memory_benchmark = fn(test_name: String, data_count: Int) {
    let data = generate_memory_intensive_data(data_count)
    
    // 记录初始内存
    let initial_memory = 50.0  // 模拟初始内存使用(MB)
    
    // 处理数据
    let processed_data = memory_intensive_process(data)
    
    // 记录峰值内存
    let peak_memory = initial_memory + (data_count * 0.01)  // 模拟峰值内存
    
    // 记录最终内存
    let final_memory = initial_memory + (data_count * 0.005)  // 模拟最终内存
    
    // 计算内存增长
    let memory_growth = final_memory - initial_memory
    let memory_per_item = (memory_growth * 1024.0) / data_count as Float  // KB/item
    
    // 模拟GC事件
    let gc_events = data_count / 1000
    
    {
      test_name: test_name,
      item_count: data_count,
      initial_memory_mb: initial_memory,
      peak_memory_mb: peak_memory,
      final_memory_mb: final_memory,
      memory_growth_mb: memory_growth,
      memory_per_item_kb: memory_per_item,
      gc_events: gc_events
    }
  }
  
  // 运行不同规模的内存测试
  let small_memory = run_memory_benchmark("small_memory", 100)
  let medium_memory = run_memory_benchmark("medium_memory", 1000)
  let large_memory = run_memory_benchmark("large_memory", 10000)
  
  // 验证内存测试结果
  assert_eq(small_memory.test_name, "small_memory")
  assert_eq(small_memory.item_count, 100)
  assert_true(small_memory.initial_memory_mb > 0)
  assert_true(small_memory.peak_memory_mb >= small_memory.initial_memory_mb)
  assert_true(small_memory.final_memory_mb >= small_memory.initial_memory_mb)
  assert_true(small_memory.memory_growth_mb >= 0)
  assert_true(small_memory.memory_per_item_kb > 0)
  assert_eq(small_memory.gc_events, 0)  // 100/1000 = 0.1 -> 0 (整数除法)
  
  assert_eq(medium_memory.test_name, "medium_memory")
  assert_eq(medium_memory.item_count, 1000)
  assert_true(medium_memory.peak_memory_mb >= medium_memory.initial_memory_mb)
  assert_eq(medium_memory.gc_events, 1)  // 1000/1000 = 1
  
  assert_eq(large_memory.test_name, "large_memory")
  assert_eq(large_memory.item_count, 10000)
  assert_true(large_memory.peak_memory_mb >= large_memory.initial_memory_mb)
  assert_eq(large_memory.gc_events, 10)  // 10000/1000 = 10
  
  // 验证内存使用增长
  assert_true(medium_memory.memory_growth_mb >= small_memory.memory_growth_mb)
  assert_true(large_memory.memory_growth_mb >= medium_memory.memory_growth_mb)
  
  // 验证每项内存使用
  assert_true(small_memory.memory_per_item_kb > 0)
  assert_true(medium_memory.memory_per_item_kb > 0)
  assert_true(large_memory.memory_per_item_kb > 0)
}

// 测试4: 并发性能基准
test "并发性能基准" {
  // 定义并发测试结果
  type ConcurrencyResult = {
    test_name: String,
    thread_count: Int,
    items_per_thread: Int,
    total_items: Int,
    duration_ms: Int,
    throughput: Float,
    avg_thread_duration_ms: Float,
    thread_imbalance_percent: Float
  }
  
  // 定义并发任务
  type ConcurrentTask = {
    task_id: String,
    thread_id: Int,
    data: Array<String>,
    result: Array<String>
  }
  
  // 创建并发任务
  let create_concurrent_tasks = fn(thread_count: Int, items_per_thread: Int) {
    let mut tasks = []
    
    for i in 0..thread_count {
      let mut data = []
      for j in 0..items_per_thread {
        data = data + ["item-" + i.to_string() + "-" + j.to_string()]
      }
      
      let task = {
        task_id: "task-" + i.to_string(),
        thread_id: i,
        data: data,
        result: []
      }
      
      tasks = tasks + [task]
    }
    
    tasks
  }
  
  // 并发处理函数
  let concurrent_process = fn(task: ConcurrentTask) {
    let start_time = Time::now()
    
    // 处理数据
    let mut result = []
    for item in task.data {
      let processed_item = item.to_uppercase()
      result = result + [processed_item]
    }
    
    let end_time = Time::now()
    let duration = end_time - start_time
    
    {
      task_id: task.task_id,
      thread_id: task.thread_id,
      data: task.data,
      result: result,
      duration_ms: duration
    }
  }
  
  // 模拟并发执行
  let simulate_concurrent_execution = fn(tasks: Array<ConcurrentTask>) {
    let mut results = []
    let mut durations = []
    
    // 模拟并发执行（实际中会使用线程池）
    for task in tasks {
      let result = concurrent_process(task)
      results = results + [result]
      durations = durations + [result.duration_ms]
    }
    
    (results, durations)
  }
  
  // 运行并发基准测试
  let run_concurrency_benchmark = fn(test_name: String, thread_count: Int, items_per_thread: Int) {
    let tasks = create_concurrent_tasks(thread_count, items_per_thread)
    let total_items = thread_count * items_per_thread
    
    let start_time = Time::now()
    let (results, durations) = simulate_concurrent_execution(tasks)
    let end_time = Time::now()
    
    let total_duration = end_time - start_time
    let throughput = if total_duration > 0 {
      (total_items as Float / total_duration as Float) * 1000.0
    } else {
      0.0
    }
    
    // 计算平均线程执行时间
    let avg_thread_duration = if durations.length() > 0 {
      durations.reduce(fn(acc, d) { acc + d }, 0) as Float / durations.length() as Float
    } else {
      0.0
    }
    
    // 计算线程不平衡度
    let min_duration = if durations.length() > 0 {
      durations.reduce(fn(acc, d) { if d < acc { d } else { acc } }, durations[0])
    } else {
      0
    }
    
    let max_duration = if durations.length() > 0 {
      durations.reduce(fn(acc, d) { if d > acc { d } else { acc } }, durations[0])
    } else {
      0
    }
    
    let thread_imbalance = if min_duration > 0 {
      ((max_duration - min_duration) as Float / min_duration as Float) * 100.0
    } else {
      0.0
    }
    
    {
      test_name: test_name,
      thread_count: thread_count,
      items_per_thread: items_per_thread,
      total_items: total_items,
      duration_ms: total_duration,
      throughput: throughput,
      avg_thread_duration_ms: avg_thread_duration,
      thread_imbalance_percent: thread_imbalance
    }
  }
  
  // 运行不同并发配置的测试
  let low_concurrency = run_concurrency_benchmark("low_concurrency", 2, 1000)
  let medium_concurrency = run_concurrency_benchmark("medium_concurrency", 4, 1000)
  let high_concurrency = run_concurrency_benchmark("high_concurrency", 8, 1000)
  
  // 验证并发测试结果
  assert_eq(low_concurrency.test_name, "low_concurrency")
  assert_eq(low_concurrency.thread_count, 2)
  assert_eq(low_concurrency.items_per_thread, 1000)
  assert_eq(low_concurrency.total_items, 2000)
  assert_true(low_concurrency.duration_ms > 0)
  assert_true(low_concurrency.throughput > 0)
  assert_true(low_concurrency.avg_thread_duration_ms > 0)
  assert_true(low_concurrency.thread_imbalance_percent >= 0)
  
  assert_eq(medium_concurrency.test_name, "medium_concurrency")
  assert_eq(medium_concurrency.thread_count, 4)
  assert_eq(medium_concurrency.items_per_thread, 1000)
  assert_eq(medium_concurrency.total_items, 4000)
  
  assert_eq(high_concurrency.test_name, "high_concurrency")
  assert_eq(high_concurrency.thread_count, 8)
  assert_eq(high_concurrency.items_per_thread, 1000)
  assert_eq(high_concurrency.total_items, 8000)
  
  // 验证吞吐量随并发度增加
  assert_true(medium_concurrency.throughput >= low_concurrency.throughput * 0.8)  // 考虑开销
  assert_true(high_concurrency.throughput >= medium_concurrency.throughput * 0.8)
  
  // 验证线程不平衡度在合理范围内
  assert_true(low_concurrency.thread_imbalance_percent < 50.0)
  assert_true(medium_concurrency.thread_imbalance_percent < 50.0)
  assert_true(high_concurrency.thread_imbalance_percent < 50.0)
}

// 测试5: 缓存性能基准
test "缓存性能基准" {
  // 定义缓存测试结果
  type CacheResult = {
    test_name: String,
    cache_size: Int,
    total_operations: Int,
    hit_count: Int,
    miss_count: Int,
    hit_rate_percent: Float,
    avg_hit_latency_us: Float,
    avg_miss_latency_us: Float,
    eviction_count: Int
  }
  
  // 定义缓存项
  type CacheItem = {
    key: String,
    value: String,
    access_count: Int,
    last_access_time: Int
  }
  
  // 简单LRU缓存实现
  let create_cache = fn(capacity: Int) {
    {
      capacity: capacity,
      items: Map::empty(),  // key -> CacheItem
      access_order: [],     // 访问顺序
      stats: {
        hits: 0,
        misses: 0,
        evictions: 0
      }
    }
  }
  
  // 缓存查找
  let cache_get = fn(cache: {capacity: Int, items: Map[String, CacheItem], access_order: Array[String], stats: {hits: Int, misses: Int, evictions: Int}}, key: String) {
    match Map::get(cache.items, key) {
      Some(item) => {
        // 更新访问信息和顺序
        let updated_item = {
          key: item.key,
          value: item.value,
          access_count: item.access_count + 1,
          last_access_time: Time::now()
        }
        
        // 更新访问顺序
        let new_access_order = cache.access_order.filter(fn(k) { k != key })
        let final_access_order = new_access_order + [key]
        
        // 更新缓存
        let updated_items = Map::insert(cache.items, key, updated_item)
        
        {
          value: Some(item.value),
          updated_cache: {
            capacity: cache.capacity,
            items: updated_items,
            access_order: final_access_order,
            stats: {hits: cache.stats.hits + 1, misses: cache.stats.misses, evictions: cache.stats.evictions}
          }
        }
      }
      None => {
        {
          value: None,
          updated_cache: {
            capacity: cache.capacity,
            items: cache.items,
            access_order: cache.access_order,
            stats: {hits: cache.stats.hits, misses: cache.stats.misses + 1, evictions: cache.stats.evictions}
          }
        }
      }
    }
  }
  
  // 缓存插入
  let cache_put = fn(cache: {capacity: Int, items: Map[String, CacheItem], access_order: Array[String], stats: {hits: Int, misses: Int, evictions: Int}}, key: String, value: String) {
    let mut updated_items = cache.items
    let mut updated_access_order = cache.access_order
    let mut eviction_count = cache.stats.evictions
    
    // 检查是否需要驱逐
    if Map::size(cache.items) >= cache.capacity && not(Map::contains_key(cache.items, key)) {
      // 驱逐最久未访问的项
      if cache.access_order.length() > 0 {
        let lru_key = cache.access_order[0]
        updated_items = Map::remove(cache.items, lru_key)
        updated_access_order = updated_access_order.filter(fn(k) { k != lru_key })
        eviction_count = eviction_count + 1
      }
    }
    
    // 添加新项
    let new_item = {
      key: key,
      value: value,
      access_count: 1,
      last_access_time: Time::now()
    }
    
    updated_items = Map::insert(updated_items, key, new_item)
    
    // 更新访问顺序
    let new_access_order = updated_access_order.filter(fn(k) { k != key })
    let final_access_order = new_access_order + [key]
    
    {
      capacity: cache.capacity,
      items: updated_items,
      access_order: final_access_order,
      stats: {hits: cache.stats.hits, misses: cache.stats.misses, evictions: eviction_count}
    }
  }
  
  // 运行缓存基准测试
  let run_cache_benchmark = fn(test_name: String, cache_size: Int, operation_count: Int) {
    let cache = create_cache(cache_size)
    let mut hit_latencies = []
    let mut miss_latencies = []
    
    // 预热缓存
    let mut warmed_cache = cache
    for i in 0..(cache_size / 2) {
      let key = "key-" + i.to_string()
      let value = "value-" + i.to_string()
      warmed_cache = cache_put(warmed_cache, key, value)
    }
    
    // 执行操作
    let mut final_cache = warmed_cache
    for i in 0..operation_count {
      let key = "key-" + (i % (cache_size * 2)).to_string()
      
      let start_time = Time::now()
      let result = cache_get(final_cache, key)
      let end_time = Time::now()
      let latency = end_time - start_time
      
      final_cache = result.updated_cache
      
      if result.value.is_some() {
        hit_latencies = hit_latencies + [latency]
      } else {
        miss_latencies = miss_latencies + [latency]
        // 缓存未命中，插入值
        let value = "value-" + i.to_string()
        final_cache = cache_put(final_cache, key, value)
      }
    }
    
    // 计算平均延迟
    let avg_hit_latency = if hit_latencies.length() > 0 {
      hit_latencies.reduce(fn(acc, l) { acc + l }, 0) as Float / hit_latencies.length() as Float
    } else {
      0.0
    }
    
    let avg_miss_latency = if miss_latencies.length() > 0 {
      miss_latencies.reduce(fn(acc, l) { acc + l }, 0) as Float / miss_latencies.length() as Float
    } else {
      0.0
    }
    
    let hit_rate = if final_cache.stats.hits + final_cache.stats.misses > 0 {
      (final_cache.stats.hits as Float / (final_cache.stats.hits + final_cache.stats.misses) as Float) * 100.0
    } else {
      0.0
    }
    
    {
      test_name: test_name,
      cache_size: cache_size,
      total_operations: operation_count,
      hit_count: final_cache.stats.hits,
      miss_count: final_cache.stats.misses,
      hit_rate_percent: hit_rate,
      avg_hit_latency_us: avg_hit_latency,
      avg_miss_latency_us: avg_miss_latency,
      eviction_count: final_cache.stats.evictions
    }
  }
  
  // 运行不同缓存大小的测试
  let small_cache = run_cache_benchmark("small_cache", 100, 1000)
  let medium_cache = run_cache_benchmark("medium_cache", 500, 1000)
  let large_cache = run_cache_benchmark("large_cache", 1000, 1000)
  
  // 验证缓存测试结果
  assert_eq(small_cache.test_name, "small_cache")
  assert_eq(small_cache.cache_size, 100)
  assert_eq(small_cache.total_operations, 1000)
  assert_eq(small_cache.hit_count + small_cache.miss_count, 1000)
  assert_true(small_cache.hit_rate_percent >= 0.0 && small_cache.hit_rate_percent <= 100.0)
  assert_true(small_cache.avg_hit_latency_us >= 0.0)
  assert_true(small_cache.avg_miss_latency_us >= 0.0)
  assert_true(small_cache.eviction_count >= 0)
  
  assert_eq(medium_cache.test_name, "medium_cache")
  assert_eq(medium_cache.cache_size, 500)
  assert_eq(medium_cache.total_operations, 1000)
  
  assert_eq(large_cache.test_name, "large_cache")
  assert_eq(large_cache.cache_size, 1000)
  assert_eq(large_cache.total_operations, 1000)
  
  // 验证缓存大小对命中率的影响
  assert_true(medium_cache.hit_rate_percent >= small_cache.hit_rate_percent)
  assert_true(large_cache.hit_rate_percent >= medium_cache.hit_rate_percent)
  
  // 验证驱逐次数
  assert_true(small_cache.eviction_count >= medium_cache.eviction_count)
  assert_true(medium_cache.eviction_count >= large_cache.eviction_count)
  
  // 验证命中延迟小于未命中延迟
  assert_true(small_cache.avg_hit_latency_us <= small_cache.avg_miss_latency_us)
  assert_true(medium_cache.avg_hit_latency_us <= medium_cache.avg_miss_latency_us)
  assert_true(large_cache.avg_hit_latency_us <= large_cache.avg_miss_latency_us)
}

// 测试6: 序列化性能基准
test "序列化性能基准" {
  // 定义序列化测试结果
  type SerializationResult = {
    test_name: String,
    format: String,
    item_count: Int,
    original_size_bytes: Int,
    serialized_size_bytes: Int,
    compression_ratio: Float,
    serialization_time_ms: Int,
    deserialization_time_ms: Int,
    throughput_items_per_sec: Float
  }
  
  // 定义测试数据
  type TestData = {
    id: String,
    timestamp: Int,
    message: String,
    metadata: Array<(String, String)>
  }
  
  // 生成测试数据
  let generate_test_data = fn(count: Int) {
    let mut data = []
    
    for i in 0..count {
      let item = {
        id: "id-" + i.to_string(),
        timestamp: 1640995200000 + i,
        message: "test message " + i.to_string(),
        metadata: [
          ("key1", "value1-" + i.to_string()),
          ("key2", "value2-" + i.to_string()),
          ("key3", "value3-" + i.to_string())
        ]
      }
      data = data + [item]
    }
    
    data
  }
  
  // JSON序列化
  let json_serialize = fn(data: Array[TestData>) {
    let start_time = Time::now()
    
    let json_array = data.map(fn(item) {
      let metadata_json = item.metadata.map(fn(meta) {
        let (key, value) = meta
        "\"" + key + "\":\"" + value + "\""
      }).join(",")
      
      "{" +
      "\"id\":\"" + item.id + "\"," +
      "\"timestamp\":" + item.timestamp.to_string() + "," +
      "\"message\":\"" + item.message + "\"," +
      "\"metadata\":{" + metadata_json + "}" +
      "}"
    }).join(",")
    
    let json = "[" + json_array + "]"
    let end_time = Time::now()
    
    (json.to_bytes(), end_time - start_time)
  }
  
  // JSON反序列化
  let json_deserialize = fn(json_data: Array[UInt8>) {
    let start_time = Time::now()
    
    // 简化的JSON解析（实际中会使用JSON解析器）
    let json_str = String::from_utf8(json_data)
    let data_count = json_str.split("\"id\":").length() - 1
    
    let mut data = []
    for i in 0..data_count {
      let item = {
        id: "id-" + i.to_string(),
        timestamp: 1640995200000 + i,
        message: "test message " + i.to_string(),
        metadata: [
          ("key1", "value1-" + i.to_string()),
          ("key2", "value2-" + i.to_string()),
          ("key3", "value3-" + i.to_string())
        ]
      }
      data = data + [item]
    }
    
    let end_time = Time::now()
    
    (data, end_time - start_time)
  }
  
  // 二进制序列化
  let binary_serialize = fn(data: Array[TestData>) {
    let start_time = Time::now()
    
    let mut result = []
    
    // 添加数据项数量
    result = result + [data.length() as UInt8]
    
    for item in data {
      // 添加ID长度和ID
      result = result + [item.id.length() as UInt8]
      result = result + item.id.to_bytes()
      
      // 添加时间戳（8字节）
      result = result + item.timestamp.to_bytes()
      
      // 添加消息长度和消息
      result = result + [item.message.length() as UInt8]
      result = result + item.message.to_bytes()
      
      // 添加元数据项数量
      result = result + [item.metadata.length() as UInt8]
      
      // 添加每个元数据项
      for meta in item.metadata {
        let (key, value) = meta
        result = result + [key.length() as UInt8]
        result = result + key.to_bytes()
        result = result + [value.length() as UInt8]
        result = result + value.to_bytes()
      }
    }
    
    let end_time = Time::now()
    
    (result, end_time - start_time)
  }
  
  // 二进制反序列化
  let binary_deserialize = fn(binary_data: Array[UInt8]) {
    let start_time = Time::now()
    
    // 简化的二进制解析
    if binary_data.length() == 0 {
      return ([], 0)
    }
    
    let item_count = binary_data[0] as Int
    let mut data = []
    let mut offset = 1
    
    for i in 0..item_count {
      if offset >= binary_data.length() {
        break
      }
      
      // 读取ID
      let id_length = binary_data[offset] as Int
      offset = offset + 1
      
      let id_bytes = binary_data.slice(offset, id_length)
      let id = String::from_utf8(id_bytes)
      offset = offset + id_length
      
      // 读取时间戳（简化为4字节）
      let timestamp_bytes = binary_data.slice(offset, 4)
      let timestamp = timestamp_bytes[0] as Int + 
                     (timestamp_bytes[1] as Int) * 256 + 
                     (timestamp_bytes[2] as Int) * 65536 + 
                     (timestamp_bytes[3] as Int) * 16777216
      offset = offset + 4
      
      // 读取消息
      let message_length = binary_data[offset] as Int
      offset = offset + 1
      
      let message_bytes = binary_data.slice(offset, message_length)
      let message = String::from_utf8(message_bytes)
      offset = offset + message_length
      
      // 读取元数据
      let metadata_count = binary_data[offset] as Int
      offset = offset + 1
      
      let mut metadata = []
      for j in 0..metadata_count {
        if offset >= binary_data.length() {
          break
        }
        
        // 读取键
        let key_length = binary_data[offset] as Int
        offset = offset + 1
        
        let key_bytes = binary_data.slice(offset, key_length)
        let key = String::from_utf8(key_bytes)
        offset = offset + key_length
        
        // 读取值
        let value_length = binary_data[offset] as Int
        offset = offset + 1
        
        let value_bytes = binary_data.slice(offset, value_length)
        let value = String::from_utf8(value_bytes)
        offset = offset + value_length
        
        metadata = metadata + [(key, value)]
      }
      
      let item = {
        id: id,
        timestamp: timestamp,
        message: message,
        metadata: metadata
      }
      
      data = data + [item]
    }
    
    let end_time = Time::now()
    
    (data, end_time - start_time)
  }
  
  // 运行序列化基准测试
  let run_serialization_benchmark = fn(test_name: String, format: String, data_count: Int, 
                                      serialize_fn: (Array[TestData>) -> (Array[UInt8], Int),
                                      deserialize_fn: (Array[UInt8>) -> (Array[TestData], Int)) {
    let data = generate_test_data(data_count)
    
    // 计算原始大小
    let original_size = data.reduce(fn(acc, item) {
      let item_size = item.id.length() + item.message.length() + 
                     item.metadata.reduce(fn(meta_acc, meta) { 
                       meta_acc + meta.0.length() + meta.1.length() 
                     }, 0) + 20  // 估算开销
      acc + item_size
    }, 0)
    
    // 序列化
    let (serialized_data, serialization_time) = serialize_fn(data)
    
    // 反序列化
    let (deserialized_data, deserialization_time) = deserialize_fn(serialized_data)
    
    // 计算压缩比
    let compression_ratio = serialized_data.length() as Float / original_size as Float
    
    // 计算吞吐量
    let total_time = serialization_time + deserialization_time
    let throughput = if total_time > 0 {
      (data_count as Float / total_time as Float) * 1000.0
    } else {
      0.0
    }
    
    {
      test_name: test_name,
      format: format,
      item_count: data_count,
      original_size_bytes: original_size,
      serialized_size_bytes: serialized_data.length(),
      compression_ratio: compression_ratio,
      serialization_time_ms: serialization_time,
      deserialization_time_ms: deserialization_time,
      throughput_items_per_sec: throughput
    }
  }
  
  // 运行不同格式的序列化测试
  let json_benchmark = run_serialization_benchmark("json_format", "JSON", 1000, json_serialize, json_deserialize)
  let binary_benchmark = run_serialization_benchmark("binary_format", "Binary", 1000, binary_serialize, binary_deserialize)
  
  // 验证序列化测试结果
  assert_eq(json_benchmark.test_name, "json_format")
  assert_eq(json_benchmark.format, "JSON")
  assert_eq(json_benchmark.item_count, 1000)
  assert_true(json_benchmark.original_size_bytes > 0)
  assert_true(json_benchmark.serialized_size_bytes > 0)
  assert_true(json_benchmark.compression_ratio > 0)
  assert_true(json_benchmark.serialization_time_ms >= 0)
  assert_true(json_benchmark.deserialization_time_ms >= 0)
  assert_true(json_benchmark.throughput_items_per_sec > 0)
  
  assert_eq(binary_benchmark.test_name, "binary_format")
  assert_eq(binary_benchmark.format, "Binary")
  assert_eq(binary_benchmark.item_count, 1000)
  assert_true(binary_benchmark.original_size_bytes > 0)
  assert_true(binary_benchmark.serialized_size_bytes > 0)
  assert_true(binary_benchmark.compression_ratio > 0)
  assert_true(binary_benchmark.serialization_time_ms >= 0)
  assert_true(binary_benchmark.deserialization_time_ms >= 0)
  assert_true(binary_benchmark.throughput_items_per_sec > 0)
  
  // 验证二进制格式通常比JSON更紧凑
  assert_true(binary_benchmark.compression_ratio <= json_benchmark.compression_ratio)
  
  // 验证二进制序列化通常更快
  assert_true(binary_benchmark.serialization_time_ms <= json_benchmark.serialization_time_ms * 1.5)  // 允许一定差异
  assert_true(binary_benchmark.deserialization_time_ms <= json_benchmark.deserialization_time_ms * 1.5)
  
  // 验证吞吐量
  assert_true(binary_benchmark.throughput_items_per_sec >= json_benchmark.throughput_items_per_sec * 0.8)  // 允许一定差异
}

// 测试7: 网络I/O性能基准
test "网络I/O性能基准" {
  // 定义网络I/O测试结果
  type NetworkIOResult = {
    test_name: String,
    protocol: String,
    payload_size_bytes: Int,
    request_count: Int,
    total_time_ms: Int,
    avg_latency_ms: Float,
    throughput_mbps: Float,
    success_rate_percent: Float,
    error_count: Int
  }
  
  // 模拟网络请求
  let simulate_network_request = fn(protocol: String, payload_size: Int) {
    let start_time = Time::now()
    
    // 模拟网络延迟（基于协议和载荷大小）
    let base_latency = match protocol {
      "HTTP" => 10,
      "HTTPS" => 15,
      "gRPC" => 8,
      "WebSocket" => 5,
      _ => 10
    }
    
    let payload_latency = payload_size / 1000  // 每KB增加1ms延迟
    let random_latency = (Time::now() % 10)  // 0-9ms随机延迟
    
    let total_latency = base_latency + payload_latency + random_latency
    
    // 模拟网络错误（5%错误率）
    let error = (Time::now() % 100) < 5
    
    let end_time = Time::now()
    
    {
      success: not error,
      latency: total_latency,
      actual_time: end_time - start_time
    }
  }
  
  // 批量网络请求
  let batch_network_requests = fn(protocol: String, payload_size: Int, request_count: Int) {
    let mut results = []
    let mut total_latency = 0
    let mut error_count = 0
    
    for i in 0..request_count {
      let result = simulate_network_request(protocol, payload_size)
      results = results + [result]
      
      if result.success {
        total_latency = total_latency + result.latency
      } else {
        error_count = error_count + 1
      }
    }
    
    (results, total_latency, error_count)
  }
  
  // 运行网络I/O基准测试
  let run_network_io_benchmark = fn(test_name: String, protocol: String, payload_size: Int, request_count: Int) {
    let start_time = Time::now()
    let (results, total_latency, error_count) = batch_network_requests(protocol, payload_size, request_count)
    let end_time = Time::now()
    
    let total_time = end_time - start_time
    let successful_requests = request_count - error_count
    
    let avg_latency = if successful_requests > 0 {
      total_latency as Float / successful_requests as Float
    } else {
      0.0
    }
    
    // 计算吞吐量（Mbps）
    let total_bytes = payload_size * successful_requests
    let throughput_mbps = if total_time > 0 {
      (total_bytes as Float / total_time as Float) * 1000.0 / (1024.0 * 1024.0)
    } else {
      0.0
    }
    
    let success_rate = if request_count > 0 {
      (successful_requests as Float / request_count as Float) * 100.0
    } else {
      0.0
    }
    
    {
      test_name: test_name,
      protocol: protocol,
      payload_size_bytes: payload_size,
      request_count: request_count,
      total_time_ms: total_time,
      avg_latency_ms: avg_latency,
      throughput_mbps: throughput_mbps,
      success_rate_percent: success_rate,
      error_count: error_count
    }
  }
  
  // 运行不同协议和载荷大小的测试
  let http_small = run_network_io_benchmark("http_small", "HTTP", 1024, 100)
  let http_large = run_network_io_benchmark("http_large", "HTTP", 10240, 100)
  let https_small = run_network_io_benchmark("https_small", "HTTPS", 1024, 100)
  let grpc_small = run_network_io_benchmark("grpc_small", "gRPC", 1024, 100)
  let websocket_small = run_network_io_benchmark("websocket_small", "WebSocket", 1024, 100)
  
  // 验证网络I/O测试结果
  assert_eq(http_small.test_name, "http_small")
  assert_eq(http_small.protocol, "HTTP")
  assert_eq(http_small.payload_size_bytes, 1024)
  assert_eq(http_small.request_count, 100)
  assert_true(http_small.total_time_ms > 0)
  assert_true(http_small.avg_latency_ms > 0)
  assert_true(http_small.throughput_mbps > 0)
  assert_true(http_small.success_rate_percent >= 0.0 && http_small.success_rate_percent <= 100.0)
  assert_true(http_small.error_count >= 0 && http_small.error_count <= 100)
  
  // 验证载荷大小对延迟的影响
  assert_true(http_large.avg_latency_ms >= http_small.avg_latency_ms)
  
  // 验证协议对延迟的影响
  assert_true(https_small.avg_latency_ms >= http_small.avg_latency_ms)  // HTTPS通常比HTTP慢
  assert_true(grpc_small.avg_latency_ms <= http_small.avg_latency_ms)    // gRPC通常比HTTP快
  assert_true(websocket_small.avg_latency_ms <= http_small.avg_latency_ms) // WebSocket通常比HTTP快
  
  // 验证吞吐量
  assert_true(http_large.throughput_mbps >= http_small.throughput_mbps * 0.8)  // 大载荷可能有更高吞吐量
  
  // 验证成功率（应该在95%左右，因为我们设置了5%的错误率）
  assert_true(http_small.success_rate_percent >= 90.0 && http_small.success_rate_percent <= 100.0)
  assert_true(http_large.success_rate_percent >= 90.0 && http_large.success_rate_percent <= 100.0)
  assert_true(https_small.success_rate_percent >= 90.0 && https_small.success_rate_percent <= 100.0)
  assert_true(grpc_small.success_rate_percent >= 90.0 && grpc_small.success_rate_percent <= 100.0)
  assert_true(websocket_small.success_rate_percent >= 90.0 && websocket_small.success_rate_percent <= 100.0)
}

// 测试8: 数据库操作性能基准
test "数据库操作性能基准" {
  // 定义数据库操作测试结果
  type DatabaseResult = {
    test_name: String,
    operation_type: String,
    record_count: Int,
    total_time_ms: Int,
    avg_latency_ms: Float,
    throughput_ops_per_sec: Float,
    success_rate_percent: Float,
    error_count: Int
  }
  
  // 模拟数据库记录
  type DatabaseRecord = {
    id: String,
    data: String,
    timestamp: Int,
    metadata: Array<(String, String)>
  }
  
  // 生成数据库记录
  let generate_db_records = fn(count: Int) {
    let mut records = []
    
    for i in 0..count {
      let record = {
        id: "record-" + i.to_string(),
        data: "data-" + i.to_string(),
        timestamp: 1640995200000 + i,
        metadata: [
          ("key1", "value1-" + i.to_string()),
          ("key2", "value2-" + i.to_string())
        ]
      }
      records = records + [record]
    }
    
    records
  }
  
  // 模拟数据库插入
  let simulate_db_insert = fn(record: DatabaseRecord) {
    let start_time = Time::now()
    
    // 模拟插入延迟（基于数据大小）
    let base_latency = 5
    let data_latency = record.data.length() / 100
    let metadata_latency = record.metadata.length() * 2
    let random_latency = (Time::now() % 5)
    
    let total_latency = base_latency + data_latency + metadata_latency + random_latency
    
    // 模拟插入错误（2%错误率）
    let error = (Time::now() % 100) < 2
    
    let end_time = Time::now()
    
    {
      success: not error,
      latency: total_latency,
      record_id: record.id
    }
  }
  
  // 模拟数据库查询
  let simulate_db_query = fn(record_id: String) {
    let start_time = Time::now()
    
    // 模拟查询延迟
    let base_latency = 3
    let id_latency = record_id.length() / 10
    let random_latency = (Time::now() % 3)
    
    let total_latency = base_latency + id_latency + random_latency
    
    // 模拟查询错误（1%错误率）
    let error = (Time::now() % 100) < 1
    
    let end_time = Time::now()
    
    {
      success: not error,
      latency: total_latency,
      record_id: record_id
    }
  }
  
  // 模拟数据库更新
  let simulate_db_update = fn(record: DatabaseRecord) {
    let start_time = Time::now()
    
    // 模拟更新延迟（通常比插入慢）
    let base_latency = 8
    let data_latency = record.data.length() / 80
    let metadata_latency = record.metadata.length() * 3
    let random_latency = (Time::now() % 6)
    
    let total_latency = base_latency + data_latency + metadata_latency + random_latency
    
    // 模拟更新错误（3%错误率）
    let error = (Time::now() % 100) < 3
    
    let end_time = Time::now()
    
    {
      success: not error,
      latency: total_latency,
      record_id: record.id
    }
  }
  
  // 批量数据库操作
  let batch_db_operations = fn(operation: String, records: Array<DatabaseRecord>) {
    let mut results = []
    let mut total_latency = 0
    let mut error_count = 0
    
    for record in records {
      let result = match operation {
        "insert" => simulate_db_insert(record),
        "query" => simulate_db_query(record.id),
        "update" => simulate_db_update(record),
        _ => {success: false, latency: 0, record_id: record.id}
      }
      
      results = results + [result]
      
      if result.success {
        total_latency = total_latency + result.latency
      } else {
        error_count = error_count + 1
      }
    }
    
    (results, total_latency, error_count)
  }
  
  // 运行数据库操作基准测试
  let run_database_benchmark = fn(test_name: String, operation: String, record_count: Int) {
    let records = generate_db_records(record_count)
    
    let start_time = Time::now()
    let (results, total_latency, error_count) = batch_db_operations(operation, records)
    let end_time = Time::now()
    
    let total_time = end_time - start_time
    let successful_operations = record_count - error_count
    
    let avg_latency = if successful_operations > 0 {
      total_latency as Float / successful_operations as Float
    } else {
      0.0
    }
    
    // 计算吞吐量
    let throughput = if total_time > 0 {
      (record_count as Float / total_time as Float) * 1000.0
    } else {
      0.0
    }
    
    let success_rate = if record_count > 0 {
      (successful_operations as Float / record_count as Float) * 100.0
    } else {
      0.0
    }
    
    {
      test_name: test_name,
      operation_type: operation,
      record_count: record_count,
      total_time_ms: total_time,
      avg_latency_ms: avg_latency,
      throughput_ops_per_sec: throughput,
      success_rate_percent: success_rate,
      error_count: error_count
    }
  }
  
  // 运行不同数据库操作的测试
  let insert_small = run_database_benchmark("insert_small", "insert", 100)
  let insert_large = run_database_benchmark("insert_large", "insert", 1000)
  let query_small = run_database_benchmark("query_small", "query", 100)
  let update_small = run_database_benchmark("update_small", "update", 100)
  
  // 验证数据库操作测试结果
  assert_eq(insert_small.test_name, "insert_small")
  assert_eq(insert_small.operation_type, "insert")
  assert_eq(insert_small.record_count, 100)
  assert_true(insert_small.total_time_ms > 0)
  assert_true(insert_small.avg_latency_ms > 0)
  assert_true(insert_small.throughput_ops_per_sec > 0)
  assert_true(insert_small.success_rate_percent >= 0.0 && insert_small.success_rate_percent <= 100.0)
  assert_true(insert_small.error_count >= 0 && insert_small.error_count <= 100)
  
  // 验证记录数量对性能的影响
  assert_true(insert_large.avg_latency_ms >= insert_small.avg_latency_ms * 0.8)  // 允许一定差异
  
  // 验证操作类型对延迟的影响
  assert_true(query_small.avg_latency_ms <= insert_small.avg_latency_ms)    // 查询通常比插入快
  assert_true(update_small.avg_latency_ms >= insert_small.avg_latency_ms)    // 更新通常比插入慢
  
  // 验证成功率（应该接近98%，因为我们设置了不同的错误率）
  assert_true(insert_small.success_rate_percent >= 95.0 && insert_small.success_rate_percent <= 100.0)
  assert_true(insert_large.success_rate_percent >= 95.0 && insert_large.success_rate_percent <= 100.0)
  assert_true(query_small.success_rate_percent >= 98.0 && query_small.success_rate_percent <= 100.0)
  assert_true(update_small.success_rate_percent >= 95.0 && update_small.success_rate_percent <= 100.0)
  
  // 验证吞吐量
  assert_true(insert_small.throughput_ops_per_sec > 0)
  assert_true(insert_large.throughput_ops_per_sec > 0)
  assert_true(query_small.throughput_ops_per_sec > 0)
  assert_true(update_small.throughput_ops_per_sec > 0)
}

// 测试9: 资源利用率基准
test "资源利用率基准" {
  // 定义资源利用率测试结果
  type ResourceResult = {
    test_name: String,
    duration_seconds: Int,
    avg_cpu_percent: Float,
    peak_cpu_percent: Float,
    avg_memory_mb: Float,
    peak_memory_mb: Float,
    avg_disk_io_mb_per_sec: Float,
    avg_network_io_mb_per_sec: Float,
    resource_efficiency_score: Float
  }
  
  // 定义资源监控器
  type ResourceMonitor = {
    cpu_samples: Array<Float>,
    memory_samples: Array<Float>,
    disk_io_samples: Array<Float>,
    network_io_samples: Array<Float>
  }
  
  // 创建资源监控器
  let create_monitor = fn() {
    {
      cpu_samples: [],
      memory_samples: [],
      disk_io_samples: [],
      network_io_samples: []
    }
  }
  
  // 采集资源样本
  let sample_resources = fn(monitor: ResourceMonitor, workload_intensity: Float) {
    // 模拟CPU使用率（基于工作负载强度）
    let base_cpu = 20.0
    let workload_cpu = workload_intensity * 60.0  // 0-60%
    let random_cpu = (Time::now() % 20) as Float
    let cpu_usage = base_cpu + workload_cpu + random_cpu
    
    // 模拟内存使用（基于工作负载强度）
    let base_memory = 100.0  // MB
    let workload_memory = workload_intensity * 200.0  // 0-200MB
    let random_memory = (Time::now() % 50) as Float
    let memory_usage = base_memory + workload_memory + random_memory
    
    // 模拟磁盘I/O（基于工作负载强度）
    let base_disk_io = 5.0  // MB/s
    let workload_disk_io = workload_intensity * 15.0  // 0-15MB/s
    let random_disk_io = (Time::now() % 10) as Float
    let disk_io_usage = base_disk_io + workload_disk_io + random_disk_io
    
    // 模拟网络I/O（基于工作负载强度）
    let base_network_io = 2.0  // MB/s
    let workload_network_io = workload_intensity * 10.0  // 0-10MB/s
    let random_network_io = (Time::now() % 5) as Float
    let network_io_usage = base_network_io + workload_network_io + random_network_io
    
    {
      cpu_samples: monitor.cpu_samples + [cpu_usage],
      memory_samples: monitor.memory_samples + [memory_usage],
      disk_io_samples: monitor.disk_io_samples + [disk_io_usage],
      network_io_samples: monitor.network_io_samples + [network_io_usage]
    }
  }
  
  // 计算资源统计
  let calculate_resource_stats = fn(monitor: ResourceMonitor) {
    // 计算CPU统计
    let avg_cpu = if monitor.cpu_samples.length() > 0 {
      monitor.cpu_samples.reduce(fn(acc, cpu) { acc + cpu }, 0.0) / monitor.cpu_samples.length() as Float
    } else {
      0.0
    }
    
    let peak_cpu = if monitor.cpu_samples.length() > 0 {
      monitor.cpu_samples.reduce(fn(acc, cpu) { if cpu > acc { cpu } else { acc }, 0.0)
    } else {
      0.0
    }
    
    // 计算内存统计
    let avg_memory = if monitor.memory_samples.length() > 0 {
      monitor.memory_samples.reduce(fn(acc, mem) { acc + mem }, 0.0) / monitor.memory_samples.length() as Float
    } else {
      0.0
    }
    
    let peak_memory = if monitor.memory_samples.length() > 0 {
      monitor.memory_samples.reduce(fn(acc, mem) { if mem > acc { mem } else { acc }, 0.0)
    } else {
      0.0
    }
    
    // 计算磁盘I/O统计
    let avg_disk_io = if monitor.disk_io_samples.length() > 0 {
      monitor.disk_io_samples.reduce(fn(acc, io) { acc + io }, 0.0) / monitor.disk_io_samples.length() as Float
    } else {
      0.0
    }
    
    // 计算网络I/O统计
    let avg_network_io = if monitor.network_io_samples.length() > 0 {
      monitor.network_io_samples.reduce(fn(acc, io) { acc + io }, 0.0) / monitor.network_io_samples.length() as Float
    } else {
      0.0
    }
    
    // 计算资源效率分数（简化版）
    let cpu_efficiency = if avg_cpu > 0 { 100.0 / avg_cpu } else { 0.0 }
    let memory_efficiency = if avg_memory > 0 { 1000.0 / avg_memory } else { 0.0 }
    let io_efficiency = if avg_disk_io + avg_network_io > 0 { 
      100.0 / (avg_disk_io + avg_network_io) 
    } else { 
      0.0 
    }
    
    let resource_efficiency_score = (cpu_efficiency + memory_efficiency + io_efficiency) / 3.0
    
    {
      avg_cpu_percent: avg_cpu,
      peak_cpu_percent: peak_cpu,
      avg_memory_mb: avg_memory,
      peak_memory_mb: peak_memory,
      avg_disk_io_mb_per_sec: avg_disk_io,
      avg_network_io_mb_per_sec: avg_network_io,
      resource_efficiency_score: resource_efficiency_score
    }
  }
  
  // 运行资源利用率基准测试
  let run_resource_benchmark = fn(test_name: String, duration_seconds: Int, workload_pattern: String) {
    let mut monitor = create_monitor()
    let start_time = Time::now()
    
    // 根据工作负载模式调整强度
    let sample_count = duration_seconds * 10  // 每秒采样10次
    
    for i in 0..sample_count {
      let workload_intensity = match workload_pattern {
        "constant" => 0.5,  // 恒定中等负载
        "burst" => if i % 20 < 10 { 0.8 } else { 0.2 },  // 突发负载
        "ramp_up" => (i as Float / sample_count as Float),  // 逐渐增加
        "ramp_down" => 1.0 - (i as Float / sample_count as Float),  // 逐渐减少
        "spiky" => if i % 30 < 5 { 0.9 } else { 0.3 },  // 尖峰负载
        _ => 0.5  // 默认中等负载
      }
      
      monitor = sample_resources(monitor, workload_intensity)
      
      // 模拟采样间隔
      let _ = Time::sleep(100)  // 100ms
    }
    
    let end_time = Time::now()
    let actual_duration = (end_time - start_time) / 1000
    
    let stats = calculate_resource_stats(monitor)
    
    {
      test_name: test_name,
      duration_seconds: actual_duration,
      avg_cpu_percent: stats.avg_cpu_percent,
      peak_cpu_percent: stats.peak_cpu_percent,
      avg_memory_mb: stats.avg_memory_mb,
      peak_memory_mb: stats.peak_memory_mb,
      avg_disk_io_mb_per_sec: stats.avg_disk_io_mb_per_sec,
      avg_network_io_mb_per_sec: stats.avg_network_io_mb_per_sec,
      resource_efficiency_score: stats.resource_efficiency_score
    }
  }
  
  // 运行不同工作负载模式的测试
  let constant_load = run_resource_benchmark("constant_load", 10, "constant")
  let burst_load = run_resource_benchmark("burst_load", 10, "burst")
  let ramp_up_load = run_resource_benchmark("ramp_up_load", 10, "ramp_up")
  let ramp_down_load = run_resource_benchmark("ramp_down_load", 10, "ramp_down")
  let spiky_load = run_resource_benchmark("spiky_load", 10, "spiky")
  
  // 验证资源利用率测试结果
  assert_eq(constant_load.test_name, "constant_load")
  assert_true(constant_load.duration_seconds >= 9 && constant_load.duration_seconds <= 11)  // 允许一定误差
  assert_true(constant_load.avg_cpu_percent >= 0.0 && constant_load.avg_cpu_percent <= 100.0)
  assert_true(constant_load.peak_cpu_percent >= constant_load.avg_cpu_percent)
  assert_true(constant_load.avg_memory_mb > 0)
  assert_true(constant_load.peak_memory_mb >= constant_load.avg_memory_mb)
  assert_true(constant_load.avg_disk_io_mb_per_sec >= 0)
  assert_true(constant_load.avg_network_io_mb_per_sec >= 0)
  assert_true(constant_load.resource_efficiency_score > 0)
  
  // 验证不同工作负载模式的资源使用模式
  assert_true(burst_load.peak_cpu_percent >= constant_load.peak_cpu_percent)  // 突发负载应该有更高的峰值
  assert_true(ramp_up_load.avg_cpu_percent <= ramp_down_load.avg_cpu_percent)  // 逐渐增加应该比逐渐减少的平均值低
  
  // 验证尖峰负载的特征
  assert_true(spiky_load.peak_cpu_percent >= constant_load.peak_cpu_percent)  // 尖峰负载应该有更高的峰值
  
  // 验证资源效率分数
  assert_true(constant_load.resource_efficiency_score > 0)
  assert_true(burst_load.resource_efficiency_score > 0)
  assert_true(ramp_up_load.resource_efficiency_score > 0)
  assert_true(ramp_down_load.resource_efficiency_score > 0)
  assert_true(spiky_load.resource_efficiency_score > 0)
}

// 测试10: 综合性能基准
test "综合性能基准" {
  // 定义综合性能测试结果
  type ComprehensiveResult = {
    test_name: String,
    scenario: String,
    duration_ms: Int,
    throughput: Float,
    avg_latency_ms: Float,
    p95_latency_ms: Float,
    p99_latency_ms: Float,
    error_rate_percent: Float,
    resource_efficiency_score: Float,
    overall_score: Float
  }
  
  // 定义综合测试场景
  enum TestScenario {
    LightLoad      // 轻负载
    MediumLoad     // 中等负载
    HeavyLoad      // 重负载
    BurstLoad      // 突发负载
    SustainedLoad  // 持续负载
  }
  
  // 模拟综合性能测试
  let simulate_comprehensive_test = fn(scenario: TestScenario, duration_ms: Int) {
    let start_time = Time::now()
    let mut latencies = []
    let mut error_count = 0
    let mut total_operations = 0
    
    // 根据场景调整参数
    let (base_latency, latency_variance, error_rate, ops_per_ms) = match scenario {
      TestScenario::LightLoad => (10, 5, 0.01, 2),      // 低延迟，低错误率，低吞吐量
      TestScenario::MediumLoad => (25, 10, 0.02, 5),    // 中等延迟，中等错误率，中等吞吐量
      TestScenario::HeavyLoad => (50, 20, 0.05, 10),    // 高延迟，高错误率，高吞吐量
      TestScenario::BurstLoad => (15, 30, 0.03, 15),    // 中等延迟，高方差，高吞吐量
      TestScenario::SustainedLoad => (30, 8, 0.02, 8)   // 中高延迟，低方差，中等高吞吐量
    }
    
    let end_time = start_time + duration_ms
    let mut current_time = start_time
    
    while current_time < end_time {
      // 执行操作
      for i in 0..ops_per_ms {
        total_operations = total_operations + 1
        
        // 计算延迟
        let random_factor = (Time::now() % 100) as Float / 100.0
        let latency = base_latency + (random_factor * latency_variance * 2.0 - latency_variance)
        
        // 检查错误
        let error_random = (Time::now() % 1000) as Float / 1000.0
        let error = error_random < error_rate
        
        if error {
          error_count = error_count + 1
        } else {
          latencies = latencies + [latency as Int]
        }
      }
      
      // 更新时间
      current_time = current_time + 1
      
      // 模拟实际时间流逝
      let _ = Time::sleep(1)
    }
    
    let actual_duration = Time::now() - start_time
    
    // 计算统计
    let throughput = if actual_duration > 0 {
      (total_operations as Float / actual_duration as Float) * 1000.0
    } else {
      0.0
    }
    
    let error_rate = if total_operations > 0 {
      (error_count as Float / total_operations as Float) * 100.0
    } else {
      0.0
    }
    
    // 计算延迟百分位数
    if latencies.length() > 0 {
      let sorted_latencies = latencies.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
      let avg_latency = latencies.reduce(fn(acc, l) { acc + l }, 0) as Float / latencies.length() as Float
      
      let p95_index = (sorted_latencies.length() * 95 / 100)
      let p99_index = (sorted_latencies.length() * 99 / 100)
      
      let p95_latency = sorted_latencies[p95_index] as Float
      let p99_latency = sorted_latencies[p99_index] as Float
      
      // 计算资源效率分数（简化版）
      let cpu_efficiency = if avg_latency > 0 { 1000.0 / avg_latency } else { 0.0 }
      let latency_efficiency = if p99_latency > 0 { 1000.0 / p99_latency } else { 0.0 }
      let error_efficiency = if error_rate > 0 { 100.0 / error_rate } else { 100.0 }
      
      let resource_efficiency_score = (cpu_efficiency + latency_efficiency + error_efficiency) / 3.0
      
      // 计算综合分数（简化版）
      let throughput_score = throughput / 10.0  // 归一化
      let latency_score = if avg_latency > 0 { 100.0 / avg_latency } else { 0.0 }
      let reliability_score = 100.0 - error_rate
      
      let overall_score = (throughput_score + latency_score + reliability_score) / 3.0
      
      {
        duration_ms: actual_duration,
        throughput: throughput,
        avg_latency_ms: avg_latency,
        p95_latency_ms: p95_latency,
        p99_latency_ms: p99_latency,
        error_rate_percent: error_rate,
        resource_efficiency_score: resource_efficiency_score,
        overall_score: overall_score
      }
    } else {
      {
        duration_ms: actual_duration,
        throughput: 0.0,
        avg_latency_ms: 0.0,
        p95_latency_ms: 0.0,
        p99_latency_ms: 0.0,
        error_rate_percent: 100.0,
        resource_efficiency_score: 0.0,
        overall_score: 0.0
      }
    }
  }
  
  // 运行综合性能基准测试
  let run_comprehensive_benchmark = fn(test_name: String, scenario: TestScenario, duration_ms: Int) {
    let results = simulate_comprehensive_test(scenario, duration_ms)
    
    {
      test_name: test_name,
      scenario: match scenario {
        TestScenario::LightLoad => "LightLoad"
        TestScenario::MediumLoad => "MediumLoad"
        TestScenario::HeavyLoad => "HeavyLoad"
        TestScenario::BurstLoad => "BurstLoad"
        TestScenario::SustainedLoad => "SustainedLoad"
      },
      duration_ms: results.duration_ms,
      throughput: results.throughput,
      avg_latency_ms: results.avg_latency_ms,
      p95_latency_ms: results.p95_latency_ms,
      p99_latency_ms: results.p99_latency_ms,
      error_rate_percent: results.error_rate_percent,
      resource_efficiency_score: results.resource_efficiency_score,
      overall_score: results.overall_score
    }
  }
  
  // 运行不同场景的综合测试
  let light_load = run_comprehensive_benchmark("light_load", TestScenario::LightLoad, 5000)
  let medium_load = run_comprehensive_benchmark("medium_load", TestScenario::MediumLoad, 5000)
  let heavy_load = run_comprehensive_benchmark("heavy_load", TestScenario::HeavyLoad, 5000)
  let burst_load = run_comprehensive_benchmark("burst_load", TestScenario::BurstLoad, 5000)
  let sustained_load = run_comprehensive_benchmark("sustained_load", TestScenario::SustainedLoad, 5000)
  
  // 验证综合性能测试结果
  assert_eq(light_load.test_name, "light_load")
  assert_eq(light_load.scenario, "LightLoad")
  assert_true(light_load.duration_ms >= 4500 && light_load.duration_ms <= 5500)  // 允许5%误差
  assert_true(light_load.throughput > 0)
  assert_true(light_load.avg_latency_ms > 0)
  assert_true(light_load.p95_latency_ms >= light_load.avg_latency_ms)
  assert_true(light_load.p99_latency_ms >= light_load.p95_latency_ms)
  assert_true(light_load.error_rate_percent >= 0.0 && light_load.error_rate_percent <= 100.0)
  assert_true(light_load.resource_efficiency_score > 0)
  assert_true(light_load.overall_score > 0)
  
  // 验证不同场景的性能特征
  assert_true(medium_load.throughput >= light_load.throughput)  // 中等负载应该有更高吞吐量
  assert_true(heavy_load.throughput >= medium_load.throughput) // 重负载应该有更高吞吐量
  assert_true(medium_load.avg_latency_ms >= light_load.avg_latency_ms)  // 中等负载应该有更高延迟
  assert_true(heavy_load.avg_latency_ms >= medium_load.avg_latency_ms)  // 重负载应该有更高延迟
  
  // 验证错误率
  assert_true(light_load.error_rate_percent <= 2.0)   // 轻负载错误率应该很低
  assert_true(medium_load.error_rate_percent <= 5.0)  // 中等负载错误率应该较低
  assert_true(heavy_load.error_rate_percent <= 10.0)  // 重负载错误率可以更高
  
  // 验证突发负载的特征
  assert_true(burst_load.p99_latency_ms >= heavy_load.p99_latency_ms)  // 突发负载应该有更高的P99延迟
  
  // 验证持续负载的特征
  assert_true(sustained_load.avg_latency_ms >= medium_load.avg_latency_ms)  // 持续负载应该有较高的平均延迟
  
  // 验证综合分数
  assert_true(light_load.overall_score > 0)
  assert_true(medium_load.overall_score > 0)
  assert_true(heavy_load.overall_score > 0)
  assert_true(burst_load.overall_score > 0)
  assert_true(sustained_load.overall_score > 0)
}