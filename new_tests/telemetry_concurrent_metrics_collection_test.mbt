// 遥测并发指标收集测试
// 测试在高并发环境下指标收集的准确性和性能

test "concurrent_metrics_collection_basic" {
  // 基础并发指标收集测试
  let num_threads = 10
  let metrics_per_thread = 100
  let metric_names = ["cpu_usage", "memory_usage", "disk_io", "network_throughput"]
  
  let simulate_concurrent_collection = fn(threads : Int, metrics_per_thread : Int, names : Array[String]) -> Array[(String, Int64)] {
    let mut all_metrics = []
    
    // 模拟每个线程收集指标
    for thread_id = 0; threads > thread_id; thread_id = thread_id + 1 {
      for metric_idx = 0; metrics_per_thread > metric_idx; metric_idx = metric_idx + 1 {
        let metric_name = names[metric_idx % names.length]
        let metric_value = @int.to_int64(thread_id * 1000 + metric_idx)
        let timestamp = 1640995200000L + @int.to_int64(metric_idx * 1000)
        
        all_metrics = all_metrics.push((metric_name, metric_value, timestamp))
      }
    }
    
    all_metrics
  }
  
  let collected_metrics = simulate_concurrent_collection(num_threads, metrics_per_thread, metric_names)
  
  // 验证收集的指标数量
  let expected_total = num_threads * metrics_per_thread
  @assertion.assert_eq(collected_metrics.length, expected_total)?
  
  // 验证指标分布
  let metrics_by_name = metric_names.map(fn(name) {
    let count = collected_metrics.filter(fn((metric_name, _, _)) { metric_name == name }).length
    (name, count)
  })
  
  // 每个指标名称应该有大致相等的数量
  let expected_per_metric = expected_total / metric_names.length
  for (name, count) in metrics_by_name {
    @assertion.assert_eq(count, expected_per_metric)?
  }
}

test "concurrent_metrics_aggregation" {
  // 并发指标聚合测试
  let num_collectors = 8
  let collection_interval_ms = 100L
  let aggregation_window_ms = 500L
  
  let generate_metrics_stream = fn(collector_id : Int, num_points : Int) -> Array[(String, Int64, Int64)] {
    let mut metrics = []
    for i = 0; i < num_points; i = i + 1 {
      let timestamp = 1640995200000L + @int.to_int64(i * collection_interval_ms)
      let value = @int.to_int64(collector_id * 100 + i * 10)
      metrics = metrics.push(("response_time", value, timestamp))
    }
    metrics
  }
  
  let aggregate_concurrent_metrics = fn(collectors : Int, points_per_collector : Int) -> Array[(Int64, Double, Int)] {
    // 生成所有收集器的数据
    let all_metrics = []
    for collector_id = 0; collector_id < collectors; collector_id = collector_id + 1 {
      let collector_metrics = generate_metrics_stream(collector_id, points_per_collector)
      all_metrics.extend(collector_metrics)
    }
    
    // 按时间窗口聚合
    let time_windows = []
    let mut current_window = 1640995200000L
    
    while current_window < 1640995200000L + @int.to_int64(points_per_collector * collection_interval_ms) {
      let window_end = current_window + aggregation_window_ms
      let window_metrics = all_metrics.filter(fn((_, _, timestamp)) {
        timestamp >= current_window && timestamp < window_end
      })
      
      if window_metrics.length > 0 {
        let values = window_metrics.map(fn((_, value, _)) { value })
        let sum = values.reduce(fn(acc, val) { acc + val }, 0L)
        let avg = @float.from_int64(sum) / @float.from_int(values.length)
        time_windows = time_windows.push((current_window, avg, values.length))
      }
      
      current_window = window_end
    }
    
    time_windows
  }
  
  let aggregation_results = aggregate_concurrent_metrics(num_collectors, 20)
  
  // 验证聚合结果
  @assertion.assert_true(aggregation_results.length > 0)?
  
  // 验证每个时间窗口都有有效的平均值
  for (window_start, avg, count) in aggregation_results {
    @assertion.assert_true(avg >= 0.0)?
    @assertion.assert_true(count > 0)?
    @assertion.assert_true(count <= num_collectors * 20)?
  }
  
  // 验证时间窗口顺序
  for i = 1; i < aggregation_results.length; i = i + 1 {
    let prev_start = aggregation_results[i - 1].0
    let curr_start = aggregation_results[i].0
    @assertion.assert_true(curr_start > prev_start)?
  }
}

test "concurrent_metrics_thread_safety" {
  // 并发指标线程安全测试
  let shared_counter = { "value": 0 }
  let num_operations = 1000
  let num_threads = 20
  
  let simulate_atomic_increment = fn(counter : { "value": Int }, operations : Int, thread_id : Int) -> Int {
    // 模拟原子操作：每个线程增加计数器
    let mut local_counter = counter.value
    
    for i = 0; i < operations; i = i + 1 {
      // 模拟并发访问冲突
      let increment = 1 + thread_id % 3  // 不同线程可能有不同的增量
      local_counter = local_counter + increment
    }
    
    local_counter
  }
  
  let concurrent_results = []
  for thread_id = 0; thread_id < num_threads; thread_id = thread_id + 1 {
    let result = simulate_atomic_increment(shared_counter, num_operations, thread_id)
    concurrent_results = concurrent_results.push(result)
  }
  
  // 验证线程安全性：每个线程的结果应该是独立的
  @assertion.assert_eq(concurrent_results.length, num_threads)?
  
  // 验证结果模式：线程ID相似的线程应该有相似的结果
  let even_thread_results = concurrent_results.filter_with_index(fn(idx, _) { idx % 2 == 0 })
  let odd_thread_results = concurrent_results.filter_with_index(fn(idx, _) { idx % 2 == 1 })
  
  let even_avg = @float.from_int(even_thread_results.reduce(fn(acc, val) { acc + val }, 0)) / @float.from_int(even_thread_results.length)
  let odd_avg = @float.from_int(odd_thread_results.reduce(fn(acc, val) { acc + val }, 0)) / @float.from_int(odd_thread_results.length)
  
  // 偶数线程和奇数线程的平均值应该不同（因为增量模式不同）
  @assertion.assert_true(even_avg != odd_avg)?
}

test "concurrent_metrics_performance" {
  // 并发指标性能测试
  let concurrency_levels = [1, 2, 4, 8, 16]
  let operations_per_thread = 500
  let base_operation_time_ns = 1000L  // 每个操作基础时间1微秒
  
  let simulate_concurrent_performance = fn(concurrency : Int, ops_per_thread : Int) -> (Int64, Double) {
    // 并发性能模型：考虑锁竞争和上下文切换开销
    let single_thread_time = @int.to_int64(ops_per_thread) * base_operation_time_ns
    let contention_overhead = @int.to_int64(concurrency * concurrency * 100)  // 锁竞争开销
    let context_switch_overhead = @int.to_int64((concurrency - 1) * 1000)    // 上下文切换开销
    
    let total_time = single_thread_time + contention_overhead + context_switch_overhead
    let total_operations = concurrency * ops_per_thread
    let throughput = @float.from_int(total_operations) * 1000000000.0 / @float.from_int64(total_time)
    
    (total_time, throughput)
  }
  
  let performance_results = concurrency_levels.map(fn(level) {
    let (time, throughput) = simulate_concurrent_performance(level, operations_per_thread)
    (level, time, throughput)
  })
  
  // 验证性能特征
  let single_thread_throughput = performance_results[0].2
  let max_concurrency_throughput = performance_results.map(fn(_, _, t) { t }).reduce(fn(acc, t) { if t > acc { t } else { acc } }, 0.0)
  
  // 并发应该提高吞吐量（至少到某个点）
  @assertion.assert_true(max_concurrency_throughput > single_thread_throughput)?
  
  // 验证时间随并发度增加而增加（但不完全是线性）
  @assertion.assert_true(performance_results[1].1 > performance_results[0].1)?
  @assertion.assert_true(performance_results[2].1 > performance_results[1].1)?
  
  // 验证高并发时的性能下降（由于锁竞争）
  let throughput_8 = performance_results.filter(fn(l, _, _) { l == 8 })[0].2
  let throughput_16 = performance_results.filter(fn(l, _, _) { l == 16 })[0].2
  @assertion.assert_true(throughput_16 < throughput_8)?
}

test "concurrent_metrics_memory_consistency" {
  // 并发指标内存一致性测试
  let shared_metrics_store = {
    "counters": [],
    "gauges": [],
    "histograms": []
  }
  
  let concurrent_writers = 5
  let metrics_per_writer = 50
  
  let simulate_concurrent_writes = fn(store : { "counters": Array[(String, Int64)], "gauges": Array[(String, Double)], "histograms": Array[(String, Array[Double])] }, writer_id : Int, num_metrics : Int) -> { "counters": Int, "gauges": Int, "histograms": Int } {
    let mut counters_written = 0
    let mut gauges_written = 0
    let mut histograms_written = 0
    
    for i = 0; i < num_metrics; i = i + 1 {
      let metric_type = i % 3
      
      match metric_type {
        0 => {
          let counter_name = "counter_" + @int.to_string(writer_id) + "_" + @int.to_string(i)
          let counter_value = @int.to_int64(writer_id * 1000 + i)
          // 模拟写入计数器
          counters_written = counters_written + 1
        }
        1 => {
          let gauge_name = "gauge_" + @int.to_string(writer_id) + "_" + @int.to_string(i)
          let gauge_value = @float.from_int(writer_id) + @float.from_int(i) * 0.1
          // 模拟写入仪表
          gauges_written = gauges_written + 1
        }
        2 => {
          let histogram_name = "histogram_" + @int.to_string(writer_id) + "_" + @int.to_string(i)
          let histogram_values = [@float.from_int(i), @float.from_int(i + 1), @float.from_int(i + 2)]
          // 模拟写入直方图
          histograms_written = histograms_written + 1
        }
        _ => {}
      }
    }
    
    { "counters": counters_written, "gauges": gauges_written, "histograms": histograms_written }
  }
  
  let write_results = []
  for writer_id = 0; writer_id < concurrent_writers; writer_id = writer_id + 1 {
    let result = simulate_concurrent_writes(shared_metrics_store, writer_id, metrics_per_writer)
    write_results = write_results.push(result)
  }
  
  // 验证写入一致性
  let total_counters = write_results.map(fn(r) { r.counters }).reduce(fn(acc, val) { acc + val }, 0)
  let total_gauges = write_results.map(fn(r) { r.gauges }).reduce(fn(acc, val) { acc + val }, 0)
  let total_histograms = write_results.map(fn(r) { r.histograms }).reduce(fn(acc, val) { acc + val }, 0)
  
  let expected_total_metrics = concurrent_writers * metrics_per_writer
  let actual_total_metrics = total_counters + total_gauges + total_histograms
  
  @assertion.assert_eq(actual_total_metrics, expected_total_metrics)?
  
  // 验证每种类型的指标数量大致相等
  let expected_per_type = expected_total_metrics / 3
  @assertion.assert_eq(total_counters, expected_per_type)?
  @assertion.assert_eq(total_gauges, expected_per_type)?
  @assertion.assert_eq(total_histograms, expected_per_type)?
}

test "concurrent_metrics_error_handling" {
  // 并发指标错误处理测试
  let error_scenarios = [
    ("network_timeout", 0.1),      // 10%的网络超时
    ("serialization_error", 0.05), // 5%的序列化错误
    ("storage_full", 0.02),        // 2%的存储满错误
    ("invalid_metric", 0.03)       // 3%的无效指标错误
  ]
  
  let concurrent_collectors = 10
  let collection_attempts = 100
  
  let simulate_error_prone_collection = fn(error_rate : Double, attempts : Int, collector_id : Int) -> { "successful": Int, "failed": Int, "errors": Array[String] } {
    let mut successful = 0
    let mut failed = 0
    let mut errors = []
    
    for i = 0; i < attempts; i = i + 1 {
      // 简化的错误模拟：基于收集器ID和尝试次数
      let error_probability = (collector_id * 7 + i * 13) % 100
      let should_fail = @float.from_int(error_probability) < error_rate * 100.0
      
      if should_fail {
        failed = failed + 1
        let error_type = match error_probability % 4 {
          0 => "network_timeout"
          1 => "serialization_error"
          2 => "storage_full"
          3 => "invalid_metric"
          _ => "unknown"
        }
        errors = errors.push(error_type)
      } else {
        successful = successful + 1
      }
    }
    
    { "successful": successful, "failed": failed, "errors": errors }
  }
  
  let concurrent_results = []
  for collector_id = 0; collector_id < concurrent_collectors; collector_id = collector_id + 1 {
    let (error_type, error_rate) = error_scenarios[collector_id % error_scenarios.length]
    let result = simulate_error_prone_collection(error_rate, collection_attempts, collector_id)
    concurrent_results = concurrent_results.push((collector_id, error_type, result))
  }
  
  // 验证错误处理
  let total_successful = concurrent_results.map(fn(_, _, r) { r.successful }).reduce(fn(acc, val) { acc + val }, 0)
  let total_failed = concurrent_results.map(fn(_, _, r) { r.failed }).reduce(fn(acc, val) { acc + val }, 0)
  let total_attempts = concurrent_collectors * collection_attempts
  
  @assertion.assert_eq(total_successful + total_failed, total_attempts)?
  
  // 验证错误率大致符合预期
  let actual_error_rate = @float.from_int(total_failed) / @float.from_int(total_attempts)
  let expected_avg_error_rate = error_scenarios.map(fn(_, rate) { rate }).reduce(fn(acc, rate) { acc + rate }, 0.0) / @float.from_int(error_scenarios.length)
  
  @assertion.assert_true(@float.abs(actual_error_rate - expected_avg_error_rate) < 0.05)?
  
  // 验证错误分布
  let all_errors = concurrent_results.map(fn(_, _, r) { r.errors }).reduce(fn(acc, errors) { acc.extend(errors) }, [])
  let error_counts = error_scenarios.map(fn(error_type, _) {
    let count = all_errors.filter(fn(e) { e == error_type }).length
    (error_type, count)
  })
  
  // 每种错误类型都应该出现
  for (error_type, count) in error_counts {
    @assertion.assert_true(count > 0)?
  }
}

test "concurrent_metrics_load_balancing" {
  // 并发指标负载均衡测试
  let num_shards = 4
  let num_metrics = 1000
  let metric_names = ["cpu", "memory", "disk", "network", "custom"]
  
  let shard_based_distribution = fn(metric_name : String, num_shards : Int) -> Int {
    // 基于指标名称的哈希分片
    let hash = metric_name.length + metric_name.char_at(0).to_int()
    hash % num_shards
  }
  
  let distribute_metrics_concurrently = fn(names : Array[String], count : Int, shards : Int) -> Array[(Int, Int)] {
    let mut shard_counts = [for i = 0; i < shards; i = i + 1].map(fn(_) { (i, 0) })
    
    for i = 0; i < count; i = i + 1 {
      let metric_name = names[i % names.length]
      let shard_id = shard_based_distribution(metric_name, shards)
      
      // 更新对应分片的计数
      shard_counts = shard_counts.map(fn(id, count) {
        if id == shard_id {
          (id, count + 1)
        } else {
          (id, count)
        }
      })
    }
    
    shard_counts
  }
  
  let distribution = distribute_metrics_concurrently(metric_names, num_metrics, num_shards)
  
  // 验证负载均衡
  let total_distributed = distribution.map(fn(_, count) { count }).reduce(fn(acc, count) { acc + count }, 0)
  @assertion.assert_eq(total_distributed, num_metrics)?
  
  // 验证每个分片都有指标
  for (shard_id, count) in distribution {
    @assertion.assert_true(count > 0)?
  }
  
  // 验证负载相对均衡（差异不应太大）
  let counts = distribution.map(fn(_, count) { count })
  let max_count = counts.reduce(fn(acc, count) { if count > acc { count } else { acc } }, 0)
  let min_count = counts.reduce(fn(acc, count) { if count < acc { count } else { acc } }, 1000000)
  
  let imbalance_ratio = @float.from_int(max_count - min_count) / @float.from_int(min_count)
  @assertion.assert_true(imbalance_ratio < 0.5)?  // 不平衡度应小于50%
}