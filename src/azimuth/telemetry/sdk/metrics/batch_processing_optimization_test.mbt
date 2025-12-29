// 批处理优化测试 - 测试批处理参数的性能影响

test "batch_size_optimization_analysis" {
  // 测试批处理大小对性能的影响
  
  struct BatchSizeConfig {
    batch_size : Int
    max_queue_size : Int
    scheduled_delay_ms : Int
    export_timeout_ms : Int
  }
  
  struct BatchPerformanceMetrics {
    config : BatchSizeConfig
    throughput_items_per_sec : Int
    latency_p50_ms : Int
    latency_p95_ms : Int
    latency_p99_ms : Int
    memory_usage_mb : Double
    cpu_utilization_percent : Int
    export_efficiency_percent : Int
  }
  
  func simulate_batch_performance(config : BatchSizeConfig, input_rate : Int) -> BatchPerformanceMetrics {
    // 模拟批处理性能计算
    let batch_processing_time = config.batch_size * 2  // 每项2μs处理时间
    let network_overhead = 5000  // 5ms网络开销
    let total_batch_time = batch_processing_time + network_overhead
    
    // 计算吞吐量
    let batches_per_sec = 1000 / config.scheduled_delay_ms
    let throughput = config.batch_size * batches_per_sec
    
    // 计算延迟 (基于排队论)
    let base_latency = config.scheduled_delay_ms / 2
    let queue_latency = if input_rate > throughput {
      (input_rate - throughput) * config.scheduled_delay_ms / config.batch_size
    } else {
      0
    }
    
    let p50_latency = base_latency
    let p95_latency = base_latency + queue_latency * 2
    let p99_latency = base_latency + queue_latency * 4
    
    // 计算内存使用
    let queue_memory = config.max_queue_size.to_double() * 0.001  // 每项1KB
    let batch_memory = config.batch_size.to_double() * 0.001
    let total_memory = (queue_memory + batch_memory) / 1024.0  // 转换为MB
    
    // 计算CPU利用率
    let processing_cpu = (throughput * 2 * 100) / 1000000  // 基于处理时间
    let network_cpu = (batches_per_sec * network_overhead * 100) / 1000000
    let total_cpu = (processing_cpu + network_cpu).to_int().min(100)
    
    // 计算导出效率
    let ideal_throughput = input_rate
    let actual_throughput = throughput.min(input_rate)
    let efficiency = if ideal_throughput > 0 {
      (actual_throughput * 100) / ideal_throughput
    } else {
      0
    }
    
    BatchPerformanceMetrics{
      config: config,
      throughput_items_per_sec: throughput,
      latency_p50_ms: p50_latency,
      latency_p95_ms: p95_latency,
      latency_p99_ms: p99_latency,
      memory_usage_mb: total_memory,
      cpu_utilization_percent: total_cpu,
      export_efficiency_percent: efficiency
    }
  }
  
  // 测试不同批处理大小的性能
  let batch_sizes = [32, 64, 128, 256, 512, 1024, 2048]
  let input_rate = 5000  // 每秒5000项
  let mut performance_results = []
  
  for batch_size in batch_sizes {
    let config = BatchSizeConfig{
      batch_size: batch_size,
      max_queue_size: batch_size * 4,
      scheduled_delay_ms: 5000 / (batch_size / 128),  // 动态调整调度延迟
      export_timeout_ms: 30000
    }
    
    let metrics = simulate_batch_performance(config, input_rate)
    performance_results.push(metrics)
  }
  
  // 分析性能趋势
  let mut max_throughput = 0
  let mut optimal_batch_size = 0
  let mut min_latency_p95 = 2147483647  // Int最大值
  
  for metrics in performance_results {
    // 找到最大吞吐量
    if metrics.throughput_items_per_sec > max_throughput {
      max_throughput = metrics.throughput_items_per_sec
      optimal_batch_size = metrics.config.batch_size
    }
    
    // 找到最小P95延迟
    if metrics.latency_p95_ms < min_latency_p95 {
      min_latency_p95 = metrics.latency_p95_ms
    }
    
    // 验证性能指标的合理性
    assert_eq(metrics.throughput_items_per_sec > 0, true)
    assert_eq(metrics.latency_p50_ms > 0, true)
    assert_eq(metrics.latency_p95_ms >= metrics.latency_p50_ms, true)
    assert_eq(metrics.latency_p99_ms >= metrics.latency_p95_ms, true)
    assert_eq(metrics.memory_usage_mb > 0.0, true)
    assert_eq(metrics.cpu_utilization_percent >= 0 && metrics.cpu_utilization_percent <= 100, true)
    assert_eq(metrics.export_efficiency_percent >= 0 && metrics.export_efficiency_percent <= 100, true)
  }
  
  // 验证最优批处理大小的合理性
  assert_eq(optimal_batch_size >= 128 && optimal_batch_size <= 1024, true, 
    "Optimal batch size should be in reasonable range")
  assert_eq(max_throughput >= input_rate * 80 / 100, true, 
    "Max throughput should be at least 80% of input rate")
}

test "scheduled_delay_optimization" {
  // 测试调度延迟优化的影响
  
  struct DelayConfig {
    scheduled_delay_ms : Int
    batch_size : Int
    input_rate_variance : Double  // 输入速率变化系数
  }
  
  struct DelayOptimizationMetrics {
    config : DelayConfig
    average_batch_fullness : Double
    export_frequency_per_min : Int
    latency_variance : Double
    resource_utilization : Double
  }
  
  func simulate_delay_optimization(config : DelayConfig) -> DelayOptimizationMetrics {
    // 模拟输入速率的变化
    let base_rate = 1000  // 基础速率：每秒1000项
    let max_rate = (base_rate.to_double() * (1.0 + config.input_rate_variance)).to_int()
    let min_rate = (base_rate.to_double() * (1.0 - config.input_rate_variance)).to_int()
    
    // 计算平均批处理填充度
    let avg_items_per_delay = base_rate * config.scheduled_delay_ms / 1000
    let batch_fullness = avg_items_per_delay.to_double() / config.batch_size.to_double()
    let average_batch_fullness = batch_fullness.min(1.0)
    
    // 计算导出频率
    let exports_per_second = 1000 / config.scheduled_delay_ms
    let export_frequency_per_min = exports_per_second * 60
    
    // 计算延迟方差（基于输入速率变化）
    let max_latency = config.scheduled_delay_ms
    let min_latency = (config.batch_size * 1000) / max_rate  // 基于批大小和最大速率
    let latency_variance = (max_latency - min_latency).to_double() / max_latency.to_double()
    
    // 计算资源利用率
    let processing_capacity = config.batch_size * 1000 / config.scheduled_delay_ms
    let utilization = base_rate.to_double() / processing_capacity.to_double()
    let resource_utilization = utilization.min(1.0)
    
    DelayOptimizationMetrics{
      config: config,
      average_batch_fullness: average_batch_fullness,
      export_frequency_per_min: export_frequency_per_min,
      latency_variance: latency_variance,
      resource_utilization: resource_utilization
    }
  }
  
  // 测试不同调度延迟配置
  let delay_configs = [
    DelayConfig{ scheduled_delay_ms: 100, batch_size: 100, input_rate_variance: 0.2 },   // 高频导出
    DelayConfig{ scheduled_delay_ms: 500, batch_size: 500, input_rate_variance: 0.2 },   // 中频导出
    DelayConfig{ scheduled_delay_ms: 1000, batch_size: 1000, input_rate_variance: 0.2 }, // 低频导出
    DelayConfig{ scheduled_delay_ms: 2000, batch_size: 2000, input_rate_variance: 0.2 }, // 超低频导出
    DelayConfig{ scheduled_delay_ms: 500, batch_size: 100, input_rate_variance: 0.8 },   // 高变化率
    DelayConfig{ scheduled_delay_ms: 500, batch_size: 500, input_rate_variance: 0.8 },   // 中变化率
    DelayConfig{ scheduled_delay_ms: 500, batch_size: 1000, input_rate_variance: 0.8 }   // 低变化率
  ]
  
  let mut optimization_results = []
  
  for config in delay_configs {
    let metrics = simulate_delay_optimization(config)
    optimization_results.push(metrics)
    
    // 验证指标合理性
    assert_eq(metrics.average_batch_fullness >= 0.0 && metrics.average_batch_fullness <= 1.0, true)
    assert_eq(metrics.export_frequency_per_min > 0, true)
    assert_eq(metrics.latency_variance >= 0.0 && metrics.latency_variance <= 1.0, true)
    assert_eq(metrics.resource_utilization >= 0.0 && metrics.resource_utilization <= 1.0, true)
  }
  
  // 分析最优配置
  let mut best_config = None
  let mut best_score = 0.0
  
  for metrics in optimization_results {
    // 计算综合评分：填充度 + 利用率 - 延迟方差
    let score = metrics.average_batch_fullness + metrics.resource_utilization - metrics.latency_variance
    
    if score > best_score {
      best_score = score
      best_config = Some(metrics.config)
    }
  }
  
  match best_config {
    Some(config) => {
      // 验证最优配置的合理性
      assert_eq(config.scheduled_delay_ms >= 100 && config.scheduled_delay_ms <= 2000, true,
        "Optimal scheduled delay should be in reasonable range")
      assert_eq(config.batch_size >= 100 && config.batch_size <= 1000, true,
        "Optimal batch size should be in reasonable range")
    }
    None => @test.fail("Should find an optimal configuration")
  }
}

test "queue_size_optimization" {
  // 测试队列大小优化的影响
  
  struct QueueConfig {
    max_queue_size : Int
    batch_size : Int
    input_rate_burst : Int
    processing_rate : Int
  }
  
  struct QueueOptimizationMetrics {
    config : QueueConfig
    queue_utilization_percent : Double
    drop_rate_percent : Double
    memory_efficiency : Double
    burst_handling_capacity : Int
  }
  
  func simulate_queue_optimization(config : QueueConfig) -> QueueOptimizationMetrics {
    // 计算队列利用率
    let steady_state_queue = (config.input_rate_burst - config.processing_rate) * 5  // 5秒突发
    let queue_utilization = steady_state_queue.to_double() / config.max_queue_size.to_double()
    let queue_utilization_percent = queue_utilization.min(1.0) * 100.0
    
    // 计算丢弃率
    let excess_items = if steady_state_queue > config.max_queue_size {
      steady_state_queue - config.max_queue_size
    } else {
      0
    }
    let total_items = config.input_rate_burst * 5
    let drop_rate_percent = if total_items > 0 {
      (excess_items * 100) / total_items
    } else {
      0
    }.to_double()
    
    // 计算内存效率
    let used_memory = steady_state_queue.to_double() * 0.001  // 每项1KB
    let allocated_memory = config.max_queue_size.to_double() * 0.001
    let memory_efficiency = if allocated_memory > 0.0 {
      used_memory / allocated_memory
    } else {
      0.0
    }
    
    // 计算突发处理能力
    let burst_duration = config.max_queue_size / (config.input_rate_burst - config.processing_rate).max(1)
    let burst_handling_capacity = burst_duration * config.input_rate_burst
    
    QueueOptimizationMetrics{
      config: config,
      queue_utilization_percent: queue_utilization_percent,
      drop_rate_percent: drop_rate_percent,
      memory_efficiency: memory_efficiency,
      burst_handling_capacity: burst_handling_capacity
    }
  }
  
  // 测试不同队列大小配置
  let base_batch_size = 512
  let queue_configs = [
    QueueConfig{ 
      max_queue_size: base_batch_size * 2, 
      batch_size: base_batch_size, 
      input_rate_burst: 2000, 
      processing_rate: 1000 
    },
    QueueConfig{ 
      max_queue_size: base_batch_size * 4, 
      batch_size: base_batch_size, 
      input_rate_burst: 2000, 
      processing_rate: 1000 
    },
    QueueConfig{ 
      max_queue_size: base_batch_size * 8, 
      batch_size: base_batch_size, 
      input_rate_burst: 2000, 
      processing_rate: 1000 
    },
    QueueConfig{ 
      max_queue_size: base_batch_size * 16, 
      batch_size: base_batch_size, 
      input_rate_burst: 2000, 
      processing_rate: 1000 
    }
  ]
  
  let mut queue_results = []
  
  for config in queue_configs {
    let metrics = simulate_queue_optimization(config)
    queue_results.push(metrics)
    
    // 验证指标合理性
    assert_eq(metrics.queue_utilization_percent >= 0.0 && metrics.queue_utilization_percent <= 100.0, true)
    assert_eq(metrics.drop_rate_percent >= 0.0, true)
    assert_eq(metrics.memory_efficiency >= 0.0 && metrics.memory_efficiency <= 1.0, true)
    assert_eq(metrics.burst_handling_capacity >= 0, true)
  }
  
  // 分析队列大小对性能的影响
  let mut smallest_queue_with_zero_drops = None
  let mut best_memory_efficiency = 0.0
  let mut most_efficient_config = None
  
  for metrics in queue_results {
    // 找到最小的零丢弃队列配置
    if metrics.drop_rate_percent == 0.0 {
      match smallest_queue_with_zero_drops {
        None => smallest_queue_with_zero_drops = Some(metrics.config)
        Some(current_config) => {
          if metrics.config.max_queue_size < current_config.max_queue_size {
            smallest_queue_with_zero_drops = Some(metrics.config)
          }
        }
      }
    }
    
    // 找到内存效率最高的配置
    if metrics.memory_efficiency > best_memory_efficiency {
      best_memory_efficiency = metrics.memory_efficiency
      most_efficient_config = Some(metrics.config)
    }
  }
  
  // 验证结果
  match smallest_queue_with_zero_drops {
    Some(config) => {
      assert_eq(config.max_queue_size >= base_batch_size * 2, true,
        "Queue should be at least 2x batch size for zero drops")
    }
    None => @test.fail("Should find a queue configuration with zero drops")
  }
  
  match most_efficient_config {
    Some(config) => {
      assert_eq(config.max_queue_size >= base_batch_size, true,
        "Most efficient config should have reasonable queue size")
    }
    None => @test.fail("Should find a most efficient configuration")
  }
}

test "adaptive_batch_processing" {
  // 测试自适应批处理算法
  
  struct AdaptiveConfig {
    initial_batch_size : Int
    min_batch_size : Int
    max_batch_size : Int
    target_latency_ms : Int
    adjustment_factor : Double
    measurement_window_ms : Int
  }
  
  struct AdaptiveMetrics {
    current_batch_size : Int
    current_latency_ms : Int
    current_throughput : Int
    adjustment_count : Int
    convergence_time_ms : Int
  }
  
  func simulate_adaptive_batching(config : AdaptiveConfig, input_rate : Int, duration_ms : Int) -> AdaptiveMetrics {
    let mut current_batch_size = config.initial_batch_size
    let mut adjustment_count = 0
    let mut convergence_time = duration_ms
    
    // 模拟自适应过程
    for time = 0; time < duration_ms; time = time + config.measurement_window_ms {
      // 计算当前延迟
      let processing_time = current_batch_size * 2  // 每项2μs
      let network_time = 5000  // 5ms网络时间
      let current_latency = processing_time + network_time
      
      // 计算当前吞吐量
      let batches_per_sec = 1000 / config.measurement_window_ms
      let current_throughput = current_batch_size * batches_per_sec
      
      // 自适应调整
      if current_latency > config.target_latency_ms {
        // 延迟过高，减小批处理大小
        let new_batch_size = (current_batch_size.to_double() / config.adjustment_factor).to_int()
        current_batch_size = new_batch_size.max(config.min_batch_size)
        adjustment_count = adjustment_count + 1
      } else if current_throughput < input_rate * 80 / 100 {
        // 吞吐量不足，增大批处理大小
        let new_batch_size = (current_batch_size.to_double() * config.adjustment_factor).to_int()
        current_batch_size = new_batch_size.min(config.max_batch_size)
        adjustment_count = adjustment_count + 1
      }
      
      // 检查是否收敛（连续3次测量无需调整）
      if adjustment_count > 0 && time > convergence_time - config.measurement_window_ms * 3 {
        convergence_time = time
        break
      }
    }
    
    // 计算最终指标
    let final_processing_time = current_batch_size * 2
    let final_latency = final_processing_time + 5000
    let final_throughput = current_batch_size * (1000 / config.measurement_window_ms)
    
    AdaptiveMetrics{
      current_batch_size: current_batch_size,
      current_latency_ms: final_latency,
      current_throughput: final_throughput,
      adjustment_count: adjustment_count,
      convergence_time_ms: convergence_time
    }
  }
  
  // 创建自适应配置
  let adaptive_config = AdaptiveConfig{
    initial_batch_size: 256,
    min_batch_size: 64,
    max_batch_size: 1024,
    target_latency_ms: 100,
    adjustment_factor: 1.2,
    measurement_window_ms: 5000
  }
  
  // 测试不同输入速率下的自适应行为
  let test_scenarios = [
    (500, "low_load"),      // 低负载
    (2000, "medium_load"),  // 中等负载
    (5000, "high_load"),    // 高负载
    (8000, "burst_load")    // 突发负载
  ]
  
  for (input_rate, scenario_name) in test_scenarios {
    let metrics = simulate_adaptive_batching(adaptive_config, input_rate, 60000)  // 1分钟测试
    
    // 验证自适应结果
    assert_eq(metrics.current_batch_size >= adaptive_config.min_batch_size, true,
      "Adaptive batch size should not go below minimum for {scenario_name}")
    assert_eq(metrics.current_batch_size <= adaptive_config.max_batch_size, true,
      "Adaptive batch size should not exceed maximum for {scenario_name}")
    assert_eq(metrics.adjustment_count >= 0, true,
      "Should track adjustment count for {scenario_name}")
    assert_eq(metrics.convergence_time_ms <= 60000, true,
      "Should converge within test duration for {scenario_name}")
    
    // 验证收敛后的性能
    let latency_acceptable = metrics.current_latency_ms <= adaptive_config.target_latency_ms * 2
    let throughput_acceptable = metrics.current_throughput >= input_rate * 70 / 100
    
    assert_eq(latency_acceptable || throughput_acceptable, true,
      "Should achieve acceptable latency or throughput for {scenario_name}")
  }
  
  // 测试自适应算法的稳定性
  let stability_metrics = simulate_adaptive_batching(adaptive_config, 2000, 300000)  // 5分钟稳定性测试
  
  // 稳定性测试应该有较少的调整次数
  assert_eq(stability_metrics.adjustment_count <= 10, true,
    "Stable load should require few adjustments")
  assert_eq(stability_metrics.convergence_time_ms <= 60000, true,
    "Should converge quickly under stable load")
}

test "batch_processing_compression_optimization" {
  // 测试批处理压缩优化
  
  struct CompressionConfig {
    compression_enabled : Bool
    compression_algorithm : String
    compression_threshold : Int  // 最小压缩大小
    batch_size : Int
    item_size_bytes : Int
  }
  
  struct CompressionMetrics {
    config : CompressionConfig
    compression_ratio : Double
    compression_overhead_ms : Int
    network_savings_percent : Double
    cpu_overhead_percent : Double
  }
  
  func simulate_compression_optimization(config : CompressionConfig) -> CompressionMetrics {
    let batch_data_size = config.batch_size * config.item_size_bytes
    
    // 模拟压缩效果
    let (compressed_size, compression_time, cpu_overhead) = if config.compression_enabled && 
                                                              batch_data_size >= config.compression_threshold {
      match config.compression_algorithm {
        "gzip" => {
          let ratio = 0.3  // gzip通常能达到30%压缩率
          let time = batch_data_size / 10000  // 压缩速度：10MB/s
          let cpu = batch_data_size / 5000    // CPU开销
          ((batch_data_size.to_double() * ratio).to_int(), time, cpu)
        }
        "lz4" => {
          let ratio = 0.5  // lz4压缩率较低但速度快
          let time = batch_data_size / 50000  // 压缩速度：50MB/s
          let cpu = batch_data_size / 20000   // CPU开销较低
          ((batch_data_size.to_double() * ratio).to_int(), time, cpu)
        }
        _ => (batch_data_size, 0, 0)  // 不压缩
      }
    } else {
      (batch_data_size, 0, 0)
    }
    
    // 计算压缩指标
    let compression_ratio = if batch_data_size > 0 {
      compressed_size.to_double() / batch_data_size.to_double()
    } else {
      1.0
    }
    
    let network_savings = if compression_ratio < 1.0 {
      (1.0 - compression_ratio) * 100.0
    } else {
      0.0
    }
    
    let base_processing_time = batch_data_size / 1000  // 基础处理时间
    let total_processing_time = base_processing_time + compression_time
    let cpu_overhead_percent = if base_processing_time > 0 {
      (cpu_overhead * 100) / base_processing_time
    } else {
      0.0
    }
    
    CompressionMetrics{
      config: config,
      compression_ratio: compression_ratio,
      compression_overhead_ms: compression_time,
      network_savings_percent: network_savings,
      cpu_overhead_percent: cpu_overhead_percent
    }
  }
  
  // 测试不同压缩配置
  let compression_configs = [
    CompressionConfig{
      compression_enabled: false,
      compression_algorithm: "",
      compression_threshold: 1024,
      batch_size: 512,
      item_size_bytes: 256
    },
    CompressionConfig{
      compression_enabled: true,
      compression_algorithm: "gzip",
      compression_threshold: 1024,
      batch_size: 512,
      item_size_bytes: 256
    },
    CompressionConfig{
      compression_enabled: true,
      compression_algorithm: "lz4",
      compression_threshold: 1024,
      batch_size: 512,
      item_size_bytes: 256
    },
    CompressionConfig{
      compression_enabled: true,
      compression_algorithm: "gzip",
      compression_threshold: 2048,  // 更高的压缩阈值
      batch_size: 1024,
      item_size_bytes: 256
    }
  ]
  
  let mut compression_results = []
  
  for config in compression_configs {
    let metrics = simulate_compression_optimization(config)
    compression_results.push(metrics)
    
    // 验证压缩指标
    assert_eq(metrics.compression_ratio > 0.0 && metrics.compression_ratio <= 1.0, true)
    assert_eq(metrics.compression_overhead_ms >= 0, true)
    assert_eq(metrics.network_savings_percent >= 0.0, true)
    assert_eq(metrics.cpu_overhead_percent >= 0.0, true)
  }
  
  // 分析压缩效果
  let no_compression = compression_results[0]
  let gzip_compression = compression_results[1]
  let lz4_compression = compression_results[2]
  
  // 验证gzip压缩效果
  assert_eq(gzip_compression.compression_ratio < no_compression.compression_ratio, true,
    "Gzip compression should reduce data size")
  assert_eq(gzip_compression.network_savings_percent > 0.0, true,
    "Gzip compression should provide network savings")
  assert_eq(gzip_compression.cpu_overhead_percent > 0.0, true,
    "Gzip compression should have CPU overhead")
  
  // 验证lz4压缩特性
  assert_eq(lz4_compression.compression_ratio < no_compression.compression_ratio, true,
    "LZ4 compression should reduce data size")
  assert_eq(lz4_compression.compression_overhead_ms < gzip_compression.compression_overhead_ms, true,
    "LZ4 should be faster than gzip")
  assert_eq(lz4_compression.cpu_overhead_percent < gzip_compression.cpu_overhead_percent, true,
    "LZ4 should have lower CPU overhead than gzip")
  
  // 验证压缩阈值的影响
  let high_threshold = compression_results[3]
  assert_eq(high_threshold.compression_ratio >= gzip_compression.compression_ratio, true,
    "Higher compression threshold may reduce effectiveness")
}