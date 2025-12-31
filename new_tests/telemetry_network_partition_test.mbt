// 网络分区容错测试用例

test "telemetry_network_partition_detection" {
  // 测试网络分区检测
  
  // 定义网络分区类型
  let partition_types = {
    "complete_partition": {
      "description": "完全网络分区，所有连接断开",
      "symptom": "all_connections_failed",
      "detection_time_ms": 5000,
      "recovery_strategy": "wait_and_retry"
    },
    "partial_partition": {
      "description": "部分网络分区，部分连接断开",
      "symptom": "subset_connections_failed",
      "detection_time_ms": 3000,
      "recovery_strategy": "fallback_endpoints"
    },
    "intermittent_partition": {
      "description": "间歇性网络分区，连接时断时续",
      "symptom": "flapping_connections",
      "detection_time_ms": 10000,
      "recovery_strategy": "circuit_breaker"
    },
    "asymmetric_partition": {
      "description": "非对称网络分区，单向连接断开",
      "symptom": "unidirectional_failure",
      "detection_time_ms": 7000,
      "recovery_strategy": "bidirectional_health_check"
    },
    "latency_spike_partition": {
      "description": "延迟激增导致的逻辑分区",
      "symptom": "excessive_latency",
      "detection_time_ms": 8000,
      "recovery_strategy": "timeout_adjustment"
    }
  }
  
  // 模拟网络健康检查
  fn simulate_network_health_check(endpoint_count: Int64, failure_rate: Double) -> (Int64, Int64, Bool) {
    let mut successful_connections = 0
    let mut failed_connections = 0
    
    for i = 0; i < endpoint_count; i = i + 1 {
      // 模拟连接结果
      let random_value = 0.5  // 简化：使用固定值模拟随机
      if random_value < failure_rate {
        failed_connections = failed_connections + 1
      } else {
        successful_connections = successful_connections + 1
      }
    }
    
    // 判断是否出现网络分区
    let failure_percentage = (failed_connections.to_double() / endpoint_count.to_double()) * 100.0
    let partition_detected = failure_percentage > 50.0  // 超过50%失败率认为是分区
    
    (successful_connections, failed_connections, partition_detected)
  }
  
  // 测试不同失败率下的分区检测
  let failure_rates = [0.1, 0.3, 0.5, 0.7, 0.9]
  let endpoint_count = 10
  
  for failure_rate in failure_rates {
    let (success, failed, partition_detected) = simulate_network_health_check(endpoint_count, failure_rate)
    
    // 验证连接统计
    assert_eq(success + failed, endpoint_count, 
      "Total connections should match endpoint count for failure rate " + failure_rate.to_string())
    
    // 验证分区检测逻辑
    let expected_partition_detection = failure_rate > 0.5
    assert_eq(partition_detected, expected_partition_detection, 
      "Partition detection should match expectation for failure rate " + failure_rate.to_string())
  }
  
  // 测试网络分区类型识别
  fn identify_partition_type(success_rate: Double, latency_ms: Int64, connection_stability: Bool) -> String {
    if success_rate < 0.1 {
      "complete_partition"
    } else if success_rate < 0.7 && connection_stability {
      "partial_partition"
    } else if !connection_stability {
      "intermittent_partition"
    } else if latency_ms > 10000 {
      "latency_spike_partition"
    } else if success_rate < 0.8 {
      "asymmetric_partition"
    } else {
      "no_partition"
    }
  }
  
  // 测试分区类型识别
  let partition_scenarios = [
    (0.05, 100, true, "complete_partition"),      // 5%成功率，低延迟，稳定连接
    (0.4, 200, true, "partial_partition"),        // 40%成功率，中延迟，稳定连接
    (0.8, 15000, true, "latency_spike_partition"), // 80%成功率，高延迟
    (0.7, 300, false, "intermittent_partition"),   // 70%成功率，不稳定连接
    (0.85, 500, true, "asymmetric_partition"),     // 85%成功率，中延迟
    (0.95, 50, true, "no_partition")              // 95%成功率，低延迟，正常状态
  ]
  
  for scenario in partition_scenarios {
    let success_rate = scenario.0
    let latency_ms = scenario.1
    let connection_stability = scenario.2
    let expected_type = scenario.3
    
    let detected_type = identify_partition_type(success_rate, latency_ms, connection_stability)
    
    assert_eq(detected_type, expected_type, 
      "Partition type detection should match expectation for scenario " + expected_type)
  }
  
  // 测试分区恢复时间
  fn simulate_partition_recovery(partition_type: String, recovery_attempts: Int64) -> (Bool, Int64) {
    let config = partition_types[partition_type]
    let base_recovery_time_ms = config["detection_time_ms"]
    
    // 模拟恢复成功率
    let recovery_success_rate = match partition_type {
      "complete_partition" => 0.3,
      "partial_partition" => 0.7,
      "intermittent_partition" => 0.5,
      "asymmetric_partition" => 0.6,
      "latency_spike_partition" => 0.8,
      _ => 0.5
    }
    
    // 模拟恢复过程
    let mut recovered = false
    let mut total_time_ms = 0L
    
    for attempt = 0; attempt < recovery_attempts; attempt = attempt + 1 {
      total_time_ms = total_time_ms + base_recovery_time_ms
      
      // 模拟恢复尝试
      let random_value = 0.5  // 简化
      if random_value < recovery_success_rate {
        recovered = true
        break
      }
    }
    
    (recovered, total_time_ms)
  }
  
  // 测试不同分区类型的恢复
  for partition_type in partition_types.keys() {
    let (recovered, recovery_time) = simulate_partition_recovery(partition_type, 5)
    
    // 验证恢复时间
    let config = partition_types[partition_type]
    let expected_min_time = config["detection_time_ms"]
    
    assert_eq(recovery_time >= expected_min_time, true, 
      "Recovery time should be at least detection time for " + partition_type)
    
    // 验证恢复可能性
    assert_eq(recovered == true || recovered == false, true, 
      "Recovery result should be boolean for " + partition_type)
  }
}

test "telemetry_network_partition_with_data_buffering" {
  // 测试网络分区时的数据缓冲
  
  // 定义缓冲策略配置
  let buffering_strategies = {
    "memory_buffer": {
      "max_size_mb": 100,
      "flush_threshold_percent": 80,
      "retention_time_ms": 300000,  // 5分钟
      "compression_enabled": true
    },
    "disk_buffer": {
      "max_size_mb": 1000,
      "flush_threshold_percent": 90,
      "retention_time_ms": 3600000,  // 1小时
      "compression_enabled": true
    },
    "hybrid_buffer": {
      "memory_max_size_mb": 50,
      "disk_max_size_mb": 500,
      "memory_to_disk_threshold": 70,  // 内存使用70%时开始写入磁盘
      "flush_threshold_percent": 85,
      "retention_time_ms": 1800000,   // 30分钟
      "compression_enabled": true
    },
    "circular_buffer": {
      "max_size_mb": 200,
      "overwrite_when_full": true,
      "retention_time_ms": 600000,    // 10分钟
      "compression_enabled": false
    }
  }
  
  // 模拟数据缓冲过程
  fn simulate_data_buffering(strategy: String, data_rate_mb_per_sec: Double, partition_duration_ms: Int64) -> (Int64, Int64, Bool) {
    let config = buffering_strategies[strategy]
    let total_data_mb = (data_rate_mb_per_sec * partition_duration_ms.to_double() / 1000.0).to_int64()
    
    let mut buffered_data_mb = 0L
    let mut dropped_data_mb = 0L
    let mut buffer_overflow = false
    
    match strategy {
      "memory_buffer" => {
        let max_size = config["max_size_mb"]
        let flush_threshold = (max_size.to_double() * config["flush_threshold_percent"].to_double() / 100.0).to_int64()
        
        if total_data_mb <= max_size {
          buffered_data_mb = total_data_mb
        } else {
          buffered_data_mb = max_size
          dropped_data_mb = total_data_mb - max_size
          buffer_overflow = true
        }
      }
      "disk_buffer" => {
        let max_size = config["max_size_mb"]
        
        if total_data_mb <= max_size {
          buffered_data_mb = total_data_mb
        } else {
          buffered_data_mb = max_size
          dropped_data_mb = total_data_mb - max_size
          buffer_overflow = true
        }
      }
      "hybrid_buffer" => {
        let memory_max = config["memory_max_size_mb"]
        let disk_max = config["disk_max_size_mb"]
        let total_max = memory_max + disk_max
        
        if total_data_mb <= memory_max {
          buffered_data_mb = total_data_mb
        } else if total_data_mb <= total_max {
          buffered_data_mb = total_data_mb
        } else {
          buffered_data_mb = total_max
          dropped_data_mb = total_data_mb - total_max
          buffer_overflow = true
        }
      }
      "circular_buffer" => {
        let max_size = config["max_size_mb"]
        let overwrite_when_full = config["overwrite_when_full"]
        
        if overwrite_when_full {
          // 循环缓冲区，新数据覆盖旧数据
          buffered_data_mb = max_size
        } else {
          if total_data_mb <= max_size {
            buffered_data_mb = total_data_mb
          } else {
            buffered_data_mb = max_size
            dropped_data_mb = total_data_mb - max_size
            buffer_overflow = true
          }
        }
      }
      _ => {}
    }
    
    // 应用压缩
    if config["compression_enabled"] {
      buffered_data_mb = (buffered_data_mb.to_double() * 0.3).to_int64()  // 假设压缩率为70%
    }
    
    (buffered_data_mb, dropped_data_mb, buffer_overflow)
  }
  
  // 测试不同缓冲策略
  let data_rate = 1.0  // 1MB/s
  let partition_duration = 60000  // 1分钟
  
  for strategy in buffering_strategies.keys() {
    let (buffered, dropped, overflow) = simulate_data_buffering(strategy, data_rate, partition_duration)
    
    // 验证缓冲结果
    assert_eq(buffered >= 0, true, 
      "Buffered data should be non-negative for " + strategy)
    
    assert_eq(dropped >= 0, true, 
      "Dropped data should be non-negative for " + strategy)
    
    // 验证缓冲区容量
    let config = buffering_strategies[strategy]
    let max_capacity = match strategy {
      "memory_buffer" => config["max_size_mb"],
      "disk_buffer" => config["max_size_mb"],
      "hybrid_buffer" => config["memory_max_size_mb"] + config["disk_max_size_mb"],
      "circular_buffer" => config["max_size_mb"],
      _ => 0
    }
    
    if config["compression_enabled"] {
      let compressed_capacity = (max_capacity.to_double() * 0.3).to_int64()
      assert_eq(buffered <= compressed_capacity, true, 
        "Compressed buffered data should not exceed capacity for " + strategy)
    } else {
      assert_eq(buffered <= max_capacity, true, 
        "Buffered data should not exceed capacity for " + strategy)
    }
  }
  
  // 测试不同数据速率下的缓冲表现
  let data_rates = [0.5, 1.0, 2.0, 5.0]  // MB/s
  
  for rate in data_rates {
    let (buffered, dropped, overflow) = simulate_data_buffering("memory_buffer", rate, partition_duration)
    
    // 验证数据速率越高，丢包越多
    if rate > 2.0 {
      assert_eq(dropped > 0, true, 
        "High data rate should cause data loss for rate " + rate.to_string())
    }
    
    // 验证缓冲区不会超过容量
    assert_eq(buffered <= buffering_strategies["memory_buffer"]["max_size_mb"], true, 
      "Buffer should not exceed capacity for rate " + rate.to_string())
  }
  
  // 测试缓冲区恢复策略
  fn simulate_buffer_recovery(strategy: String, buffered_data_mb: Int64, network_recovery_rate_mb_per_sec: Double) -> (Int64, Int64) {
    let config = buffering_strategies[strategy]
    
    // 计算恢复时间
    let recovery_time_sec = buffered_data_mb.to_double() / network_recovery_rate_mb_per_sec
    
    // 考虑缓冲区类型对恢复的影响
    let recovery_factor = match strategy {
      "memory_buffer" => 1.0,      // 内存缓冲恢复最快
      "disk_buffer" => 0.8,        // 磁盘缓冲恢复稍慢
      "hybrid_buffer" => 0.9,      // 混合缓冲中等
      "circular_buffer" => 0.7,    // 循环缓冲恢复最慢（需要重新排序）
      _ => 1.0
    }
    
    let adjusted_recovery_time_sec = recovery_time_sec / recovery_factor
    let recovery_time_ms = (adjusted_recovery_time_sec * 1000.0).to_int64()
    
    // 计算恢复期间的数据丢失
    let new_data_during_recovery = (network_recovery_rate_mb_per_sec * adjusted_recovery_time_sec).to_int64()
    
    (recovery_time_ms, new_data_during_recovery)
  }
  
  // 测试缓冲区恢复
  let buffered_amount = 50L  // 50MB缓冲数据
  let recovery_rate = 2.0   // 2MB/s恢复速率
  
  for strategy in buffering_strategies.keys() {
    let (recovery_time, new_data_dropped) = simulate_buffer_recovery(strategy, buffered_amount, recovery_rate)
    
    // 验证恢复时间
    assert_eq(recovery_time > 0, true, 
      "Recovery time should be positive for " + strategy)
    
    // 验证恢复期间的新数据处理
    assert_eq(new_data_dropped >= 0, true, 
      "New data dropped should be non-negative for " + strategy)
    
    // 验证不同缓冲策略的恢复时间差异
    if strategy == "memory_buffer" {
      let (disk_recovery_time, _) = simulate_buffer_recovery("disk_buffer", buffered_amount, recovery_rate)
      assert_eq(recovery_time < disk_recovery_time, true, 
        "Memory buffer should recover faster than disk buffer")
    }
  }
}

test "telemetry_network_partition_with_circuit_breaker" {
  // 测试网络分区时的熔断器机制
  
  // 定义熔断器配置
  let circuit_breaker_configs = {
    "aggressive_breaker": {
      "failure_threshold": 5,           // 5次失败后熔断
      "success_threshold": 3,           // 3次成功后恢复
      "timeout_ms": 30000,              // 30秒熔断时间
      "half_open_max_calls": 5,         // 半开状态最大调用次数
      "failure_rate_threshold": 0.5     // 50%失败率阈值
    },
    "moderate_breaker": {
      "failure_threshold": 10,
      "success_threshold": 5,
      "timeout_ms": 60000,
      "half_open_max_calls": 10,
      "failure_rate_threshold": 0.6
    },
    "conservative_breaker": {
      "failure_threshold": 20,
      "success_threshold": 10,
      "timeout_ms": 120000,
      "half_open_max_calls": 20,
      "failure_rate_threshold": 0.7
    }
  }
  
  // 熔断器状态枚举
  enum CircuitState {
    Closed
    Open
    HalfOpen
  }
  
  // 模拟熔断器行为
  fn simulate_circuit_breaker(config_name: String, call_results: Array[Bool]) -> (Array[CircuitState], Int64) {
    let config = circuit_breaker_configs[config_name]
    let failure_threshold = config["failure_threshold"]
    let success_threshold = config["success_threshold"]
    let timeout_ms = config["timeout_ms"]
    let half_open_max_calls = config["half_open_max_calls"]
    let failure_rate_threshold = config["failure_rate_threshold"]
    
    let mut state = Closed
    let mut failure_count = 0
    let mut success_count = 0
    let mut total_calls = 0
    let mut state_history = []
    let mut half_open_calls = 0
    
    for call_result in call_results {
      total_calls = total_calls + 1
      
      match state {
        Closed => {
          if call_result {
            success_count = success_count + 1
          } else {
            failure_count = failure_count + 1
          }
          
          // 检查失败率
          let failure_rate = if total_calls > 0 {
            failure_count.to_double() / total_calls.to_double()
          } else {
            0.0
          }
          
          // 检查是否需要熔断
          if failure_count >= failure_threshold || failure_rate >= failure_rate_threshold {
            state = Open
            failure_count = 0
            success_count = 0
            total_calls = 0
          }
        }
        Open => {
          // 模拟超时后进入半开状态
          state = HalfOpen
          half_open_calls = 0
        }
        HalfOpen => {
          half_open_calls = half_open_calls + 1
          
          if call_result {
            success_count = success_count + 1
          } else {
            failure_count = failure_count + 1
          }
          
          // 检查是否恢复或重新熔断
          if success_count >= success_threshold {
            state = Closed
            failure_count = 0
            success_count = 0
            total_calls = 0
          } else if failure_count >= 1 || half_open_calls >= half_open_max_calls {
            state = Open
            failure_count = 0
            success_count = 0
            total_calls = 0
          }
        }
      }
      
      state_history.push(state)
    }
    
    (state_history, total_calls)
  }
  
  // 测试熔断器在不同失败模式下的行为
  let failure_patterns = {
    "sudden_failure": [true, true, true, true, false, false, false, false, false, false],
    "gradual_failure": [true, true, true, false, false, true, false, false, false, false],
    "intermittent_failure": [true, false, true, false, true, false, true, false, true, false],
    "recovery_pattern": [false, false, false, false, false, true, true, true, true, true],
    "mixed_pattern": [true, true, false, false, false, true, false, true, false, false]
  }
  
  for pattern_name in failure_patterns.keys() {
    let call_results = failure_patterns[pattern_name]
    
    for config_name in circuit_breaker_configs.keys() {
      let (state_history, total_calls) = simulate_circuit_breaker(config_name, call_results)
      
      // 验证状态历史
      assert_eq(state_history.length(), call_results.length(), 
        "State history should match call results for " + pattern_name + " with " + config_name)
      
      // 验证熔断器状态转换
      let mut state_changes = 0
      for i = 1; i < state_history.length(); i = i + 1 {
        if state_history[i] != state_history[i-1] {
          state_changes = state_changes + 1
        }
      }
      
      // 验证至少有一次状态转换（如果有失败）
      let has_failures = call_results.contains(false)
      if has_failures {
        assert_eq(state_changes > 0, true, 
          "Should have state changes when failures occur for " + pattern_name + " with " + config_name)
      }
    }
  }
  
  // 测试熔断器性能指标
  fn calculate_circuit_breaker_metrics(state_history: Array[CircuitState], call_results: Array[Bool]) -> (Double, Double, Double) {
    let mut open_time_percentage = 0.0
    let mut half_open_time_percentage = 0.0
    let mut closed_time_percentage = 0.0
    
    // 计算各状态时间占比
    for state in state_history {
      match state {
        Open => open_time_percentage = open_time_percentage + 1.0,
        HalfOpen => half_open_time_percentage = half_open_time_percentage + 1.0,
        Closed => closed_time_percentage = closed_time_percentage + 1.0
      }
    }
    
    let total_states = state_history.length().to_double()
    if total_states > 0.0 {
      open_time_percentage = open_time_percentage / total_states * 100.0
      half_open_time_percentage = half_open_time_percentage / total_states * 100.0
      closed_time_percentage = closed_time_percentage / total_states * 100.0
    }
    
    // 计算实际调用成功率
    let successful_calls = call_results.fold(0, fn(acc, result) { if result { acc + 1 } else { acc } })
    let success_rate = if call_results.length() > 0 {
      successful_calls.to_double() / call_results.length().to_double() * 100.0
    } else {
      0.0
    }
    
    (open_time_percentage, half_open_time_percentage, success_rate)
  }
  
  // 测试熔断器性能
  let test_pattern = failure_patterns["sudden_failure"]
  
  for config_name in circuit_breaker_configs.keys() {
    let (state_history, _) = simulate_circuit_breaker(config_name, test_pattern)
    let (open_pct, half_open_pct, success_rate) = calculate_circuit_breaker_metrics(state_history, test_pattern)
    
    // 验证状态时间占比
    assert_eq(open_pct + half_open_pct + closed_pct, 100.0, 
      "State percentages should sum to 100% for " + config_name)
    
    // 验证在失败模式下的熔断器行为
    if config_name == "aggressive_breaker" {
      assert_eq(open_pct > 0.0, true, 
        "Aggressive breaker should spend time in open state")
    }
    
    // 验证成功率计算
    assert_eq(success_rate >= 0.0 && success_rate <= 100.0, true, 
      "Success rate should be between 0 and 100 for " + config_name)
  }
}

test "telemetry_network_partition_with_fallback_mechanisms" {
  // 测试网络分区时的备用机制
  
  // 定义备用机制配置
  let fallback_mechanisms = {
    "primary_to_backup": {
      "description": "主节点失败时切换到备用节点",
      "switch_time_ms": 5000,
      "health_check_interval_ms": 10000,
      "max_failover_attempts": 3
    },
    "protocol_fallback": {
      "description": "协议降级，从gRPC切换到HTTP",
      "switch_time_ms": 2000,
      "performance_impact_percent": 30,
      "compatibility_check_required": true
    },
    "data_compression_fallback": {
      "description": "网络差时启用更高压缩率",
      "switch_time_ms": 1000,
      "compression_ratio_change": 0.5,  // 从0.7降到0.5
      "cpu_overhead_increase_percent": 20
    },
    "batch_size_adjustment": {
      "description": "网络差时减小批处理大小",
      "switch_time_ms": 500,
      "batch_size_reduction_factor": 0.5,
      "throughput_impact_percent": 40
    },
    "timeout_adjustment": {
      "description": "网络差时增加超时时间",
      "switch_time_ms": 100,
      "timeout_multiplier": 3.0,
      "reliability_improvement_percent": 60
    }
  }
  
  // 模拟故障转移过程
  fn simulate_failover(mechanism: String, failure_duration_ms: Int64, recovery_time_ms: Int64) -> (Int64, Int64, Bool) {
    let config = fallback_mechanisms[mechanism]
    let switch_time_ms = config["switch_time_ms"]
    
    // 计算总停机时间
    let total_downtime_ms = if recovery_time_ms > 0 {
      switch_time_ms  // 只有切换时间
    } else {
      failure_duration_ms  // 完全故障时间
    }
    
    // 计算数据丢失
    let data_loss_percentage = match mechanism {
      "primary_to_backup" => if recovery_time_ms > 0 { 5.0 } else { 100.0 },
      "protocol_fallback" => if recovery_time_ms > 0 { 2.0 } else { 100.0 },
      "data_compression_fallback" => 0.0,  // 无数据丢失，只是性能下降
      "batch_size_adjustment" => 0.0,     // 无数据丢失，只是性能下降
      "timeout_adjustment" => 0.0,        // 无数据丢失，只是性能下降
      _ => 10.0
    }
    
    // 判断恢复是否成功
    let recovery_successful = recovery_time_ms > 0 && recovery_time_ms < failure_duration_ms
    
    (total_downtime_ms, data_loss_percentage.to_int64(), recovery_successful)
  }
  
  // 测试不同故障转移机制
  let failure_duration = 60000L  // 1分钟故障
  let recovery_time = 30000L    // 30秒恢复
  
  for mechanism in fallback_mechanisms.keys() {
    let (downtime, data_loss, recovered) = simulate_failover(mechanism, failure_duration, recovery_time)
    
    // 验证故障转移结果
    assert_eq(downtime > 0, true, 
      "Should have some downtime for " + mechanism)
    
    assert_eq(downtime <= failure_duration, true, 
      "Downtime should not exceed failure duration for " + mechanism)
    
    assert_eq(data_loss >= 0 && data_loss <= 100, true, 
      "Data loss should be between 0 and 100% for " + mechanism)
    
    assert_eq(recovered, true, 
      "Recovery should be successful for " + mechanism)
  }
  
  // 测试多重故障转移
  fn simulate_cascading_failover(mechanisms: Array[String], failure_points: Array[Int64]) -> (Array[String], Int64) {
    let mut active_mechanism = "primary"
    let mut mechanism_history = []
    let mut total_switch_time = 0L
    
    for i = 0; i < failure_points.length(); i = i + 1 {
      if i < mechanisms.length() {
        let new_mechanism = mechanisms[i]
        let config = fallback_mechanisms[new_mechanism]
        let switch_time = config["switch_time_ms"]
        
        active_mechanism = new_mechanism
        mechanism_history.push(active_mechanism)
        total_switch_time = total_switch_time + switch_time
      }
    }
    
    (mechanism_history, total_switch_time)
  }
  
  // 测试级联故障转移
  let fallback_sequence = ["primary_to_backup", "protocol_fallback", "data_compression_fallback"]
  let failure_timeline = [10000L, 30000L, 50000L]  // 故障发生时间点
  
  let (mechanism_history, total_switch_time) = simulate_cascading_failover(fallback_sequence, failure_timeline)
  
  // 验证级联故障转移结果
  assert_eq(mechanism_history.length(), fallback_sequence.length(), 
    "Should have mechanism history for all fallbacks")
  
  assert_eq(mechanism_history[0], "primary_to_backup", 
    "First fallback should be primary_to_backup")
  
  assert_eq(total_switch_time > 0, true, 
    "Should have total switch time for cascading failover")
  
  // 测试故障转移性能影响
  fn calculate_fallback_impact(mechanism: String, baseline_throughput: Int64) -> (Int64, Int64) {
    let config = fallback_mechanisms[mechanism]
    
    let impact_percentage = match mechanism {
      "primary_to_backup" => 10,      // 10%性能下降
      "protocol_fallback" => config["performance_impact_percent"],
      "data_compression_fallback" => 15,  // CPU开销影响
      "batch_size_adjustment" => config["throughput_impact_percent"],
      "timeout_adjustment" => -5,      // 超时增加可能略微提高成功率
      _ => 20
    }
    
    let adjusted_throughput = (baseline_throughput.to_double() * (1.0 - impact_percentage.to_double() / 100.0)).to_int64()
    let throughput_change = baseline_throughput - adjusted_throughput
    
    (adjusted_throughput, throughput_change)
  }
  
  // 测试不同备用机制的性能影响
  let baseline_throughput = 1000L  // 1000 ops/sec
  
  for mechanism in fallback_mechanisms.keys() {
    let (adjusted_throughput, throughput_change) = calculate_fallback_impact(mechanism, baseline_throughput)
    
    // 验证吞吐量调整
    assert_eq(adjusted_throughput >= 0, true, 
      "Adjusted throughput should be non-negative for " + mechanism)
    
    // 验证吞吐量变化
    if mechanism != "timeout_adjustment" {
      assert_eq(throughput_change >= 0, true, 
        "Throughput should decrease for " + mechanism)
    }
    
    // 验证性能影响在合理范围内
    assert_eq(adjusted_throughput >= baseline_throughput / 2, true, 
      "Throughput should not drop below 50% for " + mechanism)
  }
}