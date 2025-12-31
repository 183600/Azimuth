// 遥测资源限制测试用例

test "telemetry_memory_usage_monitoring" {
  // 测试遥测内存使用监控
  
  let memory_limit_mb = 512  // 内存限制512MB
  let warning_threshold = 0.8  // 80%警告阈值
  let critical_threshold = 0.95  // 95%严重阈值
  let monitoring_interval_seconds = 10
  
  // 验证内存监控配置
  assert_eq(memory_limit_mb, 512)
  assert_eq(warning_threshold, 0.8)
  assert_eq(critical_threshold, 0.95)
  assert_eq(monitoring_interval_seconds, 10)
  
  // 模拟内存使用情况
  let memory_usage_samples = [
    {"timestamp": 1703123450, "used_mb": 256, "components": {"buffer": 128, "cache": 64, "processing": 64}},
    {"timestamp": 1703123460, "used_mb": 384, "components": {"buffer": 192, "cache": 96, "processing": 96}},
    {"timestamp": 1703123470, "used_mb": 448, "components": {"buffer": 224, "cache": 112, "processing": 112}},
    {"timestamp": 1703123480, "used_mb": 480, "components": {"buffer": 240, "cache": 120, "processing": 120}},
    {"timestamp": 1703123490, "used_mb": 460, "components": {"buffer": 230, "cache": 115, "processing": 115}}
  ]
  
  // 验证内存使用样本
  assert_eq(memory_usage_samples.length(), 5)
  assert_eq(memory_usage_samples[0].used_mb, 256)
  assert_eq(memory_usage_samples[3].used_mb, 480)  // 接近限制
  
  // 分析内存使用模式
  let mut memory_analysis = []
  let mut i = 0
  
  while i < memory_usage_samples.length() {
    let sample = memory_usage_samples[i]
    let usage_percentage = sample.used_mb.to_float() / memory_limit_mb.to_float()
    
    let mut alert_level = "normal"
    if usage_percentage >= critical_threshold {
      alert_level = "critical"
    } else if usage_percentage >= warning_threshold {
      alert_level = "warning"
    }
    
    // 分析组件使用分布
    let components = sample.components
    let buffer_usage = components.buffer.to_float() / sample.used_mb.to_float()
    let cache_usage = components.cache.to_float() / sample.used_mb.to_float()
    let processing_usage = components.processing.to_float() / sample.used_mb.to_float()
    
    memory_analysis.push({
      "timestamp": sample.timestamp,
      "used_mb": sample.used_mb,
      "usage_percentage": usage_percentage,
      "alert_level": alert_level,
      "buffer_percentage": buffer_usage,
      "cache_percentage": cache_usage,
      "processing_percentage": processing_usage
    })
    
    i = i + 1
  }
  
  // 验证内存分析结果
  assert_eq(memory_analysis.length(), 5)
  
  // 验证警告级别检测
  assert_eq(memory_analysis[0].alert_level, "normal")   // 256/512 = 50%
  assert_eq(memory_analysis[1].alert_level, "normal")   // 384/512 = 75%
  assert_eq(memory_analysis[2].alert_level, "warning")  // 448/512 = 87.5%
  assert_eq(memory_analysis[3].alert_level, "critical") // 480/512 = 93.75%
  assert_eq(memory_analysis[4].alert_level, "warning")  // 460/512 = 89.8%
  
  // 验证组件分布分析
  assert_eq(memory_analysis[0].buffer_percentage, 0.5)   // 128/256
  assert_eq(memory_analysis[0].cache_percentage, 0.25)   // 64/256
  assert_eq(memory_analysis[0].processing_percentage, 0.25) // 64/256
  
  // 计算内存增长趋势
  let mut growth_rates = []
  i = 1
  while i < memory_analysis.length() {
    let current_usage = memory_analysis[i].used_mb
    let previous_usage = memory_analysis[i-1].used_mb
    let growth_rate = ((current_usage - previous_usage) * 100) / previous_usage
    growth_rates.push(growth_rate)
    i = i + 1
  }
  
  // 验证增长率计算
  assert_eq(growth_rates.length(), 4)
  assert_eq(growth_rates[0], 50)   // (384-256)*100/256 = 50%
  assert_eq(growth_rates[1], 16)   // (448-384)*100/384 ≈ 16.7%
  assert_eq(growth_rates[2], 7)    // (480-448)*100/448 ≈ 7.1%
  assert_eq(growth_rates[3], -4)   // (460-480)*100/480 = -4.2%
  
  // 预测内存耗尽时间
  let avg_growth_rate = (50 + 16 + 7 - 4) / 4  // 简化平均值
  let current_usage = memory_usage_samples[memory_usage_samples.length() - 1].used_mb
  let remaining_capacity = memory_limit_mb - current_usage
  
  let time_to_exhaustion = 
    if avg_growth_rate > 0 { remaining_capacity / (avg_growth_rate * current_usage / 100) }
    else { -1 }  // 负值表示不会耗尽
  
  // 验证预测结果
  assert_eq(avg_growth_rate, 17)  // (50+16+7-4)/4 = 69/4 = 17.25
  assert_eq(remaining_capacity, 52)  // 512-460
  assert_eq(time_to_exhaustion > 0, true)
  
  // 测试内存优化策略
  let memory_optimization_strategies = [
    {"strategy": "buffer_size_reduction", "potential_savings_mb": 64},
    {"strategy": "cache_eviction_aggressive", "potential_savings_mb": 32},
    {"strategy": "compression_enable", "potential_savings_mb": 48},
    {"strategy": "batch_size_optimization", "potential_savings_mb": 16}
  ]
  
  // 验证优化策略
  assert_eq(memory_optimization_strategies.length(), 4)
  
  // 计算总优化潜力
  let mut total_potential_savings = 0
  i = 0
  while i < memory_optimization_strategies.length() {
    total_potential_savings = total_potential_savings + memory_optimization_strategies[i].potential_savings_mb
    i = i + 1
  }
  
  assert_eq(total_potential_savings, 160)  // 64+32+48+16
  assert_eq(total_potential_savings > remaining_capacity, true)  // 优化潜力超过剩余容量
}

test "telemetry_cpu_usage_throttling" {
  // 测试遥测CPU使用限流
  
  let cpu_limit_percent = 80  // CPU使用限制80%
  let throttling_threshold = 0.9  // 90%开始限流
  let monitoring_window_seconds = 60  // 监控窗口60秒
  let adaptive_throttling_enabled = true
  
  // 验证CPU限流配置
  assert_eq(cpu_limit_percent, 80)
  assert_eq(throttling_threshold, 0.9)
  assert_eq(monitoring_window_seconds, 60)
  assert_eq(adaptive_throttling_enabled, true)
  
  // 模拟CPU使用情况
  let cpu_usage_samples = [
    {"timestamp": 1703123450, "cpu_percent": 45, "active_threads": 4},
    {"timestamp": 1703123455, "cpu_percent": 62, "active_threads": 6},
    {"timestamp": 1703123460, "cpu_percent": 78, "active_threads": 8},
    {"timestamp": 1703123465, "cpu_percent": 85, "active_threads": 9},
    {"timestamp": 1703123470, "cpu_percent": 92, "active_threads": 10},
    {"timestamp": 1703123475, "cpu_percent": 88, "active_threads": 9},
    {"timestamp": 1703123480, "cpu_percent": 75, "active_threads": 7}
  ]
  
  // 验证CPU使用样本
  assert_eq(cpu_usage_samples.length(), 7)
  assert_eq(cpu_usage_samples[4].cpu_percent, 92)  // 超过限制
  
  // 分析CPU使用和限流策略
  let mut throttling_decisions = []
  let mut i = 0
  
  while i < cpu_usage_samples.length() {
    let sample = cpu_usage_samples[i]
    let cpu_usage_ratio = sample.cpu_percent.to_float() / cpu_limit_percent.to_float()
    
    let mut should_throttle = false
    let mut throttle_level = 0.0
    
    if cpu_usage_ratio >= throttling_threshold {
      should_throttle = true
      throttle_level = (cpu_usage_ratio - throttling_threshold) / (1.0 - throttling_threshold)
      if throttle_level > 1.0 { throttle_level = 1.0 }
    }
    
    // 计算建议的线程数调整
    let optimal_threads = (cpu_limit_percent * sample.active_threads) / sample.cpu_percent
    let thread_adjustment = 
      if should_throttle { sample.active_threads - optimal_threads.to_int() }
      else { 0 }
    
    throttling_decisions.push({
      "timestamp": sample.timestamp,
      "cpu_percent": sample.cpu_percent,
      "cpu_ratio": cpu_usage_ratio,
      "should_throttle": should_throttle,
      "throttle_level": throttle_level,
      "current_threads": sample.active_threads,
      "optimal_threads": optimal_threads.to_int(),
      "thread_adjustment": thread_adjustment
    })
    
    i = i + 1
  }
  
  // 验证限流决策
  assert_eq(throttling_decisions.length(), 7)
  
  // 验证限流触发条件
  assert_eq(throttling_decisions[0].should_throttle, false)  // 45/80 = 56%
  assert_eq(throttling_decisions[1].should_throttle, false)  // 62/80 = 77%
  assert_eq(throttling_decisions[2].should_throttle, false)  // 78/80 = 97%
  assert_eq(throttling_decisions[3].should_throttle, true)   // 85/80 = 106%
  assert_eq(throttling_decisions[4].should_throttle, true)   // 92/80 = 115%
  assert_eq(throttling_decisions[5].should_throttle, true)   // 88/80 = 110%
  assert_eq(throttling_decisions[6].should_throttle, false)  // 75/80 = 94%
  
  // 验证限流级别计算
  assert_eq(throttling_decisions[3].throttle_level > 0.0, true)
  assert_eq(throttling_decisions[4].throttle_level > throttling_decisions[3].throttle_level, true)
  
  // 验证线程调整建议
  assert_eq(throttling_decisions[3].optimal_threads, 7)  // (80*9)/85 ≈ 8.47
  assert_eq(throttling_decisions[3].thread_adjustment, 2)  // 9-7 = 2
  
  // 计算限流效果
  let mut throttled_periods = 0
  let mut total_throttle_level = 0.0
  i = 0
  while i < throttling_decisions.length() {
    if throttling_decisions[i].should_throttle {
      throttled_periods = throttled_periods + 1
      total_throttle_level = total_throttle_level + throttling_decisions[i].throttle_level
    }
    i = i + 1
  }
  
  let avg_throttle_level = total_throttle_level / throttled_periods.to_float()
  
  // 验证限流统计
  assert_eq(throttled_periods, 3)
  assert_eq(avg_throttle_level > 0.0, true)
  
  // 测试自适应限流策略
  let adaptive_strategies = [
    {"cpu_range": "0-60", "action": "increase_throughput"},
    {"cpu_range": "60-80", "action": "maintain_current"},
    {"cpu_range": "80-90", "action": "reduce_batch_size"},
    {"cpu_range": "90-100", "action": "thread_throttling"},
    {"cpu_range": "100+", "action": "emergency_pause"}
  ]
  
  // 验证自适应策略
  assert_eq(adaptive_strategies.length(), 5)
  
  // 应用自适应策略
  i = 0
  while i < throttling_decisions.length() {
    let decision = throttling_decisions[i]
    let cpu_percent = decision.cpu_percent
    let mut strategy_action = ""
    
    if cpu_percent <= 60 {
      strategy_action = "increase_throughput"
    } else if cpu_percent <= 80 {
      strategy_action = "maintain_current"
    } else if cpu_percent <= 90 {
      strategy_action = "reduce_batch_size"
    } else if cpu_percent <= 100 {
      strategy_action = "thread_throttling"
    } else {
      strategy_action = "emergency_pause"
    }
    
    decision["adaptive_action"] = strategy_action
    i = i + 1
  }
  
  // 验证自适应策略应用
  assert_eq(throttling_decisions[0].adaptive_action, "increase_throughput")  // 45%
  assert_eq(throttling_decisions[2].adaptive_action, "maintain_current")     // 78%
  assert_eq(throttling_decisions[3].adaptive_action, "thread_throttling")    // 85%
  assert_eq(throttling_decisions[4].adaptive_action, "thread_throttling")    // 92%
}

test "telemetry_disk_space_management" {
  // 测试遥测磁盘空间管理
  
  let disk_limit_mb = 2048  // 磁盘限制2GB
  let cleanup_threshold = 0.85  // 85%清理阈值
  let retention_days = 7  // 保留7天数据
  let compression_enabled = true
  
  // 验证磁盘管理配置
  assert_eq(disk_limit_mb, 2048)
  assert_eq(cleanup_threshold, 0.85)
  assert_eq(retention_days, 7)
  assert_eq(compression_enabled, true)
  
  // 模拟磁盘使用情况
  let disk_usage_data = [
    {"timestamp": 1703123450, "used_mb": 1024, "files_count": 500, "oldest_file_days": 1},
    {"timestamp": 1703123460, "used_mb": 1536, "files_count": 750, "oldest_file_days": 2},
    {"timestamp": 1703123470, "used_mb": 1792, "files_count": 875, "oldest_file_days": 3},
    {"timestamp": 1703123480, "used_mb": 1945, "files_count": 950, "oldest_file_days": 4},
    {"timestamp": 1703123490, "used_mb": 1843, "files_count": 900, "oldest_file_days": 2}
  ]
  
  // 验证磁盘使用数据
  assert_eq(disk_usage_data.length(), 5)
  assert_eq(disk_usage_data[3].used_mb, 1945)  // 接近限制
  
  // 分析磁盘使用和清理策略
  let mut disk_management_actions = []
  let mut i = 0
  
  while i < disk_usage_data.length() {
    let data = disk_usage_data[i]
    let usage_percentage = data.used_mb.to_float() / disk_limit_mb.to_float()
    
    let mut should_cleanup = false
    let mut cleanup_priority = "low"
    
    if usage_percentage >= cleanup_threshold {
      should_cleanup = true
      if usage_percentage >= 0.95 {
        cleanup_priority = "critical"
      } else if usage_percentage >= 0.9 {
        cleanup_priority = "high"
      } else {
        cleanup_priority = "medium"
      }
    }
    
    // 计算清理策略
    let mut cleanup_strategies = []
    if should_cleanup {
      if data.oldest_file_days > retention_days {
        cleanup_strategies.push("delete_expired_files")
      }
      if compression_enabled {
        cleanup_strategies.push("compress_old_files")
      }
      if usage_percentage > 0.9 {
        cleanup_strategies.push("archive_to_cold_storage")
      }
    }
    
    // 估算清理空间
    let mut estimated_cleanup_space = 0
    if cleanup_strategies.contains("delete_expired_files") {
      estimated_cleanup_space = estimated_cleanup_space + (data.files_count / 10) * 2  // 假设10%文件过期，每个2MB
    }
    if cleanup_strategies.contains("compress_old_files") {
      estimated_cleanup_space = estimated_cleanup_space + (data.files_count / 5) * 1   // 假设20%文件可压缩，节省1MB每个
    }
    
    disk_management_actions.push({
      "timestamp": data.timestamp,
      "used_mb": data.used_mb,
      "usage_percentage": usage_percentage,
      "should_cleanup": should_cleanup,
      "cleanup_priority": cleanup_priority,
      "cleanup_strategies": cleanup_strategies,
      "estimated_cleanup_space": estimated_cleanup_space
    })
    
    i = i + 1
  }
  
  // 验证磁盘管理动作
  assert_eq(disk_management_actions.length(), 5)
  
  // 验证清理触发条件
  assert_eq(disk_management_actions[0].should_cleanup, false)  // 1024/2048 = 50%
  assert_eq(disk_management_actions[1].should_cleanup, false)  // 1536/2048 = 75%
  assert_eq(disk_management_actions[2].should_cleanup, true)   // 1792/2048 = 87.5%
  assert_eq(disk_management_actions[3].should_cleanup, true)   // 1945/2048 = 95%
  assert_eq(disk_management_actions[4].should_cleanup, true)   // 1843/2048 = 90%
  
  // 验证清理优先级
  assert_eq(disk_management_actions[2].cleanup_priority, "medium")   // 87.5%
  assert_eq(disk_management_actions[3].cleanup_priority, "critical") // 95%
  assert_eq(disk_management_actions[4].cleanup_priority, "high")     // 90%
  
  // 验证清理策略
  assert_eq(disk_management_actions[3].cleanup_strategies.length(), 3)  // 95%时启用所有策略
  assert_eq(disk_management_actions[3].cleanup_strategies.contains("delete_expired_files"), true)
  assert_eq(disk_management_actions[3].cleanup_strategies.contains("compress_old_files"), true)
  assert_eq(disk_management_actions[3].cleanup_strategies.contains("archive_to_cold_storage"), true)
  
  // 模拟清理执行
  let mut cleanup_results = []
  i = 0
  while i < disk_management_actions.length() {
    let action = disk_management_actions[i]
    
    if action.should_cleanup {
      let actual_cleanup_space = action.estimated_cleanup_space * 80 / 100  // 假设实际清理80%的估算空间
      let new_usage = action.used_mb - actual_cleanup_space
      let new_usage_percentage = new_usage.to_float() / disk_limit_mb.to_float()
      
      cleanup_results.push({
        "timestamp": action.timestamp,
        "original_usage": action.used_mb,
        "cleaned_space": actual_cleanup_space,
        "new_usage": new_usage,
        "new_usage_percentage": new_usage_percentage,
        "cleanup_successful": new_usage_percentage < cleanup_threshold
      })
    }
    
    i = i + 1
  }
  
  // 验证清理结果
  assert_eq(cleanup_results.length(), 3)  // 3次清理操作
  
  // 验证清理效果
  assert_eq(cleanup_results[0].cleanup_successful, true)   // 中等优先级清理
  assert_eq(cleanup_results[1].cleanup_successful, true)   // 严重优先级清理
  assert_eq(cleanup_results[2].cleanup_successful, true)   // 高优先级清理
  
  // 验证清理后的使用率
  assert_eq(cleanup_results[1].new_usage_percentage < cleanup_threshold, true)  // 严重清理后应该低于阈值
  
  // 测试磁盘空间预测
  let growth_rate_mb_per_hour = 50  // 每小时增长50MB
  let current_usage = disk_usage_data[disk_usage_data.length() - 1].used_mb
  let remaining_space = disk_limit_mb - current_usage
  let hours_until_full = remaining_space / growth_rate_mb_per_hour
  
  // 验证空间预测
  assert_eq(growth_rate_mb_per_hour, 50)
  assert_eq(current_usage, 1843)
  assert_eq(remaining_space, 205)  // 2048-1843
  assert_eq(hours_until_full, 4)   // 205/50 = 4.1
  
  // 测试预警策略
  let alert_strategies = [
    {"usage_percentage": 0.8, "alert_level": "warning", "action": "schedule_cleanup"},
    {"usage_percentage": 0.9, "alert_level": "critical", "action": "immediate_cleanup"},
    {"usage_percentage": 0.95, "alert_level": "emergency", "action": "emergency_cleanup"}
  ]
  
  // 验证预警策略
  assert_eq(alert_strategies.length(), 3)
  assert_eq(alert_strategies[0].alert_level, "warning")
  assert_eq(alert_strategies[2].action, "emergency_cleanup")
}

test "telemetry_network_bandwidth_throttling" {
  // 测试遥测网络带宽限流
  
  let bandwidth_limit_mbps = 100  // 带宽限制100Mbps
  let throttling_threshold = 0.8  // 80%开始限流
  let burst_allowance_seconds = 10  // 允许突发10秒
  let adaptive_qos_enabled = true
  
  // 验证带宽限流配置
  assert_eq(bandwidth_limit_mbps, 100)
  assert_eq(throttling_threshold, 0.8)
  assert_eq(burst_allowance_seconds, 10)
  assert_eq(adaptive_qos_enabled, true)
  
  // 模拟网络使用情况
  let network_usage_samples = [
    {"timestamp": 1703123450, "current_mbps": 45, "connection_count": 10, "data_priority": "normal"},
    {"timestamp": 1703123455, "current_mbps": 72, "connection_count": 15, "data_priority": "normal"},
    {"timestamp": 1703123460, "current_mbps": 85, "connection_count": 18, "data_priority": "high"},
    {"timestamp": 1703123465, "current_mbps": 95, "connection_count": 20, "data_priority": "critical"},
    {"timestamp": 1703123470, "current_mbps": 78, "connection_count": 16, "data_priority": "normal"},
    {"timestamp": 1703123475, "current_mbps": 88, "connection_count": 19, "data_priority": "high"}
  ]
  
  // 验证网络使用样本
  assert_eq(network_usage_samples.length(), 6)
  assert_eq(network_usage_samples[3].current_mbps, 95)  // 接近限制
  
  // 分析网络带宽使用和限流策略
  let mut bandwidth_throttling = []
  let mut i = 0
  
  while i < network_usage_samples.length() {
    let sample = network_usage_samples[i]
    let usage_ratio = sample.current_mbps.to_float() / bandwidth_limit_mbps.to_float()
    
    let mut should_throttle = false
    let mut throttle_level = 0.0
    let mut qos_level = "standard"
    
    if usage_ratio >= throttling_threshold {
      should_throttle = true
      throttle_level = (usage_ratio - throttling_threshold) / (1.0 - throttling_threshold)
      if throttle_level > 1.0 { throttle_level = 1.0 }
    }
    
    // 基于数据优先级调整QoS
    if sample.data_priority == "critical" {
      qos_level = "premium"
      throttle_level = throttle_level * 0.5  // 关键数据减少限流
    } else if sample.data_priority == "high" {
      qos_level = "enhanced"
      throttle_level = throttle_level * 0.7
    } else if should_throttle and usage_ratio > 0.9 {
      qos_level = "limited"
    }
    
    // 计算连接数调整
    let optimal_connections = (bandwidth_limit_mbps * sample.connection_count) / sample.current_mbps
    let connection_adjustment = 
      if should_throttle { sample.connection_count - optimal_connections.to_int() }
      else { 0 }
    
    bandwidth_throttling.push({
      "timestamp": sample.timestamp,
      "current_mbps": sample.current_mbps,
      "usage_ratio": usage_ratio,
      "should_throttle": should_throttle,
      "throttle_level": throttle_level,
      "qos_level": qos_level,
      "current_connections": sample.connection_count,
      "optimal_connections": optimal_connections.to_int(),
      "connection_adjustment": connection_adjustment,
      "data_priority": sample.data_priority
    })
    
    i = i + 1
  }
  
  // 验证带宽限流决策
  assert_eq(bandwidth_throttling.length(), 6)
  
  // 验证限流触发条件
  assert_eq(bandwidth_throttling[0].should_throttle, false)  // 45/100 = 45%
  assert_eq(bandwidth_throttling[1].should_throttle, false)  // 72/100 = 72%
  assert_eq(bandwidth_throttling[2].should_throttle, true)   // 85/100 = 85%
  assert_eq(bandwidth_throttling[3].should_throttle, true)   // 95/100 = 95%
  assert_eq(bandwidth_throttling[4].should_throttle, false)  // 78/100 = 78%
  assert_eq(bandwidth_throttling[5].should_throttle, true)   // 88/100 = 88%
  
  // 验证QoS级别调整
  assert_eq(bandwidth_throttling[2].qos_level, "enhanced")    // 高优先级数据
  assert_eq(bandwidth_throttling[3].qos_level, "premium")    // 关键优先级数据
  assert_eq(bandwidth_throttling[5].qos_level, "enhanced")    // 高优先级数据
  
  // 验证优先级对限流的影响
  assert_eq(bandwidth_throttling[3].throttle_level < bandwidth_throttling[2].throttle_level, true)
  
  // 测试突发流量处理
  let burst_detection_window = 5  // 5秒窗口
  let burst_threshold_multiplier = 1.5  // 1.5倍平均流量为突发
  
  // 检测突发流量
  let mut average_mbps = 0
  i = 0
  while i < network_usage_samples.length() {
    average_mbps = average_mbps + network_usage_samples[i].current_mbps
    i = i + 1
  }
  average_mbps = average_mbps / network_usage_samples.length()
  
  let mut burst_periods = []
  i = 0
  while i < network_usage_samples.length() {
    let sample = network_usage_samples[i]
    let is_burst = sample.current_mbps > (average_mbps * burst_threshold_multiplier)
    
    if is_burst {
      burst_periods.push({
        "timestamp": sample.timestamp,
        "burst_mbps": sample.current_mbps,
        "average_mbps": average_mbps,
        "burst_ratio": sample.current_mbps.to_float() / average_mbps.to_float()
      })
    }
    
    i = i + 1
  }
  
  // 验证突发检测
  assert_eq(average_mbps, 77)  // (45+72+85+95+78+88)/6 = 463/6 ≈ 77
  assert_eq(burst_threshold_multiplier * average_mbps, 115.5)  // 1.5 * 77
  
  // 计算带宽利用率
  let mut total_utilization = 0.0
  i = 0
  while i < bandwidth_throttling.length() {
    total_utilization = total_utilization + bandwidth_throttling[i].usage_ratio
    i = i + 1
  }
  
  let avg_utilization = total_utilization / bandwidth_throttling.length()
  
  // 验证利用率统计
  assert_eq(avg_utilization > 0.7, true)  // 平均利用率应该较高
  assert_eq(avg_utilization < 1.0, true)  // 但不应该达到100%
  
  // 测试自适应带宽分配
  let adaptive_allocation_rules = [
    {"priority": "critical", "min_bandwidth_percent": 30, "max_bandwidth_percent": 60},
    {"priority": "high", "min_bandwidth_percent": 20, "max_bandwidth_percent": 40},
    {"priority": "normal", "min_bandwidth_percent": 10, "max_bandwidth_percent": 30},
    {"priority": "low", "min_bandwidth_percent": 5, "max_bandwidth_percent": 15}
  ]
  
  // 验证自适应分配规则
  assert_eq(adaptive_allocation_rules.length(), 4)
  assert_eq(adaptive_allocation_rules[0].priority, "critical")
  assert_eq(adaptive_allocation_rules[0].min_bandwidth_percent, 30)
  
  // 应用自适应分配
  i = 0
  while i < bandwidth_throttling.length() {
    let throttling = bandwidth_throttling[i]
    let priority = throttling.data_priority
    
    let mut allocation_rule = {}
    let mut j = 0
    while j < adaptive_allocation_rules.length() {
      if adaptive_allocation_rules[j].priority == priority {
        allocation_rule = adaptive_allocation_rules[j]
        break
      }
      j = j + 1
    }
    
    let allocated_bandwidth = bandwidth_limit_mbps * allocation_rule.min_bandwidth_percent / 100
    throttling["allocated_bandwidth"] = allocated_bandwidth
    
    i = i + 1
  }
  
  // 验证自适应分配结果
  assert_eq(bandwidth_throttling[3].allocated_bandwidth, 30)  // 关键数据获得30Mbps
  assert_eq(bandwidth_throttling[2].allocated_bandwidth, 20)  // 高优先级获得20Mbps
}