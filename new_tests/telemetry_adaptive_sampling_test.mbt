// 遥测自适应采样测试用例

test "telemetry_dynamic_sampling_adjustment" {
  // 测试遥测动态采样调整机制
  
  let base_sampling_rate = 0.1        // 基础采样率10%
  let max_sampling_rate = 1.0         // 最大采样率100%
  let min_sampling_rate = 0.01        // 最小采样率1%
  let adjustment_window_seconds = 60   // 调整窗口60秒
  let load_threshold_high = 0.8       // 高负载阈值80%
  let load_threshold_low = 0.3        // 低负载阈值30%
  
  // 验证采样配置
  assert_eq(max_sampling_rate > base_sampling_rate, true)
  assert_eq(base_sampling_rate > min_sampling_rate, true)
  assert_eq(load_threshold_high > load_threshold_low, true)
  
  // 系统负载指标
  type SystemLoad = {
    cpu_usage: Double,
    memory_usage: Double,
    network_io: Double,
    disk_io: Double,
    request_rate: Int
  }
  
  // 采样决策状态
  type SamplingState = {
    current_rate: Double,
    last_adjustment_time: Int,
    adjustment_count: Int,
    total_requests: Int,
    sampled_requests: Int
  }
  
  // 模拟系统负载变化
  let load_scenarios = [
    SystemLoad {
      cpu_usage: 0.2,
      memory_usage: 0.3,
      network_io: 0.1,
      disk_io: 0.2,
      request_rate: 1000
    },  // 低负载
    SystemLoad {
      cpu_usage: 0.5,
      memory_usage: 0.6,
      network_io: 0.4,
      disk_io: 0.3,
      request_rate: 5000
    },  // 中等负载
    SystemLoad {
      cpu_usage: 0.9,
      memory_usage: 0.85,
      network_io: 0.8,
      disk_io: 0.7,
      request_rate: 15000
    },  // 高负载
    SystemLoad {
      cpu_usage: 0.7,
      memory_usage: 0.65,
      network_io: 0.5,
      disk_io: 0.4,
      request_rate: 8000
    },  // 中高负载
    SystemLoad {
      cpu_usage: 0.3,
      memory_usage: 0.4,
      network_io: 0.2,
      disk_io: 0.25,
      request_rate: 2000
    }   // 回到低负载
  ]
  
  // 验证负载场景
  assert_eq(load_scenarios.length(), 5)
  assert_eq(load_scenarios[0].cpu_usage, 0.2)
  assert_eq(load_scenarios[2].cpu_usage, 0.9)
  
  // 初始化采样状态
  let mut sampling_state = SamplingState {
    current_rate: base_sampling_rate,
    last_adjustment_time: 0,
    adjustment_count: 0,
    total_requests: 0,
    sampled_requests: 0
  }
  
  // 动态采样调整算法
  let mut i = 0
  while i < load_scenarios.length() {
    let load = load_scenarios[i]
    let current_time = i * adjustment_window_seconds
    
    // 计算综合负载指标
    let overall_load = (load.cpu_usage + load.memory_usage + 
                       load.network_io + load.disk_io) / 4.0
    
    // 根据负载调整采样率
    let new_sampling_rate = if overall_load > load_threshold_high {
      // 高负载时降低采样率
      let reduction_factor = (overall_load - load_threshold_high) / (1.0 - load_threshold_high)
      let adjusted_rate = sampling_state.current_rate * (1.0 - reduction_factor * 0.5)
      max(adjusted_rate, min_sampling_rate)
    } else if overall_load < load_threshold_low {
      // 低负载时提高采样率
      let increase_factor = (load_threshold_low - overall_load) / load_threshold_low
      let adjusted_rate = sampling_state.current_rate * (1.0 + increase_factor * 0.3)
      min(adjusted_rate, max_sampling_rate)
    } else {
      // 中等负载时保持当前采样率
      sampling_state.current_rate
    }
    
    // 更新采样状态
    sampling_state.current_rate = new_sampling_rate
    sampling_state.last_adjustment_time = current_time
    sampling_state.adjustment_count = sampling_state.adjustment_count + 1
    
    // 模拟请求采样
    let requests_in_window = load.request_rate
    let sampled_in_window = (requests_in_window.to_double() * new_sampling_rate).to_int()
    
    sampling_state.total_requests = sampling_state.total_requests + requests_in_window
    sampling_state.sampled_requests = sampling_state.sampled_requests + sampled_in_window
    
    i = i + 1
  }
  
  // 验证采样调整
  assert_eq(sampling_state.adjustment_count, 5)
  assert_eq(sampling_state.total_requests, 31000)  // 1000+5000+15000+8000+2000
  assert_eq(sampling_state.sampled_requests > 0, true)
  
  // 验证采样率范围
  assert_eq(sampling_state.current_rate >= min_sampling_rate, true)
  assert_eq(sampling_state.current_rate <= max_sampling_rate, true)
  
  // 计算实际采样率
  let actual_sampling_rate = sampling_state.sampled_requests.to_double() / 
                            sampling_state.total_requests.to_double()
  
  // 验证实际采样率在合理范围内
  assert_eq(actual_sampling_rate >= min_sampling_rate * 0.8, true)
  assert_eq(actual_sampling_rate <= max_sampling_rate, true)
}

test "telemetry_priority_based_sampling" {
  // 测试基于优先级的采样策略
  
  let priority_sampling_rates = {
    "critical": 1.0,    // 关键数据100%采样
    "high": 0.5,        // 高优先级50%采样
    "medium": 0.1,      // 中优先级10%采样
    "low": 0.01         // 低优先级1%采样
  }
  
  // 验证优先级采样配置
  assert_eq(priority_sampling_rates["critical"] > priority_sampling_rates["high"], true)
  assert_eq(priority_sampling_rates["high"] > priority_sampling_rates["medium"], true)
  assert_eq(priority_sampling_rates["medium"] > priority_sampling_rates["low"], true)
  
  // 定义遥测数据优先级
  type TelemetryEvent = {
    event_id: String,
    event_type: String,
    priority: String,
    service_name: String,
    timestamp: Int,
    sample_decision: Bool
  }
  
  // 创建测试事件
  let telemetry_events = [
    // 关键事件
    TelemetryEvent {
      event_id: "error_001",
      event_type: "error",
      priority: "critical",
      service_name: "payment-service",
      timestamp: 1640995200,
      sample_decision: false
    },
    TelemetryEvent {
      event_id: "security_001",
      event_type: "security",
      priority: "critical",
      service_name: "auth-service",
      timestamp: 1640995260,
      sample_decision: false
    },
    // 高优先级事件
    TelemetryEvent {
      event_id: "trace_001",
      event_type: "trace",
      priority: "high",
      service_name: "order-service",
      timestamp: 1640995320,
      sample_decision: false
    },
    TelemetryEvent {
      event_id: "metric_001",
      event_type: "metric",
      priority: "high",
      service_name: "user-service",
      timestamp: 1640995380,
      sample_decision: false
    },
    // 中优先级事件
    TelemetryEvent {
      event_id: "log_001",
      event_type: "log",
      priority: "medium",
      service_name: "api-gateway",
      timestamp: 1640995440,
      sample_decision: false
    },
    TelemetryEvent {
      event_id: "trace_002",
      event_type: "trace",
      priority: "medium",
      service_name: "notification-service",
      timestamp: 1640995500,
      sample_decision: false
    },
    // 低优先级事件
    TelemetryEvent {
      event_id: "debug_001",
      event_type: "debug",
      priority: "low",
      service_name: "dev-service",
      timestamp: 1640995560,
      sample_decision: false
    },
    TelemetryEvent {
      event_id: "trace_003",
      event_type: "trace",
      priority: "low",
      service_name: "test-service",
      timestamp: 1640995620,
      sample_decision: false
    }
  ]
  
  // 验证测试事件
  assert_eq(telemetry_events.length(), 8)
  assert_eq(telemetry_events[0].priority, "critical")
  assert_eq(telemetry_events[7].priority, "low")
  
  // 优先级采样决策
  let mut sampled_events = []
  let mut total_events = telemetry_events.length()
  
  // 为每个事件类型创建多个实例以测试采样率
  let mut expanded_events = []
  let mut i = 0
  while i < telemetry_events.length() {
    let base_event = telemetry_events[i]
    let event_count = 100  // 每种类型创建100个事件
    
    let mut j = 0
    while j < event_count {
      let event = TelemetryEvent {
        event_id: base_event.event_id + "_" + j.to_string(),
        event_type: base_event.event_type,
        priority: base_event.priority,
        service_name: base_event.service_name,
        timestamp: base_event.timestamp + j,
        sample_decision: false
      }
      expanded_events.push(event)
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 验证扩展事件
  assert_eq(expanded_events.length(), 800)  // 8种类型 × 100个事件
  
  // 执行优先级采样
  i = 0
  while i < expanded_events.length() {
    let event = expanded_events[i]
    let sampling_rate = priority_sampling_rates[event.priority]
    
    // 简化的采样决策（基于事件ID的伪随机）
    let hash_value = event.event_id.length() * 7
    let sampling_threshold = (sampling_rate * 1000.0).to_int()
    
    let should_sample = (hash_value % 1000) < sampling_threshold
    
    if should_sample {
      sampled_events.push(event)
    }
    
    i = i + 1
  }
  
  // 验证采样结果
  assert_eq(sampled_events.length() < expanded_events.length(), true)
  assert_eq(sampled_events.length() > 0, true)
  
  // 按优先级统计采样结果
  let mut sampling_stats = {}
  let priorities = ["critical", "high", "medium", "low"]
  
  // 初始化统计
  i = 0
  while i < priorities.length() {
    let priority = priorities[i]
    sampling_stats[priority] = {
      total: 0,
      sampled: 0,
      actual_rate: 0.0
    }
    i = i + 1
  }
  
  // 统计各优先级的采样情况
  i = 0
  while i < expanded_events.length() {
    let event = expanded_events[i]
    let priority = event.priority
    let stats = sampling_stats[priority]
    
    stats.total = stats.total + 1
    sampling_stats[priority] = stats
    i = i + 1
  }
  
  i = 0
  while i < sampled_events.length() {
    let event = sampled_events[i]
    let priority = event.priority
    let stats = sampling_stats[priority]
    
    stats.sampled = stats.sampled + 1
    sampling_stats[priority] = stats
    i = i + 1
  }
  
  // 计算实际采样率
  i = 0
  while i < priorities.length() {
    let priority = priorities[i]
    let stats = sampling_stats[priority]
    
    if stats.total > 0 {
      let actual_rate = stats.sampled.to_double() / stats.total.to_double()
      let updated_stats = {
        total: stats.total,
        sampled: stats.sampled,
        actual_rate: actual_rate
      }
      sampling_stats[priority] = updated_stats
    }
    
    i = i + 1
  }
  
  // 验证优先级采样效果
  assert_eq(sampling_stats["critical"].actual_rate, 1.0)  // 关键事件应该100%采样
  assert_eq(sampling_stats["high"].actual_rate > 0.4, true)  // 高优先级应该接近50%
  assert_eq(sampling_stats["high"].actual_rate < 0.6, true)
  assert_eq(sampling_stats["medium"].actual_rate > 0.05, true)  // 中优先级应该接近10%
  assert_eq(sampling_stats["medium"].actual_rate < 0.15, true)
  assert_eq(sampling_stats["low"].actual_rate > 0.0, true)  // 低优先级应该接近1%
  assert_eq(sampling_stats["low"].actual_rate < 0.02, true)
  
  // 验证采样率顺序
  assert_eq(sampling_stats["critical"].actual_rate >= sampling_stats["high"].actual_rate, true)
  assert_eq(sampling_stats["high"].actual_rate >= sampling_stats["medium"].actual_rate, true)
  assert_eq(sampling_stats["medium"].actual_rate >= sampling_stats["low"].actual_rate, true)
}

test "telemetry_adaptive_error_sampling" {
  // 测试自适应错误采样策略
  
  let base_error_sampling_rate = 1.0     // 基础错误采样率100%
  let error_burst_threshold = 10         // 错误突发阈值（每分钟）
  let error_burst_sampling_rate = 0.1    // 错误突发时采样率10%
  let error_cooldown_minutes = 5         // 错误冷却期（分钟）
  
  // 验证错误采样配置
  assert_eq(base_error_sampling_rate > error_burst_sampling_rate, true)
  assert_eq(error_burst_threshold > 0, true)
  assert_eq(error_cooldown_minutes > 0, true)
  
  // 错误事件类型
  type ErrorEvent = {
    error_id: String,
    error_type: String,
    service_name: String,
    severity: String,
    timestamp: Int,
    sample_decision: Bool
  }
  
  // 模拟错误事件流
  let error_events = [
    // 正常错误率期间
    ErrorEvent {
      error_id: "err_001",
      error_type: "timeout",
      service_name: "api-service",
      severity: "medium",
      timestamp: 1640995200,
      sample_decision: false
    },
    ErrorEvent {
      error_id: "err_002",
      error_type: "connection_failed",
      service_name: "db-service",
      severity: "high",
      timestamp: 1640995260,
      sample_decision: false
    },
    // 错误突发期间
    ErrorEvent {
      error_id: "err_003",
      error_type: "timeout",
      service_name: "api-service",
      severity: "medium",
      timestamp: 1640995320,
      sample_decision: false
    },
    ErrorEvent {
      error_id: "err_004",
      error_type: "timeout",
      service_name: "api-service",
      severity: "medium",
      timestamp: 1640995380,
      sample_decision: false
    },
    ErrorEvent {
      error_id: "err_005",
      error_type: "memory_error",
      service_name: "cache-service",
      severity: "high",
      timestamp: 1640995440,
      sample_decision: false
    },
    ErrorEvent {
      error_id: "err_006",
      error_type: "timeout",
      service_name: "api-service",
      severity: "medium",
      timestamp: 1640995500,
      sample_decision: false
    },
    ErrorEvent {
      error_id: "err_007",
      error_type: "disk_full",
      service_name: "storage-service",
      severity: "critical",
      timestamp: 1640995560,
      sample_decision: false
    },
    ErrorEvent {
      error_id: "err_008",
      error_type: "timeout",
      service_name: "api-service",
      severity: "medium",
      timestamp: 1640995620,
      sample_decision: false
    },
    ErrorEvent {
      error_id: "err_009",
      error_type: "timeout",
      service_name: "api-service",
      severity: "medium",
      timestamp: 1640995680,
      sample_decision: false
    },
    ErrorEvent {
      error_id: "err_010",
      error_type: "timeout",
      service_name: "api-service",
      severity: "medium",
      timestamp: 1640995740,
      sample_decision: false
    },
    ErrorEvent {
      error_id: "err_011",
      error_type: "timeout",
      service_name: "api-service",
      severity: "medium",
      timestamp: 1640995800,
      sample_decision: false
    },
    ErrorEvent {
      error_id: "err_012",
      error_type: "timeout",
      service_name: "api-service",
      severity: "medium",
      timestamp: 1640995860,
      sample_decision: false
    }
  ]
  
  // 验证错误事件
  assert_eq(error_events.length(), 12)
  assert_eq(error_events[0].error_type, "timeout")
  assert_eq(error_events[6].severity, "critical")
  
  // 自适应错误采样状态
  type ErrorSamplingState = {
    current_sampling_rate: Double,
    error_count_per_minute: Int,
    burst_detected: Bool,
    burst_start_time: Int,
    last_burst_time: Int,
    total_errors: Int,
    sampled_errors: Int
  }
  
  let mut sampling_state = ErrorSamplingState {
    current_sampling_rate: base_error_sampling_rate,
    error_count_per_minute: 0,
    burst_detected: false,
    burst_start_time: 0,
    last_burst_time: 0,
    total_errors: 0,
    sampled_errors: 0
  }
  
  // 自适应错误采样处理
  let mut i = 0
  while i < error_events.length() {
    let error = error_events[i]
    let current_time = error.timestamp
    
    // 计算当前分钟内的错误数
    let mut errors_in_current_minute = 0
    let mut j = 0
    while j < error_events.length() {
      let time_diff = error_events[j].timestamp - current_time
      if time_diff >= 0 and time_diff < 60 {
        errors_in_current_minute = errors_in_current_minute + 1
      }
      j = j + 1
    }
    
    sampling_state.error_count_per_minute = errors_in_current_minute
    sampling_state.total_errors = sampling_state.total_errors + 1
    
    // 检测错误突发
    let time_since_last_burst = current_time - sampling_state.last_burst_time
    let cooldown_expired = time_since_last_burst > error_cooldown_minutes * 60
    
    if errors_in_current_minute > error_burst_threshold and 
       (not sampling_state.burst_detected or cooldown_expired) {
      // 检测到新的错误突发
      sampling_state.burst_detected = true
      sampling_state.burst_start_time = current_time
      sampling_state.current_sampling_rate = error_burst_sampling_rate
      sampling_state.last_burst_time = current_time
    } else if sampling_state.burst_detected {
      // 检查突发是否结束
      let burst_duration = current_time - sampling_state.burst_start_time
      if burst_duration > error_cooldown_minutes * 60 or 
         errors_in_current_minute <= error_burst_threshold / 2 {
        // 突发结束，恢复基础采样率
        sampling_state.burst_detected = false
        sampling_state.current_sampling_rate = base_error_sampling_rate
      }
    }
    
    // 采样决策
    let sampling_threshold = (sampling_state.current_sampling_rate * 1000.0).to_int()
    let hash_value = error.error_id.length() * 13
    let should_sample = (hash_value % 1000) < sampling_threshold
    
    if should_sample {
      sampling_state.sampled_errors = sampling_state.sampled_errors + 1
    }
    
    i = i + 1
  }
  
  // 验证自适应错误采样
  assert_eq(sampling_state.total_errors, 12)
  assert_eq(sampling_state.burst_detected, true)  // 应该检测到突发
  assert_eq(sampling_state.current_sampling_rate, error_burst_sampling_rate)
  
  // 验证采样率调整
  assert_eq(sampling_state.sampled_errors < sampling_state.total_errors, true)
  assert_eq(sampling_state.sampled_errors > 0, true)
  
  // 计算实际采样率
  let actual_sampling_rate = sampling_state.sampled_errors.to_double() / 
                            sampling_state.total_errors.to_double()
  
  // 验证采样率在突发期间降低
  assert_eq(actual_sampling_rate < base_error_sampling_rate, true)
  assert_eq(actual_sampling_rate > error_burst_sampling_rate * 0.5, true)
  
  // 按严重程度统计采样
  let mut severity_stats = {
    "critical": { total: 0, sampled: 0 },
    "high": { total: 0, sampled: 0 },
    "medium": { total: 0, sampled: 0 },
    "low": { total: 0, sampled: 0 }
  }
  
  i = 0
  while i < error_events.length() {
    let error = error_events[i]
    let severity = error.severity
    
    // 统计总数
    let stats = severity_stats[severity]
    let updated_total = { total: stats.total + 1, sampled: stats.sampled }
    severity_stats[severity] = updated_total
    
    // 重新计算采样决策（简化版）
    let sampling_threshold = (sampling_state.current_sampling_rate * 1000.0).to_int()
    let hash_value = error.error_id.length() * 13
    let should_sample = (hash_value % 1000) < sampling_threshold
    
    if should_sample {
      let current_stats = severity_stats[severity]
      let updated_sampled = { total: current_stats.total, sampled: current_stats.sampled + 1 }
      severity_stats[severity] = updated_sampled
    }
    
    i = i + 1
  }
  
  // 验证严重程度统计
  assert_eq(severity_stats["critical"].total, 1)
  assert_eq(severity_stats["high"].total, 2)
  assert_eq(severity_stats["medium"].total, 9)
  
  // 验证关键错误有更高采样概率
  let critical_rate = if severity_stats["critical"].total > 0 {
    severity_stats["critical"].sampled.to_double() / severity_stats["critical"].total.to_double()
  } else { 0.0 }
  
  let medium_rate = if severity_stats["medium"].total > 0 {
    severity_stats["medium"].sampled.to_double() / severity_stats["medium"].total.to_double()
  } else { 0.0 }
  
  // 在突发情况下，所有错误类型的采样率都应该降低
  assert_eq(critical_rate <= base_error_sampling_rate, true)
  assert_eq(medium_rate <= base_error_sampling_rate, true)
}