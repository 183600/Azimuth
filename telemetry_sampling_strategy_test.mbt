// 遥测数据采样策略测试用例
// 测试不同的采样算法和策略，确保在性能与数据完整性之间的平衡

test "probability_based_sampling" {
  // 测试基于概率的采样
  
  let sampling_rate = 0.1  // 10% 采样率
  let total_requests = 10000
  let mut sampled_count = 0
  
  // 模拟随机采样决策
  let mut i = 0
  while i < total_requests {
    // 简化的随机采样模拟（实际应使用随机数生成器）
    let pseudo_random = (i * 17) % 100  // 简单的伪随机
    if pseudo_random < (sampling_rate * 100).to_int() {
      sampled_count = sampled_count + 1
    }
    i = i + 1
  }
  
  // 验证采样率在合理范围内
  let actual_sampling_rate = sampled_count.to_double() / total_requests.to_double()
  assert_eq(actual_sampling_rate > 0.08, true)  // 允许±2%的误差
  assert_eq(actual_sampling_rate < 0.12, true)
  
  // 测试不同采样率
  let high_sampling_rate = 0.8  // 80% 采样率
  let mut high_sampled_count = 0
  i = 0
  while i < 1000 {
    let pseudo_random = (i * 23) % 100
    if pseudo_random < (high_sampling_rate * 100).to_int() {
      high_sampled_count = high_sampled_count + 1
    }
    i = i + 1
  }
  
  let high_actual_rate = high_sampled_count.to_double() / 1000.0
  assert_eq(high_actual_rate > 0.75, true)
  assert_eq(high_actual_rate < 0.85, true)
}

test "trace_id_consistent_sampling" {
  // 测试基于trace_id的一致性采样
  
  let trace_ids = [
    "0af7651916cd43dd8448eb211c80319c",
    "4bf92f3577b34da6a3ce929d0e0e4736", 
    "a1b2c3d4e5f67890123456789abcdef0",
    "1234567890abcdef1234567890abcdef",
    "fedcba0987654321fedcba0987654321"
  ]
  
  let sampling_threshold = 1000000  // 2^20，约0.1%的采样率
  
  // 基于trace_id进行确定性采样
  let sampled_traces = []
  let mut i = 0
  while i < trace_ids.length() {
    let trace_id = trace_ids[i]
    // 取trace_id的前8位十六进制作为采样依据
    let trace_prefix = trace_id.slice(0, 8)
    
    // 转换为十进制数（简化模拟）
    let mut prefix_value = 0
    let mut j = 0
    while j < trace_prefix.length() {
      let char_value = match trace_prefix.slice(j, j + 1) {
        "a" => 10, "b" => 11, "c" => 12, "d" => 13, "e" => 14, "f" => 15,
        x => x.to_int()
      }
      prefix_value = prefix_value * 16 + char_value
      j = j + 1
    }
    
    // 确定性采样决策
    let should_sample = prefix_value < sampling_threshold
    if should_sample {
      sampled_traces.push(trace_id)
    }
    
    i = i + 1
  }
  
  // 验证一致性采样结果
  assert_eq(sampled_traces.length(), 1)  // 在这个阈值下应该只有一个trace被采样
  assert_eq(sampled_traces[0], "0af7651916cd43dd8448eb211c80319c")  // 第一个trace_id的值最小
}

test "adaptive_sampling_based_on_error_rate" {
  // 测试基于错误率的自适应采样
  
  let error_threshold = 0.05  // 5% 错误率阈值
  let base_sampling_rate = 0.01  // 基础采样率1%
  let max_sampling_rate = 0.5   // 最大采样率50%
  
  // 模拟不同错误率场景
  let error_rates = [0.01, 0.03, 0.05, 0.08, 0.15, 0.20]
  let expected_sampling_rates = [0.01, 0.03, 0.05, 0.25, 0.5, 0.5]  // 线性增长到最大值
  
  let mut i = 0
  while i < error_rates.length() {
    let current_error_rate = error_rates[i]
    let adaptive_sampling_rate = 
      if current_error_rate <= error_threshold {
        current_error_rate  // 低于阈值时，采样率等于错误率
      } else {
        // 高于阈值时，线性增长到最大值
        let excess_rate = current_error_rate - error_threshold
        let rate_increase = excess_rate * 10.0  // 放大因子
        let calculated_rate = base_sampling_rate + rate_increase
        if calculated_rate > max_sampling_rate {
          max_sampling_rate
        } else {
          calculated_rate
        }
      }
    
    // 验证自适应采样率
    assert_eq(adaptive_sampling_rate >= expected_sampling_rates[i] - 0.01, true)
    assert_eq(adaptive_sampling_rate <= expected_sampling_rates[i] + 0.01, true)
    
    i = i + 1
  }
}

test "priority_based_sampling" {
  // 测试基于优先级的采样
  
  let operation_priorities = [
    ("payment_processing", 10),
    ("user_authentication", 9),
    ("order_creation", 8),
    ("inventory_update", 7),
    ("catalog_browsing", 5),
    ("health_check", 1)
  ]
  
  let priority_threshold = 7  // 优先级阈值
  let total_operations = 1000
  
  // 统计不同优先级操作的采样情况
  let mut high_priority_sampled = 0
  let mut low_priority_sampled = 0
  let mut high_priority_total = 0
  let mut low_priority_total = 0
  
  // 模拟操作分布
  let operation_distribution = [
    ("payment_processing", 50),
    ("user_authentication", 100),
    ("order_creation", 150),
    ("inventory_update", 200),
    ("catalog_browsing", 400),
    ("health_check", 100)
  ]
  
  let mut i = 0
  while i < operation_priorities.length() {
    let operation_name = operation_priorities[i].0
    let priority = operation_priorities[i].1
    
    // 查找对应的操作数量
    let mut operation_count = 0
    let mut j = 0
    while j < operation_distribution.length() {
      if operation_distribution[j].0 == operation_name {
        operation_count = operation_distribution[j].1
        break
      }
      j = j + 1
    }
    
    // 基于优先级决定采样
    let should_sample = priority >= priority_threshold
    let sampled_count = if should_sample { operation_count } else { operation_count / 10 }
    
    if priority >= priority_threshold {
      high_priority_sampled = high_priority_sampled + sampled_count
      high_priority_total = high_priority_total + operation_count
    } else {
      low_priority_sampled = low_priority_sampled + sampled_count
      low_priority_total = low_priority_total + operation_count
    }
    
    i = i + 1
  }
  
  // 验证高优先级操作的采样率
  let high_priority_sampling_rate = high_priority_sampled.to_double() / high_priority_total.to_double()
  assert_eq(high_priority_sampling_rate, 1.0)  // 高优先级操作100%采样
  
  // 验证低优先级操作的采样率
  let low_priority_sampling_rate = low_priority_sampled.to_double() / low_priority_total.to_double()
  assert_eq(low_priority_sampling_rate, 0.1)  // 低优先级操作10%采样
}

test "sampling_budget_management" {
  // 测试采样预算管理
  
  let total_budget = 1000  // 每秒最大采样数
  let time_window = 60     // 时间窗口（秒）
  let current_load = 5000  // 当前每秒请求数
  
  // 计算目标采样率
  let target_sampling_rate = total_budget.to_double() / current_load.to_double()
  assert_eq(target_sampling_rate, 0.2)  // 20%采样率
  
  // 模拟不同时间段的负载变化
  let load_profile = [
    (0, 1000),   // 0秒: 1000 req/s
    (10, 3000),  // 10秒: 3000 req/s  
    (20, 8000),  // 20秒: 8000 req/s
    (30, 12000), // 30秒: 12000 req/s
    (40, 6000),  // 40秒: 6000 req/s
    (50, 2000)   // 50秒: 2000 req/s
  ]
  
  let mut total_sampled = 0
  let mut i = 0
  while i < load_profile.length() {
    let timestamp = load_profile[i].0
    let load = load_profile[i].1
    
    // 计算当前时间段的采样率
    let current_sampling_rate = if load <= total_budget {
      1.0  // 低负载时100%采样
    } else {
      total_budget.to_double() / load.to_double()
    }
    
    let sampled_in_window = (load.to_double() * current_sampling_rate).to_int()
    total_sampled = total_sampled + sampled_in_window
    
    // 验证不超过预算
    assert_eq(sampled_in_window <= total_budget, true)
    
    i = i + 1
  }
  
  // 验证总采样数在预算范围内
  let total_budget_overall = total_budget * (load_profile.length())
  assert_eq(total_sampled <= total_budget_overall, true)
}

test "sampling_decision_caching" {
  // 测试采样决策缓存
  
  let trace_ids = [
    "trace1_abcdef123456",
    "trace2_abcdef123456", 
    "trace1_abcdef123456",  // 重复
    "trace3_abcdef123456",
    "trace2_abcdef123456",  // 重复
    "trace4_abcdef123456",
    "trace1_abcdef123456",  // 重复
    "trace5_abcdef123456"
  ]
  
  let sampling_cache = []  // 模拟缓存
  let sampling_decisions = []  // 存储决策结果
  
  // 模拟采样决策缓存逻辑
  let mut i = 0
  while i < trace_ids.length() {
    let trace_id = trace_ids[i]
    
    // 检查缓存
    let mut cache_hit = false
    let mut cached_decision = false
    let mut j = 0
    while j < sampling_cache.length() {
      if sampling_cache[j].0 == trace_id {
        cache_hit = true
        cached_decision = sampling_cache[j].1
        break
      }
      j = j + 1
    }
    
    let sampling_decision = if cache_hit {
      cached_decision
    } else {
      // 模拟采样决策（基于trace_id哈希）
      let hash_value = trace_id.length() % 3  // 简化哈希
      let decision = hash_value == 0  // 约33%的采样率
      
      // 添加到缓存
      sampling_cache.push((trace_id, decision))
      decision
    }
    
    sampling_decisions.push(sampling_decision)
    i = i + 1
  }
  
  // 验证缓存效果
  assert_eq(sampling_cache.length(), 5)  // 5个唯一的trace_id
  assert_eq(sampling_decisions.length(), 8)  // 8个决策
  
  // 验证重复trace_id的决策一致性
  assert_eq(sampling_decisions[0], sampling_decisions[2])  // trace1
  assert_eq(sampling_decisions[1], sampling_decisions[4])  // trace2
  assert_eq(sampling_decisions[0], sampling_decisions[6])  // trace1
}

test "sampling_metadata_enrichment" {
  // 测试采样元数据丰富
  
  let sampling_decision = true
  let sampling_rate = 0.15
  let sampling_algorithm = "adaptive_error_rate"
  let sampling_reason = "high_error_rate_detected"
  
  // 创建采样元数据
  let sampling_metadata = {
    "sampled": sampling_decision.to_string(),
    "sampling_rate": sampling_rate.to_string(),
    "sampling_algorithm": sampling_algorithm,
    "sampling_reason": sampling_reason,
    "sampling_timestamp": "1640995200",
    "sampling_budget_used": "150",
    "sampling_budget_total": "1000"
  }
  
  // 验证元数据完整性
  assert_eq(sampling_metadata["sampled"], "true")
  assert_eq(sampling_metadata["sampling_rate"], "0.15")
  assert_eq(sampling_metadata["sampling_algorithm"], "adaptive_error_rate")
  assert_eq(sampling_metadata["sampling_reason"], "high_error_rate_detected")
  assert_eq(sampling_metadata["sampling_timestamp"], "1640995200")
  assert_eq(sampling_metadata["sampling_budget_used"], "150")
  assert_eq(sampling_metadata["sampling_budget_total"], "1000")
  
  // 创建包含采样信息的追踪头
  let enriched_traceparent = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
  let sampling_tracestate = "azimuth_sampling=" + sampling_rate.to_string() + 
                           ",azimuth_algorithm=" + sampling_algorithm + 
                           ",azimuth_reason=" + sampling_reason
  
  // 验证丰富的追踪状态
  assert_eq(sampling_tracestate.contains("azimuth_sampling=0.15"), true)
  assert_eq(sampling_tracestate.contains("azimuth_algorithm=adaptive_error_rate"), true)
  assert_eq(sampling_tracestate.contains("azimuth_reason=high_error_rate_detected"), true)
}