// 遥测采样器测试用例，测试各种采样策略和算法

test "telemetry_sampler_trace_id_ratio_based" {
  // 测试基于Trace ID比率的采样器
  
  let trace_ids = [
    "0af7651916cd43dd8448eb211c80319c",
    "1af7651916cd43dd8448eb211c80319c",
    "2af7651916cd43dd8448eb211c80319c",
    "3af7651916cd43dd8448eb211c80319c",
    "4af7651916cd43dd8448eb211c80319c",
    "5af7651916cd43dd8448eb211c80319c",
    "6af7651916cd43dd8448eb211c80319c",
    "7af7651916cd43dd8448eb211c80319c",
    "8af7651916cd43dd8448eb211c80319c",
    "9af7651916cd43dd8448eb211c80319c"
  ]
  
  let sampling_ratio = 0.5  // 50% 采样率
  let mut sampled_count = 0
  let mut total_count = 0
  
  // 模拟基于Trace ID的采样决策
  let mut i = 0
  while i < trace_ids.length() {
    total_count = total_count + 1
    
    // 使用Trace ID的第一个字符作为采样依据
    let first_char = trace_ids[i].substring(0, 1)
    let char_value = first_char.to_int()  // 0-9
    let threshold = (sampling_ratio * 10.0).to_int()
    
    if char_value < threshold {
      sampled_count = sampled_count + 1
    }
    
    i = i + 1
  }
  
  // 验证采样结果
  let actual_ratio = sampled_count.to_double() / total_count.to_double()
  assert_eq(total_count, 10)
  assert_eq(sampled_count >= 4 && sampled_count <= 6, true)  // 允许一定的随机性
  assert_eq(actual_ratio >= 0.3 && actual_ratio <= 0.7, true)  // 允许一定的偏差
  
  // 验证Trace ID格式
  assert_eq(trace_ids[0].length(), 32)
  assert_eq(trace_ids[9].length(), 32)
  assert_eq(trace_ids[0].has_prefix("0"), true)
  assert_eq(trace_ids[9].has_prefix("9"), true)
}

test "telemetry_sampler_parent_based" {
  // 测试基于父Span的采样器
  
  let parent_trace_states = [
    ("sampled", "true"),
    ("sampled", "false"),
    ("remote", "true"),
    ("remote", "false"),
    ("none", "none")
  ]
  
  let mut sampled_count = 0
  let mut total_count = 0
  
  // 模拟基于父Span的采样决策
  let mut i = 0
  while i < parent_trace_states.length() {
    total_count = total_count + 1
    
    let parent_type = parent_trace_states[i].0
    let parent_sampled = parent_trace_states[i].1
    
    // 采样逻辑：如果父Span已采样，则子Span也采样
    let should_sample = 
      if parent_type == "sampled" && parent_sampled == "true" { true }
      else if parent_type == "sampled" && parent_sampled == "false" { false }
      else if parent_type == "remote" && parent_sampled == "true" { true }
      else if parent_type == "remote" && parent_sampled == "false" { false }
      else if parent_type == "none" { true }  // 根Span默认采样
      else { false }
    
    if should_sample {
      sampled_count = sampled_count + 1
    }
    
    i = i + 1
  }
  
  // 验证采样结果
  assert_eq(total_count, 5)
  assert_eq(sampled_count, 3)  // sampled:true, remote:true, none
  
  // 验证父状态
  assert_eq(parent_trace_states[0].0, "sampled")
  assert_eq(parent_trace_states[0].1, "true")
  assert_eq(parent_trace_states[4].0, "none")
  assert_eq(parent_trace_states[4].1, "none")
}

test "telemetry_sampler_attribute_based" {
  // 测试基于属性的采样器
  
  let span_attributes = [
    [("http.method", "GET"), ("http.status_code", "200"), ("service.name", "api-gateway")],
    [("http.method", "POST"), ("http.status_code", "500"), ("service.name", "payment-service")],
    [("http.method", "GET"), ("http.status_code", "404"), ("service.name", "user-service")],
    [("error.type", "timeout"), ("service.name", "database-service")],
    [("http.method", "GET"), ("http.status_code", "200"), ("service.name", "cache-service")],
    [("http.method", "DELETE"), ("http.status_code", "204"), ("service.name", "storage-service")]
  ]
  
  let mut sampled_count = 0
  let mut total_count = 0
  
  // 采样规则：
  // 1. 错误状态码（4xx, 5xx）总是采样
  // 2. 包含error.type属性的Span总是采样
  // 3. 其他情况随机采样50%
  
  let mut i = 0
  while i < span_attributes.length() {
    total_count = total_count + 1
    
    let mut should_sample = false
    let mut has_error_attr = false
    let mut status_code = ""
    let mut method = ""
    
    // 检查属性
    let mut j = 0
    while j < span_attributes[i].length() {
      let attr_name = span_attributes[i][j].0
      let attr_value = span_attributes[i][j].1
      
      if attr_name == "error.type" {
        has_error_attr = true
      } else if attr_name == "http.status_code" {
        status_code = attr_value
      } else if attr_name == "http.method" {
        method = attr_value
      }
      
      j = j + 1
    }
    
    // 应用采样规则
    if has_error_attr {
      should_sample = true
    } else if status_code.has_prefix("4") || status_code.has_prefix("5") {
      should_sample = true
    } else {
      // 随机采样50%
      let hash = method.length() % 2
      should_sample = hash == 0
    }
    
    if should_sample {
      sampled_count = sampled_count + 1
    }
    
    i = i + 1
  }
  
  // 验证采样结果
  assert_eq(total_count, 6)
  assert_eq(sampled_count >= 3 && sampled_count <= 5, true)  // 至少包含错误Span
  
  // 验证属性数据
  assert_eq(span_attributes[0].length(), 3)
  assert_eq(span_attributes[1][1].1, "500")
  assert_eq(span_attributes[3][0].0, "error.type")
}

test "telemetry_sampler_rate_limiting" {
  // 测试速率限制采样器
  
  let max_samples_per_second = 10
  let time_window = 1000  // 1秒窗口
  let incoming_spans = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
  
  let mut sampled_count = 0
  let mut dropped_count = 0
  let mut current_window_samples = 0
  
  // 模拟速率限制采样
  let mut i = 0
  while i < incoming_spans.length() {
    // 每5个Span重置窗口（模拟时间流逝）
    if i > 0 && i % 5 == 0 {
      current_window_samples = 0
    }
    
    if current_window_samples < max_samples_per_second {
      sampled_count = sampled_count + 1
      current_window_samples = current_window_samples + 1
    } else {
      dropped_count = dropped_count + 1
    }
    
    i = i + 1
  }
  
  // 验证采样结果
  assert_eq(incoming_spans.length(), 15)
  assert_eq(sampled_count, 15)  // 所有Span都被采样，因为限制足够高
  assert_eq(dropped_count, 0)
  
  // 测试更严格的限制
  let strict_max_samples = 3
  sampled_count = 0
  dropped_count = 0
  current_window_samples = 0
  
  i = 0
  while i < incoming_spans.length() {
    if i > 0 && i % 5 == 0 {
      current_window_samples = 0
    }
    
    if current_window_samples < strict_max_samples {
      sampled_count = sampled_count + 1
      current_window_samples = current_window_samples + 1
    } else {
      dropped_count = dropped_count + 1
    }
    
    i = i + 1
  }
  
  // 验证严格限制的结果
  assert_eq(sampled_count, 9)  // 3个窗口 * 每窗口3个采样
  assert_eq(dropped_count, 6)  // 15 - 9
}

test "telemetry_sampler_adaptive" {
  // 测试自适应采样器
  
  let system_load_levels = [0.2, 0.4, 0.6, 0.8, 0.9, 0.7, 0.5, 0.3, 0.1, 0.4]
  let incoming_span_rates = [100, 150, 200, 300, 400, 250, 180, 120, 80, 160]
  
  let mut total_sampled = 0
  let mut total_incoming = 0
  
  // 自适应采样逻辑：根据系统负载动态调整采样率
  let mut i = 0
  while i < system_load_levels.length() {
    let load = system_load_levels[i]
    let incoming_rate = incoming_span_rates[i]
    
    // 计算自适应采样率
    let sampling_rate = 
      if load < 0.3 { 1.0 }  // 低负载：100%采样
      else if load < 0.5 { 0.8 }  // 中低负载：80%采样
      else if load < 0.7 { 0.5 }  // 中等负载：50%采样
      else if load < 0.9 { 0.2 }  // 高负载：20%采样
      else { 0.1 }  // 极高负载：10%采样
    
    let sampled_count = (incoming_rate.to_double() * sampling_rate).to_int()
    total_sampled = total_sampled + sampled_count
    total_incoming = total_incoming + incoming_rate
    
    i = i + 1
  }
  
  // 验证自适应采样结果
  assert_eq(system_load_levels.length(), 10)
  assert_eq(incoming_span_rates.length(), 10)
  assert_eq(total_incoming, 1940)
  assert_eq(total_sampled > 0 && total_sampled < total_incoming, true)
  
  // 验证负载级别
  assert_eq(system_load_levels[0], 0.2)
  assert_eq(system_load_levels[4], 0.9)
  assert_eq(system_load_levels[8], 0.1)
  
  // 验证传入速率
  assert_eq(incoming_span_rates[0], 100)
  assert_eq(incoming_span_rates[4], 400)
  assert_eq(incoming_span_rates[8], 80)
}

test "telemetry_sampler_consistency" {
  // 测试采样器一致性
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let sampling_decisions = []
  let sampling_ratio = 0.25  // 25%采样率
  
  // 对同一个Trace ID进行多次采样决策，验证一致性
  let mut i = 0
  while i < 10 {
    // 使用确定性采样算法
    let trace_hash = trace_id.length() % 100
    let threshold = (sampling_ratio * 100.0).to_int()
    let decision = trace_hash < threshold
    
    sampling_decisions.push(decision)
    i = i + 1
  }
  
  // 验证一致性：所有决策应该相同
  let first_decision = sampling_decisions[0]
  let mut all_consistent = true
  
  i = 0
  while i < sampling_decisions.length() {
    if sampling_decisions[i] != first_decision {
      all_consistent = false
    }
    i = i + 1
  }
  
  assert_eq(all_consistent, true)
  assert_eq(sampling_decisions.length(), 10)
  
  // 测试不同Trace ID的采样分布
  let different_trace_ids = [
    "0af7651916cd43dd8448eb211c80319c",
    "1af7651916cd43dd8448eb211c80319c",
    "2af7651916cd43dd8448eb211c80319c",
    "3af7651916cd43dd8448eb211c80319c",
    "4af7651916cd43dd8448eb211c80319c",
    "5af7651916cd43dd8448eb211c80319c",
    "6af7651916cd43dd8448eb211c80319c",
    "7af7651916cd43dd8448eb211c80319c"
  ]
  
  let mut sampled_count = 0
  i = 0
  while i < different_trace_ids.length() {
    let trace_hash = different_trace_ids[i].length() % 100
    let threshold = (sampling_ratio * 100.0).to_int()
    
    if trace_hash < threshold {
      sampled_count = sampled_count + 1
    }
    
    i = i + 1
  }
  
  // 验证采样分布
  let actual_ratio = sampled_count.to_double() / different_trace_ids.length().to_double()
  assert_eq(different_trace_ids.length(), 8)
  assert_eq(sampled_count >= 1 && sampled_count <= 3, true)  // 25%左右
}