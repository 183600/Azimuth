// 遥测数据采样测试用例

test "telemetry_sampling_strategy" {
  // 测试遥测采样策略
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let sample_rate = 0.1 // 10% 采样率
  
  // 基于trace_id计算采样决策
  let trace_hash = trace_id.length() % 100
  let should_sample = trace_hash < (sample_rate * 100.0).to_int()
  
  // 验证采样逻辑
  assert_eq(sample_rate > 0.0, true)
  assert_eq(sample_rate < 1.0, true)
  
  // 创建采样决策记录
  let sampling_decision = if should_sample { "sampled" } else { "not_sampled" }
  let sampling_record = trace_id + ":" + sampling_decision + ":" + sample_rate.to_string()
  
  // 验证采样记录
  assert_eq(sampling_record.contains(trace_id), true)
  assert_eq(sampling_record.contains("sampled") || sampling_record.contains("not_sampled"), true)
  assert_eq(sampling_record.contains("0.1"), true)
}

test "telemetry_sampling_adaptive" {
  // 测试自适应采样
  
  let current_load = 85.5 // 当前系统负载百分比
  let base_sample_rate = 0.1
  let max_sample_rate = 0.5
  
  // 根据系统负载调整采样率
  let adaptive_rate = if current_load > 80.0 {
    base_sample_rate * 0.5 // 高负载时降低采样率
  } else if current_load < 50.0 {
    max_sample_rate // 低负载时提高采样率
  } else {
    base_sample_rate // 正常负载使用基础采样率
  }
  
  // 验证自适应采样率
  assert_eq(adaptive_rate, 0.05) // 当前负载85.5% > 80%，所以采样率应该是0.05
  assert_eq(adaptive_rate > 0.0, true)
  assert_eq(adaptive_rate < max_sample_rate, true)
  
  // 创建自适应采样配置
  let adaptive_config = "load:" + current_load.to_string() + 
                        ",base_rate:" + base_sample_rate.to_string() + 
                        ",adaptive_rate:" + adaptive_rate.to_string()
  
  assert_eq(adaptive_config.contains("load:85.5"), true)
  assert_eq(adaptive_config.contains("adaptive_rate:0.05"), true)
}

test "telemetry_sampling_priority" {
  // 测试优先级采样
  
  let error_trace = "error_trace_123"
  let normal_trace = "normal_trace_456"
  let debug_trace = "debug_trace_789"
  
  // 定义优先级
  let error_priority = 100
  let normal_priority = 50
  let debug_priority = 10
  
  // 高优先级总是采样
  let error_sampled = error_priority >= 90
  let normal_sampled = normal_priority >= 50
  let debug_sampled = debug_priority >= 50
  
  // 验证优先级采样决策
  assert_eq(error_sampled, true) // 错误追踪总是采样
  assert_eq(normal_sampled, true) // 普通追踪达到阈值
  assert_eq(debug_sampled, false) // 调试追踪未达到阈值
  
  // 创建优先级采样统计
  let sampled_traces = []
  if error_sampled { sampled_traces.push(error_trace) }
  if normal_sampled { sampled_traces.push(normal_trace) }
  if debug_sampled { sampled_traces.push(debug_trace) }
  
  assert_eq(sampled_traces.length(), 2)
  assert_eq(sampled_traces.contains("error_trace_123"), true)
  assert_eq(sampled_traces.contains("normal_trace_456"), true)
  assert_eq(sampled_traces.contains("debug_trace_789"), false)
}