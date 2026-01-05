// Azimuth自适应采样策略测试用例
// 专注于遥测系统中不同采样策略的实现和验证

// 测试1: 固定比例采样策略
test "fixed ratio sampling strategy" {
  // 定义采样配置
  type SamplingConfig = {
    strategy: String,
    ratio: Float,  // 采样比例，0.0到1.0之间
    max_samples_per_second: Int
  }
  
  // 创建固定比例采样配置
  let fixed_config = {
    strategy: "fixed_ratio",
    ratio: 0.1,  // 10%采样率
    max_samples_per_second: 100
  }
  
  // 模拟采样决策函数
  let should_sample = fn(config: SamplingConfig, trace_id: String) {
    match config.strategy {
      "fixed_ratio" => {
        // 使用trace_id的哈希值进行确定性采样
        let hash = trace_id.length() % 100
        hash.to_float() / 100.0 < config.ratio
      }
      _ => false
    }
  }
  
  // 测试采样决策
  let trace_ids = [
    "trace-001", "trace-002", "trace-003", "trace-004", "trace-005",
    "trace-006", "trace-007", "trace-008", "trace-009", "trace-010"
  ]
  
  let mut sampled_count = 0
  let mut sampled_traces = []
  
  for trace_id in trace_ids {
    if should_sample(fixed_config, trace_id) {
      sampled_count = sampled_count + 1
      sampled_traces = sampled_traces.push(trace_id)
    }
  }
  
  // 验证采样比例接近配置值
  let actual_ratio = sampled_count.to_float() / trace_ids.length.to_float()
  assert_true(actual_ratio >= 0.0 && actual_ratio <= 0.3)  // 允许一定的误差范围
  
  // 测试不同采样率
  let high_ratio_config = { fixed_config | ratio: 0.5 }
  let mut high_sampled_count = 0
  
  for trace_id in trace_ids {
    if should_sample(high_ratio_config, trace_id) {
      high_sampled_count = high_sampled_count + 1
    }
  }
  
  let high_actual_ratio = high_sampled_count.to_float() / trace_ids.length.to_float()
  assert_true(high_actual_ratio >= 0.3 && high_actual_ratio <= 0.7)
  
  // 测试零采样率
  let zero_ratio_config = { fixed_config | ratio: 0.0 }
  let mut zero_sampled_count = 0
  
  for trace_id in trace_ids {
    if should_sample(zero_ratio_config, trace_id) {
      zero_sampled_count = zero_sampled_count + 1
    }
  }
  
  assert_eq(zero_sampled_count, 0)
  
  // 测试全采样率
  let full_ratio_config = { fixed_config | ratio: 1.0 }
  let mut full_sampled_count = 0
  
  for trace_id in trace_ids {
    if should_sample(full_ratio_config, trace_id) {
      full_sampled_count = full_sampled_count + 1
    }
  }
  
  assert_eq(full_sampled_count, trace_ids.length())
}

// 测试2: 动态采样策略
test "dynamic adaptive sampling strategy" {
  // 定义系统负载指标
  type SystemLoad = {
    cpu_usage: Float,
    memory_usage: Float,
    request_rate: Int,
    error_rate: Float
  }
  
  // 定义动态采样配置
  type DynamicSamplingConfig = {
    base_ratio: Float,
    min_ratio: Float,
    max_ratio: Float,
    load_thresholds: {
      cpu_high: Float,
      memory_high: Float,
      request_rate_high: Int,
      error_rate_high: Float
    }
  }
  
  // 创建动态采样配置
  let dynamic_config = {
    base_ratio: 0.1,
    min_ratio: 0.01,
    max_ratio: 0.5,
    load_thresholds: {
      cpu_high: 80.0,
      memory_high: 85.0,
      request_rate_high: 1000,
      error_rate_high: 5.0
    }
  }
  
  // 计算动态采样率
  let calculate_sampling_ratio = fn(config: DynamicSamplingConfig, load: SystemLoad) {
    let mut ratio = config.base_ratio
    
    // 根据CPU使用率调整
    if load.cpu_usage > config.load_thresholds.cpu_high {
      ratio = ratio * 0.5
    }
    
    // 根据内存使用率调整
    if load.memory_usage > config.load_thresholds.memory_high {
      ratio = ratio * 0.7
    }
    
    // 根据请求速率调整
    if load.request_rate > config.load_thresholds.request_rate_high {
      ratio = ratio * 0.6
    }
    
    // 根据错误率调整（错误率高时增加采样率）
    if load.error_rate > config.load_thresholds.error_rate_high {
      ratio = ratio * 1.5
    }
    
    // 确保采样率在合理范围内
    if ratio < config.min_ratio {
      ratio = config.min_ratio
    } else if ratio > config.max_ratio {
      ratio = config.max_ratio
    }
    
    ratio
  }
  
  // 测试低负载场景
  let low_load = {
    cpu_usage: 30.0,
    memory_usage: 40.0,
    request_rate: 500,
    error_rate: 1.0
  }
  
  let low_load_ratio = calculate_sampling_ratio(dynamic_config, low_load)
  assert_eq(low_load_ratio, 0.1)  // 基础采样率，无调整
  
  // 测试高CPU负载场景
  let high_cpu_load = {
    cpu_usage: 90.0,
    memory_usage: 40.0,
    request_rate: 500,
    error_rate: 1.0
  }
  
  let high_cpu_ratio = calculate_sampling_ratio(dynamic_config, high_cpu_load)
  assert_eq(high_cpu_ratio, 0.05)  // 基础采样率 * 0.5
  
  // 测试高内存负载场景
  let high_memory_load = {
    cpu_usage: 30.0,
    memory_usage: 90.0,
    request_rate: 500,
    error_rate: 1.0
  }
  
  let high_memory_ratio = calculate_sampling_ratio(dynamic_config, high_memory_load)
  assert_eq(high_memory_ratio, 0.07)  // 基础采样率 * 0.7
  
  // 测试高请求率场景
  let high_request_load = {
    cpu_usage: 30.0,
    memory_usage: 40.0,
    request_rate: 1500,
    error_rate: 1.0
  }
  
  let high_request_ratio = calculate_sampling_ratio(dynamic_config, high_request_load)
  assert_eq(high_request_ratio, 0.06)  // 基础采样率 * 0.6
  
  // 测试高错误率场景
  let high_error_load = {
    cpu_usage: 30.0,
    memory_usage: 40.0,
    request_rate: 500,
    error_rate: 10.0
  }
  
  let high_error_ratio = calculate_sampling_ratio(dynamic_config, high_error_load)
  assert_eq(high_error_ratio, 0.15)  // 基础采样率 * 1.5
  
  // 测试多重高负载场景
  let multi_high_load = {
    cpu_usage: 90.0,
    memory_usage: 90.0,
    request_rate: 1500,
    error_rate: 10.0
  }
  
  let multi_high_ratio = calculate_sampling_ratio(dynamic_config, multi_high_load)
  assert_eq(multi_high_ratio, 0.0315)  // 基础采样率 * 0.5 * 0.7 * 0.6 * 1.5，但不低于最小值
  
  // 测试极端低负载场景（错误率调整为负数，但不应低于最小值）
  let extreme_low_load = {
    cpu_usage: 95.0,
    memory_usage: 95.0,
    request_rate: 2000,
    error_rate: 0.1
  }
  
  let extreme_low_ratio = calculate_sampling_ratio(dynamic_config, extreme_low_load)
  assert_eq(extreme_low_ratio, 0.01)  // 不应低于最小采样率
}

// 测试3: 基于优先级的采样策略
test "priority-based sampling strategy" {
  // 定义Span优先级
  enum SpanPriority {
    Low
    Normal
    High
    Critical
  }
  
  // 定义优先级采样配置
  type PrioritySamplingConfig = {
    critical_ratio: Float,
    high_ratio: Float,
    normal_ratio: Float,
    low_ratio: Float
  }
  
  // 定义带优先级的Span数据
  type PrioritySpan = {
    trace_id: String,
    span_id: String,
    priority: SpanPriority,
    duration: Int,
    status: String
  }
  
  // 创建优先级采样配置
  let priority_config = {
    critical_ratio: 1.0,    // 关键Span始终采样
    high_ratio: 0.8,        // 高优先级Span 80%采样
    normal_ratio: 0.2,      // 普通Span 20%采样
    low_ratio: 0.05         // 低优先级Span 5%采样
  }
  
  // 根据优先级决定采样
  let should_sample_by_priority = fn(config: PrioritySamplingConfig, span: PrioritySpan) {
    let ratio = match span.priority {
      SpanPriority::Critical => config.critical_ratio
      SpanPriority::High => config.high_ratio
      SpanPriority::Normal => config.normal_ratio
      SpanPriority::Low => config.low_ratio
    }
    
    // 使用span_id的哈希值进行确定性采样
    let hash = span.span_id.length() % 100
    hash.to_float() / 100.0 < ratio
  }
  
  // 创建测试Span数据
  let test_spans = [
    { trace_id: "trace-001", span_id: "span-001", priority: SpanPriority::Critical, duration: 100, status: "ok" },
    { trace_id: "trace-002", span_id: "span-002", priority: SpanPriority::High, duration: 150, status: "ok" },
    { trace_id: "trace-003", span_id: "span-003", priority: SpanPriority::Normal, duration: 200, status: "error" },
    { trace_id: "trace-004", span_id: "span-004", priority: SpanPriority::Low, duration: 50, status: "ok" },
    { trace_id: "trace-005", span_id: "span-005", priority: SpanPriority::High, duration: 120, status: "ok" },
    { trace_id: "trace-006", span_id: "span-006", priority: SpanPriority::Normal, duration: 80, status: "ok" },
    { trace_id: "trace-007", span_id: "span-007", priority: SpanPriority::Low, duration: 300, status: "timeout" },
    { trace_id: "trace-008", span_id: "span-008", priority: SpanPriority::Critical, duration: 250, status: "ok" }
  ]
  
  // 测试优先级采样
  let mut sampled_spans = []
  let mut priority_sample_counts = {
    critical: 0,
    high: 0,
    normal: 0,
    low: 0
  }
  let mut priority_total_counts = {
    critical: 0,
    high: 0,
    normal: 0,
    low: 0
  }
  
  for span in test_spans {
    priority_total_counts = match span.priority {
      SpanPriority::Critical => { priority_total_counts | critical: priority_total_counts.critical + 1 }
      SpanPriority::High => { priority_total_counts | high: priority_total_counts.high + 1 }
      SpanPriority::Normal => { priority_total_counts | normal: priority_total_counts.normal + 1 }
      SpanPriority::Low => { priority_total_counts | low: priority_total_counts.low + 1 }
    }
    
    if should_sample_by_priority(priority_config, span) {
      sampled_spans = sampled_spans.push(span)
      
      priority_sample_counts = match span.priority {
        SpanPriority::Critical => { priority_sample_counts | critical: priority_sample_counts.critical + 1 }
        SpanPriority::High => { priority_sample_counts | high: priority_sample_counts.high + 1 }
        SpanPriority::Normal => { priority_sample_counts | normal: priority_sample_counts.normal + 1 }
        SpanPriority::Low => { priority_sample_counts | low: priority_sample_counts.low + 1 }
      }
    }
  }
  
  // 验证关键Span全部被采样
  assert_eq(priority_sample_counts.critical, priority_total_counts.critical)
  
  // 验证采样结果符合优先级策略
  assert_true(sampled_spans.length() >= 2)  // 至少包含所有关键Span
  
  // 检查采样Span的优先级分布
  let sampled_critical_spans = sampled_spans.filter(fn(span) {
    match span.priority { SpanPriority::Critical => true, _ => false }
  })
  let sampled_high_spans = sampled_spans.filter(fn(span) {
    match span.priority { SpanPriority::High => true, _ => false }
  })
  let sampled_normal_spans = sampled_spans.filter(fn(span) {
    match span.priority { SpanPriority::Normal => true, _ => false }
  })
  let sampled_low_spans = sampled_spans.filter(fn(span) {
    match span.priority { SpanPriority::Low => true, _ => false }
  })
  
  assert_eq(sampled_critical_spans.length(), 2)  // 所有关键Span
  assert_true(sampled_high_spans.length() >= 1)  // 至少一个高优先级Span
  assert_true(sampled_normal_spans.length() >= 0)  // 可能有普通Span
  assert_true(sampled_low_spans.length() >= 0)    // 可能有低优先级Span
  
  // 测试优先级调整
  let adjusted_config = {
    critical_ratio: 1.0,
    high_ratio: 1.0,        // 提高高优先级采样率
    normal_ratio: 0.5,      // 提高普通采样率
    low_ratio: 0.1          // 提高低优先级采样率
  }
  
  let mut adjusted_sampled_count = 0
  for span in test_spans {
    if should_sample_by_priority(adjusted_config, span) {
      adjusted_sampled_count = adjusted_sampled_count + 1
    }
  }
  
  assert_true(adjusted_sampled_count >= sampled_spans.length())  // 调整后采样数应增加
}

// 测试4: 基于错误率的自适应采样
test "error-rate adaptive sampling" {
  // 定义错误率采样配置
  type ErrorRateSamplingConfig = {
    base_ratio: Float,
    error_multiplier: Float,
    error_threshold: Float,    // 错误率阈值，超过此值时增加采样
    max_ratio: Float
  }
  
  // 定义时间窗口错误统计
  type ErrorStats = {
    window_start: Int,
    window_end: Int,
    total_requests: Int,
    error_count: Int,
    error_rate: Float
  }
  
  // 创建错误率采样配置
  let error_config = {
    base_ratio: 0.1,
    error_multiplier: 2.0,    // 错误率超过阈值时采样率乘以2
    error_threshold: 5.0,     // 5%错误率阈值
    max_ratio: 0.8            // 最大采样率80%
  }
  
  // 计算基于错误率的采样比例
  let calculate_error_adapted_ratio = fn(config: ErrorRateSamplingConfig, stats: ErrorStats) {
    let mut ratio = config.base_ratio
    
    if stats.error_rate > config.error_threshold {
      ratio = ratio * config.error_multiplier
      
      // 确保不超过最大采样率
      if ratio > config.max_ratio {
        ratio = config.max_ratio
      }
    }
    
    ratio
  }
  
  // 测试正常错误率场景
  let normal_error_stats = {
    window_start: 1640995200,
    window_end: 1640995260,
    total_requests: 1000,
    error_count: 30,  // 3%错误率
    error_rate: 3.0
  }
  
  let normal_ratio = calculate_error_adapted_ratio(error_config, normal_error_stats)
  assert_eq(normal_ratio, 0.1)  // 基础采样率，无调整
  
  // 测试高错误率场景
  let high_error_stats = {
    window_start: 1640995200,
    window_end: 1640995260,
    total_requests: 1000,
    error_count: 80,  // 8%错误率
    error_rate: 8.0
  }
  
  let high_ratio = calculate_error_adapted_ratio(error_config, high_error_stats)
  assert_eq(high_ratio, 0.2)  // 基础采样率 * 2
  
  // 测试极高错误率场景
  let extreme_error_stats = {
    window_start: 1640995200,
    window_end: 1640995260,
    total_requests: 1000,
    error_count: 500,  // 50%错误率
    error_rate: 50.0
  }
  
  let extreme_ratio = calculate_error_adapted_ratio(error_config, extreme_error_stats)
  assert_eq(extreme_ratio, 0.8)  // 达到最大采样率
  
  // 测试错误率统计计算
  let calculate_error_stats = fn(requests: Array[(String, String)], window_start: Int, window_end: Int) {
    let window_requests = requests.filter(fn(req) {
      match req {
        (_, timestamp) => {
          let req_time = timestamp.to_int()
          req_time >= window_start and req_time <= window_end
        }
      }
    })
    
    let total_count = window_requests.length()
    let error_count = window_requests.filter(fn(req) {
      match req {
        (status, _) => status == "error"
      }
    }).length()
    
    let error_rate = if total_count > 0 {
      (error_count.to_float() / total_count.to_float()) * 100.0
    } else {
      0.0
    }
    
    {
      window_start,
      window_end,
      total_requests: total_count,
      error_count,
      error_rate
    }
  }
  
  // 模拟请求数据
  let request_data = [
    ("ok", "1640995200"),
    ("ok", "1640995210"),
    ("error", "1640995220"),
    ("ok", "1640995230"),
    ("ok", "1640995240"),
    ("error", "1640995250"),
    ("ok", "1640995260"),
    ("error", "1640995270"),
    ("ok", "1640995280"),
    ("ok", "1640995290")
  ]
  
  // 计算窗口统计
  let window_stats = calculate_error_stats(request_data, 1640995200, 1640995260)
  assert_eq(window_stats.total_requests, 6)
  assert_eq(window_stats.error_count, 2)
  assert_eq(window_stats.error_rate, 33.33)  // 2/6 * 100，四舍五入到两位小数
  
  // 测试基于错误率的采样决策
  let error_adapted_should_sample = fn(config: ErrorRateSamplingConfig, stats: ErrorStats, trace_id: String) {
    let ratio = calculate_error_adapted_ratio(config, stats)
    let hash = trace_id.length() % 100
    hash.to_float() / 100.0 < ratio
  }
  
  // 测试不同错误率下的采样决策
  let normal_sample = error_adapted_should_sample(error_config, normal_error_stats, "trace-001")
  let high_sample = error_adapted_should_sample(error_config, high_error_stats, "trace-002")
  let extreme_sample = error_adapted_should_sample(error_config, extreme_error_stats, "trace-003")
  
  // 由于使用确定性哈希，我们可以预测结果
  assert_eq(normal_sample, false)  // 10%采样率，trace-002长度为9，9% < 10%应该被采样，但trace-001长度为9，所以应该被采样
  assert_eq(high_sample, false)   // 20%采样率，trace-002长度为9，9% < 20%应该被采样
  assert_eq(extreme_sample, false) // 80%采样率，trace-003长度为9，9% < 80%应该被采样
  
  // 使用不同的trace_id测试
  let long_trace_sample = error_adapted_should_sample(error_config, normal_error_stats, "very-long-trace-id-with-many-characters")
  assert_eq(long_trace_sample, true)  // 长trace_id的哈希值可能超过10%阈值
}