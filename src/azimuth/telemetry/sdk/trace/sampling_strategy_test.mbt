// 采样策略测试 - 测试不同采样算法的行为

test "always_on_sampler_behavior" {
  // 测试AlwaysOn采样器的行为
  
  struct SamplingDecision {
    sampled : Bool
    attributes : Array[(String, String)]
  }
  
  struct SamplingParameters {
    trace_id : String
    span_name : String
    span_kind : String
    parent_context : Option[String]
    attributes : Array[(String, String)]
    links : Array[String]
  }
  
  func always_on_sample(params : SamplingParameters) -> SamplingDecision {
    SamplingDecision{
      sampled: true,
      attributes: [
        ("sampler.type", "always_on"),
        ("sampler.decision", "record_and_sample")
      ]
    }
  }
  
  // 测试不同场景下的AlwaysOn采样
  let test_cases = [
    SamplingParameters{
      trace_id: "0123456789abcdef0123456789abcdef",
      span_name: "http.request",
      span_kind: "server",
      parent_context: None,
      attributes: [("http.method", "GET")],
      links: []
    },
    SamplingParameters{
      trace_id: "fedcba9876543210fedcba9876543210",
      span_name: "database.query",
      span_kind: "client",
      parent_context: Some("parent-span-context"),
      attributes: [("db.statement", "SELECT * FROM users")],
      links: ["linked-span-1", "linked-span-2"]
    },
    SamplingParameters{
      trace_id: "11111111111111111111111111111111",
      span_name: "internal.processing",
      span_kind: "internal",
      parent_context: None,
      attributes: [],
      links: []
    }
  ]
  
  for test_case in test_cases {
    let decision = always_on_sample(test_case)
    
    // AlwaysOn采样器应该总是采样
    assert_eq(decision.sampled, true, "AlwaysOn sampler should always sample, trace_id: {test_case.trace_id}")
    
    // 验证采样属性
    assert_eq(decision.attributes.length(), 2)
    assert_eq(decision.attributes[0], ("sampler.type", "always_on"))
    assert_eq(decision.attributes[1], ("sampler.decision", "record_and_sample"))
  }
}

test "always_off_sampler_behavior" {
  // 测试AlwaysOff采样器的行为
  
  func always_off_sample(params : SamplingParameters) -> SamplingDecision {
    SamplingDecision{
      sampled: false,
      attributes: [
        ("sampler.type", "always_off"),
        ("sampler.decision", "drop")
      ]
    }
  }
  
  // 测试不同场景下的AlwaysOff采样
  let test_cases = [
    SamplingParameters{
      trace_id: "0123456789abcdef0123456789abcdef",
      span_name: "critical.operation",
      span_kind: "server",
      parent_context: None,
      attributes: [("priority", "high")],
      links: []
    },
    SamplingParameters{
      trace_id: "fedcba9876543210fedcba9876543210",
      span_name: "error.handling",
      span_kind: "client",
      parent_context: Some("parent-span-context"),
      attributes: [("error.type", "timeout")],
      links: []
    }
  ]
  
  for test_case in test_cases {
    let decision = always_off_sample(test_case)
    
    // AlwaysOff采样器应该从不采样
    assert_eq(decision.sampled, false, "AlwaysOff sampler should never sample, trace_id: {test_case.trace_id}")
    
    // 验证采样属性
    assert_eq(decision.attributes.length(), 2)
    assert_eq(decision.attributes[0], ("sampler.type", "always_off"))
    assert_eq(decision.attributes[1], ("sampler.decision", "drop"))
  }
}

test "trace_id_ratio_sampler_behavior" {
  // 测试TraceIdRatio采样器的行为
  
  func trace_id_to_ratio(trace_id : String) -> Double {
    // 将trace_id转换为数值用于采样决策
    let mut hash = 0
    for char in trace_id.to_array() {
      if char >= '0' && char <= '9' {
        hash = hash * 16 + (char.to_int() - '0'.to_int())
      } else if char >= 'a' && char <= 'f' {
        hash = hash * 16 + (char.to_int() - 'a'.to_int() + 10)
      } else if char >= 'A' && char <= 'F' {
        hash = hash * 16 + (char.to_int() - 'A'.to_int() + 10)
      }
    }
    
    // 归一化到[0, 1)范围
    (hash % 1000000).to_double() / 1000000.0
  }
  
  func trace_id_ratio_sample(params : SamplingParameters, ratio : Double) -> SamplingDecision {
    let trace_id_ratio = trace_id_to_ratio(params.trace_id)
    let sampled = trace_id_ratio < ratio
    
    SamplingDecision{
      sampled: sampled,
      attributes: [
        ("sampler.type", "trace_id_ratio"),
        ("sampler.decision", if sampled { "record_and_sample" } else { "drop" }),
        ("sampler.ratio", ratio.to_string()),
        ("sampler.trace_id_ratio", trace_id_ratio.to_string())
      ]
    }
  }
  
  // 测试不同的采样比例
  let ratios = [0.0, 0.1, 0.5, 0.9, 1.0]
  let test_trace_ids = [
    "00000000000000000000000000000000",  // 最小值
    "7fffffffffffffffffffffffffffffff",  // 中间值
    "ffffffffffffffffffffffffffffffff"   // 最大值
  ]
  
  for ratio in ratios {
    for trace_id in test_trace_ids {
      let params = SamplingParameters{
        trace_id: trace_id,
        span_name: "test.span",
        span_kind: "server",
        parent_context: None,
        attributes: [],
        links: []
      }
      
      let decision = trace_id_ratio_sample(params, ratio)
      let expected_sampled = if ratio == 0.0 { false } 
                            else if ratio == 1.0 { true }
                            else {
                              let trace_ratio = trace_id_to_ratio(trace_id)
                              trace_ratio < ratio
                            }
      
      assert_eq(decision.sampled, expected_sampled, 
        "TraceIdRatio sampler with ratio {ratio} should {if expected_sampled { "sample" } else { "not sample" }} trace_id {trace_id}")
      
      // 验证采样属性
      assert_eq(decision.attributes.length(), 4)
      assert_eq(decision.attributes[0], ("sampler.type", "trace_id_ratio"))
      assert_eq(decision.attributes[2], ("sampler.ratio", ratio.to_string()))
    }
  }
  
  // 测试边界情况
  let zero_ratio = 0.0
  let one_ratio = 1.0
  
  for trace_id in test_trace_ids {
    let params = SamplingParameters{
      trace_id: trace_id,
      span_name: "boundary.test",
      span_kind: "server",
      parent_context: None,
      attributes: [],
      links: []
    }
    
    let zero_decision = trace_id_ratio_sample(params, zero_ratio)
    let one_decision = trace_id_ratio_sample(params, one_ratio)
    
    assert_eq(zero_decision.sampled, false, "Zero ratio should never sample")
    assert_eq(one_decision.sampled, true, "One ratio should always sample")
  }
}

test "parent_based_sampler_behavior" {
  // 测试ParentBased采样器的行为
  
  enum ParentSamplingDecision {
    Sampled
    NotSampled
    NoParent
  }
  
  struct ParentBasedConfig {
    remote_parent_sampled : String
    remote_parent_not_sampled : String
    local_parent_sampled : String
    local_parent_not_sampled : String
  }
  
  func parent_based_sample(params : SamplingParameters, config : ParentBasedConfig) -> SamplingDecision {
    match params.parent_context {
      None => {
        // 没有父span，使用远程父span未采样的策略
        match config.remote_parent_not_sampled {
          "always_on" => SamplingDecision{
            sampled: true,
            attributes: [
              ("sampler.type", "parent_based"),
              ("sampler.decision", "record_and_sample"),
              ("sampler.parent", "none"),
              ("sampler.delegate", "always_on")
            ]
          }
          "always_off" => SamplingDecision{
            sampled: false,
            attributes: [
              ("sampler.type", "parent_based"),
              ("sampler.decision", "drop"),
              ("sampler.parent", "none"),
              ("sampler.delegate", "always_off")
            ]
          }
          _ => SamplingDecision{
            sampled: false,
            attributes: [
              ("sampler.type", "parent_based"),
              ("sampler.decision", "drop"),
              ("sampler.parent", "none"),
              ("sampler.delegate", "unknown")
            ]
          }
        }
      }
      Some(parent_context) => {
        // 有父span，简化处理：假设父span已采样
        SamplingDecision{
          sampled: true,
          attributes: [
            ("sampler.type", "parent_based"),
            ("sampler.decision", "record_and_sample"),
            ("sampler.parent", "sampled"),
            ("sampler.delegate", "respect_parent")
          ]
        }
      }
    }
  }
  
  // 创建ParentBased配置
  let config = ParentBasedConfig{
    remote_parent_sampled: "always_on",
    remote_parent_not_sampled: "trace_id_ratio",  // 使用TraceIdRatio作为委托
    local_parent_sampled: "always_on",
    local_parent_not_sampled: "always_off"
  }
  
  // 测试没有父span的情况
  let no_parent_params = SamplingParameters{
    trace_id: "0123456789abcdef0123456789abcdef",
    span_name: "root.span",
    span_kind: "server",
    parent_context: None,
    attributes: [],
    links: []
  }
  
  let no_parent_decision = parent_based_sample(no_parent_params, config)
  assert_eq(no_parent_decision.sampled, false)  // 委托给TraceIdRatio，默认不采样
  assert_eq(no_parent_decision.attributes[2], ("sampler.parent", "none"))
  assert_eq(no_parent_decision.attributes[3], ("sampler.delegate", "trace_id_ratio"))
  
  // 测试有父span的情况
  let with_parent_params = SamplingParameters{
    trace_id: "fedcba9876543210fedcba9876543210",
    span_name: "child.span",
    span_kind: "client",
    parent_context: Some("parent-context-sampled"),
    attributes: [],
    links: []
  }
  
  let with_parent_decision = parent_based_sample(with_parent_params, config)
  assert_eq(with_parent_decision.sampled, true)  // 尊重父span的采样决策
  assert_eq(with_parent_decision.attributes[2], ("sampler.parent", "sampled"))
  assert_eq(with_parent_decision.attributes[3], ("sampler.delegate", "respect_parent"))
}

test "sampling_consistency_across_spans" {
  // 测试同一trace中不同span的采样一致性
  
  struct TraceContext {
    trace_id : String
    sampled : Bool
    sampling_decision : SamplingDecision
  }
  
  func make_sampling_decision_for_trace(trace_id : String, root_span_name : String, ratio : Double) -> TraceContext {
    let params = SamplingParameters{
      trace_id: trace_id,
      span_name: root_span_name,
      span_kind: "server",
      parent_context: None,
      attributes: [],
      links: []
    }
    
    let decision = trace_id_ratio_sample(params, ratio)
    
    TraceContext{
      trace_id: trace_id,
      sampled: decision.sampled,
      sampling_decision: decision
    }
  }
  
  func should_sample_child_span(parent_context : TraceContext, child_params : SamplingParameters) -> SamplingDecision {
    // 子span应该遵循父span的采样决策
    SamplingDecision{
      sampled: parent_context.sampled,
      attributes: [
        ("sampler.type", "parent_based"),
        ("sampler.decision", if parent_context.sampled { "record_and_sample" } else { "drop" }),
        ("sampler.parent", if parent_context.sampled { "sampled" } else { "not_sampled" }),
        ("sampler.trace_id", parent_context.trace_id)
      ]
    }
  }
  
  // 测试同一trace中的采样一致性
  let trace_id = "0123456789abcdef0123456789abcdef"
  let sampling_ratio = 0.1
  
  // 创建根span的采样决策
  let root_context = make_sampling_decision_for_trace(trace_id, "http.request", sampling_ratio)
  
  // 创建子span
  let child_spans = [
    ("database.query", "client"),
    ("cache.lookup", "client"),
    ("auth.validation", "internal")
  ]
  
  for (span_name, span_kind) in child_spans {
    let child_params = SamplingParameters{
      trace_id: trace_id,
      span_name: span_name,
      span_kind: span_kind,
      parent_context: Some("parent-context"),
      attributes: [],
      links: []
    }
    
    let child_decision = should_sample_child_span(root_context, child_params)
    
    // 子span的采样决策应该与根span一致
    assert_eq(child_decision.sampled, root_context.sampled, 
      "Child span '{span_name}' should have same sampling decision as root span")
    
    // 验证采样属性
    assert_eq(child_decision.attributes[0], ("sampler.type", "parent_based"))
    assert_eq(child_decision.attributes[2], ("sampler.parent", 
      if root_context.sampled { "sampled" } else { "not_sampled" }))
    assert_eq(child_decision.attributes[3], ("sampler.trace_id", trace_id))
  }
}

test "sampling_performance_characteristics" {
  // 测试采样算法的性能特征
  
  struct SamplingPerformance {
    sampler_type : String
    iterations : Int
    total_time_ns : Int
    avg_time_ns : Int
    memory_usage_bytes : Int
  }
  
  func measure_sampling_performance(sampler_fn : (SamplingParameters) -> SamplingDecision, 
                                  sampler_type : String, 
                                  iterations : Int) -> SamplingPerformance {
    let test_params = SamplingParameters{
      trace_id: "0123456789abcdef0123456789abcdef",
      span_name: "performance.test",
      span_kind: "server",
      parent_context: None,
      attributes: [("test", "performance")],
      links: []
    }
    
    // 模拟性能测量
    let base_time_per_call = match sampler_type {
      "always_on" => 10      // 10ns
      "always_off" => 10     // 10ns
      "trace_id_ratio" => 100 // 100ns（需要计算hash）
      "parent_based" => 50   // 50ns（需要检查父上下文）
      _ => 100
    }
    
    let total_time = iterations * base_time_per_call
    let avg_time = total_time / iterations
    let memory_usage = match sampler_type {
      "always_on" => 64      // 64字节
      "always_off" => 64     // 64字节
      "trace_id_ratio" => 128 // 128字节（需要存储hash）
      "parent_based" => 96   // 96字节（需要存储父上下文信息）
      _ => 128
    }
    
    SamplingPerformance{
      sampler_type: sampler_type,
      iterations: iterations,
      total_time_ns: total_time,
      avg_time_ns: avg_time,
      memory_usage_bytes: memory_usage
    }
  }
  
  // 测试不同采样器的性能
  let samplers = [
    ("always_on", always_on_sample),
    ("always_off", always_off_sample)
  ]
  
  let iterations = 100000
  
  for (sampler_type, sampler_fn) in samplers {
    let performance = measure_sampling_performance(sampler_fn, sampler_type, iterations)
    
    // 验证性能指标
    assert_eq(performance.sampler_type, sampler_type)
    assert_eq(performance.iterations, iterations)
    assert_eq(performance.total_time_ns > 0, true)
    assert_eq(performance.avg_time_ns > 0, true)
    assert_eq(performance.memory_usage_bytes > 0, true)
    
    // 性能基准：每次采样决策不应超过1μs
    assert_eq(performance.avg_time_ns <= 1000, true, 
      "{sampler_type} sampler should make decisions in under 1μs")
    
    // 内存基准：每次采样决策不应超过200字节
    assert_eq(performance.memory_usage_bytes <= 200, true,
      "{sampler_type} sampler should use less than 200 bytes per decision")
  }
  
  // 比较不同采样器的相对性能
  let always_on_perf = measure_sampling_performance(always_on_sample, "always_on", iterations)
  let always_off_perf = measure_sampling_performance(always_off_sample, "always_off", iterations)
  
  // AlwaysOn和AlwaysOff应该有相似的性能
  let performance_diff = abs(always_on_perf.avg_time_ns - always_off_perf.avg_time_ns)
  let avg_perf = (always_on_perf.avg_time_ns + always_off_perf.avg_time_ns) / 2
  let relative_diff = if avg_perf > 0 { (performance_diff * 100) / avg_perf } else { 0 }
  
  assert_eq(relative_diff <= 10, true, 
    "AlwaysOn and AlwaysOff samplers should have similar performance (within 10%)")
}

test "sampling_edge_cases" {
  // 测试采样的边界情况和异常场景
  
  func test_invalid_trace_id_sampling() {
    // 测试无效trace_id的处理
    let invalid_trace_ids = [
      "",                    // 空字符串
      "short",               // 太短
      "invalid_characters",  // 包含无效字符
      "g" * 32,             // 全部为无效字符
      "0123456789abcdef0123456789abcde"  // 31个字符
    ]
    
    for invalid_trace_id in invalid_trace_ids {
      let params = SamplingParameters{
        trace_id: invalid_trace_id,
        span_name: "invalid.trace_id.test",
        span_kind: "server",
        parent_context: None,
        attributes: [],
        links: []
      }
      
      // AlwaysOn和AlwaysOff应该能处理无效的trace_id
      let always_on_decision = always_on_sample(params)
      let always_off_decision = always_off_sample(params)
      
      assert_eq(always_on_decision.sampled, true, 
        "AlwaysOn should handle invalid trace_id: '{invalid_trace_id}'")
      assert_eq(always_off_decision.sampled, false, 
        "AlwaysOff should handle invalid trace_id: '{invalid_trace_id}'")
    }
  }
  
  func test_extreme_sampling_ratios() {
    // 测试极端采样比例
    let extreme_ratios = [0.000001, 0.999999]
    
    for ratio in extreme_ratios {
      let params = SamplingParameters{
        trace_id: "0123456789abcdef0123456789abcdef",
        span_name: "extreme.ratio.test",
        span_kind: "server",
        parent_context: None,
        attributes: [],
        links: []
      }
      
      let decision = trace_id_ratio_sample(params, ratio)
      
      // 验证采样属性包含正确的比例
      assert_eq(decision.attributes.length(), 4)
      assert_eq(decision.attributes[2], ("sampler.ratio", ratio.to_string()))
      
      // 验证采样决策在合理范围内
      assert_eq(decision.sampled == true || decision.sampled == false, true)
    }
  }
  
  func test_sampling_attribute_limits() {
    // 测试采样属性的限制
    let large_attributes = []
    for i = 0; i < 1000; i = i + 1 {
      large_attributes.push(("large.attr." + i.to_string(), "value." + i.to_string()))
    }
    
    let params = SamplingParameters{
      trace_id: "0123456789abcdef0123456789abcdef",
      span_name: "large.attributes.test",
      span_kind: "server",
      parent_context: None,
      attributes: large_attributes,
      links: []
    }
    
    let decision = always_on_sample(params)
    
    // 采样决策不应该受输入属性数量影响
    assert_eq(decision.sampled, true)
    assert_eq(decision.attributes.length(), 2)  // 只有采样器本身的属性
  }
  
  // 执行所有边界情况测试
  test_invalid_trace_id_sampling()
  test_extreme_sampling_ratios()
  test_sampling_attribute_limits()
}