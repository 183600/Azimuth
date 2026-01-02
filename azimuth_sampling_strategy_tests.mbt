// Azimuth Sampling Strategy Tests
// 采样策略优化测试用例 - 专注于遥测数据的高效采样和自适应策略

// Test 1: 基础概率采样测试
test "basic probability sampling" {
  // 创建概率采样器
  let probability_sampler = ProbabilitySampler::new()
  
  // 配置采样率
  let sampling_config = SamplingConfig::new()
  SamplingConfig::set_sampling_rate(sampling_config, 0.1) // 10%采样率
  
  ProbabilitySampler::configure(probability_sampler, sampling_config)
  
  // 创建测试数据
  let test_data = []
  for i in 0..<10000 {
    let telemetry_data = TelemetryData::new()
    TelemetryData::add_metric(telemetry_data, "response_time", 100.0 + (i % 200).to_float())
    TelemetryData::add_attribute(telemetry_data, "service", "test_service")
    TelemetryData::add_attribute(telemetry_data, "trace_id", "trace_" + i.to_string())
    
    test_data = test_data.push(telemetry_data)
  }
  
  // 应用采样
  let sampled_data = []
  let sampled_count = 0
  
  for data in test_data {
    let should_sample = ProbabilitySampler::should_sample(probability_sampler, data)
    if should_sample {
      sampled_data = sampled_data.push(data)
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证采样结果
  let actual_sampling_rate = sampled_count.to_float() / test_data.length().to_float()
  assert_true(actual_sampling_rate > 0.05 && actual_sampling_rate < 0.15) // 允许50%误差
  
  // 验证采样数据代表性
  if sampled_data.length() > 0 {
    let original_avg = test_data.reduce(0.0, fn(acc, data) { 
      acc + TelemetryData::get_metric(data, "response_time").unwrap_or(0.0) 
    }) / test_data.length().to_float()
    
    let sampled_avg = sampled_data.reduce(0.0, fn(acc, data) { 
      acc + TelemetryData::get_metric(data, "response_time").unwrap_or(0.0) 
    }) / sampled_data.length().to_float()
    
    // 采样平均值应该接近原始平均值
    let difference = Math::abs(original_avg - sampled_avg)
    assert_true(difference < original_avg * 0.1) // 差异应小于10%
  }
  
  // 测试不同采样率
  let sampling_rates = [0.01, 0.05, 0.1, 0.2, 0.5]
  
  for rate in sampling_rates {
    SamplingConfig::set_sampling_rate(sampling_config, rate)
    ProbabilitySampler::reconfigure(probability_sampler, sampling_config)
    
    let mut rate_sampled_count = 0
    for data in test_data {
      if ProbabilitySampler::should_sample(probability_sampler, data) {
        rate_sampled_count = rate_sampled_count + 1
      }
    }
    
    let actual_rate = rate_sampled_count.to_float() / test_data.length().to_float()
    assert_true(actual_rate > rate * 0.5 && actual_rate < rate * 1.5) // 允许50%误差
  }
  
  ProbabilitySampler::cleanup(probability_sampler)
  
  // 清理测试数据
  for data in test_data {
    TelemetryData::cleanup(data)
  }
}

// Test 2: 自适应采样策略测试
test "adaptive sampling strategy" {
  // 创建自适应采样器
  let adaptive_sampler = AdaptiveSampler::new()
  
  // 配置自适应采样策略
  let adaptive_config = AdaptiveSamplingConfig::new()
  AdaptiveSamplingConfig::set_target_throughput(adaptive_config, 1000) // 目标每秒1000个样本
  AdaptiveSamplingConfig::set_min_sampling_rate(adaptive_config, 0.01) // 最小1%采样率
  AdaptiveSamplingConfig::set_max_sampling_rate(adaptive_config, 0.5) // 最大50%采样率
  AdaptiveSamplingConfig::set_adjustment_interval(adaptive_config, 5000) // 5秒调整间隔
  
  AdaptiveSampler::configure(adaptive_sampler, adaptive_config)
  
  // 创建模拟数据流
  let data_stream = MockDataStream::new()
  
  // 生成不同负载的数据
  let load_scenarios = [
    {"duration": 5000, "rate": 100, "name": "low_load"},    // 每秒100个事件
    {"duration": 5000, "rate": 1000, "name": "medium_load"}, // 每秒1000个事件
    {"duration": 5000, "rate": 5000, "name": "high_load"}    // 每秒5000个事件
  ]
  
  let sampling_results = []
  let current_time = Time::now()
  
  for scenario in load_scenarios {
    // 设置数据流速率
    MockDataStream::set_rate(data_stream, scenario.rate)
    
    // 记录开始采样率
    let start_sampling_rate = AdaptiveSampler::get_current_sampling_rate(adaptive_sampler)
    
    // 处理数据流
    let start_time = Time::now()
    let mut events_processed = 0
    let mut events_sampled = 0
    
    while Time::now() - start_time < scenario.duration {
      let events = MockDataStream::get_events(data_stream, 100) // 获取100个事件
      
      for event in events {
        events_processed = events_processed + 1
        
        if AdaptiveSampler::should_sample(adaptive_sampler, event) {
          events_sampled = events_sampled + 1
        }
      }
      
      Time::sleep(100) // 100ms间隔
    }
    
    // 记录结束采样率
    let end_sampling_rate = AdaptiveSampler::get_current_sampling_rate(adaptive_sampler)
    
    // 计算实际采样率
    let actual_sampling_rate = events_sampled.to_float() / events_processed.to_float()
    let actual_throughput = events_sampled.to_float() / (scenario.duration / 1000.0)
    
    sampling_results = sampling_results.push({
      "scenario": scenario.name,
      "input_rate": scenario.rate,
      "events_processed": events_processed,
      "events_sampled": events_sampled,
      "start_sampling_rate": start_sampling_rate,
      "end_sampling_rate": end_sampling_rate,
      "actual_sampling_rate": actual_sampling_rate,
      "actual_throughput": actual_throughput
    })
  }
  
  // 验证自适应采样结果
  assert_true(sampling_results.length() == load_scenarios.length())
  
  for result in sampling_results {
    // 验证采样率在配置范围内
    assert_true(result.actual_sampling_rate >= 0.01 && result.actual_sampling_rate <= 0.5)
    
    // 验证吞吐量接近目标
    if result.scenario == "medium_load" {
      // 中等负载下应该接近目标吞吐量
      assert_true(result.actual_throughput > 800 && result.actual_throughput < 1200)
    }
  }
  
  // 验证采样率随负载变化
  let low_load_result = sampling_results.find(fn(r) { r.scenario == "low_load" })
  let high_load_result = sampling_results.find(fn(r) { r.scenario == "high_load" })
  
  match (low_load_result, high_load_result) {
    (Some(low), Some(high)) => {
      // 高负载下采样率应该降低
      assert_true(high.actual_sampling_rate < low.actual_sampling_rate)
    }
    _ => assert_true(false)
  }
  
  AdaptiveSampler::cleanup(adaptive_sampler)
  MockDataStream::cleanup(data_stream)
}

// Test 3: 基于优先级的采样测试
test "priority-based sampling" {
  // 创建优先级采样器
  let priority_sampler = PrioritySampler::new()
  
  // 配置优先级规则
  let priority_rules = [
    {
      "name": "error_priority",
      "condition": {"attribute": "error", "operator": "!=", "value": ""},
      "priority": 10, // 最高优先级
      "sampling_rate": 1.0 // 100%采样
    },
    {
      "name": "high_latency_priority",
      "condition": {"metric": "response_time", "operator": ">", "value": 1000.0},
      "priority": 8,
      "sampling_rate": 0.8 // 80%采样
    },
    {
      "name": "critical_service_priority",
      "condition": {"attribute": "service", "operator": "==", "value": "payment"},
      "priority": 7,
      "sampling_rate": 0.5 // 50%采样
    },
    {
      "name": "default_priority",
      "condition": {"attribute": "any", "operator": "==", "value": "any"},
      "priority": 1,
      "sampling_rate": 0.1 // 10%采样
    }
  ]
  
  // 注册优先级规则
  for rule in priority_rules {
    let rule_result = PrioritySampler::add_rule(priority_sampler, rule)
    match rule_result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  // 创建不同优先级的测试数据
  let test_data = [
    // 错误事件（最高优先级）
    {
      "response_time": 200.0,
      "service": "web",
      "error": "timeout",
      "expected_priority": 10,
      "expected_sampling_rate": 1.0
    },
    // 高延迟事件
    {
      "response_time": 1500.0,
      "service": "api",
      "error": "",
      "expected_priority": 8,
      "expected_sampling_rate": 0.8
    },
    // 关键服务事件
    {
      "response_time": 300.0,
      "service": "payment",
      "error": "",
      "expected_priority": 7,
      "expected_sampling_rate": 0.5
    },
    // 普通事件
    {
      "response_time": 100.0,
      "service": "cache",
      "error": "",
      "expected_priority": 1,
      "expected_sampling_rate": 0.1
    },
    // 高延迟关键服务事件（应该匹配高延迟规则，优先级更高）
    {
      "response_time": 1200.0,
      "service": "payment",
      "error": "",
      "expected_priority": 8,
      "expected_sampling_rate": 0.8
    }
  ]
  
  // 测试每个类型的采样
  for test_case in test_data {
    let telemetry_data = TelemetryData::new()
    TelemetryData::add_metric(telemetry_data, "response_time", test_case.response_time)
    TelemetryData::add_attribute(telemetry_data, "service", test_case.service)
    TelemetryData::add_attribute(telemetry_data, "error", test_case.error)
    
    // 获取匹配的规则
    let matched_rule = PrioritySampler::get_matched_rule(priority_sampler, telemetry_data)
    match matched_rule {
      Some(rule) => {
        assert_eq(rule.priority, test_case.expected_priority)
        assert_eq(rule.sampling_rate, test_case.expected_sampling_rate)
      }
      None => assert_true(false)
    }
    
    // 测试采样决策
    let mut sampled_count = 0
    for i in 0..<100 {
      if PrioritySampler::should_sample(priority_sampler, telemetry_data) {
        sampled_count = sampled_count + 1
      }
    }
    
    let actual_sampling_rate = sampled_count.to_float() / 100.0
    assert_true(actual_sampling_rate > test_case.expected_sampling_rate * 0.7 && 
               actual_sampling_rate < test_case.expected_sampling_rate * 1.3) // 允许30%误差
    
    TelemetryData::cleanup(telemetry_data)
  }
  
  // 测试规则优先级
  let rules = PrioritySampler::get_rules(priority_sampler)
  let sorted_rules = rules.sort(fn(a, b) { b.priority - a.priority })
  
  // 验证规则按优先级排序
  for i in 0..<sorted_rules.length() - 1 {
    assert_true(sorted_rules[i].priority >= sorted_rules[i + 1].priority)
  }
  
  PrioritySampler::cleanup(priority_sampler)
}

// Test 4: 基于内容的智能采样测试
test "content-based intelligent sampling" {
  // 创建智能采样器
  let intelligent_sampler = IntelligentSampler::new()
  
  // 配置智能采样策略
  let intelligent_config = IntelligentSamplingConfig::new()
  IntelligentSamplingConfig::set_learning_algorithm(intelligent_config, "clustering")
  IntelligentSamplingConfig::set_diversity_threshold(intelligent_config, 0.8)
  IntelligentSamplingConfig::set_sample_pool_size(intelligent_config, 1000)
  
  IntelligentSampler::configure(intelligent_sampler, intelligent_config)
  
  // 创建多样化的测试数据
  let test_data = []
  
  // 生成不同簇的数据
  let clusters = [
    {"center": [100.0, 50.0], "std_dev": [10.0, 5.0], "count": 500, "service": "web"},
    {"center": [300.0, 80.0], "std_dev": [15.0, 8.0], "count": 300, "service": "api"},
    {"center": [500.0, 30.0], "std_dev": [20.0, 10.0], "count": 200, "service": "db"}
  ]
  
  for cluster in clusters {
    for i in 0..<cluster.count {
      let telemetry_data = TelemetryData::new()
      
      // 基于簇中心生成数据
      let response_time = cluster.center[0] + NormalDistribution::sample(0.0, cluster.std_dev[0])
      let cpu_usage = cluster.center[1] + NormalDistribution::sample(0.0, cluster.std_dev[1])
      
      TelemetryData::add_metric(telemetry_data, "response_time", Math::max(0.0, response_time))
      TelemetryData::add_metric(telemetry_data, "cpu_usage", Math::max(0.0, Math::min(100.0, cpu_usage)))
      TelemetryData::add_attribute(telemetry_data, "service", cluster.service)
      
      test_data = test_data.push(telemetry_data)
    }
  }
  
  // 训练智能采样器
  let training_result = IntelligentSampler::train(intelligent_sampler, test_data)
  match training_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 应用智能采样
  let sampled_data = []
  let sampled_indices = []
  
  for i in 0..<test_data.length() {
    if IntelligentSampler::should_sample(intelligent_sampler, test_data[i]) {
      sampled_data = sampled_data.push(test_data[i])
      sampled_indices = sampled_indices.push(i)
    }
  }
  
  // 验证智能采样结果
  assert_true(sampled_data.length() > 0)
  assert_true(sampled_data.length() < test_data.length()) // 应该采样一部分
  
  // 验证采样多样性
  let sampled_services = []
  for data in sampled_data {
    let service = TelemetryData::get_attribute(data, "service")
    match service {
      Some(StringValue(s)) => {
        if not(sampled_services.contains(s)) {
          sampled_services = sampled_services.push(s)
        }
      }
      _ => ()
    }
  }
  
  // 应该包含所有服务
  assert_true(sampled_services.length() == 3)
  
  // 验证采样代表性
  let original_clusters = analyze_clusters(test_data)
  let sampled_clusters = analyze_clusters(sampled_data)
  
  // 采样后的簇分布应该与原始分布相似
  for i in 0..<original_clusters.length() {
    assert_true(sampled_clusters[i].count > 0)
    
    let original_ratio = original_clusters[i].count.to_float() / test_data.length().to_float()
    let sampled_ratio = sampled_clusters[i].count.to_float() / sampled_data.length().to_float()
    
    // 采样比例应该相似
    assert_true(Math::abs(original_ratio - sampled_ratio) < 0.1) // 差异应小于10%
  }
  
  // 测试增量学习
  let new_data = []
  for i in 0..<100 {
    let telemetry_data = TelemetryData::new()
    TelemetryData::add_metric(telemetry_data, "response_time", 400.0 + (i % 100).to_float())
    TelemetryData::add_metric(telemetry_data, "cpu_usage", 60.0 + (i % 40).to_float())
    TelemetryData::add_attribute(telemetry_data, "service", "cache")
    
    new_data = new_data.push(telemetry_data)
  }
  
  // 增量更新模型
  let update_result = IntelligentSampler::update_model(intelligent_sampler, new_data)
  match update_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 验证新数据被正确采样
  let new_sampled_count = new_data.reduce(0, fn(acc, data) {
    if IntelligentSampler::should_sample(intelligent_sampler, data) {
      acc + 1
    } else {
      acc
    }
  })
  
  assert_true(new_sampled_count > 0)
  
  IntelligentSampler::cleanup(intelligent_sampler)
  
  // 清理测试数据
  for data in test_data {
    TelemetryData::cleanup(data)
  }
  for data in new_data {
    TelemetryData::cleanup(data)
  }
}

// Test 5: 多级采样策略测试
test "multi-level sampling strategy" {
  // 创建多级采样器
  let multi_level_sampler = MultiLevelSampler::new()
  
  // 配置多级采样策略
  let level_configs = [
    {
      "level": 1,
      "name": "head_sampler",
      "type": "probability",
      "config": {"sampling_rate": 0.5}, // 50%采样率
      "description": "第一级采样，减少数据量"
    },
    {
      "level": 2,
      "name": "attribute_sampler",
      "type": "attribute_based",
      "config": {
        "rules": [
          {"attribute": "service", "value": "payment", "sampling_rate": 0.8},
          {"attribute": "service", "value": "auth", "sampling_rate": 0.6},
          {"default": 0.3}
        ]
      },
      "description": "第二级采样，基于属性"
    },
    {
      "level": 3,
      "name": "tail_sampler",
      "type": "adaptive",
      "config": {"target_throughput": 100, "min_rate": 0.1, "max_rate": 0.9},
      "description": "第三级采样，自适应调整"
    }
  ]
  
  // 添加采样级别
  for config in level_configs {
    let level_result = MultiLevelSampler::add_level(multi_level_sampler, config)
    match level_result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  // 创建测试数据
  let test_data = []
  let services = ["payment", "auth", "web", "api", "db"]
  
  for i in 0..<1000 {
    let telemetry_data = TelemetryData::new()
    TelemetryData::add_metric(telemetry_data, "response_time", 100.0 + (i % 500).to_float())
    TelemetryData::add_attribute(telemetry_data, "service", services[i % services.length()])
    TelemetryData::add_attribute(telemetry_data, "trace_id", "trace_" + i.to_string())
    
    test_data = test_data.push(telemetry_data)
  }
  
  // 应用多级采样
  let mut level_results = []
  
  for i in 0..<test_data.length() {
    let mut current_data = test_data[i]
    let mut sampled_at_levels = []
    
    for level in 1..=3 {
      let should_sample = MultiLevelSampler::should_sample_at_level(multi_level_sampler, current_data, level)
      
      if should_sample {
        sampled_at_levels = sampled_at_levels.push(level)
      } else {
        break // 如果在当前级别被拒绝，不再继续处理
      }
    }
    
    if sampled_at_levels.length() > 0 {
      level_results = level_results.push({
        "data_index": i,
        "sampled_levels": sampled_at_levels,
        "final_sampled": true
      })
    }
  }
  
  // 验证多级采样结果
  let final_sampled_count = level_results.length()
  let overall_sampling_rate = final_sampled_count.to_float() / test_data.length().to_float()
  
  // 整体采样率应该小于第一级采样率
  assert_true(overall_sampling_rate < 0.5)
  
  // 验证各级别采样效果
  let level_1_sampled = level_results.filter(fn(r) { r.sampled_levels.contains(1) }).length()
  let level_2_sampled = level_results.filter(fn(r) { r.sampled_levels.contains(2) }).length()
  let level_3_sampled = level_results.filter(fn(r) { r.sampled_levels.contains(3) }).length()
  
  // 每一级采样数量应该递减
  assert_true(level_1_sampled >= level_2_sampled)
  assert_true(level_2_sampled >= level_3_sampled)
  
  // 验证服务特定采样
  let payment_data_indices = []
  let auth_data_indices = []
  
  for i in 0..<test_data.length() {
    let service = TelemetryData::get_attribute(test_data[i], "service")
    match service {
      Some(StringValue(s)) => {
        if s == "payment" {
          payment_data_indices = payment_data_indices.push(i)
        } else if s == "auth" {
          auth_data_indices = auth_data_indices.push(i)
        }
      }
      _ => ()
    }
  }
  
  let payment_sampled = payment_data_indices.filter(fn(i) { 
    level_results.any(fn(r) { r.data_index == i }) 
  }).length()
  
  let auth_sampled = auth_data_indices.filter(fn(i) { 
    level_results.any(fn(r) { r.data_index == i }) 
  }).length()
  
  // payment和auth服务应该有更高的采样率
  if payment_data_indices.length() > 0 && auth_data_indices.length() > 0 {
    let payment_rate = payment_sampled.to_float() / payment_data_indices.length().to_float()
    let auth_rate = auth_sampled.to_float() / auth_data_indices.length().to_float()
    
    assert_true(payment_rate > 0.3) // 应该高于默认30%
    assert_true(auth_rate > 0.3)   // 应该高于默认30%
  }
  
  // 测试采样统计
  let sampling_stats = MultiLevelSampler::get_sampling_stats(multi_level_sampler)
  
  assert_true(sampling_stats.level_stats.length() == 3)
  
  for level_stat in sampling_stats.level_stats {
    assert_true(level_stat.level >= 1 && level_stat.level <= 3)
    assert_true(level_stat.input_count > 0)
    assert_true(level_stat.output_count <= level_stat.input_count)
    assert_true(level_stat.sampling_rate >= 0.0 && level_stat.sampling_rate <= 1.0)
  }
  
  MultiLevelSampler::cleanup(multi_level_sampler)
  
  // 清理测试数据
  for data in test_data {
    TelemetryData::cleanup(data)
  }
}

// Test 6: 采样性能优化测试
test "sampling performance optimization" {
  // 创建性能优化采样器
  let performance_sampler = PerformanceOptimizedSampler::new()
  
  // 配置性能优化策略
  let perf_config = PerformanceSamplingConfig::new()
  PerformanceSamplingConfig::set_target_cpu_usage(perf_config, 10.0) // 目标CPU使用率10%
  PerformanceSamplingConfig::set_target_memory_usage(perf_config, 100.0) // 目标内存使用100MB
  PerformanceSamplingConfig::set_max_sampling_time(perf_config, 0.1) // 最大采样时间0.1ms
  PerformanceSamplingConfig::set_adaptive_rate(perf_config, true)
  
  PerformanceOptimizedSampler::configure(performance_sampler, perf_config)
  
  // 创建大量测试数据
  let large_dataset = []
  for i in 0..<100000 {
    let telemetry_data = TelemetryData::new()
    TelemetryData::add_metric(telemetry_data, "response_time", 100.0 + (i % 500).to_float())
    TelemetryData::add_metric(telemetry_data, "cpu_usage", 30.0 + (i % 70).to_float())
    TelemetryData::add_attribute(telemetry_data, "service", "service_" + (i % 10).to_string())
    
    large_dataset = large_dataset.push(telemetry_data)
  }
  
  // 测试采样性能
  let start_time = Time::now()
  let start_memory = MemoryMonitor::get_current_usage()
  
  let sampled_data = []
  for data in large_dataset {
    if PerformanceOptimizedSampler::should_sample(performance_sampler, data) {
      sampled_data = sampled_data.push(data)
    }
  }
  
  let end_time = Time::now()
  let end_memory = MemoryMonitor::get_current_usage()
  
  let total_sampling_time = end_time - start_time
  let memory_used = end_memory - start_memory
  let avg_sampling_time = total_sampling_time.to_float() / large_dataset.length().to_float()
  
  // 验证性能指标
  assert_true(avg_sampling_time < 0.1) // 平均采样时间应小于0.1ms
  assert_true(memory_used < 100 * 1024 * 1024) // 内存使用应小于100MB
  
  // 测试自适应性能调整
  let load_scenarios = [
    {"name": "light_load", "data_size": 10000, "expected_rate": 0.2},
    {"name": "medium_load", "data_size": 50000, "expected_rate": 0.1},
    {"name": "heavy_load", "data_size": 100000, "expected_rate": 0.05}
  ]
  
  let adaptive_results = []
  
  for scenario in load_scenarios {
    // 重置采样器状态
    PerformanceOptimizedSampler::reset(performance_sampler)
    
    let scenario_start_time = Time::now()
    let scenario_sampled_count = 0
    
    for i in 0..<scenario.data_size {
      if PerformanceOptimizedSampler::should_sample(performance_sampler, large_dataset[i]) {
        scenario_sampled_count = scenario_sampled_count + 1
      }
    }
    
    let scenario_end_time = Time::now()
    let scenario_duration = scenario_end_time - scenario_start_time
    let actual_rate = scenario_sampled_count.to_float() / scenario.data_size.to_float()
    
    adaptive_results = adaptive_results.push({
      "scenario": scenario.name,
      "data_size": scenario.data_size,
      "sampled_count": scenario_sampled_count,
      "actual_rate": actual_rate,
      "duration": scenario_duration
    })
  }
  
  // 验证自适应调整
  for result in adaptive_results {
    // 采样率应该随负载增加而降低
    assert_true(result.actual_rate > 0.0)
    
    if result.scenario == "light_load" {
      assert_true(result.actual_rate > 0.1)
    } else if result.scenario == "heavy_load" {
      assert_true(result.actual_rate < 0.1)
    }
  }
  
  // 测试采样缓存优化
  let cache_config = SamplingCacheConfig::new()
  SamplingCacheConfig::set_cache_size(cache_config, 10000)
  SamplingCacheConfig::set_ttl(cache_config, 60000) // 1分钟TTL
  
  PerformanceOptimizedSampler::enable_cache(performance_sampler, cache_config)
  
  // 测试缓存命中率
  let cache_test_data = []
  for i in 0..<1000 {
    let telemetry_data = TelemetryData::new()
    TelemetryData::add_metric(telemetry_data, "response_time", 100.0 + (i % 100).to_float())
    TelemetryData::add_attribute(telemetry_data, "service", "service_" + (i % 10).to_string())
    
    cache_test_data = cache_test_data.push(telemetry_data)
  }
  
  // 第一轮采样（填充缓存）
  for data in cache_test_data {
    PerformanceOptimizedSampler::should_sample(performance_sampler, data)
  }
  
  // 第二轮采样（使用缓存）
  let cache_start_time = Time::now()
  for data in cache_test_data {
    PerformanceOptimizedSampler::should_sample(performance_sampler, data)
  }
  let cache_end_time = Time::now()
  
  let cached_sampling_time = (cache_end_time - cache_start_time).to_float() / cache_test_data.length().to_float()
  
  // 缓存采样应该更快
  assert_true(cached_sampling_time < avg_sampling_time * 0.8)
  
  // 验证缓存统计
  let cache_stats = PerformanceOptimizedSampler::get_cache_stats(performance_sampler)
  assert_true(cache_stats.hit_rate > 0.5) // 命中率应该大于50%
  
  PerformanceOptimizedSampler::cleanup(performance_sampler)
  
  // 清理测试数据
  for data in large_dataset {
    TelemetryData::cleanup(data)
  }
  for data in cache_test_data {
    TelemetryData::cleanup(data)
  }
}

// Test 7: 采样一致性测试
test "sampling consistency" {
  // 创建一致性采样器
  let consistent_sampler = ConsistentSampler::new()
  
  // 配置一致性采样策略
  let consistent_config = ConsistentSamplingConfig::new()
  ConsistentSamplingConfig::set_sampling_rate(consistent_config, 0.1) // 10%采样率
  ConsistentSamplingConfig::set_salt(consistent_config, "azimuth_sampling_salt")
  ConsistentSamplingConfig::set_attribute_based(consistent_config, "trace_id")
  
  ConsistentSampler::configure(consistent_sampler, consistent_config)
  
  // 创建测试数据
  let test_data = []
  for i in 0..<1000 {
    let telemetry_data = TelemetryData::new()
    TelemetryData::add_metric(telemetry_data, "response_time", 100.0 + (i % 500).to_float())
    TelemetryData::add_attribute(telemetry_data, "trace_id", "trace_" + i.to_string())
    TelemetryData::add_attribute(telemetry_data, "service", "service_" + (i % 5).to_string())
    
    test_data = test_data.push(telemetry_data)
  }
  
  // 第一轮采样
  let first_round_sampled = []
  for data in test_data {
    if ConsistentSampler::should_sample(consistent_sampler, data) {
      first_round_sampled = first_round_sampled.push(data)
    }
  }
  
  // 第二轮采样（应该产生相同结果）
  let second_round_sampled = []
  for data in test_data {
    if ConsistentSampler::should_sample(consistent_sampler, data) {
      second_round_sampled = second_round_sampled.push(data)
    }
  }
  
  // 验证一致性
  assert_eq(first_round_sampled.length(), second_round_sampled.length())
  
  // 验证相同的数据被采样
  for i in 0..<first_round_sampled.length() {
    let first_trace_id = TelemetryData::get_attribute(first_round_sampled[i], "trace_id")
    let second_trace_id = TelemetryData::get_attribute(second_round_sampled[i], "trace_id")
    
    match (first_trace_id, second_trace_id) {
      (Some(StringValue(first)), Some(StringValue(second))) => {
        assert_eq(first, second)
      }
      _ => assert_true(false)
    }
  }
  
  // 测试不同采样器实例的一致性
  let another_sampler = ConsistentSampler::new()
  ConsistentSampler::configure(another_sampler, consistent_config)
  
  let third_round_sampled = []
  for data in test_data {
    if ConsistentSampler::should_sample(another_sampler, data) {
      third_round_sampled = third_round_sampled.push(data)
    }
  }
  
  // 不同实例应该产生相同结果
  assert_eq(first_round_sampled.length(), third_round_sampled.length())
  
  // 测试不同采样率的一致性
  let high_rate_config = ConsistentSamplingConfig::new()
  ConsistentSamplingConfig::set_sampling_rate(high_rate_config, 0.2) // 20%采样率
  ConsistentSamplingConfig::set_salt(high_rate_config, "azimuth_sampling_salt")
  ConsistentSamplingConfig::set_attribute_based(high_rate_config, "trace_id")
  
  ConsistentSampler::reconfigure(consistent_sampler, high_rate_config)
  
  let high_rate_sampled = []
  for data in test_data {
    if ConsistentSampler::should_sample(consistent_sampler, data) {
      high_rate_sampled = high_rate_sampled.push(data)
    }
  }
  
  // 高采样率应该包含低采样率的所有样本
  let first_round_trace_ids = []
  for data in first_round_sampled {
    let trace_id = TelemetryData::get_attribute(data, "trace_id")
    match trace_id {
      Some(StringValue(id)) => first_round_trace_ids = first_round_trace_ids.push(id)
      _ => ()
    }
  }
  
  let high_rate_trace_ids = []
  for data in high_rate_sampled {
    let trace_id = TelemetryData::get_attribute(data, "trace_id")
    match trace_id {
      Some(StringValue(id)) => high_rate_trace_ids = high_rate_trace_ids.push(id)
      _ => ()
    }
  }
  
  for trace_id in first_round_trace_ids {
    assert_true(high_rate_trace_ids.contains(trace_id))
  }
  
  // 测试基于属性的一致性
  let service_based_config = ConsistentSamplingConfig::new()
  ConsistentSamplingConfig::set_sampling_rate(service_based_config, 0.1)
  ConsistentSamplingConfig::set_salt(service_based_config, "azimuth_sampling_salt")
  ConsistentSamplingConfig::set_attribute_based(service_based_config, "service")
  
  ConsistentSampler::reconfigure(consistent_sampler, service_based_config)
  
  let service_based_sampled = []
  for data in test_data {
    if ConsistentSampler::should_sample(consistent_sampler, data) {
      service_based_sampled = service_based_sampled.push(data)
    }
  }
  
  // 验证基于服务的采样一致性
  let service_sample_groups = []
  
  for data in service_based_sampled {
    let service = TelemetryData::get_attribute(data, "service")
    match service {
      Some(StringValue(s)) => {
        if not(service_sample_groups.any(fn(g) { g.service == s })) {
          service_sample_groups = service_sample_groups.push({"service": s, "count": 0})
        }
        
        for i in 0..<service_sample_groups.length() {
          if service_sample_groups[i].service == s {
            service_sample_groups[i] = {
              "service": s,
              "count": service_sample_groups[i].count + 1
            }
            break
          }
        }
      }
      _ => ()
    }
  }
  
  // 每个服务的采样应该是一致的
  for group in service_sample_groups {
    assert_true(group.count > 0)
  }
  
  ConsistentSampler::cleanup(consistent_sampler)
  ConsistentSampler::cleanup(another_sampler)
  
  // 清理测试数据
  for data in test_data {
    TelemetryData::cleanup(data)
  }
}

// Test 8: 采样质量评估测试
test "sampling quality evaluation" {
  // 创建采样质量评估器
  let quality_evaluator = SamplingQualityEvaluator::new()
  
  // 创建测试数据集
  let full_dataset = []
  let services = ["web", "api", "db", "cache", "queue"]
  
  // 生成不同分布的数据
  for i in 0..<10000 {
    let telemetry_data = TelemetryData::new()
    
    // 不同的服务有不同的响应时间分布
    let service = services[i % services.length()]
    let response_time = match service {
      "web" => 50.0 + NormalDistribution::sample(0.0, 20.0),
      "api" => 100.0 + NormalDistribution::sample(0.0, 30.0),
      "db" => 200.0 + NormalDistribution::sample(0.0, 50.0),
      "cache" => 10.0 + NormalDistribution::sample(0.0, 5.0),
      "queue" => 30.0 + NormalDistribution::sample(0.0, 15.0),
      _ => 100.0
    }
    
    TelemetryData::add_metric(telemetry_data, "response_time", Math::max(0.0, response_time))
    TelemetryData::add_metric(telemetry_data, "cpu_usage", 30.0 + NormalDistribution::sample(0.0, 20.0))
    TelemetryData::add_attribute(telemetry_data, "service", service)
    TelemetryData::add_attribute(telemetry_data, "trace_id", "trace_" + i.to_string())
    
    full_dataset = full_dataset.push(telemetry_data)
  }
  
  // 测试不同采样策略的质量
  let sampling_strategies = [
    {
      "name": "uniform_probability",
      "sampler": ProbabilitySampler::new(),
      "config": SamplingConfig::with_rate(0.1)
    },
    {
      "name": "priority_based",
      "sampler": PrioritySampler::new(),
      "config": PrioritySamplingConfig::with_default_rules()
    },
    {
      "name": "adaptive",
      "sampler": AdaptiveSampler::new(),
      "config": AdaptiveSamplingConfig::with_defaults()
    }
  ]
  
  let quality_results = []
  
  for strategy in sampling_strategies {
    // 配置采样器
    match strategy.name {
      "uniform_probability" => {
        ProbabilitySampler::configure(strategy.sampler, strategy.config)
      }
      "priority_based" => {
        PrioritySampler::configure(strategy.sampler, strategy.config)
      }
      "adaptive" => {
        AdaptiveSampler::configure(strategy.sampler, strategy.config)
      }
      _ => ()
    }
    
    // 应用采样
    let sampled_dataset = []
    for data in full_dataset {
      let should_sample = match strategy.name {
        "uniform_probability" => ProbabilitySampler::should_sample(strategy.sampler, data),
        "priority_based" => PrioritySampler::should_sample(strategy.sampler, data),
        "adaptive" => AdaptiveSampler::should_sample(strategy.sampler, data),
        _ => false
      }
      
      if should_sample {
        sampled_dataset = sampled_dataset.push(data)
      }
    }
    
    // 评估采样质量
    let quality_metrics = SamplingQualityEvaluator::evaluate(
      quality_evaluator, 
      full_dataset, 
      sampled_dataset
    )
    
    quality_results = quality_results.push({
      "strategy": strategy.name,
      "sampling_rate": sampled_dataset.length().to_float() / full_dataset.length().to_float(),
      "bias_score": quality_metrics.bias_score,
      "variance_preservation": quality_metrics.variance_preservation,
      "distribution_similarity": quality_metrics.distribution_similarity,
      "error_metrics": quality_metrics.error_metrics
    })
    
    // 清理采样器
    match strategy.name {
      "uniform_probability" => ProbabilitySampler::cleanup(strategy.sampler),
      "priority_based" => PrioritySampler::cleanup(strategy.sampler),
      "adaptive" => AdaptiveSampler::cleanup(strategy.sampler),
      _ => ()
    }
  }
  
  // 验证质量评估结果
  assert_true(quality_results.length() == sampling_strategies.length())
  
  for result in quality_results {
    // 验证采样率
    assert_true(result.sampling_rate > 0.0 && result.sampling_rate < 1.0)
    
    // 验证偏差分数（0表示无偏差，1表示高偏差）
    assert_true(result.bias_score >= 0.0 && result.bias_score <= 1.0)
    
    // 验证方差保持（0表示无方差保持，1表示完全保持）
    assert_true(result.variance_preservation >= 0.0 && result.variance_preservation <= 1.0)
    
    // 验证分布相似性（0表示不相似，1表示完全相似）
    assert_true(result.distribution_similarity >= 0.0 && result.distribution_similarity <= 1.0)
    
    // 验证错误指标
    assert_true(result.error_metrics.mae >= 0.0) // 平均绝对误差
    assert_true(result.error_metrics.rmse >= 0.0) // 均方根误差
    assert_true(result.error_metrics.mape >= 0.0) // 平均绝对百分比误差
  }
  
  // 比较不同策略的质量
  let uniform_result = quality_results.find(fn(r) { r.strategy == "uniform_probability" })
  let priority_result = quality_results.find(fn(r) { r.strategy == "priority_based" })
  
  match (uniform_result, priority_result) {
    (Some(uniform), Some(priority)) => {
      // 均匀概率采样应该有较低的偏差
      assert_true(uniform.bias_score <= priority.bias_score)
    }
    _ => assert_true(false)
  }
  
  // 测试采样质量随采样率的变化
  let rate_quality_results = []
  let sampling_rates = [0.01, 0.05, 0.1, 0.2, 0.5]
  
  for rate in sampling_rates {
    let rate_sampler = ProbabilitySampler::new()
    let rate_config = SamplingConfig::with_rate(rate)
    ProbabilitySampler::configure(rate_sampler, rate_config)
    
    let rate_sampled_dataset = []
    for data in full_dataset {
      if ProbabilitySampler::should_sample(rate_sampler, data) {
        rate_sampled_dataset = rate_sampled_dataset.push(data)
      }
    }
    
    let rate_quality = SamplingQualityEvaluator::evaluate(
      quality_evaluator, 
      full_dataset, 
      rate_sampled_dataset
    )
    
    rate_quality_results = rate_quality_results.push({
      "sampling_rate": rate,
      "bias_score": rate_quality.bias_score,
      "variance_preservation": rate_quality.variance_preservation,
      "distribution_similarity": rate_quality.distribution_similarity
    })
    
    ProbabilitySampler::cleanup(rate_sampler)
  }
  
  // 验证采样率与质量的关系
  for i in 0..<rate_quality_results.length() - 1 {
    let current = rate_quality_results[i]
    let next = rate_quality_results[i + 1]
    
    // 更高的采样率应该有更好的方差保持和分布相似性
    assert_true(next.variance_preservation >= current.variance_preservation * 0.9)
    assert_true(next.distribution_similarity >= current.distribution_similarity * 0.9)
  }
  
  SamplingQualityEvaluator::cleanup(quality_evaluator)
  
  // 清理测试数据
  for data in full_dataset {
    TelemetryData::cleanup(data)
  }
}

// Test 9: 采样策略组合测试
test "sampling strategy composition" {
  // 创建组合采样器
  let composite_sampler = CompositeSampler::new()
  
  // 定义采样策略组合
  let composition_strategies = [
    {
      "name": "probability_then_priority",
      "description": "先概率采样，再优先级采样",
      "strategies": [
        {"type": "probability", "config": {"sampling_rate": 0.2}},
        {"type": "priority", "config": {"rules": [
          {"attribute": "service", "value": "payment", "priority": 10, "sampling_rate": 0.8},
          {"default": 0.3}
        ]}}
      ]
    },
    {
      "name": "priority_then_adaptive",
      "description": "先优先级采样，再自适应采样",
      "strategies": [
        {"type": "priority", "config": {"rules": [
          {"attribute": "error", "operator": "!=", "value": "", "priority": 10, "sampling_rate": 1.0},
          {"default": 0.2}
        ]}},
        {"type": "adaptive", "config": {"target_throughput": 100, "min_rate": 0.05, "max_rate": 0.5}}
      ]
    }
  ]
  
  // 注册组合策略
  for strategy in composition_strategies {
    let strategy_result = CompositeSampler::add_composite_strategy(composite_sampler, strategy)
    match strategy_result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  // 创建测试数据
  let test_data = []
  let services = ["web", "api", "payment", "auth", "db"]
  
  for i in 0..<5000 {
    let telemetry_data = TelemetryData::new()
    TelemetryData::add_metric(telemetry_data, "response_time", 100.0 + (i % 400).to_float())
    TelemetryData::add_attribute(telemetry_data, "service", services[i % services.length()])
    
    // 10%的事件有错误
    if i % 10 == 0 {
      TelemetryData::add_attribute(telemetry_data, "error", "timeout")
    }
    
    test_data = test_data.push(telemetry_data)
  }
  
  // 测试组合策略
  let composition_results = []
  
  for strategy in composition_strategies {
    // 应用组合策略
    let sampled_data = []
    for data in test_data {
      if CompositeSampler::should_sample_with_composite(composite_sampler, data, strategy.name) {
        sampled_data = sampled_data.push(data)
      }
    }
    
    // 分析采样结果
    let payment_data = sampled_data.filter(fn(data) {
      match TelemetryData::get_attribute(data, "service") {
        Some(StringValue(s)) => s == "payment",
        _ => false
      }
    })
    
    let error_data = sampled_data.filter(fn(data) {
      match TelemetryData::get_attribute(data, "error") {
        Some(StringValue(_)) => true,
        _ => false
      }
    })
    
    composition_results = composition_results.push({
      "strategy": strategy.name,
      "total_sampled": sampled_data.length(),
      "sampling_rate": sampled_data.length().to_float() / test_data.length().to_float(),
      "payment_sampled": payment_data.length(),
      "error_sampled": error_data.length()
    })
  }
  
  // 验证组合策略结果
  assert_true(composition_results.length() == composition_strategies.length())
  
  // 验证"probability_then_priority"策略
  let prob_priority_result = composition_results.find(fn(r) { r.strategy == "probability_then_priority" })
  match prob_priority_result {
    Some(result) => {
      // payment服务应该有更高的采样率
      let payment_rate = result.payment_sampled.to_float() / result.total_sampled.to_float()
      assert_true(payment_rate > 0.2) // 应该高于默认采样率
    }
    None => assert_true(false)
  }
  
  // 验证"priority_then_adaptive"策略
  let priority_adaptive_result = composition_results.find(fn(r) { r.strategy == "priority_then_adaptive" })
  match priority_adaptive_result {
    Some(result) => {
      // 错误事件应该有更高的采样率
      let error_rate = result.error_sampled.to_float() / result.total_sampled.to_float()
      assert_true(error_rate > 0.2) // 应该高于默认采样率
    }
    None => assert_true(false)
  }
  
  // 测试动态策略组合
  let dynamic_composition = {
    "name": "dynamic_composition",
    "description": "动态调整的组合策略",
    "strategies": [
      {"type": "probability", "config": {"sampling_rate": 0.1}},
      {"type": "adaptive", "config": {"target_throughput": 50}}
    ],
    "adjustment_rules": [
      {"condition": {"metric": "throughput", "operator": "<", "value": 30}, "action": {"type": "adjust_probability", "value": 0.2}},
      {"condition": {"metric": "error_rate", "operator": ">", "value": 0.1}, "action": {"type": "adjust_priority", "rules": [{"attribute": "error", "sampling_rate": 1.0}]}}
    ]
  }
  
  let dynamic_result = CompositeSampler::add_composite_strategy(composite_sampler, dynamic_composition)
  match dynamic_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 测试动态调整
  let dynamic_sampled_data = []
  for data in test_data {
    if CompositeSampler::should_sample_with_composite(composite_sampler, data, "dynamic_composition") {
      dynamic_sampled_data = dynamic_sampled_data.push(data)
    }
  }
  
  // 验证动态组合结果
  assert_true(dynamic_sampled_data.length() > 0)
  
  // 模拟低吞吐量情况
  CompositeSampler::simulate_condition(composite_sampler, "throughput", 25.0)
  
  let adjusted_sampled_data = []
  for data in test_data {
    if CompositeSampler::should_sample_with_composite(composite_sampler, data, "dynamic_composition") {
      adjusted_sampled_data = adjusted_sampled_data.push(data)
    }
  }
  
  // 调整后采样率应该增加
  assert_true(adjusted_sampled_data.length() > dynamic_sampled_data.length())
  
  CompositeSampler::cleanup(composite_sampler)
  
  // 清理测试数据
  for data in test_data {
    TelemetryData::cleanup(data)
  }
}

// Test 10: 采样策略A/B测试
test "sampling strategy A/B testing" {
  // 创建A/B测试框架
  let ab_test_framework = SamplingABTestFramework::new()
  
  // 定义A/B测试实验
  let ab_test_experiment = {
    "name": "sampling_strategy_comparison",
    "description": "比较不同采样策略的效果",
    "traffic_split": [
      {"strategy": "uniform_probability", "weight": 50, "config": {"sampling_rate": 0.1}},
      {"strategy": "adaptive_sampling", "weight": 50, "config": {"target_throughput": 100, "min_rate": 0.05}}
    ],
    "success_metrics": [
      {"name": "bias_score", "type": "lower_is_better"},
      {"name": "variance_preservation", "type": "higher_is_better"},
      {"name": "distribution_similarity", "type": "higher_is_better"}
    ],
    "duration": 86400000 // 24小时
  }
  
  // 创建A/B测试实验
  let experiment_result = SamplingABTestFramework::create_experiment(ab_test_framework, ab_test_experiment)
  match experiment_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 创建测试数据
  let test_data = []
  let services = ["web", "api", "db", "cache", "queue"]
  
  for i in 0..<10000 {
    let telemetry_data = TelemetryData::new()
    
    let service = services[i % services.length()]
    let response_time = match service {
      "web" => 50.0 + NormalDistribution::sample(0.0, 20.0),
      "api" => 100.0 + NormalDistribution::sample(0.0, 30.0),
      "db" => 200.0 + NormalDistribution::sample(0.0, 50.0),
      "cache" => 10.0 + NormalDistribution::sample(0.0, 5.0),
      "queue" => 30.0 + NormalDistribution::sample(0.0, 15.0),
      _ => 100.0
    }
    
    TelemetryData::add_metric(telemetry_data, "response_time", Math::max(0.0, response_time))
    TelemetryData::add_attribute(telemetry_data, "service", service)
    TelemetryData::add_attribute(telemetry_data, "trace_id", "trace_" + i.to_string())
    
    test_data = test_data.push(telemetry_data)
  }
  
  // 运行A/B测试
  let ab_test_results = SamplingABTestFramework::run_experiment(ab_test_framework, "sampling_strategy_comparison", test_data)
  
  // 验证A/B测试结果
  assert_true(ab_test_results.experiment_name == "sampling_strategy_comparison")
  assert_true(ab_test_results.variants.length() == 2)
  
  for variant in ab_test_results.variants {
    assert_true(variant.strategy.length() > 0)
    assert_true(variant.traffic_percentage > 0)
    assert_true(variant.sampled_count > 0)
    assert_true(variant.sampling_rate > 0.0 && variant.sampling_rate < 1.0)
    
    // 验证质量指标
    assert_true(variant.quality_metrics.bias_score >= 0.0 && variant.quality_metrics.bias_score <= 1.0)
    assert_true(variant.quality_metrics.variance_preservation >= 0.0 && variant.quality_metrics.variance_preservation <= 1.0)
    assert_true(variant.quality_metrics.distribution_similarity >= 0.0 && variant.quality_metrics.distribution_similarity <= 1.0)
  }
  
  // 验证流量分配
  let total_percentage = ab_test_results.variants.reduce(0, fn(acc, variant) { acc + variant.traffic_percentage })
  assert_true(total_percentage == 100)
  
  // 分析A/B测试结果
  let analysis_result = SamplingABTestFramework::analyze_results(ab_test_framework, "sampling_strategy_comparison")
  
  match analysis_result {
    Ok(analysis) => {
      assert_true(analysis.winner.length() > 0)
      assert_true(analysis.confidence > 0.0 && analysis.confidence <= 1.0)
      assert_true(analysis.significant_metrics.length() > 0)
      
      for metric in analysis.significant_metrics {
        assert_true(metric.name.length() > 0)
        assert_true(metric.p_value > 0.0 && metric.p_value <= 1.0)
        assert_true(metric.effect_size != 0.0)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试多变量A/B测试
  let multivariate_experiment = {
    "name": "multivariate_sampling_test",
    "description": "多变量采样策略测试",
    "traffic_split": [
      {"strategy": "uniform_low", "weight": 25, "config": {"sampling_rate": 0.05}},
      {"strategy": "uniform_medium", "weight": 25, "config": {"sampling_rate": 0.1}},
      {"strategy": "priority_based", "weight": 25, "config": {"rules": [{"attribute": "service", "value": "payment", "sampling_rate": 0.8}, {"default": 0.1}]}},
      {"strategy": "adaptive", "weight": 25, "config": {"target_throughput": 100, "min_rate": 0.05}}
    ],
    "success_metrics": [
      {"name": "bias_score", "type": "lower_is_better"},
      {"name": "variance_preservation", "type": "higher_is_better"}
    ],
    "duration": 86400000
  }
  
  let multivariate_result = SamplingABTestFramework::create_experiment(ab_test_framework, multivariate_experiment)
  match multivariate_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 运行多变量测试
  let multivariate_results = SamplingABTestFramework::run_experiment(ab_test_framework, "multivariate_sampling_test", test_data)
  
  // 验证多变量测试结果
  assert_true(multivariate_results.variants.length() == 4)
  
  for variant in multivariate_results.variants {
    assert_true(variant.traffic_percentage == 25) // 每个变体应该有25%的流量
  }
  
  // 分析多变量测试结果
  let multivariate_analysis = SamplingABTestFramework::analyze_results(ab_test_framework, "multivariate_sampling_test")
  
  match multivariate_analysis {
    Ok(analysis) => {
      assert_true(analysis.winner.length() > 0)
      assert_true(analysis.significant_metrics.length() > 0)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试A/B测试报告生成
  let report_result = SamplingABTestFramework::generate_report(ab_test_framework, "sampling_strategy_comparison")
  
  match report_result {
    Ok(report) => {
      assert_true(report.experiment_name == "sampling_strategy_comparison")
      assert_true(report.variants.length() == 2)
      assert_true(report.summary.winner.length() > 0)
      assert_true(report.summary.confidence > 0.0)
      assert_true(report.recommendations.length() > 0)
    }
    Err(_) => assert_true(false)
  }
  
  SamplingABTestFramework::cleanup(ab_test_framework)
  
  // 清理测试数据
  for data in test_data {
    TelemetryData::cleanup(data)
  }
}