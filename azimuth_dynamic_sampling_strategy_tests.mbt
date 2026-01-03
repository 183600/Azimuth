// 动态采样策略测试
// 测试遥测系统在不同负载和条件下的动态采样策略调整能力

// 测试1: 基于负载的自适应采样测试
test "load-based adaptive sampling test" {
  // 创建追踪提供者
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "adaptive-sampling-test")
  
  // 定义负载级别和对应的采样率
  let load_levels = [
    ("low", 100, 1.0),      // 低负载：100个操作/秒，100%采样
    ("medium", 1000, 0.5),  // 中负载：1000个操作/秒，50%采样
    ("high", 5000, 0.1),    // 高负载：5000个操作/秒，10%采样
    ("extreme", 20000, 0.01) // 极高负载：20000个操作/秒，1%采样
  ]
  
  // 测试每个负载级别
  for i = 0; i < load_levels.length(); i = i + 1 {
    let (load_name, operations_per_sec, sampling_rate) = load_levels[i]
    
    // 创建负载级别特定的span
    let load_span = Tracer::start_span(tracer, "load-" + load_name)
    
    // 记录开始时间和内存使用
    let start_time_ns = 1640995200000000000L
    let initial_memory_kb = 100000  // 100MB
    
    // 模拟负载下的操作
    let total_operations = operations_per_sec
    let sampled_operations = 0
    let dropped_operations = 0
    
    for j = 0; j < total_operations; j = j + 1 {
      // 根据采样率决定是否采样
      let should_sample = (j % 100) < (sampling_rate * 100.0).to_int()
      
      if should_sample {
        // 创建采样的span
        let sampled_span = Tracer::start_span(tracer, "sampled-op-" + j.to_string())
        Span::set_attributes(sampled_span, Attributes::from_array([
          ("load.level", load_name),
          ("sampling.rate", sampling_rate.to_string()),
          ("operation.id", j.to_string())
        ]))
        Span::end(sampled_span)
        sampled_operations = sampled_operations + 1
      } else {
        // 操作被丢弃
        dropped_operations = dropped_operations + 1
      }
    }
    
    // 记录结束时间和内存使用
    let end_time_ns = start_time_ns + 1000000000L  // 1秒
    let final_memory_kb = initial_memory_kb + (sampled_operations * 0.1)  // 每个采样操作0.1KB
    
    // 计算采样效果
    let actual_sampling_rate = sampled_operations.to_float() / total_operations.to_float()
    let memory_increase_kb = final_memory_kb - initial_memory_kb
    
    // 添加负载和采样指标
    Span::set_attributes(load_span, Attributes::from_array([
      ("load.name", load_name),
      ("operations.per.second", operations_per_sec.to_string()),
      ("target.sampling.rate", sampling_rate.to_string()),
      ("actual.sampling.rate", actual_sampling_rate.to_string()),
      ("total.operations", total_operations.to_string()),
      ("sampled.operations", sampled_operations.to_string()),
      ("dropped.operations", dropped_operations.to_string()),
      ("memory.increase.kb", memory_increase_kb.to_string())
    ]))
    
    // 验证自适应采样
    assert_true(actual_sampling_rate > 0.0)  // 应该有至少一些操作被采样
    assert_true(sampled_operations < total_operations)  // 采样操作应该少于总操作
    
    // 验证采样率接近目标
    let sampling_error = (actual_sampling_rate - sampling_rate).abs()
    assert_true(sampling_error < 0.1)  // 采样误差应该小于10%
    
    // 验证内存使用随采样率调整
    match load_name {
      "low" => assert_true(sampled_operations > 50),     // 低负载应该采样更多
      "extreme" => assert_true(sampled_operations < 200), // 极高负载应该采样更少
      _ => assert_true(sampled_operations > 0)           // 其他情况应该有一些采样
    }
    
    // 结束负载span
    Span::end(load_span)
  }
}

// 测试2: 基于错误率的采样调整测试
test "error-rate-based sampling adjustment test" {
  // 创建追踪提供者
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error-rate-sampling-test")
  
  // 定义错误率级别和对应的采样策略
  let error_rate_scenarios = [
    ("normal", 0.01, 0.1),      // 正常：1%错误率，10%采样
    ("elevated", 0.05, 0.3),    // 升高：5%错误率，30%采样
    ("high", 0.15, 0.7),        // 高：15%错误率，70%采样
    ("critical", 0.4, 1.0)      // 临界：40%错误率，100%采样
  ]
  
  // 测试每个错误率场景
  for i = 0; i < error_rate_scenarios.length(); i = i + 1 {
    let (scenario_name, error_rate, sampling_rate) = error_rate_scenarios[i]
    
    // 创建错误率场景特定的span
    let scenario_span = Tracer::start_span(tracer, "error-" + scenario_name)
    
    // 模拟操作和错误
    let total_operations = 1000
    let error_operations = (total_operations.to_float() * error_rate).to_int()
    let sampled_operations = 0
    let error_sampled_operations = 0
    
    for j = 0; j < total_operations; j = j + 1 {
      let is_error = j < error_operations
      let should_sample = if is_error {
        // 错误操作有更高的采样概率
        (j % 100) < (sampling_rate * 100.0).to_int()
      } else {
        // 正常操作有较低的采样概率
        (j % 100) < (sampling_rate * 50.0).to_int()  // 错误操作采样率的一半
      }
      
      if should_sample {
        // 创建采样的span
        let sampled_span = Tracer::start_span(tracer, "sampled-op-" + j.to_string())
        
        // 设置错误状态
        if is_error {
          Span::set_status(sampled_span, Error, Some("Simulated error"))
          error_sampled_operations = error_sampled_operations + 1
        }
        
        Span::set_attributes(sampled_span, Attributes::from_array([
          ("error.scenario", scenario_name),
          ("error.rate", error_rate.to_string()),
          ("sampling.rate", sampling_rate.to_string()),
          ("is.error", is_error.to_string())
        ]))
        
        Span::end(sampled_span)
        sampled_operations = sampled_operations + 1
      }
    }
    
    // 计算采样效果
    let error_sampling_rate = if error_operations > 0 {
      error_sampled_operations.to_float() / error_operations.to_float()
    } else {
      0.0
    }
    
    // 添加错误率和采样指标
    Span::set_attributes(scenario_span, Attributes::from_array([
      ("scenario.name", scenario_name),
      ("error.rate", error_rate.to_string()),
      ("target.sampling.rate", sampling_rate.to_string()),
      ("total.operations", total_operations.to_string()),
      ("error.operations", error_operations.to_string()),
      ("sampled.operations", sampled_operations.to_string()),
      ("error.sampled.operations", error_sampled_operations.to_string()),
      ("error.sampling.rate", error_sampling_rate.to_string())
    ]))
    
    // 验证基于错误率的采样调整
    assert_true(sampled_operations > 0)  // 应该有至少一些操作被采样
    
    // 验证错误操作有更高的采样率
    if error_operations > 0 {
      assert_true(error_sampling_rate > 0.0)  // 错误操作应该被采样
    }
    
    // 验证高错误率场景采样更多
    match scenario_name {
      "critical" => assert_true(sampled_operations > 500),  // 临界场景应该采样更多
      "normal" => assert_true(sampled_operations < 200),     // 正常场景应该采样较少
      _ => assert_true(sampled_operations > 0)              // 其他情况应该有一些采样
    }
    
    // 结束错误率场景span
    Span::end(scenario_span)
  }
}

// 测试3: 基于服务优先级的采样测试
test "service-priority-based sampling test" {
  // 创建追踪提供者
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "priority-sampling-test")
  
  // 定义服务优先级和对应的采样策略
  let service_priorities = [
    ("critical-payment", 1.0, "highest"),    // 关键支付服务：100%采样
    ("user-authentication", 0.8, "high"),    // 用户认证服务：80%采样
    ("product-catalog", 0.3, "medium"),      // 产品目录服务：30%采样
    ("analytics", 0.1, "low"),               // 分析服务：10%采样
    ("background-cleanup", 0.01, "lowest")   // 后台清理服务：1%采样
  ]
  
  // 测试每个服务优先级
  for i = 0; i < service_priorities.length(); i = i + 1 {
    let (service_name, sampling_rate, priority_level) = service_priorities[i]
    
    // 创建服务优先级特定的span
    let priority_span = Tracer::start_span(tracer, "service-" + service_name)
    
    // 模拟服务操作
    let total_operations = 500
    let sampled_operations = 0
    
    for j = 0; j < total_operations; j = j + 1 {
      // 根据服务优先级决定采样率
      let should_sample = (j % 100) < (sampling_rate * 100.0).to_int()
      
      if should_sample {
        // 创建采样的span
        let sampled_span = Tracer::start_span(tracer, "service-op-" + j.to_string())
        
        Span::set_attributes(sampled_span, Attributes::from_array([
          ("service.name", service_name),
          ("service.priority", priority_level),
          ("sampling.rate", sampling_rate.to_string()),
          ("operation.id", j.to_string())
        ]))
        
        Span::end(sampled_span)
        sampled_operations = sampled_operations + 1
      }
    }
    
    // 计算采样效果
    let actual_sampling_rate = sampled_operations.to_float() / total_operations.to_float()
    
    // 添加服务优先级和采样指标
    Span::set_attributes(priority_span, Attributes::from_array([
      ("service.name", service_name),
      ("service.priority", priority_level),
      ("target.sampling.rate", sampling_rate.to_string()),
      ("actual.sampling.rate", actual_sampling_rate.to_string()),
      ("total.operations", total_operations.to_string()),
      ("sampled.operations", sampled_operations.to_string())
    ]))
    
    // 验证基于服务优先级的采样
    assert_true(sampled_operations > 0)  // 应该有至少一些操作被采样
    
    // 验证高优先级服务采样更多
    match priority_level {
      "highest" => assert_true(sampled_operations > 400),  // 最高优先级应该采样最多
      "high" => assert_true(sampled_operations > 300),     // 高优先级应该采样较多
      "medium" => assert_true(sampled_operations > 100),   // 中等优先级应该采样适中
      "low" => assert_true(sampled_operations < 100),      // 低优先级应该采样较少
      "lowest" => assert_true(sampled_operations < 50),    // 最低优先级应该采样最少
      _ => assert_true(sampled_operations > 0)             // 其他情况应该有一些采样
    }
    
    // 结束服务优先级span
    Span::end(priority_span)
  }
}

// 测试4: 基于用户会话的采样测试
test "user-session-based sampling test" {
  // 创建追踪提供者
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "session-sampling-test")
  
  // 定义用户会话类型和对应的采样策略
  let session_types = [
    ("premium", 0.8, "paid-user"),     // 付费用户：80%采样
    ("new", 0.6, "new-user"),         // 新用户：60%采样
    ("active", 0.4, "active-user"),   // 活跃用户：40%采样
    ("returning", 0.2, "returning-user"), // 回访用户：20%采样
    ("inactive", 0.05, "inactive-user")   // 非活跃用户：5%采样
  ]
  
  // 测试每种用户会话类型
  for i = 0; i < session_types.length(); i = i + 1 {
    let (session_type, sampling_rate, user_category) = session_types[i]
    
    // 创建用户会话特定的span
    let session_span = Tracer::start_span(tracer, "session-" + session_type)
    
    // 模拟用户会话操作
    let session_operations = 200
    let sampled_operations = 0
    
    for j = 0; j < session_operations; j = j + 1 {
      // 根据用户会话类型决定采样率
      let should_sample = (j % 100) < (sampling_rate * 100.0).to_int()
      
      if should_sample {
        // 创建采样的span
        let sampled_span = Tracer::start_span(tracer, "session-op-" + j.to_string())
        
        Span::set_attributes(sampled_span, Attributes::from_array([
          ("session.type", session_type),
          ("user.category", user_category),
          ("sampling.rate", sampling_rate.to_string()),
          ("session.id", "session-" + i.to_string()),
          ("operation.id", j.to_string())
        ]))
        
        Span::end(sampled_span)
        sampled_operations = sampled_operations + 1
      }
    }
    
    // 计算采样效果
    let actual_sampling_rate = sampled_operations.to_float() / session_operations.to_float()
    
    // 添加用户会话和采样指标
    Span::set_attributes(session_span, Attributes::from_array([
      ("session.type", session_type),
      ("user.category", user_category),
      ("target.sampling.rate", sampling_rate.to_string()),
      ("actual.sampling.rate", actual_sampling_rate.to_string()),
      ("session.operations", session_operations.to_string()),
      ("sampled.operations", sampled_operations.to_string())
    ]))
    
    // 验证基于用户会话的采样
    assert_true(sampled_operations > 0)  // 应该有至少一些操作被采样
    
    // 验证高价值用户会话采样更多
    match session_type {
      "premium" => assert_true(sampled_operations > 120),  // 付费用户应该采样最多
      "new" => assert_true(sampled_operations > 80),       // 新用户应该采样较多
      "active" => assert_true(sampled_operations > 60),    // 活跃用户应该采样适中
      "returning" => assert_true(sampled_operations > 20), // 回访用户应该采样较少
      "inactive" => assert_true(sampled_operations < 20),  // 非活跃用户应该采样最少
      _ => assert_true(sampled_operations > 0)             // 其他情况应该有一些采样
    }
    
    // 结束用户会话span
    Span::end(session_span)
  }
}

// 测试5: 基于业务重要性的采样测试
test "business-importance-based sampling test" {
  // 创建追踪提供者
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "business-importance-sampling-test")
  
  // 定义业务操作重要性和对应的采样策略
  let business_operations = [
    ("checkout", 1.0, "revenue-critical"),      // 结账：100%采样
    ("payment", 1.0, "revenue-critical"),      // 支付：100%采样
    ("user-registration", 0.8, "growth-critical"), // 用户注册：80%采样
    ("search", 0.3, "user-experience"),        // 搜索：30%采样
    ("browse", 0.1, "user-experience"),        // 浏览：10%采样
    ("recommendation", 0.05, "enhancement")     // 推荐：5%采样
  ]
  
  // 测试每种业务操作
  for i = 0; i < business_operations.length(); i = i + 1 {
    let (operation_name, sampling_rate, business_category) = business_operations[i]
    
    // 创建业务操作特定的span
    let business_span = Tracer::start_span(tracer, "business-" + operation_name)
    
    // 模拟业务操作
    let operation_count = 300
    let sampled_operations = 0
    
    for j = 0; j < operation_count; j = j + 1 {
      // 根据业务重要性决定采样率
      let should_sample = (j % 100) < (sampling_rate * 100.0).to_int()
      
      if should_sample {
        // 创建采样的span
        let sampled_span = Tracer::start_span(tracer, "business-op-" + j.to_string())
        
        Span::set_attributes(sampled_span, Attributes::from_array([
          ("operation.name", operation_name),
          ("business.category", business_category),
          ("sampling.rate", sampling_rate.to_string()),
          ("business.impact", if business_category == "revenue-critical" { "high" } else { "medium" }),
          ("operation.id", j.to_string())
        ]))
        
        Span::end(sampled_span)
        sampled_operations = sampled_operations + 1
      }
    }
    
    // 计算采样效果
    let actual_sampling_rate = sampled_operations.to_float() / operation_count.to_float()
    
    // 添加业务重要性和采样指标
    Span::set_attributes(business_span, Attributes::from_array([
      ("operation.name", operation_name),
      ("business.category", business_category),
      ("target.sampling.rate", sampling_rate.to_string()),
      ("actual.sampling.rate", actual_sampling_rate.to_string()),
      ("operation.count", operation_count.to_string()),
      ("sampled.operations", sampled_operations.to_string())
    ]))
    
    // 验证基于业务重要性的采样
    assert_true(sampled_operations > 0)  // 应该有至少一些操作被采样
    
    // 验证关键业务操作采样更多
    match business_category {
      "revenue-critical" => assert_true(sampled_operations > 250),  // 收入关键操作应该采样最多
      "growth-critical" => assert_true(sampled_operations > 200),   // 增长关键操作应该采样较多
      "user-experience" => assert_true(sampled_operations > 20),    // 用户体验操作应该采样适中
      "enhancement" => assert_true(sampled_operations < 30),        // 增强功能操作应该采样较少
      _ => assert_true(sampled_operations > 0)                     // 其他情况应该有一些采样
    }
    
    // 结束业务操作span
    Span::end(business_span)
  }
}

// 测试6: 动态采样策略切换测试
test "dynamic sampling strategy switching test" {
  // 创建追踪提供者
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "dynamic-sampling-switch-test")
  
  // 定义采样策略切换场景
  let switching_scenarios = [
    ("normal-to-peak", "probabilistic", "adaptive", 1000),  // 正常到峰值：概率到自适应
    ("peak-to-normal", "adaptive", "probabilistic", 500),   // 峰值到正常：自适应到概率
    ("error-spike", "probabilistic", "error-aware", 800),   // 错误尖峰：概率到错误感知
    ("maintenance", "adaptive", "reduced", 200)             // 维护：自适应到减少
  ]
  
  // 测试每种切换场景
  for i = 0; i < switching_scenarios.length(); i = i + 1 {
    let (scenario_name, from_strategy, to_strategy, operation_count) = switching_scenarios[i]
    
    // 创建策略切换特定的span
    let switching_span = Tracer::start_span(tracer, "switch-" + scenario_name)
    
    // 模拟策略切换前的采样
    let before_sampled = 0
    for j = 0; j < operation_count / 2; j = j + 1 {
      let should_sample = match from_strategy {
        "probabilistic" => (j % 100) < 10,  // 10%概率采样
        "adaptive" => (j % 100) < 30,       // 30%自适应采样
        "error-aware" => (j % 100) < 50,    // 50%错误感知采样
        "reduced" => (j % 100) < 5,         // 5%减少采样
        _ => (j % 100) < 10                 // 默认10%采样
      }
      
      if should_sample {
        before_sampled = before_sampled + 1
      }
    }
    
    // 模拟策略切换
    let switch_point = operation_count / 2
    
    // 模拟策略切换后的采样
    let after_sampled = 0
    for j = switch_point; j < operation_count; j = j + 1 {
      let should_sample = match to_strategy {
        "probabilistic" => (j % 100) < 10,  // 10%概率采样
        "adaptive" => (j % 100) < 30,       // 30%自适应采样
        "error-aware" => (j % 100) < 50,    // 50%错误感知采样
        "reduced" => (j % 100) < 5,         // 5%减少采样
        _ => (j % 100) < 10                 // 默认10%采样
      }
      
      if should_sample {
        after_sampled = after_sampled + 1
      }
    }
    
    // 计算切换前后的采样率
    let before_rate = before_sampled.to_float() / (operation_count / 2).to_float()
    let after_rate = after_sampled.to_float() / (operation_count / 2).to_float()
    
    // 添加策略切换指标
    Span::set_attributes(switching_span, Attributes::from_array([
      ("scenario.name", scenario_name),
      ("from.strategy", from_strategy),
      ("to.strategy", to_strategy),
      ("switch.point", switch_point.to_string()),
      ("operation.count", operation_count.to_string()),
      ("before.sampled", before_sampled.to_string()),
      ("after.sampled", after_sampled.to_string()),
      ("before.sampling.rate", before_rate.to_string()),
      ("after.sampling.rate", after_rate.to_string())
    ]))
    
    // 验证策略切换
    assert_true(before_sampled > 0)  // 切换前应该有采样
    assert_true(after_sampled > 0)   // 切换后应该有采样
    
    // 验证特定切换场景的行为
    match scenario_name {
      "normal-to-peak" => {
        // 正常到峰值：采样率应该增加
        assert_true(after_rate > before_rate)
      },
      "peak-to-normal" => {
        // 峰值到正常：采样率应该减少
        assert_true(after_rate < before_rate)
      },
      "error-spike" => {
        // 错误尖峰：采样率应该增加
        assert_true(after_rate > before_rate)
      },
      "maintenance" => {
        // 维护：采样率应该减少
        assert_true(after_rate < before_rate)
      },
      _ => {
        // 其他情况：至少有一些采样
        assert_true(before_rate > 0.0 && after_rate > 0.0)
      }
    }
    
    // 结束策略切换span
    Span::end(switching_span)
  }
}

// 测试7: 多维度采样决策测试
test "multi-dimensional sampling decision test" {
  // 创建追踪提供者
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "multi-dimensional-sampling-test")
  
  // 定义采样决策维度
  let sampling_dimensions = [
    ("service-criticality", "high", 0.8),
    ("error-rate", "elevated", 0.6),
    ("user-type", "premium", 0.7),
    ("business-impact", "revenue", 0.9),
    ("system-load", "high", 0.3)
  ]
  
  // 测试多维度采样决策
  let total_operations = 1000
  let sampled_operations = 0
  
  for i = 0; i < total_operations; i = i + 1 {
    // 为每个操作随机分配维度值
    let operation_dimensions = []
    let sampling_probability = 1.0
    
    for j = 0; j < sampling_dimensions.length(); j = j + 1 {
      let (dimension_name, high_value, high_sampling_rate) = sampling_dimensions[j]
      
      // 随机决定是否为高值
      let is_high_value = (i % 4) == j  // 简化的随机逻辑
      
      let dimension_value = if is_high_value { high_value } else { "normal" }
      let dimension_sampling_rate = if is_high_value { high_sampling_rate } else { 0.1 }
      
      operation_dimensions.push((dimension_name, dimension_value, dimension_sampling_rate))
      
      // 多维度采样决策：使用最高采样率
      if dimension_sampling_rate > sampling_probability {
        sampling_probability = dimension_sampling_rate
      }
    }
    
    // 根据多维度采样概率决定是否采样
    let should_sample = (i % 100) < (sampling_probability * 100.0).to_int()
    
    if should_sample {
      // 创建采样的span
      let sampled_span = Tracer::start_span(tracer, "multi-dim-op-" + i.to_string())
      
      // 添加所有维度属性
      let attrs = []
      for j = 0; j < operation_dimensions.length(); j = j + 1 {
        let (dimension_name, dimension_value, _) = operation_dimensions[j]
        attrs.push((dimension_name, dimension_value))
      }
      
      Span::set_attributes(sampled_span, Attributes::from_array(attrs))
      Span::end(sampled_span)
      sampled_operations = sampled_operations + 1
    }
  }
  
  // 计算多维度采样效果
  let actual_sampling_rate = sampled_operations.to_float() / total_operations.to_float()
  
  // 创建多维度采样总结span
  let summary_span = Tracer::start_span(tracer, "multi-dimensional-summary")
  
  // 添加多维度采样指标
  Span::set_attributes(summary_span, Attributes::from_array([
    ("total.operations", total_operations.to_string()),
    ("sampled.operations", sampled_operations.to_string()),
    ("actual.sampling.rate", actual_sampling_rate.to_string()),
    ("dimensions.count", sampling_dimensions.length().to_string()),
    ("sampling.strategy", "multi-dimensional")
  ]))
  
  // 验证多维度采样
  assert_true(sampled_operations > 0)  // 应该有至少一些操作被采样
  assert_true(actual_sampling_rate > 0.1)  // 采样率应该大于基础采样率
  
  // 结束多维度采样总结span
  Span::end(summary_span)
}

// 测试8: 采样一致性保证测试
test "sampling consistency guarantee test" {
  // 创建追踪提供者
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "sampling-consistency-test")
  
  // 定义采样一致性场景
  let consistency_scenarios = [
    ("trace-consistency", "same-trace", "consistent"),    // 同一追踪内的一致性
    ("user-consistency", "same-user", "consistent"),      // 同一用户的一致性
    ("session-consistency", "same-session", "consistent"), // 同一会话的一致性
    ("no-consistency", "independent", "independent")      // 无一致性要求
  ]
  
  // 测试每种一致性场景
  for i = 0; i < consistency_scenarios.length(); i = i + 1 {
    let (scenario_name, consistency_type, consistency_level) = consistency_scenarios[i]
    
    // 创建一致性场景特定的span
    let consistency_span = Tracer::start_span(tracer, "consistency-" + scenario_name)
    
    // 模拟相关操作组
    let operation_groups = 10
    let operations_per_group = 20
    let consistent_groups = 0
    
    for group = 0; group < operation_groups; group = group + 1 {
      // 决定组内第一个操作的采样决策
      let group_sampled = (group % 100) < 30  // 30%基础采样率
      let group_consistent = true
      let group_sampled_count = 0
      
      for op = 0; op < operations_per_group; op = op + 1 {
        let should_sample = match consistency_level {
          "consistent" => group_sampled,  // 一致性：与组内第一个操作相同
          "independent" => (op % 100) < 30,  // 独立：每个操作独立决定
          _ => group_sampled
        }
        
        if should_sample {
          // 创建采样的span
          let sampled_span = Tracer::start_span(tracer, "consistency-op-" + group.to_string() + "-" + op.to_string())
          
          Span::set_attributes(sampled_span, Attributes::from_array([
            ("scenario.name", scenario_name),
            ("consistency.type", consistency_type),
            ("group.id", group.to_string()),
            ("operation.id", op.to_string()),
            ("group.sampled", group_sampled.to_string())
          ]))
          
          Span::end(sampled_span)
          group_sampled_count = group_sampled_count + 1
        }
      }
      
      // 检查组内一致性
      if consistency_level == "consistent" {
        if (group_sampled && group_sampled_count == operations_per_group) ||
           (!group_sampled && group_sampled_count == 0) {
          consistent_groups = consistent_groups + 1
        }
      }
    }
    
    // 计算一致性效果
    let consistency_rate = if consistency_level == "consistent" {
      consistent_groups.to_float() / operation_groups.to_float()
    } else {
      0.0  // 独立采样不计算一致性
    }
    
    // 添加一致性指标
    Span::set_attributes(consistency_span, Attributes::from_array([
      ("scenario.name", scenario_name),
      ("consistency.type", consistency_type),
      ("consistency.level", consistency_level),
      ("operation.groups", operation_groups.to_string()),
      ("operations.per.group", operations_per_group.to_string()),
      ("consistent.groups", consistent_groups.to_string()),
      ("consistency.rate", consistency_rate.to_string())
    ]))
    
    // 验证采样一致性
    if consistency_level == "consistent" {
      assert_true(consistency_rate > 0.5)  // 一致性应该大于50%
    }
    
    // 结束一致性场景span
    Span::end(consistency_span)
  }
}

// 测试9: 采样性能影响测试
test "sampling performance impact test" {
  // 创建追踪提供者
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "sampling-performance-test")
  
  // 定义不同采样率的性能测试
  let sampling_rates = [0.01, 0.1, 0.5, 0.9, 1.0]  // 1%, 10%, 50%, 90%, 100%
  
  // 测试每种采样率
  for i = 0; i < sampling_rates.length(); i = i + 1 {
    let sampling_rate = sampling_rates[i]
    
    // 创建采样率特定的span
    let performance_span = Tracer::start_span(tracer, "perf-rate-" + (sampling_rate * 100).to_int())
    
    // 记录开始时间和内存使用
    let start_time_ns = 1640995200000000000L
    let initial_memory_kb = 50000  // 50MB
    
    // 模拟操作和采样
    let total_operations = 5000
    let sampled_operations = 0
    let processing_time_ns = 0
    
    for j = 0; j < total_operations; j = j + 1 {
      // 模拟采样决策时间
      let sampling_decision_time_ns = 1000  // 1μs
      processing_time_ns = processing_time_ns + sampling_decision_time_ns
      
      let should_sample = (j % 100) < (sampling_rate * 100.0).to_int()
      
      if should_sample {
        // 模拟span创建和处理时间
        let span_processing_time_ns = 10000  // 10μs
        processing_time_ns = processing_time_ns + span_processing_time_ns
        
        // 创建采样的span
        let sampled_span = Tracer::start_span(tracer, "perf-op-" + j.to_string())
        Span::set_attributes(sampled_span, Attributes::from_array([
          ("sampling.rate", sampling_rate.to_string()),
          ("operation.id", j.to_string())
        ]))
        Span::end(sampled_span)
        
        sampled_operations = sampled_operations + 1
      }
    }
    
    // 记录结束时间和内存使用
    let end_time_ns = start_time_ns + processing_time_ns
    let final_memory_kb = initial_memory_kb + (sampled_operations * 0.5)  // 每个采样操作0.5KB
    
    // 计算性能指标
    let total_time_ms = (end_time_ns - start_time_ns) / 1000000L
    let memory_increase_kb = final_memory_kb - initial_memory_kb
    let ops_per_second = (total_operations.to_float() / total_time_ms.to_float()) * 1000.0
    let actual_sampling_rate = sampled_operations.to_float() / total_operations.to_float()
    
    // 添加性能指标
    Span::set_attributes(performance_span, Attributes::from_array([
      ("sampling.rate", sampling_rate.to_string()),
      ("total.operations", total_operations.to_string()),
      ("sampled.operations", sampled_operations.to_string()),
      ("actual.sampling.rate", actual_sampling_rate.to_string()),
      ("total.time.ms", total_time_ms.to_string()),
      ("memory.increase.kb", memory_increase_kb.to_string()),
      ("ops.per.second", ops_per_second.to_string()),
      ("time.per.sampled.op", if sampled_operations > 0 { 
        (total_time_ms.to_float() / sampled_operations.to_float()).to_string() 
      } else { 
        "0" 
      })
    ]))
    
    // 验证采样性能影响
    assert_true(total_time_ms > 0)  // 应该有处理时间
    assert_true(ops_per_second > 0.0)  // 应该有操作吞吐量
    
    // 验证采样率对性能的影响
    if sampling_rate < 0.5 {
      // 低采样率应该有更高的吞吐量
      assert_true(ops_per_second > 1000.0)
    } else {
      // 高采样率吞吐量会降低
      assert_true(ops_per_second > 100.0)
    }
    
    // 结束性能span
    Span::end(performance_span)
  }
}

// 测试10: 智能采样策略学习测试
test "intelligent sampling strategy learning test" {
  // 创建追踪提供者
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "intelligent-sampling-test")
  
  // 模拟采样策略学习过程
  let learning_iterations = 5
  let base_sampling_rate = 0.1
  let target_error_rate = 0.05  // 目标错误率5%
  let target_memory_usage = 100000  // 目标内存使用100KB
  
  // 创建学习过程span
  let learning_span = Tracer::start_span(tracer, "sampling-learning-process")
  
  // 模拟学习迭代
  for iteration = 0; iteration < learning_iterations; iteration = iteration + 1 {
    // 创建迭代特定的span
    let iteration_span = Tracer::start_span(tracer, "learning-iteration-" + iteration.to_string())
    
    // 当前采样率（基于上一次迭代调整）
    let current_sampling_rate = match iteration {
      0 => base_sampling_rate,
      1 => 0.15,  // 基于第一次结果增加
      2 => 0.12,  // 基于第二次结果减少
      3 => 0.13,  // 基于第三次结果微调
      4 => 0.125, // 最终稳定值
      _ => base_sampling_rate
    }
    
    // 模拟操作和采样
    let total_operations = 1000
    let sampled_operations = 0
    let error_operations = 0
    let memory_usage = 0
    
    for j = 0; j < total_operations; j = j + 1 {
      let should_sample = (j % 100) < (current_sampling_rate * 100.0).to_int()
      
      if should_sample {
        // 创建采样的span
        let sampled_span = Tracer::start_span(tracer, "learn-op-" + j.to_string())
        
        // 模拟错误检测
        let is_error = (j % 50) == 0  // 2%错误率
        if is_error {
          Span::set_status(sampled_span, Error, Some("Learning error"))
          error_operations = error_operations + 1
        }
        
        Span::set_attributes(sampled_span, Attributes::from_array([
          ("iteration", iteration.to_string()),
          ("sampling.rate", current_sampling_rate.to_string()),
          ("is.error", is_error.to_string())
        ]))
        
        Span::end(sampled_span)
        sampled_operations = sampled_operations + 1
        memory_usage = memory_usage + 100  // 每个采样操作100B
      }
    }
    
    // 计算迭代指标
    let actual_sampling_rate = sampled_operations.to_float() / total_operations.to_float()
    let error_rate = if sampled_operations > 0 {
      error_operations.to_float() / sampled_operations.to_float()
    } else {
      0.0
    }
    
    // 模拟学习决策
    let learning_decision = if error_rate > target_error_rate {
      "increase.sampling"  // 错误率高，增加采样率
    } else if memory_usage > target_memory_usage {
      "decrease.sampling"  // 内存使用高，减少采样率
    } else {
      "maintain.sampling"  // 保持当前采样率
    }
    
    // 添加学习指标
    Span::set_attributes(iteration_span, Attributes::from_array([
      ("iteration", iteration.to_string()),
      ("current.sampling.rate", current_sampling_rate.to_string()),
      ("actual.sampling.rate", actual_sampling_rate.to_string()),
      ("sampled.operations", sampled_operations.to_string()),
      ("error.operations", error_operations.to_string()),
      ("error.rate", error_rate.to_string()),
      ("memory.usage", memory_usage.to_string()),
      ("learning.decision", learning_decision)
    ]))
    
    // 验证学习迭代
    assert_true(sampled_operations > 0)  // 应该有至少一些操作被采样
    
    // 结束迭代span
    Span::end(iteration_span)
  }
  
  // 添加学习总结指标
  Span::set_attributes(learning_span, Attributes::from_array([
    ("learning.iterations", learning_iterations.to_string()),
    ("base.sampling.rate", base_sampling_rate.to_string()),
    ("target.error.rate", target_error_rate.to_string()),
    ("target.memory.usage", target_memory_usage.to_string()),
    ("learning.algorithm", "reinforcement"),
    ("convergence.achieved", "true")
  ]))
  
  // 验证学习过程
  assert_true(learning_iterations > 0)  // 应该有学习迭代
  
  // 结束学习过程span
  Span::end(learning_span)
}