// Azimuth 自适应配置和动态更新测试
// 专注于测试系统的自适应配置管理、动态更新和智能调整功能

// 测试1: 自适应配置管理
test "自适应配置管理测试" {
  // 1. 创建自适应配置管理器
  let config_manager = AdaptiveConfigManager({
    config_sources: [
      FileSource({ path: "/etc/azimuth/config.json" }),
      EnvironmentSource({ prefix: "AZIMUTH_" }),
      RemoteSource({ endpoint: "https://config.azimuth.com/api/v1/config" })
    ],
    validation_rules: [
      ValidationRule({
        key: "log.level",
        validator: EnumValidator(["debug", "info", "warn", "error"]),
        required: true
      }),
      ValidationRule({
        key: "server.port",
        validator: RangeValidator(1, 65535),
        required: true
      }),
      ValidationRule({
        key: "cache.size",
        validator: PositiveIntegerValidator(),
        required: false
      })
    ],
    merge_strategy: OverrideMerge
  })
  
  // 2. 验证配置管理器
  assert_eq(config_manager.config_sources.length(), 3)
  assert_eq(config_manager.validation_rules.length(), 3)
  assert_eq(config_manager.merge_strategy, OverrideMerge)
  
  // 3. 加载配置
  let config_load_result = config_manager.load_config()
  
  // 4. 验证配置加载结果
  assert_true(config_load_result.success)
  
  // 5. 获取配置值
  let log_level = config_manager.get("log.level")
  let server_port = config_manager.get("server.port")
  let cache_size = config_manager.get("cache.size")
  
  // 6. 验证配置值
  match log_level {
    Some(value) => assert_true(["debug", "info", "warn", "error"].contains(value))
    None => assert_true(false)
  }
  
  match server_port {
    Some(value) => {
      let port = value.to_int()
      assert_true(port >= 1 && port <= 65535)
    }
    None => assert_true(false)
  }
  
  // 7. 测试配置验证
  let invalid_config = [
    ("log.level", "invalid"),
    ("server.port", "70000"),
    ("cache.size", "-100")
  ]
  
  for (key, value) in invalid_config {
    let validation_result = config_manager.validate_config(key, value)
    assert_false(validation_result.valid)
    assert_true(validation_result.errors.length() > 0)
  }
  
  // 8. 测试配置合并
  let base_config = [
    ("log.level", "info"),
    ("server.port", "8080"),
    ("cache.size", "1000")
  ]
  
  let override_config = [
    ("log.level", "debug"),
    ("cache.size", "2000")
  ]
  
  let merged_config = config_manager.merge_configs(base_config, override_config)
  
  // 9. 验证配置合并结果
  assert_eq(merged_config.get("log.level"), "debug") // 应该被覆盖
  assert_eq(merged_config.get("server.port"), "8080") // 应该保持原值
  assert_eq(merged_config.get("cache.size"), "2000") // 应该被覆盖
  
  // 10. 测试配置变更监听
  let mut config_changes = []
  let change_listener = fn(key, old_value, new_value) {
    config_changes = config_changes.push({
      key: key,
      old_value: old_value,
      new_value: new_value,
      timestamp: get_current_timestamp()
    })
  }
  
  config_manager.add_change_listener("log.level", change_listener)
  
  // 11. 更新配置
  let update_result = config_manager.update_config("log.level", "warn")
  
  // 12. 验证配置更新结果
  assert_true(update_result.success)
  assert_eq(config_manager.get("log.level"), "warn")
  
  // 13. 验证配置变更监听
  assert_eq(config_changes.length(), 1)
  assert_eq(config_changes[0].key, "log.level")
  assert_eq(config_changes[0].old_value, "debug")
  assert_eq(config_changes[0].new_value, "warn")
}

// 测试2: 动态配置更新
test "动态配置更新测试" {
  // 1. 创建动态配置更新器
  let dynamic_updater = DynamicConfigUpdater({
    update_strategy: RollingUpdate,
    rollback_enabled: true,
    health_check_endpoint: "/health",
    update_timeout: 30000,
    max_rollback_attempts: 3
  })
  
  // 2. 验证动态配置更新器
  assert_eq(dynamic_updater.update_strategy, RollingUpdate)
  assert_true(dynamic_updater.rollback_enabled)
  assert_eq(dynamic_updater.health_check_endpoint, "/health")
  assert_eq(dynamic_updater.update_timeout, 30000)
  assert_eq(dynamic_updater.max_rollback_attempts, 3)
  
  // 3. 创建配置更新计划
  let update_plan = ConfigUpdatePlan({
    id: "update-plan-001",
    description: "Update cache configuration for better performance",
    changes: [
      ConfigChange({
        key: "cache.size",
        old_value: "1000",
        new_value: "2000",
        requires_restart: false
      }),
      ConfigChange({
        key: "cache.ttl",
        old_value: "300",
        new_value: "600",
        requires_restart: false
      }),
      ConfigChange({
        key: "thread.pool.size",
        old_value: "10",
        new_value: "20",
        requires_restart: true
      })
    ],
    validation_steps: [
      ValidateConfigIntegrity,
      ValidateDependencies,
      ValidateResourceAvailability
    ],
    rollout_percentage: 25, // 25%的实例先更新
    rollout_interval: 60000   // 1分钟间隔
  })
  
  // 4. 验证配置更新计划
  assert_eq(update_plan.id, "update-plan-001")
  assert_eq(update_plan.changes.length(), 3)
  assert_eq(update_plan.validation_steps.length(), 3)
  assert_eq(update_plan.rollout_percentage, 25)
  assert_eq(update_plan.rollout_interval, 60000)
  
  // 5. 创建目标实例
  let target_instances = [
    Instance({
      id: "instance-001",
      host: "host-001",
      port: 8080,
      status: Running,
      config_version: "v1.0.0"
    }),
    Instance({
      id: "instance-002",
      host: "host-002",
      port: 8080,
      status: Running,
      config_version: "v1.0.0"
    }),
    Instance({
      id: "instance-003",
      host: "host-003",
      port: 8080,
      status: Running,
      config_version: "v1.0.0"
    }),
    Instance({
      id: "instance-004",
      host: "host-004",
      port: 8080,
      status: Running,
      config_version: "v1.0.0"
    })
  ]
  
  // 6. 验证目标实例
  assert_eq(target_instances.length(), 4)
  
  for instance in target_instances {
    assert_eq(instance.status, Running)
    assert_eq(instance.config_version, "v1.0.0")
  }
  
  // 7. 执行配置更新计划
  let update_result = dynamic_updater.execute_update_plan(update_plan, target_instances)
  
  // 8. 验证配置更新结果
  assert_true(update_result.success)
  assert_eq(update_result.updated_instances.length(), 1) // 25%的4个实例 = 1个实例
  
  // 9. 验证已更新实例
  let updated_instance = update_result.updated_instances[0]
  assert_eq(updated_instance.config_version, "v1.0.1") // 应该有新的配置版本
  assert_eq(updated_instance.status, Running) // 应该仍然在运行
  
  // 10. 验证未更新实例
  let not_updated_instances = target_instances.filter(fn(instance) {
    !update_result.updated_instances.any(fn(updated) { updated.id == instance.id })
  })
  
  for instance in not_updated_instances {
    assert_eq(instance.config_version, "v1.0.0") // 应该保持原配置版本
  }
  
  // 11. 继续执行剩余的更新
  let remaining_instances = not_updated_instances
  let remaining_update_result = dynamic_updater.continue_update(update_plan, remaining_instances)
  
  // 12. 验证剩余更新结果
  assert_true(remaining_update_result.success)
  assert_eq(remaining_update_result.updated_instances.length(), 1) // 又更新了25%的实例
  
  // 13. 验证配置更新验证
  let validation_results = dynamic_updater.validate_update_result(update_plan, update_result.updated_instances)
  
  // 14. 验证配置更新验证结果
  assert_true(validation_results.all_validations_passed)
  assert_eq(validation_results.failed_validations.length(), 0)
  
  // 15. 测试配置回滚
  let rollback_result = dynamic_updater.rollback_update(update_plan, update_result.updated_instances)
  
  // 16. 验证配置回滚结果
  assert_true(rollback_result.success)
  
  for instance in rollback_result.rolled_back_instances {
    assert_eq(instance.config_version, "v1.0.0") // 应该回滚到原配置版本
    assert_eq(instance.status, Running) // 应该仍然在运行
  }
}

// 测试3: 自适应性能调优
test "自适应性能调优测试" {
  // 1. 创建自适应性能调优器
  let performance_tuner = AdaptivePerformanceTuner({
    metrics_collector: MetricsCollector({
      collection_interval: 10000, // 10秒
      metrics: [
        "cpu.usage",
        "memory.usage",
        "response.time",
        "throughput",
        "error.rate"
      ]
    }),
    tuning_parameters: [
      TuningParameter({
        name: "thread.pool.size",
        min_value: 1,
        max_value: 100,
        current_value: 10,
        step_size: 2,
        impact_metrics: ["cpu.usage", "throughput"]
      }),
      TuningParameter({
        name: "cache.size",
        min_value: 100,
        max_value: 10000,
        current_value: 1000,
        step_size: 500,
        impact_metrics: ["memory.usage", "response.time"]
      }),
      TuningParameter({
        name: "connection.pool.size",
        min_value: 5,
        max_value: 50,
        current_value: 20,
        step_size: 5,
        impact_metrics: ["cpu.usage", "throughput", "response.time"]
      })
    ],
    optimization_goals: [
      OptimizationGoal({
        metric: "response.time",
        target: "< 100ms",
        weight: 0.4
      }),
      OptimizationGoal({
        metric: "throughput",
        target: "> 1000 req/s",
        weight: 0.3
      }),
      OptimizationGoal({
        metric: "error.rate",
        target: "< 0.01",
        weight: 0.3
      })
    ],
    tuning_strategy: HillClimbing,
    evaluation_period: 60000 // 1分钟
  })
  
  // 2. 验证自适应性能调优器
  assert_eq(performance_tuner.tuning_parameters.length(), 3)
  assert_eq(performance_tuner.optimization_goals.length(), 3)
  assert_eq(performance_tuner.tuning_strategy, HillClimbing)
  assert_eq(performance_tuner.evaluation_period, 60000)
  
  // 3. 模拟系统性能指标
  let initial_metrics = [
    ("cpu.usage", 60.5),
    ("memory.usage", 70.2),
    ("response.time", 150.0),
    ("throughput", 800.0),
    ("error.rate", 0.02)
  ]
  
  // 4. 验证初始性能指标
  assert_eq(initial_metrics.length(), 5)
  
  // 5. 计算初始性能得分
  let initial_score = performance_tuner.calculate_performance_score(initial_metrics)
  
  // 6. 验证初始性能得分
  assert_true(initial_score > 0.0)
  assert_true(initial_score < 1.0) // 得分应该在0-1之间
  
  // 7. 执行自适应调优
  let tuning_result = performance_tuner.tune_performance(initial_metrics)
  
  // 8. 验证调优结果
  assert_true(tuning_result.success)
  assert_eq(tuning_result.adjusted_parameters.length(), 1) // 应该调整了一个参数
  
  // 9. 验证参数调整
  let adjusted_param = tuning_result.adjusted_parameters[0]
  assert_true(adjusted_param.old_value != adjusted_param.new_value)
  
  // 10. 模拟调优后的性能指标
  let optimized_metrics = [
    ("cpu.usage", 65.0),
    ("memory.usage", 75.0),
    ("response.time", 95.0), // 响应时间改善
    ("throughput", 1200.0),  // 吞吐量提升
    ("error.rate", 0.01)     // 错误率降低
  ]
  
  // 11. 计算优化后的性能得分
  let optimized_score = performance_tuner.calculate_performance_score(optimized_metrics)
  
  // 12. 验证性能改善
  assert_true(optimized_score > initial_score) // 优化后得分应该更高
  
  // 13. 测试多轮调优
  let mut tuning_history = []
  let mut current_metrics = initial_metrics
  let mut current_score = initial_score
  
  for i in 1..=5 {
    let result = performance_tuner.tune_performance(current_metrics)
    tuning_history = tuning_history.push({
      iteration: i,
      adjusted_parameters: result.adjusted_parameters,
      metrics_before: current_metrics,
      metrics_after: simulate_metrics_after_tuning(current_metrics, result.adjusted_parameters),
      score_before: current_score,
      score_after: performance_tuner.calculate_performance_score(
        simulate_metrics_after_tuning(current_metrics, result.adjusted_parameters)
      )
    })
    
    current_metrics = simulate_metrics_after_tuning(current_metrics, result.adjusted_parameters)
    current_score = performance_tuner.calculate_performance_score(current_metrics)
  }
  
  // 14. 验证多轮调优结果
  assert_eq(tuning_history.length(), 5)
  
  // 性能得分应该逐步提高
  for i in 1..tuning_history.length() {
    let prev_score = tuning_history[i-1].score_after
    let curr_score = tuning_history[i].score_after
    assert_true(curr_score >= prev_score * 0.95) // 允许小幅波动
  }
  
  // 15. 测试调优收敛
  let convergence_result = performance_tuner.check_convergence(tuning_history)
  
  // 16. 验证调优收敛
  assert_true(convergence_result.converged)
  assert_true(convergence_result.final_score > initial_score * 1.2) // 最终得分应该至少提高20%
  
  // 17. 测试调优回滚
  let rollback_result = performance_tuner.rollback_tuning(tuning_history)
  
  // 18. 验证调优回滚
  assert_true(rollback_result.success)
  assert_eq(rollback_result.restored_parameters.length(), tuning_history.reduce(fn(acc, h) { 
    acc + h.adjusted_parameters.length() 
  }, 0))
}

// 测试4: 智能资源分配
test "智能资源分配测试" {
  // 1. 创建智能资源分配器
  let resource_allocator = IntelligentResourceAllocator({
    resources: [
      ResourceType({
        name: "cpu",
        total_capacity: 100,
        allocation_unit: 1,
        cost_per_unit: 0.01
      }),
      ResourceType({
        name: "memory",
        total_capacity: 1024, // GB
        allocation_unit: 1,
        cost_per_unit: 0.005
      }),
      ResourceType({
        name: "storage",
        total_capacity: 10240, // GB
        allocation_unit: 10,
        cost_per_unit: 0.001
      })
    ],
    allocation_strategy: BalancedAllocation,
    prediction_model: TimeSeriesPrediction,
    rebalance_interval: 300000, // 5分钟
    overcommit_ratio: 1.2
  })
  
  // 2. 验证智能资源分配器
  assert_eq(resource_allocator.resources.length(), 3)
  assert_eq(resource_allocator.allocation_strategy, BalancedAllocation)
  assert_eq(resource_allocator.prediction_model, TimeSeriesPrediction)
  assert_eq(resource_allocator.rebalance_interval, 300000)
  assert_eq(resource_allocator.overcommit_ratio, 1.2)
  
  // 3. 创建服务实例
  let service_instances = [
    ServiceInstance({
      id: "service-001",
      name: "user-service",
      priority: High,
      current_allocation: [
        ("cpu", 20),
        ("memory", 256),
        ("storage", 100)
      ],
      resource_requirements: [
        ("cpu", 10),
        ("memory", 128),
        ("storage", 50)
      ],
      usage_history: [
        (1641015000000, 15.5, 200.0, 80.0),
        (1641015100000, 18.2, 230.0, 85.0),
        (1641015200000, 16.8, 210.0, 82.0)
      ]
    }),
    ServiceInstance({
      id: "service-002",
      name: "order-service",
      priority: Medium,
      current_allocation: [
        ("cpu", 15),
        ("memory", 512),
        ("storage", 200)
      ],
      resource_requirements: [
        ("cpu", 8),
        ("memory", 256),
        ("storage", 100)
      ],
      usage_history: [
        (1641015000000, 12.3, 400.0, 150.0),
        (1641015100000, 14.7, 480.0, 180.0),
        (1641015200000, 13.5, 450.0, 170.0)
      ]
    }),
    ServiceInstance({
      id: "service-003",
      name: "analytics-service",
      priority: Low,
      current_allocation: [
        ("cpu", 10),
        ("memory", 128),
        ("storage", 500)
      ],
      resource_requirements: [
        ("cpu", 5),
        ("memory", 64),
        ("storage", 200)
      ],
      usage_history: [
        (1641015000000, 8.2, 100.0, 300.0),
        (1641015100000, 9.5, 120.0, 350.0),
        (1641015200000, 8.8, 110.0, 320.0)
      ]
    })
  ]
  
  // 4. 验证服务实例
  assert_eq(service_instances.length(), 3)
  
  // 5. 预测资源需求
  let predictions = resource_allocator.predict_resource_needs(service_instances)
  
  // 6. 验证资源需求预测
  assert_eq(predictions.length(), 3)
  
  for prediction in predictions {
    assert_eq(prediction.predicted_needs.length(), 3) // cpu, memory, storage
    assert_true(prediction.confidence > 0.5) // 置信度应该高于50%
  }
  
  // 7. 执行资源分配
  let allocation_result = resource_allocator.allocate_resources(service_instances, predictions)
  
  // 8. 验证资源分配结果
  assert_true(allocation_result.success)
  assert_eq(allocation_result.allocations.length(), 3)
  
  // 9. 验证资源分配不超额
  let total_cpu_allocation = allocation_result.allocations.reduce(fn(acc, alloc) {
    acc + alloc.allocated_resources.filter(fn(r) { r.resource_type == "cpu" })[0].amount
  }, 0)
  
  let total_memory_allocation = allocation_result.allocations.reduce(fn(acc, alloc) {
    acc + alloc.allocated_resources.filter(fn(r) { r.resource_type == "memory" })[0].amount
  }, 0)
  
  let total_storage_allocation = allocation_result.allocations.reduce(fn(acc, alloc) {
    acc + alloc.allocated_resources.filter(fn(r) { r.resource_type == "storage" })[0].amount
  }, 0)
  
  assert_true(total_cpu_allocation <= 100 * resource_allocator.overcommit_ratio)
  assert_true(total_memory_allocation <= 1024 * resource_allocator.overcommit_ratio)
  assert_true(total_storage_allocation <= 10240 * resource_allocator.overcommit_ratio)
  
  // 10. 验证高优先级服务获得足够资源
  let user_service_allocation = allocation_result.allocations.filter(fn(alloc) { 
    alloc.instance_id == "service-001" 
  })[0]
  
  let user_service_cpu = user_service_allocation.allocated_resources.filter(fn(r) { 
    r.resource_type == "cpu" 
  })[0].amount
  
  let order_service_allocation = allocation_result.allocations.filter(fn(alloc) { 
    alloc.instance_id == "service-002" 
  })[0]
  
  let order_service_cpu = order_service_allocation.allocated_resources.filter(fn(r) { 
    r.resource_type == "cpu" 
  })[0].amount
  
  // 高优先级服务应该获得更多CPU资源
  assert_true(user_service_cpu >= order_service_cpu)
  
  // 11. 测试资源重平衡
  let rebalance_result = resource_allocator.rebalance_resources(
    allocation_result.allocations,
    service_instances
  )
  
  // 12. 验证资源重平衡结果
  assert_true(rebalance_result.success)
  assert_true(rebalance_result.rebalanced_resources > 0)
  
  // 13. 测试资源不足处理
  let high_demand_instances = service_instances.map(fn(instance) {
    let increased_requirements = instance.resource_requirements.map(fn(req) {
      match req.0 {
        "cpu" => (req.0, req.1 * 3) // CPU需求增加3倍
        "memory" => (req.0, req.1 * 2) // 内存需求增加2倍
        "storage" => (req.0, req.1 * 1.5) // 存储需求增加1.5倍
        _ => req
      }
    })
    { instance | resource_requirements = increased_requirements }
  })
  
  let shortage_result = resource_allocator.handle_resource_shortage(high_demand_instances)
  
  // 14. 验证资源不足处理
  assert_true(shortage_result.shortage_detected)
  assert_true(shortage_result.mitigation_actions.length() > 0)
  
  // 15. 验证缓解措施
  let mitigation_actions = shortage_result.mitigation_actions
  let has_priority_adjustment = mitigation_actions.any(fn(action) {
    match action {
      PriorityAdjustment(_) => true
      _ => false
    }
  })
  
  let has_resource_shedding = mitigation_actions.any(fn(action) {
    match action {
      ResourceShedding(_) => true
      _ => false
    }
  })
  
  assert_true(has_priority_adjustment || has_resource_shedding)
}

// 测试5: 自适应负载均衡
test "自适应负载均衡测试" {
  // 1. 创建自适应负载均衡器
  let load_balancer = AdaptiveLoadBalancer({
    algorithm: WeightedRoundRobin,
    health_check_interval: 10000, // 10秒
    failure_threshold: 3,
    recovery_threshold: 2,
    adaptation_enabled: true,
    metrics_weight: {
      "response_time": 0.4,
      "error_rate": 0.3,
      "throughput": 0.3
    }
  })
  
  // 2. 验证自适应负载均衡器
  assert_eq(load_balancer.algorithm, WeightedRoundRobin)
  assert_eq(load_balancer.health_check_interval, 10000)
  assert_eq(load_balancer.failure_threshold, 3)
  assert_eq(load_balancer.recovery_threshold, 2)
  assert_true(load_balancer.adaptation_enabled)
  
  // 3. 创建后端服务器
  let backend_servers = [
    BackendServer({
      id: "server-001",
      host: "backend-001",
      port: 8080,
      weight: 1,
      status: Healthy,
      metrics: {
        "response_time": 100.0,
        "error_rate": 0.01,
        "throughput": 500.0
      },
      consecutive_failures: 0
    }),
    BackendServer({
      id: "server-002",
      host: "backend-002",
      port: 8080,
      weight: 1,
      status: Healthy,
      metrics: {
        "response_time": 150.0,
        "error_rate": 0.02,
        "throughput": 400.0
      },
      consecutive_failures: 0
    }),
    BackendServer({
      id: "server-003",
      host: "backend-003",
      port: 8080,
      weight: 1,
      status: Healthy,
      metrics: {
        "response_time": 80.0,
        "error_rate": 0.005,
        "throughput": 600.0
      },
      consecutive_failures: 0
    })
  ]
  
  // 4. 验证后端服务器
  assert_eq(backend_servers.length(), 3)
  
  for server in backend_servers {
    assert_eq(server.status, Healthy)
    assert_eq(server.weight, 1)
  }
  
  // 5. 模拟请求分发
  let mut request_distribution = []
  
  for i in 1..=1000 {
    let selected_server = load_balancer.select_server(backend_servers)
    request_distribution = request_distribution.push(selected_server.id)
  }
  
  // 6. 验证请求分布
  let server_001_count = request_distribution.filter(fn(id) { id == "server-001" }).length()
  let server_002_count = request_distribution.filter(fn(id) { id == "server-002" }).length()
  let server_003_count = request_distribution.filter(fn(id) { id == "server-003" }).length()
  
  assert_eq(server_001_count + server_002_count + server_003_count, 1000)
  
  // 7. 执行自适应权重调整
  let adaptation_result = load_balancer.adapt_weights(backend_servers)
  
  // 8. 验证权重调整结果
  assert_true(adaptation_result.weights_adjusted)
  
  // 性能最好的服务器（server-003）应该获得最高权重
  let server_003_weight = adaptation_result.new_weights.get("server-003")
  let server_001_weight = adaptation_result.new_weights.get("server-001")
  let server_002_weight = adaptation_result.new_weights.get("server-002")
  
  match (server_003_weight, server_001_weight, server_002_weight) {
    (Some(w3), Some(w1), Some(w2)) => {
      assert_true(w3 > w1)
      assert_true(w3 > w2)
    }
    _ => assert_true(false)
  }
  
  // 9. 使用新权重重新分发请求
  let mut new_request_distribution = []
  
  for i in 1..=1000 {
    let selected_server = load_balancer.select_server_with_weights(
      backend_servers, 
      adaptation_result.new_weights
    )
    new_request_distribution = new_request_distribution.push(selected_server.id)
  }
  
  // 10. 验证新的请求分布
  let new_server_003_count = new_request_distribution.filter(fn(id) { id == "server-003" }).length()
  
  // server-003应该获得更多请求
  assert_true(new_server_003_count > server_003_count)
  
  // 11. 测试故障检测和恢复
  let mut failed_servers = backend_servers
  
  // 模拟server-001故障
  failed_servers = failed_servers.map(fn(server) {
    if server.id == "server-001" {
      { server | 
        status = Unhealthy, 
        consecutive_failures = 3,
        metrics = { server.metrics | 
          "error_rate" = 0.5 
        }
      }
    } else {
      server
    }
  })
  
  // 12. 执行健康检查
  let health_check_result = load_balancer.perform_health_check(failed_servers)
  
  // 13. 验证健康检查结果
  assert_eq(health_check_result.healthy_servers.length(), 2)
  assert_eq(health_check_result.unhealthy_servers.length(), 1)
  
  let unhealthy_server = health_check_result.unhealthy_servers[0]
  assert_eq(unhealthy_server.id, "server-001")
  assert_eq(unhealthy_server.status, Unhealthy)
  
  // 14. 验证故障服务器不再被选中
  let mut fault_tolerance_distribution = []
  
  for i in 1..=100 {
    let selected_server = load_balancer.select_server(health_check_result.healthy_servers)
    fault_tolerance_distribution = fault_tolerance_distribution.push(selected_server.id)
  }
  
  // server-001不应该出现在分布中
  assert_false(fault_tolerance_distribution.any(fn(id) { id == "server-001" }))
  
  // 15. 测试故障恢复
  let mut recovered_servers = failed_servers
  
  // 模拟server-001恢复
  recovered_servers = recovered_servers.map(fn(server) {
    if server.id == "server-001" {
      { server | 
        status = Healthy, 
        consecutive_failures = 0,
        metrics = { server.metrics | 
          "error_rate" = 0.01 
        }
      }
    } else {
      server
    }
  })
  
  // 16. 执行恢复检查
  let recovery_check_result = load_balancer.check_recovery(recovered_servers)
  
  // 17. 验证恢复检查结果
  assert_eq(recovery_check_result.recovered_servers.length(), 1)
  
  let recovered_server = recovery_check_result.recovered_servers[0]
  assert_eq(recovered_server.id, "server-001")
  assert_eq(recovered_server.status, Healthy)
  
  // 18. 测试负载均衡算法切换
  let algorithm_switch_result = load_balancer.switch_algorithm(LeastConnections)
  
  // 19. 验证算法切换结果
  assert_true(algorithm_switch_result.success)
  assert_eq(load_balancer.algorithm, LeastConnections)
}

// 测试6: 自适应扩缩容
test "自适应扩缩容测试" {
  // 1. 创建自适应扩缩容管理器
  let autoscaler = AdaptiveAutoscaler({
    scaling_policy: HybridScaling,
    metrics_collector: MetricsCollector({
      collection_interval: 30000, // 30秒
      metrics: [
        "cpu.usage",
        "memory.usage",
        "request.rate",
        "response.time"
      ]
    }),
    scaling_rules: [
      ScalingRule({
        name: "cpu_based_scaling",
        metric: "cpu.usage",
        scale_up_threshold: 80.0,
        scale_down_threshold: 20.0,
        scale_up_cooldown: 300000,    // 5分钟
        scale_down_cooldown: 600000,  // 10分钟
        scale_up_step: 2,
        scale_down_step: 1
      }),
      ScalingRule({
        name: "request_rate_based_scaling",
        metric: "request.rate",
        scale_up_threshold: 1000.0,
        scale_down_threshold: 200.0,
        scale_up_cooldown: 180000,    // 3分钟
        scale_down_cooldown: 300000,  // 5分钟
        scale_up_step: 3,
        scale_down_step: 1
      })
    ],
    prediction_enabled: true,
    min_instances: 2,
    max_instances: 20,
    scale_up_delay: 60000,    // 1分钟
    scale_down_delay: 300000  // 5分钟
  })
  
  // 2. 验证自适应扩缩容管理器
  assert_eq(autoscaler.scaling_policy, HybridScaling)
  assert_eq(autoscaler.scaling_rules.length(), 2)
  assert_true(autoscaler.prediction_enabled)
  assert_eq(autoscaler.min_instances, 2)
  assert_eq(autoscaler.max_instances, 20)
  
  // 3. 创建服务集群
  let service_cluster = ServiceCluster({
    name: "api-service",
    current_instances: 5,
    instance_template: InstanceTemplate({
      image: "azimuth/api-service:v1.0.0",
      cpu_request: 1,
      memory_request: 2048, // MB
      ports: [8080]
    }),
    scaling_history: []
  })
  
  // 4. 验证服务集群
  assert_eq(service_cluster.name, "api-service")
  assert_eq(service_cluster.current_instances, 5)
  
  // 5. 模拟高负载场景
  let high_load_metrics = [
    ("cpu.usage", 85.0),
    ("memory.usage", 75.0),
    ("request.rate", 1500.0),
    ("response.time", 200.0)
  ]
  
  // 6. 执行扩容决策
  let scale_up_decision = autoscaler.make_scaling_decision(service_cluster, high_load_metrics)
  
  // 7. 验证扩容决策
  match scale_up_decision {
    ScaleUp(new_instance_count, reason) => {
      assert_true(new_instance_count > service_cluster.current_instances)
      assert_true(reason.contains("cpu.usage") || reason.contains("request.rate"))
    }
    NoScaling(reason) => assert_true(false)
    ScaleDown(_, _) => assert_true(false)
  }
  
  // 8. 执行扩容操作
  let scale_up_result = autoscaler.execute_scaling(service_cluster, scale_up_decision)
  
  // 9. 验证扩容结果
  assert_true(scale_up_result.success)
  assert_true(scale_up_result.new_instance_count > service_cluster.current_instances)
  
  // 10. 更新集群状态
  let scaled_up_cluster = { service_cluster | 
    current_instances = scale_up_result.new_instance_count,
    scaling_history = service_cluster.scaling_history.push({
      timestamp: get_current_timestamp(),
      action: ScaleUp,
      old_instance_count: service_cluster.current_instances,
      new_instance_count: scale_up_result.new_instance_count,
      metrics: high_load_metrics,
      reason: match scale_up_decision {
        ScaleUp(_, reason) => reason
        _ => ""
      }
    })
  }
  
  // 11. 模拟低负载场景
  let low_load_metrics = [
    ("cpu.usage", 15.0),
    ("memory.usage", 30.0),
    ("request.rate", 100.0),
    ("response.time": 50.0)
  ]
  
  // 12. 执行缩容决策
  let scale_down_decision = autoscaler.make_scaling_decision(scaled_up_cluster, low_load_metrics)
  
  // 13. 验证缩容决策
  match scale_down_decision {
    ScaleUp(_, _) => assert_true(false)
    NoScaling(reason) => assert_true(false)
    ScaleDown(new_instance_count, reason) => {
      assert_true(new_instance_count < scaled_up_cluster.current_instances)
      assert_true(new_instance_count >= autoscaler.min_instances)
      assert_true(reason.contains("cpu.usage") || reason.contains("request.rate"))
    }
  }
  
  // 14. 执行缩容操作
  let scale_down_result = autoscaler.execute_scaling(scaled_up_cluster, scale_down_decision)
  
  // 15. 验证缩容结果
  assert_true(scale_down_result.success)
  assert_true(scale_down_result.new_instance_count < scaled_up_cluster.current_instances)
  assert_true(scale_down_result.new_instance_count >= autoscaler.min_instances)
  
  // 16. 测试预测性扩容
  let predictive_metrics = [
    ("cpu.usage", 60.0),
    ("memory.usage", 65.0),
    ("request.rate", 800.0),
    ("response.time", 120.0)
  ]
  
  let prediction_result = autoscaler.predict_scaling_needs(
    scaled_up_cluster,
    predictive_metrics,
    300000 // 预测未来5分钟
  )
  
  // 17. 验证预测结果
  assert_true(prediction_result.prediction_made)
  assert_true(prediction_result.confidence > 0.5)
  
  // 18. 测试突发流量处理
  let burst_traffic_metrics = [
    ("cpu.usage", 95.0),
    ("memory.usage", 85.0),
    ("request.rate", 2500.0),
    ("response.time", 500.0)
  ]
  
  let burst_handling_result = autoscaler.handle_burst_traffic(
    scaled_up_cluster,
    burst_traffic_metrics,
    120000 // 2分钟突发时间
  )
  
  // 19. 验证突发流量处理结果
  assert_true(burst_handling_result.burst_detected)
  assert_true(burst_handling_result.emergency_scaling_triggered)
  assert_true(burst_handling_result.new_instance_count > scaled_up_cluster.current_instances)
  
  // 20. 测试扩缩容策略优化
  let optimization_result = autoscaler.optimize_scaling_policy(scaled_up_cluster.scaling_history)
  
  // 21. 验证策略优化结果
  assert_true(optimization_result.optimization_applied)
  assert_true(optimization_result.policy_improvements.length() > 0)
  
  // 22. 生成扩缩容报告
  let scaling_report = autoscaler.generate_scaling_report(
    scaled_up_cluster.scaling_history,
    [
      ("high_load", high_load_metrics),
      ("low_load", low_load_metrics),
      ("burst_traffic", burst_traffic_metrics)
    ]
  )
  
  // 23. 验证扩缩容报告
  assert_true(scaling_report.contains("scale up"))
  assert_true(scaling_report.contains("scale down"))
  assert_true(scaling_report.contains("burst handling"))
  assert_true(scaling_report.contains("prediction"))
}

// 测试7: 自适应缓存策略
test "自适应缓存策略测试" {
  // 1. 创建自适应缓存管理器
  let cache_manager = AdaptiveCacheManager({
    cache_type: DistributedCache,
    eviction_policies: [
      LRU,
      LFU,
      TTL
    ],
    adaptation_interval: 60000, // 1分钟
    performance_metrics: [
      "hit_rate",
      "miss_rate",
      "eviction_rate",
      "memory_usage"
    ],
    optimization_goals: [
      OptimizationGoal({
        metric: "hit_rate",
        target: "> 0.9",
        weight: 0.5
      }),
      OptimizationGoal({
        metric: "memory_usage",
        target: "< 0.8",
        weight: 0.3
      }),
      OptimizationGoal({
        metric: "eviction_rate",
        target: "< 0.1",
        weight: 0.2
      })
    ]
  })
  
  // 2. 验证自适应缓存管理器
  assert_eq(cache_manager.cache_type, DistributedCache)
  assert_eq(cache_manager.eviction_policies.length(), 3)
  assert_eq(cache_manager.adaptation_interval, 60000)
  assert_eq(cache_manager.performance_metrics.length(), 4)
  
  // 3. 创建缓存节点
  let cache_nodes = [
    CacheNode({
      id: "node-001",
      host: "cache-001",
      port: 6379,
      capacity: 1024, // MB
      current_usage: 512, // MB
      eviction_policy: LRU,
      performance_metrics: {
        "hit_rate": 0.85,
        "miss_rate": 0.15,
        "eviction_rate": 0.05,
        "memory_usage": 0.5
      }
    }),
    CacheNode({
      id: "node-002",
      host: "cache-002",
      port: 6379,
      capacity: 1024, // MB
      current_usage: 768, // MB
      eviction_policy: LFU,
      performance_metrics: {
        "hit_rate": 0.92,
        "miss_rate": 0.08,
        "eviction_rate": 0.12,
        "memory_usage": 0.75
      }
    }),
    CacheNode({
      id: "node-003",
      host: "cache-003",
      port: 6379,
      capacity: 1024, // MB
      current_usage: 256, // MB
      eviction_policy: TTL,
      performance_metrics: {
        "hit_rate": 0.78,
        "miss_rate": 0.22,
        "eviction_rate": 0.02,
        "memory_usage": 0.25
      }
    })
  ]
  
  // 4. 验证缓存节点
  assert_eq(cache_nodes.length(), 3)
  
  // 5. 计算缓存性能得分
  let performance_scores = cache_nodes.map(fn(node) {
    let hit_rate_score = node.performance_metrics.get("hit_rate") * 0.5
    let memory_usage_score = (1.0 - node.performance_metrics.get("memory_usage")) * 0.3
    let eviction_rate_score = (1.0 - node.performance_metrics.get("eviction_rate")) * 0.2
    
    {
      node_id: node.id,
      total_score: hit_rate_score + memory_usage_score + eviction_rate_score,
      hit_rate: node.performance_metrics.get("hit_rate"),
      memory_usage: node.performance_metrics.get("memory_usage"),
      eviction_rate: node.performance_metrics.get("eviction_rate")
    }
  })
  
  // 6. 验证性能得分计算
  assert_eq(performance_scores.length(), 3)
  
  for score in performance_scores {
    assert_true(score.total_score > 0.0)
    assert_true(score.total_score <= 1.0)
  }
  
  // 7. 执行缓存策略自适应
  let adaptation_result = cache_manager.adapt_strategies(cache_nodes)
  
  // 8. 验证自适应结果
  assert_true(adaptation_result.adaptation_applied)
  assert_eq(adaptation_result.strategy_changes.length(), 1) // 至少有一个策略改变
  
  // 9. 验证策略改变
  let strategy_change = adaptation_result.strategy_changes[0]
  assert_not_eq(strategy_change.old_policy, strategy_change.new_policy)
  assert_true(strategy_change.reason.contains("optimization"))
  
  // 10. 测试缓存数据分布优化
  let distribution_result = cache_manager.optimize_data_distribution(cache_nodes)
  
  // 11. 验证数据分布优化结果
  assert_true(distribution_result.optimization_applied)
  assert_true(distribution_result.data_migrations.length() > 0)
  
  // 12. 验证数据迁移
  for migration in distribution_result.data_migrations {
    assert_not_eq(migration.source_node, migration.target_node)
    assert_true(migration.data_size > 0)
  }
  
  // 13. 模拟缓存访问模式变化
  let access_pattern_changes = [
    AccessPatternChange({
      key_pattern: "user:*",
      frequency_increase: 2.0, // 访问频率增加2倍
      ttl_preference: 3600     // 偏好1小时TTL
    }),
    AccessPatternChange({
      key_pattern: "product:*",
      frequency_increase: 0.5, // 访问频率减少50%
      ttl_preference: 1800     // 偏好30分钟TTL
    })
  ]
  
  // 14. 执行基于访问模式的缓存调整
  let pattern_based_adjustment = cache_manager.adjust_for_access_patterns(
    cache_nodes,
    access_pattern_changes
  )
  
  // 15. 验证基于访问模式的调整结果
  assert_true(pattern_based_adjustment.adjustment_applied)
  assert_eq(pattern_based_adjustment.ttl_adjustments.length(), 2)
  
  // 验证TTL调整
  let user_ttl_adjustment = pattern_based_adjustment.ttl_adjustments.filter(fn(adj) {
    adj.key_pattern == "user:*"
  })[0]
  
  assert_eq(user_ttl_adjustment.new_ttl, 3600)
  
  let product_ttl_adjustment = pattern_based_adjustment.ttl_adjustments.filter(fn(adj) {
    adj.key_pattern == "product:*"
  })[0]
  
  assert_eq(product_ttl_adjustment.new_ttl, 1800)
  
  // 16. 测试缓存预热策略
  let warmup_strategy = CacheWarmupStrategy({
    keys: [
      "user:001",
      "user:002",
      "product:001",
      "product:002"
    ],
    priority: High,
    ttl: 3600,
    warmup_percentage: 0.8 // 预热80%的键
  })
  
  let warmup_result = cache_manager.execute_warmup_strategy(cache_nodes, warmup_strategy)
  
  // 17. 验证缓存预热结果
  assert_true(warmup_result.success)
  assert_eq(warmup_result.warmed_up_keys.length(), 3) // 4个键的80%约等于3个
  
  // 18. 测试缓存一致性保证
  let consistency_check = cache_manager.verify_consistency(cache_nodes)
  
  // 19. 验证缓存一致性检查结果
  assert_true(consistency_check.consistent)
  assert_eq(consistency_check.inconsistent_keys.length(), 0)
  
  // 20. 生成缓存自适应报告
  let cache_adaptation_report = cache_manager.generate_adaptation_report([
    adaptation_result,
    distribution_result,
    pattern_based_adjustment,
    warmup_result,
    consistency_check
  ])
  
  // 21. 验证缓存自适应报告
  assert_true(cache_adaptation_report.contains("strategy adaptation"))
  assert_true(cache_adaptation_report.contains("data distribution"))
  assert_true(cache_adaptation_report.contains("access pattern"))
  assert_true(cache_adaptation_report.contains("warmup"))
  assert_true(cache_adaptation_report.contains("consistency"))
}

// 测试8: 自适应安全策略
test "自适应安全策略测试" {
  // 1. 创建自适应安全管理器
  let security_manager = AdaptiveSecurityManager({
    threat_detection_engine: ThreatDetectionEngine({
      detection_models: [
        AnomalyDetection,
        SignatureBasedDetection,
        BehavioralAnalysis
      ],
      sensitivity_level: Medium,
      false_positive_rate: 0.05
    }),
    response_strategies: [
      BlockIP,
      RequireAdditionalAuth,
      RateLimiting,
      AlertAdmin
    ],
    adaptation_interval: 300000, // 5分钟
    learning_enabled: true,
    feedback_loop: true
  })
  
  // 2. 验证自适应安全管理器
  assert_eq(security_manager.threat_detection_engine.detection_models.length(), 3)
  assert_eq(security_manager.response_strategies.length(), 4)
  assert_eq(security_manager.adaptation_interval, 300000)
  assert_true(security_manager.learning_enabled)
  assert_true(security_manager.feedback_loop)
  
  // 3. 创建安全事件
  let security_events = [
    SecurityEvent({
      id: "event-001",
      type: FailedLogin,
      timestamp: 1641016000000,
      source_ip: "192.168.1.100",
      user_id: "user-001",
      details: {
        "failure_reason": "invalid_password",
        "attempt_count": 3
      },
      severity: Medium
    }),
    SecurityEvent({
      id: "event-002",
      type: SuspiciousRequest,
      timestamp: 1641016001000,
      source_ip: "192.168.1.101",
      user_id: "user-002",
      details: {
        "request_pattern": "sql_injection_attempt",
        "request_count": 5
      },
      severity: High
    }),
    SecurityEvent({
      id: "event-003",
      type: DataAccess,
      timestamp: 1641016002000,
      source_ip: "192.168.1.102",
      user_id: "user-003",
      details: {
        "resource_type": "sensitive_data",
        "access_volume": "unusual"
      },
      severity: Low
    })
  ]
  
  // 4. 验证安全事件
  assert_eq(security_events.length(), 3)
  
  // 5. 执行威胁检测
  let threat_detection_result = security_manager.detect_threats(security_events)
  
  // 6. 验证威胁检测结果
  assert_eq(threat_detection_result.detected_threats.length(), 2) // 应该检测到2个威胁
  
  // 验证检测到的威胁
  let failed_login_threat = threat_detection_result.detected_threats.filter(fn(threat) {
    match threat.event_type {
      FailedLogin => true
      _ => false
    }
  })[0]
  
  assert_eq(failed_login_threat.confidence, 0.8)
  assert_eq(failed_login_threat.severity, Medium)
  
  let suspicious_request_threat = threat_detection_result.detected_threats.filter(fn(threat) {
    match threat.event_type {
      SuspiciousRequest => true
      _ => false
    }
  })[0]
  
  assert_eq(suspicious_request_threat.confidence, 0.9)
  assert_eq(suspicious_request_threat.severity, High)
  
  // 7. 执行安全响应
  let response_result = security_manager.respond_to_threats(threat_detection_result.detected_threats)
  
  // 8. 验证安全响应结果
  assert_true(response_result.response_executed)
  assert_eq(response_result.applied_responses.length(), 2)
  
  // 验证响应措施
  let failed_login_response = response_result.applied_responses.filter(fn(response) {
    response.threat_id == failed_login_threat.id
  })[0]
  
  match failed_login_response.strategy {
    RateLimiting(limit) => assert_eq(limit, 10) // 限制为每分钟10次请求
    _ => assert_true(false)
  }
  
  let suspicious_request_response = response_result.applied_responses.filter(fn(response) {
    response.threat_id == suspicious_request_threat.id
  })[0]
  
  match suspicious_request_response.strategy {
    BlockIP(duration) => assert_eq(duration, 3600) // 阻止1小时
    _ => assert_true(false)
  }
  
  // 9. 执行安全策略自适应
  let adaptation_result = security_manager.adapt_security_policies(
    threat_detection_result.detected_threats,
    response_result.applied_responses
  )
  
  // 10. 验证安全策略自适应结果
  assert_true(adaptation_result.adaptation_applied)
  assert_eq(adaptation_result.policy_adjustments.length(), 2)
  
  // 验证策略调整
  let failed_login_policy_adjustment = adaptation_result.policy_adjustments.filter(fn(adj) {
    adj.threat_type == FailedLogin
  })[0]
  
  assert_eq(failed_login_policy_adjustment.new_threshold, 2) // 失败登录阈值从3降到2
  assert_eq(failed_login_policy_adjustment.new_response_duration, 1800) // 响应持续时间从30分钟延长到1小时
  
  // 11. 测试安全策略学习
  let historical_events = [
    // 历史正常事件
    SecurityEvent({
      id: "historical-001",
      type: SuccessfulLogin,
      timestamp: 1641015000000,
      source_ip: "192.168.1.100",
      user_id: "user-001",
      details: {},
      severity: Low
    }),
    SecurityEvent({
      id: "historical-002",
      type: SuccessfulLogin,
      timestamp: 1641015001000,
      source_ip: "192.168.1.100",
      user_id: "user-001",
      details: {},
      severity: Low
    }),
    // 历史威胁事件
    SecurityEvent({
      id: "historical-003",
      type: FailedLogin,
      timestamp: 1641015002000,
      source_ip: "192.168.1.100",
      user_id: "user-001",
      details: {
        "failure_reason": "invalid_password",
        "attempt_count": 2
      },
      severity: Medium
    })
  ]
  
  let learning_result = security_manager.learn_from_events(historical_events)
  
  // 12. 验证安全策略学习结果
  assert_true(learning_result.learning_applied)
  assert_eq(learning_result.learned_patterns.length(), 1)
  
  let learned_pattern = learning_result.learned_patterns[0]
  assert_eq(learned_pattern.pattern_type, UserBehaviorPattern)
  assert_eq(learned_pattern.confidence, 0.7)
  
  // 13. 测试安全策略反馈循环
  let feedback_events = [
    SecurityEvent({
      id: "feedback-001",
      type: FalsePositive,
      timestamp: 1641017000000,
      source_ip: "192.168.1.103",
      user_id: "user-004",
      details: {
        "original_threat": "suspicious_activity",
        "corrected_label": "legitimate_activity"
      },
      severity: Low
    })
  ]
  
  let feedback_result = security_manager.process_feedback(feedback_events)
  
  // 14. 验证安全策略反馈结果
  assert_true(feedback_result.feedback_processed)
  assert_eq(feedback_result.model_adjustments.length(), 1)
  
  let model_adjustment = feedback_result.model_adjustments[0]
  assert_eq(model_adjustment.adjustment_type, SensitivityAdjustment)
  assert_true(model_adjustment.new_sensitivity < security_manager.threat_detection_engine.sensitivity_level)
  
  // 15. 测试安全策略性能评估
  let performance_evaluation = security_manager.evaluate_security_performance(
    security_events + historical_events + feedback_events
  )
  
  // 16. 验证安全策略性能评估
  assert_true(performance_evaluation.detection_accuracy > 0.8)
  assert_true(performance_evaluation.false_positive_rate < 0.1)
  assert_true(performance_evaluation.response_time < 1000) // 响应时间应小于1秒
  
  // 17. 生成安全策略自适应报告
  let security_adaptation_report = security_manager.generate_adaptation_report([
    threat_detection_result,
    response_result,
    adaptation_result,
    learning_result,
    feedback_result,
    performance_evaluation
  ])
  
  // 18. 验证安全策略自适应报告
  assert_true(security_adaptation_report.contains("threat detection"))
  assert_true(security_adaptation_report.contains("security response"))
  assert_true(security_adaptation_report.contains("policy adaptation"))
  assert_true(security_adaptation_report.contains("learning"))
  assert_true(security_adaptation_report.contains("feedback"))
  assert_true(security_adaptation_report.contains("performance evaluation"))
}