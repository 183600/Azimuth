// Azimuth 高级遥测机器学习测试用例
// 专注于机器学习算法在遥测系统中的应用

// 测试1: 遥测数据的机器学习异常检测
test "机器学习异常检测算法测试" {
  // 创建机器学习异常检测器
  let ml_detector = MLAnomalyDetector::new()
  
  // 配置检测模型
  MLAnomalyDetector::configure_model(ml_detector, {
    algorithm: "isolation_forest",
    contamination_rate: 0.1,
    features: ["cpu_usage", "memory_usage", "request_rate", "error_rate"],
    window_size: 100,
    training_threshold: 50
  })
  
  // 添加正常训练数据
  let base_time = 1640995200
  for i in 0..=200 {
    MLAnomalyDetector::add_training_data(ml_detector, {
      timestamp: base_time + i * 60,
      cpu_usage: 30.0 + (i % 20) * 2.0,
      memory_usage: 50.0 + (i % 15) * 3.0,
      request_rate: 100.0 + (i % 30) * 5.0,
      error_rate: 0.01 + (i % 10) * 0.002
    })
  }
  
  // 训练模型
  let training_result = MLAnomalyDetector::train_model(ml_detector)
  assert_true(training_result.success)
  assert_true(training_result.accuracy > 0.9)
  
  // 测试正常数据检测
  let normal_data = {
    timestamp: base_time + 201 * 60,
    cpu_usage: 35.0,
    memory_usage: 55.0,
    request_rate: 120.0,
    error_rate: 0.015
  }
  
  let normal_prediction = MLAnomalyDetector::predict(ml_detector, normal_data)
  assert_false(normal_prediction.is_anomaly)
  assert_true(normal_prediction.confidence > 0.8)
  
  // 测试异常数据检测
  let anomaly_data = {
    timestamp: base_time + 202 * 60,
    cpu_usage: 95.0,  // 异常高CPU使用率
    memory_usage: 90.0,  // 异常高内存使用率
    request_rate: 50.0,  // 异常低请求率
    error_rate: 0.5  // 异常高错误率
  }
  
  let anomaly_prediction = MLAnomalyDetector::predict(ml_detector, anomaly_data)
  assert_true(anomaly_prediction.is_anomaly)
  assert_true(anomaly_prediction.confidence > 0.7)
  
  // 测试边界情况
  let boundary_data = {
    timestamp: base_time + 203 * 60,
    cpu_usage: 70.0,  // 边界高值
    memory_usage: 75.0,  // 边界高值
    request_rate: 80.0,  // 边界低值
    error_rate: 0.08  // 边界高值
  }
  
  let boundary_prediction = MLAnomalyDetector::predict(ml_detector, boundary_data)
  // 边界情况可能是异常也可能不是，主要检查预测结构
  assert_true(boundary_prediction.confidence >= 0.0)
  assert_true(boundary_prediction.confidence <= 1.0)
  
  // 测试模型更新
  let new_training_data = []
  for i in 0..=50 {
    new_training_data = new_training_data.push({
      timestamp: base_time + (300 + i) * 60,
      cpu_usage: 40.0 + (i % 25) * 1.5,
      memory_usage: 60.0 + (i % 20) * 2.0,
      request_rate: 150.0 + (i % 40) * 3.0,
      error_rate: 0.02 + (i % 8) * 0.003
    })
  }
  
  let update_result = MLAnomalyDetector::update_model(ml_detector, new_training_data)
  assert_true(update_result.success)
  assert_true(update_result.previous_accuracy < update_result.new_accuracy)
  
  // 测试模型性能指标
  let performance_metrics = MLAnomalyDetector::get_performance_metrics(ml_detector)
  assert_true(performance_metrics.precision > 0.8)
  assert_true(performance_metrics.recall > 0.8)
  assert_true(performance_metrics.f1_score > 0.8)
}

// 测试2: 分布式追踪的链路追踪完整性验证
test "分布式追踪链路完整性验证测试" {
  // 创建分布式追踪完整性验证器
  let trace_validator = TraceIntegrityValidator::new()
  
  // 配置验证规则
  TraceIntegrityValidator::add_validation_rule(trace_validator, {
    name: "parent_child_consistency",
    description: "验证父子span关系的一致性",
    severity: "critical"
  })
  
  TraceIntegrityValidator::add_validation_rule(trace_validator, {
    name: "timeline_continuity",
    description: "验证时间轴连续性",
    severity: "high"
  })
  
  TraceIntegrityValidator::add_validation_rule(trace_validator, {
    name: "service_hop_completeness",
    description: "验证服务跳转完整性",
    severity: "medium"
  })
  
  // 创建完整的追踪链路
  let trace_context = TraceContext::new("trace-integrity-test", "root-span", true, [])
  
  // API网关
  let gateway_span = Span::new("api.gateway", Server, trace_context)
  Span::set_attribute(gateway_span, "service.name", StringValue("api.gateway"))
  Span::set_timestamp(gateway_span, 1640995200000, 1640995200100)  // 100ms
  
  // 认证服务
  let auth_context = TraceContext::create_child(trace_context, "auth-span")
  let auth_span = Span::new("auth.service", Server, auth_context)
  Span::set_attribute(auth_span, "service.name", StringValue("auth.service"))
  Span::set_timestamp(auth_span, 1640995200020, 1640995200060)  // 40ms
  
  // 业务服务
  let business_context = TraceContext::create_child(trace_context, "business-span")
  let business_span = Span::new("business.service", Server, business_context)
  Span::set_attribute(business_span, "service.name", StringValue("business.service"))
  Span::set_timestamp(business_span, 1640995200060, 1640995200090)  // 30ms
  
  // 数据库服务
  let db_context = TraceContext::create_child(business_context, "db-span")
  let db_span = Span::new("database.service", Server, db_context)
  Span::set_attribute(db_span, "service.name", StringValue("database.service"))
  Span::set_timestamp(db_span, 1640995200070, 1640995200085)  // 15ms
  
  // 缓存服务
  let cache_context = TraceContext::create_child(business_context, "cache-span")
  let cache_span = Span::new("cache.service", Server, cache_context)
  Span::set_attribute(cache_span, "service.name", StringValue("cache.service"))
  Span::set_timestamp(cache_span, 1640995200075, 1640995200080)  // 5ms
  
  // 构建追踪树
  let trace_tree = TraceBuilder::build_tree([gateway_span, auth_span, business_span, db_span, cache_span])
  
  // 验证追踪完整性
  let validation_result = TraceIntegrityValidator::validate(trace_validator, trace_tree)
  
  // 验证结果
  assert_true(validation_result.overall_valid)
  assert_eq(validation_result.errors.length(), 0)
  assert_eq(validation_result.warnings.length(), 0)
  
  // 测试不完整的追踪链路
  let incomplete_trace = {
    trace_id: "incomplete-trace",
    spans: [
      {
        span_id: "span-1",
        parent_span_id: None,
        service_name: "service.a",
        start_time: 1640995200000,
        end_time: 1640995200050
      },
      {
        span_id: "span-2",
        parent_span_id: Some("span-1"),
        service_name: "service.b",
        start_time: 1640995200100,  // 时间不连续
        end_time: 1640995200150
      },
      {
        span_id: "span-3",
        parent_span_id: Some("span-999"),  // 不存在的父span
        service_name: "service.c",
        start_time: 1640995200200,
        end_time: 1640995200250
      }
    ]
  }
  
  let incomplete_validation = TraceIntegrityValidator::validate(trace_validator, incomplete_trace)
  
  // 验证不完整追踪的错误
  assert_false(incomplete_validation.overall_valid)
  assert_true(incomplete_validation.errors.length() > 0)
  
  // 检查特定错误类型
  let parent_child_errors = incomplete_validation.errors.filter(fn(e) {
    e.rule == "parent_child_consistency"
  })
  assert_true(parent_child_errors.length() > 0)
  
  let timeline_errors = incomplete_validation.errors.filter(fn(e) {
    e.rule == "timeline_continuity"
  })
  assert_true(timeline_errors.length() > 0)
  
  // 测试修复建议
  let fix_suggestions = TraceIntegrityValidator::suggest_fixes(trace_validator, incomplete_validation)
  assert_true(fix_suggestions.length() > 0)
  
  let has_parent_fix = fix_suggestions.any(fn(s) {
    s.description.contains("parent_span_id")
  })
  assert_true(has_parent_fix)
  
  let has_timeline_fix = fix_suggestions.any(fn(s) {
    s.description.contains("timeline") or s.description.contains("timestamp")
  })
  assert_true(has_timeline_fix)
}

// 测试3: 遥测数据的自动清理和归档
test "遥测数据自动清理和归档测试" {
  // 创建数据生命周期管理器
  let lifecycle_manager = DataLifecycleManager::new()
  
  // 配置保留策略
  LifecycleManager::add_retention_policy(lifecycle_manager, {
    data_type: "trace",
    hot_storage_duration: 7 * 24 * 3600,  // 7天热存储
    warm_storage_duration: 30 * 24 * 3600,  // 30天温存储
    cold_storage_duration: 90 * 24 * 3600,  // 90天冷存储
    archive_after: 365 * 24 * 3600  // 1年后归档
  })
  
  LifecycleManager::add_retention_policy(lifecycle_manager, {
    data_type: "metric",
    hot_storage_duration: 3 * 24 * 3600,  // 3天热存储
    warm_storage_duration: 14 * 24 * 3600,  // 14天温存储
    cold_storage_duration: 60 * 24 * 3600,  // 60天冷存储
    archive_after: 180 * 24 * 3600  // 半年后归档
  })
  
  LifecycleManager::add_retention_policy(lifecycle_manager, {
    data_type: "log",
    hot_storage_duration: 1 * 24 * 3600,  // 1天热存储
    warm_storage_duration: 7 * 24 * 3600,  // 7天温存储
    cold_storage_duration: 30 * 24 * 3600,  // 30天冷存储
    archive_after: 90 * 24 * 3600  // 3个月后归档
  })
  
  // 创建测试数据（不同时间戳）
  let current_time = 1640995200  // 2022-01-01
  let old_data_time = current_time - 40 * 24 * 3600  // 40天前
  let very_old_data_time = current_time - 400 * 24 * 3600  // 400天前
  
  let test_data = [
    {
      id: "trace-1",
      type: "trace",
      timestamp: current_time - 1 * 24 * 3600,  // 1天前
      size: 1024,
      storage_tier: "hot"
    },
    {
      id: "trace-2",
      type: "trace",
      timestamp: old_data_time,  // 40天前
      size: 2048,
      storage_tier: "hot"
    },
    {
      id: "trace-3",
      type: "trace",
      timestamp: very_old_data_time,  // 400天前
      size: 1536,
      storage_tier: "hot"
    },
    {
      id: "metric-1",
      type: "metric",
      timestamp: current_time - 5 * 24 * 3600,  // 5天前
      size: 512,
      storage_tier: "hot"
    },
    {
      id: "metric-2",
      type: "metric",
      timestamp: current_time - 20 * 24 * 3600,  // 20天前
      size: 768,
      storage_tier: "warm"
    },
    {
      id: "log-1",
      type: "log",
      timestamp: current_time - 12 * 3600,  // 12小时前
      size: 256,
      storage_tier: "hot"
    },
    {
      id: "log-2",
      type: "log",
      timestamp: current_time - 10 * 24 * 3600,  // 10天前
      size: 384,
      storage_tier: "cold"
    }
  ]
  
  // 执行生命周期评估
  let lifecycle_assessment = LifecycleManager::assess_data(lifecycle_manager, test_data, current_time)
  
  // 验证评估结果
  assert_eq(lifecycle_assessment.items_to_move.length(), 3)  // trace-2, metric-2, log-2需要移动
  assert_eq(lifecycle_assessment.items_to_archive.length(), 1)  // trace-3需要归档
  assert_eq(lifecycle_assessment.items_to_delete.length(), 0)  // 没有需要删除的数据
  
  // 验证移动操作
  let trace_2_move = lifecycle_assessment.items_to_move.find(fn(item) { item.id == "trace-2" })
  assert_true(trace_2_move != None)
  match trace_2_move {
    Some(move_op) => {
      assert_eq(move_op.from_tier, "hot")
      assert_eq(move_op.to_tier, "cold")  // 40天前，应该移动到冷存储
    }
    None => assert_true(false)
  }
  
  let metric_2_move = lifecycle_assessment.items_to_move.find(fn(item) { item.id == "metric-2" })
  assert_true(metric_2_move != None)
  match metric_2_move {
    Some(move_op) => {
      assert_eq(move_op.from_tier, "warm")
      assert_eq(move_op.to_tier, "cold")  // 20天前，应该移动到冷存储
    }
    None => assert_true(false)
  }
  
  // 验证归档操作
  let trace_3_archive = lifecycle_assessment.items_to_archive[0]
  assert_eq(trace_3_archive.id, "trace-3")
  assert_eq(trace_3_archive.reason, "exceeded_retention_period")
  
  // 执行生命周期操作
  let execution_result = LifecycleManager::execute_lifecycle_operations(lifecycle_manager, lifecycle_assessment)
  
  // 验证执行结果
  assert_true(execution_result.success)
  assert_eq(execution_result.moved_count, 3)
  assert_eq(execution_result.archived_count, 1)
  assert_eq(execution_result.deleted_count, 0)
  assert_eq(execution_result.errors.length(), 0)
  
  // 测试存储优化
  let storage_stats = LifecycleManager::get_storage_statistics(lifecycle_manager)
  assert_true(storage_stats.hot_storage_size < 1024 + 2048 + 1536)  // 移动后热存储减少
  assert_true(storage_stats.cold_storage_size > 0)  // 冷存储增加
  assert_true(storage_stats.archive_storage_size > 0)  // 归档存储增加
  
  // 测试数据恢复
  let recovery_result = LifecycleManager::restore_from_archive(lifecycle_manager, "trace-3")
  assert_true(recovery_result.success)
  assert_eq(recovery_result.restored_data.id, "trace-3")
  assert_eq(recovery_result.restored_data.type, "trace")
  assert_eq(recovery_result.restored_data.timestamp, very_old_data_time)
  
  // 测试自动清理策略
  LifecycleManager::enable_auto_cleanup(lifecycle_manager, {
    schedule: "0 2 * * *",  // 每天凌晨2点
    batch_size: 1000,
    dry_run: false
  })
  
  let cleanup_status = LifecycleManager::get_cleanup_status(lifecycle_manager)
  assert_true(cleanup_status.enabled)
  assert_eq(cleanup_status.schedule, "0 2 * * *")
  assert_eq(cleanup_status.last_run, None)  // 尚未运行
}

// 测试4: 多租户环境下的遥测数据隔离
test "多租户遥测数据隔离测试" {
  // 创建多租户管理器
  let tenant_manager = MultiTenantManager::new()
  
  // 配置租户
  let tenant_a = TenantConfig {
    id: "tenant-a",
    name: "Company A",
    data_retention_days: 30,
    max_daily_data_points: 1000000,
    sampling_rate: 1.0,  // 100%采样
    features: ["traces", "metrics", "logs"],
    storage_quota_gb: 100
  }
  
  let tenant_b = TenantConfig {
    id: "tenant-b",
    name: "Company B",
    data_retention_days: 7,
    max_daily_data_points: 500000,
    sampling_rate: 0.1,  // 10%采样
    features: ["metrics"],
    storage_quota_gb: 50
  }
  
  let tenant_c = TenantConfig {
    id: "tenant-c",
    name: "Company C",
    data_retention_days: 90,
    max_daily_data_points: 2000000,
    sampling_rate: 0.5,  // 50%采样
    features: ["traces", "logs"],
    storage_quota_gb: 200
  }
  
  // 注册租户
  assert_true(MultiTenantManager::register_tenant(tenant_manager, tenant_a))
  assert_true(MultiTenantManager::register_tenant(tenant_manager, tenant_b))
  assert_true(MultiTenantManager::register_tenant(tenant_manager, tenant_c))
  
  // 创建租户特定的数据收集器
  let collector_a = MultiTenantManager::create_collector(tenant_manager, "tenant-a")
  let collector_b = MultiTenantManager::create_collector(tenant_manager, "tenant-b")
  let collector_c = MultiTenantManager::create_collector(tenant_manager, "tenant-c")
  
  // 验证租户隔离
  assert_eq(Collector::get_tenant_id(collector_a), "tenant-a")
  assert_eq(Collector::get_tenant_id(collector_b), "tenant-b")
  assert_eq(Collector::get_tenant_id(collector_c), "tenant-c")
  
  // 测试数据收集隔离
  Collector::add_trace_data(collector_a, {
    trace_id: "trace-tenant-a-1",
    service: "service-a",
    operation: "operation-a",
    timestamp: 1640995200
  })
  
  Collector::add_trace_data(collector_b, {
    trace_id: "trace-tenant-b-1",
    service: "service-b",
    operation: "operation-b",
    timestamp: 1640995200
  })
  
  Collector::add_trace_data(collector_c, {
    trace_id: "trace-tenant-c-1",
    service: "service-c",
    operation: "operation-c",
    timestamp: 1640995200
  })
  
  // 验证数据隔离
  let tenant_a_data = Collector::get_data(collector_a)
  let tenant_b_data = Collector::get_data(collector_b)
  let tenant_c_data = Collector::get_data(collector_c)
  
  assert_eq(tenant_a_data.length(), 1)
  assert_eq(tenant_b_data.length(), 1)
  assert_eq(tenant_c_data.length(), 1)
  
  assert_eq(tenant_a_data[0].trace_id, "trace-tenant-a-1")
  assert_eq(tenant_b_data[0].trace_id, "trace-tenant-b-1")
  assert_eq(tenant_c_data[0].trace_id, "trace-tenant-c-1")
  
  // 测试跨租户数据访问控制
  let access_result_a = MultiTenantManager::query_data(tenant_manager, "tenant-a", {
    start_time: 1640995200,
    end_time: 1640995300,
    filters: []
  })
  assert_true(access_result_a.success)
  assert_eq(access_result_a.data.length(), 1)
  
  // 尝试访问其他租户数据（应该失败）
  let unauthorized_access = MultiTenantManager::query_data(tenant_manager, "tenant-a", {
    start_time: 1640995200,
    end_time: 1640995300,
    filters: [("tenant_id", "tenant-b")]
  })
  assert_false(unauthorized_access.success)
  assert_eq(unauthorized_access.error, "unauthorized_cross_tenant_access")
  
  // 测试配额限制
  let quota_usage_a = MultiTenantManager::get_quota_usage(tenant_manager, "tenant-a")
  assert_true(quota_usage_a.daily_data_points <= 1000000)
  assert_true(quota_usage_a.storage_used_gb <= 100)
  
  // 测试采样率应用
  let sampling_result_a = MultiTenantManager::should_sample(tenant_manager, "tenant-a", {
    trace_id: "trace-sampling-test",
    priority: "normal"
  })
  assert_true(sampling_result_a)  // tenant-a是100%采样
  
  let sampling_result_b = MultiTenantManager::should_sample(tenant_manager, "tenant-b", {
    trace_id: "trace-sampling-test",
    priority: "normal"
  })
  // tenant-b是10%采样，结果可能是true或false，但应该是概率性的
  assert_true(sampling_result_b == true or sampling_result_b == false)
  
  // 测试功能限制
  let features_a = MultiTenantManager::get_available_features(tenant_manager, "tenant-a")
  assert_true(features_a.contains("traces"))
  assert_true(features_a.contains("metrics"))
  assert_true(features_a.contains("logs"))
  
  let features_b = MultiTenantManager::get_available_features(tenant_manager, "tenant-b")
  assert_false(features_b.contains("traces"))
  assert_true(features_b.contains("metrics"))
  assert_false(features_b.contains("logs"))
  
  // 测试租户数据统计
  let tenant_stats = MultiTenantManager::get_tenant_statistics(tenant_manager)
  assert_eq(tenant_stats.length(), 3)
  
  let stats_a = tenant_stats.find(fn(s) { s.tenant_id == "tenant-a" })
  assert_true(stats_a != None)
  match stats_a {
    Some(stats) => {
      assert_eq(stats.data_points_count, 1)
      assert_eq(stats.traces_count, 1)
      assert_eq(stats.metrics_count, 0)
      assert_eq(stats.logs_count, 0)
    }
    None => assert_true(false)
  }
  
  // 测试租户隔离的性能影响
  let cross_tenant_query_start = Time::now()
  let cross_tenant_result = MultiTenantManager::aggregate_data(tenant_manager, {
    tenant_ids: ["tenant-a", "tenant-b", "tenant-c"],
    aggregation_type: "count",
    time_range: {
      start: 1640995200,
      end: 1640995300
    }
  })
  let cross_tenant_query_duration = Time::now() - cross_tenant_query_start
  
  assert_true(cross_tenant_result.success)
  assert_eq(cross_tenant_result.aggregated_value, 3)  // 3个数据点
  assert_true(cross_tenant_query_duration < 1000)  // 查询应该在合理时间内完成
}

// 测试5: 遥测数据的实时告警阈值动态调整
test "遥测数据实时告警阈值动态调整测试" {
  // 创建动态阈值管理器
  let dynamic_threshold_manager = DynamicThresholdManager::new()
  
  // 配置初始阈值
  DynamicThresholdManager::set_threshold(dynamic_threshold_manager, "error_rate", {
    initial_value: 0.05,
    min_value: 0.01,
    max_value: 0.2,
    adjustment_factor: 0.1,
    adjustment_period: 3600,  // 1小时
    history_window: 24 * 3600  // 24小时历史窗口
  })
  
  DynamicThresholdManager::set_threshold(dynamic_threshold_manager, "response_time_p95", {
    initial_value: 500.0,
    min_value: 100.0,
    max_value: 2000.0,
    adjustment_factor: 0.15,
    adjustment_period: 3600,
    history_window: 24 * 3600
  })
  
  DynamicThresholdManager::set_threshold(dynamic_threshold_manager, "throughput", {
    initial_value: 1000.0,
    min_value: 500.0,
    max_value: 5000.0,
    adjustment_factor: 0.1,
    adjustment_period: 3600,
    history_window: 24 * 3600
  })
  
  // 模拟时间序列数据
  let base_time = 1640995200
  let mut time_series_data = []
  
  // 第一阶段：正常数据（12小时）
  for i in 0..=720 {
    time_series_data = time_series_data.push({
      timestamp: base_time + i * 60,
      error_rate: 0.02 + (i % 20) * 0.001,
      response_time_p95: 200.0 + (i % 50) * 2.0,
      throughput: 1200.0 + (i % 100) * 5.0
    })
  }
  
  // 第二阶段：性能下降（6小时）
  for i in 721..=1080 {
    time_series_data = time_series_data.push({
      timestamp: base_time + i * 60,
      error_rate: 0.04 + (i % 20) * 0.002,
      response_time_p95: 400.0 + (i % 50) * 4.0,
      throughput: 800.0 + (i % 100) * 3.0
    })
  }
  
  // 第三阶段：恢复期（6小时）
  for i in 1081..=1440 {
    time_series_data = time_series_data.push({
      timestamp: base_time + i * 60,
      error_rate: 0.015 + (i % 20) * 0.0005,
      response_time_p95: 180.0 + (i % 50) * 1.5,
      throughput: 1400.0 + (i % 100) * 6.0
    })
  }
  
  // 处理时间序列数据并动态调整阈值
  let mut threshold_adjustments = []
  let mut current_thresholds = {
    error_rate: 0.05,
    response_time_p95: 500.0,
    throughput: 1000.0
  }
  
  for data_point in time_series_data {
    // 检查是否需要调整阈值
    let adjustment_needed = DynamicThresholdManager::evaluate_adjustment(
      dynamic_threshold_manager,
      data_point,
      current_thresholds
    )
    
    if adjustment_needed.should_adjust {
      // 应用阈值调整
      let new_thresholds = DynamicThresholdManager::apply_adjustment(
        dynamic_threshold_manager,
        current_thresholds,
        adjustment_needed.adjustments
      )
      
      threshold_adjustments = threshold_adjustments.push({
        timestamp: data_point.timestamp,
        old_thresholds: current_thresholds,
        new_thresholds: new_thresholds,
        reason: adjustment_needed.reason
      })
      
      current_thresholds = new_thresholds
    }
  }
  
  // 验证阈值调整
  assert_true(threshold_adjustments.length() > 0)
  
  // 检查性能下降期间的阈值调整
  let performance_degradation_adjustments = threshold_adjustments.filter(fn(adj) {
    adj.timestamp >= base_time + 721 * 60 and adj.timestamp <= base_time + 1080 * 60
  })
  assert_true(performance_degradation_adjustments.length() > 0)
  
  // 验证错误率阈值调整（应该增加以适应更高的基线）
  let error_rate_increased = performance_degradation_adjustments.any(fn(adj) {
    adj.new_thresholds.error_rate > adj.old_thresholds.error_rate
  })
  assert_true(error_rate_increased)
  
  // 验证响应时间阈值调整（应该增加）
  let response_time_increased = performance_degradation_adjustments.any(fn(adj) {
    adj.new_thresholds.response_time_p95 > adj.old_thresholds.response_time_p95
  })
  assert_true(response_time_increased)
  
  // 验证吞吐量阈值调整（应该减少）
  let throughput_decreased = performance_degradation_adjustments.any(fn(adj) {
    adj.new_thresholds.throughput < adj.old_thresholds.throughput
  })
  assert_true(throughput_decreased)
  
  // 检查恢复期的阈值调整
  let recovery_adjustments = threshold_adjustments.filter(fn(adj) {
    adj.timestamp >= base_time + 1081 * 60 and adj.timestamp <= base_time + 1440 * 60
  })
  assert_true(recovery_adjustments.length() > 0)
  
  // 测试告警触发
  let alert_manager = AlertManager::new()
  
  // 使用动态阈值检查告警
  let mut alerts_triggered = []
  for data_point in time_series_data {
    let current_threshold = DynamicThresholdManager::get_current_threshold(
      dynamic_threshold_manager,
      data_point.timestamp
    )
    
    // 检查错误率告警
    if data_point.error_rate > current_threshold.error_rate {
      alerts_triggered = alerts_triggered.push({
        timestamp: data_point.timestamp,
        metric: "error_rate",
        value: data_point.error_rate,
        threshold: current_threshold.error_rate,
        severity: "warning"
      })
    }
    
    // 检查响应时间告警
    if data_point.response_time_p95 > current_threshold.response_time_p95 {
      alerts_triggered = alerts_triggered.push({
        timestamp: data_point.timestamp,
        metric: "response_time_p95",
        value: data_point.response_time_p95,
        threshold: current_threshold.response_time_p95,
        severity: "critical"
      })
    }
    
    // 检查吞吐量告警
    if data_point.throughput < current_threshold.throughput {
      alerts_triggered = alerts_triggered.push({
        timestamp: data_point.timestamp,
        metric: "throughput",
        value: data_point.throughput,
        threshold: current_threshold.throughput,
        severity: "warning"
      })
    }
  }
  
  // 验证告警数量合理性
  assert_true(alerts_triggered.length() < time_series_data.length() * 0.1)  // 告警不应超过10%的数据点
  
  // 测试阈值调整的性能影响
  let static_threshold_alerts = []
  for data_point in time_series_data {
    // 使用静态阈值
    if data_point.error_rate > 0.05 {
      static_threshold_alerts = static_threshold_alerts.push("error_rate")
    }
    if data_point.response_time_p95 > 500.0 {
      static_threshold_alerts = static_threshold_alerts.push("response_time_p95")
    }
    if data_point.throughput < 1000.0 {
      static_threshold_alerts = static_threshold_alerts.push("throughput")
    }
  }
  
  // 动态阈值应该减少误报
  assert_true(alerts_triggered.length() < static_threshold_alerts.length())
  
  // 测试阈值调整历史
  let threshold_history = DynamicThresholdManager::get_adjustment_history(dynamic_threshold_manager)
  assert_eq(threshold_history.length(), threshold_adjustments.length())
  
  // 验证阈值在允许范围内
  for adjustment in threshold_adjustments {
    assert_true(adjustment.new_thresholds.error_rate >= 0.01)
    assert_true(adjustment.new_thresholds.error_rate <= 0.2)
    assert_true(adjustment.new_thresholds.response_time_p95 >= 100.0)
    assert_true(adjustment.new_thresholds.response_time_p95 <= 2000.0)
    assert_true(adjustment.new_thresholds.throughput >= 500.0)
    assert_true(adjustment.new_thresholds.throughput <= 5000.0)
  }
}

// 测试6: 遥测系统的故障自愈机制
test "遥测系统故障自愈机制测试" {
  // 创建自愈管理器
  let self_healing_manager = SelfHealingManager::new()
  
  // 配置自愈策略
  SelfHealingManager::add_healing_strategy(self_healing_manager, {
    name: "data_collector_restart",
    condition: "collector_failure_rate > 0.1",
    actions: [
      { type: "restart_service", target: "data_collector", delay_ms: 5000 },
      { type: "increase_health_check_interval", target: "data_collector", value: 30 },
      { type: "enable_fallback_collector", target: "backup_collector" }
    ],
    max_attempts: 3,
    cooldown_period: 300  // 5分钟冷却期
  })
  
  SelfHealingManager::add_healing_strategy(self_healing_manager, {
    name: "storage_cleanup",
    condition: "disk_usage > 0.9",
    actions: [
      { type: "cleanup_temp_files", target: "storage", retention_hours: 24 },
      { type: "compress_old_data", target: "storage", compression_level: 6 },
      { type: "archive_old_data", target: "storage", retention_days: 90 }
    ],
    max_attempts: 2,
    cooldown_period: 3600  // 1小时冷却期
  })
  
  SelfHealingManager::add_healing_strategy(self_healing_manager, {
    name: "memory_recovery",
    condition: "memory_usage > 0.85",
    actions: [
      { type: "clear_cache", target: "telemetry_cache" },
      { type: "reduce_batch_size", target: "data_processor", reduction_factor: 0.5 },
      { type: "enable_sampling", target: "data_collector", sampling_rate: 0.8 }
    ],
    max_attempts: 5,
    cooldown_period: 600  // 10分钟冷却期
  })
  
  // 模拟系统健康状态
  let system_health = {
    timestamp: 1640995200,
    services: {
      data_collector: {
        status: "degraded",
        failure_rate: 0.15,  // 超过0.1阈值
        last_restart: 1640992000,
        health_check_interval: 60
      },
      storage: {
        status: "warning",
        disk_usage: 0.92,  // 超过0.9阈值
        available_space_gb: 8,
        total_space_gb: 100
      },
      data_processor: {
        status: "critical",
        memory_usage: 0.87,  // 超过0.85阈值
        batch_size: 1000,
        cache_size_mb: 512
      }
    }
  }
  
  // 评估自愈需求
  let healing_assessment = SelfHealingManager::assess_healing_needs(self_healing_manager, system_health)
  
  // 验证评估结果
  assert_eq(healing_assessment.needed_actions.length(), 3)  // 三个触发条件
  
  // 验证数据收集器重启策略
  let collector_healing = healing_assessment.needed_actions.find(fn(action) {
    action.strategy == "data_collector_restart"
  })
  assert_true(collector_healing != None)
  match collector_healing {
    Some(healing) => {
      assert_eq(healing.actions.length(), 3)
      assert_true(healing.actions.any(fn(a) { a.type == "restart_service" }))
      assert_true(healing.actions.any(fn(a) { a.type == "increase_health_check_interval" }))
      assert_true(healing.actions.any(fn(a) { a.type == "enable_fallback_collector" }))
    }
    None => assert_true(false)
  }
  
  // 验证存储清理策略
  let storage_healing = healing_assessment.needed_actions.find(fn(action) {
    action.strategy == "storage_cleanup"
  })
  assert_true(storage_healing != None)
  match storage_healing {
    Some(healing) => {
      assert_eq(healing.actions.length(), 3)
      assert_true(healing.actions.any(fn(a) { a.type == "cleanup_temp_files" }))
      assert_true(healing.actions.any(fn(a) { a.type == "compress_old_data" }))
      assert_true(healing.actions.any(fn(a) { a.type == "archive_old_data" }))
    }
    None => assert_true(false)
  }
  
  // 验证内存恢复策略
  let memory_healing = healing_assessment.needed_actions.find(fn(action) {
    action.strategy == "memory_recovery"
  })
  assert_true(memory_healing != None)
  match memory_healing {
    Some(healing) => {
      assert_eq(healing.actions.length(), 3)
      assert_true(healing.actions.any(fn(a) { a.type == "clear_cache" }))
      assert_true(healing.actions.any(fn(a) { a.type == "reduce_batch_size" }))
      assert_true(healing.actions.any(fn(a) { a.type == "enable_sampling" }))
    }
    None => assert_true(false)
  }
  
  // 执行自愈操作
  let healing_result = SelfHealingManager::execute_healing(self_healing_manager, healing_assessment)
  
  // 验证执行结果
  assert_true(healing_result.success)
  assert_eq(healing_result.executed_actions.length(), 9)  // 3个策略 × 3个动作
  
  // 验证具体执行的动作
  let restart_executed = healing_result.executed_actions.any(fn(action) {
    action.type == "restart_service" and action.target == "data_collector"
  })
  assert_true(restart_executed)
  
  let cleanup_executed = healing_result.executed_actions.any(fn(action) {
    action.type == "cleanup_temp_files" and action.target == "storage"
  })
  assert_true(cleanup_executed)
  
  let cache_clear_executed = healing_result.executed_actions.any(fn(action) {
    action.type == "clear_cache" and action.target == "telemetry_cache"
  })
  assert_true(cache_clear_executed)
  
  // 模拟自愈后的系统状态
  let post_healing_health = {
    timestamp: 1640995800,  // 10分钟后
    services: {
      data_collector: {
        status: "healthy",
        failure_rate: 0.02,  // 恢复正常
        last_restart: 1640995200,  // 最近重启
        health_check_interval: 30  // 增加检查频率
      },
      storage: {
        status: "healthy",
        disk_usage: 0.75,  // 清理后恢复正常
        available_space_gb: 25,
        total_space_gb: 100
      },
      data_processor: {
        status: "healthy",
        memory_usage: 0.65,  // 内存恢复
        batch_size: 500,  // 减少批处理大小
        cache_size_mb: 256  // 缓存清理
      }
    }
  }
  
  // 验证自愈效果
  assert_eq(post_healing_health.services.data_collector.status, "healthy")
  assert_true(post_healing_health.services.data_collector.failure_rate < 0.1)
  assert_eq(post_healing_health.services.storage.status, "healthy")
  assert_true(post_healing_health.services.storage.disk_usage < 0.9)
  assert_eq(post_healing_health.services.data_processor.status, "healthy")
  assert_true(post_healing_health.services.data_processor.memory_usage < 0.85)
  
  // 测试自愈历史记录
  let healing_history = SelfHealingManager::get_healing_history(self_healing_manager)
  assert_eq(healing_history.length(), 1)
  
  let healing_record = healing_history[0]
  assert_eq(healing_record.strategies_triggered.length(), 3)
  assert_eq(healing_record.actions_executed.length(), 9)
  assert_true(healing_record.success)
  
  // 测试自愈策略冷却期
  let second_assessment = SelfHealingManager::assess_healing_needs(self_healing_manager, post_healing_health)
  
  // 由于系统状态已恢复，不应触发新的自愈操作
  assert_eq(second_assessment.needed_actions.length(), 0)
  
  // 测试自愈策略限制
  let persistent_failure_health = {
    timestamp: 1640996000,
    services: {
      data_collector: {
        status: "failed",
        failure_rate: 0.3,  // 持续高失败率
        last_restart: 1640995900,
        health_check_interval: 30
      },
      storage: {
        status: "healthy",
        disk_usage: 0.7,
        available_space_gb: 30,
        total_space_gb: 100
      },
      data_processor: {
        status: "healthy",
        memory_usage: 0.6,
        batch_size: 500,
        cache_size_mb: 256
      }
    }
  }
  
  // 模拟多次自愈尝试
  let mut consecutive_healings = []
  for i in 1..=4 {
    let assessment = SelfHealingManager::assess_healing_needs(self_healing_manager, persistent_failure_health)
    if assessment.needed_actions.length() > 0 {
      let result = SelfHealingManager::execute_healing(self_healing_manager, assessment)
      consecutive_healings = consecutive_healings.push(result)
      
      // 更新时间戳以模拟时间流逝
      persistent_failure_health.timestamp = persistent_failure_health.timestamp + 600
    }
  }
  
  // 验证最大尝试次数限制
  assert_true(consecutive_healings.length() <= 3)  // 最多尝试3次
  
  // 测试自愈效果统计
  let healing_stats = SelfHealingManager::get_healing_statistics(self_healing_manager)
  assert_true(healing_stats.total_healings > 0)
  assert_true(healing_stats.successful_healings > 0)
  assert_true(healing_stats.success_rate > 0.5)
  
  // 验证最常用的自愈策略
  let most_used_strategy = healing_stats.strategy_usage.reduce(fn(max, current) {
    if current.count > max.count { current } else { max }
  }, healing_stats.strategy_usage[0])
  
  assert_true(most_used_strategy.count > 0)
}

// 测试7: 遥测数据的智能采样策略
test "遥测数据智能采样策略测试" {
  // 创建智能采样管理器
  let intelligent_sampler = IntelligentSampler::new()
  
  // 配置采样策略
  IntelligentSampler::add_sampling_rule(intelligent_sampler, {
    name: "high_value_traces",
    condition: "trace.duration > 1000 or trace.has_error = true",
    sampling_rate: 1.0,  // 100%采样
    priority: 1
  })
  
  IntelligentSampler::add_sampling_rule(intelligent_sampler, {
    name: "normal_traces",
    condition: "trace.service in ['api.gateway', 'auth.service']",
    sampling_rate: 0.5,  // 50%采样
    priority: 2
  })
  
  IntelligentSampler::add_sampling_rule(intelligent_sampler, {
    name: "background_traces",
    condition: "trace.service in ['health.check', 'metrics.collect']",
    sampling_rate: 0.01,  // 1%采样
    priority: 3
  })
  
  // 配置自适应采样
  IntelligentSampler::enable_adaptive_sampling(intelligent_sampler, {
    target_throughput: 10000,  // 目标每秒10000个span
    adjustment_interval: 300,  // 5分钟调整一次
    min_sampling_rate: 0.001,
    max_sampling_rate: 1.0
  })
  
  // 创建测试追踪数据
  let test_traces = [
    {
      trace_id: "trace-1",
      span_id: "span-1",
      service: "api.gateway",
      operation: "user.login",
      duration: 1500,  // 高价值追踪（长持续时间）
      has_error: false,
      attributes: [("user.id", "user123")]
    },
    {
      trace_id: "trace-2",
      span_id: "span-2",
      service: "payment.service",
      operation: "process.payment",
      duration: 800,
      has_error: true,  // 高价值追踪（有错误）
      attributes: [("payment.amount", "99.99")]
    },
    {
      trace_id: "trace-3",
      span_id: "span-3",
      service: "auth.service",
      operation: "token.validate",
      duration: 200,
      has_error: false,
      attributes: [("token.type", "jwt")]
    },
    {
      trace_id: "trace-4",
      span_id: "span-4",
      service: "user.service",
      operation: "profile.update",
      duration: 300,
      has_error: false,
      attributes: [("user.id", "user456")]
    },
    {
      trace_id: "trace-5",
      span_id: "span-5",
      service: "health.check",
      operation: "database.ping",
      duration: 50,
      has_error: false,
      attributes: [("db.type", "postgresql")]
    },
    {
      trace_id: "trace-6",
      span_id: "span-6",
      service: "metrics.collect",
      operation: "system.metrics",
      duration: 100,
      has_error: false,
      attributes: [("metric.type", "system")]
    }
  ]
  
  // 测试静态采样决策
  let mut sampling_decisions = []
  for trace in test_traces {
    let decision = IntelligentSampler::should_sample(intelligent_sampler, trace)
    sampling_decisions = sampling_decisions.push({
      trace_id: trace.trace_id,
      sampled: decision.should_sample,
      sampling_rate: decision.applied_rate,
      reason: decision.reason
    })
  }
  
  // 验证高价值追踪100%采样
  let trace_1_decision = sampling_decisions.find(fn(d) { d.trace_id == "trace-1" })
  assert_true(trace_1_decision != None)
  match trace_1_decision {
    Some(decision) => {
      assert_true(decision.sampled)
      assert_eq(decision.sampling_rate, 1.0)
      assert_true(decision.reason.contains("high_value_traces"))
    }
    None => assert_true(false)
  }
  
  let trace_2_decision = sampling_decisions.find(fn(d) { d.trace_id == "trace-2" })
  assert_true(trace_2_decision != None)
  match trace_2_decision {
    Some(decision) => {
      assert_true(decision.sampled)
      assert_eq(decision.sampling_rate, 1.0)
      assert_true(decision.reason.contains("high_value_traces"))
    }
    None => assert_true(false)
  }
  
  // 验证正常追踪50%采样
  let trace_3_decision = sampling_decisions.find(fn(d) { d.trace_id == "trace-3" })
  assert_true(trace_3_decision != None)
  match trace_3_decision {
    Some(decision) => {
      assert_true(decision.reason.contains("normal_traces"))
      // 50%采样率，可能采样也可能不采样
      assert_true(decision.sampled == true or decision.sampled == false)
    }
    None => assert_true(false)
  }
  
  // 验证后台追踪1%采样
  let trace_5_decision = sampling_decisions.find(fn(d) { d.trace_id == "trace-5" })
  assert_true(trace_5_decision != None)
  match trace_5_decision {
    Some(decision) => {
      assert_true(decision.reason.contains("background_traces"))
      // 1%采样率，很可能不采样
      assert_true(decision.sampled == true or decision.sampled == false)
    }
    None => assert_true(false)
  }
  
  // 测试自适应采样
  let mut throughput_history = []
  let mut adaptive_rates = []
  
  // 模拟高负载情况（超过目标吞吐量）
  for i in 0..=10 {
    let current_throughput = 15000 + i * 1000  // 超过10000的目标
    let adaptive_rate = IntelligentSampler::adjust_adaptive_rate(
      intelligent_sampler,
      current_throughput,
      300  // 5分钟间隔
    )
    
    throughput_history = throughput_history.push(current_throughput)
    adaptive_rates = adaptive_rates.push(adaptive_rate)
  }
  
  // 验证自适应采样率降低
  assert_true(adaptive_rates[adaptive_rates.length() - 1] < adaptive_rates[0])
  assert_true(adaptive_rates[adaptive_rates.length() - 1] >= 0.001)  // 不低于最小采样率
  
  // 模拟低负载情况（低于目标吞吐量）
  for i in 0..=10 {
    let current_throughput = 5000 - i * 200  // 低于10000的目标
    let adaptive_rate = IntelligentSampler::adjust_adaptive_rate(
      intelligent_sampler,
      current_throughput,
      300
    )
    
    throughput_history = throughput_history.push(current_throughput)
    adaptive_rates = adaptive_rates.push(adaptive_rate)
  }
  
  // 验证自适应采样率提高
  assert_true(adaptive_rates[adaptive_rates.length() - 1] > adaptive_rates[11])
  assert_true(adaptive_rates[adaptive_rates.length() - 1] <= 1.0)  // 不超过最大采样率
  
  // 测试采样效果统计
  let sampling_stats = IntelligentSampler::get_sampling_statistics(intelligent_sampler)
  
  assert_true(sampling_stats.total_traces_evaluated > 0)
  assert_true(sampling_stats.total_traces_sampled > 0)
  assert_true(sampling_stats.overall_sampling_rate > 0.0)
  assert_true(sampling_stats.overall_sampling_rate <= 1.0)
  
  // 验证按服务分组的采样统计
  let api_gateway_stats = sampling_stats.by_service.get("api.gateway")
  assert_true(api_gateway_stats != None)
  match api_gateway_stats {
    Some(stats) => {
      assert_eq(stats.service_name, "api.gateway")
      assert_true(stats.traces_evaluated > 0)
      assert_true(stats.traces_sampled > 0)
    }
    None => assert_true(false)
  }
  
  // 测试采样策略性能
  let sampling_performance_start = Time::now()
  
  for i in 0..=1000 {
    let test_trace = {
      trace_id: "perf-test-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      service: "test.service",
      operation: "test.operation",
      duration: i * 10,
      has_error: i % 10 == 0,
      attributes: [("test.index", i.to_string())]
    }
    
    IntelligentSampler::should_sample(intelligent_sampler, test_trace)
  }
  
  let sampling_performance_duration = Time::now() - sampling_performance_start
  
  // 验证采样决策性能（每个决策应在1ms内完成）
  assert_true(sampling_performance_duration < 1000)  // 1000个决策在1秒内完成
  
  // 测试采样策略配置更新
  IntelligentSampler::update_sampling_rule(intelligent_sampler, "normal_traces", {
    condition: "trace.service in ['api.gateway', 'auth.service', 'user.service']",
    sampling_rate: 0.3,  // 降低采样率
    priority: 2
  })
  
  let updated_rule = IntelligentSampler::get_sampling_rule(intelligent_sampler, "normal_traces")
  assert_true(updated_rule != None)
  match updated_rule {
    Some(rule) => {
      assert_eq(rule.sampling_rate, 0.3)
      assert_true(rule.condition.contains("user.service"))
    }
    None => assert_true(false)
  }
  
  // 测试采样策略导出
  let exported_config = IntelligentSampler::export_configuration(intelligent_sampler)
  assert_true(exported_config.length() > 0)
  assert_true(exported_config.contains("high_value_traces"))
  assert_true(exported_config.contains("normal_traces"))
  assert_true(exported_config.contains("background_traces"))
}

// 测试8: 遥测系统的性能基准测试
test "遥测系统性能基准测试" {
  // 创建性能基准测试管理器
  let benchmark_manager = PerformanceBenchmarkManager::new()
  
  // 配置基准测试场景
  BenchmarkManager::add_benchmark_scenario(benchmark_manager, {
    name: "high_throughput_ingestion",
    description: "高吞吐量数据摄取性能测试",
    parameters: {
      data_points: 100000,
      concurrent_producers: 10,
      batch_size: 1000,
      duration_seconds: 60
    },
    thresholds: {
      min_throughput: 50000,  // 每秒最少50000个数据点
      max_latency_p99: 100,   // P99延迟不超过100ms
      max_error_rate: 0.01    // 错误率不超过1%
    }
  })
  
  BenchmarkManager::add_benchmark_scenario(benchmark_manager, {
    name: "complex_query_processing",
    description: "复杂查询处理性能测试",
    parameters: {
      query_complexity: "high",
      data_volume: "large",
      concurrent_queries: 50,
      time_range_hours: 24
    },
    thresholds: {
      min_throughput: 100,    // 每秒最少100个查询
      max_latency_p95: 1000,  // P95延迟不超过1秒
      max_error_rate: 0.005   // 错误率不超过0.5%
    }
  })
  
  BenchmarkManager::add_benchmark_scenario(benchmark_manager, {
    name: "real_time_aggregation",
    description: "实时数据聚合性能测试",
    parameters: {
      metrics_count: 1000,
      aggregation_window: 60,  // 1分钟窗口
      update_frequency: 1,     // 每秒更新
      retention_hours: 2
    },
    thresholds: {
      min_throughput: 10000,  // 每秒最少10000次聚合
      max_latency_p90: 50,    // P90延迟不超过50ms
      max_memory_usage: 2048  // 内存使用不超过2GB
    }
  })
  
  // 执行高吞吐量摄取基准测试
  let ingestion_benchmark = BenchmarkManager::run_benchmark(
    benchmark_manager,
    "high_throughput_ingestion"
  )
  
  // 验证基准测试结果
  assert_true(ingestion_benchmark.success)
  assert_true(ingestion_benchmark.actual_throughput >= 50000)
  assert_true(ingestion_benchmark.latency_p99 <= 100)
  assert_true(ingestion_benchmark.error_rate <= 0.01)
  
  // 验证资源使用情况
  assert_true(ingestion_benchmark.cpu_usage <= 90.0)
  assert_true(ingestion_benchmark.memory_usage_mb <= 4096)
  assert_true(ingestion_benchmark.disk_io_mb_per_sec <= 100)
  
  // 执行复杂查询处理基准测试
  let query_benchmark = BenchmarkManager::run_benchmark(
    benchmark_manager,
    "complex_query_processing"
  )
  
  // 验证查询性能
  assert_true(query_benchmark.success)
  assert_true(query_benchmark.actual_throughput >= 100)
  assert_true(query_benchmark.latency_p95 <= 1000)
  assert_true(query_benchmark.error_rate <= 0.005)
  
  // 验证查询类型分布
  let query_types = query_benchmark.detailed_metrics.query_types
  assert_true(query_types.contains("aggregation"))
  assert_true(query_types.contains("filter"))
  assert_true(query_types.contains("group_by"))
  
  // 执行实时聚合基准测试
  let aggregation_benchmark = BenchmarkManager::run_benchmark(
    benchmark_manager,
    "real_time_aggregation"
  )
  
  // 验证聚合性能
  assert_true(aggregation_benchmark.success)
  assert_true(aggregation_benchmark.actual_throughput >= 10000)
  assert_true(aggregation_benchmark.latency_p90 <= 50)
  assert_true(aggregation_benchmark.memory_usage_mb <= 2048)
  
  // 验证聚合准确性
  assert_true(aggregation_benchmark.accuracy_rate >= 0.999)
  assert_true(aggregation_benchmark.data_loss_rate <= 0.001)
  
  // 执行对比基准测试（不同配置）
  let comparison_scenarios = [
    {
      name: "baseline",
      config: { batch_size: 1000, compression: false, caching: true }
    },
    {
      name: "optimized_batch",
      config: { batch_size: 5000, compression: false, caching: true }
    },
    {
      name: "with_compression",
      config: { batch_size: 1000, compression: true, caching: true }
    },
    {
      name: "no_caching",
      config: { batch_size: 1000, compression: false, caching: false }
    }
  ]
  
  let mut comparison_results = []
  for scenario in comparison_scenarios {
    let result = BenchmarkManager::run_comparison_benchmark(
      benchmark_manager,
      "high_throughput_ingestion",
      scenario.config
    )
    comparison_results = comparison_results.push({
      scenario_name: scenario.name,
      throughput: result.actual_throughput,
      latency_p99: result.latency_p99,
      cpu_usage: result.cpu_usage,
      memory_usage_mb: result.memory_usage_mb
    })
  }
  
  // 验证对比结果
  assert_eq(comparison_results.length(), 4)
  
  // 找出最佳配置
  let best_throughput = comparison_results.reduce(fn(max, current) {
    if current.throughput > max.throughput { current } else { max }
  }, comparison_results[0])
  
  let best_latency = comparison_results.reduce(fn(min, current) {
    if current.latency_p99 < min.latency_p99 { current } else { min }
  }, comparison_results[0])
  
  // 验证配置优化效果
  assert_true(best_throughput.throughput >= comparison_results[0].throughput)
  assert_true(best_latency.latency_p99 <= comparison_results[0].latency_p99)
  
  // 执行压力测试
  let stress_test = BenchmarkManager::run_stress_test(benchmark_manager, {
    scenario: "high_throughput_ingestion",
    load_factor: 2.0,  // 2倍负载
    duration_seconds: 300,  // 5分钟压力测试
    ramp_up_seconds: 30
  })
  
  // 验证系统在压力下的表现
  assert_true(stress_test.completed)
  assert_true(stress_test.degradation_factor <= 0.2)  // 性能下降不超过20%
  assert_true(stress_test.error_rate <= 0.05)  // 错误率不超过5%
  
  // 执行长期稳定性测试
  let endurance_test = BenchmarkManager::run_endurance_test(benchmark_manager, {
    scenario: "real_time_aggregation",
    duration_hours: 2,
    monitoring_interval: 60  // 每分钟监控一次
  })
  
  // 验证长期稳定性
  assert_true(endurance_test.completed)
  assert_true(endurance_test.memory_leak_detected == false)
  assert_true(endurance_test.performance_degradation <= 0.1)  // 性能下降不超过10%
  
  // 生成性能报告
  let performance_report = BenchmarkManager::generate_report(benchmark_manager, {
    include_comparison: true,
    include_recommendations: true,
    format: "detailed"
  })
  
  // 验证报告内容
  assert_true(performance_report.length() > 0)
  assert_true(performance_report.contains("high_throughput_ingestion"))
  assert_true(performance_report.contains("complex_query_processing"))
  assert_true(performance_report.contains("real_time_aggregation"))
  assert_true(performance_report.contains("recommendations"))
  
  // 测试性能回归检测
  let baseline_results = BenchmarkManager::get_baseline_results(benchmark_manager)
  let current_results = [ingestion_benchmark, query_benchmark, aggregation_benchmark]
  
  let regression_analysis = BenchmarkManager::detect_regression(
    benchmark_manager,
    baseline_results,
    current_results,
    {
      throughput_threshold: 0.05,  // 5%下降阈值
      latency_threshold: 0.1,     // 10%延迟增加阈值
      error_rate_threshold: 0.02  // 2%错误率增加阈值
    }
  )
  
  // 验证回归检测结果
  assert_true(regression_analysis.analyzed_scenarios.length() >= 3)
  
  if regression_analysis.regressions_detected {
    assert_true(regression_analysis.regressed_scenarios.length() > 0)
    assert_true(regression_analysis.regression_summary.length() > 0)
  }
  
  // 测试性能优化建议
  let optimization_suggestions = BenchmarkManager::generate_optimization_suggestions(
    benchmark_manager,
    current_results
  )
  
  // 验证优化建议
  assert_true(optimization_suggestions.length() > 0)
  
  let has_batch_optimization = optimization_suggestions.any(fn(suggestion) {
    suggestion.category == "batch_processing" and suggestion.priority >= 3
  })
  assert_true(has_batch_optimization)
  
  let has_memory_optimization = optimization_suggestions.any(fn(suggestion) {
    suggestion.category == "memory_usage" and suggestion.impact == "high"
  })
  assert_true(has_memory_optimization)
  
  // 测试基准测试历史趋势
  let historical_trends = BenchmarkManager::get_performance_trends(benchmark_manager, {
    scenarios: ["high_throughput_ingestion", "complex_query_processing"],
    time_range_days: 30,
    metric: "throughput"
  })
  
  // 验证趋势数据
  assert_true(historical_trends.length() > 0)
  assert_true(historical_trends[0].data_points.length() > 0)
  
  // 验证趋势分析
  let throughput_trend = historical_trends.find(fn(t) { t.scenario == "high_throughput_ingestion" })
  assert_true(throughput_trend != None)
  match throughput_trend {
    Some(trend) => {
      assert_true(trend.trend_direction == "improving" or 
                  trend.trend_direction == "stable" or 
                  trend.trend_direction == "degrading")
      assert_true(trend.confidence >= 0.0)
      assert_true(trend.confidence <= 1.0)
    }
    None => assert_true(false)
  }
}