// Azimuth 错误处理与恢复质量测试
// 专注于系统错误处理机制、故障恢复策略和容错能力

// 测试1: 遥测数据生命周期管理
test "遥测数据生命周期管理测试" {
  // 创建生命周期管理器
  let lifecycle_manager = TelemetryLifecycleManager::new()
  
  // 配置数据生命周期策略
  LifecycleManager::set_retention_policy(lifecycle_manager, {
    hot_data_ttl: 3600,      // 热数据保留1小时
    warm_data_ttl: 86400,    // 温数据保留1天
    cold_data_ttl: 604800,   // 冷数据保留7天
    archive_ttl: 2592000     // 归档数据保留30天
  })
  
  // 配置数据迁移策略
  LifecycleManager::set_migration_strategy(lifecycle_manager, {
    hot_to_warm_threshold: 1000,    // 1000MB
    warm_to_cold_threshold: 5000,   // 5000MB
    cold_to_archive_threshold: 10000 // 10000MB
  })
  
  // 创建测试数据
  let test_data = []
  let base_time = 1640995200
  for i in 0..=100 {
    test_data = test_data.push({
      id: "data-" + i.to_string(),
      timestamp: base_time + i * 60,
      size: 1024 + i * 100,  // 逐渐增大的数据
      access_count: 100 - i,  // 访问频率递减
      tier: "hot"
    })
  }
  
  // 添加数据到生命周期管理器
  for data in test_data {
    LifecycleManager::add_data(lifecycle_manager, data)
  }
  
  // 验证初始状态
  let hot_data = LifecycleManager::get_data_by_tier(lifecycle_manager, "hot")
  assert_eq(hot_data.length(), 101)
  
  // 模拟时间流逝和数据访问
  let mut time_elapsed = 0
  while time_elapsed < 7200 {  // 2小时
    // 模拟数据访问
    for i in 0..=50 {
      let data_id = "data-" + i.to_string()
      LifecycleManager::record_access(lifecycle_manager, data_id)
    }
    
    // 执行生命周期检查
    LifecycleManager::process_lifecycle(lifecycle_manager, base_time + time_elapsed)
    time_elapsed = time_elapsed + 300  // 5分钟间隔
  }
  
  // 验证数据分层
  let hot_data_after = LifecycleManager::get_data_by_tier(lifecycle_manager, "hot")
  let warm_data = LifecycleManager::get_data_by_tier(lifecycle_manager, "warm")
  
  assert_true(hot_data_after.length() < hot_data.length())  // 部分数据已迁移
  assert_true(warm_data.length() > 0)  // 有数据迁移到温存储
  
  // 验证数据访问频率影响分层
  let frequently_accessed = LifecycleManager::get_frequently_accessed(lifecycle_manager, 10)
  assert_true(frequently_accessed.length() > 0)
  
  for data in frequently_accessed {
    assert_eq(data.tier, "hot")  // 频繁访问的数据应保留在热存储
  }
}

// 测试2: 分布式追踪上下文传播
test "分布式追踪上下文传播测试" {
  // 创建分布式追踪管理器
  let tracer = DistributedTracer::new()
  
  // 配置传播器
  let trace_context_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new()
  CompositePropagator::add_propagator(composite_propagator, trace_context_propagator)
  CompositePropagator::add_propagator(composite_propagator, baggage_propagator)
  
  DistributedTracer::set_propagator(tracer, composite_propagator)
  
  // 创建根span
  let root_span = DistributedTracer::start_span(tracer, "user.request", {
    kind: SpanKind::SERVER,
    attributes: [
      ("http.method", StringValue("GET")),
      ("http.url", StringValue("/api/user/profile")),
      ("user.id", StringValue("user-12345"))
    ]
  })
  
  // 设置baggage项
  Span::set_baggage_item(root_span, "user.role", "premium")
  Span::set_baggage_item(root_span, "request.id", "req-67890")
  Span::set_baggage_item(root_span, "session.id", "sess-11111")
  
  // 创建子span - 服务A
  let service_a_context = DistributedTracer::extract_context_from_span(tracer, root_span)
  let service_a_span = DistributedTracer::start_span_with_context(tracer, "service.a.process", service_a_context, {
    kind: SpanKind::INTERNAL,
    attributes: [
      ("service.name", StringValue("service.a")),
      ("operation.type", StringValue("data_processing"))
    ]
  })
  
  // 验证上下文传播
  assert_eq(Span::get_trace_id(service_a_span), Span::get_trace_id(root_span))
  assert_true(Span::is_parent_of(root_span, service_a_span))
  
  // 验证baggage传播
  let user_role = Span::get_baggage_item(service_a_span, "user.role")
  let request_id = Span::get_baggage_item(service_a_span, "request.id")
  let session_id = Span::get_baggage_item(service_a_span, "session.id")
  
  assert_eq(user_role, Some("premium"))
  assert_eq(request_id, Some("req-67890"))
  assert_eq(session_id, Some("sess-11111"))
  
  // 创建子span - 服务B
  let service_b_context = DistributedTracer::extract_context_from_span(tracer, service_a_span)
  let service_b_span = DistributedTracer::start_span_with_context(tracer, "service.b.query", service_b_context, {
    kind: SpanKind::CLIENT,
    attributes: [
      ("service.name", StringValue("service.b")),
      ("db.type", StringValue("postgresql")),
      ("db.statement", StringValue("SELECT * FROM users"))
    ]
  })
  
  // 在服务B中添加新的baggage项
  Span::set_baggage_item(service_b_span, "db.connection.id", "conn-22222")
  
  // 验证baggage继承和扩展
  let inherited_user_role = Span::get_baggage_item(service_b_span, "user.role")
  let new_db_connection = Span::get_baggage_item(service_b_span, "db.connection.id")
  
  assert_eq(inherited_user_role, Some("premium"))  // 继承的baggage项
  assert_eq(new_db_connection, Some("conn-22222"))  // 新增的baggage项
  
  // 创建子span - 服务C
  let service_c_context = DistributedTracer::extract_context_from_span(tracer, service_b_span)
  let service_c_span = DistributedTracer::start_span_with_context(tracer, "service.c.transform", service_c_context, {
    kind: SpanKind::INTERNAL,
    attributes: [
      ("service.name", StringValue("service.c")),
      ("operation.type", StringValue("data_transform"))
    ]
  })
  
  // 测试跨进程传播
  let carrier = TextMapCarrier::new()
  DistributedTracer::inject_context(tracer, service_c_context, carrier)
  
  // 模拟跨进程传输
  let extracted_context = DistributedTracer::extract_context(tracer, carrier)
  let service_d_span = DistributedTracer::start_span_with_context(tracer, "service.d.finalize", extracted_context, {
    kind: SpanKind::SERVER,
    attributes: [
      ("service.name", StringValue("service.d")),
      ("operation.type", StringValue("final_processing"))
    ]
  })
  
  // 验证跨进程传播的完整性
  assert_eq(Span::get_trace_id(service_d_span), Span::get_trace_id(root_span))
  assert_eq(Span::get_baggage_item(service_d_span, "user.role"), Some("premium"))
  assert_eq(Span::get_baggage_item(service_d_span, "db.connection.id"), Some("conn-22222"))
  
  // 结束所有span
  Span::end(service_c_span)
  Span::end(service_b_span)
  Span::end(service_a_span)
  Span::end(root_span)
  Span::end(service_d_span)
  
  // 构建追踪树并验证结构
  let trace_tree = DistributedTracer::build_trace_tree(tracer, Span::get_trace_id(root_span))
  assert_eq(trace_tree.spans.length(), 5)
  
  // 验证span关系
  let root_span_node = trace_tree.spans.find(fn(s) { s.span_id == Span::get_span_id(root_span) })
  match root_span_node {
    Some(span) => assert_eq(span.children.length(), 1),
    None => assert_true(false)
  }
}

// 测试3: 自适应采样策略
test "自适应采样策略测试" {
  // 创建自适应采样管理器
  let adaptive_sampler = AdaptiveSampler::new()
  
  // 配置基础采样策略
  AdaptiveSampler::set_base_sampling_rate(adaptive_sampler, 0.1)  // 10%基础采样率
  
  // 配置动态调整参数
  AdaptiveSampler::set_adjustment_parameters(adaptive_sampler, {
    max_sampling_rate: 1.0,      // 最大100%采样率
    min_sampling_rate: 0.01,     // 最小1%采样率
    adjustment_factor: 1.5,      // 调整因子
    evaluation_window: 300,      // 5分钟评估窗口
    throughput_threshold: 1000   // 1000请求/秒阈值
  })
  
  // 配置基于属性的采样规则
  AdaptiveSampler::add_attribute_rule(adaptive_sampler, {
    name: "error_sampling",
    condition: "http.status_code >= 500",
    sampling_rate: 1.0  // 错误请求100%采样
  })
  
  AdaptiveSampler::add_attribute_rule(adaptive_sampler, {
    name: "critical_service_sampling",
    condition: "service.importance == 'critical'",
    sampling_rate: 0.8  // 关键服务80%采样
  })
  
  AdaptiveSampler::add_attribute_rule(adaptive_sampler, {
    name: "high_latency_sampling",
    condition: "http.request.duration > 1000",
    sampling_rate: 0.5  // 高延迟请求50%采样
  })
  
  // 模拟正常流量
  let mut sampled_normal = 0
  let mut total_normal = 0
  
  for i in 0..=1000 {
    let span = create_test_span("normal.request", [
      ("http.method", StringValue("GET")),
      ("http.status_code", IntValue(200)),
      ("http.request.duration", IntValue(150 + i % 200))
    ])
    
    total_normal = total_normal + 1
    if AdaptiveSampler::should_sample(adaptive_sampler, span) {
      sampled_normal = sampled_normal + 1
    }
  }
  
  // 验证正常流量的采样率接近基础采样率
  let normal_sampling_rate = sampled_normal.to_float() / total_normal.to_float()
  assert_true(normal_sampling_rate >= 0.05 and normal_sampling_rate <= 0.15)
  
  // 模拟错误流量
  let mut sampled_errors = 0
  let mut total_errors = 0
  
  for i in 0..=100 {
    let span = create_test_span("error.request", [
      ("http.method", StringValue("POST")),
      ("http.status_code", IntValue(500)),
      ("http.request.duration", IntValue(2000 + i % 500))
    ])
    
    total_errors = total_errors + 1
    if AdaptiveSampler::should_sample(adaptive_sampler, span) {
      sampled_errors = sampled_errors + 1
    }
  }
  
  // 验证错误流量的采样率接近100%
  let error_sampling_rate = sampled_errors.to_float() / total_errors.to_float()
  assert_true(error_sampling_rate >= 0.95)
  
  // 模拟高流量场景
  let mut sampled_high_traffic = 0
  let mut total_high_traffic = 0
  
  for i in 0..=2000 {
    let span = create_test_span("high.traffic.request", [
      ("http.method", StringValue("GET")),
      ("http.status_code", IntValue(200)),
      ("http.request.duration", IntValue(100 + i % 100))
    ])
    
    total_high_traffic = total_high_traffic + 1
    if AdaptiveSampler::should_sample(adaptive_sampler, span) {
      sampled_high_traffic = sampled_high_traffic + 1
    }
  }
  
  // 验证高流量下的自适应调整
  let high_traffic_sampling_rate = sampled_high_traffic.to_float() / total_high_traffic.to_float()
  
  // 在高流量下，采样率应该降低
  assert_true(high_traffic_sampling_rate < normal_sampling_rate)
  assert_true(high_traffic_sampling_rate >= 0.01)  // 但不低于最小采样率
  
  // 模拟关键服务流量
  let mut sampled_critical = 0
  let mut total_critical = 0
  
  for i in 0..=200 {
    let span = create_test_span("critical.service.request", [
      ("service.importance", StringValue("critical")),
      ("http.method", StringValue("GET")),
      ("http.status_code", IntValue(200))
    ])
    
    total_critical = total_critical + 1
    if AdaptiveSampler::should_sample(adaptive_sampler, span) {
      sampled_critical = sampled_critical + 1
    }
  }
  
  // 验证关键服务的高采样率
  let critical_sampling_rate = sampled_critical.to_float() / total_critical.to_float()
  assert_true(critical_sampling_rate >= 0.7)
  
  // 测试采样策略的动态调整
  let initial_rate = AdaptiveSampler::get_current_sampling_rate(adaptive_sampler)
  
  // 模拟系统负载增加
  AdaptiveSampler::record_system_metrics(adaptive_sampler, {
    throughput: 1500,  // 超过阈值
    cpu_usage: 80.0,
    memory_usage: 70.0
  })
  
  // 触发采样率调整
  AdaptiveSampler::adjust_sampling_rate(adaptive_sampler)
  
  let adjusted_rate = AdaptiveSampler::get_current_sampling_rate(adaptive_sampler)
  
  // 验证采样率已调整
  assert_true(adjusted_rate < initial_rate)
  assert_true(adjusted_rate >= 0.01)  // 不低于最小采样率
}

// 测试4: 遥测数据质量保证
test "遥测数据质量保证测试" {
  // 创建数据质量管理器
  let quality_manager = DataQualityManager::new()
  
  // 配置数据质量规则
  QualityManager::add_rule(quality_manager, {
    name: "timestamp_validation",
    type: "validation",
    description: "验证时间戳的合理性和一致性",
    condition: "timestamp >= now - 3600 AND timestamp <= now + 60",
    severity: "error"
  })
  
  QualityManager::add_rule(quality_manager, {
    name: "required_fields",
    type: "validation",
    description: "验证必需字段的存在",
    condition: "trace_id != NULL AND span_id != NULL AND service_name != NULL",
    severity: "error"
  })
  
  QualityManager::add_rule(quality_manager, {
    name: "duration_range",
    type: "validation",
    description: "验证持续时间的合理性",
    condition: "duration >= 0 AND duration <= 3600000",
    severity: "warning"
  })
  
  QualityManager::add_rule(quality_manager, {
    name: "attribute_format",
    type: "validation",
    description: "验证属性格式",
    condition: "http.status_code >= 100 AND http.status_code <= 599",
    severity: "warning"
  })
  
  // 配置数据清洗规则
  QualityManager::add_cleaning_rule(quality_manager, {
    name: "remove_sensitive_data",
    type: "transformation",
    description: "移除敏感数据",
    condition: "attribute.key LIKE '%password%' OR attribute.key LIKE '%token%'",
    action: "remove_attribute"
  })
  
  QualityManager::add_cleaning_rule(quality_manager, {
    name: "normalize_service_names",
    type: "transformation",
    description: "标准化服务名称",
    condition: "service.name MATCHES '.*[A-Z].*'",
    action: "to_lowercase"
  })
  
  // 创建测试数据集
  let test_data = [
    // 有效数据
    {
      trace_id: "trace-001",
      span_id: "span-001",
      service_name: "service.a",
      timestamp: 1640995200,
      duration: 1500,
      attributes: [
        ("http.method", StringValue("GET")),
        ("http.status_code", IntValue(200)),
        ("http.url", StringValue("/api/users"))
      ]
    },
    // 时间戳异常数据
    {
      trace_id: "trace-002",
      span_id: "span-002",
      service_name: "service.b",
      timestamp: 1640995200 + 7200,  // 2小时后
      duration: 2000,
      attributes: [
        ("http.method", StringValue("POST")),
        ("http.status_code", IntValue(201))
      ]
    },
    // 缺少必需字段
    {
      trace_id: "",  // 空trace_id
      span_id: "span-003",
      service_name: "Service.C",  // 大写服务名
      timestamp: 1640995200,
      duration: -500,  // 负持续时间
      attributes: [
        ("http.method", StringValue("PUT")),
        ("http.status_code", IntValue(600)),  // 无效状态码
        ("user.password", StringValue("secret123"))  // 敏感数据
      ]
    },
    // 正常数据
    {
      trace_id: "trace-004",
      span_id: "span-004",
      service_name: "service.d",
      timestamp: 1640995200,
      duration: 800,
      attributes: [
        ("http.method", StringValue("DELETE")),
        ("http.status_code", IntValue(204)),
        ("api.token", StringValue("abc123xyz"))  // 敏感数据
      ]
    }
  ]
  
  // 执行数据质量检查
  let quality_results = []
  for data in test_data {
    let result = QualityManager::check_data_quality(quality_manager, data)
    quality_results = quality_results.push(result)
  }
  
  // 验证质量检查结果
  assert_eq(quality_results.length(), 4)
  
  // 第一条数据应该通过所有检查
  assert_true(quality_results[0].is_valid)
  assert_eq(quality_results[0].errors.length(), 0)
  assert_eq(quality_results[0].warnings.length(), 0)
  
  // 第二条数据应该有时间戳错误
  assert_false(quality_results[1].is_valid)
  assert_true(quality_results[1].errors.length() > 0)
  assert_true(quality_results[1].errors.any(fn(e) { e.rule == "timestamp_validation" }))
  
  // 第三条数据应该有多个错误和警告
  assert_false(quality_results[2].is_valid)
  assert_true(quality_results[2].errors.length() >= 2)  // trace_id和duration
  assert_true(quality_results[2].warnings.length() >= 1)  // status_code
  
  // 第四条数据应该通过检查但包含敏感数据
  assert_true(quality_results[3].is_valid)
  assert_eq(quality_results[3].errors.length(), 0)
  
  // 执行数据清洗
  let cleaned_data = []
  for i in 0..=test_data.length() - 1 {
    let cleaned = QualityManager::clean_data(quality_manager, test_data[i])
    cleaned_data = cleaned_data.push(cleaned)
  }
  
  // 验证数据清洗结果
  // 验证敏感数据被移除
  assert_false(cleaned_data[2].attributes.any(fn(a) { a.0 == "user.password" }))
  assert_false(cleaned_data[3].attributes.any(fn(a) { a.0 == "api.token" }))
  
  // 验证服务名称标准化
  let service_c_name = cleaned_data[2].service_name
  assert_eq(service_c_name, "service.c")  // 应该转换为小写
  
  // 测试数据质量报告
  let quality_report = QualityManager::generate_quality_report(quality_manager, test_data)
  
  // 验证报告内容
  assert_true(quality_report.total_records == 4)
  assert_true(quality_report.valid_records == 2)
  assert_true(quality_report.invalid_records == 2)
  assert_true(quality_report.quality_score >= 0.0 and quality_report.quality_score <= 1.0)
  
  // 验证错误统计
  let timestamp_errors = quality_report.error_counts.get("timestamp_validation")
  assert_eq(timestamp_errors, Some(1))
  
  let required_field_errors = quality_report.error_counts.get("required_fields")
  assert_eq(required_field_errors, Some(1))
  
  // 测试质量改进建议
  let improvement_suggestions = QualityManager::get_improvement_suggestions(quality_manager, quality_report)
  assert_true(improvement_suggestions.length() > 0)
  
  let has_timestamp_suggestion = improvement_suggestions.any(fn(s) {
    s.contains("timestamp") and s.contains("synchronization")
  })
  assert_true(has_timestamp_suggestion)
}

// 测试5: 多租户隔离测试
test "多租户隔离测试" {
  // 创建多租户管理器
  let tenant_manager = MultiTenantManager::new()
  
  // 配置租户
  let tenant_a = Tenant::new("tenant-a", "Tenant A", {
    max_spans_per_second: 1000,
    max_data_retention_days: 7,
    max_storage_mb: 1024,
    allowed_services: ["service.a", "service.b"],
    sampling_rate: 0.1
  })
  
  let tenant_b = Tenant::new("tenant-b", "Tenant B", {
    max_spans_per_second: 2000,
    max_data_retention_days: 14,
    max_storage_mb: 2048,
    allowed_services: ["service.c", "service.d"],
    sampling_rate: 0.2
  })
  
  let tenant_c = Tenant::new("tenant-c", "Tenant C", {
    max_spans_per_second: 500,
    max_data_retention_days: 3,
    max_storage_mb: 512,
    allowed_services: ["service.e"],
    sampling_rate: 0.05
  })
  
  MultiTenantManager::add_tenant(tenant_manager, tenant_a)
  MultiTenantManager::add_tenant(tenant_manager, tenant_b)
  MultiTenantManager::add_tenant(tenant_manager, tenant_c)
  
  // 创建租户A的数据
  let tenant_a_data = []
  for i in 0..=100 {
    tenant_a_data = tenant_a_data.push({
      tenant_id: "tenant-a",
      trace_id: "trace-a-" + i.to_string(),
      span_id: "span-a-" + i.to_string(),
      service_name: "service.a",
      timestamp: 1640995200 + i * 10,
      size: 1024
    })
  }
  
  // 创建租户B的数据
  let tenant_b_data = []
  for i in 0..=200 {
    tenant_b_data = tenant_b_data.push({
      tenant_id: "tenant-b",
      trace_id: "trace-b-" + i.to_string(),
      span_id: "span-b-" + i.to_string(),
      service_name: "service.c",
      timestamp: 1640995200 + i * 10,
      size: 2048
    })
  }
  
  // 添加数据到多租户管理器
  for data in tenant_a_data {
    MultiTenantManager::add_telemetry_data(tenant_manager, data)
  }
  
  for data in tenant_b_data {
    MultiTenantManager::add_telemetry_data(tenant_manager, data)
  }
  
  // 测试租户隔离查询
  let tenant_a_query_result = MultiTenantManager::query_telemetry_data(tenant_manager, "tenant-a", {
    start_time: 1640995200,
    end_time: 1640995300,
    service_filter: None
  })
  
  let tenant_b_query_result = MultiTenantManager::query_telemetry_data(tenant_manager, "tenant-b", {
    start_time: 1640995200,
    end_time: 1640995300,
    service_filter: None
  })
  
  // 验证查询结果隔离
  assert_eq(tenant_a_query_result.length(), 101)
  assert_eq(tenant_b_query_result.length(), 201)
  
  // 验证数据不混合
  assert_true(tenant_a_query_result.all(fn(d) { d.tenant_id == "tenant-a" }))
  assert_true(tenant_b_query_result.all(fn(d) { d.tenant_id == "tenant-b" }))
  
  // 测试租户资源限制
  let tenant_a_resource_usage = MultiTenantManager::get_resource_usage(tenant_manager, "tenant-a")
  let tenant_b_resource_usage = MultiTenantManager::get_resource_usage(tenant_manager, "tenant-b")
  
  // 验证资源使用计算
  assert_eq(tenant_a_resource_usage.span_count, 101)
  assert_eq(tenant_a_resource_usage.storage_used_mb, 101 * 1024 / 1024)  // 约等于101MB
  
  assert_eq(tenant_b_resource_usage.span_count, 201)
  assert_eq(tenant_b_resource_usage.storage_used_mb, 201 * 2048 / 1024)  // 约等于401MB
  
  // 测试资源限制执行
  // 尝试为租户C添加超过限制的数据
  let tenant_c_excess_data = []
  for i in 0..=600 {  // 超过500的限制
    tenant_c_excess_data = tenant_c_excess_data.push({
      tenant_id: "tenant-c",
      trace_id: "trace-c-" + i.to_string(),
      span_id: "span-c-" + i.to_string(),
      service_name: "service.e",
      timestamp: 1640995200 + i * 10,
      size: 1024
    })
  }
  
  let mut accepted_count = 0
  let mut rejected_count = 0
  
  for data in tenant_c_excess_data {
    if MultiTenantManager::add_telemetry_data(tenant_manager, data) {
      accepted_count = accepted_count + 1
    } else {
      rejected_count = rejected_count + 1
    }
  }
  
  // 验证资源限制生效
  assert_true(accepted_count <= 500)  // 不应超过限制
  assert_true(rejected_count > 0)     // 应该有数据被拒绝
  
  // 测试租户配置更新
  let updated_tenant_a = Tenant::new("tenant-a", "Tenant A Updated", {
    max_spans_per_second: 1500,  // 增加限制
    max_data_retention_days: 10,  // 延长保留期
    max_storage_mb: 1536,        // 增加存储
    allowed_services: ["service.a", "service.b", "service.f"],  // 添加新服务
    sampling_rate: 0.15          // 提高采样率
  })
  
  MultiTenantManager::update_tenant(tenant_manager, updated_tenant_a)
  
  // 验证配置更新
  let current_tenant_a = MultiTenantManager::get_tenant(tenant_manager, "tenant-a")
  assert_eq(current_tenant_a.config.max_spans_per_second, 1500)
  assert_eq(current_tenant_a.config.max_data_retention_days, 10)
  assert_eq(current_tenant_a.config.allowed_services.length(), 3)
  
  // 测试租户数据隔离删除
  let delete_result = MultiTenantManager::delete_tenant_data(tenant_manager, "tenant-a", {
    older_than_days: 0  // 删除所有数据
  })
  
  assert_true(delete_result.success)
  assert_eq(delete_result.deleted_count, 101)
  
  // 验证删除后查询结果
  let tenant_a_query_after_delete = MultiTenantManager::query_telemetry_data(tenant_manager, "tenant-a", {
    start_time: 1640995200,
    end_time: 1640995300,
    service_filter: None
  })
  
  assert_eq(tenant_a_query_after_delete.length(), 0)
  
  // 验证其他租户数据不受影响
  let tenant_b_query_after_delete = MultiTenantManager::query_telemetry_data(tenant_manager, "tenant-b", {
    start_time: 1640995200,
    end_time: 1640995300,
    service_filter: None
  })
  
  assert_eq(tenant_b_query_after_delete.length(), 201)  // 应该保持不变
}

// 辅助函数：创建测试span
let create_test_span = fn(name: String, attributes: Array[(String, AttributeValue)]) {
  Span::new(name, SpanKind::INTERNAL, SpanContext::new("test-trace", "test-span", true, ""))
}