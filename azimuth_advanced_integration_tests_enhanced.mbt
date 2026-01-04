// Azimuth 高级遥测集成测试用例
// 专注于遥测系统的高级集成场景和复杂用例

// 测试1: 分布式追踪链路传递
test "分布式追踪链路传递测试" {
  // 创建根追踪器
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "distributed.chain")
  
  // 模拟微服务调用链
  let api_gateway_span = Tracer::start_span(tracer, "api.gateway.request")
  Span::set_attribute(api_gateway_span, "http.method", StringValue("POST"))
  Span::set_attribute(api_gateway_span, "http.route", StringValue("/api/v1/orders"))
  
  // 传递上下文到订单服务
  let api_context = Span::context(api_gateway_span)
  let order_service_span = Tracer::start_span_with_context(tracer, "order.service.process", api_context)
  Span::set_attribute(order_service_span, "service.name", StringValue("order-service"))
  Span::set_attribute(order_service_span, "order.id", StringValue("order-12345"))
  
  // 传递上下文到库存服务
  let order_context = Span::context(order_service_span)
  let inventory_service_span = Tracer::start_span_with_context(tracer, "inventory.service.check", order_context)
  Span::set_attribute(inventory_service_span, "service.name", StringValue("inventory-service"))
  Span::set_attribute(inventory_service_span, "product.id", StringValue("prod-67890"))
  Span::set_attribute(inventory_service_span, "quantity.checked", IntValue(10))
  
  // 设置链路关系
  Span::add_link(inventory_service_span, Link::new(Span::context(order_service_span)))
  
  // 完成所有span
  Span::set_status(inventory_service_span, Ok)
  Span::end(inventory_service_span)
  
  Span::set_status(order_service_span, Ok)
  Span::end(order_service_span)
  
  Span::set_status(api_gateway_span, Ok)
  Span::end(api_gateway_span)
  
  // 验证追踪链路完整性
  assert_eq(Span::trace_id(api_gateway_span), Span::trace_id(order_service_span))
  assert_eq(Span::trace_id(order_service_span), Span::trace_id(inventory_service_span))
  assert_ne(Span::span_id(api_gateway_span), Span::span_id(order_service_span))
  assert_ne(Span::span_id(order_service_span), Span::span_id(inventory_service_span))
}

// 测试2: 自适应采样策略
test "自适应采样策略测试" {
  // 创建自适应采样器
  let adaptive_sampler = AdaptiveSampler::new(
    base_rate = 0.1,
    max_rate = 1.0,
    error_threshold = 0.05,
    latency_threshold = 100.0
  )
  
  // 模拟不同负载场景
  let low_load_spans = []
  for i in 0..=100 {
    let span_name = "low.load.operation." + i.to_string()
    let sampled = Sampler::should_sample(adaptive_sampler, span_name)
    low_load_spans = low_load_spans.push((span_name, sampled))
  }
  
  // 模拟高错误率场景
  let error_sampler = AdaptiveSampler::with_error_rate(adaptive_sampler, 0.08)
  let high_error_spans = []
  for i in 0..=100 {
    let span_name = "high.error.operation." + i.to_string()
    let sampled = Sampler::should_sample(error_sampler, span_name)
    high_error_spans = high_error_spans.push((span_name, sampled))
  }
  
  // 模拟高延迟场景
  let latency_sampler = AdaptiveSampler::with_latency(adaptive_sampler, 150.0)
  let high_latency_spans = []
  for i in 0..=100 {
    let span_name = "high.latency.operation." + i.to_string()
    let sampled = Sampler::should_sample(latency_sampler, span_name)
    high_latency_spans = high_latency_spans.push((span_name, sampled))
  }
  
  // 验证采样率调整
  let low_load_sampled = low_load_spans.filter(fn(t) { t.1 }).length()
  let high_error_sampled = high_error_spans.filter(fn(t) { t.1 }).length()
  let high_latency_sampled = high_latency_spans.filter(fn(t) { t.1 }).length()
  
  assert_true(low_load_sampled < high_error_sampled)
  assert_true(high_error_sampled < high_latency_sampled)
  assert_true(high_latency_sampled > 80)  // 高延迟场景应该有更高的采样率
}

// 测试3: 实时指标聚合
test "实时指标聚合测试" {
  // 创建指标聚合器
  let aggregator = MetricAggregator::new(window_size = 60000)  // 1分钟窗口
  
  // 模拟实时指标流
  let metrics_stream = [
    ("api.request.count", 100.0, 1640995200),
    ("api.request.count", 150.0, 1640995210),
    ("api.request.count", 120.0, 1640995220),
    ("api.request.duration", 0.125, 1640995200),
    ("api.request.duration", 0.180, 1640995210),
    ("api.request.duration", 0.095, 1640995220),
    ("api.error.rate", 0.02, 1640995200),
    ("api.error.rate", 0.03, 1640995210),
    ("api.error.rate", 0.01, 1640995220)
  ]
  
  // 聚合指标
  for (name, value, timestamp) in metrics_stream {
    Aggregator::add_point(aggregator, name, value, timestamp)
  }
  
  // 计算聚合统计
  let request_count_stats = Aggregator::get_stats(aggregator, "api.request.count")
  let request_duration_stats = Aggregator::get_stats(aggregator, "api.request.duration")
  let error_rate_stats = Aggregator::get_stats(aggregator, "api.error.rate")
  
  // 验证聚合结果
  assert_eq(request_count_stats.count, 3)
  assert_eq(request_count_stats.sum, 370.0)
  assert_eq(request_count_stats.avg, 123.33333333333333)
  assert_eq(request_count_stats.min, 100.0)
  assert_eq(request_count_stats.max, 150.0)
  
  assert_eq(request_duration_stats.count, 3)
  assert_eq(request_duration_stats.sum, 0.4)
  assert_eq(request_duration_stats.avg, 0.13333333333333333)
  assert_eq(request_duration_stats.min, 0.095)
  assert_eq(request_duration_stats.max, 0.18)
  
  assert_eq(error_rate_stats.count, 3)
  assert_eq(error_rate_stats.sum, 0.06)
  assert_eq(error_rate_stats.avg, 0.02)
  assert_eq(error_rate_stats.min, 0.01)
  assert_eq(error_rate_stats.max, 0.03)
}

// 测试4: 异常检测和告警
test "异常检测和告警测试" {
  // 创建异常检测器
  let anomaly_detector = AnomalyDetector::new(
    algorithm = "statistical",
    sensitivity = 0.95,
    window_size = 100
  )
  
  // 模拟正常指标数据
  let normal_metrics = []
  for i in 0..=100 {
    let value = 50.0 + (Random::float() * 10.0)  // 50-60范围内的正常值
    normal_metrics = normal_metrics.push(("response.time", value, 1640995200 + i))
  }
  
  // 训练异常检测模型
  for (name, value, timestamp) in normal_metrics {
    Detector::train(anomaly_detector, name, value, timestamp)
  }
  
  // 模拟异常指标数据
  let anomalous_metrics = [
    ("response.time", 125.0, 1640995300),  // 异常高值
    ("response.time", 5.0, 1640995310),    // 异常低值
    ("response.time", 85.0, 1640995320),   // 边界值
    ("response.time", 52.0, 1640995330)    // 正常值
  ]
  
  // 检测异常
  let anomalies = []
  for (name, value, timestamp) in anomalous_metrics {
    let is_anomaly = Detector::detect(anomaly_detector, name, value, timestamp)
    anomalies = anomalies.push((name, value, timestamp, is_anomaly))
  }
  
  // 验证异常检测结果
  assert_true(anomalies[0].3)  // 125.0 应该是异常
  assert_true(anomalies[1].3)  // 5.0 应该是异常
  assert_false(anomalies[2].3) // 85.0 可能不是异常（取决于敏感度）
  assert_false(anomalies[3].3) // 52.0 应该是正常
  
  // 创建告警规则
  let alert_rules = [
    AlertRule::new("high.response.time", "response.time", "avg", ">", 100.0, "critical"),
    AlertRule::new("low.response.time", "response.time", "min", "<", 10.0, "warning"),
    AlertRule::new("error.rate.spike", "error.rate", "rate", ">", 0.05, "critical")
  ]
  
  // 评估告警
  let alerts = []
  for rule in alert_rules {
    let evaluation = AlertRule::evaluate(rule, anomalous_metrics)
    if evaluation.triggered {
      alerts = alerts.push((rule.name, rule.severity, evaluation.message))
    }
  }
  
  // 验证告警触发
  assert_true(alerts.length() >= 1)
  assert_true(alerts.some(fn(a) { a.0 == "high.response.time" and a.1 == "critical" }))
}

// 测试5: 智能资源管理
test "智能资源管理测试" {
  // 创建资源管理器
  let resource_manager = ResourceManager::new(
    memory_threshold = 0.8,
    cpu_threshold = 0.7,
    disk_threshold = 0.9
  )
  
  // 模拟资源使用情况
  let resource_snapshots = [
    (memory = 0.6, cpu = 0.5, disk = 0.4, timestamp = 1640995200),
    (memory = 0.75, cpu = 0.65, disk = 0.5, timestamp = 1640995210),
    (memory = 0.85, cpu = 0.8, disk = 0.6, timestamp = 1640995220),  // 内存和CPU超过阈值
    (memory = 0.95, cpu = 0.9, disk = 0.95, timestamp = 1640995230), // 全部超过阈值
    (memory = 0.7, cpu = 0.6, disk = 0.5, timestamp = 1640995240)   // 恢复正常
  ]
  
  // 处理资源快照
  let resource_actions = []
  for snapshot in resource_snapshots {
    let actions = ResourceManager::analyze(resource_manager, snapshot)
    resource_actions = resource_actions.push((snapshot.timestamp, actions))
  }
  
  // 验证资源管理动作
  assert_eq(resource_actions[0].1.length(), 0)  // 正常状态，无动作
  assert_eq(resource_actions[1].1.length(), 0)  // 警告状态，可能无动作
  assert_true(resource_actions[2].1.length() > 0)  // 内存和CPU超阈值，应有动作
  assert_true(resource_actions[3].1.length() > 0)  // 全部超阈值，应有紧急动作
  
  // 验证具体的资源管理策略
  let critical_actions = resource_actions[3].1
  assert_true(critical_actions.some(fn(a) { a.action_type == "scale_up" }))
  assert_true(critical_actions.some(fn(a) { a.action_type == "cache_cleanup" }))
  assert_true(critical_actions.some(fn(a) { a.action_type == "connection_pool_reduce" }))
  
  // 测试资源预测
  let prediction_window = 300  // 5分钟预测窗口
  let resource_prediction = ResourceManager::predict(
    resource_manager,
    resource_snapshots.slice(0, 4),  // 使用前4个快照
    prediction_window
  )
  
  // 验证预测结果
  assert_true(resource_prediction.memory_trend == "increasing")
  assert_true(resource_prediction.cpu_trend == "increasing")
  assert_true(resource_prediction.disk_trend == "increasing")
  assert_true(resource_prediction.risk_level == "high")
}

// 测试6: 跨服务上下文传播
test "跨服务上下文传播测试" {
  // 创建复合传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 创建源上下文
  let source_context = Context::root()
  let trace_context = SpanContext::new(
    trace_id = "trace-7890123456789012",
    span_id = "span-1234567890123456",
    sampled = true,
    trace_state = "vendor1=value1,vendor2=value2"
  )
  let context_with_trace = Context::with_span_context(source_context, trace_context)
  
  // 添加行李项
  let user_id_key = ContextKey::new("user.id")
  let request_id_key = ContextKey::new("request.id")
  let tenant_id_key = ContextKey::new("tenant.id")
  
  let context_with_baggage = context_with_trace
    |> Context::with_value(user_id_key, "user-12345")
    |> Context::with_value(request_id_key, "req-67890")
    |> Context::with_value(tenant_id_key, "tenant-abc")
  
  // 创建HTTP头部载体
  let http_headers = TextMapCarrier::new()
  
  // 注入上下文
  CompositePropagator::inject(composite_propagator, context_with_baggage, http_headers)
  
  // 验证注入的头部
  let trace_parent = Carrier::get(http_headers, "traceparent")
  let trace_state = Carrier::get(http_headers, "tracestate")
  let baggage = Carrier::get(http_headers, "baggage")
  
  assert_true(trace_parent.is_some())
  assert_true(trace_state.is_some())
  assert_true(baggage.is_some())
  
  // 模拟HTTP请求传输
  let http_request = HttpRequest::new(
    method = "GET",
    url = "https://api.example.com/users",
    headers = Carrier::to_map(http_headers)
  )
  
  // 在目标服务中提取上下文
  let response_headers = TextMapCarrier::from_map(http_request.headers)
  let extracted_context = CompositePropagator::extract(composite_propagator, response_headers)
  
  // 验证提取的上下文
  let extracted_trace_context = Context::span_context(extracted_context)
  let extracted_user_id = Context::get(extracted_context, user_id_key)
  let extracted_request_id = Context::get(extracted_context, request_id_key)
  let extracted_tenant_id = Context::get(extracted_context, tenant_id_key)
  
  assert_eq(extracted_trace_context.trace_id, "trace-7890123456789012")
  assert_eq(extracted_user_id, Some("user-12345"))
  assert_eq(extracted_request_id, Some("req-67890"))
  assert_eq(extracted_tenant_id, Some("tenant-abc"))
  
  // 验证跨服务调用链
  let service_calls = [
    ("api.gateway", "GET", "/api/v1/users"),
    ("auth.service", "POST", "/validate"),
    ("user.service", "GET", "/users/12345"),
    ("profile.service", "GET", "/profiles/12345")
  ]
  
  let propagated_contexts = []
  let current_context = extracted_context
  
  for (service, method, path) in service_calls {
    let service_context = Context::with_value(
      current_context,
      ContextKey::new("service.name"),
      service
    )
    
    propagated_contexts = propagated_contexts.push((service, service_context))
    current_context = service_context
  }
  
  // 验证上下文传播链
  for (service, ctx) in propagated_contexts {
    let service_name = Context::get(ctx, ContextKey::new("service.name"))
    assert_eq(service_name, Some(service))
    
    let trace_ctx = Context::span_context(ctx)
    assert_eq(trace_ctx.trace_id, "trace-7890123456789012")
    
    let user_id = Context::get(ctx, user_id_key)
    assert_eq(user_id, Some("user-12345"))
  }
}

// 测试7: 智能缓存策略
test "智能缓存策略测试" {
  // 创建智能缓存管理器
  let cache_manager = SmartCacheManager::new(
    max_size = 1000,
    ttl = 3600,  // 1小时
    eviction_policy = "lru_with_adaptive_ttl"
  )
  
  // 模拟不同的访问模式
  let access_patterns = [
    // 热点数据 - 高频访问
    ("hot.config.data", 1, 1640995200),
    ("hot.config.data", 2, 1640995201),
    ("hot.config.data", 3, 1640995202),
    ("hot.config.data", 4, 1640995203),
    ("hot.config.data", 5, 1640995204),
    
    // 温数据 - 中频访问
    ("warm.user.data", 1, 1640995210),
    ("warm.user.data", 2, 1640995220),
    ("warm.user.data", 3, 1640995230),
    
    // 冷数据 - 低频访问
    ("cold.audit.data", 1, 1640995240),
    
    // 突发访问数据
    ("burst.search.data", 1, 1640995250),
    ("burst.search.data", 2, 1640995251),
    ("burst.search.data", 3, 1640995252),
    ("burst.search.data", 4, 1640995253),
    ("burst.search.data", 5, 1640995254),
    ("burst.search.data", 6, 1640995255)
  ]
  
  // 处理访问模式
  let cache_stats = []
  for (key, access_id, timestamp) in access_patterns {
    let access_result = CacheManager::access(cache_manager, key, timestamp)
    cache_stats = cache_stats.push((key, access_id, timestamp, access_result))
  }
  
  // 验证缓存命中率
  let hot_data_accesses = cache_stats.filter(fn(t) { t.0 == "hot.config.data" })
  let warm_data_accesses = cache_stats.filter(fn(t) { t.0 == "warm.user.data" })
  let cold_data_accesses = cache_stats.filter(fn(t) { t.0 == "cold.audit.data" })
  let burst_data_accesses = cache_stats.filter(fn(t) { t.0 == "burst.search.data" })
  
  let hot_hit_rate = hot_data_accesses.filter(fn(t) { t.3.cache_hit }).length() / hot_data_accesses.length()
  let warm_hit_rate = warm_data_accesses.filter(fn(t) { t.3.cache_hit }).length() / warm_data_accesses.length()
  let cold_hit_rate = cold_data_accesses.filter(fn(t) { t.3.cache_hit }).length() / cold_data_accesses.length()
  let burst_hit_rate = burst_data_accesses.filter(fn(t) { t.3.cache_hit }).length() / burst_data_accesses.length()
  
  assert_true(hot_hit_rate > 0.8)  // 热数据应该有高命中率
  assert_true(warm_hit_rate > 0.5)  // 温数据应该有中等命中率
  assert_true(cold_hit_rate < 0.5)  // 冷数据应该有低命中率
  assert_true(burst_hit_rate > 0.6)  // 突发数据应该有较高命中率
  
  // 测试自适应TTL调整
  let ttl_adjustments = CacheManager::get_ttl_adjustments(cache_manager)
  let hot_data_ttl = ttl_adjustments.get("hot.config.data")
  let cold_data_ttl = ttl_adjustments.get("cold.audit.data")
  
  assert_true(hot_data_ttl > 3600)  // 热数据TTL应该延长
  assert_true(cold_data_ttl < 3600)  // 冷数据TTL应该缩短
  
  // 测试缓存预热策略
  let preload_keys = ["hot.config.data", "warm.user.data", "critical.security.data"]
  let preload_result = CacheManager::preload(cache_manager, preload_keys)
  
  assert_true(preload_result.success_count >= 2)
  assert_eq(preload_result.total_keys, 3)
  
  // 测试缓存一致性
  let consistency_check = CacheManager::check_consistency(cache_manager)
  assert_true(consistency_check.valid_entries > 0)
  assert_eq(consistency_check.invalid_entries, 0)
}

// 测试8: 多维度数据分析
test "多维度数据分析测试" {
  // 创建多维数据分析器
  let analyzer = MultiDimensionalAnalyzer::new()
  
  // 模拟多维指标数据
  let metric_data = [
    // API请求指标
    (metric_name = "api.request.duration", value = 0.125, 
     dimensions = [
       ("service", "api.gateway"),
       ("method", "GET"),
       ("endpoint", "/api/v1/users"),
       ("status_code", "200"),
       ("region", "us-west-2")
     ],
     timestamp = 1640995200),
     
    (metric_name = "api.request.duration", value = 0.250,
     dimensions = [
       ("service", "api.gateway"),
       ("method", "POST"),
       ("endpoint", "/api/v1/orders"),
       ("status_code", "201"),
       ("region", "us-west-2")
     ],
     timestamp = 1640995201),
     
    (metric_name = "api.request.duration", value = 1.500,
     dimensions = [
       ("service", "api.gateway"),
       ("method", "GET"),
       ("endpoint", "/api/v1/reports"),
       ("status_code", "500"),
       ("region", "us-east-1")
     ],
     timestamp = 1640995202),
     
    // 数据库查询指标
    (metric_name = "db.query.duration", value = 0.050,
     dimensions = [
       ("service", "user.service"),
       ("operation", "SELECT"),
       ("table", "users"),
       ("index_used", "idx_user_email"),
       ("region", "us-west-2")
     ],
     timestamp = 1640995203),
     
    (metric_name = "db.query.duration", value = 0.200,
     dimensions = [
       ("service", "order.service"),
       ("operation", "INSERT"),
       ("table", "orders"),
       ("index_used", "none"),
       ("region", "us-west-2")
     ],
     timestamp = 1640995204),
     
    // 缓存操作指标
    (metric_name = "cache.operation.duration", value = 0.005,
     dimensions = [
       ("service", "auth.service"),
       ("operation", "GET"),
       ("cache_type", "redis"),
       ("hit", "true"),
       ("region", "us-west-2")
     ],
     timestamp = 1640995205),
     
    (metric_name = "cache.operation.duration", value = 0.015,
     dimensions = [
       ("service", "auth.service"),
       ("operation", "GET"),
       ("cache_type", "redis"),
       ("hit", "false"),
       ("region", "us-west-2")
     ],
     timestamp = 1640995206)
  ]
  
  // 添加指标数据到分析器
  for data in metric_data {
    Analyzer::add_metric(analyzer, data)
  }
  
  // 按服务维度分析
  let service_analysis = Analyzer::group_by(analyzer, "service")
  let api_gateway_metrics = service_analysis.get("api.gateway")
  let user_service_metrics = service_analysis.get("user.service")
  
  assert_true(api_gateway_metrics.is_some())
  assert_true(user_service_metrics.is_some())
  
  let api_gateway_stats = api_gateway_metrics.unwrap()
  assert_eq(api_gateway_stats.count, 3)
  assert_eq(api_gateway_stats.avg, 0.625)  // (0.125 + 0.250 + 1.500) / 3
  assert_eq(api_gateway_stats.max, 1.500)
  assert_eq(api_gateway_stats.min, 0.125)
  
  // 按地区维度分析
  let region_analysis = Analyzer::group_by(analyzer, "region")
  let us_west_2_metrics = region_analysis.get("us-west-2")
  let us_east_1_metrics = region_analysis.get("us-east-1")
  
  assert_true(us_west_2_metrics.is_some())
  assert_true(us_east_1_metrics.is_some())
  
  let us_west_2_stats = us_west_2_metrics.unwrap()
  assert_eq(us_west_2_stats.count, 5)
  assert_eq(us_west_2_stats.avg, 0.128)  // 5个us-west-2指标的平均值
  
  // 多维度交叉分析
  let cross_analysis = Analyzer::cross_group(analyzer, ["service", "method"])
  let api_get_metrics = cross_analysis.get(("api.gateway", "GET"))
  let api_post_metrics = cross_analysis.get(("api.gateway", "POST"))
  
  assert_true(api_get_metrics.is_some())
  assert_true(api_post_metrics.is_some())
  
  let api_get_stats = api_get_metrics.unwrap()
  assert_eq(api_get_stats.count, 2)
  assert_eq(api_get_stats.avg, 0.8125)  // (0.125 + 1.500) / 2
  
  // 时间序列分析
  let time_series_analysis = Analyzer::time_series(analyzer, "api.request.duration", 60)  // 60秒窗口
  assert_true(time_series_analysis.windows.length() > 0)
  
  // 异常检测
  let anomaly_detection = Analyzer::detect_anomalies(analyzer, "api.request.duration", 2.0)  // 2倍标准差
  assert_true(anomaly_detection.anomalies.length() >= 1)
  assert_true(anomaly_detection.anomalies.some(fn(a) { a.value == 1.500 }))  // 1.500应该是异常值
  
  // 趋势分析
  let trend_analysis = Analyzer::analyze_trend(analyzer, "api.request.duration")
  assert_true(trend_analysis.trend_direction == "increasing" || 
              trend_analysis.trend_direction == "decreasing" || 
              trend_analysis.trend_direction == "stable")
  
  // 生成分析报告
  let report = Analyzer::generate_report(analyzer, [
    "service", "method", "region", "status_code"
  ])
  
  assert_true(report.summary.total_metrics == metric_data.length())
  assert_true(report.summary.unique_services >= 3)
  assert_true(report.summary.unique_regions >= 2)
  assert_true(report.performance_analysis.top_slow_operations.length() > 0)
  assert_true(report.anomaly_analysis.detected_anomalies >= 1)
}