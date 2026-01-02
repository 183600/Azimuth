// Azimuth 高级遥测功能测试用例
// 专注于遥测系统的高级功能和复杂场景

// 测试1: 自定义指标聚合器
test "自定义指标聚合器测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "custom.aggregator")
  
  // 创建自定义聚合器
  let percentile_aggregator = PercentileAggregator::new([50.0, 90.0, 95.0, 99.0])
  let custom_counter = Meter::create_counter_with_aggregator(meter, "custom.requests", percentile_aggregator)
  
  // 记录不同延迟值
  let latencies = [0.025, 0.050, 0.075, 0.100, 0.150, 0.200, 0.300, 0.500]
  for latency in latencies {
    Counter::add(custom_counter, latency)
  }
  
  // 获取百分位数结果
  let p50 = PercentileAggregator::get_percentile(percentile_aggregator, 50.0)
  let p95 = PercentileAggregator::get_percentile(percentile_aggregator, 95.0)
  let p99 = PercentileAggregator::get_percentile(percentile_aggregator, 99.0)
  
  // 验证聚合结果
  assert_true(p50 >= 0.050 && p50 <= 0.100)
  assert_true(p95 >= 0.200 && p95 <= 0.500)
  assert_true(p99 >= 0.300 && p99 <= 0.500)
}

// 测试2: 动态采样策略
test "动态采样策略测试" {
  // 创建自适应采样器
  let adaptive_sampler = AdaptiveSampler::new(0.1, 100.0, 1000.0)
  
  // 测试低负载情况下的采样
  let mut sampled_low = 0
  for i in 0..=100 {
    let decision = AdaptiveSampler::should_sample(adaptive_sampler, "trace-" + i.to_string())
    if decision { sampled_low = sampled_low + 1 }
  }
  
  // 测试高负载情况下的采样
  let mut sampled_high = 0
  for i in 0..=1000 {
    let decision = AdaptiveSampler::should_sample(adaptive_sampler, "trace-" + i.to_string())
    if decision { sampled_high = sampled_high + 1 }
  }
  
  // 验证采样率调整
  assert_true(sampled_low >= 5 && sampled_low <= 15)  // 约10%采样率
  assert_true(sampled_high >= 1 && sampled_high <= 5)  // 约1%采样率
  assert_true(sampled_low > sampled_high)  // 高负载时采样率降低
}

// 测试3: 分布式追踪链路分析
test "分布式追踪链路分析测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "chain.analyzer")
  
  // 创建服务链路
  let api_span = Tracer::start_span(tracer, "api.gateway")
  let api_ctx = Span::context(api_span)
  
  let auth_span = Tracer::start_span_with_context(tracer, "auth.service", api_ctx)
  let auth_ctx = Span::context(auth_span)
  Span::end(auth_span)
  
  let user_span = Tracer::start_span_with_context(tracer, "user.service", auth_ctx)
  let user_ctx = Span::context(user_span)
  Span::end(user_span)
  
  let db_span = Tracer::start_span_with_context(tracer, "database.query", user_ctx)
  Span::set_attribute(db_span, "db.query", StringValue("SELECT * FROM users"))
  Span::end(db_span)
  
  Span::end(api_span)
  
  // 分析链路
  let chain_analyzer = TraceChainAnalyzer::new()
  let analysis = TraceChainAnalyzer::analyze(chain_analyzer, api_span)
  
  // 验证链路分析结果
  assert_eq(TraceChainAnalysis::span_count(analysis), 4)
  assert_eq(TraceChainAnalysis::max_depth(analysis), 3)
  assert_eq(TraceChainAnalysis::critical_path_duration(analysis), Span::duration(api_span))
  assert_true(TraceChainAnalysis::contains_database_operations(analysis))
}

// 测试4: 实时异常检测
test "实时异常检测测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "anomaly.detector")
  
  // 创建异常检测器
  let anomaly_detector = AnomalyDetector::new(2.0, 0.95)  // 2倍标准差，95%置信度
  
  // 创建错误率指标
  let error_rate = Meter::create_gauge(meter, "error.rate")
  let request_rate = Meter::create_counter(meter, "request.count")
  let error_count = Meter::create_counter(meter, "error.count")
  
  // 模拟正常情况
  for i in 0..=100 {
    Counter::add(request_rate, 100.0)
    Counter::add(error_count, 2.0)  // 2%错误率
    Gauge::set(error_rate, 0.02)
    AnomalyDetector::record(anomaly_detector, 0.02)
  }
  
  // 模拟异常情况
  let is_anomaly = false
  for i in 0..=10 {
    Counter::add(request_rate, 100.0)
    Counter::add(error_count, 15.0)  // 15%错误率
    Gauge::set(error_rate, 0.15)
    is_anomaly = AnomalyDetector::record(anomaly_detector, 0.15)
    if is_anomaly { break }
  }
  
  // 验证异常检测
  assert_true(is_anomaly)
  assert_eq(AnomalyDetector::anomaly_count(anomaly_detector), 1)
  assert_true(AnomalyDetector::current_baseline(anomaly_detector) < 0.05)
}

// 测试5: 多维度指标查询
test "多维度指标查询测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "multidimensional.query")
  
  // 创建多维度指标
  let api_latency = Meter::create_histogram(meter, "api.latency")
  
  // 记录不同维度的数据
  let endpoints = ["users", "orders", "products"]
  let methods = ["GET", "POST", "PUT"]
  let regions = ["us-east", "us-west", "eu-west"]
  
  for endpoint in endpoints {
    for method in methods {
      for region in regions {
        let attributes = [
          ("endpoint", StringValue(endpoint)),
          ("method", StringValue(method)),
          ("region", StringValue(region))
        ]
        
        // 记录不同延迟值
        let base_latency = match endpoint {
          "users" => 0.050,
          "orders" => 0.100,
          "products" => 0.075
        }
        
        let method_factor = match method {
          "GET" => 1.0,
          "POST" => 1.5,
          "PUT" => 1.2
        }
        
        let region_factor = match region {
          "us-east" => 1.0,
          "us-west" => 1.2,
          "eu-west" => 1.5
        }
        
        let latency = base_latency * method_factor * region_factor
        Histogram::record_with_attributes(api_latency, latency, attributes)
      }
    }
  }
  
  // 创建查询构建器
  let query_builder = MetricQueryBuilder::new()
  let query = MetricQueryBuilder::filter_by_attribute(query_builder, "endpoint", "users")
    .filter_by_attribute("method", "GET")
    .group_by("region")
    .aggregate(Aggregation::Average)
    .build()
  
  // 执行查询
  let query_engine = MetricQueryEngine::new()
  let results = MetricQueryEngine::execute(query_engine, query)
  
  // 验证查询结果
  assert_eq(MetricQueryResults::group_count(results), 3)  // 3个地区
  assert_true(MetricQueryResults::contains_group(results, "region", "us-east"))
  assert_true(MetricQueryResults::contains_group(results, "region", "us-west"))
  assert_true(MetricQueryResults::contains_group(results, "region", "eu-west"))
}

// 测试6: 智能告警系统
test "智能告警系统测试" {
  // 创建告警管理器
  let alert_manager = AlertManager::new()
  
  // 创建告警规则
  let error_rate_rule = AlertRule::new("high_error_rate")
    .with_metric("error.rate")
    .with_condition(Condition::GreaterThan(0.05))
    .with_duration(Duration::from_minutes(5))
    .with_severity(AlertSeverity::Critical)
    .with_message("Error rate is above 5% for 5 minutes")
  
  let latency_rule = AlertRule::new("high_latency")
    .with_metric("api.latency.p99")
    .with_condition(Condition::GreaterThan(1.0))
    .with_duration(Duration::from_minutes(2))
    .with_severity(AlertSeverity::Warning)
    .with_message("P99 latency is above 1 second for 2 minutes")
  
  // 注册告警规则
  AlertManager::register_rule(alert_manager, error_rate_rule)
  AlertManager::register_rule(alert_manager, latency_rule)
  
  // 模拟指标数据触发告警
  let metrics = [
    ("error.rate", 0.08, Timestamp::now()),
    ("api.latency.p99", 1.2, Timestamp::now())
  ]
  
  let mut alerts_triggered = 0
  for (metric_name, value, timestamp) in metrics {
    let alerts = AlertManager::evaluate_rules(alert_manager, metric_name, value, timestamp)
    alerts_triggered = alerts_triggered + alerts.length()
  }
  
  // 验证告警触发
  assert_eq(alerts_triggered, 2)
  
  // 验证告警内容
  let active_alerts = AlertManager::get_active_alerts(alert_manager)
  assert_eq(active_alerts.length(), 2)
  
  let error_alert = active_alerts.find(fn(alert) { Alert::rule_name(alert) == "high_error_rate" })
  let latency_alert = active_alerts.find(fn(alert) { Alert::rule_name(alert) == "high_latency" })
  
  assert_true(error_alert.is_some())
  assert_true(latency_alert.is_some())
  assert_eq(Alert::severity(error_alert.unwrap()), AlertSeverity::Critical)
  assert_eq(Alert::severity(latency_alert.unwrap()), AlertSeverity::Warning)
}

// 测试7: 遥测数据压缩和存储优化
test "遥测数据压缩和存储优化测试" {
  // 创建大量遥测数据
  let telemetry_data = []
  for i in 0..=1000 {
    let span = Span::new("span-" + i.to_string(), Server, SpanContext::new("trace-123", "span-" + i.to_string(), true, ""))
    Span::set_attribute(span, "service.name", StringValue("test.service"))
    Span::set_attribute(span, "operation.name", StringValue("test.operation"))
    Span::set_attribute(span, "duration.ms", IntValue(i * 10))
    telemetry_data = telemetry_data.push(span)
  }
  
  // 创建压缩器
  let compressor = TelemetryCompressor::new(CompressionAlgorithm::Gzip)
  
  // 压缩数据
  let compressed_data = TelemetryCompressor::compress(compressor, telemetry_data)
  let compression_ratio = compressed_data.length() as Float / telemetry_data.length() as Float
  
  // 验证压缩效果
  assert_true(compression_ratio < 0.5)  // 压缩率应小于50%
  
  // 解压数据
  let decompressed_data = TelemetryCompressor::decompress(compressor, compressed_data)
  
  // 验证解压后数据完整性
  assert_eq(decompressed_data.length(), telemetry_data.length())
  
  // 验证数据内容
  for i in 0..=telemetry_data.length() - 1 {
    let original = telemetry_data[i]
    let decompressed = decompressed_data[i]
    assert_eq(Span::name(original), Span::name(decompressed))
    assert_eq(Span::duration(original), Span::duration(decompressed))
  }
}

// 测试8: 跨服务遥测关联分析
test "跨服务遥测关联分析测试" {
  // 创建多个服务的遥测数据
  let services = ["api.gateway", "auth.service", "user.service", "payment.service"]
  let traces = []
  
  // 为每个服务创建追踪数据
  for service in services {
    let tracer_provider = TracerProvider::default()
    let tracer = TracerProvider::get_tracer(tracer_provider, service)
    
    let root_span = Tracer::start_span(tracer, service + ".operation")
    let root_ctx = Span::context(root_span)
    
    // 添加服务间调用
    if service != "payment.service" {
      let next_service_index = services.index_of(service).unwrap() + 1
      if next_service_index < services.length() {
        let next_service = services[next_service_index]
        let call_span = Tracer::start_span_with_context(tracer, "call." + next_service, root_ctx)
        Span::set_attribute(call_span, "target.service", StringValue(next_service))
        Span::end(call_span)
      }
    }
    
    Span::end(root_span)
    traces = traces.push(root_span)
  }
  
  // 创建关联分析器
  let correlation_analyzer = CrossServiceCorrelationAnalyzer::new()
  
  // 分析服务间关联
  let correlation_result = CrossServiceCorrelationAnalyzer::analyze(correlation_analyzer, traces)
  
  // 验证关联分析结果
  assert_eq(CorrelationResult::service_count(correlation_result), 4)
  assert_eq(CorrelationResult::call_count(correlation_result), 3)
  
  // 验证调用链
  let call_chain = CorrelationResult::get_call_chain(correlation_result)
  assert_eq(call_chain[0], "api.gateway")
  assert_eq(call_chain[1], "auth.service")
  assert_eq(call_chain[2], "user.service")
  assert_eq(call_chain[3], "payment.service")
  
  // 验证服务依赖图
  let dependency_graph = CorrelationResult::get_dependency_graph(correlation_result)
  assert_true(DependencyGraph::has_edge(dependency_graph, "api.gateway", "auth.service"))
  assert_true(DependencyGraph::has_edge(dependency_graph, "auth.service", "user.service"))
  assert_true(DependencyGraph::has_edge(dependency_graph, "user.service", "payment.service"))
  assert_false(DependencyGraph::has_edge(dependency_graph, "payment.service", "api.gateway"))
}