// Azimuth Telemetry System End-to-End Integration Tests
// 遥测系统端到端集成测试用例
// 测试整个遥测系统的端到端功能和集成

import "azimuth/azimuth"

// Test 1: 完整遥测数据流测试
pub test "完整遥测数据流测试" {
  // 创建端到端测试环境
  let e2e_test_env = azimuth::E2ETestEnvironment::new()
  
  // 配置完整的遥测管道
  azimuth::E2ETestEnvironment::configure_pipeline(e2e_test_env, {
    "trace.exporter": "jaeger",
    "metric.exporter": "prometheus",
    "log.exporter": "elasticsearch",
    "sampling.rate": "1.0",
    "batch.size": "100",
    "batch.timeout": "5s"
  })
  
  // 创建服务组件
  let services = [
    {
      "name": "api-gateway",
      "port": 8080,
      "endpoints": ["/api/users", "/api/orders", "/api/products"]
    },
    {
      "name": "user-service",
      "port": 8081,
      "endpoints": ["/users/profile", "/users/preferences"]
    },
    {
      "name": "order-service",
      "port": 8082,
      "endpoints": ["/orders/create", "/orders/status"]
    },
    {
      "name": "product-service",
      "port": 8083,
      "endpoints": ["/products/search", "/products/details"]
    }
  ]
  
  // 为每个服务创建遥测组件
  let service_components = {}
  
  for service in services {
    let component = {
      "tracer": azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), service["name"]),
      "meter": azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), service["name"]),
      "logger": azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), service["name"])
    }
    service_components[service["name"]] = component
  }
  
  // 模拟完整的用户请求流程
  let user_requests = 50
  let trace_ids = []
  let request_latencies = []
  
  for request_id in 0..user_requests {
    let request_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    // API Gateway处理请求
    let gateway_component = service_components["api-gateway"]
    let gateway_span = azimuth::Tracer::start_span(gateway_component["tracer"], "api-gateway.request")
    let trace_id = azimuth::SpanContext::trace_id(azimuth::Span::span_context(gateway_span))
    trace_ids.push(trace_id)
    
    // 随机选择请求类型
    let request_type = match request_id % 3 {
      0 => "user.profile",
      1 => "order.create",
      _ => "product.search"
    }
    
    azimuth::Span::add_event(gateway_span, "request.received", Some([
      ("request.id", azimuth::IntValue(request_id)),
      ("request.type", azimuth::StringValue(request_type)),
      ("user.id", azimuth::StringValue("user-" + (request_id % 100).to_string()))
    ]))
    
    // 记录API Gateway度量
    let gateway_meter = gateway_component["meter"]
    let request_counter = azimuth::Meter::create_counter(gateway_meter, "requests.total")
    azimuth::Counter::add(request_counter, 1.0, Some([
      ("request.type", azimuth::StringValue(request_type)),
      ("endpoint", azimuth::StringValue(match request_type {
        "user.profile" => "/api/users",
        "order.create" => "/api/orders",
        _ => "/api/products"
      }))
    ]))
    
    // 记录API Gateway日志
    let gateway_logger = gateway_component["logger"]
    let gateway_log = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Processing " + request_type + " request"),
      Some([
        ("request.id", azimuth::IntValue(request_id)),
        ("request.type", azimuth::StringValue(request_type)),
        ("trace.id", azimuth::StringValue(trace_id))
      ]),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some(trace_id),
      Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(gateway_span))),
      Some(azimuth::Context::root())
    )
    azimuth::Logger::emit(gateway_logger, gateway_log)
    
    // 根据请求类型调用下游服务
    if request_type == "user.profile" {
      // 调用用户服务
      let user_component = service_components["user-service"]
      let user_span = azimuth::Tracer::start_span_with_parent(user_component["tracer"], "user-service.get-profile", gateway_span)
      
      azimuth::Span::add_event(user_span, "profile.fetch.started", Some([
        ("request.id", azimuth::IntValue(request_id)),
        ("user.id", azimuth::StringValue("user-" + (request_id % 100).to_string()))
      ]))
      
      // 记录用户服务度量
      let user_meter = user_component["meter"]
      let profile_counter = azimuth::Meter::create_counter(user_meter, "profile.fetches")
      azimuth::Counter::add(profile_counter, 1.0, Some([
        ("fetch.result", azimuth::StringValue("success"))
      ]))
      
      let profile_latency = azimuth::Meter::create_histogram(user_meter, "profile.fetch.latency", Some("Profile fetch latency"), Some("ms"))
      azimuth::Histogram::record(profile_latency, 25.0 + azimuth::Random::next_double(azimuth::Random::system()) * 15.0)
      
      azimuth::Span::set_status(user_span, azimuth::Ok)
      azimuth::Span::end(user_span)
      
    } else if request_type == "order.create" {
      // 调用订单服务
      let order_component = service_components["order-service"]
      let order_span = azimuth::Tracer::start_span_with_parent(order_component["tracer"], "order-service.create", gateway_span)
      
      azimuth::Span::add_event(order_span, "order.creation.started", Some([
        ("request.id", azimuth::IntValue(request_id)),
        ("user.id", azimuth::StringValue("user-" + (request_id % 100).to_string())),
        ("product.id", azimuth::StringValue("product-" + (request_id % 50).to_string()))
      ]))
      
      // 记录订单服务度量
      let order_meter = order_component["meter"]
      let order_counter = azimuth::Meter::create_counter(order_meter, "orders.created")
      azimuth::Counter::add(order_counter, 1.0, Some([
        ("order.status", azimuth::StringValue("pending"))
      ]))
      
      let order_amount = 10.0 + azimuth::Random::next_double(azimuth::Random::system()) * 100.0
      let order_amount_histogram = azimuth::Meter::create_histogram(order_meter, "order.amount", Some("Order amount"), Some("USD"))
      azimuth::Histogram::record(order_amount_histogram, order_amount)
      
      azimuth::Span::set_status(order_span, azimuth::Ok)
      azimuth::Span::end(order_span)
      
    } else {
      // 调用产品服务
      let product_component = service_components["product-service"]
      let product_span = azimuth::Tracer::start_span_with_parent(product_component["tracer"], "product-service.search", gateway_span)
      
      azimuth::Span::add_event(product_span, "product.search.started", Some([
        ("request.id", azimuth::IntValue(request_id)),
        ("search.query", azimuth::StringValue("query-" + (request_id % 20).to_string())),
        ("results.limit", azimuth::IntValue(10 + request_id % 20))
      ]))
      
      // 记录产品服务度量
      let product_meter = product_component["meter"]
      let search_counter = azimuth::Meter::create_counter(product_meter, "product.searches")
      azimuth::Counter::add(search_counter, 1.0, Some([
        ("search.type", azimuth::StringValue("text"))
      ]))
      
      let search_results = azimuth::Meter::create_histogram(product_meter, "search.results", Some("Search results count"), None)
      azimuth::Histogram::record(search_results, 5.0 + (request_id % 15).to_double())
      
      azimuth::Span::set_status(product_span, azimuth::Ok)
      azimuth::Span::end(product_span)
    }
    
    // 完成API Gateway请求
    let request_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let request_latency = (request_end_time - request_start_time) / 1000000.0  // 转换为毫秒
    request_latencies.push(request_latency)
    
    azimuth::Span::add_event(gateway_span, "request.completed", Some([
      ("request.id", azimuth::IntValue(request_id)),
      ("request.type", azimuth::StringValue(request_type)),
      ("request.latency", azimuth::FloatValue(request_latency))
    ]))
    
    // 记录响应延迟度量
    let response_latency_histogram = azimuth::Meter::create_histogram(gateway_meter, "response.latency", Some("Response latency"), Some("ms"))
    azimuth::Histogram::record(response_latency_histogram, request_latency, Some([
      ("request.type", azimuth::StringValue(request_type))
    ]))
    
    azimuth::Span::set_status(gateway_span, azimuth::Ok)
    azimuth::Span::end(gateway_span)
  }
  
  // 等待数据导出
  azimuth::Clock::sleep(1000)
  
  // 验证端到端数据流
  let exported_traces = azimuth::E2ETestEnvironment::get_exported_traces(e2e_test_env)
  let exported_metrics = azimuth::E2ETestEnvironment::get_exported_metrics(e2e_test_env)
  let exported_logs = azimuth::E2ETestEnvironment::get_exported_logs(e2e_test_env)
  
  // 验证Trace数据
  assert_true(exported_traces.length() >= user_requests)
  for trace in exported_traces {
    // 验证Trace ID存在
    assert_true(azimuth::TraceData::has_trace_id(trace))
    
    // 验证Span层次结构
    let spans = azimuth::TraceData::get_spans(trace)
    assert_true(spans.length() >= 2)  // 至少有Gateway和下游服务的Span
    
    // 验证父子关系
    let root_span = spans.find(s => azimuth::SpanData::is_root(s))
    assert_true(root_span.is_some())
    
    // 验证所有Span属于同一个Trace
    let trace_id = azimuth::TraceData::get_trace_id(trace)
    for span in spans {
      assert_eq(azimuth::SpanData::get_trace_id(span), trace_id)
    }
  }
  
  // 验证Metric数据
  assert_true(exported_metrics.length() > 0)
  
  // 验证请求计数器
  let request_metrics = exported_metrics.filter(m => azimuth::MetricData::get_name(m) == "requests.total")
  assert_true(request_metrics.length() > 0)
  
  let total_requests = 0
  for metric in request_metrics {
    total_requests = total_requests + azimuth::MetricData::get_value(metric).to_int()
  }
  assert_true(total_requests >= user_requests)
  
  // 验证延迟直方图
  let latency_metrics = exported_metrics.filter(m => azimuth::MetricData::get_name(m) == "response.latency")
  assert_true(latency_metrics.length() > 0)
  
  // 验证Log数据
  assert_true(exported_logs.length() >= user_requests)
  for log in exported_logs {
    // 验证日志包含必要的字段
    assert_true(azimuth::LogData::has_timestamp(log))
    assert_true(azimuth::LogData::has_severity(log))
    assert_true(azimuth::LogData::has_message(log))
    
    // 验证日志与Trace关联
    if azimuth::LogData::has_trace_id(log) {
      let log_trace_id = azimuth::LogData::get_trace_id(log)
      assert_true(trace_ids.contains(log_trace_id))
    }
  }
  
  // 创建端到端性能度量
  let e2e_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "e2e-integration")
  let throughput_gauge = azimuth::Meter::create_gauge(e2e_meter, "request.throughput")
  let latency_histogram = azimuth::Meter::create_histogram(e2e_meter, "e2e.latency", Some("End-to-end latency"), Some("ms"))
  let trace_completeness_gauge = azimuth::Meter::create_gauge(e2e_meter, "trace.completeness")
  
  // 计算吞吐量
  let test_duration = 5000.0  // 5秒（毫秒）
  let throughput = user_requests.to_double() / (test_duration / 1000.0)  // 请求/秒
  azimuth::Gauge::record(throughput_gauge, throughput)
  
  // 记录延迟分布
  for latency in request_latencies {
    azimuth::Histogram::record(latency_histogram, latency)
  }
  
  // 计算Trace完整性
  let trace_completeness = exported_traces.length().to_double() / user_requests.to_double()
  azimuth::Gauge::record(trace_completeness_gauge, trace_completeness)
  
  // 验证端到端性能指标
  assert_true(throughput > 5.0)  // 至少每秒5个请求
  assert_true(trace_completeness > 0.9)  // 至少90%的Trace完整性
}

// Test 2: 多租户隔离测试
pub test "多租户隔离测试" {
  // 创建多租户测试环境
  let multi_tenant_env = azimuth::MultiTenantTestEnvironment::new()
  
  // 配置租户隔离策略
  azimuth::MultiTenantTestEnvironment::configure_isolation(multi_tenant_env, {
    "tenant.isolation.enabled": "true",
    "tenant.data.separation": "strict",
    "tenant.resource.quota": "1000",
    "tenant.rate.limit": "100"
  })
  
  // 创建租户
  let tenants = [
    {
      "id": "tenant-001",
      "name": "Acme Corp",
      "quota": 500,
      "rate_limit": 50
    },
    {
      "id": "tenant-002",
      "name": "Beta Inc",
      "quota": 300,
      "rate_limit": 30
    },
    {
      "id": "tenant-003",
      "name": "Gamma LLC",
      "quota": 200,
      "rate_limit": 20
    }
  ]
  
  // 为每个租户创建隔离的遥测组件
  let tenant_components = {}
  
  for tenant in tenants {
    let tenant_resource = azimuth::Resource::new()
    let tenant_attrs = [
      ("tenant.id", azimuth::StringValue(tenant["id"])),
      ("tenant.name", azimuth::StringValue(tenant["name"])),
      ("service.name", azimuth::StringValue("multi-tenant-service"))
    ]
    
    let resource_with_attrs = azimuth::Resource::with_attributes(tenant_resource, tenant_attrs)
    
    let component = {
      "tracer": azimuth::TracerProvider::with_resource(resource_with_attrs).get_tracer(tenant["id"] + "-tracer"),
      "meter": azimuth::MeterProvider::with_resource(resource_with_attrs).get_meter(tenant["id"] + "-meter"),
      "logger": azimuth::LoggerProvider::with_resource(resource_with_attrs).get_logger(tenant["id"] + "-logger"),
      "quota": tenant["quota"],
      "rate_limit": tenant["rate_limit"]
    }
    
    tenant_components[tenant["id"]] = component
  }
  
  // 为每个租户生成遥测数据
  let tenant_data = {}
  
  for tenant in tenants {
    let component = tenant_components[tenant["id"]]
    let tenant_telemetry = {
      "spans": [],
      "metrics": [],
      "logs": []
    }
    
    // 生成Span数据
    for i in 0..(tenant["quota"] / 5) {
      let span = azimuth::Tracer::start_span(component["tracer"], tenant["id"] + "-operation-" + i.to_string())
      azimuth::Span::add_event(span, "tenant.operation", Some([
        ("tenant.id", azimuth::StringValue(tenant["id"])),
        ("operation.id", azimuth::IntValue(i))
      ]))
      azimuth::Span::end(span)
      tenant_telemetry["spans"].push(span)
    }
    
    // 生成度量数据
    let counter = azimuth::Meter::create_counter(component["meter"], tenant["id"] + ".counter")
    let histogram = azimuth::Meter::create_histogram(component["meter"], tenant["id"] + ".histogram", Some("Tenant histogram"), Some("ms"))
    
    for i in 0..(tenant["quota"] / 2) {
      azimuth::Counter::add(counter, i.to_double(), Some([
        ("tenant.id", azimuth::StringValue(tenant["id"]))
      ]))
      
      azimuth::Histogram::record(histogram, i.to_double(), Some([
        ("tenant.id", azimuth::StringValue(tenant["id"]))
      ]))
      
      tenant_telemetry["metrics"].push({
        "index": i,
        "counter": i.to_double(),
        "histogram": i.to_double()
      })
    }
    
    // 生成日志数据
    for i in 0..(tenant["quota"] / 3) {
      let log_record = azimuth::LogRecord::new(azimuth::Info, tenant["id"] + " log " + i.to_string())
      azimuth::Logger::emit(component["logger"], log_record)
      
      tenant_telemetry["logs"].push({
        "index": i,
        "message": tenant["id"] + " log " + i.to_string()
      })
    }
    
    tenant_data[tenant["id"]] = tenant_telemetry
  }
  
  // 测试租户隔离
  let isolation_results = azimuth::MultiTenantTestEnvironment::test_isolation(multi_tenant_env, tenant_components)
  
  // 验证租户隔离结果
  for tenant in tenants {
    let result = isolation_results[tenant["id"]]
    
    // 验证数据隔离
    assert_true(azimuth::IsolationResult::is_data_isolated(result))
    
    // 验证资源配额
    assert_true(azimuth::IsolationResult::within_quota(result))
    
    // 验证速率限制
    assert_true(azimuth::IsolationResult::within_rate_limit(result))
    
    // 验证租户标识
    let tenant_resources = azimuth::IsolationResult::get_tenant_resources(result)
    for resource in tenant_resources {
      assert_eq(azimuth::Resource::get_attribute(resource, "tenant.id"), Some(azimuth::StringValue(tenant["id"])))
      assert_eq(azimuth::Resource::get_attribute(resource, "tenant.name"), Some(azimuth::StringValue(tenant["name"])))
    }
  }
  
  // 测试跨租户数据隔离
  let cross_tenant_result = azimuth::MultiTenantTestEnvironment::test_cross_tenant_isolation(multi_tenant_env)
  assert_true(azimuth::CrossTenantResult::is_isolated(cross_tenant_result))
  
  // 验证租户A不能访问租户B的数据
  let tenant_001_access = azimuth::MultiTenantTestEnvironment::test_tenant_access(multi_tenant_env, "tenant-001", "tenant-002")
  assert_false(azimuth::TenantAccessResult::has_access(tenant_001_access))
  
  // 验证租户只能访问自己的数据
  for tenant in tenants {
    for other_tenant in tenants {
      if tenant["id"] != other_tenant["id"] {
        let access_result = azimuth::MultiTenantTestEnvironment::test_tenant_access(multi_tenant_env, tenant["id"], other_tenant["id"])
        assert_false(azimuth::TenantAccessResult::has_access(access_result))
      }
    }
  }
  
  // 创建多租户性能度量
  let multi_tenant_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "multi-tenant-integration")
  let tenant_count_gauge = azimuth::Meter::create_gauge(multi_tenant_meter, "active.tenants")
  let isolation_score_gauge = azimuth::Meter::create_gauge(multi_tenant_meter, "isolation.effectiveness")
  let quota_utilization_histogram = azimuth::Meter::create_histogram(multi_tenant_meter, "quota.utilization", Some("Quota utilization"), None)
  
  azimuth::Gauge::record(tenant_count_gauge, tenants.length().to_double())
  
  // 计算隔离有效性
  let isolation_score = azimuth::MultiTenantTestEnvironment::calculate_isolation_score(multi_tenant_env)
  azimuth::Gauge::record(isolation_score_gauge, isolation_score)
  
  // 记录配额利用率
  for tenant in tenants {
    let component = tenant_components[tenant["id"]]
    let utilization = tenant_data[tenant["id"]]["spans"].length().to_double() / component["quota"].to_double()
    azimuth::Histogram::record(quota_utilization_histogram, utilization, Some([
      ("tenant.id", azimuth::StringValue(tenant["id"]))
    ]))
  }
  
  // 验证多租户性能指标
  assert_true(isolation_score > 0.95)  // 隔离有效性应该大于95%
}

// Test 3: 可观测性数据一致性测试
pub test "可观测性数据一致性测试" {
  // 创建一致性测试环境
  let consistency_env = azimuth::ConsistencyTestEnvironment::new()
  
  // 配置一致性检查策略
  azimuth::ConsistencyTestEnvironment::configure_consistency_checks(consistency_env, {
    "trace.metric.correlation": "enabled",
    "trace.log.correlation": "enabled",
    "metric.log.correlation": "enabled",
    "temporal.consistency": "strict"
  })
  
  // 创建测试服务
  let service_name = "consistency-test-service"
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), service_name)
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), service_name)
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), service_name)
  
  // 生成一致的遥测数据
  let operations = 100
  let correlation_ids = []
  
  for operation_id in 0..operations {
    let correlation_id = "corr-" + operation_id.to_string()
    correlation_ids.push(correlation_id)
    
    let operation_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    // 创建Span
    let span = azimuth::Tracer::start_span(tracer, "consistency-operation-" + operation_id.to_string())
    let span_context = azimuth::Span::span_context(span)
    let trace_id = azimuth::SpanContext::trace_id(span_context)
    let span_id = azimuth::SpanContext::span_id(span_context)
    
    // 添加相关性标识
    azimuth::Span::add_event(span, "operation.started", Some([
      ("operation.id", azimuth::IntValue(operation_id)),
      ("correlation.id", azimuth::StringValue(correlation_id)),
      ("service.name", azimuth::StringValue(service_name))
    ]))
    
    // 创建与Span相关的度量
    let operation_counter = azimuth::Meter::create_counter(meter, "operations.total")
    azimuth::Counter::add(operation_counter, 1.0, Some([
      ("operation.id", azimuth::IntValue(operation_id)),
      ("correlation.id", azimuth::StringValue(correlation_id)),
      ("trace.id", azimuth::StringValue(trace_id)),
      ("span.id", azimuth::StringValue(span_id))
    ]))
    
    let operation_latency = azimuth::Meter::create_histogram(meter, "operation.latency", Some("Operation latency"), Some("ms"))
    
    // 模拟操作处理时间
    azimuth::Clock::sleep(5 + (operation_id % 10))
    
    let operation_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let latency = (operation_end_time - operation_start_time) / 1000000.0  // 转换为毫秒
    
    azimuth::Histogram::record(operation_latency, latency, Some([
      ("operation.id", azimuth::IntValue(operation_id)),
      ("correlation.id", azimuth::StringValue(correlation_id)),
      ("trace.id", azimuth::StringValue(trace_id)),
      ("span.id", azimuth::StringValue(span_id))
    ]))
    
    // 创建与Span相关的日志
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Operation " + operation_id.to_string() + " completed successfully"),
      Some([
        ("operation.id", azimuth::IntValue(operation_id)),
        ("correlation.id", azimuth::StringValue(correlation_id)),
        ("service.name", azimuth::StringValue(service_name)),
        ("trace.id", azimuth::StringValue(trace_id)),
        ("span.id", azimuth::StringValue(span_id)),
        ("operation.latency", azimuth::FloatValue(latency))
      ]),
      Some(operation_start_time),
      Some(operation_end_time),
      Some(trace_id),
      Some(span_id),
      Some(azimuth::Context::root())
    )
    
    azimuth::Logger::emit(logger, log_record)
    
    // 完成Span
    azimuth::Span::add_event(span, "operation.completed", Some([
      ("operation.id", azimuth::IntValue(operation_id)),
      ("correlation.id", azimuth::StringValue(correlation_id)),
      ("operation.latency", azimuth::FloatValue(latency))
    ]))
    
    azimuth::Span::set_status(span, azimuth::Ok)
    azimuth::Span::end(span)
  }
  
  // 等待数据导出
  azimuth::Clock::sleep(1000)
  
  // 执行一致性检查
  let consistency_results = azimuth::ConsistencyTestEnvironment::check_consistency(consistency_env)
  
  // 验证Trace-Metric一致性
  let trace_metric_consistency = consistency_results["trace.metric"]
  assert_true(azimuth::ConsistencyResult::is_consistent(trace_metric_consistency))
  
  // 验证每个操作都有对应的度量和Trace
  let trace_metric_correlations = azimuth::ConsistencyResult::get_correlations(trace_metric_consistency)
  for correlation in trace_metric_correlations {
    let trace_id = correlation["trace.id"]
    let operation_id = correlation["operation.id"]
    
    // 验证Trace存在
    let trace = azimuth::ConsistencyTestEnvironment::get_trace_by_id(consistency_env, trace_id)
    assert_true(trace.is_some())
    
    // 验证度量存在
    let metrics = azimuth::ConsistencyTestEnvironment::get_metrics_by_correlation(consistency_env, "operation.id", operation_id)
    assert_true(metrics.length() > 0)
  }
  
  // 验证Trace-Log一致性
  let trace_log_consistency = consistency_results["trace.log"]
  assert_true(azimuth::ConsistencyResult::is_consistent(trace_log_consistency))
  
  // 验证每个Trace都有对应的日志
  let trace_log_correlations = azimuth::ConsistencyResult::get_correlations(trace_log_consistency)
  for correlation in trace_log_correlations {
    let trace_id = correlation["trace.id"]
    let correlation_id = correlation["correlation.id"]
    
    // 验证Trace存在
    let trace = azimuth::ConsistencyTestEnvironment::get_trace_by_id(consistency_env, trace_id)
    assert_true(trace.is_some())
    
    // 验证日志存在
    let logs = azimuth::ConsistencyTestEnvironment::get_logs_by_correlation(consistency_env, "correlation.id", correlation_id)
    assert_true(logs.length() > 0)
  }
  
  // 验证Metric-Log一致性
  let metric_log_consistency = consistency_results["metric.log"]
  assert_true(azimuth::ConsistencyResult::is_consistent(metric_log_consistency))
  
  // 验证时间一致性
  let temporal_consistency = consistency_results["temporal"]
  assert_true(azimuth::ConsistencyResult::is_consistent(temporal_consistency))
  
  // 验证时间戳顺序
  for correlation_id in correlation_ids {
    let temporal_data = azimuth::ConsistencyTestEnvironment::get_temporal_data_by_correlation(consistency_env, "correlation.id", correlation_id)
    
    // 验证Span开始时间 <= 日志时间戳 <= Span结束时间
    for data in temporal_data {
      let span_start_time = data["span.start_time"]
      let log_timestamp = data["log.timestamp"]
      let span_end_time = data["span.end_time"]
      
      assert_true(span_start_time <= log_timestamp)
      assert_true(log_timestamp <= span_end_time)
    }
  }
  
  // 创建一致性性能度量
  let consistency_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "consistency-integration")
  let consistency_score_gauge = azimuth::Meter::create_gauge(consistency_meter, "consistency.score")
  let correlation_completeness_gauge = azimuth::Meter::create_gauge(consistency_meter, "correlation.completeness")
  let temporal_accuracy_gauge = azimuth::Meter::create_gauge(consistency_meter, "temporal.accuracy")
  
  // 计算一致性分数
  let overall_consistency_score = azimuth::ConsistencyTestEnvironment::calculate_consistency_score(consistency_env)
  azimuth::Gauge::record(consistency_score_gauge, overall_consistency_score)
  
  // 计算关联完整性
  let correlation_completeness = azimuth::ConsistencyTestEnvironment::calculate_correlation_completeness(consistency_env)
  azimuth::Gauge::record(correlation_completeness_gauge, correlation_completeness)
  
  // 计算时间准确性
  let temporal_accuracy = azimuth::ConsistencyTestEnvironment::calculate_temporal_accuracy(consistency_env)
  azimuth::Gauge::record(temporal_accuracy_gauge, temporal_accuracy)
  
  // 验证一致性性能指标
  assert_true(overall_consistency_score > 0.95)  // 一致性分数应该大于95%
  assert_true(correlation_completeness > 0.95)  // 关联完整性应该大于95%
  assert_true(temporal_accuracy > 0.95)  // 时间准确性应该大于95%
}