// Azimuth 高级遥测测试用例
// 专注于遥测系统的高级功能、边界情况和复杂场景

// 测试1: 多层级分布式追踪和Span关联
test "多层级分布式追踪和Span关联测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "multi.layer.tracer")
  
  // 创建API网关span
  let gateway_span = Tracer::start_span(tracer, "api.gateway.request")
  Span::set_attribute(gateway_span, "http.method", StringValue("POST"))
  Span::set_attribute(gateway_span, "http.route", StringValue("/api/v2/orders"))
  let gateway_context = Span::context(gateway_span)
  
  // 创建认证服务span
  let auth_span = Tracer::start_span_with_context(tracer, "auth.service.validate", gateway_context)
  Span::set_attribute(auth_span, "auth.token.type", StringValue("JWT"))
  Span::set_attribute(auth_span, "auth.user.id", StringValue("user-789"))
  Span::set_status(auth_span, Ok)
  Span::end(auth_span)
  
  // 创建订单服务span
  let order_span = Tracer::start_span_with_context(tracer, "order.service.process", gateway_context)
  let order_context = Span::context(order_span)
  
  // 创建库存检查span
  let inventory_span = Tracer::start_span_with_context(tracer, "inventory.check", order_context)
  Span::set_attribute(inventory_span, "product.id", StringValue("prod-123"))
  Span::set_attribute(inventory_span, "quantity.requested", IntValue(5))
  Span::set_attribute(inventory_span, "quantity.available", IntValue(10))
  Span::set_status(inventory_span, Ok)
  Span::end(inventory_span)
  
  // 创建支付处理span
  let payment_span = Tracer::start_span_with_context(tracer, "payment.process", order_context)
  Span::set_attribute(payment_span, "payment.method", StringValue("credit_card"))
  Span::set_attribute(payment_span, "payment.amount", FloatValue(99.99))
  Span::set_attribute(payment_span, "payment.currency", StringValue("USD"))
  
  // 模拟支付重试
  for i in 1..=3 {
    let retry_span = Tracer::start_span_with_context(tracer, "payment.retry", Span::context(payment_span))
    Span::set_attribute(retry_span, "retry.attempt", IntValue(i))
    
    if i == 3 {
      Span::set_status(retry_span, Ok)
    } else {
      Span::set_status(retry_span, Error)
      Span::add_event(retry_span, "payment.failed", None)
    }
    Span::end(retry_span)
  }
  
  Span::set_status(payment_span, Ok)
  Span::end(payment_span)
  Span::set_status(order_span, Ok)
  Span::end(order_span)
  Span::set_status(gateway_span, Ok)
  Span::end(gateway_span)
  
  // 验证所有span状态
  assert_eq(Span::status(gateway_span), Ok)
  assert_eq(Span::status(order_span), Ok)
  assert_eq(Span::status(payment_span), Ok)
}

// 测试2: 高维时间序列数据聚合和分析
test "高维时间序列数据聚合和分析测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "time.series.analytics")
  
  // 创建多维指标
  let request_histogram = Meter::create_histogram(meter, "http.request.duration")
  
  // 模拟不同服务、端点、状态码的请求
  let services = ["auth", "payment", "inventory", "notification"]
  let endpoints = ["/login", "/pay", "/check", "/send"]
  let status_codes = [200, 400, 401, 500]
  
  for i in 0..services.length() - 1 {
    for j in 0..endpoints.length() - 1 {
      for k in 0..status_codes.length() - 1 {
        let attrs = Attributes::new()
        Attributes::set(attrs, "service.name", StringValue(services[i]))
        Attributes::set(attrs, "endpoint", StringValue(endpoints[j]))
        Attributes::set(attrs, "status.code", IntValue(status_codes[k]))
        
        // 记录不同延迟值
        let latency = 0.01 + (status_codes[k] * 0.001) + (i * 0.005)
        Histogram::record_with_attributes(request_histogram, latency, attrs)
      }
    }
  }
  
  // 创建聚合指标
  let p99_latency = Meter::create_gauge(meter, "http.request.duration.p99")
  let error_rate = Meter::create_gauge(meter, "http.request.error_rate")
  let throughput = Meter::create_counter(meter, "http.request.throughput")
  
  // 计算并设置聚合值
  Gauge::set(p99_latency, 0.125)
  Gauge::set(error_rate, 0.035)
  Counter::add(throughput, 1250.0)
  
  // 验证聚合结果
  assert_eq(Gauge::value(p99_latency), 0.125)
  assert_eq(Gauge::value(error_rate), 0.035)
  assert_eq(Counter::value(throughput), 1250.0)
}

// 测试3: 异步遥测数据流处理和背压控制
test "异步遥测数据流处理和背压控制测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "async.stream.processor")
  
  // 创建异步处理管道
  let stream_processor = AsyncStreamProcessor::new(1000) // 缓冲区大小1000
  
  // 模拟高并发span创建
  for i in 1..=500 {
    let async_span = Tracer::start_span(tracer, "async.operation." + i.to_string())
    Span::set_attribute(async_span, "operation.id", IntValue(i))
    Span::set_attribute(async_span, "async.batch", IntValue((i / 50) + 1))
    
    // 异步处理span
    AsyncStreamProcessor::process(stream_processor, async_span, fn(span) {
      // 模拟处理延迟
      let process_time = 0.001 + (i % 10 * 0.0001)
      Span::set_attribute(span, "process.time", FloatValue(process_time))
      Span::set_status(span, Ok)
    })
  }
  
  // 等待所有异步处理完成
  AsyncStreamProcessor::flush(stream_processor)
  
  // 验证背压控制
  let buffer_usage = AsyncStreamProcessor::buffer_usage(stream_processor)
  let dropped_count = AsyncStreamProcessor::dropped_count(stream_processor)
  
  assert_true(buffer_usage <= 0.95) // 缓冲区使用率不超过95%
  assert_eq(dropped_count, 0) // 没有数据丢失
}

// 测试4: 智能采样策略和动态采样率调整
test "智能采样策略和动态采样率调整测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "adaptive.sampling")
  
  // 创建自适应采样器
  let adaptive_sampler = AdaptiveSampler::new(0.1) // 初始采样率10%
  
  // 模拟不同类型的请求
  let critical_requests = 50
  let normal_requests = 1000
  let error_requests = 25
  
  // 处理关键请求（应该100%采样）
  for i in 1..=critical_requests {
    let critical_span = Tracer::start_span(tracer, "critical.request")
    Span::set_attribute(critical_span, "request.type", StringValue("critical"))
    Span::set_attribute(critical_span, "request.priority", StringValue("high"))
    
    let should_sample = AdaptiveSampler::should_sample(adaptive_sampler, Span::context(critical_span))
    assert_true(should_sample) // 关键请求总是被采样
    Span::end(critical_span)
  }
  
  // 处理普通请求
  let sampled_normal = 0
  for i in 1..=normal_requests {
    let normal_span = Tracer::start_span(tracer, "normal.request")
    Span::set_attribute(normal_span, "request.type", StringValue("normal"))
    Span::set_attribute(normal_span, "request.priority", StringValue("medium"))
    
    if AdaptiveSampler::should_sample(adaptive_sampler, Span::context(normal_span)) {
      sampled_normal = sampled_normal + 1
    }
    Span::end(normal_span)
  }
  
  // 处理错误请求（应该提高采样率）
  for i in 1..=error_requests {
    let error_span = Tracer::start_span(tracer, "error.request")
    Span::set_attribute(error_span, "request.type", StringValue("error"))
    Span::set_attribute(error_span, "error.severity", StringValue("high"))
    
    let should_sample = AdaptiveSampler::should_sample(adaptive_sampler, Span::context(error_span))
    assert_true(should_sample) // 错误请求总是被采样
    Span::set_status(error_span, Error)
    Span::end(error_span)
  }
  
  // 动态调整采样率
  AdaptiveSampler::adjust_rate(adaptive_sampler, 0.2) // 提高到20%
  
  // 验证采样结果
  let normal_sample_rate = sampled_normal.to_float() / normal_requests.to_float()
  assert_true(normal_sample_rate >= 0.08 && normal_sample_rate <= 0.12) // 接近初始采样率
  assert_eq(AdaptiveSampler::get_current_rate(adaptive_sampler), 0.2) // 验证采样率已调整
}

// 测试5: 跨服务上下文传播和一致性验证
test "跨服务上下文传播和一致性验证测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross.service.propagation")
  
  // 服务A：创建根追踪
  let service_a_span = Tracer::start_span(tracer, "service.a.operation")
  Span::set_attribute(service_a_span, "service.name", StringValue("service-a"))
  Span::set_attribute(service_a_span, "service.version", StringValue("1.0.0"))
  
  // 创建传播器
  let propagator = W3CTraceContextPropagator::new()
  let carrier = TextMapCarrier::new()
  
  // 注入服务A的上下文
  let context_a = Span::context(service_a_span)
  CompositePropagator::inject(propagator, context_a, carrier)
  
  // 服务B：提取上下文并继续追踪
  let context_b = CompositePropagator::extract(propagator, carrier)
  let service_b_span = Tracer::start_span_with_context(tracer, "service.b.operation", context_b)
  Span::set_attribute(service_b_span, "service.name", StringValue("service-b"))
  Span::set_attribute(service_b_span, "service.version", StringValue("2.1.0"))
  
  // 添加行李项
  let baggage = Baggage::new()
  Baggage::set(baggage, "user.id", "user-12345")
  Baggage::set(baggage, "request.id", "req-abcdef")
  Baggage::set(baggage, "tenant.id", "tenant-789")
  
  let context_with_baggage = Context::with_baggage(context_b, baggage)
  
  // 服务C：继续追踪
  let service_c_span = Tracer::start_span_with_context(tracer, "service.c.operation", context_with_baggage)
  Span::set_attribute(service_c_span, "service.name", StringValue("service-c"))
  Span::set_attribute(service_c_span, "service.version", StringValue("3.2.1"))
  
  // 验证上下文一致性
  let trace_id_a = SpanContext::trace_id(Span::context(service_a_span))
  let trace_id_b = SpanContext::trace_id(Span::context(service_b_span))
  let trace_id_c = SpanContext::trace_id(Span::context(service_c_span))
  
  assert_eq(trace_id_a, trace_id_b)
  assert_eq(trace_id_b, trace_id_c)
  
  // 验证行李项传播
  let user_id = Baggage::get(baggage, "user.id")
  let request_id = Baggage::get(baggage, "request.id")
  let tenant_id = Baggage::get(baggage, "tenant.id")
  
  assert_eq(user_id, Some("user-12345"))
  assert_eq(request_id, Some("req-abcdef"))
  assert_eq(tenant_id, Some("tenant-789"))
  
  Span::end(service_c_span)
  Span::end(service_b_span)
  Span::end(service_a_span)
}

// 测试6: 遥测数据压缩和批量传输优化
test "遥测数据压缩和批量传输优化测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "compression.optimization")
  
  // 创建批量导出器
  let batch_exporter = BatchExporter::new(100, 5000) // 批量大小100，超时5秒
  
  // 生成大量span数据
  for i in 1..=200 {
    let batch_span = Tracer::start_span(tracer, "batch.operation." + i.to_string())
    
    // 添加大量属性
    for j in 1..=20 {
      Span::set_attribute(batch_span, "attr." + j.to_string(), StringValue("value." + j.to_string()))
    }
    
    // 添加事件
    for k in 1..=5 {
      Span::add_event(batch_span, "event." + k.to_string(), None)
    }
    
    BatchExporter::add_span(batch_exporter, batch_span)
    Span::end(batch_span)
  }
  
  // 启用压缩
  BatchExporter::enable_compression(batch_exporter, "gzip")
  
  // 执行批量导出
  let export_result = BatchExporter::flush(batch_exporter)
  
  // 验证压缩效果
  let original_size = BatchExporter::get_original_size(batch_exporter)
  let compressed_size = BatchExporter::get_compressed_size(batch_exporter)
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  
  assert_true(compression_ratio < 0.7) // 压缩率至少30%
  assert_eq(export_result, ExportSuccess)
}

// 测试7: 实时异常检测和自动告警触发
test "实时异常检测和自动告警触发测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "anomaly.detection")
  
  // 创建异常检测器
  let anomaly_detector = AnomalyDetector::new()
  
  // 设置异常阈值
  AnomalyDetector::set_threshold(anomaly_detector, "error_rate", 0.05) // 错误率5%
  AnomalyDetector::set_threshold(anomaly_detector, "latency_p99", 0.5) // P99延迟500ms
  AnomalyDetector::set_threshold(anomaly_detector, "throughput_drop", 0.2) // 吞吐量下降20%
  
  // 创建指标
  let error_counter = Meter::create_counter(meter, "errors.total")
  let request_counter = Meter::create_counter(meter, "requests.total")
  let latency_histogram = Meter::create_histogram(meter, "request.duration")
  
  // 模拟正常情况
  for i in 1..=1000 {
    Counter::add(request_counter, 1.0)
    Histogram::record(latency_histogram, 0.1 + (i % 50 * 0.001))
  }
  Counter::add(error_counter, 20.0) // 2%错误率
  
  // 检测异常
  let normal_check = AnomalyDetector::analyze(anomaly_detector)
  assert_false(normal_check.has_anomaly)
  
  // 模拟异常情况：错误率飙升
  for i in 1..=100 {
    Counter::add(request_counter, 1.0)
    Counter::add(error_counter, 1.0) // 50%错误率
    Histogram::record(latency_histogram, 0.8) // 高延迟
  }
  
  // 检测异常
  let anomaly_check = AnomalyDetector::analyze(anomaly_detector)
  assert_true(anomaly_check.has_anomaly)
  
  // 验证告警触发
  let alerts = AnomalyDetector::get_alerts(anomaly_detector)
  assert_true(alerts.length() > 0)
  
  // 验证告警内容
  let error_alert = alerts.find(fn(alert) { alert.metric == "error_rate" })
  assert_true(error_alert != None)
  
  match error_alert {
    Some(alert) => {
      assert_true(alert.current_value > alert.threshold)
      assert_eq(alert.severity, "high")
    }
    None => assert_true(false)
  }
}

// 测试8: 多租户资源隔离和配额管理
test "多租户资源隔离和配额管理测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "multi.tenant.isolation")
  
  // 创建租户管理器
  let tenant_manager = TenantManager::new()
  
  // 配置租户A：企业版，高配额
  TenantManager::configure_tenant(tenant_manager, "tenant-a", {
    "plan": "enterprise",
    "span_quota": 1000000,
    "metric_quota": 5000000,
    "log_quota": 2000000,
    "sampling_rate": 1.0
  })
  
  // 配置租户B：标准版，中等配额
  TenantManager::configure_tenant(tenant_manager, "tenant-b", {
    "plan": "standard",
    "span_quota": 100000,
    "metric_quota": 500000,
    "log_quota": 200000,
    "sampling_rate": 0.1
  })
  
  // 配置租户C：免费版，低配额
  TenantManager::configure_tenant(tenant_manager, "tenant-c", {
    "plan": "free",
    "span_quota": 10000,
    "metric_quota": 50000,
    "log_quota": 20000,
    "sampling_rate": 0.01
  })
  
  // 模拟租户A的高负载
  for i in 1..=5000 {
    let tenant_a_span = Tracer::start_span_with_tenant(tracer, "tenant.a.operation", "tenant-a")
    Span::set_attribute(tenant_a_span, "tenant.id", StringValue("tenant-a"))
    Span::set_attribute(tenant_a_span, "request.id", IntValue(i))
    Span::end(tenant_a_span)
  }
  
  // 模拟租户B的中等负载
  for i in 1..=500 {
    let tenant_b_span = Tracer::start_span_with_tenant(tracer, "tenant.b.operation", "tenant-b")
    Span::set_attribute(tenant_b_span, "tenant.id", StringValue("tenant-b"))
    Span::set_attribute(tenant_b_span, "request.id", IntValue(i))
    Span::end(tenant_b_span)
  }
  
  // 模拟租户C的低负载
  for i in 1..=50 {
    let tenant_c_span = Tracer::start_span_with_tenant(tracer, "tenant.c.operation", "tenant-c")
    Span::set_attribute(tenant_c_span, "tenant.id", StringValue("tenant-c"))
    Span::set_attribute(tenant_c_span, "request.id", IntValue(i))
    Span::end(tenant_c_span)
  }
  
  // 验证资源使用情况
  let tenant_a_usage = TenantManager::get_usage(tenant_manager, "tenant-a")
  let tenant_b_usage = TenantManager::get_usage(tenant_manager, "tenant-b")
  let tenant_c_usage = TenantManager::get_usage(tenant_manager, "tenant-c")
  
  assert_eq(tenant_a_usage.spans_created, 5000)
  assert_eq(tenant_b_usage.spans_created, 500)
  assert_eq(tenant_c_usage.spans_created, 50)
  
  // 验证配额限制
  assert_true(tenant_a_usage.spans_created <= tenant_a_usage.span_quota)
  assert_true(tenant_b_usage.spans_created <= tenant_b_usage.span_quota)
  assert_true(tenant_c_usage.spans_created <= tenant_c_usage.span_quota)
  
  // 验证租户隔离
  let tenant_a_spans = TenantManager::get_spans(tenant_manager, "tenant-a")
  let tenant_b_spans = TenantManager::get_spans(tenant_manager, "tenant-b")
  let tenant_c_spans = TenantManager::get_spans(tenant_manager, "tenant-c")
  
  assert_true(tenant_a_spans.length() == 5000)
  assert_true(tenant_b_spans.length() == 500)
  assert_true(tenant_c_spans.length() == 50)
}

// 测试9: 遥测数据生命周期管理和自动清理
test "遥测数据生命周期管理和自动清理测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle.management")
  
  // 创建生命周期管理器
  let lifecycle_manager = LifecycleManager::new()
  
  // 配置保留策略
  LifecycleManager::set_retention_policy(lifecycle_manager, {
    "spans": {
      "hot_ttl": 3600,      // 热数据1小时
      "warm_ttl": 86400,    // 温数据1天
      "cold_ttl": 2592000,  // 冷数据30天
      "max_size": 1000000   // 最大100万条
    },
    "metrics": {
      "hot_ttl": 7200,      // 热数据2小时
      "warm_ttl": 172800,   // 温数据2天
      "cold_ttl": 7776000,  // 冷数据90天
      "max_size": 5000000   // 最大500万条
    }
  })
  
  // 创建不同时间点的数据
  let current_time = Time::now()
  
  // 创建热数据（最近1小时）
  for i in 1..=100 {
    let hot_span = Tracer::start_span(tracer, "hot.data." + i.to_string())
    Span::set_attribute(hot_span, "data.temperature", StringValue("hot"))
    Span::set_attribute(hot_span, "created_at", TimeValue(current_time - (i * 60))) // 1-100分钟前
    LifecycleManager::add_span(lifecycle_manager, hot_span)
    Span::end(hot_span)
  }
  
  // 创建温数据（1-24小时前）
  for i in 1..=50 {
    let warm_span = Tracer::start_span(tracer, "warm.data." + i.to_string())
    Span::set_attribute(warm_span, "data.temperature", StringValue("warm"))
    Span::set_attribute(warm_span, "created_at", TimeValue(current_time - (3600 + i * 1800))) // 1-24小时前
    LifecycleManager::add_span(lifecycle_manager, warm_span)
    Span::end(warm_span)
  }
  
  // 创建冷数据（1-30天前）
  for i in 1..=25 {
    let cold_span = Tracer::start_span(tracer, "cold.data." + i.to_string())
    Span::set_attribute(cold_span, "data.temperature", StringValue("cold"))
    Span::set_attribute(cold_span, "created_at", TimeValue(current_time - (86400 + i * 86400))) // 1-25天前
    LifecycleManager::add_span(lifecycle_manager, cold_span)
    Span::end(cold_span)
  }
  
  // 创建过期数据（超过30天）
  for i in 1..=10 {
    let expired_span = Tracer::start_span(tracer, "expired.data." + i.to_string())
    Span::set_attribute(expired_span, "data.temperature", StringValue("expired"))
    Span::set_attribute(expired_span, "created_at", TimeValue(current_time - (2592000 + i * 86400))) // 30-40天前
    LifecycleManager::add_span(lifecycle_manager, expired_span)
    Span::end(expired_span)
  }
  
  // 执行生命周期清理
  let cleanup_result = LifecycleManager::cleanup(lifecycle_manager)
  
  // 验证清理结果
  assert_eq(cleanup_result.hot_data_count, 100)
  assert_eq(cleanup_result.warm_data_count, 50)
  assert_eq(cleanup_result.cold_data_count, 25)
  assert_eq(cleanup_result.expired_data_count, 10)
  assert_eq(cleanup_result.cleaned_data_count, 10) // 只有过期数据被清理
  
  // 验证数据存储状态
  let storage_stats = LifecycleManager::get_storage_stats(lifecycle_manager)
  assert_eq(storage_stats.total_spans, 175) // 100 + 50 + 25
  assert_eq(storage_stats.hot_spans, 100)
  assert_eq(storage_stats.warm_spans, 50)
  assert_eq(storage_stats.cold_spans, 25)
  assert_eq(storage_stats.expired_spans, 0)
}