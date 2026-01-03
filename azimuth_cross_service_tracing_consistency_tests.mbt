// Azimuth 跨服务追踪一致性测试用例
// 测试跨多个服务的分布式追踪的一致性和完整性

test "跨服务Trace上下文传播" {
  // 创建分布式追踪器
  let tracer = azimuth::DistributedTracer::new("order-service")
  
  // 创建根Span
  let root_span = tracer.start_span("process_order", azimuth::SpanKind::Server)
  let trace_context = root_span.get_context()
  
  // 验证Trace上下文
  assert_true(trace_context.trace_id.length() > 0)
  assert_true(trace_context.span_id.length() > 0)
  assert_true(trace_context.is_valid())
  
  // 模拟调用库存服务
  let inventory_tracer = azimuth::DistributedTracer::new("inventory-service")
  let inventory_context = trace_context.with_span_name("check_inventory")
  let inventory_span = inventory_tracer.start_span_from_context("check_inventory", inventory_context)
  
  // 验证Trace ID一致性
  assert_eq(inventory_span.get_context().trace_id, trace_context.trace_id)
  assert_eq(inventory_span.get_context().is_sampled(), trace_context.is_sampled())
  
  // 模拟调用支付服务
  let payment_tracer = azimuth::DistributedTracer::new("payment-service")
  let payment_context = trace_context.with_span_name("process_payment")
  let payment_span = payment_tracer.start_span_from_context("process_payment", payment_context)
  
  // 验证Trace ID一致性
  assert_eq(payment_span.get_context().trace_id, trace_context.trace_id)
  assert_eq(payment_span.get_context().is_sampled(), trace_context.is_sampled())
  
  // 结束所有Span
  inventory_span.end()
  payment_span.end()
  root_span.end()
  
  // 验证Span关系
  assert_eq(inventory_span.get_parent_span_id(), Some(root_span.get_context().span_id))
  assert_eq(payment_span.get_parent_span_id(), Some(root_span.get_context().span_id))
}

test "Baggage跨服务传播" {
  // 创建Baggage
  let baggage = azimuth::Baggage::new()
  baggage.set_entry("user.id", "12345")
  baggage.set_entry("request.id", "req-67890")
  baggage.set_entry("tenant.id", "tenant-abc")
  
  // 创建第一个服务并设置Baggage
  let user_service = azimuth::DistributedTracer::new("user-service")
  let user_span = user_service.start_span("authenticate_user", azimuth::SpanKind::Server)
  user_span.set_baggage(baggage)
  
  // 获取传播用的Headers
  let headers = user_span.inject_context_to_headers()
  
  // 验证Headers包含Baggage信息
  assert_true(headers.contains_key("traceparent"))
  assert_true(headers.contains_key("baggage"))
  
  // 在第二个服务中提取Baggage
  let order_service = azimuth::DistributedTracer::new("order-service")
  let extracted_context = order_service.extract_context_from_headers(headers)
  let order_span = order_service.start_span_from_context("create_order", extracted_context)
  
  // 验证Baggage传播
  let extracted_baggage = order_span.get_baggage()
  assert_eq(extracted_baggage.get_entry("user.id"), Some("12345"))
  assert_eq(extracted_baggage.get_entry("request.id"), Some("req-67890"))
  assert_eq(extracted_baggage.get_entry("tenant.id"), Some("tenant-abc"))
  
  // 添加新的Baggage项
  let updated_baggage = extracted_baggage.set_entry("order.id", "order-11111")
  order_span.set_baggage(updated_baggage)
  
  // 验证Baggage更新
  let final_baggage = order_span.get_baggage()
  assert_eq(final_baggage.get_entry("order.id"), Some("order-11111"))
  assert_eq(final_baggage.get_entry("user.id"), Some("12345")) // 原有项保持不变
  
  user_span.end()
  order_span.end()
}

test "跨服务Span链接" {
  // 创建第一个服务
  let api_gateway = azimuth::DistributedTracer::new("api-gateway")
  let gateway_span = api_gateway.start_span("handle_request", azimuth::SpanKind::Server)
  
  // 创建第二个服务的Span（异步调用）
  let notification_service = azimuth::DistributedTracer::new("notification-service")
  let notification_context = gateway_span.get_context().with_span_name("send_notification")
  let notification_span = notification_service.start_span_from_context("send_notification", notification_context)
  
  // 创建第三个服务的Span（同步调用）
  let user_service = azimuth::DistributedTracer::new("user-service")
  let user_context = gateway_span.get_context().with_span_name("get_user_profile")
  let user_span = user_service.start_span_from_context("get_user_profile", user_context)
  
  // 在用户服务中创建子Span
  let db_span = user_service.start_span("query_database", azimuth::SpanKind::Client)
  db_span.set_parent_span(user_span.get_context())
  
  // 设置Span关系
  notification_span.add_link(gateway_span.get_context())
  user_span.add_link(gateway_span.get_context())
  db_span.set_parent_span(user_span.get_context())
  
  // 验证Span链接关系
  assert_eq(notification_span.get_links().length(), 1)
  assert_eq(user_span.get_links().length(), 1)
  assert_eq(db_span.get_parent_span_id(), Some(user_span.get_context().span_id))
  
  // 结束所有Span
  db_span.end()
  user_span.end()
  notification_span.end()
  gateway_span.end()
  
  // 验证Span树结构
  let trace_tree = api_gateway.get_trace_tree(gateway_span.get_context().trace_id)
  assert_eq(trace_tree.root_span.span_id, gateway_span.get_context().span_id)
  assert_eq(trace_tree.child_spans.length(), 2)
  
  // 验证子Span
  let notification_child = trace_tree.child_spans.find(fn(span) { span.span_name == "send_notification" })
  let user_child = trace_tree.child_spans.find(fn(span) { span.span_name == "get_user_profile" })
  
  assert_true(notification_child.is_some())
  assert_true(user_child.is_some())
  
  // 验证用户服务的子Span
  match user_child {
    Some(user_span_info) => {
      assert_eq(user_span_info.child_spans.length(), 1)
      assert_eq(user_span_info.child_spans[0].span_name, "query_database")
    }
    None => assert_true(false)
  }
}

test "跨服务事件一致性" {
  // 创建多个服务的追踪器
  let services = [
    ("api-gateway", azimuth::DistributedTracer::new("api-gateway")),
    ("auth-service", azimuth::DistributedTracer::new("auth-service")),
    ("user-service", azimuth::DistributedTracer::new("user-service")),
    ("order-service", azimuth::DistributedTracer::new("order-service"))
  ]
  
  // 创建根Span
  let root_span = services[0].1.start_span("process_user_request", azimuth::SpanKind::Server)
  let trace_id = root_span.get_context().trace_id
  
  // 在每个服务中创建Span和事件
  let all_spans = []
  
  for (service_name, tracer) in services {
    let span_name = "service_operation_" + service_name
    let context = root_span.get_context().with_span_name(span_name)
    let span = tracer.start_span_from_context(span_name, context)
    
    // 添加事件
    span.add_event("service_started", [("service", service_name)])
    span.add_event("processing_request", [("trace_id", trace_id)])
    span.add_event("service_completed", [("service", service_name)])
    
    all_spans.push(span)
  }
  
  // 结束所有Span
  for span in all_spans {
    span.end()
  }
  root_span.end()
  
  // 验证事件一致性
  let trace_events = services[0].1.get_trace_events(trace_id)
  
  // 验证每个服务都有3个事件
  for (service_name, _) in services {
    let service_events = trace_events.filter(fn(event) { event.attributes.contains("service", service_name) })
    assert_eq(service_events.length(), 3)
    
    // 验证事件顺序
    assert_eq(service_events[0].name, "service_started")
    assert_eq(service_events[1].name, "processing_request")
    assert_eq(service_events[2].name, "service_completed")
  }
  
  // 验证所有事件都有相同的Trace ID
  for event in trace_events {
    assert_eq(event.attributes.get("trace_id"), Some(trace_id))
  }
}

test "跨服务错误传播" {
  // 创建服务链
  let api_gateway = azimuth::DistributedTracer::new("api-gateway")
  let auth_service = azimuth::DistributedTracer::new("auth-service")
  let user_service = azimuth::DistributedTracer::new("user-service")
  
  // 创建根Span
  let gateway_span = api_gateway.start_span("authenticate_and_get_user", azimuth::SpanKind::Server)
  
  // 在认证服务中模拟错误
  let auth_context = gateway_span.get_context().with_span_name("authenticate")
  let auth_span = auth_service.start_span_from_context("authenticate", auth_context)
  
  // 记录错误
  auth_span.set_status(azimuth::StatusCode::Error, Some("Invalid credentials"))
  auth_span.add_event("authentication_failed", [
    ("error.type", "authentication_error"),
    ("error.message", "Invalid username or password"),
    ("user.id", "unknown")
  ])
  auth_span.record_exception(azimuth::Exception::new("AuthenticationException", "Invalid credentials"))
  
  // 在用户服务中模拟超时
  let user_context = gateway_span.get_context().with_span_name("get_user_profile")
  let user_span = user_service.start_span_from_context("get_user_profile", user_context)
  
  // 记录超时错误
  user_span.set_status(azimuth::StatusCode::Error, Some("Request timeout"))
  user_span.add_event("request_timeout", [
    ("error.type", "timeout_error"),
    ("timeout.ms", "5000"),
    ("service", "user-service")
  ])
  
  // 结束Span
  auth_span.end()
  user_span.end()
  gateway_span.set_status(azimuth::StatusCode::Error, Some("Authentication failed"))
  gateway_span.end()
  
  // 验证错误传播
  let trace_summary = api_gateway.get_trace_summary(gateway_span.get_context().trace_id)
  
  // 验证错误统计
  assert_eq(trace_summary.total_spans, 3)
  assert_eq(trace_summary.error_spans, 3)
  assert_eq(trace_summary.root_span_status, azimuth::StatusCode::Error)
  
  // 验证错误类型
  let auth_errors = trace_summary.errors.filter(fn(error) { error.span_name == "authenticate" })
  let user_errors = trace_summary.errors.filter(fn(error) { error.span_name == "get_user_profile" })
  
  assert_eq(auth_errors.length(), 1)
  assert_eq(user_errors.length(), 1)
  
  // 验证错误详情
  assert_eq(auth_errors[0].error_type, "AuthenticationException")
  assert_eq(auth_errors[0].message, "Invalid credentials")
  
  assert_eq(user_errors[0].error_type, "timeout_error")
  assert_true(user_errors[0].message.contains("timeout"))
}

test "跨服务性能一致性" {
  // 创建性能追踪器
  let services = [
    ("frontend", azimuth::DistributedTracer::new("frontend-service")),
    ("backend", azimuth::DistributedTracer::new("backend-service")),
    ("database", azimuth::DistributedTracer::new("database-service"))
  ]
  
  // 创建根Span
  let root_span = services[0].1.start_span("user_request_flow", azimuth::SpanKind::Server)
  let start_time = azimuth::TimeUtil::current_time_micros()
  
  // 在每个服务中创建Span并模拟处理时间
  let all_spans = []
  let expected_durations = [100, 200, 150] // 微秒
  
  for i in 0..services.length() - 1 {
    let (service_name, tracer) = services[i]
    let span_name = "process_" + service_name
    let context = root_span.get_context().with_span_name(span_name)
    let span = tracer.start_span_from_context(span_name, context)
    
    // 模拟处理时间
    azimuth::TimeUtil::sleep_micros(expected_durations[i])
    
    // 设置性能指标
    span.set_attribute("processing_time_us", expected_durations[i].to_string())
    span.set_attribute("service_name", service_name)
    
    all_spans.push(span)
  }
  
  // 结束所有Span
  for span in all_spans {
    span.end()
  }
  root_span.end()
  
  let total_duration = azimuth::TimeUtil::current_time_micros() - start_time
  
  // 验证性能一致性
  let performance_metrics = services[0].1.get_performance_metrics(root_span.get_context().trace_id)
  
  // 验证总时间
  assert_eq(performance_metrics.total_duration_us, total_duration)
  
  // 验证各服务的处理时间
  for i in 0..services.length() - 1 {
    let (service_name, _) = services[i]
    let service_metrics = performance_metrics.service_metrics.get(service_name)
    
    match service_metrics {
      Some(metrics) => {
        assert_eq(metrics.duration_us, expected_durations[i])
        assert_eq(metrics.service_name, service_name)
      }
      None => assert_true(false)
    }
  }
  
  // 验证时间顺序
  let sorted_spans = performance_metrics.spans.sort_by(fn(a, b) { a.start_time_us - b.start_time_us })
  for i in 1..sorted_spans.length() - 1 {
    assert_true(sorted_spans[i].start_time_us >= sorted_spans[i-1].start_time_us)
  }
}

test "跨服务采样一致性" {
  // 创建采样配置
  let sampling_config = azimuth::SamplingConfig {
    sampling_type: azimuth::SamplingType::TraceIdRatio,
    sampling_rate: 0.5, // 50%采样率
    parent_based: true
  }
  
  // 创建多个服务的追踪器（使用相同的采样配置）
  let services = []
  let service_names = ["service-a", "service-b", "service-c", "service-d"]
  
  for name in service_names {
    let tracer = azimuth::DistributedTracer::new_with_sampling(name, sampling_config)
    services.push((name, tracer))
  }
  
  // 创建多个Trace并验证采样一致性
  let sampled_traces = []
  let not_sampled_traces = []
  
  for i in 0..=19 {
    // 创建根Span
    let root_span = services[0].1.start_span("trace_" + i.to_string(), azimuth::SpanKind::Server)
    let is_sampled = root_span.get_context().is_sampled()
    
    // 在所有服务中创建Span
    let all_spans = [root_span]
    
    for j in 1..services.length() - 1 {
      let (service_name, tracer) = services[j]
      let span_name = "operation_" + service_name
      let context = root_span.get_context().with_span_name(span_name)
      let span = tracer.start_span_from_context(span_name, context)
      all_spans.push(span)
    }
    
    // 验证采样一致性
    for span in all_spans {
      assert_eq(span.get_context().is_sampled(), is_sampled)
    }
    
    // 记录采样结果
    if is_sampled {
      sampled_traces.push(root_span.get_context().trace_id)
    } else {
      not_sampled_traces.push(root_span.get_context().trace_id)
    }
    
    // 结束所有Span
    for span in all_spans {
      span.end()
    }
  }
  
  // 验证采样率（允许一定误差）
  let total_traces = sampled_traces.length() + not_sampled_traces.length()
  let actual_sampling_rate = sampled_traces.length().to_float() / total_traces.to_float()
  
  assert_true(actual_sampling_rate >= 0.3 && actual_sampling_rate <= 0.7) // 允许20%误差
  
  // 验证采样的Trace在所有服务中都被记录
  for trace_id in sampled_traces {
    for (service_name, tracer) in services {
      let trace_data = tracer.get_trace_data(trace_id)
      assert_true(trace_data.is_some())
      
      match trace_data {
        Some(data) => {
          assert_eq(data.trace_id, trace_id)
          assert_eq(data.spans.length(), services.length())
        }
        None => assert_true(false)
      }
    }
  }
  
  // 验证未采样的Trace在所有服务中都不被记录
  for trace_id in not_sampled_traces {
    for (service_name, tracer) in services {
      let trace_data = tracer.get_trace_data(trace_id)
      assert_true(trace_data.is_none())
    }
  }
}

test "跨服务标签一致性" {
  // 创建标签策略
  let label_policy = azimuth::LabelPolicy {
    required_labels: ["service.name", "service.version", "environment"],
    optional_labels: ["region", "zone", "instance.id"],
    label_format: azimuth::LabelFormat::SnakeCase
  }
  
  // 创建多个服务的追踪器
  let services = []
  let service_configs = [
    ("api-gateway", "1.2.0", "production", "us-east-1", "us-east-1a", "gw-001"),
    ("auth-service", "2.1.3", "production", "us-east-1", "us-east-1b", "auth-001"),
    ("user-service", "3.0.1", "production", "us-west-2", "us-west-2a", "user-001")
  ]
  
  for (name, version, env, region, zone, instance) in service_configs {
    let tracer = azimuth::DistributedTracer::new_with_labels(name, label_policy)
    tracer.set_default_labels([
      ("service.name", name),
      ("service.version", version),
      ("environment", env),
      ("region", region),
      ("zone", zone),
      ("instance.id", instance)
    ])
    services.push((name, tracer))
  }
  
  // 创建根Span
  let root_span = services[0].1.start_span("user_request", azimuth::SpanKind::Server)
  
  // 在每个服务中创建Span
  let all_spans = [root_span]
  
  for i in 1..services.length() - 1 {
    let (service_name, tracer) = services[i]
    let span_name = "process_" + service_name
    let context = root_span.get_context().with_span_name(span_name)
    let span = tracer.start_span_from_context(span_name, context)
    
    // 添加特定于服务的标签
    span.set_attribute("operation.type", "business_logic")
    span.set_attribute("trace.type", "user_flow")
    
    all_spans.push(span)
  }
  
  // 结束所有Span
  for span in all_spans {
    span.end()
  }
  
  // 验证标签一致性
  let trace_data = services[0].1.get_trace_data(root_span.get_context().trace_id)
  
  match trace_data {
    Some(data) => {
      // 验证每个Span都有必需的标签
      for span in data.spans {
        assert_true(span.attributes.contains_key("service.name"))
        assert_true(span.attributes.contains_key("service.version"))
        assert_true(span.attributes.contains_key("environment"))
        
        // 验证标签格式
        for (key, _) in span.attributes {
          assert_true(key.contains("_")) // SnakeCase格式
        }
      }
      
      // 验证服务特定的标签
      let api_gateway_span = data.spans.find(fn(span) { 
        match span.attributes.get("service.name") {
          Some(azimuth::AttributeValue::StringValue(name)) => name == "api-gateway"
          _ => false
        }
      })
      
      match api_gateway_span {
        Some(span) => {
          match span.attributes.get("instance.id") {
            Some(azimuth::AttributeValue::StringValue(instance_id)) => {
              assert_eq(instance_id, "gw-001")
            }
            _ => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}