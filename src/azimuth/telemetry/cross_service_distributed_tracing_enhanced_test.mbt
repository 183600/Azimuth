// 增强型跨服务分布式追踪测试
// 测试微服务架构中的分布式追踪功能

test "distributed_trace_context_propagation" {
  // 测试分布式追踪上下文的传播
  
  // 创建初始追踪上下文
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let parent_span_id = "00f067aa0ba902b7"
  
  let trace_context = TraceContext::{
    trace_id: trace_id,
    span_id: parent_span_id,
    trace_flags: 1,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 模拟服务A调用服务B
  let service_a_headers = inject_trace_headers(trace_context, [
    ("x-request-id", "req-12345"),
    ("x-b3-traceid", trace_id),
    ("x-b3-spanid", parent_span_id),
    ("x-b3-parentspanid", "0000000000000000"),
    ("x-b3-sampled", "1")
  ])
  
  // 验证头部注入
  assert_eq(service_a_headers.length(), 5)
  assert_eq(service_a_headers.get("x-b3-traceid"), Some(trace_id))
  assert_eq(service_a_headers.get("x-b3-spanid"), Some(parent_span_id))
  
  // 服务B提取追踪上下文
  let extracted_context = extract_trace_context(service_a_headers)
  assert_eq(extracted_context.trace_id, trace_id)
  assert_eq(extracted_context.trace_flags, 1)
  
  // 服务B创建子span
  let child_span_id = "00f067aa0ba902b8"
  let child_context = create_child_context(extracted_context, child_span_id)
  
  assert_eq(child_context.trace_id, trace_id)
  assert_eq(child_context.span_id, child_span_id)
  assert_eq(child_context.trace_state, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
}

test "multi_service_trace_aggregation" {
  // 测试多服务追踪数据的聚合
  
  // 模拟三个服务的span数据
  let service_a_spans = [
    SpanData{
      service_name: "auth-service",
      operation_name: "authenticate_user",
      start_time: 1609459200000L,
      duration: 150L,
      status: "OK",
      tags: [("user.id", "12345"), ("auth.method", "jwt")]
    },
    SpanData{
      service_name: "auth-service", 
      operation_name: "validate_token",
      start_time: 1609459200100L,
      duration: 50L,
      status: "OK",
      tags: [("token.type", "access")]
    }
  ]
  
  let service_b_spans = [
    SpanData{
      service_name: "user-service",
      operation_name: "get_user_profile",
      start_time: 1609459200200L,
      duration: 200L,
      status: "OK", 
      tags: [("user.id", "12345"), ("cache.hit", "false")]
    }
  ]
  
  let service_c_spans = [
    SpanData{
      service_name: "notification-service",
      operation_name: "send_welcome_email",
      start_time: 1609459200450L,
      duration: 500L,
      status: "OK",
      tags: [("notification.type", "email"), ("user.id", "12345")]
    }
  ]
  
  // 聚合所有span数据
  let all_spans = [service_a_spans, service_b_spans, service_c_spans].flatten()
  assert_eq(all_spans.length(), 4)
  
  // 分析追踪链路
  let trace_analysis = analyze_distributed_trace(all_spans)
  assert_eq(trace_analysis.total_spans, 4)
  assert_eq(trace_analysis.involved_services.length(), 3)
  assert_eq(trace_analysis.total_duration, 550L) // 从第一个span开始到最后一个span结束
  assert_eq(trace_analysis.critical_path.length(), 4)
  
  // 验证服务调用顺序
  let service_order = trace_analysis.service_execution_order
  assert_eq(service_order[0], "auth-service")
  assert_eq(service_order[1], "user-service") 
  assert_eq(service_order[2], "notification-service")
}

test "trace_sampling_across_services" {
  // 测试跨服务的追踪采样策略
  
  // 测试不同采样率的配置
  let sampling_configs = [
    ("auth-service", SamplingConfig{ rate: 1.0, priority: "high" }),
    ("user-service", SamplingConfig{ rate: 0.5, priority: "medium" }),
    ("analytics-service", SamplingConfig{ rate: 0.1, priority: "low" }),
    ("notification-service", SamplingConfig{ rate: 0.8, priority: "medium" })
  ]
  
  // 模拟1000个请求
  let total_requests = 1000
  let sampled_requests = []
  
  for i = 0; i < total_requests; i = i + 1 {
    let trace_id = generate_trace_id(i)
    let sampling_decision = make_sampling_decision(trace_id, sampling_configs)
    sampled_requests.push(sampling_decision)
  }
  
  // 验证采样结果
  let auth_sampled = sampled_requests.filter(fn(r) { r.service == "auth-service" && r.sampled }).length()
  let user_sampled = sampled_requests.filter(fn(r) { r.service == "user-service" && r.sampled }).length()
  let analytics_sampled = sampled_requests.filter(fn(r) { r.service == "analytics-service" && r.sampled }).length()
  let notification_sampled = sampled_requests.filter(fn(r) { r.service == "notification-service" && r.sampled }).length()
  
  // 验证采样率接近配置值（允许5%的误差）
  assert_eq(auth_sampled, 1000) // 100%采样
  assert_eq(user_sampled >= 450 && user_sampled <= 550, true) // 约50%采样
  assert_eq(analytics_sampled >= 50 && analytics_sampled <= 150, true) // 约10%采样
  assert_eq(notification_sampled >= 750 && notification_sampled <= 850, true) // 约80%采样
}

test "trace_error_propagation" {
  // 测试追踪中错误信息的传播
  
  // 创建包含错误的追踪链
  let error_trace = TraceChain{
    trace_id: "error-trace-123",
    spans: [
      Span{
        service: "api-gateway",
        operation: "route_request",
        status: "OK",
        error: None
      },
      Span{
        service: "auth-service", 
        operation: "validate_token",
        status: "ERROR",
        error: Some(ErrorInfo{
          code: "TOKEN_EXPIRED",
          message: "JWT token has expired",
          stack_trace: "at validate_token (auth.js:45:12)"
        })
      },
      Span{
        service: "api-gateway",
        operation: "handle_auth_error", 
        status: "ERROR",
        error: Some(ErrorInfo{
          code: "AUTHENTICATION_FAILED",
          message: "User authentication failed",
          stack_trace: "at handle_auth_error (gateway.js:78:5)"
        })
      }
    ]
  }
  
  // 分析错误传播
  let error_analysis = analyze_error_propagation(error_trace)
  assert_eq(error_analysis.root_cause.service, "auth-service")
  assert_eq(error_analysis.root_cause.operation, "validate_token")
  assert_eq(error_analysis.root_cause.error_code, "TOKEN_EXPIRED")
  assert_eq(error_analysis.affected_services.length(), 2)
  assert_eq(error_analysis.error_chain.length(), 2)
  
  // 验证错误上下文保留
  let error_context = extract_error_context(error_trace)
  assert_eq(error_context.contains("TOKEN_EXPIRED"), true)
  assert_eq(error_context.contains("JWT token has expired"), true)
  assert_eq(error_context.contains("auth.js:45:12"), true)
}