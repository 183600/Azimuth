// Azimuth 分布式追踪一致性测试
// 专注于测试分布式系统中追踪数据的一致性和完整性

// 测试1: 跨服务追踪链完整性
test "跨服务追踪链完整性验证" {
  // 定义追踪链节点类型
  type TraceNode = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    service_name: String,
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  // 定义追踪链类型
  type TraceChain = {
    trace_id: String,
    root_span: TraceNode,
    child_spans: Array[TraceNode],
    services_involved: Array[String]
  }
  
  // 创建追踪链验证器
  let validate_trace_chain = fn(chain: TraceChain) {
    let errors = []
    let warnings = []
    
    // 验证根span
    if chain.root_span.parent_span_id.is_some() {
      errors = errors.push("根span不应该有父span ID")
    }
    
    if chain.root_span.trace_id != chain.trace_id {
      errors = errors.push("根span的trace_id与链的trace_id不匹配")
    }
    
    // 验证子span的父子关系
    let span_map = Map::empty()
    let _ = Map::insert(span_map, chain.root_span.span_id, chain.root_span)
    
    for child in chain.child_spans {
      let _ = Map::insert(span_map, child.span_id, child)
      
      if child.trace_id != chain.trace_id {
        errors = errors.push("子span的trace_id与链的trace_id不匹配: " + child.span_id)
      }
      
      match child.parent_span_id {
        Some(parent_id) => {
          if not Map::contains_key(span_map, parent_id) {
            errors = errors.push("子span的父span不存在: " + child.span_id + " -> " + parent_id)
          }
        }
        None => {
          warnings = warnings.push("非根span缺少父span ID: " + child.span_id)
        }
      }
    }
    
    // 验证时间顺序
    let all_spans = [chain.root_span] + chain.child_spans
    for span in all_spans {
      if span.end_time < span.start_time {
        errors = errors.push("span的结束时间早于开始时间: " + span.span_id)
      }
      
      match span.parent_span_id {
        Some(parent_id) => {
          match Map::get(span_map, parent_id) {
            Some(parent_span) => {
              if span.start_time < parent_span.start_time {
                errors = errors.push("子span的开始时间早于父span: " + span.span_id + " -> " + parent_id)
              }
            }
            None => {}  // 已在上面的检查中处理
          }
        }
        None => {}
      }
    }
    
    // 验证服务一致性
    for service in chain.services_involved {
      let service_spans = all_spans.filter(fn(span) { span.service_name == service })
      if service_spans.length() == 0 {
        warnings = warnings.push("服务列表中包含无对应span的服务: " + service)
      }
    }
    
    {
      is_valid: errors.length() == 0,
      errors: errors,
      warnings: warnings,
      span_count: all_spans.length(),
      service_count: chain.services_involved.length()
    }
  }
  
  // 创建测试追踪链
  let create_test_trace_chain = fn() {
    let trace_id = "trace-" + UUID::v4()
    let root_span = {
      trace_id: trace_id,
      span_id: "span-root",
      parent_span_id: None,
      service_name: "api-gateway",
      operation_name: "handle_request",
      start_time: 1000,
      end_time: 1500,
      status: "ok",
      attributes: [
        ("http.method", "GET"),
        ("http.url", "/api/orders"),
        ("user.id", "user-123")
      ]
    }
    
    let auth_span = {
      trace_id: trace_id,
      span_id: "span-auth",
      parent_span_id: Some("span-root"),
      service_name: "auth-service",
      operation_name: "authenticate_user",
      start_time: 1050,
      end_time: 1200,
      status: "ok",
      attributes: [
        ("auth.method", "jwt"),
        ("auth.result", "success")
      ]
    }
    
    let order_span = {
      trace_id: trace_id,
      span_id: "span-order",
      parent_span_id: Some("span-root"),
      service_name: "order-service",
      operation_name: "get_orders",
      start_time: 1250,
      end_time: 1400,
      status: "ok",
      attributes: [
        ("order.count", "5"),
        ("order.status", "active")
      ]
    }
    
    let db_span = {
      trace_id: trace_id,
      span_id: "span-db",
      parent_span_id: Some("span-order"),
      service_name: "database",
      operation_name: "query_orders",
      start_time: 1300,
      end_time: 1350,
      status: "ok",
      attributes: [
        ("db.type", "postgresql"),
        ("db.query", "SELECT * FROM orders WHERE user_id = ?")
      ]
    }
    
    {
      trace_id: trace_id,
      root_span: root_span,
      child_spans: [auth_span, order_span, db_span],
      services_involved: ["api-gateway", "auth-service", "order-service", "database"]
    }
  }
  
  // 测试有效的追踪链
  let valid_chain = create_test_trace_chain()
  let validation_result = validate_trace_chain(valid_chain)
  
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  assert_eq(validation_result.span_count, 4)
  assert_eq(validation_result.service_count, 4)
  
  // 测试无效的追踪链 - 时间顺序错误
  let invalid_chain = {
    trace_id: valid_chain.trace_id,
    root_span: {
      trace_id: valid_chain.root_span.trace_id,
      span_id: valid_chain.root_span.span_id,
      parent_span_id: valid_chain.root_span.parent_span_id,
      service_name: valid_chain.root_span.service_name,
      operation_name: valid_chain.root_span.operation_name,
      start_time: 1000,
      end_time: 1500,
      status: valid_chain.root_span.status,
      attributes: valid_chain.root_span.attributes
    },
    child_spans: [
      {
        trace_id: valid_chain.child_spans[0].trace_id,
        span_id: valid_chain.child_spans[0].span_id,
        parent_span_id: valid_chain.child_spans[0].parent_span_id,
        service_name: valid_chain.child_spans[0].service_name,
        operation_name: valid_chain.child_spans[0].operation_name,
        start_time: 900,  // 早于父span开始时间
        end_time: 1200,
        status: valid_chain.child_spans[0].status,
        attributes: valid_chain.child_spans[0].attributes
      }
    ],
    services_involved: valid_chain.services_involved
  }
  
  let invalid_result = validate_trace_chain(invalid_chain)
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.errors.length() > 0)
  assert_true(invalid_result.errors.any(fn(err) { err.contains("子span的开始时间早于父span") }))
}

// 测试2: 追踪上下文传播一致性
test "追踪上下文传播一致性验证" {
  // 定义追踪上下文类型
  type TraceContext = {
    trace_id: String,
    span_id: String,
    trace_flags: Int,
    trace_state: String,
    baggage: Array[(String, String)]
  }
  
  // 定义上下文传播器
  type ContextPropagator = {
    inject: fn(TraceContext, Array[(String, String)>) -> Array[(String, String)],
    extract: fn(Array[(String, String)>) -> Option[TraceContext]
  }
  
  // 创建HTTP头传播器
  let create_http_propagator = fn() {
    let inject = fn(context: TraceContext, headers: Array[(String, String)>) {
      let updated_headers = headers
        .filter(fn(header) { header.0 != "traceparent" && header.0 != "tracestate" && header.0 != "baggage" })
      
      let traceparent = "00-" + context.trace_id + "-" + context.span_id + "-" + 
                       if context.trace_flags == 1 { "01" } else { "00" }
      
      let updated_with_traceparent = updated_headers.push(("traceparent", traceparent))
      let updated_with_tracestate = if context.trace_state != "" {
        updated_with_traceparent.push(("tracestate", context.trace_state))
      } else {
        updated_with_traceparent
      }
      
      if context.baggage.length() > 0 {
        let baggage_string = context.baggage.map(fn(item) { item.0 + "=" + item.1 }).join(",")
        updated_with_tracestate.push(("baggage", baggage_string))
      } else {
        updated_with_tracestate
      }
    }
    
    let extract = fn(headers: Array[(String, String)>) {
      match headers.find(fn(header) { header.0 == "traceparent" }) {
        Some((_, traceparent)) => {
          let parts = traceparent.split("-")
          if parts.length() >= 4 {
            let trace_id = parts[1]
            let span_id = parts[2]
            let trace_flags = if parts[3] == "01" { 1 } else { 0 }
            
            let trace_state = match headers.find(fn(header) { header.0 == "tracestate" }) {
              Some((_, value)) => value
              None => ""
            }
            
            let baggage = match headers.find(fn(header) { header.0 == "baggage" }) {
              Some((_, value)) => {
                value.split(",").map(fn(item) {
                  let kv = item.split("=")
                  if kv.length() == 2 {
                    (kv[0], kv[1])
                  } else {
                    (item, "")
                  }
                })
              }
              None => []
            }
            
            Some({
              trace_id: trace_id,
              span_id: span_id,
              trace_flags: trace_flags,
              trace_state: trace_state,
              baggage: baggage
            })
          } else {
            None
          }
        }
        None => None
      }
    }
    
    { inject, extract }
  }
  
  // 测试上下文传播的往返一致性
  let propagator = create_http_propagator()
  
  let original_context = {
    trace_id: "trace-1234567890abcdef1234567890abcdef",
    span_id: "span-1234567890abcdef",
    trace_flags: 1,
    trace_state: "vendor1=value1,vendor2=value2",
    baggage: [
      ("user.id", "user-123"),
      ("request.id", "req-456"),
      ("session.id", "sess-789")
    ]
  }
  
  // 注入上下文到HTTP头
  let original_headers = [
    ("content-type", "application/json"),
    ("authorization", "Bearer token123")
  ]
  
  let injected_headers = propagator.inject(original_context, original_headers)
  
  // 验证注入的头部
  assert_true(injected_headers.any(fn(header) { header.0 == "traceparent" }))
  assert_true(injected_headers.any(fn(header) { header.0 == "tracestate" }))
  assert_true(injected_headers.any(fn(header) { header.0 == "baggage" }))
  
  let traceparent_value = match injected_headers.find(fn(header) { header.0 == "traceparent" }) {
    Some((_, value)) => value
    None => ""
  }
  assert_true(traceparent_value.contains(original_context.trace_id))
  assert_true(traceparent_value.contains(original_context.span_id))
  
  // 从HTTP头提取上下文
  let extracted_context = propagator.extract(injected_headers)
  
  assert_true(extracted_context.is_some())
  match extracted_context {
    Some(context) => {
      assert_eq(context.trace_id, original_context.trace_id)
      assert_eq(context.span_id, original_context.span_id)
      assert_eq(context.trace_flags, original_context.trace_flags)
      assert_eq(context.trace_state, original_context.trace_state)
      assert_eq(context.baggage.length(), original_context.baggage.length())
      
      // 验证baggage内容
      for original_item in original_context.baggage {
        assert_true(context.baggage.any(fn(item) { item.0 == original_item.0 && item.1 == original_item.1 }))
      }
    }
    None => assert_true(false)
  }
  
  // 测试跨服务传播一致性
  let simulate_cross_service_propagation = fn(services: Array<String>, initial_context: TraceContext) {
    let mut current_context = initial_context
    let mut all_headers = []
    
    for service in services {
      let service_headers = [("service.name", service)]
      let injected = propagator.inject(current_context, service_headers)
      all_headers = all_headers.push((service, injected))
      
      match propagator.extract(injected) {
        Some(extracted) => current_context = extracted
        None => assert_true(false)
      }
    }
    
    (current_context, all_headers)
  }
  
  let services = ["api-gateway", "auth-service", "order-service", "payment-service"]
  let (final_context, _) = simulate_cross_service_propagation(services, original_context)
  
  // 验证跨服务传播后上下文的一致性
  assert_eq(final_context.trace_id, original_context.trace_id)
  assert_eq(final_context.trace_flags, original_context.trace_flags)
  assert_eq(final_context.trace_state, original_context.trace_state)
  assert_eq(final_context.baggage.length(), original_context.baggage.length())
  
  // span_id应该在每个服务中变化，但trace_id应该保持不变
  assert_ne(final_context.span_id, original_context.span_id)
}

// 测试3: 分布式追踪数据采样一致性
test "分布式追踪数据采样一致性验证" {
  // 定义采样决策类型
  enum SamplingDecision {
    RecordAndSample
    RecordOnly
    Drop
  }
  
  // 定义采样器类型
  type Sampler = {
    name: String,
    should_sample: fn(TraceContext, String) -> SamplingDecision,
    get_attributes: fn() -> Array[(String, String)]
  }
  
  // 创建一致性采样器
  let create_consistency_sampler = fn(sample_rate: Float) {
    let should_sample = fn(context: TraceContext, operation_name: String) {
      // 基于trace_id的确定性采样
      let hash = context.trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
      let normalized = (hash % 100) as Float / 100.0
      
      if normalized <= sample_rate {
        SamplingDecision::RecordAndSample
      } else {
        SamplingDecision::Drop
      }
    }
    
    let get_attributes = fn() {
      [
        ("sampler.type", "consistency"),
        ("sampler.rate", sample_rate.to_string())
      ]
    }
    
    {
      name: "consistency_sampler",
      should_sample,
      get_attributes
    }
  }
  
  // 创建自适应采样器
  let create_adaptive_sampler = fn(base_rate: Float, max_rate: Float) {
    let should_sample = fn(context: TraceContext, operation_name: String) {
      // 根据操作名称调整采样率
      let operation_multiplier = match operation_name {
        "critical_operation" => 2.0
        "normal_operation" => 1.0
        "debug_operation" => 0.5
        _ => 1.0
      }
      
      let adjusted_rate = base_rate * operation_multiplier
      let final_rate = if adjusted_rate > max_rate { max_rate } else { adjusted_rate }
      
      let hash = (context.trace_id + operation_name).chars().reduce(0, fn(acc, c) { acc + c.to_int() })
      let normalized = (hash % 100) as Float / 100.0
      
      if normalized <= final_rate {
        SamplingDecision::RecordAndSample
      } else {
        SamplingDecision::Drop
      }
    }
    
    let get_attributes = fn() {
      [
        ("sampler.type", "adaptive"),
        ("sampler.base_rate", base_rate.to_string()),
        ("sampler.max_rate", max_rate.to_string())
      ]
    }
    
    {
      name: "adaptive_sampler",
      should_sample,
      get_attributes
    }
  }
  
  // 测试采样一致性
  let test_sampling_consistency = fn(sampler: Sampler, trace_context: TraceContext, operations: Array<String>) {
    let decisions = []
    
    for operation in operations {
      let decision = sampler.should_sample(trace_context, operation)
      decisions = decisions.push((operation, decision))
    }
    
    // 计算采样率
    let sampled_count = decisions.filter(fn(_, decision) { 
      match decision {
        SamplingDecision::RecordAndSample => true
        _ => false
      }
    }).length()
    
    let actual_rate = (sampled_count as Float) / (decisions.length() as Float)
    
    {
      decisions: decisions,
      actual_sample_rate: actual_rate,
      sampler_attributes: sampler.get_attributes()
    }
  }
  
  // 测试一致性采样器
  let consistency_sampler = create_consistency_sampler(0.5)  // 50%采样率
  let test_context = {
    trace_id: "trace-1234567890abcdef1234567890abcdef",
    span_id: "span-1234567890abcdef",
    trace_flags: 1,
    trace_state: "",
    baggage: []
  }
  
  let test_operations = [
    "operation_1", "operation_2", "operation_3", "operation_4", "operation_5",
    "operation_6", "operation_7", "operation_8", "operation_9", "operation_10"
  ]
  
  let consistency_result = test_sampling_consistency(consistency_sampler, test_context, test_operations)
  
  // 验证采样率接近预期值（允许10%的误差）
  let expected_rate = 0.5
  assert_true(consistency_result.actual_sample_rate >= expected_rate - 0.1)
  assert_true(consistency_result.actual_sample_rate <= expected_rate + 0.1)
  
  // 验证采样器属性
  assert_true(consistency_result.sampler_attributes.any(fn(attr) { 
    attr.0 == "sampler.type" && attr.1 == "consistency" 
  }))
  assert_true(consistency_result.sampler_attributes.any(fn(attr) { 
    attr.0 == "sampler.rate" && attr.1 == "0.5" 
  }))
  
  // 测试自适应采样器
  let adaptive_sampler = create_adaptive_sampler(0.3, 0.8)
  let adaptive_operations = [
    "critical_operation", "critical_operation", "critical_operation",
    "normal_operation", "normal_operation", "normal_operation",
    "debug_operation", "debug_operation", "debug_operation",
    "unknown_operation", "unknown_operation"
  ]
  
  let adaptive_result = test_sampling_consistency(adaptive_sampler, test_context, adaptive_operations)
  
  // 验证自适应采样器的属性
  assert_true(adaptive_result.sampler_attributes.any(fn(attr) { 
    attr.0 == "sampler.type" && attr.1 == "adaptive" 
  }))
  
  // 验证关键操作有更高的采样率
  let critical_decisions = adaptive_result.decisions.filter(fn(op, _) { op == "critical_operation" })
  let debug_decisions = adaptive_result.decisions.filter(fn(op, _) { op == "debug_operation" })
  
  let critical_sampled = critical_decisions.filter(fn(_, decision) {
    match decision {
      SamplingDecision::RecordAndSample => true
      _ => false
    }
  }).length()
  
  let debug_sampled = debug_decisions.filter(fn(_, decision) {
    match decision {
      SamplingDecision::RecordAndSample => true
      _ => false
    }
  }).length()
  
  // 关键操作的采样率应该高于调试操作
  assert_true((critical_sampled as Float) / (critical_decisions.length() as Float) >= 
              (debug_sampled as Float) / (debug_decisions.length() as Float))
}

// 测试4: 分布式追踪数据完整性验证
test "分布式追踪数据完整性验证" {
  // 定义数据完整性检查器
  type IntegrityChecker = {
    check_span_completeness: fn(TraceNode) -> Array[String],
    check_attribute_consistency: fn(Array[TraceNode]) -> Array[String],
    check_timeline_coherence: fn(Array[TraceNode]) -> Array[String]
  }
  
  // 创建完整性检查器
  let create_integrity_checker = fn() {
    let check_span_completeness = fn(span: TraceNode) {
      let errors = []
      
      // 检查必需字段
      if span.trace_id == "" {
        errors = errors.push("trace_id不能为空")
      }
      
      if span.span_id == "" {
        errors = errors.push("span_id不能为空")
      }
      
      if span.service_name == "" {
        errors = errors.push("service_name不能为空")
      }
      
      if span.operation_name == "" {
        errors = errors.push("operation_name不能为空")
      }
      
      if span.status == "" {
        errors = errors.push("status不能为空")
      }
      
      // 检查时间有效性
      if span.start_time <= 0 {
        errors = errors.push("start_time必须大于0")
      }
      
      if span.end_time <= 0 {
        errors = errors.push("end_time必须大于0")
      }
      
      if span.end_time < span.start_time {
        errors = errors.push("end_time不能早于start_time")
      }
      
      // 检查span_id格式
      if span.span_id.length() < 8 {
        errors = errors.push("span_id长度不足")
      }
      
      // 检查trace_id格式
      if span.trace_id.length() < 16 {
        errors = errors.push("trace_id长度不足")
      }
      
      errors
    }
    
    let check_attribute_consistency = fn(spans: Array[TraceNode>) {
      let errors = []
      
      // 检查同一服务内的属性一致性
      let service_groups = Map::empty()
      for span in spans {
        let service_spans = match Map::get(service_groups, span.service_name) {
          Some(spans) => spans.push(span)
          None => [span]
        }
        let _ = Map::insert(service_groups, span.service_name, service_spans)
      }
      
      for (service_name, service_spans) in service_groups {
        let service_versions = service_spans.filter_map(fn(span) {
          span.attributes.find(fn(attr) { attr.0 == "service.version" })
        })
        
        if service_versions.length() > 1 {
          let unique_versions = service_versions.map(fn(version) { version.1 }).unique()
          if unique_versions.length() > 1 {
            errors = errors.push("服务" + service_name + "存在多个版本: " + unique_versions.join(","))
          }
        }
      }
      
      // 检查必需的全局属性
      let global_attributes = ["service.name", "service.instance.id"]
      for span in spans {
        for required_attr in global_attributes {
          if not span.attributes.any(fn(attr) { attr.0 == required_attr }) {
            errors = errors.push("span" + span.span_id + "缺少必需属性: " + required_attr)
          }
        }
      }
      
      errors
    }
    
    let check_timeline_coherence = fn(spans: Array[TraceNode>) {
      let errors = []
      
      // 按时间排序spans
      let sorted_spans = spans.sort(fn(a, b) { 
        if a.start_time < b.start_time { -1 } 
        else if a.start_time > b.start_time { 1 } 
        else { 0 } 
      })
      
      // 检查时间重叠和间隙
      for i in 0..(sorted_spans.length() - 1) {
        let current = sorted_spans[i]
        let next = sorted_spans[i + 1]
        
        // 检查父子关系的时间重叠
        if next.parent_span_id == Some(current.span_id) {
          if next.start_time < current.start_time {
            errors = errors.push("子span" + next.span_id + "的开始时间早于父span" + current.span_id)
          }
          
          if next.end_time > current.end_time {
            errors = errors.push("子span" + next.span_id + "的结束时间晚于父span" + current.span_id)
          }
        }
        
        // 检查异常的时间间隙
        let time_gap = next.start_time - current.end_time
        if time_gap > 60000 {  // 超过1分钟的间隙
          errors = errors.push("span" + current.span_id + "和" + next.span_id + "之间存在异常时间间隙: " + time_gap.to_string() + "ms")
        }
      }
      
      errors
    }
    
    {
      check_span_completeness,
      check_attribute_consistency,
      check_timeline_coherence
    }
  }
  
  // 创建测试数据
  let create_test_spans = fn() {
    let trace_id = "trace-1234567890abcdef1234567890abcdef"
    
    let root_span = {
      trace_id: trace_id,
      span_id: "span-root-12345678",
      parent_span_id: None,
      service_name: "api-gateway",
      operation_name: "handle_request",
      start_time: 1000,
      end_time: 1500,
      status: "ok",
      attributes: [
        ("service.name", "api-gateway"),
        ("service.instance.id", "instance-1"),
        ("service.version", "1.2.3"),
        ("http.method", "GET"),
        ("http.url", "/api/orders")
      ]
    }
    
    let auth_span = {
      trace_id: trace_id,
      span_id: "span-auth-12345679",
      parent_span_id: Some("span-root-12345678"),
      service_name: "auth-service",
      operation_name: "authenticate",
      start_time: 1050,
      end_time: 1200,
      status: "ok",
      attributes: [
        ("service.name", "auth-service"),
        ("service.instance.id", "instance-2"),
        ("service.version", "1.2.3"),
        ("auth.method", "jwt"),
        ("auth.user_id", "user-123")
      ]
    }
    
    let order_span = {
      trace_id: trace_id,
      span_id: "span-order-12345680",
      parent_span_id: Some("span-root-12345678"),
      service_name: "order-service",
      operation_name: "get_orders",
      start_time: 1250,
      end_time: 1400,
      status: "ok",
      attributes: [
        ("service.name", "order-service"),
        ("service.instance.id", "instance-3"),
        ("service.version", "1.2.3"),
        ("order.count", "5")
      ]
    }
    
    // 创建一个有问题的span用于测试
    let invalid_span = {
      trace_id: "",  // 无效的trace_id
      span_id: "short",  // 太短的span_id
      parent_span_id: Some("non-existent-parent"),
      service_name: "",  // 缺少服务名
      operation_name: "test",
      start_time: 2000,
      end_time: 1000,  // 结束时间早于开始时间
      status: "",
      attributes: []  // 缺少必需属性
    }
    
    [root_span, auth_span, order_span, invalid_span]
  }
  
  // 执行完整性检查
  let checker = create_integrity_checker()
  let test_spans = create_test_spans()
  
  // 检查span完整性
  let completeness_errors = []
  for span in test_spans {
    let span_errors = checker.check_span_completeness(span)
    completeness_errors = completeness_errors + span_errors
  }
  
  // 验证完整性错误
  assert_true(completeness_errors.length() > 0)  // 应该检测到无效span的错误
  assert_true(completeness_errors.any(fn(err) { err.contains("trace_id不能为空") }))
  assert_true(completeness_errors.any(fn(err) { err.contains("span_id长度不足") }))
  assert_true(completeness_errors.any(fn(err) { err.contains("service_name不能为空") }))
  assert_true(completeness_errors.any(fn(err) { err.contains("end_time不能早于start_time") }))
  
  // 检查属性一致性
  let attribute_errors = checker.check_attribute_consistency(test_spans)
  
  // 验证属性一致性错误
  assert_true(attribute_errors.length() > 0)  // 应该检测到无效span缺少属性的错误
  assert_true(attribute_errors.any(fn(err) { err.contains("缺少必需属性") }))
  
  // 检查时间线一致性
  let timeline_errors = checker.check_timeline_coherence(test_spans)
  
  // 验证时间线错误
  assert_true(timeline_errors.length() > 0)  // 应该检测到无效span的时间错误
  assert_true(timeline_errors.any(fn(err) { err.contains("结束时间早于开始时间") }))
  
  // 测试有效的spans
  let valid_spans = test_spans.filter(fn(span) { 
    span.trace_id != "" && span.service_name != "" && span.end_time >= span.start_time 
  })
  
  let valid_completeness_errors = []
  for span in valid_spans {
    let span_errors = checker.check_span_completeness(span)
    valid_completeness_errors = valid_completeness_errors + span_errors
  }
  
  // 有效的spans应该没有完整性错误
  assert_eq(valid_completeness_errors.length(), 0)
}