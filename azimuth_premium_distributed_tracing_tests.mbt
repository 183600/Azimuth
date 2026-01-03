// Azimuth Distributed Tracing Tests
// This file contains test cases for distributed tracing functionality

// Test 1: Span Creation and Management
test "分布式追踪Span创建和管理" {
  // 定义Span类型
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Option[Int],
    status: String,
    tags: Array[(String, String)],
    events: Array[SpanEvent]
  }
  
  type SpanEvent = {
    timestamp: Int,
    name: String,
    attributes: Array[(String, String)]
  }
  
  // 创建根Span
  let create_root_span = fn(trace_id: String, operation_name: String) {
    {
      trace_id,
      span_id: "span-" + Time::now().to_string(),
      parent_span_id: None,
      operation_name,
      start_time: Time::now(),
      end_time: None,
      status: "running",
      tags: [],
      events: []
    }
  }
  
  // 创建子Span
  let create_child_span = fn(parent: Span, operation_name: String) {
    {
      trace_id: parent.trace_id,
      span_id: "span-" + Time::now().to_string(),
      parent_span_id: Some(parent.span_id),
      operation_name,
      start_time: Time::now(),
      end_time: None,
      status: "running",
      tags: [],
      events: []
    }
  }
  
  // 完成Span
  let finish_span = fn(span: Span, status: String) {
    {
      ...span,
      end_time: Some(Time::now()),
      status: status
    }
  }
  
  // 添加标签
  let add_tag = fn(span: Span, key: String, value: String) {
    {
      ...span,
      tags: span.tags.push((key, value))
    }
  }
  
  // 添加事件
  let add_event = fn(span: Span, event_name: String, attributes: Array[(String, String)]) {
    let event = {
      timestamp: Time::now(),
      name: event_name,
      attributes: attributes
    }
    {
      ...span,
      events: span.events.push(event)
    }
  }
  
  // 测试Span创建
  let root_span = create_root_span("trace-123", "http-request")
  assert_eq(root_span.trace_id, "trace-123")
  assert_eq(root_span.operation_name, "http-request")
  assert_eq(root_span.parent_span_id, None)
  assert_eq(root_span.status, "running")
  assert_eq(root_span.end_time, None)
  
  // 测试子Span创建
  let child_span = create_child_span(root_span, "database-query")
  assert_eq(child_span.trace_id, "trace-123")
  assert_eq(child_span.operation_name, "database-query")
  assert_eq(child_span.parent_span_id, Some(root_span.span_id))
  
  // 测试添加标签
  let tagged_span = add_tag(root_span, "http.method", "GET")
  assert_eq(tagged_span.tags.length(), 1)
  assert_eq(tagged_span.tags[0], ("http.method", "GET"))
  
  // 测试添加事件
  let event_span = add_tag(tagged_span, "db.statement", "SELECT * FROM users")
  let event_with_attributes = add_event(event_span, "db.query.start", [
    ("db.type", "postgresql"),
    ("db.connection_id", "conn-123")
  ])
  assert_eq(event_with_attributes.events.length(), 1)
  assert_eq(event_with_attributes.events[0].name, "db.query.start")
  
  // 测试完成Span
  let finished_span = finish_span(event_with_attributes, "success")
  assert_eq(finished_span.status, "success")
  assert_true(finished_span.end_time.is_some())
}

// Test 2: Trace Context Propagation
test "分布式追踪上下文传播" {
  // 定义追踪上下文
  type TraceContext = {
    trace_id: String,
    span_id: String,
    baggage: Array[(String, String)]
  }
  
  // 创建追踪上下文
  let create_trace_context = fn(trace_id: String, span_id: String) {
    {
      trace_id,
      span_id,
      baggage: []
    }
  }
  
  // 添加行李项
  let add_baggage_item = fn(context: TraceContext, key: String, value: String) {
    {
      ...context,
      baggage: context.baggage.push((key, value))
    }
  }
  
  // 序列化上下文为HTTP头
  let serialize_context = fn(context: TraceContext) {
    let trace_header = "trace-id=" + context.trace_id + ";span-id=" + context.span_id
    let baggage_header = context.baggage.map(fn(item) {
      let (key, value) = item
      key + "=" + value
    }).join(",")
    
    [
      ("x-trace-context", trace_header),
      ("x-baggage", baggage_header)
    ]
  }
  
  // 从HTTP头反序列化上下文
  let deserialize_context = fn(headers: Array[(String, String)]) {
    let mut trace_id = ""
    let mut span_id = ""
    let mut baggage = []
    
    for (key, value) in headers {
      if key == "x-trace-context" {
        let parts = value.split(";")
        for part in parts {
          let kv = part.split("=")
          if kv.length() == 2 {
            if kv[0] == "trace-id" {
              trace_id = kv[1]
            } else if kv[0] == "span-id" {
              span_id = kv[1]
            }
          }
        }
      } else if key == "x-baggage" && value != "" {
        let items = value.split(",")
        for item in items {
          let kv = item.split("=")
          if kv.length() == 2 {
            baggage = baggage.push((kv[0], kv[1]))
          }
        }
      }
    }
    
    if trace_id != "" && span_id != "" {
      Some({
        trace_id,
        span_id,
        baggage
      })
    } else {
      None
    }
  }
  
  // 测试上下文创建和传播
  let original_context = create_trace_context("trace-456", "span-789")
  let context_with_baggage = add_baggage_item(original_context, "user.id", "user-123")
  let full_context = add_baggage_item(context_with_baggage, "request.source", "mobile")
  
  // 序列化上下文
  let headers = serialize_context(full_context)
  
  // 验证追踪头
  let trace_header = headers.find(fn(h) { h.0 == "x-trace-context" })
  match trace_header {
    Some((_, value)) => {
      assert_true(value.contains("trace-id=trace-456"))
      assert_true(value.contains("span-id=span-789"))
    }
    None => assert_true(false)
  }
  
  // 验证行李头
  let baggage_header = headers.find(fn(h) { h.0 == "x-baggage" })
  match baggage_header {
    Some((_, value)) => {
      assert_true(value.contains("user.id=user-123"))
      assert_true(value.contains("request.source=mobile"))
    }
    None => assert_true(false)
  }
  
  // 反序列化上下文
  let deserialized_context = deserialize_context(headers)
  match deserialized_context {
    Some(context) => {
      assert_eq(context.trace_id, "trace-456")
      assert_eq(context.span_id, "span-789")
      assert_eq(context.baggage.length(), 2)
      
      let user_id_item = context.baggage.find(fn(item) { item.0 == "user.id" })
      match user_id_item {
        Some((key, value)) => {
          assert_eq(key, "user.id")
          assert_eq(value, "user-123")
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 3: Span Sampling
test "分布式追踪采样策略" {
  // 定义采样决策
  type SamplingDecision = {
    sampled: Bool,
    attributes: Array[(String, String)]
  }
  
  // 常量采样器
  let constant_sampler = fn(sample: Bool) {
    fn(trace_id: String, span_name: String) {
      {
        sampled: sample,
        attributes: if sample {
          [("sampler.type", "constant")]
        } else {
          []
        }
      }
    }
  }
  
  // 概率采样器
  let probability_sampler = fn(probability: Float) {
    fn(trace_id: String, span_name: String) {
      // 简化的哈希函数
      let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
      let normalized = (hash % 1000) as Float / 1000.0
      
      {
        sampled: normalized <= probability,
        attributes: [
          ("sampler.type", "probability"),
          ("sampler.probability", probability.to_string())
        ]
      }
    }
  }
  
  // 基于操作名的采样器
  let operation_name_sampler = fn(operations: Array[String>) {
    fn(trace_id: String, span_name: String) {
      let should_sample = operations.any(fn(op) { op == span_name })
      
      {
        sampled: should_sample,
        attributes: [
          ("sampler.type", "operation_name"),
          ("operation.name", span_name)
        ]
      }
    }
  }
  
  // 组合采样器（任一通过则采样）
  let any_sampler = fn(samplers: Array[(String, String) -> SamplingDecision>) {
    fn(trace_id: String, span_name: String) {
      let mut decisions = []
      
      for sampler in samplers {
        let decision = sampler(trace_id, span_name)
        decisions = decisions.push(decision)
        
        if decision.sampled {
          return {
            sampled: true,
            attributes: decisions.reduce(fn(acc, d) { acc + d.attributes }, [])
          }
        }
      }
      
      {
        sampled: false,
        attributes: decisions.reduce(fn(acc, d) { acc + d.attributes }, [])
      }
    }
  }
  
  // 测试常量采样器
  let always_sampler = constant_sampler(true)
  let always_decision = always_sampler("trace-123", "http-request")
  assert_true(always_decision.sampled)
  assert_eq(always_decision.attributes[0], ("sampler.type", "constant"))
  
  let never_sampler = constant_sampler(false)
  let never_decision = never_sampler("trace-123", "http-request")
  assert_false(never_decision.sampled)
  assert_eq(never_decision.attributes.length(), 0)
  
  // 测试概率采样器
  let prob_sampler = probability_sampler(0.5)  // 50%采样率
  let prob_decision1 = prob_sampler("trace-123", "http-request")
  let prob_decision2 = prob_sampler("trace-456", "http-request")
  
  // 至少有一个应该被采样（取决于哈希值）
  assert_true(prob_decision1.sampled || prob_decision2.sampled)
  assert_eq(prob_decision1.attributes[0], ("sampler.type", "probability"))
  
  // 测试操作名采样器
  let ops_sampler = operation_name_sampler(["http-request", "database-query"])
  let sampled_op = ops_sampler("trace-123", "http-request")
  assert_true(sampled_op.sampled)
  
  let not_sampled_op = ops_sampler("trace-123", "cache-lookup")
  assert_false(not_sampled_op.sampled)
  
  // 测试组合采样器
  let combined_sampler = any_sampler([
    operation_name_sampler(["critical-operation"]),
    probability_sampler(0.3)
  ])
  
  let critical_op = combined_sampler("trace-123", "critical-operation")
  assert_true(critical_op.sampled)  // 应该被操作名采样器采样
  
  let normal_op = combined_sampler("trace-456", "normal-operation")
  // 可能被概率采样器采样（取决于哈希值）
}

// Test 4: Trace Analysis
test "分布式追踪分析" {
  // 定义Span类型
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Option[Int],
    status: String,
    tags: Array[(String, String)],
    events: Array[SpanEvent]
  }
  
  type SpanEvent = {
    timestamp: Int,
    name: String,
    attributes: Array[(String, String)]
  }
  
  // 计算Span持续时间
  let span_duration = fn(span: Span) {
    match span.end_time {
      Some(end_time) => end_time - span.start_time
      None => 0
    }
  }
  
  // 构建Span树
  let build_span_tree = fn(spans: Array[Span]) {
    let mut root_spans = []
    let mut child_spans = []
    
    // 分离根Span和子Span
    for span in spans {
      match span.parent_span_id {
        Some(_) => child_spans = child_spans.push(span)
        None => root_spans = root_spans.push(span)
      }
    }
    
    // 为每个根Span构建子树
    let build_tree = fn(root: Span, children: Array[Span]) {
      let mut direct_children = []
      let mut remaining_children = []
      
      // 找到直接子Span
      for child in children {
        match child.parent_span_id {
          Some(parent_id) => {
            if parent_id == root.span_id {
              direct_children = direct_children.push(child)
            } else {
              remaining_children = remaining_children.push(child)
            }
          }
          None => remaining_children = remaining_children.push(child)
        }
      }
      
      // 递归构建子树
      let processed_children = direct_children.map(fn(child) {
        build_tree(child, remaining_children)
      })
      
      {
        span: root,
        children: processed_children
      }
    }
    
    root_spans.map(fn(root) { build_tree(root, child_spans) })
  }
  
  // 分析追踪性能
  let analyze_trace_performance = fn(spans: Array[Span]) {
    if spans.length() == 0 {
      return {
        total_spans: 0,
        total_duration: 0,
        average_span_duration: 0.0,
        error_rate: 0.0,
        slowest_span: None
      }
    }
    
    let durations = spans.map(span_duration)
    let total_duration = durations.reduce(fn(acc, d) { acc + d }, 0)
    let average_duration = total_duration as Float / spans.length() as Float
    
    let error_count = spans.filter(fn(span) { span.status != "success" }).length()
    let error_rate = error_count as Float / spans.length() as Float
    
    let slowest_span = if spans.length() > 0 {
      let max_duration = durations.reduce(fn(acc, d) { if d > acc { d } else { acc } }, durations[0])
      let index = durations.find_index(fn(d) { d == max_duration })
      match index {
        Some(i) => Some(spans[i])
        None => None
      }
    } else {
      None
    }
    
    {
      total_spans: spans.length(),
      total_duration,
      average_span_duration: average_duration,
      error_rate,
      slowest_span
    }
  }
  
  // 创建测试Span
  let test_spans = [
    {
      trace_id: "trace-123",
      span_id: "span-1",
      parent_span_id: None,
      operation_name: "http-request",
      start_time: 1000,
      end_time: Some(1500),
      status: "success",
      tags: [],
      events: []
    },
    {
      trace_id: "trace-123",
      span_id: "span-2",
      parent_span_id: Some("span-1"),
      operation_name: "database-query",
      start_time: 1100,
      end_time: Some(1400),
      status: "success",
      tags: [],
      events: []
    },
    {
      trace_id: "trace-123",
      span_id: "span-3",
      parent_span_id: Some("span-1"),
      operation_name: "cache-lookup",
      start_time: 1200,
      end_time: Some(1250),
      status: "error",
      tags: [],
      events: []
    }
  ]
  
  // 测试Span持续时间计算
  let root_span = test_spans[0]
  let root_duration = span_duration(root_span)
  assert_eq(root_duration, 500)  // 1500 - 1000
  
  // 测试Span树构建
  let span_trees = build_span_tree(test_spans)
  assert_eq(span_trees.length(), 1)  // 只有一个根Span
  
  let root_tree = span_trees[0]
  assert_eq(root_tree.span.operation_name, "http-request")
  assert_eq(root_tree.children.length(), 2)  // 两个子Span
  
  // 测试追踪性能分析
  let performance = analyze_trace_performance(test_spans)
  assert_eq(performance.total_spans, 3)
  assert_eq(performance.total_duration, 500 + 300 + 50)  // 850
  assert_eq(performance.error_rate, 1.0 / 3.0)  // 1个错误/3个Span
  
  match performance.slowest_span {
    Some(slowest) => assert_eq(slowest.operation_name, "http-request")
    None => assert_true(false)
  }
}