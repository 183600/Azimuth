// Azimuth Enhanced MoonBit Test Suite
// 专注于核心遥测系统的高级测试用例

// 测试1: 遥测数据转换和过滤
test "telemetry data transformation and filtering" {
  // 定义遥测数据结构
  type TelemetryData = {
    id: String,
    timestamp: Int,
    metric_type: String,
    value: Float,
    tags: Array[(String, String)]
  }
  
  // 创建测试数据集
  let telemetry_data = [
    { id: "data-001", timestamp: 1640995200, metric_type: "cpu", value: 65.5, tags: [("service", "api"), ("region", "us-west")] },
    { id: "data-002", timestamp: 1640995260, metric_type: "memory", value: 78.2, tags: [("service", "db"), ("region", "us-west")] },
    { id: "data-003", timestamp: 1640995320, metric_type: "cpu", value: 45.8, tags: [("service", "cache"), ("region", "us-east")] },
    { id: "data-004", timestamp: 1640995380, metric_type: "network", value: 125.3, tags: [("service", "api"), ("region", "us-west")] },
    { id: "data-005", timestamp: 1640995440, metric_type: "memory", value: 82.1, tags: [("service", "worker"), ("region", "us-east")] }
  ]
  
  // 数据转换函数 - 标准化时间戳为分钟
  let normalize_timestamp = fn(data: TelemetryData) {
    { data | timestamp: (data.timestamp / 60) * 60 }
  }
  
  // 数据过滤函数 - 按指标类型过滤
  let filter_by_metric_type = fn(data: Array[TelemetryData], metric_type: String) {
    data.filter(fn(item) { item.metric_type == metric_type })
  }
  
  // 数据过滤函数 - 按标签过滤
  let filter_by_tag = fn(data: Array[TelemetryData], tag_key: String, tag_value: String) {
    data.filter(fn(item) { 
      item.tags.contains((tag_key, tag_value)) 
    })
  }
  
  // 测试数据转换
  let normalized_data = telemetry_data.map(normalize_timestamp)
  assert_eq(normalized_data.length(), 5)
  assert_eq(normalized_data[0].timestamp, 1640995200)
  assert_eq(normalized_data[1].timestamp, 1640995200)
  assert_eq(normalized_data[2].timestamp, 1640995320)
  
  // 测试按指标类型过滤
  let cpu_data = filter_by_metric_type(telemetry_data, "cpu")
  assert_eq(cpu_data.length(), 2)
  assert_eq(cpu_data[0].metric_type, "cpu")
  assert_eq(cpu_data[1].metric_type, "cpu")
  
  let memory_data = filter_by_metric_type(telemetry_data, "memory")
  assert_eq(memory_data.length(), 2)
  assert_eq(memory_data[0].metric_type, "memory")
  assert_eq(memory_data[1].metric_type, "memory")
  
  // 测试按标签过滤
  let us_west_data = filter_by_tag(telemetry_data, "region", "us-west")
  assert_eq(us_west_data.length(), 3)
  
  let api_service_data = filter_by_tag(telemetry_data, "service", "api")
  assert_eq(api_service_data.length(), 2)
  
  // 复合过滤 - 组合多个条件
  let complex_filter = fn(data: Array[TelemetryData]) {
    data.filter(fn(item) { 
      item.metric_type == "cpu" and 
      item.tags.contains(("region", "us-west")) and
      item.value > 50.0
    })
  }
  
  let filtered_data = complex_filter(telemetry_data)
  assert_eq(filtered_data.length(), 1)
  assert_eq(filtered_data[0].id, "data-001")
  assert_eq(filtered_data[0].value, 65.5)
}

// 测试2: 度量聚合和统计操作
test "metric aggregation and statistical operations" {
  // 定义度量数据点
  type MetricPoint = {
    name: String,
    value: Float,
    timestamp: Int,
    attributes: Array[(String, String)]
  }
  
  // 创建测试度量数据
  let metrics = [
    { name: "response_time", value: 120.5, timestamp: 1640995200, attributes: [("endpoint", "/api/users")] },
    { name: "response_time", value: 85.3, timestamp: 1640995260, attributes: [("endpoint", "/api/users")] },
    { name: "response_time", value: 200.1, timestamp: 1640995320, attributes: [("endpoint", "/api/orders")] },
    { name: "response_time", value: 95.7, timestamp: 1640995380, attributes: [("endpoint", "/api/users")] },
    { name: "response_time", value: 150.2, timestamp: 1640995440, attributes: [("endpoint", "/api/products")] },
    { name: "response_time", value: 110.8, timestamp: 1640995500, attributes: [("endpoint", "/api/users")] }
  ]
  
  // 计算平均值
  let calculate_average = fn(values: Array[Float]) {
    let sum = values.reduce(fn(acc, val) { acc + val }, 0.0)
    sum / values.length().to_float()
  }
  
  // 计算中位数
  let calculate_median = fn(values: Array[Float]) {
    let sorted = values.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
    let len = sorted.length()
    if len % 2 == 1 {
      sorted[len / 2]
    } else {
      (sorted[len / 2 - 1] + sorted[len / 2]) / 2.0
    }
  }
  
  // 计算百分位数
  let calculate_percentile = fn(values: Array[Float], percentile: Float) {
    let sorted = values.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
    let index = ((sorted.length() - 1).to_float() * percentile / 100.0).to_int()
    sorted[index]
  }
  
  // 按端点分组度量
  let group_by_endpoint = fn(metrics: Array[MetricPoint]) {
    let mut groups = []
    let mut processed = []
    
    for metric in metrics {
      if not(processed.contains(metric)) {
        let endpoint = match metric.attributes.find(fn(attr) { attr.0 == "endpoint" }) {
          Some((_, endpoint)) => endpoint
          None => "unknown"
        }
        
        let group_metrics = metrics.filter(fn(m) { 
          match m.attributes.find(fn(attr) { attr.0 == "endpoint" }) {
            Some((_, e)) => e == endpoint
            None => false
          }
        })
        
        groups = groups.push((endpoint, group_metrics))
        processed = processed + group_metrics
      }
    }
    
    groups
  }
  
  // 测试统计计算
  let all_values = metrics.map(fn(m) { m.value })
  let avg_value = calculate_average(all_values)
  let median_value = calculate_median(all_values)
  let p95_value = calculate_percentile(all_values, 95.0)
  let p99_value = calculate_percentile(all_values, 99.0)
  
  assert_eq(avg_value, 127.1)
  assert_eq(median_value, 115.65)
  assert_eq(p95_value, 200.1)
  assert_eq(p99_value, 200.1)
  
  // 测试分组统计
  let grouped_metrics = group_by_endpoint(metrics)
  assert_eq(grouped_metrics.length(), 3)
  
  // 验证用户端点的统计
  let users_metrics = match grouped_metrics.find(fn(group) { group.0 == "/api/users" }) {
    Some((_, metrics)) => metrics
    None => []
  }
  
  assert_eq(users_metrics.length(), 4)
  let users_values = users_metrics.map(fn(m) { m.value })
  let users_avg = calculate_average(users_values)
  let users_median = calculate_median(users_values)
  
  assert_eq(users_avg, 103.075)
  assert_eq(users_median, 105.75)
  
  // 测试聚合函数
  let calculate_aggregates = fn(values: Array[Float]) {
    {
      count: values.length(),
      sum: values.reduce(fn(acc, val) { acc + val }, 0.0),
      min: values.reduce(fn(acc, val) { if val < acc { val } else { acc } }, values[0]),
      max: values.reduce(fn(acc, val) { if val > acc { val } else { acc } }, values[0]),
      average: calculate_average(values),
      median: calculate_median(values),
      p95: calculate_percentile(values, 95.0),
      p99: calculate_percentile(values, 99.0)
    }
  }
  
  let aggregates = calculate_aggregates(all_values)
  assert_eq(aggregates.count, 6)
  assert_eq(aggregates.sum, 762.6)
  assert_eq(aggregates.min, 85.3)
  assert_eq(aggregates.max, 200.1)
}

// 测试3: Span关系和层次管理
test "span relationship and hierarchy management" {
  // 定义Span结构
  type Span = {
    span_id: String,
    parent_span_id: Option[String],
    trace_id: String,
    operation_name: String,
    start_time: Int,
    end_time: Option[Int],
    status: String
  }
  
  // 创建Span层次结构
  let spans = [
    { span_id: "span-001", parent_span_id: None, trace_id: "trace-001", operation_name: "root_operation", start_time: 1000, end_time: Some(5000), status: "ok" },
    { span_id: "span-002", parent_span_id: Some("span-001"), trace_id: "trace-001", operation_name: "database_query", start_time: 1200, end_time: Some(3000), status: "ok" },
    { span_id: "span-003", parent_span_id: Some("span-001"), trace_id: "trace-001", operation_name: "cache_lookup", start_time: 1500, end_time: Some(1800), status: "ok" },
    { span_id: "span-004", parent_span_id: Some("span-002"), trace_id: "trace-001", operation_name: "sql_parse", start_time: 1300, end_time: Some(1600), status: "ok" },
    { span_id: "span-005", parent_span_id: Some("span-002"), trace_id: "trace-001", operation_name: "sql_execute", start_time: 1600, end_time: Some(2800), status: "ok" },
    { span_id: "span-006", parent_span_id: Some("span-005"), trace_id: "trace-001", operation_name: "result_process", start_time: 2000, end_time: Some(2500), status: "ok" }
  ]
  
  // 查找子Span函数
  let find_children = fn(parent_span_id: String, spans: Array[Span]) {
    spans.filter(fn(span) { 
      match span.parent_span_id {
        Some(id) => id == parent_span_id
        None => false
      }
    })
  }
  
  // 查找根Span函数
  let find_root_spans = fn(spans: Array[Span]) {
    spans.filter(fn(span) { 
      match span.parent_span_id {
        None => true
        Some(_) => false
      }
    })
  }
  
  // 计算Span深度函数
  let calculate_span_depth = fn(span: Span, spans: Array[Span]) -> Int {
    match span.parent_span_id {
      None => 0
      Some(parent_id) => {
        match spans.find(fn(s) { s.span_id == parent_id }) {
          Some(parent) => 1 + calculate_span_depth(parent, spans)
          None => 0
        }
      }
    }
  }
  
  // 计算Span路径函数
  let calculate_span_path = fn(span: Span, spans: Array[Span]) -> Array[String] {
    match span.parent_span_id {
      None => [span.span_id]
      Some(parent_id) => {
        match spans.find(fn(s) { s.span_id == parent_id }) {
          Some(parent) => calculate_span_path(parent, spans) + [span.span_id]
          None => [span.span_id]
        }
      }
    }
  }
  
  // 测试查找根Span
  let root_spans = find_root_spans(spans)
  assert_eq(root_spans.length(), 1)
  assert_eq(root_spans[0].span_id, "span-001")
  assert_eq(root_spans[0].operation_name, "root_operation")
  
  // 测试查找子Span
  let span_001_children = find_children("span-001", spans)
  assert_eq(span_001_children.length(), 2)
  assert_true(span_001_children.map(fn(s) { s.span_id }).contains("span-002"))
  assert_true(span_001_children.map(fn(s) { s.span_id }).contains("span-003"))
  
  let span_002_children = find_children("span-002", spans)
  assert_eq(span_002_children.length(), 2)
  assert_true(span_002_children.map(fn(s) { s.span_id }).contains("span-004"))
  assert_true(span_002_children.map(fn(s) { s.span_id }).contains("span-005"))
  
  let span_005_children = find_children("span-005", spans)
  assert_eq(span_005_children.length(), 1)
  assert_eq(span_005_children[0].span_id, "span-006")
  
  // 测试计算Span深度
  let span_001 = spans.find(fn(s) { s.span_id == "span-001" }).unwrap()
  let span_002 = spans.find(fn(s) { s.span_id == "span-002" }).unwrap()
  let span_004 = spans.find(fn(s) { s.span_id == "span-004" }).unwrap()
  let span_006 = spans.find(fn(s) { s.span_id == "span-006" }).unwrap()
  
  assert_eq(calculate_span_depth(span_001, spans), 0)
  assert_eq(calculate_span_depth(span_002, spans), 1)
  assert_eq(calculate_span_depth(span_004, spans), 2)
  assert_eq(calculate_span_depth(span_006, spans), 3)
  
  // 测试计算Span路径
  let path_001 = calculate_span_path(span_001, spans)
  let path_002 = calculate_span_path(span_002, spans)
  let path_004 = calculate_span_path(span_004, spans)
  let path_006 = calculate_span_path(span_006, spans)
  
  assert_eq(path_001, ["span-001"])
  assert_eq(path_002, ["span-001", "span-002"])
  assert_eq(path_004, ["span-001", "span-002", "span-004"])
  assert_eq(path_006, ["span-001", "span-002", "span-005", "span-006"])
  
  // 测试计算Span持续时间
  let calculate_duration = fn(span: Span) {
    match span.end_time {
      Some(end_time) => end_time - span.start_time
      None => -1  // 表示仍在运行
    }
  }
  
  assert_eq(calculate_duration(span_001), 4000)
  assert_eq(calculate_duration(span_002), 1800)
  assert_eq(calculate_duration(span_004), 300)
  assert_eq(calculate_duration(span_006), 500)
  
  // 测试计算总执行时间（并行Span不重复计算）
  let calculate_total_execution_time = fn(spans: Array[Span]) {
    let root_span = find_root_spans(spans)[0]
    let root_children = find_children(root_span.span_id, spans)
    
    // 简化计算：取根Span的持续时间
    calculate_duration(root_span)
  }
  
  let total_time = calculate_total_execution_time(spans)
  assert_eq(total_time, 4000)
}

// 测试4: 上下文传播和复杂数据结构
test "context propagation with complex data structures" {
  // 定义上下文键类型
  type ContextKey = String
  
  // 定义上下文值类型
  type ContextValue = 
    | StringValue(String)
    | IntValue(Int)
    | FloatValue(Float)
    | BoolValue(Bool)
    | ArrayValue(Array[ContextValue])
    | MapValue(Array[(String, ContextValue)])
  
  // 定义上下文结构
  type Context = {
    values: Array[(ContextKey, ContextValue)],
    parent: Option[Context]
  }
  
  // 创建空上下文
  let empty_context = { values: [], parent: None }
  
  // 设置上下文值函数
  let set_context_value = fn(ctx: Context, key: ContextKey, value: ContextValue) {
    let updated_values = ctx.values.filter(fn(kv) { kv.0 != key }) + [(key, value)]
    { ctx | values: updated_values }
  }
  
  // 获取上下文值函数
  let get_context_value = fn(ctx: Context, key: ContextKey) -> Option[ContextValue] {
    match ctx.values.find(fn(kv) { kv.0 == key }) {
      Some((_, value)) => Some(value)
      None => {
        match ctx.parent {
          Some(parent) => get_context_value(parent, key)
          None => None
        }
      }
    }
  }
  
  // 创建子上下文函数
  let create_child_context = fn(parent: Context) {
    { values: [], parent: Some(parent) }
  }
  
  // 测试基本上下文操作
  let ctx1 = set_context_value(empty_context, "trace.id", StringValue("trace-001"))
  let ctx2 = set_context_value(ctx1, "user.id", StringValue("user-123"))
  let ctx3 = set_context_value(ctx2, "request.id", StringValue("req-456"))
  
  assert_eq(get_context_value(ctx3, "trace.id"), Some(StringValue("trace-001")))
  assert_eq(get_context_value(ctx3, "user.id"), Some(StringValue("user-123")))
  assert_eq(get_context_value(ctx3, "request.id"), Some(StringValue("req-456")))
  assert_eq(get_context_value(ctx3, "missing.key"), None)
  
  // 测试上下文层次结构
  let child_ctx = create_child_context(ctx3)
  let child_ctx_with_value = set_context_value(child_ctx, "operation.name", StringValue("database_query"))
  
  // 子上下文应该能访问父上下文的值
  assert_eq(get_context_value(child_ctx_with_value, "trace.id"), Some(StringValue("trace-001")))
  assert_eq(get_context_value(child_ctx_with_value, "user.id"), Some(StringValue("user-123")))
  assert_eq(get_context_value(child_ctx_with_value, "operation.name"), Some(StringValue("database_query")))
  
  // 测试复杂数据结构
  let complex_value = MapValue([
    ("service", StringValue("payment-service")),
    ("version", StringValue("1.2.3")),
    ("metadata", MapValue([
      ("region", StringValue("us-west-2")),
      ("instance", StringValue("i-1234567890abcdef0")),
      ("tags", ArrayValue([
        StringValue("production"),
        StringValue("critical"),
        StringValue("monitored")
      ]))
    ])),
    ("metrics", MapValue([
      ("cpu", FloatValue(75.5)),
      ("memory", FloatValue(68.2)),
      ("request_count", IntValue(10000))
    ]))
  ])
  
  let ctx_with_complex = set_context_value(empty_context, "service.info", complex_value)
  
  // 测试复杂值的获取和解析
  let service_info = get_context_value(ctx_with_complex, "service.info")
  match service_info {
    Some(MapValue(pairs)) => {
      assert_eq(pairs.length(), 4)
      assert_true(pairs.contains(("service", StringValue("payment-service"))))
      assert_true(pairs.contains(("version", StringValue("1.2.3"))))
      
      // 测试嵌套结构访问
      match pairs.find(fn(kv) { kv.0 == "metadata" }) {
        Some((_, MapValue(metadata_pairs))) => {
          assert_eq(metadata_pairs.length(), 3)
          assert_true(metadata_pairs.contains(("region", StringValue("us-west-2"))))
          
          match metadata_pairs.find(fn(kv) { kv.0 == "tags" }) {
            Some((_, ArrayValue(tags))) => {
              assert_eq(tags.length(), 3)
              assert_true(tags.contains(StringValue("production")))
              assert_true(tags.contains(StringValue("critical")))
              assert_true(tags.contains(StringValue("monitored")))
            }
            _ => assert_true(false)
          }
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 测试上下文合并
  let merge_contexts = fn(ctx1: Context, ctx2: Context) {
    let merged_values = ctx1.values + ctx2.values.filter(fn(kv2) { 
      not(ctx1.values.some(fn(kv1) { kv1.0 == kv2.0 }))
    })
    { values: merged_values, parent: None }
  }
  
  let additional_ctx = set_context_value(empty_context, "session.id", StringValue("session-789"))
  let merged_ctx = merge_contexts(ctx3, additional_ctx)
  
  assert_eq(get_context_value(merged_ctx, "trace.id"), Some(StringValue("trace-001")))
  assert_eq(get_context_value(merged_ctx, "user.id"), Some(StringValue("user-123")))
  assert_eq(get_context_value(merged_ctx, "request.id"), Some(StringValue("req-456")))
  assert_eq(get_context_value(merged_ctx, "session.id"), Some(StringValue("session-789")))
}

// 测试5: 资源管理和清理
test "resource management and cleanup" {
  // 定义资源状态
  type ResourceState = 
    | Active
    | Idle
    | Closing
    | Closed
    | Error(String)
  
  // 定义资源结构
  type Resource = {
    id: String,
    resource_type: String,
    state: ResourceState,
    created_at: Int,
    last_accessed: Int,
    reference_count: Int,
    cleanup_actions: Array[String]
  }
  
  // 创建资源
  let create_resource = fn(id: String, resource_type: String) {
    {
      id,
      resource_type,
      state: Active,
      created_at: 1640995200,
      last_accessed: 1640995200,
      reference_count: 1,
      cleanup_actions: []
    }
  }
  
  // 增加引用计数
  let add_reference = fn(resource: Resource) {
    { resource | 
      reference_count: resource.reference_count + 1,
      last_accessed: 1640995300
    }
  }
  
  // 释放引用
  let release_reference = fn(resource: Resource) {
    let new_count = resource.reference_count - 1
    {
      resource | 
      reference_count: new_count,
      state: if new_count <= 0 { Closing } else { resource.state },
      last_accessed: 1640995400
    }
  }
  
  // 添加清理操作
  let add_cleanup_action = fn(resource: Resource, action: String) {
    { resource | cleanup_actions: resource.cleanup_actions + [action] }
  }
  
  // 执行清理
  let cleanup_resource = fn(resource: Resource) {
    if resource.reference_count <= 0 {
      {
        resource | 
        state: Closed,
        cleanup_actions: resource.cleanup_actions + ["cleanup_completed"]
      }
    } else {
      {
        resource | 
        state: Error("Cannot cleanup resource with active references")
      }
    }
  }
  
  // 测试资源生命周期
  let resource1 = create_resource("res-001", "database_connection")
  assert_eq(resource1.id, "res-001")
  assert_eq(resource1.resource_type, "database_connection")
  assert_eq(resource1.reference_count, 1)
  
  // 测试引用计数
  let resource1_with_ref = add_reference(resource1)
  assert_eq(resource1_with_ref.reference_count, 2)
  
  let resource1_released = release_reference(resource1_with_ref)
  assert_eq(resource1_released.reference_count, 1)
  
  // 测试完全释放
  let resource1_fully_released = release_reference(resource1_released)
  assert_eq(resource1_fully_released.reference_count, 0)
  match resource1_fully_released.state {
    Closing => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试清理操作
  let resource_with_cleanup = add_cleanup_action(resource1_fully_released, "close_database_connection")
  let cleaned_resource = cleanup_resource(resource_with_cleanup)
  
  match cleaned_resource.state {
    Closed => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(cleaned_resource.cleanup_actions.contains("close_database_connection"))
  assert_true(cleaned_resource.cleanup_actions.contains("cleanup_completed"))
  
  // 测试资源池管理
  type ResourcePool = {
    resources: Array[Resource],
    max_size: Int,
    cleanup_threshold: Int
  }
  
  let create_resource_pool = fn(max_size: Int, cleanup_threshold: Int) {
    { resources: [], max_size, cleanup_threshold }
  }
  
  let add_to_pool = fn(pool: ResourcePool, resource: Resource) {
    if pool.resources.length() < pool.max_size {
      { pool | resources: pool.resources + [resource] }
    } else {
      pool  // 池已满，不添加资源
    }
  }
  
  let cleanup_idle_resources = fn(pool: ResourcePool, idle_threshold: Int) {
    let now = 1640995500
    let { active, idle } = pool.resources.partition(fn(resource) {
      (now - resource.last_accessed) <= idle_threshold
    })
    
    let cleaned_idle = idle.map(cleanup_resource)
    { pool | resources: active + cleaned_idle }
  }
  
  // 测试资源池操作
  let pool = create_resource_pool(5, 3)
  
  let res1 = create_resource("res-001", "database_connection")
  let res2 = create_resource("res-002", "cache_connection")
  let res3 = create_resource("res-003", "http_client")
  
  let pool_with_resources = add_to_pool(add_to_pool(add_to_pool(pool, res1), res2), res3)
  assert_eq(pool_with_resources.resources.length(), 3)
  
  // 释放一些资源
  let released_res1 = release_reference(res1)
  let released_res2 = release_reference(res2)
  
  let pool_with_released = { pool_with_resources | 
    resources: [released_res1, released_res2, res3] 
  }
  
  // 清理空闲资源
  let cleaned_pool = cleanup_idle_resources(pool_with_released, 100)
  let closed_resources = cleaned_pool.resources.filter(fn(r) { 
    match r.state { Closed => true _ => false }
  })
  
  assert_eq(closed_resources.length(), 2)
  
  // 测试错误情况 - 尝试清理有活跃引用的资源
  let active_resource = create_resource("res-004", "file_handle")
  let active_with_refs = add_reference(add_reference(active_resource))
  let cleanup_attempt = cleanup_resource(active_with_refs)
  
  match cleanup_attempt.state {
    Error(msg) => assert_eq(msg, "Cannot cleanup resource with active references")
    _ => assert_true(false)
  }
}

// 测试6: 性能基准测试
test "performance benchmarking" {
  // 定义性能指标
  type PerformanceMetric = {
    operation: String,
    execution_time: Int,  // 毫秒
    memory_usage: Int,    // KB
    cpu_usage: Float,     // 百分比
    timestamp: Int
  }
  
  // 创建基准测试数据
  let benchmark_data = [
    { operation: "telemetry_serialize", execution_time: 15, memory_usage: 1024, cpu_usage: 25.5, timestamp: 1640995200 },
    { operation: "telemetry_serialize", execution_time: 18, memory_usage: 1100, cpu_usage: 28.2, timestamp: 1640995260 },
    { operation: "telemetry_serialize", execution_time: 12, memory_usage: 980, cpu_usage: 22.1, timestamp: 1640995320 },
    { operation: "telemetry_serialize", execution_time: 20, memory_usage: 1200, cpu_usage: 30.0, timestamp: 1640995380 },
    { operation: "telemetry_serialize", execution_time: 14, memory_usage: 950, cpu_usage: 24.8, timestamp: 1640995440 },
    { operation: "context_propagation", execution_time: 8, memory_usage: 512, cpu_usage: 15.2, timestamp: 1640995500 },
    { operation: "context_propagation", execution_time: 10, memory_usage: 580, cpu_usage: 18.5, timestamp: 1640995560 },
    { operation: "context_propagation", execution_time: 7, memory_usage: 490, cpu_usage: 14.1, timestamp: 1640995620 },
    { operation: "context_propagation", execution_time: 9, memory_usage: 550, cpu_usage: 16.8, timestamp: 1640995680 },
    { operation: "context_propagation", execution_time: 11, memory_usage: 600, cpu_usage: 19.2, timestamp: 1640995740 }
  ]
  
  // 按操作分组
  let group_by_operation = fn(metrics: Array[PerformanceMetric]) {
    let mut groups = []
    let mut processed_operations = []
    
    for metric in metrics {
      if not(processed_operations.contains(metric.operation)) {
        let operation_metrics = metrics.filter(fn(m) { m.operation == metric.operation })
        groups = groups.push((metric.operation, operation_metrics))
        processed_operations = processed_operations + [metric.operation]
      }
    }
    
    groups
  }
  
  // 计算性能统计
  let calculate_performance_stats = fn(metrics: Array[PerformanceMetric]) {
    let execution_times = metrics.map(fn(m) { m.execution_time })
    let memory_usages = metrics.map(fn(m) { m.memory_usage })
    let cpu_usages = metrics.map(fn(m) { m.cpu_usage })
    
    let avg_execution_time = execution_times.reduce(fn(acc, time) { acc + time }, 0) / execution_times.length()
    let min_execution_time = execution_times.reduce(fn(acc, time) { if time < acc { time } else { acc } }, execution_times[0])
    let max_execution_time = execution_times.reduce(fn(acc, time) { if time > acc { time } else { acc } }, execution_times[0])
    
    let avg_memory_usage = memory_usages.reduce(fn(acc, mem) { acc + mem }, 0) / memory_usages.length()
    let min_memory_usage = memory_usages.reduce(fn(acc, mem) { if mem < acc { mem } else { acc } }, memory_usages[0])
    let max_memory_usage = memory_usages.reduce(fn(acc, mem) { if mem > acc { mem } else { acc } }, memory_usages[0])
    
    let avg_cpu_usage = cpu_usages.reduce(fn(acc, cpu) { acc + cpu }, 0.0) / cpu_usages.length().to_float()
    let min_cpu_usage = cpu_usages.reduce(fn(acc, cpu) { if cpu < acc { cpu } else { acc } }, cpu_usages[0])
    let max_cpu_usage = cpu_usages.reduce(fn(acc, cpu) { if cpu > acc { cpu } else { acc } }, cpu_usages[0])
    
    {
      operation: metrics[0].operation,
      sample_count: metrics.length(),
      execution_time: { avg: avg_execution_time, min: min_execution_time, max: max_execution_time },
      memory_usage: { avg: avg_memory_usage, min: min_memory_usage, max: max_memory_usage },
      cpu_usage: { avg: avg_cpu_usage, min: min_cpu_usage, max: max_cpu_usage }
    }
  }
  
  // 性能回归检测
  let detect_performance_regression = fn(current_stats, baseline_stats, threshold_percentage: Float) {
    let execution_regression = (current_stats.execution_time.avg - baseline_stats.execution_time.avg).to_float() / baseline_stats.execution_time.avg.to_float() * 100.0
    let memory_regression = (current_stats.memory_usage.avg - baseline_stats.memory_usage.avg).to_float() / baseline_stats.memory_usage.avg.to_float() * 100.0
    let cpu_regression = (current_stats.cpu_usage.avg - baseline_stats.cpu_usage.avg) / baseline_stats.cpu_usage.avg * 100.0
    
    {
      has_regression: execution_regression > threshold_percentage or 
                     memory_regression > threshold_percentage or 
                     cpu_regression > threshold_percentage,
      execution_regression,
      memory_regression,
      cpu_regression
    }
  }
  
  // 测试分组和统计计算
  let grouped_metrics = group_by_operation(benchmark_data)
  assert_eq(grouped_metrics.length(), 2)
  
  let serialize_stats = calculate_performance_stats(
    match grouped_metrics.find(fn(g) { g.0 == "telemetry_serialize" }) {
      Some((_, metrics)) => metrics
      None => []
    }
  )
  
  let context_stats = calculate_performance_stats(
    match grouped_metrics.find(fn(g) { g.0 == "context_propagation" }) {
      Some((_, metrics)) => metrics
      None => []
    }
  )
  
  // 验证序列化统计
  assert_eq(serialize_stats.operation, "telemetry_serialize")
  assert_eq(serialize_stats.sample_count, 5)
  assert_eq(serialize_stats.execution_time.avg, 15)
  assert_eq(serialize_stats.execution_time.min, 12)
  assert_eq(serialize_stats.execution_time.max, 20)
  assert_eq(serialize_stats.memory_usage.avg, 1050)
  assert_eq(serialize_stats.memory_usage.min, 950)
  assert_eq(serialize_stats.memory_usage.max, 1200)
  assert_eq(serialize_stats.cpu_usage.avg, 26.12)
  assert_eq(serialize_stats.cpu_usage.min, 22.1)
  assert_eq(serialize_stats.cpu_usage.max, 30.0)
  
  // 验证上下文传播统计
  assert_eq(context_stats.operation, "context_propagation")
  assert_eq(context_stats.sample_count, 5)
  assert_eq(context_stats.execution_time.avg, 9)
  assert_eq(context_stats.execution_time.min, 7)
  assert_eq(context_stats.execution_time.max, 11)
  assert_eq(context_stats.memory_usage.avg, 546)
  assert_eq(context_stats.memory_usage.min, 490)
  assert_eq(context_stats.memory_usage.max, 600)
  assert_eq(context_stats.cpu_usage.avg, 16.76)
  assert_eq(context_stats.cpu_usage.min, 14.1)
  assert_eq(context_stats.cpu_usage.max, 19.2)
  
  // 测试性能回归检测
  let baseline_serialize = { serialize_stats | 
    execution_time: { avg: 14, min: 10, max: 18 },
    memory_usage: { avg: 1000, min: 900, max: 1100 },
    cpu_usage: { avg: 25.0, min: 20.0, max: 30.0 }
  }
  
  let regression_result = detect_performance_regression(serialize_stats, baseline_serialize, 10.0)
  assert_false(regression_result.has_regression)
  assert_eq(regression_result.execution_regression, 7.14)
  assert_eq(regression_result.memory_regression, 5.0)
  assert_eq(regression_result.cpu_usage.avg, 4.48)
  
  // 测试有明显回归的情况
  let degraded_serialize = { serialize_stats | 
    execution_time: { avg: 20, min: 18, max: 25 },
    memory_usage: { avg: 1300, min: 1200, max: 1400 },
    cpu_usage: { avg: 35.0, min: 30.0, max: 40.0 }
  }
  
  let regression_result2 = detect_performance_regression(degraded_serialize, baseline_serialize, 10.0)
  assert_true(regression_result2.has_regression)
  assert_eq(regression_result2.execution_regression, 42.86)
  assert_eq(regression_result2.memory_regression, 30.0)
  assert_eq(regression_result2.cpu_usage.avg, 40.0)
}

// 测试7: 错误处理和恢复
test "error handling and recovery" {
  // 定义错误类型
  type ErrorType = 
    | NetworkError(String)
    | DatabaseError(String)
    | ValidationError(String)
    | TimeoutError(String)
    | ResourceExhaustedError(String)
    | UnknownError(String)
  
  // 定义操作结果
  type OperationResult[T] = 
    | Success(T)
    | Failure(ErrorType)
    | Retryable(ErrorType, Int)  // 错误类型，重试次数
  
  // 安全执行函数
  let safe_execute = fn[T](operation: () -> OperationResult[T], fallback: () -> T) {
    match operation() {
      Success(value) => Success(value)
      Failure(error) => Success(fallback())
      Retryable(error, retry_count) => {
        if retry_count > 0 {
          // 在实际实现中，这里会有延迟和重试逻辑
          safe_execute(operation, fallback)
        } else {
          Success(fallback())
        }
      }
    }
  }
  
  // 错误分类函数
  let categorize_error = fn(error: ErrorType) {
    match error {
      NetworkError(_) => "transient"
      DatabaseError(_) => "persistent"
      ValidationError(_) => "client"
      TimeoutError(_) => "transient"
      ResourceExhaustedError(_) => "resource"
      UnknownError(_) => "unknown"
    }
  }
  
  // 错误恢复策略
  let apply_recovery_strategy = fn(error: ErrorType) {
    let category = categorize_error(error)
    match category {
      "transient" => "retry_with_backoff"
      "persistent" => "fail_fast"
      "client" => "return_validation_error"
      "resource" => "throttle_requests"
      "unknown" => "log_and_continue"
      _ => "default_recovery"
    }
  }
  
  // 测试错误处理
  let network_operation = fn() -> OperationResult[String] {
    Failure(NetworkError("Connection timeout"))
  }
  
  let database_operation = fn() -> OperationResult[String] {
    Failure(DatabaseError("Connection pool exhausted"))
  }
  
  let validation_operation = fn() -> OperationResult[String] {
    Failure(ValidationError("Invalid input parameter"))
  }
  
  let timeout_operation = fn() -> OperationResult[String] {
    Retryable(TimeoutError("Operation timed out"), 2)
  }
  
  let successful_operation = fn() -> OperationResult[String] {
    Success("Operation completed successfully")
  }
  
  let fallback_operation = fn() { "Fallback result" }
  
  // 测试安全执行
  let network_result = safe_execute(network_operation, fallback_operation)
  match network_result {
    Success(value) => assert_eq(value, "Fallback result")
    _ => assert_true(false)
  }
  
  let database_result = safe_execute(database_operation, fallback_operation)
  match database_result {
    Success(value) => assert_eq(value, "Fallback result")
    _ => assert_true(false)
  }
  
  let validation_result = safe_execute(validation_operation, fallback_operation)
  match validation_result {
    Success(value) => assert_eq(value, "Fallback result")
    _ => assert_true(false)
  }
  
  let timeout_result = safe_execute(timeout_operation, fallback_operation)
  match timeout_result {
    Success(value) => assert_eq(value, "Fallback result")
    _ => assert_true(false)
  }
  
  let success_result = safe_execute(successful_operation, fallback_operation)
  match success_result {
    Success(value) => assert_eq(value, "Operation completed successfully")
    _ => assert_true(false)
  }
  
  // 测试错误分类
  assert_eq(categorize_error(NetworkError("Connection failed")), "transient")
  assert_eq(categorize_error(DatabaseError("Query failed")), "persistent")
  assert_eq(categorize_error(ValidationError("Invalid data")), "client")
  assert_eq(categorize_error(TimeoutError("Request timeout")), "transient")
  assert_eq(categorize_error(ResourceExhaustedError("Memory limit exceeded")), "resource")
  assert_eq(categorize_error(UnknownError("Unexpected error")), "unknown")
  
  // 测试恢复策略
  assert_eq(apply_recovery_strategy(NetworkError("Connection failed")), "retry_with_backoff")
  assert_eq(apply_recovery_strategy(DatabaseError("Query failed")), "fail_fast")
  assert_eq(apply_recovery_strategy(ValidationError("Invalid data")), "return_validation_error")
  assert_eq(apply_recovery_strategy(TimeoutError("Request timeout")), "retry_with_backoff")
  assert_eq(apply_recovery_strategy(ResourceExhaustedError("Memory limit exceeded")), "throttle_requests")
  assert_eq(apply_recovery_strategy(UnknownError("Unexpected error")), "log_and_continue")
  
  // 测试错误链和错误聚合
  type ErrorChain = {
    primary_error: ErrorType,
    secondary_errors: Array[ErrorType],
    context: Array[(String, String)]
  }
  
  let create_error_chain = fn(primary: ErrorType, secondary: Array[ErrorType], context: Array[(String, String)]) {
    { primary_error: primary, secondary_errors: secondary, context }
  }
  
  let error_chain = create_error_chain(
    NetworkError("Primary network failure"),
    [
      TimeoutError("Sub-operation timeout"),
      DatabaseError("Database connection lost during retry")
    ],
    [
      ("operation", "data_sync"),
      ("retry_count", "3"),
      ("duration_ms", "5000")
    ]
  )
  
  assert_eq(error_chain.secondary_errors.length(), 2)
  assert_eq(error_chain.context.length(), 3)
  
  // 测试错误恢复管道
  let recovery_pipeline = fn(error_chain: ErrorChain) {
    let primary_recovery = apply_recovery_strategy(error_chain.primary_error)
    let secondary_recoveries = error_chain.secondary_errors.map(apply_recovery_strategy)
    
    {
      primary_strategy: primary_recovery,
      secondary_strategies: secondary_recoveries,
      combined_strategy: if primary_recovery == "fail_fast" {
        "fail_fast"
      } else if secondary_recoveries.some(fn(s) { s == "fail_fast" }) {
        "partial_recovery"
      } else {
        "comprehensive_recovery"
      }
    }
  }
  
  let pipeline_result = recovery_pipeline(error_chain)
  assert_eq(pipeline_result.primary_strategy, "retry_with_backoff")
  assert_eq(pipeline_result.secondary_strategies[0], "retry_with_backoff")
  assert_eq(pipeline_result.secondary_strategies[1], "fail_fast")
  assert_eq(pipeline_result.combined_strategy, "partial_recovery")
}

// 测试8: 多租户数据隔离
test "multi-tenant data isolation" {
  // 定义租户上下文
  type TenantContext = {
    tenant_id: String,
    namespace: String,
    isolation_level: String,  // "strict", "medium", "basic"
    permissions: Array[String]
  }
  
  // 定义隔离的数据点
  type IsolatedData = {
    id: String,
    tenant_context: TenantContext,
    data: String,
    visibility: String  // "private", "shared", "public"
  }
  
  // 创建租户上下文
  let create_tenant_context = fn(tenant_id: String, namespace: String, isolation_level: String, permissions: Array[String>) {
    { tenant_id, namespace, isolation_level, permissions }
  }
  
  // 创建隔离数据
  let create_isolated_data = fn(id: String, tenant_ctx: TenantContext, data: String, visibility: String) {
    { id, tenant_context: tenant_ctx, data, visibility }
  }
  
  // 检查数据访问权限
  let can_access_data = fn(data: IsolatedData, requesting_tenant: TenantContext) {
    let same_tenant = data.tenant_context.tenant_id == requesting_tenant.tenant_id
    
    match data.visibility {
      "public" => true
      "shared" => same_tenant or requesting_tenant.permissions.contains("access_shared_data")
      "private" => same_tenant and requesting_tenant.permissions.contains("access_private_data")
      _ => false
    }
  }
  
  // 应用数据过滤
  let filter_tenant_data = fn(all_data: Array[IsolatedData], requesting_tenant: TenantContext) {
    all_data.filter(fn(data) { can_access_data(data, requesting_tenant) })
  }
  
  // 数据脱敏
  let sanitize_data = fn(data: IsolatedData, requesting_tenant: TenantContext) {
    let can_view_raw = data.tenant_context.tenant_id == requesting_tenant.tenant_id and 
                      requesting_tenant.permissions.contains("view_raw_data")
    
    if can_view_raw {
      data.data
    } else {
      // 简单脱敏：只显示前3个字符
      if data.data.length() > 3 {
        data.data.substring(0, 3) + "***"
      } else {
        "***"
      }
    }
  }
  
  // 创建测试租户
  let tenant_a = create_tenant_context("tenant-a", "production", "strict", ["access_private_data", "view_raw_data"])
  let tenant_b = create_tenant_context("tenant-b", "production", "strict", ["access_shared_data"])
  let tenant_c = create_tenant_context("tenant-c", "staging", "medium", ["access_private_data"])
  
  // 创建测试数据
  let data_a_private = create_isolated_data("data-001", tenant_a, "Sensitive data for tenant A", "private")
  let data_a_shared = create_isolated_data("data-002", tenant_a, "Shared data from tenant A", "shared")
  let data_a_public = create_isolated_data("data-003", tenant_a, "Public data from tenant A", "public")
  
  let data_b_private = create_isolated_data("data-004", tenant_b, "Sensitive data for tenant B", "private")
  let data_b_shared = create_isolated_data("data-005", tenant_b, "Shared data from tenant B", "shared")
  let data_b_public = create_isolated_data("data-006", tenant_b, "Public data from tenant B", "public")
  
  let data_c_private = create_isolated_data("data-007", tenant_c, "Sensitive data for tenant C", "private")
  let data_c_shared = create_isolated_data("data-008", tenant_c, "Shared data from tenant C", "shared")
  
  let all_data = [
    data_a_private, data_a_shared, data_a_public,
    data_b_private, data_b_shared, data_b_public,
    data_c_private, data_c_shared
  ]
  
  // 测试租户A的数据访问
  let tenant_a_accessible = filter_tenant_data(all_data, tenant_a)
  assert_eq(tenant_a_accessible.length(), 5)  // 自己的所有数据 + 其他租户的public数据
  
  // 测试租户B的数据访问
  let tenant_b_accessible = filter_tenant_data(all_data, tenant_b)
  assert_eq(tenant_b_accessible.length(), 4)  // 自己的shared和public数据 + 其他租户的public数据
  
  // 测试租户C的数据访问
  let tenant_c_accessible = filter_tenant_data(all_data, tenant_c)
  assert_eq(tenant_c_accessible.length(), 4)  // 自己的private和shared数据 + 其他租户的public数据
  
  // 测试数据脱敏
  let sanitized_for_a = sanitize_data(data_b_private, tenant_a)
  let sanitized_for_b = sanitize_data(data_b_private, tenant_b)
  let sanitized_for_c = sanitize_data(data_b_private, tenant_c)
  
  assert_eq(sanitized_for_a, "Sen***")  // 不同租户，脱敏
  assert_eq(sanitized_for_b, "Sensitive data for tenant B")  // 自己的数据，不脱敏
  assert_eq(sanitized_for_c, "Sen***")  // 不同租户，脱敏
  
  // 测试命名空间隔离
  let filter_by_namespace = fn(data: Array[IsolatedData], namespace: String) {
    data.filter(fn(d) { d.tenant_context.namespace == namespace })
  }
  
  let production_data = filter_by_namespace(all_data, "production")
  let staging_data = filter_by_namespace(all_data, "staging")
  
  assert_eq(production_data.length(), 6)
  assert_eq(staging_data.length(), 2)
  
  // 测试隔离级别
  let apply_isolation_policy = fn(data: IsolatedData, requesting_tenant: TenantContext) {
    let isolation_level = requesting_tenant.isolation_level
    
    match isolation_level {
      "strict" => {
        // 严格隔离：只能访问明确授权的数据
        can_access_data(data, requesting_tenant)
      }
      "medium" => {
        // 中等隔离：可以访问同命名空间的共享数据
        let same_namespace = data.tenant_context.namespace == requesting_tenant.namespace
        (same_namespace and data.visibility == "shared") or can_access_data(data, requesting_tenant)
      }
      "basic" => {
        // 基本隔离：可以访问所有非私有数据
        data.visibility != "private" or can_access_data(data, requesting_tenant)
      }
      _ => can_access_data(data, requesting_tenant)
    }
  }
  
  // 创建中等隔离级别的租户
  let tenant_d = create_tenant_context("tenant-d", "production", "medium", ["access_shared_data"])
  
  // 测试不同隔离级别的访问控制
  let can_a_access_b_shared = apply_isolation_policy(data_b_shared, tenant_a)
  let can_d_access_b_shared = apply_isolation_policy(data_b_shared, tenant_d)
  
  assert_false(can_a_access_b_shared)  // 严格隔离，不能访问其他租户的共享数据
  assert_true(can_d_access_b_shared)   // 中等隔离，可以访问同命名空间的共享数据
}

// 测试9: 实时流处理
test "real-time stream processing" {
  // 定义流事件
  type StreamEvent = 
    | MetricEvent(String, Float, Int)  // name, value, timestamp
    | LogEvent(String, String, Int)    // level, message, timestamp
    | TraceEvent(String, String, Int)  // trace_id, span_id, timestamp
    | AlertEvent(String, String, Int)  // alert_type, message, timestamp
  
  // 定义窗口操作
  type WindowOperation = 
    | Count
    | Sum
    | Average
    | Min
    | Max
    | Custom(String)
  
  // 定义流处理器
  type StreamProcessor = {
    event_buffer: Array[StreamEvent],
    window_size: Int,
    operations: Array[WindowOperation],
    last_processed: Int
  }
  
  // 创建流处理器
  let create_stream_processor = fn(window_size: Int, operations: Array[WindowOperation]) {
    {
      event_buffer: [],
      window_size,
      operations,
      last_processed: 0
    }
  }
  
  // 添加事件到缓冲区
  let add_event = fn(processor: StreamProcessor, event: StreamEvent) {
    let updated_buffer = processor.event_buffer + [event]
    // 保持缓冲区大小在限制内
    let trimmed_buffer = if updated_buffer.length() > processor.window_size * 2 {
      updated_buffer.slice(updated_buffer.length() - processor.window_size * 2, updated_buffer.length())
    } else {
      updated_buffer
    }
    { processor | event_buffer: trimmed_buffer }
  }
  
  // 获取时间窗口内的事件
  let get_window_events = fn(processor: StreamProcessor, window_end: Int) {
    let window_start = window_end - processor.window_size
    processor.event_buffer.filter(fn(event) {
      let timestamp = match event {
        MetricEvent(_, _, ts) => ts
        LogEvent(_, _, ts) => ts
        TraceEvent(_, _, ts) => ts
        AlertEvent(_, _, ts) => ts
      }
      timestamp >= window_start and timestamp <= window_end
    })
  }
  
  // 应用窗口操作
  let apply_window_operation = fn(events: Array[StreamEvent], operation: WindowOperation) {
    match operation {
      Count => events.length().to_float()
      Sum => {
        let mut sum = 0.0
        for event in events {
          match event {
            MetricEvent(_, value, _) => sum = sum + value
            _ => ()
          }
        }
        sum
      }
      Average => {
        let mut sum = 0.0
        let mut count = 0
        for event in events {
          match event {
            MetricEvent(_, value, _) => {
              sum = sum + value
              count = count + 1
            }
            _ => ()
          }
        }
        if count > 0 { sum / count.to_float() } else { 0.0 }
      }
      Min => {
        let mut min = Float::max_value()
        let mut has_value = false
        for event in events {
          match event {
            MetricEvent(_, value, _) => {
              if value < min { min = value }
              has_value = true
            }
            _ => ()
          }
        }
        if has_value { min } else { 0.0 }
      }
      Max => {
        let mut max = Float::min_value()
        let mut has_value = false
        for event in events {
          match event {
            MetricEvent(_, value, _) => {
              if value > max { max = value }
              has_value = true
            }
            _ => ()
          }
        }
        if has_value { max } else { 0.0 }
      }
      Custom(_) => 0.0  // 简化实现
    }
  }
  
  // 处理时间窗口
  let process_time_window = fn(processor: StreamProcessor, window_end: Int) {
    let window_events = get_window_events(processor, window_end)
    let results = processor.operations.map(fn(op) { apply_window_operation(window_events, op) })
    { processor | last_processed: window_end, results }
  }
  
  // 创建测试流处理器
  let processor = create_stream_processor(1000, [Count, Sum, Average, Min, Max])  // 1秒窗口
  
  // 添加测试事件
  let events = [
    MetricEvent("cpu_usage", 65.5, 1640995200),
    LogEvent("info", "Application started", 1640995205),
    MetricEvent("cpu_usage", 70.2, 1640995210),
    TraceEvent("trace-001", "span-001", 1640995215),
    MetricEvent("cpu_usage", 68.8, 1640995220),
    LogEvent("warn", "High memory usage", 1640995230),
    MetricEvent("cpu_usage", 75.1, 1640995240),
    AlertEvent("performance", "CPU usage spike", 1640995250),
    MetricEvent("cpu_usage", 72.3, 1640995260),
    LogEvent("info", "Cache cleared", 1640995270)
  ]
  
  // 将事件添加到处理器
  let processor_with_events = events.reduce(fn(acc, event) { add_event(acc, event) }, processor)
  assert_eq(processor_with_events.event_buffer.length(), 10)
  
  // 处理时间窗口
  let window_result = process_time_window(processor_with_events, 1640995300)
  
  // 验证窗口结果
  match window_result.results {
    [count, sum, avg, min, max] => {
      assert_eq(count, 10.0)  // 10个事件
      assert_eq(sum, 351.9)   // 65.5 + 70.2 + 68.8 + 75.1 + 72.3
      assert_eq(avg, 70.38)   // 351.9 / 5
      assert_eq(min, 65.5)    // 最小值
      assert_eq(max, 75.1)    // 最大值
    }
    _ => assert_true(false)
  }
  
  // 测试事件过滤
  let filter_events_by_type = fn(events: Array[StreamEvent], event_type: String) {
    events.filter(fn(event) {
      match (event, event_type) {
        (MetricEvent(_, _, _), "metric") => true
        (LogEvent(_, _, _), "log") => true
        (TraceEvent(_, _, _), "trace") => true
        (AlertEvent(_, _, _), "alert") => true
        _ => false
      }
    })
  }
  
  let metric_events = filter_events_by_type(events, "metric")
  let log_events = filter_events_by_type(events, "log")
  let trace_events = filter_events_by_type(events, "trace")
  let alert_events = filter_events_by_type(events, "alert")
  
  assert_eq(metric_events.length(), 5)
  assert_eq(log_events.length(), 3)
  assert_eq(trace_events.length(), 1)
  assert_eq(alert_events.length(), 1)
  
  // 测试事件聚合
  let aggregate_events = fn(events: Array[StreamEvent]) {
    let mut metric_count = 0
    let mut log_count = 0
    let mut trace_count = 0
    let mut alert_count = 0
    
    for event in events {
      match event {
        MetricEvent(_, _, _) => metric_count = metric_count + 1
        LogEvent(_, _, _) => log_count = log_count + 1
        TraceEvent(_, _, _) => trace_count = trace_count + 1
        AlertEvent(_, _, _) => alert_count = alert_count + 1
      }
    }
    
    {
      total_events: events.length(),
      metric_count,
      log_count,
      trace_count,
      alert_count
    }
  }
  
  let aggregation_result = aggregate_events(events)
  assert_eq(aggregation_result.total_events, 10)
  assert_eq(aggregation_result.metric_count, 5)
  assert_eq(aggregation_result.log_count, 3)
  assert_eq(aggregation_result.trace_count, 1)
  assert_eq(aggregation_result.alert_count, 1)
  
  // 测试滑动窗口
  let sliding_window_processor = create_stream_processor(500, [Count])  // 0.5秒窗口
  
  let events_in_time = [
    MetricEvent("cpu_usage", 65.5, 1640995200),
    MetricEvent("cpu_usage", 70.2, 1640995202),
    MetricEvent("cpu_usage", 68.8, 1640995204),
    MetricEvent("cpu_usage", 75.1, 1640995206),
    MetricEvent("cpu_usage", 72.3, 1640995208)
  ]
  
  let sliding_processor = events_in_time.reduce(fn(acc, event) { add_event(acc, event) }, sliding_window_processor)
  
  // 第一个窗口 (1640995200-1640995205)
  let window1_result = process_time_window(sliding_processor, 1640995205)
  match window1_result.results {
    [count] => assert_eq(count, 3.0)  // 前3个事件
    _ => assert_true(false)
  }
  
  // 第二个窗口 (1640995203-1640995208)
  let window2_result = process_time_window(sliding_processor, 1640995208)
  match window2_result.results {
    [count] => assert_eq(count, 3.0)  // 后3个事件
    _ => assert_true(false)
  }
}

// 测试10: 配置管理和验证
test "configuration management and validation" {
  // 定义配置值类型
  type ConfigValue = 
    | StringConfig(String)
    | IntConfig(Int)
    | FloatConfig(Float)
    | BoolConfig(Bool)
    | ArrayConfig(Array[String])
    | MapConfig(Array[(String, String)])
  
  // 定义配置项
  type ConfigItem = {
    key: String,
    value: ConfigValue,
    default_value: ConfigValue,
    description: String,
    required: Bool,
    validator: Option[(ConfigValue) -> Bool]
  }
  
  // 定义配置管理器
  type ConfigManager = {
    configs: Array[ConfigItem],
    environment: String
  }
  
  // 创建配置管理器
  let create_config_manager = fn(environment: String) {
    { configs: [], environment }
  }
  
  // 添加配置项
  let add_config = fn(manager: ConfigManager, item: ConfigItem) {
    let updated_configs = manager.configs.filter(fn(c) { c.key != item.key }) + [item]
    { manager | configs: updated_configs }
  }
  
  // 获取配置值
  let get_config_value = fn(manager: ConfigManager, key: String) -> Option[ConfigValue] {
    match manager.configs.find(fn(c) { c.key == key }) {
      Some(item) => Some(item.value)
      None => None
    }
  }
  
  // 验证配置
  let validate_config = fn(item: ConfigItem) {
    match item.validator {
      Some(validator) => validator(item.value)
      None => true
    }
  }
  
  // 验证所有配置
  let validate_all_configs = fn(manager: ConfigManager) {
    let mut validation_results = []
    
    for item in manager.configs {
      let is_valid = validate_config(item)
      validation_results = validation_results + [(item.key, is_valid)]
    }
    
    let invalid_configs = validation_results.filter(fn(result) { not(result.1) })
    
    {
      is_valid: invalid_configs.length() == 0,
      invalid_keys: invalid_configs.map(fn(result) { result.0 }),
      validation_results
    }
  }
  
  // 重置配置为默认值
  let reset_to_default = fn(manager: ConfigManager, key: String) {
    let updated_configs = manager.configs.map(fn(item) {
      if item.key == key {
        { item | value: item.default_value }
      } else {
        item
      }
    })
    { manager | configs: updated_configs }
  }
  
  // 创建测试配置项
  let create_string_config = fn(key: String, value: String, default: String, required: Bool, validator: Option[(String) -> Bool>) {
    {
      key,
      value: StringConfig(value),
      default_value: StringConfig(default),
      description: "String configuration",
      required,
      validator: match validator {
        Some(v) => Some(fn(cv) { match cv { StringConfig(s) => v(s) _ => false } })
        None => None
      }
    }
  }
  
  let create_int_config = fn(key: String, value: Int, default: Int, required: Bool, validator: Option[(Int) -> Bool]) {
    {
      key,
      value: IntConfig(value),
      default_value: IntConfig(default),
      description: "Integer configuration",
      required,
      validator: match validator {
        Some(v) => Some(fn(cv) { match cv { IntConfig(i) => v(i) _ => false } })
        None => None
      }
    }
  }
  
  // 创建配置管理器并添加配置
  let config_manager = create_config_manager("production")
  
  let service_name_config = create_string_config(
    "service.name", 
    "payment-service", 
    "default-service", 
    true, 
    Some(fn(name) { name.length() > 0 and name.length() <= 50 })
  )
  
  let max_connections_config = create_int_config(
    "database.max_connections", 
    100, 
    10, 
    true, 
    Some(fn(val) { val > 0 and val <= 1000 })
  )
  
  let timeout_config = create_int_config(
    "request.timeout_ms", 
    5000, 
    30000, 
    false, 
    Some(fn(val) { val > 0 and val <= 300000 })
  )
  
  let config_with_items = add_config(
    add_config(
      add_config(config_manager, service_name_config),
      max_connections_config
    ),
    timeout_config
  )
  
  // 测试配置获取
  let service_name = get_config_value(config_with_items, "service.name")
  let max_connections = get_config_value(config_with_items, "database.max_connections")
  let timeout = get_config_value(config_with_items, "request.timeout_ms")
  let missing_config = get_config_value(config_with_items, "missing.key")
  
  match service_name {
    Some(StringConfig(name)) => assert_eq(name, "payment-service")
    _ => assert_true(false)
  }
  
  match max_connections {
    Some(IntConfig(val)) => assert_eq(val, 100)
    _ => assert_true(false)
  }
  
  match timeout {
    Some(IntConfig(val)) => assert_eq(val, 5000)
    _ => assert_true(false)
  }
  
  assert_eq(missing_config, None)
  
  // 测试配置验证
  let validation_result = validate_all_configs(config_with_items)
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.invalid_keys.length(), 0)
  
  // 测试无效配置
  let invalid_service_config = create_string_config(
    "service.name", 
    "", 
    "default-service", 
    true, 
    Some(fn(name) { name.length() > 0 and name.length() <= 50 })
  )
  
  let invalid_connections_config = create_int_config(
    "database.max_connections", 
    0, 
    10, 
    true, 
    Some(fn(val) { val > 0 and val <= 1000 })
  )
  
  let config_with_invalid = add_config(
    add_config(config_manager, invalid_service_config),
    invalid_connections_config
  )
  
  let invalid_validation_result = validate_all_configs(config_with_invalid)
  assert_false(invalid_validation_result.is_valid)
  assert_eq(invalid_validation_result.invalid_keys.length(), 2)
  assert_true(invalid_validation_result.invalid_keys.contains("service.name"))
  assert_true(invalid_validation_result.invalid_keys.contains("database.max_connections"))
  
  // 测试重置为默认值
  let reset_config = reset_to_default(config_with_items, "request.timeout_ms")
  let reset_timeout = get_config_value(reset_config, "request.timeout_ms")
  
  match reset_timeout {
    Some(IntConfig(val)) => assert_eq(val, 30000)  // 默认值
    _ => assert_true(false)
  }
  
  // 测试环境特定配置
  let get_environment_config = fn(manager: ConfigManager, key: String, environment: String) -> Option[ConfigValue] {
    let env_key = environment + "." + key
    get_config_value(manager, env_key).or_else(fn() { get_config_value(manager, key) })
  }
  
  let prod_config = create_string_config("production.service.name", "prod-payment-service", "default-service", true, None)
  let config_with_env = add_config(config_with_items, prod_config)
  
  let prod_service_name = get_environment_config(config_with_env, "service.name", "production")
  let dev_service_name = get_environment_config(config_with_env, "service.name", "development")
  
  match prod_service_name {
    Some(StringConfig(name)) => assert_eq(name, "prod-payment-service")  // 环境特定值
    _ => assert_true(false)
  }
  
  match dev_service_name {
    Some(StringConfig(name)) => assert_eq(name, "payment-service")  // 默认值
    _ => assert_true(false)
  }
  
  // 测试配置组
  let get_config_group = fn(manager: ConfigManager, prefix: String) {
    manager.configs.filter(fn(item) { item.key.starts_with(prefix) })
  }
  
  let database_configs = get_config_group(config_with_items, "database")
  assert_eq(database_configs.length(), 1)
  assert_eq(database_configs[0].key, "database.max_connections")
  
  // 测试配置依赖验证
  let validate_config_dependencies = fn(manager: ConfigManager) {
    let mut dependency_issues = []
    
    // 检查池大小不能小于最大连接数
    let pool_size = match get_config_value(manager, "database.pool_size") {
      Some(IntConfig(val)) => Some(val)
      _ => None
    }
    
    let max_conn = match get_config_value(manager, "database.max_connections") {
      Some(IntConfig(val)) => Some(val)
      _ => None
    }
    
    match (pool_size, max_conn) {
      (Some(pool), Some(max)) => {
        if pool < max {
          dependency_issues = dependency_issues + ["database.pool_size should be >= database.max_connections"]
        }
      }
      _ => ()
    }
    
    {
      has_issues: dependency_issues.length() > 0,
      issues: dependency_issues
    }
  }
  
  let pool_size_config = create_int_config("database.pool_size", 50, 100, true, None)
  let config_with_pool = add_config(config_with_items, pool_size_config)
  
  let dependency_validation = validate_config_dependencies(config_with_pool)
  assert_true(dependency_validation.has_issues)
  assert_true(dependency_validation.issues.contains("database.pool_size should be >= database.max_connections"))
}