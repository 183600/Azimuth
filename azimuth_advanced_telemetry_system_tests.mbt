// Azimuth 高级遥测系统测试用例
// 专注于分布式追踪、指标聚合、异常检测等高级功能

// 测试1: 分布式追踪链路管理
test "分布式追踪链路管理" {
  // 定义追踪上下文类型
  type TraceContext {
    trace_id: String
    span_id: String
    parent_span_id: Option[String]
    baggage: Map[String, String]
    flags: Int
  }
  
  // 创建根追踪上下文
  let create_root_context = fn(trace_id: String) -> TraceContext {
    {
      trace_id,
      span_id: "span-" + trace_id.split("-")[1],
      parent_span_id: None,
      baggage: Map::empty(),
      flags: 1
    }
  }
  
  // 创建子追踪上下文
  let create_child_context = fn(parent: TraceContext, operation_name: String) -> TraceContext {
    let child_span_id = "span-" + operation_name + "-" + parent.span_id.split("-")[1]
    {
      trace_id: parent.trace_id,
      span_id: child_span_id,
      parent_span_id: Some(parent.span_id),
      baggage: parent.baggage,
      flags: parent.flags
    }
  }
  
  // 添加追踪行李信息
  let add_baggage = fn(context: TraceContext, key: String, value: String) -> TraceContext {
    {
      trace_id: context.trace_id,
      span_id: context.span_id,
      parent_span_id: context.parent_span_id,
      baggage: Map::insert(context.baggage, key, value),
      flags: context.flags
    }
  }
  
  // 测试根追踪上下文创建
  let root_context = create_root_context("trace-12345")
  assert_eq(root_context.trace_id, "trace-12345")
  assert_eq(root_context.span_id, "span-12345")
  assert_eq(root_context.parent_span_id, None)
  assert_eq(root_context.baggage.size(), 0)
  
  // 测试子追踪上下文创建
  let child_context = create_child_context(root_context, "database.query")
  assert_eq(child_context.trace_id, "trace-12345")
  assert_eq(child_context.span_id, "span-database.query-12345")
  assert_eq(child_context.parent_span_id, Some("span-12345"))
  
  // 测试行李信息添加
  let context_with_baggage = add_baggage(root_context, "user.id", "user123")
  assert_eq(context_with_baggage.baggage.size(), 1)
  
  match Map::get(context_with_baggage.baggage, "user.id") {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  // 测试多级追踪链路
  let service_context = create_child_context(root_context, "api.request")
  let db_context = create_child_context(service_context, "database.query")
  let cache_context = create_child_context(db_context, "cache.lookup")
  
  assert_eq(cache_context.trace_id, "trace-12345")
  assert_eq(cache_context.parent_span_id, Some("span-database.query-12345"))
}

// 测试2: 指标聚合和统计
test "指标聚合和统计计算" {
  // 定义指标数据点
  type MetricPoint {
    name: String
    value: Float
    timestamp: Int
    tags: Map[String, String]
  }
  
  // 定义聚合窗口
  type AggregationWindow {
    start_time: Int
    end_time: Int
    aggregation_type: String // "sum", "avg", "min", "max", "count"
  }
  
  // 创建指标点
  let create_metric_point = fn(name: String, value: Float, timestamp: Int, tags: Map[String, String]) -> MetricPoint {
    { name, value, timestamp, tags }
  }
  
  // 按时间窗口过滤指标点
  let filter_by_window = fn(points: Array[MetricPoint], window: AggregationWindow) -> Array[MetricPoint] {
    points.filter(fn(point) {
      point.timestamp >= window.start_time && point.timestamp <= window.end_time
    })
  }
  
  // 执行指标聚合
  let aggregate_metrics = fn(points: Array[MetricPoint], window: AggregationWindow) -> Float {
    let filtered_points = filter_by_window(points, window)
    
    if filtered_points.length() == 0 {
      return 0.0
    }
    
    match window.aggregation_type {
      "sum" => filtered_points.reduce(fn(acc, point) { acc + point.value }, 0.0),
      "avg" => {
        let sum = filtered_points.reduce(fn(acc, point) { acc + point.value }, 0.0)
        sum / (filtered_points.length() as Float)
      },
      "min" => filtered_points.reduce(fn(acc, point) { 
        if point.value < acc { point.value } else { acc } 
      }, 999999.0),
      "max" => filtered_points.reduce(fn(acc, point) { 
        if point.value > acc { point.value } else { acc } 
      }, 0.0),
      "count" => filtered_points.length() as Float,
      _ => 0.0
    }
  }
  
  // 创建测试指标数据
  let base_time = 1640995200000
  let cpu_metrics = [
    create_metric_point("cpu.usage", 75.5, base_time, Map::from([("service", "api")])),
    create_metric_point("cpu.usage", 80.2, base_time + 60000, Map::from([("service", "api")])),
    create_metric_point("cpu.usage", 65.8, base_time + 120000, Map::from([("service", "api")])),
    create_metric_point("cpu.usage", 90.1, base_time + 180000, Map::from([("service", "api")])),
    create_metric_point("cpu.usage", 70.3, base_time + 240000, Map::from([("service", "api")])),
    create_metric_point("memory.usage", 60.5, base_time, Map::from([("service", "api")])),
    create_metric_point("memory.usage", 65.2, base_time + 60000, Map::from([("service", "api")])),
    create_metric_point("memory.usage", 58.9, base_time + 120000, Map::from([("service", "api")])),
    create_metric_point("memory.usage", 72.1, base_time + 180000, Map::from([("service", "api")])),
    create_metric_point("memory.usage", 68.7, base_time + 240000, Map::from([("service", "api")]))
  ]
  
  // 测试不同类型的聚合
  let sum_window = { start_time: base_time, end_time: base_time + 300000, aggregation_type: "sum" }
  let avg_window = { start_time: base_time, end_time: base_time + 300000, aggregation_type: "avg" }
  let min_window = { start_time: base_time, end_time: base_time + 300000, aggregation_type: "min" }
  let max_window = { start_time: base_time, end_time: base_time + 300000, aggregation_type: "max" }
  let count_window = { start_time: base_time, end_time: base_time + 300000, aggregation_type: "count" }
  
  // 测试CPU使用率聚合
  let cpu_points = cpu_metrics.filter(fn(point) { point.name == "cpu.usage" })
  
  let cpu_sum = aggregate_metrics(cpu_points, sum_window)
  let cpu_avg = aggregate_metrics(cpu_points, avg_window)
  let cpu_min = aggregate_metrics(cpu_points, min_window)
  let cpu_max = aggregate_metrics(cpu_points, max_window)
  let cpu_count = aggregate_metrics(cpu_points, count_window)
  
  assert_eq(cpu_count, 5.0)
  assert_eq(cpu_min, 65.8)
  assert_eq(cpu_max, 90.1)
  assert_eq(cpu_avg.round(), ((75.5 + 80.2 + 65.8 + 90.1 + 70.3) / 5.0).round())
  assert_eq(cpu_sum.round(), (75.5 + 80.2 + 65.8 + 90.1 + 70.3).round())
  
  // 测试内存使用率聚合
  let memory_points = cpu_metrics.filter(fn(point) { point.name == "memory.usage" })
  
  let memory_avg = aggregate_metrics(memory_points, avg_window)
  let memory_min = aggregate_metrics(memory_points, min_window)
  let memory_max = aggregate_metrics(memory_points, max_window)
  
  assert_eq(memory_min, 58.9)
  assert_eq(memory_max, 72.1)
  assert_eq(memory_avg.round(), ((60.5 + 65.2 + 58.9 + 72.1 + 68.7) / 5.0).round())
}

// 测试3: 异常检测和告警
test "异常检测和告警系统" {
  // 定义异常检测阈值
  type AnomalyThreshold {
    metric_name: String
    min_value: Option[Float]
    max_value: Option[Float]
    deviation_percent: Option[Float]
  }
  
  // 定义异常事件
  type AnomalyEvent {
    timestamp: Int
    metric_name: String
    current_value: Float
    expected_value: Option[Float]
    threshold: AnomalyThreshold
    severity: String // "low", "medium", "high", "critical"
    message: String
  }
  
  // 检测静态阈值异常
  let detect_static_anomaly = fn(value: Float, threshold: AnomalyThreshold) -> Bool {
    let exceeds_max = match threshold.max_value {
      Some(max) => value > max,
      None => false
    }
    
    let below_min = match threshold.min_value {
      Some(min) => value < min,
      None => false
    }
    
    exceeds_max || below_min
  }
  
  // 检测偏差异常
  let detect_deviation_anomaly = fn(current: Float, expected: Float, threshold: AnomalyThreshold) -> Bool {
    match threshold.deviation_percent {
      Some(deviation) => {
        let diff = (current - expected).abs()
        let percent_diff = (diff / expected) * 100.0
        percent_diff > deviation
      },
      None => false
    }
  }
  
  // 计算异常严重程度
  let calculate_severity = fn(current: Float, threshold: AnomalyThreshold) -> String {
    match threshold.max_value {
      Some(max) => {
        let exceed_percent = ((current - max) / max) * 100.0
        if exceed_percent > 50.0 {
          "critical"
        } else if exceed_percent > 25.0 {
          "high"
        } else if exceed_percent > 10.0 {
          "medium"
        } else {
          "low"
        }
      },
      None => {
        match threshold.min_value {
          Some(min) => {
            let below_percent = ((min - current) / min) * 100.0
            if below_percent > 50.0 {
              "critical"
            } else if below_percent > 25.0 {
              "high"
            } else if below_percent > 10.0 {
              "medium"
            } else {
              "low"
            }
          },
          None => "low"
        }
      }
    }
  }
  
  // 创建异常检测阈值
  let thresholds = [
    { metric_name: "cpu.usage", min_value: None, max_value: Some(80.0), deviation_percent: Some(20.0) },
    { metric_name: "memory.usage", min_value: None, max_value: Some(85.0), deviation_percent: Some(15.0) },
    { metric_name: "error.rate", min_value: None, max_value: Some(5.0), deviation_percent: Some(50.0) },
    { metric_name: "response.time", min_value: None, max_value: Some(500.0), deviation_percent: Some(30.0) }
  ]
  
  // 测试静态阈值异常检测
  let cpu_threshold = thresholds[0]
  let normal_cpu = 75.5
  let high_cpu = 85.2
  let critical_cpu = 95.8
  
  assert_false(detect_static_anomaly(normal_cpu, cpu_threshold))
  assert_true(detect_static_anomaly(high_cpu, cpu_threshold))
  assert_true(detect_static_anomaly(critical_cpu, cpu_threshold))
  
  // 测试严重程度计算
  assert_eq(calculate_severity(normal_cpu, cpu_threshold), "low")
  assert_eq(calculate_severity(high_cpu, cpu_threshold), "medium")
  assert_eq(calculate_severity(critical_cpu, cpu_threshold), "critical")
  
  // 测试偏差异常检测
  let expected_cpu = 60.0
  let current_cpu = 85.0
  
  assert_true(detect_deviation_anomaly(current_cpu, expected_cpu, cpu_threshold))
  assert_false(detect_deviation_anomaly(normal_cpu, expected_cpu, cpu_threshold))
  
  // 测试多指标异常检测
  let current_metrics = [
    ("cpu.usage", 85.2),
    ("memory.usage", 78.5),
    ("error.rate", 8.2),
    ("response.time", 450.0)
  ]
  
  let mut anomaly_count = 0
  for (metric_name, value) in current_metrics {
    match thresholds.find(fn(t) { t.metric_name == metric_name }) {
      Some(threshold) => {
        if detect_static_anomaly(value, threshold) {
          anomaly_count = anomaly_count + 1
        }
      },
      None => {}
    }
  }
  
  assert_eq(anomaly_count, 2) // cpu.usage 和 error.rate 超过阈值
}

// 测试4: 服务依赖关系图
test "服务依赖关系图分析" {
  // 定义服务节点
  type ServiceNode {
    name: String
    type: String // "service", "database", "cache", "queue"
    health: String // "healthy", "degraded", "unhealthy"
    response_time: Float
    error_rate: Float
  }
  
  // 定义服务依赖边
  type ServiceEdge {
    from: String
    to: String
    call_count: Int
    avg_response_time: Float
    error_rate: Float
  }
  
  // 定义服务依赖图
  type ServiceDependencyGraph {
    nodes: Array[ServiceNode]
    edges: Array[ServiceEdge]
  }
  
  // 创建服务节点
  let create_service_node = fn(name: String, type: String, health: String, response_time: Float, error_rate: Float) -> ServiceNode {
    { name, type, health, response_time, error_rate }
  }
  
  // 创建服务依赖边
  let create_service_edge = fn(from: String, to: String, call_count: Int, avg_response_time: Float, error_rate: Float) -> ServiceEdge {
    { from, to, call_count, avg_response_time, error_rate }
  }
  
  // 查找服务的直接依赖
  let find_direct_dependencies = fn(graph: ServiceDependencyGraph, service_name: String) -> Array[String] {
    graph.edges
      .filter(fn(edge) { edge.from == service_name })
      .map(fn(edge) { edge.to })
  }
  
  // 查找服务的下游依赖
  let find_downstream_dependencies = fn(graph: ServiceDependencyGraph, service_name: String) -> Array[String] {
    let mut visited = []
    let mut to_visit = [service_name]
    let mut result = []
    
    while to_visit.length() > 0 {
      let current = to_visit[0]
      to_visit = to_visit.slice(1)
      
      if not(visited.contains(current)) {
        visited = visited.push(current)
        let dependencies = find_direct_dependencies(graph, current)
        
        for dep in dependencies {
          if not(result.contains(dep)) {
            result = result.push(dep)
          }
          if not(to_visit.contains(dep)) {
            to_visit = to_visit.push(dep)
          }
        }
      }
    }
    
    result
  }
  
  // 查找服务路径中的关键节点
  let find_critical_path = fn(graph: ServiceDependencyGraph, from: String, to: String) -> Array[String] {
    // 简化的路径查找算法
    let mut path = []
    let mut current = from
    
    while current != to {
      path = path.push(current)
      
      match graph.edges.find(fn(edge) { edge.from == current }) {
        Some(edge) => current = edge.to,
        None => break
      }
    }
    
    if current == to {
      path = path.push(to)
    }
    
    path
  }
  
  // 创建测试服务依赖图
  let nodes = [
    create_service_node("api-gateway", "service", "healthy", 120.5, 0.2),
    create_service_node("user-service", "service", "healthy", 85.3, 0.5),
    create_service_node("order-service", "service", "degraded", 150.8, 2.1),
    create_service_node("payment-service", "service", "healthy", 200.2, 0.8),
    create_service_node("user-db", "database", "healthy", 25.5, 0.1),
    create_service_node("order-db", "database", "degraded", 45.8, 1.5),
    create_service_node("payment-db", "database", "healthy", 35.2, 0.3),
    create_service_node("redis-cache", "cache", "healthy", 5.2, 0.05),
    create_service_node("message-queue", "queue", "healthy", 15.8, 0.1)
  ]
  
  let edges = [
    create_service_edge("api-gateway", "user-service", 1000, 85.3, 0.5),
    create_service_edge("api-gateway", "order-service", 800, 150.8, 2.1),
    create_service_edge("api-gateway", "payment-service", 500, 200.2, 0.8),
    create_service_edge("user-service", "user-db", 1200, 25.5, 0.1),
    create_service_edge("user-service", "redis-cache", 800, 5.2, 0.05),
    create_service_edge("order-service", "order-db", 900, 45.8, 1.5),
    create_service_edge("order-service", "redis-cache", 600, 5.2, 0.05),
    create_service_edge("order-service", "message-queue", 300, 15.8, 0.1),
    create_service_edge("payment-service", "payment-db", 600, 35.2, 0.3),
    create_service_edge("payment-service", "message-queue", 200, 15.8, 0.1)
  ]
  
  let graph = { nodes, edges }
  
  // 测试直接依赖查找
  let api_dependencies = find_direct_dependencies(graph, "api-gateway")
  assert_eq(api_dependencies.length(), 3)
  assert_true(api_dependencies.contains("user-service"))
  assert_true(api_dependencies.contains("order-service"))
  assert_true(api_dependencies.contains("payment-service"))
  
  // 测试下游依赖查找
  let api_downstream = find_downstream_dependencies(graph, "api-gateway")
  assert_eq(api_downstream.length(), 6) // 所有下游服务
  
  // 测试关键路径查找
  let critical_path = find_critical_path(graph, "api-gateway", "user-db")
  assert_eq(critical_path.length(), 3)
  assert_eq(critical_path[0], "api-gateway")
  assert_eq(critical_path[1], "user-service")
  assert_eq(critical_path[2], "user-db")
  
  // 测试健康状态分析
  let unhealthy_nodes = nodes.filter(fn(node) { node.health == "unhealthy" })
  let degraded_nodes = nodes.filter(fn(node) { node.health == "degraded" })
  let healthy_nodes = nodes.filter(fn(node) { node.health == "healthy" })
  
  assert_eq(unhealthy_nodes.length(), 0)
  assert_eq(degraded_nodes.length(), 2) // order-service 和 order-db
  assert_eq(healthy_nodes.length(), 7)
}

// 测试5: 遥测数据压缩和传输优化
test "遥测数据压缩和传输优化" {
  // 定义遥测数据点
  type TelemetryPoint {
    timestamp: Int
    metric_name: String
    value: Float
    tags: Map[String, String]
  }
  
  // 定义压缩策略
  type CompressionStrategy {
    name: String
    threshold: Int // 数据点数量阈值
    compression_ratio: Float // 压缩比例
  }
  
  // 时间序列数据压缩 - 简单的降采样
  let downsample_time_series = fn(points: Array[TelemetryPoint], interval_ms: Int) -> Array[TelemetryPoint] {
    if points.length() == 0 {
      return []
    }
    
    let mut result = []
    let mut i = 0
    
    while i < points.length() {
      let base_point = points[i]
      let window_end = base_point.timestamp + interval_ms
      
      // 收集窗口内的所有点
      let mut window_points = []
      let mut j = i
      
      while j < points.length() && points[j].timestamp <= window_end {
        window_points = window_points.push(points[j])
        j = j + 1
      }
      
      // 计算窗口内的平均值
      if window_points.length() > 0 {
        let sum = window_points.reduce(fn(acc, point) { acc + point.value }, 0.0)
        let avg_value = sum / (window_points.length() as Float)
        
        let compressed_point = {
          timestamp: base_point.timestamp,
          metric_name: base_point.metric_name,
          value: avg_value,
          tags: base_point.tags
        }
        
        result = result.push(compressed_point)
      }
      
      i = j
    }
    
    result
  }
  
  // 标签压缩 - 移除重复的标签
  let compress_tags = fn(points: Array[TelemetryPoint]) -> Array[TelemetryPoint] {
    if points.length() == 0 {
      return []
    }
    
    let base_tags = points[0].tags
    let common_tags = base_tags.filter(fn(tag) {
      let (key, value) = tag
      points.all(fn(point) {
        match Map::get(point.tags, key) {
          Some(v) => v == value,
          None => false
        }
      })
    })
    
    points.map(fn(point) {
      let compressed_tags = Map::empty()
      
      // 添加公共标签
      for tag in common_tags {
        let (key, value) = tag
        compressed_tags = Map::insert(compressed_tags, key, value)
      }
      
      // 添加非公共标签
      for tag in point.tags.to_array() {
        let (key, value) = tag
        if not(common_tags.contains(tag)) {
          compressed_tags = Map::insert(compressed_tags, key, value)
        }
      }
      
      {
        timestamp: point.timestamp,
        metric_name: point.metric_name,
        value: point.value,
        tags: compressed_tags
      }
    })
  }
  
  // 计算压缩效果
  let calculate_compression_ratio = fn(original_size: Int, compressed_size: Int) -> Float {
    if original_size == 0 {
      0.0
    } else {
      (original_size as Float - compressed_size as Float) / (original_size as Float) * 100.0
    }
  }
  
  // 创建测试数据
  let base_time = 1640995200000
  let service_tags = Map::from([("service", "api"), ("env", "prod"), ("version", "1.0.0")])
  
  let telemetry_points = []
  let mut i = 0
  while i < 60 {
    let point = {
      timestamp: base_time + (i * 1000),
      metric_name: "cpu.usage",
      value: 70.0 + (i as Float) * 0.1,
      tags: service_tags
    }
    telemetry_points = telemetry_points.push(point)
    i = i + 1
  }
  
  // 测试时间序列压缩
  let downsampled_points = downsample_time_series(telemetry_points, 5000) // 5秒间隔
  assert_eq(downsampled_points.length(), 12) // 60秒 / 5秒 = 12个点
  
  // 验证第一个压缩点的值
  let first_window_avg = (70.0 + 70.1 + 70.2 + 70.3 + 70.4) / 5.0
  assert_eq(downsampled_points[0].value.round(), first_window_avg.round())
  
  // 测试标签压缩
  let tag_compressed_points = compress_tags(telemetry_points)
  assert_eq(tag_compressed_points.length(), telemetry_points.length())
  
  // 验证标签压缩效果
  let original_tag_count = telemetry_points[0].tags.size()
  let compressed_tag_count = tag_compressed_points[0].tags.size()
  
  assert_eq(original_tag_count, 3)
  assert_eq(compressed_tag_count, 3) // 所有标签都是公共标签
  
  // 测试压缩比例计算
  let original_data_size = telemetry_points.length() * 100 // 假设每个点100字节
  let compressed_data_size = downsampled_points.length() * 100 // 假设每个压缩点100字节
  
  let compression_ratio = calculate_compression_ratio(original_data_size, compressed_data_size)
  assert_eq(compression_ratio.round(), 80.0) // 从60个点压缩到12个点，压缩80%
  
  // 测试不同压缩策略
  let strategies = [
    { name: "aggressive", threshold: 100, compression_ratio: 90.0 },
    { name: "moderate", threshold: 50, compression_ratio: 70.0 },
    { name: "conservative", threshold: 20, compression_ratio: 50.0 }
  ]
  
  let selected_strategy = if telemetry_points.length() > 100 {
    strategies[0] // aggressive
  } else if telemetry_points.length() > 50 {
    strategies[1] // moderate
  } else {
    strategies[2] // conservative
  }
  
  assert_eq(selected_strategy.name, "moderate")
  assert_eq(selected_strategy.compression_ratio, 70.0)
}

// 测试6: 遥测数据采样策略
test "遥测数据采样策略" {
  // 定义采样策略
  type SamplingStrategy {
    name: String
    sample_rate: Float // 0.0 到 1.0
    priority_threshold: Int // 优先级阈值
  }
  
  // 定义遥测事件
  type TelemetryEvent {
    trace_id: String
    span_id: String
    operation_name: String
    priority: Int // 1-10，10为最高优先级
    duration: Int
    has_error: Bool
  }
  
  // 基于概率的采样
  let probabilistic_sampling = fn(event: TelemetryEvent, sample_rate: Float) -> Bool {
    // 简化的概率采样，基于trace_id的哈希值
    let hash = event.trace_id.length() % 100
    let threshold = (sample_rate * 100.0) as Int
    hash < threshold
  }
  
  // 基于优先级的采样
  let priority_sampling = fn(event: TelemetryEvent, threshold: Int) -> Bool {
    event.priority >= threshold
  }
  
  // 基于错误状态的采样
  let error_sampling = fn(event: TelemetryEvent) -> Bool {
    event.has_error
  }
  
  // 组合采样策略
  let combined_sampling = fn(event: TelemetryEvent, strategy: SamplingStrategy) -> Bool {
    let priority_sample = priority_sampling(event, strategy.priority_threshold)
    let error_sample = error_sampling(event)
    let probabilistic_sample = probabilistic_sampling(event, strategy.sample_rate)
    
    // 错误事件总是采样
    // 高优先级事件总是采样
    // 其他事件基于概率采样
    error_sample || priority_sample || probabilistic_sample
  }
  
  // 创建测试事件
  let create_event = fn(trace_id: String, operation_name: String, priority: Int, duration: Int, has_error: Bool) -> TelemetryEvent {
    {
      trace_id,
      span_id: "span-" + trace_id.split("-")[1],
      operation_name,
      priority,
      duration,
      has_error
    }
  }
  
  let test_events = [
    create_event("trace-001", "api.request", 5, 120, false),
    create_event("trace-002", "database.query", 8, 85, false),
    create_event("trace-003", "cache.lookup", 3, 15, false),
    create_event("trace-004", "external.api", 9, 500, true),
    create_event("trace-005", "user.auth", 7, 200, false),
    create_event("trace-006", "payment.process", 10, 350, false),
    create_event("trace-007", "order.create", 6, 280, true),
    create_event("trace-008", "notification.send", 4, 95, false),
    create_event("trace-009", "file.upload", 2, 1200, false),
    create_event("trace-010", "report.generate", 8, 2500, false)
  ]
  
  // 创建采样策略
  let strategies = [
    { name: "production", sample_rate: 0.1, priority_threshold: 8 },
    { name: "staging", sample_rate: 0.5, priority_threshold: 6 },
    { name: "development", sample_rate: 1.0, priority_threshold: 1 }
  ]
  
  // 测试生产环境采样策略
  let prod_strategy = strategies[0]
  let mut prod_sampled_count = 0
  
  for event in test_events {
    if combined_sampling(event, prod_strategy) {
      prod_sampled_count = prod_sampled_count + 1
    }
  }
  
  // 错误事件 (trace-004, trace-007) 应该总是被采样
  // 高优先级事件 (trace-002, trace-004, trace-006, trace-010) 应该总是被采样
  // 其他事件基于概率采样
  assert_true(prod_sampled_count >= 4) // 至少错误和高优先级事件
  
  // 测试开发环境采样策略
  let dev_strategy = strategies[2]
  let mut dev_sampled_count = 0
  
  for event in test_events {
    if combined_sampling(event, dev_strategy) {
      dev_sampled_count = dev_sampled_count + 1
    }
  }
  
  assert_eq(dev_sampled_count, 10) // 开发环境应该采样所有事件
  
  // 测试基于持续时间的采样
  let duration_sampling = fn(event: TelemetryEvent, threshold_ms: Int) -> Bool {
    event.duration > threshold_ms
  }
  
  let slow_operations = test_events.filter(fn(event) {
    duration_sampling(event, 1000) // 超过1秒的操作
  })
  
  assert_eq(slow_operations.length(), 2) // file.upload 和 report.generate
  
  // 测试自适应采样率调整
  let adaptive_sampling_rate = fn(current_rate: Float, target_sample_count: Int, current_sample_count: Int, total_events: Int) -> Float {
    if total_events == 0 {
      return current_rate
    }
    
    let current_ratio = (current_sample_count as Float) / (total_events as Float)
    
    if current_ratio < (target_sample_count as Float) / (total_events as Float) {
      // 增加采样率
      (current_rate + 0.1).min(1.0)
    } else {
      // 减少采样率
      (current_rate - 0.1).max(0.01)
    }
  }
  
  let current_rate = 0.1
  let target_sample_count = 5
  let current_sample_count = 3
  let total_events = 20
  
  let adjusted_rate = adaptive_sampling_rate(current_rate, target_sample_count, current_sample_count, total_events)
  assert_true(adjusted_rate > current_rate) // 应该增加采样率
}

// 测试7: 遥测数据生命周期管理
test "遥测数据生命周期管理" {
  // 定义数据保留策略
  type RetentionPolicy {
    name: String
    retention_days: Int
    compression_after_days: Int
    delete_after_days: Int
  }
  
  // 定义数据块
  type DataBlock {
    block_id: String
    creation_time: Int
    size_bytes: Int
    is_compressed: Bool
    access_count: Int
    last_access_time: Int
  }
  
  // 计算数据年龄（天数）
  let calculate_age_days = fn(creation_time: Int, current_time: Int) -> Int {
    let age_ms = current_time - creation_time
    (age_ms / (24 * 60 * 60 * 1000)) // 转换为天数
  }
  
  // 检查数据是否应该压缩
  let should_compress = fn(block: DataBlock, policy: RetentionPolicy, current_time: Int) -> Bool {
    let age_days = calculate_age_days(block.creation_time, current_time)
    age_days >= policy.compression_after_days && not(block.is_compressed)
  }
  
  // 检查数据是否应该删除
  let should_delete = fn(block: DataBlock, policy: RetentionPolicy, current_time: Int) -> Bool {
    let age_days = calculate_age_days(block.creation_time, current_time)
    age_days >= policy.delete_after_days
  }
  
  // 计算存储空间节省
  let calculate_storage_savings = fn(blocks: Array[DataBlock], compression_ratio: Float) -> Int {
    let uncompressed_size = blocks.filter(fn(block) { not(block.is_compressed) })
      .reduce(fn(acc, block) { acc + block.size_bytes }, 0)
    
    let compressed_size = (uncompressed_size as Float * (1.0 - compression_ratio)) as Int
    uncompressed_size - compressed_size
  }
  
  // 查找冷数据（访问频率低的数据）
  let find_cold_data = fn(blocks: Array[DataBlock], days_threshold: Int, access_threshold: Int, current_time: Int) -> Array[DataBlock] {
    blocks.filter(fn(block) {
      let days_since_last_access = calculate_age_days(block.last_access_time, current_time)
      days_since_last_access >= days_threshold && block.access_count <= access_threshold
    })
  }
  
  // 创建测试数据块
  let create_data_block = fn(block_id: String, creation_time: Int, size_bytes: Int, is_compressed: Bool, access_count: Int, last_access_time: Int) -> DataBlock {
    {
      block_id,
      creation_time,
      size_bytes,
      is_compressed,
      access_count,
      last_access_time
    }
  }
  
  let current_time = 1640995200000 // 2022-01-01
  let day_ms = 24 * 60 * 60 * 1000
  
  let data_blocks = [
    create_data_block("block-001", current_time - (1 * day_ms), 1024, false, 100, current_time - (1 * day_ms)), // 1天前
    create_data_block("block-002", current_time - (7 * day_ms), 2048, false, 50, current_time - (3 * day_ms)), // 7天前
    create_data_block("block-003", current_time - (30 * day_ms), 4096, true, 20, current_time - (15 * day_ms)), // 30天前，已压缩
    create_data_block("block-004", current_time - (60 * day_ms), 8192, false, 5, current_time - (50 * day_ms)), // 60天前
    create_data_block("block-005", current_time - (90 * day_ms), 16384, true, 2, current_time - (80 * day_ms)), // 90天前，已压缩
    create_data_block("block-006", current_time - (120 * day_ms), 32768, false, 1, current_time - (110 * day_ms)), // 120天前
    create_data_block("block-007", current_time - (180 * day_ms), 65536, false, 0, current_time - (180 * day_ms)), // 180天前
    create_data_block("block-008", current_time - (365 * day_ms), 131072, false, 0, current_time - (365 * day_ms)) // 365天前
  ]
  
  // 创建保留策略
  let policies = [
    { name: "hot", retention_days: 7, compression_after_days: 1, delete_after_days: 30 },
    { name: "warm", retention_days: 30, compression_after_days: 7, delete_after_days: 90 },
    { name: "cold", retention_days: 365, compression_after_days: 30, delete_after_days: 730 },
    { name: "archive", retention_days: 1825, compression_after_days: 90, delete_after_days: 3650 }
  ]
  
  // 测试压缩检查
  let hot_policy = policies[0]
  let blocks_to_compress = data_blocks.filter(fn(block) {
    should_compress(block, hot_policy, current_time)
  })
  
  assert_eq(blocks_to_compress.length(), 1) // 只有block-002需要压缩（7天前，未压缩）
  
  // 测试删除检查
  let blocks_to_delete = data_blocks.filter(fn(block) {
    should_delete(block, hot_policy, current_time)
  })
  
  assert_eq(blocks_to_delete.length(), 2) // block-007和block-008应该被删除（超过30天）
  
  // 测试存储空间计算
  let uncompressed_blocks = data_blocks.filter(fn(block) { not(block.is_compressed) })
  let total_uncompressed_size = uncompressed_blocks.reduce(fn(acc, block) { acc + block.size_bytes }, 0)
  
  let compression_ratio = 0.7 // 70%压缩率
  let storage_savings = calculate_storage_savings(data_blocks, compression_ratio)
  
  assert_eq(storage_savings, (total_uncompressed_size as Float * compression_ratio) as Int)
  
  // 测试冷数据识别
  let cold_blocks = find_cold_data(data_blocks, 30, 5, current_time) // 30天未访问，访问次数<=5
  assert_eq(cold_blocks.length(), 3) // block-004, block-006, block-007
  
  // 测试数据生命周期阶段
  let classify_data_lifecycle = fn(block: DataBlock, current_time: Int) -> String {
    let age_days = calculate_age_days(block.creation_time, current_time)
    
    if age_days <= 7 {
      "hot"
    } else if age_days <= 30 {
      "warm"
    } else if age_days <= 365 {
      "cold"
    } else {
      "archive"
    }
  }
  
  let mut hot_count = 0
  let mut warm_count = 0
  let mut cold_count = 0
  let mut archive_count = 0
  
  for block in data_blocks {
    let lifecycle = classify_data_lifecycle(block, current_time)
    match lifecycle {
      "hot" => hot_count = hot_count + 1,
      "warm" => warm_count = warm_count + 1,
      "cold" => cold_count = cold_count + 1,
      "archive" => archive_count = archive_count + 1,
      _ => {}
    }
  }
  
  assert_eq(hot_count, 1) // block-001
  assert_eq(warm_count, 1) // block-002
  assert_eq(cold_count, 4) // block-003, block-004, block-005, block-006
  assert_eq(archive_count, 2) // block-007, block-008
}

// 测试8: 遥测数据质量评估
test "遥测数据质量评估" {
  // 定义数据质量指标
  type DataQualityMetrics {
    completeness: Float // 完整性 0-1
    accuracy: Float // 准确性 0-1
    consistency: Float // 一致性 0-1
    timeliness: Float // 及时性 0-1
    validity: Float // 有效性 0-1
  }
  
  // 定义数据质量规则
  type QualityRule {
    name: String
    description: String
    check_fn: (String, Float, Int) -> Bool
  }
  
  // 定义遥测数据点
  type TelemetryDataPoint {
    timestamp: Int
    metric_name: String
    value: Float
    source: String
    tags: Map[String, String]
  }
  
  // 检查数据完整性
  let check_completeness = fn(points: Array[TelemetryDataPoint]) -> Float {
    if points.length() == 0 {
      return 0.0
    }
    
    let complete_points = points.filter(fn(point) {
      point.timestamp > 0 && 
      point.metric_name != "" && 
      not(point.value.is_nan()) && 
      point.source != ""
    })
    
    (complete_points.length() as Float) / (points.length() as Float)
  }
  
  // 检查数据准确性
  let check_accuracy = fn(points: Array[TelemetryDataPoint], expected_ranges: Map[String, (Float, Float)]) -> Float {
    if points.length() == 0 {
      return 0.0
    }
    
    let accurate_points = points.filter(fn(point) {
      match Map::get(expected_ranges, point.metric_name) {
        Some((min, max)) => point.value >= min && point.value <= max,
        None => true // 如果没有定义范围，假设准确
      }
    })
    
    (accurate_points.length() as Float) / (points.length() as Float)
  }
  
  // 检查数据一致性
  let check_consistency = fn(points: Array[TelemetryDataPoint]) -> Float {
    if points.length() == 0 {
      return 0.0
    }
    
    // 按指标名称分组
    let mut groups = Map::empty()
    
    for point in points {
      let group = match Map::get(groups, point.metric_name) {
        Some(g) => g,
        None => []
      }
      groups = Map::insert(groups, point.metric_name, group.push(point))
    }
    
    // 检查每组内数据的一致性
    let mut consistent_groups = 0
    let total_groups = groups.size()
    
    for (_, group_points) in groups.to_array() {
      if group_points.length() > 1 {
        // 检查时间戳是否递增
        let mut is_consistent = true
        let mut i = 1
        
        while i < group_points.length() && is_consistent {
          if group_points[i].timestamp <= group_points[i-1].timestamp {
            is_consistent = false
          }
          i = i + 1
        }
        
        if is_consistent {
          consistent_groups = consistent_groups + 1
        }
      } else {
        consistent_groups = consistent_groups + 1
      }
    }
    
    if total_groups == 0 {
      0.0
    } else {
      (consistent_groups as Float) / (total_groups as Float)
    }
  }
  
  // 检查数据及时性
  let check_timeliness = fn(points: Array[TelemetryDataPoint], current_time: Int, max_delay_ms: Int) -> Float {
    if points.length() == 0 {
      return 0.0
    }
    
    let timely_points = points.filter(fn(point) {
      let delay = current_time - point.timestamp
      delay <= max_delay_ms
    })
    
    (timely_points.length() as Float) / (points.length() as Float)
  }
  
  // 检查数据有效性
  let check_validity = fn(points: Array[TelemetryDataPoint], validation_rules: Array[QualityRule]) -> Float {
    if points.length() == 0 {
      return 0.0
    }
    
    let mut valid_points = 0
    
    for point in points {
      let mut is_valid = true
      let mut i = 0
      
      while i < validation_rules.length() && is_valid {
        let rule = validation_rules[i]
        if not(rule.check_fn(point.metric_name, point.value, point.timestamp)) {
          is_valid = false
        }
        i = i + 1
      }
      
      if is_valid {
        valid_points = valid_points + 1
      }
    }
    
    (valid_points as Float) / (points.length() as Float)
  }
  
  // 创建测试数据点
  let create_data_point = fn(timestamp: Int, metric_name: String, value: Float, source: String, tags: Map[String, String]) -> TelemetryDataPoint {
    { timestamp, metric_name, value, source, tags }
  }
  
  let current_time = 1640995200000
  let one_hour_ms = 60 * 60 * 1000
  
  let telemetry_points = [
    create_data_point(current_time - (2 * one_hour_ms), "cpu.usage", 75.5, "host-001", Map::from([("service", "api")])),
    create_data_point(current_time - (1 * one_hour_ms), "cpu.usage", 80.2, "host-001", Map::from([("service", "api")])),
    create_data_point(current_time, "cpu.usage", 65.8, "host-001", Map::from([("service", "api")])),
    create_data_point(current_time - (2 * one_hour_ms), "memory.usage", 60.5, "host-001", Map::from([("service", "api")])),
    create_data_point(current_time - (1 * one_hour_ms), "memory.usage", 125.5, "host-001", Map::from([("service", "api")])), // 异常值
    create_data_point(current_time, "memory.usage", 58.9, "host-001", Map::from([("service", "api")])),
    create_data_point(current_time - (30 * one_hour_ms), "disk.usage", 45.2, "host-001", Map::from([("service", "api")])), // 过时数据
    create_data_point(current_time - (2 * one_hour_ms), "response.time", 120.0, "host-001", Map::from([("service", "api")])),
    create_data_point(current_time - (1 * one_hour_ms), "response.time", 150.0, "host-001", Map::from([("service", "api")])),
    create_data_point(current_time, "response.time", 95.0, "host-001", Map::from([("service", "api")]))
  ]
  
  // 创建预期范围
  let expected_ranges = Map::from([
    ("cpu.usage", (0.0, 100.0)),
    ("memory.usage", (0.0, 100.0)),
    ("disk.usage", (0.0, 100.0)),
    ("response.time", (0.0, 10000.0))
  ])
  
  // 创建验证规则
  let validation_rules = [
    { 
      name: "positive_values", 
      description: "Values should be positive",
      check_fn: fn(metric_name: String, value: Float, timestamp: Int) -> Bool { value >= 0.0 }
    },
    { 
      name: "reasonable_timestamps", 
      description: "Timestamps should be reasonable",
      check_fn: fn(metric_name: String, value: Float, timestamp: Int) -> Bool { timestamp > 0 }
    },
    { 
      name: "finite_values", 
      description: "Values should be finite",
      check_fn: fn(metric_name: String, value: Float, timestamp: Int) -> Bool { not(value.is_nan()) && not(value.is_infinite()) }
    }
  ]
  
  // 测试数据质量检查
  let completeness = check_completeness(telemetry_points)
  let accuracy = check_accuracy(telemetry_points, expected_ranges)
  let consistency = check_consistency(telemetry_points)
  let timeliness = check_timeliness(telemetry_points, current_time, 5 * one_hour_ms) // 5小时内
  let validity = check_validity(telemetry_points, validation_rules)
  
  // 验证完整性
  assert_eq(completeness, 1.0) // 所有数据点都是完整的
  
  // 验证准确性
  assert_true(accuracy < 1.0) // memory.usage有一个异常值(125.5)
  assert_eq(accuracy, 0.9) // 10个点中9个准确
  
  // 验证一致性
  assert_eq(consistency, 1.0) // 所有组的数据都是一致的
  
  // 验证及时性
  assert_true(timeliness < 1.0) // 有一个过时数据点
  assert_eq(timeliness, 0.9) // 10个点中9个及时
  
  // 验证有效性
  assert_eq(validity, 1.0) // 所有数据点都通过验证规则
  
  // 计算总体质量分数
  let quality_metrics = {
    completeness,
    accuracy,
    consistency,
    timeliness,
    validity
  }
  
  let overall_quality = (quality_metrics.completeness + 
                        quality_metrics.accuracy + 
                        quality_metrics.consistency + 
                        quality_metrics.timeliness + 
                        quality_metrics.validity) / 5.0
  
  assert_eq(overall_quality, (1.0 + 0.9 + 1.0 + 0.9 + 1.0) / 5.0)
  
  // 测试质量阈值检查
  let quality_threshold = 0.9
  let meets_quality_threshold = overall_quality >= quality_threshold
  
  assert_false(meets_quality_threshold) // 总体质量低于阈值
}