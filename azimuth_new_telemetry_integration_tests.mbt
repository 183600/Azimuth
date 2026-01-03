// Azimuth 遥测系统集成测试
// 专注于测试遥测系统的集成功能和高级特性

// 测试1: 分布式追踪链
test "分布式追踪链测试" {
  // 1. 创建根span
  let root_span = {
    trace_id: "trace-root-123456789",
    span_id: "span-root-987654321",
    name: "api.request.handler",
    kind: "server",
    sampled: true,
    trace_state: "service=api,env=production"
  }
  
  // 2. 创建子span - 数据库查询
  let db_span = {
    trace_id: "trace-root-123456789",  // 相同的trace_id
    span_id: "span-db-111111111",
    name: "database.query",
    kind: "client",
    sampled: true,
    trace_state: "service=database,env=production"
  }
  
  // 3. 创建子span - 外部API调用
  let api_span = {
    trace_id: "trace-root-123456789",  // 相同的trace_id
    span_id: "span-api-222222222",
    name: "external.api.call",
    kind: "client",
    sampled: true,
    trace_state: "service=external,env=production"
  }
  
  // 4. 验证追踪链
  assert_eq(root_span.trace_id, "trace-root-123456789")
  assert_eq(db_span.trace_id, "trace-root-123456789")
  assert_eq(api_span.trace_id, "trace-root-123456789")
  
  // 验证所有span都在同一个追踪中
  assert_true(root_span.trace_id == db_span.trace_id)
  assert_true(db_span.trace_id == api_span.trace_id)
  
  // 验证span ID各不相同
  assert_true(root_span.span_id != db_span.span_id)
  assert_true(db_span.span_id != api_span.span_id)
  assert_true(root_span.span_id != api_span.span_id)
}

// 测试2: 性能指标收集
test "性能指标收集测试" {
  // 1. 创建性能指标
  let request_counter = {
    name: "http.requests.total",
    description: "HTTP请求总数",
    unit: "requests",
    value: 1500
  }
  
  let response_time_histogram = {
    name: "http.response.time",
    description: "HTTP响应时间分布",
    unit: "milliseconds",
    values: [120, 150, 200, 180, 90]
  }
  
  let active_connections_gauge = {
    name: "http.active.connections",
    description: "当前活跃连接数",
    unit: "connections",
    value: 25
  }
  
  // 2. 创建指标集合
  let metrics_collected = [request_counter, response_time_histogram, active_connections_gauge]
  
  // 3. 验证指标数据
  assert_eq(metrics_collected.length(), 3)
  
  // 验证请求计数器
  assert_eq(metrics_collected[0].name, "http.requests.total")
  assert_eq(metrics_collected[0].description, "HTTP请求总数")
  assert_eq(metrics_collected[0].unit, "requests")
  assert_eq(metrics_collected[0].value, 1500)
  
  // 验证响应时间直方图
  assert_eq(metrics_collected[1].name, "http.response.time")
  assert_eq(metrics_collected[1].description, "HTTP响应时间分布")
  assert_eq(metrics_collected[1].unit, "milliseconds")
  assert_eq(metrics_collected[1].values.length(), 5)
  assert_eq(metrics_collected[1].values[0], 120)
  assert_eq(metrics_collected[1].values[4], 90)
  
  // 验证活跃连接仪表
  assert_eq(metrics_collected[2].name, "http.active.connections")
  assert_eq(metrics_collected[2].description, "当前活跃连接数")
  assert_eq(metrics_collected[2].unit, "connections")
  assert_eq(metrics_collected[2].value, 25)
}

// 测试3: 日志关联
test "日志关联测试" {
  // 1. 创建日志记录
  let log_entry = {
    timestamp: 1640995200000,  // 2022-01-01 00:00:00 UTC
    severity: "INFO",
    body: "Request processed successfully",
    trace_id: "trace-log-123456789",
    span_id: "span-log-987654321",
    user_id: "user-12345",
    request_id: "req-abcdef",
    response_status: "200"
  }
  
  // 2. 创建span上下文
  let span_context = {
    trace_id: "trace-log-123456789",
    span_id: "span-log-987654321",
    sampled: true,
    trace_state: "service=api,env=production"
  }
  
  // 3. 验证日志与span的关联
  assert_eq(log_entry.timestamp, 1640995200000)
  assert_eq(log_entry.severity, "INFO")
  assert_eq(log_entry.body, "Request processed successfully")
  
  // 验证日志属性中的追踪信息
  assert_eq(log_entry.trace_id, span_context.trace_id)
  assert_eq(log_entry.span_id, span_context.span_id)
  assert_eq(log_entry.user_id, "user-12345")
  assert_eq(log_entry.request_id, "req-abcdef")
  assert_eq(log_entry.response_status, "200")
}

// 测试4: 资源属性管理
test "资源属性管理测试" {
  // 1. 创建服务资源
  let service_resource = {
    attributes: [
      ("service.name", "azimuth-api"),
      ("service.version", "2.1.0"),
      ("service.instance.id", "api-instance-001"),
      ("service.namespace", "production"),
      ("host.name", "api-server-01"),
      ("host.arch", "amd64"),
      ("os.type", "linux"),
      ("os.version", "ubuntu-20.04"),
      ("deployment.environment", "production"),
      ("telemetry.sdk.name", "azimuth"),
      ("telemetry.sdk.version", "1.0.0"),
      ("telemetry.sdk.language", "moonbit")
    ]
  }
  
  // 2. 验证资源属性
  assert_eq(service_resource.attributes.length(), 11)
  
  // 3. 验证特定属性
  let mut service_attrs = []
  for (key, value) in service_resource.attributes {
    service_attrs = service_attrs.push({ key: key, value: value })
  }
  
  assert_eq(service_attrs[0].key, "service.name")
  assert_eq(service_attrs[0].value, "azimuth-api")
  assert_eq(service_attrs[1].key, "service.version")
  assert_eq(service_attrs[1].value, "2.1.0")
  assert_eq(service_attrs[2].key, "service.instance.id")
  assert_eq(service_attrs[2].value, "api-instance-001")
  assert_eq(service_attrs[3].key, "service.namespace")
  assert_eq(service_attrs[3].value, "production")
  assert_eq(service_attrs[4].key, "host.name")
  assert_eq(service_attrs[4].value, "api-server-01")
  assert_eq(service_attrs[5].key, "host.arch")
  assert_eq(service_attrs[5].value, "amd64")
  assert_eq(service_attrs[6].key, "os.type")
  assert_eq(service_attrs[6].value, "linux")
  assert_eq(service_attrs[7].key, "os.version")
  assert_eq(service_attrs[7].value, "ubuntu-20.04")
  assert_eq(service_attrs[8].key, "deployment.environment")
  assert_eq(service_attrs[8].value, "production")
  assert_eq(service_attrs[9].key, "telemetry.sdk.name")
  assert_eq(service_attrs[9].value, "azimuth")
  assert_eq(service_attrs[10].key, "telemetry.sdk.version")
  assert_eq(service_attrs[10].value, "1.0.0")
}

// 测试5: 跨服务上下文传播
test "跨服务上下文传播测试" {
  // 1. 创建源服务上下文
  let source_context = {
    trace_id: "trace-cross-123456789",
    baggage: [
      ("user.id", "user-12345"),
      ("request.id", "req-abcdef"),
      ("session.id", "session-987654321"),
      ("tenant.id", "tenant-001")
    ],
    entries: [
      ("service.name", "frontend-service"),
      ("service.version", "1.5.2"),
      ("deployment.environment", "production")
    ]
  }
  
  // 2. 创建传播载体
  let carrier = {
    headers: [
      ("traceparent", "00-trace-cross-123456789-span-111111111-01"),
      ("tracestate", "service=frontend,env=production"),
      ("baggage", "user.id=user-12345,request.id=req-abcdef,session.id=session-987654321,tenant.id=tenant-001"),
      ("x-request-id", "req-abcdef"),
      ("x-session-id", "session-987654321")
    ]
  }
  
  // 3. 验证传播载体
  assert_eq(carrier.headers.length(), 5)
  
  // 验证每个header
  assert_eq(carrier.headers[0], ("traceparent", "00-trace-cross-123456789-span-111111111-01"))
  assert_eq(carrier.headers[1], ("tracestate", "service=frontend,env=production"))
  assert_eq(carrier.headers[2], ("baggage", "user.id=user-12345,request.id=req-abcdef,session.id=session-987654321,tenant.id=tenant-001"))
  assert_eq(carrier.headers[3], ("x-request-id", "req-abcdef"))
  assert_eq(carrier.headers[4], ("x-session-id", "session-987654321"))
  
  // 验证traceparent包含trace_id
  assert_true(carrier.headers[0].1.contains("trace-cross-123456789"))
  
  // 验证baggage包含所有必要信息
  assert_true(carrier.headers[2].1.contains("user.id=user-12345"))
  assert_true(carrier.headers[2].1.contains("request.id=req-abcdef"))
  assert_true(carrier.headers[2].1.contains("session.id=session-987654321"))
  assert_true(carrier.headers[2].1.contains("tenant.id=tenant-001"))
}

// 测试6: 时间序列数据处理
test "时间序列数据处理测试" {
  // 1. 创建时间序列数据点
  let time_series_points = [
    { timestamp: 1640995200000, value: 100.5, host: "server-01", region: "us-east-1" },
    { timestamp: 1640995260000, value: 105.2, host: "server-01", region: "us-east-1" },
    { timestamp: 1640995320000, value: 98.7, host: "server-01", region: "us-east-1" },
    { timestamp: 1640995380000, value: 110.3, host: "server-01", region: "us-east-1" },
    { timestamp: 1640995440000, value: 115.8, host: "server-01", region: "us-east-1" }
  ]
  
  // 2. 创建时间序列
  let time_series = {
    name: "cpu.usage.percentage",
    description: "CPU使用率百分比",
    unit: "percent",
    data_points: time_series_points
  }
  
  // 3. 验证时间序列
  assert_eq(time_series.name, "cpu.usage.percentage")
  assert_eq(time_series.description, "CPU使用率百分比")
  assert_eq(time_series.unit, "percent")
  assert_eq(time_series.data_points.length(), 5)
  
  // 4. 验证数据点
  for i in 0..time_series.data_points.length() {
    let point = time_series.data_points[i]
    assert_true(point.timestamp >= 1640995200000)
    assert_true(point.timestamp <= 1640995440000)
    assert_true(point.value >= 90.0)
    assert_true(point.value <= 120.0)
    
    // 验证属性
    assert_eq(point.host, "server-01")
    assert_eq(point.region, "us-east-1")
  }
  
  // 5. 验证时间戳递增
  for i in 1..time_series.data_points.length() {
    let prev_point = time_series.data_points[i-1]
    let curr_point = time_series.data_points[i]
    assert_true(curr_point.timestamp > prev_point.timestamp)
  }
}

// 测试7: 错误处理和恢复
test "错误处理和恢复测试" {
  // 1. 创建错误span
  let error_span = {
    name: "api.request.handler",
    trace_id: "trace-error-123456789",
    span_id: "span-error-987654321",
    kind: "server",
    sampled: true,
    trace_state: "service=api,env=production",
    status: "ERROR",
    status_message: "Database connection timeout",
    exception_type: "TimeoutException",
    exception_message: "Connection timeout after 30 seconds",
    exception_stacktrace: "at com.example.Database.connect(Database.java:123)"
  }
  
  // 2. 验证错误span
  assert_eq(error_span.name, "api.request.handler")
  assert_eq(error_span.trace_id, "trace-error-123456789")
  assert_eq(error_span.span_id, "span-error-987654321")
  assert_eq(error_span.status, "ERROR")
  assert_eq(error_span.status_message, "Database connection timeout")
  assert_eq(error_span.exception_type, "TimeoutException")
  assert_eq(error_span.exception_message, "Connection timeout after 30 seconds")
  assert_eq(error_span.exception_stacktrace, "at com.example.Database.connect(Database.java:123)")
}

// 测试8: 配置管理
test "配置管理测试" {
  // 1. 创建遥测配置
  let telemetry_config = {
    service_name: "azimuth-api",
    service_version: "2.1.0",
    sampler_type: "trace_id_ratio",
    sampler_argument: 0.1,  // 10%采样率
    max_queue_size: 2048,
    scheduled_delay_millis: 5000,
    export_timeout_millis: 30000,
    max_export_batch_size: 512,
    export_interval_millis: 10000,
    attributes: [
      ("service.namespace", "production"),
      ("host.name", "api-server-01"),
      ("deployment.environment", "production")
    ]
  }
  
  // 2. 验证服务配置
  assert_eq(telemetry_config.service_name, "azimuth-api")
  assert_eq(telemetry_config.service_version, "2.1.0")
  
  // 3. 验证采样配置
  assert_eq(telemetry_config.sampler_type, "trace_id_ratio")
  assert_eq(telemetry_config.sampler_argument, 0.1)
  
  // 4. 验证批处理配置
  assert_eq(telemetry_config.max_queue_size, 2048)
  assert_eq(telemetry_config.scheduled_delay_millis, 5000)
  assert_eq(telemetry_config.export_timeout_millis, 30000)
  assert_eq(telemetry_config.max_export_batch_size, 512)
  
  // 5. 验证指标读取器配置
  assert_eq(telemetry_config.export_interval_millis, 10000)
  
  // 6. 验证资源属性
  assert_eq(telemetry_config.attributes.length(), 3)
  assert_eq(telemetry_config.attributes[0], ("service.namespace", "production"))
  assert_eq(telemetry_config.attributes[1], ("host.name", "api-server-01"))
  assert_eq(telemetry_config.attributes[2], ("deployment.environment", "production"))
}

// 测试9: 数据序列化和反序列化
test "数据序列化和反序列化测试" {
  // 1. 创建span用于序列化
  let span = {
    trace_id: "trace-serialize-123456789",
    span_id: "span-serialize-987654321",
    name: "http.request",
    kind: "server",
    sampled: true,
    trace_state: "service=api,env=production",
    parent_span_id: "span-parent-111111111",
    http_method: "GET",
    http_url: "https://api.example.com/users",
    http_status_code: "200",
    user_id: "user-12345",
    status: "OK",
    status_message: "Request completed successfully",
    start_time: 1640995200000,
    end_time: 1640995200500
  }
  
  // 2. 序列化span为JSON字符串
  let serialized_json = "{"
    + "\"trace_id\":\"" + span.trace_id + "\","
    + "\"span_id\":\"" + span.span_id + "\","
    + "\"name\":\"" + span.name + "\","
    + "\"kind\":\"" + span.kind + "\","
    + "\"sampled\":" + (if span.sampled { "true" } else { "false" }) + ","
    + "\"trace_state\":\"" + span.trace_state + "\","
    + "\"parent_span_id\":\"" + span.parent_span_id + "\","
    + "\"http_method\":\"" + span.http_method + "\","
    + "\"http_url\":\"" + span.http_url + "\","
    + "\"http_status_code\":\"" + span.http_status_code + "\","
    + "\"user_id\":\"" + span.user_id + "\","
    + "\"status\":\"" + span.status + "\","
    + "\"status_message\":\"" + span.status_message + "\","
    + "\"start_time\":" + span.start_time.to_string() + ","
    + "\"end_time\":" + span.end_time.to_string()
    + "}"
  
  // 3. 验证序列化结果包含必要字段
  assert_true(serialized_json.contains("trace_id"))
  assert_true(serialized_json.contains("span_id"))
  assert_true(serialized_json.contains("name"))
  assert_true(serialized_json.contains("kind"))
  assert_true(serialized_json.contains("status"))
  assert_true(serialized_json.contains("start_time"))
  assert_true(serialized_json.contains("end_time"))
  
  // 4. 模拟从JSON反序列化span（简化版）
  let deserialized_span = {
    trace_id: "trace-serialize-123456789",
    span_id: "span-serialize-987654321",
    name: "http.request",
    kind: "server",
    sampled: true,
    trace_state: "service=api,env=production",
    parent_span_id: "span-parent-111111111",
    http_method: "GET",
    http_url: "https://api.example.com/users",
    http_status_code: "200",
    user_id: "user-12345",
    status: "OK",
    status_message: "Request completed successfully",
    start_time: 1640995200000,
    end_time: 1640995200500
  }
  
  // 5. 验证反序列化结果
  assert_eq(deserialized_span.name, span.name)
  assert_eq(deserialized_span.trace_id, span.trace_id)
  assert_eq(deserialized_span.span_id, span.span_id)
  assert_eq(deserialized_span.sampled, span.sampled)
  assert_eq(deserialized_span.trace_state, span.trace_state)
  assert_eq(deserialized_span.parent_span_id, span.parent_span_id)
  assert_eq(deserialized_span.status, span.status)
  assert_eq(deserialized_span.status_message, span.status_message)
  assert_eq(deserialized_span.start_time, span.start_time)
  assert_eq(deserialized_span.end_time, span.end_time)
}

// 测试10: 仪表板数据聚合
test "仪表板数据聚合测试" {
  // 1. 创建多个时间序列数据点
  let cpu_points = [
    { timestamp: 1640995200000, value: 45.2, host: "server-01" },
    { timestamp: 1640995260000, value: 52.8, host: "server-01" },
    { timestamp: 1640995320000, value: 48.1, host: "server-01" },
    { timestamp: 1640995380000, value: 55.7, host: "server-01" },
    { timestamp: 1640995440000, value: 51.3, host: "server-01" }
  ]
  
  let memory_points = [
    { timestamp: 1640995200000, value: 68.4, host: "server-01" },
    { timestamp: 1640995260000, value: 70.2, host: "server-01" },
    { timestamp: 1640995320000, value: 72.8, host: "server-01" },
    { timestamp: 1640995380000, value: 69.5, host: "server-01" },
    { timestamp: 1640995440000, value: 71.1, host: "server-01" }
  ]
  
  let request_count_points = [
    { timestamp: 1640995200000, value: 120.0, host: "server-01" },
    { timestamp: 1640995260000, value: 135.0, host: "server-01" },
    { timestamp: 1640995320000, value: 128.0, host: "server-01" },
    { timestamp: 1640995380000, value: 142.0, host: "server-01" },
    { timestamp: 1640995440000, value: 138.0, host: "server-01" }
  ]
  
  // 2. 创建时间序列
  let cpu_series = {
    name: "cpu.usage.percentage",
    description: "CPU使用率百分比",
    unit: "percent",
    data_points: cpu_points
  }
  
  let memory_series = {
    name: "memory.usage.percentage",
    description: "内存使用率百分比",
    unit: "percent",
    data_points: memory_points
  }
  
  let request_count_series = {
    name: "http.requests.count",
    description: "HTTP请求计数",
    unit: "requests",
    data_points: request_count_points
  }
  
  // 3. 聚合数据
  let time_series_list = [cpu_series, memory_series, request_count_series]
  
  // 计算CPU指标聚合
  let mut cpu_sum = 0.0
  let mut cpu_min = cpu_points[0].value
  let mut cpu_max = cpu_points[0].value
  for point in cpu_points {
    cpu_sum = cpu_sum + point.value
    if point.value < cpu_min { cpu_min = point.value }
    if point.value > cpu_max { cpu_max = point.value }
  }
  let cpu_avg = cpu_sum / cpu_points.length().to_float()
  
  // 计算内存指标聚合
  let mut memory_sum = 0.0
  let mut memory_min = memory_points[0].value
  let mut memory_max = memory_points[0].value
  for point in memory_points {
    memory_sum = memory_sum + point.value
    if point.value < memory_min { memory_min = point.value }
    if point.value > memory_max { memory_max = point.value }
  }
  let memory_avg = memory_sum / memory_points.length().to_float()
  
  // 计算请求计数指标聚合
  let mut request_sum = 0.0
  let mut request_min = request_count_points[0].value
  let mut request_max = request_count_points[0].value
  for point in request_count_points {
    request_sum = request_sum + point.value
    if point.value < request_min { request_min = point.value }
    if point.value > request_max { request_max = point.value }
  }
  let request_avg = request_sum / request_count_points.length().to_float()
  
  // 4. 创建聚合结果
  let aggregated_metrics = [
    {
      metric_name: "cpu.usage.percentage",
      avg_value: cpu_avg,
      min_value: cpu_min,
      max_value: cpu_max,
      data_point_count: cpu_points.length(),
      total_value: cpu_sum
    },
    {
      metric_name: "memory.usage.percentage",
      avg_value: memory_avg,
      min_value: memory_min,
      max_value: memory_max,
      data_point_count: memory_points.length(),
      total_value: memory_sum
    },
    {
      metric_name: "http.requests.count",
      avg_value: request_avg,
      min_value: request_min,
      max_value: request_max,
      data_point_count: request_count_points.length(),
      total_value: request_sum
    }
  ]
  
  // 5. 验证聚合结果
  assert_eq(aggregated_metrics.length(), 3)
  
  // 验证CPU指标聚合
  assert_eq(aggregated_metrics[0].metric_name, "cpu.usage.percentage")
  assert_eq(aggregated_metrics[0].avg_value, 50.62)  // (45.2 + 52.8 + 48.1 + 55.7 + 51.3) / 5
  assert_eq(aggregated_metrics[0].min_value, 45.2)
  assert_eq(aggregated_metrics[0].max_value, 55.7)
  assert_eq(aggregated_metrics[0].data_point_count, 5)
  
  // 验证内存指标聚合
  assert_eq(aggregated_metrics[1].metric_name, "memory.usage.percentage")
  assert_eq(aggregated_metrics[1].avg_value, 70.4)  // (68.4 + 70.2 + 72.8 + 69.5 + 71.1) / 5
  assert_eq(aggregated_metrics[1].min_value, 68.4)
  assert_eq(aggregated_metrics[1].max_value, 72.8)
  assert_eq(aggregated_metrics[1].data_point_count, 5)
  
  // 验证请求计数指标聚合
  assert_eq(aggregated_metrics[2].metric_name, "http.requests.count")
  assert_eq(aggregated_metrics[2].avg_value, 132.6)  // (120 + 135 + 128 + 142 + 138) / 5
  assert_eq(aggregated_metrics[2].min_value, 120.0)
  assert_eq(aggregated_metrics[2].max_value, 142.0)
  assert_eq(aggregated_metrics[2].data_point_count, 5)
  assert_eq(aggregated_metrics[2].total_value, 663.0)  // 120 + 135 + 128 + 142 + 138
}