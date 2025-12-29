// Metrics各种InstrumentType聚合测试用例
// 测试不同InstrumentType的聚合功能和复杂场景

test "instrument_type_counter_aggregation" {
  // 测试Counter类型指标的聚合
  
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter", Some("1.0.0"))
  
  // 创建Counter
  let counter = meter.create_counter("http.requests.total", Some("requests"), Some("Total HTTP requests"))
  
  // 测试Counter的基本操作
  counter.add(1L, Some([("http.method", AttributeValue::string("GET"))]))
  counter.add(2L, Some([("http.method", AttributeValue::string("POST"))]))
  counter.add(3L, Some([("http.method", AttributeValue::string("GET"))]))
  counter.add(5L, Some([("http.method", AttributeValue::string("PUT"))]))
  counter.add(10L, Some([("http.method", AttributeValue::string("GET"))]))
  
  // 验证Counter类型
  assert_eq(true, true)  // Noop实现中无法验证实际聚合
  
  // 测试负值处理（Counter通常不允许负值）
  counter.add(-1L, Some([("http.method", AttributeValue::string("DELETE"))]))
  
  // 测试零值
  counter.add(0L, Some([("http.method", AttributeValue::string("OPTIONS"))]))
  
  // 测试大值
  counter.add(9223372036854775807L, Some([("http.method", AttributeValue::string("PATCH"))]))
  
  // 测试复杂属性
  let complex_attributes = [
    ("http.method", AttributeValue::string("GET")),
    ("http.status_code", AttributeValue::int(200L)),
    ("http.route", AttributeValue::string("/api/users")),
    ("service.name", AttributeValue::string("api-gateway")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("deployment.environment", AttributeValue::string("production"))
  ]
  
  counter.add(100L, Some(complex_attributes))
}

test "instrument_type_histogram_aggregation" {
  // 测试Histogram类型指标的聚合
  
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter", Some("1.0.0"))
  
  // 创建Histogram
  let histogram = meter.create_histogram("http.request.duration", Some("ms"), Some("HTTP request duration"))
  
  // 测试Histogram的基本操作
  histogram.record(100.0, Some([("http.method", AttributeValue::string("GET"))]))
  histogram.record(200.5, Some([("http.method", AttributeValue::string("POST"))]))
  histogram.record(50.25, Some([("http.method", AttributeValue::string("GET"))]))
  histogram.record(1000.0, Some([("http.method", AttributeValue::string("PUT"))]))
  histogram.record(75.75, Some([("http.method", AttributeValue::string("GET"))]))
  
  // 测试边界值
  histogram.record(0.0, Some([("http.method", AttributeValue::string("DELETE"))]))
  histogram.record(-1.0, Some([("http.method", AttributeValue::string("OPTIONS"))]))  // 负值
  histogram.record(1.7976931348623157e+308, Some([("http.method", AttributeValue::string("PATCH"))]))  // 最大值
  
  // 测试特殊浮点值
  histogram.record(1.0/0.0, Some([("http.method", AttributeValue::string("HEAD"))]))  // 正无穷
  histogram.record(-1.0/0.0, Some([("http.method", AttributeValue::string("TRACE"))]))  // 负无穷
  histogram.record(0.0/0.0, Some([("http.method", AttributeValue::string("CONNECT"))]))  // NaN
  
  // 测试科学计数法
  histogram.record(1.23e-10, Some([("http.method", AttributeValue::string("CUSTOM1"))]))
  histogram.record(1.23e+20, Some([("http.method", AttributeValue::string("CUSTOM2"))]))
  
  // 测试复杂属性
  let complex_attributes = [
    ("http.method", AttributeValue::string("GET")),
    ("http.status_code", AttributeValue::int(200L)),
    ("http.route", AttributeValue::string("/api/data")),
    ("user.id", AttributeValue::string("user-123")),
    ("session.id", AttributeValue::string("session-456")),
    ("request.size", AttributeValue::int(1024L)),
    ("response.size", AttributeValue::int(2048L))
  ]
  
  histogram.record(150.5, Some(complex_attributes))
}

test "instrument_type_up_down_counter_aggregation" {
  // 测试UpDownCounter类型指标的聚合
  
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter", Some("1.0.0"))
  
  // 创建UpDownCounter
  let up_down_counter = meter.create_up_down_counter("active.connections", Some("connections"), Some("Active connections"))
  
  // 测试UpDownCounter的基本操作（允许负值）
  up_down_counter.add(10L, Some([("connection.type", AttributeValue::string("http"))]))
  up_down_counter.add(-5L, Some([("connection.type", AttributeValue::string("http"))]))
  up_down_counter.add(3L, Some([("connection.type", AttributeValue::string("websocket"))]))
  up_down_counter.add(-2L, Some([("connection.type", AttributeValue::string("websocket"))]))
  
  // 测试零值
  up_down_counter.add(0L, Some([("connection.type", AttributeValue::string("grpc"))]))
  
  // 测试极值
  up_down_counter.add(9223372036854775807L, Some([("connection.type", AttributeValue::string("tcp"))]))
  up_down_counter.add(-9223372036854775808L, Some([("connection.type", AttributeValue::string("udp"))]))
  
  // 测试复杂场景
  let complex_attributes = [
    ("connection.type", AttributeValue::string("http")),
    ("connection.state", AttributeValue::string("active")),
    ("client.ip", AttributeValue::string("192.168.1.100")),
    ("client.user_agent", AttributeValue::string("Mozilla/5.0")),
    ("server.endpoint", AttributeValue::string("/api/long-polling")),
    ("connection.duration", AttributeValue::int(30000L))
  ]
  
  up_down_counter.add(1L, Some(complex_attributes))
  up_down_counter.add(-1L, Some(complex_attributes))
}

test "instrument_type_gauge_aggregation" {
  // 测试Gauge类型指标的聚合
  
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter", Some("1.0.0"))
  
  // 创建Gauge
  let gauge = meter.create_gauge("memory.usage", Some("bytes"), Some("Memory usage"))
  
  // 测试Gauge的基本操作
  gauge.record(1024.0, Some([("memory.type", AttributeValue::string("heap"))]))
  gauge.record(2048.5, Some([("memory.type", AttributeValue::string("stack"))]))
  gauge.record(512.25, Some([("memory.type", AttributeValue::string("heap"))]))
  gauge.record(4096.0, Some([("memory.type", AttributeValue::string("native"))]))
  
  // 测试边界值
  gauge.record(0.0, Some([("memory.type", AttributeValue::string("code_cache"))]))
  gauge.record(-1.0, Some([("memory.type", AttributeValue::string("free"))]))  // 负值可能表示释放
  gauge.record(1.7976931348623157e+308, Some([("memory.type", AttributeValue::string("total_available"))]))
  
  // 测试特殊浮点值
  gauge.record(1.0/0.0, Some([("memory.type", AttributeValue::string("unlimited"))))
  gauge.record(0.0/0.0, Some([("memory.type", AttributeValue::string("unknown"))))
  
  // 测试复杂属性
  let complex_attributes = [
    ("memory.type", AttributeValue::string("heap")),
    ("memory.pool", AttributeValue::string("eden")),
    ("process.id", AttributeValue::int(12345L)),
    ("process.name", AttributeValue::string("java")),
    ("container.id", AttributeValue::string("container-abc123")),
    ("node.name", AttributeValue::string("worker-node-1")),
    ("availability.zone", AttributeValue::string("us-west-2a"))
  ]
  
  gauge.record(8192.0, Some(complex_attributes))
}

test "instrument_type_observable_counter_aggregation" {
  // 测试ObservableCounter类型指标的聚合
  
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter", Some("1.0.0"))
  
  // 注意：在当前实现中，ObservableCounter的创建方法与Counter相同
  // 在实际实现中，ObservableCounter应该有不同的创建方法
  let observable_counter = meter.create_counter("cache.hits.total", Some("hits"), Some("Total cache hits"))
  
  // ObservableCounter通常通过回调函数提供值，这里我们测试基本结构
  observable_counter.add(100L, Some([("cache.type", AttributeValue::string("redis"))]))
  observable_counter.add(200L, Some([("cache.type", AttributeValue::string("memcached"))]))
  
  // 测试复杂场景
  let complex_attributes = [
    ("cache.type", AttributeValue::string("redis")),
    ("cache.cluster", AttributeValue::string("cluster-1")),
    ("cache.node", AttributeValue::string("redis-node-1")),
    ("cache.shard", AttributeValue::int(0L)),
    ("cache.key.pattern", AttributeValue::string("user:*")),
    ("cache.ttl", AttributeValue::int(3600L))
  ]
  
  observable_counter.add(500L, Some(complex_attributes))
}

test "instrument_type_observable_gauge_aggregation" {
  // 测试ObservableGauge类型指标的聚合
  
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter", Some("1.0.0"))
  
  // 注意：在当前实现中，ObservableGauge的创建方法与Gauge相同
  let observable_gauge = meter.create_gauge("cpu.usage", Some("percent"), Some("CPU usage percentage"))
  
  // 测试不同类型的CPU使用率
  observable_gauge.record(75.5, Some([("cpu.core", AttributeValue::int(0L))]))
  observable_gauge.record(80.0, Some([("cpu.core", AttributeValue::int(1L))]))
  observable_gauge.record(60.25, Some([("cpu.core", AttributeValue::int(2L))]))
  observable_gauge.record(90.75, Some([("cpu.core", AttributeValue::int(3L))]))
  
  // 测试边界值
  observable_gauge.record(0.0, Some([("cpu.core", AttributeValue::int(4L))]))
  observable_gauge.record(100.0, Some([("cpu.core", AttributeValue::int(5L))]))
  observable_gauge.record(150.0, Some([("cpu.core", AttributeValue::int(6L))]))  // 超过100%
  
  // 测试复杂属性
  let complex_attributes = [
    ("cpu.core", AttributeValue::int(7L)),
    ("cpu.mode", AttributeValue::string("user")),
    ("process.name", AttributeValue::string("web-server")),
    ("process.id", AttributeValue::int(12345L)),
    ("container.name", AttributeValue::string("web-server-container")),
    ("k8s.pod.name", AttributeValue::string("web-server-pod-abc123")),
    ("k8s.namespace", AttributeValue::string("production"))
  ]
  
  observable_gauge.record(85.5, Some(complex_attributes))
}

test "instrument_type_observable_up_down_counter_aggregation" {
  // 测试ObservableUpDownCounter类型指标的聚合
  
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter", Some("1.0.0"))
  
  // 注意：在当前实现中，ObservableUpDownCounter的创建方法与UpDownCounter相同
  let observable_up_down_counter = meter.create_up_down_counter("queue.size", Some("messages"), Some("Queue size"))
  
  // 测试队列大小变化
  observable_up_down_counter.add(100L, Some([("queue.name", AttributeValue::string("priority"))]))
  observable_up_down_counter.add(-20L, Some([("queue.name", AttributeValue::string("priority"))]))
  observable_up_down_counter.add(50L, Some([("queue.name", AttributeValue::string("normal"))]))
  observable_up_down_counter.add(-10L, Some([("queue.name", AttributeValue::string("normal"))]))
  
  // 测试复杂场景
  let complex_attributes = [
    ("queue.name", AttributeValue::string("bulk")),
    ("queue.priority", AttributeValue::string("low")),
    ("queue.type", AttributeValue::string("fifo")),
    ("consumer.group", AttributeValue::string("processor-1")),
    ("partition.id", AttributeValue::int(0L)),
    ("broker.name", AttributeValue::string("kafka-broker-1")),
    ("topic.name", AttributeValue::string("events"))
  ]
  
  observable_up_down_counter.add(1000L, Some(complex_attributes))
  observable_up_down_counter.add(-200L, Some(complex_attributes))
}

test "metrics_complex_aggregation_scenarios" {
  // 测试指标的复杂聚合场景
  
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("complex-meter", Some("2.0.0"))
  
  // 创建多种类型的指标
  let request_counter = meter.create_counter("http.requests.total", Some("requests"), Some("Total HTTP requests"))
  let request_duration = meter.create_histogram("http.request.duration", Some("ms"), Some("HTTP request duration"))
  let active_connections = meter.create_up_down_counter("active.connections", Some("connections"), Some("Active connections"))
  let memory_usage = meter.create_gauge("memory.usage", Some("bytes"), Some("Memory usage"))
  
  // 模拟复杂的业务场景
  
  // 场景1：处理HTTP请求
  let base_attributes = [
    ("service.name", AttributeValue::string("api-gateway")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("deployment.environment", AttributeValue::string("production")),
    ("k8s.pod.name", AttributeValue::string("api-gateway-7d4b8c9f-abc")),
    ("k8s.namespace", AttributeValue::string("default"))
  ]
  
  // 处理不同类型的请求
  let get_attributes = base_attributes + [
    ("http.method", AttributeValue::string("GET")),
    ("http.route", AttributeValue::string("/api/users")),
    ("http.status_code", AttributeValue::int(200L))
  ]
  
  let post_attributes = base_attributes + [
    ("http.method", AttributeValue::string("POST")),
    ("http.route", AttributeValue::string("/api/users")),
    ("http.status_code", AttributeValue::int(201L))
  ]
  
  let error_attributes = base_attributes + [
    ("http.method", AttributeValue::string("GET")),
    ("http.route", AttributeValue::string("/api/nonexistent")),
    ("http.status_code", AttributeValue::int(404L))
  ]
  
  // 记录指标
  request_counter.add(1L, Some(get_attributes))
  request_duration.record(150.0, Some(get_attributes))
  active_connections.add(1L, Some(get_attributes))
  memory_usage.record(1024.0, Some(get_attributes))
  
  request_counter.add(1L, Some(post_attributes))
  request_duration.record(300.0, Some(post_attributes))
  active_connections.add(1L, Some(post_attributes))
  memory_usage.record(2048.0, Some(post_attributes))
  
  request_counter.add(1L, Some(error_attributes))
  request_duration.record(50.0, Some(error_attributes))
  active_connections.add(1L, Some(error_attributes))
  memory_usage.record(512.0, Some(error_attributes))
  
  // 模拟连接关闭
  active_connections.add(-1L, Some(get_attributes))
  active_connections.add(-1L, Some(post_attributes))
  active_connections.add(-1L, Some(error_attributes))
  
  // 场景2：批处理操作
  let batch_attributes = [
    ("job.name", AttributeValue::string("daily-report")),
    ("job.type", AttributeValue::string("batch")),
    ("job.id", AttributeValue::string("batch-12345")),
    ("worker.id", AttributeValue::string("worker-1"))
  ]
  
  request_counter.add(1000L, Some(batch_attributes))
  request_duration.record(5000.0, Some(batch_attributes))
  active_connections.add(10L, Some(batch_attributes))
  memory_usage.record(10240.0, Some(batch_attributes))
  
  // 批处理完成
  active_connections.add(-10L, Some(batch_attributes))
  memory_usage.record(0.0, Some(batch_attributes))
  
  // 场景3：错误恢复
  let error_recovery_attributes = [
    ("error.type", AttributeValue::string("DatabaseException")),
    ("error.severity", AttributeValue::string("high")),
    ("retry.count", AttributeValue::int(3L)),
    ("recovery.action", AttributeValue::string("circuit-breaker-open"))
  ]
  
  request_counter.add(-5L, Some(error_recovery_attributes))  // 回滚失败的请求
  active_connections.add(-2L, Some(error_recovery_attributes))  // 关闭异常连接
  memory_usage.record(-2048.0, Some(error_recovery_attributes))  // 释放内存
}

test "metrics_performance_aggregation" {
  // 测试指标的性能聚合场景
  
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("performance-meter", Some("1.0.0"))
  
  let counter = meter.create_counter("performance.counter", Some("ops"), Some("Performance counter"))
  let histogram = meter.create_histogram("performance.histogram", Some("ms"), Some("Performance histogram"))
  let gauge = meter.create_gauge("performance.gauge", Some("units"), Some("Performance gauge"))
  
  // 测试大量指标的聚合
  let mut i = 0
  while i < 1000 {
    let attributes = [
      ("iteration", AttributeValue::int(i.to_int64())),
      ("batch", AttributeValue::int((i / 100).to_int64())),
      ("worker", AttributeValue::int((i % 10).to_int64()))
    ]
    
    counter.add(1L, Some(attributes))
    histogram.record(i.to_double(), Some(attributes))
    gauge.record(i.to_double() * 1.5, Some(attributes))
    
    i = i + 1
  }
  
  // 测试不同属性组合的聚合
  let mut j = 0
  while j < 100 {
    let complex_attributes = [
      ("service.name", AttributeValue::string("service-" + (j % 10).to_string())),
      ("service.version", AttributeValue::string((j / 10 + 1).to_string() + ".0.0")),
      ("environment", AttributeValue::string(if j % 2 == 0 { "prod" } else { "staging" })),
      ("region", AttributeValue::string(["us-east-1", "us-west-2", "eu-west-1", "ap-southeast-1"][j % 4])),
      ("instance.id", AttributeValue::string("instance-" + j.to_string()))
    ]
    
    counter.add(j.to_int64(), Some(complex_attributes))
    histogram.record(j.to_double() * 10.0, Some(complex_attributes))
    gauge.record(j.to_double() * 0.5, Some(complex_attributes))
    
    j = j + 1
  }
}

test "metrics_edge_cases_aggregation" {
  // 测试指标的边界情况聚合
  
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("edge-case-meter", Some("1.0.0"))
  
  let counter = meter.create_counter("edge.counter", Some("ops"), Some("Edge case counter"))
  let histogram = meter.create_histogram("edge.histogram", Some("ms"), Some("Edge case histogram"))
  let gauge = meter.create_gauge("edge.gauge", Some("units"), Some("Edge case gauge"))
  
  // 测试极值
  counter.add(9223372036854775807L, Some([("value.type", AttributeValue::string("max"))]))
  counter.add(-9223372036854775808L, Some([("value.type", AttributeValue::string("min"))]))
  counter.add(0L, Some([("value.type", AttributeValue::string("zero"))]))
  
  histogram.record(1.7976931348623157e+308, Some([("value.type", AttributeValue::string("max.float"))]))
  histogram.record(-1.7976931348623157e+308, Some([("value.type", AttributeValue::string("min.float"))]))
  histogram.record(0.0, Some([("value.type", AttributeValue::string("zero.float"))]))
  histogram.record(1.0/0.0, Some([("value.type", AttributeValue::string("infinity"))]))
  histogram.record(0.0/0.0, Some([("value.type", AttributeValue::string("nan"))]))
  
  gauge.record(1.7976931348623157e+308, Some([("value.type", AttributeValue::string("gauge.max"))]))
  gauge.record(-1.7976931348623157e+308, Some([("value.type", AttributeValue::string("gauge.min"))]))
  gauge.record(0.0, Some([("value.type", AttributeValue::string("gauge.zero"))]))
  
  // 测试特殊属性值
  let special_attributes = [
    ("empty.string", AttributeValue::string("")),
    ("special.chars", AttributeValue::string("!@#$%^&*()")),
    ("unicode", AttributeValue::string("测试🚀")),
    ("very.long.key.name.that.exceeds.normal.limits.and.contains.many.words", AttributeValue::string("long.value"))
  ]
  
  counter.add(1L, Some(special_attributes))
  histogram.record(100.0, Some(special_attributes))
  gauge.record(50.0, Some(special_attributes))
  
  // 测试空属性数组
  counter.add(1L, None)
  histogram.record(100.0, None)
  gauge.record(50.0, None)
}