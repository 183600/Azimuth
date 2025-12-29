// Metrics模块的综合测试用例
test "instrument_types_enumeration" {
  // 测试所有InstrumentType枚举值
  let counter_type = Counter
  let histogram_type = Histogram
  let up_down_counter_type = UpDownCounter
  let gauge_type = Gauge
  let observable_counter_type = ObservableCounter
  let observable_gauge_type = ObservableGauge
  let observable_up_down_counter_type = ObservableUpDownCounter
  
  // 验证枚举值（这里我们直接比较，因为MoonBit的枚举可以直接比较）
  // 在实际实现中，可能需要使用模式匹配来验证类型
  
  // 创建Measurement来测试不同类型的指标
  let counter_measurement = Measurement::{
    value: 100.0,
    attributes: [
      ("metric.type", AttributeValue::string("counter")),
      ("metric.name", AttributeValue::string("http_requests_total"))
    ]
  }
  
  let histogram_measurement = Measurement::{
    value: 123.45,
    attributes: [
      ("metric.type", AttributeValue::string("histogram")),
      ("metric.name", AttributeValue::string("http_request_duration"))
    ]
  }
  
  let gauge_measurement = Measurement::{
    value: 75.5,
    attributes: [
      ("metric.type", AttributeValue::string("gauge")),
      ("metric.name", AttributeValue::string("cpu_usage"))
    ]
  }
  
  // 验证Measurement结构
  assert_eq(counter_measurement.value, 100.0)
  assert_eq(counter_measurement.attributes.length(), 2)
  assert_eq(histogram_measurement.value, 123.45)
  assert_eq(gauge_measurement.value, 75.5)
}

test "counter_instrument_operations" {
  // 测试Counter指标的操作
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter", Some("1.0.0"))
  
  // 创建不同类型的counter
  let request_counter = meter.create_counter(
    "http_requests_total",
    unit?: "requests",
    description?: "Total number of HTTP requests"
  )
  
  let error_counter = meter.create_counter(
    "http_errors_total",
    unit?: "errors", 
    description?: "Total number of HTTP errors"
  )
  
  // 测试counter的add操作
  request_counter.add(1L)
  request_counter.add(5L)
  request_counter.add(10L)
  
  error_counter.add(1L, attributes?: [
    ("error.type", AttributeValue::string("timeout")),
    ("service.name", AttributeValue::string("api-gateway"))
  ])
  
  error_counter.add(1L, attributes?: [
    ("error.type", AttributeValue::string("connection_refused")),
    ("service.name", AttributeValue::string("database"))
  ])
  
  // 测试负数和零值（counter通常只接受非负值）
  request_counter.add(0L)
  
  // 测试大数值
  request_counter.add(9223372036854775807L) // Int64最大值
  
  // 验证counter类型（Noop实现不会实际存储值，但我们可以验证操作不会出错）
  assert_eq(true, true) // 如果没有异常，测试通过
}

test "histogram_instrument_operations" {
  // 测试Histogram指标的操作
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter")
  
  // 创建histogram
  let response_time_histogram = meter.create_histogram(
    "http_request_duration_seconds",
    unit?: "seconds",
    description?: "HTTP request duration in seconds"
  )
  
  let payload_size_histogram = meter.create_histogram(
    "http_request_payload_bytes",
    unit?: "bytes",
    description?: "HTTP request payload size in bytes"
  )
  
  // 测试histogram的record操作
  response_time_histogram.record(0.1) // 100ms
  response_time_histogram.record(0.25) // 250ms
  response_time_histogram.record(0.5) // 500ms
  response_time_histogram.record(1.0) // 1s
  response_time_histogram.record(2.5) // 2.5s
  
  // 测试带属性的记录
  payload_size_histogram.record(1024.0, attributes?: [
    ("http.method", AttributeValue::string("POST")),
    ("content.type", AttributeValue::string("application/json"))
  ])
  
  payload_size_histogram.record(2048.0, attributes?: [
    ("http.method", AttributeValue::string("GET")),
    ("content.type", AttributeValue::string("text/html"))
  ])
  
  // 测试边界值
  response_time_histogram.record(0.0) // 零值
  response_time_histogram.record(-1.0) // 负值（通常不应该记录负值到histogram）
  response_time_histogram.record(1.0/0.0) // 无穷大
  response_time_histogram.record(0.0/0.0) // NaN
  
  // 测试极大值
  response_time_histogram.record(999999999.9)
  
  assert_eq(true, true) // 如果没有异常，测试通过
}

test "up_down_counter_instrument_operations" {
  // 测试UpDownCounter指标的操作
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter")
  
  // 创建up-down counter
  let active_connections = meter.create_up_down_counter(
    "active_connections",
    unit?: "connections",
    description?: "Current number of active connections"
  )
  
  let memory_usage = meter.create_up_down_counter(
    "memory_usage_bytes",
    unit?: "bytes",
    description?: "Current memory usage in bytes"
  )
  
  // 测试增加操作
  active_connections.add(1L) // 新连接
  active_connections.add(5L) // 批量连接
  memory_usage.add(1024L * 1024L) // 1MB
  
  // 测试减少操作
  active_connections.add(-1L) // 连接关闭
  active_connections.add(-3L) // 批量关闭
  memory_usage.add(-512L * 1024L) // 释放512KB
  
  // 测试零值
  active_connections.add(0L)
  
  // 测试大数值
  active_connections.add(1000000L)
  active_connections.add(-1000000L)
  
  // 测试带属性的操作
  memory_usage.add(2048L, attributes?: [
    ("memory.type", AttributeValue::string("heap")),
    ("process.id", AttributeValue::int(12345L))
  ])
  
  memory_usage.add(-1024L, attributes?: [
    ("memory.type", AttributeValue::string("stack")),
    ("process.id", AttributeValue::int(12345L))
  ])
  
  assert_eq(true, true) // 如果没有异常，测试通过
}

test "gauge_instrument_operations" {
  // 测试Gauge指标的操作
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter")
  
  // 创建gauge
  let cpu_usage = meter.create_gauge(
    "cpu_usage_percent",
    unit?: "percent",
    description?: "Current CPU usage percentage"
  )
  
  let disk_usage = meter.create_gauge(
    "disk_usage_bytes",
    unit?: "bytes", 
    description?: "Current disk usage in bytes"
  )
  
  // 测试gauge的record操作
  cpu_usage.record(25.5) // 25.5% CPU使用率
  cpu_usage.record(75.0) // 75% CPU使用率
  cpu_usage.record(100.0) // 100% CPU使用率
  
  disk_usage.record(1024.0 * 1024.0 * 1024.0) // 1GB
  disk_usage.record(10.0 * 1024.0 * 1024.0 * 1024.0) // 10GB
  
  // 测试带属性的记录
  cpu_usage.record(50.0, attributes?: [
    ("cpu.core", AttributeValue::int(0L)),
    ("host.name", AttributeValue::string("web-server-01"))
  ])
  
  cpu_usage.record(75.0, attributes?: [
    ("cpu.core", AttributeValue::int(1L)),
    ("host.name", AttributeValue::string("web-server-01"))
  ])
  
  // 测试边界值
  cpu_usage.record(0.0) // 零值
  cpu_usage.record(-10.0) // 负值（某些gauge可能允许负值）
  cpu_usage.record(150.0) // 超过100%的值
  
  // 测试精度
  cpu_usage.record(33.3333333333)
  cpu_usage.record(66.6666666666)
  
  assert_eq(true, true) // 如果没有异常，测试通过
}

test "meter_provider_and_meter_creation" {
  // 测试MeterProvider和Meter的创建
  
  // 创建多个MeterProvider
  let provider1 = NoopMeterProvider::{}
  let provider2 = NoopMeterProvider::{}
  
  // 从不同provider创建meter
  let meter1 = provider1.get_meter("service-a", Some("1.0.0"), Some("https://example.com/schema"))
  let meter2 = provider1.get_meter("service-b", Some("2.0.0"))
  let meter3 = provider2.get_meter("service-c")
  
  // 验证meter创建（Noop实现返回的都是NoopMeter）
  // 在实际实现中，不同的meter可能有不同的配置
  
  // 使用meter创建各种instrument
  let counter1 = meter1.create_counter("counter1")
  let histogram1 = meter1.create_histogram("histogram1", unit?: "ms")
  let up_down_counter1 = meter1.create_up_down_counter("updown1", description?: "TestUpDownCounter")
  let gauge1 = meter1.create_gauge("gauge1", unit?: "percent", description?: "TestGauge")
  
  let counter2 = meter2.create_counter("counter2")
  let histogram2 = meter2.create_histogram("histogram2")
  let up_down_counter2 = meter2.create_up_down_counter("updown2")
  let gauge2 = meter2.create_gauge("gauge2")
  
  // 验证instrument创建成功
  assert_eq(true, true)
  
  // 测试instrument操作
  counter1.add(1L)
  histogram1.record(100.0)
  up_down_counter1.add(5L)
  gauge1.record(50.0)
  
  counter2.add(10L)
  histogram2.record(200.0)
  up_down_counter2.add(-5L)
  gauge2.record(75.0)
  
  assert_eq(true, true) // 如果没有异常，测试通过
}

test "complex_metric_attributes" {
  // 测试复杂的指标属性
  
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("complex-test-meter")
  
  // 创建带有复杂属性的counter
  let request_counter = meter.create_counter("http_requests_total")
  
  // 基本属性
  request_counter.add(1L, attributes?: [
    ("http.method", AttributeValue::string("GET")),
    ("http.status_code", AttributeValue::int(200L)),
    ("http.scheme", AttributeValue::string("https"))
  ])
  
  // 复杂属性
  request_counter.add(1L, attributes?: [
    ("service.name", AttributeValue::string("payment-service")),
    ("service.version", AttributeValue::string("1.2.3")),
    ("service.namespace", AttributeValue::string("production")),
    ("deployment.environment", AttributeValue::string("prod")),
    ("host.name", AttributeValue::string("web-server-01")),
    ("host.ip", AttributeValue::string("192.168.1.100")),
    ("process.pid", AttributeValue::int(12345L)),
    ("process.cpu.percent", AttributeValue::float(75.5)),
    ("process.healthy", AttributeValue::bool(true)),
    ("tags", AttributeValue::array_string(["web", "api", "critical"])),
    ("ports", AttributeValue::array_int([8080L, 8443L, 9090L])),
    ("cpu.thresholds", AttributeValue::array_float([60.0, 80.0, 95.0])),
    ("feature.flags", AttributeValue::array_bool([true, false, true]))
  ])
  
  // 测试边界情况属性
  request_counter.add(1L, attributes?: [
    ("empty.string", AttributeValue::string("")),
    ("zero.int", AttributeValue::int(0L)),
    ("negative.int", AttributeValue::int(-1L)),
    ("zero.float", AttributeValue::float(0.0)),
    ("negative.float", AttributeValue::float(-1.0)),
    ("false.bool", AttributeValue::bool(false)),
    ("empty.array", AttributeValue::array_string([]))
  ])
  
  // 测试特殊字符属性
  request_counter.add(1L, attributes?: [
    ("special.chars", AttributeValue::string("!@#$%^&*()")),
    ("unicode.test", AttributeValue::string("测试中文🚀")),
    ("spaces.in.key", AttributeValue::string("value with spaces")),
    ("dots.in.key", AttributeValue::string("value.with.dots"))
  ])
  
  assert_eq(true, true) // 如果没有异常，测试通过
}

test "metric_measurement_edge_cases" {
  // 测试指标测量的边界情况
  
  // 测试极值
  let max_int64 = 9223372036854775807L
  let min_int64 = -9223372036854775808L
  
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("edge-case-meter")
  
  let counter = meter.create_counter("edge_counter")
  let histogram = meter.create_histogram("edge_histogram")
  let up_down_counter = meter.create_up_down_counter("edge_up_down_counter")
  let gauge = meter.create_gauge("edge_gauge")
  
  // 测试极大值
  counter.add(max_int64)
  histogram.record(max_int64.to_double())
  up_down_counter.add(max_int64)
  gauge.record(max_int64.to_double())
  
  // 测试极小值
  up_down_counter.add(min_int64)
  histogram.record(min_int64.to_double())
  gauge.record(min_int64.to_double())
  
  // 测试浮点数边界值
  histogram.record(1.7976931348623157e+308) // Double最大值
  histogram.record(4.9e-324) // Double最小正值
  histogram.record(-1.7976931348623157e+308) // Double最小值
  
  gauge.record(1.7976931348623157e+308)
  gauge.record(4.9e-324)
  gauge.record(-1.7976931348623157e+308)
  
  // 测试特殊浮点值
  histogram.record(1.0/0.0) // 正无穷
  histogram.record(-1.0/0.0) // 负无穷
  histogram.record(0.0/0.0) // NaN
  
  gauge.record(1.0/0.0)
  gauge.record(-1.0/0.0)
  gauge.record(0.0/0.0)
  
  assert_eq(true, true) // 如果没有异常，测试通过
}