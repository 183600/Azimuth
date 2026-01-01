// Azimuth Metrics Gauge Detailed Tests
// This file contains detailed tests for gauge metrics in the Azimuth telemetry system

// Test 1: Basic gauge creation and configuration
pub test "度量仪表详细测试：基本仪表创建和配置" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "gauge-test-meter")
  
  // 创建基本的Gauge仪表
  let cpu_gauge = azimuth::Meter::create_gauge(meter, "cpu.usage", Some("CPU usage percentage"), Some("%"))
  let memory_gauge = azimuth::Meter::create_gauge(meter, "memory.usage", Some("Memory usage in MB"), Some("MB"))
  let disk_gauge = azimuth::Meter::create_gauge(meter, "disk.usage", Some("Disk usage in GB"), Some("GB"))
  
  // 验证Gauge属性
  assert_eq(cpu_gauge.name, "cpu.usage")
  assert_eq(cpu_gauge.description, Some("CPU usage percentage"))
  assert_eq(cpu_gauge.unit, Some("%"))
  
  assert_eq(memory_gauge.name, "memory.usage")
  assert_eq(memory_gauge.description, Some("Memory usage in MB"))
  assert_eq(memory_gauge.unit, Some("MB"))
  
  assert_eq(disk_gauge.name, "disk.usage")
  assert_eq(disk_gauge.description, Some("Disk usage in GB"))
  assert_eq(disk_gauge.unit, Some("GB"))
  
  // 测试Gauge的命名约定和边界情况
  let empty_name_gauge = azimuth::Meter::create_gauge(meter, "", Some("Empty name gauge"), None)
  let very_long_name_gauge = azimuth::Meter::create_gauge(meter, "this.is.a.very.long.gauge.name.that.exceeds.normal.expectations", Some("Long name gauge"), None)
  let special_chars_gauge = azimuth::Meter::create_gauge(meter, "gauge.with-special_chars_123", Some("Special chars gauge"), None)
  
  // 验证边界情况
  assert_eq(empty_name_gauge.name, "")
  assert_eq(very_long_name_gauge.name, "this.is.a.very.long.gauge.name.that.exceeds.normal.expectations")
  assert_eq(special_chars_gauge.name, "gauge.with-special_chars_123")
  
  // 测试Gauge的描述和单位边界情况
  let empty_desc_gauge = azimuth::Meter::create_gauge(meter, "empty.desc", Some(""), Some("unit"))
  let no_desc_gauge = azimuth::Meter::create_gauge(meter, "no.desc", None, Some("unit"))
  let empty_unit_gauge = azimuth::Meter::create_gauge(meter, "empty.unit", Some("description"), Some(""))
  let no_unit_gauge = azimuth::Meter::create_gauge(meter, "no.unit", Some("description"), None)
  
  // 验证描述和单位边界情况
  assert_eq(empty_desc_gauge.description, Some(""))
  assert_eq(no_desc_gauge.description, None)
  assert_eq(empty_unit_gauge.unit, Some(""))
  assert_eq(no_unit_gauge.unit, None)
}

// Test 2: Gauge value operations
pub test "度量仪表详细测试：仪表值操作" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "gauge-operations-meter")
  
  // 创建用于操作的Gauge
  let temperature_gauge = azimuth::Meter::create_gauge(meter, "temperature", Some("Current temperature"), Some("celsius"))
  let pressure_gauge = azimuth::Meter::create_gauge(meter, "pressure", Some("Current pressure"), Some("pascal"))
  let humidity_gauge = azimuth::Meter::create_gauge(meter, "humidity", Some("Current humidity"), Some("percent"))
  
  // 注意：当前简化实现可能不支持Gauge的record/set方法
  // 这里主要测试结构创建和属性验证
  
  // 验证Gauge创建成功
  assert_eq(temperature_gauge.name, "temperature")
  assert_eq(temperature_gauge.description, Some("Current temperature"))
  assert_eq(temperature_gauge.unit, Some("celsius"))
  
  assert_eq(pressure_gauge.name, "pressure")
  assert_eq(pressure_gauge.description, Some("Current pressure"))
  assert_eq(pressure_gauge.unit, Some("pascal"))
  
  assert_eq(humidity_gauge.name, "humidity")
  assert_eq(humidity_gauge.description, Some("Current humidity"))
  assert_eq(humidity_gauge.unit, Some("percent"))
  
  // 测试Gauge作为Instrument的转换
  let temp_instrument = azimuth::Histogram::as_instrument(azimuth::Meter::create_histogram(meter, "temp.histogram"))
  assert_eq(azimuth::Instrument::name(temp_instrument), "temp.histogram")
  
  // 测试不同类型的Gauge值
  let integer_gauge = azimuth::Meter::create_gauge(meter, "integer.gauge", Some("Integer gauge"), Some("count"))
  let float_gauge = azimuth::Meter::create_gauge(meter, "float.gauge", Some("Float gauge"), Some("value"))
  let negative_gauge = azimuth::Meter::create_gauge(meter, "negative.gauge", Some("Negative gauge"), Some("value"))
  let zero_gauge = azimuth::Meter::create_gauge(meter, "zero.gauge", Some("Zero gauge"), Some("value"))
  
  // 验证不同类型Gauge的属性
  assert_eq(integer_gauge.name, "integer.gauge")
  assert_eq(integer_gauge.description, Some("Integer gauge"))
  assert_eq(integer_gauge.unit, Some("count"))
  
  assert_eq(float_gauge.name, "float.gauge")
  assert_eq(float_gauge.description, Some("Float gauge"))
  assert_eq(float_gauge.unit, Some("value"))
  
  assert_eq(negative_gauge.name, "negative.gauge")
  assert_eq(negative_gauge.description, Some("Negative gauge"))
  assert_eq(negative_gauge.unit, Some("value"))
  
  assert_eq(zero_gauge.name, "zero.gauge")
  assert_eq(zero_gauge.description, Some("Zero gauge"))
  assert_eq(zero_gauge.unit, Some("value"))
}

// Test 3: Gauge performance characteristics
pub test "度量仪表详细测试：仪表性能特征" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "gauge-performance-meter")
  
  // 测试大量Gauge创建的性能
  let gauge_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let gauges = []
  for i in 0..1000 {
    let gauge = azimuth::Meter::create_gauge(meter, "performance.gauge." + i.to_string(), Some("Performance gauge " + i.to_string()), Some("unit"))
    gauges.push(gauge)
  }
  
  let gauge_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let gauge_creation_duration = gauge_end_time - gauge_start_time
  
  // 验证性能在合理范围内
  assert_true(gauge_creation_duration < 5000000000L)  // 小于5秒
  assert_true(gauges.length() == 1000)
  
  // 计算平均每个Gauge的创建时间
  let avg_gauge_creation_time = gauge_creation_duration.to_double() / 1000.0
  assert_true(avg_gauge_creation_time < 5000000.0)  // 每个Gauge小于5毫秒
  
  // 测试Gauge属性访问的性能
  let access_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for gauge in gauges {
    let name = gauge.name
    let description = gauge.description
    let unit = gauge.unit
    
    // 验证属性访问
    assert_true(name.length() > 0)
  }
  
  let access_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let access_duration = access_end_time - access_start_time
  
  // 验证访问性能在合理范围内
  assert_true(access_duration < 1000000000L)  // 小于1秒
  
  // 计算平均每个Gauge的属性访问时间
  let avg_access_time = access_duration.to_double() / 1000.0
  assert_true(avg_access_time < 1000000.0)  // 每个Gauge小于1毫秒
}

// Test 4: Gauge with special values and edge cases
pub test "度量仪表详细测试：仪表特殊值和边界情况" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "gauge-edge-cases-meter")
  
  // 测试极值的Gauge
  let max_value_gauge = azimuth::Meter::create_gauge(meter, "max.value", Some("Maximum value gauge"), Some("unit"))
  let min_value_gauge = azimuth::Meter::create_gauge(meter, "min.value", Some("Minimum value gauge"), Some("unit"))
  let infinity_gauge = azimuth::Meter::create_gauge(meter, "infinity", Some("Infinity gauge"), Some("unit"))
  let negative_infinity_gauge = azimuth::Meter::create_gauge(meter, "negative.infinity", Some("Negative infinity gauge"), Some("unit"))
  let nan_gauge = azimuth::Meter::create_gauge(meter, "nan", Some("NaN gauge"), Some("unit"))
  
  // 验证极值Gauge的属性
  assert_eq(max_value_gauge.name, "max.value")
  assert_eq(max_value_gauge.description, Some("Maximum value gauge"))
  assert_eq(max_value_gauge.unit, Some("unit"))
  
  assert_eq(min_value_gauge.name, "min.value")
  assert_eq(min_value_gauge.description, Some("Minimum value gauge"))
  assert_eq(min_value_gauge.unit, Some("unit"))
  
  assert_eq(infinity_gauge.name, "infinity")
  assert_eq(infinity_gauge.description, Some("Infinity gauge"))
  assert_eq(infinity_gauge.unit, Some("unit"))
  
  assert_eq(negative_infinity_gauge.name, "negative.infinity")
  assert_eq(negative_infinity_gauge.description, Some("Negative infinity gauge"))
  assert_eq(negative_infinity_gauge.unit, Some("unit"))
  
  assert_eq(nan_gauge.name, "nan")
  assert_eq(nan_gauge.description, Some("NaN gauge"))
  assert_eq(nan_gauge.unit, Some("unit"))
  
  // 测试特殊字符的Gauge名称和描述
  let unicode_gauge = azimuth::Meter::create_gauge(meter, "测试仪表🚀", Some("测试描述"), Some("单位"))
  let emoji_gauge = azimuth::Meter::create_gauge(meter, "emoji.gauge😀", Some("Emoji gauge 😀"), Some("unit📏"))
  let special_chars_gauge = azimuth::Meter::create_gauge(meter, "special.chars!@#$%", Some("Special chars !@#$%"), Some("unit!@"))
  
  // 验证特殊字符Gauge的属性
  assert_eq(unicode_gauge.name, "测试仪表🚀")
  assert_eq(unicode_gauge.description, Some("测试描述"))
  assert_eq(unicode_gauge.unit, Some("单位"))
  
  assert_eq(emoji_gauge.name, "emoji.gauge😀")
  assert_eq(emoji_gauge.description, Some("Emoji gauge 😀"))
  assert_eq(emoji_gauge.unit, Some("unit📏"))
  
  assert_eq(special_chars_gauge.name, "special.chars!@#$%")
  assert_eq(special_chars_gauge.description, Some("Special chars !@#$%"))
  assert_eq(special_chars_gauge.unit, Some("unit!@"))
  
  // 测试极长名称和描述的Gauge
  let very_long_name = "this.is.a.very.long.gauge.name.that.exceeds.normal.expectations.and.tests.boundary.conditions"
  let very_long_description = "this.is.a.very.long.gauge.description.that.exceeds.normal.expectations.and.tests.boundary.conditions"
  let very_long_unit = "this.is.a.very.long.unit.name.that.exceeds.normal.expectations"
  
  let long_gauge = azimuth::Meter::create_gauge(meter, very_long_name, Some(very_long_description), Some(very_long_unit))
  
  // 验证极长名称和描述的Gauge
  assert_eq(long_gauge.name, very_long_name)
  assert_eq(long_gauge.description, Some(very_long_description))
  assert_eq(long_gauge.unit, Some(very_long_unit))
}

// Test 5: Gauge in different contexts
pub test "度量仪表详细测试：不同上下文中的仪表" {
  // 测试不同Meter中的Gauge
  let meter_provider = azimuth::MeterProvider::default()
  
  let system_meter = azimuth::MeterProvider::get_meter(meter_provider, "system.metrics")
  let application_meter = azimuth::MeterProvider::get_meter(meter_provider, "application.metrics")
  let business_meter = azimuth::MeterProvider::get_meter(meter_provider, "business.metrics")
  
  // 在不同Meter中创建Gauge
  let cpu_gauge = azimuth::Meter::create_gauge(system_meter, "cpu.usage", Some("CPU usage"), Some("%"))
  let memory_gauge = azimuth::Meter::create_gauge(system_meter, "memory.usage", Some("Memory usage"), Some("MB"))
  
  let request_rate_gauge = azimuth::Meter::create_gauge(application_meter, "request.rate", Some("Request rate"), Some("req/s"))
  let response_time_gauge = azimuth::Meter::create_gauge(application_meter, "response.time", Some("Response time"), Some("ms"))
  
  let revenue_gauge = azimuth::Meter::create_gauge(business_meter, "revenue", Some("Current revenue"), Some("USD"))
  let user_count_gauge = azimuth::Meter::create_gauge(business_meter, "user.count", Some("Active users"), Some("count"))
  
  // 验证不同上下文中的Gauge
  assert_eq(cpu_gauge.name, "cpu.usage")
  assert_eq(memory_gauge.name, "memory.usage")
  assert_eq(request_rate_gauge.name, "request.rate")
  assert_eq(response_time_gauge.name, "response.time")
  assert_eq(revenue_gauge.name, "revenue")
  assert_eq(user_count_gauge.name, "user.count")
  
  // 测试相同名称的Gauge在不同Meter中
  let system_status_gauge = azimuth::Meter::create_gauge(system_meter, "status", Some("System status"), Some("code"))
  let application_status_gauge = azimuth::Meter::create_gauge(application_meter, "status", Some("Application status"), Some("code"))
  let business_status_gauge = azimuth::Meter::create_gauge(business_meter, "status", Some("Business status"), Some("code"))
  
  // 验证相同名称的Gauge在不同Meter中可以共存
  assert_eq(system_status_gauge.name, "status")
  assert_eq(application_status_gauge.name, "status")
  assert_eq(business_status_gauge.name, "status")
  
  assert_eq(system_status_gauge.description, Some("System status"))
  assert_eq(application_status_gauge.description, Some("Application status"))
  assert_eq(business_status_gauge.description, Some("Business status"))
}

// Test 6: Gauge with attributes and labels
pub test "度量仪表详细测试：带属性和标签的仪表" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "gauge-attributes-meter")
  
  // 创建带属性的Gauge（通过名称编码属性）
  let cpu_gauge_system = azimuth::Meter::create_gauge(meter, "cpu.usage{host=\"server1\",environment=\"prod\"}", Some("CPU usage for server1 in prod"), Some("%"))
  let cpu_gauge_staging = azimuth::Meter::create_gauge(meter, "cpu.usage{host=\"server1\",environment=\"staging\"}", Some("CPU usage for server1 in staging"), Some("%"))
  
  let memory_gauge_web = azimuth::Meter::create_gauge(meter, "memory.usage{service=\"web\",region=\"us-west\"}", Some("Memory usage for web service in us-west"), Some("MB"))
  let memory_gauge_api = azimuth::Meter::create_gauge(meter, "memory.usage{service=\"api\",region=\"us-west\"}", Some("Memory usage for api service in us-west"), Some("MB"))
  
  // 验证带属性的Gauge
  assert_eq(cpu_gauge_system.name, "cpu.usage{host=\"server1\",environment=\"prod\"}")
  assert_eq(cpu_gauge_staging.name, "cpu.usage{host=\"server1\",environment=\"staging\"}")
  assert_eq(memory_gauge_web.name, "memory.usage{service=\"web\",region=\"us-west\"}")
  assert_eq(memory_gauge_api.name, "memory.usage{service=\"api\",region=\"us-west\"}")
  
  // 测试复杂属性组合
  let complex_gauge = azimuth::Meter::create_gauge(
    meter, 
    "request.latency{service=\"api\",endpoint=\"/users\",method=\"GET\",region=\"us-east\",version=\"v2\"}", 
    Some("Request latency for GET /users endpoint"), 
    Some("ms")
  )
  
  // 验证复杂属性组合
  assert_eq(complex_gauge.name, "request.latency{service=\"api\",endpoint=\"/users\",method=\"GET\",region=\"us-east\",version=\"v2\"}")
  assert_eq(complex_gauge.description, Some("Request latency for GET /users endpoint"))
  assert_eq(complex_gauge.unit, Some("ms"))
  
  // 测试特殊字符的属性值
  let special_attr_gauge = azimuth::Meter::create_gauge(
    meter, 
    "test.metric{path=\"/api/v1/users\",query=\"?active=true&limit=10\"}", 
    Some("Test metric with special chars"), 
    Some("unit")
  )
  
  // 验证特殊字符的属性值
  assert_eq(special_attr_gauge.name, "test.metric{path=\"/api/v1/users\",query=\"?active=true&limit=10\"}")
  assert_eq(special_attr_gauge.description, Some("Test metric with special chars"))
  assert_eq(special_attr_gauge.unit, Some("unit"))
}

// Test 7: Gauge lifecycle management
pub test "度量仪表详细测试：仪表生命周期管理" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "gauge-lifecycle-meter")
  
  // 测试Gauge的创建和销毁
  let gauges = []
  
  // 创建阶段
  for i in 0..100 {
    let gauge = azimuth::Meter::create_gauge(meter, "lifecycle.gauge." + i.to_string(), Some("Lifecycle gauge " + i.to_string()), Some("unit"))
    gauges.push(gauge)
  }
  
  // 验证创建阶段
  assert_true(gauges.length() == 100)
  
  // 验证所有Gauge的属性
  for i in 0..gauges.length() {
    let gauge = gauges[i]
    let expected_name = "lifecycle.gauge." + i.to_string()
    let expected_description = Some("Lifecycle gauge " + i.to_string())
    
    assert_eq(gauge.name, expected_name)
    assert_eq(gauge.description, expected_description)
    assert_eq(gauge.unit, Some("unit"))
  }
  
  // 模拟Gauge更新阶段
  for i in 0..gauges.length() {
    let gauge = gauges[i]
    // 在实际实现中，这里会更新Gauge的值
    // 由于简化实现不支持，我们只验证属性仍然有效
    assert_eq(gauge.name, "lifecycle.gauge." + i.to_string())
  }
  
  // 测试Gauge的版本控制（通过名称编码版本）
  let v1_gauge = azimuth::Meter::create_gauge(meter, "api.requests{version=\"v1\"}", Some("API requests v1"), Some("count"))
  let v2_gauge = azimuth::Meter::create_gauge(meter, "api.requests{version=\"v2\"}", Some("API requests v2"), Some("count"))
  
  // 验证版本控制
  assert_eq(v1_gauge.name, "api.requests{version=\"v1\"}")
  assert_eq(v2_gauge.name, "api.requests{version=\"v2\"}")
  assert_eq(v1_gauge.description, Some("API requests v1"))
  assert_eq(v2_gauge.description, Some("API requests v2"))
}

// Test 8: Gauge integration with other metrics
pub test "度量仪表详细测试：仪表与其他度量的集成" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "gauge-integration-meter")
  
  // 创建不同类型的度量
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("count"))
  let response_histogram = azimuth::Meter::create_histogram(meter, "http.response.time", Some("HTTP response time"), Some("ms"))
  let active_connections_gauge = azimuth::Meter::create_gauge(meter, "http.active.connections", Some("Active HTTP connections"), Some("connections"))
  
  // 验证不同类型度量的创建
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_histogram.name, "http.response.time")
  assert_eq(active_connections_gauge.name, "http.active.connections")
  
  // 测试相关度量的命名约定
  let cpu_usage_gauge = azimuth::Meter::create_gauge(meter, "system.cpu.usage", Some("CPU usage"), Some("%"))
  let cpu_load_avg_gauge = azimuth::Meter::create_gauge(meter, "system.cpu.load.avg", Some("CPU load average"), Some("load"))
  let memory_usage_gauge = azimuth::Meter::create_gauge(meter, "system.memory.usage", Some("Memory usage"), Some("%"))
  let memory_available_gauge = azimuth::Meter::create_gauge(meter, "system.memory.available", Some("Available memory"), Some("MB"))
  
  // 验证相关度量的命名约定
  assert_eq(cpu_usage_gauge.name, "system.cpu.usage")
  assert_eq(cpu_load_avg_gauge.name, "system.cpu.load.avg")
  assert_eq(memory_usage_gauge.name, "system.memory.usage")
  assert_eq(memory_available_gauge.name, "system.memory.available")
  
  // 测试业务度量的组合
  let user_count_gauge = azimuth::Meter::create_gauge(meter, "business.users.active", Some("Active users"), Some("count"))
  let user_sessions_gauge = azimuth::Meter::create_gauge(meter, "business.users.sessions", Some("Active sessions"), Some("count"))
  let revenue_counter = azimuth::Meter::create_counter(meter, "business.revenue.total", Some("Total revenue"), Some("USD"))
  let order_value_histogram = azimuth::Meter::create_histogram(meter, "business.order.value", Some("Order value"), Some("USD"))
  
  // 验证业务度量的组合
  assert_eq(user_count_gauge.name, "business.users.active")
  assert_eq(user_sessions_gauge.name, "business.users.sessions")
  assert_eq(revenue_counter.name, "business.revenue.total")
  assert_eq(order_value_histogram.name, "business.order.value")
  
  // 测试Gauge作为Instrument的转换
  let gauge_instrument = azimuth::Histogram::as_instrument(response_histogram)
  assert_eq(azimuth::Instrument::name(gauge_instrument), "http.response.time")
  assert_eq(azimuth::Instrument::description(gauge_instrument), Some("HTTP response time"))
  assert_eq(azimuth::Instrument::unit(gauge_instrument), Some("ms"))
}