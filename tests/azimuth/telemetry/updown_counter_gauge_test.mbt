// Azimuth Telemetry System - UpDownCounter and Gauge Instruments Test
// 测试Azimuth遥测系统的UpDownCounter和Gauge度量仪器功能

test "UpDownCounter基础创建和操作测试" {
  // 测试UpDownCounter的基础创建和操作
  
  // 创建MeterProvider和Meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "updown.test.meter", Some("1.0.0"))
  
  // 创建UpDownCounter
  let updown_counter = Meter::create_updown_counter(meter, "active_connections", Some("Active network connections"), Some("connections"))
  
  // 验证UpDownCounter属性
  assert_eq(updown_counter.name, "active_connections")
  assert_eq(updown_counter.description, Some("Active network connections"))
  assert_eq(updown_counter.unit, Some("connections"))
  
  // 测试增加操作
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, 5.0, Some(Attributes::new()))
  
  // 测试减少操作
  UpDownCounter::add(updown_counter, -3.0)
  UpDownCounter::add(updown_counter, -2.0, Some(Attributes::new()))
  
  // 测试零值操作
  UpDownCounter::add(updown_counter, 0.0)
  
  // 测试大值操作
  UpDownCounter::add(updown_counter, 1000000.0)
  UpDownCounter::add(updown_counter, -1000000.0)
  
  // 测试小数值操作
  UpDownCounter::add(updown_counter, 0.1)
  UpDownCounter::add(updown_counter, -0.05)
}

test "Gauge基础创建和操作测试" {
  // 测试Gauge的基础创建和操作
  
  // 创建MeterProvider和Meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "gauge.test.meter", Some("1.0.0"))
  
  // 创建Gauge
  let gauge = Meter::create_gauge(meter, "cpu_usage", Some("CPU usage percentage"), Some("percent"))
  
  // 验证Gauge属性
  assert_eq(gauge.name, "cpu_usage")
  assert_eq(gauge.description, Some("CPU usage percentage"))
  assert_eq(gauge.unit, Some("percent"))
  
  // 测试设置不同值
  UpDownCounter::add(gauge, 25.5) // Gauge使用相同的add方法
  UpDownCounter::add(gauge, 75.0, Some(Attributes::new()))
  UpDownCounter::add(gauge, 100.0)
  UpDownCounter::add(gauge, 0.0)
  UpDownCounter::add(gauge, 50.25)
  
  // 测试边界值
  UpDownCounter::add(gauge, -10.0) // 负值可能表示无效数据
  UpDownCounter::add(gauge, 150.0) // 超过100%的值
  
  // 测试精度值
  UpDownCounter::add(gauge, 33.333333)
  UpDownCounter::add(gauge, 66.666666)
}

test "UpDownCounter场景化测试" {
  // 测试UpDownCounter在实际场景中的应用
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "connection.manager")
  
  // 创建连接计数器
  let active_connections = Meter::create_updown_counter(meter, "active_connections", Some("Currently active connections"), Some("connections"))
  
  // 模拟连接建立和断开
  UpDownCounter::add(active_connections, 1.0) // 新连接
  UpDownCounter::add(active_connections, 1.0) // 又一个新连接
  UpDownCounter::add(active_connections, 1.0) // 第三个连接
  
  UpDownCounter::add(active_connections, -1.0) // 连接断开
  UpDownCounter::add(active_connections, 1.0) // 新连接
  
  UpDownCounter::add(active_connections, -2.0) // 两个连接断开
  
  // 创建队列长度计数器
  let queue_length = Meter::create_updown_counter(meter, "queue_length", Some("Current queue length"), Some("items"))
  
  // 模拟队列操作
  UpDownCounter::add(queue_length, 5.0) // 添加5个项目
  UpDownCounter::add(queue_length, 3.0) // 添加3个项目
  UpDownCounter::add(queue_length, -2.0) // 处理2个项目
  UpDownCounter::add(queue_length, -1.0) // 处理1个项目
  UpDownCounter::add(queue_length, 10.0) // 添加10个项目
  UpDownCounter::add(queue_length, -15.0) // 处理所有项目
  
  // 创建内存使用计数器
  let memory_usage = Meter::create_updown_counter(meter, "memory_usage", Some("Memory usage in bytes"), Some("bytes"))
  
  // 模拟内存分配和释放
  UpDownCounter::add(memory_usage, 1024.0) // 分配1KB
  UpDownCounter::add(memory_usage, 2048.0) // 分配2KB
  UpDownCounter::add(memory_usage, -512.0) // 释放512B
  UpDownCounter::add(memory_usage, 4096.0) // 分配4KB
  UpDownCounter::add(memory_usage, -6656.0) // 释放所有内存
}

test "Gauge场景化测试" {
  // 测试Gauge在实际场景中的应用
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "system.monitor")
  
  // 创建CPU使用率Gauge
  let cpu_usage = Meter::create_gauge(meter, "cpu_usage_percent", Some("CPU usage percentage"), Some("percent"))
  
  // 模拟CPU使用率变化
  UpDownCounter::add(cpu_usage, 15.5) // 低使用率
  UpDownCounter::add(cpu_usage, 45.2) // 中等使用率
  UpDownCounter::add(cpu_usage, 78.9) // 高使用率
  UpDownCounter::add(cpu_usage, 95.0) // 很高使用率
  UpDownCounter::add(cpu_usage, 32.1) // 回到中等使用率
  UpDownCounter::add(cpu_usage, 5.8)  // 低使用率
  
  // 创建内存使用率Gauge
  let memory_usage_percent = Meter::create_gauge(meter, "memory_usage_percent", Some("Memory usage percentage"), Some("percent"))
  
  // 模拟内存使用率变化
  UpDownCounter::add(memory_usage_percent, 60.0)
  UpDownCounter::add(memory_usage_percent, 65.5)
  UpDownCounter::add(memory_usage_percent, 70.2)
  UpDownCounter::add(memory_usage_percent, 68.8)
  UpDownCounter::add(memory_usage_percent, 72.1)
  
  // 创建磁盘使用率Gauge
  let disk_usage_percent = Meter::create_gauge(meter, "disk_usage_percent", Some("Disk usage percentage"), Some("percent"))
  
  // 模拟磁盘使用率变化
  UpDownCounter::add(disk_usage_percent, 45.0)
  UpDownCounter::add(disk_usage_percent, 46.5)
  UpDownCounter::add(disk_usage_percent, 47.2)
  UpDownCounter::add(disk_usage_percent, 47.8)
  
  // 创建温度Gauge
  let temperature_celsius = Meter::create_gauge(meter, "temperature_celsius", Some("Temperature in Celsius"), Some("celsius"))
  
  // 模拟温度变化
  UpDownCounter::add(temperature_celsius, 35.5)
  UpDownCounter::add(temperature_celsius, 42.1)
  UpDownCounter::add(temperature_celsius, 38.7)
  UpDownCounter::add(temperature_celsius, 40.2)
  UpDownCounter::add(temperature_celsius, 36.9)
}

test "UpDownCounter和Gauge属性测试" {
  // 测试UpDownCounter和Gauge的属性功能
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "attributes.test")
  
  // 创建带属性的UpDownCounter
  let updown_with_attrs = Meter::create_updown_counter(
    meter, 
    "queue_size", 
    Some("Current queue size"), 
    Some("items")
  )
  
  // 创建测试属性
  let attrs1 = Attributes::new()
  Attributes::set(attrs1, "queue.name", StringValue("user_requests"))
  Attributes::set(attrs1, "priority", StringValue("high"))
  
  let attrs2 = Attributes::new()
  Attributes::set(attrs2, "queue.name", StringValue("background_tasks"))
  Attributes::set(attrs2, "priority", StringValue("low"))
  
  // 使用不同属性记录值
  UpDownCounter::add(updown_with_attrs, 10.0, Some(attrs1))
  UpDownCounter::add(updown_with_attrs, 25.0, Some(attrs2))
  UpDownCounter::add(updown_with_attrs, -5.0, Some(attrs1))
  UpDownCounter::add(updown_with_attrs, -10.0, Some(attrs2))
  
  // 创建带属性的Gauge
  let gauge_with_attrs = Meter::create_gauge(
    meter, 
    "response_time", 
    Some("Response time in milliseconds"), 
    Some("ms")
  )
  
  // 创建响应时间属性
  let response_attrs1 = Attributes::new()
  Attributes::set(response_attrs1, "endpoint", StringValue("/api/users"))
  Attributes::set(response_attrs1, "method", StringValue("GET"))
  
  let response_attrs2 = Attributes::new()
  Attributes::set(response_attrs2, "endpoint", StringValue("/api/orders"))
  Attributes::set(response_attrs2, "method", StringValue("POST"))
  
  // 使用不同属性记录响应时间
  UpDownCounter::add(gauge_with_attrs, 120.5, Some(response_attrs1))
  UpDownCounter::add(gauge_with_attrs, 250.8, Some(response_attrs2))
  UpDownCounter::add(gauge_with_attrs, 98.2, Some(response_attrs1))
  UpDownCounter::add(gauge_with_attrs, 310.1, Some(response_attrs2))
}

test "UpDownCounter和Gauge边界值测试" {
  // 测试UpDownCounter和Gauge的边界值处理
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "boundary.test")
  
  // 创建测试仪器
  let boundary_updown = Meter::create_updown_counter(meter, "boundary_updown", Some("Boundary test updown"), Some("unit"))
  let boundary_gauge = Meter::create_gauge(meter, "boundary_gauge", Some("Boundary test gauge"), Some("unit"))
  
  // 测试极大正值
  let max_positive : Double = 1.7976931348623157e+308 // Double最大值
  UpDownCounter::add(boundary_updown, max_positive)
  UpDownCounter::add(boundary_gauge, max_positive)
  
  // 测试极大负值
  let max_negative : Double = -1.7976931348623157e+308 // Double最小值
  UpDownCounter::add(boundary_updown, max_negative)
  UpDownCounter::add(boundary_gauge, max_negative)
  
  // 测试零值
  UpDownCounter::add(boundary_updown, 0.0)
  UpDownCounter::add(boundary_gauge, 0.0)
  
  // 测试很小的正值
  let tiny_positive : Double = 1.0e-308
  UpDownCounter::add(boundary_updown, tiny_positive)
  UpDownCounter::add(boundary_gauge, tiny_positive)
  
  // 测试很小的负值
  let tiny_negative : Double = -1.0e-308
  UpDownCounter::add(boundary_updown, tiny_negative)
  UpDownCounter::add(boundary_gauge, tiny_negative)
  
  // 测试特殊浮点值
  UpDownCounter::add(boundary_updown, 1.0e+10) // 大数值
  UpDownCounter::add(boundary_updown, 1.0e-10) // 小数值
  UpDownCounter::add(boundary_gauge, 1.0e+15) // 很大数值
  UpDownCounter::add(boundary_gauge, 1.0e-15) // 很小数值
  
  // 测试精度边界
  UpDownCounter::add(boundary_updown, 0.1 + 0.2) // 浮点精度问题
  UpDownCounter::add(boundary_gauge, 0.3 - 0.1) // 浮点精度问题
}

test "UpDownCounter和Gauge作为Instrument测试" {
  // 测试UpDownCounter和Gauge作为Instrument类型的属性
  
  // 创建Instrument实例
  let updown_instrument = UpDownCounter("test_updown", Some("Test UpDownCounter"), Some("count"))
  let gauge_instrument = Gauge("test_gauge", Some("Test Gauge"), Some("percent"))
  
  // 测试Instrument属性获取
  assert_eq(Instrument::name(updown_instrument), "test_updown")
  assert_eq(Instrument::description(updown_instrument), Some("Test UpDownCounter"))
  assert_eq(Instrument::unit(updown_instrument), Some("count"))
  
  assert_eq(Instrument::name(gauge_instrument), "test_gauge")
  assert_eq(Instrument::description(gauge_instrument), Some("Test Gauge"))
  assert_eq(Instrument::unit(gauge_instrument), Some("percent"))
  
  // 测试Instrument模式匹配
  let instrument_type = function(instrument : Instrument) -> String {
    match instrument {
      UpDownCounter(_, _, _) => "UpDownCounter"
      Gauge(_, _, _) => "Gauge"
      Counter(_, _, _) => "Counter"
      Histogram(_, _, _) => "Histogram"
    }
  }
  
  assert_eq(instrument_type(updown_instrument), "UpDownCounter")
  assert_eq(instrument_type(gauge_instrument), "Gauge")
  
  // 测试所有Instrument类型的创建
  let all_instruments = [
    Counter("counter_name", Some("Counter desc"), Some("unit1")),
    Histogram("histogram_name", Some("Histogram desc"), Some("unit2")),
    UpDownCounter("updown_name", Some("UpDownCounter desc"), Some("unit3")),
    Gauge("gauge_name", Some("Gauge desc"), Some("unit4"))
  ]
  
  // 验证所有Instrument的名称
  let expected_names = ["counter_name", "histogram_name", "updown_name", "gauge_name"]
  for i = 0; i < all_instruments.length(); i = i + 1 {
    assert_eq(Instrument::name(all_instruments[i]), expected_names[i])
  }
}

test "UpDownCounter和Gauge性能测试" {
  // 测试UpDownCounter和Gauge的性能
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance.test")
  
  // 创建性能测试仪器
  let perf_updown = Meter::create_updown_counter(meter, "perf_updown", Some("Performance test updown"), Some("ops"))
  let perf_gauge = Meter::create_gauge(meter, "perf_gauge", Some("Performance test gauge"), Some("value"))
  
  // 测试大量操作
  for i = 0; i < 1000; i = i + 1 {
    UpDownCounter::add(perf_updown, 1.0)
    UpDownCounter::add(perf_gauge, i.to_double())
  }
  
  // 测试批量减少操作
  for i = 0; i < 500; i = i + 1 {
    UpDownCounter::add(perf_updown, -1.0)
  }
  
  // 测试不同数值范围
  let small_values = [0.001, 0.01, 0.1, 1.0, 10.0]
  let large_values = [100.0, 1000.0, 10000.0, 100000.0]
  
  for value in small_values {
    UpDownCounter::add(perf_updown, value)
    UpDownCounter::add(perf_gauge, value)
  }
  
  for value in large_values {
    UpDownCounter::add(perf_updown, value)
    UpDownCounter::add(perf_gauge, value)
  }
  
  // 测试交替操作
  for i = 0; i < 100; i = i + 1 {
    if i % 2 == 0 {
      UpDownCounter::add(perf_updown, 100.0)
      UpDownCounter::add(perf_gauge, 75.0)
    } else {
      UpDownCounter::add(perf_updown, -50.0)
      UpDownCounter::add(perf_gauge, 25.0)
    }
  }
}

test "UpDownCounter和Gauge实际监控场景测试" {
  // 测试UpDownCounter和Gauge在实际监控场景中的应用
  
  let meter_provider = MeterProvider::default()
  let app_meter = MeterProvider::get_meter(meter_provider, "application.monitor", Some("1.0.0"))
  
  // Web应用监控指标
  let active_users = Meter::create_updown_counter(app_meter, "active_users", Some("Currently active users"), Some("users"))
  let pending_requests = Meter::create_updown_counter(app_meter, "pending_requests", Some("Pending HTTP requests"), Some("requests"))
  let cpu_usage = Meter::create_gauge(app_meter, "cpu_usage_percent", Some("CPU usage percentage"), Some("percent"))
  let memory_usage = Meter::create_gauge(app_meter, "memory_usage_percent", Some("Memory usage percentage"), Some("percent"))
  
  // 模拟用户登录和登出
  UpDownCounter::add(active_users, 1.0) // 用户登录
  UpDownCounter::add(active_users, 1.0) // 又一个用户登录
  UpDownCounter::add(active_users, 1.0) // 第三个用户登录
  UpDownCounter::add(active_users, -1.0) // 用户登出
  UpDownCounter::add(active_users, 1.0) // 新用户登录
  
  // 模拟请求处理
  UpDownCounter::add(pending_requests, 5.0) // 5个新请求
  UpDownCounter::add(pending_requests, 3.0) // 3个新请求
  UpDownCounter::add(pending_requests, -4.0) // 处理了4个请求
  UpDownCounter::add(pending_requests, 2.0) // 2个新请求
  UpDownCounter::add(pending_requests, -6.0) // 处理了所有请求
  
  // 模拟系统资源使用率变化
  UpDownCounter::add(cpu_usage, 25.5) // 低CPU使用率
  UpDownCounter::add(cpu_usage, 45.2) // 中等CPU使用率
  UpDownCounter::add(cpu_usage, 78.9) // 高CPU使用率
  UpDownCounter::add(cpu_usage, 32.1) // 回到中等使用率
  
  UpDownCounter::add(memory_usage, 60.0) // 中等内存使用率
  UpDownCounter::add(memory_usage, 75.5) // 较高内存使用率
  UpDownCounter::add(memory_usage, 68.8) // 稍有下降
  UpDownCounter::add(memory_usage, 82.1) // 再次上升
  
  // 数据库连接池监控
  let db_meter = MeterProvider::get_meter(meter_provider, "database.monitor")
  let active_connections = Meter::create_updown_counter(db_meter, "active_connections", Some("Active database connections"), Some("connections"))
  let connection_pool_usage = Meter::create_gauge(db_meter, "connection_pool_usage_percent", Some("Connection pool usage percentage"), Some("percent"))
  
  // 模拟数据库连接使用
  UpDownCounter::add(active_connections, 3.0) // 建立3个连接
  UpDownCounter::add(active_connections, 2.0) // 建立2个连接
  UpDownCounter::add(active_connections, -1.0) // 释放1个连接
  UpDownCounter::add(active_connections, 4.0) // 建立4个连接
  UpDownCounter::add(active_connections, -8.0) // 释放所有连接
  
  // 模拟连接池使用率
  UpDownCounter::add(connection_pool_usage, 30.0) // 30%使用率
  UpDownCounter::add(connection_pool_usage, 50.0) // 50%使用率
  UpDownCounter::add(connection_pool_usage, 80.0) // 80%使用率
  UpDownCounter::add(connection_pool_usage, 45.0) // 45%使用率
  
  // 验证所有仪器的基本属性
  assert_eq(active_users.name, "active_users")
  assert_eq(pending_requests.name, "pending_requests")
  assert_eq(cpu_usage.name, "cpu_usage_percent")
  assert_eq(memory_usage.name, "memory_usage_percent")
  assert_eq(active_connections.name, "active_connections")
  assert_eq(connection_pool_usage.name, "connection_pool_usage_percent")
}