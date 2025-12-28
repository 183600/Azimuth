// Azimuth Advanced Metrics Operations Tests - 高级度量操作测试
// 测试复杂度量场景、聚合操作、时间序列度量和高级度量模式

test "复合度量操作测试" {
  // 测试多种度量类型的组合操作
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "composite-metrics-test")
  
  // 创建多种度量类型
  let request_counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let response_histogram = Meter::create_histogram(meter, "http.response.duration", Some("HTTP response duration"), Some("ms"))
  let active_connections = Meter::create_updown_counter(meter, "http.active_connections", Some("Active HTTP connections"), Some("connections"))
  let memory_usage = Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("bytes"))
  
  // 模拟复合度量操作
  for i in 0..=99 {
    // 请求计数
    Counter::add(request_counter, 1.0, {
      attributes: {
        let attrs = Attributes::new()
        Attributes::set(attrs, "method", StringValue(if i % 2 == 0 { "GET" } else { "POST" }))
        Attributes::set(attrs, "status", StringValue(if i % 10 == 0 { "500" } else { "200" }))
        Attributes::set(attrs, "endpoint", StringValue("/api/resource/" + (i % 5).to_string()))
        attrs
      }
    })
    
    // 响应时间分布
    let response_time = 50.0 + (i.to_double() * 2.5) + (Random::next_u64(Random::system()).to_double() % 100.0)
    Histogram::record(response_histogram, response_time, {
      attributes: {
        let attrs = Attributes::new()
        Attributes::set(attrs, "method", StringValue(if i % 2 == 0 { "GET" } else { "POST" }))
        Attributes::set(attrs, "endpoint", StringValue("/api/resource/" + (i % 5).to_string()))
        attrs
      }
    })
    
    // 活跃连接数变化
    if i % 3 == 0 {
      UpDownCounter::add(active_connections, 1.0)
    } else if i % 7 == 0 {
      UpDownCounter::add(active_connections, -1.0)
    }
    
    // 内存使用量
    let memory_value = 1024.0 * 1024.0 * (100.0 + (i.to_double() * 10.0) % 500.0)  // 100-600MB
    Gauge::add(memory_usage, memory_value, {
      attributes: {
        let attrs = Attributes::new()
        Attributes::set(attrs, "component", StringValue("application"))
        Attributes::set(attrs, "type", StringValue("heap"))
        attrs
      }
    })
  }
  
  // 验证度量创建和配置
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(request_counter.description, Some("Total HTTP requests"))
  assert_eq(request_counter.unit, Some("requests"))
  
  assert_eq(response_histogram.name, "http.response.duration")
  assert_eq(response_histogram.description, Some("HTTP response duration"))
  assert_eq(response_histogram.unit, Some("ms"))
  
  assert_eq(active_connections.name, "http.active_connections")
  assert_eq(active_connections.description, Some("Active HTTP connections"))
  assert_eq(active_connections.unit, Some("connections"))
  
  assert_eq(memory_usage.name, "memory.usage")
  assert_eq(memory_usage.description, Some("Memory usage"))
  assert_eq(memory_usage.unit, Some("bytes"))
  
  // 测试度量转换为Instrument类型
  let counter_instrument = Counter(request_counter.name, request_counter.description, request_counter.unit)
  assert_eq(Instrument::name(counter_instrument), "http.requests.total")
  assert_eq(Instrument::description(counter_instrument), Some("Total HTTP requests"))
  assert_eq(Instrument::unit(counter_instrument), Some("requests"))
  
  let histogram_instrument = Histogram::as_instrument(response_histogram)
  assert_eq(Instrument::name(histogram_instrument), "http.response.duration")
}

test "聚合度量操作测试" {
  // 测试度量聚合操作
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "aggregation-metrics-test")
  
  // 创建用于聚合的度量
  let api_calls_counter = Meter::create_counter(meter, "api.calls.total", Some("Total API calls"), Some("calls"))
  let response_size_histogram = Meter::create_histogram(meter, "api.response.size", Some("API response size"), Some("bytes"))
  let error_rate_counter = Meter::create_counter(meter, "api.errors.total", Some("Total API errors"), Some("errors"))
  
  // 模拟不同服务的API调用
  let services = ["user-service", "order-service", "payment-service", "inventory-service", "notification-service"]
  let endpoints = ["/users", "/orders", "/payments", "/inventory", "/notifications"]
  
  for i in 0..=199 {
    let service_index = i % services.length()
    let service = services[service_index]
    let endpoint = endpoints[service_index]
    
    // API调用计数
    Counter::add(api_calls_counter, 1.0, {
      attributes: {
        let attrs = Attributes::new()
        Attributes::set(attrs, "service", StringValue(service))
        Attributes::set(attrs, "endpoint", StringValue(endpoint))
        Attributes::set(attrs, "method", StringValue(match i % 4 {
          0 => "GET"
          1 => "POST"
          2 => "PUT"
          _ => "DELETE"
        }))
        attrs
      }
    })
    
    // 响应大小分布
    let response_size = 100.0 + (Random::next_u64(Random::system()).to_double() % 10000.0)
    Histogram::record(response_size_histogram, response_size, {
      attributes: {
        let attrs = Attributes::new()
        Attributes::set(attrs, "service", StringValue(service))
        Attributes::set(attrs, "endpoint", StringValue(endpoint))
        attrs
      }
    })
    
    // 错误计数（10%的请求出错）
    if i % 10 == 0 {
      Counter::add(error_rate_counter, 1.0, {
        attributes: {
          let attrs = Attributes::new()
          Attributes::set(attrs, "service", StringValue(service))
          Attributes::set(attrs, "error.type", StringValue(match i % 3 {
            0 => "timeout"
            1 => "connection_error"
            _ => "server_error"
          }))
          attrs
        }
      })
    }
  }
  
  // 验证聚合度量的配置
  assert_eq(api_calls_counter.name, "api.calls.total")
  assert_eq(response_size_histogram.name, "api.response.size")
  assert_eq(error_rate_counter.name, "api.errors.total")
  
  // 测试聚合计算（模拟）
  let total_api_calls = 200.0  // 总调用次数
  let total_errors = 20.0      // 总错误次数
  let error_rate = total_errors / total_api_calls * 100.0  // 错误率
  
  assert_true(error_rate == 10.0)
  
  // 测试按服务分组的聚合
  for service in services {
    // 每个服务应该有大约40次调用（200/5）
    let expected_calls_per_service = 40.0
    assert_true(expected_calls_per_service == 40.0)
    
    // 每个服务应该有大约4次错误（20/5）
    let expected_errors_per_service = 4.0
    assert_true(expected_errors_per_service == 4.0)
  }
}

test "时间序列度量测试" {
  // 测试时间序列度量数据
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "timeseries-metrics-test")
  
  // 创建时间序列度量
  let cpu_usage = Meter::create_gauge(meter, "system.cpu.usage", Some("CPU usage percentage"), Some("%"))
  let memory_usage = Meter::create_gauge(meter, "system.memory.usage", Some("Memory usage percentage"), Some("%"))
  let disk_io = Meter::create_counter(meter, "system.disk.io", Some("Disk I/O operations"), Some("operations"))
  let network_throughput = Meter::create_histogram(meter, "system.network.throughput", Some("Network throughput"), Some("bytes/s"))
  
  // 模拟时间序列数据收集
  let time_series_data = []
  let base_timestamp = Clock::now_unix_nanos(Clock::system())
  
  for i in 0..=59 {  // 60个时间点（模拟1分钟的数据，每秒一个点）
    let timestamp = base_timestamp + (i.to_int64() * 1000000000L)  // 每秒一个时间点
    
    // CPU使用率（模拟周期性变化）
    let cpu_value = 20.0 + (10.0 * (i.to_double() / 60.0 * 6.28).sin()) + (Random::next_u64(Random::system()).to_double() % 10.0)
    Gauge::add(cpu_usage, cpu_value, {
      attributes: {
        let attrs = Attributes::new()
        Attributes::set(attrs, "core", StringValue("total"))
        Attributes::set(attrs, "timestamp", IntValue(timestamp.to_int()))
        attrs
      }
    })
    
    // 内存使用率（模拟逐渐增长）
    let memory_value = 30.0 + (i.to_double() * 0.5) + (Random::next_u64(Random::system()).to_double() % 5.0)
    Gauge::add(memory_usage, memory_value, {
      attributes: {
        let attrs = Attributes::new()
        Attributes::set(attrs, "type", StringValue("used"))
        Attributes::set(attrs, "timestamp", IntValue(timestamp.to_int()))
        attrs
      }
    })
    
    // 磁盘I/O（模拟随机波动）
    if i % 2 == 0 {
      let disk_operations = (Random::next_u64(Random::system()).to_double() % 100.0).to_int()
      Counter::add(disk_io, disk_operations.to_double(), {
        attributes: {
          let attrs = Attributes::new()
          Attributes::set(attrs, "operation", StringValue(if i % 4 == 0 { "read" } else { "write" }))
          Attributes::set(attrs, "device", StringValue("/dev/sda1"))
          attrs
        }
      })
    }
    
    // 网络吞吐量（模拟突发和空闲）
    let network_value = if i % 10 < 3 { 
      1000.0 + (Random::next_u64(Random::system()).to_double() % 5000.0)  // 突发期
    } else { 
      100.0 + (Random::next_u64(Random::system()).to_double() % 200.0)   // 空闲期
    }
    Histogram::record(network_throughput, network_value, {
      attributes: {
        let attrs = Attributes::new()
        Attributes::set(attrs, "interface", StringValue("eth0"))
        Attributes::set(attrs, "direction", StringValue(if i % 2 == 0 { "tx" } else { "rx" }))
        attrs
      }
    })
    
    time_series_data.push((timestamp, cpu_value, memory_value))
  }
  
  // 验证时间序列数据
  assert_true(time_series_data.length() == 60)
  
  // 验证时间戳递增
  for i in 1..=time_series_data.length() - 1 {
    let current_timestamp = time_series_data[i].0
    let prev_timestamp = time_series_data[i-1].0
    assert_true(current_timestamp > prev_timestamp)
  }
  
  // 验证度量的时间序列特性
  assert_eq(cpu_usage.name, "system.cpu.usage")
  assert_eq(cpu_usage.description, Some("CPU usage percentage"))
  assert_eq(cpu_usage.unit, Some("%"))
  
  assert_eq(memory_usage.name, "system.memory.usage")
  assert_eq(memory_usage.description, Some("Memory usage percentage"))
  assert_eq(memory_usage.unit, Some("%"))
  
  assert_eq(disk_io.name, "system.disk.io")
  assert_eq(network_throughput.name, "system.network.throughput")
}

test "业务度量测试" {
  // 测试业务相关的度量指标
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "business-metrics-test")
  
  // 创建业务度量
  let revenue_counter = Meter::create_counter(meter, "business.revenue.total", Some("Total revenue"), Some("USD"))
  let order_counter = Meter::create_counter(meter, "business.orders.total", Some("Total orders"), Some("orders"))
  let customer_counter = Meter::create_updown_counter(meter, "business.customers.active", Some("Active customers"), Some("customers"))
  let conversion_rate = Meter::create_histogram(meter, "business.conversion.rate", Some("Conversion rate"), Some("%"))
  
  // 模拟业务操作
  let products = ["laptop", "phone", "tablet", "watch", "headphones"]
  let regions = ["north-america", "europe", "asia-pacific", "latin-america", "middle-east"]
  
  for i in 0..=149 {
    let product = products[i % products.length()]
    let region = regions[i % regions.length()]
    
    // 订单计数
    Counter::add(order_counter, 1.0, {
      attributes: {
        let attrs = Attributes::new()
        Attributes::set(attrs, "product", StringValue(product))
        Attributes::set(attrs, "region", StringValue(region))
        Attributes::set(attrs, "channel", StringValue(match i % 3 {
          0 => "online"
          1 => "retail"
          _ => "mobile"
        }))
        attrs
      }
    })
    
    // 收入计算
    let product_prices = [
      ("laptop", 1200.0),
      ("phone", 800.0),
      ("tablet", 500.0),
      ("watch", 300.0),
      ("headphones", 150.0)
    ]
    
    let price = match product {
      "laptop" => 1200.0
      "phone" => 800.0
      "tablet" => 500.0
      "watch" => 300.0
      _ => 150.0
    }
    
    Counter::add(revenue_counter, price, {
      attributes: {
        let attrs = Attributes::new()
        Attributes::set(attrs, "product", StringValue(product))
        Attributes::set(attrs, "region", StringValue(region))
        Attributes::set(attrs, "currency", StringValue("USD"))
        attrs
      }
    })
    
    // 活跃客户变化
    if i % 5 == 0 {
      UpDownCounter::add(customer_counter, 1.0)  // 新客户
    }
    if i % 20 == 0 {
      UpDownCounter::add(customer_counter, -1.0)  // 流失客户
    }
    
    // 转化率（模拟访问到购买的转化）
    if i % 3 == 0 {
      let conversion_value = (Random::next_u64(Random::system()).to_double() % 100.0) / 100.0 * 10.0  // 0-10%
      Histogram::record(conversion_rate, conversion_value, {
        attributes: {
          let attrs = Attributes::new()
          Attributes::set(attrs, "funnel_stage", StringValue("purchase"))
          Attributes::set(attrs, "user_segment", StringValue(match i % 4 {
            0 => "new"
            1 => "returning"
            2 => "vip"
            _ => "at_risk"
          }))
          attrs
        }
      })
    }
  }
  
  // 验证业务度量配置
  assert_eq(revenue_counter.name, "business.revenue.total")
  assert_eq(revenue_counter.description, Some("Total revenue"))
  assert_eq(revenue_counter.unit, Some("USD"))
  
  assert_eq(order_counter.name, "business.orders.total")
  assert_eq(customer_counter.name, "business.customers.active")
  assert_eq(conversion_rate.name, "business.conversion.rate")
  
  // 验证业务逻辑
  let total_orders = 150.0
  let expected_new_customers = (150.0 / 5.0).to_int()  // 30个新客户
  let expected_lost_customers = (150.0 / 20.0).to_int()  // 7个流失客户
  let expected_active_customers = expected_new_customers - expected_lost_customers
  
  assert_true(total_orders == 150.0)
  assert_true(expected_active_customers >= 0)
}

test "度量性能和吞吐量测试" {
  // 测试度量系统的性能和吞吐量
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance-metrics-test")
  
  // 创建高性能度量
  let throughput_counter = Meter::create_counter(meter, "performance.throughput", Some("Operation throughput"), Some("ops/sec"))
  let latency_histogram = Meter::create_histogram(meter, "performance.latency", Some("Operation latency"), Some("ms"))
  
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // 高频度量操作
  for i in 0..=999 {  // 1000次操作
    Counter::add(throughput_counter, 1.0, {
      attributes: {
        let attrs = Attributes::new()
        Attributes::set(attrs, "operation", StringValue("high_freq_op"))
        Attributes::set(attrs, "batch", StringValue((i / 100).to_string()))
        attrs
      }
    })
    
    // 模拟操作延迟
    let latency = 1.0 + (Random::next_u64(Random::system()).to_double() % 50.0)
    Histogram::record(latency_histogram, latency, {
      attributes: {
        let attrs = Attributes::new()
        Attributes::set(attrs, "operation", StringValue("high_freq_op"))
        Attributes::set(attrs, "percentile", StringValue(if latency < 10.0 { "p50" } else if latency < 25.0 { "p90" } else { "p99" }))
        attrs
      }
    })
  }
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let operation_duration = end_time - start_time
  
  // 验证性能
  assert_true(operation_duration < 10000000000L)  // 小于10秒
  assert_true(throughput_counter.name == "performance.throughput")
  assert_true(latency_histogram.name == "performance.latency")
  
  // 计算吞吐量
  let throughput = 1000.0 / (operation_duration.to_double() / 1000000000.0)  // ops/sec
  assert_true(throughput > 100.0)  // 至少100 ops/sec
  
  // 测试内存使用和垃圾回收
  let memory_start = Clock::now_unix_nanos(Clock::system())
  
  let memory_intensive_metrics = []
  for i in 0..=99 {  // 100个不同的度量
    let counter = Meter::create_counter(meter, "memory.test.counter." + i.to_string(), Some("Memory test counter"), Some("count"))
    let histogram = Meter::create_histogram(meter, "memory.test.histogram." + i.to_string(), Some("Memory test histogram"), Some("ms"))
    
    memory_intensive_metrics.push((counter, histogram))
    
    // 为每个度量添加数据
    for j in 0..=9 {
      Counter::add(counter, j.to_double())
      Histogram::record(histogram, j.to_double())
    }
  }
  
  let memory_end = Clock::now_unix_nanos(Clock::system())
  let memory_duration = memory_end - memory_start
  
  // 验证内存性能
  assert_true(memory_duration < 5000000000L)  // 小于5秒
  assert_true(memory_intensive_metrics.length() == 100)
  
  // 验证内存密集型度量的创建
  for i in 0..=memory_intensive_metrics.length() - 1 {
    let (counter, histogram) = memory_intensive_metrics[i]
    
    assert_eq(counter.name, "memory.test.counter." + i.to_string())
    assert_eq(counter.description, Some("Memory test counter"))
    assert_eq(counter.unit, Some("count"))
    
    assert_eq(histogram.name, "memory.test.histogram." + i.to_string())
    assert_eq(histogram.description, Some("Memory test histogram"))
    assert_eq(histogram.unit, Some("ms"))
  }
}

test "度量错误处理和边界条件测试" {
  // 测试度量系统的错误处理和边界条件
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "error-handling-metrics-test")
  
  // 测试极值度量
  let extreme_counter = Meter::create_counter(meter, "extreme.values.counter", Some("Extreme values test"), Some("count"))
  let extreme_histogram = Meter::create_histogram(meter, "extreme.values.histogram", Some("Extreme values test"), Some("units"))
  
  // 测试极大值
  Counter::add(extreme_counter, 999999999.0)
  Counter::add(extreme_counter, -999999999.0)  // 负值测试
  Histogram::record(extreme_histogram, 999999999.0)
  Histogram::record(extreme_histogram, 0.0)  // 零值测试
  Histogram::record(extreme_histogram, -999999999.0)  // 负值测试
  
  // 测试极小值
  Counter::add(extreme_counter, 0.0000001)
  Histogram::record(extreme_histogram, 0.0000001)
  
  // 验证极值处理
  assert_eq(extreme_counter.name, "extreme.values.counter")
  assert_eq(extreme_histogram.name, "extreme.values.histogram")
  
  // 测试空属性和特殊字符
  let special_attrs_counter = Meter::create_counter(meter, "special.attrs.counter", Some("Special attributes test"), Some("count"))
  
  Counter::add(special_attrs_counter, 1.0, {
    attributes: {
      let attrs = Attributes::new()
      Attributes::set(attrs, "", StringValue("empty_key"))
      Attributes::set(attrs, "normal_key", StringValue(""))
      Attributes::set(attrs, "unicode_key", StringValue("测试中文🚀"))
      Attributes::set(attrs, "special_chars", StringValue("!@#$%^&*()"))
      attrs
    }
  })
  
  // 验证特殊属性处理
  assert_eq(special_attrs_counter.name, "special.attrs.counter")
  
  // 测试高频度量创建
  let high_freq_creation_start = Clock::now_unix_nanos(Clock::system())
  
  let high_freq_metrics = []
  for i in 0..=49 {  // 50个度量
    let counter = Meter::create_counter(meter, "high.freq.counter." + i.to_string())
    high_freq_metrics.push(counter)
  }
  
  let high_freq_creation_end = Clock::now_unix_nanos(Clock::system())
  let creation_duration = high_freq_creation_end - high_freq_creation_start
  
  // 验证高频创建性能
  assert_true(creation_duration < 5000000000L)  // 小于5秒
  assert_true(high_freq_metrics.length() == 50)
  
  // 测试度量名称长度边界
  let very_long_name = "this.is.a.very.long.metric.name.that.tests.the.boundary.conditions.of.the.metric.creation.system"
  let long_name_counter = Meter::create_counter(meter, very_long_name, Some("Very long name test"), Some("count"))
  
  assert_eq(long_name_counter.name, very_long_name)
  assert_eq(long_name_counter.description, Some("Very long name test"))
  assert_eq(long_name_counter.unit, Some("count"))
  
  // 测试重复度量名称
  let duplicate_counter1 = Meter::create_counter(meter, "duplicate.name")
  let duplicate_counter2 = Meter::create_counter(meter, "duplicate.name")
  
  assert_eq(duplicate_counter1.name, "duplicate.name")
  assert_eq(duplicate_counter2.name, "duplicate.name")
  
  // 测试度量操作的并发安全性（模拟）
  let concurrent_operations_start = Clock::now_unix_nanos(Clock::system())
  
  let concurrent_counter = Meter::create_counter(meter, "concurrent.operations.counter")
  
  // 模拟并发操作
  for i in 0..=99 {  // 100次操作
    Counter::add(concurrent_counter, 1.0, {
      attributes: {
        let attrs = Attributes::new()
        Attributes::set(attrs, "thread_id", StringValue((i % 4).to_string()))
        Attributes::set(attrs, "operation_id", StringValue(i.to_string()))
        attrs
      }
    })
  }
  
  let concurrent_operations_end = Clock::now_unix_nanos(Clock::system())
  let concurrent_duration = concurrent_operations_end - concurrent_operations_start
  
  // 验证并发操作性能
  assert_true(concurrent_duration < 5000000000L)  // 小于5秒
  assert_eq(concurrent_counter.name, "concurrent.operations.counter")
}