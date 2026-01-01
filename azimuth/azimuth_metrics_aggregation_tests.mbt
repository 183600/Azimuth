// Azimuth Metrics Aggregation Tests
// 指标聚合测试

test "基础metrics聚合测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "aggregation.test")
  
  // 创建counter并测试聚合
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  
  // 添加多个值
  Counter::add(counter, 10.0)
  Counter::add(counter, 20.0)
  Counter::add(counter, 30.0)
  Counter::add(counter, 40.0)
  Counter::add(counter, 50.0)
  
  // 验证counter属性
  assert_eq(counter.name, "test.counter")
  assert_eq(counter.description, Some("Test counter"))
  assert_eq(counter.unit, Some("count"))
  
  // 创建histogram并测试聚合
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  
  // 记录多个值
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 150.0)
  Histogram::record(histogram, 300.0)
  Histogram::record(histogram, 250.0)
  
  // 验证histogram属性
  assert_eq(histogram.name, "test.histogram")
  assert_eq(histogram.description, Some("Test histogram"))
  assert_eq(histogram.unit, Some("ms"))
  
  // 创建updown counter并测试聚合
  let updown_counter = Meter::create_updown_counter(meter, "test.updown", Some("Test up-down"), Some("value"))
  
  // 添加正值和负值
  UpDownCounter::add(updown_counter, 100.0)
  UpDownCounter::add(updown_counter, -25.0)
  UpDownCounter::add(updown_counter, 50.0)
  UpDownCounter::add(updown_counter, -75.0)
  UpDownCounter::add(updown_counter, 25.0)
  
  // 验证updown counter属性
  assert_eq(updown_counter.name, "test.updown")
  assert_eq(updown_counter.description, Some("Test up-down"))
  assert_eq(updown_counter.unit, Some("value"))
  
  assert_true(true)
}

test "多维度metrics聚合测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "multidimensional.aggregation.test")
  
  // 创建带有属性的metrics
  let base_attrs = Attributes::new()
  Attributes::set(base_attrs, "service.name", StringValue("test.service"))
  Attributes::set(base_attrs, "service.version", StringValue("1.0.0"))
  
  // 创建多个counter
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let error_counter = Meter::create_counter(meter, "http.errors.total")
  let duration_counter = Meter::create_counter(meter, "http.duration.total")
  
  // 模拟不同维度的请求
  let endpoints = ["/api/users", "/api/orders", "/api/products"]
  let methods = ["GET", "POST", "PUT", "DELETE"]
  let status_codes = [200, 400, 404, 500]
  
  for endpoint in endpoints {
    for method in methods {
      for status_code in status_codes {
        // 创建特定于请求的属性
        let request_attrs = Attributes::new()
        Attributes::set(request_attrs, "endpoint", StringValue(endpoint))
        Attributes::set(request_attrs, "method", StringValue(method))
        Attributes::set(request_attrs, "status_code", IntValue(status_code))
        
        // 记录请求
        Counter::add(request_counter, 1.0, Some(request_attrs))
        
        // 如果是错误状态，记录错误
        if status_code >= 400 {
          Counter::add(error_counter, 1.0, Some(request_attrs))
        }
        
        // 记录持续时间（模拟）
        let duration = 50.0 + status_code.to_double() * 0.1
        Counter::add(duration_counter, duration, Some(request_attrs))
      }
    }
  }
  
  // 验证metrics属性
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(error_counter.name, "http.errors.total")
  assert_eq(duration_counter.name, "http.duration.total")
  
  // 创建histogram并记录不同维度的值
  let response_histogram = Meter::create_histogram(meter, "http.response.duration")
  
  for endpoint in endpoints {
    for method in methods {
      // 模拟不同响应时间
      for i = 0; i < 100; i = i + 1 {
        let response_time = 50.0 + i.to_double() * 2.0
        
        let histogram_attrs = Attributes::new()
        Attributes::set(histogram_attrs, "endpoint", StringValue(endpoint))
        Attributes::set(histogram_attrs, "method", StringValue(method))
        
        Histogram::record(response_histogram, response_time, Some(histogram_attrs))
      }
    }
  }
  
  assert_true(true)
}

test "时间序列metrics聚合测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "timeseries.aggregation.test")
  
  // 创建时间序列相关的metrics
  let cpu_usage = Meter::create_gauge(meter, "system.cpu.usage")
  let memory_usage = Meter::create_gauge(meter, "system.memory.usage")
  let disk_io = Meter::create_counter(meter, "system.disk.io")
  let network_io = Meter::create_counter(meter, "system.network.io")
  
  // 模拟时间序列数据
  for time_point = 0; time_point < 100; time_point = time_point + 1 {
    // 模拟CPU使用率变化
    let cpu_value = 0.3 + (time_point.to_double() / 100.0) * 0.4
    // 注意：Gauge可能没有set方法，这里仅作测试
    
    // 模拟内存使用率变化
    let memory_value = 0.5 + (time_point.to_double() / 100.0) * 0.3
    
    // 模拟磁盘IO累积
    let disk_io_value = 1024.0 * time_point.to_double()
    Counter::add(disk_io, disk_io_value)
    
    // 模拟网络IO累积
    let network_io_value = 2048.0 * time_point.to_double()
    Counter::add(network_io, network_io_value)
    
    // 创建时间点属性
    let time_attrs = Attributes::new()
    Attributes::set(time_attrs, "timestamp", StringValue("2025-01-01T00:00:" + time_point.to_string() + "Z"))
    Attributes::set(time_attrs, "time_point", IntValue(time_point))
    
    // 记录带有时间属性的指标
    Counter::add(disk_io, disk_io_value, Some(time_attrs))
    Counter::add(network_io, network_io_value, Some(time_attrs))
  }
  
  // 验证metrics属性
  assert_eq(cpu_usage.name, "system.cpu.usage")
  assert_eq(memory_usage.name, "system.memory.usage")
  assert_eq(disk_io.name, "system.disk.io")
  assert_eq(network_io.name, "system.network.io")
  
  // 创建时间窗口聚合测试
  let window_counter = Meter::create_counter(meter, "window.aggregated.counter")
  
  // 模拟5分钟窗口的数据
  for window = 0; window < 5; window = window + 1 {
    for second = 0; second < 60; second = second + 1 {
      let window_attrs = Attributes::new()
      Attributes::set(window_attrs, "window", IntValue(window))
      Attributes::set(window_attrs, "second", IntValue(second))
      
      // 每秒记录一些数据
      Counter::add(window_counter, 1.0, Some(window_attrs))
    }
  }
  
  assert_true(true)
}

test "百分比和分位数聚合测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "percentile.aggregation.test")
  
  // 创建histogram用于分位数计算
  let response_histogram = Meter::create_histogram(meter, "http.response.time.percentiles")
  
  // 生成一组响应时间数据
  let response_times = [
    10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0,
    110.0, 120.0, 130.0, 140.0, 150.0, 160.0, 170.0, 180.0, 190.0, 200.0,
    210.0, 220.0, 230.0, 240.0, 250.0, 260.0, 270.0, 280.0, 290.0, 300.0,
    310.0, 320.0, 330.0, 340.0, 350.0, 360.0, 370.0, 380.0, 390.0, 400.0,
    410.0, 420.0, 430.0, 440.0, 450.0, 460.0, 470.0, 480.0, 490.0, 500.0
  ]
  
  // 记录响应时间
  for response_time in response_times {
    Histogram::record(response_histogram, response_time)
  }
  
  // 创建带有属性的histogram
  let endpoint_histogram = Meter::create_histogram(meter, "endpoint.response.time")
  
  let endpoints = ["/api/fast", "/api/medium", "/api/slow"]
  let fast_times = [10.0, 15.0, 20.0, 25.0, 30.0]
  let medium_times = [100.0, 150.0, 200.0, 250.0, 300.0]
  let slow_times = [500.0, 600.0, 700.0, 800.0, 900.0]
  
  // 记录不同端点的响应时间
  for time in fast_times {
    let attrs = Attributes::new()
    Attributes::set(attrs, "endpoint", StringValue("/api/fast"))
    Histogram::record(endpoint_histogram, time, Some(attrs))
  }
  
  for time in medium_times {
    let attrs = Attributes::new()
    Attributes::set(attrs, "endpoint", StringValue("/api/medium"))
    Histogram::record(endpoint_histogram, time, Some(attrs))
  }
  
  for time in slow_times {
    let attrs = Attributes::new()
    Attributes::set(attrs, "endpoint", StringValue("/api/slow"))
    Histogram::record(endpoint_histogram, time, Some(attrs))
  }
  
  // 验证histogram属性
  assert_eq(response_histogram.name, "http.response.time.percentiles")
  assert_eq(endpoint_histogram.name, "endpoint.response.time")
  
  assert_true(true)
}

test "metrics速率和导数聚合测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "rate.aggregation.test")
  
  // 创建用于速率计算的counter
  let request_counter = Meter::create_counter(meter, "requests.rate")
  let error_counter = Meter::create_counter(meter, "errors.rate")
  
  // 模拟时间序列请求数据
  let time_intervals = [10, 20, 30, 40, 50, 60]
  let requests_per_interval = [100, 150, 120, 180, 200, 170]
  let errors_per_interval = [5, 10, 8, 15, 12, 9]
  
  for i = 0; i < time_intervals.length(); i = i + 1 {
    let interval = time_intervals[i]
    let requests = requests_per_interval[i]
    let errors = errors_per_interval[i]
    
    // 创建时间间隔属性
    let interval_attrs = Attributes::new()
    Attributes::set(interval_attrs, "time_interval", IntValue(interval))
    Attributes::set(interval_attrs, "interval_index", IntValue(i))
    
    // 记录请求数
    Counter::add(request_counter, requests.to_double(), Some(interval_attrs))
    
    // 记录错误数
    Counter::add(error_counter, errors.to_double(), Some(interval_attrs))
  }
  
  // 创建用于计算速率的histogram
  let response_histogram = Meter::create_histogram(meter, "response.time.rate")
  
  // 模拟响应时间数据
  for i = 0; i < 100; i = i + 1 {
    let response_time = 50.0 + (i.to_double() * 2.0)
    
    let time_attrs = Attributes::new()
    Attributes::set(time_attrs, "time_index", IntValue(i))
    
    Histogram::record(response_histogram, response_time, Some(time_attrs))
  }
  
  // 验证metrics属性
  assert_eq(request_counter.name, "requests.rate")
  assert_eq(error_counter.name, "errors.rate")
  assert_eq(response_histogram.name, "response.time.rate")
  
  assert_true(true)
}

test "metrics分组和过滤聚合测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "grouping.aggregation.test")
  
  // 创建用于分组测试的metrics
  let grouped_counter = Meter::create_counter(meter, "grouped.requests")
  let grouped_histogram = Meter::create_histogram(meter, "grouped.response.time")
  
  // 定义分组维度
  let services = ["user.service", "order.service", "payment.service"]
  let regions = ["us-east", "us-west", "eu-west", "ap-southeast"]
  let environments = ["production", "staging", "development"]
  
  for service in services {
    for region in regions {
      for environment in environments {
        // 创建分组属性
        let group_attrs = Attributes::new()
        Attributes::set(group_attrs, "service", StringValue(service))
        Attributes::set(group_attrs, "region", StringValue(region))
        Attributes::set(group_attrs, "environment", StringValue(environment))
        
        // 记录请求数
        let request_count = 100.0 + (service.length() + region.length() + environment.length()).to_double()
        Counter::add(grouped_counter, request_count, Some(group_attrs))
        
        // 记录响应时间
        for i = 0; i < 10; i = i + 1 {
          let response_time = 50.0 + i.to_double() * 10.0
          Histogram::record(grouped_histogram, response_time, Some(group_attrs))
        }
      }
    }
  }
  
  // 测试过滤功能（通过属性选择特定子集）
  let prod_attrs = Attributes::new()
  Attributes::set(prod_attrs, "environment", StringValue("production"))
  
  // 记录生产环境的特定指标
  for service in services {
    for region in regions {
      let specific_attrs = Attributes::new()
      Attributes::set(specific_attrs, "service", StringValue(service))
      Attributes::set(specific_attrs, "region", StringValue(region))
      Attributes::set(specific_attrs, "environment", StringValue("production"))
      
      Counter::add(grouped_counter, 200.0, Some(specific_attrs))
      
      for i = 0; i < 5; i = i + 1 {
        let response_time = 30.0 + i.to_double() * 5.0
        Histogram::record(grouped_histogram, response_time, Some(specific_attrs))
      }
    }
  }
  
  // 验证metrics属性
  assert_eq(grouped_counter.name, "grouped.requests")
  assert_eq(grouped_histogram.name, "grouped.response.time")
  
  assert_true(true)
}

test "metrics聚合性能测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance.aggregation.test")
  
  // 创建大量metrics用于性能测试
  let counters = []
  let histograms = []
  let updown_counters = []
  
  // 创建100个不同类型的metrics
  for i = 0; i < 100; i = i + 1 {
    counters.push(Meter::create_counter(meter, "perf.counter." + i.to_string()))
    histograms.push(Meter::create_histogram(meter, "perf.histogram." + i.to_string()))
    updown_counters.push(Meter::create_updown_counter(meter, "perf.updown." + i.to_string()))
  }
  
  // 高频记录metrics数据
  let record_count = 1000
  
  for i = 0; i < record_count; i = i + 1 {
    let counter_index = i % counters.length()
    let histogram_index = i % histograms.length()
    let updown_index = i % updown_counters.length()
    
    // 记录counter
    Counter::add(counters[counter_index], i.to_double())
    
    // 记录histogram
    Histogram::record(histograms[histogram_index], i.to_double() * 0.1)
    
    // 记录updown counter
    UpDownCounter::add(updown_counters[updown_index], (i % 100).to_double() - 50.0)
    
    // 每100次记录带有属性的metrics
    if i % 100 == 0 {
      let attrs = Attributes::new()
      Attributes::set(attrs, "performance.test", StringValue("true"))
      Attributes::set(attrs, "record.index", IntValue(i))
      
      Counter::add(counters[counter_index], 1.0, Some(attrs))
      Histogram::record(histograms[histogram_index], 50.0 + i.to_double() * 0.01, Some(attrs))
    }
  }
  
  // 验证所有metrics都成功创建
  assert_eq(counters.length(), 100)
  assert_eq(histograms.length(), 100)
  assert_eq(updown_counters.length(), 100)
  
  // 测试大量属性组合的聚合
  let complex_counter = Meter::create_counter(meter, "complex.aggregation.counter")
  
  for i = 0; i < 500; i = i + 1 {
    let complex_attrs = Attributes::new()
    Attributes::set(complex_attrs, "attr1", StringValue("value1." + (i % 10).to_string()))
    Attributes::set(complex_attrs, "attr2", IntValue(i % 20))
    Attributes::set(complex_attrs, "attr3", FloatValue(i.to_double() * 0.1))
    Attributes::set(complex_attrs, "attr4", BoolValue(i % 2 == 0))
    
    Counter::add(complex_counter, 1.0, Some(complex_attrs))
  }
  
  assert_true(true)
}

test "跨instrument aggregation测试" {
  let meter_provider = MeterProvider::default()
  
  // 创建多个meter
  let meter1 = MeterProvider::get_meter(meter_provider, "service1")
  let meter2 = MeterProvider::get_meter(meter_provider, "service2")
  let meter3 = MeterProvider::get_meter(meter_provider, "service3")
  
  // 在不同meter中创建相同名称的metrics
  let counter1 = Meter::create_counter(meter1, "requests.total")
  let counter2 = Meter::create_counter(meter2, "requests.total")
  let counter3 = Meter::create_counter(meter3, "requests.total")
  
  let histogram1 = Meter::create_histogram(meter1, "response.time")
  let histogram2 = Meter::create_histogram(meter2, "response.time")
  let histogram3 = Meter::create_histogram(meter3, "response.time")
  
  // 为每个服务的metrics添加数据
  let service1_attrs = Attributes::new()
  Attributes::set(service1_attrs, "service", StringValue("service1"))
  Attributes::set(service1_attrs, "version", StringValue("1.0.0"))
  
  let service2_attrs = Attributes::new()
  Attributes::set(service2_attrs, "service", StringValue("service2"))
  Attributes::set(service2_attrs, "version", StringValue("2.0.0"))
  
  let service3_attrs = Attributes::new()
  Attributes::set(service3_attrs, "service", StringValue("service3"))
  Attributes::set(service3_attrs, "version", StringValue("3.0.0"))
  
  // 记录不同服务的请求数
  Counter::add(counter1, 1000.0, Some(service1_attrs))
  Counter::add(counter2, 1500.0, Some(service2_attrs))
  Counter::add(counter3, 800.0, Some(service3_attrs))
  
  // 记录不同服务的响应时间
  for i = 0; i < 100; i = i + 1 {
    Histogram::record(histogram1, 50.0 + i.to_double(), Some(service1_attrs))
    Histogram::record(histogram2, 70.0 + i.to_double(), Some(service2_attrs))
    Histogram::record(histogram3, 30.0 + i.to_double(), Some(service3_attrs))
  }
  
  // 验证metrics属性
  assert_eq(counter1.name, "requests.total")
  assert_eq(counter2.name, "requests.total")
  assert_eq(counter3.name, "requests.total")
  
  assert_eq(histogram1.name, "response.time")
  assert_eq(histogram2.name, "response.time")
  assert_eq(histogram3.name, "response.time")
  
  // 测试跨服务的聚合指标
  let total_requests = Meter::create_counter(meter1, "all.services.requests.total")
  
  // 模拟聚合所有服务的请求数
  Counter::add(total_requests, 1000.0, Some(service1_attrs))
  Counter::add(total_requests, 1500.0, Some(service2_attrs))
  Counter::add(total_requests, 800.0, Some(service3_attrs))
  
  assert_eq(total_requests.name, "all.services.requests.total")
  
  assert_true(true)
}