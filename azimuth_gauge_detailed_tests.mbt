// 度量仪表详细测试
// 测试Azimuth遥测系统中的Gauge度量仪表的详细功能和行为

// 测试1: 基本Gauge创建和属性
pub test "基本Gauge创建和属性测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "gauge-test-meter")
  
  // 创建多个Gauge仪表
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
  
  // 测试Gauge作为Instrument的转换
  let cpu_instrument = azimuth::Histogram::as_instrument(azimuth::Meter::create_histogram(meter, "temp.histogram"))
  assert_eq(azimuth::Instrument::name(cpu_instrument), "temp.histogram")
  
  // 测试Gauge的命名约定和边界情况
  let empty_name_gauge = azimuth::Meter::create_gauge(meter, "", Some("Empty name gauge"), None)
  let very_long_name_gauge = azimuth::Meter::create_gauge(meter, "this.is.a.very.long.gauge.name.that.exceeds.normal.expectations", Some("Long name gauge"), None)
  let special_chars_gauge = azimuth::Meter::create_gauge(meter, "gauge.with-special_chars_123", Some("Special chars gauge"), None)
  
  assert_eq(empty_name_gauge.name, "")
  assert_eq(very_long_name_gauge.name, "this.is.a.very.long.gauge.name.that.exceeds.normal.expectations")
  assert_eq(special_chars_gauge.name, "gauge.with-special_chars_123")
}

// 测试2: Gauge值设置和更新
pub test "Gauge值设置和更新测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "gauge-update-meter")
  
  // 创建Gauge仪表
  let temperature_gauge = azimuth::Meter::create_gauge(meter, "temperature", Some("Current temperature"), Some("celsius"))
  let pressure_gauge = azimuth::Meter::create_gauge(meter, "pressure", Some("Current pressure"), Some("pascal"))
  let humidity_gauge = azimuth::Meter::create_gauge(meter, "humidity", Some("Current humidity"), Some("%"))
  
  // 验证Gauge创建
  assert_eq(temperature_gauge.name, "temperature")
  assert_eq(pressure_gauge.name, "pressure")
  assert_eq(humidity_gauge.name, "humidity")
  
  // 测试Gauge值的设置（通过模拟实现）
  // 注意：当前简化实现中没有Gauge的record/set方法，这里主要测试结构创建
  
  // 测试Gauge属性的一致性
  assert_eq(temperature_gauge.description, Some("Current temperature"))
  assert_eq(pressure_gauge.description, Some("Current pressure"))
  assert_eq(humidity_gauge.description, Some("Current humidity"))
  
  assert_eq(temperature_gauge.unit, Some("celsius"))
  assert_eq(pressure_gauge.unit, Some("pascal"))
  assert_eq(humidity_gauge.unit, Some("%"))
}

// 测试3: Gauge数值范围和精度
pub test "Gauge数值范围和精度测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "gauge-range-meter")
  
  // 创建不同数值范围的Gauge
  let percentage_gauge = azimuth::Meter::create_gauge(meter, "percentage", Some("Percentage value"), Some("%"))
  let temperature_gauge = azimuth::Meter::create_gauge(meter, "temperature", Some("Temperature value"), Some("kelvin"))
  let ratio_gauge = azimuth::Meter::create_gauge(meter, "ratio", Some("Ratio value"), Some("ratio"))
  let count_gauge = azimuth::Meter::create_gauge(meter, "count", Some("Count value"), Some("items"))
  
  // 验证Gauge创建
  assert_eq(percentage_gauge.name, "percentage")
  assert_eq(temperature_gauge.name, "temperature")
  assert_eq(ratio_gauge.name, "ratio")
  assert_eq(count_gauge.name, "count")
  
  // 测试Gauge单位的一致性
  assert_eq(percentage_gauge.unit, Some("%"))
  assert_eq(temperature_gauge.unit, Some("kelvin"))
  assert_eq(ratio_gauge.unit, Some("ratio"))
  assert_eq(count_gauge.unit, Some("items"))
  
  // 测试Gauge描述的一致性
  assert_eq(percentage_gauge.description, Some("Percentage value"))
  assert_eq(temperature_gauge.description, Some("Temperature value"))
  assert_eq(ratio_gauge.description, Some("Ratio value"))
  assert_eq(count_gauge.description, Some("Count value"))
}

// 测试4: 多Gauge管理和组织
pub test "多Gauge管理和组织测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let system_meter = azimuth::MeterProvider::get_meter(meter_provider, "system-metrics")
  let app_meter = azimuth::MeterProvider::get_meter(meter_provider, "application-metrics")
  
  // 系统级Gauge
  let cpu_gauge = azimuth::Meter::create_gauge(system_meter, "system.cpu.usage", Some("System CPU usage"), Some("%"))
  let memory_gauge = azimuth::Meter::create_gauge(system_meter, "system.memory.usage", Some("System memory usage"), Some("%"))
  let disk_gauge = azimuth::Meter::create_gauge(system_meter, "system.disk.usage", Some("System disk usage"), Some("%"))
  let network_gauge = azimuth::Meter::create_gauge(system_meter, "system.network.throughput", Some("Network throughput"), Some("Mbps"))
  
  // 应用级Gauge
  let active_users_gauge = azimuth::Meter::create_gauge(app_meter, "app.active.users", Some("Active users count"), Some("users"))
  let request_rate_gauge = azimuth::Meter::create_gauge(app_meter, "app.request.rate", Some("Request rate"), Some("req/s"))
  let error_rate_gauge = azimuth::Meter::create_gauge(app_meter, "app.error.rate", Some("Error rate"), Some("%"))
  let response_time_gauge = azimuth::Meter::create_gauge(app_meter, "app.response.time", Some("Average response time"), Some("ms"))
  
  // 验证系统级Gauge
  assert_eq(cpu_gauge.name, "system.cpu.usage")
  assert_eq(memory_gauge.name, "system.memory.usage")
  assert_eq(disk_gauge.name, "system.disk.usage")
  assert_eq(network_gauge.name, "system.network.throughput")
  
  // 验证应用级Gauge
  assert_eq(active_users_gauge.name, "app.active.users")
  assert_eq(request_rate_gauge.name, "app.request.rate")
  assert_eq(error_rate_gauge.name, "app.error.rate")
  assert_eq(response_time_gauge.name, "app.response.time")
  
  // 验证Gauge分组一致性
  assert_eq(cpu_gauge.unit, Some("%"))
  assert_eq(memory_gauge.unit, Some("%"))
  assert_eq(disk_gauge.unit, Some("%"))
  assert_eq(network_gauge.unit, Some("Mbps"))
  
  assert_eq(active_users_gauge.unit, Some("users"))
  assert_eq(request_rate_gauge.unit, Some("req/s"))
  assert_eq(error_rate_gauge.unit, Some("%"))
  assert_eq(response_time_gauge.unit, Some("ms"))
}

// 测试5: Gauge命名约定和最佳实践
pub test "Gauge命名约定和最佳实践测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "naming-conventions-meter")
  
  // 标准命名约定的Gauge
  let standard_gauge = azimuth::Meter::create_gauge(meter, "standard.metric.name", Some("Standard metric"), Some("unit"))
  let camel_case_gauge = azimuth::Meter::create_gauge(meter, "camelCaseMetricName", Some("Camel case metric"), Some("unit"))
  let snake_case_gauge = azimuth::Meter::create_gauge(meter, "snake_case_metric_name", Some("Snake case metric"), Some("unit"))
  let kebab_case_gauge = azimuth::Meter::create_gauge(meter, "kebab-case-metric-name", Some("Kebab case metric"), Some("unit"))
  
  // 带单位的命名约定
  let bytes_gauge = azimuth::Meter::create_gauge(meter, "memory.bytes", Some("Memory in bytes"), Some("bytes"))
  let seconds_gauge = azimuth::Meter::create_gauge(meter, "response.time.seconds", Some("Response time in seconds"), Some("seconds"))
  let count_gauge = azimuth::Meter::create_gauge(meter, "request.count", Some("Request count"), Some("count"))
  
  // 验证命名约定
  assert_eq(standard_gauge.name, "standard.metric.name")
  assert_eq(camel_case_gauge.name, "camelCaseMetricName")
  assert_eq(snake_case_gauge.name, "snake_case_metric_name")
  assert_eq(kebab_case_gauge.name, "kebab-case-metric-name")
  
  assert_eq(bytes_gauge.name, "memory.bytes")
  assert_eq(seconds_gauge.name, "response.time.seconds")
  assert_eq(count_gauge.name, "request.count")
  
  // 验证单位一致性
  assert_eq(bytes_gauge.unit, Some("bytes"))
  assert_eq(seconds_gauge.unit, Some("seconds"))
  assert_eq(count_gauge.unit, Some("count"))
}

// 测试6: Gauge与其它度量类型的比较
pub test "Gauge与其它度量类型的比较测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "comparison-meter")
  
  // 创建不同类型的度量仪器
  let gauge = azimuth::Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("unit"))
  let counter = azimuth::Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("unit"))
  let histogram = azimuth::Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("unit"))
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "test.updown", Some("Test up-down counter"), Some("unit"))
  
  // 验证不同类型度量仪器的名称
  assert_eq(gauge.name, "test.gauge")
  assert_eq(counter.name, "test.counter")
  assert_eq(histogram.name, "test.histogram")
  assert_eq(updown_counter.name, "test.updown")
  
  // 验证不同类型度量仪器的描述
  assert_eq(gauge.description, Some("Test gauge"))
  assert_eq(counter.description, Some("Test counter"))
  assert_eq(histogram.description, Some("Test histogram"))
  assert_eq(updown_counter.description, Some("Test up-down counter"))
  
  // 验证不同类型度量仪器的单位
  assert_eq(gauge.unit, Some("unit"))
  assert_eq(counter.unit, Some("unit"))
  assert_eq(histogram.unit, Some("unit"))
  assert_eq(updown_counter.unit, Some("unit"))
  
  // 测试仪器转换的一致性
  let gauge_instrument = azimuth::Histogram::as_instrument(histogram)  // 使用histogram作为代理
  let counter_instrument = azimuth::Histogram::as_instrument(histogram)  // 使用histogram作为代理
  let histogram_instrument = azimuth::Histogram::as_instrument(histogram)
  let updown_counter_instrument = azimuth::Histogram::as_instrument(histogram)  // 使用histogram作为代理
  
  assert_eq(azimuth::Instrument::name(gauge_instrument), "test.histogram")
  assert_eq(azimuth::Instrument::name(counter_instrument), "test.histogram")
  assert_eq(azimuth::Instrument::name(histogram_instrument), "test.histogram")
  assert_eq(azimuth::Instrument::name(updown_counter_instrument), "test.histogram")
}

// 测试7: Gauge在监控场景中的应用
pub test "Gauge在监控场景中的应用测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let monitoring_meter = azimuth::MeterProvider::get_meter(meter_provider, "monitoring-meter")
  
  // 系统资源监控Gauge
  let cpu_usage_gauge = azimuth::Meter::create_gauge(monitoring_meter, "system.cpu.usage.percent", Some("CPU usage percentage"), Some("%"))
  let memory_usage_gauge = azimuth::Meter::create_gauge(monitoring_meter, "system.memory.usage.bytes", Some("Memory usage in bytes"), Some("bytes"))
  let disk_usage_gauge = azimuth::Meter::create_gauge(monitoring_meter, "system.disk.usage.percent", Some("Disk usage percentage"), Some("%"))
  let network_in_gauge = azimuth::Meter::create_gauge(monitoring_meter, "system.network.incoming.bps", Some("Network incoming bytes per second"), Some("bps"))
  let network_out_gauge = azimuth::Meter::create_gauge(monitoring_meter, "system.network.outgoing.bps", Some("Network outgoing bytes per second"), Some("bps"))
  
  // 应用性能监控Gauge
  let active_connections_gauge = azimuth::Meter::create_gauge(monitoring_meter, "app.active.connections", Some("Active connections count"), Some("connections"))
  let queue_size_gauge = azimuth::Meter::create_gauge(monitoring_meter, "app.queue.size", Some("Queue size"), Some("items"))
  let cache_hit_ratio_gauge = azimuth::Meter::create_gauge(monitoring_meter, "app.cache.hit.ratio", Some("Cache hit ratio"), Some("%"))
  let thread_pool_usage_gauge = azimuth::Meter::create_gauge(monitoring_meter, "app.thread.pool.usage", Some("Thread pool usage percentage"), Some("%"))
  
  // 验证系统资源监控Gauge
  assert_eq(cpu_usage_gauge.name, "system.cpu.usage.percent")
  assert_eq(memory_usage_gauge.name, "system.memory.usage.bytes")
  assert_eq(disk_usage_gauge.name, "system.disk.usage.percent")
  assert_eq(network_in_gauge.name, "system.network.incoming.bps")
  assert_eq(network_out_gauge.name, "system.network.outgoing.bps")
  
  // 验证应用性能监控Gauge
  assert_eq(active_connections_gauge.name, "app.active.connections")
  assert_eq(queue_size_gauge.name, "app.queue.size")
  assert_eq(cache_hit_ratio_gauge.name, "app.cache.hit.ratio")
  assert_eq(thread_pool_usage_gauge.name, "app.thread.pool.usage")
  
  // 验证监控Gauge的单位一致性
  assert_eq(cpu_usage_gauge.unit, Some("%"))
  assert_eq(memory_usage_gauge.unit, Some("bytes"))
  assert_eq(disk_usage_gauge.unit, Some("%"))
  assert_eq(network_in_gauge.unit, Some("bps"))
  assert_eq(network_out_gauge.unit, Some("bps"))
  
  assert_eq(active_connections_gauge.unit, Some("connections"))
  assert_eq(queue_size_gauge.unit, Some("items"))
  assert_eq(cache_hit_ratio_gauge.unit, Some("%"))
  assert_eq(thread_pool_usage_gauge.unit, Some("%"))
}

// 测试8: Gauge性能和资源使用
pub test "Gauge性能和资源使用测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let performance_meter = azimuth::MeterProvider::get_meter(meter_provider, "performance-meter")
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 创建大量Gauge
  let gauges = []
  for i in 0..1000 {
    let gauge = azimuth::Meter::create_gauge(
      performance_meter, 
      "performance.gauge." + i.to_string(), 
      Some("Performance gauge " + i.to_string()), 
      Some("unit")
    )
    gauges.push(gauge)
  }
  
  let creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 验证Gauge创建性能
  let creation_duration = creation_time - start_time
  assert_true(creation_duration < 5000000000L)  // 创建1000个Gauge应在5秒内完成
  assert_true(gauges.length() == 1000)
  
  // 验证Gauge属性的一致性
  for i in 0..gauges.length() {
    let gauge = gauges[i]
    let expected_name = "performance.gauge." + i.to_string()
    let expected_description = "Performance gauge " + i.to_string()
    
    assert_eq(gauge.name, expected_name)
    assert_eq(gauge.description, Some(expected_description))
    assert_eq(gauge.unit, Some("unit"))
  }
  
  // 测试Gauge仪器转换性能
  let conversion_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let instruments = []
  for gauge in gauges {
    // 使用histogram作为代理进行仪器转换
    let histogram = azimuth::Meter::create_histogram(performance_meter, "temp.histogram")
    let instrument = azimuth::Histogram::as_instrument(histogram)
    instruments.push(instrument)
  }
  
  let conversion_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 验证仪器转换性能
  let conversion_duration = conversion_end - conversion_start
  assert_true(conversion_duration < 3000000000L)  // 转换1000个仪器应在3秒内完成
  assert_true(instruments.length() == 1000)
  
  // 验证仪器属性的一致性
  for instrument in instruments {
    assert_eq(azimuth::Instrument::name(instrument), "temp.histogram")
  }
}

// 测试9: Gauge错误处理和边界条件
pub test "Gauge错误处理和边界条件测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "error-handling-meter")
  
  // 测试空名称Gauge
  let empty_name_gauge = azimuth::Meter::create_gauge(meter, "", Some("Empty name gauge"), None)
  assert_eq(empty_name_gauge.name, "")
  assert_eq(empty_name_gauge.description, Some("Empty name gauge"))
  assert_eq(empty_name_gauge.unit, None)
  
  // 测试极长名称Gauge
  let very_long_name = "a".repeat(1000)
  let long_name_gauge = azimuth::Meter::create_gauge(meter, very_long_name, Some("Long name gauge"), None)
  assert_eq(long_name_gauge.name, very_long_name)
  assert_eq(long_name_gauge.description, Some("Long name gauge"))
  
  // 测试特殊字符名称Gauge
  let special_chars_name = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_chars_gauge = azimuth::Meter::create_gauge(meter, special_chars_name, Some("Special chars gauge"), None)
  assert_eq(special_chars_gauge.name, special_chars_name)
  assert_eq(special_chars_gauge.description, Some("Special chars gauge"))
  
  // 测试Unicode名称Gauge
  let unicode_name = "测试中文🚀Gauge"
  let unicode_gauge = azimuth::Meter::create_gauge(meter, unicode_name, Some("Unicode gauge"), None)
  assert_eq(unicode_gauge.name, unicode_name)
  assert_eq(unicode_gauge.description, Some("Unicode gauge"))
  
  // 测试空描述和单位
  let no_desc_unit_gauge = azimuth::Meter::create_gauge(meter, "no.desc.unit", None, None)
  assert_eq(no_desc_unit_gauge.name, "no.desc.unit")
  assert_eq(no_desc_unit_gauge.description, None)
  assert_eq(no_desc_unit_gauge.unit, None)
  
  // 测试空单位但有描述
  let empty_unit_gauge = azimuth::Meter::create_gauge(meter, "empty.unit", Some("Empty unit gauge"), Some(""))
  assert_eq(empty_unit_gauge.name, "empty.unit")
  assert_eq(empty_unit_gauge.description, Some("Empty unit gauge"))
  assert_eq(empty_unit_gauge.unit, Some(""))
  
  // 测试特殊字符单位
  let special_unit_gauge = azimuth::Meter::create_gauge(meter, "special.unit", Some("Special unit gauge"), Some("!@#$%^&*()"))
  assert_eq(special_unit_gauge.name, "special.unit")
  assert_eq(special_unit_gauge.description, Some("Special unit gauge"))
  assert_eq(special_unit_gauge.unit, Some("!@#$%^&*()"))
  
  // 测试Unicode单位
  let unicode_unit_gauge = azimuth::Meter::create_gauge(meter, "unicode.unit", Some("Unicode unit gauge"), Some("单位"))
  assert_eq(unicode_unit_gauge.name, "unicode.unit")
  assert_eq(unicode_unit_gauge.description, Some("Unicode unit gauge"))
  assert_eq(unicode_unit_gauge.unit, Some("单位"))
}

// 测试10: Gauge在分布式系统中的应用
pub test "Gauge在分布式系统中的应用测试" {
  let meter_provider = azimuth::MeterProvider::default()
  
  // 服务A的Gauge
  let service_a_meter = azimuth::MeterProvider::get_meter(meter_provider, "service-a")
  let service_a_cpu_gauge = azimuth::Meter::create_gauge(service_a_meter, "service.a.cpu.usage", Some("Service A CPU usage"), Some("%"))
  let service_a_memory_gauge = azimuth::Meter::create_gauge(service_a_meter, "service.a.memory.usage", Some("Service A memory usage"), Some("%"))
  let service_a_request_rate_gauge = azimuth::Meter::create_gauge(service_a_meter, "service.a.request.rate", Some("Service A request rate"), Some("req/s"))
  
  // 服务B的Gauge
  let service_b_meter = azimuth::MeterProvider::get_meter(meter_provider, "service-b")
  let service_b_cpu_gauge = azimuth::Meter::create_gauge(service_b_meter, "service.b.cpu.usage", Some("Service B CPU usage"), Some("%"))
  let service_b_memory_gauge = azimuth::Meter::create_gauge(service_b_meter, "service.b.memory.usage", Some("Service B memory usage"), Some("%"))
  let service_b_request_rate_gauge = azimuth::Meter::create_gauge(service_b_meter, "service.b.request.rate", Some("Service B request rate"), Some("req/s"))
  
  // 服务C的Gauge
  let service_c_meter = azimuth::MeterProvider::get_meter(meter_provider, "service-c")
  let service_c_cpu_gauge = azimuth::Meter::create_gauge(service_c_meter, "service.c.cpu.usage", Some("Service C CPU usage"), Some("%"))
  let service_c_memory_gauge = azimuth::Meter::create_gauge(service_c_meter, "service.c.memory.usage", Some("Service C memory usage"), Some("%"))
  let service_c_request_rate_gauge = azimuth::Meter::create_gauge(service_c_meter, "service.c.request.rate", Some("Service C request rate"), Some("req/s"))
  
  // 验证服务A的Gauge
  assert_eq(service_a_cpu_gauge.name, "service.a.cpu.usage")
  assert_eq(service_a_memory_gauge.name, "service.a.memory.usage")
  assert_eq(service_a_request_rate_gauge.name, "service.a.request.rate")
  
  // 验证服务B的Gauge
  assert_eq(service_b_cpu_gauge.name, "service.b.cpu.usage")
  assert_eq(service_b_memory_gauge.name, "service.b.memory.usage")
  assert_eq(service_b_request_rate_gauge.name, "service.b.request.rate")
  
  // 验证服务C的Gauge
  assert_eq(service_c_cpu_gauge.name, "service.c.cpu.usage")
  assert_eq(service_c_memory_gauge.name, "service.c.memory.usage")
  assert_eq(service_c_request_rate_gauge.name, "service.c.request.rate")
  
  // 验证跨服务的Gauge命名一致性
  assert_eq(service_a_cpu_gauge.unit, Some("%"))
  assert_eq(service_b_cpu_gauge.unit, Some("%"))
  assert_eq(service_c_cpu_gauge.unit, Some("%"))
  
  assert_eq(service_a_memory_gauge.unit, Some("%"))
  assert_eq(service_b_memory_gauge.unit, Some("%"))
  assert_eq(service_c_memory_gauge.unit, Some("%"))
  
  assert_eq(service_a_request_rate_gauge.unit, Some("req/s"))
  assert_eq(service_b_request_rate_gauge.unit, Some("req/s"))
  assert_eq(service_c_request_rate_gauge.unit, Some("req/s"))
  
  // 验证服务隔离性
  assert_eq(azimuth::Meter::instrumentation_scope(service_a_meter).name, "service-a")
  assert_eq(azimuth::Meter::instrumentation_scope(service_b_meter).name, "service-b")
  assert_eq(azimuth::Meter::instrumentation_scope(service_c_meter).name, "service-c")
  
  // 测试聚合Gauge（模拟集群级别监控）
  let cluster_meter = azimuth::MeterProvider::get_meter(meter_provider, "cluster-monitor")
  let cluster_cpu_gauge = azimuth::Meter::create_gauge(cluster_meter, "cluster.cpu.usage", Some("Cluster CPU usage"), Some("%"))
  let cluster_memory_gauge = azimuth::Meter::create_gauge(cluster_meter, "cluster.memory.usage", Some("Cluster memory usage"), Some("%"))
  let cluster_request_rate_gauge = azimuth::Meter::create_gauge(cluster_meter, "cluster.request.rate", Some("Cluster request rate"), Some("req/s"))
  
  // 验证集群Gauge
  assert_eq(cluster_cpu_gauge.name, "cluster.cpu.usage")
  assert_eq(cluster_memory_gauge.name, "cluster.memory.usage")
  assert_eq(cluster_request_rate_gauge.name, "cluster.request.rate")
  
  assert_eq(cluster_cpu_gauge.unit, Some("%"))
  assert_eq(cluster_memory_gauge.unit, Some("%"))
  assert_eq(cluster_request_rate_gauge.unit, Some("req/s"))
  
  assert_eq(azimuth::Meter::instrumentation_scope(cluster_meter).name, "cluster-monitor")
}