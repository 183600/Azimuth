// Instrument类型全面测试用例
// 测试所有Instrument类型的创建、操作和属性

test "counter_instrument_comprehensive_operations" {
  // 测试Counter仪器的全面操作
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "counter.test")
  
  // 创建不同类型的counter
  let simple_counter = Meter::create_counter(meter, "simple.counter")
  let detailed_counter = Meter::create_counter(
    meter, 
    "detailed.counter", 
    Some("Detailed counter description"), 
    Some("operations")
  )
  let unit_counter = Meter::create_counter(
    meter, 
    "unit.counter", 
    Some("Counter with unit"), 
    Some("bytes")
  )
  
  // 验证counter属性
  assert_eq(simple_counter.name, "simple.counter")
  assert_eq(simple_counter.description, None)
  assert_eq(simple_counter.unit, None)
  
  assert_eq(detailed_counter.name, "detailed.counter")
  assert_eq(detailed_counter.description, Some("Detailed counter description"))
  assert_eq(detailed_counter.unit, Some("operations"))
  
  assert_eq(unit_counter.name, "unit.counter")
  assert_eq(unit_counter.description, Some("Counter with unit"))
  assert_eq(unit_counter.unit, Some("bytes"))
  
  // 测试counter操作
  Counter::add(simple_counter, 1.0)
  Counter::add(simple_counter, 5.5)
  Counter::add(simple_counter, 10.25)
  
  Counter::add(detailed_counter, 100.0)
  Counter::add(detailed_counter, 200.5)
  
  Counter::add(unit_counter, 1024.0)
  Counter::add(unit_counter, 2048.75)
  
  // 测试counter转换为Instrument类型
  let simple_instrument = Counter(simple_counter.name, simple_counter.description, simple_counter.unit)
  assert_eq(Instrument::name(simple_instrument), "simple.counter")
  assert_eq(Instrument::description(simple_instrument), None)
  assert_eq(Instrument::unit(simple_instrument), None)
  
  let detailed_instrument = Counter(detailed_counter.name, detailed_counter.description, detailed_counter.unit)
  assert_eq(Instrument::name(detailed_instrument), "detailed.counter")
  assert_eq(Instrument::description(detailed_instrument), Some("Detailed counter description"))
  assert_eq(Instrument::unit(detailed_instrument), Some("operations"))
}

test "histogram_instrument_comprehensive_operations" {
  // 测试Histogram仪器的全面操作
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram.test")
  
  // 创建不同类型的histogram
  let latency_histogram = Meter::create_histogram(
    meter, 
    "request.latency", 
    Some("Request latency histogram"), 
    Some("ms")
  )
  let size_histogram = Meter::create_histogram(
    meter, 
    "response.size", 
    Some("Response size distribution"), 
    Some("bytes")
  )
  let simple_histogram = Meter::create_histogram(meter, "simple.histogram")
  
  // 验证histogram属性
  assert_eq(latency_histogram.name, "request.latency")
  assert_eq(latency_histogram.description, Some("Request latency histogram"))
  assert_eq(latency_histogram.unit, Some("ms"))
  
  assert_eq(size_histogram.name, "response.size")
  assert_eq(size_histogram.description, Some("Response size distribution"))
  assert_eq(size_histogram.unit, Some("bytes"))
  
  assert_eq(simple_histogram.name, "simple.histogram")
  assert_eq(simple_histogram.description, None)
  assert_eq(simple_histogram.unit, None)
  
  // 测试histogram记录操作 - 模拟各种延迟数据
  Histogram::record(latency_histogram, 10.5)    // 10.5ms
  Histogram::record(latency_histogram, 25.0)    // 25ms
  Histogram::record(latency_histogram, 50.75)   // 50.75ms
  Histogram::record(latency_histogram, 100.25)  // 100.25ms
  Histogram::record(latency_histogram, 250.5)   // 250.5ms
  Histogram::record(latency_histogram, 500.0)   // 500ms
  
  // 测试响应大小数据
  Histogram::record(size_histogram, 1024.0)     // 1KB
  Histogram::record(size_histogram, 4096.0)     // 4KB
  Histogram::record(size_histogram, 8192.5)     // 8KB
  Histogram::record(size_histogram, 16384.75)   // 16KB
  
  // 简单histogram测试
  Histogram::record(simple_histogram, 1.0)
  Histogram::record(simple_histogram, 2.5)
  Histogram::record(simple_histogram, 3.75)
  
  // 测试histogram转换为Instrument类型
  let histogram_instrument = Histogram::as_instrument(latency_histogram)
  assert_eq(Instrument::name(histogram_instrument), "request.latency")
  assert_eq(Instrument::description(histogram_instrument), Some("Request latency histogram"))
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
}

test "updown_counter_instrument_comprehensive_operations" {
  // 测试UpDownCounter仪器的全面操作
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "updown.test")
  
  // 创建不同类型的updown counter
  let connection_counter = Meter::create_updown_counter(
    meter, 
    "active.connections", 
    Some("Currently active connections"), 
    Some("connections")
  )
  let memory_counter = Meter::create_updown_counter(
    meter, 
    "memory.usage", 
    Some("Current memory usage"), 
    Some("bytes")
  )
  let queue_counter = Meter::create_updown_counter(
    meter, 
    "queue.size", 
    Some("Current queue size")
  )
  
  // 验证updown counter属性
  assert_eq(connection_counter.name, "active.connections")
  assert_eq(connection_counter.description, Some("Currently active connections"))
  assert_eq(connection_counter.unit, Some("connections"))
  
  assert_eq(memory_counter.name, "memory.usage")
  assert_eq(memory_counter.description, Some("Current memory usage"))
  assert_eq(memory_counter.unit, Some("bytes"))
  
  assert_eq(queue_counter.name, "queue.size")
  assert_eq(queue_counter.description, Some("Current queue size"))
  assert_eq(queue_counter.unit, None)
  
  // 测试updown counter的增减操作
  // 模拟连接数变化
  UpDownCounter::add(connection_counter, 5.0)    // 5个新连接
  UpDownCounter::add(connection_counter, 3.0)    // 3个更多连接
  UpDownCounter::add(connection_counter, -2.0)   // 2个连接断开
  UpDownCounter::add(connection_counter, -1.0)   // 1个连接断开
  
  // 模拟内存使用变化
  UpDownCounter::add(memory_counter, 1048576.0)    // 分配1MB
  UpDownCounter::add(memory_counter, 2097152.0)    // 分配2MB
  UpDownCounter::add(memory_counter, -524288.0)    // 释放512KB
  UpDownCounter::add(memory_counter, -1572864.0)   // 释放1.5MB
  
  // 模拟队列大小变化
  UpDownCounter::add(queue_counter, 10.0)    // 10个项目入队
  UpDownCounter::add(queue_counter, 5.0)     // 5个项目入队
  UpDownCounter::add(queue_counter, -3.0)    // 3个项目出队
  UpDownCounter::add(queue_counter, -7.0)    // 7个项目出队
  
  // 测试转换为Instrument类型
  let connection_instrument = UpDownCounter(
    connection_counter.name, 
    connection_counter.description, 
    connection_counter.unit
  )
  assert_eq(Instrument::name(connection_instrument), "active.connections")
  assert_eq(Instrument::description(connection_instrument), Some("Currently active connections"))
  assert_eq(Instrument::unit(connection_instrument), Some("connections"))
}

test "gauge_instrument_comprehensive_operations" {
  // 测试Gauge仪器的全面操作
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "gauge.test")
  
  // 创建不同类型的gauge
  let cpu_gauge = Meter::create_gauge(
    meter, 
    "cpu.usage", 
    Some("Current CPU usage percentage"), 
    Some("percent")
  )
  let temperature_gauge = Meter::create_gauge(
    meter, 
    "system.temperature", 
    Some("Current system temperature"), 
    Some("celsius")
  )
  let disk_gauge = Meter::create_gauge(
    meter, 
    "disk.usage", 
    Some("Current disk usage")
  )
  
  // 验证gauge属性
  assert_eq(cpu_gauge.name, "cpu.usage")
  assert_eq(cpu_gauge.description, Some("Current CPU usage percentage"))
  assert_eq(cpu_gauge.unit, Some("percent"))
  
  assert_eq(temperature_gauge.name, "system.temperature")
  assert_eq(temperature_gauge.description, Some("Current system temperature"))
  assert_eq(temperature_gauge.unit, Some("celsius"))
  
  assert_eq(disk_gauge.name, "disk.usage")
  assert_eq(disk_gauge.description, Some("Current disk usage"))
  assert_eq(disk_gauge.unit, None)
  
  // 注意：在简化实现中，Gauge可能没有set方法，但我们可以测试其属性
  // 在实际实现中，这里应该有类似 Gauge::set(cpu_gauge, 75.5) 的调用
  
  // 测试转换为Instrument类型
  let cpu_instrument = Gauge(cpu_gauge.name, cpu_gauge.description, cpu_gauge.unit)
  assert_eq(Instrument::name(cpu_instrument), "cpu.usage")
  assert_eq(Instrument::description(cpu_instrument), Some("Current CPU usage percentage"))
  assert_eq(Instrument::unit(cpu_instrument), Some("percent"))
}

test "instrument_type_polymorphism" {
  // 测试Instrument类型的多态性
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "polymorphism.test")
  
  // 创建各种类型的仪器
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "test.updown", Some("Test updown"), Some("value"))
  let gauge = Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("percent"))
  
  // 将所有仪器转换为Instrument类型
  let instruments = [
    Counter(counter.name, counter.description, counter.unit),
    Histogram(histogram.name, histogram.description, histogram.unit),
    UpDownCounter(updown_counter.name, updown_counter.description, updown_counter.unit),
    Gauge(gauge.name, gauge.description, gauge.unit)
  ]
  
  // 测试多态操作
  for (i, instrument) in instruments.enumerate() {
    match instrument {
      Counter(name, desc, unit) => {
        assert_eq(name, "test.counter")
        assert_eq(desc, Some("Test counter"))
        assert_eq(unit, Some("count"))
        assert_eq(i, 0)
      }
      Histogram(name, desc, unit) => {
        assert_eq(name, "test.histogram")
        assert_eq(desc, Some("Test histogram"))
        assert_eq(unit, Some("ms"))
        assert_eq(i, 1)
      }
      UpDownCounter(name, desc, unit) => {
        assert_eq(name, "test.updown")
        assert_eq(desc, Some("Test updown"))
        assert_eq(unit, Some("value"))
        assert_eq(i, 2)
      }
      Gauge(name, desc, unit) => {
        assert_eq(name, "test.gauge")
        assert_eq(desc, Some("Test gauge"))
        assert_eq(unit, Some("percent"))
        assert_eq(i, 3)
      }
    }
  }
}

test "instrument_attribute_integration" {
  // 测试仪器与属性的集成
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "attribute.test")
  
  // 创建属性
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("test.service"))
  Attributes::set(attrs, "service.version", StringValue("1.0.0"))
  Attributes::set(attrs, "environment", StringValue("test"))
  
  // 创建带属性的仪器操作
  let counter = Meter::create_counter(meter, "attributed.counter")
  let histogram = Meter::create_histogram(meter, "attributed.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "attributed.updown")
  
  // 使用属性进行仪器操作
  Counter::add(counter, 1.0, Some(attrs))
  Counter::add(counter, 5.5, Some(attrs))
  
  Histogram::record(histogram, 100.0, Some(attrs))
  Histogram::record(histogram, 200.5, Some(attrs))
  
  UpDownCounter::add(updown_counter, 10.0, Some(attrs))
  UpDownCounter::add(updown_counter, -5.0, Some(attrs))
  
  // 验证仪器属性不受影响
  assert_eq(counter.name, "attributed.counter")
  assert_eq(histogram.name, "attributed.histogram")
  assert_eq(updown_counter.name, "attributed.updown")
}