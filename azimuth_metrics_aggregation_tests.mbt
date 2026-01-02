// Azimuth Metrics Aggregation Test Suite
// 指标聚合测试用例，专注于各种指标类型的创建、使用和聚合逻辑

test "多种指标类型的创建和使用测试" {
  // 创建MeterProvider和Meter
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "aggregation_test_meter")
  
  // 创建不同类型的指标
  let counter = azimuth::Meter::create_counter(meter, "request_count", Some("总请求数"), Some("requests"))
  let histogram = azimuth::Meter::create_histogram(meter, "request_duration", Some("请求持续时间"), Some("ms"))
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "active_connections", Some("活跃连接数"), Some("connections"))
  let gauge = azimuth::Meter::create_gauge(meter, "memory_usage", Some("内存使用量"), Some("bytes"))
  
  // 验证指标创建
  assert_eq(azimuth::Counter::name(counter), "request_count")
  assert_eq(azimuth::Histogram::name(histogram), "request_duration")
  assert_eq(azimuth::UpDownCounter::name(updown_counter), "active_connections")
  assert_eq(azimuth::Gauge::name(gauge), "memory_usage")
  
  // 验证指标描述
  match azimuth::Instrument::description(azimuth::Histogram::as_instrument(histogram)) {
    Some(desc) => assert_eq(desc, "请求持续时间")
    None => assert_true(false)
  }
  
  // 验证指标单位
  match azimuth::Instrument::unit(azimuth::Histogram::as_instrument(histogram)) {
    Some(unit) => assert_eq(unit, "ms")
    None => assert_true(false)
  }
  
  // 使用Counter记录值
  azimuth::Counter::add(counter, 1.0)
  azimuth::Counter::add(counter, 5.0)
  azimuth::Counter::add(counter, 10.0)
  
  // 使用Histogram记录值
  azimuth::Histogram::record(histogram, 100.0)
  azimuth::Histogram::record(histogram, 200.0)
  azimuth::Histogram::record(histogram, 150.0)
  azimuth::Histogram::record(histogram, 300.0)
  
  // 使用UpDownCounter记录值
  azimuth::UpDownCounter::add(updown_counter, 10.0)  // 增加10个连接
  azimuth::UpDownCounter::add(updown_counter, 5.0)   // 再增加5个连接
  azimuth::UpDownCounter::add(updown_counter, -3.0)  // 减少3个连接
  
  // 使用Gauge记录值（模拟瞬时值）
  azimuth::Gauge::record(gauge, 1024.0)      // 1KB
  azimuth::Gauge::record(gauge, 2048.0)      // 2KB
  azimuth::Gauge::record(gauge, 1536.0)      // 1.5KB
}

test "带属性的指标聚合测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "attributes_aggregation_meter")
  
  // 创建带属性的指标
  let request_counter = azimuth::Meter::create_counter(meter, "requests_by_method")
  let response_histogram = azimuth::Meter::create_histogram(meter, "response_time_by_status")
  
  // 创建属性集合
  let get_method_attrs = azimuth::Attributes::new()
  let post_method_attrs = azimuth::Attributes::new()
  let put_method_attrs = azimuth::Attributes::new()
  
  let success_status_attrs = azimuth::Attributes::new()
  let error_status_attrs = azimuth::Attributes::new()
  
  // 记录不同方法的请求计数
  azimuth::Counter::add(request_counter, 100.0, Some(get_method_attrs))
  azimuth::Counter::add(request_counter, 50.0, Some(post_method_attrs))
  azimuth::Counter::add(request_counter, 25.0, Some(put_method_attrs))
  
  // 记录不同状态的响应时间
  azimuth::Histogram::record(response_histogram, 100.0, Some(success_status_attrs))
  azimuth::Histogram::record(response_histogram, 150.0, Some(success_status_attrs))
  azimuth::Histogram::record(response_histogram, 200.0, Some(success_status_attrs))
  azimuth::Histogram::record(response_histogram, 5000.0, Some(error_status_attrs))
  azimuth::Histogram::record(response_histogram, 3000.0, Some(error_status_attrs))
  
  // 创建更复杂的属性场景
  let http_counter = azimuth::Meter::create_counter(meter, "http_requests_total")
  
  // 模拟不同端点的请求
  let api_attrs = azimuth::Attributes::new()
  let web_attrs = azimuth::Attributes::new()
  let admin_attrs = azimuth::Attributes::new()
  
  azimuth::Counter::add(http_counter, 1000.0, Some(api_attrs))
  azimuth::Counter::add(http_counter, 500.0, Some(web_attrs))
  azimuth::Counter::add(http_counter, 50.0, Some(admin_attrs))
  
  // 验证指标名称
  assert_eq(azimuth::Counter::name(request_counter), "requests_by_method")
  assert_eq(azimuth::Histogram::name(response_histogram), "response_time_by_status")
  assert_eq(azimuth::Counter::name(http_counter), "http_requests_total")
}

test "指标边界值和特殊值测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "boundary_values_meter")
  
  // 创建指标
  let boundary_counter = azimuth::Meter::create_counter(meter, "boundary_counter")
  let boundary_histogram = azimuth::Meter::create_histogram(meter, "boundary_histogram")
  let boundary_updown = azimuth::Meter::create_updown_counter(meter, "boundary_updown")
  let boundary_gauge = azimuth::Meter::create_gauge(meter, "boundary_gauge")
  
  // 测试Counter的边界值
  azimuth::Counter::add(boundary_counter, 0.0)          // 零值
  azimuth::Counter::add(boundary_counter, 1.0)          // 最小正值
  azimuth::Counter::add(boundary_counter, -1.0)         // 负值（虽然Counter通常不应该为负）
  azimuth::Counter::add(boundary_counter, 999999.0)     // 大值
  azimuth::Counter::add(boundary_counter, 0.000001)     // 小正值
  
  // 测试Histogram的边界值
  azimuth::Histogram::record(boundary_histogram, 0.0)           // 零值
  azimuth::Histogram::record(boundary_histogram, 1.0)           // 最小正值
  azimuth::Histogram::record(boundary_histogram, -1.0)          // 负值
  azimuth::Histogram::record(boundary_histogram, 999999.0)      // 大值
  azimuth::Histogram::record(boundary_histogram, 0.000001)      // 小正值
  azimuth::Histogram::record(boundary_histogram, 3.1415926535)  // 浮点精度
  
  // 测试UpDownCounter的边界值
  azimuth::UpDownCounter::add(boundary_updown, 100.0)     // 初始正值
  azimuth::UpDownCounter::add(boundary_updown, -100.0)    // 完全抵消
  azimuth::UpDownCounter::add(boundary_updown, -50.0)     // 负值
  azimuth::UpDownCounter::add(boundary_updown, 200.0)     // 大正值
  azimuth::UpDownCounter::add(boundary_updown, 0.0)       // 零变化
  
  // 测试Gauge的边界值
  azimuth::Gauge::record(boundary_gauge, 0.0)             // 零值
  azimuth::Gauge::record(boundary_gauge, 1.0)             // 最小正值
  azimuth::Gauge::record(boundary_gauge, -1.0)            // 负值
  azimuth::Gauge::record(boundary_gauge, 999999.0)        // 大值
  azimuth::Gauge::record(boundary_gauge, 0.000001)        // 小正值
  azimuth::Gauge::record(boundary_gauge, -999999.0)       // 大负值
  
  // 验证指标名称
  assert_eq(azimuth::Counter::name(boundary_counter), "boundary_counter")
  assert_eq(azimuth::Histogram::name(boundary_histogram), "boundary_histogram")
  assert_eq(azimuth::UpDownCounter::name(boundary_updown), "boundary_updown")
  assert_eq(azimuth::Gauge::name(boundary_gauge), "boundary_gauge")
}

test "高频指标记录和性能测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "performance_meter")
  
  // 创建高频使用的指标
  let high_freq_counter = azimuth::Meter::create_counter(meter, "high_frequency_counter")
  let high_freq_histogram = azimuth::Meter::create_histogram(meter, "high_frequency_histogram")
  
  // 模拟高频Counter记录
  for i in range(0, 1000) {
    azimuth::Counter::add(high_freq_counter, 1.0)
  }
  
  // 模拟高频Histogram记录
  for i in range(0, 1000) {
    let value = (i % 100).to_double() + 1.0  // 1.0 到 100.0 的值
    azimuth::Histogram::record(high_freq_histogram, value)
  }
  
  // 创建多个不同类型的指标进行批量测试
  let batch_counters = [
    for i in range(0, 10) {
      azimuth::Meter::create_counter(meter, "batch_counter_" + i.to_string())
    }
  ]
  
  let batch_histograms = [
    for i in range(0, 10) {
      azimuth::Meter::create_histogram(meter, "batch_histogram_" + i.to_string())
    }
  ]
  
  let batch_updown_counters = [
    for i in range(0, 10) {
      azimuth::Meter::create_updown_counter(meter, "batch_updown_" + i.to_string())
    }
  ]
  
  let batch_gauges = [
    for i in range(0, 10) {
      azimuth::Meter::create_gauge(meter, "batch_gauge_" + i.to_string())
    }
  ]
  
  // 批量记录指标值
  for i in range(0, 100) {
    for j in range(0, 10) {
      // Counter记录
      azimuth::Counter::add(batch_counters[j], (i + 1).to_double())
      
      // Histogram记录
      azimuth::Histogram::record(batch_histograms[j], (i * 10 + j).to_double())
      
      // UpDownCounter记录
      let increment = if i % 2 == 0 { 1.0 } else { -1.0 }
      azimuth::UpDownCounter::add(batch_updown_counters[j], increment)
      
      // Gauge记录
      azimuth::Gauge::record(batch_gauges[j], (i * j).to_double())
    }
  }
  
  // 验证所有指标名称
  for i in range(0, 10) {
    assert_eq(azimuth::Counter::name(batch_counters[i]), "batch_counter_" + i.to_string())
    assert_eq(azimuth::Histogram::name(batch_histograms[i]), "batch_histogram_" + i.to_string())
    assert_eq(azimuth::UpDownCounter::name(batch_updown_counters[i]), "batch_updown_" + i.to_string())
    assert_eq(azimuth::Gauge::name(batch_gauges[i]), "batch_gauge_" + i.to_string())
  }
  
  // 验证数组长度
  assert_eq(batch_counters.length(), 10)
  assert_eq(batch_histograms.length(), 10)
  assert_eq(batch_updown_counters.length(), 10)
  assert_eq(batch_gauges.length(), 10)
}

test "复杂指标场景和业务逻辑测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "business_logic_meter")
  
  // 模拟电子商务网站的指标
  let order_counter = azimuth::Meter::create_counter(meter, "orders_total", Some("总订单数"), Some("orders"))
  let revenue_counter = azimuth::Meter::create_counter(meter, "revenue_total", Some("总收入"), Some("currency"))
  let cart_size_histogram = azimuth::Meter::create_histogram(meter, "cart_size", Some("购物车商品数量"), Some("items"))
  let checkout_time_histogram = azimuth::Meter::create_histogram(meter, "checkout_duration", Some("结账时间"), Some("seconds"))
  let active_users_gauge = azimuth::Meter::create_gauge(meter, "active_users", Some("活跃用户数"), Some("users"))
  let inventory_updown = azimuth::Meter::create_updown_counter(meter, "inventory_count", Some("库存数量"), Some("items"))
  
  // 创建属性
  let mobile_attrs = azimuth::Attributes::new()
  let desktop_attrs = azimuth::Attributes::new()
  let tablet_attrs = azimuth::Attributes::new()
  
  let electronics_attrs = azimuth::Attributes::new()
  let clothing_attrs = azimuth::Attributes::new()
  let books_attrs = azimuth::Attributes::new()
  
  // 模拟订单数据
  azimuth::Counter::add(order_counter, 1.0, Some(mobile_attrs))      // 移动端订单
  azimuth::Counter::add(order_counter, 1.0, Some(desktop_attrs))     // 桌面端订单
  azimuth::Counter::add(order_counter, 1.0, Some(mobile_attrs))      // 又一个移动端订单
  azimuth::Counter::add(order_counter, 1.0, Some(tablet_attrs))      // 平板端订单
  
  // 模拟收入数据
  azimuth::Counter::add(revenue_counter, 299.99, Some(electronics_attrs))  // 电子产品
  azimuth::Counter::add(revenue_counter, 49.99, Some(clothing_attrs))      // 服装
  azimuth::Counter::add(revenue_counter, 19.99, Some(books_attrs))         // 书籍
  azimuth::Counter::add(revenue_counter, 599.99, Some(electronics_attrs))  // 又一个电子产品
  
  // 模拟购物车大小
  azimuth::Histogram::record(cart_size_histogram, 3.0)  // 3件商品
  azimuth::Histogram::record(cart_size_histogram, 1.0)  // 1件商品
  azimuth::Histogram::record(cart_size_histogram, 5.0)  // 5件商品
  azimuth::Histogram::record(cart_size_histogram, 2.0)  // 2件商品
  azimuth::Histogram::record(cart_size_histogram, 10.0) // 10件商品
  
  // 模拟结账时间
  azimuth::Histogram::record(checkout_time_histogram, 45.5)  // 45.5秒
  azimuth::Histogram::record(checkout_time_histogram, 120.0) // 2分钟
  azimuth::Histogram::record(checkout_time_histogram, 30.0)  // 30秒
  azimuth::Histogram::record(checkout_time_histogram, 90.0)  // 1.5分钟
  
  // 模拟活跃用户数
  azimuth::Gauge::record(active_users_gauge, 1250.0)  // 1250个活跃用户
  azimuth::Gauge::record(active_users_gauge, 1180.0)  // 下降到1180
  azimuth::Gauge::record(active_users_gauge, 1320.0)  // 上升到1320
  
  // 模拟库存变化
  azimuth::UpDownCounter::add(inventory_updown, 100.0)   // 进货100件
  azimuth::UpDownCounter::add(inventory_updown, -25.0)   // 售出25件
  azimuth::UpDownCounter::add(inventory_updown, -10.0)   // 又售出10件
  azimuth::UpDownCounter::add(inventory_updown, 50.0)    // 再进货50件
  
  // 验证指标名称
  assert_eq(azimuth::Counter::name(order_counter), "orders_total")
  assert_eq(azimuth::Counter::name(revenue_counter), "revenue_total")
  assert_eq(azimuth::Histogram::name(cart_size_histogram), "cart_size")
  assert_eq(azimuth::Histogram::name(checkout_time_histogram), "checkout_duration")
  assert_eq(azimuth::Gauge::name(active_users_gauge), "active_users")
  assert_eq(azimuth::UpDownCounter::name(inventory_updown), "inventory_count")
}