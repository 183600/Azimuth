// Azimuth 数据一致性和完整性测试用例
// 专注于遥测数据的一致性和完整性验证

// 测试1: 追踪数据一致性验证
test "追踪数据一致性验证测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "data.consistency.test")
  
  // 创建根追踪
  let root_trace_id = "consistency-test-trace-12345"
  let root_span_id = "consistency-test-span-67890"
  let root_span = Tracer::start_span_with_trace_id(tracer, "root.operation", root_trace_id, root_span_id)
  
  // 设置根Span属性
  Span::set_attribute(root_span, "operation.type", StringValue("consistency.test"))
  Span::set_attribute(root_span, "test.scenario", StringValue("trace.data.consistency"))
  
  // 创建子Span
  let child_spans = []
  for i in 0..5 {
    let child_span = Tracer::start_span_with_parent(tracer, "child.operation." + i.to_string(), root_span)
    
    // 验证子Span的追踪ID与父Span一致
    let child_context = Span::span_context(child_span)
    assert_eq(SpanContext::trace_id(child_context), root_trace_id)
    
    // 验证子Span的父SpanID正确
    assert_eq(SpanContext::parent_span_id(child_context), Some(root_span_id))
    
    // 设置子Span属性
    Span::set_attribute(child_span, "child.index", IntValue(i))
    Span::set_attribute(child_span, "parent.trace.id", StringValue(root_trace_id))
    Span::set_attribute(child_span, "parent.span.id", StringValue(root_span_id))
    
    // 添加事件
    let event_attrs = Attributes::new()
    Attributes::set(event_attrs, "event.type", StringValue("child.execution"))
    Attributes::set(event_attrs, "child.index", IntValue(i))
    Attributes::set(event_attrs, "trace.id", StringValue(root_trace_id))
    Attributes::set(event_attrs, "parent.span.id", StringValue(root_span_id))
    
    Span::add_event(child_span, "child.event", Some(event_attrs))
    
    child_spans.push(child_span)
  }
  
  // 验证所有子Span的追踪ID一致性
  for child_span in child_spans {
    let child_context = Span::span_context(child_span)
    assert_eq(SpanContext::trace_id(child_context), root_trace_id)
  }
  
  // 结束所有Span
  for child_span in child_spans {
    Span::end(child_span)
  }
  Span::end(root_span)
  
  // 验证Span树结构完整性
  let trace_tree = TracerProvider::get_trace_tree(tracer_provider, root_trace_id)
  assert_eq(trace_tree.trace_id, root_trace_id)
  assert_eq(trace_tree.spans.length(), 6)  // 1个根Span + 5个子Span
  
  // 验证父子关系
  let root_span_node = trace_tree.spans.find(fn(s) { s.span_id == root_span_id })
  assert_true(root_span_node != None)
  
  match root_span_node {
    Some(span) => {
      assert_eq(span.parent_span_id, None)  // 根Span没有父Span
      assert_eq(span.children.length(), 5)  // 根Span有5个子Span
    }
    None => assert_true(false)
  }
  
  // 验证子Span的父关系
  for child_span in child_spans {
    let child_context = Span::span_context(child_span)
    let child_span_id = SpanContext::span_id(child_context)
    
    let child_span_node = trace_tree.spans.find(fn(s) { s.span_id == child_span_id })
    assert_true(child_span_node != None)
    
    match child_span_node {
      Some(span) => {
        assert_eq(span.parent_span_id, Some(root_span_id))  // 子Span的父SpanID应该是根SpanID
        assert_eq(span.children.length(), 0)  // 子Span没有子Span
      }
      None => assert_true(false)
    }
  }
}

// 测试2: 指标数据一致性验证
test "指标数据一致性验证测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "data.consistency.test")
  
  // 创建测试指标
  let counter = Meter::create_counter(meter, "consistency.test.counter", Some("一致性测试计数器"), Some("count"))
  let histogram = Meter::create_histogram(meter, "consistency.test.histogram", Some("一致性测试直方图"), Some("ms"))
  let gauge = Meter::create_gauge(meter, "consistency.test.gauge", Some("一致性测试仪表"), Some("value"))
  
  // 定义测试属性集
  let test_attributes = [
    [("service.name", StringValue("test.service")), ("operation.type", StringValue("consistency.test"))],
    [("service.name", StringValue("test.service")), ("operation.type", StringValue("data.validation"))],
    [("service.name", StringValue("another.service")), ("operation.type", StringValue("consistency.test"))]
  ]
  
  // 记录指标数据
  for i in 0..100 {
    for attrs in test_attributes {
      // 记录计数器
      Counter::add(counter, 1.0, Some(attrs))
      
      // 记录直方图
      Histogram::record(histogram, i.to_float(), Some(attrs))
      
      // 记录仪表（每10次更新一次）
      if i % 10 == 0 {
        Gauge::set(gauge, i.to_float(), Some(attrs))
      }
    }
  }
  
  // 验证指标数据一致性
  let counter_data = Meter::collect_counter_data(counter)
  let histogram_data = Meter::collect_histogram_data(histogram)
  let gauge_data = Meter::collect_gauge_data(gauge)
  
  // 验证计数器数据
  for attrs in test_attributes {
    let counter_value = counter_data.get(attrs)
    match counter_value {
      Some(value) => assert_eq(value, 100.0)  // 每个属性组合应该有100次计数
      None => assert_true(false)
    }
  }
  
  // 验证直方图数据
  for attrs in test_attributes {
    let histogram_values = histogram_data.get(attrs)
    match histogram_values {
      Some(values) => {
        assert_eq(values.count, 100)  // 每个属性组合应该有100个值
        assert_eq(values.min, 0.0)    // 最小值应该是0
        assert_eq(values.max, 99.0)   // 最大值应该是99
        assert_true(values.sum > 0)   // 总和应该大于0
        assert_true(values.avg > 0)   // 平均值应该大于0
      }
      None => assert_true(false)
    }
  }
  
  // 验证仪表数据
  for attrs in test_attributes {
    let gauge_value = gauge_data.get(attrs)
    match gauge_value {
      Some(value) => assert_eq(value, 90.0)  // 最后更新的值应该是90
      None => assert_true(false)
    }
  }
  
  // 验证指标元数据一致性
  assert_eq(counter.name, "consistency.test.counter")
  assert_eq(counter.description, Some("一致性测试计数器"))
  assert_eq(counter.unit, Some("count"))
  
  assert_eq(histogram.name, "consistency.test.histogram")
  assert_eq(histogram.description, Some("一致性测试直方图"))
  assert_eq(histogram.unit, Some("ms"))
  
  assert_eq(gauge.name, "consistency.test.gauge")
  assert_eq(gauge.description, Some("一致性测试仪表"))
  assert_eq(gauge.unit, Some("value"))
}

// 测试3: 日志数据一致性验证
test "日志数据一致性验证测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "data.consistency.test")
  
  // 创建测试日志记录
  let log_records = []
  let base_timestamp = 1640995200000  // 2022-01-01 00:00:00 UTC in milliseconds
  
  for i in 0..50 {
    let log_attrs = Attributes::new()
    Attributes::set(log_attrs, "log.sequence", IntValue(i))
    Attributes::set(log_attrs, "test.scenario", StringValue("data.consistency"))
    Attributes::set(log_attrs, "service.name", StringValue("test.service"))
    
    let severity = if i % 4 == 0 { Info } else if i % 4 == 1 { Warn } else if i % 4 == 2 { Error } else { Debug }
    
    let log_record = LogRecord::new(
      severity,
      Some("Consistency test log " + i.to_string()),
      Some(log_attrs),
      Some(base_timestamp + i * 1000),  // 每个日志间隔1秒
      Some(base_timestamp + i * 1000 + 100),  // 观察时间戳比日志时间戳晚100ms
      Some("consistency-test-trace-" + (i % 10).to_string()),
      Some("consistency-test-span-" + i.to_string())
    )
    
    log_records.push(log_record)
    Logger::emit(logger, log_record)
  }
  
  // 收集日志数据
  let collected_logs = LoggerProvider::get_collected_logs(logger_provider)
  
  // 验证日志数量一致性
  assert_eq(collected_logs.length(), 50)
  
  // 验证日志序列一致性
  for i in 0..50 {
    let log = collected_logs[i]
    
    // 验证日志消息
    match LogRecord::body(log) {
      Some(message) => assert_eq(message, "Consistency test log " + i.to_string())
      None => assert_true(false)
    }
    
    // 验证时间戳一致性
    assert_eq(LogRecord::timestamp(log), Some(base_timestamp + i * 1000))
    assert_eq(LogRecord::observed_timestamp(log), Some(base_timestamp + i * 1000 + 100))
    
    // 验证追踪ID一致性
    let expected_trace_id = "consistency-test-trace-" + (i % 10).to_string()
    assert_eq(LogRecord::trace_id(log), Some(expected_trace_id))
    
    // 验证Span ID一致性
    let expected_span_id = "consistency-test-span-" + i.to_string()
    assert_eq(LogRecord::span_id(log), Some(expected_span_id))
    
    // 验证严重性分布
    let expected_severity = if i % 4 == 0 { Info } else if i % 4 == 1 { Warn } else if i % 4 == 2 { Error } else { Debug }
    assert_eq(LogRecord::severity(log), expected_severity)
    
    // 验证属性一致性
    let sequence_attr = LogRecord::get_attribute(log, "log.sequence")
    match sequence_attr {
      IntValue(value) => assert_eq(value, i)
      _ => assert_true(false)
    }
    
    let scenario_attr = LogRecord::get_attribute(log, "test.scenario")
    match scenario_attr {
      StringValue(value) => assert_eq(value, "data.consistency")
      _ => assert_true(false)
    }
  }
  
  // 验证日志统计一致性
  let log_stats = LoggerProvider::get_log_statistics(logger_provider)
  assert_eq(log_stats.total_logs, 50)
  assert_eq(log_stats.info_logs, 13)  // 0, 4, 8, ..., 48 (13个)
  assert_eq(log_stats.warn_logs, 12)  // 1, 5, 9, ..., 49 (12个)
  assert_eq(log_stats.error_logs, 12) // 2, 6, 10, ..., 46 (12个)
  assert_eq(log_stats.debug_logs, 13) // 3, 7, 11, ..., 47 (13个)
}

// 测试4: 跨组件数据一致性验证
test "跨组件数据一致性验证测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross.component.consistency.test")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "cross.component.consistency.test")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "cross.component.consistency.test")
  
  // 创建跨组件测试场景
  let trace_id = "cross-component-test-trace-12345"
  let root_span = Tracer::start_span_with_trace_id(tracer, "cross.component.operation", trace_id, "root-span-67890")
  
  // 创建组件指标
  let component_counter = Meter::create_counter(meter, "component.operations")
  let component_latency = Meter::create_histogram(meter, "component.latency")
  
  // 模拟跨组件操作
  let components = ["component.a", "component.b", "component.c"]
  let component_spans = []
  
  for i in 0..components.length() {
    let component_name = components[i]
    
    // 创建组件Span
    let component_span = Tracer::start_span_with_parent(tracer, component_name + ".operation", root_span)
    
    // 验证追踪ID一致性
    let component_context = Span::span_context(component_span)
    assert_eq(SpanContext::trace_id(component_context), trace_id)
    
    // 设置组件Span属性
    Span::set_attribute(component_span, "component.name", StringValue(component_name))
    Span::set_attribute(component_span, "component.index", IntValue(i))
    Span::set_attribute(component_span, "trace.id", StringValue(trace_id))
    
    // 记录组件指标
    let component_attrs = [
      ("component.name", StringValue(component_name)),
      ("trace.id", StringValue(trace_id)),
      ("operation.type", StringValue("cross.component.test"))
    ]
    
    Counter::add(component_counter, 1.0, Some(component_attrs))
    Histogram::record(component_latency, (100 + i * 50).to_float(), Some(component_attrs))
    
    // 记录组件日志
    let log_attrs = Attributes::new()
    Attributes::set(log_attrs, "component.name", StringValue(component_name))
    Attributes::set(log_attrs, "component.index", IntValue(i))
    Attributes::set(log_attrs, "trace.id", StringValue(trace_id))
    Attributes::set(log_attrs, "operation.type", StringValue("cross.component.test"))
    
    let log_record = LogRecord::new_with_context(
      Info,
      Some("Component " + component_name + " operation completed"),
      Some(log_attrs),
      Some(Time::now()),
      Some(Time::now() + 1),
      Some(trace_id),
      Some(SpanContext::span_id(component_context))
    )
    Logger::emit(logger, log_record)
    
    component_spans.push(component_span)
  }
  
  // 结束所有Span
  for component_span in component_spans {
    Span::end(component_span)
  }
  Span::end(root_span)
  
  // 验证跨组件数据一致性
  let trace_tree = TracerProvider::get_trace_tree(tracer_provider, trace_id)
  assert_eq(trace_tree.spans.length(), 4)  // 1个根Span + 3个组件Span
  
  // 验证指标数据一致性
  let counter_data = Meter::collect_counter_data(component_counter)
  let histogram_data = Meter::collect_histogram_data(component_latency)
  
  for i in 0..components.length() {
    let component_name = components[i]
    let component_attrs = [
      ("component.name", StringValue(component_name)),
      ("trace.id", StringValue(trace_id)),
      ("operation.type", StringValue("cross.component.test"))
    ]
    
    // 验证计数器数据
    let counter_value = counter_data.get(component_attrs)
    match counter_value {
      Some(value) => assert_eq(value, 1.0)  // 每个组件应该有1次操作
      None => assert_true(false)
    }
    
    // 验证直方图数据
    let histogram_values = histogram_data.get(component_attrs)
    match histogram_values {
      Some(values) => {
        assert_eq(values.count, 1)  // 每个组件应该有1个延迟值
        assert_eq(values.min, (100 + i * 50).to_float())  // 验证延迟值
        assert_eq(values.max, (100 + i * 50).to_float())
        assert_eq(values.avg, (100 + i * 50).to_float())
      }
      None => assert_true(false)
    }
  }
  
  // 验证日志数据一致性
  let collected_logs = LoggerProvider::get_collected_logs(logger_provider)
  assert_eq(collected_logs.length(), 3)  // 应该有3个组件日志
  
  for i in 0..collected_logs.length() {
    let log = collected_logs[i]
    let component_name = components[i]
    
    // 验证日志属性
    let log_component_attr = LogRecord::get_attribute(log, "component.name")
    match log_component_attr {
      StringValue(value) => assert_eq(value, component_name)
      _ => assert_true(false)
    }
    
    let log_trace_attr = LogRecord::get_attribute(log, "trace.id")
    match log_trace_attr {
      StringValue(value) => assert_eq(value, trace_id)
      _ => assert_true(false)
    }
    
    // 验证追踪ID一致性
    assert_eq(LogRecord::trace_id(log), Some(trace_id))
  }
}

// 测试5: 数据序列化反序列化一致性验证
test "数据序列化反序列化一致性验证测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "serialization.consistency.test")
  
  // 创建测试Span
  let original_span = Tracer::start_span(tracer, "serialization.test.span")
  
  // 设置Span属性
  let test_attrs = [
    ("string.attr", StringValue("test string value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("array.attr", ArrayStringValue(["item1", "item2", "item3"]))
  ]
  
  Span::set_attributes(original_span, test_attrs)
  
  // 添加Span事件
  let event_attrs = Attributes::new()
  Attributes::set(event_attrs, "event.type", StringValue("test.event"))
  Attributes::set(event_attrs, "event.data", StringValue("test event data"))
  
  Span::add_event(original_span, "serialization.test.event", Some(event_attrs))
  
  // 序列化Span
  let serialized_data = Span::serialize(original_span)
  
  // 反序列化Span
  let deserialized_span = Span::deserialize(serialized_data)
  
  // 验证序列化反序列化一致性
  assert_eq(Span::name(deserialized_span), Span::name(original_span))
  assert_eq(Span::kind(deserialized_span), Span::kind(original_span))
  
  // 验证Span上下文一致性
  let original_context = Span::span_context(original_span)
  let deserialized_context = Span::span_context(deserialized_span)
  
  assert_eq(SpanContext::trace_id(deserialized_context), SpanContext::trace_id(original_context))
  assert_eq(SpanContext::span_id(deserialized_context), SpanContext::span_id(original_context))
  assert_eq(SpanContext::is_sampled(deserialized_context), SpanContext::is_sampled(original_context))
  
  // 验证属性一致性
  for (key, expected_value) in test_attrs {
    let actual_value = Span::get_attribute(deserialized_span, key)
    match actual_value {
      Some(value) => assert_eq(value, expected_value)
      None => assert_true(false)
    }
  }
  
  // 验证事件一致性
  let original_events = Span::get_events(original_span)
  let deserialized_events = Span::get_events(deserialized_span)
  
  assert_eq(original_events.length(), deserialized_events.length())
  
  if original_events.length() > 0 {
    let original_event = original_events[0]
    let deserialized_event = deserialized_events[0]
    
    assert_eq(original_event.name, deserialized_event.name)
    assert_eq(original_event.attributes.length(), deserialized_event.attributes.length())
    
    // 验证事件属性
    for (key, expected_value) in event_attrs {
      let actual_value = deserialized_event.attributes.find(fn(attr) { attr.0 == key })
      match actual_value {
        Some((_, value)) => assert_eq(value, expected_value)
        None => assert_true(false)
      }
    }
  }
  
  // 结束Span
  Span::end(original_span)
}

// 测试6: 数据聚合一致性验证
test "数据聚合一致性验证测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "aggregation.consistency.test")
  
  // 创建测试指标
  let test_counter = Meter::create_counter(meter, "aggregation.test.counter")
  let test_histogram = Meter::create_histogram(meter, "aggregation.test.histogram")
  
  // 定义测试数据
  let test_values = [10.0, 20.0, 30.0, 40.0, 50.0]
  let test_attributes = [
    [("region", StringValue("us-east-1")), ("service", StringValue("api"))],
    [("region", StringValue("us-west-2")), ("service", StringValue("api"))],
    [("region", StringValue("us-east-1")), ("service", StringValue("worker"))],
    [("region", StringValue("us-west-2")), ("service", StringValue("worker"))]
  ]
  
  // 记录测试数据
  for attrs in test_attributes {
    for value in test_values {
      Counter::add(test_counter, value, Some(attrs))
      Histogram::record(test_histogram, value, Some(attrs))
    }
  }
  
  // 执行数据聚合
  let aggregated_data = Meter::aggregate_data(meter_provider)
  
  // 验证聚合一致性
  let counter_data = aggregated_data.get("aggregation.test.counter")
  match counter_data {
    Some(data) => {
      // 验证按区域聚合
      let us_east_1_total = data.aggregations.get("region=us-east-1")
      match us_east_1_total {
        Some(value) => assert_eq(value, 150.0)  // (10+20+30+40+50) * 2 (api + worker)
        None => assert_true(false)
      }
      
      let us_west_2_total = data.aggregations.get("region=us-west-2")
      match us_west_2_total {
        Some(value) => assert_eq(value, 150.0)  // (10+20+30+40+50) * 2 (api + worker)
        None => assert_true(false)
      }
      
      // 验证按服务聚合
      let api_total = data.aggregations.get("service=api")
      match api_total {
        Some(value) => assert_eq(value, 150.0)  // (10+20+30+40+50) * 2 (us-east-1 + us-west-2)
        None => assert_true(false)
      }
      
      let worker_total = data.aggregations.get("service=worker")
      match worker_total {
        Some(value) => assert_eq(value, 150.0)  // (10+20+30+40+50) * 2 (us-east-1 + us-west-2)
        None => assert_true(false)
      }
      
      // 验证总和
      assert_eq(data.total, 300.0)  // 4个属性组合 * 150.0
    }
    None => assert_true(false)
  }
  
  // 验证直方图聚合
  let histogram_data = aggregated_data.get("aggregation.test.histogram")
  match histogram_data {
    Some(data) => {
      // 验证百分位数聚合
      let p50 = data.aggregations.get("p50")
      match p50 {
        Some(value) => assert_eq(value, 30.0)  // 中位数应该是30
        None => assert_true(false)
      }
      
      let p95 = data.aggregations.get("p95")
      match p95 {
        Some(value) => assert_eq(value, 50.0)  // 95百分位数应该是50
        None => assert_true(false)
      }
      
      // 验证统计聚合
      let avg = data.aggregations.get("avg")
      match avg {
        Some(value) => assert_eq(value, 30.0)  // 平均值应该是30
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// 测试7: 时间序列数据一致性验证
test "时间序列数据一致性验证测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "time.series.consistency.test")
  
  // 创建时间序列指标
  let time_series_gauge = Meter::create_gauge(meter, "time.series.gauge")
  let time_series_counter = Meter::create_counter(meter, "time.series.counter")
  
  // 定义时间序列数据点
  let base_timestamp = 1640995200000  // 2022-01-01 00:00:00 UTC in milliseconds
  let time_points = 100
  
  // 记录时间序列数据
  for i in 0..time_points {
    let timestamp = base_timestamp + i * 60000  // 每分钟一个数据点
    let value = 100.0 + (i % 20) * 5.0  // 周期性变化值
    
    let time_attrs = [
      ("timestamp", IntValue(timestamp)),
      ("series.id", StringValue("test.series.1"))
    ]
    
    Gauge::set(time_series_gauge, value, Some(time_attrs))
    
    if i > 0 {
      Counter::add(time_series_counter, 1.0, Some(time_attrs))
    }
  }
  
  // 获取时间序列数据
  let time_series_data = Meter::get_time_series_data(meter_provider)
  
  // 验证时间序列数据一致性
  let gauge_series = time_series_data.get("time.series.gauge")
  match gauge_series {
    Some(series) => {
      assert_eq(series.points.length(), time_points)
      
      // 验证时间戳序列
      for i in 0..series.points.length() {
        let point = series.points[i]
        let expected_timestamp = base_timestamp + i * 60000
        assert_eq(point.timestamp, expected_timestamp)
        
        // 验证值序列
        let expected_value = 100.0 + (i % 20) * 5.0
        assert_eq(point.value, expected_value)
      }
      
      // 验证时间序列单调性
      for i in 1..series.points.length() {
        assert_true(series.points[i].timestamp > series.points[i-1].timestamp)
      }
    }
    None => assert_true(false)
  }
  
  // 验证计数器时间序列
  let counter_series = time_series_data.get("time.series.counter")
  match counter_series {
    Some(series) => {
      assert_eq(series.points.length(), time_points - 1)  // 第一个点没有计数器数据
      
      // 验证计数器值单调递增
      for i in 1..series.points.length() {
        assert_eq(series.points[i].value, i.to_float())
      }
    }
    None => assert_true(false)
  }
}

// 测试8: 数据完整性验证
test "数据完整性验证测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "data.integrity.test")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "data.integrity.test")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "data.integrity.test")
  
  // 创建完整性检查场景
  let trace_id = "integrity-test-trace-12345"
  let root_span = Tracer::start_span_with_trace_id(tracer, "integrity.test.operation", trace_id, "root-span-67890")
  
  // 设置完整性检查属性
  let integrity_attrs = [
    ("test.scenario", StringValue("data.integrity")),
    ("trace.id", StringValue(trace_id)),
    ("checksum", StringValue("test-checksum-12345")),
    ("data.version", StringValue("1.0"))
  ]
  
  Span::set_attributes(root_span, integrity_attrs)
  
  // 创建完整性检查指标
  let integrity_counter = Meter::create_counter(meter, "integrity.checks")
  let integrity_gauge = Meter::create_gauge(meter, "integrity.score")
  
  // 记录完整性检查指标
  let check_attrs = [
    ("trace.id", StringValue(trace_id)),
    ("check.type", StringValue("data.integrity"))
  ]
  
  Counter::add(integrity_counter, 1.0, Some(check_attrs))
  Gauge::set(integrity_gauge, 100.0, Some(check_attrs))  // 完整性分数100%
  
  // 记录完整性检查日志
  let log_attrs = Attributes::new()
  Attributes::set(log_attrs, "test.scenario", StringValue("data.integrity"))
  Attributes::set(log_attrs, "trace.id", StringValue(trace_id))
  Attributes::set(log_attrs, "check.result", StringValue("passed"))
  Attributes::set(log_attrs, "integrity.score", IntValue(100))
  
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Data integrity check passed"),
    Some(log_attrs),
    Some(Time::now()),
    Some(Time::now() + 1),
    Some(trace_id),
    Some("root-span-67890")
  )
  Logger::emit(logger, log_record)
  
  // 添加完整性检查事件
  let event_attrs = Attributes::new()
  Attributes::set(event_attrs, "check.type", StringValue("data.integrity"))
  Attributes::set(event_attrs, "check.result", StringValue("passed"))
  Attributes::set(event_attrs, "integrity.score", IntValue(100))
  
  Span::add_event(root_span, "integrity.check.completed", Some(event_attrs))
  
  Span::end(root_span)
  
  // 验证数据完整性
  let trace_tree = TracerProvider::get_trace_tree(tracer_provider, trace_id)
  assert_eq(trace_tree.spans.length(), 1)
  
  let span_node = trace_tree.spans[0]
  
  // 验证完整性属性存在
  let checksum_attr = span_node.attributes.find(fn(attr) { attr.0 == "checksum" })
  match checksum_attr {
    Some((_, StringValue(value))) => assert_eq(value, "test-checksum-12345")
    _ => assert_true(false)
  }
  
  let version_attr = span_node.attributes.find(fn(attr) { attr.0 == "data.version" })
  match version_attr {
    Some((_, StringValue(value))) => assert_eq(value, "1.0")
    _ => assert_true(false)
  }
  
  // 验证完整性指标
  let counter_data = Meter::collect_counter_data(integrity_counter)
  let counter_value = counter_data.get(check_attrs)
  match counter_value {
    Some(value) => assert_eq(value, 1.0)
    None => assert_true(false)
  }
  
  let gauge_data = Meter::collect_gauge_data(integrity_gauge)
  let gauge_value = gauge_data.get(check_attrs)
  match gauge_value {
    Some(value) => assert_eq(value, 100.0)
    None => assert_true(false)
  }
  
  // 验证完整性日志
  let collected_logs = LoggerProvider::get_collected_logs(logger_provider)
  assert_eq(collected_logs.length(), 1)
  
  let log = collected_logs[0]
  let check_result_attr = LogRecord::get_attribute(log, "check.result")
  match check_result_attr {
    StringValue(value) => assert_eq(value, "passed")
    _ => assert_true(false)
  }
  
  let integrity_score_attr = LogRecord::get_attribute(log, "integrity.score")
  match integrity_score_attr {
    IntValue(value) => assert_eq(value, 100)
    _ => assert_true(false)
  }
  
  // 验证完整性事件
  let events = span_node.events
  assert_eq(events.length(), 1)
  
  let event = events[0]
  assert_eq(event.name, "integrity.check.completed")
  
  let event_result_attr = event.attributes.find(fn(attr) { attr.0 == "check.result" })
  match event_result_attr {
    Some((_, StringValue(value))) => assert_eq(value, "passed")
    _ => assert_true(false)
  }
  
  let event_score_attr = event.attributes.find(fn(attr) { attr.0 == "integrity.score" })
  match event_score_attr {
    Some((_, IntValue(value))) => assert_eq(value, 100)
    _ => assert_true(false)
  }
}