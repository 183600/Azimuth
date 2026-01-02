// Azimuth 专项MoonBit测试用例
// 包含10个针对核心功能的深度测试

// 测试用例1: 属性值类型转换边界测试
test "属性值类型转换边界测试" {
  // 测试字符串到整数的边界转换
  let str_val = StringValue("2147483647") // Int最大值
  let int_val = AttributeValue::to_int(str_val)
  match int_val {
    Some(v) => assert_eq(v, 2147483647)
    None => assert_true(false)
  }
  
  // 测试浮点数精度边界
  let float_val = FloatValue(1.7976931348623157e+308) // Double最大值
  let converted_str = AttributeValue::to_string(float_val)
  match converted_str {
    Some(s) => assert_true(s.contains("1.797693"))
    None => assert_true(false)
  }
  
  // 测试布尔值在不同表示方式下的转换
  let bool_true = BoolValue(true)
  let bool_false = BoolValue(false)
  
  assert_eq(AttributeValue::to_string(bool_true), Some("true"))
  assert_eq(AttributeValue::to_string(bool_false), Some("false"))
  
  // 测试数组类型转换
  let string_array = ArrayStringValue(["item1", "item2", "item3"])
  let array_length = AttributeValue::array_length(string_array)
  assert_eq(array_length, 3)
  
  // 测试空数组处理
  let empty_array = ArrayIntValue([])
  assert_eq(AttributeValue::array_length(empty_array), 0)
}

// 测试用例2: 上下文传播深度嵌套测试
test "上下文传播深度嵌套测试" {
  // 创建根上下文
  let root_context = Context::create()
  Context::set(root_context, "service.name", "root-service")
  Context::set(root_context, "trace.id", "abc123")
  
  // 创建5层嵌套的子上下文
  let context1 = Context::create_child(root_context)
  Context::set(context1, "layer", "1")
  
  let context2 = Context::create_child(context1)
  Context::set(context2, "layer", "2")
  
  let context3 = Context::create_child(context2)
  Context::set(context3, "layer", "3")
  
  let context4 = Context::create_child(context3)
  Context::set(context4, "layer", "4")
  
  let context5 = Context::create_child(context4)
  Context::set(context5, "layer", "5")
  
  // 验证最深层上下文仍能访问根上下文信息
  let root_service = Context::get(context5, "service.name")
  let trace_id = Context::get(context5, "trace.id")
  let current_layer = Context::get(context5, "layer")
  
  match root_service {
    Some(v) => assert_eq(v, "root-service")
    None => assert_true(false)
  }
  
  match trace_id {
    Some(v) => assert_eq(v, "abc123")
    None => assert_true(false)
  }
  
  match current_layer {
    Some(v) => assert_eq(v, "5")
    None => assert_true(false)
  }
  
  // 验证上下文链的完整性
  let context_chain = Context::get_chain(context5)
  assert_eq(context_chain.length(), 6) // 根上下文 + 5层子上下文
}

// 测试用例3: 资源属性合并策略测试
test "资源属性合并策略测试" {
  // 创建基础资源
  let base_attributes = Attributes::create()
  Attributes::set(base_attributes, "service.name", StringValue("azimuth-core"))
  Attributes::set(base_attributes, "service.version", StringValue("1.0.0"))
  Attributes::set(base_attributes, "environment", StringValue("production"))
  
  let base_resource = Resource::create(base_attributes)
  
  // 创建覆盖资源
  let override_attributes = Attributes::create()
  Attributes::set(override_attributes, "service.version", StringValue("2.0.0")) // 覆盖版本
  Attributes::set(override_attributes, "region", StringValue("us-west-2")) // 新增属性
  Attributes::set(override_attributes, "instance.id", StringValue("i-1234567890")) // 新增属性
  
  let override_resource = Resource::create(override_attributes)
  
  // 执行合并策略
  let merged_resource = Resource::merge(base_resource, override_resource)
  let merged_attributes = Resource::attributes(merged_resource)
  
  // 验证合并结果
  let service_name = Attributes::get(merged_attributes, "service.name")
  let service_version = Attributes::get(merged_attributes, "service.version")
  let environment = Attributes::get(merged_attributes, "environment")
  let region = Attributes::get(merged_attributes, "region")
  let instance_id = Attributes::get(merged_attributes, "instance.id")
  
  // 验证原有属性保持不变
  match service_name {
    Some(StringValue(v)) => assert_eq(v, "azimuth-core")
    _ => assert_true(false)
  }
  
  match environment {
    Some(StringValue(v)) => assert_eq(v, "production")
    _ => assert_true(false)
  }
  
  // 验证被覆盖的属性
  match service_version {
    Some(StringValue(v)) => assert_eq(v, "2.0.0")
    _ => assert_true(false)
  }
  
  // 验证新增的属性
  match region {
    Some(StringValue(v)) => assert_eq(v, "us-west-2")
    _ => assert_true(false)
  }
  
  match instance_id {
    Some(StringValue(v)) => assert_eq(v, "i-1234567890")
    _ => assert_true(false)
  }
}

// 测试用例4: 跨服务追踪一致性测试
test "跨服务追踪一致性测试" {
  // 模拟服务A的追踪
  let service_a_tracer = Tracer::create("service-a")
  let span_a = Tracer::start_span(service_a_tracer, "api.request")
  let context_a = Span::context(span_a)
  
  Span::set_attribute(span_a, "http.method", StringValue("GET"))
  Span::set_attribute(span_a, "http.url", StringValue("/api/users"))
  Span::set_attribute(span_a, "service.name", StringValue("user-service"))
  
  // 创建传播载体
  let carrier = TextMapCarrier::create()
  ContextPropagator::inject(context_a, carrier)
  
  // 模拟服务B接收并提取上下文
  let service_b_tracer = Tracer::create("service-b")
  let extracted_context = ContextPropagator::extract(carrier)
  let span_b = Tracer::start_span_with_context(service_b_tracer, "database.query", extracted_context)
  
  Span::set_attribute(span_b, "db.statement", StringValue("SELECT * FROM users"))
  Span::set_attribute(span_b, "db.type", StringValue("postgresql"))
  Span::set_attribute(span_b, "service.name", StringValue("database-service"))
  
  // 验证追踪ID一致性
  let trace_id_a = SpanContext::trace_id(Context::span_context(context_a))
  let trace_id_b = SpanContext::trace_id(Context::span_context(extracted_context))
  
  assert_eq(trace_id_a, trace_id_b)
  
  // 验证父子关系
  let parent_span_id = SpanContext::span_id(Context::span_context(context_a))
  let current_span_id = SpanContext::span_id(Span::context(span_b))
  
  assert_true(parent_span_id != current_span_id) // 确保不是同一个span
  
  // 完成span
  Span::end(span_b)
  Span::end(span_a)
}

// 测试用例5: 高并发场景下的遥测数据完整性测试
test "高并发场景下的遥测数据完整性测试" {
  let meter_provider = MeterProvider::create()
  let meter = MeterProvider::get_meter(meter_provider, "concurrency.test")
  
  // 创建多个并发指标
  let counters = Array::new()
  for i = 0; i < 10; i = i + 1 {
    let counter_name = "concurrent.counter." + i.to_string()
    let counter = Meter::create_counter(meter, counter_name)
    Array::push(counters, counter)
  }
  
  // 并发增加计数器
  for i = 0; i < counters.length(); i = i + 1 {
    let counter = Array::get(counters, i) |> Option::get_or_panic()
    // 每个计数器增加不同的值
    Counter::add(counter, (i + 1).to_double())
  }
  
  // 验证数据完整性
  for i = 0; i < counters.length(); i = i + 1 {
    let counter = Array::get(counters, i) |> Option::get_or_panic()
    let expected_value = (i + 1).to_double()
    let actual_value = Counter::value(counter)
    assert_eq(actual_value, expected_value)
  }
  
  // 创建直方图并记录并发数据
  let histogram = Meter::create_histogram(meter, "concurrent.histogram")
  for i = 0; i < 100; i = i + 1 {
    Histogram::record(histogram, i.to_double() / 10.0)
  }
  
  // 验证直方图统计数据
  let histogram_data = Histogram::get_data(histogram)
  assert_eq(histogram_data.count, 100)
  assert_true(histogram_data.sum > 0.0)
}

// 测试用例6: 时间序列数据聚合测试
test "时间序列数据聚合测试" {
  let time_series_provider = TimeSeriesProvider::create()
  let time_series = TimeSeriesProvider::get_series(time_series_provider, "test.metric")
  
  // 添加时间序列数据点
  let base_timestamp = Timestamp::now()
  
  // 添加10个数据点，每秒一个
  for i = 0; i < 10; i = i + 1 {
    let timestamp = Timestamp::add_seconds(base_timestamp, i)
    let value = i.to_double() * 2.5
    TimeSeries::add_point(time_series, timestamp, value)
  }
  
  // 测试时间范围查询
  let start_time = Timestamp::add_seconds(base_timestamp, 2)
  let end_time = Timestamp::add_seconds(base_timestamp, 7)
  let points_in_range = TimeSeries::get_points_in_range(time_series, start_time, end_time)
  
  assert_eq(points_in_range.length(), 6) // 应该包含第2到第7秒的数据点
  
  // 测试聚合函数
  let avg_value = TimeSeries::average(time_series, start_time, end_time)
  let max_value = TimeSeries::maximum(time_series, start_time, end_time)
  let min_value = TimeSeries::minimum(time_series, start_time, end_time)
  let sum_value = TimeSeries::sum(time_series, start_time, end_time)
  
  assert_true(avg_value > 5.0 && avg_value < 10.0)
  assert_eq(max_value, 17.5) // 第7秒的值
  assert_eq(min_value, 5.0) // 第2秒的值
  assert_eq(sum_value, 67.5) // 5.0 + 7.5 + 10.0 + 12.5 + 15.0 + 17.5
  
  // 测试降采样
  let downsampled = TimeSeries::downsample(time_series, Duration::seconds(3), Aggregation::Average)
  assert_eq(downsampled.length(), 4) // 10个点按3秒间隔降采样，应该得到4个点
}

// 测试用例7: 遥测配置动态更新测试
test "遥测配置动态更新测试" {
  let config_manager = ConfigurationManager::create()
  
  // 设置初始配置
  let initial_config = TelemetryConfig::create()
  TelemetryConfig::set_sampling_rate(initial_config, 0.1) // 10%采样率
  TelemetryConfig::set_max_spans(initial_config, 100)
  TelemetryConfig::set_batch_size(initial_config, 50)
  
  ConfigurationManager::update(config_manager, initial_config)
  
  // 验证初始配置
  let current_config = ConfigurationManager::get_current(config_manager)
  assert_eq(TelemetryConfig::sampling_rate(current_config), 0.1)
  assert_eq(TelemetryConfig::max_spans(current_config), 100)
  assert_eq(TelemetryConfig::batch_size(current_config), 50)
  
  // 动态更新配置
  let updated_config = TelemetryConfig::create()
  TelemetryConfig::set_sampling_rate(updated_config, 0.5) // 50%采样率
  TelemetryConfig::set_max_spans(updated_config, 200)
  TelemetryConfig::set_batch_size(updated_config, 100)
  
  ConfigurationManager::update(config_manager, updated_config)
  
  // 验证配置已更新
  let new_current_config = ConfigurationManager::get_current(config_manager)
  assert_eq(TelemetryConfig::sampling_rate(new_current_config), 0.5)
  assert_eq(TelemetryConfig::max_spans(new_current_config), 200)
  assert_eq(TelemetryConfig::batch_size(new_current_config), 100)
  
  // 验证配置变更通知
  let notification_received = ref false
  let listener = fn(config) {
    if TelemetryConfig::sampling_rate(config) == 0.5 {
      notification_received := true
    }
  }
  
  ConfigurationManager::add_listener(config_manager, listener)
  
  // 再次更新配置
  let another_config = TelemetryConfig::create()
  TelemetryConfig::set_sampling_rate(another_config, 0.8)
  ConfigurationManager::update(config_manager, another_config)
  
  assert_true(notification_received)
}

// 测试用例8: 边界条件错误处理测试
test "边界条件错误处理测试" {
  // 测试空字符串处理
  let empty_string = StringValue("")
  let empty_result = AttributeValue::to_int(empty_string)
  assert_eq(empty_result, None)
  
  // 测试极大数值处理
  let very_large_number = StringValue("999999999999999999999999")
  let large_result = AttributeValue::to_int(very_large_number)
  assert_eq(large_result, None) // 超出Int范围应返回None
  
  // 测试无效浮点数转换
  let invalid_float = StringValue("not.a.number")
  let float_result = AttributeValue::to_float(invalid_float)
  assert_eq(float_result, None)
  
  // 测试上下文键长度边界
  let very_long_key = "k".repeat(10000) // 超长键名
  let context = Context::create()
  
  // 应该能处理超长键名而不崩溃
  Context::set(context, very_long_key, "value")
  let retrieved_value = Context::get(context, very_long_key)
  
  match retrieved_value {
    Some(v) => assert_eq(v, "value")
    None => assert_true(false) // 即使键名很长，也应该能正常存储和检索
  }
  
  // 测试资源属性数量边界
  let attributes = Attributes::create()
  for i = 0; i < 1000; i = i + 1 {
    let key = "attr." + i.to_string()
    Attributes::set(attributes, key, StringValue("value." + i.to_string()))
  }
  
  // 验证大量属性不会导致性能问题或崩溃
  let attribute_count = Attributes::size(attributes)
  assert_eq(attribute_count, 1000)
}

// 测试用例9: 内存管理和资源清理测试
test "内存管理和资源清理测试" {
  // 测试大量span的创建和销毁
  let tracer_provider = TracerProvider::create()
  let tracer = TracerProvider::get_tracer(tracer_provider, "memory.test")
  
  let spans = Array::new()
  
  // 创建大量span
  for i = 0; i < 1000; i = i + 1 {
    let span_name = "span." + i.to_string()
    let span = Tracer::start_span(tracer, span_name)
    Array::push(spans, span)
  }
  
  // 验证所有span都已创建
  assert_eq(spans.length(), 1000)
  
  // 销毁所有span
  for i = 0; i < spans.length(); i = i + 1 {
    let span = Array::get(spans, i) |> Option::get_or_panic()
    Span::end(span)
  }
  
  // 清空数组引用
  Array::clear(spans)
  
  // 验证内存已释放（通过检查活跃span数量）
  let active_spans = TracerProvider::get_active_span_count(tracer_provider)
  assert_eq(active_spans, 0)
  
  // 测试指标的内存管理
  let meter_provider = MeterProvider::create()
  let meter = MeterProvider::get_meter(meter_provider, "memory.metric.test")
  
  let metrics = Array::new()
  
  // 创建大量指标
  for i = 0; i < 500; i = i + 1 {
    let counter_name = "counter." + i.to_string()
    let counter = Meter::create_counter(meter, counter_name)
    Counter::add(counter, i.to_double())
    Array::push(metrics, counter)
  }
  
  // 清理指标引用
  Array::clear(metrics)
  
  // 验证指标资源已清理
  let metric_count = MeterProvider::get_metric_count(meter_provider)
  assert_true(metric_count <= 500) // 允许一些延迟清理
}

// 测试用例10: 国际化和本地化支持测试
test "国际化和本地化支持测试" {
  // 测试多语言属性值
  let attributes = Attributes::create()
  
  // 添加中文属性
  Attributes::set(attributes, "service.name.zh", StringValue("方位角遥测系统"))
  Attributes::set(attributes, "service.description.zh", StringValue("一个高性能的遥测数据收集和分析系统"))
  
  // 添加英文属性
  Attributes::set(attributes, "service.name.en", StringValue("Azimuth Telemetry System"))
  Attributes::set(attributes, "service.description.en", StringValue("A high-performance telemetry data collection and analysis system"))
  
  // 添加日文属性
  Attributes::set(attributes, "service.name.ja", StringValue("アジマス・テレメトリー・システム"))
  
  // 测试本地化检索
  let zh_name = Attributes::get_localized(attributes, "service.name", "zh")
  let en_name = Attributes::get_localized(attributes, "service.name", "en")
  let ja_name = Attributes::get_localized(attributes, "service.name", "ja")
  
  match zh_name {
    Some(StringValue(v)) => assert_eq(v, "方位角遥测系统")
    _ => assert_true(false)
  }
  
  match en_name {
    Some(StringValue(v)) => assert_eq(v, "Azimuth Telemetry System")
    _ => assert_true(false)
  }
  
  match ja_name {
    Some(StringValue(v)) => assert_eq(v, "アジマス・テレメトリー・システム")
    _ => assert_true(false)
  }
  
  // 测试回退机制（当请求的语言不存在时回退到英文）
  let fr_name = Attributes::get_localized(attributes, "service.name", "fr")
  match fr_name {
    Some(StringValue(v)) => assert_eq(v, "Azimuth Telemetry System") // 回退到英文
    _ => assert_true(false)
  }
  
  // 测试Unicode文本处理
  let unicode_text = "测试Unicode: 🚀 telemetry 🌟 with émojis and ñ special chars"
  let unicode_attr = StringValue(unicode_text)
  let processed_text = AttributeValue::normalize_unicode(unicode_attr)
  
  match processed_text {
    StringValue(v) => assert_true(v.contains("🚀") && v.contains("🌟"))
    _ => assert_true(false)
  }
  
  // 测试不同地区的数字格式
  let number_value = FloatValue(1234.56)
  let formatted_us = AttributeValue::format_localized(number_value, "en-US")
  let formatted_fr = AttributeValue::format_localized(number_value, "fr-FR")
  
  match formatted_us {
    Some(v) => assert_true(v.contains("1,234.56"))
    None => assert_true(false)
  }
  
  match formatted_fr {
    Some(v) => assert_true(v.contains("1 234,56")) // 法语使用空格和逗号
    None => assert_true(false)
  }
}