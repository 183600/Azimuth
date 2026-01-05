// Azimuth 遥测数据过滤和转换测试
// 测试遥测数据的过滤和转换功能

// 测试1: 基于属性的过滤
test "基于属性的过滤测试" {
  // 创建过滤器管理器
  let filter_manager = FilterManager::new()
  
  // 创建测试数据
  let test_data = []
  
  // 添加不同服务的Span
  for i in 0..20 {
    let service_name = match i % 4 {
      0 => "payment-service",
      1 => "user-service",
      2 => "order-service",
      _ => "notification-service"
    }
    
    let span = {
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      operation_name: "operation." + i.to_string(),
      service_name: service_name,
      start_time: 1640995200 + i * 1000,
      end_time: 1640995250 + i * 1000,
      status: if i % 5 == 0 { "error" } else { "ok" },
      attributes: [
        ("http.method", if i % 2 == 0 { "GET" } else { "POST" }),
        ("http.status_code", (200 + (i % 4) * 100).to_string()),
        ("user.id", "user-" + (i % 10).to_string())
      ]
    }
    
    test_data = test_data.push(span)
  }
  
  // 测试服务名过滤
  let service_filter = FilterManager::create_attribute_filter(filter_manager, {
    field: "service_name",
    operator: "equals",
    value: "payment-service"
  })
  
  let filtered_by_service = FilterManager::apply_filter(filter_manager, test_data, service_filter)
  
  // 验证过滤结果
  assert_eq(filtered_by_service.length(), 5)  // 20个项目中5个属于payment-service
  
  for item in filtered_by_service {
    assert_eq(item.service_name, "payment-service")
  }
  
  // 测试状态过滤
  let status_filter = FilterManager::create_attribute_filter(filter_manager, {
    field: "status",
    operator: "equals",
    value: "error"
  })
  
  let filtered_by_status = FilterManager::apply_filter(filter_manager, test_data, status_filter)
  
  // 验证过滤结果
  assert_eq(filtered_by_status.length(), 4)  // 20个项目中4个状态为error
  
  for item in filtered_by_status {
    assert_eq(item.status, "error")
  }
  
  // 测试属性过滤
  let attribute_filter = FilterManager::create_attribute_filter(filter_manager, {
    field: "attributes.http.method",
    operator: "equals",
    value: "GET"
  })
  
  let filtered_by_attribute = FilterManager::apply_filter(filter_manager, test_data, attribute_filter)
  
  // 验证过滤结果
  assert_eq(filtered_by_attribute.length(), 10)  // 20个项目中10个使用GET方法
  
  for item in filtered_by_attribute {
    let http_method = item.attributes.find(fn(a) { a[0] == "http.method" })
    assert_true(http_method != None)
    match http_method {
      Some((_, method)) => assert_eq(method, "GET"),
      None => assert_true(false)
    }
  }
  
  // 测试组合过滤
  let combined_filter = FilterManager::create_and_filter(filter_manager, [
    service_filter,
    attribute_filter
  ])
  
  let filtered_by_combined = FilterManager::apply_filter(filter_manager, test_data, combined_filter)
  
  // 验证组合过滤结果
  assert_eq(filtered_by_combined.length(), 2)  // payment-service中使用GET方法的
  
  for item in filtered_by_combined {
    assert_eq(item.service_name, "payment-service")
    let http_method = item.attributes.find(fn(a) { a[0] == "http.method" })
    match http_method {
      Some((_, method)) => assert_eq(method, "GET"),
      None => assert_true(false)
    }
  }
  
  // 测试范围过滤
  let range_filter = FilterManager::create_range_filter(filter_manager, {
    field: "start_time",
    min_value: 1640995200,
    max_value: 1640995250
  })
  
  let filtered_by_range = FilterManager::apply_filter(filter_manager, test_data, range_filter)
  
  // 验证范围过滤结果
  assert_eq(filtered_by_range.length(), 1)  // 只有第一个项目在范围内
  
  for item in filtered_by_range {
    assert_true(item.start_time >= 1640995200)
    assert_true(item.start_time <= 1640995250)
  }
}

// 测试2: 基于时间的过滤
test "基于时间的过滤测试" {
  // 创建过滤器管理器
  let filter_manager = FilterManager::new()
  
  // 创建测试数据
  let test_data = []
  let base_time = 1640995200  // 2022-01-01 00:00:00 UTC
  
  for i in 0..100 {
    let span = {
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      operation_name: "operation." + i.to_string(),
      service_name: "test-service",
      start_time: base_time + i * 3600,  // 每小时一个
      end_time: base_time + i * 3600 + 100,
      status: "ok",
      attributes: []
    }
    
    test_data = test_data.push(span)
  }
  
  // 测试相对时间过滤（最近24小时）
  let now = base_time + 100 * 3600
  let recent_filter = FilterManager::create_relative_time_filter(filter_manager, {
    field: "start_time",
    time_range: "last_24h",
    reference_time: now
  })
  
  let filtered_recent = FilterManager::apply_filter(filter_manager, test_data, recent_filter)
  
  // 验证最近24小时过滤结果
  assert_eq(filtered_recent.length(), 24)  // 最近24小时的数据
  
  for item in filtered_recent {
    assert_true(item.start_time > now - 24 * 3600)
    assert_true(item.start_time <= now)
  }
  
  // 测试绝对时间过滤
  let absolute_filter = FilterManager::create_absolute_time_filter(filter_manager, {
    field: "start_time",
    start_time: base_time + 24 * 3600,  // 从第24小时开始
    end_time: base_time + 48 * 3600     // 到第48小时结束
  })
  
  let filtered_absolute = FilterManager::apply_filter(filter_manager, test_data, absolute_filter)
  
  // 验证绝对时间过滤结果
  assert_eq(filtered_absolute.length(), 25)  // 24小时到48小时之间（包含边界）
  
  for item in filtered_absolute {
    assert_true(item.start_time >= base_time + 24 * 3600)
    assert_true(item.start_time <= base_time + 48 * 3600)
  }
  
  // 测试时间窗口过滤（滑动窗口）
  let sliding_window_filter = FilterManager::create_sliding_window_filter(filter_manager, {
    field: "start_time",
    window_size: 12 * 3600,  // 12小时窗口
    slide_interval: 6 * 3600  // 每6小时滑动一次
  })
  
  let sliding_windows = FilterManager::apply_sliding_window(filter_manager, test_data, sliding_window_filter)
  
  // 验证滑动窗口结果
  assert_eq(sliding_windows.length(), 17)  // 100小时 / 6小时滑动间隔 ≈ 17个窗口
  
  // 验证第一个窗口（0-12小时）
  assert_eq(sliding_windows[0].data.length(), 13)  // 0-12小时，包含边界
  assert_eq(sliding_windows[0].window_start, base_time)
  assert_eq(sliding_windows[0].window_end, base_time + 12 * 3600)
  
  // 验证第二个窗口（6-18小时）
  assert_eq(sliding_windows[1].data.length(), 13)  // 6-18小时，包含边界
  assert_eq(sliding_windows[1].window_start, base_time + 6 * 3600)
  assert_eq(sliding_windows[1].window_end, base_time + 18 * 3600)
  
  // 测试时间模式过滤（工作日 vs 周末）
  let weekday_filter = FilterManager::create_time_pattern_filter(filter_manager, {
    field: "start_time",
    pattern: "weekday",
    timezone: "UTC"
  })
  
  let filtered_weekday = FilterManager::apply_filter(filter_manager, test_data, weekday_filter)
  
  // 验证工作日过滤结果（假设base_time是周六）
  // 周六到下周五共6天是工作日
  assert_eq(filtered_weekday.length(), 6 * 24)  // 6天 × 24小时
  
  // 测试小时模式过滤（工作时间）
  let business_hours_filter = FilterManager::create_time_pattern_filter(filter_manager, {
    field: "start_time",
    pattern: "business_hours",
    timezone: "UTC",
    business_start_hour: 9,
    business_end_hour: 17
  })
  
  let filtered_business_hours = FilterManager::apply_filter(filter_manager, test_data, business_hours_filter)
  
  // 验证工作时间过滤结果
  // 每天9小时（9:00-17:00），100天共900小时
  assert_eq(filtered_business_hours.length(), 100 * 9)
  
  for item in filtered_business_hours {
    let hour = (item.start_time - base_time) / 3600 % 24
    assert_true(hour >= 9 and hour < 17)
  }
}

// 测试3: 数据转换
test "数据转换测试" {
  // 创建转换管理器
  let transform_manager = TransformManager::new()
  
  // 创建测试数据
  let test_span = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    operation_name: "http.request",
    service_name: "api.gateway",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    attributes: [
      ("http.method", "GET"),
      ("http.status_code", "200"),
      ("http.url", "/api/users/12345"),
      ("user.id", "12345"),
      ("request.size", "1024"),
      ("response.size", "2048")
    ]
  }
  
  // 测试字段重命名转换
  let rename_transform = TransformManager::create_rename_transform(transform_manager, [
    ("operation_name", "operation"),
    ("service_name", "service"),
    ("start_time", "timestamp")
  ])
  
  let renamed_span = TransformManager::apply_transform(transform_manager, test_span, rename_transform)
  
  // 验证重命名结果
  assert_eq(renamed_span.operation, "http.request")
  assert_eq(renamed_span.service, "api.gateway")
  assert_eq(renamed_span.timestamp, 1640995200)
  
  // 原字段应不存在
  assert_true(renamed_span.operation_name == None)
  assert_true(renamed_span.service_name == None)
  assert_true(renamed_span.start_time == None)
  
  // 测试字段提取转换
  let extract_transform = TransformManager::create_extract_transform(transform_manager, [
    {
      source_field: "attributes.http.url",
      target_field: "endpoint",
      pattern: "/api/([^/]+)/.*",
      group_index: 1
    },
    {
      source_field: "attributes.user.id",
      target_field: "user_id",
      pattern: "([0-9]+)",
      group_index: 1
    }
  ])
  
  let extracted_span = TransformManager::apply_transform(transform_manager, test_span, extract_transform)
  
  // 验证提取结果
  assert_eq(extracted_span.endpoint, "users")
  assert_eq(extracted_span.user_id, "12345")
  
  // 测试计算字段转换
  let calculate_transform = TransformManager::create_calculate_transform(transform_manager, [
    {
      target_field: "duration",
      expression: "end_time - start_time"
    },
    {
      target_field: "request_response_ratio",
      expression: "attributes.response.size / attributes.request.size"
    }
  ])
  
  let calculated_span = TransformManager::apply_transform(transform_manager, test_span, calculate_transform)
  
  // 验证计算结果
  assert_eq(calculated_span.duration, 50)
  assert_eq(calculated_span.request_response_ratio, 2.0)
  
  // 测试类型转换
  let type_transform = TransformManager::create_type_transform(transform_manager, [
    {
      field: "attributes.request.size",
      from_type: "string",
      to_type: "integer"
    },
    {
      field: "attributes.response.size",
      from_type: "string",
      to_type: "integer"
    }
  ])
  
  let type_converted_span = TransformManager::apply_transform(transform_manager, test_span, type_transform)
  
  // 验证类型转换结果
  let request_size = type_converted_span.attributes.find(fn(a) { a[0] == "request.size" })
  match request_size {
    Some((_, value)) => assert_eq(value, 1024),  // 现在是整数
    None => assert_true(false)
  }
  
  let response_size = type_converted_span.attributes.find(fn(a) { a[0] == "response.size" })
  match response_size {
    Some((_, value)) => assert_eq(value, 2048),  // 现在是整数
    None => assert_true(false)
  }
  
  // 测试聚合转换
  let aggregate_transform = TransformManager::create_aggregate_transform(transform_manager, {
    group_by: ["service_name", "attributes.http.method"],
    aggregations: [
      {
        target_field: "total_requests",
        function: "count"
      },
      {
        target_field: "avg_duration",
        function: "avg",
        source_field: "duration"
      },
      {
        target_field: "max_response_size",
        function: "max",
        source_field: "attributes.response.size"
      }
    ]
  })
  
  // 创建多个测试Span用于聚合
  let multiple_spans = []
  
  for i in 0..10 {
    let span = {
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      operation_name: "http.request",
      service_name: if i % 2 == 0 { "api.gateway" } else { "user.service" },
      start_time: 1640995200 + i * 1000,
      end_time: 1640995250 + i * 1000,
      status: "ok",
      attributes: [
        ("http.method", if i % 3 == 0 { "GET" } else { "POST" }),
        ("http.status_code", "200"),
        ("response.size", (1000 + i * 100).to_string())
      ]
    }
    
    multiple_spans = multiple_spans.push(span)
  }
  
  let aggregated_data = TransformManager::apply_aggregate_transform(transform_manager, multiple_spans, aggregate_transform)
  
  // 验证聚合结果
  assert_true(aggregated_data.length() > 0)
  
  // 应该有不同的服务和方法组合
  let api_gateway_get = aggregated_data.find(fn(d) { 
    d.service_name == "api.gateway" and d.http_method == "GET" 
  })
  assert_true(api_gateway_get != None)
  
  match api_gateway_get {
    Some(data) => {
      assert_true(data.total_requests > 0)
      assert_true(data.avg_duration > 0)
      assert_true(data.max_response_size > 0)
    }
    None => assert_true(false)
  }
}

// 测试4: 数据格式转换
test "数据格式转换测试" {
  // 创建格式转换管理器
  let format_transformer = FormatTransformer::new()
  
  // 创建测试数据
  let test_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    operation_name: "database.query",
    service_name: "payment-service",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM orders"),
      ("db.rows_read", "10"),
      ("db.duration_ms", "50")
    ]
  }
  
  // 转换为OpenTelemetry格式
  let otel_format = FormatTransformer::to_opentelemetry(format_transformer, test_data)
  
  // 验证OpenTelemetry格式
  assert_eq(otel_format.trace_id, "trace-12345")
  assert_eq(otel_format.span_id, "span-67890")
  assert_eq(otel_format.name, "database.query")
  assert_eq(otel_format.kind, "internal")
  
  // 验证属性转换
  let db_type_attr = otel_format.attributes.find(fn(a) { a.key == "db.type" })
  assert_true(db_type_attr != None)
  match db_type_attr {
    Some(attr) => {
      assert_eq(attr.value.string_value, Some("postgresql"))
      assert_eq(attr.value.type, "string")
    }
    None => assert_true(false)
  }
  
  let db_rows_attr = otel_format.attributes.find(fn(a) { a.key == "db.rows_read" })
  assert_true(db_rows_attr != None)
  match db_rows_attr {
    Some(attr) => {
      assert_eq(attr.value.int_value, Some(10))
      assert_eq(attr.value.type, "int")
    }
    None => assert_true(false)
  }
  
  // 转换为Prometheus格式
  let prometheus_format = FormatTransformer::to_prometheus(format_transformer, test_data)
  
  // 验证Prometheus格式
  assert_true(prometheus_format.metrics.length() > 0)
  
  let duration_metric = prometheus_format.metrics.find(fn(m) { m.name == "span_duration_ms" })
  assert_true(duration_metric != None)
  
  match duration_metric {
    Some(metric) => {
      assert_eq(metric.value, 50.0)
      assert_true(metric.labels.contains(("service_name", "payment-service")))
      assert_true(metric.labels.contains(("operation_name", "database.query")))
    }
    None => assert_true(false)
  }
  
  // 转换为Jaeger格式
  let jaeger_format = FormatTransformer::to_jaeger(format_transformer, test_data)
  
  // 验证Jaeger格式
  assert_eq(jaeger_format.traceID, "trace-12345")
  assert_eq(jaeger_format.spanID, "span-67890")
  assert_eq(jaeger_format.operationName, "database.query")
  
  // 验证时间戳转换（微秒）
  assert_eq(jaeger_format.startTime, 1640995200 * 1000000)
  assert_eq(jaeger_format.duration, 50 * 1000)
  
  // 验证标签转换
  let service_tag = jaeger_format.tags.find(fn(t) { t.key == "service.name" })
  assert_true(service_tag != None)
  match service_tag {
    Some(tag) => assert_eq(tag.value, "payment-service"),
    None => assert_true(false)
  }
  
  // 转换为Zipkin格式
  let zipkin_format = FormatTransformer::to_zipkin(format_transformer, test_data)
  
  // 验证Zipkin格式
  assert_eq(zipkin_format.traceId, "trace-12345")
  assert_eq(zipkin_format.id, "span-67890")
  assert_eq(zipkin_format.name, "database.query")
  
  // 验证时间戳转换（微秒）
  assert_eq(zipkin_format.timestamp, 1640995200 * 1000000)
  assert_eq(zipkin_format.duration, 50 * 1000)
  
  // 验证标签转换
  let local_endpoint = zipkin_format.localEndpoint
  assert_true(local_endpoint != None)
  match local_endpoint {
    Some(endpoint) => assert_eq(endpoint.serviceName, "payment-service"),
    None => assert_true(false)
  }
  
  // 测试批量格式转换
  let batch_data = []
  
  for i in 0..10 {
    let data = {
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      operation_name: "operation." + i.to_string(),
      service_name: "service-" + (i % 3).to_string(),
      start_time: 1640995200 + i * 1000,
      end_time: 1640995250 + i * 1000,
      status: "ok",
      attributes: []
    }
    
    batch_data = batch_data.push(data)
  }
  
  // 批量转换为OpenTelemetry格式
  let batch_otel_format = FormatTransformer::batch_to_opentelemetry(format_transformer, batch_data)
  
  // 验证批量转换结果
  assert_eq(batch_otel_format.resource_spans.length(), 3)  // 3个不同的服务
  assert_eq(batch_otel_format.resource_spans[0].scope_spans[0].spans.length(), 4)  // 第一个服务有4个span
  
  // 批量转换为Prometheus格式
  let batch_prometheus_format = FormatTransformer::batch_to_prometheus(format_transformer, batch_data)
  
  // 验证批量Prometheus转换结果
  assert_true(batch_prometheus_format.metrics.length() > 0)
  
  // 验证服务维度
  let service_metrics = batch_prometheus_format.metrics.filter(fn(m) { 
    m.name == "span_count" 
  })
  assert_eq(service_metrics.length(), 3)  // 3个服务
  
  for metric in service_metrics {
    assert_eq(metric.value, 1.0)  // 每个服务一个指标
  }
}

// 测试5: 高级过滤和转换组合
test "高级过滤和转换组合测试" {
  // 创建处理管道
  let pipeline = ProcessingPipeline::new()
  
  // 创建测试数据
  let test_data = []
  
  for i in 0..100 {
    let service_name = match i % 4 {
      0 => "payment-service",
      1 => "user-service",
      2 => "order-service",
      _ => "notification-service"
    }
    
    let status = if i % 10 == 0 { "error" } else { "ok" }
    
    let data = {
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      operation_name: "operation." + i.to_string(),
      service_name: service_name,
      start_time: 1640995200 + i * 1000,
      end_time: 1640995250 + i * 1000,
      status: status,
      attributes: [
        ("http.method", if i % 2 == 0 { "GET" } else { "POST" }),
        ("http.status_code", if status == "error" { "500" } else { "200" }),
        ("user.id", "user-" + (i % 20).to_string()),
        ("request.size", (1000 + i * 10).to_string()),
        ("response.size", (2000 + i * 20).to_string())
      ]
    }
    
    test_data = test_data.push(data)
  }
  
  // 添加过滤阶段
  let service_filter = Pipeline::add_filter_stage(pipeline, {
    name: "service_filter",
    filter: {
      field: "service_name",
      operator: "in",
      value: ["payment-service", "order-service"]
    }
  })
  
  let status_filter = Pipeline::add_filter_stage(pipeline, {
    name: "status_filter",
    filter: {
      field: "status",
      operator: "equals",
      value: "ok"
    }
  })
  
  // 添加转换阶段
  let calculate_transform = Pipeline::add_transform_stage(pipeline, {
    name: "calculate_fields",
    transforms: [
      {
        target_field: "duration",
        expression: "end_time - start_time"
      },
      {
        target_field: "throughput",
        expression: "attributes.response.size / duration"
      }
    ]
  })
  
  let extract_transform = Pipeline::add_transform_stage(pipeline, {
    name: "extract_fields",
    transforms: [
      {
        source_field: "attributes.user.id",
        target_field: "user_id",
        pattern: "user-([0-9]+)",
        group_index: 1
      }
    ]
  })
  
  // 添加聚合阶段
  let aggregate_stage = Pipeline::add_aggregate_stage(pipeline, {
    name: "aggregate_by_service",
    group_by: ["service_name"],
    aggregations: [
      {
        target_field: "total_requests",
        function: "count"
      },
      {
        target_field: "avg_duration",
        function: "avg",
        source_field: "duration"
      },
      {
        target_field: "total_throughput",
        function: "sum",
        source_field: "throughput"
      }
    ]
  })
  
  // 执行管道
  let pipeline_result = Pipeline::execute(pipeline, test_data)
  
  // 验证管道结果
  assert_true(pipeline_result.success)
  assert_eq(pipeline_result.final_data.length(), 2)  // payment-service和order-service
  
  // 验证中间结果
  assert_eq(pipeline_result.stage_results.length(), 5)  // 2个过滤 + 2个转换 + 1个聚合
  
  // 验证过滤后数据
  let filtered_data = pipeline_result.stage_results[1].data  // 第二个过滤阶段后
  assert_eq(filtered_data.length(), 45)  // 100个项目中，50个属于指定服务，其中45个状态为ok
  
  // 验证转换后数据
  let transformed_data = pipeline_result.stage_results[3].data  // 第二个转换阶段后
  assert_eq(transformed_data.length(), 45)
  
  for item in transformed_data {
    assert_true(item.duration > 0)
    assert_true(item.throughput > 0)
    assert_true(item.user_id.length() > 0)
  }
  
  // 验证聚合结果
  let payment_service_result = pipeline_result.final_data.find(fn(d) { d.service_name == "payment-service" })
  assert_true(payment_service_result != None)
  
  match payment_service_result {
    Some(result) => {
      assert_true(result.total_requests > 0)
      assert_true(result.avg_duration > 0)
      assert_true(result.total_throughput > 0)
    }
    None => assert_true(false)
  }
  
  // 测试条件管道
  let conditional_pipeline = ProcessingPipeline::new()
  
  // 添加条件过滤阶段
  Pipeline::add_conditional_filter_stage(conditional_pipeline, {
    name: "error_analysis",
    condition: fn(data) { data.status == "error" },
    filter: {
      field: "service_name",
      operator: "equals",
      value: "payment-service"
    }
  })
  
  // 添加条件转换阶段
  Pipeline::add_conditional_transform_stage(conditional_pipeline, {
    name: "error_enrichment",
    condition: fn(data) { data.status == "error" },
    transforms: [
      {
        target_field: "error_category",
        expression: "\"server_error\""
      },
      {
        target_field: "requires_investigation",
        expression: "attributes.http.status_code == \"500\""
      }
    ]
  })
  
  // 执行条件管道
  let conditional_result = Pipeline::execute(conditional_pipeline, test_data)
  
  // 验证条件管道结果
  assert_true(conditional_result.success)
  
  // 只有payment-service的错误数据会被处理
  let payment_service_errors = conditional_result.final_data.filter(fn(d) { 
    d.service_name == "payment-service" and d.status == "error" 
  })
  
  assert_true(payment_service_errors.length() > 0)
  
  for error in payment_service_errors {
    assert_eq(error.error_category, "server_error")
    assert_eq(error.requires_investigation, true)
  }
  
  // 测试管道性能
  let performance_result = Pipeline::execute_with_profiling(pipeline, test_data)
  
  // 验证性能指标
  assert_true(performance_result.execution_time > 0)
  assert_true(performance_result.memory_usage > 0)
  
  // 验证各阶段性能
  for stage_perf in performance_result.stage_performance {
    assert_true(stage_perf.execution_time >= 0)
    assert_true(stage_perf.input_count >= stage_perf.output_count)
  }
}