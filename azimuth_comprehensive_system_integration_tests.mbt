// Azimuth Comprehensive System Integration Test Suite
// 测试Azimuth遥测系统的综合集成功能

// Test 1: 跨服务遥测数据序列化
test "cross-service telemetry data serialization" {
  // 定义跨服务遥测数据结构
  let telemetry_data = {
    trace_id: "trace-abc123def456",
    spans: [
      {
        span_id: "span-001",
        service_name: "auth-service",
        operation_name: "authenticate_user",
        start_time: 1640995200000,
        end_time: 1640995202500,
        status: "ok",
        attributes: [
          ("user.id", "12345"),
          ("auth.method", "oauth2"),
          ("auth.provider", "google")
        ]
      },
      {
        span_id: "span-002",
        service_name: "user-service",
        operation_name: "get_user_profile",
        start_time: 1640995202600,
        end_time: 1640995203100,
        status: "ok",
        attributes: [
          ("user.id", "12345"),
          ("profile.fields", "name,email,preferences"),
          ("cache.hit", "false")
        ]
      }
    ],
    metrics: [
      {
        name: "auth.request.duration",
        value: 250.0,
        unit: "ms",
        attributes: [
          ("service", "auth-service"),
          ("operation", "authenticate_user")
        ]
      },
      {
        name: "user.profile.request.duration",
        value: 500.0,
        unit: "ms",
        attributes: [
          ("service", "user-service"),
          ("operation", "get_user_profile")
        ]
      }
    ]
  }
  
  // 验证trace_id格式
  assert_eq(telemetry_data.trace_id.length(), 15)
  assert_true(telemetry_data.trace_id.starts_with("trace-"))
  
  // 验证spans数量和内容
  assert_eq(telemetry_data.spans.length(), 2)
  assert_eq(telemetry_data.spans[0].service_name, "auth-service")
  assert_eq(telemetry_data.spans[1].service_name, "user-service")
  
  // 验证时间顺序
  assert_true(telemetry_data.spans[0].start_time < telemetry_data.spans[1].start_time)
  
  // 验证metrics数量和内容
  assert_eq(telemetry_data.metrics.length(), 2)
  assert_eq(telemetry_data.metrics[0].name, "auth.request.duration")
  assert_eq(telemetry_data.metrics[1].name, "user.profile.request.duration")
  
  // 验证属性关联
  let auth_span = telemetry_data.spans[0]
  let auth_metric = telemetry_data.metrics[0]
  assert_eq(auth_span.attributes[0].1, "12345")
  assert_eq(auth_metric.attributes[0].1, "auth-service")
}

// Test 2: 实时性能监控和警报
test "real-time performance monitoring and alerting" {
  // 定义性能阈值配置
  let performance_thresholds = {
    response_time_p50: 100.0,  // 50百分位响应时间阈值(ms)
    response_time_p95: 500.0,  // 95百分位响应时间阈值(ms)
    error_rate: 0.05,  // 错误率阈值(5%)
    throughput_min: 100.0  // 最小吞吐量(requests/sec)
  }
  
  // 模拟实时性能数据
  let performance_metrics = [
    {
      timestamp: 1640995200,
      service: "api-gateway",
      endpoint: "/api/v1/orders",
      response_times: [50.0, 75.0, 120.0, 200.0, 450.0, 600.0, 80.0, 95.0],
      error_count: 3,
      total_requests: 100
    },
    {
      timestamp: 1640995260,
      service: "api-gateway",
      endpoint: "/api/v1/orders",
      response_times: [60.0, 85.0, 110.0, 180.0, 420.0, 580.0, 90.0, 105.0],
      error_count: 5,
      total_requests: 120
    },
    {
      timestamp: 1640995320,
      service: "api-gateway",
      endpoint: "/api/v1/orders",
      response_times: [55.0, 80.0, 115.0, 190.0, 430.0, 590.0, 85.0, 100.0],
      error_count: 2,
      total_requests: 110
    }
  ]
  
  // 计算百分位数的函数
  let calculate_percentile = fn(values: Array[Float], percentile: Float) {
    let sorted = values.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
    let index = ((sorted.length() - 1) as Float * percentile / 100.0) as Int
    sorted[index]
  }
  
  // 分析每个时间点的性能指标
  let analyze_performance = fn(metrics: {timestamp: Int, service: String, endpoint: String, response_times: Array[Float], error_count: Int, total_requests: Int}) {
    let p50 = calculate_percentile(metrics.response_times, 50.0)
    let p95 = calculate_percentile(metrics.response_times, 95.0)
    let error_rate = (metrics.error_count as Float) / (metrics.total_requests as Float)
    let throughput = (metrics.total_requests as Float) / 60.0  // requests per second
    
    {
      p50,
      p95,
      error_rate,
      throughput,
      alert_p50: p50 > performance_thresholds.response_time_p50,
      alert_p95: p95 > performance_thresholds.response_time_p95,
      alert_error_rate: error_rate > performance_thresholds.error_rate,
      alert_throughput: throughput < performance_thresholds.throughput_min
    }
  }
  
  // 分析所有时间点的性能
  let analysis_results = performance_metrics.map(analyze_performance)
  
  // 验证第一个时间点的分析结果
  let first_analysis = analysis_results[0]
  assert_true(first_analysis.p50 > 80.0 && first_analysis.p50 < 100.0)
  assert_true(first_analysis.p95 > 500.0 && first_analysis.p95 < 650.0)
  assert_eq(first_analysis.error_rate, 0.03)  // 3/100
  assert_eq(first_analysis.throughput, 100.0 / 60.0)
  assert_false(first_analysis.alert_p50)  // p50应该低于阈值
  assert_true(first_analysis.alert_p95)  // p95应该高于阈值
  assert_false(first_analysis.alert_error_rate)  // 错误率应该低于阈值
  assert_true(first_analysis.alert_throughput)  // 吞吐量应该低于阈值
  
  // 验证第二个时间点的分析结果
  let second_analysis = analysis_results[1]
  assert_eq(second_analysis.error_rate, 0.0417)  // 约5/120
  assert_true(second_analysis.alert_p95)  // p95应该高于阈值
  assert_false(second_analysis.alert_error_rate)  // 错误率应该低于阈值
}

// Test 3: 资源管理和内存泄漏检测
test "resource management and memory leak detection" {
  // 定义资源使用情况跟踪
  let resource_tracker = {
    allocated_objects: 0,
    deallocated_objects: 0,
    memory_usage: 0,
    peak_memory_usage: 0,
    resource_pools: []
  }
  
  // 模拟资源分配和释放
  let allocate_resource = fn(tracker: {allocated_objects: Int, deallocated_objects: Int, memory_usage: Int, peak_memory_usage: Int, resource_pools: Array[String>}, size: Int, pool: String) {
    let new_allocated = tracker.allocated_objects + 1
    let new_memory = tracker.memory_usage + size
    let new_peak = if new_memory > tracker.peak_memory_usage { new_memory } else { tracker.peak_memory_usage }
    
    {
      allocated_objects: new_allocated,
      deallocated_objects: tracker.deallocated_objects,
      memory_usage: new_memory,
      peak_memory_usage: new_peak,
      resource_pools: tracker.resource_pools.push(pool)
    }
  }
  
  let deallocate_resource = fn(tracker: {allocated_objects: Int, deallocated_objects: Int, memory_usage: Int, peak_memory_usage: Int, resource_pools: Array[String]}, size: Int) {
    let new_deallocated = tracker.deallocated_objects + 1
    let new_memory = if tracker.memory_usage >= size { tracker.memory_usage - size } else { 0 }
    
    {
      allocated_objects: tracker.allocated_objects,
      deallocated_objects: new_deallocated,
      memory_usage: new_memory,
      peak_memory_usage: tracker.peak_memory_usage,
      resource_pools: tracker.resource_pools
    }
  }
  
  // 模拟资源操作序列
  let step1 = allocate_resource(resource_tracker, 1024, "span-buffer")
  let step2 = allocate_resource(step1, 2048, "metric-buffer")
  let step3 = allocate_resource(step2, 512, "attribute-buffer")
  let step4 = deallocate_resource(step3, 512)
  let step5 = allocate_resource(step4, 4096, "large-buffer")
  let step6 = deallocate_resource(step5, 1024)
  let step7 = deallocate_resource(step6, 2048)
  
  // 验证资源使用情况
  assert_eq(step7.allocated_objects, 4)
  assert_eq(step7.deallocated_objects, 3)
  assert_eq(step7.memory_usage, 4096)  // 只剩large-buffer
  assert_eq(step7.peak_memory_usage, 3584)  // 1024+2048+512
  
  // 检测内存泄漏
  let detect_memory_leak = fn(tracker: {allocated_objects: Int, deallocated_objects: Int, memory_usage: Int, peak_memory_usage: Int, resource_pools: Array[String]}) {
    let leak_count = tracker.allocated_objects - tracker.deallocated_objects
    let leak_ratio = if tracker.allocated_objects > 0 {
      (leak_count as Float) / (tracker.allocated_objects as Float)
    } else {
      0.0
    }
    
    {
      leak_detected: leak_count > 0,
      leak_count,
      leak_ratio,
      memory_efficiency: if tracker.peak_memory_usage > 0 {
        (tracker.memory_usage as Float) / (tracker.peak_memory_usage as Float)
      } else {
        0.0
      }
    }
  }
  
  let leak_analysis = detect_memory_leak(step7)
  assert_true(leak_analysis.leak_detected)  // 有1个对象未释放
  assert_eq(leak_analysis.leak_count, 1)
  assert_eq(leak_analysis.leak_ratio, 0.25)  // 1/4
  assert_eq(leak_analysis.memory_efficiency, 4096.0 / 3584.0)  // 当前内存/峰值内存
  
  // 模拟完全释放
  let step8 = deallocate_resource(step7, 4096)
  let final_analysis = detect_memory_leak(step8)
  assert_eq(final_analysis.leak_count, 0)
  assert_false(final_analysis.leak_detected)
  assert_eq(final_analysis.memory_usage, 0)
}

// Test 4: 安全性和隐私保护测试
test "security and privacy protection tests" {
  // 定义敏感数据类型
  enum SensitiveDataType {
    PersonalIdentifier
    FinancialInformation
    HealthInformation
    AuthenticationCredentials
    Custom(String)
  }
  
  // 定义数据脱敏规则
  let data_masking_rules = [
    (SensitiveDataType::PersonalIdentifier, fn(data: String) {
      if data.length() <= 4 { "***" }
      else { data.substring(0, 2) + "*".repeat(data.length() - 4) + data.substring(data.length() - 2, 2) }
    }),
    (SensitiveDataType::FinancialInformation, fn(data: String) {
      if data.length() <= 4 { "****" }
      else { "*".repeat(data.length() - 4) + data.substring(data.length() - 4, 4) }
    }),
    (SensitiveDataType::AuthenticationCredentials, fn(_data: String) { "[REDACTED]" }),
    (SensitiveDataType::HealthInformation, fn(data: String) {
      if data.length() <= 6 { "[PROTECTED]" }
      else { data.substring(0, 3) + "[PROTECTED]" + data.substring(data.length() - 3, 3) }
    })
  ]
  
  // 应用脱敏规则
  let apply_masking = fn(data: String, data_type: SensitiveDataType) {
    let mut result = data
    for (rule_type, masking_fn) in data_masking_rules {
      match (rule_type, data_type) {
        (SensitiveDataType::PersonalIdentifier, SensitiveDataType::PersonalIdentifier) => {
          result = masking_fn(data)
        }
        (SensitiveDataType::FinancialInformation, SensitiveDataType::FinancialInformation) => {
          result = masking_fn(data)
        }
        (SensitiveDataType::AuthenticationCredentials, SensitiveDataType::AuthenticationCredentials) => {
          result = masking_fn(data)
        }
        (SensitiveDataType::HealthInformation, SensitiveDataType::HealthInformation) => {
          result = masking_fn(data)
        }
        (SensitiveDataType::Custom(custom_type), SensitiveDataType::Custom(rule_custom_type)) => {
          if custom_type == rule_custom_type {
            result = masking_fn(data)
          }
        }
        _ => {}
      }
    }
    result
  }
  
  // 测试个人身份信息脱敏
  let ssn = "123456789"
  let masked_ssn = apply_masking(ssn, SensitiveDataType::PersonalIdentifier)
  assert_eq(masked_ssn, "12*******89")
  
  let short_id = "123"
  let masked_short_id = apply_masking(short_id, SensitiveDataType::PersonalIdentifier)
  assert_eq(masked_short_id, "***")
  
  // 测试财务信息脱敏
  let credit_card = "1234567890123456"
  let masked_card = apply_masking(credit_card, SensitiveDataType::FinancialInformation)
  assert_eq(masked_card, "************3456")
  
  // 测试认证凭据脱敏
  let password = "supersecret123"
  let masked_password = apply_masking(password, SensitiveDataType::AuthenticationCredentials)
  assert_eq(masked_password, "[REDACTED]")
  
  // 测试健康信息脱敏
  let medical_record = "PATIENT_DIAGNOSIS_CODE_12345"
  let masked_record = apply_masking(medical_record, SensitiveDataType::HealthInformation)
  assert_eq(masked_record, "PAT[PROTECTED]345")
  
  // 测试自定义敏感数据类型
  let api_key = "sk-1234567890abcdef"
  let masked_api_key = apply_masking(api_key, SensitiveDataType::Custom("api_key"))
  assert_eq(masked_api_key, api_key)  // 没有匹配的规则，保持原样
  
  // 测试遥测数据中的敏感信息处理
  let telemetry_with_sensitive_data = {
    trace_id: "trace-123456",
    spans: [
      {
        span_id: "span-001",
        service: "auth-service",
        operation: "login",
        attributes: [
          ("user.id", "123456789"),
          ("user.email", "user@example.com"),
          ("user.ssn", "987654321"),
          ("auth.token", "bearer-token-12345")
        ]
      }
    ]
  }
  
  // 定义需要脱敏的属性列表
  let sensitive_attributes = [
    ("user.ssn", SensitiveDataType::PersonalIdentifier),
    ("auth.token", SensitiveDataType::AuthenticationCredentials),
    ("payment.card", SensitiveDataType::FinancialInformation)
  ]
  
  // 对遥测数据进行脱敏处理
  let sanitize_telemetry = fn(telemetry: {trace_id: String, spans: Array<{span_id: String, service: String, operation: String, attributes: Array<(String, String)}>}) {
    let sanitized_spans = []
    
    for span in telemetry.spans {
      let sanitized_attributes = []
      
      for attr in span.attributes {
        let is_sensitive = sensitive_attributes.any(fn(sensitive_attr) {
          sensitive_attr.0 == attr.0
        })
        
        if is_sensitive {
          let data_type = match sensitive_attributes.find(fn(sensitive_attr) {
            sensitive_attr.0 == attr.0
          }) {
            Some((_, dt)) => dt
            None => SensitiveDataType::Custom("unknown")
          }
          
          let masked_value = apply_masking(attr.1, data_type)
          sanitized_attributes = sanitized_attributes.push((attr.0, masked_value))
        } else {
          sanitized_attributes = sanitized_attributes.push(attr)
        }
      }
      
      sanitized_spans = sanitized_spans.push({
        span_id: span.span_id,
        service: span.service,
        operation: span.operation,
        attributes: sanitized_attributes
      })
    }
    
    {
      trace_id: telemetry.trace_id,
      spans: sanitized_spans
    }
  }
  
  let sanitized_telemetry = sanitize_telemetry(telemetry_with_sensitive_data)
  let sanitized_span = sanitized_telemetry.spans[0]
  
  // 验证敏感数据已被脱敏
  assert_eq(sanitized_span.attributes[0].0, "user.id")  // 非敏感，保持原样
  assert_eq(sanitized_span.attributes[0].1, "123456789")
  
  assert_eq(sanitized_span.attributes[2].0, "user.ssn")  // 敏感，已脱敏
  assert_eq(sanitized_span.attributes[2].1, "98*******21")
  
  assert_eq(sanitized_span.attributes[3].0, "auth.token")  // 敏感，已脱敏
  assert_eq(sanitized_span.attributes[3].1, "[REDACTED]")
}

// Test 5: 国际化和本地化支持
test "internationalization and localization support" {
  // 定义支持的语言环境
  let supported_locales = [
    "en-US",  // 英语(美国)
    "zh-CN",  // 中文(简体)
    "ja-JP",  // 日语
    "fr-FR",  // 法语
    "de-DE",  // 德语
    "es-ES"   // 西班牙语
  ]
  
  // 定义本地化字符串
  let localized_strings = {
    "en-US": {
      "telemetry.service.name": "Telemetry Service",
      "telemetry.service.description": "Collects and processes telemetry data",
      "span.operation.create": "Create",
      "span.operation.update": "Update",
      "span.operation.delete": "Delete",
      "metric.unit.milliseconds": "ms",
      "metric.unit.requests_per_second": "req/s",
      "error.timeout": "Request timed out",
      "error.connection_failed": "Connection failed"
    },
    "zh-CN": {
      "telemetry.service.name": "遥测服务",
      "telemetry.service.description": "收集和处理遥测数据",
      "span.operation.create": "创建",
      "span.operation.update": "更新",
      "span.operation.delete": "删除",
      "metric.unit.milliseconds": "毫秒",
      "metric.unit.requests_per_second": "请求/秒",
      "error.timeout": "请求超时",
      "error.connection_failed": "连接失败"
    },
    "ja-JP": {
      "telemetry.service.name": "テレメトリサービス",
      "telemetry.service.description": "テレメトリデータを収集・処理します",
      "span.operation.create": "作成",
      "span.operation.update": "更新",
      "span.operation.delete": "削除",
      "metric.unit.milliseconds": "ミリ秒",
      "metric.unit.requests_per_second": "リクエスト/秒",
      "error.timeout": "リクエストタイムアウト",
      "error.connection_failed": "接続失敗"
    }
  }
  
  // 获取本地化字符串的函数
  let get_localized_string = fn(locale: String, key: String) {
    match Map::get(localized_strings, locale) {
      Some(strings) => {
        match Map::get(strings, key) {
          Some(value) => value
          None => "[" + key + "]"  // 键不存在时返回键名
        }
      }
      None => {
        // 回退到英语
        match Map::get(localized_strings["en-US"], key) {
          Some(value) => value
          None => "[" + key + "]"
        }
      }
    }
  }
  
  // 测试英语本地化
  assert_eq(get_localized_string("en-US", "telemetry.service.name"), "Telemetry Service")
  assert_eq(get_localized_string("en-US", "span.operation.create"), "Create")
  assert_eq(get_localized_string("en-US", "metric.unit.milliseconds"), "ms")
  assert_eq(get_localized_string("en-US", "error.timeout"), "Request timed out")
  
  // 测试中文本地化
  assert_eq(get_localized_string("zh-CN", "telemetry.service.name"), "遥测服务")
  assert_eq(get_localized_string("zh-CN", "span.operation.create"), "创建")
  assert_eq(get_localized_string("zh-CN", "metric.unit.milliseconds"), "毫秒")
  assert_eq(get_localized_string("zh-CN", "error.timeout"), "请求超时")
  
  // 测试日语本地化
  assert_eq(get_localized_string("ja-JP", "telemetry.service.name"), "テレメトリサービス")
  assert_eq(get_localized_string("ja-JP", "span.operation.create"), "作成")
  assert_eq(get_localized_string("ja-JP", "metric.unit.milliseconds"), "ミリ秒")
  assert_eq(get_localized_string("ja-JP", "error.timeout"), "リクエストタイムアウト")
  
  // 测试回退机制
  assert_eq(get_localized_string("fr-FR", "telemetry.service.name"), "Telemetry Service")  // 回退到英语
  assert_eq(get_localized_string("de-DE", "span.operation.create"), "Create")  // 回退到英语
  
  // 测试不存在的键
  assert_eq(get_localized_string("zh-CN", "nonexistent.key"), "[nonexistent.key]")
  
  // 测试本地化数字和日期格式
  let format_number_for_locale = fn(number: Float, locale: String) {
    match locale {
      "en-US" => number.to_string()  // 简化处理
      "zh-CN" => number.to_string()
      "ja-JP" => number.to_string()
      "de-DE" => number.to_string().replace(".", ",")  // 德语使用逗号作为小数点
      "fr-FR" => number.to_string().replace(".", ",")  // 法语使用逗号作为小数点
      _ => number.to_string()
    }
  }
  
  let format_date_for_locale = fn(timestamp: Int, locale: String) {
    match locale {
      "en-US" => "01/01/2022"  // MM/DD/YYYY
      "zh-CN" => "2022/01/01"  // YYYY/MM/DD
      "ja-JP" => "2022/01/01"  // YYYY/MM/DD
      "de-DE" => "01.01.2022"  // DD.MM.YYYY
      "fr-FR" => "01/01/2022"  // DD/MM/YYYY
      _ => "2022-01-01"  // ISO格式
    }
  }
  
  // 测试数字格式化
  assert_eq(format_number_for_locale(1234.56, "en-US"), "1234.56")
  assert_eq(format_number_for_locale(1234.56, "de-DE"), "1234,56")
  assert_eq(format_number_for_locale(1234.56, "fr-FR"), "1234,56")
  
  // 测试日期格式化
  assert_eq(format_date_for_locale(1640995200, "en-US"), "01/01/2022")
  assert_eq(format_date_for_locale(1640995200, "zh-CN"), "2022/01/01")
  assert_eq(format_date_for_locale(1640995200, "de-DE"), "01.01.2022")
}

// Test 6: 高并发场景下的遥测数据处理
test "telemetry data processing under high concurrency" {
  // 定义并发工作负载模拟
  let concurrent_workload = {
    threads: 10,
    operations_per_thread: 100,
    span_creation_time: 5,  // ms
    metric_collection_time: 2,  // ms
    data_serialization_time: 3  // ms
  }
  
  // 模拟单个线程的操作
  let simulate_thread_operations = fn(thread_id: Int, operations: Int, span_time: Int, metric_time: Int, serialization_time: Int) {
    let mut spans = []
    let mut metrics = []
    let total_operations = operations * 3  // 每个操作包括创建span、收集metric、序列化
    
    for i in 0..operations {
      let operation_id = thread_id * 1000 + i
      let base_timestamp = 1640995200 + i
      
      // 创建span
      let span = {
        span_id: "span-" + operation_id.to_string(),
        thread_id: thread_id,
        operation: "concurrent_operation_" + i.to_string(),
        start_time: base_timestamp * 1000,
        end_time: (base_timestamp + span_time) * 1000,
        status: "ok"
      }
      spans = spans.push(span)
      
      // 收集metric
      let metric = {
        metric_id: "metric-" + operation_id.to_string(),
        thread_id: thread_id,
        name: "operation.duration",
        value: span_time as Float,
        unit: "ms",
        timestamp: base_timestamp * 1000
      }
      metrics = metrics.push(metric)
    }
    
    {
      thread_id,
      spans_created: spans.length(),
      metrics_collected: metrics.length(),
      total_operations,
      estimated_duration: total_operations * (span_time + metric_time + serialization_time)
    }
  }
  
  // 模拟所有线程的操作
  let simulate_concurrent_workload = fn(workload: {threads: Int, operations_per_thread: Int, span_creation_time: Int, metric_collection_time: Int, data_serialization_time: Int}) {
    let mut thread_results = []
    let mut total_spans = 0
    let mut total_metrics = 0
    let mut total_operations = 0
    
    for thread_id in 0..workload.threads {
      let result = simulate_thread_operations(
        thread_id,
        workload.operations_per_thread,
        workload.span_creation_time,
        workload.metric_collection_time,
        workload.data_serialization_time
      )
      
      thread_results = thread_results.push(result)
      total_spans = total_spans + result.spans_created
      total_metrics = total_metrics + result.metrics_collected
      total_operations = total_operations + result.total_operations
    }
    
    // 计算并发性能指标
    let avg_spans_per_thread = (total_spans as Float) / (workload.threads as Float)
    let avg_metrics_per_thread = (total_metrics as Float) / (workload.threads as Float)
    let avg_operations_per_thread = (total_operations as Float) / (workload.threads as Float)
    
    // 计算预期总执行时间（考虑并行执行）
    let max_thread_duration = thread_results.reduce(0, fn(max, result) {
      if result.estimated_duration > max { result.estimated_duration } else { max }
    })
    
    {
      total_threads: workload.threads,
      total_spans,
      total_metrics,
      total_operations,
      avg_spans_per_thread,
      avg_metrics_per_thread,
      avg_operations_per_thread,
      estimated_concurrent_duration: max_thread_duration,
      estimated_sequential_duration: total_operations * (workload.span_creation_time + workload.metric_collection_time + workload.data_serialization_time),
      thread_results
    }
  }
  
  // 执行并发工作负载模拟
  let workload_results = simulate_concurrent_workload(concurrent_workload)
  
  // 验证结果
  assert_eq(workload_results.total_threads, 10)
  assert_eq(workload_results.total_spans, 1000)  // 10 threads * 100 operations
  assert_eq(workload_results.total_metrics, 1000)
  assert_eq(workload_results.total_operations, 3000)  // 每个操作3个步骤
  
  assert_eq(workload_results.avg_spans_per_thread, 100.0)
  assert_eq(workload_results.avg_metrics_per_thread, 100.0)
  assert_eq(workload_results.avg_operations_per_thread, 300.0)
  
  // 验证并发执行的优势
  assert_true(workload_results.estimated_concurrent_duration < workload_results.estimated_sequential_duration)
  
  // 测试数据聚合和去重
  let aggregate_telemetry_data = fn(results: {thread_id: Int, spans_created: Int, metrics_collected: Int, total_operations: Int, estimated_duration: Int}) {
    let unique_span_ids = []
    let unique_metric_ids = []
    
    for thread_result in results.thread_results {
      // 模拟生成唯一ID
      for i in 0..thread_result.spans_created {
        let span_id = "span-" + (thread_result.thread_id * 1000 + i).to_string()
        if not(unique_span_ids.contains(span_id)) {
          unique_span_ids = unique_span_ids.push(span_id)
        }
      }
      
      for i in 0..thread_result.metrics_collected {
        let metric_id = "metric-" + (thread_result.thread_id * 1000 + i).to_string()
        if not(unique_metric_ids.contains(metric_id)) {
          unique_metric_ids = unique_metric_ids.push(metric_id)
        }
      }
    }
    
    {
      unique_span_count: unique_span_ids.length(),
      unique_metric_count: unique_metric_ids.length(),
      deduplication_ratio: (unique_span_ids.length() as Float) / (results.total_spans as Float)
    }
  }
  
  let aggregation_results = aggregate_telemetry_data(workload_results)
  assert_eq(aggregation_results.unique_span_count, 1000)
  assert_eq(aggregation_results.unique_metric_count, 1000)
  assert_eq(aggregation_results.deduplication_ratio, 1.0)  // 没有重复
}

// Test 7: 自适应采样策略测试
test "adaptive sampling strategy tests" {
  // 定义采样策略配置
  enum SamplingStrategy {
    FixedRate(Float)  // 固定采样率
    Adaptive(String)  // 自适应采样
    Dynamic(String)   // 动态采样
  }
  
  let sampling_configurations = [
    ("low-traffic", SamplingStrategy::FixedRate(1.0)),  // 低流量：100%采样
    ("medium-traffic", SamplingStrategy::Adaptive("error-based")),  // 中流量：基于错误的自适应采样
    ("high-traffic", SamplingStrategy::Dynamic("throughput-based"))  // 高流量：基于吞吐量的动态采样
  ]
  
  // 模拟流量数据
  let traffic_data = [
    {
      timestamp: 1640995200,
      endpoint: "/api/v1/users",
      request_count: 50,
      error_count: 2,
      avg_response_time: 120.0
    },
    {
      timestamp: 1640995260,
      endpoint: "/api/v1/users",
      request_count: 150,
      error_count: 15,
      avg_response_time: 350.0
    },
    {
      timestamp: 1640995320,
      endpoint: "/api/v1/users",
      request_count: 500,
      error_count: 10,
      avg_response_time: 180.0
    }
  ]
  
  // 实现固定采样率策略
  let fixed_rate_sampling = fn(trace_id: String, sampling_rate: Float) {
    // 简单哈希函数
    let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
    let normalized = (hash % 100) as Float / 100.0
    normalized <= sampling_rate
  }
  
  // 实现基于错误的自适应采样
  let error_adaptive_sampling = fn(trace_id: String, error_rate: Float, base_rate: Float) {
    // 错误率越高，采样率越高
    let adaptive_rate = if error_rate > 0.1 {
      1.0  // 错误率超过10%，100%采样
    } else if error_rate > 0.05 {
      0.5  // 错误率超过5%，50%采样
    } else {
      base_rate  // 使用基础采样率
    }
    
    fixed_rate_sampling(trace_id, adaptive_rate)
  }
  
  // 实现基于吞吐量的动态采样
  let throughput_dynamic_sampling = fn(trace_id: String, request_rate: Float, target_rate: Float) {
    // 根据当前请求率调整采样率
    let dynamic_rate = if request_rate > target_rate * 2.0 {
      0.01  // 请求率过高，降低采样率到1%
    } else if request_rate > target_rate {
      0.1  // 请求率较高，采样率10%
    } else {
      0.5  // 请求率正常，采样率50%
    }
    
    fixed_rate_sampling(trace_id, dynamic_rate)
  }
  
  // 根据配置应用采样策略
  let apply_sampling_strategy = fn(config: (String, SamplingStrategy), trace_id: String, traffic: {timestamp: Int, endpoint: String, request_count: Int, error_count: Int, avg_response_time: Float}) {
    let error_rate = (traffic.error_count as Float) / (traffic.request_count as Float)
    let request_rate = traffic.request_count as Float / 60.0  // 每秒请求数
    
    match config.1 {
      SamplingStrategy::FixedRate(rate) => {
        fixed_rate_sampling(trace_id, rate)
      }
      SamplingStrategy::Adaptive(strategy) => {
        if strategy == "error-based" {
          error_adaptive_sampling(trace_id, error_rate, 0.1)  // 基础采样率10%
        } else {
          false  // 未知策略
        }
      }
      SamplingStrategy::Dynamic(strategy) => {
        if strategy == "throughput-based" {
          throughput_dynamic_sampling(trace_id, request_rate, 100.0)  // 目标每秒100个请求
        } else {
          false  // 未知策略
        }
      }
    }
  }
  
  // 测试不同流量场景下的采样
  let test_scenarios = [
    {
      traffic_profile: "low-traffic",
      traffic_data: traffic_data[0],
      test_trace_ids: ["trace-low-1", "trace-low-2", "trace-low-3"]
    },
    {
      traffic_profile: "medium-traffic",
      traffic_data: traffic_data[1],
      test_trace_ids: ["trace-medium-1", "trace-medium-2", "trace-medium-3"]
    },
    {
      traffic_profile: "high-traffic",
      traffic_data: traffic_data[2],
      test_trace_ids: ["trace-high-1", "trace-high-2", "trace-high-3"]
    }
  ]
  
  // 测试采样策略
  for scenario in test_scenarios {
    let config = match sampling_configurations.find(fn(c) { c.0 == scenario.traffic_profile }) {
      Some(c) => c
      None => ("default", SamplingStrategy::FixedRate(0.1))
    }
    
    let mut sampled_count = 0
    
    for trace_id in scenario.test_trace_ids {
      let should_sample = apply_sampling_strategy(config, trace_id, scenario.traffic_data)
      if should_sample {
        sampled_count = sampled_count + 1
      }
    }
    
    // 验证采样结果
    match config.1 {
      SamplingStrategy::FixedRate(rate) => {
        if rate == 1.0 {
          assert_eq(sampled_count, 3)  // 100%采样率，所有trace都应该被采样
        }
      }
      SamplingStrategy::Adaptive(_) => {
        // 中等流量场景，错误率较高（15/150=10%），应该有较高的采样率
        assert_true(sampled_count >= 1)
      }
      SamplingStrategy::Dynamic(_) => {
        // 高流量场景，采样率应该较低
        assert_true(sampled_count <= 2)
      }
    }
  }
  
  // 测试采样策略切换
  let switch_sampling_strategy = fn(current_strategy: SamplingStrategy, traffic_conditions: {request_rate: Float, error_rate: Float, response_time: Float}) {
    match current_strategy {
      SamplingStrategy::FixedRate(_) => {
        if traffic_conditions.request_rate > 200.0 {
          SamplingStrategy::Dynamic("throughput-based")
        } else if traffic_conditions.error_rate > 0.05 {
          SamplingStrategy::Adaptive("error-based")
        } else {
          current_strategy
        }
      }
      SamplingStrategy::Adaptive(_) => {
        if traffic_conditions.request_rate > 500.0 {
          SamplingStrategy::Dynamic("throughput-based")
        } else if traffic_conditions.error_rate < 0.01 {
          SamplingStrategy::FixedRate(0.1)
        } else {
          current_strategy
        }
      }
      SamplingStrategy::Dynamic(_) => {
        if traffic_conditions.request_rate < 100.0 and traffic_conditions.error_rate < 0.02 {
          SamplingStrategy::FixedRate(0.5)
        } else {
          current_strategy
        }
      }
    }
  }
  
  // 测试策略切换逻辑
  let initial_strategy = SamplingStrategy::FixedRate(0.1)
  let high_traffic_conditions = { request_rate: 300.0, error_rate: 0.02, response_time: 150.0 }
  let high_error_conditions = { request_rate: 100.0, error_rate: 0.08, response_time: 300.0 }
  
  let switched_strategy_1 = switch_sampling_strategy(initial_strategy, high_traffic_conditions)
  match switched_strategy_1 {
    SamplingStrategy::Dynamic(strategy) => assert_eq(strategy, "throughput-based")
    _ => assert_true(false)
  }
  
  let switched_strategy_2 = switch_sampling_strategy(initial_strategy, high_error_conditions)
  match switched_strategy_2 {
    SamplingStrategy::Adaptive(strategy) => assert_eq(strategy, "error-based")
    _ => assert_true(false)
  }
}

// Test 8: 遥测数据压缩和传输优化
test "telemetry data compression and transmission optimization" {
  // 定义遥测数据结构
  let telemetry_batch = {
    batch_id: "batch-12345",
    timestamp: 1640995200,
    spans: [
      {
        trace_id: "trace-001",
        span_id: "span-001",
        parent_span_id: None,
        operation_name: "http.request",
        service_name: "api-gateway",
        start_time: 1640995200000,
        end_time: 1640995200500,
        status: "ok",
        attributes: [
          ("http.method", "GET"),
          ("http.url", "/api/v1/users"),
          ("http.status_code", "200"),
          ("user.id", "12345")
        ]
      },
      {
        trace_id: "trace-001",
        span_id: "span-002",
        parent_span_id: Some("span-001"),
        operation_name: "db.query",
        service_name: "user-service",
        start_time: 1640995200100,
        end_time: 1640995200300,
        status: "ok",
        attributes: [
          ("db.statement", "SELECT * FROM users WHERE id = ?"),
          ("db.type", "postgresql"),
          ("db.connection_pool", "primary")
        ]
      },
      {
        trace_id: "trace-002",
        span_id: "span-003",
        parent_span_id: None,
        operation_name: "http.request",
        service_name: "api-gateway",
        start_time: 1640995201000,
        end_time: 1640995201700,
        status: "error",
        attributes: [
          ("http.method", "POST"),
          ("http.url", "/api/v1/orders"),
          ("http.status_code", "500"),
          ("error.message", "Database connection timeout")
        ]
      }
    ],
    metrics: [
      {
        name: "http.server.request.duration",
        value: 500.0,
        unit: "ms",
        attributes: [
          ("http.method", "GET"),
          ("http.status_code", "200")
        ]
      },
      {
        name: "db.client.operation.duration",
        value: 200.0,
        unit: "ms",
        attributes: [
          ("db.type", "postgresql"),
          ("db.operation", "SELECT")
        ]
      },
      {
        name: "http.server.request.duration",
        value: 700.0,
        unit: "ms",
        attributes: [
          ("http.method", "POST"),
          ("http.status_code", "500")
        ]
      }
    ]
  }
  
  // 计算原始数据大小（简化计算）
  let calculate_data_size = fn(batch: {batch_id: String, timestamp: Int, spans: Array[Any], metrics: Array[Any]}) {
    let batch_size = batch.batch_id.length() + batch.timestamp.to_string().length()
    let spans_size = batch.spans.length() * 200  // 假设每个span平均200字节
    let metrics_size = batch.metrics.length() * 100  // 假设每个metric平均100字节
    batch_size + spans_size + metrics_size
  }
  
  let original_size = calculate_data_size(telemetry_batch)
  
  // 模拟数据压缩
  let compress_data = fn(data: Any, compression_level: Int) {
    // 简化压缩模拟：根据压缩级别返回不同的压缩比
    match compression_level {
      1 => 0.8  // 20%压缩
      2 => 0.6  // 40%压缩
      3 => 0.4  // 60%压缩
      4 => 0.3  // 70%压缩
      5 => 0.2  // 80%压缩
      _ => 0.5  // 默认50%压缩
    }
  }
  
  // 测试不同压缩级别
  let compression_levels = [1, 2, 3, 4, 5]
  let compression_results = []
  
  for level in compression_levels {
    let compression_ratio = compress_data(telemetry_batch, level)
    let compressed_size = (original_size as Float * compression_ratio) as Int
    let space_saved = original_size - compressed_size
    
    compression_results = compression_results.push({
      compression_level: level,
      compression_ratio,
      original_size,
      compressed_size,
      space_saved,
      space_saved_percent: ((space_saved as Float) / (original_size as Float)) * 100.0
    })
  }
  
  // 验证压缩结果
  assert_eq(compression_results.length(), 5)
  assert_eq(compression_results[0].compression_ratio, 0.8)
  assert_eq(compression_results[4].compression_ratio, 0.2)
  
  // 验证压缩级别越高，压缩比越小（压缩率越高）
  for i in 0..(compression_results.length() - 1) {
    assert_true(compression_results[i].compression_ratio > compression_results[i + 1].compression_ratio)
    assert_true(compression_results[i].space_saved < compression_results[i + 1].space_saved)
  }
  
  // 模拟数据传输优化
  let optimize_for_transmission = fn(batch: {batch_id: String, timestamp: Int, spans: Array[Any], metrics: Array[Any]}, network_conditions: {bandwidth: Int, latency: Int, packet_loss: Float}) {
    // 根据网络条件选择优化策略
    let compression_level = if network_conditions.bandwidth < 1000 {  // 带宽小于1MB/s
      5  // 最高压缩级别
    } else if network_conditions.bandwidth < 5000 {  // 带宽小于5MB/s
      3  // 中等压缩级别
    } else {
      1  // 最低压缩级别
    }
    
    let compression_ratio = compress_data(batch, compression_level)
    let compressed_size = (original_size as Float * compression_ratio) as Int
    
    // 计算传输时间（简化）
    let transmission_time = (compressed_size as Float) / (network_conditions.bandwidth as Float) + (network_conditions.latency as Float) / 1000.0
    
    // 考虑丢包重传
    let adjusted_transmission_time = if network_conditions.packet_loss > 0.0 {
      transmission_time * (1.0 + network_conditions.packet_loss)
    } else {
      transmission_time
    }
    
    {
      compression_level,
      compression_ratio,
      compressed_size,
      estimated_transmission_time: adjusted_transmission_time,
      bandwidth_utilization: (compressed_size as Float) / (network_conditions.bandwidth as Float)
    }
  }
  
  // 测试不同网络条件下的传输优化
  let network_conditions = [
    { bandwidth: 500, latency: 100, packet_loss: 0.01 },  // 差网络
    { bandwidth: 2000, latency: 50, packet_loss: 0.005 },  // 一般网络
    { bandwidth: 10000, latency: 10, packet_loss: 0.001 }  // 好网络
  ]
  
  let transmission_results = []
  for conditions in network_conditions {
    let result = optimize_for_transmission(telemetry_batch, conditions)
    transmission_results = transmission_results.push(result)
  }
  
  // 验证传输优化结果
  assert_eq(transmission_results.length(), 3)
  
  // 差网络应该使用最高压缩级别
  assert_eq(transmission_results[0].compression_level, 5)
  assert_eq(transmission_results[0].compression_ratio, 0.2)
  
  // 一般网络应该使用中等压缩级别
  assert_eq(transmission_results[1].compression_level, 3)
  assert_eq(transmission_results[1].compression_ratio, 0.4)
  
  // 好网络应该使用最低压缩级别
  assert_eq(transmission_results[2].compression_level, 1)
  assert_eq(transmission_results[2].compression_ratio, 0.8)
  
  // 验证传输时间与网络条件的关系
  assert_true(transmission_results[0].estimated_transmission_time > transmission_results[1].estimated_transmission_time)
  assert_true(transmission_results[1].estimated_transmission_time > transmission_results[2].estimated_transmission_time)
}

// Test 9: 遥测数据质量验证和清洗
test "telemetry data quality validation and cleansing" {
  // 定义数据质量规则
  let data_quality_rules = {
    trace_id: {
      required: true,
      format: "trace-[a-z0-9]{12}",
      min_length: 16,
      max_length: 16
    },
    span_id: {
      required: true,
      format: "span-[a-z0-9]{12}",
      min_length: 16,
      max_length: 16
    },
    service_name: {
      required: true,
      min_length: 1,
      max_length: 50,
      allowed_characters: "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_."
    },
    operation_name: {
      required: true,
      min_length: 1,
      max_length: 100
    },
    start_time: {
      required: true,
      type: "timestamp",
      min_value: 0
    },
    end_time: {
      required: true,
      type: "timestamp",
      min_value: 0
    },
    status: {
      required: true,
      allowed_values: ["ok", "error", "timeout", "cancelled"]
    }
  }
  
  // 定义验证函数
  let validate_field = fn(field_name: String, value: String, rules: Any) {
    // 简化验证逻辑
    match field_name {
      "trace_id" => {
        if value.length() < 16 || value.length() > 16 {
          (false, "Invalid trace_id length")
        } else if not(value.starts_with("trace-")) {
          (false, "Invalid trace_id format")
        } else {
          (true, "")
        }
      }
      "span_id" => {
        if value.length() < 16 || value.length() > 16 {
          (false, "Invalid span_id length")
        } else if not(value.starts_with("span-")) {
          (false, "Invalid span_id format")
        } else {
          (true, "")
        }
      }
      "service_name" => {
        if value.length() == 0 {
          (false, "Service name is required")
        } else if value.length() > 50 {
          (false, "Service name too long")
        } else {
          (true, "")
        }
      }
      "operation_name" => {
        if value.length() == 0 {
          (false, "Operation name is required")
        } else if value.length() > 100 {
          (false, "Operation name too long")
        } else {
          (true, "")
        }
      }
      "status" => {
        let allowed_values = ["ok", "error", "timeout", "cancelled"]
        if allowed_values.contains(value) {
          (true, "")
        } else {
          (false, "Invalid status value")
        }
      }
      _ => (true, "")  // 未知字段，跳过验证
    }
  }
  
  let validate_timestamp = fn(timestamp: Int, field_name: String) {
    if timestamp < 0 {
      (false, field_name + " must be positive")
    } else if timestamp > 2147483647 {  // 32位整数最大值
      (false, field_name + " exceeds maximum value")
    } else {
      (true, "")
    }
  }
  
  // 定义测试数据（包含一些质量问题）
  let test_spans = [
    {
      trace_id: "trace-abc123def456",  // 有效
      span_id: "span-1234567890ab",   // 有效
      service_name: "auth-service",    // 有效
      operation_name: "authenticate",  // 有效
      start_time: 1640995200000,       // 有效
      end_time: 1640995202500,         // 有效
      status: "ok"                     // 有效
    },
    {
      trace_id: "invalid",             // 无效：格式错误
      span_id: "span-1234567890ab",   // 有效
      service_name: "",                 // 无效：空字符串
      operation_name: "authenticate",  // 有效
      start_time: 1640995200000,       // 有效
      end_time: 1640995202500,         // 有效
      status: "ok"                     // 有效
    },
    {
      trace_id: "trace-abc123def456",  // 有效
      span_id: "invalid",              // 无效：格式错误
      service_name: "user-service",    // 有效
      operation_name: "get_user",      // 有效
      start_time: -1000,               // 无效：负数
      end_time: 1640995202500,         // 有效
      status: "unknown"                // 无效：不允许的值
    },
    {
      trace_id: "trace-abc123def456",  // 有效
      span_id: "span-1234567890ab",   // 有效
      service_name: "a".repeat(60),    // 无效：太长
      operation_name: "get_user",      // 有效
      start_time: 1640995200000,       // 有效
      end_time: 1640995199000,         // 无效：end_time < start_time
      status: "error"                  // 有效
    }
  ]
  
  // 验证和清洗数据
  let validate_and_cleanse_span = fn(span: {trace_id: String, span_id: String, service_name: String, operation_name: String, start_time: Int, end_time: Int, status: String}) {
    let mut validation_errors = []
    let mut cleansed_span = span
    
    // 验证trace_id
    let (trace_id_valid, trace_id_error) = validate_field("trace_id", span.trace_id, data_quality_rules.trace_id)
    if not(trace_id_valid) {
      validation_errors = validation_errors.push(("trace_id", trace_id_error))
      // 清洗：生成有效的trace_id
      cleansed_span = { cleansed_span | trace_id: "trace-" + "0".repeat(12) }
    }
    
    // 验证span_id
    let (span_id_valid, span_id_error) = validate_field("span_id", span.span_id, data_quality_rules.span_id)
    if not(span_id_valid) {
      validation_errors = validation_errors.push(("span_id", span_id_error))
      // 清洗：生成有效的span_id
      cleansed_span = { cleansed_span | span_id: "span-" + "0".repeat(12) }
    }
    
    // 验证service_name
    let (service_name_valid, service_name_error) = validate_field("service_name", span.service_name, data_quality_rules.service_name)
    if not(service_name_valid) {
      validation_errors = validation_errors.push(("service_name", service_name_error))
      // 清洗：使用默认服务名
      cleansed_span = { cleansed_span | service_name: "unknown-service" }
    }
    
    // 验证operation_name
    let (operation_name_valid, operation_name_error) = validate_field("operation_name", span.operation_name, data_quality_rules.operation_name)
    if not(operation_name_valid) {
      validation_errors = validation_errors.push(("operation_name", operation_name_error))
      // 清洗：使用默认操作名
      cleansed_span = { cleansed_span | operation_name: "unknown-operation" }
    }
    
    // 验证start_time
    let (start_time_valid, start_time_error) = validate_timestamp(span.start_time, "start_time")
    if not(start_time_valid) {
      validation_errors = validation_errors.push(("start_time", start_time_error))
      // 清洗：使用当前时间
      cleansed_span = { cleansed_span | start_time: 1640995200000 }
    }
    
    // 验证end_time
    let (end_time_valid, end_time_error) = validate_timestamp(span.end_time, "end_time")
    if not(end_time_valid) {
      validation_errors = validation_errors.push(("end_time", end_time_error))
      // 清洗：使用start_time + 1000
      cleansed_span = { cleansed_span | end_time: cleansed_span.start_time + 1000 }
    }
    
    // 验证时间顺序
    if cleansed_span.end_time < cleansed_span.start_time {
      validation_errors = validation_errors.push(("time_order", "end_time must be greater than or equal to start_time"))
      // 清洗：设置end_time = start_time + 1000
      cleansed_span = { cleansed_span | end_time: cleansed_span.start_time + 1000 }
    }
    
    // 验证status
    let (status_valid, status_error) = validate_field("status", span.status, data_quality_rules.status)
    if not(status_valid) {
      validation_errors = validation_errors.push(("status", status_error))
      // 清洗：使用默认状态
      cleansed_span = { cleansed_span | status: "unknown" }
    }
    
    {
      original_span: span,
      cleansed_span,
      validation_errors,
      is_valid: validation_errors.length() == 0
    }
  }
  
  // 验证所有测试span
  let validation_results = []
  for span in test_spans {
    let result = validate_and_cleanse_span(span)
    validation_results = validation_results.push(result)
  }
  
  // 验证结果
  assert_eq(validation_results.length(), 4)
  
  // 第一个span应该完全有效
  assert_true(validation_results[0].is_valid)
  assert_eq(validation_results[0].validation_errors.length(), 0)
  assert_eq(validation_results[0].cleansed_span.trace_id, "trace-abc123def456")
  
  // 第二个span应该有多个错误
  assert_false(validation_results[1].is_valid)
  assert_eq(validation_results[1].validation_errors.length(), 2)
  assert_eq(validation_results[1].cleansed_span.trace_id, "trace-000000000000")
  assert_eq(validation_results[1].cleansed_span.service_name, "unknown-service")
  
  // 第三个span应该有多个错误
  assert_false(validation_results[2].is_valid)
  assert_eq(validation_results[2].validation_errors.length(), 3)
  assert_eq(validation_results[2].cleansed_span.span_id, "span-000000000000")
  assert_eq(validation_results[2].cleansed_span.start_time, 1640995200000)
  assert_eq(validation_results[2].cleansed_span.status, "unknown")
  
  // 第四个span应该有多个错误
  assert_false(validation_results[3].is_valid)
  assert_eq(validation_results[3].validation_errors.length(), 3)
  assert_eq(validation_results[3].cleansed_span.service_name, "unknown-service")
  assert_eq(validation_results[3].cleansed_span.end_time, validation_results[3].cleansed_span.start_time + 1000)
  
  // 计算数据质量指标
  let total_spans = validation_results.length()
  let valid_spans = validation_results.filter(fn(r) { r.is_valid }).length()
  let invalid_spans = total_spans - valid_spans
  let total_errors = validation_results.reduce(0, fn(acc, r) { acc + r.validation_errors.length() })
  
  assert_eq(total_spans, 4)
  assert_eq(valid_spans, 1)
  assert_eq(invalid_spans, 3)
  assert_eq(total_errors, 8)
  
  let quality_score = (valid_spans as Float) / (total_spans as Float) * 100.0
  assert_eq(quality_score, 25.0)  // 1/4 * 100
}