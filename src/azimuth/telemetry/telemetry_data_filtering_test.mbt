// 遥测数据过滤和清洗测试用例

test "telemetry_data_noise_filtering" {
  // 测试遥测数据噪声过滤
  
  let raw_telemetry_data = [
    ("cpu_usage", 75.5, "service:api,env:production"),
    ("cpu_usage", 75.6, "service:api,env:production"),
    ("cpu_usage", 75.4, "service:api,env:production"),
    ("cpu_usage", 95.2, "service:api,env:production"), // 异常值
    ("cpu_usage", 75.7, "service:api,env:production"),
    ("cpu_usage", 75.3, "service:api,env:production"),
    ("cpu_usage", 25.1, "service:api,env:production"), // 异常值
    ("cpu_usage", 75.8, "service:api,env:production"),
    ("cpu_usage", 75.2, "service:api,env:production"),
    ("cpu_usage", 75.9, "service:api,env:production")
  ]
  
  // 计算平均值和标准差
  let mut sum = 0.0
  let mut i = 0
  while i < raw_telemetry_data.length() {
    sum = sum + raw_telemetry_data[i].1
    i = i + 1
  }
  let mean = sum / raw_telemetry_data.length().to_double()
  
  // 计算标准差
  let mut variance_sum = 0.0
  i = 0
  while i < raw_telemetry_data.length() {
    let diff = raw_telemetry_data[i].1 - mean
    variance_sum = variance_sum + diff * diff
    i = i + 1
  }
  let standard_deviation = (variance_sum / raw_telemetry_data.length().to_double()).sqrt()
  
  // 过滤噪声：移除超过2个标准差的值
  let noise_threshold = 2.0 * standard_deviation
  let mut filtered_data = []
  i = 0
  while i < raw_telemetry_data.length() {
    let value = raw_telemetry_data[i].1
    let deviation = (value - mean).abs()
    if deviation <= noise_threshold {
      filtered_data.push(raw_telemetry_data[i])
    }
    i = i + 1
  }
  
  // 验证噪声过滤效果
  assert_eq(filtered_data.length() < raw_telemetry_data.length(), true)
  assert_eq(filtered_data.length() > 0, true)
  
  // 验证过滤后的数据更稳定
  let mut filtered_sum = 0.0
  i = 0
  while i < filtered_data.length() {
    filtered_sum = filtered_sum + filtered_data[i].1
    i = i + 1
  }
  let filtered_mean = filtered_sum / filtered_data.length().to_double()
  
  // 过滤后的平均值应该更接近正常值
  assert_eq(filtered_mean > 70.0, true)
  assert_eq(filtered_mean < 80.0, true)
  
  // 验证异常值被移除
  let mut j = 0
  while j < filtered_data.length() {
    assert_eq(filtered_data[j].1 > 50.0, true)
    assert_eq(filtered_data[j].1 < 90.0, true)
    j = j + 1
  }
}

test "telemetry_data_duplicate_removal" {
  // 测试重复数据移除
  
  let telemetry_events = [
    ("trace_001", "span_001", "api_call", "success", 1640995200L),
    ("trace_001", "span_001", "api_call", "success", 1640995200L), // 重复
    ("trace_002", "span_002", "db_query", "success", 1640995201L),
    ("trace_003", "span_003", "cache_lookup", "hit", 1640995202L),
    ("trace_002", "span_002", "db_query", "success", 1640995201L), // 重复
    ("trace_004", "span_004", "auth_check", "success", 1640995203L),
    ("trace_005", "span_005", "data_processing", "success", 1640995204L),
    ("trace_003", "span_003", "cache_lookup", "hit", 1640995202L), // 重复
    ("trace_006", "span_006", "notification", "sent", 1640995205L),
    ("trace_007", "span_007", "cleanup", "success", 1640995206L)
  ]
  
  // 移除重复事件
  let mut unique_events = []
  let mut i = 0
  while i < telemetry_events.length() {
    let event = telemetry_events[i]
    let mut is_duplicate = false
    let mut j = 0
    while j < unique_events.length() {
      if unique_events[j] == event {
        is_duplicate = true
        break
      }
      j = j + 1
    }
    if not is_duplicate {
      unique_events.push(event)
    }
    i = i + 1
  }
  
  // 验证重复移除效果
  assert_eq(unique_events.length(), 7) // 应该有7个唯一事件
  assert_eq(unique_events.length() < telemetry_events.length(), true)
  
  // 验证数据完整性
  let mut j = 0
  while j < unique_events.length() {
    let event = unique_events[j]
    assert_eq(event.0.has_prefix("trace_"), true)
    assert_eq(event.1.has_prefix("span_"), true)
    assert_eq(event.2.length() > 0, true)
    j = j + 1
  }
  
  // 验证特定事件存在
  let mut found_trace_001 = false
  let mut found_trace_004 = false
  j = 0
  while j < unique_events.length() {
    if unique_events[j].0 == "trace_001" {
      found_trace_001 = true
    }
    if unique_events[j].0 == "trace_004" {
      found_trace_004 = true
    }
    j = j + 1
  }
  assert_eq(found_trace_001, true)
  assert_eq(found_trace_004, true)
}

test "telemetry_data_outlier_detection" {
  // 测试异常值检测和处理
  
  let performance_metrics = [
    ("response_time", 120.5),
    ("response_time", 125.3),
    ("response_time", 118.7),
    ("response_time", 122.1),
    ("response_time", 119.8),
    ("response_time", 450.2), // 异常值
    ("response_time", 121.4),
    ("response_time", 123.6),
    ("response_time", 117.9),
    ("response_time", 124.2),
    ("response_time", 15.3),  // 异常值
    ("response_time", 120.8),
    ("response_time", 126.1),
    ("response_time", 119.3),
    ("response_time", 122.7)
  ]
  
  // 使用IQR方法检测异常值
  let mut values = []
  let mut i = 0
  while i < performance_metrics.length() {
    values.push(performance_metrics[i].1)
    i = i + 1
  }
  
  // 简单排序（冒泡排序）
  let mut j = 0
  while j < values.length() - 1 {
    let mut k = 0
    while k < values.length() - j - 1 {
      if values[k] > values[k + 1] {
        let temp = values[k]
        values[k] = values[k + 1]
        values[k + 1] = temp
      }
      k = k + 1
    }
    j = j + 1
  }
  
  // 计算四分位数
  let q1_index = values.length() / 4
  let q3_index = (values.length() * 3) / 4
  let q1 = values[q1_index]
  let q3 = values[q3_index]
  let iqr = q3 - q1
  let outlier_threshold = 1.5 * iqr
  
  // 检测异常值
  let mut outliers = []
  let mut normal_values = []
  i = 0
  while i < performance_metrics.length() {
    let value = performance_metrics[i].1
    if value < (q1 - outlier_threshold) or value > (q3 + outlier_threshold) {
      outliers.push(performance_metrics[i])
    } else {
      normal_values.push(performance_metrics[i])
    }
    i = i + 1
  }
  
  // 验证异常值检测
  assert_eq(outliers.length() > 0, true)
  assert_eq(normal_values.length() > 0, true)
  assert_eq(normal_values.length() + outliers.length(), performance_metrics.length())
  
  // 验证检测到的异常值
  let mut found_high_outlier = false
  let mut found_low_outlier = false
  i = 0
  while i < outliers.length() {
    if outliers[i].1 > 200.0 {
      found_high_outlier = true
    }
    if outliers[i].1 < 50.0 {
      found_low_outlier = true
    }
    i = i + 1
  }
  assert_eq(found_high_outlier, true)
  assert_eq(found_low_outlier, true)
  
  // 验证正常值范围
  i = 0
  while i < normal_values.length() {
    assert_eq(normal_values[i].1 > 100.0, true)
    assert_eq(normal_values[i].1 < 150.0, true)
    i = i + 1
  }
}

test "telemetry_data_format_standardization" {
  // 测试数据格式标准化
  
  let unstandardized_data = [
    ("service_name", "payment-service"),
    ("service-name", "auth-service"),
    ("serviceName", "user-service"),
    ("SERVICE_NAME", "order-service"),
    ("service_name", "inventory-service"),
    ("timestamp", "1640995200"),
    ("Timestamp", "1640995201"),
    ("timeStamp", "1640995202"),
    ("status_code", "200"),
    ("statusCode", "201"),
    ("status-code", "400"),
    ("environment", "production"),
    ("ENVIRONMENT", "staging"),
    ("env", "development")
  ]
  
  // 标准化规则
  fn standardize_key(key : String) -> String {
    let mut standardized = key.to_lower()
    standardized = standardized.replace("-", "_")
    return standardized
  }
  
  fn standardize_value(key : String, value : String) -> String {
    match key {
      "service_name" => value.replace("-", "_").to_lower()
      "timestamp" => value
      "status_code" => value
      "environment" => value.to_lower()
      _ => value
    }
  }
  
  // 应用标准化
  let mut standardized_data = []
  let mut i = 0
  while i < unstandardized_data.length() {
    let original_key = unstandardized_data[i].0
    let original_value = unstandardized_data[i].1
    
    let standardized_key = standardize_key(original_key)
    let standardized_value = standardize_value(standardized_key, original_value)
    
    standardized_data.push((standardized_key, standardized_value))
    i = i + 1
  }
  
  // 验证标准化效果
  let mut service_name_count = 0
  let mut timestamp_count = 0
  let mut status_code_count = 0
  let mut environment_count = 0
  
  let mut j = 0
  while j < standardized_data.length() {
    match standardized_data[j].0 {
      "service_name" => {
        service_name_count = service_name_count + 1
        assert_eq(standardized_data[j].1.contains("_"), true) // 应该包含下划线
      }
      "timestamp" => {
        timestamp_count = timestamp_count + 1
        assert_eq(standardized_data[j].1.length(), 10) // 时间戳长度应该一致
      }
      "status_code" => {
        status_code_count = status_code_count + 1
        assert_eq(standardized_data[j].1.is_numeric(), true)
      }
      "environment" => {
        environment_count = environment_count + 1
        assert_eq(standardized_data[j].1.to_lower() == standardized_data[j].1, true) // 应该是小写
      }
      _ => {}
    }
    j = j + 1
  }
  
  // 验证标准化后的统计
  assert_eq(service_name_count, 5)
  assert_eq(timestamp_count, 3)
  assert_eq(status_code_count, 3)
  assert_eq(environment_count, 3)
}

test "telemetry_data_validation_rules" {
  // 测试数据验证规则
  
  let telemetry_records = [
    ("trace_001", "span_001", "api_call", 1640995200L, 200, 150.5),
    ("trace_002", "span_002", "db_query", 1640995201L, 200, 25.3),
    ("trace_003", "span_003", "cache_lookup", 1640995202L, 200, 2.1),
    ("", "span_004", "auth_check", 1640995203L, 200, 5.8), // 无效：空的trace_id
    ("trace_005", "", "data_processing", 1640995204L, 200, 120.7), // 无效：空的span_id
    ("trace_006", "span_006", "", 1640995205L, 200, 8.9), // 无效：空的操作名
    ("trace_007", "span_007", "notification", 0L, 200, 3.2), // 无效：无效的时间戳
    ("trace_008", "span_008", "cleanup", 1640995206L, 99, 4.5), // 无效：无效的状态码
    ("trace_009", "span_009", "health_check", 1640995207L, 200, -1.0), // 无效：负的持续时间
    ("trace_010", "span_010", "metrics", 1640995208L, 200, 350.2) // 有效
  ]
  
  // 验证规则
  fn validate_record(record : (String, String, String, Int, Int, Double)) -> Bool {
    let trace_id = record.0
    let span_id = record.1
    let operation = record.2
    let timestamp = record.3
    let status_code = record.4
    let duration = record.5
    
    // 验证规则
    if trace_id.length() == 0 {
      return false
    }
    if span_id.length() == 0 {
      return false
    }
    if operation.length() == 0 {
      return false
    }
    if timestamp <= 0L {
      return false
    }
    if status_code < 100 or status_code > 599 {
      return false
    }
    if duration < 0.0 {
      return false
    }
    
    return true
  }
  
  // 应用验证规则
  let mut valid_records = []
  let mut invalid_records = []
  let mut i = 0
  while i < telemetry_records.length() {
    let record = telemetry_records[i]
    if validate_record(record) {
      valid_records.push(record)
    } else {
      invalid_records.push(record)
    }
    i = i + 1
  }
  
  // 验证过滤效果
  assert_eq(valid_records.length(), 5) // 应该有5个有效记录
  assert_eq(invalid_records.length(), 5) // 应该有5个无效记录
  assert_eq(valid_records.length() + invalid_records.length(), telemetry_records.length())
  
  // 验证有效记录
  let mut j = 0
  while j < valid_records.length() {
    let record = valid_records[j]
    assert_eq(record.0.length() > 0, true)
    assert_eq(record.1.length() > 0, true)
    assert_eq(record.2.length() > 0, true)
    assert_eq(record.3 > 0L, true)
    assert_eq(record.4 >= 100, true)
    assert_eq(record.4 <= 599, true)
    assert_eq(record.5 >= 0.0, true)
    j = j + 1
  }
  
  // 验证无效记录确实包含错误
  j = 0
  while j < invalid_records.length() {
    let record = invalid_records[j]
    let is_invalid = not validate_record(record)
    assert_eq(is_invalid, true)
    j = j + 1
  }
}

test "telemetry_data_aggregation_filtering" {
  // 测试聚合数据过滤
  
  let raw_metrics = [
    ("api_requests", "GET", "/api/users", 200, 10.5),
    ("api_requests", "GET", "/api/users", 200, 12.3),
    ("api_requests", "GET", "/api/users", 200, 11.8),
    ("api_requests", "POST", "/api/orders", 201, 25.7),
    ("api_requests", "POST", "/api/orders", 201, 28.1),
    ("api_requests", "GET", "/api/products", 200, 8.9),
    ("api_requests", "GET", "/api/products", 200, 9.2),
    ("api_requests", "PUT", "/api/users/123", 200, 15.4),
    ("api_requests", "DELETE", "/api/orders/456", 204, 6.8),
    ("api_requests", "GET", "/api/users", 200, 13.1)
  ]
  
  // 按端点和方法分组聚合
  let mut aggregated_metrics = []
  let mut processed_keys = []
  
  let mut i = 0
  while i < raw_metrics.length() {
    let metric = raw_metrics[i]
    let key = metric.1 + ":" + metric.2 // 方法:端点
    
    // 检查是否已经处理过这个键
    let mut key_processed = false
    let mut j = 0
    while j < processed_keys.length() {
      if processed_keys[j] == key {
        key_processed = true
        break
      }
      j = j + 1
    }
    
    if not key_processed {
      // 聚合相同键的所有指标
      let mut total_duration = 0.0
      let mut count = 0
      let mut status_code = 0
      
      j = 0
      while j < raw_metrics.length() {
        if raw_metrics[j].1 + ":" + raw_metrics[j].2 == key {
          total_duration = total_duration + raw_metrics[j].4
          count = count + 1
          status_code = raw_metrics[j].3
        }
        j = j + 1
      }
      
      let average_duration = total_duration / count.to_double()
      aggregated_metrics.push((key, count, status_code, average_duration))
      processed_keys.push(key)
    }
    
    i = i + 1
  }
  
  // 验证聚合效果
  assert_eq(aggregated_metrics.length(), 5) // 应该有5个不同的端点
  
  // 验证聚合数据
  let mut j = 0
  while j < aggregated_metrics.length() {
    let aggregated = aggregated_metrics[j]
    let key = aggregated.0
    let count = aggregated.1
    let status_code = aggregated.2
    let avg_duration = aggregated.3
    
    assert_eq(count > 0, true)
    assert_eq(status_code >= 200, true)
    assert_eq(avg_duration > 0.0, true)
    
    // 验证特定聚合
    if key == "GET:/api/users" {
      assert_eq(count, 4) // 应该有4个GET /api/users请求
      assert_eq(avg_duration > 10.0, true)
      assert_eq(avg_duration < 15.0, true)
    }
    
    j = j + 1
  }
}

test "telemetry_data_time_window_filtering" {
  // 测试时间窗口过滤
  
  let time_series_data = [
    (1640995200L, "cpu_usage", 75.5),
    (1640995201L, "cpu_usage", 76.2),
    (1640995202L, "cpu_usage", 74.8),
    (1640995203L, "cpu_usage", 75.9),
    (1640995204L, "cpu_usage", 75.1),
    (1640995205L, "cpu_usage", 75.7),
    (1640995206L, "cpu_usage", 75.3),
    (1640995207L, "cpu_usage", 75.8),
    (1640995208L, "cpu_usage", 75.2),
    (1640995209L, "cpu_usage", 75.9)
  ]
  
  // 定义时间窗口
  let window_start = 1640995203L
  let window_end = 1640995207L
  
  // 过滤时间窗口内的数据
  let mut windowed_data = []
  let mut i = 0
  while i < time_series_data.length() {
    let timestamp = time_series_data[i].0
    if timestamp >= window_start and timestamp <= window_end {
      windowed_data.push(time_series_data[i])
    }
    i = i + 1
  }
  
  // 验证时间窗口过滤
  assert_eq(windowed_data.length(), 5) // 应该有5个数据点在窗口内
  
  // 验证窗口内的数据
  let mut j = 0
  while j < windowed_data.length() {
    let timestamp = windowed_data[j].0
    let metric_name = windowed_data[j].1
    let value = windowed_data[j].2
    
    assert_eq(timestamp >= window_start, true)
    assert_eq(timestamp <= window_end, true)
    assert_eq(metric_name, "cpu_usage")
    assert_eq(value > 70.0, true)
    assert_eq(value < 80.0, true)
    
    j = j + 1
  }
  
  // 验证窗口边界
  assert_eq(windowed_data[0].0, window_start)
  assert_eq(windowed_data[windowed_data.length() - 1].0, window_end)
  
  // 验证窗口外数据被排除
  let mut outside_window_count = 0
  i = 0
  while i < time_series_data.length() {
    let timestamp = time_series_data[i].0
    if timestamp < window_start or timestamp > window_end {
      outside_window_count = outside_window_count + 1
    }
    i = i + 1
  }
  assert_eq(outside_window_count, 5)
}

test "telemetry_data_privacy_filtering" {
  // 测试隐私数据过滤
  
  let sensitive_telemetry = [
    ("user_id", "12345", "personal_identifier"),
    ("email", "user@example.com", "personal_identifier"),
    ("phone", "+1234567890", "personal_identifier"),
    ("session_id", "sess_abc123", "session_identifier"),
    ("ip_address", "192.168.1.100", "network_identifier"),
    ("credit_card", "4111-1111-1111-1111", "financial_identifier"),
    ("service_name", "payment-service", "service_metadata"),
    ("response_time", "150.5", "performance_metric"),
    ("status_code", "200", "response_metadata"),
    ("request_path", "/api/payments", "request_metadata")
  ]
  
  // 定义需要过滤的敏感数据类型
  let sensitive_types = [
    "personal_identifier",
    "financial_identifier"
  ]
  
  // 过滤敏感数据
  let mut filtered_data = []
  let mut masked_data = []
  let mut i = 0
  while i < sensitive_telemetry.length() {
    let record = sensitive_telemetry[i]
    let key = record.0
    let value = record.1
    let data_type = record.2
    
    let mut is_sensitive = false
    let mut j = 0
    while j < sensitive_types.length() {
      if data_type == sensitive_types[j] {
        is_sensitive = true
        break
      }
      j = j + 1
    }
    
    if is_sensitive {
      // 掩码处理
      let masked_value = if value.length() > 4 {
        value[0:4] + "***"
      } else {
        "***"
      }
      masked_data.push((key, masked_value, data_type))
    } else {
      filtered_data.push(record)
    }
    
    i = i + 1
  }
  
  // 验证隐私过滤效果
  assert_eq(filtered_data.length(), 5) // 应该有5个非敏感数据
  assert_eq(masked_data.length(), 5)  // 应该有5个敏感数据被掩码
  
  // 验证非敏感数据
  let mut j = 0
  while j < filtered_data.length() {
    let record = filtered_data[j]
    assert_eq(record.2 != "personal_identifier", true)
    assert_eq(record.2 != "financial_identifier", true)
    j = j + 1
  }
  
  // 验证掩码数据
  j = 0
  while j < masked_data.length() {
    let record = masked_data[j]
    assert_eq(record.1.contains("***"), true) // 应该包含掩码
    assert_eq(record.2 == "personal_identifier" or record.2 == "financial_identifier", true)
    j = j + 1
  }
  
  // 验证特定数据被正确处理
  let mut found_masked_email = false
  let mut found_masked_credit_card = false
  j = 0
  while j < masked_data.length() {
    if masked_data[j].0 == "email" {
      found_masked_email = true
      assert_eq(masked_data[j].1, "user***")
    }
    if masked_data[j].0 == "credit_card" {
      found_masked_credit_card = true
      assert_eq(masked_data[j].1, "4111***")
    }
    j = j + 1
  }
  assert_eq(found_masked_email, true)
  assert_eq(found_masked_credit_card, true)
}