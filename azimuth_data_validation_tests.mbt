// Azimuth 数据验证测试用例
// 专注于数据验证和数据完整性检查

// 测试1: 遥测数据格式验证
test "遥测数据格式验证" {
  // 定义有效的遥测数据格式
  let valid_telemetry_format = {
    trace_id: "^[a-f0-9]{32}$",  // 32位十六进制
    span_id: "^[a-f0-9]{16}$",    // 16位十六进制
    service_name: "^[a-zA-Z][a-zA-Z0-9_-]{2,63}$",  // 3-64字符，字母开头
    timestamp: "^\\d{10}$",       // 10位Unix时间戳
    metric_value: "^\\d+(\\.\\d+)?$"  // 整数或小数
  }
  
  // 模拟正则表达式验证函数
  let validate_format = fn(value: String, pattern: String) {
    match pattern {
      "^[a-f0-9]{32}$" => value.length() == 32 && value.all(fn(c) { 
        (c >= 'a' && c <= 'f') || (c >= '0' && c <= '9') 
      })
      "^[a-f0-9]{16}$" => value.length() == 16 && value.all(fn(c) { 
        (c >= 'a' && c <= 'f') || (c >= '0' && c <= '9') 
      })
      "^[a-zA-Z][a-zA-Z0-9_-]{2,63}$" => {
        value.length() >= 3 && value.length() <= 64 && 
        ((value[0] >= 'a' && value[0] <= 'z') || (value[0] >= 'A' && value[0] <= 'Z')) &&
        value.all(fn(c) { 
          (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' || c == '-'
        })
      }
      "^\\d{10}$" => value.length() == 10 && value.all(fn(c) { c >= '0' && c <= '9' })
      "^\\d+(\\.\\d+)?$" => {
        let mut has_dot = false
        let mut valid = true
        for i in 0..value.length() {
          let c = value[i]
          if c == '.' {
            if has_dot {
              valid = false
              break
            }
            has_dot = true
          } else if c < '0' || c > '9' {
            valid = false
            break
          }
        }
        valid && value.length() > 0
      }
      _ => false
    }
  }
  
  // 测试有效数据
  let valid_trace_id = "1234567890abcdef1234567890abcdef"
  let valid_span_id = "1234567890abcdef"
  let valid_service_name = "payment-service"
  let valid_timestamp = "1640995200"
  let valid_metric_value = "42.5"
  
  assert_true(validate_format(valid_trace_id, valid_telemetry_format.trace_id))
  assert_true(validate_format(valid_span_id, valid_telemetry_format.span_id))
  assert_true(validate_format(valid_service_name, valid_telemetry_format.service_name))
  assert_true(validate_format(valid_timestamp, valid_telemetry_format.timestamp))
  assert_true(validate_format(valid_metric_value, valid_telemetry_format.metric_value))
  
  // 测试无效数据
  let invalid_trace_id = "12345"  // 太短
  let invalid_span_id = "1234567890abcde"  // 15个字符
  let invalid_service_name = "1invalid"  // 数字开头
  let invalid_timestamp = "164099520"  // 9位
  let invalid_metric_value = "42.5.3"  // 多个小数点
  
  assert_false(validate_format(invalid_trace_id, valid_telemetry_format.trace_id))
  assert_false(validate_format(invalid_span_id, valid_telemetry_format.span_id))
  assert_false(validate_format(invalid_service_name, valid_telemetry_format.service_name))
  assert_false(validate_format(invalid_timestamp, valid_telemetry_format.timestamp))
  assert_false(validate_format(invalid_metric_value, valid_telemetry_format.metric_value))
}

// 测试2: 数据范围和边界值验证
test "数据范围和边界值验证" {
  // 定义数据范围约束
  let constraints = {
    cpu_usage: { min: 0.0, max: 100.0 },
    memory_usage: { min: 0.0, max: 100.0 },
    response_time: { min: 0, max: 300000 },  // 最多5分钟
    error_rate: { min: 0.0, max: 1.0 },      // 0-100%
    request_count: { min: 0, max: 1000000 }  // 最多100万请求
  }
  
  // 验证数值范围
  let validate_range = fn(value: Float, min: Float, max: Float) {
    value >= min && value <= max
  }
  
  let validate_int_range = fn(value: Int, min: Int, max: Int) {
    value >= min && value <= max
  }
  
  // 测试有效范围内的值
  assert_true(validate_range(50.0, constraints.cpu_usage.min, constraints.cpu_usage.max))
  assert_true(validate_range(75.5, constraints.memory_usage.min, constraints.memory_usage.max))
  assert_true(validate_int_range(5000, constraints.response_time.min, constraints.response_time.max))
  assert_true(validate_range(0.05, constraints.error_rate.min, constraints.error_rate.max))
  assert_true(validate_int_range(10000, constraints.request_count.min, constraints.request_count.max))
  
  // 测试边界值
  assert_true(validate_range(0.0, constraints.cpu_usage.min, constraints.cpu_usage.max))
  assert_true(validate_range(100.0, constraints.cpu_usage.min, constraints.cpu_usage.max))
  assert_true(validate_int_range(0, constraints.response_time.min, constraints.response_time.max))
  assert_true(validate_int_range(300000, constraints.response_time.min, constraints.response_time.max))
  
  // 测试超出范围的值
  assert_false(validate_range(-1.0, constraints.cpu_usage.min, constraints.cpu_usage.max))
  assert_false(validate_range(101.0, constraints.cpu_usage.min, constraints.cpu_usage.max))
  assert_false(validate_int_range(-100, constraints.response_time.min, constraints.response_time.max))
  assert_false(validate_int_range(300001, constraints.response_time.min, constraints.response_time.max))
  assert_false(validate_range(1.1, constraints.error_rate.min, constraints.error_rate.max))
  
  // 测试特殊值
  assert_true(validate_range(0.0, constraints.cpu_usage.min, constraints.cpu_usage.max))
  assert_false(validate_range(-0.1, constraints.cpu_usage.min, constraints.cpu_usage.max))
  assert_true(validate_range(99.999, constraints.cpu_usage.min, constraints.cpu_usage.max))
  assert_false(validate_range(100.001, constraints.cpu_usage.min, constraints.cpu_usage.max))
}

// 测试3: 数据依赖关系验证
test "数据依赖关系验证" {
  // 定义数据依赖关系
  let validate_span_hierarchy = fn(span: { parent_span_id: Option[String], span_id: String, trace_id: String }) {
    match span.parent_span_id {
      Some(parent_id) => {
        // 父span ID应该不同于当前span ID
        parent_id != span.span_id
      }
      None => true  // 根span没有父span
    }
  }
  
  let validate_timestamp_order = fn(start_time: Int, end_time: Int) {
    start_time <= end_time
  }
  
  let validate_metric_consistency = fn(metric_type: String, value: Float) {
    match metric_type {
      "percentage" => value >= 0.0 && value <= 100.0
      "ratio" => value >= 0.0 && value <= 1.0
      "count" => value >= 0.0 && value.to_int() == value  // 整数
      "duration_ms" => value >= 0.0
      _ => true  // 未知类型暂时通过
    }
  }
  
  // 测试span层次结构验证
  let root_span = { parent_span_id: None, span_id: "span123", trace_id: "trace456" }
  let child_span = { parent_span_id: Some("span123"), span_id: "span789", trace_id: "trace456" }
  let invalid_span = { parent_span_id: Some("span789"), span_id: "span789", trace_id: "trace456" }
  
  assert_true(validate_span_hierarchy(root_span))
  assert_true(validate_span_hierarchy(child_span))
  assert_false(validate_span_hierarchy(invalid_span))
  
  // 测试时间戳顺序验证
  assert_true(validate_timestamp_order(1640995200, 1640995250))
  assert_true(validate_timestamp_order(1640995200, 1640995200))  // 相等时间
  assert_false(validate_timestamp_order(1640995250, 1640995200))  // 结束时间早于开始时间
  
  // 测试指标一致性验证
  assert_true(validate_metric_consistency("percentage", 50.0))
  assert_false(validate_metric_consistency("percentage", 150.0))  // 超出百分比范围
  assert_false(validate_metric_consistency("percentage", -10.0))  // 负百分比
  
  assert_true(validate_metric_consistency("ratio", 0.5))
  assert_true(validate_metric_consistency("ratio", 0.0))
  assert_true(validate_metric_consistency("ratio", 1.0))
  assert_false(validate_metric_consistency("ratio", 1.5))  // 超出比率范围
  
  assert_true(validate_metric_consistency("count", 42.0))
  assert_false(validate_metric_consistency("count", 42.5))  // 非整数
  
  assert_true(validate_metric_consistency("duration_ms", 1000.0))
  assert_false(validate_metric_consistency("duration_ms", -100.0))  // 负持续时间
}

// 测试4: 数据完整性验证
test "数据完整性验证" {
  // 定义必需字段
  let required_span_fields = ["trace_id", "span_id", "operation_name", "start_time", "end_time"]
  let required_metric_fields = ["name", "value", "unit", "timestamp"]
  
  // 验证必需字段
  let validate_required_fields = fn(data: { fields: Array[String] }, required: Array[String]) {
    for field in required {
      if not(data.fields.contains(field)) {
        return false
      }
    }
    true
  }
  
  // 验证数据一致性
  let validate_data_consistency = fn(span_data: { trace_id: String, span_id: String, parent_span_id: Option[String] }, 
                                    metric_data: { trace_id: String, span_id: String }) {
    span_data.trace_id == metric_data.trace_id && 
    span_data.span_id == metric_data.span_id
  }
  
  // 测试必需字段验证
  let complete_span = { fields: ["trace_id", "span_id", "operation_name", "start_time", "end_time", "tags"] }
  let incomplete_span = { fields: ["trace_id", "span_id", "operation_name"] }
  let complete_metric = { fields: ["name", "value", "unit", "timestamp", "tags"] }
  let incomplete_metric = { fields: ["name", "value"] }
  
  assert_true(validate_required_fields(complete_span, required_span_fields))
  assert_false(validate_required_fields(incomplete_span, required_span_fields))
  assert_true(validate_required_fields(complete_metric, required_metric_fields))
  assert_false(validate_required_fields(incomplete_metric, required_metric_fields))
  
  // 测试数据一致性验证
  let consistent_span = { trace_id: "trace123", span_id: "span456", parent_span_id: None }
  let consistent_metric = { trace_id: "trace123", span_id: "span456" }
  let inconsistent_metric = { trace_id: "trace789", span_id: "span456" }
  
  assert_true(validate_data_consistency(consistent_span, consistent_metric))
  assert_false(validate_data_consistency(consistent_span, inconsistent_metric))
  
  // 测试循环引用检测
  let detect_circular_reference = fn(spans: Array<{ span_id: String, parent_span_id: Option[String] }>) {
    let mut visited = []
    let mut stack = []
    
    let has_cycle = fn(span_id: String) {
      if stack.contains(span_id) {
        return true  // 发现循环
      }
      if visited.contains(span_id) {
        return false  // 已检查过，无循环
      }
      
      stack = stack.push(span_id)
      visited = visited.push(span_id)
      
      // 查找当前span的子span
      for span in spans {
        match span.parent_span_id {
          Some(parent) => {
            if parent == span_id && has_cycle(span.span_id) {
              return true
            }
          }
          None => ()
        }
      }
      
      stack = stack.slice(0, stack.length() - 1)  // 弹出
      false
    }
    
    // 查找根span（没有父span的span）
    for span in spans {
      match span.parent_span_id {
        None => {
          if has_cycle(span.span_id) {
            return true
          }
        }
        Some(_) => ()
      }
    }
    
    false
  }
  
  // 测试无循环引用的span树
  let valid_spans = [
    { span_id: "root", parent_span_id: None },
    { span_id: "child1", parent_span_id: Some("root") },
    { span_id: "child2", parent_span_id: Some("root") },
    { span_id: "grandchild", parent_span_id: Some("child1") }
  ]
  
  // 测试有循环引用的span树
  let circular_spans = [
    { span_id: "root", parent_span_id: None },
    { span_id: "child", parent_span_id: Some("root") },
    { span_id: "root", parent_span_id: Some("child") }  // 循环引用
  ]
  
  assert_false(detect_circular_reference(valid_spans))
  assert_true(detect_circular_reference(circular_spans))
}

// 测试5: 数据质量评估
test "数据质量评估" {
  // 定义数据质量指标
  let calculate_completeness = fn(expected_fields: Int, present_fields: Int) {
    if expected_fields == 0 {
      1.0  // 避免除以零
    } else {
      present_fields.to_float() / expected_fields.to_float()
    }
  }
  
  let calculate_accuracy = fn(total_records: Int, valid_records: Int) {
    if total_records == 0 {
      1.0  // 避免除以零
    } else {
      valid_records.to_float() / total_records.to_float()
    }
  }
  
  let calculate_consistency = fn(total_comparisons: Int, consistent_comparisons: Int) {
    if total_comparisons == 0 {
      1.0  // 避免除以零
    } else {
      consistent_comparisons.to_float() / total_comparisons.to_float()
    }
  }
  
  // 测试完整性计算
  assert_eq(calculate_completeness(5, 5), 1.0)  // 100%完整
  assert_eq(calculate_completeness(5, 4), 0.8)  // 80%完整
  assert_eq(calculate_completeness(5, 0), 0.0)  // 0%完整
  assert_eq(calculate_completeness(0, 0), 1.0)  // 边界情况
  
  // 测试准确性计算
  assert_eq(calculate_accuracy(100, 95), 0.95)  // 95%准确
  assert_eq(calculate_accuracy(100, 80), 0.8)   // 80%准确
  assert_eq(calculate_accuracy(100, 0), 0.0)    // 0%准确
  assert_eq(calculate_accuracy(0, 0), 1.0)      // 边界情况
  
  // 测试一致性计算
  assert_eq(calculate_consistency(50, 48), 0.96)  // 96%一致
  assert_eq(calculate_consistency(50, 40), 0.8)   // 80%一致
  assert_eq(calculate_consistency(50, 0), 0.0)    // 0%一致
  assert_eq(calculate_consistency(0, 0), 1.0)      // 边界情况
  
  // 综合数据质量评分
  let calculate_quality_score = fn(completeness: Float, accuracy: Float, consistency: Float) {
    // 加权平均：完整性30%，准确性40%，一致性30%
    (completeness * 0.3 + accuracy * 0.4 + consistency * 0.3)
  }
  
  let quality_score1 = calculate_quality_score(1.0, 0.95, 0.96)
  let quality_score2 = calculate_quality_score(0.8, 0.8, 0.8)
  let quality_score3 = calculate_quality_score(0.5, 0.9, 0.7)
  
  assert_true(quality_score1 > 0.95)  // 高质量
  assert_eq(quality_score2, 0.8)      // 中等质量
  assert_true(quality_score3 > 0.6 && quality_score3 < 0.8)  // 中低质量
  
  // 测试异常值检测
  let detect_outliers = fn(values: Array[Float], threshold: Float) {
    if values.length() == 0 {
      return []
    }
    
    // 计算平均值
    let mut sum = 0.0
    for v in values {
      sum = sum + v
    }
    let mean = sum / values.length().to_float()
    
    // 计算标准差
    let mut variance_sum = 0.0
    for v in values {
      let diff = v - mean
      variance_sum = variance_sum + diff * diff
    }
    let variance = variance_sum / values.length().to_float()
    let std_dev = if variance >= 0.0 { variance.sqrt() } else { 0.0 }
    
    // 识别异常值（超过阈值个标准差）
    let mut outliers = []
    for i in 0..values.length() {
      let v = values[i]
      let z_score = if std_dev > 0.0 { (v - mean).abs() / std_dev } else { 0.0 }
      if z_score > threshold {
        outliers = outliers.push((i, v, z_score))
      }
    }
    
    outliers
  }
  
  let normal_data = [10.0, 12.0, 11.5, 9.8, 10.2, 11.0, 10.5, 9.9, 10.8, 11.2]
  let data_with_outliers = [10.0, 12.0, 50.0, 9.8, 10.2, 11.0, -20.0, 9.9, 10.8, 11.2]
  
  let normal_outliers = detect_outliers(normal_data, 2.0)
  let outlier_outliers = detect_outliers(data_with_outliers, 2.0)
  
  assert_eq(normal_outliers.length(), 0)  // 正常数据无异常值
  assert_true(outlier_outliers.length() >= 2)  // 异常数据有异常值
}