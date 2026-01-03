// Azimuth 遥测数据验证增强测试用例
// 专注于测试数据完整性检查和验证规则

// 测试1: 数据类型验证
test "数据类型验证" {
  // 定义验证规则
  type ValidationRule = {
    field_name: String,
    required: Bool,
    data_type: String,
    min_value: Option[Float],
    max_value: Option[Float],
    allowed_values: Option[Array<String>],
    pattern: Option[String]
  }
  
  // 定义验证结果
  type ValidationResult = {
    is_valid: Bool,
    field_name: String,
    error_message: String,
    actual_value: String
  }
  
  // 定义遥测数据点
  type TelemetryDataPoint = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    labels: Array<(String, String)>,
    data_type: String
  }
  
  // 创建数据类型验证器
  let data_type_validator = fn(rules: Array<ValidationRule>) {
    fn(data_point: TelemetryDataPoint) -> Array<ValidationResult> {
      let mut results = Array::empty()
      
      for rule in rules {
        let field_value = match rule.field_name {
          "timestamp" => data_point.timestamp.to_string()
          "metric_name" => data_point.metric_name
          "value" => data_point.value.to_string()
          "data_type" => data_point.data_type
          _ => ""
        }
        
        let mut is_valid = true
        let mut error_message = ""
        
        // 检查必填字段
        if rule.required && field_value.length() == 0 {
          is_valid = false
          error_message = "Field is required but empty"
        }
        
        // 检查数据类型
        if is_valid {
          match rule.data_type {
            "int" => {
              match field_value.parse_int() {
                Some(_) => ()
                None => {
                  is_valid = false
                  error_message = "Field must be an integer"
                }
              }
            }
            "float" => {
              match field_value.parse_float() {
                Some(_) => ()
                None => {
                  is_valid = false
                  error_message = "Field must be a float"
                }
              }
            }
            "string" => {
              // 字符串类型总是有效
            }
            _ => {
              // 未知类型，默认有效
            }
          }
        }
        
        // 检查最小值
        if is_valid && rule.data_type == "float" || rule.data_type == "int" {
          match field_value.parse_float() {
            Some(value) => {
              match rule.min_value {
                Some(min) => {
                  if value < min {
                    is_valid = false
                    error_message = "Value is below minimum: " + min.to_string()
                  }
                }
                None => ()
              }
            }
            None => ()
          }
        }
        
        // 检查最大值
        if is_valid && rule.data_type == "float" || rule.data_type == "int" {
          match field_value.parse_float() {
            Some(value) => {
              match rule.max_value {
                Some(max) => {
                  if value > max {
                    is_valid = false
                    error_message = "Value is above maximum: " + max.to_string()
                  }
                }
                None => ()
              }
            }
            None => ()
          }
        }
        
        // 检查允许值
        if is_valid && rule.data_type == "string" {
          match rule.allowed_values {
            Some(allowed) => {
              if not allowed.contains(field_value) {
                is_valid = false
                error_message = "Value not in allowed list"
              }
            }
            None => ()
          }
        }
        
        // 检查模式匹配
        if is_valid && rule.data_type == "string" {
          match rule.pattern {
            Some(pattern) => {
              // 简化的模式匹配
              if pattern == "^[a-z_][a-z0-9_]*$" {
                if not (field_value.chars().all(fn(c) { 
                  (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_' 
                }) && field_value.chars().nth(0) != Some('_')) {
                  is_valid = false
                  error_message = "Value does not match pattern: " + pattern
                }
              }
            }
            None => ()
          }
        }
        
        results = results.push(ValidationResult({
          is_valid: is_valid,
          field_name: rule.field_name,
          error_message: error_message,
          actual_value: field_value
        }))
      }
      
      results
    }
  }
  
  // 定义验证规则
  let validation_rules = [
    {
      field_name: "timestamp",
      required: true,
      data_type: "int",
      min_value: Some(1600000000000),  // 2020年之后的时间戳
      max_value: None,
      allowed_values: None,
      pattern: None
    },
    {
      field_name: "metric_name",
      required: true,
      data_type: "string",
      min_value: None,
      max_value: None,
      allowed_values: None,
      pattern: Some("^[a-z_][a-z0-9_]*$")
    },
    {
      field_name: "value",
      required: true,
      data_type: "float",
      min_value: Some(0.0),
      max_value: Some(1000000.0),
      allowed_values: None,
      pattern: None
    },
    {
      field_name: "data_type",
      required: true,
      data_type: "string",
      min_value: None,
      max_value: None,
      allowed_values: Some(["counter", "gauge", "histogram", "summary"]),
      pattern: None
    }
  ]
  
  let validator = data_type_validator(validation_rules)
  
  // 测试有效数据
  let valid_data = TelemetryDataPoint({
    timestamp: 1735689600000,
    metric_name: "cpu_usage",
    value: 45.2,
    labels: [("host", "server1")],
    data_type: "gauge"
  })
  
  let valid_results = validator(valid_data)
  assert_true(valid_results.reduce(fn(acc, r) { acc && r.is_valid }, true))
  
  // 测试无效时间戳
  let invalid_timestamp_data = TelemetryDataPoint({
    timestamp: 1500000000000,  // 2017年，早于最小值
    metric_name: "cpu_usage",
    value: 45.2,
    labels: [("host", "server1")],
    data_type: "gauge"
  })
  
  let invalid_timestamp_results = validator(invalid_timestamp_data)
  let timestamp_result = invalid_timestamp_results.find(fn(r) { r.field_name == "timestamp" })!
  assert_false(timestamp_result.is_valid)
  assert_true(timestamp_result.error_message.contains("below minimum"))
  
  // 测试无效指标名称
  let invalid_name_data = TelemetryDataPoint({
    timestamp: 1735689600000,
    metric_name: "CPU-Usage",  // 包含大写字母和连字符
    value: 45.2,
    labels: [("host", "server1")],
    data_type: "gauge"
  })
  
  let invalid_name_results = validator(invalid_name_data)
  let name_result = invalid_name_results.find(fn(r) { r.field_name == "metric_name" })!
  assert_false(name_result.is_valid)
  assert_true(name_result.error_message.contains("pattern"))
  
  // 测试无效值
  let invalid_value_data = TelemetryDataPoint({
    timestamp: 1735689600000,
    metric_name: "cpu_usage",
    value: -5.0,  // 负值，低于最小值
    labels: [("host", "server1")],
    data_type: "gauge"
  })
  
  let invalid_value_results = validator(invalid_value_data)
  let value_result = invalid_value_results.find(fn(r) { r.field_name == "value" })!
  assert_false(value_result.is_valid)
  assert_true(value_result.error_message.contains("below minimum"))
  
  // 测试无效数据类型
  let invalid_type_data = TelemetryDataPoint({
    timestamp: 1735689600000,
    metric_name: "cpu_usage",
    value: 45.2,
    labels: [("host", "server1")],
    data_type: "invalid_type"  // 不在允许列表中
  })
  
  let invalid_type_results = validator(invalid_type_data)
  let type_result = invalid_type_results.find(fn(r) { r.field_name == "data_type" })!
  assert_false(type_result.is_valid)
  assert_true(type_result.error_message.contains("allowed list"))
}

// 测试2: 时间序列一致性验证
test "时间序列一致性验证" {
  // 定义时间序列点
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    series_id: String
  }
  
  // 定义时间序列
  type TimeSeries = {
    series_id: String,
    points: Array[TimeSeriesPoint>,
    start_time: Int,
    end_time: Int,
    expected_interval_ms: Int
  }
  
  // 定义一致性检查结果
  type ConsistencyResult = {
    is_consistent: Bool,
    issues: Array<String>,
    warnings: Array<String>,
    statistics: {
      total_points: Int,
      missing_points: Int,
      duplicate_points: Int,
      out_of_order_points: Int,
      interval_violations: Int
    }
  }
  
  // 创建时间序列一致性验证器
  let time_series_consistency_validator = fn() {
    fn(series: TimeSeries) -> ConsistencyResult {
      let mut issues = Array::empty()
      let mut warnings = Array::empty()
      
      // 检查时间序列是否为空
      if series.points.length() == 0 {
        return ConsistencyResult({
          is_consistent: false,
          issues: ["Time series is empty"],
          warnings: [],
          statistics: {
            total_points: 0,
            missing_points: 0,
            duplicate_points: 0,
            out_of_order_points: 0,
            interval_violations: 0
          }
        })
      }
      
      // 按时间戳排序
      let sorted_points = series.points.sort(fn(a, b) {
        if a.timestamp < b.timestamp { -1 }
        else if a.timestamp > b.timestamp { 1 }
        else { 0 }
      })
      
      // 检查时间戳是否在预期范围内
      let first_timestamp = sorted_points[0].timestamp
      let last_timestamp = sorted_points[sorted_points.length() - 1].timestamp
      
      if series.start_time > 0 && first_timestamp < series.start_time {
        issues = issues.push("First timestamp is before expected start time")
      }
      
      if series.end_time > 0 && last_timestamp > series.end_time {
        issues = issues.push("Last timestamp is after expected end time")
      }
      
      // 检查时间戳顺序
      let mut out_of_order_count = 0
      for i in 1..sorted_points.length() {
        if sorted_points[i].timestamp < sorted_points[i-1].timestamp {
          out_of_order_count = out_of_order_count + 1
        }
      }
      
      if out_of_order_count > 0 {
        issues = issues.push(out_of_order_count.to_string() + " points are out of order")
      }
      
      // 检查重复时间戳
      let mut duplicate_count = 0
      let mut seen_timestamps = Array::empty()
      
      for point in sorted_points {
        if seen_timestamps.contains(point.timestamp) {
          duplicate_count = duplicate_count + 1
        } else {
          seen_timestamps = seen_timestamps.push(point.timestamp)
        }
      }
      
      if duplicate_count > 0 {
        warnings = warnings.push(duplicate_count.to_string() + " duplicate timestamps found")
      }
      
      // 检查时间间隔一致性
      let mut interval_violations = 0
      let mut expected_timestamps = Array::empty()
      
      if series.expected_interval_ms > 0 && sorted_points.length() > 1 {
        let mut current_timestamp = sorted_points[0].timestamp
        
        while current_timestamp <= last_timestamp {
          expected_timestamps = expected_timestamps.push(current_timestamp)
          current_timestamp = current_timestamp + series.expected_interval_ms
        }
        
        // 查找缺失的时间戳
        let actual_timestamps = sorted_points.map(fn(p) { p.timestamp })
        let missing_timestamps = expected_timestamps.filter(fn(ts) { not actual_timestamps.contains(ts) })
        
        if missing_timestamps.length() > 0 {
          warnings = warnings.push(missing_timestamps.length().to_string() + " expected points are missing")
        }
        
        // 检查间隔违规
        for i in 1..sorted_points.length() {
          let actual_interval = sorted_points[i].timestamp - sorted_points[i-1].timestamp
          if actual_interval != series.expected_interval_ms {
            interval_violations = interval_violations + 1
          }
        }
        
        if interval_violations > 0 {
          warnings = warnings.push(interval_violations.to_string() + " interval violations found")
        }
      }
      
      // 检查值异常
      let values = sorted_points.map(fn(p) { p.value })
      let mean_value = values.reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() as Float)
      let variance = values.reduce(fn(acc, v) { 
        let diff = v - mean_value
        acc + diff * diff
      }, 0.0) / (values.length() as Float)
      let std_dev = variance.sqrt()
      
      // 3σ规则检测异常值
      let outlier_threshold = 3.0
      let mut outliers = 0
      
      for value in values {
        if (value - mean_value).abs() > outlier_threshold * std_dev {
          outliers = outliers + 1
        }
      }
      
      if outliers > 0 {
        warnings = warnings.push(outliers.to_string() + " statistical outliers detected")
      }
      
      // 计算统计信息
      let missing_points = if series.expected_interval_ms > 0 {
        let expected_count = ((last_timestamp - first_timestamp) / series.expected_interval_ms) + 1
        expected_count - sorted_points.length()
      } else {
        0
      }
      
      let statistics = {
        total_points: sorted_points.length(),
        missing_points: missing_points,
        duplicate_points: duplicate_count,
        out_of_order_points: out_of_order_count,
        interval_violations: interval_violations
      }
      
      // 确定整体一致性
      let is_consistent = issues.length() == 0
      
      ConsistencyResult({
        is_consistent: is_consistent,
        issues: issues,
        warnings: warnings,
        statistics: statistics
      })
    }
  }
  
  let validator = time_series_consistency_validator()
  
  // 测试一致的时间序列
  let consistent_series = TimeSeries({
    series_id: "cpu_usage",
    points: [
      { timestamp: 1735689600000, value: 45.2, series_id: "cpu_usage" },
      { timestamp: 1735689660000, value: 47.8, series_id: "cpu_usage" },
      { timestamp: 1735689720000, value: 46.1, series_id: "cpu_usage" },
      { timestamp: 1735689780000, value: 48.9, series_id: "cpu_usage" },
      { timestamp: 1735689840000, value: 49.3, series_id: "cpu_usage" }
    ],
    start_time: 1735689600000,
    end_time: 1735689840000,
    expected_interval_ms: 60000  // 1分钟间隔
  })
  
  let consistent_result = validator(consistent_series)
  assert_true(consistent_result.is_consistent)
  assert_eq(consistent_result.issues.length(), 0)
  assert_eq(consistent_result.statistics.total_points, 5)
  assert_eq(consistent_result.statistics.out_of_order_points, 0)
  
  // 测试有问题的数据点
  let problematic_series = TimeSeries({
    series_id: "memory_usage",
    points: [
      { timestamp: 1735689600000, value: 78.5, series_id: "memory_usage" },
      { timestamp: 1735689660000, value: 80.2, series_id: "memory_usage" },
      { timestamp: 1735689620000, value: 79.1, series_id: "memory_usage" },  // 乱序
      { timestamp: 1735689660000, value: 81.3, series_id: "memory_usage" },  // 重复时间戳
      { timestamp: 1735689840000, value: 82.7, series_id: "memory_usage" }   // 间隔不一致
    ],
    start_time: 1735689600000,
    end_time: 1735689840000,
    expected_interval_ms: 60000  // 1分钟间隔
  })
  
  let problematic_result = validator(problematic_series)
  assert_false(problematic_result.is_consistent)
  assert_true(problematic_result.issues.length() > 0)
  assert_eq(problematic_result.statistics.out_of_order_points, 1)
  assert_eq(problematic_result.statistics.duplicate_points, 1)
  assert_true(problematic_result.statistics.interval_violations > 0)
  
  // 测试空时间序列
  let empty_series = TimeSeries({
    series_id: "empty_series",
    points: [],
    start_time: 0,
    end_time: 0,
    expected_interval_ms: 60000
  })
  
  let empty_result = validator(empty_series)
  assert_false(empty_result.is_consistent)
  assert_true(empty_result.issues.contains("Time series is empty"))
}

// 测试3: 标签一致性验证
test "标签一致性验证" {
  // 定义标签规范
  type LabelSchema = {
    required_labels: Array<String>,
    optional_labels: Array<String>,
    label_value_rules: Array<(String, Array<String>)>  // (label_name, allowed_values)
  }
  
  // 定义标签验证结果
  type LabelValidationResult = {
    is_valid: Bool,
    missing_labels: Array<String>,
    invalid_labels: Array<(String, String)>,  // (label_name, error_message)
    warnings: Array<String>
  }
  
  // 创建标签一致性验证器
  let label_consistency_validator = fn(schema: LabelSchema) {
    fn(labels: Array<(String, String)>, metric_name: String) -> LabelValidationResult {
      let mut is_valid = true
      let mut missing_labels = Array::empty()
      let mut invalid_labels = Array::empty()
      let mut warnings = Array::empty()
      
      // 检查必需标签
      for required_label in schema.required_labels {
        if not labels.any(fn(l) { l.0 == required_label }) {
          missing_labels = missing_labels.push(required_label)
          is_valid = false
        }
      }
      
      // 检查标签值规则
      for (label_name, label_value) in labels {
        // 检查标签名称是否在规范中
        let is_valid_label_name = schema.required_labels.contains(label_name) || 
                                 schema.optional_labels.contains(label_name)
        
        if not is_valid_label_name {
          warnings = warnings.push("Unknown label: " + label_name)
        }
        
        // 检查标签值是否符合规则
        match schema.label_value_rules.find(fn(r) { r.0 == label_name }) {
          Some((_, allowed_values)) => {
            if not allowed_values.contains(label_value) {
              invalid_labels = invalid_labels.push((label_name, "Invalid value: " + label_value))
              is_valid = false
            }
          }
          None => {
            // 没有特定规则，允许任何值
          }
        }
        
        // 检查标签值是否为空
        if label_value.length() == 0 {
          invalid_labels = invalid_labels.push((label_name, "Empty label value"))
          is_valid = false
        }
        
        // 检查标签值格式
        if label_value.contains(" ") || label_value.contains("\t") || label_value.contains("\n") {
          invalid_labels = invalid_labels.push((label_name, "Label value contains whitespace"))
          is_valid = false
        }
      }
      
      // 检查标签数量限制
      if labels.length() > 20 {
        warnings = warnings.push("Too many labels: " + labels.length().to_string())
      }
      
      LabelValidationResult({
        is_valid: is_valid,
        missing_labels: missing_labels,
        invalid_labels: invalid_labels,
        warnings: warnings
      })
    }
  }
  
  // 定义标签规范
  let label_schema = LabelSchema({
    required_labels: ["service", "instance"],
    optional_labels: ["version", "environment", "region", "host", "team"],
    label_value_rules: [
      ("environment", ["development", "staging", "production"]),
      ("region", ["us-east", "us-west", "eu-west", "ap-southeast"]),
      ("team", ["backend", "frontend", "devops", "data"])
    ]
  })
  
  let validator = label_consistency_validator(label_schema)
  
  // 测试有效的标签
  let valid_labels = [
    ("service", "api-service"),
    ("instance", "api-server-01"),
    ("version", "1.2.3"),
    ("environment", "production"),
    ("region", "us-west"),
    ("team", "backend")
  ]
  
  let valid_result = validator(valid_labels, "http_requests_total")
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.missing_labels.length(), 0)
  assert_eq(valid_result.invalid_labels.length(), 0)
  
  // 测试缺少必需标签
  let missing_required_labels = [
    ("service", "api-service"),
    ("version", "1.2.3"),
    ("environment", "production")
  ]
  
  let missing_result = validator(missing_required_labels, "http_requests_total")
  assert_false(missing_result.is_valid)
  assert_true(missing_result.missing_labels.contains("instance"))
  
  // 测试无效标签值
  let invalid_value_labels = [
    ("service", "api-service"),
    ("instance", "api-server-01"),
    ("environment", "test"),  // 不在允许值列表中
    ("region", "us-central")   // 不在允许值列表中
  ]
  
  let invalid_value_result = validator(invalid_value_labels, "http_requests_total")
  assert_false(invalid_value_result.is_valid)
  assert_true(invalid_value_result.invalid_labels.any(fn(l) { l.0 == "environment" }))
  assert_true(invalid_value_result.invalid_labels.any(fn(l) { l.0 == "region" }))
  
  // 测试空标签值
  let empty_value_labels = [
    ("service", "api-service"),
    ("instance", ""),  // 空值
    ("environment", "production")
  ]
  
  let empty_value_result = validator(empty_value_labels, "http_requests_total")
  assert_false(empty_value_result.is_valid)
  assert_true(empty_value_result.invalid_labels.any(fn(l) { l.0 == "instance" }))
  
  // 测试未知标签
  let unknown_labels = [
    ("service", "api-service"),
    ("instance", "api-server-01"),
    ("unknown_label", "some_value"),  // 未知标签
    ("another_unknown", "another_value")  // 另一个未知标签
  ]
  
  let unknown_result = validator(unknown_labels, "http_requests_total")
  assert_true(unknown_result.is_valid)  // 未知标签只是警告，不是错误
  assert_true(unknown_result.warnings.length() > 0)
  assert_true(unknown_result.warnings.any(fn(w) { w.contains("Unknown label") }))
}

// 测试4: 跨数据源一致性验证
test "跨数据源一致性验证" {
  // 定义数据源
  enum DataSource {
    Metrics
    Logs
    Traces
    Events
  }
  
  // 定义跨源数据点
  type CrossSourceDataPoint = {
    source: DataSource,
    timestamp: Int,
    trace_id: Option<String>,
    span_id: Option<String>,
    correlation_id: Option<String>,
    service_name: String,
    operation_name: String,
    data: Array<(String, String)>
  }
  
  // 定义一致性检查结果
  type CrossSourceConsistencyResult = {
    is_consistent: Bool,
    inconsistencies: Array<String>,
    correlations_found: Int,
    orphaned_records: Int
  }
  
  // 创建跨数据源一致性验证器
  let cross_source_consistency_validator = fn() {
    fn(data_points: Array[CrossSourceDataPoint>) -> CrossSourceConsistencyResult {
      let mut inconsistencies = Array::empty()
      let mut correlations_found = 0
      let mut orphaned_records = 0
      
      // 按时间戳排序
      let sorted_points = data_points.sort(fn(a, b) {
        if a.timestamp < b.timestamp { -1 }
        else if a.timestamp > b.timestamp { 1 }
        else { 0 }
      })
      
      // 按trace_id分组
      let trace_groups = sorted_points.group_by(fn(p) { 
        match p.trace_id {
          Some(id) => id
          None => "no-trace"
        }
      })
      
      // 检查每个trace组的一致性
      for (trace_id, points) in trace_groups {
        if trace_id == "no-trace" {
          // 没有trace_id的记录
          orphaned_records = orphaned_records + points.length()
          continue
        }
        
        // 检查同一trace中的服务一致性
        let services = points.map(fn(p) { p.service_name }).unique()
        if services.length() > 10 {
          inconsistencies = inconsistencies.push("Trace " + trace_id + " involves too many services: " + services.length().to_string())
        }
        
        // 检查时间顺序
        let mut time_violations = 0
        for i in 1..points.length() {
          if points[i].timestamp < points[i-1].timestamp {
            time_violations = time_violations + 1
          }
        }
        
        if time_violations > 0 {
          inconsistencies = inconsistencies.push("Trace " + trace_id + " has " + time_violations.to_string() + " time order violations")
        }
        
        // 检查span父子关系
        let spans = points.filter(fn(p) { p.source == DataSource::Traces })
        let span_ids = spans.map(fn(s) { s.span_id }).filter_map(fn(id) { id })
        let unique_span_ids = span_ids.unique()
        
        if unique_span_ids.length() != span_ids.length() {
          inconsistencies = inconsistencies.push("Trace " + trace_id + " has duplicate span IDs")
        }
        
        // 检查跨源关联
        let metrics_points = points.filter(fn(p) { p.source == DataSource::Metrics })
        let logs_points = points.filter(fn(p) { p.source == DataSource::Logs })
        let traces_points = points.filter(fn(p) { p.source == DataSource::Traces })
        
        // 如果有多个源的数据，则认为找到了关联
        if (metrics_points.length() > 0 && logs_points.length() > 0) ||
           (metrics_points.length() > 0 && traces_points.length() > 0) ||
           (logs_points.length() > 0 && traces_points.length() > 0) {
          correlations_found = correlations_found + 1
        }
      }
      
      // 按correlation_id分组
      let correlation_groups = sorted_points.group_by(fn(p) { 
        match p.correlation_id {
          Some(id) => id
          None => "no-correlation"
        }
      })
      
      // 检查每个correlation组的一致性
      for (correlation_id, points) in correlation_groups {
        if correlation_id == "no-correlation" {
          continue
        }
        
        // 检查同一correlation中的操作一致性
        let operations = points.map(fn(p) { p.operation_name }).unique()
        if operations.length() > 5 {
          inconsistencies = inconsistencies.push("Correlation " + correlation_id + " involves too many operations: " + operations.length().to_string())
        }
        
        // 检查服务一致性
        let services = points.map(fn(p) { p.service_name }).unique()
        if services.length() > 8 {
          inconsistencies = inconsistencies.push("Correlation " + correlation_id + " involves too many services: " + services.length().to_string())
        }
        
        // 检查时间窗口
        let time_window = points[points.length() - 1].timestamp - points[0].timestamp
        if time_window > 300000 {  // 5分钟
          inconsistencies = inconsistencies.push("Correlation " + correlation_id + " time window too large: " + (time_window / 1000).to_string() + "s")
        }
      }
      
      // 检查数据量平衡
      let metrics_count = data_points.filter(fn(p) { p.source == DataSource::Metrics }).length()
      let logs_count = data_points.filter(fn(p) { p.source == DataSource::Logs }).length()
      let traces_count = data_points.filter(fn(p) { p.source == DataSource::Traces }).length()
      
      let total_count = metrics_count + logs_count + traces_count
      if total_count > 0 {
        let metrics_ratio = (metrics_count as Float) / (total_count as Float)
        let logs_ratio = (logs_count as Float) / (total_count as Float)
        let traces_ratio = (traces_count as Float) / (total_count as Float)
        
        // 检查极端不平衡
        if metrics_ratio > 0.95 || logs_ratio > 0.95 || traces_ratio > 0.95 {
          inconsistencies = inconsistencies.push("Data source imbalance detected")
        }
      }
      
      let is_consistent = inconsistencies.length() == 0
      
      CrossSourceConsistencyResult({
        is_consistent: is_consistent,
        inconsistencies: inconsistencies,
        correlations_found: correlations_found,
        orphaned_records: orphaned_records
      })
    }
  }
  
  let validator = cross_source_consistency_validator()
  
  // 测试一致的跨源数据
  let consistent_data = [
    CrossSourceDataPoint({
      source: DataSource::Traces,
      timestamp: 1735689600000,
      trace_id: Some("trace-123"),
      span_id: Some("span-001"),
      correlation_id: Some("corr-456"),
      service_name: "api-service",
      operation_name: "process_request",
      data: [("http.method", "GET"), ("http.status_code", "200")]
    }),
    CrossSourceDataPoint({
      source: DataSource::Metrics,
      timestamp: 1735689601000,
      trace_id: Some("trace-123"),
      span_id: None,
      correlation_id: Some("corr-456"),
      service_name: "api-service",
      operation_name: "process_request",
      data: [("http.request.duration", "150"), ("cpu.usage", "45.2")]
    }),
    CrossSourceDataPoint({
      source: DataSource::Logs,
      timestamp: 1735689602000,
      trace_id: Some("trace-123"),
      span_id: None,
      correlation_id: Some("corr-456"),
      service_name: "api-service",
      operation_name: "process_request",
      data: [("level", "INFO"), ("message", "Request processed successfully")]
    })
  ]
  
  let consistent_result = validator(consistent_data)
  assert_true(consistent_result.is_consistent)
  assert_eq(consistent_result.inconsistencies.length(), 0)
  assert_eq(consistent_result.correlations_found, 1)
  assert_eq(consistent_result.orphaned_records, 0)
  
  // 测试不一致的跨源数据
  let inconsistent_data = [
    CrossSourceDataPoint({
      source: DataSource::Traces,
      timestamp: 1735689600000,
      trace_id: Some("trace-789"),
      span_id: Some("span-001"),
      correlation_id: Some("corr-abc"),
      service_name: "api-service",
      operation_name: "process_request",
      data: []
    }),
    CrossSourceDataPoint({
      source: DataSource::Traces,
      timestamp: 1735689590000,  // 时间戳更早，乱序
      trace_id: Some("trace-789"),
      span_id: Some("span-002"),
      correlation_id: Some("corr-abc"),
      service_name: "api-service",
      operation_name: "process_request",
      data: []
    }),
    CrossSourceDataPoint({
      source: DataSource::Metrics,
      timestamp: 1735689700000,  // 时间窗口太大
      trace_id: Some("trace-789"),
      span_id: None,
      correlation_id: Some("corr-abc"),
      service_name: "api-service",
      operation_name: "process_request",
      data: []
    }),
    CrossSourceDataPoint({
      source: DataSource::Logs,
      timestamp: 1735689650000,
      trace_id: None,  // 没有trace_id，孤儿记录
      span_id: None,
      correlation_id: None,
      service_name: "different-service",
      operation_name: "different_operation",
      data: []
    })
  ]
  
  let inconsistent_result = validator(inconsistent_data)
  assert_false(inconsistent_result.is_consistent)
  assert_true(inconsistent_result.inconsistencies.length() > 0)
  assert_eq(inconsistent_result.orphaned_records, 1)
}

// 测试5: 业务逻辑验证
test "业务逻辑验证" {
  // 定义业务规则
  type BusinessRule = {
    rule_name: String,
    description: String,
    condition: (TelemetryDataPoint) -> Bool,
    severity: String  // "error", "warning", "info"
  }
  
  // 定义业务验证结果
  type BusinessValidationResult = {
    is_valid: Bool,
    violations: Array<(String, String, String)>,  // (rule_name, severity, message)
    warnings: Array<String>
  }
  
  // 创建业务逻辑验证器
  let business_logic_validator = fn(rules: Array<BusinessRule>) {
    fn(data_points: Array[TelemetryDataPoint>) -> BusinessValidationResult {
      let mut violations = Array::empty()
      let mut warnings = Array::empty()
      
      for data_point in data_points {
        for rule in rules {
          if not rule.condition(data_point) {
            let message = "Business rule violated: " + rule.description
            violations = violations.push((rule.rule_name, rule.severity, message))
            
            if rule.severity == "warning" {
              warnings = warnings.push(message)
            }
          }
        }
      }
      
      let error_violations = violations.filter(fn(v) { v.1 == "error" })
      let is_valid = error_violations.length() == 0
      
      BusinessValidationResult({
        is_valid: is_valid,
        violations: violations,
        warnings: warnings
      })
    }
  }
  
  // 定义业务规则
  let business_rules = [
    {
      rule_name: "cpu_usage_range",
      description: "CPU usage should be between 0 and 100",
      condition: fn(dp: TelemetryDataPoint) {
        if dp.metric_name == "cpu_usage" {
          dp.value >= 0.0 && dp.value <= 100.0
        } else {
          true
        }
      },
      severity: "error"
    },
    {
      rule_name: "memory_usage_range",
      description: "Memory usage should be between 0 and 100",
      condition: fn(dp: TelemetryDataPoint) {
        if dp.metric_name == "memory_usage" {
          dp.value >= 0.0 && dp.value <= 100.0
        } else {
          true
        }
      },
      severity: "error"
    },
    {
      rule_name: "request_rate_limit",
      description: "Request rate should not exceed 10000 per minute",
      condition: fn(dp: TelemetryDataPoint) {
        if dp.metric_name == "request_rate" {
          dp.value <= 10000.0
        } else {
          true
        }
      },
      severity: "warning"
    },
    {
      rule_name: "error_rate_threshold",
      description: "Error rate should not exceed 5%",
      condition: fn(dp: TelemetryDataPoint) {
        if dp.metric_name == "error_rate" {
          dp.value <= 5.0
        } else {
          true
        }
      },
      severity: "error"
    },
    {
      rule_name: "response_time_p95",
      description: "95th percentile response time should be under 1 second",
      condition: fn(dp: TelemetryDataPoint) {
        if dp.metric_name == "response_time_p95" {
          dp.value <= 1000.0
        } else {
          true
        }
      },
      severity: "warning"
    }
  ]
  
  let validator = business_logic_validator(business_rules)
  
  // 测试符合业务规则的数据
  let valid_data = [
    TelemetryDataPoint({
      timestamp: 1735689600000,
      metric_name: "cpu_usage",
      value: 45.2,
      labels: [("host", "server1")],
      data_type: "gauge"
    }),
    TelemetryDataPoint({
      timestamp: 1735689660000,
      metric_name: "memory_usage",
      value: 78.5,
      labels: [("host", "server1")],
      data_type: "gauge"
    }),
    TelemetryDataPoint({
      timestamp: 1735689720000,
      metric_name: "request_rate",
      value: 5000.0,
      labels: [("service", "api")],
      data_type: "gauge"
    }),
    TelemetryDataPoint({
      timestamp: 1735689780000,
      metric_name: "error_rate",
      value: 2.5,
      labels: [("service", "api")],
      data_type: "gauge"
    }),
    TelemetryDataPoint({
      timestamp: 1735689840000,
      metric_name: "response_time_p95",
      value: 850.0,
      labels: [("service", "api")],
      data_type: "gauge"
    })
  ]
  
  let valid_result = validator(valid_data)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.violations.length(), 0)
  
  // 测试违反业务规则的数据
  let invalid_data = [
    TelemetryDataPoint({
      timestamp: 1735689600000,
      metric_name: "cpu_usage",
      value: 105.0,  // 超过100%
      labels: [("host", "server1")],
      data_type: "gauge"
    }),
    TelemetryDataPoint({
      timestamp: 1735689660000,
      metric_name: "memory_usage",
      value: 95.0,
      labels: [("host", "server1")],
      data_type: "gauge"
    }),
    TelemetryDataPoint({
      timestamp: 1735689720000,
      metric_name: "request_rate",
      value: 12000.0,  // 超过10000
      labels: [("service", "api")],
      data_type: "gauge"
    }),
    TelemetryDataPoint({
      timestamp: 1735689780000,
      metric_name: "error_rate",
      value: 8.0,  // 超过5%
      labels: [("service", "api")],
      data_type: "gauge"
    }),
    TelemetryDataPoint({
      timestamp: 1735689840000,
      metric_name: "response_time_p95",
      value: 1500.0,  // 超过1000ms
      labels: [("service", "api")],
      data_type: "gauge"
    })
  ]
  
  let invalid_result = validator(invalid_data)
  assert_false(invalid_result.is_valid)
  assert_eq(invalid_result.violations.length(), 4)  // 除了memory_usage，其他都违反
  
  // 验证具体违规
  let cpu_violation = invalid_result.violations.find(fn(v) { v.0 == "cpu_usage_range" })
  assert_true(cpu_violation.is_some())
  assert_eq(cpu_violation.unwrap().1, "error")
  
  let request_rate_violation = invalid_result.violations.find(fn(v) { v.0 == "request_rate_limit" })
  assert_true(request_rate_violation.is_some())
  assert_eq(request_rate_violation.unwrap().1, "warning")
  
  let error_rate_violation = invalid_result.violations.find(fn(v) { v.0 == "error_rate_threshold" })
  assert_true(error_rate_violation.is_some())
  assert_eq(error_rate_violation.unwrap().1, "error")
  
  let response_time_violation = invalid_result.violations.find(fn(v) { v.0 == "response_time_p95" })
  assert_true(response_time_violation.is_some())
  assert_eq(response_time_violation.unwrap().1, "warning")
  
  // 验证警告
  assert_eq(invalid_result.warnings.length(), 2)  // request_rate和response_time是警告
}