// Azimuth 遥测数据质量保证测试
// 专注于测试遥测数据的质量检查、验证和改进机制

// 测试1: 遥测数据完整性验证
test "遥测数据完整性验证" {
  // 定义数据质量维度
  enum DataQualityDimension {
    Completeness    // 完整性
    Accuracy        // 准确性
    Consistency     // 一致性
    Timeliness      // 及时性
    Validity        // 有效性
    Uniqueness      // 唯一性
  }
  
  // 定义数据质量规则
  type DataQualityRule = {
    name: String,
    dimension: DataQualityDimension,
    description: String,
    severity: String,  // "critical", "warning", "info"
    validation_fn: String -> Bool
  }
  
  // 定义数据质量检查结果
  type DataQualityCheckResult = {
    rule_name: String,
    dimension: DataQualityDimension,
    passed: Bool,
    severity: String,
    message: String,
    details: Map[String, String]
  }
  
  // 定义数据质量报告
  type DataQualityReport = {
    timestamp: Int,
    total_checks: Int,
    passed_checks: Int,
    failed_checks: Int,
    overall_score: Float,  // 0.0 to 1.0
    results: Array[DataQualityCheckResult]
  }
  
  // 创建数据质量检查器
  let create_data_quality_checker = fn() {
    let rules = []
    
    {
      rules,
      
      // 添加质量规则
      add_rule: fn(rule: DataQualityRule) {
        rules = rules.push(rule)
      },
      
      // 检查数据质量
      check_quality: fn(data: String) {
        let results = []
        
        for rule in rules {
          let passed = rule.validation_fn(data)
          let message = if passed {
            "Rule '" + rule.name + "' passed"
          } else {
            "Rule '" + rule.name + "' failed: " + rule.description
          }
          
          let result = {
            rule_name: rule.name,
            dimension: rule.dimension,
            passed: passed,
            severity: rule.severity,
            message: message,
            details: [
              ("rule_name", rule.name),
              ("dimension", match rule.dimension {
                Completeness => "completeness"
                Accuracy => "accuracy"
                Consistency => "consistency"
                Timeliness => "timeliness"
                Validity => "validity"
                Uniqueness => "uniqueness"
              }),
              ("severity", rule.severity)
            ]
          }
          
          results = results.push(result)
        }
        
        let total_checks = results.length()
        let passed_checks = results.filter(fn(result) { result.passed }).length()
        let failed_checks = total_checks - passed_checks
        let overall_score = if total_checks > 0 {
          (passed_checks as Float) / (total_checks as Float)
        } else {
          1.0
        }
        
        {
          timestamp: Time::now(),
          total_checks,
          passed_checks,
          failed_checks,
          overall_score,
          results
        }
      }
    }
  }
  
  // 创建遥测数据生成器
  let create_telemetry_data = fn(complete: Bool, valid: Bool, consistent: Bool) {
    let trace_id = if complete { "trace-" + UUID::v4() } else { "" }
    let span_id = if complete { "span-" + UUID::v4().substring(0, 8) } else { "" }
    let operation_name = if complete { "test-operation" } else { "" }
    let start_time = if valid { Time::now() } else { -1 }
    let end_time = if valid && consistent { start_time + 100 } else { if valid { start_time - 50 } else { -1 } }
    let status = if valid { "ok" } else { "invalid_status" }
    let service_name = if consistent { "test-service" } else { "inconsistent-service" }
    
    JSON::stringify({
      trace_id: trace_id,
      span_id: span_id,
      parent_span_id: None,
      operation_name: operation_name,
      start_time: start_time,
      end_time: end_time,
      status: status,
      service_name: service_name,
      attributes: [
        ("service.name", service_name),
        ("operation.name", operation_name),
        ("http.method", "GET")
      ],
      events: []
    })
  }
  
  // 创建质量检查器并添加规则
  let quality_checker = create_data_quality_checker()
  
  // 添加完整性规则
  quality_checker.add_rule({
    name: "trace_id_not_empty",
    dimension: Completeness,
    description: "Trace ID should not be empty",
    severity: "critical",
    validation_fn: fn(data) {
      let parsed = JSON::parse(data)
      match Map::get(parsed, "trace_id") {
        Some(trace_id) => trace_id.length() > 0
        None => false
      }
    }
  })
  
  quality_checker.add_rule({
    name: "span_id_not_empty",
    dimension: Completeness,
    description: "Span ID should not be empty",
    severity: "critical",
    validation_fn: fn(data) {
      let parsed = JSON::parse(data)
      match Map::get(parsed, "span_id") {
        Some(span_id) => span_id.length() > 0
        None => false
      }
    }
  })
  
  // 添加有效性规则
  quality_checker.add_rule({
    name: "valid_timestamps",
    dimension: Validity,
    description: "Start time and end time should be valid timestamps",
    severity: "critical",
    validation_fn: fn(data) {
      let parsed = JSON::parse(data)
      match (Map::get(parsed, "start_time"), Map::get(parsed, "end_time")) {
        (Some(start_time), Some(end_time)) => {
          start_time > 0 && end_time > 0
        }
        _ => false
      }
    }
  })
  
  // 添加一致性规则
  quality_checker.add_rule({
    name: "time_consistency",
    dimension: Consistency,
    description: "End time should be after start time",
    severity: "warning",
    validation_fn: fn(data) {
      let parsed = JSON::parse(data)
      match (Map::get(parsed, "start_time"), Map::get(parsed, "end_time")) {
        (Some(start_time), Some(end_time)) => {
          end_time > start_time
        }
        _ => false
      }
    }
  })
  
  quality_checker.add_rule({
    name: "service_name_consistency",
    dimension: Consistency,
    description: "Service name should be consistent across attributes",
    severity: "warning",
    validation_fn: fn(data) {
      let parsed = JSON::parse(data)
      match (Map::get(parsed, "service_name"), Map::get(parsed, "attributes")) {
        (Some(service_name), Some(attributes)) => {
          match Map::get(attributes, "service.name") {
            Some(attr_service_name) => service_name == attr_service_name
            None => false
          }
        }
        _ => false
      }
    }
  })
  
  // 测试完整且有效的数据
  let good_data = create_telemetry_data(true, true, true)
  let good_quality_report = quality_checker.check_quality(good_data)
  
  assert_eq(good_quality_report.total_checks, 5)
  assert_eq(good_quality_report.passed_checks, 5)
  assert_eq(good_quality_report.failed_checks, 0)
  assert_eq(good_quality_report.overall_score, 1.0)
  
  for result in good_quality_report.results {
    assert_true(result.passed)
    assert_true(result.message.contains("passed"))
  }
  
  // 测试不完整的数据
  let incomplete_data = create_telemetry_data(false, true, true)
  let incomplete_quality_report = quality_checker.check_quality(incomplete_data)
  
  assert_eq(incomplete_quality_report.total_checks, 5)
  assert_true(incomplete_quality_report.passed_checks < 5)
  assert_true(incomplete_quality_report.failed_checks > 0)
  assert_true(incomplete_quality_report.overall_score < 1.0)
  
  // 验证失败的规则
  let failed_results = incomplete_quality_report.results.filter(fn(result) { not(result.passed) })
  for result in failed_results {
    assert_true(result.message.contains("failed"))
    assert_true(result.details.contains("severity"))
  }
  
  // 测试无效的数据
  let invalid_data = create_telemetry_data(true, false, true)
  let invalid_quality_report = quality_checker.check_quality(invalid_data)
  
  assert_eq(invalid_quality_report.total_checks, 5)
  assert_true(invalid_quality_report.failed_checks > 0)
  
  // 验证关键失败
  let critical_failures = invalid_quality_report.results.filter(fn(result) {
    not(result.passed) && result.severity == "critical"
  })
  assert_true(critical_failures.length() > 0)
  
  // 测试不一致的数据
  let inconsistent_data = create_telemetry_data(true, true, false)
  let inconsistent_quality_report = quality_checker.check_quality(inconsistent_data)
  
  assert_eq(inconsistent_quality_report.total_checks, 5)
  assert_true(inconsistent_quality_report.failed_checks > 0)
  
  // 验证警告级别的失败
  let warning_failures = inconsistent_quality_report.results.filter(fn(result) {
    not(result.passed) && result.severity == "warning"
  })
  assert_true(warning_failures.length() > 0)
}

// 测试2: 遥测数据清洗和修复
test "遥测数据清洗和修复" {
  // 定义数据质量问题类型
  enum DataQualityIssue {
    MissingField
    InvalidValue
    InconsistentFormat
    OutOfRange
    Duplicate
    Corrupted
  }
  
  // 定义数据清洗策略
  enum CleaningStrategy {
    RemoveRecord      // 删除记录
    FillDefault       // 填充默认值
    Interpolate       // 插值
    CorrectFormat     // 修正格式
    RemoveDuplicate   // 去重
    ValidateAndFix    // 验证并修复
  }
  
  // 定义清洗规则
  type CleaningRule = {
    issue_type: DataQualityIssue,
    field_path: String,
    strategy: CleaningStrategy,
    default_value: Option[String],
    validation_fn: String -> Bool,
    correction_fn: String -> String
  }
  
  // 定义清洗结果
  type CleaningResult = {
    original_data: String,
    cleaned_data: String,
    issues_found: Array[DataQualityIssue],
    issues_fixed: Array[DataQualityIssue],
    applied_rules: Array[String]
  }
  
  // 创建数据清洗器
  let create_data_cleaner = fn() {
    let cleaning_rules = []
    
    {
      cleaning_rules,
      
      // 添加清洗规则
      add_rule: fn(rule: CleaningRule) {
        cleaning_rules = cleaning_rules.push(rule)
      },
      
      // 清洗数据
      clean_data: fn(raw_data: String) {
        let mut issues_found = []
        let mut issues_fixed = []
        let mut applied_rules = []
        let mut cleaned_data = raw_data
        
        // 解析数据
        let parsed_data = JSON::parse(cleaned_data)
        
        // 应用清洗规则
        for rule in cleaning_rules {
          let field_value = get_nested_field(parsed_data, rule.field_path)
          
          match field_value {
            Some(value) => {
              let is_valid = rule.validation_fn(value)
              
              if not(is_valid) {
                issues_found = issues_found.push(rule.issue_type)
                
                // 应用清洗策略
                let new_value = match rule.strategy {
                  RemoveRecord => {
                    // 删除整个记录（在字符串操作中简化为返回空对象）
                    "{}"
                  }
                  FillDefault => {
                    match rule.default_value {
                      Some(default_val) => default_val
                      None => value
                    }
                  }
                  Interpolate => {
                    // 简化的插值：对于数值字段，使用合理默认值
                    if rule.field_path.contains("time") {
                      Time::now().to_string()
                    } else if rule.field_path.contains("id") {
                      "default-" + UUID::v4().substring(0, 8)
                    } else {
                      value
                    }
                  }
                  CorrectFormat => {
                    rule.correction_fn(value)
                  }
                  RemoveDuplicate => {
                    // 在单个记录清洗中不适用
                    value
                  }
                  ValidateAndFix => {
                    rule.correction_fn(value)
                  }
                }
                
                // 更新数据
                if rule.strategy != RemoveRecord {
                  cleaned_data = set_nested_field(cleaned_data, rule.field_path, new_value)
                  issues_fixed = issues_fixed.push(rule.issue_type)
                } else {
                  cleaned_data = new_value
                }
                
                applied_rules = applied_rules.push(rule.field_path + ":" + match rule.strategy {
                  RemoveRecord => "remove"
                  FillDefault => "fill_default"
                  Interpolate => "interpolate"
                  CorrectFormat => "correct_format"
                  RemoveDuplicate => "remove_duplicate"
                  ValidateAndFix => "validate_and_fix"
                })
              }
            }
            None => {
              // 字段不存在
              if rule.issue_type == MissingField {
                issues_found = issues_found.push(rule.issue_type)
                
                match rule.strategy {
                  FillDefault => {
                    match rule.default_value {
                      Some(default_val) => {
                        cleaned_data = set_nested_field(cleaned_data, rule.field_path, default_val)
                        issues_fixed = issues_fixed.push(rule.issue_type)
                        applied_rules = applied_rules.push(rule.field_path + ":fill_default")
                      }
                      None => ()
                    }
                  }
                  _ => ()
                }
              }
            }
          }
        }
        
        {
          original_data: raw_data,
          cleaned_data: cleaned_data,
          issues_found: issues_found,
          issues_fixed: issues_fixed,
          applied_rules: applied_rules
        }
      }
    }
  }
  
  // 辅助函数：获取嵌套字段值
  let get_nested_field = fn(data: Map[String, Any], path: String) {
    let path_parts = path.split(".")
    let mut current = data
    
    for part in path_parts {
      match Map::get(current, part) {
        Some(value) => {
          match value {
            Map(nested) => current = nested
            _ => return Some(value.to_string())
          }
        }
        None => return None
      }
    }
    
    None
  }
  
  // 辅助函数：设置嵌套字段值
  let set_nested_field = fn(data: String, path: String, value: String) {
    // 简化实现：直接替换字段值
    let path_parts = path.split(".")
    let field_name = path_parts[path_parts.length() - 1]
    
    // 在实际实现中，这里会正确处理嵌套结构
    if path.contains("trace_id") {
      data.replace("\"trace_id\":\"\"", "\"trace_id\":\"" + value + "\"")
    } else if path.contains("span_id") {
      data.replace("\"span_id\":\"\"", "\"span_id\":\"" + value + "\"")
    } else if path.contains("start_time") {
      data.replace("\"start_time\":-1", "\"start_time\":" + value)
    } else if path.contains("end_time") {
      data.replace("\"end_time\":-1", "\"end_time\":" + value)
    } else if path.contains("status") {
      data.replace("\"status\":\"invalid_status\"", "\"status\":\"" + value + "\"")
    } else {
      data
    }
  }
  
  // 创建有质量问题的测试数据
  let create_problematic_data = fn() {
    JSON::stringify({
      trace_id: "",  // 缺失
      span_id: "",   // 缺失
      parent_span_id: None,
      operation_name: "test-operation",
      start_time: -1,  // 无效值
      end_time: -1,    // 无效值
      status: "invalid_status",  // 无效值
      service_name: "test-service",
      attributes: [
        ("service.name", "test-service"),
        ("operation.name", "test-operation"),
        ("http.method", "GET")
      ],
      events: []
    })
  }
  
  // 创建数据清洗器并添加规则
  let data_cleaner = create_data_cleaner()
  
  // 添加缺失字段处理规则
  data_cleaner.add_rule({
    issue_type: MissingField,
    field_path: "trace_id",
    strategy: FillDefault,
    default_value: Some("generated-" + UUID::v4()),
    validation_fn: fn(value) { value.length() > 0 },
    correction_fn: fn(value) { "generated-" + UUID::v4() }
  })
  
  data_cleaner.add_rule({
    issue_type: MissingField,
    field_path: "span_id",
    strategy: FillDefault,
    default_value: Some("generated-" + UUID::v4().substring(0, 8)),
    validation_fn: fn(value) { value.length() > 0 },
    correction_fn: fn(value) { "generated-" + UUID::v4().substring(0, 8) }
  })
  
  // 添加无效值处理规则
  data_cleaner.add_rule({
    issue_type: InvalidValue,
    field_path: "start_time",
    strategy: Interpolate,
    default_value: None,
    validation_fn: fn(value) { value.to_int() > 0 },
    correction_fn: fn(value) { Time::now().to_string() }
  })
  
  data_cleaner.add_rule({
    issue_type: InvalidValue,
    field_path: "end_time",
    strategy: Interpolate,
    default_value: None,
    validation_fn: fn(value) { value.to_int() > 0 },
    correction_fn: fn(value) { (Time::now() + 100).to_string() }
  })
  
  // 添加状态值修正规则
  data_cleaner.add_rule({
    issue_type: InvalidValue,
    field_path: "status",
    strategy: CorrectFormat,
    default_value: None,
    validation_fn: fn(value) { 
      value == "ok" || value == "error" || value == "timeout" 
    },
    correction_fn: fn(value) { "ok" }
  })
  
  // 测试数据清洗
  let problematic_data = create_problematic_data()
  let cleaning_result = data_cleaner.clean_data(problematic_data)
  
  // 验证清洗结果
  assert_eq(cleaning_result.original_data, problematic_data)
  assert_true(cleaning_result.cleaned_data != problematic_data)
  assert_true(cleaning_result.issues_found.length() > 0)
  assert_true(cleaning_result.issues_fixed.length() > 0)
  assert_true(cleaning_result.applied_rules.length() > 0)
  
  // 验证发现的问题类型
  assert_true(cleaning_result.issues_found.contains(MissingField))
  assert_true(cleaning_result.issues_found.contains(InvalidValue))
  
  // 验证修复的问题类型
  assert_true(cleaning_result.issues_fixed.contains(MissingField))
  assert_true(cleaning_result.issues_fixed.contains(InvalidValue))
  
  // 验证清洗后的数据
  let cleaned_parsed = JSON::parse(cleaning_result.cleaned_data)
  
  match Map::get(cleaned_parsed, "trace_id") {
    Some(trace_id) => assert_true(trace_id.length() > 0)
    None => assert_true(false)
  }
  
  match Map::get(cleaned_parsed, "span_id") {
    Some(span_id) => assert_true(span_id.length() > 0)
    None => assert_true(false)
  }
  
  match Map::get(cleaned_parsed, "start_time") {
    Some(start_time) => assert_true(start_time.to_int() > 0)
    None => assert_true(false)
  }
  
  match Map::get(cleaned_parsed, "status") {
    Some(status) => assert_eq(status, "ok")
    None => assert_true(false)
  }
  
  // 测试已经干净的数据
  let clean_data = create_telemetry_data(true, true, true)
  let clean_result = data_cleaner.clean_data(clean_data)
  
  assert_eq(clean_result.original_data, clean_data)
  assert_eq(clean_result.cleaned_data, clean_data)
  assert_eq(clean_result.issues_found.length(), 0)
  assert_eq(clean_result.issues_fixed.length(), 0)
  assert_eq(clean_result.applied_rules.length(), 0)
}

// 测试3: 遥测数据质量监控和报告
test "遥测数据质量监控和报告" {
  // 定义质量指标
  type QualityMetrics = {
    completeness_score: Float,
    accuracy_score: Float,
    consistency_score: Float,
    timeliness_score: Float,
    validity_score: Float,
    uniqueness_score: Float,
    overall_score: Float
  }
  
  // 定义时间窗口质量统计
  type TimeWindowQualityStats = {
    window_start: Int,
    window_end: Int,
    total_records: Int,
    valid_records: Int,
    quality_metrics: QualityMetrics,
    top_issues: Array[(String, Int)]  // (issue_type, count)
  }
  
  // 定义质量趋势分析
  type QualityTrend = {
    direction: String,  // "improving", "declining", "stable"
    change_rate: Float,
    confidence: Float,
    significant_changes: Array[(Int, Float)]  // (timestamp, score_change)
  }
  
  // 创建质量监控器
  let create_quality_monitor = fn() {
    let quality_history = []
    let current_window_records = []
    let window_start_time = Time::now()
    let window_duration = 60 * 60 * 1000  // 1小时窗口
    
    {
      quality_history,
      current_window_records,
      window_start_time,
      
      // 添加记录到当前窗口
      add_record: fn(record: String, quality_score: Float) {
        current_window_records = current_window_records.push({
          record: record,
          quality_score: quality_score,
          timestamp: Time::now()
        })
      },
      
      // 完成当前时间窗口并生成统计
      complete_window: fn() {
        let window_end_time = Time::now()
        let total_records = current_window_records.length()
        
        if total_records == 0 {
          return None
        }
        
        let valid_records = current_window_records.filter(fn(r) { r.quality_score >= 0.8 }).length()
        
        // 计算质量指标
        let scores = current_window_records.map(fn(r) { r.quality_score })
        let avg_score = scores.reduce(0.0, fn(acc, score) { acc + score }) / (scores.length() as Float)
        
        let quality_metrics = {
          completeness_score: avg_score,  // 简化实现
          accuracy_score: avg_score,
          consistency_score: avg_score,
          timeliness_score: avg_score,
          validity_score: avg_score,
          uniqueness_score: avg_score,
          overall_score: avg_score
        }
        
        // 统计问题类型（简化实现）
        let top_issues = []
        let low_quality_records = current_window_records.filter(fn(r) { r.quality_score < 0.8 })
        if low_quality_records.length() > 0 {
          top_issues = top_issues.push(("low_quality", low_quality_records.length()))
        }
        
        let window_stats = {
          window_start: window_start_time,
          window_end: window_end_time,
          total_records,
          valid_records,
          quality_metrics,
          top_issues
        }
        
        // 添加到历史记录
        quality_history = quality_history.push(window_stats)
        
        // 重置当前窗口
        current_window_records = []
        window_start_time = window_end_time
        
        Some(window_stats)
      },
      
      // 分析质量趋势
      analyze_trend: fn(window_count: Int) {
        if quality_history.length() < 2 {
          return None
        }
        
        // 获取最近的N个窗口
        let recent_windows = if quality_history.length() > window_count {
          quality_history.slice(quality_history.length() - window_count, quality_history.length())
        } else {
          quality_history
        }
        
        // 计算趋势
        let scores = recent_windows.map(fn(w) { w.quality_metrics.overall_score })
        let first_score = scores[0]
        let last_score = scores[scores.length() - 1]
        let change = last_score - first_score
        let change_rate = change / (scores.length() as Float)
        
        // 确定趋势方向
        let direction = if change_rate > 0.01 {
          "improving"
        } else if change_rate < -0.01 {
          "declining"
        } else {
          "stable"
        }
        
        // 计算置信度（简化实现）
        let variance = scores.reduce(0.0, fn(acc, score) { 
          let mean = scores.reduce(0.0, fn(sum, s) { sum + s }) / (scores.length() as Float)
          acc + (score - mean) * (score - mean)
        }) / (scores.length() as Float)
        let confidence = 1.0 - (variance / (change * change + 0.001))
        
        // 识别显著变化
        let significant_changes = []
        for i in 1..scores.length() {
          let change = scores[i] - scores[i-1]
          if Float::abs(change) > 0.1 {  // 超过10%的变化
            significant_changes = significant_changes.push((recent_windows[i].window_start, change))
          }
        }
        
        Some({
          direction,
          change_rate,
          confidence: Float::max(0.0, Float::min(1.0, confidence)),
          significant_changes
        })
      },
      
      // 生成质量报告
      generate_report: fn() {
        if quality_history.length() == 0 {
          return None
        }
        
        let latest_window = quality_history[quality_history.length() - 1]
        let trend_analysis = analyze_trend(10)  // 分析最近10个窗口的趋势
        
        // 计算总体统计
        let all_records = quality_history.reduce(0, fn(acc, w) { acc + w.total_records })
        let all_valid_records = quality_history.reduce(0, fn(acc, w) { acc + w.valid_records })
        let overall_validity_rate = (all_valid_records as Float) / (all_records as Float)
        
        // 汇总所有问题
        let all_issues = Map::empty()
        for window in quality_history {
          for (issue_type, count) in window.top_issues {
            let current_count = match Map::get(all_issues, issue_type) {
              Some(existing) => existing
              None => 0
            }
            all_issues = Map::insert(all_issues, issue_type, current_count + count)
          }
        }
        
        let top_overall_issues = all_issues.map(fn(kv) { kv }).sort(fn(a, b) { b.1 - a.1 })
        
        Some({
          timestamp: Time::now(),
          latest_window_metrics: latest_window.quality_metrics,
          overall_validity_rate: overall_validity_rate,
          total_records_processed: all_records,
          trend_analysis: trend_analysis,
          top_issues: top_overall_issues.slice(0, 5)  // 前5个问题
        })
      }
    }
  }
  
  // 创建质量监控器
  let quality_monitor = create_quality_monitor()
  
  // 模拟添加不同质量的数据记录
  let records_with_quality = [
    ("high_quality_record_1", 0.95),
    ("high_quality_record_2", 0.92),
    ("medium_quality_record_1", 0.85),
    ("low_quality_record_1", 0.65),
    ("medium_quality_record_2", 0.80),
    ("high_quality_record_3", 0.98),
    ("low_quality_record_2", 0.70),
    ("high_quality_record_4", 0.90),
    ("medium_quality_record_3", 0.82),
    ("low_quality_record_3", 0.60)
  ]
  
  for (record, score) in records_with_quality {
    quality_monitor.add_record(record, score)
  }
  
  // 完成时间窗口
  let window_stats = quality_monitor.complete_window()
  match window_stats {
    Some(stats) => {
      assert_eq(stats.total_records, 10)
      assert_eq(stats.valid_records, 6)  // 质量分数 >= 0.8的记录
      assert_true(stats.quality_metrics.overall_score > 0.8)
      assert_true(stats.top_issues.length() > 0)
      
      let low_quality_issue = stats.top_issues.find(fn(issue) { issue.0 == "low_quality" })
      match low_quality_issue {
        Some((_, count)) => assert_eq(count, 3)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 添加第二个时间窗口的数据（质量改善）
  let improved_records = [
    ("high_quality_record_5", 0.96),
    ("high_quality_record_6", 0.94),
    ("high_quality_record_7", 0.91),
    ("medium_quality_record_4", 0.88),
    ("high_quality_record_8", 0.97),
    ("medium_quality_record_5", 0.86),
    ("high_quality_record_9", 0.93),
    ("high_quality_record_10", 0.95)
  ]
  
  for (record, score) in improved_records {
    quality_monitor.add_record(record, score)
  }
  
  // 完成第二个时间窗口
  let second_window_stats = quality_monitor.complete_window()
  match second_window_stats {
    Some(stats) => {
      assert_eq(stats.total_records, 8)
      assert_eq(stats.valid_records, 8)  // 所有记录都是高质量
      assert_true(stats.quality_metrics.overall_score > 0.9)
    }
    None => assert_true(false)
  }
  
  // 分析质量趋势
  let trend_analysis = quality_monitor.analyze_trend(2)
  match trend_analysis {
    Some(trend) => {
      assert_eq(trend.direction, "improving")  // 质量应该改善
      assert_true(trend.change_rate > 0.0)
      assert_true(trend.confidence >= 0.0)
      assert_true(trend.confidence <= 1.0)
    }
    None => assert_true(false)
  }
  
  // 生成质量报告
  let quality_report = quality_monitor.generate_report()
  match quality_report {
    Some(report) => {
      assert_true(report.timestamp > 0)
      assert_true(report.latest_window_metrics.overall_score > 0.0)
      assert_true(report.overall_validity_rate >= 0.0)
      assert_true(report.overall_validity_rate <= 1.0)
      assert_eq(report.total_records_processed, 18)  // 两个窗口的总记录数
      assert_true(report.top_issues.length() > 0)
      
      match report.trend_analysis {
        Some(trend) => {
          assert_eq(trend.direction, "improving")
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}