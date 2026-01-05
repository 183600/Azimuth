// Azimuth 遥测数据质量验证测试
// 测试遥测数据的质量验证功能

// 测试1: 数据完整性验证
test "数据完整性验证测试" {
  // 创建数据质量验证器
  let quality_validator = DataQualityValidator::new()
  
  // 配置验证规则
  QualityValidator::add_rule(quality_validator, {
    name: "required_fields",
    type: "field_presence",
    description: "验证必要字段是否存在",
    fields: ["trace_id", "span_id", "operation_name", "start_time", "end_time"],
    severity: "critical"
  })
  
  QualityValidator::add_rule(quality_validator, {
    name: "time_range",
    type: "value_range",
    description: "验证时间范围是否合理",
    field: "duration",
    min_value: 0,
    max_value: 3600000,  // 最大1小时
    severity: "warning"
  })
  
  // 测试完整数据
  let complete_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    operation_name: "database.query",
    start_time: 1640995200,
    end_time: 1640995250,
    service_name: "payment-service",
    status: "ok"
  }
  
  let complete_result = QualityValidator::validate(quality_validator, complete_data)
  assert_true(complete_result.is_valid)
  assert_eq(complete_result.errors.length(), 0)
  assert_eq(complete_result.warnings.length(), 0)
  
  // 测试缺少必要字段的数据
  let incomplete_data = {
    trace_id: "trace-12345",
    operation_name: "database.query",
    start_time: 1640995200,
    service_name: "payment-service",
    status: "ok"
  }
  
  let incomplete_result = QualityValidator::validate(quality_validator, incomplete_data)
  assert_false(incomplete_result.is_valid)
  assert_true(incomplete_result.errors.length() > 0)
  
  let missing_field_error = incomplete_result.errors.find(fn(e) { 
    e.rule == "required_fields" and e.field == "span_id" 
  })
  assert_true(missing_field_error != None)
  
  // 测试时间范围异常的数据
  let invalid_time_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    operation_name: "database.query",
    start_time: 1640995200,
    end_time: 1640995200 - 100,  // 结束时间早于开始时间
    service_name: "payment-service",
    status: "ok"
  }
  
  let invalid_time_result = QualityValidator::validate(quality_validator, invalid_time_data)
  assert_true(invalid_time_result.warnings.length() > 0)
  
  let time_range_warning = invalid_time_result.warnings.find(fn(w) { 
    w.rule == "time_range" 
  })
  assert_true(time_range_warning != None)
}

// 测试2: 数据一致性验证
test "数据一致性验证测试" {
  // 创建一致性验证器
  let consistency_validator = ConsistencyValidator::new()
  
  // 配置一致性规则
  ConsistencyValidator::add_rule(consistency_validator, {
    name: "parent_child_relationship",
    description: "验证父子Span关系的一致性",
    validation: fn(data) {
      match data.parent_span_id {
        Some(parent_id) => {
          // 如果有父Span，则开始时间应该晚于父Span
          let parent_span = SpanRepository::get_by_id(parent_id)
          match parent_span {
            Some(parent) => data.start_time >= parent.start_time,
            None => false  // 父Span不存在
          }
        }
        None => true  // 根Span，无需验证
      }
    }
  })
  
  ConsistencyValidator::add_rule(consistency_validator, {
    name: "status_consistency",
    description: "验证状态与错误事件的一致性",
    validation: fn(data) {
      let has_error_events = data.events.any(fn(e) { 
        e.name.contains("error") or e.name.contains("exception") 
      })
      
      if has_error_events {
        data.status == "error"
      } else {
        true  // 没有错误事件，状态可以是任何值
      }
    }
  })
  
  // 创建父Span
  let parent_span = {
    trace_id: "trace-12345",
    span_id: "parent-span",
    operation_name: "parent.operation",
    start_time: 1640995200,
    end_time: 1640995300,
    status: "ok"
  }
  SpanRepository::store(parent_span)
  
  // 测试一致的子Span
  let consistent_child = {
    trace_id: "trace-12345",
    span_id: "child-span",
    parent_span_id: Some("parent-span"),
    operation_name: "child.operation",
    start_time: 1640995250,  // 晚于父Span开始时间
    end_time: 1640995270,
    status: "ok",
    events: []
  }
  
  let consistent_result = ConsistencyValidator::validate(consistency_validator, consistent_child)
  assert_true(consistent_result.is_consistent)
  assert_eq(consistent_result.inconsistencies.length(), 0)
  
  // 测试时间不一致的子Span
  let time_inconsistent_child = {
    trace_id: "trace-12345",
    span_id: "time-inconsistent-child",
    parent_span_id: Some("parent-span"),
    operation_name: "time.inconsistent.child",
    start_time: 1640995100,  // 早于父Span开始时间
    end_time: 1640995150,
    status: "ok",
    events: []
  }
  
  let time_inconsistent_result = ConsistencyValidator::validate(consistency_validator, time_inconsistent_child)
  assert_false(time_inconsistent_result.is_consistent)
  assert_true(time_inconsistent_result.inconsistencies.length() > 0)
  
  let time_inconsistency = time_inconsistent_result.inconsistencies.find(fn(i) { 
    i.rule == "parent_child_relationship" 
  })
  assert_true(time_inconsistency != None)
  
  // 测试状态不一致的Span
  let status_inconsistent_span = {
    trace_id: "trace-12345",
    span_id: "status-inconsistent-span",
    operation_name: "status.inconsistent.operation",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",  // 状态为ok
    events: [
      {
        name: "error.exception",
        timestamp: 1640995230,
        attributes: [("error.type", "NullPointerException"), ("error.message", "Null pointer")]
      }
    ]
  }
  
  let status_inconsistent_result = ConsistencyValidator::validate(consistency_validator, status_inconsistent_span)
  assert_false(status_inconsistent_result.is_consistent)
  
  let status_inconsistency = status_inconsistent_result.inconsistencies.find(fn(i) { 
    i.rule == "status_consistency" 
  })
  assert_true(status_inconsistency != None)
}

// 测试3: 数据格式验证
test "数据格式验证测试" {
  // 创建格式验证器
  let format_validator = FormatValidator::new()
  
  // 配置格式规则
  FormatValidator::add_rule(format_validator, {
    name: "trace_id_format",
    field: "trace_id",
    pattern: "trace-[a-f0-9]{8}",
    description: "Trace ID格式应为trace-后跟8位十六进制字符"
  })
  
  FormatValidator::add_rule(format_validator, {
    name: "span_id_format",
    field: "span_id",
    pattern: "span-[a-f0-9]{8}",
    description: "Span ID格式应为span-后跟8位十六进制字符"
  })
  
  FormatValidator::add_rule(format_validator, {
    name: "timestamp_format",
    field: "start_time",
    type: "timestamp",
    description: "时间戳应为Unix时间戳格式"
  })
  
  FormatValidator::add_rule(format_validator, {
    name: "service_name_format",
    field: "service_name",
    pattern: "[a-z][a-z0-9-]*",
    description: "服务名应以小写字母开头，只包含小写字母、数字和连字符"
  })
  
  // 测试格式正确的数据
  let valid_format_data = {
    trace_id: "trace-12345678",
    span_id: "span-abcdef01",
    operation_name: "database.query",
    start_time: 1640995200,
    end_time: 1640995250,
    service_name: "payment-service",
    status: "ok"
  }
  
  let valid_format_result = FormatValidator::validate(format_validator, valid_format_data)
  assert_true(valid_format_result.is_valid)
  assert_eq(valid_format_result.errors.length(), 0)
  
  // 测试Trace ID格式错误的数据
  let invalid_trace_id_data = {
    trace_id: "invalid-trace-id",
    span_id: "span-abcdef01",
    operation_name: "database.query",
    start_time: 1640995200,
    end_time: 1640995250,
    service_name: "payment-service",
    status: "ok"
  }
  
  let invalid_trace_id_result = FormatValidator::validate(format_validator, invalid_trace_id_data)
  assert_false(invalid_trace_id_result.is_valid)
  
  let trace_id_error = invalid_trace_id_result.errors.find(fn(e) { 
    e.field == "trace_id" and e.rule == "trace_id_format" 
  })
  assert_true(trace_id_error != None)
  
  // 测试服务名格式错误的数据
  let invalid_service_name_data = {
    trace_id: "trace-12345678",
    span_id: "span-abcdef01",
    operation_name: "database.query",
    start_time: 1640995200,
    end_time: 1640995250,
    service_name: "Invalid-Service-Name",  // 包含大写字母
    status: "ok"
  }
  
  let invalid_service_name_result = FormatValidator::validate(format_validator, invalid_service_name_data)
  assert_false(invalid_service_name_result.is_valid)
  
  let service_name_error = invalid_service_name_result.errors.find(fn(e) { 
    e.field == "service_name" and e.rule == "service_name_format" 
  })
  assert_true(service_name_error != None)
  
  // 测试时间戳格式错误的数据
  let invalid_timestamp_data = {
    trace_id: "trace-12345678",
    span_id: "span-abcdef01",
    operation_name: "database.query",
    start_time: "invalid-timestamp",  // 字符串而非数字
    end_time: 1640995250,
    service_name: "payment-service",
    status: "ok"
  }
  
  let invalid_timestamp_result = FormatValidator::validate(format_validator, invalid_timestamp_data)
  assert_false(invalid_timestamp_result.is_valid)
  
  let timestamp_error = invalid_timestamp_result.errors.find(fn(e) { 
    e.field == "start_time" and e.rule == "timestamp_format" 
  })
  assert_true(timestamp_error != None)
}

// 测试4: 数据业务逻辑验证
test "数据业务逻辑验证测试" {
  // 创建业务逻辑验证器
  let business_validator = BusinessLogicValidator::new()
  
  // 配置业务规则
  BusinessLogicValidator::add_rule(business_validator, {
    name: "http_status_code_logic",
    description: "验证HTTP状态码与逻辑的一致性",
    validation: fn(data) {
      match data.attributes.find(fn(a) { a[0] == "http.status_code" }) {
        Some((_, status_str)) => {
          let status = status_str.to_int()
          match data.status {
            "ok" => status >= 200 and status < 400,
            "error" => status >= 400,
            _ => false
          }
        }
        None => true  // 没有HTTP状态码，无需验证
      }
    }
  })
  
  BusinessLogicValidator::add_rule(business_validator, {
    name: "database_operation_logic",
    description: "验证数据库操作的业务逻辑",
    validation: fn(data) {
      if data.operation_name.contains("select") or data.operation_name.contains("query") {
        // 查询操作应该有相应的结果属性
        data.attributes.any(fn(a) { 
          a[0] == "db.rows_read" or a[0] == "db.result_count" 
        })
      } else if data.operation_name.contains("insert") or data.operation_name.contains("update") or data.operation_name.contains("delete") {
        // 修改操作应该有影响的行数
        data.attributes.any(fn(a) { 
          a[0] == "db.rows_affected" 
        })
      } else {
        true  // 其他操作无需验证
      }
    }
  })
  
  // 测试HTTP状态码逻辑正确的数据
  let valid_http_data = {
    trace_id: "trace-12345678",
    span_id: "span-abcdef01",
    operation_name: "http.request",
    start_time: 1640995200,
    end_time: 1640995250,
    service_name: "api.gateway",
    status: "ok",
    attributes: [
      ("http.method", "GET"),
      ("http.status_code", "200"),
      ("http.url", "/api/users")
    ]
  }
  
  let valid_http_result = BusinessLogicValidator::validate(business_validator, valid_http_data)
  assert_true(valid_http_result.is_valid)
  assert_eq(valid_http_result.violations.length(), 0)
  
  // 测试HTTP状态码逻辑错误的数据
  let invalid_http_data = {
    trace_id: "trace-12345678",
    span_id: "span-abcdef02",
    operation_name: "http.request",
    start_time: 1640995200,
    end_time: 1640995250,
    service_name: "api.gateway",
    status: "ok",  // 状态为ok
    attributes: [
      ("http.method", "GET"),
      ("http.status_code", "500"),  // 但状态码为500
      ("http.url", "/api/users")
    ]
  }
  
  let invalid_http_result = BusinessLogicValidator::validate(business_validator, invalid_http_data)
  assert_false(invalid_http_result.is_valid)
  
  let http_violation = invalid_http_result.violations.find(fn(v) { 
    v.rule == "http_status_code_logic" 
  })
  assert_true(http_violation != None)
  
  // 测试数据库操作逻辑正确的数据
  let valid_db_data = {
    trace_id: "trace-12345678",
    span_id: "span-abcdef03",
    operation_name: "select.users",
    start_time: 1640995200,
    end_time: 1640995250,
    service_name: "user-service",
    status: "ok",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users"),
      ("db.rows_read", "10")
    ]
  }
  
  let valid_db_result = BusinessLogicValidator::validate(business_validator, valid_db_data)
  assert_true(valid_db_result.is_valid)
  assert_eq(valid_db_result.violations.length(), 0)
  
  // 测试数据库操作逻辑错误的数据
  let invalid_db_data = {
    trace_id: "trace-12345678",
    span_id: "span-abcdef04",
    operation_name: "select.users",
    start_time: 1640995200,
    end_time: 1640995250,
    service_name: "user-service",
    status: "ok",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users")
      // 缺少db.rows_read或db.result_count
    ]
  }
  
  let invalid_db_result = BusinessLogicValidator::validate(business_validator, invalid_db_data)
  assert_false(invalid_db_result.is_valid)
  
  let db_violation = invalid_db_result.violations.find(fn(v) { 
    v.rule == "database_operation_logic" 
  })
  assert_true(db_violation != None)
}

// 测试5: 数据质量评分
test "数据质量评分测试" {
  // 创建质量评分器
  let quality_scorer = DataQualityScorer::new()
  
  // 配置评分规则
  QualityScorer::add_completeness_rule(quality_scorer, {
    field: "trace_id",
    weight: 0.2,
    required: true
  })
  
  QualityScorer::add_completeness_rule(quality_scorer, {
    field: "span_id",
    weight: 0.2,
    required: true
  })
  
  QualityScorer::add_completeness_rule(quality_scorer, {
    field: "service_name",
    weight: 0.1,
    required: true
  })
  
  QualityScorer::add_accuracy_rule(quality_scorer, {
    field: "start_time",
    weight: 0.15,
    validation: fn(value) { value > 0 and value < 4102444800 }  // 合理的时间戳范围
  })
  
  QualityScorer::add_consistency_rule(quality_scorer, {
    name: "time_consistency",
    weight: 0.2,
    validation: fn(data) { data.start_time <= data.end_time }
  })
  
  QualityScorer::add_validity_rule(quality_scorer, {
    field: "status",
    weight: 0.15,
    valid_values: ["ok", "error", "timeout"]
  })
  
  // 测试高质量数据
  let high_quality_data = {
    trace_id: "trace-12345678",
    span_id: "span-abcdef01",
    operation_name: "database.query",
    start_time: 1640995200,
    end_time: 1640995250,
    service_name: "payment-service",
    status: "ok",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM orders")
    ]
  }
  
  let high_quality_score = QualityScorer::score(quality_scorer, high_quality_data)
  assert_true(high_quality_score.overall >= 0.9)  // 至少90%分数
  assert_eq(high_quality_score.completeness, 1.0)  // 100%完整性
  assert_eq(high_quality_score.accuracy, 1.0)      // 100%准确性
  assert_eq(high_quality_score.consistency, 1.0)   // 100%一致性
  assert_eq(high_quality_score.validity, 1.0)      // 100%有效性
  
  // 测试中等质量数据
  let medium_quality_data = {
    trace_id: "trace-12345678",
    span_id: "span-abcdef01",
    operation_name: "database.query",
    start_time: 1640995200,
    end_time: 1640995250,
    service_name: "payment-service",
    status: "unknown",  // 无效状态
    attributes: [
      ("db.type", "postgresql")
    ]
  }
  
  let medium_quality_score = QualityScorer::score(quality_scorer, medium_quality_data)
  assert_true(medium_quality_score.overall >= 0.7 and medium_quality_score.overall < 0.9)
  assert_eq(medium_quality_score.completeness, 1.0)  // 100%完整性
  assert_eq(medium_quality_score.accuracy, 1.0)      // 100%准确性
  assert_eq(medium_quality_score.consistency, 1.0)   // 100%一致性
  assert_true(medium_quality_score.validity < 1.0)   // 有效性不足100%
  
  // 测试低质量数据
  let low_quality_data = {
    trace_id: "trace-12345678",
    operation_name: "database.query",
    start_time: 1640995200,
    end_time: 1640995100,  // 结束时间早于开始时间
    service_name: "payment-service",
    status: "unknown",  // 无效状态
    attributes: []
  }
  
  let low_quality_score = QualityScorer::score(quality_scorer, low_quality_data)
  assert_true(low_quality_score.overall < 0.7)
  assert_true(low_quality_score.completeness < 1.0)  // 完整性不足100%（缺少span_id）
  assert_eq(low_quality_score.accuracy, 1.0)         // 100%准确性
  assert_true(low_quality_score.consistency < 1.0)   // 一致性不足100%
  assert_true(low_quality_score.validity < 1.0)      // 有效性不足100%
  
  // 测试批量数据质量评分
  let batch_data = [high_quality_data, medium_quality_data, low_quality_data]
  let batch_scores = QualityScorer::score_batch(quality_scorer, batch_data)
  
  assert_eq(batch_scores.length(), 3)
  assert_eq(batch_scores[0].overall, high_quality_score.overall)
  assert_eq(batch_scores[1].overall, medium_quality_score.overall)
  assert_eq(batch_scores[2].overall, low_quality_score.overall)
  
  // 计算平均质量分数
  let average_score = batch_scores.reduce(fn(acc, score) { acc + score.overall }, 0.0) / batch_scores.length().to_float()
  assert_true(average_score >= 0.7 and average_score <= 0.9)
  
  // 计算质量分布
  let quality_distribution = QualityScorer::analyze_distribution(batch_scores)
  assert_eq(quality_distribution.high_quality_count, 1)
  assert_eq(quality_distribution.medium_quality_count, 1)
  assert_eq(quality_distribution.low_quality_count, 1)
  assert_eq(quality_distribution.total_count, 3)
}

// 测试6: 数据质量改进建议
test "数据质量改进建议测试" {
  // 创建质量改进建议器
  let quality_advisor = DataQualityAdvisor::new()
  
  // 配置建议规则
  QualityAdvisor::add_suggestion_rule(quality_advisor, {
    name: "missing_trace_id",
    condition: fn(data) { data.trace_id == "" or data.trace_id == None },
    suggestion: "添加Trace ID以实现分布式追踪",
    priority: "high",
    implementation: "使用分布式追踪上下文注入Trace ID"
  })
  
  QualityAdvisor::add_suggestion_rule(quality_advisor, {
    name: "missing_service_name",
    condition: fn(data) { data.service_name == "" or data.service_name == None },
    suggestion: "添加服务名以便于服务级别监控",
    priority: "high",
    implementation: "在服务启动时配置服务名并在遥测数据中包含"
  })
  
  QualityAdvisor::add_suggestion_rule(quality_advisor, {
    name: "inconsistent_timestamps",
    condition: fn(data) { data.start_time > data.end_time },
    suggestion: "修复时间戳不一致问题",
    priority: "medium",
    implementation: "确保使用统一的时钟源或进行时钟同步"
  })
  
  QualityAdvisor::add_suggestion_rule(quality_advisor, {
    name: "missing_operation_name",
    condition: fn(data) { data.operation_name == "" or data.operation_name == None },
    suggestion: "添加操作名以便于性能分析",
    priority: "medium",
    implementation: "在关键操作开始时设置有意义的操作名"
  })
  
  // 测试需要改进的数据
  let needs_improvement_data = {
    trace_id: "",  // 缺少Trace ID
    span_id: "span-abcdef01",
    operation_name: "",  // 缺少操作名
    start_time: 1640995250,
    end_time: 1640995200,  // 时间戳不一致
    service_name: "",  // 缺少服务名
    status: "ok",
    attributes: []
  }
  
  let suggestions = QualityAdvisor::generate_suggestions(quality_advisor, needs_improvement_data)
  
  // 验证建议数量和优先级
  assert_eq(suggestions.length(), 4)
  
  let high_priority_suggestions = suggestions.filter(fn(s) { s.priority == "high" })
  let medium_priority_suggestions = suggestions.filter(fn(s) { s.priority == "medium" })
  
  assert_eq(high_priority_suggestions.length(), 2)
  assert_eq(medium_priority_suggestions.length(), 2)
  
  // 验证具体建议
  let trace_id_suggestion = suggestions.find(fn(s) { s.name == "missing_trace_id" })
  assert_true(trace_id_suggestion != None)
  
  match trace_id_suggestion {
    Some(suggestion) => {
      assert_eq(suggestion.suggestion, "添加Trace ID以实现分布式追踪")
      assert_eq(suggestion.implementation, "使用分布式追踪上下文注入Trace ID")
    }
    None => assert_true(false)
  }
  
  let service_name_suggestion = suggestions.find(fn(s) { s.name == "missing_service_name" })
  assert_true(service_name_suggestion != None)
  
  let timestamp_suggestion = suggestions.find(fn(s) { s.name == "inconsistent_timestamps" })
  assert_true(timestamp_suggestion != None)
  
  let operation_name_suggestion = suggestions.find(fn(s) { s.name == "missing_operation_name" })
  assert_true(operation_name_suggestion != None)
  
  // 测试批量数据质量改进建议
  let batch_data = [
    needs_improvement_data,
    {
      trace_id: "trace-12345678",
      span_id: "span-abcdef02",
      operation_name: "database.query",
      start_time: 1640995200,
      end_time: 1640995250,
      service_name: "",  // 缺少服务名
      status: "ok",
      attributes: []
    },
    {
      trace_id: "trace-12345678",
      span_id: "span-abcdef03",
      operation_name: "http.request",
      start_time: 1640995200,
      end_time: 1640995250,
      service_name: "api.gateway",
      status: "ok",
      attributes: []
    }
  ]
  
  let batch_suggestions = QualityAdvisor::generate_batch_suggestions(quality_advisor, batch_data)
  
  // 验证批量建议
  assert_true(batch_suggestions.length() > 0)
  
  // 验证建议频率分析
  let suggestion_frequency = QualityAdvisor::analyze_suggestion_frequency(batch_suggestions)
  
  let missing_service_name_frequency = suggestion_frequency.get("missing_service_name")
  assert_true(missing_service_name_frequency != None)
  match missing_service_name_frequency {
    Some(freq) => assert_eq(freq, 2),  // 两个数据项缺少服务名
    None => assert_true(false)
  }
  
  let missing_trace_id_frequency = suggestion_frequency.get("missing_trace_id")
  assert_true(missing_trace_id_frequency != None)
  match missing_trace_id_frequency {
    Some(freq) => assert_eq(freq, 1),  // 一个数据项缺少Trace ID
    None => assert_true(false)
  }
  
  // 生成改进优先级报告
  let priority_report = QualityAdvisor::generate_priority_report(batch_suggestions)
  
  assert_eq(priority_report.high_priority_count, 3)
  assert_eq(priority_report.medium_priority_count, 2)
  assert_eq(priority_report.total_suggestions, 5)
  
  // 验证最高优先级建议
  assert_eq(priority_report.top_suggestions[0].name, "missing_trace_id")  // 高优先级
  assert_eq(priority_report.top_suggestions[1].name, "missing_service_name")  // 高优先级
}

// 测试7: 数据质量趋势分析
test "数据质量趋势分析测试" {
  // 创建质量趋势分析器
  let trend_analyzer = QualityTrendAnalyzer::new()
  
  // 模拟历史质量数据
  let historical_quality_data = []
  let base_time = 1640995200  // 2022-01-01
  
  for day in 0..30 {
    let day_score = 0.7 + (day.to_float() / 30.0) * 0.25  // 从70%逐渐提升到95%
    let daily_score = {
      timestamp: base_time + day * 86400,  // 每天一个数据点
      overall_score: day_score,
      completeness: 0.8 + (day.to_float() / 30.0) * 0.2,
      accuracy: 0.9 + (day.to_float() / 30.0) * 0.1,
      consistency: 0.7 + (day.to_float() / 30.0) * 0.25,
      validity: 0.85 + (day.to_float() / 30.0) * 0.15,
      data_point_count: 1000 + day * 100
    }
    
    historical_quality_data = historical_quality_data.push(daily_score)
  }
  
  // 分析质量趋势
  let trend_analysis = TrendAnalyzer::analyze_trend(trend_analyzer, historical_quality_data)
  
  // 验证趋势分析结果
  assert_true(trend_analysis.overall_trend == "improving")
  assert_true(trend_analysis.overall_change > 0.2)  // 整体提升超过20%
  
  // 验证各维度趋势
  assert_eq(trend_analysis.completeness_trend, "improving")
  assert_eq(trend_analysis.accuracy_trend, "stable")
  assert_eq(trend_analysis.consistency_trend, "improving")
  assert_eq(trend_analysis.validity_trend, "stable")
  
  // 验证趋势统计
  assert_true(trend_analysis.average_improvement_rate > 0.01)  // 平均每天改善超过1%
  assert_true(trend_analysis.best_day_score > 0.9)  // 最好的一天超过90%
  assert_true(trend_analysis.worst_day_score < 0.8)  // 最差的一天低于80%
  
  // 预测未来质量趋势
  let future_prediction = TrendAnalyzer::predict_future_quality(trend_analyzer, historical_quality_data, 7)  // 预测7天
  
  assert_eq(future_prediction.predictions.length(), 7)
  
  // 验证预测值合理
  let last_historical_score = historical_quality_data[historical_quality_data.length() - 1].overall_score
  let first_predicted_score = future_prediction.predictions[0].score
  
  assert_true(first_predicted_score >= last_historical_score)  // 预测值应不低于最后一个历史值
  
  // 验证预测置信度
  assert_true(future_prediction.confidence_level > 0.7)  // 置信度应超过70%
  
  // 分析质量异常点
  let anomaly_analysis = TrendAnalyzer::detect_quality_anomalies(trend_analyzer, historical_quality_data)
  
  // 由于我们创建的是平滑递增的数据，应该没有异常点
  assert_eq(anomaly_analysis.anomalies.length(), 0)
  
  // 添加一些异常点并重新分析
  let data_with_anomalies = historical_quality_data.map(fn(score, index) {
    if index == 10 {
      { score | overall_score: 0.4 }  // 第10天异常低分
    } else if index == 20 {
      { score | overall_score: 1.0 }  // 第20天异常高分
    } else {
      score
    }
  })
  
  let anomaly_analysis_with_anomalies = TrendAnalyzer::detect_quality_anomalies(trend_analyzer, data_with_anomalies)
  
  assert_eq(anomaly_analysis_with_anomalies.anomalies.length(), 2)
  
  // 验证异常点详情
  let low_anomaly = anomaly_analysis_with_anomalies.anomalies.find(fn(a) { a.type == "low" })
  assert_true(low_anomaly != None)
  
  match low_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.day_index, 10)
      assert_true(anomaly.severity == "high")
    }
    None => assert_true(false)
  }
  
  let high_anomaly = anomaly_analysis_with_anomalies.anomalies.find(fn(a) { a.type == "high" })
  assert_true(high_anomaly != None)
  
  match high_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.day_index, 20)
      assert_true(anomaly.severity == "medium")  // 高分异常严重性较低
    }
    None => assert_true(false)
  }
  
  // 生成质量改进建议
  let improvement_recommendations = TrendAnalyzer::generate_improvement_recommendations(trend_analyzer, trend_analysis, anomaly_analysis_with_anomalies)
  
  assert_true(improvement_recommendations.length() > 0)
  
  // 验证建议内容
  let trend_recommendation = improvement_recommendations.find(fn(r) { r.category == "trend" })
  assert_true(trend_recommendation != None)
  
  let anomaly_recommendation = improvement_recommendations.find(fn(r) { r.category == "anomaly" })
  assert_true(anomaly_recommendation != None)
}