// Azimuth 遥测数据质量验证测试用例
// 专注于确保遥测数据的完整性、准确性和一致性

// 测试1: 数据完整性验证
test "遥测数据完整性验证测试" {
  // 创建数据质量验证器
  let data_validator = DataQualityValidator::new()
  
  // 配置完整性验证规则
  DataQualityValidator::add_integrity_rule(data_validator, {
    name: "required_fields",
    description: "验证必填字段是否存在",
    fields: ["trace_id", "span_id", "timestamp", "service_name"],
    severity: "error"
  })
  
  DataQualityValidator::add_integrity_rule(data_validator, {
    name: "timestamp_range",
    description: "验证时间戳是否在合理范围内",
    field: "timestamp",
    min_value: 1600000000,  // 2020年之前的时间戳无效
    max_value: 2000000000,  // 2033年之后的时间戳无效
    severity: "warning"
  })
  
  // 创建有效测试数据
  let valid_telemetry_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-11111"),
    timestamp: 1640995200,
    service_name: "payment.service",
    operation_name: "process_payment",
    duration: 250,
    status: "ok",
    attributes: [
      ("http.method", "POST"),
      ("http.status_code", "200"),
      ("user.id", "user-123")
    ]
  }
  
  // 验证有效数据
  let valid_result = DataQualityValidator::validate(data_validator, valid_telemetry_data)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // 创建无效测试数据（缺少必填字段）
  let invalid_data_missing_fields = {
    trace_id: "trace-12345",
    // 缺少span_id
    timestamp: 1640995200,
    service_name: "payment.service",
    operation_name: "process_payment",
    duration: 250,
    status: "ok",
    attributes: []
  }
  
  // 验证无效数据
  let invalid_result = DataQualityValidator::validate(data_validator, invalid_data_missing_fields)
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.errors.length() > 0)
  
  // 检查具体的错误信息
  let missing_field_error = invalid_result.errors.find(fn(e) { 
    e.rule == "required_fields" and e.field == "span_id" 
  })
  assert_true(missing_field_error != None)
  
  // 创建时间戳无效的数据
  let invalid_timestamp_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-11111"),
    timestamp: 1500000000,  // 无效时间戳
    service_name: "payment.service",
    operation_name: "process_payment",
    duration: 250,
    status: "ok",
    attributes: []
  }
  
  // 验证时间戳无效的数据
  let timestamp_result = DataQualityValidator::validate(data_validator, invalid_timestamp_data)
  assert_false(timestamp_result.is_valid)
  
  let timestamp_error = timestamp_result.errors.find(fn(e) { 
    e.rule == "timestamp_range" and e.field == "timestamp" 
  })
  assert_true(timestamp_error != None)
}

// 测试2: 数据准确性验证
test "遥测数据准确性验证测试" {
  // 创建数据准确性验证器
  let accuracy_validator = DataAccuracyValidator::new()
  
  // 配置准确性验证规则
  AccuracyValidator::add_numeric_range_rule(accuracy_validator, {
    field: "duration",
    min_value: 0,
    max_value: 3600000,  // 最大1小时
    unit: "milliseconds"
  })
  
  AccuracyValidator::add_enum_rule(accuracy_validator, {
    field: "status",
    allowed_values: ["ok", "error", "timeout", "cancelled"]
  })
  
  AccuracyValidator::add_pattern_rule(accuracy_validator, {
    field: "trace_id",
    pattern: "trace-[a-zA-Z0-9-]+",
    description: "Trace ID必须符合格式要求"
  })
  
  AccuracyValidator::add_pattern_rule(accuracy_validator, {
    field: "span_id",
    pattern: "span-[a-zA-Z0-9-]+",
    description: "Span ID必须符合格式要求"
  })
  
  // 创建准确的数据
  let accurate_data = {
    trace_id: "trace-abc123",
    span_id: "span-def456",
    timestamp: 1640995200,
    service_name: "auth.service",
    operation_name: "authenticate",
    duration: 150,  // 在范围内
    status: "ok",   // 允许的值
    attributes: []
  }
  
  // 验证准确数据
  let accurate_result = AccuracyValidator::validate(accuracy_validator, accurate_data)
  assert_true(accurate_result.is_valid)
  assert_eq(accurate_result.errors.length(), 0)
  
  // 创建持续时间无效的数据
  let invalid_duration_data = {
    trace_id: "trace-abc123",
    span_id: "span-def456",
    timestamp: 1640995200,
    service_name: "auth.service",
    operation_name: "authenticate",
    duration: -50,  // 负数持续时间
    status: "ok",
    attributes: []
  }
  
  // 验证持续时间无效的数据
  let duration_result = AccuracyValidator::validate(accuracy_validator, invalid_duration_data)
  assert_false(duration_result.is_valid)
  
  let duration_error = duration_result.errors.find(fn(e) { 
    e.field == "duration" and e.type == "range_violation" 
  })
  assert_true(duration_error != None)
  
  // 创建状态无效的数据
  let invalid_status_data = {
    trace_id: "trace-abc123",
    span_id: "span-def456",
    timestamp: 1640995200,
    service_name: "auth.service",
    operation_name: "authenticate",
    duration: 150,
    status: "unknown_status",  // 不允许的状态
    attributes: []
  }
  
  // 验证状态无效的数据
  let status_result = AccuracyValidator::validate(accuracy_validator, invalid_status_data)
  assert_false(status_result.is_valid)
  
  let status_error = status_result.errors.find(fn(e) { 
    e.field == "status" and e.type == "enum_violation" 
  })
  assert_true(status_error != None)
  
  // 创建ID格式无效的数据
  let invalid_pattern_data = {
    trace_id: "invalid_trace_id",  // 不符合模式
    span_id: "span-def456",
    timestamp: 1640995200,
    service_name: "auth.service",
    operation_name: "authenticate",
    duration: 150,
    status: "ok",
    attributes: []
  }
  
  // 验证ID格式无效的数据
  let pattern_result = AccuracyValidator::validate(accuracy_validator, invalid_pattern_data)
  assert_false(pattern_result.is_valid)
  
  let pattern_error = pattern_result.errors.find(fn(e) { 
    e.field == "trace_id" and e.type == "pattern_violation" 
  })
  assert_true(pattern_error != None)
}

// 测试3: 数据一致性验证
test "遥测数据一致性验证测试" {
  // 创建数据一致性验证器
  let consistency_validator = DataConsistencyValidator::new()
  
  // 配置一致性验证规则
  ConsistencyValidator::add_parent_child_rule(consistency_validator, {
    description: "验证父子span的时间一致性",
    rules: [
      { check: "parent_start_before_child_start", enabled: true },
      { check: "child_end_before_parent_end", enabled: true },
      { check: "child_duration_within_parent", enabled: true }
    ]
  })
  
  ConsistencyValidator::add_trace_continuity_rule(consistency_validator, {
    description: "验证追踪链路的连续性",
    max_gap_duration: 60000  // 最大60秒间隔
  })
  
  // 创建一致的追踪数据
  let parent_span = {
    trace_id: "trace-consistency",
    span_id: "parent-span",
    parent_span_id: None,
    start_time: 1640995200,
    end_time: 1640995250,
    service_name: "api.gateway",
    operation_name: "handle_request",
    status: "ok"
  }
  
  let child_span = {
    trace_id: "trace-consistency",
    span_id: "child-span",
    parent_span_id: Some("parent-span"),
    start_time: 1640995210,  // 在父span开始之后
    end_time: 1640995230,    // 在父span结束之前
    service_name: "auth.service",
    operation_name: "authenticate",
    status: "ok"
  }
  
  let grandchild_span = {
    trace_id: "trace-consistency",
    span_id: "grandchild-span",
    parent_span_id: Some("child-span"),
    start_time: 1640995215,  // 在父span开始之后
    end_time: 1640995225,    // 在父span结束之前
    service_name: "database.service",
    operation_name: "query_user",
    status: "ok"
  }
  
  // 验证一致的追踪数据
  let trace_data = [parent_span, child_span, grandchild_span]
  let consistency_result = ConsistencyValidator::validate_trace(consistency_validator, trace_data)
  assert_true(consistency_result.is_consistent)
  assert_eq(consistency_result.violations.length(), 0)
  
  // 创建时间不一致的数据（子span在父span开始之前）
  let inconsistent_child_span = {
    trace_id: "trace-inconsistency",
    span_id: "child-span",
    parent_span_id: Some("parent-span"),
    start_time: 1640995190,  // 在父span开始之前
    end_time: 1640995230,
    service_name: "auth.service",
    operation_name: "authenticate",
    status: "ok"
  }
  
  let inconsistent_parent_span = {
    trace_id: "trace-inconsistency",
    span_id: "parent-span",
    parent_span_id: None,
    start_time: 1640995200,
    end_time: 1640995250,
    service_name: "api.gateway",
    operation_name: "handle_request",
    status: "ok"
  }
  
  // 验证时间不一致的数据
  let inconsistent_trace = [inconsistent_parent_span, inconsistent_child_span]
  let inconsistent_result = ConsistencyValidator::validate_trace(consistency_validator, inconsistent_trace)
  assert_false(inconsistent_result.is_consistent)
  assert_true(inconsistent_result.violations.length() > 0)
  
  let timing_violation = inconsistent_result.violations.find(fn(v) { 
    v.type == "parent_child_timing" 
  })
  assert_true(timing_violation != None)
  
  // 创建追踪链路不连续的数据
  let span1 = {
    trace_id: "trace-discontinuous",
    span_id: "span-1",
    parent_span_id: None,
    start_time: 1640995200,
    end_time: 1640995210,
    service_name: "service.a",
    operation_name: "operation.a",
    status: "ok"
  }
  
  let span2 = {
    trace_id: "trace-discontinuous",
    span_id: "span-2",
    parent_span_id: Some("span-1"),
    start_time: 1640995400,  // 与前一个span间隔超过60秒
    end_time: 1640995410,
    service_name: "service.b",
    operation_name: "operation.b",
    status: "ok"
  }
  
  // 验证追踪链路不连续的数据
  let discontinuous_trace = [span1, span2]
  let discontinuous_result = ConsistencyValidator::validate_trace(consistency_validator, discontinuous_trace)
  assert_false(discontinuous_result.is_consistent)
  
  let continuity_violation = discontinuous_result.violations.find(fn(v) { 
    v.type == "trace_continuity" 
  })
  assert_true(continuity_violation != None)
}

// 测试4: 数据质量评分
test "遥测数据质量评分测试" {
  // 创建数据质量评分器
  let quality_scorer = DataQualityScorer::new()
  
  // 配置评分权重
  QualityScorer::set_weight(quality_scorer, "completeness", 0.3)
  QualityScorer::set_weight(quality_scorer, "accuracy", 0.3)
  QualityScorer::set_weight(quality_scorer, "consistency", 0.2)
  QualityScorer::set_weight(quality_scorer, "timeliness", 0.2)
  
  // 创建高质量数据集
  let high_quality_data = []
  for i in 0..=100 {
    high_quality_data = high_quality_data.push({
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      timestamp: 1640995200 + i,
      service_name: "service-" + (i % 10).to_string(),
      operation_name: "operation-" + (i % 20).to_string(),
      duration: 50 + (i % 200),
      status: if i % 50 == 0 { "error" } else { "ok" },
      attributes: [
        ("http.method", "GET"),
        ("http.status_code", (200 + (i % 5) * 100).to_string())
      ]
    })
  }
  
  // 评分高质量数据
  let high_quality_score = QualityScorer::score_dataset(quality_scorer, high_quality_data)
  assert_true(high_quality_score.overall_score >= 0.9)
  assert_true(high_quality_score.completeness_score >= 0.95)
  assert_true(high_quality_score.accuracy_score >= 0.95)
  assert_true(high_quality_score.consistency_score >= 0.9)
  assert_true(high_quality_score.timeliness_score >= 0.9)
  
  // 创建低质量数据集（包含各种质量问题）
  let low_quality_data = []
  for i in 0..=100 {
    let base_data = {
      trace_id: if i % 10 == 0 { "" } else { "trace-" + i.to_string() },  // 有时缺少trace_id
      span_id: "span-" + i.to_string(),
      timestamp: 1640995200 + i,
      service_name: "service-" + (i % 10).to_string(),
      operation_name: "operation-" + (i % 20).to_string(),
      duration: if i % 15 == 0 { -10 } else { 50 + (i % 200) },  // 有时负数持续时间
      status: if i % 30 == 0 { "invalid_status" } else { "ok" },  // 有时无效状态
      attributes: []
    }
    low_quality_data = low_quality_data.push(base_data)
  }
  
  // 评分低质量数据
  let low_quality_score = QualityScorer::score_dataset(quality_scorer, low_quality_data)
  assert_true(low_quality_score.overall_score < 0.8)
  assert_true(low_quality_score.completeness_score < 0.9)
  assert_true(low_quality_score.accuracy_score < 0.9)
  
  // 创建中等质量数据集
  let medium_quality_data = []
  for i in 0..=100 {
    let base_data = {
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      timestamp: 1640995200 + i,
      service_name: "service-" + (i % 10).to_string(),
      operation_name: "operation-" + (i % 20).to_string(),
      duration: 50 + (i % 200),
      status: if i % 20 == 0 { "error" } else { "ok" },
      attributes: if i % 5 == 0 { [] } else {  // 有时缺少属性
        [("http.method", "GET"), ("http.status_code", "200")]
      }
    }
    medium_quality_data = medium_quality_data.push(base_data)
  }
  
  // 评分中等质量数据
  let medium_quality_score = QualityScorer::score_dataset(quality_scorer, medium_quality_data)
  assert_true(medium_quality_score.overall_score >= 0.8 and medium_quality_score.overall_score < 0.9)
  
  // 测试质量趋势分析
  let quality_trend = QualityScorer::analyze_quality_trend(quality_scorer, [
    ("2023-01-01", high_quality_score),
    ("2023-01-02", medium_quality_score),
    ("2023-01-03", low_quality_score),
    ("2023-01-04", medium_quality_score),
    ("2023-01-05", high_quality_score)
  ])
  
  // 验证质量趋势分析
  assert_true(quality_trend.has_decline)
  assert_true(quality_trend.has_improvement)
  assert_eq(quality_trend.peak_quality_date, "2023-01-01")
  assert_eq(quality_trend.lowest_quality_date, "2023-01-03")
}

// 测试5: 数据质量改进建议
test "遥测数据质量改进建议测试" {
  // 创建数据质量改进分析器
  let quality_analyzer = DataQualityAnalyzer::new()
  
  // 创建有质量问题的数据集
  let problematic_data = []
  for i in 0..=50 {
    let base_data = {
      trace_id: if i % 8 == 0 { "" } else { "trace-" + i.to_string() },
      span_id: "span-" + i.to_string(),
      timestamp: if i % 12 == 0 { 1000000000 } else { 1640995200 + i },  // 有时无效时间戳
      service_name: if i % 15 == 0 { "" } else { "service-" + (i % 10).to_string() },
      operation_name: "operation-" + (i % 20).to_string(),
      duration: if i % 7 == 0 { -50 } else { 50 + (i % 200) },
      status: if i % 25 == 0 { "unknown" } else { "ok" },
      attributes: []
    }
    problematic_data = problematic_data.push(base_data)
  }
  
  // 分析数据质量问题
  let quality_analysis = DataQualityAnalyzer::analyze(quality_analyzer, problematic_data)
  
  // 验证质量问题检测
  assert_true(quality_analysis.issues.length() > 0)
  
  // 检查完整性问题
  let completeness_issues = quality_analysis.issues.filter(fn(i) { i.category == "completeness" })
  assert_true(completeness_issues.length() > 0)
  
  let missing_trace_id_issue = completeness_issues.find(fn(i) { i.field == "trace_id" })
  assert_true(missing_trace_id_issue != None)
  
  match missing_trace_id_issue {
    Some(issue) => {
      assert_true(issue.affected_records > 0)
      assert_true(issue.severity == "medium" or issue.severity == "high")
    }
    None => assert_true(false)
  }
  
  // 检查准确性问题
  let accuracy_issues = quality_analysis.issues.filter(fn(i) { i.category == "accuracy" })
  assert_true(accuracy_issues.length() > 0)
  
  let invalid_duration_issue = accuracy_issues.find(fn(i) { i.field == "duration" })
  assert_true(invalid_duration_issue != None)
  
  // 检查时间戳问题
  let timestamp_issues = quality_analysis.issues.filter(fn(i) { i.field == "timestamp" })
  assert_true(timestamp_issues.length() > 0)
  
  // 获取改进建议
  let improvement_recommendations = DataQualityAnalyzer::get_recommendations(quality_analyzer, quality_analysis)
  
  // 验证改进建议
  assert_true(improvement_recommendations.length() > 0)
  
  // 检查必填字段验证建议
  let field_validation_recommendation = improvement_recommendations.find(fn(r) { 
    r.type == "field_validation" and r.target_field == "trace_id" 
  })
  assert_true(field_validation_recommendation != None)
  
  match field_validation_recommendation {
    Some(rec) => {
      assert_true(rec.description.contains("必填字段"))
      assert_true(rec.priority == "high")
      assert_true(rec.effort == "low")
    }
    None => assert_true(false)
  }
  
  // 检查数值范围验证建议
  let range_validation_recommendation = improvement_recommendations.find(fn(r) { 
    r.type == "range_validation" and r.target_field == "duration" 
  })
  assert_true(range_validation_recommendation != None)
  
  // 检查时间戳验证建议
  let timestamp_validation_recommendation = improvement_recommendations.find(fn(r) { 
    r.type == "timestamp_validation" and r.target_field == "timestamp" 
  })
  assert_true(timestamp_validation_recommendation != None)
  
  // 测试改进建议优先级排序
  let prioritized_recommendations = DataQualityAnalyzer::prioritize_recommendations(quality_analyzer, improvement_recommendations)
  
  // 验证高优先级建议排在前面
  assert_eq(prioritized_recommendations[0].priority, "high")
  
  // 测试改进效果预测
  let improvement_impact = DataQualityAnalyzer::predict_improvement_impact(quality_analyzer, quality_analysis, prioritized_recommendations)
  
  // 验证改进效果预测
  assert_true(improvement_impact.quality_score_increase > 0.0)
  assert_true(improvement_impact.issue_reduction_percentage > 0.0)
  
  // 测试实施计划生成
  let implementation_plan = DataQualityAnalyzer::generate_implementation_plan(quality_analyzer, prioritized_recommendations)
  
  // 验证实施计划
  assert_true(implementation_plan.phases.length() > 0)
  
  let first_phase = implementation_plan.phases[0]
  assert_true(first_phase.tasks.length() > 0)
  assert_true(first_phase.estimated_duration > 0)
}