// Azimuth 遥测数据质量验证测试
// 专注于遥测数据的质量保证和验证机制

// 测试1: 遥测数据完整性验证
test "遥测数据完整性验证" {
  // 创建数据质量验证器
  let quality_validator = DataQualityValidator::new()
  
  // 配置验证规则
  QualityValidator::add_rule(quality_validator, "timestamp_range", {
    min_timestamp: 1640995200,
    max_timestamp: 1735689599,
    required: true
  })
  
  QualityValidator::add_rule(quality_validator, "required_fields", {
    fields: ["trace_id", "span_id", "service_name", "operation_name"],
    required: true
  })
  
  QualityValidator::add_rule(quality_validator, "value_ranges", {
    rules: [
      { field: "duration", min: 0, max: 3600000 },  // 最大1小时
      { field: "status_code", min: 100, max: 599 }
    ]
  })
  
  // 创建测试数据集
  let valid_data = [
    {
      timestamp: 1640995200,
      trace_id: "trace-12345",
      span_id: "span-67890",
      service_name: "api.service",
      operation_name: "process_request",
      duration: 150,
      status_code: 200
    },
    {
      timestamp: 1640995300,
      trace_id: "trace-12346",
      span_id: "span-67891",
      service_name: "db.service",
      operation_name: "query_data",
      duration: 50,
      status_code: 200
    }
  ]
  
  let invalid_data = [
    {
      timestamp: 1640995200,
      trace_id: "",  // 缺少必需字段
      span_id: "span-67892",
      service_name: "cache.service",
      operation_name: "get_value",
      duration: -10,  // 无效值
      status_code: 200
    },
    {
      timestamp: 1000000000,  // 超出范围
      trace_id: "trace-12347",
      span_id: "span-67893",
      service_name: "queue.service",
      operation_name: "publish_message",
      duration: 5000,  // 超出范围
      status_code: 999  // 超出范围
    }
  ]
  
  // 验证有效数据
  let valid_results = QualityValidator::validate_batch(quality_validator, valid_data)
  
  // 验证无效数据
  let invalid_results = QualityValidator::validate_batch(quality_validator, invalid_data)
  
  // 验证结果
  assert_eq(valid_results.passed_count, 2)
  assert_eq(valid_results.failed_count, 0)
  assert_eq(valid_results.total_count, 2)
  assert_eq(valid_results.quality_score, 100.0)
  
  assert_eq(invalid_results.passed_count, 0)
  assert_eq(invalid_results.failed_count, 2)
  assert_eq(invalid_results.total_count, 2)
  assert_eq(invalid_results.quality_score, 0.0)
  
  // 验证具体错误
  let first_invalid_errors = invalid_results.items[0].errors
  assert_true(first_invalid_errors.length() >= 2)
  assert_true(first_invalid_errors.any(fn(e) { e.field == "trace_id" and e.error_type == "missing_field" }))
  assert_true(first_invalid_errors.any(fn(e) { e.field == "duration" and e.error_type == "value_out_of_range" }))
  
  let second_invalid_errors = invalid_results.items[1].errors
  assert_true(second_invalid_errors.length() >= 3)
  assert_true(second_invalid_errors.any(fn(e) { e.field == "timestamp" and e.error_type == "value_out_of_range" }))
  assert_true(second_invalid_errors.any(fn(e) { e.field == "duration" and e.error_type == "value_out_of_range" }))
  assert_true(second_invalid_errors.any(fn(e) { e.field == "status_code" and e.error_type == "value_out_of_range" }))
}

// 测试2: 遥测数据一致性检查
test "遥测数据一致性检查" {
  // 创建一致性检查器
  let consistency_checker = ConsistencyChecker::new()
  
  // 配置一致性规则
  ConsistencyChecker::add_rule(consistency_checker, "trace_hierarchy", {
    require_parent_child: true,
    allow_orphan_spans: false,
    validate_timing: true
  })
  
  ConsistencyChecker::add_rule(consistency_checker, "service_naming", {
    pattern: "^[a-z]+\\.[a-z]+$",
    case_sensitive: false
  })
  
  ConsistencyChecker::add_rule(consistency_checker, "status_code_consistency", {
    error_statuses: ["error", "timeout", "cancelled"],
    error_status_codes: [400, 401, 403, 404, 500, 502, 503, 504]
  })
  
  // 创建一致的测试数据
  let consistent_trace = [
    {
      trace_id: "trace-consistent-1",
      span_id: "span-root",
      parent_span_id: "",
      service_name: "api.gateway",
      operation_name: "handle_request",
      status: "ok",
      status_code: 200,
      start_time: 1640995200000,
      end_time: 1640995200150
    },
    {
      trace_id: "trace-consistent-1",
      span_id: "span-child-1",
      parent_span_id: "span-root",
      service_name: "auth.service",
      operation_name: "authenticate",
      status: "ok",
      status_code: 200,
      start_time: 1640995200020,
      end_time: 1640995200080
    },
    {
      trace_id: "trace-consistent-1",
      span_id: "span-child-2",
      parent_span_id: "span-root",
      service_name: "data.service",
      operation_name: "fetch_data",
      status: "ok",
      status_code: 200,
      start_time: 1640995200090,
      end_time: 1640995200140
    }
  ]
  
  // 创建不一致的测试数据
  let inconsistent_trace = [
    {
      trace_id: "trace-inconsistent-1",
      span_id: "span-orphan",
      parent_span_id: "non-existent-parent",  // 不存在的父span
      service_name: "InvalidServiceName",     // 不符合命名规范
      operation_name: "process",
      status: "error",
      status_code: 200,  // 状态不一致
      start_time: 1640995200200,
      end_time: 1640995200100  // 结束时间早于开始时间
    },
    {
      trace_id: "trace-inconsistent-1",
      span_id: "span-root-2",
      parent_span_id: "",
      service_name: "queue.service",
      operation_name: "publish",
      status: "timeout",
      status_code: 200,  // 状态不一致
      start_time: 1640995200300,
      end_time: 1640995200350
    }
  ]
  
  // 检查一致性
  let consistent_results = ConsistencyChecker::check_trace_consistency(consistency_checker, consistent_trace)
  let inconsistent_results = ConsistencyChecker::check_trace_consistency(consistency_checker, inconsistent_trace)
  
  // 验证结果
  assert_true(consistent_results.is_consistent)
  assert_eq(consistent_results.violations.length(), 0)
  assert_eq(consistent_results.consistency_score, 100.0)
  
  assert_false(inconsistent_results.is_consistent)
  assert_true(inconsistent_results.violations.length() > 0)
  assert_true(inconsistent_results.consistency_score < 100.0)
  
  // 验证具体违规
  let violations = inconsistent_results.violations
  assert_true(violations.any(fn(v) { v.type == "missing_parent" }))
  assert_true(violations.any(fn(v) { v.type == "invalid_service_name" }))
  assert_true(violations.any(fn(v) { v.type == "status_mismatch" }))
  assert_true(violations.any(fn(v) { v.type == "invalid_timing" }))
}

// 测试3: 遥测数据时效性验证
test "遥测数据时效性验证" {
  // 创建时效性验证器
  let timeliness_validator = TimelinessValidator::new()
  
  // 配置时效性规则
  TimelinessValidator::set_rules(timeliness_validator, {
    max_data_age: 300,        // 最大数据年龄5分钟
    expected_delay: 30,       // 预期延迟30秒
    batch_window: 60,         // 批处理窗口1分钟
    real_time_threshold: 10   // 实时数据阈值10秒
  })
  
  // 获取当前时间
  let current_time = 1640995800  // 模拟当前时间
  
  // 创建不同时效性的测试数据
  let realtime_data = [
    { timestamp: current_time - 5, data_id: "realtime-1" },    // 5秒前
    { timestamp: current_time - 8, data_id: "realtime-2" },    // 8秒前
    { timestamp: current_time - 10, data_id: "realtime-3" }    // 10秒前
  ]
  
  let recent_data = [
    { timestamp: current_time - 60, data_id: "recent-1" },     // 1分钟前
    { timestamp: current_time - 120, data_id: "recent-2" },    // 2分钟前
    { timestamp: current_time - 240, data_id: "recent-3" }     // 4分钟前
  ]
  
  let stale_data = [
    { timestamp: current_time - 360, data_id: "stale-1" },     // 6分钟前
    { timestamp: current_time - 600, data_id: "stale-2" },     // 10分钟前
    { timestamp: current_time - 1800, data_id: "stale-3" }     // 30分钟前
  ]
  
  // 验证实时数据
  let realtime_results = TimelinessValidator::validate_batch(timeliness_validator, realtime_data, current_time)
  
  // 验证近期数据
  let recent_results = TimelinessValidator::validate_batch(timeliness_validator, recent_data, current_time)
  
  // 验证过期数据
  let stale_results = TimelinessValidator::validate_batch(timeliness_validator, stale_data, current_time)
  
  // 验证结果
  assert_true(realtime_results.avg_delay <= 10)  // 平均延迟不超过10秒
  assert_eq(realtime_results.fresh_items, 3)     // 所有数据都是新鲜的
  assert_eq(realtime_results.stale_items, 0)     // 没有过期数据
  assert_eq(realtime_results.timeliness_score, 100.0)
  
  assert_true(recent_results.avg_delay <= 120)   // 平均延迟不超过2分钟
  assert_eq(recent_results.fresh_items, 3)       // 所有数据都是新鲜的
  assert_eq(recent_results.stale_items, 0)       // 没有过期数据
  assert_true(recent_results.timeliness_score >= 80.0)
  
  assert_true(stale_results.avg_delay > 300)      // 平均延迟超过5分钟
  assert_eq(stale_results.fresh_items, 0)         // 没有新鲜数据
  assert_eq(stale_results.stale_items, 3)         // 所有数据都过期
  assert_true(stale_results.timeliness_score < 50.0)
  
  // 测试时效性趋势分析
  let trend_data = []
  for i in 0..=24 {
    trend_data = trend_data.push({
      timestamp: current_time - i * 60,  // 每分钟一个数据点
      data_id: "trend-" + i.to_string()
    })
  }
  
  let trend_analysis = TimelinessValidator::analyze_timeliness_trend(timeliness_validator, trend_data, current_time)
  
  // 验证趋势分析
  assert_true(trend_analysis.data_points > 0)
  assert_true(trend_analysis.oldest_data_age > 0)
  assert_true(trend_analysis.newest_data_age >= 0)
  assert_true(trend_analysis.timeliness_trend == "improving" or 
              trend_analysis.timeliness_trend == "stable" or 
              trend_analysis.timeliness_trend == "degrading")
}

// 测试4: 遥测数据准确性验证
test "遥测数据准确性验证" {
  // 创建准确性验证器
  let accuracy_validator = AccuracyValidator::new()
  
  // 配置准确性规则
  AccuracyValidator::add_rule(accuracy_validator, "metric_precision", {
    max_decimal_places: 3,
    allow_scientific_notation: false
  })
  
  AccuracyValidator::add_rule(accuracy_validator, "duration_plausibility", {
    min_operation_duration: 1,      // 最小操作持续时间1ms
    max_operation_duration: 3600000, // 最大操作持续时间1小时
    check_parent_child_timing: true
  })
  
  AccuracyValidator::add_rule(accuracy_validator, "resource_usage_plausibility", {
    max_cpu_usage: 100.0,
    max_memory_usage: 1073741824,  // 1GB
    max_disk_io_rate: 1073741824   // 1GB/s
  })
  
  // 创建准确的测试数据
  let accurate_data = [
    {
      metric_name: "response_time",
      value: 125.456,
      unit: "ms",
      precision: 3
    },
    {
      metric_name: "cpu_usage",
      value: 75.5,
      unit: "percent",
      precision: 1
    },
    {
      metric_name: "memory_usage",
      value: 536870912,
      unit: "bytes",
      precision: 0
    }
  ]
  
  // 创建不准确的测试数据
  let inaccurate_data = [
    {
      metric_name: "response_time",
      value: 125.456789,  // 超过精度限制
      unit: "ms",
      precision: 6
    },
    {
      metric_name: "cpu_usage",
      value: 150.0,       // 超出合理范围
      unit: "percent",
      precision: 1
    },
    {
      metric_name: "operation_duration",
      value: -50,         // 负值不合理
      unit: "ms",
      precision: 0
    },
    {
      metric_name: "memory_usage",
      value: 2147483648,  // 超出合理范围
      unit: "bytes",
      precision: 0
    }
  ]
  
  // 验证准确数据
  let accurate_results = AccuracyValidator::validate_metrics(accuracy_validator, accurate_data)
  
  // 验证不准确数据
  let inaccurate_results = AccuracyValidator::validate_metrics(accuracy_validator, inaccurate_data)
  
  // 验证结果
  assert_eq(accurate_results.accurate_count, 3)
  assert_eq(accurate_results.inaccurate_count, 0)
  assert_eq(accurate_results.accuracy_score, 100.0)
  
  assert_eq(inaccurate_results.accurate_count, 0)
  assert_eq(inaccurate_results.inaccurate_count, 4)
  assert_true(inaccurate_results.accuracy_score < 50.0)
  
  // 验证具体错误
  let errors = inaccurate_results.errors
  assert_true(errors.any(fn(e) { e.type == "precision_exceeded" }))
  assert_true(errors.any(fn(e) { e.type == "value_out_of_range" }))
  assert_true(errors.any(fn(e) { e.type == "negative_value" }))
  
  // 测试父子span时间准确性
  let span_timing_data = [
    {
      span_id: "parent",
      parent_span_id: "",
      start_time: 1640995200000,
      end_time: 1640995200200,  // 200ms
      operation_name: "parent_operation"
    },
    {
      span_id: "child1",
      parent_span_id: "parent",
      start_time: 1640995200010,
      end_time: 1640995200060,  // 50ms
      operation_name: "child_operation_1"
    },
    {
      span_id: "child2",
      parent_span_id: "parent",
      start_time: 1640995200070,
      end_time: 1640995200150,  // 80ms
      operation_name: "child_operation_2"
    }
  ]
  
  let timing_validation = AccuracyValidator::validate_span_timing(accuracy_validator, span_timing_data)
  
  // 验证时间准确性
  assert_true(timing_validation.is_accurate)
  assert_eq(timing_validation.violations.length(), 0)
  
  // 测试不准确的时间数据
  let invalid_timing_data = [
    {
      span_id: "parent_invalid",
      parent_span_id: "",
      start_time: 1640995200200,
      end_time: 1640995200000,  // 结束时间早于开始时间
      operation_name: "invalid_parent"
    },
    {
      span_id: "child_invalid",
      parent_span_id: "parent_invalid",
      start_time: 1640995200100,
      end_time: 1640995200300,  // 子span超出父span时间范围
      operation_name: "invalid_child"
    }
  ]
  
  let invalid_timing_validation = AccuracyValidator::validate_span_timing(accuracy_validator, invalid_timing_data)
  
  // 验证无效时间数据
  assert_false(invalid_timing_validation.is_accurate)
  assert_true(invalid_timing_validation.violations.length() > 0)
  assert_true(invalid_timing_validation.violations.any(fn(v) { v.type == "invalid_time_range" }))
  assert_true(invalid_timing_validation.violations.any(fn(v) { v.type == "child_exceeds_parent" }))
}

// 测试5: 遥测数据重复检测
test "遥测数据重复检测" {
  // 创建重复检测器
  let duplicate_detector = DuplicateDetector::new()
  
  // 配置检测规则
  DuplicateDetector::set_strategy(duplicate_detector, "exact_match", {
    fields: ["trace_id", "span_id", "timestamp"],
    time_tolerance: 0
  })
  
  DuplicateDetector::set_strategy(duplicate_detector, "fuzzy_match", {
    fields: ["trace_id", "span_id"],
    time_tolerance: 1000,  // 1秒容差
    similarity_threshold: 0.9
  })
  
  // 创建测试数据集
  let unique_data = [
    {
      trace_id: "trace-unique-1",
      span_id: "span-unique-1",
      timestamp: 1640995200000,
      service_name: "service.a",
      operation_name: "operation.a"
    },
    {
      trace_id: "trace-unique-2",
      span_id: "span-unique-2",
      timestamp: 1640995200100,
      service_name: "service.b",
      operation_name: "operation.b"
    },
    {
      trace_id: "trace-unique-3",
      span_id: "span-unique-3",
      timestamp: 1640995200200,
      service_name: "service.c",
      operation_name: "operation.c"
    }
  ]
  
  // 创建包含重复数据的测试集
  let data_with_duplicates = [
    {
      trace_id: "trace-dup-1",
      span_id: "span-dup-1",
      timestamp: 1640995200000,
      service_name: "service.a",
      operation_name: "operation.a"
    },
    {
      trace_id: "trace-dup-1",
      span_id: "span-dup-1",
      timestamp: 1640995200000,  // 完全重复
      service_name: "service.a",
      operation_name: "operation.a"
    },
    {
      trace_id: "trace-dup-2",
      span_id: "span-dup-2",
      timestamp: 1640995200100,
      service_name: "service.b",
      operation_name: "operation.b"
    },
    {
      trace_id: "trace-dup-2",
      span_id: "span-dup-2",
      timestamp: 1640995200105,  // 时间略有差异
      service_name: "service.b",
      operation_name: "operation.b"
    },
    {
      trace_id: "trace-dup-3",
      span_id: "span-dup-3",
      timestamp: 1640995200200,
      service_name: "service.c",
      operation_name: "operation.c"
    }
  ]
  
  // 检测唯一数据
  let unique_results = DuplicateDetector::detect_duplicates(duplicate_detector, unique_data, "exact_match")
  
  // 检测重复数据
  let duplicate_results = DuplicateDetector::detect_duplicates(duplicate_detector, data_with_duplicates, "exact_match")
  
  // 验证唯一数据结果
  assert_eq(unique_results.total_items, 3)
  assert_eq(unique_results.unique_items, 3)
  assert_eq(unique_results.duplicate_items, 0)
  assert_eq(unique_results.duplicate_groups.length(), 0)
  assert_eq(unique_results.duplicate_rate, 0.0)
  
  // 验证重复数据结果
  assert_eq(duplicate_results.total_items, 5)
  assert_eq(duplicate_results.unique_items, 3)
  assert_eq(duplicate_results.duplicate_items, 2)
  assert_eq(duplicate_results.duplicate_groups.length(), 1)
  assert_eq(duplicate_results.duplicate_rate, 0.4)  // 2/5 = 0.4
  
  // 验证重复组
  let duplicate_group = duplicate_results.duplicate_groups[0]
  assert_eq(duplicate_group.key, "trace-dup-1:span-dup-1:1640995200000")
  assert_eq(duplicate_group.items.length(), 2)
  assert_eq(duplicate_group.duplicate_count, 1)
  
  // 使用模糊匹配检测
  let fuzzy_results = DuplicateDetector::detect_duplicates(duplicate_detector, data_with_duplicates, "fuzzy_match")
  
  // 验证模糊匹配结果
  assert_eq(fuzzy_results.total_items, 5)
  assert_eq(fuzzy_results.unique_items, 2)  // 更少的唯一项，因为时间容差
  assert_eq(fuzzy_results.duplicate_items, 3)
  assert_eq(fuzzy_results.duplicate_groups.length(), 2)
  assert_eq(fuzzy_results.duplicate_rate, 0.6)  // 3/5 = 0.6
  
  // 测试重复数据处理策略
  let deduplication_strategy = DeduplicationStrategy::new()
  
  // 配置策略
  DeduplicationStrategy::set_rule(deduplication_strategy, "keep_first", {
    priority: 1,
    conditions: []
  })
  
  DeduplicationStrategy::set_rule(deduplication_strategy, "keep_last", {
    priority: 2,
    conditions: [{ field: "timestamp", operator: ">", value: 1640995200000 }]
  })
  
  DeduplicationStrategy::set_rule(deduplication_strategy, "merge", {
    priority: 3,
    conditions: [{ field: "service_name", operator: "=", value: "service.b" }],
    merge_fields: ["attributes", "metrics"]
  })
  
  // 应用去重策略
  let deduplicated_data = DeduplicationStrategy::apply(deduplication_strategy, data_with_duplicates, duplicate_results)
  
  // 验证去重结果
  assert_eq(deduplicated_data.length(), 3)  // 原始5项减去2个重复项
  
  // 验证保留的数据
  let kept_item = deduplicated_data.find(fn(item) { 
    item.trace_id == "trace-dup-1" and item.span_id == "span-dup-1" 
  })
  assert_true(kept_item != None)
  
  // 测试重复数据报告
  let duplicate_report = DuplicateDetector::generate_report(duplicate_detector, duplicate_results)
  
  // 验证报告内容
  assert_true(duplicate_report.summary.total_items > 0)
  assert_true(duplicate_report.summary.duplicate_rate > 0.0)
  assert_true(duplicate_report.summary.duplicate_groups > 0)
  assert_true(duplicate_report.recommendations.length() > 0)
  
  // 验证建议内容
  let recommendations = duplicate_report.recommendations
  assert_true(recommendations.any(fn(r) { r.type == "data_quality_improvement" }))
  assert_true(recommendations.any(fn(r) { r.type == "deduplication_strategy" }))
  assert_true(recommendations.any(fn(r) { r.type == "source_system_check" }))
}