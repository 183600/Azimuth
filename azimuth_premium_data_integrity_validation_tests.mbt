// Azimuth 数据完整性和验证测试用例
// 专注于遥测数据的完整性、一致性验证和数据质量保证

// 测试1: 遥测数据完整性校验
test "遥测数据完整性校验" {
  // 数据完整性校验结果
  type IntegrityCheckResult = {
    is_valid: Bool,
    checksum: String,
    expected_checksum: String,
    data_size: Int,
    validation_errors: Array[String]
  }
  
  // 简单的校验和计算
  let calculate_checksum = fn(data: String) {
    let mut hash = 0
    for c in data {
      hash = (hash * 31 + c.to_int()) % 1000000007
    }
    hash.to_string()
  }
  
  // 数据完整性验证器
  let validate_data_integrity = fn(data: String, expected_checksum: String) {
    if data.length() == 0 {
      return {
        is_valid: false,
        checksum: "",
        expected_checksum: expected_checksum,
        data_size: 0,
        validation_errors: ["数据为空"]
      }
    }
    
    let actual_checksum = calculate_checksum(data)
    let is_valid = actual_checksum == expected_checksum
    let mut errors = []
    
    if not is_valid {
      errors = errors.push("校验和不匹配")
    }
    
    {
      is_valid: is_valid,
      checksum: actual_checksum,
      expected_checksum: expected_checksum,
      data_size: data.length(),
      validation_errors: errors
    }
  }
  
  // 批量数据完整性验证
  let batch_validate_integrity = fn(data_batch: Array[(String, String)>) {
    let mut results = []
    let mut all_valid = true
    
    for (data, expected_checksum) in data_batch {
      let result = validate_data_integrity(data, expected_checksum)
      results = results.push(result)
      
      if not result.is_valid {
        all_valid = false
      }
    }
    
    {
      results: results,
      all_valid: all_valid,
      valid_count: results.filter(fn(r) { r.is_valid }).length(),
      total_count: results.length()
    }
  }
  
  // 创建测试数据
  let test_data1 = "遥测数据：服务响应时间，延迟，错误率"
  let checksum1 = calculate_checksum(test_data1)
  
  let test_data2 = "性能指标：CPU使用率，内存占用，网络吞吐量"
  let checksum2 = calculate_checksum(test_data2)
  
  let test_data3 = "业务指标：用户活跃度，转化率，留存率"
  let checksum3 = calculate_checksum(test_data3)
  
  // 测试单个数据完整性验证
  let valid_result = validate_data_integrity(test_data1, checksum1)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.checksum, checksum1)
  assert_eq(valid_result.data_size, test_data1.length())
  assert_eq(valid_result.validation_errors.length(), 0)
  
  // 测试校验和不匹配
  let invalid_result = validate_data_integrity(test_data1, "invalid_checksum")
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.validation_errors.contains("校验和不匹配"))
  
  // 测试空数据
  let empty_result = validate_data_integrity("", checksum1)
  assert_false(empty_result.is_valid)
  assert_true(empty_result.validation_errors.contains("数据为空"))
  
  // 测试批量验证
  let data_batch = [
    (test_data1, checksum1),
    (test_data2, checksum2),
    (test_data3, checksum3)
  ]
  
  let batch_result = batch_validate_integrity(data_batch)
  assert_true(batch_result.all_valid)
  assert_eq(batch_result.valid_count, 3)
  assert_eq(batch_result.total_count, 3)
  
  // 测试包含无效数据的批量验证
  let mixed_batch = [
    (test_data1, checksum1),
    (test_data2, "invalid_checksum"),
    (test_data3, checksum3)
  ]
  
  let mixed_result = batch_validate_integrity(mixed_batch)
  assert_false(mixed_result.all_valid)
  assert_eq(mixed_result.valid_count, 2)
  assert_eq(mixed_result.total_count, 3)
}

// 测试2: 数据一致性验证
test "数据一致性验证" {
  // 数据点类型
  type DataPoint = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Map[String, String]
  }
  
  // 一致性验证规则
  type ConsistencyRule = {
    name: String,
    description: String,
    validator: (Array[DataPoint>) -> Bool
  }
  
  // 一致性验证结果
  type ConsistencyResult = {
    rule_name: String,
    is_consistent: Bool,
    violations: Array[String]
  }
  
  // 时间序列一致性检查（时间戳递增）
  let timestamp_consistency_rule = {
    name: "timestamp_monotonic",
    description: "时间戳应该单调递增",
    validator: fn(points: Array[DataPoint>) {
      if points.length() < 2 {
        true
      } else {
        let mut consistent = true
        for i in 1..points.length() {
          if points[i].timestamp <= points[i - 1].timestamp {
            consistent = false
            break
          }
        }
        consistent
      }
    }
  }
  
  // 值范围一致性检查
  let value_range_rule = fn(min_value: Float, max_value: Float) {
    {
      name: "value_range_check",
      description: "值应该在指定范围内",
      validator: fn(points: Array[DataPoint>) {
        points.all(fn(p) { p.value >= min_value && p.value <= max_value })
      }
    }
  }
  
  // 标签一致性检查
  let tag_consistency_rule = fn(required_tags: Array[String]) {
    {
      name: "required_tags_check",
      description: "数据点应包含必需的标签",
      validator: fn(points: Array[DataPoint>) {
        points.all(fn(p) {
          required_tags.all(fn(tag) { Map::contains_key(p.tags, tag) })
        })
      }
    }
  }
  
  // 指标名称一致性检查
  let metric_name_consistency_rule = fn(expected_name: String) {
    {
      name: "metric_name_check",
      description: "所有数据点应有相同的指标名称",
      validator: fn(points: Array[DataPoint>) {
        if points.length() == 0 {
          true
        } else {
          let first_name = points[0].metric_name
          points.all(fn(p) { p.metric_name == first_name })
        }
      }
    }
  }
  
  // 执行一致性验证
  let validate_consistency = fn(points: Array[DataPoint>, rules: Array[ConsistencyRule>) {
    let mut results = []
    
    for rule in rules {
      let is_consistent = rule.validator(points)
      let mut violations = []
      
      if not is_consistent {
        violations = violations.push(rule.description)
      }
      
      results = results.push({
        rule_name: rule.name,
        is_consistent: is_consistent,
        violations: violations
      })
    }
    
    results
  }
  
  // 创建测试数据
  let valid_points = [
    { timestamp: 1000, metric_name: "response_time", value: 100.5, tags: [ ("service", "api"), ("env", "prod") ] },
    { timestamp: 2000, metric_name: "response_time", value: 120.3, tags: [ ("service", "api"), ("env", "prod") ] },
    { timestamp: 3000, metric_name: "response_time", value: 95.7, tags: [ ("service", "api"), ("env", "prod") ] }
  ]
  
  let invalid_timestamp_points = [
    { timestamp: 1000, metric_name: "response_time", value: 100.5, tags: [ ("service", "api"), ("env", "prod") ] },
    { timestamp: 2000, metric_name: "response_time", value: 120.3, tags: [ ("service", "api"), ("env", "prod") ] },
    { timestamp: 1500, metric_name: "response_time", value: 95.7, tags: [ ("service", "api"), ("env", "prod") ] } // 时间戳不递增
  ]
  
  let invalid_value_points = [
    { timestamp: 1000, metric_name: "response_time", value: 100.5, tags: [ ("service", "api"), ("env", "prod") ] },
    { timestamp: 2000, metric_name: "response_time", value: 500.0, tags: [ ("service", "api"), ("env", "prod") ] }, // 超出范围
    { timestamp: 3000, metric_name: "response_time", value: 95.7, tags: [ ("service", "api"), ("env", "prod") ] }
  ]
  
  let missing_tag_points = [
    { timestamp: 1000, metric_name: "response_time", value: 100.5, tags: [ ("service", "api") ] }, // 缺少env标签
    { timestamp: 2000, metric_name: "response_time", value: 120.3, tags: [ ("service", "api"), ("env", "prod") ] },
    { timestamp: 3000, metric_name: "response_time", value: 95.7, tags: [ ("service", "api"), ("env", "prod") ] }
  ]
  
  // 创建验证规则
  let rules = [
    timestamp_consistency_rule,
    value_range_rule(0.0, 200.0),
    tag_consistency_rule(["service", "env"]),
    metric_name_consistency_rule("response_time")
  ]
  
  // 测试有效数据
  let valid_results = validate_consistency(valid_points, rules)
  assert_true(valid_results.all(fn(r) { r.is_consistent }))
  
  // 测试无效时间戳
  let timestamp_results = validate_consistency(invalid_timestamp_points, [timestamp_consistency_rule])
  assert_false(timestamp_results[0].is_consistent)
  assert_true(timestamp_results[0].violations.contains("时间戳应该单调递增"))
  
  // 测试无效值范围
  let value_results = validate_consistency(invalid_value_points, [value_range_rule(0.0, 200.0)])
  assert_false(value_results[0].is_consistent)
  assert_true(value_results[0].violations.contains("值应该在指定范围内"))
  
  // 测试缺少标签
  let tag_results = validate_consistency(missing_tag_points, [tag_consistency_rule(["service", "env"])])
  assert_false(tag_results[0].is_consistent)
  assert_true(tag_results[0].violations.contains("数据点应包含必需的标签"))
}

// 测试3: 数据质量评估
test "数据质量评估" {
  // 数据质量维度
  type QualityDimension = {
    name: String,
    weight: Float,
    score: Float,
    description: String
  }
  
  // 数据质量评估结果
  type QualityAssessment = {
    overall_score: Float,
    dimensions: Array[QualityDimension],
    issues: Array[String],
    recommendations: Array[String]
  }
  
  // 完整性评估
  let assess_completeness = fn(points: Array[DataPoint>) {
    let total_points = points.length()
    if total_points == 0 {
      return 0.0
    }
    
    let complete_points = points.filter(fn(p) {
      p.metric_name != "" && p.tags.size() > 0
    }).length()
    
    (complete_points as Float) / (total_points as Float)
  }
  
  // 准确性评估（基于值范围）
  let assess_accuracy = fn(points: Array[DataPoint>, expected_ranges: Map[String, (Float, Float)>) {
    if points.length() == 0 {
      return 1.0
    }
    
    let mut accurate_points = 0
    
    for point in points {
      match Map::get(expected_ranges, point.metric_name) {
        Some((min, max)) => {
          if point.value >= min && point.value <= max {
            accurate_points = accurate_points + 1
          }
        }
        None => {
          // 如果没有范围定义，认为是准确的
          accurate_points = accurate_points + 1
        }
      }
    }
    
    (accurate_points as Float) / (points.length() as Float)
  }
  
  // 及时性评估（基于时间戳）
  let assess_timeliness = fn(points: Array[DataPoint>, current_time: Int, max_age: Int) {
    if points.length() == 0 {
      return 1.0
    }
    
    let recent_points = points.filter(fn(p) {
      current_time - p.timestamp <= max_age
    }).length()
    
    (recent_points as Float) / (points.length() as Float)
  }
  
  // 一致性评估
  let assess_consistency = fn(points: Array[DataPoint>) {
    if points.length() < 2 {
      return 1.0
    }
    
    // 检查时间戳单调性
    let mut timestamp_consistent = true
    for i in 1..points.length() {
      if points[i].timestamp <= points[i - 1].timestamp {
        timestamp_consistent = false
        break
      }
    }
    
    // 检查指标名称一致性
    let first_metric = points[0].metric_name
    let metric_consistent = points.all(fn(p) { p.metric_name == first_metric })
    
    if timestamp_consistent && metric_consistent {
      1.0
    } else {
      0.5
    }
  }
  
  // 执行数据质量评估
  let assess_data_quality = fn(points: Array[DataPoint>, current_time: Int, expected_ranges: Map[String, (Float, Float)>) {
    let completeness_score = assess_completeness(points)
    let accuracy_score = assess_accuracy(points, expected_ranges)
    let timeliness_score = assess_timeliness(points, current_time, 3600) // 1小时内
    let consistency_score = assess_consistency(points)
    
    let dimensions = [
      {
        name: "完整性",
        weight: 0.3,
        score: completeness_score,
        description: "数据是否完整，无缺失"
      },
      {
        name: "准确性",
        weight: 0.3,
        score: accuracy_score,
        description: "数据是否准确，符合预期范围"
      },
      {
        name: "及时性",
        weight: 0.2,
        score: timeliness_score,
        description: "数据是否及时，在有效时间范围内"
      },
      {
        name: "一致性",
        weight: 0.2,
        score: consistency_score,
        description: "数据是否一致，格式和逻辑统一"
      }
    ]
    
    // 计算总体得分
    let overall_score = dimensions.reduce(fn(acc, dim) {
      acc + dim.score * dim.weight
    }, 0.0)
    
    // 生成问题和建议
    let mut issues = []
    let mut recommendations = []
    
    if completeness_score < 0.8 {
      issues = issues.push("数据完整性不足")
      recommendations = recommendations.push("检查数据采集流程，确保所有必要字段都被采集")
    }
    
    if accuracy_score < 0.8 {
      issues = issues.push("数据准确性不足")
      recommendations = recommendations.push("验证数据源和采集逻辑，确保数据在合理范围内")
    }
    
    if timeliness_score < 0.8 {
      issues = issues.push("数据及时性不足")
      recommendations = recommendations.push("优化数据传输和处理流程，减少延迟")
    }
    
    if consistency_score < 0.8 {
      issues = issues.push("数据一致性不足")
      recommendations = recommendations.push("统一数据格式和采集标准，确保数据一致性")
    }
    
    {
      overall_score: overall_score,
      dimensions: dimensions,
      issues: issues,
      recommendations: recommendations
    }
  }
  
  // 创建测试数据
  let high_quality_points = [
    { timestamp: 1000, metric_name: "cpu_usage", value: 45.2, tags: [ ("host", "server1"), ("env", "prod") ] },
    { timestamp: 2000, metric_name: "cpu_usage", value: 52.7, tags: [ ("host", "server1"), ("env", "prod") ] },
    { timestamp: 3000, metric_name: "cpu_usage", value: 38.9, tags: [ ("host", "server1"), ("env", "prod") ] }
  ]
  
  let low_quality_points = [
    { timestamp: 1000, metric_name: "", value: 145.2, tags: [] }, // 缺少指标名称和标签，值超出范围
    { timestamp: 2000, metric_name: "cpu_usage", value: 52.7, tags: [ ("host", "server1") ] }, // 缺少标签
    { timestamp: 1500, metric_name: "cpu_usage", value: 38.9, tags: [ ("host", "server1"), ("env", "prod") ] } // 时间戳不递增
  ]
  
  let expected_ranges = [
    ("cpu_usage", (0.0, 100.0))
  ]
  
  // 测试高质量数据
  let high_quality_assessment = assess_data_quality(high_quality_points, 4000, expected_ranges)
  assert_true(high_quality_assessment.overall_score > 0.8)
  assert_eq(high_quality_assessment.issues.length(), 0)
  assert_eq(high_quality_assessment.recommendations.length(), 0)
  
  // 测试低质量数据
  let low_quality_assessment = assess_data_quality(low_quality_points, 4000, expected_ranges)
  assert_true(low_quality_assessment.overall_score < 0.8)
  assert_true(low_quality_assessment.issues.length() > 0)
  assert_true(low_quality_assessment.recommendations.length() > 0)
  
  // 验证具体问题
  assert_true(low_quality_assessment.issues.any(fn(issue) { issue.contains("完整性") }))
  assert_true(low_quality_assessment.issues.any(fn(issue) { issue.contains("准确性") }))
  assert_true(low_quality_assessment.issues.any(fn(issue) { issue.contains("一致性") }))
}

// 测试4: 数据转换验证
test "数据转换验证" {
  // 数据转换函数类型
  type DataTransform = (String) -> String
  
  // 转换验证结果
  type TransformValidationResult = {
    is_valid: Bool,
    input_data: String,
    output_data: String,
    expected_output: String,
    validation_errors: Array[String]
  }
  
  // 数据转换验证器
  let validate_transform = fn(transform: DataTransform, input_data: String, expected_output: String) {
    let output_data = transform(input_data)
    let is_valid = output_data == expected_output
    let mut errors = []
    
    if not is_valid {
      errors = errors.push("转换结果与预期不符")
    }
    
    {
      is_valid: is_valid,
      input_data: input_data,
      output_data: output_data,
      expected_output: expected_output,
      validation_errors: errors
    }
  }
  
  // JSON格式化转换
  let json_format_transform = fn(data: String) {
    // 简化的JSON格式化，实际实现会更复杂
    if data.contains("{") && data.contains("}") {
      let formatted = data
        .replace("{", "{\n  ")
        .replace("}", "\n}")
        .replace(",", ",\n  ")
      formatted
    } else {
      data
    }
  }
  
  // 单位转换（毫秒转秒）
  let ms_to_seconds_transform = fn(data: String) {
    match data.to_int() {
      Some(ms) => (ms / 1000).to_string()
      None => data
    }
  }
  
  // 百分比格式转换
  let decimal_to_percent_transform = fn(data: String) {
    match data.to_float() {
      Some(decimal) => (decimal * 100.0).to_string() + "%"
      None => data
    }
  }
  
  // 批量转换验证
  let batch_validate_transforms = fn(transforms: Array[(String, DataTransform)>, test_cases: Array[(String, String)>) {
    let mut results = []
    
    for (transform_name, transform) in transforms {
      for (input, expected) in test_cases {
        let result = validate_transform(transform, input, expected)
        results = results.push({
          ...result,
          transform_name: transform_name
        })
      }
    }
    
    results
  }
  
  // 测试JSON格式化
  let json_input = "{\"name\":\"service\",\"status\":\"healthy\"}"
  let json_expected = "{\n  \"name\":\"service\",\n  \"status\":\"healthy\"\n}"
  
  let json_result = validate_transform(json_format_transform, json_input, json_expected)
  assert_true(json_result.is_valid)
  
  // 测试毫秒转秒
  let ms_result1 = validate_transform(ms_to_seconds_transform, "5000", "5")
  assert_true(ms_result1.is_valid)
  
  let ms_result2 = validate_transform(ms_to_seconds_transform, "1500", "1")
  assert_true(ms_result2.is_valid)
  
  let ms_result3 = validate_transform(ms_to_seconds_transform, "invalid", "invalid")
  assert_true(ms_result3.is_valid) // 无效输入应原样返回
  
  // 测试小数转百分比
  let percent_result1 = validate_transform(decimal_to_percent_transform, "0.25", "25.0%")
  assert_true(percent_result1.is_valid)
  
  let percent_result2 = validate_transform(decimal_to_percent_transform, "0.75", "75.0%")
  assert_true(percent_result2.is_valid)
  
  // 批量转换验证
  let transforms = [
    ("json_format", json_format_transform),
    ("ms_to_seconds", ms_to_seconds_transform),
    ("decimal_to_percent", decimal_to_percent_transform)
  ]
  
  let test_cases = [
    ("{\"test\":\"value\"}", "{\n  \"test\":\"value\"\n}"),
    ("3000", "3"),
    ("0.5", "50.0%")
  ]
  
  let batch_results = batch_validate_transforms(transforms, test_cases)
  assert_true(batch_results.all(fn(r) { r.is_valid }))
}

// 测试5: 数据模式验证
test "数据模式验证" {
  // 数据字段类型
  enum FieldType {
    StringType
    IntType
    FloatType
    BoolType
    TimestampType
  }
  
  // 字段定义
  type FieldDefinition = {
    name: String,
    field_type: FieldType,
    required: Bool,
    min_value: Option[Float],
    max_value: Option[Float]
  }
  
  // 数据模式
  type DataSchema = {
    name: String,
    version: String,
    fields: Array[FieldDefinition>
  }
  
  // 模式验证结果
  type SchemaValidationResult = {
    is_valid: Bool,
    schema_name: String,
    validation_errors: Array[String]
  }
  
  // 验证字段值
  let validate_field_value = fn(field_def: FieldDefinition, value: String) {
    let mut errors = []
    
    // 检查必需字段
    if field_def.required && (value == "" || value == null) {
      errors = errors.push("缺少必需字段: " + field_def.name)
      return errors
    }
    
    if value == "" || value == null {
      return errors // 可选字段为空是有效的
    }
    
    // 检查类型
    match field_def.field_type {
      FieldType::StringType => {
        // 字符串类型总是有效
      }
      FieldType::IntType => {
        match value.to_int() {
          Some(_) => {}
          None => {
            errors = errors.push("字段 " + field_def.name + " 应为整数类型")
          }
        }
      }
      FieldType::FloatType => {
        match value.to_float() {
          Some(_) => {}
          None => {
            errors = errors.push("字段 " + field_def.name + " 应为浮点数类型")
          }
        }
      }
      FieldType::BoolType => {
        if value != "true" && value != "false" {
          errors = errors.push("字段 " + field_def.name + " 应为布尔类型 (true/false)")
        }
      }
      FieldType::TimestampType => {
        match value.to_int() {
          Some(timestamp) => {
            if timestamp < 0 {
              errors = errors.push("字段 " + field_def.name + " 时间戳不能为负数")
            }
          }
          None => {
            errors = errors.push("字段 " + field_def.name + " 应为时间戳类型")
          }
        }
      }
    }
    
    // 检查值范围
    match field_def.field_type {
      FieldType::IntType => {
        match value.to_int() {
          Some(int_value) => {
            let float_value = int_value as Float
            match field_def.min_value {
              Some(min) => {
                if float_value < min {
                  errors = errors.push("字段 " + field_def.name + " 值小于最小值 " + min.to_string())
                }
              }
              None => {}
            }
            
            match field_def.max_value {
              Some(max) => {
                if float_value > max {
                  errors = errors.push("字段 " + field_def.name + " 值大于最大值 " + max.to_string())
                }
              }
              None => {}
            }
          }
          None => {}
        }
      }
      FieldType::FloatType => {
        match value.to_float() {
          Some(float_value) => {
            match field_def.min_value {
              Some(min) => {
                if float_value < min {
                  errors = errors.push("字段 " + field_def.name + " 值小于最小值 " + min.to_string())
                }
              }
              None => {}
            }
            
            match field_def.max_value {
              Some(max) => {
                if float_value > max {
                  errors = errors.push("字段 " + field_def.name + " 值大于最大值 " + max.to_string())
                }
              }
              None => {}
            }
          }
          None => {}
        }
      }
      _ => {}
    }
    
    errors
  }
  
  // 验证数据模式
  let validate_schema = fn(schema: DataSchema, data: Map[String, String]) {
    let mut all_errors = []
    
    for field_def in schema.fields {
      let value = match Map::get(data, field_def.name) {
        Some(v) => v
        None => ""
      }
      
      let field_errors = validate_field_value(field_def, value)
      all_errors = all_errors.concat(field_errors)
    }
    
    {
      is_valid: all_errors.length() == 0,
      schema_name: schema.name,
      validation_errors: all_errors
    }
  }
  
  // 创建测试模式
  let metric_schema = {
    name: "metric_data",
    version: "1.0",
    fields: [
      {
        name: "metric_name",
        field_type: FieldType::StringType,
        required: true,
        min_value: None,
        max_value: None
      },
      {
        name: "value",
        field_type: FieldType::FloatType,
        required: true,
        min_value: Some(0.0),
        max_value: Some(1000.0)
      },
      {
        name: "timestamp",
        field_type: FieldType::TimestampType,
        required: true,
        min_value: None,
        max_value: None
      },
      {
        name: "is_anomaly",
        field_type: FieldType::BoolType,
        required: false,
        min_value: None,
        max_value: None
      },
      {
        name: "count",
        field_type: FieldType::IntType,
        required: false,
        min_value: Some(0.0),
        max_value: None
      }
    ]
  }
  
  // 测试有效数据
  let valid_data = [
    ("metric_name", "cpu_usage"),
    ("value", "75.5"),
    ("timestamp", "1625097600"),
    ("is_anomaly", "false"),
    ("count", "100")
  ]
  
  let valid_result = validate_schema(metric_schema, valid_data)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.validation_errors.length(), 0)
  
  // 测试无效数据
  let invalid_data = [
    ("metric_name", ""), // 必需字段为空
    ("value", "1500.0"), // 超出最大值
    ("timestamp", "-100"), // 负时间戳
    ("is_anomaly", "maybe"), // 无效布尔值
    ("count", "-5") // 小于最小值
  ]
  
  let invalid_result = validate_schema(metric_schema, invalid_data)
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.validation_errors.length() > 0)
  
  // 验证具体错误
  let errors = invalid_result.validation_errors
  assert_true(errors.any(fn(e) { e.contains("缺少必需字段") }))
  assert_true(errors.any(fn(e) { e.contains("值大于最大值") }))
  assert_true(errors.any(fn(e) { e.contains("时间戳不能为负数") }))
  assert_true(errors.any(fn(e) { e.contains("应为布尔类型") }))
  assert_true(errors.any(fn(e) { e.contains("值小于最小值") }))
}