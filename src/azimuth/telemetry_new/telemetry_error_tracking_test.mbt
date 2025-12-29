// 遥测错误追踪测试用例

test "error_classification_categorization" {
  // 测试错误分类和归类
  
  let error_types = [
    ("ValidationError", "client", 400),
    ("AuthenticationError", "client", 401),
    ("PermissionError", "client", 403),
    ("NotFoundError", "client", 404),
    ("RateLimitError", "client", 429),
    ("InternalError", "server", 500),
    ("DatabaseError", "server", 500),
    ("NetworkError", "server", 503),
    ("TimeoutError", "server", 504)
  ]
  
  // 验证错误类型数量
  assert_eq(error_types.length(), 9)
  
  // 统计客户端错误
  let mut client_errors = 0
  let mut server_errors = 0
  for error in error_types {
    if error.1 == "client" {
      client_errors = client_errors + 1
    } else if error.1 == "server" {
      server_errors = server_errors + 1
    }
  }
  
  // 验证错误分类统计
  assert_eq(client_errors, 5)
  assert_eq(server_errors, 4)
  
  // 验证特定错误类型
  assert_eq(error_types[0].0, "ValidationError")
  assert_eq(error_types[0].2, 400)
  assert_eq(error_types[5].0, "InternalError")
  assert_eq(error_types[5].2, 500)
}

test "error_stack_trace_analysis" {
  // 测试错误堆栈跟踪分析
  
  let stack_frames = [
    ("UserService.createUser", "user_service.mbt", 45),
    ("ValidationService.validate", "validation_service.mbt", 23),
    ("DatabaseService.insert", "database_service.mbt", 67),
    ("ConnectionPool.execute", "connection_pool.mbt", 89),
    ("Driver.query", "driver.mbt", 123)
  ]
  
  // 验证堆栈帧数量
  assert_eq(stack_frames.length(), 5)
  
  // 验证顶层函数
  assert_eq(stack_frames[0].0, "UserService.createUser")
  assert_eq(stack_frames[0].1, "user_service.mbt")
  assert_eq(stack_frames[0].2, 45)
  
  // 验证底层函数
  assert_eq(stack_frames[4].0, "Driver.query")
  assert_eq(stack_frames[4].1, "driver.mbt")
  assert_eq(stack_frames[4].2, 123)
  
  // 验证行号递增趋势 (通常底层调用行号更大)
  let mut i = 1
  while i < stack_frames.length() {
    assert_eq(stack_frames[i].2 > stack_frames[i-1].2, true)
    i = i + 1
  }
  
  // 创建堆栈跟踪字符串
  let stack_trace = ""
  let mut i = 0
  while i < stack_frames.length() {
    if i > 0 {
      stack_trace = stack_trace + "\n"
    }
    stack_trace = stack_trace + "  at " + stack_frames[i].0 + " (" + stack_frames[i].1 + ":" + stack_frames[i].2.to_string() + ")"
    i = i + 1
  }
  
  // 验证堆栈跟踪格式
  assert_eq(stack_trace.contains("UserService.createUser"), true)
  assert_eq(stack_trace.contains("user_service.mbt:45"), true)
  assert_eq(stack_trace.contains("Driver.query"), true)
  assert_eq(stack_trace.contains("driver.mbt:123"), true)
}

test "error_frequency_analysis" {
  // 测试错误频率分析
  
  let error_occurrences = [
    ("DatabaseConnectionError", 15),
    ("TimeoutError", 8),
    ("ValidationError", 25),
    ("AuthenticationError", 12),
    ("NetworkError", 5),
    ("RateLimitError", 3)
  ]
  
  // 验证错误类型数量
  assert_eq(error_occurrences.length(), 6)
  
  // 找出最频繁的错误
  let mut max_error = error_occurrences[0]
  let mut i = 1
  while i < error_occurrences.length() {
    if error_occurrences[i].1 > max_error.1 {
      max_error = error_occurrences[i]
    }
    i = i + 1
  }
  
  // 验证最频繁错误
  assert_eq(max_error.0, "ValidationError")
  assert_eq(max_error.1, 25)
  
  // 找出最不频繁的错误
  let mut min_error = error_occurrences[0]
  i = 1
  while i < error_occurrences.length() {
    if error_occurrences[i].1 < min_error.1 {
      min_error = error_occurrences[i]
    }
    i = i + 1
  }
  
  // 验证最不频繁错误
  assert_eq(min_error.0, "RateLimitError")
  assert_eq(min_error.1, 3)
  
  // 计算总错误数
  let mut total_errors = 0
  for error in error_occurrences {
    total_errors = total_errors + error.1
  }
  assert_eq(total_errors, 68)
  
  // 计算最频繁错误的百分比
  let max_error_percentage = (max_error.1 * 100) / total_errors
  assert_eq(max_error_percentage, 36)
}

test "error_impact_assessment" {
  // 测试错误影响评估
  
  let error_impacts = [
    ("ValidationError", "low", 1, 5),      // (错误类型, 影响级别, 严重性, 频率)
    ("TimeoutError", "medium", 3, 8),
    ("DatabaseError", "high", 4, 6),
    ("NetworkPartition", "critical", 5, 2),
    ("AuthenticationError", "medium", 2, 12)
  ]
  
  // 验证错误影响记录数量
  assert_eq(error_impacts.length(), 5)
  
  // 统计不同影响级别的错误
  let mut low_impact = 0
  let mut medium_impact = 0
  let mut high_impact = 0
  let mut critical_impact = 0
  
  for impact in error_impacts {
    match impact.1 {
      "low" => low_impact = low_impact + 1
      "medium" => medium_impact = medium_impact + 1
      "high" => high_impact = high_impact + 1
      "critical" => critical_impact = critical_impact + 1
      _ => () // 忽略其他情况
    }
  }
  
  // 验证影响级别统计
  assert_eq(low_impact, 1)
  assert_eq(medium_impact, 2)
  assert_eq(high_impact, 1)
  assert_eq(critical_impact, 1)
  
  // 计算风险分数 (严重性 × 频率)
  let risk_scores = []
  for impact in error_impacts {
    let risk_score = impact.2 * impact.3
    risk_scores.push((impact.0, risk_score))
  }
  
  // 验证风险分数计算
  assert_eq(risk_scores[0].0, "ValidationError")
  assert_eq(risk_scores[0].1, 5)  // 1 × 5
  
  assert_eq(risk_scores[2].0, "DatabaseError")
  assert_eq(risk_scores[2].1, 24) // 4 × 6
  
  assert_eq(risk_scores[3].0, "NetworkPartition")
  assert_eq(risk_scores[3].1, 10) // 5 × 2
  
  // 找出最高风险错误
  let mut max_risk = risk_scores[0]
  let mut i = 1
  while i < risk_scores.length() {
    if risk_scores[i].1 > max_risk.1 {
      max_risk = risk_scores[i]
    }
    i = i + 1
  }
  
  assert_eq(max_risk.0, "DatabaseError")
  assert_eq(max_risk.1, 24)
}