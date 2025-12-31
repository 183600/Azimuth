// 结构化日志格式化测试
// 测试不同格式的日志输出和结构化数据处理

test "json_log_formatting" {
  // 测试JSON格式日志输出
  
  // 创建结构化日志记录
  let log_entry = LogRecord{
    timestamp: 1609459200123L,
    level: "INFO",
    message: "User authentication successful",
    resource: Resource{
      service_name: "auth-service",
      service_version: "1.2.3",
      deployment_environment: "production"
    },
    attributes: [
      ("user.id", "12345"),
      ("auth.method", "jwt"),
      ("session.duration_ms", "250"),
      ("ip.address", "192.168.1.100"),
      ("user.agent", "Mozilla/5.0...")
    ],
    trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
    span_id: Some("00f067aa0ba902b7")
  }
  
  // 格式化为JSON
  let json_output = format_log_as_json(log_entry)
  
  // 验证JSON结构
  assert_eq(json_output.contains("\"timestamp\": 1609459200123"), true)
  assert_eq(json_output.contains("\"level\": \"INFO\""), true)
  assert_eq(json_output.contains("\"message\": \"User authentication successful\""), true)
  assert_eq(json_output.contains("\"service_name\": \"auth-service\""), true)
  assert_eq(json_output.contains("\"user.id\": \"12345\""), true)
  assert_eq(json_output.contains("\"trace_id\": \"4bf92f3577b34da6a3ce929d0e0e4736\""), true)
  
  // 验证JSON有效性
  let parsed_json = parse_json(json_output)
  match parsed_json {
    Ok(obj) => {
      assert_eq(obj["timestamp"], 1609459200123L)
      assert_eq(obj["level"], "INFO")
      assert_eq(obj["message"], "User authentication successful")
      assert_eq(obj["resource"]["service_name"], "auth-service")
      assert_eq(obj["attributes"]["user.id"], "12345")
    }
    Err(_) => @test.fail("Invalid JSON output")
  }
}

test "log_template_formatting" {
  // 测试日志模板格式化
  
  // 定义日志模板
  let template = "[{timestamp}] {level} {service_name} - {message} | user={user_id} method={auth_method} duration={session_duration}ms"
  
  // 创建日志数据
  let log_data = {
    "timestamp": "2021-01-01T00:00:00.123Z",
    "level": "INFO", 
    "service_name": "auth-service",
    "message": "User login completed",
    "user_id": "user-12345",
    "auth_method": "oauth2",
    "session_duration": "150"
  }
  
  // 应用模板
  let formatted_log = apply_log_template(template, log_data)
  
  // 验证模板应用结果
  assert_eq(formatted_log, "[2021-01-01T00:00:00.123Z] INFO auth-service - User login completed | user=user-12345 method=oauth2 duration=150ms")
  
  // 测试缺失字段的处理
  let incomplete_data = {
    "timestamp": "2021-01-01T00:00:01.456Z",
    "level": "ERROR",
    "service_name": "user-service", 
    "message": "Database connection failed"
    // 缺少user_id, auth_method, session_duration
  }
  
  let formatted_incomplete = apply_log_template(template, incomplete_data)
  assert_eq(formatted_incomplete, "[2021-01-01T00:00:01.456Z] ERROR user-service - Database connection failed | user=<missing> method=<missing> duration=<missing>ms")
}

test "log_level_filtering" {
  // 测试日志级别过滤
  
  // 创建不同级别的日志记录
  let log_records = [
    LogRecord{ timestamp: 1609459200000L, level: "DEBUG", message: "Debug info" },
    LogRecord{ timestamp: 1609459201000L, level: "INFO", message: "Info message" },
    LogRecord{ timestamp: 1609459202000L, level: "WARN", message: "Warning message" },
    LogRecord{ timestamp: 1609459203000L, level: "ERROR", message: "Error occurred" },
    LogRecord{ timestamp: 1609459204000L, level: "FATAL", message: "Fatal error" }
  ]
  
  // 测试不同级别的过滤
  let debug_logs = filter_logs_by_level(log_records, "DEBUG")
  assert_eq(debug_logs.length(), 5)
  
  let info_logs = filter_logs_by_level(log_records, "INFO")
  assert_eq(info_logs.length(), 4) // INFO, WARN, ERROR, FATAL
  
  let warn_logs = filter_logs_by_level(log_records, "WARN") 
  assert_eq(warn_logs.length(), 3) // WARN, ERROR, FATAL
  
  let error_logs = filter_logs_by_level(log_records, "ERROR")
  assert_eq(error_logs.length(), 2) // ERROR, FATAL
  
  let fatal_logs = filter_logs_by_level(log_records, "FATAL")
  assert_eq(fatal_logs.length(), 1) // FATAL only
  
  // 验证过滤结果的级别正确性
  for log in info_logs {
    assert_eq(log.level != "DEBUG", true)
    assert_eq(["INFO", "WARN", "ERROR", "FATAL"].contains(log.level), true)
  }
}

test "log_compression_and_batching" {
  // 测试日志压缩和批处理
  
  // 创建大量日志记录
  let large_log_batch = []
  let base_time = 1609459200L
  
  for i = 0; i < 1000; i = i + 1 {
    large_log_batch.push(LogRecord{
      timestamp: base_time + i.to_int64(),
      level: if i % 100 == 0 { "ERROR" } else { "INFO" },
      message: "Log message " + i.to_string(),
      attributes: [("request.id", "req-" + i.to_string())]
    })
  }
  
  // 原始数据大小
  let original_size = calculate_log_batch_size(large_log_batch)
  assert_eq(original_size > 50000, true) // 确保数据足够大
  
  // 压缩日志批次
  let compressed_batch = compress_log_batch(large_log_batch)
  let compressed_size = compressed_batch.length()
  
  // 验证压缩效果
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  assert_eq(compression_ratio < 0.7, true) // 至少30%的压缩率
  
  // 解压缩并验证完整性
  let decompressed_batch = decompress_log_batch(compressed_batch)
  assert_eq(decompressed_batch.length(), large_log_batch.length())
  
  // 验证首尾日志记录
  assert_eq(decompressed_batch[0].message, "Log message 0")
  assert_eq(decompressed_batch[999].message, "Log message 999")
  assert_eq(decompressed_batch[100].level, "ERROR") // 每100条有一个ERROR
}

test "log_search_and_indexing" {
  // 测试日志搜索和索引功能
  
  // 创建多样化的日志记录
  let searchable_logs = [
    LogRecord{
      timestamp: 1609459200000L,
      level: "INFO",
      message: "User login successful for user_id: 12345",
      attributes: [
        ("user.id", "12345"),
        ("action", "login"),
        ("ip.address", "192.168.1.100")
      ]
    },
    LogRecord{
      timestamp: 1609459205000L,
      level: "ERROR", 
      message: "Database connection failed to host: db-primary",
      attributes: [
        ("component", "database"),
        ("error.code", "CONN_TIMEOUT"),
        ("host", "db-primary")
      ]
    },
    LogRecord{
      timestamp: 1609459210000L,
      level: "WARN",
      message: "Rate limit exceeded for IP: 192.168.1.200",
      attributes: [
        ("ip.address", "192.168.1.200"),
        ("action", "rate_limit"),
        ("limit.type", "ip_based")
      ]
    },
    LogRecord{
      timestamp: 1609459215000L,
      level: "INFO",
      message: "User logout for user_id: 12345",
      attributes: [
        ("user.id", "12345"),
        ("action", "logout"),
        ("session.duration", "3600")
      ]
    }
  ]
  
  // 创建搜索索引
  let search_index = build_log_search_index(searchable_logs)
  
  // 按消息内容搜索
  let user_logs = search_logs(search_index, "user_id: 12345")
  assert_eq(user_logs.length(), 2)
  
  // 按属性搜索
  let db_logs = search_logs_by_attribute(search_index, "component", "database")
  assert_eq(db_logs.length(), 1)
  assert_eq(db_logs[0].level, "ERROR")
  
  // 按级别搜索
  let error_logs = search_logs_by_level(search_index, "ERROR")
  assert_eq(error_logs.length(), 1)
  assert_eq(error_logs[0].message.contains("Database connection"), true)
  
  // 按时间范围搜索
  let time_filtered = search_logs_by_time_range(search_index, 1609459201000L, 1609459212000L)
  assert_eq(time_filtered.length(), 2) // 排除第一条和最后一条
  
  // 复合搜索
  let complex_search = search_logs_complex(search_index, SearchCriteria{
    level: Some("INFO"),
    attribute_filter: Some(("user.id", "12345")),
    time_range: Some((1609459200000L, 1609459215000L)),
    message_contains: Some("")
  })
  assert_eq(complex_search.length(), 1)  // 只有第一条记录满足所有条件
}