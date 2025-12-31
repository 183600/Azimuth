// 遥测状态码测试用例

test "basic_status_code_operations" {
  // 测试基本状态码操作
  
  // 测试预定义状态码
  let ok_status = azimuth::telemetry::api::trace::StatusCode::OK
  assert_eq(ok_status.get_code(), "OK")
  assert_eq(ok_status.get_description(), "Operation completed successfully")
  assert_eq(ok_status.is_error(), false)
  
  let error_status = azimuth::telemetry::api::trace::StatusCode::ERROR
  assert_eq(error_status.get_code(), "ERROR")
  assert_eq(error_status.get_description(), "Operation failed")
  assert_eq(error_status.is_error(), true)
  
  // 测试自定义状态码
  let custom_status = azimuth::telemetry::api::trace::StatusCode::custom("TIMEOUT", "Operation timed out")
  assert_eq(custom_status.get_code(), "TIMEOUT")
  assert_eq(custom_status.get_description(), "Operation timed out")
  assert_eq(custom_status.is_error(), true)
}

test "http_status_code_mapping" {
  // 测试HTTP状态码映射
  
  // 测试成功状态码
  let http_200 = azimuth::telemetry::api::trace::StatusCode::from_http(200)
  assert_eq(http_200.get_code(), "OK")
  assert_eq(http_200.is_error(), false)
  
  let http_201 = azimuth::telemetry::api::trace::StatusCode::from_http(201)
  assert_eq(http_201.get_code(), "OK")
  assert_eq(http_201.is_error(), false)
  
  let http_204 = azimuth::telemetry::api::trace::StatusCode::from_http(204)
  assert_eq(http_204.get_code(), "OK")
  assert_eq(http_204.is_error(), false)
  
  // 测试客户端错误状态码
  let http_400 = azimuth::telemetry::api::trace::StatusCode::from_http(400)
  assert_eq(http_400.get_code(), "ERROR")
  assert_eq(http_400.is_error(), true)
  
  let http_401 = azimuth::telemetry::api::trace::StatusCode::from_http(401)
  assert_eq(http_401.get_code(), "ERROR")
  assert_eq(http_401.is_error(), true)
  
  let http_404 = azimuth::telemetry::api::trace::StatusCode::from_http(404)
  assert_eq(http_404.get_code(), "ERROR")
  assert_eq(http_404.is_error(), true)
  
  // 测试服务器错误状态码
  let http_500 = azimuth::telemetry::api::trace::StatusCode::from_http(500)
  assert_eq(http_500.get_code(), "ERROR")
  assert_eq(http_500.is_error(), true)
  
  let http_502 = azimuth::telemetry::api::trace::StatusCode::from_http(502)
  assert_eq(http_502.get_code(), "ERROR")
  assert_eq(http_502.is_error(), true)
  
  let http_503 = azimuth::telemetry::api::trace::StatusCode::from_http(503)
  assert_eq(http_503.get_code(), "ERROR")
  assert_eq(http_503.is_error(), true)
}

test "grpc_status_code_mapping" {
  // 测试gRPC状态码映射
  
  // 测试成功状态码
  let grpc_ok = azimuth::telemetry::api::trace::StatusCode::from_grpc(0)  // gRPC OK
  assert_eq(grpc_ok.get_code(), "OK")
  assert_eq(grpc_ok.is_error(), false)
  
  // 测试错误状态码
  let grpc_cancelled = azimuth::telemetry::api::trace::StatusCode::from_grpc(1)  // gRPC CANCELLED
  assert_eq(grpc_cancelled.get_code(), "ERROR")
  assert_eq(grpc_cancelled.is_error(), true)
  
  let grpc_unknown = azimuth::telemetry::api::trace::StatusCode::from_grpc(2)  // gRPC UNKNOWN
  assert_eq(grpc_unknown.get_code(), "ERROR")
  assert_eq(grpc_unknown.is_error(), true)
  
  let grpc_deadline_exceeded = azimuth::telemetry::api::trace::StatusCode::from_grpc(4)  // gRPC DEADLINE_EXCEEDED
  assert_eq(grpc_deadline_exceeded.get_code(), "ERROR")
  assert_eq(grpc_deadline_exceeded.is_error(), true)
  
  let grpc_not_found = azimuth::telemetry::api::trace::StatusCode::from_grpc(5)  // gRPC NOT_FOUND
  assert_eq(grpc_not_found.get_code(), "ERROR")
  assert_eq(grpc_not_found.is_error(), true)
  
  let grpc_permission_denied = azimuth::telemetry::api::trace::StatusCode::from_grpc(7)  // gRPC PERMISSION_DENIED
  assert_eq(grpc_permission_denied.get_code(), "ERROR")
  assert_eq(grpc_permission_denied.is_error(), true)
  
  let grpc_unauthenticated = azimuth::telemetry::api::trace::StatusCode::from_grpc(16)  // gRPC UNAUTHENTICATED
  assert_eq(grpc_unauthenticated.get_code(), "ERROR")
  assert_eq(grpc_unauthenticated.is_error(), true)
  
  let grpc_unavailable = azimuth::telemetry::api::trace::StatusCode::from_grpc(14)  // gRPC UNAVAILABLE
  assert_eq(grpc_unavailable.get_code(), "ERROR")
  assert_eq(grpc_unavailable.is_error(), true)
}

test "status_code_with_error_details" {
  // 测试带错误详情的状态码
  
  // 创建带错误详情的状态码
  let error_status = azimuth::telemetry::api::trace::StatusCode::error_with_details(
    "DATABASE_ERROR",
    "Failed to connect to database",
    azimuth::telemetry::api::trace::ErrorDetails::new()
      .with_exception_type("ConnectionException")
      .with_stack_trace("at db.connect() line 42\nat service.init() line 123")
      .with_retryable(true)
      .with_error_code("DB_CONN_001")
  )
  
  assert_eq(error_status.get_code(), "DATABASE_ERROR")
  assert_eq(error_status.get_description(), "Failed to connect to database")
  assert_eq(error_status.is_error(), true)
  
  let details = error_status.get_error_details().unwrap()
  assert_eq(details.get_exception_type(), "ConnectionException")
  assert_eq(details.get_stack_trace().contains("db.connect()"), true)
  assert_eq(details.is_retryable(), true)
  assert_eq(details.get_error_code(), "DB_CONN_001")
}

test "status_code_comparison_and_equality" {
  // 测试状态码比较和相等性
  
  let ok1 = azimuth::telemetry::api::trace::StatusCode::OK
  let ok2 = azimuth::telemetry::api::trace::StatusCode::OK
  let error1 = azimuth::telemetry::api::trace::StatusCode::ERROR
  let custom1 = azimuth::telemetry::api::trace::StatusCode::custom("TIMEOUT", "Timeout")
  let custom2 = azimuth::telemetry::api::trace::StatusCode::custom("TIMEOUT", "Timeout")
  let custom3 = azimuth::telemetry::api::trace::StatusCode::custom("TIMEOUT", "Different description")
  
  // 测试相等性
  assert_eq(ok1.equals(ok2), true)
  assert_eq(ok1.equals(error1), false)
  assert_eq(custom1.equals(custom2), true)
  assert_eq(custom1.equals(custom3), false)  // 不同描述
  
  // 测试比较
  assert_eq(ok1.compare_to(error1) < 0, true)  // OK < ERROR
  assert_eq(error1.compare_to(ok1) > 0, true)  // ERROR > OK
  assert_eq(custom1.compare_to(custom2), 0)     // 相等
  
  // 测试排序
  let status_codes = [error1, ok1, custom1]
  let sorted_codes = status_codes.sorted()
  assert_eq(sorted_codes[0].equals(ok1), true)
  assert_eq(sorted_codes[1].equals(custom1), true)
  assert_eq(sorted_codes[2].equals(error1), true)
}

test "status_code_categories" {
  // 测试状态码分类
  
  // 测试成功类别
  let ok_status = azimuth::telemetry::api::trace::StatusCode::OK
  assert_eq(ok_status.get_category(), "SUCCESS")
  assert_eq(ok_status.is_success(), true)
  assert_eq(ok_status.is_client_error(), false)
  assert_eq(ok_status.is_server_error(), false)
  
  // 测试客户端错误类别
  let client_error = azimuth::telemetry::api::trace::StatusCode::from_http(400)
  assert_eq(client_error.get_category(), "CLIENT_ERROR")
  assert_eq(client_error.is_success(), false)
  assert_eq(client_error.is_client_error(), true)
  assert_eq(client_error.is_server_error(), false)
  
  // 测试服务器错误类别
  let server_error = azimuth::telemetry::api::trace::StatusCode::from_http(500)
  assert_eq(server_error.get_category(), "SERVER_ERROR")
  assert_eq(server_error.is_success(), false)
  assert_eq(server_error.is_client_error(), false)
  assert_eq(server_error.is_server_error(), true)
  
  // 测试自定义类别
  let custom_error = azimuth::telemetry::api::trace::StatusCode::custom("NETWORK_ERROR", "Network failure")
  assert_eq(custom_error.get_category(), "UNKNOWN")
  assert_eq(custom_error.is_success(), false)
  assert_eq(custom_error.is_client_error(), false)
  assert_eq(custom_error.is_server_error(), false)
}

test "status_code_serialization" {
  // 测试状态码序列化
  
  let status = azimuth::telemetry::api::trace::StatusCode::error_with_details(
    "VALIDATION_ERROR",
    "Input validation failed",
    azimuth::telemetry::api::trace::ErrorDetails::new()
      .with_exception_type("ValidationException")
      .with_field("email", "Invalid email format")
      .with_field("age", "Age must be positive")
      .with_retryable(false)
  )
  
  // 序列化为JSON
  let json_status = status.to_json()
  assert_eq(json_status.length() > 0, true)
  assert_eq(json_status.contains("VALIDATION_ERROR"), true)
  assert_eq(json_status.contains("Input validation failed"), true)
  assert_eq(json_status.contains("ValidationException"), true)
  assert_eq(json_status.contains("Invalid email format"), true)
  
  // 从JSON恢复
  let restored_status = azimuth::telemetry::api::trace::StatusCode::from_json(json_status)
  assert_eq(restored_status.get_code(), status.get_code())
  assert_eq(restored_status.get_description(), status.get_description())
  assert_eq(restored_status.is_error(), status.is_error())
  
  let restored_details = restored_status.get_error_details().unwrap()
  let original_details = status.get_error_details().unwrap()
  assert_eq(restored_details.get_exception_type(), original_details.get_exception_type())
  assert_eq(restored_details.is_retryable(), original_details.is_retryable())
}

test "status_code_chaining_and_causality" {
  // 测试状态码链和因果关系
  
  // 创建根本原因
  let root_cause = azimuth::telemetry::api::trace::StatusCode::error_with_details(
    "NETWORK_TIMEOUT",
    "Network connection timeout",
    azimuth::telemetry::api::trace::ErrorDetails::new()
      .with_exception_type("TimeoutException")
      .with_retryable(true)
  )
  
  // 创建中间原因
  let intermediate_cause = azimuth::telemetry::api::trace::StatusCode::error_with_details(
    "DATABASE_UNAVAILABLE",
    "Database not responding",
    azimuth::telemetry::api::trace::ErrorDetails::new()
      .with_exception_type("DatabaseException")
      .with_retryable(true)
      .with_cause(root_cause)
  )
  
  // 创建最终错误
  let final_error = azimuth::telemetry::api::trace::StatusCode::error_with_details(
    "USER_REQUEST_FAILED",
    "Failed to process user request",
    azimuth::telemetry::api::trace::ErrorDetails::new()
      .with_exception_type("RequestException")
      .with_retryable(false)
      .with_cause(intermediate_cause)
  )
  
  // 验证错误链
  assert_eq(final_error.get_code(), "USER_REQUEST_FAILED")
  assert_eq(final_error.is_error(), true)
  
  let cause_chain = final_error.get_cause_chain()
  assert_eq(cause_chain.length(), 3)
  assert_eq(cause_chain[0].get_code(), "USER_REQUEST_FAILED")
  assert_eq(cause_chain[1].get_code(), "DATABASE_UNAVAILABLE")
  assert_eq(cause_chain[2].get_code(), "NETWORK_TIMEOUT")
  
  // 验证根本原因
  let root = final_error.get_root_cause().unwrap()
  assert_eq(root.get_code(), "NETWORK_TIMEOUT")
  assert_eq(root.is_retryable(), true)
}

test "status_code_metrics_and_analytics" {
  // 测试状态码指标和分析
  
  let collector = azimuth::telemetry::api::trace::StatusCodeCollector::new()
  
  // 记录不同的状态码
  collector.record(azimuth::telemetry::api::trace::StatusCode::OK)
  collector.record(azimuth::telemetry::api::trace::StatusCode::OK)
  collector.record(azimuth::telemetry::api::trace::StatusCode::from_http(200))
  collector.record(azimuth::telemetry::api::trace::StatusCode::from_http(404))
  collector.record(azimuth::telemetry::api::trace::StatusCode::from_http(500))
  collector.record(azimuth::telemetry::api::trace::StatusCode::from_http(500))
  collector.record(azimuth::telemetry::api::trace::StatusCode::custom("TIMEOUT", "Timeout"))
  
  // 获取统计信息
  let stats = collector.get_statistics()
  
  assert_eq(stats.get_total_count(), 7)
  assert_eq(stats.get_success_count(), 3)  // OK + OK + 200
  assert_eq(stats.get_error_count(), 4)    // 404 + 500 + 500 + TIMEOUT
  assert_eq(stats.get_success_rate(), 3.0 / 7.0)
  assert_eq(stats.get_error_rate(), 4.0 / 7.0)
  
  // 按状态码分组
  let by_code = stats.get_counts_by_code()
  assert_eq(by_code.get("OK"), 2)
  assert_eq(by_code.get("ERROR"), 3)  // 404 + 500 + 500
  assert_eq(by_code.get("TIMEOUT"), 1)
  
  // 按类别分组
  let by_category = stats.get_counts_by_category()
  assert_eq(by_category.get("SUCCESS"), 3)
  assert_eq(by_category.get("CLIENT_ERROR"), 1)  // 404
  assert_eq(by_category.get("SERVER_ERROR"), 2)  // 500 + 500
  assert_eq(by_category.get("UNKNOWN"), 1)       // TIMEOUT
}

test "status_code_validation_and_edge_cases" {
  // 测试状态码验证和边界情况
  
  // 测试空状态码
  let empty_result = azimuth::telemetry::api::trace::StatusCode::validate("")
  assert_eq(empty_result.is_valid(), false)
  assert_eq(empty_result.get_error_message().is_some(), true)
  
  // 测试无效HTTP状态码
  let invalid_http = azimuth::telemetry::api::trace::StatusCode::from_http(999)
  assert_eq(invalid_http.get_code(), "ERROR")
  assert_eq(invalid_http.is_error(), true)
  
  let negative_http = azimuth::telemetry::api::trace::StatusCode::from_http(-1)
  assert_eq(negative_http.get_code(), "ERROR")
  assert_eq(negative_http.is_error(), true)
  
  // 测试无效gRPC状态码
  let invalid_grpc = azimuth::telemetry::api::trace::StatusCode::from_grpc(999)
  assert_eq(invalid_grpc.get_code(), "ERROR")
  assert_eq(invalid_grpc.is_error(), true)
  
  let negative_grpc = azimuth::telemetry::api::trace::StatusCode::from_grpc(-1)
  assert_eq(negative_grpc.get_code(), "ERROR")
  assert_eq(negative_grpc.is_error(), true)
  
  // 测试极长的状态码和描述
  let long_code = "THIS_IS_A_VERY_LONG_STATUS_CODE_NAME_THAT_EXCEEDS_NORMAL_LIMITS"
  let long_description = "This is a very long description that contains a lot of detailed information about the error condition and might be used to test the system's ability to handle extremely long descriptions without issues"
  
  let long_status = azimuth::telemetry::api::trace::StatusCode::custom(long_code, long_description)
  assert_eq(long_status.get_code().length(), long_code.length())
  assert_eq(long_status.get_description().length(), long_description.length())
  
  // 测试特殊字符
  let special_status = azimuth::telemetry::api::trace::StatusCode::custom("ERROR@#$%", "Error with special chars: !@#$%^&*()")
  assert_eq(special_status.get_code(), "ERROR@#$%")
  assert_eq(special_status.get_description(), "Error with special chars: !@#$%^&*()")
  
  // 测试Unicode
  let unicode_status = azimuth::telemetry::api::trace::StatusCode::custom("错误", "这是一个包含中文的错误描述 🚀")
  assert_eq(unicode_status.get_code(), "错误")
  assert_eq(unicode_status.get_description(), "这是一个包含中文的错误描述 🚀")
}