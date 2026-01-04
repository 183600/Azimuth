// Azimuth Premium Serialization/Deserialization Tests
// 高质量序列化/反序列化测试用例 - 专注于数据转换的准确性、效率和兼容性

test "遥测数据的JSON序列化和反序列化" {
  let json_serializer = JsonSerializer::new()
  let data_validator = SerializationDataValidator::new()
  
  // 创建复杂的遥测数据
  let telemetry_record = TelemetryRecord::new()
  TelemetryRecord::set_trace_id(telemetry_record, "0af7651916cd43dd8448eb211c80319c")
  TelemetryRecord::set_span_id(telemetry_record, "b7ad6b7169203331")
  TelemetryRecord::set_parent_span_id(telemetry_record, "b7ad6b7169203330")
  TelemetryRecord::set_operation_name(telemetry_record, "database_query")
  TelemetryRecord::set_start_time(telemetry_record, 1234567890123L)
  TelemetryRecord::set_end_time(telemetry_record, 1234567891123L)
  TelemetryRecord::set_status(telemetry_record, SpanStatus::Ok)
  
  // 添加各种类型的属性
  TelemetryRecord::add_attribute(telemetry_record, "service.name", StringValue("payment_service"))
  TelemetryRecord::add_attribute(telemetry_record, "service.version", StringValue("1.2.3"))
  TelemetryRecord::add_attribute(telemetry_record, "db.statement", StringValue("SELECT * FROM orders WHERE id = ?"))
  TelemetryRecord::add_attribute(telemetry_record, "db.type", StringValue("postgresql"))
  TelemetryRecord::add_attribute(telemetry_record, "db.rows_affected", IntValue(42))
  TelemetryRecord::add_attribute(telemetry_record, "db.duration_ms", FloatValue(123.45))
  TelemetryRecord::add_attribute(telemetry_record, "cache.hit", BoolValue(true))
  TelemetryRecord::add_attribute(telemetry_record, "http.status_code", IntValue(200))
  
  // 添加数组属性
  TelemetryRecord::add_attribute(telemetry_record, "tag.list", ArrayStringValue(["api", "payment", "critical"]))
  TelemetryRecord::add_attribute(telemetry_record, "retry.attempts", ArrayIntValue([1, 2, 3]))
  
  // 添加事件
  TelemetryRecord::add_event(telemetry_record, SpanEvent::new_with_attributes(
    "db.query.start",
    1234567890123L,
    [("query.type", StringValue("select")), ("table.name", StringValue("orders"))]
  ))
  
  TelemetryRecord::add_event(telemetry_record, SpanEvent::new_with_attributes(
    "db.query.complete",
    1234567891123L,
    [("rows.returned", IntValue(42)), ("execution.time", FloatValue(123.45))]
  ))
  
  // 添加链接
  TelemetryRecord::add_link(telemetry_record, SpanLink::new(
    "linked_trace_id",
    "linked_span_id",
    [("link.type", StringValue("parent")), ("correlation.id", StringValue("req-123"))]
  ))
  
  // 序列化为JSON
  let serialization_result = JsonSerializer::serialize(json_serializer, telemetry_record)
  match serialization_result {
    Success(json_data) => {
      assert_true(json_data.length() > 0)
      
      // 验证JSON结构
      assert_true(json_data.contains("\"traceId\":\"0af7651916cd43dd8448eb211c80319c\""))
      assert_true(json_data.contains("\"spanId\":\"b7ad6b7169203331\""))
      assert_true(json_data.contains("\"operationName\":\"database_query\""))
      assert_true(json_data.contains("\"attributes\""))
      assert_true(json_data.contains("\"events\""))
      assert_true(json_data.contains("\"links\""))
      
      // 反序列化
      let deserialization_result = JsonSerializer::deserialize(json_serializer, json_data)
      match deserialization_result {
        Success(deserialized_record) => {
          // 验证反序列化的数据
          assert_eq(TelemetryRecord::trace_id(deserialized_record), "0af7651916cd43dd8448eb211c80319c")
          assert_eq(TelemetryRecord::span_id(deserialized_record), "b7ad6b7169203331")
          assert_eq(TelemetryRecord::operation_name(deserialized_record), "database_query")
          assert_eq(TelemetryRecord::start_time(deserialized_record), 1234567890123L)
          assert_eq(TelemetryRecord::end_time(deserialized_record), 1234567891123L)
          
          // 验证属性
          let service_name = TelemetryRecord::get_attribute(deserialized_record, "service.name")
          match service_name {
            Some(StringValue(name)) => assert_eq(name, "payment_service")
            _ => assert_true(false)
          }
          
          let db_rows = TelemetryRecord::get_attribute(deserialized_record, "db.rows_affected")
          match db_rows {
            Some(IntValue(rows)) => assert_eq(rows, 42)
            _ => assert_true(false)
          }
          
          let cache_hit = TelemetryRecord::get_attribute(deserialized_record, "cache.hit")
          match cache_hit {
            Some(BoolValue(hit)) => assert_true(hit)
            _ => assert_true(false)
          }
          
          // 验证数组属性
          let tag_list = TelemetryRecord::get_attribute(deserialized_record, "tag.list")
          match tag_list {
            Some(ArrayStringValue(tags)) => {
              assert_eq(tags.length(), 3)
              assert_true(tags.contains("api"))
              assert_true(tags.contains("payment"))
              assert_true(tags.contains("critical"))
            }
            _ => assert_true(false)
          }
          
          // 验证事件
          let events = TelemetryRecord::events(deserialized_record)
          assert_eq(events.length(), 2)
          assert_eq(SpanEvent::name(events[0]), "db.query.start")
          assert_eq(SpanEvent::name(events[1]), "db.query.complete")
          
          // 验证链接
          let links = TelemetryRecord::links(deserialized_record)
          assert_eq(links.length(), 1)
          assert_eq(SpanLink::trace_id(links[0]), "linked_trace_id")
          assert_eq(SpanLink::span_id(links[0]), "linked_span_id")
          
          // 执行数据完整性验证
          let validation_result = SerializationDataValidator::validate_roundtrip(
            data_validator,
            telemetry_record,
            deserialized_record
          )
          assert_true(validation_result.is_identical)
          assert_eq(validation_result.differences.length(), 0)
        }
        Error(error) => assert_true(false, "反序列化失败: " + error.message)
      }
    }
    Error(error) => assert_true(false, "序列化失败: " + error.message)
  }
}

test "二进制序列化的性能和准确性" {
  let binary_serializer = BinarySerializer::new()
  let performance_profiler = SerializationPerformanceProfiler::new()
  
  // 创建大量测试数据
  let test_records = []
  for i in 0..10000 {
    let record = TelemetryRecord::new()
    TelemetryRecord::set_trace_id(record, "trace_" + i.to_string())
    TelemetryRecord::set_span_id(record, "span_" + i.to_string())
    TelemetryRecord::set_operation_name(record, "operation_" + (i % 100).to_string())
    TelemetryRecord::set_start_time(record, 1234567890L + i.to_long())
    TelemetryRecord::set_end_time(record, 1234567890L + i.to_long() + 100L)
    
    // 添加属性
    TelemetryRecord::add_attribute(record, "index", IntValue(i))
    TelemetryRecord::add_attribute(record, "value", FloatValue(i.to_float() * 1.5))
    TelemetryRecord::add_attribute(record, "category", StringValue("cat_" + (i % 10).to_string()))
    
    test_records.push(record)
  }
  
  // 性能测试：序列化
  SerializationPerformanceProfiler::start_profiling(performance_profiler)
  let binary_data = BinarySerializer::serialize_batch(binary_serializer, test_records)
  let serialization_performance = SerializationPerformanceProfiler::stop_profiling(performance_profiler)
  
  // 验证序列化性能
  assert_true(serialization_performance.total_time_ms > 0)
  assert_true(serialization_performance.throughput_records_per_sec > 1000) // 至少1000记录/秒
  assert_true(binary_data.length() > 0)
  
  // 计算压缩率
  let original_size = test_records.length() * 1000 // 估算原始大小
  let compression_ratio = binary_data.length() / original_size
  assert_true(compression_ratio < 0.5) // 二进制应该至少压缩50%
  
  // 性能测试：反序列化
  SerializationPerformanceProfiler::start_profiling(performance_profiler)
  let deserialized_records = BinarySerializer::deserialize_batch(binary_serializer, binary_data)
  let deserialization_performance = SerializationPerformanceProfiler::stop_profiling(performance_profiler)
  
  // 验证反序列化性能
  assert_true(deserialization_performance.total_time_ms > 0)
  assert_true(deserialization_performance.throughput_records_per_sec > 1000) // 至少1000记录/秒
  assert_eq(deserialized_records.length(), test_records.length())
  
  // 验证数据准确性
  for i in 0..test_records.length() {
    let original = test_records[i]
    let deserialized = deserialized_records[i]
    
    assert_eq(TelemetryRecord::trace_id(original), TelemetryRecord::trace_id(deserialized))
    assert_eq(TelemetryRecord::span_id(original), TelemetryRecord::span_id(deserialized))
    assert_eq(TelemetryRecord::operation_name(original), TelemetryRecord::operation_name(deserialized))
    assert_eq(TelemetryRecord::start_time(original), TelemetryRecord::start_time(deserialized))
    assert_eq(TelemetryRecord::end_time(original), TelemetryRecord::end_time(deserialized))
    
    // 验证属性
    let original_index = TelemetryRecord::get_attribute(original, "index")
    let deserialized_index = TelemetryRecord::get_attribute(deserialized, "index")
    match (original_index, deserialized_index) {
      (Some(IntValue(orig)), Some(IntValue(deser))) => assert_eq(orig, deser)
      _ => assert_true(false)
    }
  }
  
  // 内存使用测试
  let memory_usage = SerializationPerformanceProfiler::get_memory_usage(performance_profiler)
  assert_true(memory_usage.peak_memory_mb < 100) // 峰值内存使用应小于100MB
  assert_true(memory_usage.memory_per_record_kb < 10) // 每记录内存使用应小于10KB
}

test "跨版本序列化兼容性" {
  let compatibility_tester = SerializationCompatibilityTester::new()
  
  // 创建当前版本的遥测数据
  let current_record = TelemetryRecord::new()
  TelemetryRecord::set_trace_id(current_record, "compatibility_test_trace")
  TelemetryRecord::set_span_id(current_record, "compatibility_test_span")
  TelemetryRecord::set_operation_name(current_record, "compatibility_operation")
  TelemetryRecord::add_attribute(current_record, "current.field", StringValue("current_value"))
  TelemetryRecord::add_attribute(current_record, "numeric.field", IntValue(42))
  
  // 测试向后兼容性（读取旧版本数据）
  let v1_data = "{\"version\":\"1.0\",\"traceId\":\"old_trace\",\"spanId\":\"old_span\",\"operationName\":\"old_operation\",\"legacyField\":\"legacy_value\"}"
  let v1_deserialization_result = compatibility_tester.deserialize_with_compatibility(v1_data, "1.0")
  
  match v1_deserialization_result {
    Success(record) => {
      assert_eq(TelemetryRecord::trace_id(record), "old_trace")
      assert_eq(TelemetryRecord::span_id(record), "old_span")
      assert_eq(TelemetryRecord::operation_name(record), "old_operation")
      
      // 验证旧字段被正确处理
      let legacy_field = TelemetryRecord::get_attribute(record, "legacyField")
      match legacy_field {
        Some(StringValue(value)) => assert_eq(value, "legacy_value")
        _ => assert_true(false)
      }
    }
    Error(_) => assert_true(false)
  }
  
  // 测试向前兼容性（新版本数据在旧版本中读取）
  let current_serialized = compatibility_tester.serialize_current_version(current_record)
  let forward_compatibility_result = compatibility_tester.test_forward_compatibility(current_serialized, "1.0")
  
  assert_true(forward_compatibility_result.is_compatible)
  assert_true(forward_compatibility_result.missing_fields.length() > 0) // 应该有新字段缺失
  assert_true(forward_compatibility_result.core_data_preserved) // 核心数据应该保留
  
  // 测试字段演化
  let evolved_record = TelemetryRecord::new()
  TelemetryRecord::set_trace_id(evolved_record, "evolved_trace")
  TelemetryRecord::set_span_id(evolved_record, "evolved_span")
  TelemetryRecord::set_operation_name(evolved_record, "evolved_operation")
  
  // 添加新字段
  TelemetryRecord::add_attribute(evolved_record, "new.field", StringValue("new_value"))
  TelemetryRecord::add_attribute(evolved_record, "another.new.field", FloatValue(3.14))
  
  // 保留旧字段
  TelemetryRecord::add_attribute(evolved_record, "current.field", StringValue("updated_value"))
  
  let evolved_serialized = compatibility_tester.serialize_current_version(evolved_record)
  let evolution_result = compatibility_tester.test_field_evolution(evolved_serialized, "1.0", "2.0")
  
  assert_true(evolution_result.is_compatible)
  assert_true(evolution_result.new_fields_recognized.length() >= 2)
  assert_true(evolution_result.preserved_fields.length() >= 1)
  
  // 测试类型兼容性
  let type_compatibility_data = [
    ("string_to_int", StringValue("123"), IntValue(123)),
    ("int_to_string", IntValue(456), StringValue("456")),
    ("float_to_double", FloatValue(3.14), DoubleValue(3.14)),
    ("bool_to_int", BoolValue(true), IntValue(1))
  ]
  
  for (test_name, old_value, new_value) in type_compatibility_data {
    let type_compatibility_result = compatibility_tester.test_type_compatibility(old_value, new_value)
    assert_true(type_compatibility_result.is_compatible, test_name + " 类型兼容性测试失败")
  }
}

test "序列化错误处理和恢复" {
  let error_handling_serializer = ErrorHandlingSerializer::new()
  let error_recovery_tester = SerializationErrorRecoveryTester::new()
  
  // 测试各种错误情况
  let error_scenarios = [
    // 损坏的JSON数据
    ("corrupted_json", "{\"traceId\":\"test\",\"incomplete\":"),
    // 无效的数据类型
    ("invalid_type", "{\"traceId\":12345,\"spanId\":\"valid\"}"),
    // 缺少必需字段
    ("missing_required", "{\"spanId\":\"valid_span\"}"),
    // 过大的数据
    ("oversized_data", "{\"traceId\":\"" + "x".repeat(1000000) + "\"}"),
    // 无效的Unicode字符
    ("invalid_unicode", "{\"traceId\":\"test\\uFFFF\"}"),
    // 循环引用（如果适用）
    ("circular_reference", "{\"traceId\":\"test\",\"self\":{}}")
  ]
  
  for (scenario_name, invalid_data) in error_scenarios {
    let error_result = ErrorHandlingSerializer::deserialize_with_recovery(
      error_handling_serializer,
      invalid_data
    )
    
    match error_result {
      Success(recovered_record) => {
        // 如果成功恢复，验证数据的基本完整性
        assert_true(TelemetryRecord::trace_id(recovered_record).length() > 0 || 
                   TelemetryRecord::span_id(recovered_record).length() > 0)
        
        // 验证恢复策略
        let recovery_info = ErrorHandlingSerializer::get_recovery_info(error_handling_serializer)
        assert_true(recovery_info.strategy_used.length() > 0)
        assert_true(recovery_info.original_errors.length() > 0)
      }
      Error(error) => {
        // 如果无法恢复，验证错误信息
        assert_true(error.error_type.length() > 0)
        assert_true(error.description.length() > 0)
        assert_true(error.recovery_attempts.length() > 0)
        
        // 验证错误分类
        match scenario_name {
          "corrupted_json" => assert_eq(error.error_type, "json_parse_error"),
          "invalid_type" => assert_eq(error.error_type, "type_mismatch_error"),
          "missing_required" => assert_eq(error.error_type, "missing_field_error"),
          "oversized_data" => assert_eq(error.error_type, "size_limit_error"),
          "invalid_unicode" => assert_eq(error.error_type, "encoding_error"),
          _ => assert_true(true)
        }
      }
    }
  }
  
  // 测试部分数据恢复
  let partial_data = "{\"traceId\":\"partial_trace\",\"invalidField\":12345,\"validField\":\"valid_value\"}"
  let partial_recovery_result = error_recovery_tester.test_partial_recovery(partial_data)
  
  assert_true(partial_recovery_result.partial_recovery_successful)
  assert_true(partial_recovery_result.recovered_fields.length() > 0)
  assert_true(partial_recovery_result.failed_fields.length() > 0)
  assert_true(partial_recovery_result.core_data_intact)
  
  // 测试数据清理和修复
  let dirty_data = "{\"traceId\":\"  dirty_trace  \",\"spanId\":\"null\",\"operationName\":\"\"}"
  let cleaning_result = error_recovery_tester.test_data_cleaning(dirty_data)
  
  assert_true(cleaning_result.cleaning_successful)
  assert_true(cleaning_result.cleaned_data.contains("cleaned_trace")) // 应该清理空格
  assert_false(cleaning_result.cleaned_data.contains("\"spanId\":\"null\"")) // 应该移除null值
  assert_true(cleaning_result.cleaned_data.contains("\"operationName\":\"default_operation\"")) // 应该提供默认值
}

test "序列化安全性测试" {
  let security_serializer = SecureSerializer::new()
  let security_tester = SerializationSecurityTester::new()
  
  // 测试恶意数据注入
  let malicious_inputs = [
    // XSS攻击
    "{\"traceId\":\"<script>alert('xss')</script>\"}",
    // SQL注入
    "{\"operationName\":\"'; DROP TABLE users; --\"}",
    // 路径遍历
    "{\"service.name\":\"../../../etc/passwd\"}",
    // 命令注入
    "{\"command\":\"$(rm -rf /)\"}",
    // 大量数据攻击
    "{\"largeField\":\"" + "A".repeat(10000000) + "\"}",
    // 深度嵌套攻击
    "{\"deep\":" + "{\"nested\":1}".repeat(1000) + "}",
    // Unicode攻击
    "{\"traceId\":\"\\u0000\\u0001\\u0002\"}"
  ]
  
  for malicious_input in malicious_inputs {
    let security_result = SecureSerializer::secure_deserialize(security_serializer, malicious_input)
    
    match security_result {
      Success(sanitized_record) => {
        // 如果成功，验证数据已被清理
        let trace_id = TelemetryRecord::trace_id(sanitized_record)
        assert_false(trace_id.contains("<script>"))
        assert_false(trace_id.contains("DROP TABLE"))
        assert_false(trace_id.contains("../"))
        assert_false(trace_id.contains("$("))
        assert_false(trace_id.contains("\\u0000"))
        
        // 验证长度限制
        assert_true(trace_id.length() < 1000)
      }
      Error(security_error) => {
        // 如果被拒绝，验证是安全原因
        assert_eq(security_error.error_type, "security_violation")
        assert_true(security_error.security_reason.length() > 0)
      }
    }
  }
  
  // 测试加密序列化
  let sensitive_record = TelemetryRecord::new()
  TelemetryRecord::set_trace_id(sensitive_record, "sensitive_trace")
  TelemetryRecord::add_attribute(sensitive_record, "user.email", StringValue("user@example.com"))
  TelemetryRecord::add_attribute(sensitive_record, "credit.card", StringValue("4111-1111-1111-1111"))
  TelemetryRecord::add_attribute(sensitive_record, "api.key", StringValue("sk-1234567890abcdef"))
  
  let encryption_result = SecureSerializer::encrypt_serialize(security_serializer, sensitive_record)
  match encryption_result {
    Success(encrypted_data) => {
      assert_true(encrypted_data.length() > 0)
      assert_false(encrypted_data.contains("user@example.com"))
      assert_false(encrypted_data.contains("4111-1111-1111-1111"))
      assert_false(encrypted_data.contains("sk-1234567890abcdef"))
      
      // 测试解密
      let decryption_result = SecureSerializer::decrypt_deserialize(security_serializer, encrypted_data)
      match decryption_result {
        Success(decrypted_record) => {
          assert_eq(TelemetryRecord::trace_id(decrypted_record), "sensitive_trace")
          
          let email = TelemetryRecord::get_attribute(decrypted_record, "user.email")
          match email {
            Some(StringValue(e)) => assert_eq(e, "user@example.com")
            _ => assert_true(false)
          }
        }
        Error(_) => assert_true(false)
      }
    }
    Error(_) => assert_true(false)
  }
  
  // 测试签名验证
  let signing_result = SecureSerializer::sign_serialize(security_serializer, sensitive_record)
  match signing_result {
    Success(signed_data) => {
      // 验证签名
      let verification_result = SecureSerializer::verify_and_deserialize(security_serializer, signed_data)
      match verification_result {
        Success(verified_record) => {
          assert_eq(TelemetryRecord::trace_id(verified_record), "sensitive_trace")
        }
        Error(_) => assert_true(false)
      }
      
      // 测试篡改检测
      let tampered_data = signed_data.replace("sensitive_trace", "tampered_trace")
      let tamper_detection_result = SecureSerializer::verify_and_deserialize(security_serializer, tampered_data)
      match tamper_detection_result {
        Success(_) => assert_true(false, "应该检测到篡改")
        Error(tamper_error) => assert_eq(tamper_error.error_type, "signature_verification_failed")
      }
    }
    Error(_) => assert_true(false)
  }
}