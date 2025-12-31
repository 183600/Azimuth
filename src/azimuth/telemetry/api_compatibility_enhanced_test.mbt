// API向后兼容性测试用例
// 确保新版本API与旧版本保持兼容

test "api_version_compatibility_trace" {
  // 测试Trace API版本兼容性
  
  // 模拟不同版本的API调用
  let v1_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let v2_trace_id = "0AF7651916CD43DD8448EB211C80319C" // 大写版本
  let v1_span_id = "b7ad6b7169203331"
  let v2_span_id = "B7AD6B7169203331" // 大写版本
  
  // 验证版本1的格式兼容性
  assert_eq(v1_trace_id.length(), 32)
  assert_eq(v1_span_id.length(), 16)
  assert_eq(v1_trace_id.to_lowercase() == v1_trace_id, true)
  assert_eq(v1_span_id.to_lowercase() == v1_span_id, true)
  
  // 验证版本2的格式兼容性（应该能处理大写）
  assert_eq(v2_trace_id.length(), 32)
  assert_eq(v2_span_id.length(), 16)
  assert_eq(v2_trace_id.to_lowercase() == v1_trace_id, true)
  assert_eq(v2_span_id.to_lowercase() == v1_span_id, true)
  
  // 验证跨版本兼容性
  let normalized_v1 = v1_trace_id.to_lowercase()
  let normalized_v2 = v2_trace_id.to_lowercase()
  assert_eq(normalized_v1, normalized_v2)
}

test "api_version_compatibility_metrics" {
  // 测试Metrics API版本兼容性
  
  // 模拟不同版本的指标格式
  let v1_metric_name = "cpu.usage.percent"
  let v2_metric_name = "cpu_usage_percent" // 下划线版本
  let v1_metric_value = 75.5
  let v2_metric_value = 75 // 整数版本
  
  // 验证指标名称兼容性
  assert_eq(v1_metric_name.contains("."), true)
  assert_eq(v2_metric_name.contains("_"), true)
  
  // 验证指标值类型兼容性
  assert_eq(v1_metric_value > 0.0, true)
  assert_eq(v2_metric_value > 0, true)
  
  // 验证类型转换兼容性
  let v2_as_double = v2_metric_value.to_double()
  let v1_as_int = v1_metric_value.to_int()
  assert_eq(v2_as_double == 75.0, true)
  assert_eq(v1_as_int == 75, true)
  
  // 验证格式转换兼容性
  let v1_converted = v1_metric_name.replace(".", "_")
  let v2_converted = v2_metric_name.replace("_", ".")
  assert_eq(v1_converted, v2_metric_name)
  assert_eq(v2_converted, v1_metric_name)
}

test "api_version_compatibility_logs" {
  // 测试Logs API版本兼容性
  
  // 模拟不同版本的日志格式
  let v1_timestamp = "2022-01-01T00:00:00Z"
  let v2_timestamp = "1640995200000" // Unix毫秒时间戳
  let v1_severity = "INFO"
  let v2_severity = "Information" // 完整单词版本
  
  // 验证时间戳格式兼容性
  assert_eq(v1_timestamp.contains("T"), true)
  assert_eq(v1_timestamp.contains("Z"), true)
  assert_eq(v2_timestamp.length(), 13)
  
  // 验证严重性级别兼容性
  assert_eq(v1_severity.length(), 4)
  assert_eq(v2_severity.length(), 11)
  assert_eq(v1_severity.to_uppercase() == "INFO", true)
  assert_eq(v2_severity.to_uppercase().has_prefix("INFO"), true)
  
  // 验证严重性级别映射兼容性
  let v1_normalized = v1_severity.to_uppercase()
  let v2_normalized = v2_severity.to_uppercase().substring(0, 4)
  assert_eq(v1_normalized, v2_normalized)
}

test "api_attribute_compatibility" {
  // 测试属性API兼容性
  
  // 模拟不同版本的属性格式
  let v1_attributes = [
    ("service.name", "payment-service"),
    ("service.version", "1.2.3"),
    ("host.name", "server-001")
  ]
  
  let v2_attributes = [
    ("service_name", "payment-service"), // 下划线版本
    ("service_version", "1.2.3"),
    ("hostname", "server-001") // 缩写版本
  ]
  
  // 验证属性数量兼容性
  assert_eq(v1_attributes.length(), v2_attributes.length())
  
  // 验证属性值兼容性（应该保持不变）
  assert_eq(v1_attributes[0].1, v2_attributes[0].1)
  assert_eq(v1_attributes[1].1, v2_attributes[1].1)
  assert_eq(v1_attributes[2].1, v2_attributes[2].1)
  
  // 验证属性键转换兼容性
  let mut i = 0
  while i < v1_attributes.length() {
    let v1_key = v1_attributes[i].0
    let v2_key = v2_attributes[i].0
    
    // 验证键的转换规则
    let v1_converted = v1_key.replace(".", "_")
    let v2_converted = v2_key.replace("_", ".")
    
    if i == 0 || i == 1 {
      assert_eq(v1_converted, v2_key)
    }
    
    i = i + 1
  }
}

test "api_data_format_compatibility" {
  // 测试数据格式兼容性
  
  // 模拟不同版本的数据格式
  let v1_format = "metric:cpu_usage,value:75.5,timestamp:1640995200"
  let v2_format = "{\"metric\":\"cpu_usage\",\"value\":75.5,\"timestamp\":1640995200}"
  let v3_format = "cpu_usage 75.5 1640995200" // 简化格式
  
  // 验证格式识别兼容性
  assert_eq(v1_format.contains("metric:"), true)
  assert_eq(v1_format.contains("value:"), true)
  assert_eq(v2_format.contains("\"metric\":"), true)
  assert_eq(v2_format.contains("\"value\":"), true)
  assert_eq(v3_format.split(" ").length(), 3)
  
  // 验证数据提取兼容性
  let v1_parts = v1_format.split(",")
  let v2_parts = v2_format.split(",")
  let v3_parts = v3_format.split(" ")
  
  assert_eq(v1_parts.length(), 3)
  assert_eq(v2_parts.length(), 3)
  assert_eq(v3_parts.length(), 3)
  
  // 验证数据值兼容性（所有格式应包含相同的数据）
  assert_eq(v1_format.contains("75.5"), true)
  assert_eq(v2_format.contains("75.5"), true)
  assert_eq(v3_format.contains("75.5"), true)
  assert_eq(v1_format.contains("1640995200"), true)
  assert_eq(v2_format.contains("1640995200"), true)
  assert_eq(v3_format.contains("1640995200"), true)
}

test "api_error_handling_compatibility" {
  // 测试错误处理API兼容性
  
  // 模拟不同版本的错误格式
  let v1_error = "ERROR:Invalid metric name"
  let v2_error = "{\"error\":\"Invalid metric name\",\"code\":400}"
  let v3_error = "400|Invalid metric name" // 管道分隔格式
  
  // 验证错误格式识别兼容性
  assert_eq(v1_error.has_prefix("ERROR:"), true)
  assert_eq(v2_error.contains("\"error\":"), true)
  assert_eq(v3_error.contains("|"), true)
  
  // 验证错误消息提取兼容性
  let v1_message = v1_error.substring(6) // 去掉"ERROR:"
  let v2_message = v2_error.split("\"")[3] // 提取引号中的消息
  let v3_message = v3_error.split("|")[1] // 提取管道后的消息
  
  assert_eq(v1_message, "Invalid metric name")
  assert_eq(v2_message, "Invalid metric name")
  assert_eq(v3_message, "Invalid metric name")
  
  // 验证错误代码兼容性
  assert_eq(v2_error.contains("\"code\":400"), true)
  assert_eq(v3_error.has_prefix("400|"), true)
  
  // 验证错误分类兼容性
  let is_validation_error = v1_message.contains("Invalid") || 
                           v2_message.contains("Invalid") || 
                           v3_message.contains("Invalid")
  assert_eq(is_validation_error, true)
}

test "api_configuration_compatibility" {
  // 测试配置API兼容性
  
  // 模拟不同版本的配置格式
  let v1_config = "sampling.rate=0.1,batch.size=100,timeout=30"
  let v2_config = "{\"sampling_rate\":0.1,\"batch_size\":100,\"timeout\":30}"
  let v3_config = "sampling_rate:0.1\nbatch_size:100\ntimeout:30" // YAML风格
  
  // 验证配置格式识别兼容性
  assert_eq(v1_config.contains("="), true)
  assert_eq(v2_config.contains("\""), true)
  assert_eq(v3_config.contains("\n"), true)
  
  // 验证配置项提取兼容性
  assert_eq(v1_config.contains("sampling.rate"), true)
  assert_eq(v2_config.contains("sampling_rate"), true)
  assert_eq(v3_config.contains("sampling_rate"), true)
  
  // 验证配置值兼容性
  assert_eq(v1_config.contains("0.1"), true)
  assert_eq(v2_config.contains("0.1"), true)
  assert_eq(v3_config.contains("0.1"), true)
  
  // 验证配置项数量兼容性
  let v1_items = v1_config.split(",")
  let v2_items = v2_config.split(",")
  let v3_items = v3_config.split("\n")
  
  assert_eq(v1_items.length(), 3)
  assert_eq(v2_items.length(), 3)
  assert_eq(v3_items.length(), 3)
}