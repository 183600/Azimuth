// 遥测数据转换测试用例

test "telemetry_data_type_conversion" {
  // 测试遥测数据类型转换
  
  let string_value = "123.45"
  let int_value = 42
  let bool_value = true
  let double_value = 67.89
  
  // 字符串转数值
  let parsed_double = string_value.to_double()
  assert_eq(parsed_double, 123.45)
  assert_eq(parsed_double > 100.0, true)
  assert_eq(parsed_double < 200.0, true)
  
  // 数值转字符串
  let int_string = int_value.to_string()
  let double_string = double_value.to_string()
  
  assert_eq(int_string, "42")
  assert_eq(double_string, "67.89")
  
  // 布尔值转字符串
  let bool_string = if bool_value { "true" } else { "false" }
  assert_eq(bool_string, "true")
  
  // 创建类型转换记录
  let conversion_record = "string->double:" + string_value + "->" + parsed_double.to_string() +
                         ", int->string:" + int_value.to_string() + "->" + int_string +
                         ", bool->string:" + bool_value.to_string() + "->" + bool_string
  
  assert_eq(conversion_record.contains("string->double:123.45->123.45"), true)
  assert_eq(conversion_record.contains("int->string:42->42"), true)
  assert_eq(conversion_record.contains("bool->string:true->true"), true)
}

test "telemetry_unit_conversion" {
  // 测试遥测单位转换
  
  let bytes_value = 1048576L // 1MB in bytes
  let milliseconds_value = 5000L // 5 seconds in milliseconds
  let bytes_per_second = 1048576L // 1MB/s
  
  // 字节转MB
  let mb_value = bytes_value.to_double() / 1024.0 / 1024.0
  assert_eq(mb_value, 1.0)
  
  // 毫秒转秒
  let seconds_value = milliseconds_value.to_double() / 1000.0
  assert_eq(seconds_value, 5.0)
  
  // 计算传输时间
  let transfer_time = bytes_value.to_double() / bytes_per_second.to_double()
  assert_eq(transfer_time, 1.0) // 1秒
  
  // 创建单位转换报告
  let conversion_report = "bytes->MB:" + bytes_value.to_string() + "->" + mb_value.to_string() + "MB" +
                         ", ms->s:" + milliseconds_value.to_string() + "->" + seconds_value.to_string() + "s" +
                         ", transfer_time:" + transfer_time.to_string() + "s"
  
  assert_eq(conversion_report.contains("bytes->MB:1048576->1MB"), true)
  assert_eq(conversion_report.contains("ms->s:5000->5s"), true)
  assert_eq(conversion_report.contains("transfer_time:1s"), true)
}

test "telemetry_format_conversion" {
  // 测试遥测格式转换
  
  let metric_name = "cpu_usage"
  let metric_value = 75.5
  let metric_timestamp = 1640995200L
  let metric_tags = ["service:api", "env:production", "region:us-west"]
  
  // 转换为JSON格式
  let json_format = "{"
  json_format = json_format + "\"metric\":\"" + metric_name + "\","
  json_format = json_format + "\"value\":" + metric_value.to_string() + ","
  json_format = json_format + "\"timestamp\":" + metric_timestamp.to_string() + ","
  json_format = json_format + "\"tags\":[" 
  
  let mut i = 0
  while i < metric_tags.length() {
    json_format = json_format + "\"" + metric_tags[i] + "\""
    if i < metric_tags.length() - 1 {
      json_format = json_format + ","
    }
    i = i + 1
  }
  
  json_format = json_format + "]}"
  
  // 验证JSON格式
  assert_eq(json_format.has_prefix("{"), true)
  assert_eq(json_format.has_suffix("}"), true)
  assert_eq(json_format.contains("\"metric\":\"cpu_usage\""), true)
  assert_eq(json_format.contains("\"value\":75.5"), true)
  
  // 转换为Prometheus格式
  let prometheus_tags = metric_tags.join(",")
  let prometheus_format = metric_name + "{" + prometheus_tags + "} " + metric_value.to_string() + " " + metric_timestamp.to_string()
  
  // 验证Prometheus格式
  assert_eq(prometheus_format.has_prefix("cpu_usage{"), true)
  assert_eq(prometheus_format.contains("service:api"), true)
  assert_eq(prometheus_format.contains("env:production"), true)
  assert_eq(prometheus_format.contains("region:us-west"), true)
  assert_eq(prometheus_format.has_suffix(" 1640995200"), true)
  
  // 转换为InfluxDB格式
  let influxdb_tags = metric_tags.join(",")
  let influxdb_format = metric_name + "," + influxdb_tags + " value=" + metric_value.to_string() + " " + metric_timestamp.to_string()
  
  // 验证InfluxDB格式
  assert_eq(influxdb_format.has_prefix("cpu_usage,"), true)
  assert_eq(influxdb_format.contains("value=75.5"), true)
  assert_eq(influxdb_format.has_suffix(" 1640995200"), true)
}