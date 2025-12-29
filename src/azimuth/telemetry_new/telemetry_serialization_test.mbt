// 遥测序列化测试用例

test "telemetry_json_serialization" {
  // 测试遥测JSON序列化
  
  let telemetry_data = {
    "trace_id": "0af7651916cd43dd8448eb211c80319c",
    "span_id": "b7ad6b7169203331",
    "parent_span_id": "a9bc5a6058d13f20",
    "operation_name": "HTTP GET /api/users",
    "start_time": 1672531200000000,
    "end_time": 1672531200500000,
    "duration_ms": 500,
    "status": "ok",
    "attributes": {
      "http.method": "GET",
      "http.url": "https://api.example.com/users",
      "http.status_code": "200",
      "user.id": "12345",
      "service.name": "user-service"
    },
    "events": [
      {
        "name": "database.query.start",
        "timestamp": 1672531200100000,
        "attributes": {
          "db.statement": "SELECT * FROM users WHERE id = ?"
        }
      },
      {
        "name": "database.query.end",
        "timestamp": 1672531200400000,
        "attributes": {
          "db.rows_affected": "1"
        }
      }
    ],
    "links": [
      {
        "trace_id": "c8be7c827a31d4429558fc322d91420d",
        "span_id": "d9cf8d838b42e553",
        "attributes": {
          "link.type": "causality"
        }
      }
    ]
  }
  
  // 验证遥测数据结构
  assert_eq(telemetry_data.contains("trace_id"), true)
  assert_eq(telemetry_data.contains("span_id"), true)
  assert_eq(telemetry_data.contains("operation_name"), true)
  assert_eq(telemetry_data.contains("start_time"), true)
  assert_eq(telemetry_data.contains("end_time"), true)
  assert_eq(telemetry_data.contains("duration_ms"), true)
  assert_eq(telemetry_data.contains("status"), true)
  assert_eq(telemetry_data.contains("attributes"), true)
  assert_eq(telemetry_data.contains("events"), true)
  assert_eq(telemetry_data.contains("links"), true)
  
  // 验证基本字段
  let trace_id = telemetry_data.get("trace_id", "")
  let span_id = telemetry_data.get("span_id", "")
  let operation_name = telemetry_data.get("operation_name", "")
  let duration_ms = telemetry_data.get("duration_ms", 0)
  let status = telemetry_data.get("status", "")
  
  assert_eq(trace_id.length(), 32)
  assert_eq(span_id.length(), 16)
  assert_eq(operation_name.length() > 0, true)
  assert_eq(duration_ms > 0, true)
  assert_eq(status == "ok" || status == "error", true)
  
  // 验证属性
  let attributes = telemetry_data.get("attributes", {})
  assert_eq(attributes.contains("http.method"), true)
  assert_eq(attributes.contains("http.url"), true)
  assert_eq(attributes.contains("http.status_code"), true)
  assert_eq(attributes.contains("user.id"), true)
  assert_eq(attributes.contains("service.name"), true)
  
  // 验证事件
  let events = telemetry_data.get("events", [])
  assert_eq(events.length(), 2)
  
  let mut i = 0
  while i < events.length() {
    let event = events[i]
    assert_eq(event.contains("name"), true)
    assert_eq(event.contains("timestamp"), true)
    assert_eq(event.contains("attributes"), true)
    i = i + 1
  }
  
  // 验证链接
  let links = telemetry_data.get("links", [])
  assert_eq(links.length(), 1)
  
  let link = links[0]
  assert_eq(link.contains("trace_id"), true)
  assert_eq(link.contains("span_id"), true)
  assert_eq(link.contains("attributes"), true)
  
  // 模拟JSON序列化
  let json_string = "{"
    + "\"trace_id\":\"" + trace_id + "\","
    + "\"span_id\":\"" + span_id + "\","
    + "\"operation_name\":\"" + operation_name + "\","
    + "\"duration_ms\":" + duration_ms.to_string() + ","
    + "\"status\":\"" + status + "\""
    + "}"
  
  // 验证JSON字符串格式
  assert_eq(json_string.has_prefix("{"), true)
  assert_eq(json_string.has_suffix("}"), true)
  assert_eq(json_string.contains("\"trace_id\":"), true)
  assert_eq(json_string.contains("\"span_id\":"), true)
  assert_eq(json_string.contains("\"operation_name\":"), true)
  assert_eq(json_string.contains("\"duration_ms\":"), true)
  assert_eq(json_string.contains("\"status\":"), true)
  
  // 模拟JSON反序列化（简化）
  let parsed_trace_id = if json_string.contains("\"trace_id\":\"") {
    let start = json_string.index_of("\"trace_id\":\"") + 12
    let end = json_string.index_of("\"", start)
    json_string.substring(start, end)
  } else {
    ""
  }
  
  assert_eq(parsed_trace_id, trace_id)
}

test "telemetry_protobuf_serialization" {
  // 测试遥测Protobuf序列化
  
  let span_data = {
    "trace_id": "0af7651916cd43dd8448eb211c80319c",
    "span_id": "b7ad6b7169203331",
    "parent_span_id": "a9bc5a6058d13f20",
    "name": "HTTP GET /api/users",
    "kind": "SPAN_KIND_SERVER",
    "start_time_unix_nano": 1672531200000000000,
    "end_time_unix_nano": 1672531200500000000,
    "status": {
      "code": "STATUS_CODE_OK",
      "message": "Success"
    },
    "attributes": [
      {"key": "http.method", "value": {"string_value": "GET"}},
      {"key": "http.url", "value": {"string_value": "https://api.example.com/users"}},
      {"key": "http.status_code", "value": {"int_value": 200}},
      {"key": "user.id", "value": {"string_value": "12345"}}
    ],
    "events": [
      {
        "time_unix_nano": 1672531200100000000,
        "name": "database.query.start",
        "attributes": [
          {"key": "db.statement", "value": {"string_value": "SELECT * FROM users WHERE id = ?"}}
        ]
      }
    ]
  }
  
  // 验证Protobuf数据结构
  assert_eq(span_data.contains("trace_id"), true)
  assert_eq(span_data.contains("span_id"), true)
  assert_eq(span_data.contains("name"), true)
  assert_eq(span_data.contains("kind"), true)
  assert_eq(span_data.contains("start_time_unix_nano"), true)
  assert_eq(span_data.contains("end_time_unix_nano"), true)
  assert_eq(span_data.contains("status"), true)
  assert_eq(span_data.contains("attributes"), true)
  assert_eq(span_data.contains("events"), true)
  
  // 验证基本字段
  let trace_id = span_data.get("trace_id", "")
  let span_id = span_data.get("span_id", "")
  let name = span_data.get("name", "")
  let kind = span_data.get("kind", "")
  let start_time = span_data.get("start_time_unix_nano", 0)
  let end_time = span_data.get("end_time_unix_nano", 0)
  
  assert_eq(trace_id.length(), 32)
  assert_eq(span_id.length(), 16)
  assert_eq(name.length() > 0, true)
  assert_eq(kind == "SPAN_KIND_SERVER" || kind == "SPAN_KIND_CLIENT", true)
  assert_eq(start_time > 0, true)
  assert_eq(end_time > start_time, true)
  
  // 验证状态
  let status = span_data.get("status", {})
  assert_eq(status.contains("code"), true)
  assert_eq(status.contains("message"), true)
  assert_eq(status.get("code", ""), "STATUS_CODE_OK")
  assert_eq(status.get("message", ""), "Success")
  
  // 验证属性
  let attributes = span_data.get("attributes", [])
  assert_eq(attributes.length(), 4)
  
  let mut i = 0
  while i < attributes.length() {
    let attr = attributes[i]
    assert_eq(attr.contains("key"), true)
    assert_eq(attr.contains("value"), true)
    
    let key = attr.get("key", "")
    let value = attr.get("value", {})
    
    assert_eq(key.length() > 0, true)
    assert_eq(value.contains("string_value") || value.contains("int_value"), true)
    
    i = i + 1
  }
  
  // 模拟Protobuf二进制序列化（简化）
  let mut binary_data = []
  
  // 添加trace_id (field 1, string)
  binary_data.push_all(trace_id.to_bytes())
  
  // 添加span_id (field 2, string)
  binary_data.push_all(span_id.to_bytes())
  
  // 添加name (field 3, string)
  binary_data.push_all(name.to_bytes())
  
  // 验证二进制数据
  assert_eq(binary_data.length() > 0, true)
  
  // 计算序列化大小
  let serialized_size = binary_data.length()
  assert_eq(serialized_size > 100, true)  // 应该大于100字节
  assert_eq(serialized_size < 10000, true)  // 应该小于10KB
}

test "telemetry_metrics_serialization" {
  // 测试遥测指标序列化
  
  let metrics_data = [
    {
      "name": "http_requests_total",
      "description": "Total number of HTTP requests",
      "unit": "requests",
      "type": "counter",
      "data_points": [
        {
          "attributes": {
            "method": "GET",
            "status": "200",
            "service": "user-service"
          },
          "value": 1250,
          "time_unix_nano": 1672531200000000000
        },
        {
          "attributes": {
            "method": "POST",
            "status": "201",
            "service": "user-service"
          },
          "value": 320,
          "time_unix_nano": 1672531200000000000
        }
      ]
    },
    {
      "name": "http_request_duration_seconds",
      "description": "HTTP request duration in seconds",
      "unit": "seconds",
      "type": "histogram",
      "data_points": [
        {
          "attributes": {
            "method": "GET",
            "service": "user-service"
          },
          "count": 1000,
          "sum": 125.5,
          "explicit_bounds": [0.1, 0.5, 1.0, 2.5, 5.0, 10.0],
          "bucket_counts": [50, 200, 300, 250, 150, 40, 10]
        }
      ]
    }
  ]
  
  // 验证指标数据结构
  let mut i = 0
  while i < metrics_data.length() {
    let metric = metrics_data[i]
    
    assert_eq(metric.contains("name"), true)
    assert_eq(metric.contains("description"), true)
    assert_eq(metric.contains("unit"), true)
    assert_eq(metric.contains("type"), true)
    assert_eq(metric.contains("data_points"), true)
    
    let name = metric.get("name", "")
    let description = metric.get("description", "")
    let unit = metric.get("unit", "")
    let type_val = metric.get("type", "")
    let data_points = metric.get("data_points", [])
    
    assert_eq(name.length() > 0, true)
    assert_eq(description.length() > 0, true)
    assert_eq(unit.length() > 0, true)
    assert_eq(type_val == "counter" || type_val == "gauge" || type_val == "histogram" || type_val == "summary", true)
    assert_eq(data_points.length() > 0, true)
    
    // 验证数据点
    let mut j = 0
    while j < data_points.length() {
      let data_point = data_points[j]
      assert_eq(data_point.contains("attributes"), true)
      assert_eq(data_point.contains("time_unix_nano"), true)
      
      let attributes = data_point.get("attributes", {})
      assert_eq(attributes.length() > 0, true)
      
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 验证计数器指标
  let counter_metric = metrics_data[0]
  assert_eq(counter_metric.get("name", ""), "http_requests_total")
  assert_eq(counter_metric.get("type", ""), "counter")
  
  let counter_data_points = counter_metric.get("data_points", [])
  let counter_point = counter_data_points[0]
  assert_eq(counter_point.contains("value"), true)
  assert_eq(counter_point.get("value", 0) > 0, true)
  
  // 验证直方图指标
  let histogram_metric = metrics_data[1]
  assert_eq(histogram_metric.get("name", ""), "http_request_duration_seconds")
  assert_eq(histogram_metric.get("type", ""), "histogram")
  
  let histogram_data_points = histogram_metric.get("data_points", [])
  let histogram_point = histogram_data_points[0]
  assert_eq(histogram_point.contains("count"), true)
  assert_eq(histogram_point.contains("sum"), true)
  assert_eq(histogram_point.contains("explicit_bounds"), true)
  assert_eq(histogram_point.contains("bucket_counts"), true)
  
  let explicit_bounds = histogram_point.get("explicit_bounds", [])
  let bucket_counts = histogram_point.get("bucket_counts", [])
  assert_eq(explicit_bounds.length() + 1, bucket_counts.length())
  
  // 模拟指标序列化为JSON
  let metrics_json = "[" + 
    "{\"name\":\"" + counter_metric.get("name", "") + "\",\"type\":\"" + counter_metric.get("type", "") + "\"}," +
    "{\"name\":\"" + histogram_metric.get("name", "") + "\",\"type\":\"" + histogram_metric.get("type", "") + "\"}" +
    "]"
  
  // 验证JSON格式
  assert_eq(metrics_json.has_prefix("["), true)
  assert_eq(metrics_json.has_suffix("]"), true)
  assert_eq(metrics_json.contains("http_requests_total"), true)
  assert_eq(metrics_json.contains("http_request_duration_seconds"), true)
  assert_eq(metrics_json.contains("\"type\":\"counter\""), true)
  assert_eq(metrics_json.contains("\"type\":\"histogram\""), true)
}

test "telemetry_logs_serialization" {
  // 测试遥测日志序列化
  
  let log_records = [
    {
      "time_unix_nano": 1672531200000000000,
      "observed_time_unix_nano": 1672531200000000000,
      "severity_number": "SEVERITY_NUMBER_INFO",
      "severity_text": "INFO",
      "body": {
        "string_value": "User login successful"
      },
      "attributes": {
        "user.id": "12345",
        "user.name": "john.doe",
        "ip.address": "192.168.1.100",
        "service.name": "auth-service"
      },
      "trace_id": "0af7651916cd43dd8448eb211c80319c",
      "span_id": "b7ad6b7169203331",
      "flags": 1
    },
    {
      "time_unix_nano": 1672531200500000000,
      "observed_time_unix_nano": 1672531200500000000,
      "severity_number": "SEVERITY_NUMBER_ERROR",
      "severity_text": "ERROR",
      "body": {
        "string_value": "Database connection failed"
      },
      "attributes": {
        "error.type": "connection_timeout",
        "database.host": "db.example.com",
        "database.port": "5432",
        "service.name": "user-service"
      },
      "trace_id": "c8be7c827a31d4429558fc322d91420d",
      "span_id": "d9cf8d838b42e553",
      "flags": 1
    }
  ]
  
  // 验证日志记录结构
  let mut i = 0
  while i < log_records.length() {
    let log_record = log_records[i]
    
    assert_eq(log_record.contains("time_unix_nano"), true)
    assert_eq(log_record.contains("observed_time_unix_nano"), true)
    assert_eq(log_record.contains("severity_number"), true)
    assert_eq(log_record.contains("severity_text"), true)
    assert_eq(log_record.contains("body"), true)
    assert_eq(log_record.contains("attributes"), true)
    assert_eq(log_record.contains("trace_id"), true)
    assert_eq(log_record.contains("span_id"), true)
    assert_eq(log_record.contains("flags"), true)
    
    // 验证时间戳
    let time = log_record.get("time_unix_nano", 0)
    let observed_time = log_record.get("observed_time_unix_nano", 0)
    assert_eq(time > 0, true)
    assert_eq(observed_time > 0, true)
    
    // 验证严重性级别
    let severity_number = log_record.get("severity_number", "")
    let severity_text = log_record.get("severity_text", "")
    assert_eq(severity_number.length() > 0, true)
    assert_eq(severity_text.length() > 0, true)
    assert_eq(severity_text == "TRACE" || severity_text == "DEBUG" || severity_text == "INFO" || severity_text == "WARN" || severity_text == "ERROR" || severity_text == "FATAL", true)
    
    // 验证消息体
    let body = log_record.get("body", {})
    assert_eq(body.contains("string_value"), true)
    let message = body.get("string_value", "")
    assert_eq(message.length() > 0, true)
    
    // 验证属性
    let attributes = log_record.get("attributes", {})
    assert_eq(attributes.length() > 0, true)
    
    // 验证追踪信息
    let trace_id = log_record.get("trace_id", "")
    let span_id = log_record.get("span_id", "")
    assert_eq(trace_id.length(), 32)
    assert_eq(span_id.length(), 16)
    
    // 验证标志
    let flags = log_record.get("flags", 0)
    assert_eq(flags >= 0, true)
    
    i = i + 1
  }
  
  // 验证不同严重性级别的日志
  let info_log = log_records[0]
  let error_log = log_records[1]
  
  assert_eq(info_log.get("severity_text", ""), "INFO")
  assert_eq(error_log.get("severity_text", ""), "ERROR")
  
  // 验证日志消息内容
  let info_body = info_log.get("body", {})
  let error_body = error_log.get("body", {})
  
  assert_eq(info_body.get("string_value", ""), "User login successful")
  assert_eq(error_body.get("string_value", ""), "Database connection failed")
  
  // 模拟日志序列化为JSON数组
  let mut logs_json = "["
  i = 0
  while i < log_records.length() {
    let log = log_records[i]
    let severity_text = log.get("severity_text", "")
    let body = log.get("body", {})
    let message = body.get("string_value", "")
    
    if i > 0 {
      logs_json = logs_json + ","
    }
    
    logs_json = logs_json + "{\"severity\":\"" + severity_text + "\",\"message\":\"" + message + "\"}"
    
    i = i + 1
  }
  logs_json = logs_json + "]"
  
  // 验证JSON格式
  assert_eq(logs_json.has_prefix("["), true)
  assert_eq(logs_json.has_suffix("]"), true)
  assert_eq(logs_json.contains("\"severity\":\"INFO\""), true)
  assert_eq(logs_json.contains("\"severity\":\"ERROR\""), true)
  assert_eq(logs_json.contains("User login successful"), true)
  assert_eq(logs_json.contains("Database connection failed"), true)
}

test "telemetry_compression_serialization" {
  // 测试遥测压缩序列化
  
  let original_data = "This is a large telemetry data payload that should be compressed to save bandwidth during transmission. "
    + "It contains multiple spans, metrics, and logs that need to be efficiently serialized and sent to the telemetry backend. "
    + "Compression helps reduce network overhead and improves performance in high-throughput scenarios."
  
  let compression_formats = ["gzip", "deflate", "lz4", "snappy"]
  
  // 验证原始数据
  assert_eq(original_data.length() > 100, true)
  
  // 模拟不同压缩格式的压缩
  let mut compression_results = []
  let mut i = 0
  while i < compression_formats.length() {
    let format = compression_formats[i]
    
    // 模拟压缩（简化：基于格式计算压缩比）
    let compression_ratio = if format == "gzip" {
      0.3
    } else if format == "deflate" {
      0.35
    } else if format == "lz4" {
      0.5
    } else if format == "snappy" {
      0.45
    } else {
      1.0
    }
    
    let original_size = original_data.length()
    let compressed_size = (original_size.to_double() * compression_ratio).to_int()
    
    let compression_result = {
      "format": format,
      "original_size": original_size,
      "compressed_size": compressed_size,
      "compression_ratio": compression_ratio,
      "space_saved": original_size - compressed_size
    }
    
    compression_results.push(compression_result)
    i = i + 1
  }
  
  // 验证压缩结果
  assert_eq(compression_results.length(), compression_formats.length())
  
  i = 0
  while i < compression_results.length() {
    let result = compression_results[i]
    
    // 验证压缩结果结构
    assert_eq(result.contains("format"), true)
    assert_eq(result.contains("original_size"), true)
    assert_eq(result.contains("compressed_size"), true)
    assert_eq(result.contains("compression_ratio"), true)
    assert_eq(result.contains("space_saved"), true)
    
    // 验证压缩值
    let format = result.get("format", "")
    let original_size = result.get("original_size", 0)
    let compressed_size = result.get("compressed_size", 0)
    let compression_ratio = result.get("compression_ratio", 0.0)
    let space_saved = result.get("space_saved", 0)
    
    assert_eq(format.length() > 0, true)
    assert_eq(original_size > 0, true)
    assert_eq(compressed_size > 0, true)
    assert_eq(compressed_size <= original_size, true)
    assert_eq(compression_ratio > 0.0, true)
    assert_eq(compression_ratio <= 1.0, true)
    assert_eq(space_saved >= 0, true)
    assert_eq(space_saved, original_size - compressed_size)
    
    i = i + 1
  }
  
  // 找出最佳压缩格式
  let mut best_format = ""
  let mut best_ratio = 1.0
  
  i = 0
  while i < compression_results.length() {
    let result = compression_results[i]
    let ratio = result.get("compression_ratio", 1.0)
    
    if ratio < best_ratio {
      best_ratio = ratio
      best_format = result.get("format", "")
    }
    
    i = i + 1
  }
  
  // 验证最佳压缩格式
  assert_eq(best_format.length() > 0, true)
  assert_eq(best_ratio < 1.0, true)
  assert_eq(best_format == "gzip", true)  // gzip应该有最好的压缩比
  
  // 验证所有格式都节省了空间
  i = 0
  while i < compression_results.length() {
    let result = compression_results[i]
    let space_saved = result.get("space_saved", 0)
    assert_eq(space_saved > 0, true)
    i = i + 1
  }
}

test "telemetry_batch_serialization" {
  // 测试遥测批量序列化
  
  let batch_config = {
    "max_batch_size": 1000,
    "max_batch_bytes": 1048576,  // 1MB
    "compression": "gzip",
    "format": "json"
  }
  
  let telemetry_spans = [
    {
      "trace_id": "0af7651916cd43dd8448eb211c80319c",
      "span_id": "b7ad6b7169203331",
      "operation_name": "HTTP GET /api/users",
      "duration_ms": 500,
      "status": "ok"
    },
    {
      "trace_id": "c8be7c827a31d4429558fc322d91420d",
      "span_id": "d9cf8d838b42e553",
      "operation_name": "Database Query",
      "duration_ms": 200,
      "status": "ok"
    },
    {
      "trace_id": "e0e09e949c53f664b77a1e544fb36422f",
      "span_id": "f1f1a0a5bd64g775",
      "operation_name": "Cache Get",
      "duration_ms": 50,
      "status": "ok"
    }
  ]
  
  // 验证批量配置
  assert_eq(batch_config.contains("max_batch_size"), true)
  assert_eq(batch_config.contains("max_batch_bytes"), true)
  assert_eq(batch_config.contains("compression"), true)
  assert_eq(batch_config.contains("format"), true)
  
  let max_batch_size = batch_config.get("max_batch_size", 0)
  let max_batch_bytes = batch_config.get("max_batch_bytes", 0)
  let compression = batch_config.get("compression", "")
  let format = batch_config.get("format", "")
  
  assert_eq(max_batch_size > 0, true)
  assert_eq(max_batch_bytes > 0, true)
  assert_eq(compression.length() > 0, true)
  assert_eq(format == "json" || format == "protobuf" || format == "avro", true)
  
  // 验证span数据
  let mut i = 0
  while i < telemetry_spans.length() {
    let span = telemetry_spans[i]
    assert_eq(span.contains("trace_id"), true)
    assert_eq(span.contains("span_id"), true)
    assert_eq(span.contains("operation_name"), true)
    assert_eq(span.contains("duration_ms"), true)
    assert_eq(span.contains("status"), true)
    i = i + 1
  }
  
  // 模拟批量序列化
  let mut batch_json = "{\"spans\":["
  i = 0
  while i < telemetry_spans.length() {
    let span = telemetry_spans[i]
    let trace_id = span.get("trace_id", "")
    let span_id = span.get("span_id", "")
    let operation_name = span.get("operation_name", "")
    let duration_ms = span.get("duration_ms", 0)
    let status = span.get("status", "")
    
    if i > 0 {
      batch_json = batch_json + ","
    }
    
    batch_json = batch_json + "{"
      + "\"trace_id\":\"" + trace_id + "\","
      + "\"span_id\":\"" + span_id + "\","
      + "\"operation_name\":\"" + operation_name + "\","
      + "\"duration_ms\":" + duration_ms.to_string() + ","
      + "\"status\":\"" + status + "\""
      + "}"
    
    i = i + 1
  }
  batch_json = batch_json + "]}"
  
  // 验证批量JSON格式
  assert_eq(batch_json.has_prefix("{\"spans\":["), true)
  assert_eq(batch_json.has_suffix("]}"), true)
  assert_eq(batch_json.contains("\"trace_id\""), true)
  assert_eq(batch_json.contains("\"span_id\""), true)
  assert_eq(batch_json.contains("\"operation_name\""), true)
  
  // 计算批量大小
  let batch_size = telemetry_spans.length()
  let batch_bytes = batch_json.length()
  
  assert_eq(batch_size <= max_batch_size, true)
  assert_eq(batch_bytes <= max_batch_bytes, true)
  
  // 模拟压缩后的批量大小
  let compression_ratio = if compression == "gzip" {
    0.3
  } else if compression == "deflate" {
    0.35
  } else if compression == "lz4" {
    0.5
  } else if compression == "snappy" {
    0.45
  } else {
    1.0
  }
  
  let compressed_bytes = (batch_bytes.to_double() * compression_ratio).to_int()
  assert_eq(compressed_bytes < batch_bytes, true)
  assert_eq(compressed_bytes > 0, true)
  
  // 计算压缩效率
  let space_saved = batch_bytes - compressed_bytes
  let compression_efficiency = space_saved.to_double() / batch_bytes.to_double()
  
  assert_eq(space_saved > 0, true)
  assert_eq(compression_efficiency > 0.0, true)
  assert_eq(compression_efficiency < 1.0, true)
}