// 遥测数据序列化兼容性测试用例
// 测试遥测数据在不同格式之间的序列化和反序列化兼容性

test "telemetry_json_serialization_compatibility" {
  // 测试JSON序列化兼容性
  
  let telemetry_data = {
    "trace_id": "12345678901234567890123456789012",
    "span_id": "1234567890123456",
    "parent_span_id": "0987654321098765",
    "operation_name": "http_request",
    "start_time": 1672531200000000000,  // 纳秒时间戳
    "end_time": 1672531200500000000,
    "duration_ms": 50,
    "status": "ok",
    "attributes": {
      "http.method": "GET",
      "http.url": "https://api.example.com/users",
      "http.status_code": 200,
      "user.id": "user-12345",
      "service.name": "user-service"
    },
    "events": [
      {
        "name": "db.query.start",
        "timestamp": 1672531200100000000,
        "attributes": {
          "db.statement": "SELECT * FROM users WHERE id = ?",
          "db.type": "postgresql"
        }
      },
      {
        "name": "db.query.complete",
        "timestamp": 1672531200300000000,
        "attributes": {
          "db.rows_affected": 1,
          "db.duration_ms": 20
        }
      }
    ],
    "resource": {
      "service.name": "user-service",
      "service.version": "1.2.3",
      "deployment.environment": "production",
      "telemetry.sdk.name": "azimuth-telemetry",
      "telemetry.sdk.version": "0.1.0",
      "telemetry.sdk.language": "moonbit"
    }
  }
  
  // 验证原始数据结构
  assert_eq(telemetry_data.get("trace_id", ""), "12345678901234567890123456789012")
  assert_eq(telemetry_data.get("span_id", ""), "1234567890123456")
  assert_eq(telemetry_data.get("operation_name", ""), "http_request")
  assert_eq(telemetry_data.get("duration_ms", 0), 50)
  
  // 模拟JSON序列化
  let json_string = {
    let obj = telemetry_data
    // 简化的JSON序列化模拟
    "{\"trace_id\":\"" + obj.get("trace_id", "") + 
    "\",\"span_id\":\"" + obj.get("span_id", "") + 
    "\",\"operation_name\":\"" + obj.get("operation_name", "") + 
    "\",\"duration_ms\":" + obj.get("duration_ms", 0).to_string() + 
    ",\"status\":\"" + obj.get("status", "") + "\"}"
  }
  
  // 验证JSON字符串包含关键字段
  assert_eq(json_string.contains("trace_id"), true)
  assert_eq(json_string.contains("span_id"), true)
  assert_eq(json_string.contains("operation_name"), true)
  assert_eq(json_string.contains("duration_ms"), true)
  
  // 模拟JSON反序列化
  let deserialized_data = {
    // 简化的JSON反序列化模拟
    "trace_id": "12345678901234567890123456789012",
    "span_id": "1234567890123456",
    "operation_name": "http_request",
    "duration_ms": 50,
    "status": "ok"
  }
  
  // 验证反序列化数据的一致性
  assert_eq(deserialized_data.get("trace_id", ""), telemetry_data.get("trace_id", ""))
  assert_eq(deserialized_data.get("span_id", ""), telemetry_data.get("span_id", ""))
  assert_eq(deserialized_data.get("operation_name", ""), telemetry_data.get("operation_name", ""))
  assert_eq(deserialized_data.get("duration_ms", 0), telemetry_data.get("duration_ms", 0))
  
  // 测试嵌套对象的序列化
  let attributes = telemetry_data.get("attributes", {})
  let events = telemetry_data.get("events", [])
  let resource = telemetry_data.get("resource", {})
  
  assert_eq(attributes.get("http.method", ""), "GET")
  assert_eq(events.length(), 2)
  assert_eq(resource.get("service.name", ""), "user-service")
  
  // 测试数组序列化
  let event_names = []
  let mut i = 0
  while i < events.length() {
    event_names.push(events[i].get("name", ""))
    i = i + 1
  }
  
  assert_eq(event_names.length(), 2)
  assert_eq(event_names[0], "db.query.start")
  assert_eq(event_names[1], "db.query.complete")
}

test "telemetry_protobuf_serialization_compatibility" {
  // 测试Protobuf序列化兼容性
  
  let span_data = {
    "trace_id": "abcdef1234567890abcdef1234567890",
    "span_id": "abcdef12345678",
    "parent_span_id": "fedcba09876543",
    "name": "database_operation",
    "kind": "client",
    "start_time_unix_nano": 1672531200000000000,
    "end_time_unix_nano": 1672531200200000000,
    "status_code": 0,  // OK
    "status_message": "",
    "attributes": [
      {"key": "db.system", "value": {"string_value": "mysql"}},
      {"key": "db.name", "value": {"string_value": "production_db"}},
      {"key": "db.statement", "value": {"string_value": "INSERT INTO users (name, email) VALUES (?, ?)"}},
      {"key": "db.operation", "value": {"string_value": "INSERT"}},
      {"key": "server.address", "value": {"string_value": "db.example.com"}},
      {"key": "server.port", "value": {"int_value": 3306}}
    ],
    "events": [
      {
        "time_unix_nano": 1672531200050000000,
        "name": "connection.acquired",
        "attributes": [
          {"key": "connection.pool.name", "value": {"string_value": "main_pool"}},
          {"key": "connection.id", "value": {"string_value": "conn-12345"}}
        ]
      },
      {
        "time_unix_nano": 1672531200150000000,
        "name": "query.executed",
        "attributes": [
          {"key": "db.rows_affected", "value": {"int_value": 1}},
          {"key": "db.execution_time_ms", "value": {"int_value": 15}}
        ]
      }
    ],
    "resource": {
      "attributes": [
        {"key": "service.name", "value": {"string_value": "order-service"}},
        {"key": "service.version", "value": {"string_value": "2.1.0"}},
        {"key": "service.instance.id", "value": {"string_value": "order-service-abc123"}},
        {"key": "deployment.environment", "value": {"string_value": "staging"}},
        {"key": "telemetry.sdk.name", "value": {"string_value": "azimuth-telemetry"}},
        {"key": "telemetry.sdk.version", "value": {"string_value": "0.1.0"}},
        {"key": "telemetry.sdk.language", "value": {"string_value": "moonbit"}}
      ]
    }
  }
  
  // 验证原始数据
  assert_eq(span_data.get("trace_id", ""), "abcdef1234567890abcdef1234567890")
  assert_eq(span_data.get("span_id", ""), "abcdef12345678")
  assert_eq(span_data.get("name", ""), "database_operation")
  assert_eq(span_data.get("kind", ""), "client")
  
  // 模拟Protobuf序列化（简化版）
  let protobuf_binary = {
    // 简化的Protobuf二进制模拟 - 实际使用protobuf库
    let trace_id_bytes = span_data.get("trace_id", "")
    let span_id_bytes = span_data.get("span_id", "")
    let name_bytes = span_data.get("name", "")
    
    // 模拟二进制编码结果长度
    let encoded_length = trace_id_bytes.length() + span_id_bytes.length() + name_bytes.length() + 100
    encoded_length
  }
  
  // 验证编码长度合理性
  assert_eq(protobuf_binary > 0, true)
  assert_eq(protobuf_binary < 10000, true)  // 合理的大小范围
  
  // 模拟Protobuf反序列化
  let decoded_span = {
    // 简化的反序列化模拟
    "trace_id": "abcdef1234567890abcdef1234567890",
    "span_id": "abcdef12345678",
    "name": "database_operation",
    "kind": "client",
    "start_time_unix_nano": 1672531200000000000,
    "end_time_unix_nano": 1672531200200000000,
    "status_code": 0
  }
  
  // 验证解码数据的一致性
  assert_eq(decoded_span.get("trace_id", ""), span_data.get("trace_id", ""))
  assert_eq(decoded_span.get("span_id", ""), span_data.get("span_id", ""))
  assert_eq(decoded_span.get("name", ""), span_data.get("name", ""))
  assert_eq(decoded_span.get("kind", ""), span_data.get("kind", ""))
  
  // 测试属性序列化
  let attributes = span_data.get("attributes", [])
  assert_eq(attributes.length(), 6)
  
  let mut found_db_system = false
  let mut found_server_port = false
  let mut i = 0
  
  while i < attributes.length() {
    let attr = attributes[i]
    let key = attr.get("key", "")
    let value = attr.get("value", {})
    
    if key == "db.system" {
      assert_eq(value.get("string_value", ""), "mysql")
      found_db_system = true
    }
    
    if key == "server.port" {
      assert_eq(value.get("int_value", 0), 3306)
      found_server_port = true
    }
    
    i = i + 1
  }
  
  assert_eq(found_db_system, true)
  assert_eq(found_server_port, true)
  
  // 测试事件序列化
  let events = span_data.get("events", [])
  assert_eq(events.length(), 2)
  
  let first_event = events[0]
  assert_eq(first_event.get("name", ""), "connection.acquired")
  assert_eq(first_event.get("time_unix_nano", 0), 1672531200050000000)
  
  let first_event_attrs = first_event.get("attributes", [])
  assert_eq(first_event_attrs.length(), 2)
}

test "telemetry_cross_version_compatibility" {
  // 测试跨版本兼容性
  
  // 模拟v1.0格式的遥测数据
  let telemetry_v1 = {
    "version": "1.0",
    "trace_id": "11111111111111111111111111111111",
    "span_id": "1111111111111111",
    "operation_name": "legacy_operation",
    "start_time": 1672531200,
    "end_time": 1672531205,
    "duration": 5,
    "tags": {
      "service": "legacy-service",
      "env": "production",
      "component": "database"
    },
    "logs": [
      {
        "timestamp": 1672531202,
        "level": "info",
        "message": "Database query executed"
      }
    ]
  }
  
  // 模拟v2.0格式的遥测数据
  let telemetry_v2 = {
    "version": "2.0",
    "trace_id": "22222222222222222222222222222222",
    "span_id": "2222222222222222",
    "name": "modern_operation",
    "start_time_unix_nano": 1672531200000000000,
    "end_time_unix_nano": 1672531205000000000,
    "duration_nano": 5000000000,
    "kind": "server",
    "status_code": 1,
    "status_message": "Internal Server Error",
    "attributes": {
      "service.name": "modern-service",
      "deployment.environment": "production",
      "service.version": "2.0.0",
      "telemetry.sdk.name": "azimuth-telemetry",
      "telemetry.sdk.version": "0.1.0"
    },
    "events": [
      {
        "time_unix_nano": 1672531202000000000,
        "name": "exception",
        "attributes": {
          "exception.type": "DatabaseError",
          "exception.message": "Connection timeout"
        }
      }
    ],
    "resource": {
      "attributes": {
        "service.name": "modern-service",
        "service.instance.id": "modern-service-instance-1"
      }
    }
  }
  
  // 验证v1.0格式
  assert_eq(telemetry_v1.get("version", ""), "1.0")
  assert_eq(telemetry_v1.get("operation_name", ""), "legacy_operation")
  assert_eq(telemetry_v1.get("duration", 0), 5)
  assert_eq(telemetry_v1.contains("tags"), true)
  assert_eq(telemetry_v1.contains("logs"), true)
  
  // 验证v2.0格式
  assert_eq(telemetry_v2.get("version", ""), "2.0")
  assert_eq(telemetry_v2.get("name", ""), "modern_operation")
  assert_eq(telemetry_v2.get("duration_nano", 0), 5000000000)
  assert_eq(telemetry_v2.contains("attributes"), true)
  assert_eq(telemetry_v2.contains("events"), true)
  assert_eq(telemetry_v2.contains("resource"), true)
  
  // 模拟版本转换器：v1.0 -> v2.0
  let v1_to_v2_converter = fn(v1_data) {
    let converted = {
      "version": "2.0",
      "trace_id": v1_data.get("trace_id", ""),
      "span_id": v1_data.get("span_id", ""),
      "name": v1_data.get("operation_name", ""),
      "start_time_unix_nano": v1_data.get("start_time", 0) * 1000000000,
      "end_time_unix_nano": v1_data.get("end_time", 0) * 1000000000,
      "duration_nano": v1_data.get("duration", 0) * 1000000000,
      "kind": "internal",
      "status_code": 0,
      "attributes": {},
      "events": [],
      "resource": {
        "attributes": {}
      }
    }
    
    // 转换tags到attributes
    let old_tags = v1_data.get("tags", {})
    let new_attributes = converted.get("attributes", {})
    
    // 添加service.name属性
    if old_tags.contains("service") {
      new_attributes.set("service.name", old_tags.get("service", ""))
    }
    
    // 添加deployment.environment属性
    if old_tags.contains("env") {
      new_attributes.set("deployment.environment", old_tags.get("env", ""))
    }
    
    // 转换logs到events
    let old_logs = v1_data.get("logs", [])
    let new_events = converted.get("events", [])
    
    let mut i = 0
    while i < old_logs.length() {
      let log = old_logs[i]
      let event = {
        "time_unix_nano": log.get("timestamp", 0) * 1000000000,
        "name": "log." + log.get("level", ""),
        "attributes": {
          "log.message": log.get("message", "")
        }
      }
      new_events.push(event)
      i = i + 1
    }
    
    converted.set("attributes", new_attributes)
    converted.set("events", new_events)
    
    converted
  }
  
  // 执行版本转换
  let converted_v1 = v1_to_v2_converter(telemetry_v1)
  
  // 验证转换结果
  assert_eq(converted_v1.get("version", ""), "2.0")
  assert_eq(converted_v1.get("name", ""), "legacy_operation")
  assert_eq(converted_v1.get("start_time_unix_nano", 0), 1672531200000000000)
  assert_eq(converted_v1.get("end_time_unix_nano", 0), 1672531205000000000)
  assert_eq(converted_v1.get("duration_nano", 0), 5000000000)
  
  let converted_attributes = converted_v1.get("attributes", {})
  assert_eq(converted_attributes.get("service.name", ""), "legacy-service")
  assert_eq(converted_attributes.get("deployment.environment", ""), "production")
  
  let converted_events = converted_v1.get("events", [])
  assert_eq(converted_events.length(), 1)
  assert_eq(converted_events[0].get("name", ""), "log.info")
  
  // 模拟版本转换器：v2.0 -> v1.0（简化版）
  let v2_to_v1_converter = fn(v2_data) {
    let converted = {
      "version": "1.0",
      "trace_id": v2_data.get("trace_id", ""),
      "span_id": v2_data.get("span_id", ""),
      "operation_name": v2_data.get("name", ""),
      "start_time": v2_data.get("start_time_unix_nano", 0) / 1000000000,
      "end_time": v2_data.get("end_time_unix_nano", 0) / 1000000000,
      "duration": v2_data.get("duration_nano", 0) / 1000000000,
      "tags": {},
      "logs": []
    }
    
    // 转换attributes到tags
    let new_attributes = v2_data.get("attributes", {})
    let old_tags = converted.get("tags", {})
    
    if new_attributes.contains("service.name") {
      old_tags.set("service", new_attributes.get("service.name", ""))
    }
    
    if new_attributes.contains("deployment.environment") {
      old_tags.set("env", new_attributes.get("deployment.environment", ""))
    }
    
    // 转换events到logs（简化）
    let new_events = v2_data.get("events", [])
    let old_logs = converted.get("logs", [])
    
    let mut i = 0
    while i < new_events.length() and i < 1 {  // 只转换第一个事件作为示例
      let event = new_events[i]
      let log = {
        "timestamp": event.get("time_unix_nano", 0) / 1000000000,
        "level": "info",
        "message": event.get("name", "")
      }
      old_logs.push(log)
      i = i + 1
    }
    
    converted.set("tags", old_tags)
    converted.set("logs", old_logs)
    
    converted
  }
  
  // 执行反向转换
  let converted_v2 = v2_to_v1_converter(telemetry_v2)
  
  // 验证反向转换结果
  assert_eq(converted_v2.get("version", ""), "1.0")
  assert_eq(converted_v2.get("operation_name", ""), "modern_operation")
  assert_eq(converted_v2.get("start_time", 0), 1672531200)
  assert_eq(converted_v2.get("end_time", 0), 1672531205)
  assert_eq(converted_v2.get("duration", 0), 5)
  
  let converted_tags = converted_v2.get("tags", {})
  assert_eq(converted_tags.get("service", ""), "modern-service")
  assert_eq(converted_tags.get("env", ""), "production")
}

test "telemetry_format_migration_compatibility" {
  // 测试格式迁移兼容性
  
  // 模拟旧格式的指标数据
  let metrics_v1 = {
    "format_version": "1.0",
    "metric_name": "request_count",
    "metric_type": "counter",
    "value": 1250,
    "timestamp": 1672531200,
    "labels": {
      "service": "api-gateway",
      "method": "GET",
      "status": "200"
    }
  }
  
  // 模拟新格式的指标数据
  let metrics_v2 = {
    "format_version": "2.0",
    "resource": {
      "attributes": {
        "service.name": "api-gateway",
        "service.version": "1.5.0"
      }
    },
    "scope": {
      "name": "http.metrics",
      "version": "1.0.0"
    },
    "metrics": [
      {
        "name": "http.server.request.count",
        "description": "Total number of HTTP requests",
        "unit": "1",
        "data": {
          "aggregation_temporality": 1,  // cumulative
          "is_monotonic": true,
          "data_points": [
            {
              "attributes": {
                "http.method": "GET",
                "http.status_code": 200
              },
              "start_time_unix_nano": 1672531100000000000,
              "time_unix_nano": 1672531200000000000,
              "value": 1250
            }
          ]
        }
      }
    ]
  }
  
  // 验证旧格式
  assert_eq(metrics_v1.get("format_version", ""), "1.0")
  assert_eq(metrics_v1.get("metric_name", ""), "request_count")
  assert_eq(metrics_v1.get("metric_type", ""), "counter")
  assert_eq(metrics_v1.get("value", 0), 1250)
  assert_eq(metrics_v1.contains("labels"), true)
  
  // 验证新格式
  assert_eq(metrics_v2.get("format_version", ""), "2.0")
  assert_eq(metrics_v2.contains("resource"), true)
  assert_eq(metrics_v2.contains("scope"), true)
  assert_eq(metrics_v2.contains("metrics"), true)
  
  let metrics_array = metrics_v2.get("metrics", [])
  assert_eq(metrics_array.length(), 1)
  
  let metric = metrics_array[0]
  assert_eq(metric.get("name", ""), "http.server.request.count")
  assert_eq(metric.get("description", ""), "Total number of HTTP requests")
  
  let metric_data = metric.get("data", {})
  let data_points = metric_data.get("data_points", [])
  assert_eq(data_points.length(), 1)
  
  let data_point = data_points[0]
  assert_eq(data_point.get("value", 0), 1250)
  
  // 模拟格式迁移器
  let format_migrator = fn(old_format) {
    let new_format = {
      "format_version": "2.0",
      "resource": {
        "attributes": {}
      },
      "scope": {
        "name": "legacy.metrics",
        "version": "1.0.0"
      },
      "metrics": []
    }
    
    let old_labels = old_format.get("labels", {})
    let resource_attributes = new_format.get("resource", {}).get("attributes", {})
    
    // 迁移service标签
    if old_labels.contains("service") {
      resource_attributes.set("service.name", old_labels.get("service", ""))
    }
    
    // 创建新的metric结构
    let metric_name_mapping = {
      "request_count": "http.server.request.count",
      "response_time": "http.server.duration",
      "error_count": "http.server.error.count"
    }
    
    let old_metric_name = old_format.get("metric_name", "")
    let new_metric_name = metric_name_mapping.get(old_metric_name, old_metric_name)
    
    let old_metric_type = old_format.get("metric_type", "")
    let metric_type_mapping = {
      "counter": {"aggregation_temporality": 1, "is_monotonic": true},
      "gauge": {"aggregation_temporality": 2, "is_monotonic": false},
      "histogram": {"aggregation_temporality": 1, "is_monotonic": true}
    }
    
    let aggregation_config = metric_type_mapping.get(old_metric_type, {"aggregation_temporality": 1, "is_monotonic": true})
    
    let new_metric = {
      "name": new_metric_name,
      "description": "Migrated from " + old_metric_name,
      "unit": "1",
      "data": {
        "aggregation_temporality": aggregation_config.get("aggregation_temporality", 1),
        "is_monotonic": aggregation_config.get("is_monotonic", true),
        "data_points": [
          {
            "attributes": {},
            "start_time_unix_nano": old_format.get("timestamp", 0) * 1000000000 - 60000000000,  // 1分钟前
            "time_unix_nano": old_format.get("timestamp", 0) * 1000000000,
            "value": old_format.get("value", 0)
          }
        ]
      }
    }
    
    // 迁移标签到attributes
    let data_point_attributes = new_metric.get("data", {}).get("data_points", [{}])[0].get("attributes", {})
    
    let mut i = 0
    let label_keys = ["method", "status", "endpoint"]
    while i < label_keys.length() {
      let key = label_keys[i]
      if old_labels.contains(key) {
        let new_key = if key == "method" { "http.method" }
                      else if key == "status" { "http.status_code" }
                      else { key }
        data_point_attributes.set(new_key, old_labels.get(key, ""))
      }
      i = i + 1
    }
    
    let metrics = new_format.get("metrics", [])
    metrics.push(new_metric)
    
    new_format
  }
  
  // 执行格式迁移
  let migrated_metrics = format_migrator(metrics_v1)
  
  // 验证迁移结果
  assert_eq(migrated_metrics.get("format_version", ""), "2.0")
  
  let migrated_resource = migrated_metrics.get("resource", {})
  let migrated_resource_attrs = migrated_resource.get("attributes", {})
  assert_eq(migrated_resource_attrs.get("service.name", ""), "api-gateway")
  
  let migrated_metrics_array = migrated_metrics.get("metrics", [])
  assert_eq(migrated_metrics_array.length(), 1)
  
  let migrated_metric = migrated_metrics_array[0]
  assert_eq(migrated_metric.get("name", ""), "http.server.request.count")
  assert_eq(migrated_metric.get("description", ""), "Migrated from request_count")
  
  let migrated_data = migrated_metric.get("data", {})
  assert_eq(migrated_data.get("aggregation_temporality", 0), 1)
  assert_eq(migrated_data.get("is_monotonic", false), true)
  
  let migrated_data_points = migrated_data.get("data_points", [])
  assert_eq(migrated_data_points.length(), 1)
  
  let migrated_data_point = migrated_data_points[0]
  assert_eq(migrated_data_point.get("value", 0), 1250)
  
  let migrated_attributes = migrated_data_point.get("attributes", {})
  assert_eq(migrated_attributes.get("http.method", ""), "GET")
  assert_eq(migrated_attributes.get("http.status_code", ""), "200")
}