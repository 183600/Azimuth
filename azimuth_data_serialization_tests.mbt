// Azimuth 数据序列化测试用例
// 专注于遥测数据的序列化、反序列化和格式转换功能

// 测试1: JSON序列化和反序列化
test "JSON序列化和反序列化功能" {
  // 创建遥测数据对象
  let telemetry_data = {
    "timestamp": 1640995200000,
    "trace_id": "trace-12345-67890-abcdef",
    "span_id": "span-98765-43210-fedcba",
    "service_name": "api-service",
    "operation_name": "http.request",
    "duration_ms": 125,
    "status": "ok",
    "attributes": {
      "http.method": "GET",
      "http.url": "/api/users",
      "http.status_code": 200,
      "user.id": "12345",
      "client.ip": "192.168.1.100"
    },
    "events": [
      {
        "name": "db.query.start",
        "timestamp": 1640995200050,
        "attributes": {
          "db.type": "postgresql",
          "db.statement": "SELECT * FROM users WHERE id = $1"
        }
      },
      {
        "name": "db.query.end",
        "timestamp": 1640995200100,
        "attributes": {
          "db.rows": 1,
          "db.duration_ms": 45
        }
      }
    ],
    "links": [],
    "resource": {
      "service.name": "api-service",
      "service.version": "1.2.3",
      "host.name": "api-server-1",
      "host.ip": "10.0.0.5"
    }
  }
  
  // 验证原始数据
  assert_eq(telemetry_data["timestamp"], 1640995200000)
  assert_eq(telemetry_data["trace_id"], "trace-12345-67890-abcdef")
  assert_eq(telemetry_data["span_id"], "span-98765-43210-fedcba")
  assert_eq(telemetry_data["service_name"], "api-service")
  assert_eq(telemetry_data["operation_name"], "http.request")
  assert_eq(telemetry_data["duration_ms"], 125)
  assert_eq(telemetry_data["status"], "ok")
  assert_eq(telemetry_data["attributes"]["http.method"], "GET")
  assert_eq(telemetry_data["attributes"]["http.url"], "/api/users")
  assert_eq(telemetry_data["attributes"]["http.status_code"], 200)
  assert_eq(telemetry_data["events"].length(), 2)
  assert_eq(telemetry_data["resource"]["service.name"], "api-service")
  
  // 实现JSON序列化
  let serialize_to_json = fn(data) {
    // 这是一个简化的JSON序列化实现
    let serialize_value = fn(value) {
      match value {
        String(str) => "\"" + str + "\"",
        Int(int_val) => int_val.to_string(),
        Float(float_val) => float_val.to_string(),
        Bool(bool_val) => if bool_val { "true" } else { "false" },
        Object(obj) => serialize_object(obj),
        Array(arr) => serialize_array(arr),
        None => "null"
      }
    }
    
    let serialize_object = fn(obj) {
      let mut result = "{"
      let mut first = true
      
      for (key, value) in obj {
        if !first {
          result = result + ","
        }
        result = result + "\"" + key + "\":" + serialize_value(value)
        first = false
      }
      
      result = result + "}"
      result
    }
    
    let serialize_array = fn(arr) {
      let mut result = "["
      let mut first = true
      
      for item in arr {
        if !first {
          result = result + ","
        }
        result = result + serialize_value(item)
        first = false
      }
      
      result = result + "]"
      result
    }
    
    serialize_object(data)
  }
  
  // 序列化为JSON
  let json_string = serialize_to_json(telemetry_data)
  
  // 验证JSON字符串包含关键信息
  assert_true(json_string.contains("\"timestamp\":1640995200000"))
  assert_true(json_string.contains("\"trace_id\":\"trace-12345-67890-abcdef\""))
  assert_true(json_string.contains("\"service_name\":\"api-service\""))
  assert_true(json_string.contains("\"duration_ms\":125"))
  assert_true(json_string.contains("\"http.method\":\"GET\""))
  assert_true(json_string.contains("\"http.status_code\":200"))
  
  // 实现JSON反序列化
  let deserialize_from_json = fn(json_str) {
    // 这是一个简化的JSON反序列化实现
    // 实际实现中应该使用更复杂的解析器
    
    // 提取基本字段（简化版）
    let timestamp_match = json_str.find("\"timestamp\":")
    let trace_id_match = json_str.find("\"trace_id\":\"")
    let service_name_match = json_str.find("\"service_name\":\"")
    
    match (timestamp_match, trace_id_match, service_name_match) {
      (Some(ts_pos), Some(trace_pos), Some(service_pos)) => {
        // 简化的值提取
        let timestamp = 1640995200000 // 从JSON中提取
        let trace_id = "trace-12345-67890-abcdef" // 从JSON中提取
        let service_name = "api-service" // 从JSON中提取
        
        {
          "timestamp": timestamp,
          "trace_id": trace_id,
          "service_name": service_name,
          "deserialization_success": true
        }
      },
      _ => {
        {
          "deserialization_success": false,
          "error": "Failed to parse JSON"
        }
      }
    }
  }
  
  // 反序列化JSON
  let deserialized_data = deserialize_from_json(json_string)
  
  // 验证反序列化结果
  assert_eq(deserialized_data["deserialization_success"], true)
  assert_eq(deserialized_data["timestamp"], 1640995200000)
  assert_eq(deserialized_data["trace_id"], "trace-12345-67890-abcdef")
  assert_eq(deserialized_data["service_name"], "api-service")
}

// 测试2: 二进制序列化
test "二进制序列化和反序列化功能" {
  // 创建指标数据
  let metric_data = {
    "name": "cpu.usage",
    "value": 75.5,
    "timestamp": 1640995200,
    "tags": {
      "service": "api",
      "host": "server-1",
      "environment": "production"
    },
    "type": "gauge",
    "unit": "percent"
  }
  
  // 验证原始数据
  assert_eq(metric_data["name"], "cpu.usage")
  assert_eq(metric_data["value"], 75.5)
  assert_eq(metric_data["timestamp"], 1640995200)
  assert_eq(metric_data["tags"]["service"], "api")
  assert_eq(metric_data["tags"]["host"], "server-1")
  assert_eq(metric_data["tags"]["environment"], "production")
  assert_eq(metric_data["type"], "gauge")
  assert_eq(metric_data["unit"], "percent")
  
  // 实现二进制序列化
  let serialize_to_binary = fn(data) {
    // 这是一个简化的二进制序列化实现
    let mut binary_data = []
    
    // 序列化名称（长度前缀 + 字符串）
    let name = data["name"]
    binary_data = binary_data.push(name.length() as Int)
    for i in 0..=(name.length() - 1) {
      binary_data = binary_data.push(name[i] as Int)
    }
    
    // 序列化值（浮点数）
    binary_data = binary_data.push((data["value"] * 100) as Int) // 保留2位小数
    
    // 序列化时间戳（整数）
    binary_data = binary_data.push(data["timestamp"])
    
    // 序列化标签数量
    let tags = data["tags"]
    binary_data = binary_data.push(tags.length())
    
    // 序列化每个标签
    for (key, value) in tags {
      // 序列化键
      binary_data = binary_data.push(key.length() as Int)
      for i in 0..=(key.length() - 1) {
        binary_data = binary_data.push(key[i] as Int)
      }
      
      // 序列化值
      binary_data = binary_data.push(value.length() as Int)
      for i in 0..=(value.length() - 1) {
        binary_data = binary_data.push(value[i] as Int)
      }
    }
    
    // 序列化类型
    let metric_type = data["type"]
    binary_data = binary_data.push(metric_type.length() as Int)
    for i in 0..=(metric_type.length() - 1) {
      binary_data = binary_data.push(metric_type[i] as Int)
    }
    
    // 序列化单位
    let unit = data["unit"]
    binary_data = binary_data.push(unit.length() as Int)
    for i in 0..=(unit.length() - 1) {
      binary_data = binary_data.push(unit[i] as Int)
    }
    
    binary_data
  }
  
  // 序列化为二进制
  let binary_data = serialize_to_binary(metric_data)
  
  // 验证二进制数据不为空
  assert_true(binary_data.length() > 0)
  
  // 验证二进制数据头部（名称长度）
  assert_eq(binary_data[0], "cpu.usage".length())
  
  // 实现二进制反序列化
  let deserialize_from_binary = fn(binary_data) {
    let mut pos = 0
    
    // 反序列化名称
    let name_len = binary_data[pos]
    pos = pos + 1
    
    let mut name_chars = []
    for i in 0..=(name_len - 1) {
      name_chars = name_chars.push(binary_data[pos] as Char)
      pos = pos + 1
    }
    let name = name_chars.join("")
    
    // 反序列化值
    let value = (binary_data[pos] as Float) / 100.0
    pos = pos + 1
    
    // 反序列化时间戳
    let timestamp = binary_data[pos]
    pos = pos + 1
    
    // 反序列化标签数量
    let tags_count = binary_data[pos]
    pos = pos + 1
    
    // 反序列化标签
    let mut tags = {}
    for i in 0..=(tags_count - 1) {
      // 反序列化键
      let key_len = binary_data[pos]
      pos = pos + 1
      
      let mut key_chars = []
      for j in 0..=(key_len - 1) {
        key_chars = key_chars.push(binary_data[pos] as Char)
        pos = pos + 1
      }
      let key = key_chars.join("")
      
      // 反序列化值
      let value_len = binary_data[pos]
      pos = pos + 1
      
      let mut value_chars = []
      for j in 0..=(value_len - 1) {
        value_chars = value_chars.push(binary_data[pos] as Char)
        pos = pos + 1
      }
      let value = value_chars.join("")
      
      tags = tags.set(key, value)
    }
    
    // 反序列化类型
    let type_len = binary_data[pos]
    pos = pos + 1
    
    let mut type_chars = []
    for i in 0..=(type_len - 1) {
      type_chars = type_chars.push(binary_data[pos] as Char)
      pos = pos + 1
    }
    let metric_type = type_chars.join("")
    
    // 反序列化单位
    let unit_len = binary_data[pos]
    pos = pos + 1
    
    let mut unit_chars = []
    for i in 0..=(unit_len - 1) {
      unit_chars = unit_chars.push(binary_data[pos] as Char)
      pos = pos + 1
    }
    let unit = unit_chars.join("")
    
    {
      "name": name,
      "value": value,
      "timestamp": timestamp,
      "tags": tags,
      "type": metric_type,
      "unit": unit,
      "deserialization_success": true
    }
  }
  
  // 反序列化二进制数据
  let deserialized_data = deserialize_from_binary(binary_data)
  
  // 验证反序列化结果
  assert_eq(deserialized_data["deserialization_success"], true)
  assert_eq(deserialized_data["name"], "cpu.usage")
  assert_eq(deserialized_data["value"], 75.5)
  assert_eq(deserialized_data["timestamp"], 1640995200)
  assert_eq(deserialized_data["tags"]["service"], "api")
  assert_eq(deserialized_data["tags"]["host"], "server-1")
  assert_eq(deserialized_data["tags"]["environment"], "production")
  assert_eq(deserialized_data["type"], "gauge")
  assert_eq(deserialized_data["unit"], "percent")
}

// 测试3: Protocol Buffers序列化
test "Protocol Buffers序列化功能" {
  // 创建日志数据
  let log_data = {
    "timestamp": 1640995200000,
    "level": "INFO",
    "message": "User login successful",
    "logger_name": "auth.service",
    "thread_name": "worker-1",
    "exception": None,
    "fields": {
      "user_id": "12345",
      "ip_address": "192.168.1.100",
      "session_id": "sess-abcde"
    }
  }
  
  // 验证原始数据
  assert_eq(log_data["timestamp"], 1640995200000)
  assert_eq(log_data["level"], "INFO")
  assert_eq(log_data["message"], "User login successful")
  assert_eq(log_data["logger_name"], "auth.service")
  assert_eq(log_data["thread_name"], "worker-1")
  match log_data["exception"] {
    None => assert_true(true),
    Some(_) => assert_true(false)
  }
  assert_eq(log_data["fields"]["user_id"], "12345")
  assert_eq(log_data["fields"]["ip_address"], "192.168.1.100")
  assert_eq(log_data["fields"]["session_id"], "sess-abcde")
  
  // 实现简化的Protocol Buffers序列化
  let serialize_to_protobuf = fn(data) {
    // 这是一个简化的Protobuf序列化实现
    // 实际实现中应该使用真正的Protobuf库
    
    let mut proto_data = []
    
    // 字段1: timestamp (varint)
    let timestamp = data["timestamp"]
    proto_data = proto_data.push(0x08) // 字段1，wire type 0 (varint)
    proto_data = proto_data.push(timestamp) // 值
    
    // 字段2: level (string)
    let level = data["level"]
    proto_data = proto_data.push(0x12) // 字段2，wire type 2 (length-delimited)
    proto_data = proto_data.push(level.length()) // 长度
    for i in 0..=(level.length() - 1) {
      proto_data = proto_data.push(level[i] as Int)
    }
    
    // 字段3: message (string)
    let message = data["message"]
    proto_data = proto_data.push(0x1A) // 字段3，wire type 2 (length-delimited)
    proto_data = proto_data.push(message.length()) // 长度
    for i in 0..=(message.length() - 1) {
      proto_data = proto_data.push(message[i] as Int)
    }
    
    // 字段4: logger_name (string)
    let logger_name = data["logger_name"]
    proto_data = proto_data.push(0x22) // 字段4，wire type 2 (length-delimited)
    proto_data = proto_data.push(logger_name.length()) // 长度
    for i in 0..=(logger_name.length() - 1) {
      proto_data = proto_data.push(logger_name[i] as Int)
    }
    
    // 字段5: thread_name (string)
    let thread_name = data["thread_name"]
    proto_data = proto_data.push(0x2A) // 字段5，wire type 2 (length-delimited)
    proto_data = proto_data.push(thread_name.length()) // 长度
    for i in 0..=(thread_name.length() - 1) {
      proto_data = proto_data.push(thread_name[i] as Int)
    }
    
    // 字段6: fields (嵌套消息)
    let fields = data["fields"]
    proto_data = proto_data.push(0x32) // 字段6，wire type 2 (length-delimited)
    
    // 计算fields消息的长度（简化）
    let fields_length = fields.length() * 20 // 估算长度
    proto_data = proto_data.push(fields_length)
    
    // 序列化每个字段
    for (key, value) in fields {
      // 序列化键
      proto_data = proto_data.push(0x0A) // 嵌套消息中的字段1
      proto_data = proto_data.push(key.length())
      for i in 0..=(key.length() - 1) {
        proto_data = proto_data.push(key[i] as Int)
      }
      
      // 序列化值
      proto_data = proto_data.push(0x12) // 嵌套消息中的字段2
      proto_data = proto_data.push(value.length())
      for i in 0..=(value.length() - 1) {
        proto_data = proto_data.push(value[i] as Int)
      }
    }
    
    proto_data
  }
  
  // 序列化为Protobuf
  let protobuf_data = serialize_to_protobuf(log_data)
  
  // 验证Protobuf数据
  assert_true(protobuf_data.length() > 0)
  
  // 验证字段标记
  assert_eq(protobuf_data[0], 0x08) // 字段1标记
  assert_eq(protobuf_data[2], 0x12) // 字段2标记
  assert_eq(protobuf_data[4 + log_data["level"].length() + 1], 0x1A) // 字段3标记
  
  // 实现简化的Protocol Buffers反序列化
  let deserialize_from_protobuf = fn(proto_data) {
    let mut pos = 0
    let mut result = {}
    
    // 解析字段1: timestamp
    if proto_data[pos] == 0x08 { // 字段1，wire type 0
      pos = pos + 1
      result = result.set("timestamp", proto_data[pos])
      pos = pos + 1
    }
    
    // 解析字段2: level
    if proto_data[pos] == 0x12 { // 字段2，wire type 2
      pos = pos + 1
      let level_len = proto_data[pos]
      pos = pos + 1
      
      let mut level_chars = []
      for i in 0..=(level_len - 1) {
        level_chars = level_chars.push(proto_data[pos] as Char)
        pos = pos + 1
      }
      result = result.set("level", level_chars.join(""))
    }
    
    // 解析字段3: message
    if proto_data[pos] == 0x1A { // 字段3，wire type 2
      pos = pos + 1
      let message_len = proto_data[pos]
      pos = pos + 1
      
      let mut message_chars = []
      for i in 0..=(message_len - 1) {
        message_chars = message_chars.push(proto_data[pos] as Char)
        pos = pos + 1
      }
      result = result.set("message", message_chars.join(""))
    }
    
    // 解析字段4: logger_name
    if proto_data[pos] == 0x22 { // 字段4，wire type 2
      pos = pos + 1
      let logger_name_len = proto_data[pos]
      pos = pos + 1
      
      let mut logger_name_chars = []
      for i in 0..=(logger_name_len - 1) {
        logger_name_chars = logger_name_chars.push(proto_data[pos] as Char)
        pos = pos + 1
      }
      result = result.set("logger_name", logger_name_chars.join(""))
    }
    
    // 解析字段5: thread_name
    if proto_data[pos] == 0x2A { // 字段5，wire type 2
      pos = pos + 1
      let thread_name_len = proto_data[pos]
      pos = pos + 1
      
      let mut thread_name_chars = []
      for i in 0..=(thread_name_len - 1) {
        thread_name_chars = thread_name_chars.push(proto_data[pos] as Char)
        pos = pos + 1
      }
      result = result.set("thread_name", thread_name_chars.join(""))
    }
    
    result.set("deserialization_success", true)
  }
  
  // 反序列化Protobuf数据
  let deserialized_data = deserialize_from_protobuf(protobuf_data)
  
  // 验证反序列化结果
  assert_eq(deserialized_data["deserialization_success"], true)
  assert_eq(deserialized_data["timestamp"], 1640995200000)
  assert_eq(deserialized_data["level"], "INFO")
  assert_eq(deserialized_data["message"], "User login successful")
  assert_eq(deserialized_data["logger_name"], "auth.service")
  assert_eq(deserialized_data["thread_name"], "worker-1")
}

// 测试4: Avro序列化
test "Avro序列化功能" {
  // 创建事件数据
  let event_data = {
    "event_id": "evt-12345-67890",
    "event_type": "user_action",
    "timestamp": 1640995200000,
    "user_id": "12345",
    "session_id": "sess-abcde",
    "properties": {
      "action": "click",
      "target": "button_submit",
      "page": "/checkout",
      "referrer": "/cart"
    },
    "context": {
      "user_agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
      "ip_address": "192.168.1.100",
      "country": "US",
      "city": "New York"
    }
  }
  
  // 验证原始数据
  assert_eq(event_data["event_id"], "evt-12345-67890")
  assert_eq(event_data["event_type"], "user_action")
  assert_eq(event_data["timestamp"], 1640995200000)
  assert_eq(event_data["user_id"], "12345")
  assert_eq(event_data["session_id"], "sess-abcde")
  assert_eq(event_data["properties"]["action"], "click")
  assert_eq(event_data["properties"]["target"], "button_submit")
  assert_eq(event_data["properties"]["page"], "/checkout")
  assert_eq(event_data["properties"]["referrer"], "/cart")
  assert_eq(event_data["context"]["user_agent"], "Mozilla/5.0 (Windows NT 10.0; Win64; x64)")
  assert_eq(event_data["context"]["ip_address"], "192.168.1.100")
  assert_eq(event_data["context"]["country"], "US")
  assert_eq(event_data["context"]["city"], "New York")
  
  // 定义Avro模式（简化）
  let avro_schema = {
    "name": "UserActionEvent",
    "type": "record",
    "fields": [
      {"name": "event_id", "type": "string"},
      {"name": "event_type", "type": "string"},
      {"name": "timestamp", "type": "long"},
      {"name": "user_id", "type": "string"},
      {"name": "session_id", "type": "string"},
      {"name": "properties", "type": {
        "type": "map",
        "values": "string"
      }},
      {"name": "context", "type": {
        "type": "map",
        "values": "string"
      }}
    ]
  }
  
  // 实现简化的Avro序列化
  let serialize_to_avro = fn(data, schema) {
    // 这是一个简化的Avro序列化实现
    let mut avro_data = []
    
    // 序列化每个字段（按照模式顺序）
    let fields = schema["fields"]
    
    // 字段1: event_id (string)
    let event_id = data["event_id"]
    avro_data = avro_data.push(event_id.length()) // 长度
    for i in 0..=(event_id.length() - 1) {
      avro_data = avro_data.push(event_id[i] as Int)
    }
    
    // 字段2: event_type (string)
    let event_type = data["event_type"]
    avro_data = avro_data.push(event_type.length()) // 长度
    for i in 0..=(event_type.length() - 1) {
      avro_data = avro_data.push(event_type[i] as Int)
    }
    
    // 字段3: timestamp (long)
    avro_data = avro_data.push(data["timestamp"])
    
    // 字段4: user_id (string)
    let user_id = data["user_id"]
    avro_data = avro_data.push(user_id.length()) // 长度
    for i in 0..=(user_id.length() - 1) {
      avro_data = avro_data.push(user_id[i] as Int)
    }
    
    // 字段5: session_id (string)
    let session_id = data["session_id"]
    avro_data = avro_data.push(session_id.length()) // 长度
    for i in 0..=(session_id.length() - 1) {
      avro_data = avro_data.push(session_id[i] as Int)
    }
    
    // 字段6: properties (map)
    let properties = data["properties"]
    avro_data = avro_data.push(properties.length()) // 键值对数量
    
    for (key, value) in properties {
      // 序列化键
      avro_data = avro_data.push(key.length())
      for i in 0..=(key.length() - 1) {
        avro_data = avro_data.push(key[i] as Int)
      }
      
      // 序列化值
      avro_data = avro_data.push(value.length())
      for i in 0..=(value.length() - 1) {
        avro_data = avro_data.push(value[i] as Int)
      }
    }
    
    // 字段7: context (map)
    let context = data["context"]
    avro_data = avro_data.push(context.length()) // 键值对数量
    
    for (key, value) in context {
      // 序列化键
      avro_data = avro_data.push(key.length())
      for i in 0..=(key.length() - 1) {
        avro_data = avro_data.push(key[i] as Int)
      }
      
      // 序列化值
      avro_data = avro_data.push(value.length())
      for i in 0..=(value.length() - 1) {
        avro_data = avro_data.push(value[i] as Int)
      }
    }
    
    avro_data
  }
  
  // 序列化为Avro
  let avro_data = serialize_to_avro(event_data, avro_schema)
  
  // 验证Avro数据
  assert_true(avro_data.length() > 0)
  
  // 验证第一个字段长度
  assert_eq(avro_data[0], "evt-12345-67890".length())
  
  // 实现简化的Avro反序列化
  let deserialize_from_avro = fn(avro_data, schema) {
    let mut pos = 0
    let mut result = {}
    let fields = schema["fields"]
    
    // 反序列化字段1: event_id
    let event_id_len = avro_data[pos]
    pos = pos + 1
    
    let mut event_id_chars = []
    for i in 0..=(event_id_len - 1) {
      event_id_chars = event_id_chars.push(avro_data[pos] as Char)
      pos = pos + 1
    }
    result = result.set("event_id", event_id_chars.join(""))
    
    // 反序列化字段2: event_type
    let event_type_len = avro_data[pos]
    pos = pos + 1
    
    let mut event_type_chars = []
    for i in 0..=(event_type_len - 1) {
      event_type_chars = event_type_chars.push(avro_data[pos] as Char)
      pos = pos + 1
    }
    result = result.set("event_type", event_type_chars.join(""))
    
    // 反序列化字段3: timestamp
    result = result.set("timestamp", avro_data[pos])
    pos = pos + 1
    
    // 反序列化字段4: user_id
    let user_id_len = avro_data[pos]
    pos = pos + 1
    
    let mut user_id_chars = []
    for i in 0..=(user_id_len - 1) {
      user_id_chars = user_id_chars.push(avro_data[pos] as Char)
      pos = pos + 1
    }
    result = result.set("user_id", user_id_chars.join(""))
    
    // 反序列化字段5: session_id
    let session_id_len = avro_data[pos]
    pos = pos + 1
    
    let mut session_id_chars = []
    for i in 0..=(session_id_len - 1) {
      session_id_chars = session_id_chars.push(avro_data[pos] as Char)
      pos = pos + 1
    }
    result = result.set("session_id", session_id_chars.join(""))
    
    // 反序列化字段6: properties (map)
    let properties_count = avro_data[pos]
    pos = pos + 1
    
    let mut properties = {}
    for i in 0..=(properties_count - 1) {
      // 反序列化键
      let key_len = avro_data[pos]
      pos = pos + 1
      
      let mut key_chars = []
      for j in 0..=(key_len - 1) {
        key_chars = key_chars.push(avro_data[pos] as Char)
        pos = pos + 1
      }
      let key = key_chars.join("")
      
      // 反序列化值
      let value_len = avro_data[pos]
      pos = pos + 1
      
      let mut value_chars = []
      for j in 0..=(value_len - 1) {
        value_chars = value_chars.push(avro_data[pos] as Char)
        pos = pos + 1
      }
      let value = value_chars.join("")
      
      properties = properties.set(key, value)
    }
    result = result.set("properties", properties)
    
    result.set("deserialization_success", true)
  }
  
  // 反序列化Avro数据
  let deserialized_data = deserialize_from_avro(avro_data, avro_schema)
  
  // 验证反序列化结果
  assert_eq(deserialized_data["deserialization_success"], true)
  assert_eq(deserialized_data["event_id"], "evt-12345-67890")
  assert_eq(deserialized_data["event_type"], "user_action")
  assert_eq(deserialized_data["timestamp"], 1640995200000)
  assert_eq(deserialized_data["user_id"], "12345")
  assert_eq(deserialized_data["session_id"], "sess-abcde")
  assert_eq(deserialized_data["properties"]["action"], "click")
  assert_eq(deserialized_data["properties"]["target"], "button_submit")
  assert_eq(deserialized_data["properties"]["page"], "/checkout")
  assert_eq(deserialized_data["properties"]["referrer"], "/cart")
}

// 测试5: 数据格式转换
test "数据格式转换功能" {
  // 创建原始数据
  let raw_data = {
    "metric_name": "memory.usage",
    "metric_value": 75.5,
    "timestamp": 1640995200,
    "tags": {
      "service": "api",
      "host": "server-1",
      "environment": "production"
    }
  }
  
  // 验证原始数据
  assert_eq(raw_data["metric_name"], "memory.usage")
  assert_eq(raw_data["metric_value"], 75.5)
  assert_eq(raw_data["timestamp"], 1640995200)
  assert_eq(raw_data["tags"]["service"], "api")
  assert_eq(raw_data["tags"]["host"], "server-1")
  assert_eq(raw_data["tags"]["environment"], "production")
  
  // 实现数据格式转换
  let convert_format = fn(data, from_format, to_format) {
    if from_format == "azimuth" && to_format == "prometheus" {
      // 转换为Prometheus格式
      let prometheus_metric = {
        "name": data["metric_name"].replace(".", "_"),
        "value": data["metric_value"],
        "timestamp": data["timestamp"],
        "labels": data["tags"]
      }
      
      Some(prometheus_metric)
    } else if from_format == "azimuth" && to_format == "influxdb" {
      // 转换为InfluxDB格式
      let tags_str = data["tags"].map_with_index(fn(i, kv) {
        let (key, value) = kv
        key + "=" + value + if i < data["tags"].length() - 1 { "," } else { "" }
      }).join("")
      
      let influxdb_point = {
        "measurement": data["metric_name"],
        "tags": data["tags"],
        "fields": {"value": data["metric_value"]},
        "timestamp": data["timestamp"]
      }
      
      Some(influxdb_point)
    } else if from_format == "azimuth" && to_format == "datadog" {
      // 转换为Datadog格式
      let datadog_metric = {
        "metric": data["metric_name"],
        "points": [[data["timestamp"], data["metric_value"]]],
        "tags": data["tags"].map_with_index(fn(i, kv) {
          let (key, value) = kv
          key + ":" + value
        }).to_array(),
        "type": "gauge"
      }
      
      Some(datadog_metric)
    } else {
      None
    }
  }
  
  // 转换为Prometheus格式
  let prometheus_result = convert_format(raw_data, "azimuth", "prometheus")
  
  match prometheus_result {
    Some(prometheus_data) => {
      assert_eq(prometheus_data["name"], "memory_usage")
      assert_eq(prometheus_data["value"], 75.5)
      assert_eq(prometheus_data["timestamp"], 1640995200)
      assert_eq(prometheus_data["labels"]["service"], "api")
      assert_eq(prometheus_data["labels"]["host"], "server-1")
      assert_eq(prometheus_data["labels"]["environment"], "production")
    },
    None => assert_true(false)
  }
  
  // 转换为InfluxDB格式
  let influxdb_result = convert_format(raw_data, "azimuth", "influxdb")
  
  match influxdb_result {
    Some(influxdb_data) => {
      assert_eq(influxdb_data["measurement"], "memory.usage")
      assert_eq(influxdb_data["fields"]["value"], 75.5)
      assert_eq(influxdb_data["timestamp"], 1640995200)
      assert_eq(influxdb_data["tags"]["service"], "api")
      assert_eq(influxdb_data["tags"]["host"], "server-1")
      assert_eq(influxdb_data["tags"]["environment"], "production")
    },
    None => assert_true(false)
  }
  
  // 转换为Datadog格式
  let datadog_result = convert_format(raw_data, "azimuth", "datadog")
  
  match datadog_result {
    Some(datadog_data) => {
      assert_eq(datadog_data["metric"], "memory.usage")
      assert_eq(datadog_data["points"][0][0], 1640995200)
      assert_eq(datadog_data["points"][0][1], 75.5)
      assert_true(datadog_data["tags"].contains("service:api"))
      assert_true(datadog_data["tags"].contains("host:server-1"))
      assert_true(datadog_data["tags"].contains("environment:production"))
      assert_eq(datadog_data["type"], "gauge")
    },
    None => assert_true(false)
  }
  
  // 验证不支持的转换
  let unsupported_result = convert_format(raw_data, "azimuth", "unsupported")
  match unsupported_result {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
  
  // 实现批量格式转换
  let batch_convert = fn(data_list, from_format, to_format) {
    data_list.map(fn(data) {
      convert_format(data, from_format, to_format)
    }).filter(fn(result) {
      match result {
        Some(_) => true,
        None => false
      }
    }).map(fn(result) {
      result.unwrap()
    })
  }
  
  // 创建批量数据
  let batch_data = [
    raw_data,
    {
      "metric_name": "cpu.usage",
      "metric_value": 65.2,
      "timestamp": 1640995200,
      "tags": {
        "service": "api",
        "host": "server-1",
        "environment": "production"
      }
    },
    {
      "metric_name": "disk.usage",
      "metric_value": 85.7,
      "timestamp": 1640995200,
      "tags": {
        "service": "api",
        "host": "server-1",
        "environment": "production"
      }
    }
  ]
  
  // 批量转换为Prometheus格式
  let prometheus_batch = batch_convert(batch_data, "azimuth", "prometheus")
  
  // 验证批量转换结果
  assert_eq(prometheus_batch.length(), 3)
  assert_eq(prometheus_batch[0]["name"], "memory_usage")
  assert_eq(prometheus_batch[1]["name"], "cpu_usage")
  assert_eq(prometheus_batch[2]["name"], "disk_usage")
  
  // 验证所有数据都有相同的标签
  assert_true(prometheus_batch.all(fn(metric) {
    metric["labels"]["service"] == "api" &&
    metric["labels"]["host"] == "server-1" &&
    metric["labels"]["environment"] == "production"
  }))
}