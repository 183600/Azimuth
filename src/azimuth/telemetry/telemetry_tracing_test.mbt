// 遥测追踪测试用例
// 测试分布式追踪功能

test "telemetry_span_creation" {
  // 测试span创建
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let parent_span_id = "a1b2c3d4e5f67890"
  let operation_name = "http_get_request"
  let start_time = 1640995200L
  let end_time = 1640995250L
  
  // 创建span数据
  let span_data = {
    "trace_id": trace_id,
    "span_id": span_id,
    "parent_span_id": parent_span_id,
    "operation_name": operation_name,
    "start_time": start_time,
    "end_time": end_time,
    "duration": end_time - start_time
  }
  
  // 验证span数据
  assert_eq(span_data["trace_id"], trace_id)
  assert_eq(span_data["span_id"], span_id)
  assert_eq(span_data["parent_span_id"], parent_span_id)
  assert_eq(span_data["operation_name"], operation_name)
  assert_eq(span_data["start_time"], start_time)
  assert_eq(span_data["end_time"], end_time)
  assert_eq(span_data["duration"], 50L) // 1640995250 - 1640995200
  
  // 创建span字符串表示
  let span_string = operation_name + " " + trace_id + ":" + span_id + " " + 
                   span_data["duration"].to_string() + "ms"
  
  assert_eq(span_string, "http_get_request 0af7651916cd43dd8448eb211c80319c:b7ad6b7169203331 50ms")
  assert_eq(span_string.has_prefix(operation_name), true)
  assert_eq(span_string.contains(trace_id + ":" + span_id), true)
  assert_eq(span_string.has_suffix("50ms"), true)
}

test "telemetry_span_hierarchy" {
  // 测试span层次结构
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let root_span_id = "a1b2c3d4e5f67890"
  let child_span_ids = ["b7ad6b7169203331", "c8d9e0f1a2b3c4d5", "d1e2f3a4b5c6d7e8"]
  
  // 创建span层次关系
  let span_hierarchy = [
    (root_span_id, ""), // 根span没有父span
    (child_span_ids[0], root_span_id),
    (child_span_ids[1], root_span_id),
    (child_span_ids[2], child_span_ids[0]) // 嵌套子span
  ]
  
  // 验证层次关系
  assert_eq(span_hierarchy.length(), 4)
  assert_eq(span_hierarchy[0].0, root_span_id)
  assert_eq(span_hierarchy[0].1, "") // 根span
  
  assert_eq(span_hierarchy[1].0, child_span_ids[0])
  assert_eq(span_hierarchy[1].1, root_span_id) // 第一层子span
  
  assert_eq(span_hierarchy[3].0, child_span_ids[2])
  assert_eq(span_hierarchy[3].1, child_span_ids[0]) // 第二层子span
  
  // 计算每个span的子span数量
  let mut child_counts = [0, 0, 0, 0]
  let mut i = 0
  while i < span_hierarchy.length() {
    let parent_id = span_hierarchy[i].1
    if parent_id != "" {
      // 找到父span的索引
      let mut j = 0
      while j < span_hierarchy.length() {
        if span_hierarchy[j].0 == parent_id {
          child_counts[j] = child_counts[j] + 1
        }
        j = j + 1
      }
    }
    i = i + 1
  }
  
  // 验证子span数量
  assert_eq(child_counts[0], 2) // root_span有2个直接子span
  assert_eq(child_counts[1], 1) // child_span_ids[0]有1个子span
  assert_eq(child_counts[2], 0) // child_span_ids[1]没有子span
  assert_eq(child_counts[3], 0) // child_span_ids[2]没有子span
}

test "telemetry_span_attributes" {
  // 测试span属性
  
  let span_id = "b7ad6b7169203331"
  let attributes = [
    ("http.method", "GET"),
    ("http.url", "https://api.example.com/users"),
    ("http.status_code", "200"),
    ("user.id", "12345"),
    ("service.name", "user-service")
  ]
  
  // 创建span属性字符串
  let mut attributes_string = ""
  let mut i = 0
  while i < attributes.length() {
    if i > 0 {
      attributes_string = attributes_string + ","
    }
    attributes_string = attributes_string + attributes[i].0 + "=\"" + attributes[i].1 + "\""
    i = i + 1
  }
  
  // 验证属性字符串
  assert_eq(attributes_string.contains("http.method=\"GET\""), true)
  assert_eq(attributes_string.contains("http.url=\"https://api.example.com/users\""), true)
  assert_eq(attributes_string.contains("http.status_code=\"200\""), true)
  assert_eq(attributes_string.contains("user.id=\"12345\""), true)
  assert_eq(attributes_string.contains("service.name=\"user-service\""), true)
  
  // 创建带属性的span表示
  let span_with_attributes = "span_" + span_id + "{" + attributes_string + "}"
  
  assert_eq(span_with_attributes.has_prefix("span_" + span_id + "{"), true)
  assert_eq(span_with_attributes.has_suffix("}"), true)
  assert_eq(span_with_attributes.contains(attributes_string), true)
  
  // 查找特定属性
  let has_http_method = attributes_string.contains("http.method=\"GET\"")
  let has_user_id = attributes_string.contains("user.id=\"12345\"")
  
  assert_eq(has_http_method, true)
  assert_eq(has_user_id, true)
}

test "telemetry_span_events" {
  // 测试span事件
  
  let span_id = "b7ad6b7169203331"
  let events = [
    ("cache_miss", 1640995201L, [("cache.key", "user:12345")]),
    ("db_query_start", 1640995202L, [("db.query", "SELECT * FROM users WHERE id = 12345")]),
    ("db_query_complete", 1640995205L, [("db.duration", "3ms")]),
    ("response_sent", 1640995250L, [("response.size", "1024")])
  ]
  
  // 创建事件字符串
  let mut events_string = ""
  let mut i = 0
  while i < events.length() {
    let event = events[i]
    let event_str = event.0 + "@" + event.1.to_string()
    
    // 添加事件属性
    let mut j = 0
    let mut event_attrs = ""
    while j < event.2.length() {
      if j > 0 {
        event_attrs = event_attrs + ","
      }
      event_attrs = event_attrs + event.2[j].0 + "=" + event.2[j].1
      j = j + 1
    }
    
    if event_attrs != "" {
      event_str = event_str + "[" + event_attrs + "]"
    }
    
    if i > 0 {
      events_string = events_string + ";"
    }
    events_string = events_string + event_str
    i = i + 1
  }
  
  // 验证事件字符串
  assert_eq(events_string.contains("cache_miss@1640995201"), true)
  assert_eq(events_string.contains("db_query_start@1640995202"), true)
  assert_eq(events_string.contains("db_query_complete@1640995205"), true)
  assert_eq(events_string.contains("response_sent@1640995250"), true)
  
  assert_eq(events_string.contains("cache.key=user:12345"), true)
  assert_eq(events_string.contains("db.duration=3ms"), true)
  
  // 创建带事件的span表示
  let span_with_events = "span_" + span_id + "_events{" + events_string + "}"
  
  assert_eq(span_with_events.has_prefix("span_" + span_id + "_events{"), true)
  assert_eq(span_with_events.has_suffix("}"), true)
  assert_eq(span_with_events.contains(events_string), true)
}

test "telemetry_span_status" {
  // 测试span状态
  
  let span_id = "b7ad6b7169203331"
  let status_codes = ["ok", "error", "cancelled", "deadline_exceeded", "not_found"]
  let status_messages = [
    "Operation completed successfully",
    "Database connection failed",
    "Request cancelled by client",
    "Operation timed out after 30 seconds",
    "Resource not found"
  ]
  
  // 创建状态信息
  let mut status_info = []
  let mut i = 0
  while i < status_codes.length() {
    let status = {
      "code": status_codes[i],
      "message": status_messages[i],
      "span_id": span_id
    }
    status_info.push(status)
    i = i + 1
  }
  
  // 验证状态信息
  assert_eq(status_info.length(), 5)
  assert_eq(status_info[0]["code"], "ok")
  assert_eq(status_info[1]["code"], "error")
  assert_eq(status_info[2]["code"], "cancelled")
  assert_eq(status_info[3]["code"], "deadline_exceeded")
  assert_eq(status_info[4]["code"], "not_found")
  
  assert_eq(status_info[0]["message"], "Operation completed successfully")
  assert_eq(status_info[1]["message"], "Database connection failed")
  
  // 创建状态字符串
  let status_string = status_codes[1] + ":" + status_messages[1]
  assert_eq(status_string, "error:Database connection failed")
  
  // 创建带状态的span表示
  let span_with_status = "span_" + span_id + "_status{" + status_string + "}"
  
  assert_eq(span_with_status.has_prefix("span_" + span_id + "_status{"), true)
  assert_eq(span_with_status.has_suffix("}"), true)
  assert_eq(span_with_status.contains("error:Database connection failed"), true)
  
  // 统计不同状态的数量
  let mut ok_count = 0
  let mut error_count = 0
  
  i = 0
  while i < status_info.length() {
    if status_info[i]["code"] == "ok" {
      ok_count = ok_count + 1
    } else {
      error_count = error_count + 1
    }
    i = i + 1
  }
  
  assert_eq(ok_count, 1)
  assert_eq(error_count, 4)
}