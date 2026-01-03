// Azimuth 分布式追踪测试用例
// 专注于分布式系统中的链路追踪和上下文传播功能

// 测试1: 追踪标识符生成和验证
test "追踪标识符生成和验证" {
  // 生成追踪ID
  let trace_id = "trace-12345-67890-abcdef"
  let span_id = "span-98765-43210-fedcba"
  
  // 验证追踪ID格式
  assert_eq(trace_id.length(), 21)
  assert_true(trace_id.contains("trace"))
  assert_true(trace_id.starts_with("trace"))
  
  // 验证Span ID格式
  assert_eq(span_id.length(), 21)
  assert_true(span_id.contains("span"))
  assert_true(span_id.starts_with("span"))
  
  // 创建追踪上下文
  let trace_context = {
    "trace_id": trace_id,
    "span_id": span_id,
    "parent_span_id": Some("span-parent-11111-22222"),
    "trace_flags": "01",
    "trace_state": "vendor1=value1,vendor2=value2"
  }
  
  // 验证追踪上下文
  assert_eq(trace_context["trace_id"], trace_id)
  assert_eq(trace_context["span_id"], span_id)
  match trace_context["parent_span_id"] {
    Some(parent_id) => assert_eq(parent_id, "span-parent-11111-22222")
    None => assert_true(false)
  }
  assert_eq(trace_context["trace_flags"], "01")
  assert_eq(trace_context["trace_state"], "vendor1=value1,vendor2=value2")
}

// 测试2: Span创建和属性设置
test "Span创建和属性设置" {
  // 创建根Span
  let root_span = {
    "trace_id": "trace-12345-67890-abcdef",
    "span_id": "span-root-00000-00000",
    "parent_span_id": None,
    "name": "http.request",
    "kind": "server",
    "start_time": 1640995200000,
    "end_time": None,
    "status": "ok",
    "attributes": {
      "http.method": "GET",
      "http.url": "/api/users",
      "http.status_code": 200,
      "service.name": "api-service"
    },
    "events": [],
    "links": []
  }
  
  // 验证根Span
  assert_eq(root_span["trace_id"], "trace-12345-67890-abcdef")
  assert_eq(root_span["span_id"], "span-root-00000-00000")
  match root_span["parent_span_id"] {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  assert_eq(root_span["name"], "http.request")
  assert_eq(root_span["kind"], "server")
  assert_eq(root_span["start_time"], 1640995200000)
  match root_span["end_time"] {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  assert_eq(root_span["status"], "ok")
  
  // 验证属性
  assert_eq(root_span["attributes"]["http.method"], "GET")
  assert_eq(root_span["attributes"]["http.url"], "/api/users")
  assert_eq(root_span["attributes"]["http.status_code"], 200)
  assert_eq(root_span["attributes"]["service.name"], "api-service")
  
  // 创建子Span
  let child_span = {
    "trace_id": root_span["trace_id"],
    "span_id": "span-child-11111-11111",
    "parent_span_id": Some(root_span["span_id"]),
    "name": "database.query",
    "kind": "client",
    "start_time": 1640995200100,
    "end_time": Some(1640995200200),
    "status": "ok",
    "attributes": {
      "db.system": "postgresql",
      "db.statement": "SELECT * FROM users",
      "db.operation": "SELECT"
    },
    "events": [],
    "links": []
  }
  
  // 验证子Span
  assert_eq(child_span["trace_id"], root_span["trace_id"])
  assert_eq(child_span["span_id"], "span-child-11111-11111")
  match child_span["parent_span_id"] {
    Some(parent_id) => assert_eq(parent_id, root_span["span_id"])
    None => assert_true(false)
  }
  assert_eq(child_span["name"], "database.query")
  assert_eq(child_span["kind"], "client")
  assert_eq(child_span["start_time"], 1640995200100)
  match child_span["end_time"] {
    Some(end_time) => assert_eq(end_time, 1640995200200)
    None => assert_true(false)
  }
  assert_eq(child_span["status"], "ok")
  
  // 验证子Span属性
  assert_eq(child_span["attributes"]["db.system"], "postgresql")
  assert_eq(child_span["attributes"]["db.statement"], "SELECT * FROM users")
  assert_eq(child_span["attributes"]["db.operation"], "SELECT")
}

// 测试3: Span事件记录
test "Span事件记录" {
  // 创建Span
  let span = {
    "trace_id": "trace-12345-67890-abcdef",
    "span_id": "span-event-00000-00000",
    "parent_span_id": None,
    "name": "file.processing",
    "kind": "internal",
    "start_time": 1640995200000,
    "end_time": Some(1640995200500),
    "status": "ok",
    "attributes": {
      "file.name": "data.csv",
      "file.size": 1048576
    },
    "events": [],
    "links": []
  }
  
  // 添加事件
  let events = [
    {
      "name": "file.opened",
      "timestamp": 1640995200100,
      "attributes": {
        "file.path": "/tmp/data.csv",
        "file.mode": "read"
      }
    },
    {
      "name": "file.parsing.started",
      "timestamp": 1640995200200,
      "attributes": {
        "parser.type": "csv",
        "record.count": 1000
      }
    },
    {
      "name": "file.parsing.completed",
      "timestamp": 1640995200400,
      "attributes": {
        "records.processed": 1000,
        "records.failed": 5
      }
    },
    {
      "name": "file.closed",
      "timestamp": 1640995200450,
      "attributes": {
        "file.path": "/tmp/data.csv"
      }
    }
  ]
  
  // 更新Span事件
  let span_with_events = { ...span, "events": events }
  
  // 验证事件
  assert_eq(span_with_events["events"].length(), 4)
  
  // 验证第一个事件
  let first_event = span_with_events["events"][0]
  assert_eq(first_event["name"], "file.opened")
  assert_eq(first_event["timestamp"], 1640995200100)
  assert_eq(first_event["attributes"]["file.path"], "/tmp/data.csv")
  assert_eq(first_event["attributes"]["file.mode"], "read")
  
  // 验证解析完成事件
  let parsing_completed = span_with_events["events"][2]
  assert_eq(parsing_completed["name"], "file.parsing.completed")
  assert_eq(parsing_completed["timestamp"], 1640995200400)
  assert_eq(parsing_completed["attributes"]["records.processed"], 1000)
  assert_eq(parsing_completed["attributes"]["records.failed"], 5)
  
  // 验证事件时间顺序
  for i in 0..=(events.length() - 2) {
    let current = events[i]
    let next = events[i + 1]
    assert_true(current["timestamp"] <= next["timestamp"])
  }
}

// 测试4: Span链接关系
test "Span链接关系" {
  // 创建Span
  let span = {
    "trace_id": "trace-12345-67890-abcdef",
    "span_id": "span-links-00000-00000",
    "parent_span_id": None,
    "name": "workflow.execution",
    "kind": "internal",
    "start_time": 1640995200000,
    "end_time": Some(1640995200300),
    "status": "ok",
    "attributes": {
      "workflow.name": "data.processing",
      "workflow.version": "1.0"
    },
    "events": [],
    "links": []
  }
  
  // 创建链接
  let links = [
    {
      "trace_id": "trace-external-11111-11111",
      "span_id": "span-external-11111-11111",
      "attributes": {
        "link.type": "caused_by",
        "service.name": "external.service"
      }
    },
    {
      "trace_id": "trace-batch-22222-22222",
      "span_id": "span-batch-22222-22222",
      "attributes": {
        "link.type": "part_of",
        "batch.id": "batch-12345"
      }
    }
  ]
  
  // 更新Span链接
  let span_with_links = { ...span, "links": links }
  
  // 验证链接
  assert_eq(span_with_links["links"].length(), 2)
  
  // 验证第一个链接
  let first_link = span_with_links["links"][0]
  assert_eq(first_link["trace_id"], "trace-external-11111-11111")
  assert_eq(first_link["span_id"], "span-external-11111-11111")
  assert_eq(first_link["attributes"]["link.type"], "caused_by")
  assert_eq(first_link["attributes"]["service.name"], "external.service")
  
  // 验证第二个链接
  let second_link = span_with_links["links"][1]
  assert_eq(second_link["trace_id"], "trace-batch-22222-22222")
  assert_eq(second_link["span_id"], "span-batch-22222-22222")
  assert_eq(second_link["attributes"]["link.type"], "part_of")
  assert_eq(second_link["attributes"]["batch.id"], "batch-12345")
}

// 测试5: 追踪树构建
test "追踪树构建" {
  // 创建多个Span
  let spans = [
    {
      "trace_id": "trace-tree-12345-67890",
      "span_id": "span-root-00000-00000",
      "parent_span_id": None,
      "name": "http.request",
      "start_time": 1640995200000,
      "end_time": Some(1640995200500)
    },
    {
      "trace_id": "trace-tree-12345-67890",
      "span_id": "span-child-11111-11111",
      "parent_span_id": Some("span-root-00000-00000"),
      "name": "auth.verify",
      "start_time": 1640995200100,
      "end_time": Some(1640995200150)
    },
    {
      "trace_id": "trace-tree-12345-67890",
      "span_id": "span-child-22222-22222",
      "parent_span_id": Some("span-root-00000-00000"),
      "name": "database.query",
      "start_time": 1640995200160,
      "end_time": Some(1640995200300)
    },
    {
      "trace_id": "trace-tree-12345-67890",
      "span_id": "span-grandchild-33333-33333",
      "parent_span_id": Some("span-child-22222-22222"),
      "name": "db.connection",
      "start_time": 1640995200170,
      "end_time": Some(1640995200180)
    },
    {
      "trace_id": "trace-tree-12345-67890",
      "span_id": "span-child-44444-44444",
      "parent_span_id": Some("span-root-00000-00000"),
      "name": "cache.get",
      "start_time": 1640995200310,
      "end_time": Some(1640995200320)
    }
  ]
  
  // 找到根Span
  let root_spans = spans.filter(fn(span) { 
    match span["parent_span_id"] {
      None => true
      Some(_) => false
    }
  })
  
  // 验证根Span
  assert_eq(root_spans.length(), 1)
  let root_span = root_spans[0]
  assert_eq(root_span["span_id"], "span-root-00000-00000")
  assert_eq(root_span["name"], "http.request")
  
  // 找到根Span的直接子Span
  let direct_children = spans.filter(fn(span) { 
    match span["parent_span_id"] {
      Some(parent_id) => parent_id == root_span["span_id"]
      None => false
    }
  })
  
  // 验证直接子Span
  assert_eq(direct_children.length(), 3)
  let child_names = direct_children.map(fn(span) { span["name"] })
  assert_true(child_names.contains("auth.verify"))
  assert_true(child_names.contains("database.query"))
  assert_true(child_names.contains("cache.get"))
  
  // 找到特定Span的子Span
  let db_query_span = direct_children.filter(fn(span) { 
    span["name"] == "database.query" 
  })[0]
  
  let db_children = spans.filter(fn(span) { 
    match span["parent_span_id"] {
      Some(parent_id) => parent_id == db_query_span["span_id"]
      None => false
    }
  })
  
  // 验证数据库查询的子Span
  assert_eq(db_children.length(), 1)
  assert_eq(db_children[0]["name"], "db.connection")
  assert_eq(db_children[0]["span_id"], "span-grandchild-33333-33333")
}

// 测试6: 追踪上下文传播
test "追踪上下文传播" {
  // 创建原始追踪上下文
  let original_context = {
    "trace_id": "trace-propagation-12345",
    "span_id": "span-origin-00000-00000",
    "parent_span_id": None,
    "trace_flags": "01",
    "trace_state": "vendor1=value1,vendor2=value2",
    "baggage": {
      "user.id": "12345",
      "request.id": "req-67890",
      "session.id": "sess-abcde"
    }
  }
  
  // 序列化追踪上下文为HTTP头
  let traceparent_header = "00-" + original_context["trace_id"] + "-" + 
                          original_context["span_id"] + "-" + 
                          original_context["trace_flags"]
  let tracestate_header = original_context["trace_state"]
  
  // 序列化baggage
  let baggage_items = []
  for (key, value) in original_context["baggage"] {
    baggage_items = baggage_items.push(key + "=" + value)
  }
  let baggage_header = baggage_items.join(",")
  
  // 验证序列化结果
  assert_eq(traceparent_header, "00-trace-propagation-12345-span-origin-00000-00000-01")
  assert_eq(tracestate_header, "vendor1=value1,vendor2=value2")
  assert_true(baggage_header.contains("user.id=12345"))
  assert_true(baggage_header.contains("request.id=req-67890"))
  assert_true(baggage_header.contains("session.id=sess-abcde"))
  
  // 反序列化追踪上下文
  let parsed_traceparent = traceparent_header.split("-")
  let restored_context = {
    "trace_id": parsed_traceparent[1],
    "span_id": parsed_traceparent[2],
    "trace_flags": parsed_traceparent[3],
    "trace_state": tracestate_header,
    "baggage": {}
  }
  
  // 解析baggage
  let baggage_pairs = baggage_header.split(",")
  let restored_baggage = {}
  for pair in baggage_pairs {
    let kv = pair.split("=")
    if kv.length() == 2 {
      restored_baggage = restored_baggage.set(kv[0], kv[1])
    }
  }
  restored_context = restored_context.set("baggage", restored_baggage)
  
  // 验证反序列化结果
  assert_eq(restored_context["trace_id"], original_context["trace_id"])
  assert_eq(restored_context["span_id"], original_context["span_id"])
  assert_eq(restored_context["trace_flags"], original_context["trace_flags"])
  assert_eq(restored_context["trace_state"], original_context["trace_state"])
  assert_eq(restored_context["baggage"]["user.id"], "12345")
  assert_eq(restored_context["baggage"]["request.id"], "req-67890")
  assert_eq(restored_context["baggage"]["session.id"], "sess-abcde")
  
  // 创建新的Span上下文 (继承父上下文)
  let new_span_context = {
    "trace_id": restored_context["trace_id"],
    "span_id": "span-new-11111-11111",
    "parent_span_id": Some(restored_context["span_id"]),
    "trace_flags": restored_context["trace_flags"],
    "trace_state": restored_context["trace_state"],
    "baggage": restored_context["baggage"]
  }
  
  // 验证新Span上下文
  assert_eq(new_span_context["trace_id"], original_context["trace_id"])
  assert_eq(new_span_context["span_id"], "span-new-11111-11111")
  match new_span_context["parent_span_id"] {
    Some(parent_id) => assert_eq(parent_id, original_context["span_id"])
    None => assert_true(false)
  }
  assert_eq(new_span_context["trace_flags"], original_context["trace_flags"])
  assert_eq(new_span_context["trace_state"], original_context["trace_state"])
  assert_eq(new_span_context["baggage"]["user.id"], "12345")
}

// 测试7: 追踪采样决策
test "追踪采样决策" {
  // 定义采样配置
  let sampling_config = {
    "sample_rate": 0.1, // 10%采样率
    "always_sample": ["trace-debug-12345", "trace-error-67890"],
    "never_sample": ["trace-health-11111"],
    "sampling_attributes": ["http.method=GET", "service.name=api-service"]
  }
  
  // 测试用例
  let test_cases = [
    {
      "trace_id": "trace-normal-12345",
      "attributes": {"http.method": "POST", "service.name": "api-service"},
      "expected_sampled": false // 基于采样率的随机决策
    },
    {
      "trace_id": "trace-debug-12345",
      "attributes": {"http.method": "GET", "service.name": "api-service"},
      "expected_sampled": true // 在always_sample列表中
    },
    {
      "trace_id": "trace-health-11111",
      "attributes": {"http.method": "GET", "service.name": "health-service"},
      "expected_sampled": false // 在never_sample列表中
    },
    {
      "trace_id": "trace-api-get-22222",
      "attributes": {"http.method": "GET", "service.name": "api-service"},
      "expected_sampled": true // 匹配采样属性
    }
  ]
  
  // 采样决策函数
  let should_sample = fn(trace_id, attributes) {
    // 检查never_sample列表
    if sampling_config["never_sample"].contains(trace_id) {
      false
    } else if sampling_config["always_sample"].contains(trace_id) {
      true // 检查always_sample列表
    } else {
      // 检查采样属性
      let matches_attributes = sampling_config["sampling_attributes"].all(fn(attr) {
        let kv = attr.split("=")
        if kv.length() == 2 {
          match attributes.get(kv[0]) {
            Some(value) => value == kv[1]
            None => false
          }
        } else {
          false
        }
      })
      
      if matches_attributes {
        true
      } else {
        // 基于采样率的随机决策 (简化版，使用trace_id哈希)
        trace_id.length() % 10 == 0 // 10%的采样率
      }
    }
  }
  
  // 验证采样决策
  for test_case in test_cases {
    let trace_id = test_case["trace_id"]
    let attributes = test_case["attributes"]
    let expected = test_case["expected_sampled"]
    let actual = should_sample(trace_id, attributes)
    
    assert_eq(actual, expected)
  }
  
  // 验证采样配置
  assert_eq(sampling_config["sample_rate"], 0.1)
  assert_eq(sampling_config["always_sample"].length(), 2)
  assert_eq(sampling_config["never_sample"].length(), 1)
  assert_eq(sampling_config["sampling_attributes"].length(), 2)
}