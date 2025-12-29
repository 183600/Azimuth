// 数据一致性测试 - 测试数据在不同模块间传递的一致性

test "data consistency: resource to context propagation" {
  // 测试Resource数据传播到Context的一致性
  let resource = Resource::{
    service_name: "consistency_test_service",
    service_version: Some("1.2.3"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.instance.id", AttributeValue::string("instance-123")),
      ("deployment.environment", AttributeValue::string("production")),
      ("host.name", AttributeValue::string("host-abc")),
      ("process.id", AttributeValue::int(456L))
    ]
  }
  
  // 将Resource信息传播到Context
  let context = Context::empty()
  let service_name_key = create_key("service.name")
  let service_version_key = create_key("service.version")
  let instance_id_key = create_key("service.instance.id")
  let environment_key = create_key("deployment.environment")
  
  let propagated_context = context
    .with_value(service_name_key, resource.service_name)
    .with_value(service_version_key, match (resource.service_version) {
      Some(v) => v
      None => "unknown"
    })
    .with_value(instance_id_key, match (resource.attributes[0].1) {
      StringValue(id) => id
      _ => "unknown"
    })
    .with_value(environment_key, match (resource.attributes[1].1) {
      StringValue(env) => env
      _ => "unknown"
    })
  
  // 验证数据一致性
  assert match (propagated_context.get(service_name_key)) {
    Some(value) => value == resource.service_name
    None => false
  }
  
  assert match (propagated_context.get(service_version_key)) {
    Some(value) => value == "1.2.3"
    None => false
  }
  
  assert match (propagated_context.get(instance_id_key)) {
    Some(value) => value == "instance-123"
    None => false
  }
  
  assert match (propagated_context.get(environment_key)) {
    Some(value) => value == "production"
    None => false
  }
  
  // 验证从Context重构Resource的数据一致性
  let reconstructed_service_name = match (propagated_context.get(service_name_key)) {
    Some(name) => name
    None => "unknown"
  }
  
  let reconstructed_service_version = match (propagated_context.get(service_version_key)) {
    Some(version) => Some(version)
    None => None
  }
  
  assert reconstructed_service_name == resource.service_name
  assert match (reconstructed_service_version) {
    Some(v) => v == "1.2.3"
    None => false
  }
}

test "data consistency: baggage to metrics attributes" {
  // 测试Baggage数据转换为Metrics Attributes的一致性
  let baggage = Baggage::empty()
    .with_entry("user.id", "user-789")
    .with_entry("session.id", "session-456")
    .with_entry("request.id", "req-123")
    .with_entry("trace.id", "trace-abc")
    .with_entry("span.id", "span-def")
  
  // 将Baggage转换为Metrics Attributes
  let mut attributes = []
  
  // 模拟转换过程
  let baggage_entries = [
    ("user.id", "user.id"),
    ("session.id", "session.id"),
    ("request.id", "request.id"),
    ("trace.id", "trace.id"),
    ("span.id", "span.id")
  ]
  
  let mut i = 0
  while i < baggage_entries.length {
    let (baggage_key, attr_key) = baggage_entries[i]
    match (baggage.get(baggage_key)) {
      Some(value) => attributes.push((attr_key, AttributeValue::string(value)))
      None => ()
    }
    i = i + 1
  }
  
  // 验证转换的一致性
  assert attributes.length == 5
  
  // 验证每个属性值
  let mut found_user_id = false
  let mut found_session_id = false
  let mut found_request_id = false
  let mut found_trace_id = false
  let mut found_span_id = false
  
  let mut j = 0
  while j < attributes.length {
    let (key, value) = attributes[j]
    match (key) {
      "user.id" => {
        assert match (value) {
          StringValue(id) => id == "user-789"
          _ => false
        }
        found_user_id = true
      }
      "session.id" => {
        assert match (value) {
          StringValue(id) => id == "session-456"
          _ => false
        }
        found_session_id = true
      }
      "request.id" => {
        assert match (value) {
          StringValue(id) => id == "req-123"
          _ => false
        }
        found_request_id = true
      }
      "trace.id" => {
        assert match (value) {
          StringValue(id) => id == "trace-abc"
          _ => false
        }
        found_trace_id = true
      }
      "span.id" => {
        assert match (value) {
          StringValue(id) => id == "span-def"
          _ => false
        }
        found_span_id = true
      }
      _ => ()
    }
    j = j + 1
  }
  
  assert found_user_id
  assert found_session_id
  assert found_request_id
  assert found_trace_id
  assert found_span_id
  
  // 验证从Attributes重构Baggage的一致性
  let mut reconstructed_baggage = Baggage::empty()
  let mut k = 0
  while k < attributes.length {
    let (key, value) = attributes[k]
    match (value) {
      StringValue(s) => reconstructed_baggage = reconstructed_baggage.with_entry(key, s)
      _ => ()
    }
    k = k + 1
  }
  
  // 验证重构的Baggage与原始Baggage的一致性
  assert match (reconstructed_baggage.get("user.id")) {
    Some(value) => value == "user-789"
    None => false
  }
  assert match (reconstructed_baggage.get("session.id")) {
    Some(value) => value == "session-456"
    None => false
  }
}

test "data consistency: instrumentation scope across modules" {
  // 测试InstrumentationScope在不同模块间传递的一致性
  let scope = InstrumentationScope::{
    name: "consistency_scope",
    version: Some("2.1.0"),
    schema_url: Some("https://example.com/consistency-schema")
  }
  
  // 将Scope信息传播到Resource
  let scope_attributes = [
    ("scope.name", AttributeValue::string(scope.name)),
    ("scope.version", match (scope.version) {
      Some(v) => AttributeValue::string(v)
      None => AttributeValue::string("")
    }),
    ("scope.schema", match (scope.schema_url) {
      Some(url) => AttributeValue::string(url)
      None => AttributeValue::string("")
    })
  ]
  
  let resource_with_scope = Resource::{
    service_name: "scope_consistency_service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: scope_attributes
  }
  
  // 将Scope信息传播到Context
  let context = Context::empty()
  let scope_name_key = create_key("instrumentation.scope.name")
  let scope_version_key = create_key("instrumentation.scope.version")
  let scope_schema_key = create_key("instrumentation.scope.schema")
  
  let context_with_scope = context
    .with_value(scope_name_key, scope.name)
    .with_value(scope_version_key, match (scope.version) {
      Some(v) => v
      None => ""
    })
    .with_value(scope_schema_key, match (scope.schema_url) {
      Some(url) => url
      None => ""
    })
  
  // 验证跨模块的一致性
  // 从Resource中提取Scope信息
  let mut resource_scope_name = ""
  let mut resource_scope_version = ""
  let mut resource_scope_schema = ""
  
  let mut i = 0
  while i < resource_with_scope.attributes.length {
    let (key, value) = resource_with_scope.attributes[i]
    match (key) {
      "scope.name" => {
        assert match (value) {
          StringValue(name) => {
            resource_scope_name = name
            name == scope.name
          }
          _ => false
        }
      }
      "scope.version" => {
        assert match (value) {
          StringValue(version) => {
            resource_scope_version = version
            version == "2.1.0"
          }
          _ => false
        }
      }
      "scope.schema" => {
        assert match (value) {
          StringValue(schema) => {
            resource_scope_schema = schema
            schema == "https://example.com/consistency-schema"
          }
          _ => false
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  // 从Context中提取Scope信息
  let context_scope_name = match (context_with_scope.get(scope_name_key)) {
    Some(name) => name
    None => ""
  }
  
  let context_scope_version = match (context_with_scope.get(scope_version_key)) {
    Some(version) => version
    None => ""
  }
  
  let context_scope_schema = match (context_with_scope.get(scope_schema_key)) {
    Some(schema) => schema
    None => ""
  }
  
  // 验证三种表示方式的一致性
  assert resource_scope_name == scope.name
  assert resource_scope_version == "2.1.0"
  assert resource_scope_schema == "https://example.com/consistency-schema"
  
  assert context_scope_name == scope.name
  assert context_scope_version == "2.1.0"
  assert context_scope_schema == "https://example.com/consistency-schema"
  
  assert resource_scope_name == context_scope_name
  assert resource_scope_version == context_scope_version
  assert resource_scope_schema == context_scope_schema
}

test "data consistency: complex attribute type conversion" {
  // 测试复杂属性类型在不同模块间转换的一致性
  let complex_attributes = [
    ("string.attr", AttributeValue::string("test_string")),
    ("int.attr", AttributeValue::int(12345L)),
    ("float.attr", AttributeValue::float(3.14159)),
    ("bool.attr", AttributeValue::bool(true)),
    ("string_array.attr", AttributeValue::array_string(["a", "b", "c"])),
    ("int_array.attr", AttributeValue::array_int([1L, 2L, 3L])),
    ("float_array.attr", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool_array.attr", AttributeValue::array_bool([true, false, true]))
  ]
  
  // 创建包含复杂属性的Resource
  let resource = Resource::{
    service_name: "complex_attr_service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: complex_attributes
  }
  
  // 将Resource属性转换为Context字符串表示
  let context = Context::empty()
  let mut context_with_attrs = context
  
  let mut i = 0
  while i < resource.attributes.length {
    let (key, value) = resource.attributes[i]
    let context_key = create_key(key)
    
    let string_value = match (value) {
      StringValue(s) => s
      IntValue(i) => i.to_string()
      FloatValue(f) => f.to_string()
      BoolValue(b) => b.to_string()
      ArrayStringValue(arr) => {
        let mut result = "["
        let mut j = 0
        while j < arr.length {
          result = result + arr[j]
          if j < arr.length - 1 {
            result = result + ","
          }
          j = j + 1
        }
        result = result + "]"
        result
      }
      ArrayIntValue(arr) => {
        let mut result = "["
        let mut j = 0
        while j < arr.length {
          result = result + arr[j].to_string()
          if j < arr.length - 1 {
            result = result + ","
          }
          j = j + 1
        }
        result = result + "]"
        result
      }
      ArrayFloatValue(arr) => {
        let mut result = "["
        let mut j = 0
        while j < arr.length {
          result = result + arr[j].to_string()
          if j < arr.length - 1 {
            result = result + ","
          }
          j = j + 1
        }
        result = result + "]"
        result
      }
      ArrayBoolValue(arr) => {
        let mut result = "["
        let mut j = 0
        while j < arr.length {
          result = result + arr[j].to_string()
          if j < arr.length - 1 {
            result = result + ","
          }
          j = j + 1
        }
        result = result + "]"
        result
      }
    }
    
    context_with_attrs = context_with_attrs.with_value(context_key, string_value)
    i = i + 1
  }
  
  // 验证转换的一致性
  let mut j = 0
  while j < resource.attributes.length {
    let (key, original_value) = resource.attributes[j]
    let context_key = create_key(key)
    
    let context_value = match (context_with_attrs.get(context_key)) {
      Some(value) => value
      None => ""
    }
    
    // 验证字符串表示的正确性
    match (original_value) {
      StringValue(s) => assert context_value == s
      IntValue(i) => assert context_value == i.to_string()
      FloatValue(f) => assert context_value == f.to_string()
      BoolValue(b) => assert context_value == b.to_string()
      ArrayStringValue(arr) => {
        assert context_value.starts_with("[")
        assert context_value.ends_with("]")
        assert context_value.contains("a")
        assert context_value.contains("b")
        assert context_value.contains("c")
      }
      ArrayIntValue(arr) => {
        assert context_value.starts_with("[")
        assert context_value.ends_with("]")
        assert context_value.contains("1")
        assert context_value.contains("2")
        assert context_value.contains("3")
      }
      ArrayFloatValue(arr) => {
        assert context_value.starts_with("[")
        assert context_value.ends_with("]")
        assert context_value.contains("1.1")
        assert context_value.contains("2.2")
        assert context_value.contains("3.3")
      }
      ArrayBoolValue(arr) => {
        assert context_value.starts_with("[")
        assert context_value.ends_with("]")
        assert context_value.contains("true")
        assert context_value.contains("false")
      }
    }
    
    j = j + 1
  }
}

test "data consistency: round trip conversion" {
  // 测试往返转换的一致性
  let original_resource = Resource::{
    service_name: "round_trip_service",
    service_version: Some("3.2.1"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.type", AttributeValue::string("web")),
      ("service.namespace", AttributeValue::string("production")),
      ("service.instance.id", AttributeValue::string("instance-xyz")),
      ("deployment.region", AttributeValue::string("us-west-2")),
      ("host.name", AttributeValue::string("web-server-01")),
      ("process.pid", AttributeValue::int(1234L)),
      ("process.executable.name", AttributeValue::string("app")),
      ("process.command_args", AttributeValue::array_string(["--port", "8080", "--env", "prod"]))
    ]
  }
  
  // Resource -> Context -> Baggage -> Attributes -> Resource
  // 步骤1: Resource -> Context
  let context = Context::empty()
  let mut intermediate_context = context
  
  let mut i = 0
  while i < original_resource.attributes.length {
    let (key, value) = original_resource.attributes[i]
    let context_key = create_key("resource." + key)
    
    let string_value = match (value) {
      StringValue(s) => s
      IntValue(int_val) => int_val.to_string()
      FloatValue(float_val) => float_val.to_string()
      BoolValue(bool_val) => bool_val.to_string()
      ArrayStringValue(arr) => {
        let mut result = ""
        let mut j = 0
        while j < arr.length {
          result = result + arr[j]
          if j < arr.length - 1 {
            result = result + "|"
          }
          j = j + 1
        }
        result
      }
      ArrayIntValue(arr) => {
        let mut result = ""
        let mut j = 0
        while j < arr.length {
          result = result + arr[j].to_string()
          if j < arr.length - 1 {
            result = result + "|"
          }
          j = j + 1
        }
        result
      }
      ArrayFloatValue(arr) => {
        let mut result = ""
        let mut j = 0
        while j < arr.length {
          result = result + arr[j].to_string()
          if j < arr.length - 1 {
            result = result + "|"
          }
          j = j + 1
        }
        result
      }
      ArrayBoolValue(arr) => {
        let mut result = ""
        let mut j = 0
        while j < arr.length {
          result = result + arr[j].to_string()
          if j < arr.length - 1 {
            result = result + "|"
          }
          j = j + 1
        }
        result
      }
    }
    
    intermediate_context = intermediate_context.with_value(context_key, string_value)
    i = i + 1
  }
  
  // 步骤2: Context -> Baggage
  let baggage = Baggage::empty()
  let mut intermediate_baggage = baggage
  
  let mut j = 0
  while j < original_resource.attributes.length {
    let (key, _) = original_resource.attributes[j]
    let context_key = create_key("resource." + key)
    
    match (intermediate_context.get(context_key)) {
      Some(value) => {
        intermediate_baggage = intermediate_baggage.with_entry(key, value)
      }
      None => ()
    }
    j = j + 1
  }
  
  // 步骤3: Baggage -> Attributes
  let mut reconstructed_attributes = []
  
  let mut k = 0
  while k < original_resource.attributes.length {
    let (key, original_value) = original_resource.attributes[k]
    
    match (intermediate_baggage.get(key)) {
      Some(baggage_value) => {
        let reconstructed_value = match (original_value) {
          StringValue(_) => AttributeValue::string(baggage_value)
          IntValue(_) => {
            match (baggage_value.to_int64()) {
              Some(int_val) => AttributeValue::int(int_val)
              None => AttributeValue::int(0L)
            }
          }
          FloatValue(_) => {
            match (baggage_value.to_double()) {
              Some(float_val) => AttributeValue::float(float_val)
              None => AttributeValue::float(0.0)
            }
          }
          BoolValue(_) => {
            AttributeValue::bool(baggage_value == "true")
          }
          ArrayStringValue(_) => {
            let parts = baggage_value.split("|")
            AttributeValue::array_string(parts)
          }
          ArrayIntValue(_) => {
            let parts = baggage_value.split("|")
            let mut int_parts = []
            let mut m = 0
            while m < parts.length {
              match (parts[m].to_int64()) {
                Some(int_val) => int_parts.push(int_val)
                None => ()
              }
              m = m + 1
            }
            AttributeValue::array_int(int_parts)
          }
          ArrayFloatValue(_) => {
            let parts = baggage_value.split("|")
            let mut float_parts = []
            let mut m = 0
            while m < parts.length {
              match (parts[m].to_double()) {
                Some(float_val) => float_parts.push(float_val)
                None => ()
              }
              m = m + 1
            }
            AttributeValue::array_float(float_parts)
          }
          ArrayBoolValue(_) => {
            let parts = baggage_value.split("|")
            let mut bool_parts = []
            let mut m = 0
            while m < parts.length {
              bool_parts.push(parts[m] == "true")
              m = m + 1
            }
            AttributeValue::array_bool(bool_parts)
          }
        }
        
        reconstructed_attributes.push((key, reconstructed_value))
      }
      None => ()
    }
    k = k + 1
  }
  
  // 步骤4: Attributes -> Resource
  let reconstructed_resource = Resource::{
    service_name: original_resource.service_name,
    service_version: original_resource.service_version,
    telemetry_sdk_name: original_resource.telemetry_sdk_name,
    telemetry_sdk_version: original_resource.telemetry_sdk_version,
    attributes: reconstructed_attributes
  }
  
  // 验证往返转换的一致性
  assert reconstructed_resource.service_name == original_resource.service_name
  assert reconstructed_resource.service_version == original_resource.service_version
  assert reconstructed_resource.telemetry_sdk_name == original_resource.telemetry_sdk_name
  assert reconstructed_resource.telemetry_sdk_version == original_resource.telemetry_sdk_version
  assert reconstructed_resource.attributes.length == original_resource.attributes.length
  
  // 验证关键属性的一致性
  let mut found_service_type = false
  let mut found_instance_id = false
  let mut found_pid = false
  let mut found_args = false
  
  let mut l = 0
  while l < reconstructed_resource.attributes.length {
    let (key, value) = reconstructed_resource.attributes[l]
    match (key) {
      "service.type" => {
        assert match (value) {
          StringValue(s) => s == "web"
          _ => false
        }
        found_service_type = true
      }
      "service.instance.id" => {
        assert match (value) {
          StringValue(s) => s == "instance-xyz"
          _ => false
        }
        found_instance_id = true
      }
      "process.pid" => {
        assert match (value) {
          IntValue(pid) => pid == 1234L
          _ => false
        }
        found_pid = true
      }
      "process.command_args" => {
        assert match (value) {
          ArrayStringValue(args) => {
            args.length == 2 && args[0] == "--port" && args[1] == "--env"
          }
          _ => false
        }
        found_args = true
      }
      _ => ()
    }
    l = l + 1
  }
  
  assert found_service_type
  assert found_instance_id
  assert found_pid
  assert found_args
}