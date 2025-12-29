// 序列化兼容性测试 - 测试数据结构的序列化和反序列化

// 模拟序列化接口
pub trait Serializable {
  serialize() -> String
}

pub trait Deserializable[T] {
  deserialize(data : String) -> T?
}

// AttributeValue的序列化实现
pub fn AttributeValue::serialize(self : AttributeValue) -> String {
  match (self) {
    StringValue(s) => "string:" + s
    IntValue(i) => "int:" + i.to_string()
    FloatValue(f) => "float:" + f.to_string()
    BoolValue(b) => "bool:" + b.to_string()
    ArrayStringValue(arr) => {
      let mut result = "array_string:"
      let mut i = 0
      while i < arr.length {
        result = result + arr[i]
        if i < arr.length - 1 {
          result = result + ","
        }
        i = i + 1
      }
      result
    }
    ArrayIntValue(arr) => {
      let mut result = "array_int:"
      let mut i = 0
      while i < arr.length {
        result = result + arr[i].to_string()
        if i < arr.length - 1 {
          result = result + ","
        }
        i = i + 1
      }
      result
    }
    ArrayFloatValue(arr) => {
      let mut result = "array_float:"
      let mut i = 0
      while i < arr.length {
        result = result + arr[i].to_string()
        if i < arr.length - 1 {
          result = result + ","
        }
        i = i + 1
      }
      result
    }
    ArrayBoolValue(arr) => {
      let mut result = "array_bool:"
      let mut i = 0
      while i < arr.length {
        result = result + arr[i].to_string()
        if i < arr.length - 1 {
          result = result + ","
        }
        i = i + 1
      }
      result
    }
  }
}

pub fn AttributeValue::deserialize(data : String) -> AttributeValue? {
  if data.starts_with("string:") {
    let value = data.substring(7)  // 跳过 "string:"
    Some(StringValue(value))
  } else if data.starts_with("int:") {
    let value_str = data.substring(4)  // 跳过 "int:"
    match (value_str.to_int64()) {
      Some(value) => Some(IntValue(value))
      None => None
    }
  } else if data.starts_with("float:") {
    let value_str = data.substring(6)  // 跳过 "float:"
    match (value_str.to_double()) {
      Some(value) => Some(FloatValue(value))
      None => None
    }
  } else if data.starts_with("bool:") {
    let value_str = data.substring(5)  // 跳过 "bool:"
    Some(BoolValue(value_str == "true"))
  } else if data.starts_with("array_string:") {
    let values_str = data.substring(12)  // 跳过 "array_string:"
    let values = values_str.split(",")
    Some(ArrayStringValue(values))
  } else if data.starts_with("array_int:") {
    let values_str = data.substring(10)  // 跳过 "array_int:"
    let values = values_str.split(",")
    let mut int_values = []
    let mut i = 0
    while i < values.length {
      match (values[i].to_int64()) {
        Some(int_val) => int_values.push(int_val)
        None => return None
      }
      i = i + 1
    }
    Some(ArrayIntValue(int_values))
  } else if data.starts_with("array_float:") {
    let values_str = data.substring(12)  // 跳过 "array_float:"
    let values = values_str.split(",")
    let mut float_values = []
    let mut i = 0
    while i < values.length {
      match (values[i].to_double()) {
        Some(float_val) => float_values.push(float_val)
        None => return None
      }
      i = i + 1
    }
    Some(ArrayFloatValue(float_values))
  } else if data.starts_with("array_bool:") {
    let values_str = data.substring(11)  // 跳过 "array_bool:"
    let values = values_str.split(",")
    let mut bool_values = []
    let mut i = 0
    while i < values.length {
      bool_values.push(values[i] == "true")
      i = i + 1
    }
    Some(ArrayBoolValue(bool_values))
  } else {
    None
  }
}

// Resource的序列化实现
pub fn Resource::serialize(self : Resource) -> String {
  let mut result = "Resource{"
  result = result + "service_name:" + self.service_name + ","
  
  result = result + "service_version:"
  result = result + match (self.service_version) {
    Some(v) => v
    None => ""
  }
  result = result + ","
  
  result = result + "telemetry_sdk_name:" + self.telemetry_sdk_name + ","
  result = result + "telemetry_sdk_version:" + self.telemetry_sdk_version + ","
  
  result = result + "attributes:["
  let mut i = 0
  while i < self.attributes.length {
    let (key, value) = self.attributes[i]
    result = result + key + "=" + value.serialize()
    if i < self.attributes.length - 1 {
      result = result + ";"
    }
    i = i + 1
  }
  result = result + "]}"
  
  result
}

pub fn Resource::deserialize(data : String) -> Resource? {
  // 简化的反序列化实现
  if !data.starts_with("Resource{") || !data.ends_with("}") {
    return None
  }
  
  // 提取服务名
  let service_name_start = data.find("service_name:") + 13
  let service_name_end = data.find(",", service_name_start)
  let service_name = data.substring(service_name_start, service_name_end - service_name_start)
  
  // 提取SDK信息
  let sdk_name_start = data.find("telemetry_sdk_name:") + 19
  let sdk_name_end = data.find(",", sdk_name_start)
  let sdk_name = data.substring(sdk_name_start, sdk_name_end - sdk_name_start)
  
  let sdk_version_start = data.find("telemetry_sdk_version:") + 22
  let sdk_version_end = data.find(",", sdk_version_start)
  let sdk_version = data.substring(sdk_version_start, sdk_version_end - sdk_version_start)
  
  Some(Resource::{
    service_name: service_name,
    service_version: None,  // 简化实现
    telemetry_sdk_name: sdk_name,
    telemetry_sdk_version: sdk_version,
    attributes: []  // 简化实现
  })
}

// Context的序列化实现
pub fn Context::serialize(self : Context) -> String {
  let mut result = "Context{"
  let mut i = 0
  while i < self.values.length {
    let (key, value) = self.values[i]
    result = result + key + "=" + value
    if i < self.values.length - 1 {
      result = result + ";"
    }
    i = i + 1
  }
  result = result + "}"
  
  result
}

pub fn Context::deserialize(data : String) -> Context? {
  if !data.starts_with("Context{") || !data.ends_with("}") {
    return None
  }
  
  let content = data.substring(7, data.length() - 8)  // 移除 "Context{" 和 "}"
  
  if content == "" {
    return Some(Context::{ values: [] })
  }
  
  let pairs = content.split(";")
  let mut values = []
  
  let mut i = 0
  while i < pairs.length {
    let pair = pairs[i]
    let eq_pos = pair.find("=")
    if eq_pos > 0 {
      let key = pair.substring(0, eq_pos)
      let value = pair.substring(eq_pos + 1)
      values.push((key, value))
    }
    i = i + 1
  }
  
  Some(Context::{ values: values })
}

// Baggage的序列化实现
pub fn Baggage::serialize(self : Baggage) -> String {
  let mut result = "Baggage{"
  let mut i = 0
  while i < self.entries.length {
    let (key, value) = self.entries[i]
    result = result + key + "=" + value
    if i < self.entries.length - 1 {
      result = result + ";"
    }
    i = i + 1
  }
  result = result + "}"
  
  result
}

pub fn Baggage::deserialize(data : String) -> Baggage? {
  if !data.starts_with("Baggage{") || !data.ends_with("}") {
    return None
  }
  
  let content = data.substring(7, data.length() - 8)  // 移除 "Baggage{" 和 "}"
  
  if content == "" {
    return Some(Baggage::{ entries: [] })
  }
  
  let pairs = content.split(";")
  let mut entries = []
  
  let mut i = 0
  while i < pairs.length {
    let pair = pairs[i]
    let eq_pos = pair.find("=")
    if eq_pos > 0 {
      let key = pair.substring(0, eq_pos)
      let value = pair.substring(eq_pos + 1)
      entries.push((key, value))
    }
    i = i + 1
  }
  
  Some(Baggage::{ entries: entries })
}

// 测试用例
test "serialization compatibility: attribute value roundtrip" {
  // 测试AttributeValue的序列化/反序列化往返
  let original_values = [
    StringValue("test_string"),
    IntValue(12345L),
    FloatValue(3.14159),
    BoolValue(true),
    ArrayStringValue(["a", "b", "c"]),
    ArrayIntValue([1L, 2L, 3L]),
    ArrayFloatValue([1.1, 2.2, 3.3]),
    ArrayBoolValue([true, false, true])
  ]
  
  let mut i = 0
  while i < original_values.length {
    let original = original_values[i]
    
    // 序列化
    let serialized = original.serialize()
    
    // 反序列化
    let deserialized = AttributeValue::deserialize(serialized)
    
    // 验证往返一致性
    assert match (deserialized) {
      Some(value) => {
        match (original, value) {
          (StringValue(s1), StringValue(s2)) => s1 == s2
          (IntValue(i1), IntValue(i2)) => i1 == i2
          (FloatValue(f1), FloatValue(f2)) => f1 == f2
          (BoolValue(b1), BoolValue(b2)) => b1 == b2
          (ArrayStringValue(arr1), ArrayStringValue(arr2)) => {
            if arr1.length != arr2.length {
              false
            } else {
              let mut j = 0
              let mut equal = true
              while j < arr1.length {
                if arr1[j] != arr2[j] {
                  equal = false
                }
                j = j + 1
              }
              equal
            }
          }
          (ArrayIntValue(arr1), ArrayIntValue(arr2)) => {
            if arr1.length != arr2.length {
              false
            } else {
              let mut j = 0
              let mut equal = true
              while j < arr1.length {
                if arr1[j] != arr2[j] {
                  equal = false
                }
                j = j + 1
              }
              equal
            }
          }
          (ArrayFloatValue(arr1), ArrayFloatValue(arr2)) => {
            if arr1.length != arr2.length {
              false
            } else {
              let mut j = 0
              let mut equal = true
              while j < arr1.length {
                if arr1[j] != arr2[j] {
                  equal = false
                }
                j = j + 1
              }
              equal
            }
          }
          (ArrayBoolValue(arr1), ArrayBoolValue(arr2)) => {
            if arr1.length != arr2.length {
              false
            } else {
              let mut j = 0
              let mut equal = true
              while j < arr1.length {
                if arr1[j] != arr2[j] {
                  equal = false
                }
                j = j + 1
              }
              equal
            }
          }
          _ => false
        }
      }
      None => false
    }
    
    i = i + 1
  }
}

test "serialization compatibility: resource roundtrip" {
  // 测试Resource的序列化/反序列化往返
  let original_resource = Resource::{
    service_name: "serialization_test_service",
    service_version: Some("1.2.3"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("env", AttributeValue::string("test")),
      ("region", AttributeValue::string("us-west-2")),
      ("instance.id", AttributeValue::string("instance-123")),
      ("process.id", AttributeValue::int(456L))
    ]
  }
  
  // 序列化
  let serialized = original_resource.serialize()
  
  // 验证序列化格式
  assert serialized.starts_with("Resource{")
  assert serialized.ends_with("}")
  assert serialized.contains("service_name:serialization_test_service")
  assert serialized.contains("telemetry_sdk_name:azimuth")
  
  // 反序列化
  let deserialized = Resource::deserialize(serialized)
  
  // 验证往返一致性
  assert match (deserialized) {
    Some(resource) => {
      resource.service_name == original_resource.service_name &&
      resource.telemetry_sdk_name == original_resource.telemetry_sdk_name &&
      resource.telemetry_sdk_version == original_resource.telemetry_sdk_version
    }
    None => false
  }
}

test "serialization compatibility: context roundtrip" {
  // 测试Context的序列化/反序列化往返
  let original_context = Context::empty()
    .with_value(create_key("key1"), "value1")
    .with_value(create_key("key2"), "value2")
    .with_value(create_key("key3"), "value3")
  
  // 序列化
  let serialized = original_context.serialize()
  
  // 验证序列化格式
  assert serialized.starts_with("Context{")
  assert serialized.ends_with("}")
  assert serialized.contains("key1=value1")
  assert serialized.contains("key2=value2")
  assert serialized.contains("key3=value3")
  
  // 反序列化
  let deserialized = Context::deserialize(serialized)
  
  // 验证往返一致性
  assert match (deserialized) {
    Some(context) => {
      match (context.get(create_key("key1"))) {
        Some(value) => value == "value1"
        None => false
      } &&
      match (context.get(create_key("key2"))) {
        Some(value) => value == "value2"
        None => false
      } &&
      match (context.get(create_key("key3"))) {
        Some(value) => value == "value3"
        None => false
      }
    }
    None => false
  }
}

test "serialization compatibility: baggage roundtrip" {
  // 测试Baggage的序列化/反序列化往返
  let original_baggage = Baggage::empty()
    .with_entry("user.id", "user-123")
    .with_entry("session.id", "session-456")
    .with_entry("request.id", "req-789")
    .with_entry("trace.id", "trace-abc")
  
  // 序列化
  let serialized = original_baggage.serialize()
  
  // 验证序列化格式
  assert serialized.starts_with("Baggage{")
  assert serialized.ends_with("}")
  assert serialized.contains("user.id=user-123")
  assert serialized.contains("session.id=session-456")
  assert serialized.contains("request.id=req-789")
  assert serialized.contains("trace.id=trace-abc")
  
  // 反序列化
  let deserialized = Baggage::deserialize(serialized)
  
  // 验证往返一致性
  assert match (deserialized) {
    Some(baggage) => {
      match (baggage.get("user.id")) {
        Some(value) => value == "user-123"
        None => false
      } &&
      match (baggage.get("session.id")) {
        Some(value) => value == "session-456"
        None => false
      } &&
      match (baggage.get("request.id")) {
        Some(value) => value == "req-789"
        None => false
      } &&
      match (baggage.get("trace.id")) {
        Some(value) => value == "trace-abc"
        None => false
      }
    }
    None => false
  }
}

test "serialization compatibility: cross format conversion" {
  // 测试跨格式转换兼容性
  let resource = Resource::default("cross_format_service")
  
  // Resource -> Context -> Baggage -> Resource
  let context = Context::empty()
    .with_value(create_key("service.name"), resource.service_name)
    .with_value(create_key("sdk.name"), resource.telemetry_sdk_name)
    .with_value(create_key("sdk.version"), resource.telemetry_sdk_version)
  
  let baggage = Baggage::empty()
    .with_entry("service.name", resource.service_name)
    .with_entry("sdk.name", resource.telemetry_sdk_name)
    .with_entry("sdk.version", resource.telemetry_sdk_version)
  
  // 序列化所有格式
  let resource_serialized = resource.serialize()
  let context_serialized = context.serialize()
  let baggage_serialized = baggage.serialize()
  
  // 验证所有序列化都包含关键信息
  assert resource_serialized.contains("cross_format_service")
  assert context_serialized.contains("cross_format_service")
  assert baggage_serialized.contains("cross_format_service")
  
  assert resource_serialized.contains("azimuth")
  assert context_serialized.contains("azimuth")
  assert baggage_serialized.contains("azimuth")
  
  // 反序列化并验证信息一致性
  let resource_deserialized = Resource::deserialize(resource_serialized)
  let context_deserialized = Context::deserialize(context_serialized)
  let baggage_deserialized = Baggage::deserialize(baggage_serialized)
  
  // 验证服务名在所有格式中保持一致
  assert match (resource_deserialized) {
    Some(r) => r.service_name == "cross_format_service"
    None => false
  }
  
  assert match (context_deserialized) {
    Some(c) => match (c.get(create_key("service.name"))) {
      Some(name) => name == "cross_format_service"
      None => false
    }
    None => false
  }
  
  assert match (baggage_deserialized) {
    Some(b) => match (b.get("service.name")) {
      Some(name) => name == "cross_format_service"
      None => false
    }
    None => false
  }
}

test "serialization compatibility: empty and special cases" {
  // 测试空和特殊情况
  let empty_context = Context::empty()
  let empty_baggage = Baggage::empty()
  
  // 序列化空对象
  let empty_context_serialized = empty_context.serialize()
  let empty_baggage_serialized = empty_baggage.serialize()
  
  assert empty_context_serialized == "Context{}"
  assert empty_baggage_serialized == "Baggage{}"
  
  // 反序列化空对象
  let empty_context_deserialized = Context::deserialize(empty_context_serialized)
  let empty_baggage_deserialized = Baggage::deserialize(empty_baggage_serialized)
  
  assert match (empty_context_deserialized) {
    Some(c) => c.values.length == 0
    None => false
  }
  
  assert match (empty_baggage_deserialized) {
    Some(b) => b.entries.length == 0
    None => false
  }
  
  // 测试单元素情况
  let single_context = Context::empty().with_value(create_key("single"), "value")
  let single_baggage = Baggage::empty().with_entry("single", "value")
  
  let single_context_serialized = single_context.serialize()
  let single_baggage_serialized = single_baggage.serialize()
  
  assert single_context_serialized == "Context{single=value}"
  assert single_baggage_serialized == "Baggage{single=value}"
  
  // 测试特殊字符
  let special_context = Context::empty()
    .with_value(create_key("special"), "!@#$%^&*()")
    .with_value(create_key("unicode"), "测试")
    .with_value(create_key("empty"), "")
  
  let special_baggage = Baggage::empty()
    .with_entry("special", "!@#$%^&*()")
    .with_entry("unicode", "测试")
    .with_entry("empty", "")
  
  let special_context_serialized = special_context.serialize()
  let special_baggage_serialized = special_baggage.serialize()
  
  assert special_context_serialized.contains("!@#$%^&*()")
  assert special_context_serialized.contains("测试")
  assert special_context_serialized.contains("empty=")
  
  assert special_baggage_serialized.contains("!@#$%^&*()")
  assert special_baggage_serialized.contains("测试")
  assert special_baggage_serialized.contains("empty=")
}

test "serialization compatibility: error handling" {
  // 测试错误处理
  // 无效的序列化数据
  let invalid_resource_data = "InvalidResource"
  let invalid_context_data = "InvalidContext"
  let invalid_baggage_data = "InvalidBaggage"
  
  // 反序列化应该返回None
  assert Resource::deserialize(invalid_resource_data) == None
  assert Context::deserialize(invalid_context_data) == None
  assert Baggage::deserialize(invalid_baggage_data) == None
  
  // 不完整的序列化数据
  let incomplete_resource_data = "Resource{service_name:test"
  let incomplete_context_data = "Context{key1=value1"
  let incomplete_baggage_data = "Baggage{key1=value1"
  
  // 反序列化应该返回None
  assert Resource::deserialize(incomplete_resource_data) == None
  assert Context::deserialize(incomplete_context_data) == None
  assert Baggage::deserialize(incomplete_baggage_data) == None
  
  // 无效的AttributeValue数据
  let invalid_attribute_data = "invalid_type:value"
  let invalid_int_data = "int:not_a_number"
  let invalid_float_data = "float:not_a_float"
  
  // 反序列化应该返回None
  assert AttributeValue::deserialize(invalid_attribute_data) == None
  assert AttributeValue::deserialize(invalid_int_data) == None
  assert AttributeValue::deserialize(invalid_float_data) == None
  
  // 空数据
  assert AttributeValue::deserialize("") == None
  assert Resource::deserialize("") == None
  assert Context::deserialize("") == None
  assert Baggage::deserialize("") == None
}