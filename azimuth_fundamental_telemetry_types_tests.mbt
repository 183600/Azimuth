// Azimuth 基础遥测类型测试
// 测试遥测系统的核心数据类型和基本操作

// 测试1: AttributeValue 类型创建和转换
test "AttributeValue类型创建和转换测试" {
  // 测试字符串属性值
  let string_attr = AttributeValue::StringValue("test_value")
  match string_attr {
    AttributeValue::StringValue(s) => assert_eq(s, "test_value")
    _ => assert_true(false, "期望字符串属性值")
  }
  
  // 测试整数属性值
  let int_attr = AttributeValue::IntValue(42)
  match int_attr {
    AttributeValue::IntValue(i) => assert_eq(i, 42)
    _ => assert_true(false, "期望整数属性值")
  }
  
  // 测试浮点属性值
  let float_attr = AttributeValue::FloatValue(3.14)
  match float_attr {
    AttributeValue::FloatValue(f) => assert_eq(f, 3.14)
    _ => assert_true(false, "期望浮点属性值")
  }
  
  // 测试布尔属性值
  let bool_attr = AttributeValue::BoolValue(true)
  match bool_attr {
    AttributeValue::BoolValue(b) => assert_true(b)
    _ => assert_true(false, "期望布尔属性值")
  }
  
  // 测试字符串数组属性值
  let string_array_attr = AttributeValue::ArrayStringValue(["a", "b", "c"])
  match string_array_attr {
    AttributeValue::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => assert_true(false, "期望字符串数组属性值")
  }
  
  // 测试整数数组属性值
  let int_array_attr = AttributeValue::ArrayIntValue([1, 2, 3])
  match int_array_attr {
    AttributeValue::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1)
      assert_eq(arr[1], 2)
      assert_eq(arr[2], 3)
    }
    _ => assert_true(false, "期望整数数组属性值")
  }
}

// 测试2: Attributes 结构操作
test "Attributes结构操作测试" {
  // 创建空属性集合
  let empty_attrs = Attributes { values: [] }
  assert_eq(empty_attrs.values.length(), 0)
  
  // 创建带有属性的集合
  let attrs = Attributes { 
    values: [
      ("key1", AttributeValue::StringValue("value1")),
      ("key2", AttributeValue::IntValue(100)),
      ("key3", AttributeValue::BoolValue(true))
    ]
  }
  
  assert_eq(attrs.values.length(), 3)
  
  // 验证属性值
  let find_attr = fn(key: String, attrs: Attributes) {
    let rec_find = fn(i: Int) {
      if i >= attrs.values.length() {
        Option::None
      } else {
        let (k, v) = attrs.values[i]
        if k == key {
          Option::Some(v)
        } else {
          rec_find(i + 1)
        }
      }
    }
    rec_find(0)
  }
  
  match find_attr("key1", attrs) {
    Option::Some(AttributeValue::StringValue(v)) => assert_eq(v, "value1")
    _ => assert_true(false, "期望找到key1的字符串值")
  }
  
  match find_attr("key2", attrs) {
    Option::Some(AttributeValue::IntValue(v)) => assert_eq(v, 100)
    _ => assert_true(false, "期望找到key2的整数值")
  }
  
  match find_attr("key3", attrs) {
    Option::Some(AttributeValue::BoolValue(v)) => assert_true(v)
    _ => assert_true(false, "期望找到key3的布尔值")
  }
  
  // 验证不存在的键
  match find_attr("nonexistent", attrs) {
    Option::None => assert_true(true)
    _ => assert_true(false, "期望不存在的键返回None")
  }
}

// 测试3: Resource 结构操作
test "Resource结构操作测试" {
  // 创建带有属性的资源
  let resource = Resource {
    attributes: [
      ("service.name", AttributeValue::StringValue("azimuth-service")),
      ("service.version", AttributeValue::StringValue("1.0.0")),
      ("service.instance.id", AttributeValue::StringValue("instance-123")),
      ("host.name", AttributeValue::StringValue("localhost")),
      ("process.pid", AttributeValue::IntValue(12345))
    ]
  }
  
  assert_eq(resource.attributes.length(), 5)
  
  // 验证资源属性
  let find_resource_attr = fn(key: String, resource: Resource) {
    let rec_find = fn(i: Int) {
      if i >= resource.attributes.length() {
        Option::None
      } else {
        let (k, v) = resource.attributes[i]
        if k == key {
          Option::Some(v)
        } else {
          rec_find(i + 1)
        }
      }
    }
    rec_find(0)
  }
  
  match find_resource_attr("service.name", resource) {
    Option::Some(AttributeValue::StringValue(v)) => assert_eq(v, "azimuth-service")
    _ => assert_true(false, "期望找到服务名称")
  }
  
  match find_resource_attr("process.pid", resource) {
    Option::Some(AttributeValue::IntValue(v)) => assert_eq(v, 12345)
    _ => assert_true(false, "期望找到进程ID")
  }
}

// 测试4: InstrumentationScope 结构操作
test "InstrumentationScope结构操作测试" {
  // 创建完整的仪器作用域
  let scope = InstrumentationScope {
    name: "azimuth.tracer",
    version: Option::Some("1.0.0"),
    schema_url: Option::Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  assert_eq(scope.name, "azimuth.tracer")
  
  match scope.version {
    Option::Some(v) => assert_eq(v, "1.0.0")
    Option::None => assert_true(false, "期望有版本信息")
  }
  
  match scope.schema_url {
    Option::Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.20.0")
    Option::None => assert_true(false, "期望有schema URL")
  }
  
  // 创建最小仪器作用域
  let minimal_scope = InstrumentationScope {
    name: "minimal.tracer",
    version: Option::None,
    schema_url: Option::None
  }
  
  assert_eq(minimal_scope.name, "minimal.tracer")
  
  match minimal_scope.version {
    Option::None => assert_true(true)
    Option::Some(_) => assert_true(false, "期望没有版本信息")
  }
  
  match minimal_scope.schema_url {
    Option::None => assert_true(true)
    Option::Some(_) => assert_true(false, "期望没有schema URL")
  }
}

// 测试5: SpanKind 枚举操作
test "SpanKind枚举操作测试" {
  // 测试所有跨度种类
  let kinds = [
    SpanKind::Internal,
    SpanKind::Server,
    SpanKind::Client,
    SpanKind::Producer,
    SpanKind::Consumer
  ]
  
  assert_eq(kinds.length(), 5)
  
  // 验证跨度种类判断函数
  let is_client = fn(kind: SpanKind) {
    match kind {
      SpanKind::Client => true
      _ => false
    }
  }
  
  let is_server = fn(kind: SpanKind) {
    match kind {
      SpanKind::Server => true
      _ => false
    }
  }
  
  assert_true(is_client(SpanKind::Client))
  assert_false(is_client(SpanKind::Server))
  assert_false(is_client(SpanKind::Internal))
  
  assert_true(is_server(SpanKind::Server))
  assert_false(is_server(SpanKind::Client))
  assert_false(is_server(SpanKind::Producer))
}

// 测试6: StatusCode 枚举操作
test "StatusCode枚举操作测试" {
  // 测试所有状态码
  let statuses = [
    StatusCode::Unset,
    StatusCode::Ok,
    StatusCode::Error
  ]
  
  assert_eq(statuses.length(), 3)
  
  // 验证状态码判断函数
  let is_error = fn(status: StatusCode) {
    match status {
      StatusCode::Error => true
      _ => false
    }
  }
  
  let is_success = fn(status: StatusCode) {
    match status {
      StatusCode::Ok => true
      _ => false
    }
  }
  
  assert_true(is_error(StatusCode::Error))
  assert_false(is_error(StatusCode::Ok))
  assert_false(is_error(StatusCode::Unset))
  
  assert_true(is_success(StatusCode::Ok))
  assert_false(is_success(StatusCode::Error))
  assert_false(is_success(StatusCode::Unset))
}