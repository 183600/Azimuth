// 跨模块集成测试用例
// 测试不同telemetry API模块之间的集成

test "resource_and_attributes_integration" {
  // 创建Resource
  let resource = Resource::default("integration-test-service")
  
  // 创建复杂的属性集合
  let complex_attributes = [
    ("service.name", AttributeValue::string("integration-test-service")),
    ("service.version", AttributeValue::string("1.0.0")),
    ("service.namespace", AttributeValue::string("production")),
    ("deployment.environment", AttributeValue::string("prod")),
    ("host.name", AttributeValue::string("integration-server-01")),
    ("process.pid", AttributeValue::int(12345L)),
    ("process.cpu.percent", AttributeValue::float(75.5)),
    ("process.healthy", AttributeValue::bool(true)),
    ("tags", AttributeValue::array_string(["api", "integration", "test"])),
    ("status.codes", AttributeValue::array_int([200L, 201L, 400L])),
    ("response.times", AttributeValue::array_float([0.1, 0.5, 1.0])),
    ("feature.flags", AttributeValue::array_bool([true, false, true]))
  ]
  
  // 验证Resource
  assert_eq(resource.service_name, "integration-test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
  
  // 验证复杂属性
  assert_eq(complex_attributes.length(), 12)
  
  // 验证特定属性
  match complex_attributes[0].1 {
    StringValue(service_name) => assert_eq(service_name, "integration-test-service")
    _ => @test.fail("Expected StringValue for service.name")
  }
  
  match complex_attributes[6].1 {
    IntValue(pid) => assert_eq(pid, 12345L)
    _ => @test.fail("Expected IntValue for process.pid")
  }
  
  match complex_attributes[7].1 {
    FloatValue(cpu_percent) => assert_eq(cpu_percent, 75.5)
    _ => @test.fail("Expected FloatValue for process.cpu.percent")
  }
  
  match complex_attributes[8].1 {
    BoolValue(healthy) => assert_eq(healthy, true)
    _ => @test.fail("Expected BoolValue for process.healthy")
  }
  
  // 验证数组属性
  match complex_attributes[9].1 {
    ArrayStringValue(tags) => {
      assert_eq(tags.length(), 3)
      assert_eq(tags[0], "api")
      assert_eq(tags[1], "integration")
      assert_eq(tags[2], "test")
    }
    _ => @test.fail("Expected ArrayStringValue for tags")
  }
  
  match complex_attributes[10].1 {
    ArrayIntValue(status_codes) => {
      assert_eq(status_codes.length(), 3)
      assert_eq(status_codes[0], 200L)
      assert_eq(status_codes[1], 201L)
      assert_eq(status_codes[2], 400L)
    }
    _ => @test.fail("Expected ArrayIntValue for status.codes")
  }
}

test "attribute_complex_integration" {
  // 创建复杂的属性集合，测试不同类型属性的集成
  
  let http_attributes = [
    ("http.method", AttributeValue::string("POST")),
    ("http.url", AttributeValue::string("/api/v1/payments")),
    ("http.status_code", AttributeValue::int(200L)),
    ("http.version", AttributeValue::string("1.1")),
    ("http.scheme", AttributeValue::string("https"))
  ]
  
  let user_attributes = [
    ("user.id", AttributeValue::string("user-12345")),
    ("user.tier", AttributeValue::string("premium")),
    ("user.age", AttributeValue::int(30L)),
    ("user.active", AttributeValue::bool(true)),
    ("user.tags", AttributeValue::array_string(["vip", "early_adopter"]))
  ]
  
  let payment_attributes = [
    ("payment.id", AttributeValue::string("pay-67890")),
    ("payment.amount", AttributeValue::float(99.99)),
    ("payment.currency", AttributeValue::string("USD")),
    ("payment.status", AttributeValue::string("completed")),
    ("payment.methods", AttributeValue::array_string(["credit_card", "paypal"]))
  ]
  
  let performance_attributes = [
    ("duration.ms", AttributeValue::int(500L)),
    ("cpu.usage", AttributeValue::float(75.5)),
    ("memory.usage", AttributeValue::float(512.0)),
    ("cache.hit", AttributeValue::bool(true)),
    ("response.sizes", AttributeValue::array_int([200L, 400L, 800L]))
  ]
  
  // 验证HTTP属性
  assert_eq(http_attributes.length(), 5)
  match http_attributes[0].1 {
    StringValue(method) => assert_eq(method, "POST")
    _ => @test.fail("Expected StringValue")
  }
  
  match http_attributes[2].1 {
    IntValue(status_code) => assert_eq(status_code, 200L)
    _ => @test.fail("Expected IntValue")
  }
  
  // 验证用户属性
  assert_eq(user_attributes.length(), 5)
  match user_attributes[2].1 {
    IntValue(age) => assert_eq(age, 30L)
    _ => @test.fail("Expected IntValue")
  }
  
  match user_attributes[3].1 {
    BoolValue(active) => assert_eq(active, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  match user_attributes[4].1 {
    ArrayStringValue(tags) => {
      assert_eq(tags.length(), 2)
      assert_eq(tags[0], "vip")
      assert_eq(tags[1], "early_adopter")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  // 验证支付属性
  assert_eq(payment_attributes.length(), 5)
  match payment_attributes[1].1 {
    FloatValue(amount) => assert_eq(amount, 99.99)
    _ => @test.fail("Expected FloatValue")
  }
  
  match payment_attributes[4].1 {
    ArrayStringValue(methods) => {
      assert_eq(methods.length(), 2)
      assert_eq(methods[0], "credit_card")
      assert_eq(methods[1], "paypal")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  // 验证性能属性
  assert_eq(performance_attributes.length(), 5)
  match performance_attributes[0].1 {
    IntValue(duration) => assert_eq(duration, 500L)
    _ => @test.fail("Expected IntValue")
  }
  
  match performance_attributes[1].1 {
    FloatValue(cpu) => assert_eq(cpu, 75.5)
    _ => @test.fail("Expected FloatValue")
  }
  
  match performance_attributes[4].1 {
    ArrayIntValue(sizes) => {
      assert_eq(sizes.length(), 3)
      assert_eq(sizes[0], 200L)
      assert_eq(sizes[1], 400L)
      assert_eq(sizes[2], 800L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  // 测试属性合并
  let all_attributes = []
  
  // 合并所有属性集合
  let mut i = 0
  while i < http_attributes.length() {
    all_attributes.push(http_attributes[i])
    i = i + 1
  }
  
  i = 0
  while i < user_attributes.length() {
    all_attributes.push(user_attributes[i])
    i = i + 1
  }
  
  i = 0
  while i < payment_attributes.length() {
    all_attributes.push(payment_attributes[i])
    i = i + 1
  }
  
  i = 0
  while i < performance_attributes.length() {
    all_attributes.push(performance_attributes[i])
    i = i + 1
  }
  
  // 验证合并后的属性
  assert_eq(all_attributes.length(), 20)
  
  // 验证特定属性
  let mut found_http_method = false
  let mut found_user_id = false
  let mut found_payment_amount = false
  let mut found_duration = false
  
  i = 0
  while i < all_attributes.length() {
    let (key, value) = all_attributes[i]
    
    if key == "http.method" {
      match value {
        StringValue(method) => {
          assert_eq(method, "POST")
          found_http_method = true
        }
        _ => @test.fail("Expected StringValue for http.method")
      }
    }
    
    if key == "user.id" {
      match value {
        StringValue(user_id) => {
          assert_eq(user_id, "user-12345")
          found_user_id = true
        }
        _ => @test.fail("Expected StringValue for user.id")
      }
    }
    
    if key == "payment.amount" {
      match value {
        FloatValue(amount) => {
          assert_eq(amount, 99.99)
          found_payment_amount = true
        }
        _ => @test.fail("Expected FloatValue for payment.amount")
      }
    }
    
    if key == "duration.ms" {
      match value {
        IntValue(duration) => {
          assert_eq(duration, 500L)
          found_duration = true
        }
        _ => @test.fail("Expected IntValue for duration.ms")
      }
    }
    
    i = i + 1
  }
  
  assert_eq(found_http_method, true)
  assert_eq(found_user_id, true)
  assert_eq(found_payment_amount, true)
  assert_eq(found_duration, true)
}

test "attribute_type_comprehensive_integration" {
  // 创建包含所有类型属性的Resource
  let resource = Resource::default("comprehensive-test-service")
  
  // 创建包含所有属性类型的集合
  let comprehensive_attributes = [
    // 字符串属性
    ("string.simple", AttributeValue::string("simple string")),
    ("string.empty", AttributeValue::string("")),
    ("string.special", AttributeValue::string("special!@#$%^&*()")),
    ("string.unicode", AttributeValue::string("Unicode测试🚀")),
    ("string.long", AttributeValue::string("this is a very long string that tests the system")),
    
    // 整数属性
    ("int.zero", AttributeValue::int(0L)),
    ("int.positive", AttributeValue::int(42L)),
    ("int.negative", AttributeValue::int(-42L)),
    ("int.max", AttributeValue::int(9223372036854775807L)),
    ("int.min", AttributeValue::int(-9223372036854775808L)),
    
    // 浮点数属性
    ("float.zero", AttributeValue::float(0.0)),
    ("float.positive", AttributeValue::float(3.14)),
    ("float.negative", AttributeValue::float(-3.14)),
    ("float.infinity", AttributeValue::float(1.0/0.0)),
    ("float.nan", AttributeValue::float(0.0/0.0)),
    
    // 布尔属性
    ("bool.true", AttributeValue::bool(true)),
    ("bool.false", AttributeValue::bool(false)),
    
    // 数组属性
    ("array.string", AttributeValue::array_string(["a", "b", "c"])),
    ("array.int", AttributeValue::array_int([1L, 2L, 3L])),
    ("array.float", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("array.bool", AttributeValue::array_bool([true, false, true])),
    ("array.empty", AttributeValue::array_string([]))
  ]
  
  // 验证Resource
  assert_eq(resource.service_name, "comprehensive-test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  
  // 验证综合属性
  assert_eq(comprehensive_attributes.length(), 22)
  
  // 验证字符串属性
  match comprehensive_attributes[0].1 {
    StringValue(value) => assert_eq(value, "simple string")
    _ => @test.fail("Expected StringValue")
  }
  
  match comprehensive_attributes[1].1 {
    StringValue(value) => assert_eq(value, "")
    _ => @test.fail("Expected StringValue")
  }
  
  match comprehensive_attributes[3].1 {
    StringValue(value) => assert_eq(value, "Unicode测试🚀")
    _ => @test.fail("Expected StringValue")
  }
  
  // 验证整数属性
  match comprehensive_attributes[5].1 {
    IntValue(value) => assert_eq(value, 0L)
    _ => @test.fail("Expected IntValue")
  }
  
  match comprehensive_attributes[8].1 {
    IntValue(value) => assert_eq(value, 9223372036854775807L)
    _ => @test.fail("Expected IntValue")
  }
  
  // 验证浮点数属性
  match comprehensive_attributes[10].1 {
    FloatValue(value) => assert_eq(value, 0.0)
    _ => @test.fail("Expected FloatValue")
  }
  
  match comprehensive_attributes[13].1 {
    FloatValue(value) => {
      let is_nan = value != value
      assert_eq(is_nan, true)
    }
    _ => @test.fail("Expected FloatValue")
  }
  
  // 验证布尔属性
  match comprehensive_attributes[14].1 {
    BoolValue(value) => assert_eq(value, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  match comprehensive_attributes[15].1 {
    BoolValue(value) => assert_eq(value, false)
    _ => @test.fail("Expected BoolValue")
  }
  
  // 验证数组属性
  match comprehensive_attributes[16].1 {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match comprehensive_attributes[17].1 {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1L)
      assert_eq(arr[1], 2L)
      assert_eq(arr[2], 3L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match comprehensive_attributes[18].1 {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1.1)
      assert_eq(arr[1], 2.2)
      assert_eq(arr[2], 3.3)
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  match comprehensive_attributes[19].1 {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
    }
    _ => @test.fail("Expected ArrayBoolValue")
  }
  
  match comprehensive_attributes[20].1 {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayStringValue")
  }
}
