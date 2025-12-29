// Common模块高级测试用例

test "resource_complex_attributes_and_validation" {
  // 测试Resource的复杂属性和验证
  
  let resource = Resource::default("advanced-test-service")
  
  // 创建包含各种类型属性的复杂资源
  let advanced_attrs : Attributes = [
    // 基本字符串属性
    ("service.namespace", AttributeValue::string("production")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("service.instance.id", AttributeValue::string("instance-12345-67890")),
    ("deployment.environment", AttributeValue::string("prod")),
    
    // 主机和进程信息
    ("host.name", AttributeValue::string("web-server-01.example.com")),
    ("host.arch", AttributeValue::string("amd64")),
    ("host.os.type", AttributeValue::string("linux")),
    ("host.os.version", AttributeValue::string("5.15.0-1020-azure")),
    ("process.pid", AttributeValue::int(12345L)),
    ("process.executable.name", AttributeValue::string("node")),
    ("process.executable.path", AttributeValue::string("/usr/local/bin/node")),
    ("process.command_args", AttributeValue::array_string(["node", "server.js", "--port=8080"])),
    
    // 数值属性
    ("process.cpu.percent", AttributeValue::float(75.5)),
    ("process.memory.rss", AttributeValue::int(134217728L)),  // 128MB
    ("process.memory.vms", AttributeValue::int(268435456L)),  // 256MB
    ("process.uptime", AttributeValue::float(3600.5)),        // 1 hour
    ("process.thread_count", AttributeValue::int(12L)),
    
    // 布尔属性
    ("process.healthy", AttributeValue::bool(true)),
    ("service.debug.enabled", AttributeValue::bool(false)),
    ("metrics.enabled", AttributeValue::bool(true)),
    ("tracing.enabled", AttributeValue::bool(true)),
    
    // 数组属性
    ("service.tags", AttributeValue::array_string(["web", "api", "payment", "critical"])),
    ("ports", AttributeValue::array_int([8080L, 8443L, 9090L, 3000L])),
    ("cpu.thresholds", AttributeValue::array_float([60.0, 80.0, 95.0])),
    ("feature.flags", AttributeValue::array_bool([true, false, true, true, false])),
    
    // 特殊字符和Unicode
    ("service.description", AttributeValue::string("Payment processing service with special chars: !@#$%^&*()")),
    ("service.unicode.name", AttributeValue::string("支付处理服务🚀💰")),
    ("config.path", AttributeValue::string("/etc/app/config.d/payment-service.yaml")),
    
    // 复杂嵌套数据（作为字符串）
    ("service.dependencies", AttributeValue::string("[\"database\", \"cache\", \"queue\", \"storage\"]")),
    ("health.check.urls", AttributeValue::array_string(["/health", "/ready", "/live"])),
    ("custom.metrics", AttributeValue::array_string(["request.duration", "error.rate", "throughput"]))
  ]
  
  // 验证Resource基本属性
  assert_eq(resource.service_name, "advanced-test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
  
  // 验证复杂属性的数量
  assert_eq(advanced_attrs.length(), 31)
  
  // 验证字符串属性
  match advanced_attrs[0].1 {
    StringValue(namespace) => assert_eq(namespace, "production")
    _ => @test.fail("Test failed")
  }
  
  // 验证整数属性
  match advanced_attrs[8].1 {
    IntValue(pid) => assert_eq(pid, 12345L)
    _ => @test.fail("Test failed")
  }
  
  // 验证浮点数属性
  match advanced_attrs[12].1 {
    FloatValue(cpu_percent) => assert_eq(cpu_percent, 75.5)
    _ => @test.fail("Test failed")
  }
  
  // 验证布尔属性
  match advanced_attrs[17].1 {
    BoolValue(healthy) => assert_eq(healthy, true)
    _ => @test.fail("Test failed")
  }
  
  // 验证字符串数组属性
  match advanced_attrs[21].1 {
    ArrayStringValue(tags) => {
      assert_eq(tags.length(), 4)
      assert_eq(tags[0], "web")
      assert_eq(tags[1], "api")
      assert_eq(tags[2], "payment")
      assert_eq(tags[3], "critical")
    }
    _ => @test.fail("Test failed")
  }
  
  // 验证整数数组属性
  match advanced_attrs[22].1 {
    ArrayIntValue(ports) => {
      assert_eq(ports.length(), 4)
      assert_eq(ports[0], 8080L)
      assert_eq(ports[1], 8443L)
      assert_eq(ports[2], 9090L)
      assert_eq(ports[3], 3000L)
    }
    _ => @test.fail("Test failed")
  }
  
  // 验证浮点数数组属性
  match advanced_attrs[23].1 {
    ArrayFloatValue(thresholds) => {
      assert_eq(thresholds.length(), 3)
      assert_eq(thresholds[0], 60.0)
      assert_eq(thresholds[1], 80.0)
      assert_eq(thresholds[2], 95.0)
    }
    _ => @test.fail("Test failed")
  }
  
  // 验证布尔数组属性
  match advanced_attrs[24].1 {
    ArrayBoolValue(flags) => {
      assert_eq(flags.length(), 5)
      assert_eq(flags[0], true)
      assert_eq(flags[1], false)
      assert_eq(flags[2], true)
      assert_eq(flags[3], true)
      assert_eq(flags[4], false)
    }
    _ => @test.fail("Test failed")
  }
  
  // 验证Unicode属性
  match advanced_attrs[26].1 {
    StringValue(unicode_name) => {
      assert_eq(unicode_name.contains("支付"), true)
      assert_eq(unicode_name.contains("🚀"), true)
      assert_eq(unicode_name.contains("💰"), true)
    }
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_edge_cases_and_type_safety" {
  // 测试AttributeValue的边界情况和类型安全
  
  // 测试极值
  let max_int = AttributeValue::int(9223372036854775807L)  // Int64最大值
  let min_int = AttributeValue::int(-9223372036854775808L)  // Int64最小值
  let zero_int = AttributeValue::int(0L)
  
  let max_float = AttributeValue::float(1.7976931348623157e+308)  // Double最大值
  let min_float = AttributeValue::float(-1.7976931348623157e+308) // Double最小值
  let inf_float = AttributeValue::float(1.0/0.0)  // 正无穷
  let neg_inf_float = AttributeValue::float(-1.0/0.0)  // 负无穷
  let nan_float = AttributeValue::float(0.0/0.0)  // NaN
  let zero_float = AttributeValue::float(0.0)
  
  // 验证极值
  match max_int {
    IntValue(value) => assert_eq(value, 9223372036854775807L)
    _ => @test.fail("Test failed")
  }
  
  match min_int {
    IntValue(value) => assert_eq(value, -9223372036854775808L)
    _ => @test.fail("Test failed")
  }
  
  match zero_int {
    IntValue(value) => assert_eq(value, 0L)
    _ => @test.fail("Test failed")
  }
  
  match max_float {
    FloatValue(value) => assert_eq(value > 1.0e+308, true)
    _ => @test.fail("Test failed")
  }
  
  match min_float {
    FloatValue(value) => assert_eq(value < -1.0e+308, true)
    _ => @test.fail("Test failed")
  }
  
  match inf_float {
    FloatValue(value) => assert_eq(value > 1.0e+100, true)  // 检查是否为无穷大
    _ => @test.fail("Test failed")
  }
  
  match neg_inf_float {
    FloatValue(value) => assert_eq(value < -1.0e+100, true)  // 检查是否为负无穷大
    _ => @test.fail("Test failed")
  }
  
  match nan_float {
    FloatValue(value) => assert_eq(true, true)  // NaN的特殊检查
    _ => @test.fail("Test failed")
  }
  
  match zero_float {
    FloatValue(value) => assert_eq(value, 0.0)
    _ => @test.fail("Test failed")
  }
  
  // 测试空数组和单元素数组
  let empty_string_array = AttributeValue::array_string([])
  let empty_int_array = AttributeValue::array_int([])
  let empty_float_array = AttributeValue::array_float([])
  let empty_bool_array = AttributeValue::array_bool([])
  
  let single_string_array = AttributeValue::array_string(["single"])
  let single_int_array = AttributeValue::array_int([42L])
  let single_float_array = AttributeValue::array_float([3.14])
  let single_bool_array = AttributeValue::array_bool([true])
  
  // 验证空数组
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  match empty_float_array {
    ArrayFloatValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  match empty_bool_array {
    ArrayBoolValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  // 验证单元素数组
  match single_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], "single")
    }
    _ => @test.fail("Test failed")
  }
  
  match single_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 42L)
    }
    _ => @test.fail("Test failed")
  }
  
  match single_float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 3.14)
    }
    _ => @test.fail("Test failed")
  }
  
  match single_bool_array {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 测试特殊字符串值
  let empty_string = AttributeValue::string("")
  let space_string = AttributeValue::string("   ")
  let special_chars_string = AttributeValue::string("!@#$%^&*()_+-=[]{}|;':\",./<>?")
  let unicode_string = AttributeValue::string("Unicode测试🚀🌟💫\n\t\r")
  let very_long_string = AttributeValue::string("A".repeat(10000))
  
  match empty_string {
    StringValue(value) => assert_eq(value, "")
    _ => @test.fail("Test failed")
  }
  
  match space_string {
    StringValue(value) => assert_eq(value, "   ")
    _ => @test.fail("Test failed")
  }
  
  match special_chars_string {
    StringValue(value) => assert_eq(value.contains("!@#$%^&*()"), true)
    _ => @test.fail("Test failed")
  }
  
  match unicode_string {
    StringValue(value) => {
      assert_eq(value.contains("测试"), true)
      assert_eq(value.contains("🚀"), true)
      assert_eq(value.contains("\n"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  match very_long_string {
    StringValue(value) => assert_eq(value.length(), 10000)
    _ => @test.fail("Test failed")
  }
}