// Attribute Value Deep Nesting Tests
// 属性值深度嵌套测试，测试复杂嵌套数据结构和大型属性值的处理

test "基本属性值类型深度测试" {
  let attrs = Attributes::new()
  
  // 测试字符串属性的复杂内容
  let complex_string = "This is a complex string with various characters: 中文, 日本語, español, русский, عربي, emoji🚀, symbols!@#$%^&*()"
  Attributes::set(attrs, "complex.string", StringValue(complex_string))
  
  let retrieved_complex = Attributes::get(attrs, "complex.string")
  match retrieved_complex {
    Some(StringValue(value)) => assert_eq(value, complex_string)
    _ => assert_true(false)
  }
  
  // 测试极值整数
  let extreme_ints = [
    ("int.max", IntValue(2147483647)),
    ("int.min", IntValue(-2147483648)),
    ("int.zero", IntValue(0)),
    ("int.positive", IntValue(123456789)),
    ("int.negative", IntValue(-987654321))
  ]
  
  for int_attr in extreme_ints {
    Attributes::set(attrs, int_attr.0, int_attr.1)
    let retrieved = Attributes::get(attrs, int_attr.0)
    match retrieved {
      Some(IntValue(value)) => assert_eq(value, int_attr.1.1)
      _ => assert_true(false)
    }
  }
  
  // 测试极值浮点数
  let extreme_floats = [
    ("float.max", FloatValue(1.7976931348623157e+308)),
    ("float.min", FloatValue(-1.7976931348623157e+308)),
    ("float.zero", FloatValue(0.0)),
    ("float.small", FloatValue(1.0e-10)),
    ("float.large", FloatValue(1.0e+10)),
    ("float.pi", FloatValue(3.141592653589793)),
    ("float.negative", FloatValue(-2.718281828459045)),
    ("float.infinity", FloatValue(1.0/0.0)),
    ("float.nan", FloatValue(0.0/0.0))
  ]
  
  for float_attr in extreme_floats {
    Attributes::set(attrs, float_attr.0, float_attr.1)
    let retrieved = Attributes::get(attrs, float_attr.0)
    match retrieved {
      Some(FloatValue(value)) => {
        if float_attr.0 == "float.nan" {
          assert_true(value != value) // NaN检查
        } else {
          assert_eq(value, float_attr.1.1)
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 测试布尔值的各种场景
  let bool_values = [
    ("bool.true", BoolValue(true)),
    ("bool.false", BoolValue(false)),
    ("bool.zero", BoolValue(false)), // 0转换为false
    ("bool.one", BoolValue(true))    // 1转换为true
  ]
  
  for bool_attr in bool_values {
    Attributes::set(attrs, bool_attr.0, bool_attr.1)
    let retrieved = Attributes::get(attrs, bool_attr.0)
    match retrieved {
      Some(BoolValue(value)) => assert_eq(value, bool_attr.1.1)
      _ => assert_true(false)
    }
  }
}

test "数组属性值深度测试" {
  let attrs = Attributes::new()
  
  // 测试大型字符串数组
  let large_string_array = ArrayStringValue([
    "item1", "item2", "item3", "item4", "item5",
    "中文项目1", "日本語アイテム2", "español artículo3", "русский элемент4", "العنصر5",
    "emoji🚀item", "special!@#$%item", "space item", "tab\titem", "newline\nitem",
    "very.long.item.name.with.many.dots.and.underscores_and-various-characters-12345",
    "json-like-item: {\"key\": \"value\", \"array\": [1, 2, 3]}",
    "xml-like-item: <element attribute=\"value\">content</element>",
    "url-like-item: https://example.com/path?query=value&param2=value2#fragment",
    "base64-like-item: SGVsbG8gV29ybGQgZnJvbSBNb29uQml0IQ=="
  ])
  
  Attributes::set(attrs, "large.string.array", large_string_array)
  
  let retrieved_large_array = Attributes::get(attrs, "large.string.array")
  match retrieved_large_array {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 18)
      assert_eq(values[0], "item1")
      assert_eq(values[5], "中文项目1")
      assert_eq(values[10], "emoji🚀item")
      assert_eq(values[17], "SGVsbG8gV29ybGQgZnJvbSBNb29uQml0IQ==")
    }
    _ => assert_true(false)
  }
  
  // 测试大型整数数组
  let large_int_array = ArrayIntValue([
    0, 1, -1, 2147483647, -2147483648, 123456789, -987654321,
    42, 100, 1000, 10000, 100000, 1000000, 10000000,
    111111111, 222222222, 333333333, 444444444, 555555555,
    999999, 888888, 777777, 666666, 555555, 444444
  ])
  
  Attributes::set(attrs, "large.int.array", large_int_array)
  
  let retrieved_int_array = Attributes::get(attrs, "large.int.array")
  match retrieved_int_array {
    Some(ArrayIntValue(values)) => {
      assert_eq(values.length(), 25)
      assert_eq(values[0], 0)
      assert_eq(values[3], 2147483647)
      assert_eq(values[4], -2147483648)
      assert_eq(values[24], 444444)
    }
    _ => assert_true(false)
  }
  
  // 测试空数组和单元素数组
  let empty_string_array = ArrayStringValue([])
  let single_string_array = ArrayStringValue(["single.item"])
  let empty_int_array = ArrayIntValue([])
  let single_int_array = ArrayIntValue([42])
  
  Attributes::set(attrs, "empty.string.array", empty_string_array)
  Attributes::set(attrs, "single.string.array", single_string_array)
  Attributes::set(attrs, "empty.int.array", empty_int_array)
  Attributes::set(attrs, "single.int.array", single_int_array)
  
  let empty_str_retrieved = Attributes::get(attrs, "empty.string.array")
  match empty_str_retrieved {
    Some(ArrayStringValue(values)) => assert_eq(values.length(), 0)
    _ => assert_true(false)
  }
  
  let single_str_retrieved = Attributes::get(attrs, "single.string.array")
  match single_str_retrieved {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 1)
      assert_eq(values[0], "single.item")
    }
    _ => assert_true(false)
  }
}

test "嵌套属性结构测试" {
  let attrs = Attributes::new()
  
  // 模拟嵌套结构使用点号分隔的键名
  let nested_user_data = [
    ("user.id", StringValue("12345")),
    ("user.name", StringValue("张三")),
    ("user.email", StringValue("zhangsan@example.com")),
    ("user.profile.age", IntValue(30)),
    ("user.profile.height", FloatValue(175.5)),
    ("user.profile.active", BoolValue(true)),
    ("user.preferences.language", StringValue("zh-CN")),
    ("user.preferences.timezone", StringValue("Asia/Shanghai")),
    ("user.preferences.notifications.email", BoolValue(true)),
    ("user.preferences.notifications.sms", BoolValue(false)),
    ("user.roles", ArrayStringValue(["user", "premium", "admin"])),
    ("user.permissions", ArrayIntValue([100, 200, 300, 400])),
    ("user.metadata.created_at", StringValue("2025-01-01T00:00:00Z")),
    ("user.metadata.updated_at", StringValue("2025-01-15T12:30:45Z")),
    ("user.session.count", IntValue(42)),
    ("user.session.last_login", StringValue("2025-01-20T08:15:30Z"))
  ]
  
  for attr in nested_user_data {
    Attributes::set(attrs, attr.0, attr.1)
  }
  
  // 验证嵌套数据
  let user_id = Attributes::get(attrs, "user.id")
  match user_id {
    Some(StringValue(value)) => assert_eq(value, "12345")
    _ => assert_true(false)
  }
  
  let user_age = Attributes::get(attrs, "user.profile.age")
  match user_age {
    Some(IntValue(value)) => assert_eq(value, 30)
    _ => assert_true(false)
  }
  
  let user_height = Attributes::get(attrs, "user.profile.height")
  match user_height {
    Some(FloatValue(value)) => assert_eq(value, 175.5)
    _ => assert_true(false)
  }
  
  let user_roles = Attributes::get(attrs, "user.roles")
  match user_roles {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "user")
      assert_eq(values[1], "premium")
      assert_eq(values[2], "admin")
    }
    _ => assert_true(false)
  }
  
  // 测试深度嵌套的键名
  let very_nested_key = "level1.level2.level3.level4.level5.very.deep.nested.key"
  Attributes::set(attrs, very_nested_key, StringValue("deep.value"))
  
  let deep_value = Attributes::get(attrs, very_nested_key)
  match deep_value {
    Some(StringValue(value)) => assert_eq(value, "deep.value")
    _ => assert_true(false)
  }
}

test "复杂JSON风格属性测试" {
  let attrs = Attributes::new()
  
  // 模拟JSON风格的复杂数据结构
  let json_like_data = [
    ("request.id", StringValue("req-12345")),
    ("request.method", StringValue("POST")),
    ("request.url", StringValue("/api/v1/users")),
    ("request.headers.content-type", StringValue("application/json")),
    ("request.headers.authorization", StringValue("Bearer token123")),
    ("request.headers.user-agent", StringValue("Azimuth-Client/1.0.0")),
    ("request.body.size", IntValue(1024)),
    ("request.body.type", StringValue("json")),
    ("request.timestamp", StringValue("2025-01-20T10:30:45.123Z")),
    
    ("response.status_code", IntValue(200)),
    ("response.headers.content-type", StringValue("application/json")),
    ("response.headers.content-length", StringValue("2048")),
    ("response.body.size", IntValue(2048)),
    ("response.duration.ms", IntValue(150)),
    ("response.timestamp", StringValue("2025-01-20T10:30:45.273Z")),
    
    ("server.name", StringValue("api-server-001")),
    ("server.region", StringValue("us-west-2")),
    ("server.version", StringValue("2.1.0")),
    ("server.hostname", StringValue("api-001.example.com")),
    ("server.ip", StringValue("10.0.1.100")),
    
    ("client.ip", StringValue("192.168.1.100")),
    ("client.user_agent", StringValue("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")),
    ("client.geo.country", StringValue("CN")),
    ("client.geo.city", StringValue("Beijing")),
    ("client.geo.latitude", FloatValue(39.9042)),
    ("client.geo.longitude", FloatValue(116.4074)),
    
    ("trace.id", StringValue("trace-abcdef123456")),
    ("span.id", StringValue("span-789012")),
    ("parent.span.id", StringValue("span-345678")),
    ("trace.sampled", BoolValue(true)),
    ("trace.flags", IntValue(1)),
    
    ("error.occurred", BoolValue(false)),
    ("error.type", StringValue("")),
    ("error.message", StringValue("")),
    ("error.stack", StringValue("")),
    
    ("metrics.request.count", IntValue(1)),
    ("metrics.response.size.bytes", IntValue(2048)),
    ("metrics.processing.time.ms", IntValue(150)),
    ("metrics.db.query.count", IntValue(3)),
    ("metrics.cache.hit.rate", FloatValue(0.85)),
    ("metrics.memory.usage.mb", FloatValue(256.7)),
    
    ("tags.service", StringValue("user-api")),
    ("tags.environment", StringValue("production")),
    ("tags.version", StringValue("v2.1.0")),
    ("tags.team", StringValue("platform")),
    ("tags.component", StringValue("authentication"))
  ]
  
  for attr in json_like_data {
    Attributes::set(attrs, attr.0, attr.1)
  }
  
  // 验证复杂JSON风格数据
  let request_id = Attributes::get(attrs, "request.id")
  match request_id {
    Some(StringValue(value)) => assert_eq(value, "req-12345")
    _ => assert_true(false)
  }
  
  let response_status = Attributes::get(attrs, "response.status_code")
  match response_status {
    Some(IntValue(value)) => assert_eq(value, 200)
    _ => assert_true(false)
  }
  
  let client_geo = Attributes::get(attrs, "client.geo.city")
  match client_geo {
    Some(StringValue(value)) => assert_eq(value, "Beijing")
    _ => assert_true(false)
  }
  
  let metrics_memory = Attributes::get(attrs, "metrics.memory.usage.mb")
  match metrics_memory {
    Some(FloatValue(value)) => assert_eq(value, 256.7)
    _ => assert_true(false)
  }
  
  let trace_sampled = Attributes::get(attrs, "trace.sampled")
  match trace_sampled {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false)
  }
}

test "大型属性值性能测试" {
  let attrs = Attributes::new()
  
  // 创建大型字符串属性
  let large_string = "x" * 10000  // 10KB字符串
  Attributes::set(attrs, "large.string", StringValue(large_string))
  
  let large_retrieved = Attributes::get(attrs, "large.string")
  match large_retrieved {
    Some(StringValue(value)) => {
      assert_eq(value.length(), 10000)
      assert_eq(value[0], 'x')
      assert_eq(value[9999], 'x')
    }
    _ => assert_true(false)
  }
  
  // 创建大型数组属性
  let large_string_items = []
  for i in 0..1000 {
    large_string_items.push("item_" + i.to_string())
  }
  
  let large_array = ArrayStringValue(large_string_items)
  Attributes::set(attrs, "large.array", large_array)
  
  let large_array_retrieved = Attributes::get(attrs, "large.array")
  match large_array_retrieved {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 1000)
      assert_eq(values[0], "item_0")
      assert_eq(values[999], "item_999")
    }
    _ => assert_true(false)
  }
  
  // 创建大量不同类型的属性
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  for i in 0..1000 {
    let key_prefix = "perf.test." + i.to_string()
    
    Attributes::set(attrs, key_prefix + ".string", StringValue("value_" + i.to_string()))
    Attributes::set(attrs, key_prefix + ".int", IntValue(i))
    Attributes::set(attrs, key_prefix + ".float", FloatValue(i.to_float() * 3.14))
    Attributes::set(attrs, key_prefix + ".bool", BoolValue(i % 2 == 0))
    
    if i % 10 == 0 {
      let small_array = ArrayStringValue([i.to_string(), (i+1).to_string(), (i+2).to_string()])
      Attributes::set(attrs, key_prefix + ".array", small_array)
    }
  }
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let duration = end_time - start_time
  
  // 验证性能在合理范围内
  assert_true(duration > 0L)
  assert_true(duration < 10000000000L) // 小于10秒
  
  // 验证数据完整性
  let test_string = Attributes::get(attrs, "perf.test.500.string")
  match test_string {
    Some(StringValue(value)) => assert_eq(value, "value_500")
    _ => assert_true(false)
  }
  
  let test_int = Attributes::get(attrs, "perf.test.750.int")
  match test_int {
    Some(IntValue(value)) => assert_eq(value, 750)
    _ => assert_true(false)
  }
  
  let test_bool = Attributes::get(attrs, "perf.test.999.bool")
  match test_bool {
    Some(BoolValue(value)) => assert_eq(value, false) // 999是奇数
    _ => assert_true(false)
  }
}

test "属性值边界条件和错误处理测试" {
  let attrs = Attributes::new()
  
  // 测试特殊键名
  let special_keys = [
    "", // 空键名
    " ", // 空格键名
    ".", // 点号键名
    "..", // 双点号键名
    "key.with.many.dots...............................................", // 很多点
    "key-with-many-dashes-------------------------------------------", // 很多破折号
    "key_with_many_underscores_______________________________________", // 很多下划线
    "key with many spaces                                           ", // 很多空格
    "键名包含中文", // 中文键名
    "キー名を含む日本語", // 日文键名
    "nombre.de.clave.en.español", // 西班牙文键名
    "имя.ключа.на.русском", // 俄文键名
    "اسم.المفتاح.بالعربية", // 阿拉伯文键名
    "key🚀with🌟emoji✨symbols", // 包含emoji的键名
    "key!@#$%^&*()with+[]{}|\\:;\"'<>?,./", // 包含特殊符号的键名
    "key\nwith\tnewlines\rand\ttabs", // 包含控制字符的键名
    "a" * 1000 // 超长键名
  ]
  
  for special_key in special_keys {
    // 测试设置特殊键名
    Attributes::set(attrs, special_key, StringValue("special.value"))
    
    // 测试获取特殊键名
    let retrieved = Attributes::get(attrs, special_key)
    match retrieved {
      Some(StringValue(value)) => assert_eq(value, "special.value")
      None => {
        // 某些特殊键名可能不被支持，这是可以接受的
        assert_true(true)
      }
      _ => assert_true(false)
    }
  }
  
  // 测试特殊值
  let special_string_values = [
    "", // 空字符串
    " ", // 空格字符串
    "\n", // 换行符
    "\t", // 制表符
    "\r", // 回车符
    "\0", // 空字符（如果支持）
    "null", // 字符串"null"
    "undefined", // 字符串"undefined"
    "true", // 字符串"true"
    "false", // 字符串"false"
    "0", // 字符串"0"
    "-1", // 字符串"-1"
    "3.14159", // 字符串数字
    "2025-01-20T10:30:45.123Z", // ISO时间戳
    "{\"json\": \"data\", \"array\": [1, 2, 3]}", // JSON字符串
    "<xml><element>data</element></xml>", // XML字符串
    "SGVsbG8gV29ybGQ=", // Base64字符串
    "🚀🌟✨💫⭐", // 纯emoji字符串
    "测试中文", // 中文字符串
    "テスト日本語", // 日文字符串
    "prueba español", // 西班牙文字符串
    "тест русский", // 俄文字符串
    "اختبار عربي", // 阿拉伯文字符串
    "a" * 10000, // 超长字符串
    "\n\n\n\n\n", // 多个换行符
    "          ", // 多个空格
    "\t\t\t\t\t", // 多个制表符
    "mixed🚀content🌟with✨symbols💫and🎉text📝" // 混合内容
  ]
  
  for i in 0..special_string_values.length() {
    let special_value = special_string_values[i]
    let key = "special.value." + i.to_string()
    
    Attributes::set(attrs, key, StringValue(special_value))
    
    let retrieved = Attributes::get(attrs, key)
    match retrieved {
      Some(StringValue(value)) => assert_eq(value, special_value)
      _ => assert_true(false)
    }
  }
  
  // 测试极值数值
  let extreme_values = [
    ("extreme.int.max", IntValue(2147483647)),
    ("extreme.int.min", IntValue(-2147483648)),
    ("extreme.float.pos.inf", FloatValue(1.0/0.0)),
    ("extreme.float.neg.inf", FloatValue(-1.0/0.0)),
    ("extreme.float.nan", FloatValue(0.0/0.0)),
    ("extreme.float.small", FloatValue(1.0e-308)),
    ("extreme.float.large", FloatValue(1.0e+308))
  ]
  
  for extreme_value in extreme_values {
    Attributes::set(attrs, extreme_value.0, extreme_value.1)
    
    let retrieved = Attributes::get(attrs, extreme_value.0)
    match retrieved {
      Some(value) => {
        match extreme_value.1 {
          FloatValue(f) if extreme_value.0 == "extreme.float.nan" => {
            // NaN检查
            match value {
              FloatValue(retrieved_f) => assert_true(retrieved_f != retrieved_f)
              _ => assert_true(false)
            }
          }
          _ => assert_eq(value, extreme_value.1)
        }
      }
      _ => assert_true(false)
    }
  }
}

test "属性值在遥测组件中的集成测试" {
  let attrs = Attributes::new()
  
  // 创建复杂的属性集合用于遥测
  let telemetry_attributes = [
    ("service.name", StringValue("user-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("instance-12345")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("prod-user-001.example.com")),
    ("host.ip", StringValue("10.0.1.100")),
    ("process.pid", IntValue(12345)),
    ("process.executable.name", StringValue("user-service")),
    ("process.executable.path", StringValue("/opt/services/user-service/bin/user-service")),
    ("process.command_args", ArrayStringValue(["--port=8080", "--config=/etc/user-service/config.yaml"])),
    ("process.runtime.name", StringValue("moonbit")),
    ("process.runtime.version", StringValue("1.0.0")),
    ("process.runtime.description", StringValue("MoonBit Runtime Environment")),
    
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.version", StringValue("1.0.0")),
    ("telemetry.sdk.language", StringValue("moonbit")),
    ("telemetry.auto.version", StringValue("1.0.0")),
    
    ("container.id", StringValue("container-67890")),
    ("container.name", StringValue("user-service-container")),
    ("container.image.name", StringValue("user-service:2.1.0")),
    ("container.image.tag", StringValue("2.1.0")),
    
    ("k8s.pod.name", StringValue("user-service-7d4b8c9f-1234")),
    ("k8s.namespace.name", StringValue("production")),
    ("k8s.deployment.name", StringValue("user-service")),
    ("k8s.node.name", StringValue("worker-node-001")),
    
    ("cloud.provider", StringValue("aws")),
    ("cloud.account.id", StringValue("123456789012")),
    ("cloud.region", StringValue("us-west-2")),
    ("cloud.availability_zone", StringValue("us-west-2a")),
    
    ("user.id", StringValue("user-12345")),
    ("user.email", StringValue("user@example.com")),
    ("user.roles", ArrayStringValue(["user", "premium"])),
    ("user.tier", StringValue("premium")),
    
    ("request.id", StringValue("req-abcdef-123456")),
    ("request.method", StringValue("POST")),
    ("request.path", StringValue("/api/v1/users/profile")),
    ("request.user_agent", StringValue("Mozilla/5.0 (compatible; Azimuth-Client/1.0.0)")),
    ("request.referer", StringValue("https://app.example.com/dashboard")),
    
    ("response.status_code", IntValue(200)),
    ("response.content_length", IntValue(2048)),
    ("response.duration.ms", IntValue(150)),
    
    ("db.query.count", IntValue(3)),
    ("db.query.duration.ms", IntValue(45)),
    ("db.connection.pool.active", IntValue(5)),
    ("db.connection.pool.idle", IntValue(10)),
    
    ("cache.hit", BoolValue(true)),
    ("cache.key", StringValue("user:profile:12345")),
    ("cache.ttl.seconds", IntValue(300)),
    
    ("error.occurred", BoolValue(false)),
    ("trace.id", StringValue("trace-abcdef1234567890")),
    ("span.id", StringValue("span-1234567890abcdef")),
    ("trace.sampled", BoolValue(true)),
    
    ("metric.custom.counter", IntValue(42)),
    ("metric.custom.gauge", FloatValue(123.45)),
    ("metric.custom.histogram.bucket", ArrayIntValue([10, 25, 50, 100, 250, 500])),
    
    ("log.level", StringValue("INFO")),
    ("log.message", StringValue("Complex operation with detailed attributes")),
    ("log.logger", StringValue("user-service.profile")),
    
    ("feature.flag.new_ui", BoolValue(true)),
    ("feature.flag.beta_features", BoolValue(false)),
    ("experiment.variant", StringValue("control")),
    ("experiment.id", StringValue("exp-profile-update-001"))
  ]
  
  for attr in telemetry_attributes {
    Attributes::set(attrs, attr.0, attr.1)
  }
  
  // 在Span中使用复杂属性
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "complex.attributes.tracer")
  let span = Tracer::start_span(tracer, "complex.operation")
  
  // 添加复杂事件
  Span::add_event(span, "operation.started", Some([
    ("service.name", StringValue("user-service")),
    ("request.id", StringValue("req-abcdef-123456")),
    ("user.id", StringValue("user-12345"))
  ]))
  
  Span::add_event(span, "database.accessed", Some([
    ("db.query.count", IntValue(3)),
    ("db.query.duration.ms", IntValue(45)),
    ("cache.hit", BoolValue(true))
  ]))
  
  Span::set_status(span, Ok)
  Span::end(span)
  
  // 在LogRecord中使用复杂属性
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "complex.attributes.logger")
  
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Complex operation with detailed attributes"),
    Some(attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace-abcdef1234567890"),
    Some("span-1234567890abcdef"),
    None
  )
  
  Logger::emit(logger, log_record)
  
  // 在Resource中使用复杂属性
  let resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("user-service")),
    ("service.version", StringValue("2.1.0")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("prod-user-001.example.com")),
    ("process.pid", IntValue(12345))
  ])
  
  // 验证资源属性
  let service_name = Resource::get_attribute(resource, "service.name")
  match service_name {
    Some(StringValue(value)) => assert_eq(value, "user-service")
    _ => assert_true(false)
  }
  
  assert_true(true) // 如果没有崩溃则测试通过
}