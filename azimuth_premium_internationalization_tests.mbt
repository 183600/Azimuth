// Azimuth Premium Internationalization Tests
// 高质量国际化和本地化测试用例
// 专注于测试遥测系统在不同语言、地区和文化环境下的兼容性和正确性

// 测试1: 多语言字符串处理
test "多语言字符串处理测试" {
  // 1. 测试中文字符串处理
  let chinese_attr = AttributeValue::StringValue("遥测数据处理系统")
  match chinese_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "遥测数据处理系统")
      assert_eq(s.length(), 6)
      assert_true(s.contains("遥测"))
      assert_true(s.contains("系统"))
    }
    _ => assert_true(false, "中文字符串类型转换失败")
  }
  
  // 2. 测试日文字符串处理
  let japanese_attr = AttributeValue::StringValue("テレメトリデータ処理システム")
  match japanese_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "テレメトリデータ処理システム")
      assert_eq(s.length(), 12)
      assert_true(s.contains("テレメトリ"))
      assert_true(s.contains("システム"))
    }
    _ => assert_true(false, "日文字符串类型转换失败")
  }
  
  // 3. 测试韩文字符串处理
  let korean_attr = AttributeValue::StringValue("원격 측정 데이터 처리 시스템")
  match korean_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "원격 측정 데이터 처리 시스템")
      assert_eq(s.length(), 12)
      assert_true(s.contains("원격"))
      assert_true(s.contains("시스템"))
    }
    _ => assert_true(false, "韩文字符串类型转换失败")
  }
  
  // 4. 测试阿拉伯文字符串处理
  let arabic_attr = AttributeValue::StringValue("نظام معالجة بيانات القياس عن بعد")
  match arabic_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "نظام معالجة بيانات القياس عن بعد")
      assert_eq(s.length(), 26)
      assert_true(s.contains("نظام"))
      assert_true(s.contains("معالجة"))
    }
    _ => assert_true(false, "阿拉伯文字符串类型转换失败")
  }
  
  // 5. 测试俄文字符串处理
  let russian_attr = AttributeValue::StringValue("Система обработки телеметрических данных")
  match russian_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "Система обработки телеметрических данных")
      assert_eq(s.length(), 36)
      assert_true(s.contains("Система"))
      assert_true(s.contains("обработки"))
    }
    _ => assert_true(false, "俄文字符串类型转换失败")
  }
  
  // 6. 测试混合语言字符串处理
  let mixed_attr = AttributeValue::StringValue("遥测系统 Telemetry System تيليمتري نظام")
  match mixed_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "遥测系统 Telemetry System تيليمتري نظام")
      assert_eq(s.length(), 23)
      assert_true(s.contains("遥测系统"))
      assert_true(s.contains("Telemetry"))
      assert_true(s.contains("تيليمتري"))
    }
    _ => assert_true(false, "混合语言字符串类型转换失败")
  }
  
  // 7. 测试包含表情符号的字符串处理
  let emoji_attr = AttributeValue::StringValue("遥测系统 🌟📊🔄 数据处理")
  match emoji_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "遥测系统 🌟📊🔄 数据处理")
      assert_eq(s.length(), 11)
      assert_true(s.contains("🌟"))
      assert_true(s.contains("📊"))
      assert_true(s.contains("🔄"))
    }
    _ => assert_true(false, "表情符号字符串类型转换失败")
  }
}

// 测试2: 多语言环境下的Baggage处理
test "多语言环境下的Baggage处理测试" {
  // 1. 创建包含多语言键值对的Baggage
  let multilingual_baggage = Baggage({
    entries: [
      ("用户.id", "user123"),
      ("user.id", "user123"),
      ("ユーザー.id", "user123"),
      ("사용자.id", "user123"),
      ("معرف.المستخدم", "user123"),
      ("идентификатор.пользователя", "user123"),
      ("操作.名称", "数据处理"),
      ("operation.name", "data_processing"),
      ("操作名", "データ処理"),
      ("작업.이름", "데이터 처리"),
      ("اسم.العملية", "معالجة البيانات"),
      ("название.операции", "обработка данных")
    ]
  })
  
  // 2. 验证多语言Baggage条目数量
  assert_eq(multilingual_baggage.entries.length(), 12)
  
  // 3. 验证中文键值对
  let mut chinese_user_found = false
  let mut chinese_operation_found = false
  
  for (key, value) in multilingual_baggage.entries {
    match key {
      "用户.id" => {
        assert_eq(value, "user123")
        chinese_user_found = true
      }
      "操作.名称" => {
        assert_eq(value, "数据处理")
        chinese_operation_found = true
      }
      _ => () // 忽略其他键
    }
  }
  
  assert_true(chinese_user_found, "中文用户ID键应该存在")
  assert_true(chinese_operation_found, "中文操作名称键应该存在")
  
  // 4. 验证日文键值对
  let mut japanese_user_found = false
  let mut japanese_operation_found = false
  
  for (key, value) in multilingual_baggage.entries {
    match key {
      "ユーザー.id" => {
        assert_eq(value, "user123")
        japanese_user_found = true
      }
      "操作名" => {
        assert_eq(value, "データ処理")
        japanese_operation_found = true
      }
      _ => () // 忽略其他键
    }
  }
  
  assert_true(japanese_user_found, "日文用户ID键应该存在")
  assert_true(japanese_operation_found, "日文操作名称键应该存在")
  
  // 5. 验证韩文键值对
  let mut korean_user_found = false
  let mut korean_operation_found = false
  
  for (key, value) in multilingual_baggage.entries {
    match key {
      "사용자.id" => {
        assert_eq(value, "user123")
        korean_user_found = true
      }
      "작업.이름" => {
        assert_eq(value, "데이터 처리")
        korean_operation_found = true
      }
      _ => () // 忽略其他键
    }
  }
  
  assert_true(korean_user_found, "韩文用户ID键应该存在")
  assert_true(korean_operation_found, "韩文操作名称键应该存在")
  
  // 6. 验证阿拉伯文键值对
  let mut arabic_user_found = false
  let mut arabic_operation_found = false
  
  for (key, value) in multilingual_baggage.entries {
    match key {
      "معرف.المستخدم" => {
        assert_eq(value, "user123")
        arabic_user_found = true
      }
      "اسم.العملية" => {
        assert_eq(value, "معالجة البيانات")
        arabic_operation_found = true
      }
      _ => () // 忽略其他键
    }
  }
  
  assert_true(arabic_user_found, "阿拉伯文用户ID键应该存在")
  assert_true(arabic_operation_found, "阿拉伯文操作名称键应该存在")
  
  // 7. 验证俄文键值对
  let mut russian_user_found = false
  let mut russian_operation_found = false
  
  for (key, value) in multilingual_baggage.entries {
    match key {
      "идентификатор.пользователя" => {
        assert_eq(value, "user123")
        russian_user_found = true
      }
      "название.операции" => {
        assert_eq(value, "обработка данных")
        russian_operation_found = true
      }
      _ => () // 忽略其他键
    }
  }
  
  assert_true(russian_user_found, "俄文用户ID键应该存在")
  assert_true(russian_operation_found, "俄文操作名称键应该存在")
}

// 测试3: 多语言环境下的TextMapCarrier处理
test "多语言环境下的TextMapCarrier处理测试" {
  // 1. 创建包含多语言header的TextMapCarrier
  let multilingual_carrier = TextMapCarrier({
    headers: [
      ("traceparent", "00-12345678901234567890123456789012-1234567890123456-01"),
      ("tracestate", "服务=遥测系统,操作=数据处理"),
      ("baggage", "用户.姓名=张三,操作.类型=数据收集"),
      ("x-请求-id", "req-123456"),
      ("x-request-id", "req-123456"),
      ("x-リクエスト-id", "req-123456"),
      ("x-요청-id", "req-123456"),
      ("x-معرف-الطلب", "req-123456"),
      ("x-идентификатор-запроса", "req-123456"),
      ("用户-代理", "遥测客户端/1.0"),
      ("user-agent", "telemetry-client/1.0"),
      ("ユーザーエージェント", "テレメトリクライアント/1.0"),
      ("사용자-에이전트", "원격 측정 클라이언트/1.0"),
      ("وكيل-المستخدم", "عميل القياس عن بعد/1.0"),
      ("агент-пользователя", "клиент телеметрии/1.0")
    ]
  })
  
  // 2. 验证多语言TextMapCarrier条目数量
  assert_eq(multilingual_carrier.headers.length(), 14)
  
  // 3. 验证中文header
  let mut chinese_request_id_found = false
  let mut chinese_user_agent_found = false
  
  for (key, value) in multilingual_carrier.headers {
    match key {
      "x-请求-id" => {
        assert_eq(value, "req-123456")
        chinese_request_id_found = true
      }
      "用户-代理" => {
        assert_eq(value, "遥测客户端/1.0")
        chinese_user_agent_found = true
      }
      _ => () // 忽略其他键
    }
  }
  
  assert_true(chinese_request_id_found, "中文请求ID header应该存在")
  assert_true(chinese_user_agent_found, "中文用户代理header应该存在")
  
  // 4. 验证日文header
  let mut japanese_request_id_found = false
  let mut japanese_user_agent_found = false
  
  for (key, value) in multilingual_carrier.headers {
    match key {
      "x-リクエスト-id" => {
        assert_eq(value, "req-123456")
        japanese_request_id_found = true
      }
      "ユーザーエージェント" => {
        assert_eq(value, "テレメトリクライアント/1.0")
        japanese_user_agent_found = true
      }
      _ => () // 忽略其他键
    }
  }
  
  assert_true(japanese_request_id_found, "日文请求ID header应该存在")
  assert_true(japanese_user_agent_found, "日文用户代理header应该存在")
  
  // 5. 验证韩文header
  let mut korean_request_id_found = false
  let mut korean_user_agent_found = false
  
  for (key, value) in multilingual_carrier.headers {
    match key {
      "x-요청-id" => {
        assert_eq(value, "req-123456")
        korean_request_id_found = true
      }
      "사용자-에이전트" => {
        assert_eq(value, "원격 측정 클라이언트/1.0")
        korean_user_agent_found = true
      }
      _ => () // 忽略其他键
    }
  }
  
  assert_true(korean_request_id_found, "韩文请求ID header应该存在")
  assert_true(korean_user_agent_found, "韩文用户代理header应该存在")
  
  // 6. 验证阿拉伯文header
  let mut arabic_request_id_found = false
  let mut arabic_user_agent_found = false
  
  for (key, value) in multilingual_carrier.headers {
    match key {
      "x-معرف-الطلب" => {
        assert_eq(value, "req-123456")
        arabic_request_id_found = true
      }
      "وكيل-المستخدم" => {
        assert_eq(value, "عميل القياس عن بعد/1.0")
        arabic_user_agent_found = true
      }
      _ => () // 忽略其他键
    }
  }
  
  assert_true(arabic_request_id_found, "阿拉伯文请求ID header应该存在")
  assert_true(arabic_user_agent_found, "阿拉伯文用户代理header应该存在")
  
  // 7. 验证俄文header
  let mut russian_request_id_found = false
  let mut russian_user_agent_found = false
  
  for (key, value) in multilingual_carrier.headers {
    match key {
      "x-идентификатор-запроса" => {
        assert_eq(value, "req-123456")
        russian_request_id_found = true
      }
      "агент-пользователя" => {
        assert_eq(value, "клиент телеметрии/1.0")
        russian_user_agent_found = true
      }
      _ => () // 忽略其他键
    }
  }
  
  assert_true(russian_request_id_found, "俄文请求ID header应该存在")
  assert_true(russian_user_agent_found, "俄文用户代理header应该存在")
}

// 测试4: 多语言环境下的InstrumentationScope处理
test "多语言环境下的InstrumentationScope处理测试" {
  // 1. 创建中文环境下的InstrumentationScope
  let chinese_scope = InstrumentationScope({
    name: "遥测.数据处理.模块",
    version: Some("1.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/v1.20.0")
  })
  
  // 验证中文InstrumentationScope
  assert_eq(chinese_scope.name, "遥测.数据处理.模块")
  assert_true(chinese_scope.name.contains("遥测"))
  assert_true(chinese_scope.name.contains("数据处理"))
  assert_true(chinese_scope.name.contains("模块"))
  
  // 2. 创建日文环境下的InstrumentationScope
  let japanese_scope = InstrumentationScope({
    name: "テレメトリ.データ処理.モジュール",
    version: Some("1.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/v1.20.0")
  })
  
  // 验证日文InstrumentationScope
  assert_eq(japanese_scope.name, "テレメトリ.データ処理.モジュール")
  assert_true(japanese_scope.name.contains("テレメトリ"))
  assert_true(japanese_scope.name.contains("データ処理"))
  assert_true(japanese_scope.name.contains("モジュール"))
  
  // 3. 创建韩文环境下的InstrumentationScope
  let korean_scope = InstrumentationScope({
    name: "원격측정.데이터처리.모듈",
    version: Some("1.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/v1.20.0")
  })
  
  // 验证韩文InstrumentationScope
  assert_eq(korean_scope.name, "원격측정.데이터처리.모듈")
  assert_true(korean_scope.name.contains("원격측정"))
  assert_true(korean_scope.name.contains("데이터처리"))
  assert_true(korean_scope.name.contains("모듈"))
  
  // 4. 创建阿拉伯文环境下的InstrumentationScope
  let arabic_scope = InstrumentationScope({
    name: "القياس.عن.بعد.معالجة.البيانات.وحدة",
    version: Some("1.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/v1.20.0")
  })
  
  // 验证阿拉伯文InstrumentationScope
  assert_eq(arabic_scope.name, "القياس.عن.بعد.معالجة.البيانات.وحدة")
  assert_true(arabic_scope.name.contains("القياس"))
  assert_true(arabic_scope.name.contains("معالجة"))
  assert_true(arabic_scope.name.contains("وحدة"))
  
  // 5. 创建俄文环境下的InstrumentationScope
  let russian_scope = InstrumentationScope({
    name: "телеметрия.обработка.данных.модуль",
    version: Some("1.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/v1.20.0")
  })
  
  // 验证俄文InstrumentationScope
  assert_eq(russian_scope.name, "телеметрия.обработка.данных.модуль")
  assert_true(russian_scope.name.contains("телеметрия"))
  assert_true(russian_scope.name.contains("обработка"))
  assert_true(russian_scope.name.contains("модуль"))
  
  // 6. 验证所有scope的版本和schema URL一致性
  let scopes = [chinese_scope, japanese_scope, korean_scope, arabic_scope, russian_scope]
  
  for scope in scopes {
    match scope.version {
      Some(v) => assert_eq(v, "1.0.0")
      None => assert_true(false, "版本不应为None")
    }
    
    match scope.schema_url {
      Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/v1.20.0")
      None => assert_true(false, "Schema URL不应为None")
    }
  }
}

// 测试5: 多语言环境下的Context处理
test "多语言环境下的Context处理测试" {
  // 1. 创建中文环境下的Context
  let chinese_context = Context({
    data: Some(("操作.名称", "数据收集与分析"))
  })
  
  match chinese_context.data {
    Some((key, value)) => {
      assert_eq(key, "操作.名称")
      assert_eq(value, "数据收集与分析")
      assert_true(key.contains("操作"))
      assert_true(value.contains("数据收集"))
    }
    None => assert_true(false, "中文Context数据不应为None")
  }
  
  // 2. 创建日文环境下的Context
  let japanese_context = Context({
    data: Some(("操作.名前", "データ収集と分析"))
  })
  
  match japanese_context.data {
    Some((key, value)) => {
      assert_eq(key, "操作.名前")
      assert_eq(value, "データ収集と分析")
      assert_true(key.contains("操作"))
      assert_true(value.contains("データ収集"))
    }
    None => assert_true(false, "日文Context数据不应为None")
  }
  
  // 3. 创建韩文环境下的Context
  let korean_context = Context({
    data: Some(("작업.이름", "데이터 수집 및 분석"))
  })
  
  match korean_context.data {
    Some((key, value)) => {
      assert_eq(key, "작업.이름")
      assert_eq(value, "데이터 수집 및 분석")
      assert_true(key.contains("작업"))
      assert_true(value.contains("데이터 수집"))
    }
    None => assert_true(false, "韩文Context数据不应为None")
  }
  
  // 4. 创建阿拉伯文环境下的Context
  let arabic_context = Context({
    data: Some(("اسم.العملية", "جمع وتحليل البيانات"))
  })
  
  match arabic_context.data {
    Some((key, value)) => {
      assert_eq(key, "اسم.العملية")
      assert_eq(value, "جمع وتحليل البيانات")
      assert_true(key.contains("عملية"))
      assert_true(value.contains("جمع"))
    }
    None => assert_true(false, "阿拉伯文Context数据不应为None")
  }
  
  // 5. 创建俄文环境下的Context
  let russian_context = Context({
    data: Some(("название.операции", "сбор и анализ данных"))
  })
  
  match russian_context.data {
    Some((key, value)) => {
      assert_eq(key, "название.операции")
      assert_eq(value, "сбор и анализ данных")
      assert_true(key.contains("операции"))
      assert_true(value.contains("сбор"))
    }
    None => assert_true(false, "俄文Context数据不应为None")
  }
  
  // 6. 创建包含混合语言的Context
  let mixed_context = Context({
    data: Some(("operation.name", "遥测操作 テレメトリー 操作 원격 측定 قياس عن بعد телеметрия"))
  })
  
  match mixed_context.data {
    Some((key, value)) => {
      assert_eq(key, "operation.name")
      assert_true(value.contains("遥测操作"))
      assert_true(value.contains("テレメトリー"))
      assert_true(value.contains("원격 측정"))
      assert_true(value.contains("قياس عن بعد"))
      assert_true(value.contains("телеметрия"))
    }
    None => assert_true(false, "混合语言Context数据不应为None")
  }
}

// 测试6: 区域特定格式处理
test "区域特定格式处理测试" {
  // 1. 测试不同区域的日期时间格式
  let chinese_date_attr = AttributeValue::StringValue("2023年1月1日 00:00:00")
  match chinese_date_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "2023年1月1日 00:00:00")
      assert_true(s.contains("年"))
      assert_true(s.contains("月"))
      assert_true(s.contains("日"))
    }
    _ => assert_true(false, "中文日期格式转换失败")
  }
  
  let japanese_date_attr = AttributeValue::StringValue("2023年1月1日 00:00:00")
  match japanese_date_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "2023年1月1日 00:00:00")
      assert_true(s.contains("年"))
      assert_true(s.contains("月"))
      assert_true(s.contains("日"))
    }
    _ => assert_true(false, "日文日期格式转换失败")
  }
  
  let korean_date_attr = AttributeValue::StringValue("2023년 1월 1일 00:00:00")
  match korean_date_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "2023년 1월 1일 00:00:00")
      assert_true(s.contains("년"))
      assert_true(s.contains("월"))
      assert_true(s.contains("일"))
    }
    _ => assert_true(false, "韩文日期格式转换失败")
  }
  
  let arabic_date_attr = AttributeValue::StringValue("١ يناير ٢٠٢٣ ٠٠:٠٠:٠٠")
  match arabic_date_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "١ يناير ٢٠٢٣ ٠٠:٠٠:٠٠")
      assert_true(s.contains("يناير"))
      assert_true(s.contains("٢٠٢٣"))
    }
    _ => assert_true(false, "阿拉伯文日期格式转换失败")
  }
  
  let russian_date_attr = AttributeValue::StringValue("1 января 2023 г. 00:00:00")
  match russian_date_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "1 января 2023 г. 00:00:00")
      assert_true(s.contains("января"))
      assert_true(s.contains("2023"))
    }
    _ => assert_true(false, "俄文日期格式转换失败")
  }
  
  // 2. 测试不同区域的数字格式
  let western_number_attr = AttributeValue::StringValue("1,234,567.89")
  match western_number_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "1,234,567.89")
      assert_true(s.contains(","))
      assert_true(s.contains("."))
    }
    _ => assert_true(false, "西方数字格式转换失败")
  }
  
  let chinese_number_attr = AttributeValue::StringValue("1,234,567.89")
  match chinese_number_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "1,234,567.89")
      assert_true(s.contains(","))
      assert_true(s.contains("."))
    }
    _ => assert_true(false, "中文数字格式转换失败")
  }
  
  let european_number_attr = AttributeValue::StringValue("1.234.567,89")
  match european_number_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "1.234.567,89")
      assert_true(s.contains("."))
      assert_true(s.contains(","))
    }
    _ => assert_true(false, "欧洲数字格式转换失败")
  }
  
  let arabic_number_attr = AttributeValue::StringValue("١٬٢٣٤٬٥٦٧٫٨٩")
  match arabic_number_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "١٬٢٣٤٬٥٦٧٫٨٩")
      assert_true(s.contains("١"))
      assert_true(s.contains("٨٩"))
    }
    _ => assert_true(false, "阿拉伯文数字格式转换失败")
  }
  
  // 3. 测试不同区域的货币格式
  let usd_currency_attr = AttributeValue::StringValue("$1,234.56")
  match usd_currency_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "$1,234.56")
      assert_true(s.contains("$"))
      assert_true(s.contains(","))
    }
    _ => assert_true(false, "美元货币格式转换失败")
  }
  
  let eur_currency_attr = AttributeValue::StringValue("1.234,56 €")
  match eur_currency_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "1.234,56 €")
      assert_true(s.contains("€"))
      assert_true(s.contains(","))
    }
    _ => assert_true(false, "欧元货币格式转换失败")
  }
  
  let cny_currency_attr = AttributeValue::StringValue("¥1,234.56")
  match cny_currency_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "¥1,234.56")
      assert_true(s.contains("¥"))
      assert_true(s.contains(","))
    }
    _ => assert_true(false, "人民币货币格式转换失败")
  }
  
  let jpy_currency_attr = AttributeValue::StringValue("¥1,235")
  match jpy_currency_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "¥1,235")
      assert_true(s.contains("¥"))
      assert_false(s.contains("."))
    }
    _ => assert_true(false, "日元货币格式转换失败")
  }
  
  // 4. 创建包含区域特定格式的Baggage
  let regional_baggage = Baggage({
    entries: [
      ("transaction.amount.usd", "$1,234.56"),
      ("transaction.amount.eur", "1.234,56 €"),
      ("transaction.amount.cny", "¥1,234.56"),
      ("transaction.amount.jpy", "¥1,235"),
      ("transaction.date.us", "01/01/2023"),
      ("transaction.date.eu", "01.01.2023"),
      ("transaction.date.cn", "2023年1月1日"),
      ("transaction.date.jp", "2023/01/01"),
      ("transaction.date.kr", "2023. 01. 01."),
      ("transaction.date.ar", "١/١/٢٠٢٣"),
      ("transaction.date.ru", "01.01.2023")
    ]
  })
  
  // 5. 验证区域特定格式的Baggage
  assert_eq(regional_baggage.entries.length(), 11)
  
  let mut usd_found = false
  let mut eur_found = false
  let mut cny_found = false
  let mut jpy_found = false
  let mut cn_date_found = false
  
  for (key, value) in regional_baggage.entries {
    match key {
      "transaction.amount.usd" => {
        assert_eq(value, "$1,234.56")
        usd_found = true
      }
      "transaction.amount.eur" => {
        assert_eq(value, "1.234,56 €")
        eur_found = true
      }
      "transaction.amount.cny" => {
        assert_eq(value, "¥1,234.56")
        cny_found = true
      }
      "transaction.amount.jpy" => {
        assert_eq(value, "¥1,235")
        jpy_found = true
      }
      "transaction.date.cn" => {
        assert_eq(value, "2023年1月1日")
        cn_date_found = true
      }
      _ => () // 忽略其他键
    }
  }
  
  assert_true(usd_found, "美元格式应该存在")
  assert_true(eur_found, "欧元格式应该存在")
  assert_true(cny_found, "人民币格式应该存在")
  assert_true(jpy_found, "日元格式应该存在")
  assert_true(cn_date_found, "中文日期格式应该存在")
}