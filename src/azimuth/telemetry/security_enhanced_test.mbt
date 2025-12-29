// 安全性测试 - 测试恶意数据处理

test "malicious_attribute_value_handling" {
  // 测试恶意AttributeValue的处理
  
  // 1. SQL注入攻击测试
  let sql_injection_string = "'; DROP TABLE users; --"
  let sql_injection_attr = AttributeValue::string(sql_injection_string)
  
  match sql_injection_attr {
    StringValue(s) => {
      assert_eq(s, sql_injection_string)
      // 验证字符串被正确存储，没有执行SQL
      assert_eq(s.contains("DROP TABLE"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 2. XSS攻击测试
  let xss_string = "<script>alert('XSS')</script>"
  let xss_attr = AttributeValue::string(xss_string)
  
  match xss_attr {
    StringValue(s) => {
      assert_eq(s, xss_string)
      // 验证XSS代码被正确存储，没有执行
      assert_eq(s.contains("<script>"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 3. 路径遍历攻击测试
  let path_traversal_string = "../../../etc/passwd"
  let path_traversal_attr = AttributeValue::string(path_traversal_string)
  
  match path_traversal_attr {
    StringValue(s) => {
      assert_eq(s, path_traversal_string)
      // 验证路径遍历字符串被正确存储
      assert_eq(s.contains("../"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 4. 命令注入测试
  let command_injection_string = "rm -rf /"
  let command_injection_attr = AttributeValue::string(command_injection_string)
  
  match command_injection_attr {
    StringValue(s) => {
      assert_eq(s, command_injection_string)
      // 验证命令注入字符串被正确存储，没有执行
      assert_eq(s.contains("rm -rf"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 5. Unicode攻击测试
  let unicode_attack_string = "𝕳𝕰𝕷𝕷𝕺 𝕎𝕺𝕽𝕷𝕯"
  let unicode_attack_attr = AttributeValue::string(unicode_attack_string)
  
  match unicode_attack_attr {
    StringValue(s) => {
      assert_eq(s, unicode_attack_string)
      // 验证Unicode字符被正确存储
      assert_eq(s.contains("𝕳"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 6. 长字符串攻击测试（缓冲区溢出）
  let long_string = "A" * 1000000
  let long_string_attr = AttributeValue::string(long_string)
  
  match long_string_attr {
    StringValue(s) => {
      assert_eq(s.length(), 1000000)
      // 验证长字符串被正确存储
      assert_eq(s[0], "A")
      assert_eq(s[s.length() - 1], "A")
    }
    _ => @test.fail("Test failed")
  }
  
  // 7. 空字节注入测试
  let null_byte_string = "prefix\x00\x01\x02suffix"
  let null_byte_attr = AttributeValue::string(null_byte_string)
  
  match null_byte_attr {
    StringValue(s) => {
      assert_eq(s, null_byte_string)
      // 验证空字节被正确存储
      assert_eq(s.contains("\x00"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 8. 特殊字符攻击测试
  let special_chars_string = "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./\x00\x01\x02\x7f"
  let special_chars_attr = AttributeValue::string(special_chars_string)
  
  match special_chars_attr {
    StringValue(s) => {
      assert_eq(s, special_chars_string)
      // 验证特殊字符被正确存储
      assert_eq(s.contains("!@#$%"), true)
    }
    _ => @test.fail("Test failed")
  }
}

test "malicious_context_key_handling" {
  // 测试恶意Context Key的处理
  
  let ctx = Context::empty()
  
  // 1. SQL注入在键名中
  let sql_injection_key = create_key("'; DROP TABLE context; --")
  let ctx_with_sql = ctx.with_value(sql_injection_key, "safe_value")
  
  match ctx_with_sql.get(sql_injection_key) {
    Some(value) => assert_eq(value, "safe_value")
    None => @test.fail("Test failed")
  }
  
  // 2. XSS在键名中
  let xss_key = create_key("<script>alert('XSS')</script>")
  let ctx_with_xss = ctx_with_sql.with_value(xss_key, "xss_value")
  
  match ctx_with_xss.get(xss_key) {
    Some(value) => assert_eq(value, "xss_value")
    None => @test.fail("Test failed")
  }
  
  // 3. 路径遍历在键名中
  let path_traversal_key = create_key("../../../etc/context")
  let ctx_with_path = ctx_with_xss.with_value(path_traversal_key, "path_value")
  
  match ctx_with_path.get(path_traversal_key) {
    Some(value) => assert_eq(value, "path_value")
    None => @test.fail("Test failed")
  }
  
  // 4. Unicode攻击在键名中
  let unicode_key = create_key("𝕸𝕬𝕷𝕴𝕮𝕴𝕺𝖀𝖘_𝕶𝕰𝖄")
  let ctx_with_unicode = ctx_with_path.with_value(unicode_key, "unicode_value")
  
  match ctx_with_unicode.get(unicode_key) {
    Some(value) => assert_eq(value, "unicode_value")
    None => @test.fail("Test failed")
  }
  
  // 5. 长键名攻击
  let long_key_name = "A" * 10000
  let long_key = create_key(long_key_name)
  let ctx_with_long = ctx_with_unicode.with_value(long_key, "long_value")
  
  match ctx_with_long.get(long_key) {
    Some(value) => assert_eq(value, "long_value")
    None => @test.fail("Test failed")
  }
  
  // 6. 空字节在键名中
  let null_byte_key = create_key("key\x00\x01\x02injection")
  let ctx_with_null = ctx_with_long.with_value(null_byte_key, "null_value")
  
  match ctx_with_null.get(null_byte_key) {
    Some(value) => assert_eq(value, "null_value")
    None => @test.fail("Test failed")
  }
  
  // 7. 特殊字符在键名中
  let special_chars_key = create_key("!@#$%^&*()_+-={}[]|\\:;\"'<>?,./key")
  let ctx_with_special = ctx_with_null.with_value(special_chars_key, "special_value")
  
  match ctx_with_special.get(special_chars_key) {
    Some(value) => assert_eq(value, "special_value")
    None => @test.failed")
  }
  
  // 8. 验证所有恶意键都正确处理
  assert_eq(ctx_with_special.get(sql_injection_key).unwrap(), "safe_value")
  assert_eq(ctx_with_special.get(xss_key).unwrap(), "xss_value")
  assert_eq(ctx_with_special.get(path_traversal_key).unwrap(), "path_value")
  assert_eq(ctx_with_special.get(unicode_key).unwrap(), "unicode_value")
  assert_eq(ctx_with_special.get(long_key).unwrap(), "long_value")
  assert_eq(ctx_with_special.get(null_byte_key).unwrap(), "null_value")
  assert_eq(ctx_with_special.get(special_chars_key).unwrap(), "special_value")
}

test "malicious_span_name_handling" {
  // 测试恶意Span名称的处理
  
  let ctx = Context::empty()
  
  // 1. SQL注入在Span名称中
  let sql_injection_span_name = "'; DROP TABLE spans; --"
  let (_, sql_span) = NoopTracer::start_span(
    ctx,
    sql_injection_span_name,
    Server,
    Some([("test.type", AttributeValue::string("sql_injection"))])
  )
  
  assert_eq(sql_span.name, sql_injection_span_name)
  assert_eq(sql_span.name.contains("DROP TABLE"), true)
  
  // 2. XSS在Span名称中
  let xss_span_name = "<script>alert('XSS in span')</script>"
  let (_, xss_span) = NoopTracer::start_span(
    ctx,
    xss_span_name,
    Server,
    Some([("test.type", AttributeValue::string("xss"))])
  )
  
  assert_eq(xss_span.name, xss_span_name)
  assert_eq(xss_span.name.contains("<script>"), true)
  
  // 3. 长Span名称攻击
  let long_span_name = "A" * 100000
  let (_, long_span) = NoopTracer::start_span(
    ctx,
    long_span_name,
    Server,
    Some([("test.type", AttributeValue::string("long_name"))])
  )
  
  assert_eq(long_span.name.length(), 100000)
  assert_eq(long_span.name[0], "A")
  assert_eq(long_span.name[long_span.name.length() - 1], "A")
  
  // 4. Unicode攻击在Span名称中
  let unicode_span_name = "𝕾𝖕𝖆𝖆_𝖂𝖎𝖙𝖍_𝖀𝖚𝖓𝖎𝖈𝖔𝖉𝖊"
  let (_, unicode_span) = NoopTracer::start_span(
    ctx,
    unicode_span_name,
    Server,
    Some([("test.type", AttributeValue::string("unicode"))])
  )
  
  assert_eq(unicode_span.name, unicode_span_name)
  assert_eq(unicode_span.name.contains("𝕾"), true)
  
  // 5. 特殊字符在Span名称中
  let special_chars_span_name = "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./span"
  let (_, special_span) = NoopTracer::start_span(
    ctx,
    special_chars_span_name,
    Server,
    Some([("test.type", AttributeValue::string("special_chars"))])
  )
  
  assert_eq(special_span.name, special_chars_span_name)
  assert_eq(special_span.name.contains("!@#$%"), true)
  
  // 6. 空字节在Span名称中
  let null_byte_span_name = "span\x00\x01\x02injection"
  let (_, null_span) = NoopTracer::start_span(
    ctx,
    null_byte_span_name,
    Server,
    Some([("test.type", AttributeValue::string("null_bytes"))])
  )
  
  assert_eq(null_span.name, null_byte_span_name)
  assert_eq(null_span.name.contains("\x00"), true)
  
  // 7. 格式化字符串攻击
  let format_string_span_name = "%s%s%s%s%s%s%s%s"
  let (_, format_span) = NoopTracer::start_span(
    ctx,
    format_string_span_name,
    Server,
    Some([("test.type", AttributeValue::string("format_string"))])
  )
  
  assert_eq(format_span.name, format_string_span_name)
  assert_eq(format_span.name.contains("%s"), true)
}

test "malicious_log_record_handling" {
  // 测试恶意LogRecord的处理
  
  let logger = NoopLogger::{}
  
  // 1. SQL注入在日志消息中
  let sql_injection_log = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Error)
    .body("'; DROP TABLE logs; --")
    .with_attribute("attack.type", AttributeValue::string("sql_injection"))
    .build()
  
  logger.emit(sql_injection_log)
  
  match sql_injection_log.body {
    Some(message) => {
      assert_eq(message, "'; DROP TABLE logs; --")
      assert_eq(message.contains("DROP TABLE"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 2. XSS在日志消息中
  let xss_log = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Error)
    .body("<script>alert('XSS in log')</script>")
    .with_attribute("attack.type", AttributeValue::string("xss"))
    .build()
  
  logger.emit(xss_log)
  
  match xss_log.body {
    Some(message) => {
      assert_eq(message, "<script>alert('XSS in log')</script>")
      assert_eq(message.contains("<script>"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 3. 长日志消息攻击
  let long_message = "A" * 1000000
  let long_log = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Error)
    .body(long_message)
    .with_attribute("attack.type", AttributeValue::string("long_message"))
    .build()
  
  logger.emit(long_log)
  
  match long_log.body {
    Some(message) => {
      assert_eq(message.length(), 1000000)
      assert_eq(message[0], "A")
      assert_eq(message[message.length() - 1], "A")
    }
    None => @test.fail("Test failed")
  }
  
  // 4. 恶意属性在日志中
  let malicious_attributes = [
    ("sql.injection", AttributeValue::string("'; DROP TABLE attributes; --")),
    ("xss.attack", AttributeValue::string("<script>alert('XSS')</script>")),
    ("path.traversal", AttributeValue::string("../../../etc/passwd")),
    ("command.injection", AttributeValue::string("rm -rf /")),
    ("unicode.attack", AttributeValue::string("𝕸𝕬𝕷𝕴𝕮𝕴𝕺𝖀𝖘")),
    ("null.bytes", AttributeValue::string("prefix\x00\x01\x02suffix"))
  ]
  
  let malicious_log = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Error)
    .body("Log with malicious attributes")
    .with_attribute("attack.type", AttributeValue::string("malicious_attributes"))
    .build()
  
  // 手动创建包含恶意属性的LogRecord
  let full_malicious_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Error,
    severity_text: Some("MALICIOUS_LOG"),
    body: Some("Log with malicious attributes"),
    attributes: malicious_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  logger.emit(full_malicious_log)
  
  // 验证恶意属性被正确存储
  assert_eq(full_malicious_log.attributes.length(), 6)
  match full_malicious_log.attributes[0].1 {
    StringValue(value) => {
      assert_eq(value, "'; DROP TABLE attributes; --")
      assert_eq(value.contains("DROP TABLE"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  match full_malicious_log.attributes[1].1 {
    StringValue(value) => {
      assert_eq(value, "<script>alert('XSS')</script>")
      assert_eq(value.contains("<script>"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 5. 恶意严重性文本
  let malicious_severity_log = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Error)
    .severity_text("'; DROP TABLE severity; --")
    .body("Log with malicious severity text")
    .with_attribute("attack.type", AttributeValue::string("malicious_severity"))
    .build()
  
  logger.emit(malicious_severity_log)
  
  match malicious_severity_log.severity_text {
    Some(text) => {
      assert_eq(text, "'; DROP TABLE severity; --")
      assert_eq(text.contains("DROP TABLE"), true)
    }
    None => @test.fail("Test failed")
  }
}

test "malicious_baggage_handling" {
  // 测试恶意Baggage的处理
  
  // 1. SQL注入在Baggage条目中
  let sql_injection_baggage = Baggage::empty()
    .with_entry("'; DROP TABLE baggage; --", "sql_value")
    .with_entry("safe_key", "'; DROP TABLE baggage; --")
  
  match sql_injection_baggage.get("'; DROP TABLE baggage; --") {
    Some(value) => {
      assert_eq(value, "sql_value")
      assert_eq(value.contains("sql_value"), true)
    }
    None => @test.fail("Test failed")
  }
  
  match sql_injection_baggage.get("safe_key") {
    Some(value) => {
      assert_eq(value, "'; DROP TABLE baggage; --")
      assert_eq(value.contains("DROP TABLE"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 2. XSS在Baggage条目中
  let xss_baggage = Baggage::empty()
    .with_entry("<script>alert('XSS')</script>", "xss_value")
    .with_entry("xss_key", "<script>alert('XSS')</script>")
  
  match xss_baggage.get("<script>alert('XSS')</script>") {
    Some(value) => {
      assert_eq(value, "xss_value")
      assert_eq(value.contains("xss_value"), true)
    }
    None => @test.fail("Test failed")
  }
  
  match xss_baggage.get("xss_key") {
    Some(value) => {
      assert_eq(value, "<script>alert('XSS')</script>")
      assert_eq(value.contains("<script>"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 3. 长键值在Baggage中
  let long_key = "A" * 10000
  let long_value = "B" * 10000
  let long_baggage = Baggage::empty()
    .with_entry(long_key, long_value)
  
  match long_baggage.get(long_key) {
    Some(value) => {
      assert_eq(value.length(), 10000)
      assert_eq(value[0], "B")
      assert_eq(value[value.length() - 1], "B")
    }
    None => @test.fail("Test failed")
  }
  
  // 4. Unicode攻击在Baggage中
  let unicode_baggage = Baggage::empty()
    .with_entry("𝕸𝕬𝕷𝕴𝕮𝕴𝕺𝖀𝖘_𝕶𝕰𝖄", "𝕸𝕬𝕷𝕴𝕮𝕴𝕺𝖀𝖘_𝖁𝕬𝕷𝖀𝖀𝕴𝕰")
  
  match unicode_baggage.get("𝕸𝕬𝕷𝕴𝕮𝕴𝕺𝖀𝖘_𝕶𝕰𝖄") {
    Some(value) => {
      assert_eq(value, "𝕸𝕬𝕷𝕴𝕮𝕴𝕺𝖀𝖘_𝖁𝕬𝕷𝖀𝖀𝕴𝕰")
      assert_eq(value.contains("𝕸"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 5. 特殊字符在Baggage中
  let special_baggage = Baggage::empty()
    .with_entry("!@#$%^&*()_+-={}[]|\\:;\"'<>?,./key", "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./value")
  
  match special_baggage.get("!@#$%^&*()_+-={}[]|\\:;\"'<>?,./key") {
    Some(value) => {
      assert_eq(value, "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./value")
      assert_eq(value.contains("!@#$%"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 6. 空字节在Baggage中
  let null_byte_baggage = Baggage::empty()
    .with_entry("key\x00\x01\x02injection", "value\x00\x01\x02injection")
  
  match null_byte_baggage.get("key\x00\x01\x02injection") {
    Some(value) => {
      assert_eq(value, "value\x00\x01\x02injection")
      assert_eq(value.contains("\x00"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 7. 路径遍历在Baggage中
  let path_baggage = Baggage::empty()
    .with_entry("../../../etc/baggage", "../../../etc/passwd")
  
  match path_baggage.get("../../../etc/baggage") {
    Some(value) => {
      assert_eq(value, "../../../etc/passwd")
      assert_eq(value.contains("../"), true)
    }
    None => @test.fail("Test failed")
  }
}

test "malicious_propagation_data_handling" {
  // 测试恶意传播数据的处理
  
  // 1. 恶意traceparent header
  let malicious_traceparent = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01'; DROP TABLE traces; --"
  let malicious_carrier = MapCarrier::from_map([
    ("traceparent", malicious_traceparent),
    ("baggage", "user_id='; DROP TABLE users; --")
  ])
  
  let w3c_propagator = W3CTraceContextPropagator::{}
  let ctx = Context::empty()
  
  // 测试提取恶意traceparent不会崩溃
  let extracted_ctx = w3c_propagator.extract(ctx, malicious_carrier)
  assert_eq(true, true)  // 如果没有崩溃，测试通过
  
  // 2. 恶意baggage header
  let malicious_baggage = "user_id=<script>alert('XSS')</script>,session_id=../../../etc/passwd"
  let baggage_carrier = MapCarrier::from_map([
    ("baggage", malicious_baggage),
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  ])
  
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 测试提取恶意baggage不会崩溃
  let baggage_extracted_ctx = baggage_propagator.extract(ctx, baggage_carrier)
  assert_eq(true, true)  // 如果没有崩溃，测试通过
  
  // 3. 格式错误的traceparent
  let malformed_traceparents = [
    "invalid-traceparent-format",
    "00-not-hex-trace-id-b7ad6b7169203331-01",
    "00-0af7651916cd43dd8448eb211c80319c-not-hex-span-id-01",
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-not-hex-flags",
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331",  // 缺少flags
    "00-0af7651916cd43dd8448eb211c80319c",  // 缺少span_id和flags
    "00",  // 只有版本
    "",  // 空字符串
    "01-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01-extra-data"  // 额外数据
  ]
  
  let mut i = 0
  while i < malformed_traceparents.length() {
    let malformed_carrier = MapCarrier::from_map([
      ("traceparent", malformed_traceparents[i])
    ])
    
    // 测试格式错误的traceparent不会崩溃
    let malformed_extracted_ctx = w3c_propagator.extract(ctx, malformed_carrier)
    assert_eq(true, true)  // 如果没有崩溃，测试通过
    i = i + 1
  }
  
  // 4. 格式错误的baggage
  let malformed_baggages = [
    "malformed=baggage=format",
    "key=",
    "=value",
    "key",
    "key=value=with=too=many=equals",
    "key=value;invalid-properties",
    "key=value;property=",
    "key=value;=invalid-property",
    "key=value;property\x00with\x00nulls"
  ]
  
  let mut j = 0
  while j < malformed_baggages.length() {
    let malformed_baggage_carrier = MapCarrier::from_map([
      ("baggage", malformed_baggages[j])
    ])
    
    // 测试格式错误的baggage不会崩溃
    let malformed_baggage_extracted_ctx = baggage_propagator.extract(ctx, malformed_baggage_carrier)
    assert_eq(true, true)  // 如果没有崩溃，测试通过
    j = j + 1
  }
  
  // 5. 超长header
  let very_long_traceparent = "00-" + "a" * 32 + "-" + "b" * 16 + "-01"
  let very_long_baggage = "key=" + "c" * 10000
  
  let very_long_carrier = MapCarrier::from_map([
    ("traceparent", very_long_traceparent),
    ("baggage", very_long_baggage)
  ])
  
  // 测试超长header不会崩溃
  let very_long_extracted_ctx = w3c_propagator.extract(ctx, very_long_carrier)
  assert_eq(true, true)  // 如果没有崩溃，测试通过
  
  // 6. Unicode攻击header
  let unicode_traceparent = "00-𝕵𝖓𝖎𝖈𝖔𝖉𝖊𝕿𝖗𝖆𝖈𝖊𝕴𝖉-𝕴𝖓𝖎𝖈𝖔𝖉𝖊𝕾𝖕𝖆𝖓𝕴𝖉-01"
  let unicode_baggage = "𝖀𝖓𝖎𝖈𝖔𝖉𝖊𝕶𝖊𝖞=𝖀𝖓𝖎𝖈𝖔𝖉𝖊𝖁𝖆𝖑𝖚𝖊"
  
  let unicode_carrier = MapCarrier::from_map([
    ("traceparent", unicode_traceparent),
    ("baggage", unicode_baggage)
  ])
  
  // 测试Unicode header不会崩溃
  let unicode_extracted_ctx = w3c_propagator.extract(ctx, unicode_carrier)
  assert_eq(true, true)  // 如果没有崩溃，测试通过
  
  // 7. 复合传播器恶意数据处理
  let composite_propagator = CompositePropagator::new([w3c_propagator, baggage_propagator])
  let composite_malicious_carrier = MapCarrier::from_map([
    ("traceparent", malicious_traceparent),
    ("baggage", malicious_baggage),
    ("malicious-header", "malicious-value")
  ])
  
  // 测试复合传播器处理恶意数据不会崩溃
  let composite_extracted_ctx = composite_propagator.extract(ctx, composite_malicious_carrier)
  assert_eq(true, true)  // 如果没有崩溃，测试通过
}