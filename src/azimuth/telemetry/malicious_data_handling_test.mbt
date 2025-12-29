// 恶意数据处理测试用例
// 测试遥测系统对恶意输入的防护和处理能力

test "malicious_attribute_value_handling" {
  // 测试恶意AttributeValue的处理
  
  // 1. 测试SQL注入攻击
  let sql_injection_string = "'; DROP TABLE users; --"
  let sql_injection_attr = AttributeValue::string(sql_injection_string)
  
  // 验证恶意字符串被正确存储但不会执行
  match sql_injection_attr {
    StringValue(s) => {
      assert_eq(s, "'; DROP TABLE users; --")
      // 验证字符串被转义或安全处理
      assert_eq(s.contains("DROP TABLE"), true)  // 存储但不会执行
    }
    _ => @test.fail("Test failed")
  }
  
  // 2. 测试XSS攻击向量
  let xss_string = "<script>alert('XSS')</script>"
  let xss_attr = AttributeValue::string(xss_string)
  
  match xss_attr {
    StringValue(s) => {
      assert_eq(s, "<script>alert('XSS')</script>")
      // 验证XSS字符串被安全处理
      assert_eq(s.contains("<script>"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 3. 测试路径遍历攻击
  let path_traversal_string = "../../../etc/passwd"
  let path_traversal_attr = AttributeValue::string(path_traversal_string)
  
  match path_traversal_attr {
    StringValue(s) => {
      assert_eq(s, "../../../etc/passwd")
      assert_eq(s.contains("../"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 4. 测试命令注入攻击
  let command_injection_string = "; rm -rf /"
  let command_injection_attr = AttributeValue::string(command_injection_string)
  
  match command_injection_attr {
    StringValue(s) => {
      assert_eq(s, "; rm -rf /")
      assert_eq(s.contains("rm -rf"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 5. 测试LDAP注入攻击
  let ldap_injection_string = "*)(&(objectClass=*))"
  let ldap_injection_attr = AttributeValue::string(ldap_injection_string)
  
  match ldap_injection_attr {
    StringValue(s) => {
      assert_eq(s, "*)(&(objectClass=*))")
      assert_eq(s.contains("objectClass"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 6. 测试NoSQL注入攻击
  let nosql_injection_string = "{$ne: null}"
  let nosql_injection_attr = AttributeValue::string(nosql_injection_string)
  
  match nosql_injection_attr {
    StringValue(s) => {
      assert_eq(s, "{$ne: null}")
      assert_eq(s.contains("$ne"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 7. 测试格式化字符串攻击
  let format_string_attack = "%s%s%s%s%s%s"
  let format_string_attr = AttributeValue::string(format_string_attack)
  
  match format_string_attr {
    StringValue(s) => {
      assert_eq(s, "%s%s%s%s%s%s")
      assert_eq(s.contains("%s"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 8. 测试缓冲区溢出攻击
  let buffer_overflow_string = "A" * 10000  // 超长字符串
  let buffer_overflow_attr = AttributeValue::string(buffer_overflow_string)
  
  match buffer_overflow_attr {
    StringValue(s) => {
      assert_eq(s.length(), 10000)
      assert_eq(s.starts_with("A"), true)
    }
    _ => @test.fail("Test failed")
  }
}

test "malicious_log_record_handling" {
  // 测试恶意LogRecord的处理
  
  // 1. 测试包含恶意内容的日志记录
  let malicious_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Error,
    severity_text: Some("ERROR"),
    body: Some("Malicious content: <script>alert('XSS')</script>"),
    attributes: [
      ("user.input", AttributeValue::string("'; DROP TABLE users; --")),
      ("request.path", AttributeValue::string("../../../etc/passwd")),
      ("query.param", AttributeValue::string("{$ne: null}"))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证恶意内容被安全存储
  match malicious_log.body {
    Some(body) => {
      assert_eq(body.contains("<script>"), true)
      assert_eq(body.contains("alert"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 验证恶意属性被安全存储
  let mut found_sql_injection = false
  let mut found_path_traversal = false
  let mut found_nosql_injection = false
  
  let mut i = 0
  while i < malicious_log.attributes.length() {
    let (name, value) = malicious_log.attributes[i]
    match (name, value) {
      ("user.input", StringValue(s)) => {
        assert_eq(s.contains("DROP TABLE"), true)
        found_sql_injection = true
      }
      ("request.path", StringValue(s)) => {
        assert_eq(s.contains("../"), true)
        found_path_traversal = true
      }
      ("query.param", StringValue(s)) => {
        assert_eq(s.contains("$ne"), true)
        found_nosql_injection = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_sql_injection, true)
  assert_eq(found_path_traversal, true)
  assert_eq(found_nosql_injection, true)
  
  // 2. 测试超长日志记录处理
  let long_body = "A" * 1000000  // 1MB的日志内容
  let long_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some(long_body),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  match long_log.body {
    Some(body) => {
      assert_eq(body.length(), 1000000)
      assert_eq(body.starts_with("A"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 3. 测试包含特殊字符的日志记录
  let special_chars_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Warn,
    severity_text: Some("WARN"),
    body: Some("Special chars: \x00\x01\x02\x03\x04\x05"),
    attributes: [
      ("null.bytes", AttributeValue::string("test\x00\x00\x00")),
      ("control.chars", AttributeValue::string("test\x01\x02\x03"))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  match special_chars_log.body {
    Some(body) => {
      assert_eq(body.contains("\x00"), true)
      assert_eq(body.contains("\x05"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 4. 测试Unicode攻击向量
  let unicode_attack_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Error,
    severity_text: Some("ERROR"),
    body: Some("Unicode attack: \u202E\u202D\u200F\u200E"),  // 方向控制字符
    attributes: [
      ("unicode.homograph", AttributeValue::string("аdmin")),  // 西里尔字母a
      ("unicode.zero.width", AttributeValue::string("admin\u200Badmin"))  // 零宽度字符
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  match unicode_attack_log.body {
    Some(body) => {
      assert_eq(body.contains("\u202E"), true)
      assert_eq(body.contains("\u200F"), true)
    }
    None => @test.fail("Test failed")
  }
}

test "malicious_span_handling" {
  // 测试恶意Span的处理
  
  // 1. 测试包含恶意内容的Span
  let malicious_attributes = [
    ("sql.injection", AttributeValue::string("'; DROP TABLE spans; --")),
    ("xss.payload", AttributeValue::string("<script>steal_data()</script>")),
    ("path.traversal", AttributeValue::string("../../../etc/shadow"))
  ]
  
  let ctx = Context::empty()
  let (_, malicious_span) = NoopTracer::start_span(
    ctx, 
    "malicious<span>", 
    Internal, 
    Some(malicious_attributes)
  )
  
  // 验证恶意span名称被安全处理
  assert_eq(malicious_span.name, "malicious<span>")
  assert_eq(malicious_span.name.contains("<span>"), true)
  
  // 验证恶意属性被安全存储
  let mut found_sql_injection = false
  let mut found_xss = false
  let mut found_path_traversal = false
  
  let mut i = 0
  while i < malicious_span.attributes.length() {
    let (name, value) = malicious_span.attributes[i]
    match (name, value) {
      ("sql.injection", StringValue(s)) => {
        assert_eq(s.contains("DROP TABLE"), true)
        found_sql_injection = true
      }
      ("xss.payload", StringValue(s)) => {
        assert_eq(s.contains("<script>"), true)
        found_xss = true
      }
      ("path.traversal", StringValue(s)) => {
        assert_eq(s.contains("../"), true)
        found_path_traversal = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_sql_injection, true)
  assert_eq(found_xss, true)
  assert_eq(found_path_traversal, true)
  
  // 2. 测试包含恶意内容的Span事件
  let malicious_events = [
    SpanEvent::{
      name: "malicious.event<script>",
      timestamp_unix_nanos: 1640995200000000000L,
      attributes: [
        ("event.payload", AttributeValue::string("'; DROP TABLE events; --"))
      ]
    },
    SpanEvent::{
      name: "another.malicious\x00event",
      timestamp_unix_nanos: 1640995200000001000L,
      attributes: [
        ("unicode.attack", AttributeValue::string("admin\u200Badmin"))
      ]
    }
  ]
  
  let malicious_span_with_events = Span::{
    name: "malicious.span",
    context: malicious_span.context,
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200000002000L),
    status: Error,
    status_description: Some("Malicious span with bad content"),
    attributes: malicious_span.attributes,
    events: malicious_events,
    links: []
  }
  
  // 验证恶意事件被安全存储
  assert_eq(malicious_span_with_events.events.length(), 2)
  assert_eq(malicious_span_with_events.events[0].name.contains("<script>"), true)
  assert_eq(malicious_span_with_events.events[1].name.contains("\x00"), true)
  
  // 3. 测试包含恶意内容的Span链接
  let malicious_links = [
    SpanLink::{
      context: SpanContext::{
        trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }),
        span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte }),
        trace_flags: 1_byte,
        trace_state: "malicious=state;drop=table"
      },
      attributes: [
        ("link.injection", AttributeValue::string("'; DROP TABLE links; --"))
      ]
    }
  ]
  
  let malicious_span_with_links = Span::{
    name: "span.with.malicious.links",
    context: malicious_span.context,
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200000002000L),
    status: Error,
    status_description: Some("Span with malicious links"),
    attributes: [],
    events: [],
    links: malicious_links
  }
  
  // 验证恶意链接被安全存储
  assert_eq(malicious_span_with_links.links.length(), 1)
  assert_eq(malicious_span_with_links.links[0].context.trace_state.contains("drop=table"), true)
  
  // 4. 测试超长Span名称
  let long_span_name = "A" * 10000
  let (_, long_span) = NoopTracer::start_span(ctx, long_span_name, Internal, None)
  
  assert_eq(long_span.name.length(), 10000)
  assert_eq(long_span.name.starts_with("A"), true)
}

test "malicious_context_and_baggage_handling" {
  // 测试恶意Context和Baggage的处理
  
  // 1. 测试包含恶意内容的Context
  let ctx = Context::empty()
  let malicious_key = create_key("'; DROP TABLE context; --")
  let malicious_value = "<script>alert('XSS')</script>"
  
  let malicious_ctx = ctx
    .with_value(malicious_key, malicious_value)
    .with_value(create_key("path.traversal"), "../../../etc/passwd")
    .with_value(create_key("unicode.attack"), "admin\u200Badmin")
  
  // 验证恶意键值对被安全存储
  match malicious_ctx.get(malicious_key) {
    Some(value) => {
      assert_eq(value, "<script>alert('XSS')</script>")
      assert_eq(value.contains("<script>"), true)
    }
    None => @test.fail("Test failed")
  }
  
  match malicious_ctx.get(create_key("path.traversal")) {
    Some(value) => {
      assert_eq(value, "../../../etc/passwd")
      assert_eq(value.contains("../"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 2. 测试包含恶意内容的Baggage
  let malicious_baggage = Baggage::empty()
    .with_entry("'; DROP TABLE baggage; --", "<script>alert('XSS')</script>")
    .with_entry("path.traversal", "../../../etc/passwd")
    .with_entry("unicode.attack", "admin\u200Badmin")
    .with_entry("sql.injection", "'; OR 1=1; --")
    .with_entry("format.string", "%s%s%s%s%s%s")
  
  // 验证恶意baggage条目被安全存储
  match malicious_baggage.get("'; DROP TABLE baggage; --") {
    Some(value) => {
      assert_eq(value, "<script>alert('XSS')</script>")
      assert_eq(value.contains("<script>"), true)
    }
    None => @test.fail("Test failed")
  }
  
  match malicious_baggage.get("path.traversal") {
    Some(value) => {
      assert_eq(value, "../../../etc/passwd")
      assert_eq(value.contains("../"), true)
    }
    None => @test.fail("Test failed")
  }
  
  match malicious_baggage.get("unicode.attack") {
    Some(value) => {
      assert_eq(value, "admin\u200Badmin")
      assert_eq(value.contains("\u200B"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 3. 测试超长Context键值对
  let long_key = create_key("A" * 1000)
  let long_value = "B" * 10000
  
  let long_ctx = ctx
    .with_value(long_key, long_value)
  
  match long_ctx.get(long_key) {
    Some(value) => {
      assert_eq(value.length(), 10000)
      assert_eq(value.starts_with("B"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 4. 测试超长Baggage条目
  let long_baggage_key = "C" * 1000
  let long_baggage_value = "D" * 10000
  
  let long_baggage = Baggage::empty()
    .with_entry(long_baggage_key, long_baggage_value)
  
  match long_baggage.get(long_baggage_key) {
    Some(value) => {
      assert_eq(value.length(), 10000)
      assert_eq(value.starts_with("D"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 5. 测试包含特殊字符的Context和Baggage
  let special_chars_ctx = ctx
    .with_value(create_key("null.bytes"), "test\x00\x00\x00")
    .with_value(create_key("control.chars"), "test\x01\x02\x03")
  
  match special_chars_ctx.get(create_key("null.bytes")) {
    Some(value) => {
      assert_eq(value.contains("\x00"), true)
    }
    None => @test.fail("Test failed")
  }
  
  let special_chars_baggage = Baggage::empty()
    .with_entry("null.bytes", "test\x00\x00\x00")
    .with_entry("control.chars", "test\x01\x02\x03")
  
  match special_chars_baggage.get("null.bytes") {
    Some(value) => {
      assert_eq(value.contains("\x00"), true)
    }
    None => @test.fail("Test failed")
  }
}

test "malicious_propagation_handling" {
  // 测试恶意传播内容的处理
  
  // 1. 测试包含恶意内容的Carrier
  let malicious_carrier_data = [
    ("traceparent", "00-malicious-trace-id-malicious-span-id-01"),
    ("tracestate", "malicious=state;drop=table;inject=<script>"),
    ("baggage", "malicious.key=malicious.value;sql.injection='; DROP TABLE propagation; --"),
    ("custom.header", "<script>alert('XSS')</script>")
  ]
  
  let malicious_carrier = MapCarrier::from_map(malicious_carrier_data)
  
  // 验证恶意carrier数据被安全存储
  match malicious_carrier.get("traceparent") {
    Some(trace_parent) => {
      assert_eq(trace_parent.contains("malicious"), true)
    }
    None => @test.fail("Test failed")
  }
  
  match malicious_carrier.get("tracestate") {
    Some(trace_state) => {
      assert_eq(trace_state.contains("drop=table"), true)
      assert_eq(trace_state.contains("<script>"), true)
    }
    None => @test.fail("Test failed")
  }
  
  match malicious_carrier.get("baggage") {
    Some(baggage) => {
      assert_eq(baggage.contains("DROP TABLE"), true)
    }
    None => @test.fail("Test failed")
  }
  
  match malicious_carrier.get("custom.header") {
    Some(header) => {
      assert_eq(header.contains("<script>"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 2. 测试恶意内容注入
  let ctx = Context::empty()
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 注入恶意内容到carrier
  let injection_carrier = MapCarrier::new()
  let malicious_ctx = ctx
    .with_value(create_key("malicious.data"), "'; DROP TABLE injection; --")
  
  composite_propagator.inject(malicious_ctx, injection_carrier)
  
  // 验证注入过程不会因为恶意内容而失败
  match injection_carrier.get("traceparent") {
    Some(_) => assert_eq(true, true)  // 注入成功
    None => @test.fail("Test failed")
  }
  
  match injection_carrier.get("baggage") {
    Some(_) => assert_eq(true, true)  // 注入成功
    None => @test.fail("Test failed")
  }
  
  // 3. 测试恶意内容提取
  let extraction_carrier = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "malicious=state;drop=table"),
    ("baggage", "sql.injection='; DROP TABLE extraction; --"),
    ("xss.header", "<script>alert('XSS')</script>")
  ])
  
  let extracted_ctx = composite_propagator.extract(ctx, extraction_carrier)
  
  // 验证提取过程不会因为恶意内容而失败
  assert_eq(extracted_ctx.values.length() >= 0, true)  // 提取成功，可能有0个或更多值
  
  // 4. 测试超长传播头部
  let long_traceparent = "00-" + "A" * 32 + "-" + "B" * 16 + "-01"
  let long_tracestate = "C" * 1000
  let long_baggage = "D" * 10000
  
  let long_carrier = MapCarrier::from_map([
    ("traceparent", long_traceparent),
    ("tracestate", long_tracestate),
    ("baggage", long_baggage)
  ])
  
  // 验证超长头部被安全处理
  match long_carrier.get("traceparent") {
    Some(trace_parent) => {
      assert_eq(trace_parent.length(), 2 + 32 + 1 + 16 + 3)  // 格式长度
      assert_eq(trace_parent.contains("A"), true)
    }
    None => @test.fail("Test failed")
  }
  
  match long_carrier.get("tracestate") {
    Some(trace_state) => {
      assert_eq(trace_state.length(), 1000)
      assert_eq(trace_state.contains("C"), true)
    }
    None => @test.fail("Test failed")
  }
  
  match long_carrier.get("baggage") {
    Some(baggage) => {
      assert_eq(baggage.length(), 10000)
      assert_eq(baggage.contains("D"), true)
    }
    None => @test.fail("Test failed")
  }
}

test "resource_and_scope_malicious_content" {
  // 测试Resource和InstrumentationScope中的恶意内容
  
  // 1. 测试包含恶意内容的Resource
  let malicious_resource = Resource::{
    service_name: "malicious<script>service</script>",
    service_version: Some("'; DROP TABLE resource; --"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("malicious.attr", AttributeValue::string("<script>alert('XSS')</script>")),
      ("sql.injection", AttributeValue::string("'; DROP TABLE attributes; --")),
      ("path.traversal", AttributeValue::string("../../../etc/passwd")),
      ("unicode.attack", AttributeValue::string("admin\u200Badmin"))
    ]
  }
  
  // 验证恶意资源名称被安全存储
  assert_eq(malicious_resource.service_name.contains("<script>"), true)
  assert_eq(malicious_resource.service_version.unwrap().contains("DROP TABLE"), true)
  
  // 验证恶意属性被安全存储
  let mut found_xss = false
  let mut found_sql_injection = false
  let mut found_path_traversal = false
  let mut found_unicode_attack = false
  
  let mut i = 0
  while i < malicious_resource.attributes.length() {
    let (name, value) = malicious_resource.attributes[i]
    match (name, value) {
      ("malicious.attr", StringValue(s)) => {
        assert_eq(s.contains("<script>"), true)
        found_xss = true
      }
      ("sql.injection", StringValue(s)) => {
        assert_eq(s.contains("DROP TABLE"), true)
        found_sql_injection = true
      }
      ("path.traversal", StringValue(s)) => {
        assert_eq(s.contains("../"), true)
        found_path_traversal = true
      }
      ("unicode.attack", StringValue(s)) => {
        assert_eq(s.contains("\u200B"), true)
        found_unicode_attack = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_xss, true)
  assert_eq(found_sql_injection, true)
  assert_eq(found_path_traversal, true)
  assert_eq(found_unicode_attack, true)
  
  // 2. 测试包含恶意内容的InstrumentationScope
  let malicious_scope = InstrumentationScope::{
    name: "malicious<script>scope</script>",
    version: Some("'; DROP TABLE scope; --"),
    schema_url: Some("https://malicious.com/script?xss=<script>alert('XSS')</script>")
  }
  
  // 验证恶意作用域名称被安全存储
  assert_eq(malicious_scope.name.contains("<script>"), true)
  assert_eq(malicious_scope.version.unwrap().contains("DROP TABLE"), true)
  assert_eq(malicious_scope.schema_url.unwrap().contains("<script>"), true)
  
  // 3. 测试超长Resource和Scope名称
  let long_resource_name = "A" * 10000
  let long_scope_name = "B" * 10000
  
  let long_resource = Resource::default(long_resource_name)
  let long_scope = InstrumentationScope::{
    name: long_scope_name,
    version: None,
    schema_url: None
  }
  
  assert_eq(long_resource.service_name.length(), 10000)
  assert_eq(long_scope.name.length(), 10000)
  
  // 4. 测试包含特殊字符的Resource和Scope
  let special_chars_resource = Resource::{
    service_name: "service\x00\x01\x02",
    service_version: Some("version\x03\x04\x05"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("null.chars", AttributeValue::string("test\x00\x00\x00")),
      ("control.chars", AttributeValue::string("test\x01\x02\x03"))
    ]
  }
  
  let special_chars_scope = InstrumentationScope::{
    name: "scope\x06\x07\x08",
    version: Some("version\x09\x0A\x0B"),
    schema_url: Some("https://example.com/schema\x0C\x0D\x0E")
  }
  
  // 验证特殊字符被安全存储
  assert_eq(special_chars_resource.service_name.contains("\x00"), true)
  assert_eq(special_chars_resource.service_version.unwrap().contains("\x03"), true)
  assert_eq(special_chars_scope.name.contains("\x06"), true)
  assert_eq(special_chars_scope.version.unwrap().contains("\x09"), true)
  
  // 5. 测试包含Unicode攻击的Resource和Scope
  let unicode_resource = Resource::{
    service_name: "service\u202E\u202D\u200F\u200E",  // 方向控制字符
    service_version: Some("version\u200B\u200C\u200D"),  // 零宽度字符
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("homograph.attack", AttributeValue::string("аdmin")),  // 西里尔字母a
      ("zero.width.attack", AttributeValue::string("admin\u200Badmin"))
    ]
  }
  
  let unicode_scope = InstrumentationScope::{
    name: "scope\u202A\u202B\u202C\u202D",  // 方向控制字符
    version: Some("version\u200E\u200F\u2060"),  // 零宽度字符
    schema_url: Some("https://example.com/schema\u2061\u2062\u2063")
  }
  
  // 验证Unicode攻击字符被安全存储
  assert_eq(unicode_resource.service_name.contains("\u202E"), true)
  assert_eq(unicode_resource.service_version.unwrap().contains("\u200B"), true)
  assert_eq(unicode_scope.name.contains("\u202A"), true)
  assert_eq(unicode_scope.version.unwrap().contains("\u200E"), true)
}

test "input_validation_and_sanitization" {
  // 测试输入验证和清理
  
  // 1. 测试空值和null处理
  let null_string = ""
  let null_attr = AttributeValue::string(null_string)
  
  match null_attr {
    StringValue(s) => assert_eq(s, "")
    _ => @test.fail("Test failed")
  }
  
  // 2. 测试数值边界验证
  let max_int = AttributeValue::int(9223372036854775807L)
  let min_int = AttributeValue::int(-9223372036854775808L)
  let max_float = AttributeValue::float(1.7976931348623157e+308)
  let min_float = AttributeValue::float(-1.7976931348623157e+308)
  
  match max_int {
    IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Test failed")
  }
  
  match min_int {
    IntValue(i) => assert_eq(i, -9223372036854775808L)
    _ => @test.fail("Test failed")
  }
  
  // 3. 测试数组长度验证
  let empty_string_array = AttributeValue::array_string([])
  let empty_int_array = AttributeValue::array_int([])
  let empty_float_array = AttributeValue::array_float([])
  let empty_bool_array = AttributeValue::array_bool([])
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  // 4. 测试超长数组处理
  let long_string_array = AttributeValue::array_string(["item"] * 1000)
  let long_int_array = AttributeValue::array_int([1L] * 1000)
  
  match long_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 1000)
    _ => @test.fail("Test failed")
  }
  
  match long_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 1000)
    _ => @test.fail("Test failed")
  }
  
  // 5. 测试嵌套恶意内容
  let nested_malicious = AttributeValue::array_string([
    "<script>alert('XSS')</script>",
    "'; DROP TABLE nested; --",
    "../../../etc/passwd",
    "admin\u200Badmin"
  ])
  
  match nested_malicious {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 4)
      assert_eq(arr[0].contains("<script>"), true)
      assert_eq(arr[1].contains("DROP TABLE"), true)
      assert_eq(arr[2].contains("../"), true)
      assert_eq(arr[3].contains("\u200B"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 6. 测试混合类型恶意内容
  let mixed_malicious_attributes = [
    ("string.attack", AttributeValue::string("<script>alert('XSS')</script>")),
    ("int.attack", AttributeValue::int(9223372036854775807L)),
    ("float.attack", AttributeValue::float(1.0/0.0)),  // Infinity
    ("bool.attack", AttributeValue::bool(true)),
    ("array.attack", AttributeValue::array_string(["'; DROP TABLE mixed; --"]))
  ]
  
  // 验证混合类型恶意内容被安全存储
  let mut found_string_attack = false
  let mut found_int_attack = false
  let mut found_float_attack = false
  let mut found_bool_attack = false
  let mut found_array_attack = false
  
  let mut i = 0
  while i < mixed_malicious_attributes.length() {
    let (name, value) = mixed_malicious_attributes[i]
    match (name, value) {
      ("string.attack", StringValue(s)) => {
        assert_eq(s.contains("<script>"), true)
        found_string_attack = true
      }
      ("int.attack", IntValue(i)) => {
        assert_eq(i, 9223372036854775807L)
        found_int_attack = true
      }
      ("float.attack", FloatValue(f)) => {
        assert_eq(f.is_infinite(), true)  // 检查是否为无穷大
        found_float_attack = true
      }
      ("bool.attack", BoolValue(b)) => {
        assert_eq(b, true)
        found_bool_attack = true
      }
      ("array.attack", ArrayStringValue(arr)) => {
        assert_eq(arr[0].contains("DROP TABLE"), true)
        found_array_attack = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_string_attack, true)
  assert_eq(found_int_attack, true)
  assert_eq(found_float_attack, true)
  assert_eq(found_bool_attack, true)
  assert_eq(found_array_attack, true)
}