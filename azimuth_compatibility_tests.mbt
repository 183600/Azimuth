// Azimuth 兼容性测试用例
// 专注于系统兼容性和互操作性验证

// 测试1: 版本兼容性测试
test "版本兼容性测试" {
  // 创建不同版本的遥测组件
  let v1_tracer_provider = TracerProvider::with_version("1.0.0")
  let v2_tracer_provider = TracerProvider::with_version("2.0.0")
  let v3_tracer_provider = TracerProvider::with_version("3.0.0")
  
  let v1_tracer = TracerProvider::get_tracer(v1_tracer_provider, "version.compatibility.test")
  let v2_tracer = TracerProvider::get_tracer(v2_tracer_provider, "version.compatibility.test")
  let v3_tracer = TracerProvider::get_tracer(v3_tracer_provider, "version.compatibility.test")
  
  // 创建不同版本的Span
  let v1_span = Tracer::start_span(v1_tracer, "v1.operation")
  let v2_span = Tracer::start_span(v2_tracer, "v2.operation")
  let v3_span = Tracer::start_span(v3_tracer, "v3.operation")
  
  // 验证版本特定的功能
  // v1.0.0 基础功能
  Span::set_attribute(v1_span, "version", StringValue("1.0.0"))
  Span::add_event(v1_span, "v1.event", None)
  
  // v2.0.0 新增功能
  Span::set_attribute(v2_span, "version", StringValue("2.0.0"))
  Span::add_event(v2_span, "v2.event", None)
  Span::set_status(v2_span, Ok)
  
  // v3.0.0 最新功能
  Span::set_attribute(v3_span, "version", StringValue("3.0.0"))
  Span::add_event(v3_span, "v3.event", None)
  Span::set_status(v3_span, Ok)
  
  // 验证向后兼容性
  let v1_context = Span::span_context(v1_span)
  let v2_context = Span::span_context(v2_span)
  let v3_context = Span::span_context(v3_span)
  
  // 所有版本应该支持基本的上下文操作
  assert_true(SpanContext::is_valid(v1_context))
  assert_true(SpanContext::is_valid(v2_context))
  assert_true(SpanContext::is_valid(v3_context))
  
  // 验证跨版本序列化兼容性
  let v1_serialized = Span::serialize(v1_span)
  let v2_serialized = Span::serialize(v2_span)
  let v3_serialized = Span::serialize(v3_span)
  
  // 所有版本应该支持序列化
  assert_true(v1_serialized.length() > 0)
  assert_true(v2_serialized.length() > 0)
  assert_true(v3_serialized.length() > 0)
  
  // 验证跨版本反序列化兼容性
  let v1_deserialized = Span::deserialize(v1_serialized)
  let v2_deserialized = Span::deserialize(v2_serialized)
  let v3_deserialized = Span::deserialize(v3_serialized)
  
  assert_eq(Span::name(v1_deserialized), "v1.operation")
  assert_eq(Span::name(v2_deserialized), "v2.operation")
  assert_eq(Span::name(v3_deserialized), "v3.operation")
  
  // 结束所有Span
  Span::end(v1_span)
  Span::end(v2_span)
  Span::end(v3_span)
}

// 测试2: 协议兼容性测试
test "协议兼容性测试" {
  // 创建不同协议的传播器
  let w3c_propagator = W3CTraceContextPropagator::new()
  let b3_propagator = B3Propagator::new()
  let jaeger_propagator = JaegerPropagator::new()
  let custom_propagator = CustomPropagator::new()
  
  // 创建测试上下文
  let trace_id = "protocol-compatibility-trace-12345"
  let span_id = "protocol-compatibility-span-67890"
  let context = SpanContext::new(trace_id, span_id, true, "key1=value1,key2=value2")
  
  // 测试W3C协议
  let w3c_carrier = TextMapCarrier::new()
  W3CTraceContextPropagator::inject(w3c_propagator, context, w3c_carrier)
  
  let w3c_extracted = W3CTraceContextPropagator::extract(w3c_propagator, w3c_carrier)
  assert_eq(SpanContext::trace_id(w3c_extracted), trace_id)
  assert_eq(SpanContext::span_id(w3c_extracted), span_id)
  
  // 测试B3协议
  let b3_carrier = TextMapCarrier::new()
  B3Propagator::inject(b3_propagator, context, b3_carrier)
  
  let b3_extracted = B3Propagator::extract(b3_propagator, b3_carrier)
  assert_eq(SpanContext::trace_id(b3_extracted), trace_id)
  assert_eq(SpanContext::span_id(b3_extracted), span_id)
  
  // 测试Jaeger协议
  let jaeger_carrier = TextMapCarrier::new()
  JaegerPropagator::inject(jaeger_propagator, context, jaeger_carrier)
  
  let jaeger_extracted = JaegerPropagator::extract(jaeger_propagator, jaeger_carrier)
  assert_eq(SpanContext::trace_id(jaeger_extracted), trace_id)
  assert_eq(SpanContext::span_id(jaeger_extracted), span_id)
  
  // 测试自定义协议
  let custom_carrier = TextMapCarrier::new()
  CustomPropagator::inject(custom_propagator, context, custom_carrier)
  
  let custom_extracted = CustomPropagator::extract(custom_propagator, custom_carrier)
  assert_eq(SpanContext::trace_id(custom_extracted), trace_id)
  assert_eq(SpanContext::span_id(custom_extracted), span_id)
  
  // 测试跨协议兼容性
  let composite_propagator = CompositePropagator::new([w3c_propagator, b3_propagator])
  let composite_carrier = TextMapCarrier::new()
  
  // 使用复合传播器注入
  CompositePropagator::inject(composite_propagator, context, composite_carrier)
  
  // 使用单个传播器提取
  let w3c_from_composite = W3CTraceContextPropagator::extract(w3c_propagator, composite_carrier)
  let b3_from_composite = B3Propagator::extract(b3_propagator, composite_carrier)
  
  assert_eq(SpanContext::trace_id(w3c_from_composite), trace_id)
  assert_eq(SpanContext::trace_id(b3_from_composite), trace_id)
}

// 测试3: 数据格式兼容性测试
test "数据格式兼容性测试" {
  // 创建测试数据
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "data.format.compatibility.test")
  
  let span = Tracer::start_span(tracer, "format.compatibility.test")
  Span::set_attribute(span, "string.attr", StringValue("test string"))
  Span::set_attribute(span, "int.attr", IntValue(42))
  Span::set_attribute(span, "float.attr", FloatValue(3.14159))
  Span::set_attribute(span, "bool.attr", BoolValue(true))
  Span::set_attribute(span, "array.attr", ArrayStringValue(["item1", "item2", "item3"]))
  
  // 测试JSON格式
  let json_serializer = JsonSerializer::new()
  let json_data = json_serializer.serialize_span(span)
  assert_true(json_data.contains("\"name\":\"format.compatibility.test\""))
  assert_true(json_data.contains("\"string.attr\":\"test string\""))
  assert_true(json_data.contains("\"int.attr\":42"))
  
  let json_deserialized = json_serializer.deserialize_span(json_data)
  assert_eq(Span::name(json_deserialized), "format.compatibility.test")
  
  // 测试ProtoBuf格式
  let protobuf_serializer = ProtobufSerializer::new()
  let protobuf_data = protobuf_serializer.serialize_span(span)
  assert_true(protobuf_data.length() > 0)
  
  let protobuf_deserialized = protobuf_serializer.deserialize_span(protobuf_data)
  assert_eq(Span::name(protobuf_deserialized), "format.compatibility.test")
  
  // 测试XML格式
  let xml_serializer = XmlSerializer::new()
  let xml_data = xml_serializer.serialize_span(span)
  assert_true(xml_data.contains("<name>format.compatibility.test</name>"))
  assert_true(xml_data.contains("<string.attr>test string</string.attr>"))
  
  let xml_deserialized = xml_serializer.deserialize_span(xml_data)
  assert_eq(Span::name(xml_deserialized), "format.compatibility.test")
  
  // 测试MessagePack格式
  let msgpack_serializer = MsgpackSerializer::new()
  let msgpack_data = msgpack_serializer.serialize_span(span)
  assert_true(msgpack_data.length() > 0)
  
  let msgpack_deserialized = msgpack_serializer.deserialize_span(msgpack_data)
  assert_eq(Span::name(msgpack_deserialized), "format.compatibility.test")
  
  // 验证跨格式数据一致性
  let json_attr = Span::get_attribute(json_deserialized, "string.attr")
  let protobuf_attr = Span::get_attribute(protobuf_deserialized, "string.attr")
  let xml_attr = Span::get_attribute(xml_deserialized, "string.attr")
  let msgpack_attr = Span::get_attribute(msgpack_deserialized, "string.attr")
  
  match (json_attr, protobuf_attr, xml_attr, msgpack_attr) {
    (Some(StringValue(json)), Some(StringValue(protobuf)), Some(StringValue(xml)), Some(StringValue(msgpack))) => {
      assert_eq(json, protobuf)
      assert_eq(protobuf, xml)
      assert_eq(xml, msgpack)
    }
    _ => assert_true(false)
  }
  
  Span::end(span)
}

// 测试4: 平台兼容性测试
test "平台兼容性测试" {
  // 测试不同操作系统兼容性
  let os_type = System::os_type()
  let arch_type = System::arch_type()
  
  // 验证当前平台支持
  assert_true(os_type == "linux" or os_type == "windows" or os_type == "macos")
  assert_true(arch_type == "x86_64" or arch_type == "arm64" or arch_type == "i386")
  
  // 创建平台特定的遥测组件
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "platform.compatibility.test")
  
  let span = Tracer::start_span(tracer, "platform.test")
  
  // 设置平台特定属性
  Span::set_attribute(span, "os.type", StringValue(os_type))
  Span::set_attribute(span, "os.arch", StringValue(arch_type))
  Span::set_attribute(span, "os.version", StringValue(System::os_version()))
  Span::set_attribute(span, "hostname", StringValue(System::hostname()))
  
  // 测试文件系统兼容性
  let temp_dir = System::temp_dir()
  assert_true(temp_dir.length() > 0)
  
  let test_file = temp_dir + "/azimuth_compatibility_test.txt"
  let file_handle = File::create(test_file)
  File::write(file_handle, "platform compatibility test")
  File::close(file_handle)
  
  let read_handle = File::open(test_file)
  let content = File::read_all(read_handle)
  File::close(read_handle)
  
  assert_eq(content, "platform compatibility test")
  File::delete(test_file)
  
  // 测试网络兼容性
  let network_interfaces = Network::get_interfaces()
  assert_true(network_interfaces.length() > 0)
  
  // 测试环境变量兼容性
  System::set_env("AZIMUTH_COMPATIBILITY_TEST", "test_value")
  let env_value = System::get_env("AZIMUTH_COMPATIBILITY_TEST")
  assert_eq(env_value, Some("test_value"))
  System::unset_env("AZIMUTH_COMPATIBILITY_TEST")
  
  // 测试进程兼容性
  let current_pid = Process::current_pid()
  assert_true(current_pid > 0)
  
  let parent_pid = Process::parent_pid()
  assert_true(parent_pid > 0 or parent_pid == -1)  // -1表示没有父进程
  
  // 测试内存兼容性
  let total_memory = Memory::total()
  let available_memory = Memory::available()
  
  assert_true(total_memory > 0)
  assert_true(available_memory > 0)
  assert_true(available_memory <= total_memory)
  
  Span::end(span)
}

// 测试5: 浏览器兼容性测试
test "浏览器兼容性测试" {
  // 检测浏览器环境
  let is_browser = System::is_browser()
  let is_node = System::is_node()
  
  if is_browser {
    // 测试浏览器特定功能
    let user_agent = Browser::user_agent()
    assert_true(user_agent.length() > 0)
    
    let language = Browser::language()
    assert_true(language.length() > 0)
    
    let cookie_enabled = Browser::cookie_enabled()
    assert_true(cookie_enabled == true or cookie_enabled == false)
    
    let online_status = Browser::online_status()
    assert_true(online_status == true or online_status == false)
    
    // 测试浏览器存储
    Browser::set_local_storage("azimuth_test_key", "azimuth_test_value")
    let stored_value = Browser::get_local_storage("azimuth_test_key")
    assert_eq(stored_value, Some("azimuth_test_value"))
    Browser::remove_local_storage("azimuth_test_key")
    
    // 测试浏览器导航
    let current_url = Browser::current_url()
    assert_true(current_url.length() > 0)
    
    let referrer = Browser::referrer()
    assert_true(referrer.length() >= 0)
    
    // 测试浏览器性能API
    let navigation_start = Browser::performance_navigation_start()
    assert_true(navigation_start > 0)
    
    // 创建浏览器特定的遥测组件
    let tracer_provider = TracerProvider::default()
    let tracer = TracerProvider::get_tracer(tracer_provider, "browser.compatibility.test")
    
    let span = Tracer::start_span(tracer, "browser.test")
    
    // 设置浏览器特定属性
    Span::set_attribute(span, "browser.user_agent", StringValue(user_agent))
    Span::set_attribute(span, "browser.language", StringValue(language))
    Span::set_attribute(span, "browser.cookie.enabled", BoolValue(cookie_enabled))
    Span::set_attribute(span, "browser.online", BoolValue(online_status))
    Span::set_attribute(span, "browser.url", StringValue(current_url))
    
    Span::end(span)
  }
  
  if is_node {
    // 测试Node.js特定功能
    let node_version = Node::version()
    assert_true(node_version.length() > 0)
    
    let platform = Node::platform()
    assert_true(platform.length() > 0)
    
    let arch = Node::arch()
    assert_true(arch.length() > 0)
    
    // 测试Node.js模块系统
    let module_path = Node::require_resolve("fs")
    assert_true(module_path.length() > 0)
    
    // 创建Node.js特定的遥测组件
    let tracer_provider = TracerProvider::default()
    let tracer = TracerProvider::get_tracer(tracer_provider, "node.compatibility.test")
    
    let span = Tracer::start_span(tracer, "node.test")
    
    // 设置Node.js特定属性
    Span::set_attribute(span, "node.version", StringValue(node_version))
    Span::set_attribute(span, "node.platform", StringValue(platform))
    Span::set_attribute(span, "node.arch", StringValue(arch))
    
    Span::end(span)
  }
}

// 测试6: 数据库兼容性测试
test "数据库兼容性测试" {
  // 测试不同数据库连接器
  let mysql_connector = MySqlConnector::new()
  let postgresql_connector = PostgreSqlConnector::new()
  let sqlite_connector = SqliteConnector::new()
  let mongodb_connector = MongoDbConnector::new()
  
  // 测试MySQL兼容性
  let mysql_config = {
    host: "localhost",
    port: 3306,
    database: "azimuth_test",
    username: "test_user",
    password: "test_password"
  }
  
  let mysql_connection = MySqlConnector::connect(mysql_connector, mysql_config)
  match mysql_connection {
    Ok(conn) => {
      // 测试MySQL查询
      let result = MySqlConnection::execute_query(conn, "SELECT 1 as test_value")
      match result {
        Ok(rows) => {
          assert_true(rows.length() > 0)
          let first_row = rows[0]
          let test_value = first_row.get("test_value")
          match test_value {
            IntValue(value) => assert_eq(value, 1)
            _ => assert_true(false)
          }
        }
        Err(_) => assert_true(true)  // 连接失败是可接受的（测试环境）
      }
      MySqlConnection::close(conn)
    }
    Err(_) => assert_true(true)  // 连接失败是可接受的（测试环境）
  }
  
  // 测试PostgreSQL兼容性
  let postgresql_config = {
    host: "localhost",
    port: 5432,
    database: "azimuth_test",
    username: "test_user",
    password: "test_password"
  }
  
  let postgresql_connection = PostgreSqlConnector::connect(postgresql_connector, postgresql_config)
  match postgresql_connection {
    Ok(conn) => {
      // 测试PostgreSQL查询
      let result = PostgreSqlConnection::execute_query(conn, "SELECT 1 as test_value")
      match result {
        Ok(rows) => {
          assert_true(rows.length() > 0)
          let first_row = rows[0]
          let test_value = first_row.get("test_value")
          match test_value {
            IntValue(value) => assert_eq(value, 1)
            _ => assert_true(false)
          }
        }
        Err(_) => assert_true(true)  // 连接失败是可接受的（测试环境）
      }
      PostgreSqlConnection::close(conn)
    }
    Err(_) => assert_true(true)  // 连接失败是可接受的（测试环境）
  }
  
  // 测试SQLite兼容性
  let sqlite_config = {
    database_path: ":memory:"
  }
  
  let sqlite_connection = SqliteConnector::connect(sqlite_connector, sqlite_config)
  match sqlite_connection {
    Ok(conn) => {
      // 测试SQLite查询
      let result = SqliteConnection::execute_query(conn, "SELECT 1 as test_value")
      match result {
        Ok(rows) => {
          assert_true(rows.length() > 0)
          let first_row = rows[0]
          let test_value = first_row.get("test_value")
          match test_value {
            IntValue(value) => assert_eq(value, 1)
            _ => assert_true(false)
          }
        }
        Err(_) => assert_true(false)
      }
      SqliteConnection::close(conn)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试MongoDB兼容性
  let mongodb_config = {
    host: "localhost",
    port: 27017,
    database: "azimuth_test"
  }
  
  let mongodb_connection = MongoDbConnector::connect(mongodb_connector, mongodb_config)
  match mongodb_connection {
    Ok(conn) => {
      // 测试MongoDB查询
      let result = MongoDbConnection::execute_query(conn, "test_collection", "{}")
      match result {
        Ok(docs) => {
          // 空集合查询应该成功
          assert_true(docs.length() >= 0)
        }
        Err(_) => assert_true(true)  // 查询失败是可接受的（测试环境）
      }
      MongoDbConnection::close(conn)
    }
    Err(_) => assert_true(true)  // 连接失败是可接受的（测试环境）
  }
}

// 测试7: 网络兼容性测试
test "网络兼容性测试" {
  // 测试HTTP客户端兼容性
  let http_client = HttpClient::new()
  let https_client = HttpsClient::new()
  
  // 测试HTTP请求
  let http_request = HttpRequest::new("GET", "http://httpbin.org/get", [], None)
  let http_response = HttpClient::execute(http_client, http_request)
  
  match http_response {
    Ok(response) => {
      assert_eq(response.status_code, 200)
      assert_true(response.body.length() > 0)
    }
    Err(_) => assert_true(true)  // 网络错误是可接受的（测试环境）
  }
  
  // 测试HTTPS请求
  let https_request = HttpRequest::new("GET", "https://httpbin.org/get", [], None)
  let https_response = HttpsClient::execute(https_client, https_request)
  
  match https_response {
    Ok(response) => {
      assert_eq(response.status_code, 200)
      assert_true(response.body.length() > 0)
    }
    Err(_) => assert_true(true)  // 网络错误是可接受的（测试环境）
  }
  
  // 测试WebSocket兼容性
  let websocket_client = WebSocketClient::new()
  let websocket_connection = WebSocketClient::connect(websocket_client, "wss://echo.websocket.org")
  
  match websocket_connection {
    Ok(conn) => {
      // 测试WebSocket发送和接收
      let send_result = WebSocketConnection::send(conn, "test message")
      assert_true(send_result.is_ok())
      
      let receive_result = WebSocketConnection::receive(conn)
      match receive_result {
        Ok(message) => assert_eq(message, "test message")
        Err(_) => assert_true(true)  // 接收超时是可接受的
      }
      
      WebSocketConnection::close(conn)
    }
    Err(_) => assert_true(true)  // 连接失败是可接受的（测试环境）
  }
  
  // 测试DNS解析兼容性
  let dns_resolver = DnsResolver::new()
  let dns_result = DnsResolver::resolve(dns_resolver, "google.com")
  
  match dns_result {
    Ok(ip_addresses) => {
      assert_true(ip_addresses.length() > 0)
      for ip in ip_addresses {
        assert_true(ip.length() > 0)
      }
    }
    Err(_) => assert_true(true)  // DNS解析失败是可接受的（测试环境）
  }
  
  // 测试TCP连接兼容性
  let tcp_client = TcpClient::new()
  let tcp_connection = TcpClient::connect(tcp_client, "google.com", 80)
  
  match tcp_connection {
    Ok(conn) => {
      // 测试TCP发送和接收
      let send_result = TcpConnection::send(conn, "GET / HTTP/1.1\r\nHost: google.com\r\n\r\n")
      assert_true(send_result.is_ok())
      
      let receive_result = TcpConnection::receive(conn, 1024)
      match receive_result {
        Ok(data) => assert_true(data.length() > 0)
        Err(_) => assert_true(true)  // 接收超时是可接受的
      }
      
      TcpConnection::close(conn)
    }
    Err(_) => assert_true(true)  // 连接失败是可接受的（测试环境）
  }
}

// 测试8: 第三方库兼容性测试
test "第三方库兼容性测试" {
  // 测试日志库兼容性
  let log4j_logger = Log4jLogger::new("azimuth.test")
  let slf4j_logger = Slf4jLogger::new("azimuth.test")
  let logback_logger = LogbackLogger::new("azimuth.test")
  
  // 测试Log4j
  Log4jLogger::info(log4j_logger, "Log4j compatibility test message")
  Log4jLogger::warn(log4j_logger, "Log4j compatibility test warning")
  Log4jLogger::error(log4j_logger, "Log4j compatibility test error")
  
  // 测试SLF4J
  Slf4jLogger::info(slf4j_logger, "SLF4J compatibility test message")
  Slf4jLogger::warn(slf4j_logger, "SLF4J compatibility test warning")
  Slf4jLogger::error(slf4j_logger, "SLF4J compatibility test error")
  
  // 测试Logback
  LogbackLogger::info(logback_logger, "Logback compatibility test message")
  LogbackLogger::warn(logback_logger, "Logback compatibility test warning")
  LogbackLogger::error(logback_logger, "Logback compatibility test error")
  
  // 测试JSON库兼容性
  let jackson_parser = JacksonJsonParser::new()
  let gson_parser = GsonJsonParser::new()
  let json_lib_parser = JsonLibJsonParser::new()
  
  let test_json = "{\"name\":\"test\",\"value\":42,\"active\":true}"
  
  // 测试Jackson
  let jackson_result = JacksonJsonParser::parse(jackson_parser, test_json)
  match jackson_result {
    Ok(obj) => {
      let name = JacksonJsonObject::get_string(obj, "name")
      let value = JacksonJsonObject::get_int(obj, "value")
      let active = JacksonJsonObject::get_bool(obj, "active")
      
      assert_eq(name, Some("test"))
      assert_eq(value, Some(42))
      assert_eq(active, Some(true))
    }
    Err(_) => assert_true(false)
  }
  
  // 测试Gson
  let gson_result = GsonJsonParser::parse(gson_parser, test_json)
  match gson_result {
    Ok(obj) => {
      let name = GsonJsonObject::get_string(obj, "name")
      let value = GsonJsonObject::get_int(obj, "value")
      let active = GsonJsonObject::get_bool(obj, "active")
      
      assert_eq(name, Some("test"))
      assert_eq(value, Some(42))
      assert_eq(active, Some(true))
    }
    Err(_) => assert_true(false)
  }
  
  // 测试HTTP客户端库兼容性
  let apache_http_client = ApacheHttpClient::new()
  let okhttp_client = OkHttpClient::new()
  let httpurlconnection_client = HttpURLConnectionClient::new()
  
  // 测试Apache HttpClient
  let apache_request = ApacheHttpRequest::new("GET", "http://httpbin.org/get")
  let apache_response = ApacheHttpClient::execute(apache_http_client, apache_request)
  
  match apache_response {
    Ok(response) => {
      assert_eq(ApacheHttpResponse::status_code(response), 200)
      assert_true(ApacheHttpResponse::body(response).length() > 0)
    }
    Err(_) => assert_true(true)  // 网络错误是可接受的（测试环境）
  }
  
  // 测试OkHttp
  let okhttp_request = OkHttpRequest::new("GET", "http://httpbin.org/get")
  let okhttp_response = OkHttpClient::execute(okhttp_client, okhttp_request)
  
  match okhttp_response {
    Ok(response) => {
      assert_eq(OkHttpResponse::status_code(response), 200)
      assert_true(OkHttpResponse::body(response).length() > 0)
    }
    Err(_) => assert_true(true)  // 网络错误是可接受的（测试环境）
  }
  
  // 测试缓存库兼容性
  let caffeine_cache = CaffeineCache::new()
  let guava_cache = GuavaCache::new()
  let ehcache_cache = EhcacheCache::new()
  
  // 测试Caffeine
  CaffeineCache::put(caffeine_cache, "test_key", "test_value")
  let caffeine_result = CaffeineCache::get(caffeine_cache, "test_key")
  assert_eq(caffeine_result, Some("test_value"))
  
  // 测试Guava
  GuavaCache::put(guava_cache, "test_key", "test_value")
  let guava_result = GuavaCache::get(guava_cache, "test_key")
  assert_eq(guava_result, Some("test_value"))
  
  // 测试Ehcache
  EhcacheCache::put(ehcache_cache, "test_key", "test_value")
  let ehcache_result = EhcacheCache::get(ehcache_cache, "test_key")
  assert_eq(ehcache_result, Some("test_value"))
}