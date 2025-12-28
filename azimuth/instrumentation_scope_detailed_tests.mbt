// InstrumentationScope Detailed Tests
// 仪表化作用域详细测试，测试作用域的各种配置和边界情况

test "InstrumentationScope基本属性测试" {
  // 测试基本属性设置
  let scope1 = InstrumentationScope::{ 
    name: "test.scope", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/schema/v1") 
  }
  
  assert_eq(scope1.name, "test.scope")
  assert_eq(scope1.version, Some("1.0.0"))
  assert_eq(scope1.schema_url, Some("https://example.com/schema/v1"))
  
  // 测试只有名称的scope
  let scope2 = InstrumentationScope::{ 
    name: "minimal.scope", 
    version: None, 
    schema_url: None 
  }
  
  assert_eq(scope2.name, "minimal.scope")
  assert_eq(scope2.version, None)
  assert_eq(scope2.schema_url, None)
  
  // 测试只有名称和版本的scope
  let scope3 = InstrumentationScope::{ 
    name: "version.only.scope", 
    version: Some("2.1.0"), 
    schema_url: None 
  }
  
  assert_eq(scope3.name, "version.only.scope")
  assert_eq(scope3.version, Some("2.1.0"))
  assert_eq(scope3.schema_url, None)
  
  // 测试只有名称和schema_url的scope
  let scope4 = InstrumentationScope::{ 
    name: "schema.only.scope", 
    version: None, 
    schema_url: Some("https://opentelemetry.io/schemas/v1.20.0") 
  }
  
  assert_eq(scope4.name, "schema.only.scope")
  assert_eq(scope4.version, None)
  assert_eq(scope4.schema_url, Some("https://opentelemetry.io/schemas/v1.20.0"))
}

test "InstrumentationScope特殊字符和边界值测试" {
  // 测试包含特殊字符的scope名称
  let special_name_scope = InstrumentationScope::{ 
    name: "test.scope-with_special.chars_and.numbers123", 
    version: Some("1.0.0-beta.1+build.123"), 
    schema_url: Some("https://example.com/path-with_underscore/v1.0") 
  }
  
  assert_eq(special_name_scope.name, "test.scope-with_special.chars_and.numbers123")
  assert_eq(special_name_scope.version, Some("1.0.0-beta.1+build.123"))
  assert_eq(special_name_scope.schema_url, Some("https://example.com/path-with_underscore/v1.0"))
  
  // 测试Unicode字符
  let unicode_scope = InstrumentationScope::{ 
    name: "测试.作用域", 
    version: Some("版本.1.0"), 
    schema_url: Some("https://例子.公司/模式/v1") 
  }
  
  assert_eq(unicode_scope.name, "测试.作用域")
  assert_eq(unicode_scope.version, Some("版本.1.0"))
  assert_eq(unicode_scope.schema_url, Some("https://例子.公司/模式/v1"))
  
  // 测试空字符串版本和schema_url
  let empty_version_scope = InstrumentationScope::{ 
    name: "empty.version.scope", 
    version: Some(""), 
    schema_url: Some("") 
  }
  
  assert_eq(empty_version_scope.name, "empty.version.scope")
  assert_eq(empty_version_scope.version, Some(""))
  assert_eq(empty_version_scope.schema_url, Some(""))
  
  // 测试非常长的名称
  let long_name = "a" * 100
  let long_name_scope = InstrumentationScope::{ 
    name: long_name, 
    version: Some("1.0.0"), 
    schema_url: None 
  }
  
  assert_eq(long_name_scope.name.length(), 100)
  assert_eq(long_name_scope.version, Some("1.0.0"))
}

test "InstrumentationScope在Tracer中的使用测试" {
  let tracer_provider = TracerProvider::default()
  
  // 测试带有完整信息的scope
  let full_scope = InstrumentationScope::{ 
    name: "full.tracer.scope", 
    version: Some("2.0.0"), 
    schema_url: Some("https://opentelemetry.io/schemas/v1.20.0") 
  }
  
  let tracer = Tracer::{ scope: full_scope }
  let retrieved_scope = Tracer::instrumentation_scope(tracer)
  
  assert_eq(retrieved_scope.name, "full.tracer.scope")
  assert_eq(retrieved_scope.version, Some("2.0.0"))
  assert_eq(retrieved_scope.schema_url, Some("https://opentelemetry.io/schemas/v1.20.0"))
  
  // 测试使用TracerProvider创建的tracer
  let provider_tracer = TracerProvider::get_tracer(tracer_provider, "provider.tracer", Some("1.5.0"))
  let provider_scope = Tracer::instrumentation_scope(provider_tracer)
  
  assert_eq(provider_scope.name, "provider.tracer")
  assert_eq(provider_scope.version, Some("1.5.0"))
  assert_eq(provider_scope.schema_url, None)
}

test "InstrumentationScope在Meter中的使用测试" {
  let meter_provider = MeterProvider::default()
  
  // 测试带有完整信息的scope
  let meter_scope = InstrumentationScope::{ 
    name: "metrics.scope", 
    version: Some("3.1.0"), 
    schema_url: Some("https://metrics.example.com/schema/v2") 
  }
  
  let meter = Meter::{ scope: meter_scope }
  assert_eq(meter.scope.name, "metrics.scope")
  assert_eq(meter.scope.version, Some("3.1.0"))
  assert_eq(meter.scope.schema_url, Some("https://metrics.example.com/schema/v2"))
  
  // 测试使用MeterProvider创建的meter
  let provider_meter = MeterProvider::get_meter(meter_provider, "provider.meter")
  assert_eq(provider_meter.scope.name, "provider.meter")
  assert_eq(provider_meter.scope.version, None)
  assert_eq(provider_meter.scope.schema_url, None)
}

test "InstrumentationScope在Logger中的使用测试" {
  let logger_provider = LoggerProvider::default()
  
  // 测试带有完整信息的scope
  let logger_scope = InstrumentationScope::{ 
    name: "logging.scope", 
    version: Some("4.2.1"), 
    schema_url: Some("https://logging.example.com/schema/v1") 
  }
  
  let logger = Logger::{ scope: logger_scope }
  assert_eq(logger.scope.name, "logging.scope")
  assert_eq(logger.scope.version, Some("4.2.1"))
  assert_eq(logger.scope.schema_url, Some("https://logging.example.com/schema/v1"))
  
  // 测试使用LoggerProvider创建的logger
  let provider_logger = LoggerProvider::get_logger(logger_provider, "provider.logger")
  assert_eq(provider_logger.scope.name, "provider.logger")
  assert_eq(provider_logger.scope.version, None)
  assert_eq(provider_logger.scope.schema_url, None)
  
  // 测试带有版本和schema的logger
  let versioned_logger = LoggerProvider::get_logger(logger_provider, "versioned.logger")
  // 注意：当前实现不支持在get_logger中指定版本和schema_url
  // 这里我们直接创建带有完整信息的logger
  let full_logger = Logger::{ 
    scope: InstrumentationScope::{ 
      name: "full.logger", 
      version: Some("5.0.0"), 
      schema_url: Some("https://full.example.com/schema/v3") 
    } 
  }
  
  assert_eq(full_logger.scope.name, "full.logger")
  assert_eq(full_logger.scope.version, Some("5.0.0"))
  assert_eq(full_logger.scope.schema_url, Some("https://full.example.com/schema/v3"))
}

test "InstrumentationScope唯一性和比较测试" {
  // 创建多个不同的scope
  let scope1 = InstrumentationScope::{ 
    name: "scope.one", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/v1") 
  }
  
  let scope2 = InstrumentationScope::{ 
    name: "scope.two", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/v1") 
  }
  
  let scope3 = InstrumentationScope::{ 
    name: "scope.one", 
    version: Some("2.0.0"), 
    schema_url: Some("https://example.com/v1") 
  }
  
  let scope4 = InstrumentationScope::{ 
    name: "scope.one", 
    version: Some("1.0.0"), 
    schema_url: Some("https://different.com/v1") 
  }
  
  let scope5 = InstrumentationScope::{ 
    name: "scope.one", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/v1") 
  }
  
  // 验证名称不同
  assert_true(scope1.name != scope2.name)
  assert_eq(scope1.version, scope2.version)
  assert_eq(scope1.schema_url, scope2.schema_url)
  
  // 验证版本不同
  assert_eq(scope1.name, scope3.name)
  assert_true(scope1.version != scope3.version)
  assert_eq(scope1.schema_url, scope3.schema_url)
  
  // 验证schema_url不同
  assert_eq(scope1.name, scope4.name)
  assert_eq(scope1.version, scope4.version)
  assert_true(scope1.schema_url != scope4.schema_url)
  
  // 验证完全相同的scope
  assert_eq(scope1.name, scope5.name)
  assert_eq(scope1.version, scope5.version)
  assert_eq(scope1.schema_url, scope5.schema_url)
}

test "InstrumentationScope在复杂场景中的使用测试" {
  // 测试在创建多个instrument时的scope一致性
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let shared_scope = InstrumentationScope::{ 
    name: "shared.scope", 
    version: Some("1.2.3"), 
    schema_url: Some("https://shared.example.com/schema/v1") 
  }
  
  // 创建使用相同scope的不同instrument
  let tracer = Tracer::{ scope: shared_scope }
  let meter = Meter::{ scope: shared_scope }
  let logger = Logger::{ scope: shared_scope }
  
  // 验证所有instrument使用相同的scope
  assert_eq(Tracer::instrumentation_scope(tracer).name, "shared.scope")
  assert_eq(meter.scope.name, "shared.scope")
  assert_eq(logger.scope.name, "shared.scope")
  
  assert_eq(Tracer::instrumentation_scope(tracer).version, Some("1.2.3"))
  assert_eq(meter.scope.version, Some("1.2.3"))
  assert_eq(logger.scope.version, Some("1.2.3"))
  
  // 测试创建span和metric时的scope传递
  let span = Tracer::start_span(tracer, "test.span")
  let counter = Meter::create_counter(meter, "test.counter")
  let log_record = LogRecord::new(Info, "Test log message")
  
  // 验证操作成功（scope信息在span和metric创建中被正确使用）
  assert_true(Span::name(span) == "test.span")
  assert_eq(counter.name, "test.counter")
  assert_eq(LogRecord::severity_number(log_record), Info)
  
  Span::end(span)
  Logger::emit(logger, log_record)
}

test "InstrumentationScope边界条件和错误处理测试" {
  // 测试极端长度的各个字段
  let very_long_name = "scope.name." + ("very.long." * 20)
  let very_long_version = "1." + ("0." * 50) + "0"
  let very_long_schema = "https://very.long.domain.name.example.com/" + ("path/segment/" * 20) + "schema/v1"
  
  let extreme_scope = InstrumentationScope::{ 
    name: very_long_name, 
    version: Some(very_long_version), 
    schema_url: Some(very_long_schema) 
  }
  
  assert_true(extreme_scope.name.length() > 200)
  assert_true(extreme_scope.version.unwrap().length() > 100)
  assert_true(extreme_scope.schema_url.unwrap().length() > 200)
  
  // 测试包含特殊URL字符的schema_url
  let special_schema_scope = InstrumentationScope::{ 
    name: "special.schema.scope", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/path?query=value&param2=value2#fragment") 
  }
  
  match special_schema_scope.schema_url {
    Some(url) => assert_true(url.contains("?") && url.contains("#"))
    None => assert_true(false)
  }
  
  // 测试包含各种协议的schema_url
  let protocols = [
    "https://example.com/schema/v1",
    "http://example.com/schema/v1", 
    "ftp://example.com/schema/v1",
    "file://example.com/schema/v1"
  ]
  
  for protocol in protocols {
    let protocol_scope = InstrumentationScope::{ 
      name: "protocol.test.scope", 
      version: Some("1.0.0"), 
      schema_url: Some(protocol) 
    }
    
    assert_eq(protocol_scope.schema_url, Some(protocol))
  }
}