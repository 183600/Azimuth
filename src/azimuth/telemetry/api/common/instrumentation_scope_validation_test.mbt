// 仪器范围(InstrumentationScope)测试用例

test "instrumentation_scope_basic_creation" {
  // 测试仪器范围基本创建功能
  
  let scope_name = "http-client"
  let scope = InstrumentationScope::{
    name: scope_name,
    version: None,
    schema_url: None
  }
  
  // 验证基本属性
  assert_eq(scope.name, "http-client")
  
  // 验证可选属性
  match scope.version {
    Some(version) => assert_eq(false, true) // 默认应该是None
    None => assert_eq(true, true)
  }
  
  match scope.schema_url {
    Some(url) => assert_eq(false, true) // 默认应该是None
    None => assert_eq(true, true)
  }
}

test "instrumentation_scope_with_version" {
  // 测试带版本的仪器范围
  
  let scope_name = "database-connector"
  let scope_version = Some("2.5.1")
  let scope = InstrumentationScope::{
    name: scope_name,
    version: scope_version,
    schema_url: None
  }
  
  // 验证基本属性
  assert_eq(scope.name, "database-connector")
  
  // 验证版本
  match scope.version {
    Some(version) => assert_eq(version, "2.5.1")
    None => assert_eq(false, true)
  }
  
  // 验证schema_url仍然为None
  match scope.schema_url {
    Some(url) => assert_eq(false, true)
    None => assert_eq(true, true)
  }
}

test "instrumentation_scope_with_schema_url" {
  // 测试带schema URL的仪器范围
  
  let scope_name = "message-queue"
  let schema_url = Some("https://opentelemetry.io/schemas/v1.20.0")
  let scope = InstrumentationScope::{
    name: scope_name,
    version: Some("1.3.0"),
    schema_url: schema_url
  }
  
  // 验证基本属性
  assert_eq(scope.name, "message-queue")
  
  // 验证版本
  match scope.version {
    Some(version) => assert_eq(version, "1.3.0")
    None => assert_eq(false, true)
  }
  
  // 验证schema URL
  match scope.schema_url {
    Some(url) => {
      assert_eq(url.has_prefix("https://"), true)
      assert_eq(url.contains("opentelemetry.io"), true)
      assert_eq(url.contains("v1.20.0"), true)
    }
    None => assert_eq(false, true)
  }
}

test "instrumentation_scope_naming_conventions" {
  // 测试仪器范围命名约定
  
  let scopes = [
    ("http.server", Some("1.0.0"), Some("https://opentelemetry.io/schemas/v1.20.0")),
    ("grpc.client", Some("2.1.3"), None),
    ("redis-client", Some("3.0.0"), Some("https://opentelemetry.io/schemas/v1.19.0")),
    ("mongodb.driver", None, None),
    ("kafka.producer", Some("1.5.2"), Some("https://opentelemetry.io/schemas/v1.18.0"))
  ]
  
  // 验证每个scope
  let mut i = 0
  while i < scopes.length() {
    let (name, version, schema_url) = scopes[i]
    let scope = InstrumentationScope::{
      name: name,
      version: version,
      schema_url: schema_url
    }
    
    // 验证名称
    assert_eq(scope.name, name)
    
    // 验证版本
    match version {
      Some(expected_version) => {
        match scope.version {
          Some(actual_version) => assert_eq(actual_version, expected_version)
          None => assert_eq(false, true)
        }
      }
      None => {
        match scope.version {
          Some(_) => assert_eq(false, true)
          None => assert_eq(true, true)
        }
      }
    }
    
    // 验证schema URL
    match schema_url {
      Some(expected_url) => {
        match scope.schema_url {
          Some(actual_url) => assert_eq(actual_url, expected_url)
          None => assert_eq(false, true)
        }
      }
      None => {
        match scope.schema_url {
          Some(_) => assert_eq(false, true)
          None => assert_eq(true, true)
        }
      }
    }
    
    i = i + 1
  }
}

test "instrumentation_scope_version_validation" {
  // 测试仪器范围版本验证
  
  let version_cases = [
    ("1.0.0", true),
    ("2.5.1", true),
    ("10.20.30", true),
    ("1.0.0-alpha", true),
    ("2.1.3-beta.2", true),
    ("3.0.0-rc.1", true),
    ("1.0", false), // 不完整的版本号
    ("latest", false), // 非语义化版本
    ("", false) // 空版本号
  ]
  
  let mut i = 0
  while i < version_cases.length() {
    let (version, is_valid) = version_cases[i]
    let scope = InstrumentationScope::{
      name: "test-scope",
      version: Some(version),
      schema_url: None
    }
    
    match scope.version {
      Some(actual_version) => {
        assert_eq(actual_version, version)
        // 如果版本有效，验证版本格式
        if is_valid {
          assert_eq(version.contains("."), true)
        }
      }
      None => assert_eq(false, true)
    }
    
    i = i + 1
  }
}

test "instrumentation_scope_schema_url_validation" {
  // 测试仪器范围schema URL验证
  
  let url_cases = [
    ("https://opentelemetry.io/schemas/v1.20.0", true),
    ("https://opentelemetry.io/schemas/v1.19.0", true),
    ("https://opentelemetry.io/schemas/v1.18.0", true),
    ("http://example.com/schema", true),
    ("https://custom-schema.org/v1.0", true),
    ("ftp://invalid-protocol.com/schema", false), // 不支持的协议
    ("not-a-url", false), // 不是URL
    ("", false) // 空URL
  ]
  
  let mut i = 0
  while i < url_cases.length() {
    let (url, is_valid) = url_cases[i]
    let scope = InstrumentationScope::{
      name: "test-scope",
      version: Some("1.0.0"),
      schema_url: Some(url)
    }
    
    match scope.schema_url {
      Some(actual_url) => {
        assert_eq(actual_url, url)
        // 如果URL有效，验证URL格式
        if is_valid {
          assert_eq(actual_url.has_prefix("http"), true)
          assert_eq(actual_url.contains("://"), true)
        }
      }
      None => assert_eq(false, true)
    }
    
    i = i + 1
  }
}

test "instrumentation_scope_edge_cases" {
  // 测试仪器范围边界情况
  
  // 空名称
  let empty_scope = InstrumentationScope::{
    name: "",
    version: None,
    schema_url: None
  }
  assert_eq(empty_scope.name, "")
  
  // 长名称
  let long_name = "very-long-instrumentation-scope-name-with-many-components"
  let long_scope = InstrumentationScope::{
    name: long_name,
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  assert_eq(long_scope.name.length(), 58)
  
  // 包含特殊字符的名称
  let special_name = "scope_with_underscores.and.dashes"
  let special_scope = InstrumentationScope::{
    name: special_name,
    version: None,
    schema_url: None
  }
  assert_eq(special_scope.name.contains("_"), true)
  assert_eq(special_scope.name.contains("."), true)
  assert_eq(special_scope.name.contains("-"), true)
  
  // 只有版本，没有schema URL
  let version_only_scope = InstrumentationScope::{
    name: "version-only-scope",
    version: Some("2.0.0"),
    schema_url: None
  }
  match version_only_scope.version {
    Some(v) => assert_eq(v, "2.0.0")
    None => assert_eq(false, true)
  }
  match version_only_scope.schema_url {
    Some(_) => assert_eq(false, true)
    None => assert_eq(true, true)
  }
  
  // 只有schema URL，没有版本
  let schema_only_scope = InstrumentationScope::{
    name: "schema-only-scope",
    version: None,
    schema_url: Some("https://example.com/schema")
  }
  match schema_only_scope.version {
    Some(_) => assert_eq(false, true)
    None => assert_eq(true, true)
  }
  match schema_only_scope.schema_url {
    Some(url) => assert_eq(url, "https://example.com/schema")
    None => assert_eq(false, true)
  }
}

test "instrumentation_scope_real_world_examples" {
  // 测试真实世界的仪器范围示例
  
  let real_world_scopes = [
    (
      "io.opentelemetry.javaagent.shaded.io.opentelemetry.api.trace",
      Some("1.28.0"),
      Some("https://opentelemetry.io/schemas/v1.20.0")
    ),
    (
      "org.springframework.boot.actuate.autoconfigure.tracing",
      Some("3.1.0"),
      None
    ),
    (
      "com.google.cloud.trace.v1",
      Some("2.15.0"),
      Some("https://cloud.google.com/trace/schema/v1")
    ),
    (
      "io.prometheus.client",
      Some("0.16.0"),
      None
    ),
    (
      "org.apache.kafka.clients",
      Some("3.4.0"),
      Some("https://opentelemetry.io/schemas/v1.19.0")
    )
  ]
  
  let mut i = 0
  while i < real_world_scopes.length() {
    let (name, version, schema_url) = real_world_scopes[i]
    let scope = InstrumentationScope::{
      name: name,
      version: version,
      schema_url: schema_url
    }
    
    // 验证名称
    assert_eq(scope.name, name)
    
    // 验证名称包含点号（表示包结构）
    assert_eq(name.contains("."), true)
    
    // 验证版本（如果有）
    match version {
      Some(expected_version) => {
        match scope.version {
          Some(actual_version) => assert_eq(actual_version, expected_version)
          None => assert_eq(false, true)
        }
      }
      None => {
        match scope.version {
          Some(_) => assert_eq(false, true)
          None => assert_eq(true, true)
        }
      }
    }
    
    // 验证schema URL（如果有）
    match schema_url {
      Some(expected_url) => {
        match scope.schema_url {
          Some(actual_url) => assert_eq(actual_url, expected_url)
          None => assert_eq(false, true)
        }
      }
      None => {
        match scope.schema_url {
          Some(_) => assert_eq(false, true)
          None => assert_eq(true, true)
        }
      }
    }
    
    i = i + 1
  }
}