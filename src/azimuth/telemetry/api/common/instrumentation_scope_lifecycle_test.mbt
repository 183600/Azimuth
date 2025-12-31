// InstrumentationScope生命周期测试
// 测试InstrumentationScope的生命周期，包括创建、使用和销毁的各个阶段

test "instrumentation_scope_basic_lifecycle" {
  // 测试InstrumentationScope的基本生命周期
  
  // 阶段1：创建最小化的InstrumentationScope
  let minimal_scope = InstrumentationScope::{
    name: "minimal-scope",
    version: None,
    schema_url: None
  }
  
  // 验证最小化scope
  assert_eq(minimal_scope.name, "minimal-scope")
  match minimal_scope.version {
    Some(_) => @test.fail("Test failed")  // 应该为None
    None => assert_eq(true, true)
  }
  match minimal_scope.schema_url {
    Some(_) => @test.fail("Test failed")  // 应该为None
    None => assert_eq(true, true)
  }
  
  // 阶段2：创建完整的InstrumentationScope
  let complete_scope = InstrumentationScope::{
    name: "complete-scope",
    version: Some("1.2.3"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  // 验证完整scope
  assert_eq(complete_scope.name, "complete-scope")
  match complete_scope.version {
    Some(version) => assert_eq(version, "1.2.3")
    None => @test.fail("Test failed")
  }
  match complete_scope.schema_url {
    Some(schema_url) => assert_eq(schema_url, "https://opentelemetry.io/schemas/1.20.0")
    None => @test.fail("Test failed")
  }
  
  // 阶段3：验证scope的不可变性（在真实实现中）
  // 这里我们验证scope的值保持不变
  assert_eq(minimal_scope.name, "minimal-scope")
  assert_eq(complete_scope.name, "complete-scope")
  match complete_scope.version {
    Some(version) => assert_eq(version, "1.2.3")
    None => @test.fail("Test failed")
  }
}

test "instrumentation_scope_creation_variations" {
  // 测试InstrumentationScope创建的各种变化
  
  // 变化1：只有名称和版本
  let name_version_scope = InstrumentationScope::{
    name: "name-version-scope",
    version: Some("2.0.0"),
    schema_url: None
  }
  
  assert_eq(name_version_scope.name, "name-version-scope")
  match name_version_scope.version {
    Some(version) => assert_eq(version, "2.0.0")
    None => @test.fail("Test failed")
  }
  match name_version_scope.schema_url {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 变化2：只有名称和schema_url
  let name_schema_scope = InstrumentationScope::{
    name: "name-schema-scope",
    version: None,
    schema_url: Some("https://example.com/schema/v1")
  }
  
  assert_eq(name_schema_scope.name, "name-schema-scope")
  match name_schema_scope.version {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  match name_schema_scope.schema_url {
    Some(schema_url) => assert_eq(schema_url, "https://example.com/schema/v1")
    None => @test.fail("Test failed")
  }
  
  // 变化3：空版本字符串
  let empty_version_scope = InstrumentationScope::{
    name: "empty-version-scope",
    version: Some(""),
    schema_url: None
  }
  
  assert_eq(empty_version_scope.name, "empty-version-scope")
  match empty_version_scope.version {
    Some(version) => assert_eq(version, "")
    None => @test.fail("Test failed")
  }
  
  // 变化4：空schema_url字符串
  let empty_schema_scope = InstrumentationScope::{
    name: "empty-schema-scope",
    version: None,
    schema_url: Some("")
  }
  
  assert_eq(empty_schema_scope.name, "empty-schema-scope")
  match empty_schema_scope.schema_url {
    Some(schema_url) => assert_eq(schema_url, "")
    None => @test.fail("Test failed")
  }
}

test "instrumentation_scope_edge_cases" {
  // 测试InstrumentationScope的边界情况
  
  // 边界情况1：长名称
  let long_name_scope = InstrumentationScope::{
    name: "this-is-a-very-long-instrumentation-scope-name-that-exceeds-normal-expectations-and-tests-boundary-conditions-for-name-length-validation",
    version: Some("1.0.0"),
    schema_url: None
  }
  
  assert_eq(long_name_scope.name.length() > 100, true)
  match long_name_scope.version {
    Some(version) => assert_eq(version, "1.0.0")
    None => @test.fail("Test failed")
  }
  
  // 边界情况2：包含特殊字符的名称
  let special_chars_scope = InstrumentationScope::{
    name: "scope!@#$%^&*()_+-=[]{}|;':\",./<>?",
    version: Some("2.1.0"),
    schema_url: None
  }
  
  assert_eq(special_chars_scope.name, "scope!@#$%^&*()_+-=[]{}|;':\",./<>?")
  
  // 边界情况3：Unicode名称
  let unicode_scope = InstrumentationScope::{
    name: "仪表范围 🌍 ñáéíóú",
    version: Some("3.0.0"),
    schema_url: None
  }
  
  assert_eq(unicode_scope.name, "仪表范围 🌍 ñáéíóú")
  
  // 边界情况4：复杂版本字符串
  let complex_version_scope = InstrumentationScope::{
    name: "complex-version-scope",
    version: Some("1.2.3-alpha.1+build.1234567890.abcdef"),
    schema_url: None
  }
  
  match complex_version_scope.version {
    Some(version) => assert_eq(version, "1.2.3-alpha.1+build.1234567890.abcdef")
    None => @test.fail("Test failed")
  }
  
  // 边界情况5：复杂schema_url
  let complex_schema_scope = InstrumentationScope::{
    name: "complex-schema-scope",
    version: None,
    schema_url: Some("https://example.com/schemas/v1.2.3?param=value&other=param#fragment")
  }
  
  match complex_schema_scope.schema_url {
    Some(schema_url) => assert_eq(schema_url, "https://example.com/schemas/v1.2.3?param=value&other=param#fragment")
    None => @test.fail("Test failed")
  }
}

test "instrumentation_scope_in_telemetry_context" {
  // 测试InstrumentationScope在遥测上下文中的使用
  
  // 创建不同类型的scope
  let http_scope = InstrumentationScope::{
    name: "http-client",
    version: Some("1.5.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  let db_scope = InstrumentationScope::{
    name: "database",
    version: Some("2.1.3"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  let cache_scope = InstrumentationScope::{
    name: "redis-client",
    version: Some("0.9.8"),
    schema_url: None
  }
  
  // 模拟在遥测操作中使用scope
  let scopes = [http_scope, db_scope, cache_scope]
  
  // 验证所有scope都正确创建
  assert_eq(scopes.length(), 3)
  
  // 验证HTTP scope
  assert_eq(scopes[0].name, "http-client")
  match scopes[0].version {
    Some(version) => assert_eq(version, "1.5.0")
    None => @test.fail("Test failed")
  }
  match scopes[0].schema_url {
    Some(schema_url) => assert_eq(schema_url, "https://opentelemetry.io/schemas/1.20.0")
    None => @test.fail("Test failed")
  }
  
  // 验证数据库scope
  assert_eq(scopes[1].name, "database")
  match scopes[1].version {
    Some(version) => assert_eq(version, "2.1.3")
    None => @test.fail("Test failed")
  }
  
  // 验证缓存scope
  assert_eq(scopes[2].name, "redis-client")
  match scopes[2].version {
    Some(version) => assert_eq(version, "0.9.8")
    None => @test.fail("Test failed")
  }
  match scopes[2].schema_url {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
}

test "instrumentation_scope_lifecycle_with_span_creation" {
  // 测试InstrumentationScope在Span创建过程中的生命周期
  
  // 创建scope
  let tracer_scope = InstrumentationScope::{
    name: "custom-tracer",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schemas/tracer/v1")
  }
  
  // 模拟使用scope创建tracer和span
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建与scope相关的span
  let scope_attributes = [
    ("instrumentation.scope.name", AttributeValue::string(tracer_scope.name)),
    ("instrumentation.scope.version", AttributeValue::string(
      match tracer_scope.version {
        Some(v) => v
        None => "unknown"
      }
    ))
  ]
  
  let (_, span) = tracer.start_span(ctx, "scoped_operation", Some(Server), Some(scope_attributes))
  
  // 验证span包含了scope信息
  assert_eq(span.name, "scoped_operation")
  assert_eq(span.kind, Server)
  assert_eq(span.attributes.length(), 2)
  
  // 验证scope属性
  let mut found_scope_name = false
  let mut found_scope_version = false
  
  let mut i = 0
  while i < span.attributes.length() {
    let (key, value) = span.attributes[i]
    
    match key {
      "instrumentation.scope.name" => {
        match value {
          StringValue(v) => {
            assert_eq(v, tracer_scope.name)
            found_scope_name = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "instrumentation.scope.version" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "1.0.0")
            found_scope_version = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => ()  // 忽略其他属性
    }
    
    i = i + 1
  }
  
  assert_eq(found_scope_name, true)
  assert_eq(found_scope_version, true)
}

test "instrumentation_scope_version_evolution" {
  // 测试InstrumentationScope版本演进的场景
  
  // 阶段1：初始版本
  let v1_scope = InstrumentationScope::{
    name: "evolving-scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schemas/v1")
  }
  
  // 阶段2：功能更新版本
  let v2_scope = InstrumentationScope::{
    name: "evolving-scope",
    version: Some("1.1.0"),
    schema_url: Some("https://example.com/schemas/v1")
  }
  
  // 阶段3：主要版本更新
  let v3_scope = InstrumentationScope::{
    name: "evolving-scope",
    version: Some("2.0.0"),
    schema_url: Some("https://example.com/schemas/v2")
  }
  
  // 阶段4：预发布版本
  let prerelease_scope = InstrumentationScope::{
    name: "evolving-scope",
    version: Some("2.1.0-alpha.1"),
    schema_url: Some("https://example.com/schemas/v2")
  }
  
  // 验证版本演进
  let versioned_scopes = [v1_scope, v2_scope, v3_scope, prerelease_scope]
  
  let expected_versions = ["1.0.0", "1.1.0", "2.0.0", "2.1.0-alpha.1"]
  let expected_schemas = [
    "https://example.com/schemas/v1",
    "https://example.com/schemas/v1",
    "https://example.com/schemas/v2",
    "https://example.com/schemas/v2"
  ]
  
  let mut i = 0
  while i < versioned_scopes.length() {
    let scope = versioned_scopes[i]
    
    assert_eq(scope.name, "evolving-scope")
    match scope.version {
      Some(version) => assert_eq(version, expected_versions[i])
      None => @test.fail("Test failed")
    }
    match scope.schema_url {
      Some(schema_url) => assert_eq(schema_url, expected_schemas[i])
      None => @test.fail("Test failed")
    }
    
    i = i + 1
  }
}

test "instrumentation_scope_performance_considerations" {
  // 测试InstrumentationScope的性能考虑
  
  // 创建大量scope来测试性能
  let mut scopes = []
  let mut i = 0
  while i < 100 {
    let scope = InstrumentationScope::{
      name: "perf-scope-" + i.to_string(),
      version: Some("1." + i.to_string() + ".0"),
      schema_url: Some("https://example.com/schemas/v" + i.to_string())
    }
    scopes.push(scope)
    i = i + 1
  }
  
  // 验证所有scope都正确创建
  assert_eq(scopes.length(), 100)
  
  // 验证scope数据的完整性
  i = 0
  while i < scopes.length() {
    let scope = scopes[i]
    let expected_name = "perf-scope-" + i.to_string()
    let expected_version = "1." + i.to_string() + ".0"
    let expected_schema = "https://example.com/schemas/v" + i.to_string()
    
    assert_eq(scope.name, expected_name)
    match scope.version {
      Some(version) => assert_eq(version, expected_version)
      None => @test.fail("Test failed")
    }
    match scope.schema_url {
      Some(schema_url) => assert_eq(schema_url, expected_schema)
      None => @test.fail("Test failed")
    }
    
    i = i + 1
  }
  
  // 测试scope查找性能（模拟）
  let test_indices = [0, 49, 99, 25, 75]
  i = 0
  while i < test_indices.length() {
    let index = test_indices[i]
    let scope = scopes[index]
    let expected_name = "perf-scope-" + index.to_string()
    
    assert_eq(scope.name, expected_name)
    i = i + 1
  }
}

test "instrumentation_scope_error_handling" {
  // 测试InstrumentationScope的错误处理
  
  // 测试空名称（在真实实现中可能抛出错误）
  let empty_name_scope = InstrumentationScope::{
    name: "",
    version: Some("1.0.0"),
    schema_url: None
  }
  
  assert_eq(empty_name_scope.name, "")
  match empty_name_scope.version {
    Some(version) => assert_eq(version, "1.0.0")
    None => @test.fail("Test failed")
  }
  
  // 测试只包含空格的名称
  let spaces_name_scope = InstrumentationScope::{
    name: "   ",
    version: Some("1.0.0"),
    schema_url: None
  }
  
  assert_eq(spaces_name_scope.name, "   ")
  
  // 测试无效的schema_url格式
  let invalid_schema_scope = InstrumentationScope::{
    name: "invalid-schema-scope",
    version: None,
    schema_url: Some("not-a-valid-url")
  }
  
  match invalid_schema_scope.schema_url {
    Some(schema_url) => assert_eq(schema_url, "not-a-valid-url")
    None => @test.fail("Test failed")
  }
  
  // 测试非常长的版本字符串
  let long_version_scope = InstrumentationScope::{
    name: "long-version-scope",
    version: Some("1.2.3-alpha.1+build.1234567890.abcdef.very.long.version.string.that.exceeds.normal.expectations"),
    schema_url: None
  }
  
  match long_version_scope.version {
    Some(version) => assert_eq(version.length() > 80, true)
    None => @test.fail("Test failed")
  }
}

test "instrumentation_scope_comprehensive_lifecycle" {
  // 综合生命周期测试
  
  // 阶段1：初始化
  let initial_scope = InstrumentationScope::{
    name: "comprehensive-scope",
    version: Some("0.1.0"),
    schema_url: Some("https://example.com/schemas/v1")
  }
  
  // 阶段2：开发阶段
  let dev_scope = InstrumentationScope::{
    name: "comprehensive-scope",
    version: Some("0.9.0-beta.1"),
    schema_url: Some("https://example.com/schemas/v1")
  }
  
  // 阶段3：发布阶段
  let release_scope = InstrumentationScope::{
    name: "comprehensive-scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schemas/v1")
  }
  
  // 阶段4：功能更新
  let feature_scope = InstrumentationScope::{
    name: "comprehensive-scope",
    version: Some("1.1.0"),
    schema_url: Some("https://example.com/schemas/v1")
  }
  
  // 阶段5：主要版本更新
  let major_scope = InstrumentationScope::{
    name: "comprehensive-scope",
    version: Some("2.0.0"),
    schema_url: Some("https://example.com/schemas/v2")
  }
  
  // 阶段6：长期支持版本
  let lts_scope = InstrumentationScope::{
    name: "comprehensive-scope",
    version: Some("2.1.0-lts"),
    schema_url: Some("https://example.com/schemas/v2")
  }
  
  // 验证完整的生命周期
  let lifecycle_scopes = [
    initial_scope, dev_scope, release_scope, 
    feature_scope, major_scope, lts_scope
  ]
  
  let expected_versions = [
    "0.1.0", "0.9.0-beta.1", "1.0.0", 
    "1.1.0", "2.0.0", "2.1.0-lts"
  ]
  
  let expected_schemas = [
    "https://example.com/schemas/v1", "https://example.com/schemas/v1", 
    "https://example.com/schemas/v1", "https://example.com/schemas/v1", 
    "https://example.com/schemas/v2", "https://example.com/schemas/v2"
  ]
  
  let mut i = 0
  while i < lifecycle_scopes.length() {
    let scope = lifecycle_scopes[i]
    
    // 验证名称在整个生命周期中保持一致
    assert_eq(scope.name, "comprehensive-scope")
    
    // 验证版本演进
    match scope.version {
      Some(version) => assert_eq(version, expected_versions[i])
      None => @test.fail("Test failed")
    }
    
    // 验证schema URL的演进
    match scope.schema_url {
      Some(schema_url) => assert_eq(schema_url, expected_schemas[i])
      None => @test.fail("Test failed")
    }
    
    i = i + 1
  }
  
  // 验证版本递增的逻辑
  assert_eq(expected_versions[0] < expected_versions[1], true)  // 0.1.0 < 0.9.0-beta.1
  assert_eq(expected_versions[2] < expected_versions[3], true)  // 1.0.0 < 1.1.0
  assert_eq(expected_versions[3] < expected_versions[4], true)  // 1.1.0 < 2.0.0
  assert_eq(expected_versions[4] < expected_versions[5], true)  // 2.0.0 < 2.1.0-lts
}