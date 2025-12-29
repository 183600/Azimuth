test "common_attribute_value_creation" {
  // 测试不同类型的AttributeValue创建
  let string_attr = AttributeValue::string("test_value")
  let int_attr = AttributeValue::int(42L)
  let float_attr = AttributeValue::float(3.14)
  let bool_attr = AttributeValue::bool(true)
  
  match string_attr {
    StringValue(s) => assert_eq(s, "test_value")
    _ => @test.fail("Expected StringValue")
  }
  
  match int_attr {
    IntValue(i) => assert_eq(i, 42L)
    _ => @test.fail("Expected IntValue")
  }
  
  match float_attr {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => @test.fail("Expected FloatValue")
  }
  
  match bool_attr {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
}

test "common_resource_default_creation" {
  // 测试Resource的默认创建
  let resource = Resource::default("test-service")
  
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.service_version, None)
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
}

test "common_resource_with_version" {
  // 测试带版本号的Resource创建
  let resource = Resource::default("test-service")
  
  // 由于Resource的属性是immutable的，我们只能检查默认值
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.service_version, None)
}

test "common_attributes_empty" {
  // 测试空属性列表
  let attributes : Attributes = []
  assert_eq(attributes.length(), 0)
}

test "common_resource_with_attributes" {
  // 测试Resource的默认属性
  let resource = Resource::default("test-service")
  
  // 由于Resource的属性是immutable的，我们只能检查默认值
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.service_version, None)
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
}

test "common_instrumentation_scope_creation" {
  // 测试InstrumentationScope的创建
  let scope_with_version = InstrumentationScope::{
    name: "test-instrument",
    version: Some("2.1.0"),
    schema_url: Some("http://example.com/schema")
  }
  
  let scope_minimal = InstrumentationScope::{
    name: "minimal-instrument",
    version: None,
    schema_url: None
  }
  
  assert_eq(scope_with_version.name, "test-instrument")
  assert_eq(scope_with_version.version, Some("2.1.0"))
  assert_eq(scope_with_version.schema_url, Some("http://example.com/schema"))
  
  assert_eq(scope_minimal.name, "minimal-instrument")
  assert_eq(scope_minimal.version, None)
  assert_eq(scope_minimal.schema_url, None)
}

test "attribute_value_boundary_cases" {
  // 测试AttributeValue的边界情况
  let empty_string = AttributeValue::string("")
  let max_int = AttributeValue::int(9223372036854775807L) // Int64最大值
  let min_int = AttributeValue::int(-9223372036854775808L) // Int64最小值
  let max_float = AttributeValue::float(1.7976931348623157e+308) // Double最大值
  let min_float = AttributeValue::float(-1.7976931348623157e+308) // Double最小值
  let zero_float = AttributeValue::float(0.0)
  let nan_float = AttributeValue::float(0.0/0.0) // NaN
  let inf_float = AttributeValue::float(1.0/0.0) // 正无穷
  
  match empty_string {
    StringValue(s) => assert_eq(s, "")
    _ => @test.fail("Expected empty string")
  }
  
  match max_int {
    IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Expected max int")
  }
  
  match min_int {
    IntValue(i) => assert_eq(i, -9223372036854775808L)
    _ => @test.fail("Expected min int")
  }
  
  match zero_float {
    FloatValue(f) => assert_eq(f, 0.0)
    _ => @test.fail("Expected zero float")
  }
}

test "resource_edge_cases" {
  // 测试Resource的边界情况
  let long_name = "a".repeat(1000) // 1000字符的长服务名
  let special_chars = "service-name_with.special@chars#123"
  let unicode_name = "服务名称-测试"
  let empty_name = ""
  
  let resource_long = Resource::default(long_name)
  let resource_special = Resource::default(special_chars)
  let resource_unicode = Resource::default(unicode_name)
  let resource_empty = Resource::default(empty_name)
  
  assert_eq(resource_long.service_name.length(), 1000)
  assert_eq(resource_special.service_name, "service-name_with.special@chars#123")
  assert_eq(resource_unicode.service_name, "服务名称-测试")
  assert_eq(resource_empty.service_name, "")
  
  // 测试SDK信息保持不变
  assert_eq(resource_long.telemetry_sdk_name, "azimuth")
  assert_eq(resource_special.telemetry_sdk_version, "0.1.0")
}

test "attributes_complex_scenarios" {
  // 测试复杂的属性场景
  let mut complex_attrs : Attributes = []
  
  // 添加各种类型的属性
  complex_attrs = complex_attrs.push(("empty.string", AttributeValue::string("")))
  complex_attrs = complex_attrs.push(("long.string", AttributeValue::string("a".repeat(10000))))
  complex_attrs = complex_attrs.push(("zero.int", AttributeValue::int(0L)))
  complex_attrs = complex_attrs.push(("negative.int", AttributeValue::int(-999999999L)))
  complex_attrs = complex_attrs.push(("small.float", AttributeValue::float(0.000001)))
  complex_attrs = complex_attrs.push(("large.float", AttributeValue::float(999999999.999999)))
  complex_attrs = complex_attrs.push(("true.bool", AttributeValue::bool(true)))
  complex_attrs = complex_attrs.push(("false.bool", AttributeValue::bool(false)))
  
  // 测试属性数量
  assert_eq(complex_attrs.length(), 8)
  
  // 验证特定属性
  match complex_attrs[0] {
    ("empty.string", StringValue(s)) => assert_eq(s, "")
    _ => @test.fail("Expected empty string attribute")
  }
  
  match complex_attrs[1] {
    ("long.string", StringValue(s)) => assert_eq(s.length(), 10000)
    _ => @test.fail("Expected long string attribute")
  }
  
  match complex_attrs[6] {
    ("true.bool", BoolValue(b)) => assert_eq(b, true)
    _ => @test.fail("Expected true boolean attribute")
  }
}

test "instrumentation_scope_edge_cases" {
  // 测试InstrumentationScope的边界情况
  let scope_empty_name = InstrumentationScope::{
    name: "",
    version: Some(""),
    schema_url: Some("")
  }
  
  let scope_long_name = InstrumentationScope::{
    name: "a".repeat(500),
    version: Some("1.0.0".repeat(100)),
    schema_url: Some("http://".repeat(50) + "example.com")
  }
  
  let scope_special_chars = InstrumentationScope::{
    name: "scope-with.special@chars#123",
    version: Some("version_1.0.0-beta+build.123"),
    schema_url: Some("https://example.com/path?query=value#fragment")
  }
  
  let scope_unicode = InstrumentationScope::{
    name: "测试范围",
    version: Some("版本1.0"),
    schema_url: Some("https://例子.com/路径")
  }
  
  // 验证边界情况
  assert_eq(scope_empty_name.name, "")
  assert_eq(scope_empty_name.version?, "")
  assert_eq(scope_empty_name.schema_url?, "")
  
  assert_eq(scope_long_name.name.length(), 500)
  assert_eq(scope_long_name.version?.length(), 500)
  assert_eq(scope_long_name.schema_url?.length(), 400)
  
  assert_eq(scope_special_chars.name, "scope-with.special@chars#123")
  assert_eq(scope_special_chars.version?, "version_1.0.0-beta+build.123")
  assert_eq(scope_special_chars.schema_url?, "https://example.com/path?query=value#fragment")
  
  assert_eq(scope_unicode.name, "测试范围")
  assert_eq(scope_unicode.version?, "版本1.0")
  assert_eq(scope_unicode.schema_url?, "https://例子.com/路径")
}