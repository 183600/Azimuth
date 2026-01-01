// Azimuth Resource Merge Strategy Comprehensive Test Suite
// This file contains comprehensive test cases for resource merge strategies

// Test 1: Basic resource merge strategy
pub test "basic resource merge strategy" {
  // Create base resource with common attributes
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("base-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("base-instance-123")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("base-host")),
    ("os.type", azimuth::StringValue("linux")),
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0")),
    ("base.unique.attr", azimuth::StringValue("base-unique-value"))
  ])
  
  // Create override resource with some overlapping and some unique attributes
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("override-service")),  // Override existing
    ("service.version", azimuth::StringValue("2.0.0")),          // Override existing
    ("deployment.environment", azimuth::StringValue("staging")), // Override existing
    ("override.unique.attr", azimuth::StringValue("override-unique-value")),
    ("additional.attr", azimuth::StringValue("additional-value"))
  ])
  
  // Merge resources (override wins)
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // Verify merge results
  // Overridden attributes should come from override resource
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), Some(azimuth::StringValue("override-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.version"), Some(azimuth::StringValue("2.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "deployment.environment"), Some(azimuth::StringValue("staging")))
  
  // Non-overridden attributes should come from base resource
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.instance.id"), Some(azimuth::StringValue("base-instance-123")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "host.name"), Some(azimuth::StringValue("base-host")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "os.type"), Some(azimuth::StringValue("linux")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "telemetry.sdk.version"), Some(azimuth::StringValue("0.1.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "base.unique.attr"), Some(azimuth::StringValue("base-unique-value")))
  
  // Unique override attributes should be present
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "override.unique.attr"), Some(azimuth::StringValue("override-unique-value")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "additional.attr"), Some(azimuth::StringValue("additional-value")))
  
  // Test merge with empty base resource
  let empty_base = azimuth::Resource::new()
  let merged_with_empty_base = azimuth::Resource::merge(empty_base, override_resource)
  
  // All attributes should come from override resource
  assert_eq(azimuth::Resource::get_attribute(merged_with_empty_base, "service.name"), Some(azimuth::StringValue("override-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_with_empty_base, "service.version"), Some(azimuth::StringValue("2.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_with_empty_base, "deployment.environment"), Some(azimuth::StringValue("staging")))
  assert_eq(azimuth::Resource::get_attribute(merged_with_empty_base, "override.unique.attr"), Some(azimuth::StringValue("override-unique-value")))
  assert_eq(azimuth::Resource::get_attribute(merged_with_empty_base, "additional.attr"), Some(azimuth::StringValue("additional-value")))
  
  // Test merge with empty override resource
  let empty_override = azimuth::Resource::new()
  let merged_with_empty_override = azimuth::Resource::merge(base_resource, empty_override)
  
  // All attributes should come from base resource
  assert_eq(azimuth::Resource::get_attribute(merged_with_empty_override, "service.name"), Some(azimuth::StringValue("base-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_with_empty_override, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_with_empty_override, "service.instance.id"), Some(azimuth::StringValue("base-instance-123")))
  assert_eq(azimuth::Resource::get_attribute(merged_with_empty_override, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(merged_with_empty_override, "host.name"), Some(azimuth::StringValue("base-host")))
  assert_eq(azimuth::Resource::get_attribute(merged_with_empty_override, "os.type"), Some(azimuth::StringValue("linux")))
  assert_eq(azimuth::Resource::get_attribute(merged_with_empty_override, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  assert_eq(azimuth::Resource::get_attribute(merged_with_empty_override, "telemetry.sdk.version"), Some(azimuth::StringValue("0.1.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_with_empty_override, "base.unique.attr"), Some(azimuth::StringValue("base-unique-value")))
}

// Test 2: Multi-level resource merge strategy
pub test "multi-level resource merge strategy" {
  // Create level 1 resource (lowest priority)
  let level1_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("level1-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("level1-instance-123")),
    ("deployment.environment", azimuth::StringValue("development")),
    ("host.name", azimuth::StringValue("level1-host")),
    ("os.type", azimuth::StringValue("linux")),
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0")),
    ("level1.unique.attr", azimuth::StringValue("level1-unique-value"))
  ])
  
  // Create level 2 resource (medium priority)
  let level2_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("level2-service")),  // Override level1
    ("service.version", azimuth::StringValue("1.5.0")),          // Override level1
    ("deployment.environment", azimuth::StringValue("testing")), // Override level1
    ("host.name", azimuth::StringValue("level2-host")),          // Override level1
    ("level2.unique.attr", azimuth::StringValue("level2-unique-value")),
    ("level2.additional.attr", azimuth::StringValue("level2-additional-value"))
  ])
  
  // Create level 3 resource (highest priority)
  let level3_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("level3-service")),  // Override level2
    ("deployment.environment", azimuth::StringValue("staging")), // Override level2
    ("telemetry.sdk.version", azimuth::StringValue("0.2.0")),    // Override level1 and level2
    ("level3.unique.attr", azimuth::StringValue("level3-unique-value")),
    ("level3.additional.attr", azimuth::StringValue("level3-additional-value"))
  ])
  
  // Merge level1 and level2
  let merged_level1_2 = azimuth::Resource::merge(level1_resource, level2_resource)
  
  // Verify level1 + level2 merge results
  assert_eq(azimuth::Resource::get_attribute(merged_level1_2, "service.name"), Some(azimuth::StringValue("level2-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_level1_2, "service.version"), Some(azimuth::StringValue("1.5.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_level1_2, "service.instance.id"), Some(azimuth::StringValue("level1-instance-123")))
  assert_eq(azimuth::Resource::get_attribute(merged_level1_2, "deployment.environment"), Some(azimuth::StringValue("testing")))
  assert_eq(azimuth::Resource::get_attribute(merged_level1_2, "host.name"), Some(azimuth::StringValue("level2-host")))
  assert_eq(azimuth::Resource::get_attribute(merged_level1_2, "os.type"), Some(azimuth::StringValue("linux")))
  assert_eq(azimuth::Resource::get_attribute(merged_level1_2, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  assert_eq(azimuth::Resource::get_attribute(merged_level1_2, "telemetry.sdk.version"), Some(azimuth::StringValue("0.1.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_level1_2, "level1.unique.attr"), Some(azimuth::StringValue("level1-unique-value")))
  assert_eq(azimuth::Resource::get_attribute(merged_level1_2, "level2.unique.attr"), Some(azimuth::StringValue("level2-unique-value")))
  assert_eq(azimuth::Resource::get_attribute(merged_level1_2, "level2.additional.attr"), Some(azimuth::StringValue("level2-additional-value")))
  
  // Merge result with level3
  let final_merged = azimuth::Resource::merge(merged_level1_2, level3_resource)
  
  // Verify final merge results
  assert_eq(azimuth::Resource::get_attribute(final_merged, "service.name"), Some(azimuth::StringValue("level3-service")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "service.version"), Some(azimuth::StringValue("1.5.0")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "service.instance.id"), Some(azimuth::StringValue("level1-instance-123")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "deployment.environment"), Some(azimuth::StringValue("staging")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "host.name"), Some(azimuth::StringValue("level2-host")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "os.type"), Some(azimuth::StringValue("linux")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "telemetry.sdk.version"), Some(azimuth::StringValue("0.2.0")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "level1.unique.attr"), Some(azimuth::StringValue("level1-unique-value")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "level2.unique.attr"), Some(azimuth::StringValue("level2-unique-value")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "level2.additional.attr"), Some(azimuth::StringValue("level2-additional-value")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "level3.unique.attr"), Some(azimuth::StringValue("level3-unique-value")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "level3.additional.attr"), Some(azimuth::StringValue("level3-additional-value")))
}

// Test 3: Resource merge with different attribute types
pub test "resource merge with different attribute types" {
  // Create base resource with different attribute types
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("string.attr", azimuth::StringValue("base-string-value")),
    ("int.attr", azimuth::IntValue(100)),
    ("float.attr", azimuth::FloatValue(3.14)),
    ("bool.attr", azimuth::BoolValue(true)),
    ("string.array.attr", azimuth::ArrayStringValue(["base-item1", "base-item2"])),
    ("int.array.attr", azimuth::ArrayIntValue([1, 2, 3]))
  ])
  
  // Create override resource with different attribute types
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("string.attr", azimuth::StringValue("override-string-value")),
    ("int.attr", azimuth::IntValue(200)),
    ("float.attr", azimuth::FloatValue(2.71)),
    ("bool.attr", azimuth::BoolValue(false)),
    ("string.array.attr", azimuth::ArrayStringValue(["override-item1", "override-item2"])),
    ("int.array.attr", azimuth::ArrayIntValue([4, 5, 6]))
  ])
  
  // Merge resources
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // Verify merge results for different attribute types
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "string.attr"), Some(azimuth::StringValue("override-string-value")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "int.attr"), Some(azimuth::IntValue(200)))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "float.attr"), Some(azimuth::FloatValue(2.71)))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "bool.attr"), Some(azimuth::BoolValue(false)))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "string.array.attr"), Some(azimuth::ArrayStringValue(["override-item1", "override-item2"])))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "int.array.attr"), Some(azimuth::ArrayIntValue([4, 5, 6])))
  
  // Test merge with extreme values
  let extreme_base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("max.int.attr", azimuth::IntValue(2147483647)),
    ("min.int.attr", azimuth::IntValue(-2147483648)),
    ("max.float.attr", azimuth::FloatValue(1.7976931348623157e+308)),
    ("min.float.attr", azimuth::FloatValue(-1.7976931348623157e+308)),
    ("infinity.float.attr", azimuth::FloatValue(1.0/0.0)),
    ("neg.infinity.float.attr", azimuth::FloatValue(-1.0/0.0)),
    ("nan.float.attr", azimuth::FloatValue(0.0/0.0))
  ])
  
  let extreme_override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("max.int.attr", azimuth::IntValue(100)),  // Override with smaller value
    ("min.int.attr", azimuth::IntValue(-100)), // Override with larger value
    ("max.float.attr", azimuth::FloatValue(100.0)),  // Override with smaller value
    ("min.float.attr", azimuth::FloatValue(-100.0)), // Override with larger value
    ("infinity.float.attr", azimuth::FloatValue(0.0)),  // Override infinity with 0
    ("neg.infinity.float.attr", azimuth::FloatValue(0.0)), // Override negative infinity with 0
    ("nan.float.attr", azimuth::FloatValue(0.0))  // Override NaN with 0
  ])
  
  // Merge extreme resources
  let merged_extreme = azimuth::Resource::merge(extreme_base_resource, extreme_override_resource)
  
  // Verify extreme merge results
  assert_eq(azimuth::Resource::get_attribute(merged_extreme, "max.int.attr"), Some(azimuth::IntValue(100)))
  assert_eq(azimuth::Resource::get_attribute(merged_extreme, "min.int.attr"), Some(azimuth::IntValue(-100)))
  assert_eq(azimuth::Resource::get_attribute(merged_extreme, "max.float.attr"), Some(azimuth::FloatValue(100.0)))
  assert_eq(azimuth::Resource::get_attribute(merged_extreme, "min.float.attr"), Some(azimuth::FloatValue(-100.0)))
  assert_eq(azimuth::Resource::get_attribute(merged_extreme, "infinity.float.attr"), Some(azimuth::FloatValue(0.0)))
  assert_eq(azimuth::Resource::get_attribute(merged_extreme, "neg.infinity.float.attr"), Some(azimuth::FloatValue(0.0)))
  assert_eq(azimuth::Resource::get_attribute(merged_extreme, "nan.float.attr"), Some(azimuth::FloatValue(0.0)))
}

// Test 4: Resource merge with special characters
pub test "resource merge with special characters" {
  // Create base resource with special characters in keys and values
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("attr.with.dots", azimuth::StringValue("base.value.with.dots")),
    ("attr-with-dashes", azimuth::StringValue("base-value-with-dashes")),
    ("attr_with_underscores", azimuth::StringValue("base_value_with_underscores")),
    ("attr with spaces", azimuth::StringValue("base value with spaces")),
    ("attr/with/slashes", azimuth::StringValue("base/value/with/slashes")),
    ("attr@with@symbols", azimuth::StringValue("base@value@with@symbols")),
    ("attr#with#hash", azimuth::StringValue("base#value#with#hash")),
    ("attr$with$dollar", azimuth::StringValue("base$value$with$dollar")),
    ("attr%with%percent", azimuth::StringValue("base%value%with%percent")),
    ("attr^with^caret", azimuth::StringValue("base^value^with^caret"))
  ])
  
  // Create override resource with special characters in keys and values
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("attr.with.dots", azimuth::StringValue("override.value.with.dots")),
    ("attr-with-dashes", azimuth::StringValue("override-value-with-dashes")),
    ("attr_with_underscores", azimuth::StringValue("override_value_with_underscores")),
    ("attr with spaces", azimuth::StringValue("override value with spaces")),
    ("attr/with/slashes", azimuth::StringValue("override/value/with/slashes")),
    ("attr@with@symbols", azimuth::StringValue("override@value@with@symbols")),
    ("attr#with#hash", azimuth::StringValue("override#value#with#hash")),
    ("attr$with$dollar", azimuth::StringValue("override$value$with$dollar")),
    ("attr%with%percent", azimuth::StringValue("override%value%with%percent")),
    ("attr^with^caret", azimuth::StringValue("override^value^with^caret"))
  ])
  
  // Merge resources
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // Verify merge results with special characters
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "attr.with.dots"), Some(azimuth::StringValue("override.value.with.dots")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "attr-with-dashes"), Some(azimuth::StringValue("override-value-with-dashes")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "attr_with_underscores"), Some(azimuth::StringValue("override_value_with_underscores")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "attr with spaces"), Some(azimuth::StringValue("override value with spaces")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "attr/with/slashes"), Some(azimuth::StringValue("override/value/with/slashes")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "attr@with@symbols"), Some(azimuth::StringValue("override@value@with@symbols")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "attr#with#hash"), Some(azimuth::StringValue("override#value#with#hash")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "attr$with$dollar"), Some(azimuth::StringValue("override$value$with$dollar")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "attr%with%percent"), Some(azimuth::StringValue("override%value%with%percent")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "attr^with^caret"), Some(azimuth::StringValue("override^value^with^caret")))
}

// Test 5: Resource merge with Unicode characters
pub test "resource merge with unicode characters" {
  // Create base resource with Unicode characters in keys and values
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("服务名称", azimuth::StringValue("基础服务")),
    ("服务版本", azimuth::StringValue("1.0.0")),
    ("服务实例ID", azimuth::StringValue("基础实例-12345")),
    ("部署环境", azimuth::StringValue("生产")),
    ("主机名", azimuth::StringValue("基础主机")),
    ("имя.службы", azimuth::StringValue("базовая.служба")),
    ("версия.службы", azimuth::StringValue("1.0.0")),
    ("имя.хоста", azimuth::StringValue("базовый.хост")),
    ("اسم.الخدمة", azimuth::StringValue("الخدمة.الأساسية")),
    ("اسم.المضيف", azimuth::StringValue("المضيف.الأساسي"))
  ])
  
  // Create override resource with Unicode characters in keys and values
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("服务名称", azimuth::StringValue("覆盖服务")),
    ("服务版本", azimuth::StringValue("2.0.0")),
    ("部署环境", azimuth::StringValue("测试")),
    ("主机名", azimuth::StringValue("覆盖主机")),
    ("имя.службы", azimuth::StringValue("переопределяющая.служба")),
    ("версия.службы", azimuth::StringValue("2.0.0")),
    ("имя.хоста", azimuth::StringValue("переопределяющий.хост")),
    ("اسم.الخدمة", azimuth::StringValue("خدمة.التغطية")),
    ("اسم.المضيف", azimuth::StringValue("مضيف.التغطية"))
  ])
  
  // Merge resources
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // Verify merge results with Unicode characters
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "服务名称"), Some(azimuth::StringValue("覆盖服务")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "服务版本"), Some(azimuth::StringValue("2.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "服务实例ID"), Some(azimuth::StringValue("基础实例-12345")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "部署环境"), Some(azimuth::StringValue("测试")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "主机名"), Some(azimuth::StringValue("覆盖主机")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "имя.службы"), Some(azimuth::StringValue("переопределяющая.служба")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "версия.службы"), Some(azimuth::StringValue("2.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "имя.хоста"), Some(azimuth::StringValue("переопределяющий.хост")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "اسم.الخدمة"), Some(azimuth::StringValue("خدمة.التغطية")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "اسم.المضيف"), Some(azimuth::StringValue("مضيف.التغطية")))
}

// Test 6: Resource merge with very long values
pub test "resource merge with very long values" {
  // Create base resource with very long values
  let very_long_base_string = "This is a very long base string value that tests resource merge integrity. ".repeat(50)
  let very_long_base_key = "this.is.a.very.long.base.attribute.key.that.tests.merge.integrity".repeat(5)
  
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    (very_long_base_key, azimuth::StringValue(very_long_base_string)),
    ("normal.base.attr", azimuth::StringValue("normal-base-value"))
  ])
  
  // Create override resource with very long values
  let very_long_override_string = "This is a very long override string value that tests resource merge integrity. ".repeat(50)
  let very_long_override_key = "this.is.a.very.long.override.attribute.key.that.tests.merge.integrity".repeat(5)
  
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    (very_long_base_key, azimuth::StringValue(very_long_override_string)),  // Override existing
    (very_long_override_key, azimuth::StringValue(very_long_override_string)),  // New attribute
    ("normal.override.attr", azimuth::StringValue("normal-override-value"))
  ])
  
  // Merge resources
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // Verify merge results with very long values
  assert_eq(azimuth::Resource::get_attribute(merged_resource, very_long_base_key), Some(azimuth::StringValue(very_long_override_string)))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, very_long_override_key), Some(azimuth::StringValue(very_long_override_string)))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "normal.base.attr"), Some(azimuth::StringValue("normal-base-value")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "normal.override.attr"), Some(azimuth::StringValue("normal-override-value")))
}

// Test 7: Resource merge with conflicting attributes
pub test "resource merge with conflicting attributes" {
  // Create base resource with common telemetry attributes
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("base-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("base-instance-123")),
    ("telemetry.sdk.name", azimuth::StringValue("base-sdk")),
    ("telemetry.sdk.version", azimuth::StringValue("1.0.0")),
    ("telemetry.auto.version", azimuth::StringValue("base-auto-version"))
  ])
  
  // Create override resource with conflicting telemetry attributes
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("override-service")),
    ("service.version", azimuth::StringValue("2.0.0")),
    ("service.instance.id", azimuth::StringValue("override-instance-456")),
    ("telemetry.sdk.name", azimuth::StringValue("override-sdk")),
    ("telemetry.sdk.version", azimuth::StringValue("2.0.0")),
    ("telemetry.auto.version", azimuth::StringValue("override-auto-version"))
  ])
  
  // Merge resources
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // Verify conflicting attributes are resolved by override
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), Some(azimuth::StringValue("override-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.version"), Some(azimuth::StringValue("2.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.instance.id"), Some(azimuth::StringValue("override-instance-456")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "telemetry.sdk.name"), Some(azimuth::StringValue("override-sdk")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "telemetry.sdk.version"), Some(azimuth::StringValue("2.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "telemetry.auto.version"), Some(azimuth::StringValue("override-auto-version")))
  
  // Test merge with case-sensitive conflicts
  let case_base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("Service.Name", azimuth::StringValue("case-base-service")),
    ("service.name", azimuth::StringValue("lowercase-base-service")),
    ("SERVICE.NAME", azimuth::StringValue("uppercase-base-service"))
  ])
  
  let case_override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("Service.Name", azimuth::StringValue("case-override-service")),
    ("service.name", azimuth::StringValue("lowercase-override-service")),
    ("SERVICE.NAME", azimuth::StringValue("uppercase-override-service"))
  ])
  
  // Merge case-sensitive resources
  let case_merged_resource = azimuth::Resource::merge(case_base_resource, case_override_resource)
  
  // Verify case-sensitive conflicts are resolved by override
  assert_eq(azimuth::Resource::get_attribute(case_merged_resource, "Service.Name"), Some(azimuth::StringValue("case-override-service")))
  assert_eq(azimuth::Resource::get_attribute(case_merged_resource, "service.name"), Some(azimuth::StringValue("lowercase-override-service")))
  assert_eq(azimuth::Resource::get_attribute(case_merged_resource, "SERVICE.NAME"), Some(azimuth::StringValue("uppercase-override-service")))
}

// Test 8: Resource merge performance and consistency
pub test "resource merge performance and consistency" {
  // Create large base resource with many attributes
  let mut base_attrs = []: Array[(String, azimuth::AttributeValue)]
  
  for i = 0; i < 100; i++ {
    let key = "base.attr." + i.to_string()
    let value = "base.value." + i.to_string()
    base_attrs = base_attrs.concat([(key, azimuth::StringValue(value))])
  }
  
  let large_base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), base_attrs)
  
  // Create large override resource with many attributes
  let mut override_attrs = []: Array[(String, azimuth::AttributeValue)]
  
  // Override some existing attributes
  for i = 0; i < 50; i++ {
    let key = "base.attr." + i.to_string()
    let value = "override.value." + i.to_string()
    override_attrs = override_attrs.concat([(key, azimuth::StringValue(value))])
  }
  
  // Add new attributes
  for i = 100; i < 150; i++ {
    let key = "override.attr." + i.to_string()
    let value = "override.value." + i.to_string()
    override_attrs = override_attrs.concat([(key, azimuth::StringValue(value))])
  }
  
  let large_override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), override_attrs)
  
  // Merge large resources
  let merged_resource = azimuth::Resource::merge(large_base_resource, large_override_resource)
  
  // Verify merge consistency for overridden attributes
  for i = 0; i < 50; i++ {
    let key = "base.attr." + i.to_string()
    let expected_value = "override.value." + i.to_string()
    assert_eq(azimuth::Resource::get_attribute(merged_resource, key), Some(azimuth::StringValue(expected_value)))
  }
  
  // Verify merge consistency for non-overridden attributes
  for i = 50; i < 100; i++ {
    let key = "base.attr." + i.to_string()
    let expected_value = "base.value." + i.to_string()
    assert_eq(azimuth::Resource::get_attribute(merged_resource, key), Some(azimuth::StringValue(expected_value)))
  }
  
  // Verify merge consistency for new attributes
  for i = 100; i < 150; i++ {
    let key = "override.attr." + i.to_string()
    let expected_value = "override.value." + i.to_string()
    assert_eq(azimuth::Resource::get_attribute(merged_resource, key), Some(azimuth::StringValue(expected_value)))
  }
  
  // Test merge idempotency
  let merged_again = azimuth::Resource::merge(merged_resource, azimuth::Resource::new())
  
  // Verify idempotent merge produces same result
  for i = 0; i < 50; i++ {
    let key = "base.attr." + i.to_string()
    let expected_value = "override.value." + i.to_string()
    assert_eq(azimuth::Resource::get_attribute(merged_again, key), Some(azimuth::StringValue(expected_value)))
  }
  
  for i = 50; i < 100; i++ {
    let key = "base.attr." + i.to_string()
    let expected_value = "base.value." + i.to_string()
    assert_eq(azimuth::Resource::get_attribute(merged_again, key), Some(azimuth::StringValue(expected_value)))
  }
  
  for i = 100; i < 150; i++ {
    let key = "override.attr." + i.to_string()
    let expected_value = "override.value." + i.to_string()
    assert_eq(azimuth::Resource::get_attribute(merged_again, key), Some(azimuth::StringValue(expected_value)))
  }
}