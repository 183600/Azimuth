// Azimuth Premium Resource Management Tests
// This file contains high-quality test cases for resource management and cleanup

// Test 1: Advanced resource lifecycle management
pub test "premium advanced resource lifecycle" {
  // Test resource creation and initialization
  let empty_resource = Resource::new()
  assert_true(empty_resource.attributes.length() == 0)
  
  // Test resource with initial attributes
  let initial_attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-12345"))
  ]
  
  let resource_with_initial = Resource::with_attributes(empty_resource, initial_attrs)
  
  // Verify initial attributes
  assert_eq(Resource::get_attribute(resource_with_initial, "service.name"), Some(StringValue("test-service")))
  assert_eq(Resource::get_attribute(resource_with_initial, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(resource_with_initial, "service.instance.id"), Some(StringValue("instance-12345")))
  
  // Test resource attribute updates
  let updated_attrs = [
    ("service.version", StringValue("1.1.0")),  // Update existing
    ("deployment.environment", StringValue("production"))  // Add new
  ]
  
  let updated_resource = Resource::with_attributes(resource_with_initial, updated_attrs)
  
  // Verify updated attributes
  assert_eq(Resource::get_attribute(updated_resource, "service.name"), Some(StringValue("test-service")))
  assert_eq(Resource::get_attribute(updated_resource, "service.version"), Some(StringValue("1.1.0")))
  assert_eq(Resource::get_attribute(updated_resource, "service.instance.id"), Some(StringValue("instance-12345")))
  assert_eq(Resource::get_attribute(updated_resource, "deployment.environment"), Some(StringValue("production")))
  
  // Test resource attribute removal (simulated by creating new resource without the attribute)
  let filtered_attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.1.0"))
  ]
  
  let filtered_resource = Resource::with_attributes(empty_resource, filtered_attrs)
  
  // Verify attribute removal
  assert_eq(Resource::get_attribute(filtered_resource, "service.name"), Some(StringValue("test-service")))
  assert_eq(Resource::get_attribute(filtered_resource, "service.version"), Some(StringValue("1.1.0")))
  assert_eq(Resource::get_attribute(filtered_resource, "service.instance.id"), None)
  assert_eq(Resource::get_attribute(filtered_resource, "deployment.environment"), None)
}

// Test 2: Resource merging strategies
pub test "premium resource merging strategies" {
  // Test basic resource merging
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("base.attribute", StringValue("base-value"))
  ])
  
  let override_resource = Resource::with_attributes(Resource::new(), [
    ("service.version", StringValue("2.0.0")),  // Override
    ("override.attribute", StringValue("override-value"))
  ])
  
  let merged_basic = Resource::merge(base_resource, override_resource)
  
  // Verify basic merging
  assert_eq(Resource::get_attribute(merged_basic, "service.name"), Some(StringValue("base-service")))
  assert_eq(Resource::get_attribute(merged_basic, "service.version"), Some(StringValue("2.0.0")))
  assert_eq(Resource::get_attribute(merged_basic, "base.attribute"), Some(StringValue("base-value")))
  assert_eq(Resource::get_attribute(merged_basic, "override.attribute"), Some(StringValue("override-value")))
  
  // Test deep resource merging with multiple levels
  let level1_resource = Resource::with_attributes(Resource::new(), [
    ("level1.attr1", StringValue("level1-value1")),
    ("level1.attr2", StringValue("level1-value2"))
  ])
  
  let level2_resource = Resource::with_attributes(Resource::new(), [
    ("level2.attr1", StringValue("level2-value1")),
    ("level1.attr2", StringValue("level2-override-value2"))  // Override level1.attr2
  ])
  
  let level3_resource = Resource::with_attributes(Resource::new(), [
    ("level3.attr1", StringValue("level3-value1")),
    ("level2.attr1", StringValue("level3-override-value1"))  // Override level2.attr1
  ])
  
  let merged_deep = Resource::merge(Resource::merge(level1_resource, level2_resource), level3_resource)
  
  // Verify deep merging
  assert_eq(Resource::get_attribute(merged_deep, "level1.attr1"), Some(StringValue("level1-value1")))
  assert_eq(Resource::get_attribute(merged_deep, "level1.attr2"), Some(StringValue("level2-override-value2")))
  assert_eq(Resource::get_attribute(merged_deep, "level2.attr1"), Some(StringValue("level3-override-value1")))
  assert_eq(Resource::get_attribute(merged_deep, "level3.attr1"), Some(StringValue("level3-value1")))
  
  // Test resource merging with different attribute types
  let typed_base = Resource::with_attributes(Resource::new(), [
    ("string.attr", StringValue("string-value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14)),
    ("bool.attr", BoolValue(true))
  ])
  
  let typed_override = Resource::with_attributes(Resource::new(), [
    ("string.attr", StringValue("overridden-string")),
    ("int.attr", IntValue(100)),
    ("new.string.attr", StringValue("new-string-value"))
  ])
  
  let merged_typed = Resource::merge(typed_base, typed_override)
  
  // Verify typed merging
  assert_eq(Resource::get_attribute(merged_typed, "string.attr"), Some(StringValue("overridden-string")))
  assert_eq(Resource::get_attribute(merged_typed, "int.attr"), Some(IntValue(100)))
  assert_eq(Resource::get_attribute(merged_typed, "float.attr"), Some(FloatValue(3.14)))
  assert_eq(Resource::get_attribute(merged_typed, "bool.attr"), Some(BoolValue(true)))
  assert_eq(Resource::get_attribute(merged_typed, "new.string.attr"), Some(StringValue("new-string-value")))
}

// Test 3: Resource attribute validation and sanitization
pub test "premium resource attribute validation" {
  // Test resource with valid attributes
  let valid_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("valid-service")),
    ("service.version", StringValue("1.0.0")),
    ("host.name", StringValue("valid-host")),
    ("process.id", IntValue(12345))
  ])
  
  // Verify valid attributes
  assert_eq(Resource::get_attribute(valid_resource, "service.name"), Some(StringValue("valid-service")))
  assert_eq(Resource::get_attribute(valid_resource, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(valid_resource, "host.name"), Some(StringValue("valid-host")))
  assert_eq(Resource::get_attribute(valid_resource, "process.id"), Some(IntValue(12345)))
  
  // Test resource with special characters in keys
  let special_chars_resource = Resource::with_attributes(Resource::new(), [
    ("service.name-with.dashes_and_underscores", StringValue("special-chars-service")),
    ("service.name.with.dots", StringValue("dots-service")),
    ("service.name-with.symbols!@#$%", StringValue("symbols-service"))
  ])
  
  // Verify special characters are handled
  assert_eq(Resource::get_attribute(special_chars_resource, "service.name-with.dashes_and_underscores"), Some(StringValue("special-chars-service")))
  assert_eq(Resource::get_attribute(special_chars_resource, "service.name.with.dots"), Some(StringValue("dots-service")))
  assert_eq(Resource::get_attribute(special_chars_resource, "service.name-with.symbols!@#$%"), Some(StringValue("symbols-service")))
  
  // Test resource with Unicode characters
  let unicode_resource = Resource::with_attributes(Resource::new(), [
    ("service.name.中文", StringValue("中文服务")),
    ("service.name.日本語", StringValue("日本語サービス")),
    ("service.name.한국어", StringValue("한국어 서비스")),
    ("service.name.العربية", StringValue("خدمة عربية")),
    ("service.name.עברית", StringValue("שירות עברי"))
  ])
  
  // Verify Unicode characters are handled
  assert_eq(Resource::get_attribute(unicode_resource, "service.name.中文"), Some(StringValue("中文服务")))
  assert_eq(Resource::get_attribute(unicode_resource, "service.name.日本語"), Some(StringValue("日本語サービス")))
  assert_eq(Resource::get_attribute(unicode_resource, "service.name.한국어"), Some(StringValue("한국어 서비스")))
  assert_eq(Resource::get_attribute(unicode_resource, "service.name.العربية"), Some(StringValue("خدمة عربية")))
  assert_eq(Resource::get_attribute(unicode_resource, "service.name.עברית"), Some(StringValue("שירות עברי")))
  
  // Test resource with empty values
  let empty_values_resource = Resource::with_attributes(Resource::new(), [
    ("empty.string", StringValue("")),
    ("empty.int", IntValue(0)),
    ("empty.float", FloatValue(0.0)),
    ("empty.bool", BoolValue(false))
  ])
  
  // Verify empty values are handled
  assert_eq(Resource::get_attribute(empty_values_resource, "empty.string"), Some(StringValue("")))
  assert_eq(Resource::get_attribute(empty_values_resource, "empty.int"), Some(IntValue(0)))
  assert_eq(Resource::get_attribute(empty_values_resource, "empty.float"), Some(FloatValue(0.0)))
  assert_eq(Resource::get_attribute(empty_values_resource, "empty.bool"), Some(BoolValue(false)))
  
  // Test resource with extreme values
  let extreme_values_resource = Resource::with_attributes(Resource::new(), [
    ("max.int", IntValue(9223372036854775807)),  // Max Int64
    ("min.int", IntValue(-9223372036854775808)),  // Min Int64
    ("max.float", FloatValue(1.7976931348623157e+308)),  // Max Double
    ("min.float", FloatValue(-1.7976931348623157e+308)),  // Min Double
    ("infinity", FloatValue(1.0/0.0)),  // Infinity
    ("neg.infinity", FloatValue(-1.0/0.0)),  // Negative Infinity
    ("nan", FloatValue(0.0/0.0))  // NaN
  ])
  
  // Verify extreme values are handled
  assert_eq(Resource::get_attribute(extreme_values_resource, "max.int"), Some(IntValue(9223372036854775807)))
  assert_eq(Resource::get_attribute(extreme_values_resource, "min.int"), Some(IntValue(-9223372036854775808)))
  assert_eq(Resource::get_attribute(extreme_values_resource, "max.float"), Some(FloatValue(1.7976931348623157e+308)))
  assert_eq(Resource::get_attribute(extreme_values_resource, "min.float"), Some(FloatValue(-1.7976931348623157e+308)))
  assert_eq(Resource::get_attribute(extreme_values_resource, "infinity"), Some(FloatValue(1.0/0.0)))
  assert_eq(Resource::get_attribute(extreme_values_resource, "neg.infinity"), Some(FloatValue(-1.0/0.0)))
  assert_eq(Resource::get_attribute(extreme_values_resource, "nan"), Some(FloatValue(0.0/0.0)))
}

// Test 4: Resource cleanup and memory management
pub test "premium resource cleanup memory" {
  // Test resource cleanup with large number of attributes
  let large_resource = Resource::new()
  
  // Add many attributes
  for i in 0...1000 {
    let key = "large.attr." + i.to_string()
    let value = StringValue("large.value." + i.to_string())
    large_resource = Resource::with_attributes(large_resource, [(key, value)])
  }
  
  // Verify all attributes were added
  for i in 0...1000 {
    let key = "large.attr." + i.to_string()
    let expected_value = StringValue("large.value." + i.to_string())
    assert_eq(Resource::get_attribute(large_resource, key), Some(expected_value))
  }
  
  // Test resource cleanup by creating new resource without old attributes
  let cleaned_resource = Resource::new()
  
  // Verify cleanup - old attributes should not be present
  assert_eq(Resource::get_attribute(cleaned_resource, "large.attr.0"), None)
  assert_eq(Resource::get_attribute(cleaned_resource, "large.attr.500"), None)
  assert_eq(Resource::get_attribute(cleaned_resource, "large.attr.999"), None)
  
  // Test resource cleanup with selective attribute retention
  let selective_resource = Resource::with_attributes(Resource::new(), [
    ("keep.attr1", StringValue("keep-value1")),
    ("keep.attr2", StringValue("keep-value2"))
  ])
  
  // Verify selective cleanup
  assert_eq(Resource::get_attribute(selective_resource, "keep.attr1"), Some(StringValue("keep-value1")))
  assert_eq(Resource::get_attribute(selective_resource, "keep.attr2"), Some(StringValue("keep-value2")))
  assert_eq(Resource::get_attribute(selective_resource, "large.attr.0"), None)
  
  // Test resource cleanup with array attributes
  let array_resource = Resource::with_attributes(Resource::new(), [
    ("large.string.array", ArrayStringValue(["item"] * 1000)),
    ("large.int.array", ArrayIntValue([1] * 1000))
  ])
  
  // Verify array attributes
  assert_true(Resource::get_attribute(array_resource, "large.string.array").is_some())
  assert_true(Resource::get_attribute(array_resource, "large.int.array").is_some())
  
  // Test cleanup of array attributes
  let cleaned_array_resource = Resource::new()
  
  // Verify array cleanup
  assert_eq(Resource::get_attribute(cleaned_array_resource, "large.string.array"), None)
  assert_eq(Resource::get_attribute(cleaned_array_resource, "large.int.array"), None)
}

// Test 5: Resource hierarchical management
pub test "premium resource hierarchical management" {
  // Test resource hierarchy with parent-child relationships
  let parent_resource = Resource::with_attributes(Resource::new(), [
    ("parent.attr1", StringValue("parent-value1")),
    ("parent.attr2", StringValue("parent-value2")),
    ("inherited.attr", StringValue("inherited-value"))
  ])
  
  let child_resource = Resource::with_attributes(Resource::new(), [
    ("child.attr1", StringValue("child-value1")),
    ("child.attr2", StringValue("child-value2")),
    ("inherited.attr", StringValue("child-override-value"))  // Override inherited
  ])
  
  // Test resource inheritance (simulated by merging)
  let inherited_resource = Resource::merge(parent_resource, child_resource)
  
  // Verify inheritance
  assert_eq(Resource::get_attribute(inherited_resource, "parent.attr1"), Some(StringValue("parent-value1")))
  assert_eq(Resource::get_attribute(inherited_resource, "parent.attr2"), Some(StringValue("parent-value2")))
  assert_eq(Resource::get_attribute(inherited_resource, "child.attr1"), Some(StringValue("child-value1")))
  assert_eq(Resource::get_attribute(inherited_resource, "child.attr2"), Some(StringValue("child-value2")))
  assert_eq(Resource::get_attribute(inherited_resource, "inherited.attr"), Some(StringValue("child-override-value")))
  
  // Test multi-level resource hierarchy
  let grandparent_resource = Resource::with_attributes(Resource::new(), [
    ("grandparent.attr1", StringValue("grandparent-value1")),
    ("grandparent.attr2", StringValue("grandparent-value2")),
    ("level.inherited", StringValue("grandparent-inherited"))
  ])
  
  let parent_level2 = Resource::with_attributes(Resource::new(), [
    ("parent.attr1", StringValue("parent-value1")),
    ("parent.attr2", StringValue("parent-value2")),
    ("level.inherited", StringValue("parent-inherited"))
  ])
  
  let child_level2 = Resource::with_attributes(Resource::new(), [
    ("child.attr1", StringValue("child-value1")),
    ("child.attr2", StringValue("child-value2")),
    ("level.inherited", StringValue("child-inherited"))
  ])
  
  // Build multi-level hierarchy
  let level2_resource = Resource::merge(grandparent_resource, parent_level2)
  let multi_level_resource = Resource::merge(level2_resource, child_level2)
  
  // Verify multi-level inheritance
  assert_eq(Resource::get_attribute(multi_level_resource, "grandparent.attr1"), Some(StringValue("grandparent-value1")))
  assert_eq(Resource::get_attribute(multi_level_resource, "grandparent.attr2"), Some(StringValue("grandparent-value2")))
  assert_eq(Resource::get_attribute(multi_level_resource, "parent.attr1"), Some(StringValue("parent-value1")))
  assert_eq(Resource::get_attribute(multi_level_resource, "parent.attr2"), Some(StringValue("parent-value2")))
  assert_eq(Resource::get_attribute(multi_level_resource, "child.attr1"), Some(StringValue("child-value1")))
  assert_eq(Resource::get_attribute(multi_level_resource, "child.attr2"), Some(StringValue("child-value2")))
  assert_eq(Resource::get_attribute(multi_level_resource, "level.inherited"), Some(StringValue("child-inherited")))
  
  // Test resource hierarchy with different attribute types
  let typed_parent = Resource::with_attributes(Resource::new(), [
    ("string.inherited", StringValue("parent-string")),
    ("int.inherited", IntValue(100)),
    ("float.inherited", FloatValue(3.14)),
    ("bool.inherited", BoolValue(true))
  ])
  
  let typed_child = Resource::with_attributes(Resource::new(), [
    ("string.inherited", StringValue("child-string")),  // Override
    ("int.inherited", IntValue(200)),  // Override
    ("child.only", StringValue("child-only-value"))
  ])
  
  let typed_inherited = Resource::merge(typed_parent, typed_child)
  
  // Verify typed inheritance
  assert_eq(Resource::get_attribute(typed_inherited, "string.inherited"), Some(StringValue("child-string")))
  assert_eq(Resource::get_attribute(typed_inherited, "int.inherited"), Some(IntValue(200)))
  assert_eq(Resource::get_attribute(typed_inherited, "float.inherited"), Some(FloatValue(3.14)))
  assert_eq(Resource::get_attribute(typed_inherited, "bool.inherited"), Some(BoolValue(true)))
  assert_eq(Resource::get_attribute(typed_inherited, "child.only"), Some(StringValue("child-only-value")))
}

// Test 6: Resource metadata and versioning
pub test "premium resource metadata_versioning" {
  // Test resource with metadata attributes
  let metadata_resource = Resource::with_attributes(Resource::new(), [
    ("resource.name", StringValue("test-resource")),
    ("resource.version", StringValue("1.0.0")),
    ("resource.created", StringValue("2025-01-01T00:00:00Z")),
    ("resource.updated", StringValue("2025-01-01T01:00:00Z")),
    ("resource.owner", StringValue("test-team")),
    ("resource.environment", StringValue("development"))
  ])
  
  // Verify metadata attributes
  assert_eq(Resource::get_attribute(metadata_resource, "resource.name"), Some(StringValue("test-resource")))
  assert_eq(Resource::get_attribute(metadata_resource, "resource.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(metadata_resource, "resource.created"), Some(StringValue("2025-01-01T00:00:00Z")))
  assert_eq(Resource::get_attribute(metadata_resource, "resource.updated"), Some(StringValue("2025-01-01T01:00:00Z")))
  assert_eq(Resource::get_attribute(metadata_resource, "resource.owner"), Some(StringValue("test-team")))
  assert_eq(Resource::get_attribute(metadata_resource, "resource.environment"), Some(StringValue("development")))
  
  // Test resource versioning by updating version attribute
  let versioned_resource = Resource::with_attributes(metadata_resource, [
    ("resource.version", StringValue("1.1.0")),
    ("resource.updated", StringValue("2025-01-01T02:00:00Z")),
    ("resource.changes", StringValue("Added new features"))
  ])
  
  // Verify versioning
  assert_eq(Resource::get_attribute(versioned_resource, "resource.name"), Some(StringValue("test-resource")))
  assert_eq(Resource::get_attribute(versioned_resource, "resource.version"), Some(StringValue("1.1.0")))
  assert_eq(Resource::get_attribute(versioned_resource, "resource.created"), Some(StringValue("2025-01-01T00:00:00Z")))
  assert_eq(Resource::get_attribute(versioned_resource, "resource.updated"), Some(StringValue("2025-01-01T02:00:00Z")))
  assert_eq(Resource::get_attribute(versioned_resource, "resource.owner"), Some(StringValue("test-team")))
  assert_eq(Resource::get_attribute(versioned_resource, "resource.environment"), Some(StringValue("development")))
  assert_eq(Resource::get_attribute(versioned_resource, "resource.changes"), Some(StringValue("Added new features")))
  
  // Test resource with semantic versioning
  let semantic_versions = [
    ("1.0.0", StringValue("Initial release")),
    ("1.1.0", StringValue("Feature update")),
    ("1.1.1", StringValue("Bug fix")),
    ("2.0.0", StringValue("Major update")),
    ("2.1.0", StringValue("Feature update for v2"))
  ]
  
  let versioned_resources = []
  
  for i in 0...semantic_versions.length() - 1 {
    let version = semantic_versions[i].0
    let description = semantic_versions[i].1
    
    let version_resource = Resource::with_attributes(Resource::new(), [
      ("resource.name", StringValue("versioned-resource")),
      ("resource.version", StringValue(version)),
      ("resource.description", StringValue(description)),
      ("resource.build", StringValue("build." + (i + 1).to_string()))
    ])
    
    versioned_resources.push(version_resource)
  }
  
  // Verify semantic versioning
  for i in 0...versioned_resources.length() - 1 {
    let resource = versioned_resources[i]
    let version = semantic_versions[i].0
    let description = semantic_versions[i].1
    
    assert_eq(Resource::get_attribute(resource, "resource.name"), Some(StringValue("versioned-resource")))
    assert_eq(Resource::get_attribute(resource, "resource.version"), Some(StringValue(version)))
    assert_eq(Resource::get_attribute(resource, "resource.description"), Some(StringValue(description)))
    assert_eq(Resource::get_attribute(resource, "resource.build"), Some(StringValue("build." + (i + 1).to_string())))
  }
  
  // Test resource with dependency metadata
  let dependency_resource = Resource::with_attributes(Resource::new(), [
    ("resource.name", StringValue("dependent-resource")),
    ("resource.version", StringValue("1.0.0")),
    ("dependency.lib1.version", StringValue("2.1.0")),
    ("dependency.lib2.version", StringValue("1.5.3")),
    ("dependency.framework.version", StringValue("3.0.0")),
    ("dependency.os.version", StringValue("linux-5.15.0"))
  ])
  
  // Verify dependency metadata
  assert_eq(Resource::get_attribute(dependency_resource, "resource.name"), Some(StringValue("dependent-resource")))
  assert_eq(Resource::get_attribute(dependency_resource, "resource.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(dependency_resource, "dependency.lib1.version"), Some(StringValue("2.1.0")))
  assert_eq(Resource::get_attribute(dependency_resource, "dependency.lib2.version"), Some(StringValue("1.5.3")))
  assert_eq(Resource::get_attribute(dependency_resource, "dependency.framework.version"), Some(StringValue("3.0.0")))
  assert_eq(Resource::get_attribute(dependency_resource, "dependency.os.version"), Some(StringValue("linux-5.15.0")))
}

// Test 7: Resource isolation and namespace management
pub test "premium resource isolation_namespaces" {
  // Test resource isolation with namespaces
  let namespace1_resource = Resource::with_attributes(Resource::new(), [
    ("namespace1.service.name", StringValue("service1")),
    ("namespace1.service.version", StringValue("1.0.0")),
    ("namespace1.shared.attr", StringValue("namespace1-value"))
  ])
  
  let namespace2_resource = Resource::with_attributes(Resource::new(), [
    ("namespace2.service.name", StringValue("service2")),
    ("namespace2.service.version", StringValue("2.0.0")),
    ("namespace2.shared.attr", StringValue("namespace2-value"))
  ])
  
  let shared_resource = Resource::with_attributes(Resource::new(), [
    ("shared.attr", StringValue("shared-value")),
    ("namespace1.shared.attr", StringValue("namespace1-override")),
    ("namespace2.shared.attr", StringValue("namespace2-override"))
  ])
  
  // Verify namespace isolation
  assert_eq(Resource::get_attribute(namespace1_resource, "namespace1.service.name"), Some(StringValue("service1")))
  assert_eq(Resource::get_attribute(namespace1_resource, "namespace1.service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(namespace1_resource, "namespace1.shared.attr"), Some(StringValue("namespace1-value")))
  assert_eq(Resource::get_attribute(namespace1_resource, "namespace2.service.name"), None)
  
  assert_eq(Resource::get_attribute(namespace2_resource, "namespace2.service.name"), Some(StringValue("service2")))
  assert_eq(Resource::get_attribute(namespace2_resource, "namespace2.service.version"), Some(StringValue("2.0.0")))
  assert_eq(Resource::get_attribute(namespace2_resource, "namespace2.shared.attr"), Some(StringValue("namespace2-value")))
  assert_eq(Resource::get_attribute(namespace2_resource, "namespace1.service.name"), None)
  
  assert_eq(Resource::get_attribute(shared_resource, "shared.attr"), Some(StringValue("shared-value")))
  assert_eq(Resource::get_attribute(shared_resource, "namespace1.shared.attr"), Some(StringValue("namespace1-override")))
  assert_eq(Resource::get_attribute(shared_resource, "namespace2.shared.attr"), Some(StringValue("namespace2-override")))
  
  // Test resource merging across namespaces
  let merged_namespaces = Resource::merge(
    Resource::merge(namespace1_resource, namespace2_resource),
    shared_resource
  )
  
  // Verify cross-namespace merging
  assert_eq(Resource::get_attribute(merged_namespaces, "namespace1.service.name"), Some(StringValue("service1")))
  assert_eq(Resource::get_attribute(merged_namespaces, "namespace1.service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(merged_namespaces, "namespace2.service.name"), Some(StringValue("service2")))
  assert_eq(Resource::get_attribute(merged_namespaces, "namespace2.service.version"), Some(StringValue("2.0.0")))
  assert_eq(Resource::get_attribute(merged_namespaces, "shared.attr"), Some(StringValue("shared-value")))
  assert_eq(Resource::get_attribute(merged_namespaces, "namespace1.shared.attr"), Some(StringValue("namespace1-override")))
  assert_eq(Resource::get_attribute(merged_namespaces, "namespace2.shared.attr"), Some(StringValue("namespace2-override")))
  
  // Test resource with hierarchical namespaces
  let hierarchical_resource = Resource::with_attributes(Resource::new(), [
    ("level1.level2.level3.attr", StringValue("deep-value")),
    ("level1.level2.attr", StringValue("mid-value")),
    ("level1.attr", StringValue("top-value")),
    ("root.attr", StringValue("root-value"))
  ])
  
  // Verify hierarchical namespaces
  assert_eq(Resource::get_attribute(hierarchical_resource, "level1.level2.level3.attr"), Some(StringValue("deep-value")))
  assert_eq(Resource::get_attribute(hierarchical_resource, "level1.level2.attr"), Some(StringValue("mid-value")))
  assert_eq(Resource::get_attribute(hierarchical_resource, "level1.attr"), Some(StringValue("top-value")))
  assert_eq(Resource::get_attribute(hierarchical_resource, "root.attr"), Some(StringValue("root-value")))
  
  // Test resource with dynamic namespaces
  let dynamic_base = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("dynamic-service")),
    ("service.version", StringValue("1.0.0"))
  ])
  
  let env_namespace = "production"
  let team_namespace = "backend"
  let instance_namespace = "instance-12345"
  
  let dynamic_namespaced = Resource::with_attributes(dynamic_base, [
    (env_namespace + ".service.name", StringValue("prod-service")),
    (team_namespace + ".service.name", StringValue("backend-service")),
    (instance_namespace + ".service.name", StringValue("instance-service")),
    (env_namespace + "." + team_namespace + ".service.name", StringValue("prod-backend-service"))
  ])
  
  // Verify dynamic namespaces
  assert_eq(Resource::get_attribute(dynamic_namespaced, "service.name"), Some(StringValue("dynamic-service")))
  assert_eq(Resource::get_attribute(dynamic_namespaced, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(dynamic_namespaced, "production.service.name"), Some(StringValue("prod-service")))
  assert_eq(Resource::get_attribute(dynamic_namespaced, "backend.service.name"), Some(StringValue("backend-service")))
  assert_eq(Resource::get_attribute(dynamic_namespaced, "instance-12345.service.name"), Some(StringValue("instance-service")))
  assert_eq(Resource::get_attribute(dynamic_namespaced, "production.backend.service.name"), Some(StringValue("prod-backend-service")))
}