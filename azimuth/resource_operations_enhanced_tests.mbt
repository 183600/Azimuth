// Resource Operations Enhanced Tests for Azimuth
// Tests for resource attribute management and merging functionality

test "resource_creation_and_attributes" {
  // Test creating a new resource with no attributes
  let empty_resource = Resource::new()
  @assertion.assert_eq(empty_resource.attributes.length, 0)?
  
  // Test creating resource with attributes
  let attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("test-host")),
    ("process.pid", IntValue(1234))
  ]
  let resource_with_attrs = Resource::with_attributes(empty_resource, attrs)
  @assertion.assert_eq(resource_with_attrs.attributes.length, 5)?
}

test "resource_attribute_retrieval" {
  // Test resource attribute retrieval
  let attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("port", IntValue(8080)),
    ("debug.enabled", BoolValue(true)),
    ("memory.limit", FloatValue(1024.5))
  ]
  let resource = Resource::with_attributes(Resource::new(), attrs)
  
  // Test string attribute retrieval
  match Resource::get_attribute(resource, "service.name") {
    Some(StringValue(name)) => @assertion.assert_eq(name, "azimuth-service")?
    _ => @test.fail("Expected StringValue for service.name")?
  }
  
  // Test int attribute retrieval
  match Resource::get_attribute(resource, "port") {
    Some(IntValue(port)) => @assertion.assert_eq(port, 8080)?
    _ => @test.fail("Expected IntValue for port")?
  }
  
  // Test bool attribute retrieval
  match Resource::get_attribute(resource, "debug.enabled") {
    Some(BoolValue(enabled)) => @assertion.assert_eq(enabled, true)?
    _ => @test.fail("Expected BoolValue for debug.enabled")?
  }
  
  // Test non-existent attribute
  let result = Resource::get_attribute(resource, "non.existent")
  @assertion.assert_eq(result, None)?
}

test "resource_merging_priority" {
  // Test resource merging with override priority
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("environment", StringValue("development"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("environment", StringValue("production")),
    ("deployment.region", StringValue("us-west-2"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // Merge resources
  let merged = Resource::merge(base_resource, override_resource)
  
  // Verify override resource takes precedence
  match Resource::get_attribute(merged, "service.name") {
    Some(StringValue(name)) => @assertion.assert_eq(name, "override-service")?
    _ => @test.fail("Expected overridden service.name")?
  }
  
  match Resource::get_attribute(merged, "environment") {
    Some(StringValue(env)) => @assertion.assert_eq(env, "production")?
    _ => @test.fail("Expected overridden environment")?
  }
  
  match Resource::get_attribute(merged, "deployment.region") {
    Some(StringValue(region)) => @assertion.assert_eq(region, "us-west-2")?
    _ => @test.fail("Expected deployment.region from override")?
  }
}

test "resource_complex_attribute_types" {
  // Test resource with complex attribute types
  let complex_attrs = [
    ("service.tags", ArrayStringValue(["web", "api", "microservice"])),
    ("cluster.nodes", ArrayIntValue([1, 2, 3, 4, 5])),
    ("service.startup.time", FloatValue(3.14159)),
    ("maintenance.mode", BoolValue(false))
  ]
  let resource = Resource::with_attributes(Resource::new(), complex_attrs)
  
  // Test array string attribute
  match Resource::get_attribute(resource, "service.tags") {
    Some(ArrayStringValue(tags)) => {
      @assertion.assert_eq(tags.length, 3)?
      @assertion.assert_eq(tags[0], "web")?
      @assertion.assert_eq(tags[1], "api")?
      @assertion.assert_eq(tags[2], "microservice")?
    }
    _ => @test.fail("Expected ArrayStringValue for service.tags")?
  }
  
  // Test array int attribute
  match Resource::get_attribute(resource, "cluster.nodes") {
    Some(ArrayIntValue(nodes)) => {
      @assertion.assert_eq(nodes.length, 5)?
      @assertion.assert_eq(nodes[0], 1)?
      @assertion.assert_eq(nodes[4], 5)?
    }
    _ => @test.fail("Expected ArrayIntValue for cluster.nodes")?
  }
  
  // Test float attribute
  match Resource::get_attribute(resource, "service.startup.time") {
    Some(FloatValue(time)) => @assertion.assert_eq(time, 3.14159)?
    _ => @test.fail("Expected FloatValue for service.startup.time")?
  }
}

test "resource_empty_and_edge_cases" {
  // Test empty resource operations
  let empty_resource = Resource::new()
  let result = Resource::get_attribute(empty_resource, "any.key")
  @assertion.assert_eq(result, None)?
  
  // Test resource with empty attributes array
  let resource_with_empty_attrs = Resource::with_attributes(empty_resource, [])
  @assertion.assert_eq(resource_with_empty_attrs.attributes.length, 0)?
  
  // Test merging with empty resources
  let attrs = [("test.key", StringValue("test.value"))]
  let populated_resource = Resource::with_attributes(Resource::new(), attrs)
  let merged_with_empty = Resource::merge(populated_resource, empty_resource)
  
  match Resource::get_attribute(merged_with_empty, "test.key") {
    Some(StringValue(value)) => @assertion.assert_eq(value, "test.value")?
    _ => @test.fail("Expected test.key to be preserved")?
  }
}