// Azimuth Telemetry System - MoonBit Resource Management Tests
// This file contains test cases for resource management operations

// Define additional types needed for resource management testing
pub enum ResourceMergeStrategy {
  Override
  Merge
  Ignore
}

// Test 1: Resource creation and initialization
test "resource creation and initialization" {
  // Create empty resource
  let empty_resource = Resource({ attributes = [] })
  assert_eq(empty_resource.attributes.length(), 0)
  
  // Create resource with attributes
  let resource = Resource({ attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("service.namespace", StringValue("production"))
  ] })
  
  assert_eq(resource.attributes.length(), 4)
  
  // Verify attributes
  let mut found_service_name = false
  let mut found_service_version = false
  let mut found_instance_id = false
  let mut found_namespace = false
  
  for (key, value) in resource.attributes {
    match key {
      "service.name" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "azimuth-service")
            found_service_name = true
          }
          _ => assert_true(false)
        }
      }
      "service.version" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "1.0.0")
            found_service_version = true
          }
          _ => assert_true(false)
        }
      }
      "service.instance.id" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "instance-123")
            found_instance_id = true
          }
          _ => assert_true(false)
        }
      }
      "service.namespace" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "production")
            found_namespace = true
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)  // Unexpected key
    }
  }
  
  assert_true(found_service_name)
  assert_true(found_service_version)
  assert_true(found_instance_id)
  assert_true(found_namespace)
}

// Test 2: Resource attribute operations
test "resource attribute operations" {
  let resource = Resource({ attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.port", IntValue(8080)),
    ("service.ssl", BoolValue(true)),
    ("service.cpu.count", IntValue(4))
  ] })
  
  // Find specific attribute
  let mut found_port = false
  let mut found_ssl = false
  
  for (key, value) in resource.attributes {
    match key {
      "service.port" => {
        match value {
          IntValue(v) => {
            assert_eq(v, 8080)
            found_port = true
          }
          _ => assert_true(false)
        }
      }
      "service.ssl" => {
        match value {
          BoolValue(v) => {
            assert_true(v)
            found_ssl = true
          }
          _ => assert_true(false)
        }
      }
      _ => ()  // Ignore other keys
    }
  }
  
  assert_true(found_port)
  assert_true(found_ssl)
  
  // Filter attributes by key prefix
  let mut service_attributes = []
  for (key, value) in resource.attributes {
    if key.length() >= 8 && key[:8] == "service." {
      service_attributes = service_attributes @ [(key, value)]
    }
  }
  
  assert_eq(service_attributes.length(), 4)
}

// Test 3: Resource merging with Override strategy
test "resource merging with override strategy" {
  let resource1 = Resource({ attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("host.name", StringValue("host1"))
  ] })
  
  let resource2 = Resource({ attributes = [
    ("service.version", StringValue("2.0.0")),  // This should override
    ("service.port", IntValue(8080)),
    ("host.name", StringValue("host2"))  // This should override
  ] })
  
  // Simulate Override strategy (resource2 takes precedence)
  let mut merged_attributes = resource1.attributes
  let mut processed_keys = []
  
  // Add all unique keys from resource1
  for (key, value) in resource1.attributes {
    if !processed_keys.contains(key) {
      processed_keys = processed_keys @ [key]
    }
  }
  
  // Add/override with resource2
  for (key, value) in resource2.attributes {
    let mut found = false
    let mut index = 0
    
    // Find if key exists in merged_attributes
    for i in 0..merged_attributes.length() {
      let (existing_key, _) = merged_attributes[i]
      if existing_key == key {
        found = true
        index = i
        break
      }
    }
    
    if found {
      // Override existing value
      merged_attributes[index] = (key, value)
    } else {
      // Add new value
      merged_attributes = merged_attributes @ [(key, value)]
    }
    
    if !processed_keys.contains(key) {
      processed_keys = processed_keys @ [key]
    }
  }
  
  let merged_resource = Resource({ attributes = merged_attributes })
  
  // Verify merged resource
  assert_eq(merged_resource.attributes.length(), 4)
  
  // Verify overridden values
  let mut found_version = false
  let mut found_host = false
  
  for (key, value) in merged_resource.attributes {
    match key {
      "service.version" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "2.0.0")  // Should be from resource2
            found_version = true
          }
          _ => assert_true(false)
        }
      }
      "host.name" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "host2")  // Should be from resource2
            found_host = true
          }
          _ => assert_true(false)
        }
      }
      _ => ()  // Ignore other keys
    }
  }
  
  assert_true(found_version)
  assert_true(found_host)
}

// Test 4: Resource merging with Merge strategy
test "resource merging with merge strategy" {
  let resource1 = Resource({ attributes = [
    ("service.tags", ArrayStringValue(["tag1", "tag2"])),
    ("service.endpoints", ArrayStringValue(["/api/v1"]))
  ] })
  
  let resource2 = Resource({ attributes = [
    ("service.tags", ArrayStringValue(["tag3", "tag4"])),  // Should merge
    ("service.endpoints", ArrayStringValue(["/api/v2"]))  // Should merge
  ] })
  
  // Simulate Merge strategy for array attributes
  let mut merged_attributes = []
  let processed_keys = []
  
  // Process resource1
  for (key, value) in resource1.attributes {
    merged_attributes = merged_attributes @ [(key, value)]
  }
  
  // Process resource2 with merge logic
  for (key, value) in resource2.attributes {
    let mut found = false
    let mut index = 0
    
    // Find if key exists in merged_attributes
    for i in 0..merged_attributes.length() {
      let (existing_key, _) = merged_attributes[i]
      if existing_key == key {
        found = true
        index = i
        break
      }
    }
    
    if found {
      // Merge array values
      let (_, existing_value) = merged_attributes[index]
      match (existing_value, value) {
        (ArrayStringValue(arr1), ArrayStringValue(arr2)) => {
          let merged_array = arr1 @ arr2
          merged_attributes[index] = (key, ArrayStringValue(merged_array))
        }
        (ArrayIntValue(arr1), ArrayIntValue(arr2)) => {
          let merged_array = arr1 @ arr2
          merged_attributes[index] = (key, ArrayIntValue(merged_array))
        }
        _ => {
          // For non-array types, override
          merged_attributes[index] = (key, value)
        }
      }
    } else {
      // Add new value
      merged_attributes = merged_attributes @ [(key, value)]
    }
  }
  
  let merged_resource = Resource({ attributes = merged_attributes })
  
  // Verify merged resource
  assert_eq(merged_resource.attributes.length(), 2)
  
  // Verify merged arrays
  for (key, value) in merged_resource.attributes {
    match key {
      "service.tags" => {
        match value {
          ArrayStringValue(arr) => {
            assert_eq(arr.length(), 4)
            assert_eq(arr[0], "tag1")
            assert_eq(arr[1], "tag2")
            assert_eq(arr[2], "tag3")
            assert_eq(arr[3], "tag4")
          }
          _ => assert_true(false)
        }
      }
      "service.endpoints" => {
        match value {
          ArrayStringValue(arr) => {
            assert_eq(arr.length(), 2)
            assert_eq(arr[0], "/api/v1")
            assert_eq(arr[1], "/api/v2")
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)  // Unexpected key
    }
  }
}

// Test 5: Resource merging with Ignore strategy
test "resource merging with ignore strategy" {
  let resource1 = Resource({ attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("host.name", StringValue("host1"))
  ] })
  
  let resource2 = Resource({ attributes = [
    ("service.version", StringValue("2.0.0")),  // Should be ignored
    ("service.port", IntValue(8080)),           // Should be added
    ("host.name", StringValue("host2"))         // Should be ignored
  ] })
  
  // Simulate Ignore strategy (only add non-existing keys)
  let mut merged_attributes = resource1.attributes
  
  // Add only non-existing keys from resource2
  for (key, value) in resource2.attributes {
    let mut found = false
    
    // Check if key exists in resource1
    for (existing_key, _) in resource1.attributes {
      if existing_key == key {
        found = true
        break
      }
    }
    
    if !found {
      // Add new value
      merged_attributes = merged_attributes @ [(key, value)]
    }
  }
  
  let merged_resource = Resource({ attributes = merged_attributes })
  
  // Verify merged resource
  assert_eq(merged_resource.attributes.length(), 4)
  
  // Verify ignored values are not overridden
  let mut found_version = false
  let mut found_host = false
  let mut found_port = false
  
  for (key, value) in merged_resource.attributes {
    match key {
      "service.version" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "1.0.0")  // Should be from resource1 (ignored override)
            found_version = true
          }
          _ => assert_true(false)
        }
      }
      "host.name" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "host1")  // Should be from resource1 (ignored override)
            found_host = true
          }
          _ => assert_true(false)
        }
      }
      "service.port" => {
        match value {
          IntValue(v) => {
            assert_eq(v, 8080)  // Should be from resource2 (new key)
            found_port = true
          }
          _ => assert_true(false)
        }
      }
      _ => ()  // Ignore other keys
    }
  }
  
  assert_true(found_version)
  assert_true(found_host)
  assert_true(found_port)
}

// Test 6: Resource attribute filtering
test "resource attribute filtering" {
  let resource = Resource({ attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("host.name", StringValue("host1")),
    ("host.ip", StringValue("192.168.1.1")),
    ("process.pid", IntValue(1234)),
    ("process.name", StringValue("azimuth"))
  ] })
  
  // Filter by key prefix
  let mut service_attributes = []
  let mut host_attributes = []
  let mut process_attributes = []
  
  for (key, value) in resource.attributes {
    if key.length() >= 8 && key[:8] == "service." {
      service_attributes = service_attributes @ [(key, value)]
    } else if key.length() >= 5 && key[:5] == "host." {
      host_attributes = host_attributes @ [(key, value)]
    } else if key.length() >= 8 && key[:8] == "process." {
      process_attributes = process_attributes @ [(key, value)]
    }
  }
  
  assert_eq(service_attributes.length(), 2)
  assert_eq(host_attributes.length(), 2)
  assert_eq(process_attributes.length(), 2)
  
  // Verify service attributes
  let mut found_service_name = false
  for (key, value) in service_attributes {
    if key == "service.name" {
      match value {
        StringValue(v) => {
          assert_eq(v, "azimuth-service")
          found_service_name = true
        }
        _ => assert_true(false)
      }
    }
  }
  assert_true(found_service_name)
  
  // Verify host attributes
  let mut found_host_ip = false
  for (key, value) in host_attributes {
    if key == "host.ip" {
      match value {
        StringValue(v) => {
          assert_eq(v, "192.168.1.1")
          found_host_ip = true
        }
        _ => assert_true(false)
      }
    }
  }
  assert_true(found_host_ip)
  
  // Verify process attributes
  let mut found_process_pid = false
  for (key, value) in process_attributes {
    if key == "process.pid" {
      match value {
        IntValue(v) => {
          assert_eq(v, 1234)
          found_process_pid = true
        }
        _ => assert_true(false)
      }
    }
  }
  assert_true(found_process_pid)
}

// Test 7: Resource attribute validation
test "resource attribute validation" {
  // Test valid resource
  let valid_resource = Resource({ attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ] })
  
  // All attributes should be valid
  for (key, value) in valid_resource.attributes {
    assert_true(key.length() > 0)
    match value {
      StringValue(v) => assert_true(v.length() >= 0)
      IntValue(_) => assert_true(true)
      BoolValue(_) => assert_true(true)
      FloatValue(_) => assert_true(true)
      ArrayStringValue(arr) => assert_true(arr.length() >= 0)
      ArrayIntValue(arr) => assert_true(arr.length() >= 0)
    }
  }
  
  // Test edge cases
  let edge_case_resource = Resource({ attributes = [
    ("", StringValue("empty_key")),  // Empty key
    ("empty_value", StringValue("")),  // Empty string value
    ("zero_int", IntValue(0)),  // Zero value
    ("false_bool", BoolValue(false)),  // False value
    ("empty_array", ArrayStringValue([]))  // Empty array
  ] })
  
  assert_eq(edge_case_resource.attributes.length(), 5)
  
  // Verify empty key
  let (empty_key, empty_key_value) = edge_case_resource.attributes[0]
  assert_eq(empty_key, "")
  match empty_key_value {
    StringValue(v) => assert_eq(v, "empty_key")
    _ => assert_true(false)
  }
  
  // Verify empty string value
  let (empty_value_key, empty_value) = edge_case_resource.attributes[1]
  assert_eq(empty_value_key, "empty_value")
  match empty_value {
    StringValue(v) => assert_eq(v, "")
    _ => assert_true(false)
  }
  
  // Verify empty array
  let (empty_array_key, empty_array) = edge_case_resource.attributes[4]
  assert_eq(empty_array_key, "empty_array")
  match empty_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_true(false)
  }
}

// Test 8: Resource serialization preparation
test "resource serialization preparation" {
  let resource = Resource({ attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.port", IntValue(8080)),
    ("service.ssl", BoolValue(true)),
    ("service.tags", ArrayStringValue(["web", "api"])),
    ("service.endpoints", ArrayIntValue([8080, 8443]))
  ] })
  
  // Prepare for serialization by converting to string representations
  let mut serializable_pairs = []
  for (key, value) in resource.attributes {
    let string_value = match value {
      StringValue(v) => v
      IntValue(v) => v.to_string()
      FloatValue(v) => v.to_string()
      BoolValue(v) => if v { "true" } else { "false" }
      ArrayStringValue(arr) => {
        let mut result = "["
        let mut first = true
        for item in arr {
          if first {
            result = result + "\"" + item + "\""
            first = false
          } else {
            result = result + ",\"" + item + "\""
          }
        }
        result = result + "]"
        result
      }
      ArrayIntValue(arr) => {
        let mut result = "["
        let mut first = true
        for item in arr {
          if first {
            result = result + item.to_string()
            first = false
          } else {
            result = result + "," + item.to_string()
          }
        }
        result = result + "]"
        result
      }
    }
    serializable_pairs = serializable_pairs @ [(key, string_value)]
  }
  
  assert_eq(serializable_pairs.length(), 5)
  
  // Verify serialized values
  for (key, value) in serializable_pairs {
    match key {
      "service.name" => assert_eq(value, "azimuth-service")
      "service.port" => assert_eq(value, "8080")
      "service.ssl" => assert_eq(value, "true")
      "service.tags" => assert_eq(value, "[\"web\",\"api\"]")
      "service.endpoints" => assert_eq(value, "[8080,8443]")
      _ => assert_true(false)
    }
  }
}

// Test 9: Resource deep comparison
test "resource deep comparison" {
  let resource1 = Resource({ attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.port", IntValue(8080))
  ] })
  
  let resource2 = Resource({ attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.port", IntValue(8080))
  ] })
  
  let resource3 = Resource({ attributes = [
    ("service.name", StringValue("different-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.port", IntValue(8080))
  ] })
  
  // Compare resource1 and resource2 (should be equal)
  let mut equal_1_2 = true
  if resource1.attributes.length() != resource2.attributes.length() {
    equal_1_2 = false
  } else {
    for i in 0..resource1.attributes.length() {
      let (key1, value1) = resource1.attributes[i]
      let (key2, value2) = resource2.attributes[i]
      
      if key1 != key2 {
        equal_1_2 = false
        break
      }
      
      // Simple value comparison (in real implementation would be more robust)
      match (value1, value2) {
        (StringValue(v1), StringValue(v2)) => {
          if v1 != v2 { equal_1_2 = false; break }
        }
        (IntValue(v1), IntValue(v2)) => {
          if v1 != v2 { equal_1_2 = false; break }
        }
        (BoolValue(v1), BoolValue(v2)) => {
          if v1 != v2 { equal_1_2 = false; break }
        }
        _ => { equal_1_2 = false; break }
      }
    }
  }
  assert_true(equal_1_2)
  
  // Compare resource1 and resource3 (should be different)
  let mut equal_1_3 = true
  if resource1.attributes.length() != resource3.attributes.length() {
    equal_1_3 = false
  } else {
    for i in 0..resource1.attributes.length() {
      let (key1, value1) = resource1.attributes[i]
      let (key3, value3) = resource3.attributes[i]
      
      if key1 != key3 {
        equal_1_3 = false
        break
      }
      
      match (value1, value3) {
        (StringValue(v1), StringValue(v3)) => {
          if v1 != v3 { equal_1_3 = false; break }
        }
        (IntValue(v1), IntValue(v3)) => {
          if v1 != v3 { equal_1_3 = false; break }
        }
        (BoolValue(v1), BoolValue(v3)) => {
          if v1 != v3 { equal_1_3 = false; break }
        }
        _ => { equal_1_3 = false; break }
      }
    }
  }
  assert_false(equal_1_3)
}

// Test 10: Resource with special characters in keys and values
test "resource with special characters" {
  let special_resource = Resource({ attributes = [
    ("key.with.dots", StringValue("value.with.dots")),
    ("key-with-dashes", StringValue("value-with-dashes")),
    ("key_with_underscores", StringValue("value_with_underscores")),
    ("key/with/slashes", StringValue("value/with/slashes")),
    ("key with spaces", StringValue("value with spaces")),
    ("key.with.special.chars!@#$%", StringValue("value.with.special.chars!@#$%"))
  ] })
  
  assert_eq(special_resource.attributes.length(), 6)
  
  // Verify all special characters are preserved
  for (key, value) in special_resource.attributes {
    match value {
      StringValue(v) => {
        // Key and value should have the same structure
        assert_eq(key.replace(".", " ").replace("-", " ").replace("_", " ").replace("/", " "),
                 v.replace(".", " ").replace("-", " ").replace("_", " ").replace("/", " "))
      }
      _ => assert_true(false)
    }
  }
}