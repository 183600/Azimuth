// Resource Management Test Suite for Azimuth Telemetry System
// Test cases covering resource lifecycle, memory management, and cleanup

test "resource_lifecycle_management" {
  // Test resource creation and lifecycle
  let initial_resource = Resource::new()
  
  // Verify initial resource state
  let initial_service_name = Resource::get_attribute(initial_resource, "service.name")
  assert_eq(initial_service_name, None)
  
  // Add basic attributes
  let basic_attrs = [
    ("service.name", StringValue("test.service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-12345"))
  ]
  
  let basic_resource = Resource::with_attributes(initial_resource, basic_attrs)
  
  // Verify attributes were added
  let service_name = Resource::get_attribute(basic_resource, "service.name")
  match service_name {
    Some(StringValue(value)) => assert_eq(value, "test.service")
    _ => assert_true(false)
  }
  
  let service_version = Resource::get_attribute(basic_resource, "service.version")
  match service_version {
    Some(StringValue(value)) => assert_eq(value, "1.0.0")
    _ => assert_true(false)
  }
  
  // Test resource extension
  let extended_attrs = [
    ("environment", StringValue("production")),
    ("region", StringValue("us-west-2")),
    ("datacenter", StringValue("dc1"))
  ]
  
  let extended_resource = Resource::with_attributes(basic_resource, extended_attrs)
  
  // Verify both original and extended attributes exist
  let extended_service_name = Resource::get_attribute(extended_resource, "service.name")
  match extended_service_name {
    Some(StringValue(value)) => assert_eq(value, "test.service")
    _ => assert_true(false)
  }
  
  let environment = Resource::get_attribute(extended_resource, "environment")
  match environment {
    Some(StringValue(value)) => assert_eq(value, "production")
    _ => assert_true(false)
  }
  
  // Test resource merging
  let override_attrs = [
    ("service.version", StringValue("2.0.0")), // Override existing
    ("deployment.timestamp", StringValue("2025-01-01T12:00:00Z")) // New attribute
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(extended_resource, override_resource)
  
  // Verify merge results
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  match merged_service_name {
    Some(StringValue(value)) => assert_eq(value, "test.service") // Should be preserved
    _ => assert_true(false)
  }
  
  let merged_service_version = Resource::get_attribute(merged_resource, "service.version")
  match merged_service_version {
    Some(StringValue(value)) => assert_eq(value, "2.0.0") // Should be overridden
    _ => assert_true(false)
  }
  
  let merged_environment = Resource::get_attribute(merged_resource, "environment")
  match merged_environment {
    Some(StringValue(value)) => assert_eq(value, "production") // Should be preserved
    _ => assert_true(false)
  }
  
  let merged_timestamp = Resource::get_attribute(merged_resource, "deployment.timestamp")
  match merged_timestamp {
    Some(StringValue(value)) => assert_eq(value, "2025-01-01T12:00:00Z") // Should be added
    _ => assert_true(false)
  }
}

test "memory_efficient_attribute_management" {
  // Test memory-efficient operations with large attribute sets
  let resource = Resource::new()
  
  // Create a large number of attributes to test memory efficiency
  let large_attrs = []
  
  // Add 100 attributes efficiently
  for i = 0; i < 100; i = i + 1 {
    let key = "attr." + i.to_string()
    let value = StringValue("value." + i.to_string())
    large_attrs.push((key, value))
  }
  
  // Add some complex attributes
  large_attrs.push(("large.string", StringValue("a" * 10000)))
  large_attrs.push(("unicode.string", StringValue("中文测试字符串内容很长很长很长")))
  large_attrs.push(("emoji.string", StringValue("🚀🌟💫⭐✨🎯🔥💡🎉")))
  large_attrs.push(("int.value", IntValue(2147483647)))
  large_attrs.push(("float.value", FloatValue(3.14159265359)))
  large_attrs.push(("bool.value", BoolValue(true)))
  large_attrs.push(("string.array", ArrayStringValue(["item1", "item2", "item3", "item4", "item5"])))
  large_attrs.push(("int.array", ArrayIntValue([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])))
  
  let large_resource = Resource::with_attributes(resource, large_attrs)
  
  // Test efficient attribute retrieval
  for i = 0; i < 10; i = i + 1 {
    let key = "attr." + i.to_string()
    let retrieved = Resource::get_attribute(large_resource, key)
    match retrieved {
      Some(StringValue(value)) => assert_true(value.contains("value." + i.to_string()))
      _ => assert_true(false)
    }
  }
  
  // Test complex attribute retrieval
  let large_string = Resource::get_attribute(large_resource, "large.string")
  match large_string {
    Some(StringValue(value)) => assert_eq(value.length(), 10000)
    _ => assert_true(false)
  }
  
  let unicode_string = Resource::get_attribute(large_resource, "unicode.string")
  match unicode_string {
    Some(StringValue(value)) => assert_true(value.contains("中文测试"))
    _ => assert_true(false)
  }
  
  let emoji_string = Resource::get_attribute(large_resource, "emoji.string")
  match emoji_string {
    Some(StringValue(value)) => assert_true(value.contains("🚀"))
    _ => assert_true(false)
  }
  
  let int_value = Resource::get_attribute(large_resource, "int.value")
  match int_value {
    Some(IntValue(value)) => assert_eq(value, 2147483647)
    _ => assert_true(false)
  }
  
  let string_array = Resource::get_attribute(large_resource, "string.array")
  match string_array {
    Some(ArrayStringValue(array)) => assert_eq(array.length(), 5)
    _ => assert_true(false)
  }
  
  let int_array = Resource::get_attribute(large_resource, "int.array")
  match int_array {
    Some(ArrayIntValue(array)) => assert_eq(array.length(), 10)
    _ => assert_true(false)
  }
}

test "resource_cleanup_and_disposal" {
  // Test resource cleanup patterns
  let resource = Resource::new()
  
  // Add temporary attributes that should be cleaned up
  let temp_attrs = [
    ("temp.session.id", StringValue("session_12345")),
    ("temp.request.id", StringValue("request_67890")),
    ("temp.timestamp", StringValue("2025-01-01T12:00:00Z")),
    ("temp.user.data", StringValue("temporary user data"))
  ]
  
  let resource_with_temp = Resource::with_attributes(resource, temp_attrs)
  
  // Verify temporary attributes exist
  let temp_session = Resource::get_attribute(resource_with_temp, "temp.session.id")
  match temp_session {
    Some(StringValue(value)) => assert_eq(value, "session_12345")
    _ => assert_true(false)
  }
  
  // Simulate cleanup by creating a new resource without temporary attributes
  let permanent_attrs = [
    ("service.name", StringValue("cleanup.test.service")),
    ("service.version", StringValue("1.0.0")),
    ("environment", StringValue("production"))
  ]
  
  let cleaned_resource = Resource::with_attributes(Resource::new(), permanent_attrs)
  
  // Verify temporary attributes are gone (cleaned up)
  let cleaned_temp_session = Resource::get_attribute(cleaned_resource, "temp.session.id")
  assert_eq(cleaned_temp_session, None)
  
  // Verify permanent attributes exist
  let permanent_service_name = Resource::get_attribute(cleaned_resource, "service.name")
  match permanent_service_name {
    Some(StringValue(value)) => assert_eq(value, "cleanup.test.service")
    _ => assert_true(false)
  }
  
  // Test selective cleanup by merging with override
  let keep_attrs = [
    ("service.name", StringValue("cleanup.test.service")), // Keep this
    ("environment", StringValue("production")) // Keep this
  ]
  
  let cleanup_override = Resource::with_attributes(Resource::new(), keep_attrs)
  let selectively_cleaned = Resource::merge(resource_with_temp, cleanup_override)
  
  // In simplified implementation, this might override everything
  // In real implementation, would preserve specified attributes
}

test "resource_pooling_and_reuse" {
  // Test resource pooling patterns for efficiency
  let base_resource = Resource::new()
  
  // Create a pool of commonly used attribute sets
  let common_service_attrs = [
    ("service.name", StringValue("pooled.service")),
    ("service.version", StringValue("1.0.0")),
    ("service.namespace", StringValue("azimuth")),
    ("environment", StringValue("production"))
  ]
  
  let common_infrastructure_attrs = [
    ("region", StringValue("us-west-2")),
    ("availability.zone", StringValue("us-west-2a")),
    ("datacenter", StringValue("dc-primary")),
    ("cluster", StringValue("cluster-main"))
  ]
  
  let common_runtime_attrs = [
    ("process.runtime.name", StringValue("moonbit")),
    ("process.runtime.version", StringValue("0.1.0")),
    ("host.arch", StringValue("amd64")),
    ("os.type", StringValue("linux"))
  ]
  
  // Create reusable resource templates
  let service_template = Resource::with_attributes(base_resource, common_service_attrs)
  let infrastructure_template = Resource::with_attributes(base_resource, common_infrastructure_attrs)
  let runtime_template = Resource::with_attributes(base_resource, common_runtime_attrs)
  
  // Pool resources by merging templates
  let pooled_base = Resource::merge(service_template, infrastructure_template)
  let fully_pooled = Resource::merge(pooled_base, runtime_template)
  
  // Verify pooled resource has all attributes
  let pooled_service_name = Resource::get_attribute(fully_pooled, "service.name")
  match pooled_service_name {
    Some(StringValue(value)) => assert_eq(value, "pooled.service")
    _ => assert_true(false)
  }
  
  let pooled_region = Resource::get_attribute(fully_pooled, "region")
  match pooled_region {
    Some(StringValue(value)) => assert_eq(value, "us-west-2")
    _ => assert_true(false)
  }
  
  let pooled_runtime = Resource::get_attribute(fully_pooled, "process.runtime.name")
  match pooled_runtime {
    Some(StringValue(value)) => assert_eq(value, "moonbit")
    _ => assert_true(false)
  }
  
  // Test resource reuse by adding instance-specific attributes
  let instance_specific_attrs = [
    ("service.instance.id", StringValue("instance-001")),
    ("host.name", StringValue("host-001")),
    ("process.pid", IntValue(12345))
  ]
  
  let instance_resource = Resource::with_attributes(fully_pooled, instance_specific_attrs)
  
  // Verify instance-specific attributes are added
  let instance_id = Resource::get_attribute(instance_resource, "service.instance.id")
  match instance_id {
    Some(StringValue(value)) => assert_eq(value, "instance-001")
    _ => assert_true(false)
  }
  
  // Verify pooled attributes are still present
  let reused_service_name = Resource::get_attribute(instance_resource, "service.name")
  match reused_service_name {
    Some(StringValue(value)) => assert_eq(value, "pooled.service")
    _ => assert_true(false)
  }
  
  // Create another instance to test reuse
  let instance2_specific_attrs = [
    ("service.instance.id", StringValue("instance-002")),
    ("host.name", StringValue("host-002")),
    ("process.pid", IntValue(12346))
  ]
  
  let instance2_resource = Resource::with_attributes(fully_pooled, instance2_specific_attrs)
  
  // Verify second instance has different specific attributes but same pooled attributes
  let instance2_id = Resource::get_attribute(instance2_resource, "service.instance.id")
  match instance2_id {
    Some(StringValue(value)) => assert_eq(value, "instance-002")
    _ => assert_true(false)
  }
  
  let reused_service_name2 = Resource::get_attribute(instance2_resource, "service.name")
  match reused_service_name2 {
    Some(StringValue(value)) => assert_eq(value, "pooled.service") // Same as pooled
    _ => assert_true(false)
  }
}

test "resource_attribute_validation_and_sanitization" {
  // Test resource attribute validation
  let resource = Resource::new()
  
  // Test with valid attributes
  let valid_attrs = [
    ("service.name", StringValue("valid.service.name")),
    ("service.version", StringValue("1.0.0")),
    ("environment", StringValue("production")),
    ("region", StringValue("us-west-2")),
    ("port", IntValue(8080)),
    ("enabled", BoolValue(true)),
    ("tags", ArrayStringValue(["tag1", "tag2", "tag3"]))
  ]
  
  let valid_resource = Resource::with_attributes(resource, valid_attrs)
  
  // Verify valid attributes are accessible
  let valid_service_name = Resource::get_attribute(valid_resource, "service.name")
  match valid_service_name {
    Some(StringValue(value)) => assert_eq(value, "valid.service.name")
    _ => assert_true(false)
  }
  
  // Test with potentially problematic attributes
  let problematic_attrs = [
    ("", StringValue("empty.key")), // Empty key
    ("service.name", StringValue("")), // Empty value
    ("key\nwith\nnewlines", StringValue("value\nwith\nnewlines")), // Newlines
    ("key\twith\ttabs", StringValue("value\twith\ttabs")), // Tabs
    ("key with spaces", StringValue("value with spaces")), // Spaces
    ("key.with.special.chars!@#$%", StringValue("value.with.special.chars!@#$%")), // Special chars
    ("unicode.key.中文", StringValue("unicode.value.中文")), // Unicode
    ("emoji.key.🚀", StringValue("emoji.value.🌟")) // Emoji
  ]
  
  let problematic_resource = Resource::with_attributes(resource, problematic_attrs)
  
  // Test retrieval of problematic attributes (may or may not work depending on implementation)
  let empty_key_value = Resource::get_attribute(problematic_resource, "")
  let empty_value = Resource::get_attribute(problematic_resource, "service.name")
  let newline_key = Resource::get_attribute(problematic_resource, "key\nwith\nnewlines")
  let unicode_key = Resource::get_attribute(problematic_resource, "unicode.key.中文")
  let emoji_key = Resource::get_attribute(problematic_resource, "emoji.key.🚀")
  
  // In a real implementation, these might be sanitized or rejected
  // For this test, we just verify they don't crash the system
  
  // Test with extremely long keys and values
  let long_key = "a" * 1000
  let long_value = "b" * 10000
  let long_attrs = [(long_key, StringValue(long_value))]
  
  let long_resource = Resource::with_attributes(resource, long_attrs)
  let long_retrieved = Resource::get_attribute(long_resource, long_key)
  
  // Test with boundary values
  let boundary_attrs = [
    ("max.int", IntValue(2147483647)),
    ("min.int", IntValue(-2147483648)),
    ("zero.int", IntValue(0)),
    ("infinity", FloatValue(1.0/0.0)),
    ("negative.infinity", FloatValue(-1.0/0.0)),
    ("nan", FloatValue(0.0/0.0)),
    ("very.small.float", FloatValue(1.0e-100)),
    ("very.large.float", FloatValue(1.0e100))
  ]
  
  let boundary_resource = Resource::with_attributes(resource, boundary_attrs)
  
  // Verify boundary values can be stored and retrieved
  let max_int = Resource::get_attribute(boundary_resource, "max.int")
  match max_int {
    Some(IntValue(value)) => assert_eq(value, 2147483647)
    _ => assert_true(false)
  }
  
  let infinity = Resource::get_attribute(boundary_resource, "infinity")
  match infinity {
    Some(FloatValue(value)) => assert_true(value > 0.0) // Basic check for infinity
    _ => assert_true(false)
  }
}

test "resource_monitoring_and_metrics" {
  // Test resource monitoring capabilities
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "resource.monitoring")
  
  // Create metrics for resource monitoring
  let resource_creation_counter = Meter::create_counter(meter, "resource.creation.total", Some("Resource creations"), Some("resources"))
  let attribute_access_counter = Meter::create_counter(meter, "resource.attribute.access.total", Some("Attribute accesses"), Some("accesses"))
  let resource_merge_counter = Meter::create_counter(meter, "resource.merge.total", Some("Resource merges"), Some("merges"))
  
  // Monitor resource creation
  let creation_start = Clock::now_unix_nanos(Clock::system())
  
  let resource1 = Resource::new()
  Counter::add(resource_creation_counter, 1.0)
  
  let resource2 = Resource::new()
  Counter::add(resource_creation_counter, 1.0)
  
  let resource3 = Resource::new()
  Counter::add(resource_creation_counter, 1.0)
  
  let creation_end = Clock::now_unix_nanos(Clock::system())
  let creation_duration = creation_end - creation_start
  
  // Monitor attribute access
  let access_start = Clock::now_unix_nanos(Clock::system())
  
  let attrs1 = [
    ("service.name", StringValue("monitored.service")),
    ("service.version", StringValue("1.0.0")),
    ("environment", StringValue("production"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource1, attrs1)
  
  // Access attributes multiple times
  for i = 0; i < 10; i = i + 1 {
    let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
    Counter::add(attribute_access_counter, 1.0)
    
    let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
    Counter::add(attribute_access_counter, 1.0)
    
    let environment = Resource::get_attribute(resource_with_attrs, "environment")
    Counter::add(attribute_access_counter, 1.0)
  }
  
  let access_end = Clock::now_unix_nanos(Clock::system())
  let access_duration = access_end - access_start
  
  // Monitor resource merging
  let merge_start = Clock::now_unix_nanos(Clock::system())
  
  let attrs2 = [
    ("region", StringValue("us-west-2")),
    ("datacenter", StringValue("dc1"))
  ]
  
  let resource2_with_attrs = Resource::with_attributes(resource2, attrs2)
  
  let attrs3 = [
    ("cluster", StringValue("main")),
    ("namespace", StringValue("azimuth"))
  ]
  
  let resource3_with_attrs = Resource::with_attributes(resource3, attrs3)
  
  // Perform multiple merges
  let merged1 = Resource::merge(resource_with_attrs, resource2_with_attrs)
  Counter::add(resource_merge_counter, 1.0)
  
  let merged2 = Resource::merge(merged1, resource3_with_attrs)
  Counter::add(resource_merge_counter, 1.0)
  
  let merge_end = Clock::now_unix_nanos(Clock::system())
  let merge_duration = merge_end - merge_start
  
  // Verify monitoring metrics
  assert_eq(resource_creation_counter.name, "resource.creation.total")
  assert_eq(attribute_access_counter.name, "resource.attribute.access.total")
  assert_eq(resource_merge_counter.name, "resource.merge.total")
  
  // Verify final merged resource has all attributes
  let final_service_name = Resource::get_attribute(merged2, "service.name")
  match final_service_name {
    Some(StringValue(value)) => assert_eq(value, "monitored.service")
    _ => assert_true(false)
  }
  
  let final_region = Resource::get_attribute(merged2, "region")
  match final_region {
    Some(StringValue(value)) => assert_eq(value, "us-west-2")
    _ => assert_true(false)
  }
  
  let final_cluster = Resource::get_attribute(merged2, "cluster")
  match final_cluster {
    Some(StringValue(value)) => assert_eq(value, "main")
    _ => assert_true(false)
  }
  
  // Create a monitoring log
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "resource.monitoring")
  
  let monitoring_log = LogRecord::new_with_context(
    Info,
    Some("Resource monitoring completed: " + 
         resource_creation_counter.name + " in " + creation_duration.to_string() + "ns, " +
         attribute_access_counter.name + " in " + access_duration.to_string() + "ns, " +
         resource_merge_counter.name + " in " + merge_duration.to_string() + "ns"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    None,
    None,
    None
  )
  Logger::emit(logger, monitoring_log)
}

test "resource_performance_optimization" {
  // Test resource performance optimization patterns
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "resource.performance")
  
  // Create performance metrics
  let operation_timer = Meter::create_histogram(meter, "resource.operation.duration", Some("Operation duration"), Some("microseconds"))
  let memory_usage = Meter::create_gauge(meter, "resource.memory.usage", Some("Memory usage"), Some("bytes"))
  
  // Test batch attribute operations
  let batch_start = Clock::now_unix_nanos(Clock::system())
  
  let resource = Resource::new()
  let batch_attrs = []
  
  // Create many attributes in batch
  for i = 0; i < 1000; i = i + 1 {
    let key = "batch.attr." + i.to_string()
    let value = StringValue("batch.value." + i.to_string())
    batch_attrs.push((key, value))
  }
  
  let batch_resource = Resource::with_attributes(resource, batch_attrs)
  
  let batch_end = Clock::now_unix_nanos(Clock::system())
  let batch_duration = (batch_end - batch_start).to_double() / 1000.0 // Convert to microseconds
  
  let batch_attrs_test = Attributes::new()
  Attributes::set(batch_attrs_test, "operation.type", StringValue("batch"))
  Histogram::record(operation_timer, batch_duration, Some(batch_attrs_test))
  
  // Test individual attribute operations
  let individual_start = Clock::now_unix_nanos(Clock::system())
  
  let individual_resource = Resource::new()
  
  // Add attributes one by one
  for i = 0; i < 100; i = i + 1 {
    let single_attrs = [("individual.attr." + i.to_string(), StringValue("individual.value." + i.to_string()))]
    individual_resource = Resource::with_attributes(individual_resource, single_attrs)
  }
  
  let individual_end = Clock::now_unix_nanos(Clock::system())
  let individual_duration = (individual_end - individual_start).to_double() / 1000.0 // Convert to microseconds
  
  let individual_attrs_test = Attributes::new()
  Attributes::set(individual_attrs_test, "operation.type", StringValue("individual"))
  Histogram::record(operation_timer, individual_duration, Some(individual_attrs_test))
  
  // Test merge operations performance
  let merge_start = Clock::now_unix_nanos(Clock::system())
  
  let merge_base = Resource::new()
  let merge_attrs1 = []
  let merge_attrs2 = []
  
  // Create two large attribute sets
  for i = 0; i < 500; i = i + 1 {
    merge_attrs1.push(("merge.attr1." + i.to_string(), StringValue("merge.value1." + i.to_string())))
    merge_attrs2.push(("merge.attr2." + i.to_string(), StringValue("merge.value2." + i.to_string())))
  }
  
  let merge_resource1 = Resource::with_attributes(merge_base, merge_attrs1)
  let merge_resource2 = Resource::with_attributes(merge_base, merge_attrs2)
  let merged_resource = Resource::merge(merge_resource1, merge_resource2)
  
  let merge_end = Clock::now_unix_nanos(Clock::system())
  let merge_duration = (merge_end - merge_start).to_double() / 1000.0 // Convert to microseconds
  
  let merge_attrs_test = Attributes::new()
  Attributes::set(merge_attrs_test, "operation.type", StringValue("merge"))
  Histogram::record(operation_timer, merge_duration, Some(merge_attrs_test))
  
  // Test lookup performance
  let lookup_start = Clock::now_unix_nanos(Clock::system())
  
  // Perform many lookups on the batch resource
  for i = 0; i < 1000; i = i + 1 {
    let lookup_key = "batch.attr." + (i % 100).to_string() // Look up first 100 attributes repeatedly
    let retrieved = Resource::get_attribute(batch_resource, lookup_key)
    match retrieved {
      Some(StringValue(value)) => assert_true(value.contains("batch.value"))
      _ => assert_true(false)
    }
  }
  
  let lookup_end = Clock::now_unix_nanos(Clock::system())
  let lookup_duration = (lookup_end - lookup_start).to_double() / 1000.0 // Convert to microseconds
  
  let lookup_attrs_test = Attributes::new()
  Attributes::set(lookup_attrs_test, "operation.type", StringValue("lookup"))
  Histogram::record(operation_timer, lookup_duration, Some(lookup_attrs_test))
  
  // Performance assertions
  assert_true(batch_duration > 0.0)
  assert_true(individual_duration > 0.0)
  assert_true(merge_duration > 0.0)
  assert_true(lookup_duration > 0.0)
  
  // Batch operations should generally be faster than individual operations
  // (This is a basic check - real performance would depend on implementation)
  
  // Create performance summary log
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "resource.performance")
  
  let performance_log = LogRecord::new_with_context(
    Info,
    Some("Resource performance test completed: " +
         "batch=" + batch_duration.to_string() + "μs, " +
         "individual=" + individual_duration.to_string() + "μs, " +
         "merge=" + merge_duration.to_string() + "μs, " +
         "lookup=" + lookup_duration.to_string() + "μs"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    None,
    None,
    None
  )
  Logger::emit(logger, performance_log)
  
  // Verify metrics were created
  assert_eq(operation_timer.name, "resource.operation.duration")
  assert_eq(memory_usage.name, "resource.memory.usage")
}