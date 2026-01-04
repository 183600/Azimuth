// Azimuth Premium Context Propagation Tests
// This file contains comprehensive test cases for context propagation across service boundaries

// Test 1: Context Key-Value Propagation
test "context key-value propagation" {
  // Define context key
  type ContextKey = {
    name: String,
    namespace: String
  }
  
  // Define context value
  type ContextValue = {
    data: String,
    metadata: Array[(String, String)]
  }
  
  // Define context entry
  type ContextEntry = {
    key: ContextKey,
    value: ContextValue
  }
  
  // Define context
  type Context = {
    entries: Array[ContextEntry],
    parent_id: Option[String],
    trace_id: String,
    span_id: String
  }
  
  // Create context key
  let create_context_key = fn(name: String, namespace: String) {
    {
      name,
      namespace
    }
  }
  
  // Create context value
  let create_context_value = fn(data: String, metadata: Array[(String, String)]) {
    {
      data,
      metadata
    }
  }
  
  // Create root context
  let create_root_context = fn(trace_id: String, span_id: String) {
    {
      entries: [],
      parent_id: None,
      trace_id,
      span_id
    }
  }
  
  // Create child context
  let create_child_context = fn(parent: Context, span_id: String) {
    {
      entries: parent.entries,
      parent_id: Some(parent.span_id),
      trace_id: parent.trace_id,
      span_id
    }
  }
  
  // Set context value
  let set_context_value = fn(context: Context, key: ContextKey, value: ContextValue) {
    let mut updated_entries = context.entries.filter(fn(entry) { 
      !(entry.key.name == key.name && entry.key.namespace == key.namespace) 
    })
    
    updated_entries = updated_entries.push({
      key,
      value
    })
    
    {
      entries: updated_entries,
      parent_id: context.parent_id,
      trace_id: context.trace_id,
      span_id: context.span_id
    }
  }
  
  // Get context value
  let get_context_value = fn(context: Context, key: ContextKey) {
    context.entries.find(fn(entry) { 
      entry.key.name == key.name && entry.key.namespace == key.namespace 
    })
  }
  
  // Test context key creation
  let user_key = create_context_key("user_id", "azimuth.auth")
  assert_eq(user_key.name, "user_id")
  assert_eq(user_key.namespace, "azimuth.auth")
  
  // Test context value creation
  let user_value = create_context_value("12345", [("source", "database"), ("verified", "true")])
  assert_eq(user_value.data, "12345")
  assert_eq(user_value.metadata.length(), 2)
  
  // Test root context creation
  let root_context = create_root_context("trace-123", "span-456")
  assert_eq(root_context.trace_id, "trace-123")
  assert_eq(root_context.span_id, "span-456")
  match root_context.parent_id {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  assert_eq(root_context.entries.length(), 0)
  
  // Test child context creation
  let child_context = create_child_context(root_context, "span-789")
  assert_eq(child_context.trace_id, "trace-123")
  assert_eq(child_context.span_id, "span-789")
  match child_context.parent_id {
    Some(parent_id) => assert_eq(parent_id, "span-456")
    None => assert_true(false)
  }
  assert_eq(child_context.entries.length(), 0)
  
  // Test setting context value
  let context_with_value = set_context_value(root_context, user_key, user_value)
  assert_eq(context_with_value.entries.length(), 1)
  
  // Test getting context value
  match get_context_value(context_with_value, user_key) {
    Some(entry) => {
      assert_eq(entry.value.data, "12345")
      assert_eq(entry.value.metadata.length(), 2)
    }
    None => assert_true(false)
  }
  
  // Test context inheritance
  let inherited_context = create_child_context(context_with_value, "span-999")
  assert_eq(inherited_context.entries.length(), 1)  // Should inherit parent entries
  
  match get_context_value(inherited_context, user_key) {
    Some(entry) => {
      assert_eq(entry.value.data, "12345")
    }
    None => assert_true(false)
  }
  
  // Test context isolation (child modifications don't affect parent)
  let request_key = create_context_key("request_id", "azimuth.tracing")
  let request_value = create_context_value("req-789", [])
  
  let modified_child = set_context_value(inherited_context, request_key, request_value)
  assert_eq(modified_child.entries.length(), 2)  // Original + new
  
  // Parent should still have only one entry
  assert_eq(context_with_value.entries.length(), 1)
  
  // Test context override in child
  let new_user_value = create_context_value("67890", [("source", "cache")])
  let override_child = set_context_value(modified_child, user_key, new_user_value)
  
  match get_context_value(override_child, user_key) {
    Some(entry) => {
      assert_eq(entry.value.data, "67890")  // Should be overridden
      assert_eq(entry.value.metadata.length(), 1)
    }
    None => assert_true(false)
  }
  
  // Parent should still have original value
  match get_context_value(context_with_value, user_key) {
    Some(entry) => {
      assert_eq(entry.value.data, "12345")  // Should be unchanged
    }
    None => assert_true(false)
  }
}

// Test 2: Cross-Process Context Propagation
test "cross-process context propagation" {
  // Define propagation format
  enum PropagationFormat {
    HttpHeader
    MessageQueue
    GrpcMetadata
    Custom
  }
  
  // Define propagator
  type Propagator = {
    format: PropagationFormat,
    headers: Array[(String, String)],
    metadata: Array[(String, String)]
  }
  
  // Define propagation carrier
  type Carrier = {
    format: PropagationFormat,
    data: Array[(String, String)]
  }
  
  // Create propagator
  let create_propagator = fn(format: PropagationFormat) {
    {
      format,
      headers: [],
      metadata: []
    }
  }
  
  // Create carrier
  let create_carrier = fn(format: PropagationFormat, data: Array[(String, String)]) {
    {
      format,
      data
    }
  }
  
  // Inject context into carrier
  let inject_context = fn(propagator: Propagator, context: Context) {
    match propagator.format {
      PropagationFormat::HttpHeader => {
        let mut headers = []
        
        // Inject trace context
        headers = headers.push(("x-trace-id", context.trace_id))
        headers = headers.push(("x-span-id", context.span_id))
        
        match context.parent_id {
          Some(parent_id) => headers = headers.push(("x-parent-span-id", parent_id))
          None => ()
        }
        
        // Inject context entries
        for entry in context.entries {
          let header_name = "x-azimuth-" + entry.key.namespace + "-" + entry.key.name
          let header_value = entry.value.data + "|" + entry.value.metadata.map(fn(pair) { 
            pair.0 + "=" + pair.1 
          }).reduce(fn(acc, pair) { 
            if acc == "" { pair } else { acc + "," + pair } 
          }, "")
          
          headers = headers.push((header_name, header_value))
        }
        
        create_carrier(PropagationFormat::HttpHeader, headers)
      }
      PropagationFormat::MessageQueue => {
        let mut message_data = []
        
        // Inject trace context as message properties
        message_data = message_data.push(("trace_id", context.trace_id))
        message_data = message_data.push(("span_id", context.span_id))
        
        match context.parent_id {
          Some(parent_id) => message_data = message_data.push(("parent_span_id", parent_id))
          None => ()
        }
        
        // Inject context entries
        for entry in context.entries {
          let key = entry.key.namespace + "." + entry.key.name
          let value = entry.value.data + "|" + entry.value.metadata.map(fn(pair) { 
            pair.0 + "=" + pair.1 
          }).reduce(fn(acc, pair) { 
            if acc == "" { pair } else { acc + "," + pair } 
          }, "")
          
          message_data = message_data.push((key, value))
        }
        
        create_carrier(PropagationFormat::MessageQueue, message_data)
      }
      _ => {
        create_carrier(propagator.format, [])
      }
    }
  }
  
  // Extract context from carrier
  let extract_context = fn(carrier: Carrier) {
    match carrier.format {
      PropagationFormat::HttpHeader => {
        let mut trace_id = ""
        let mut span_id = ""
        let mut parent_id = None
        let mut entries = []
        
        for (name, value) in carrier.data {
          if name == "x-trace-id" {
            trace_id = value
          } else if name == "x-span-id" {
            span_id = value
          } else if name == "x-parent-span-id" {
            parent_id = Some(value)
          } else if name.starts_with("x-azimuth-") {
            // Parse context entry
            let parts = name.split("-")
            if parts.length() >= 4 {
              let namespace = parts[2]
              let key_name = parts[3]
              
              let value_parts = value.split("|")
              if value_parts.length() >= 1 {
                let data = value_parts[0]
                let metadata_str = if value_parts.length() > 1 { value_parts[1] } else { "" }
                
                let metadata = if metadata_str == "" {
                  []
                } else {
                  metadata_str.split(",").map(fn(pair) {
                    let kv = pair.split("=")
                    if kv.length() == 2 { (kv[0], kv[1]) } else { ("", "") }
                  })
                }
                
                entries = entries.push({
                  key: { name: key_name, namespace },
                  value: { data, metadata }
                })
              }
            }
          }
        }
        
        {
          entries,
          parent_id,
          trace_id,
          span_id
        }
      }
      PropagationFormat::MessageQueue => {
        let mut trace_id = ""
        let mut span_id = ""
        let mut parent_id = None
        let mut entries = []
        
        for (key, value) in carrier.data {
          if key == "trace_id" {
            trace_id = value
          } else if key == "span_id" {
            span_id = value
          } else if key == "parent_span_id" {
            parent_id = Some(value)
          } else {
            // Parse context entry
            let parts = key.split(".")
            if parts.length() == 2 {
              let namespace = parts[0]
              let key_name = parts[1]
              
              let value_parts = value.split("|")
              if value_parts.length() >= 1 {
                let data = value_parts[0]
                let metadata_str = if value_parts.length() > 1 { value_parts[1] } else { "" }
                
                let metadata = if metadata_str == "" {
                  []
                } else {
                  metadata_str.split(",").map(fn(pair) {
                    let kv = pair.split("=")
                    if kv.length() == 2 { (kv[0], kv[1]) } else { ("", "") }
                  })
                }
                
                entries = entries.push({
                  key: { name: key_name, namespace },
                  value: { data, metadata }
                })
              }
            }
          }
        }
        
        {
          entries,
          parent_id,
          trace_id,
          span_id
        }
      }
      _ => {
        {
          entries: [],
          parent_id: None,
          trace_id: "",
          span_id: ""
        }
      }
    }
  }
  
  // Create context for testing
  let user_key = create_context_key("user_id", "azimuth.auth")
  let user_value = create_context_value("12345", [("source", "database")])
  
  let request_key = create_context_key("request_id", "azimuth.tracing")
  let request_value = create_context_value("req-789", [])
  
  let root_context = create_root_context("trace-123", "span-456")
  let context_with_values = set_context_value(
    set_context_value(root_context, user_key, user_value),
    request_key,
    request_value
  )
  
  // Test HTTP header propagation
  let http_propagator = create_propagator(PropagationFormat::HttpHeader)
  let http_carrier = inject_context(http_propagator, context_with_values)
  
  assert_eq(http_carrier.data.length(), 5)  // trace, span, user, request, parent (none)
  
  // Verify trace headers
  match http_carrier.data.find(fn(pair) { pair.0 == "x-trace-id" }) {
    Some(pair) => assert_eq(pair.1, "trace-123")
    None => assert_true(false)
  }
  
  match http_carrier.data.find(fn(pair) { pair.0 == "x-span-id" }) {
    Some(pair) => assert_eq(pair.1, "span-456")
    None => assert_true(false)
  }
  
  // Verify context headers
  match http_carrier.data.find(fn(pair) { pair.0 == "x-azimuth-auth-user_id" }) {
    Some(pair) => assert_eq(pair.1, "12345|source=database")
    None => assert_true(false)
  }
  
  // Test extraction from HTTP headers
  let extracted_context = extract_context(http_carrier)
  assert_eq(extracted_context.trace_id, "trace-123")
  assert_eq(extracted_context.span_id, "span-456")
  assert_eq(extracted_context.entries.length(), 2)
  
  match extracted_context.entries.find(fn(entry) { 
    entry.key.name == "user_id" && entry.key.namespace == "azimuth.auth" 
  }) {
    Some(entry) => {
      assert_eq(entry.value.data, "12345")
      assert_eq(entry.value.metadata.length(), 1)
    }
    None => assert_true(false)
  }
  
  // Test message queue propagation
  let mq_propagator = create_propagator(PropagationFormat::MessageQueue)
  let mq_carrier = inject_context(mq_propagator, context_with_values)
  
  assert_eq(mq_carrier.data.length(), 4)  // trace, span, user, request
  
  // Test extraction from message queue
  let extracted_mq_context = extract_context(mq_carrier)
  assert_eq(extracted_mq_context.trace_id, "trace-123")
  assert_eq(extracted_mq_context.span_id, "span-456")
  assert_eq(extracted_mq_context.entries.length(), 2)
}

// Test 3: Baggage Propagation
test "baggage propagation" {
  // Define baggage entry
  type BaggageEntry = {
    key: String,
    value: String,
    properties: Array[(String, String)]
  }
  
  // Define baggage
  type Baggage = {
    entries: Array[BaggageEntry]
  }
  
  // Create baggage entry
  let create_baggage_entry = fn(key: String, value: String, properties: Array[(String, String)]) {
    {
      key,
      value,
      properties
    }
  }
  
  // Create baggage
  let create_baggage = fn(entries: Array[BaggageEntry]) {
    {
      entries
    }
  }
  
  // Add baggage entry
  let add_baggage_entry = fn(baggage: Baggage, entry: BaggageEntry) {
    let mut updated_entries = baggage.entries.filter(fn(e) { e.key != entry.key })
    updated_entries = updated_entries.push(entry)
    
    {
      entries: updated_entries
    }
  }
  
  // Get baggage entry
  let get_baggage_entry = fn(baggage: Baggage, key: String) {
    baggage.entries.find(fn(entry) { entry.key == key })
  }
  
  // Serialize baggage to header format
  let serialize_baggage = fn(baggage: Baggage) {
    baggage.entries.map(fn(entry) {
      let mut serialized = entry.key + "=" + entry.value
      
      for (prop_key, prop_value) in entry.properties {
        serialized = serialized + ";" + prop_key + "=" + prop_value
      }
      
      serialized
    }).reduce(fn(acc, entry) { 
      if acc == "" { entry } else { acc + "," + entry } 
    }, "")
  }
  
  // Parse baggage from header format
  let parse_baggage = fn(baggage_str: String) {
    if baggage_str == "" {
      create_baggage([])
    } else {
      let entry_strs = baggage_str.split(",")
      let entries = entry_strs.map(fn(entry_str) {
        let parts = entry_str.split(";")
        let key_value = parts[0]
        let kv_parts = key_value.split("=")
        
        if kv_parts.length() == 2 {
          let key = kv_parts[0]
          let value = kv_parts[1]
          
          let properties = if parts.length() > 1 {
            parts.slice(1, parts.length()).map(fn(prop) {
              let prop_parts = prop.split("=")
              if prop_parts.length() == 2 {
                (prop_parts[0], prop_parts[1])
              } else {
                ("", "")
              }
            })
          } else {
            []
          }
          
          create_baggage_entry(key, value, properties)
        } else {
          create_baggage_entry("", "", [])
        }
      })
      
      create_baggage(entries)
    }
  }
  
  // Test baggage entry creation
  let entry1 = create_baggage_entry("user-id", "12345", [("source", "auth-service")])
  assert_eq(entry1.key, "user-id")
  assert_eq(entry1.value, "12345")
  assert_eq(entry1.properties.length(), 1)
  
  // Test baggage creation
  let baggage = create_baggage([entry1])
  assert_eq(baggage.entries.length(), 1)
  
  // Test adding baggage entry
  let entry2 = create_baggage_entry("request-id", "req-789", [])
  let baggage2 = add_baggage_entry(baggage, entry2)
  assert_eq(baggage2.entries.length(), 2)
  
  // Test getting baggage entry
  match get_baggage_entry(baggage2, "user-id") {
    Some(entry) => {
      assert_eq(entry.value, "12345")
      assert_eq(entry.properties.length(), 1)
    }
    None => assert_true(false)
  }
  
  // Test baggage override
  let entry1_updated = create_baggage_entry("user-id", "67890", [("source", "cache")])
  let baggage3 = add_baggage_entry(baggage2, entry1_updated)
  
  match get_baggage_entry(baggage3, "user-id") {
    Some(entry) => {
      assert_eq(entry.value, "67890")  // Should be updated
      assert_eq(entry.properties.length(), 1)
      match entry.properties.find(fn(p) { p.0 == "source" }) {
        Some(prop) => assert_eq(prop.1, "cache")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test baggage serialization
  let entry3 = create_baggage_entry("service", "api-gateway", [("version", "1.2.3"), ("region", "us-west")])
  let complex_baggage = create_baggage([entry1, entry2, entry3])
  
  let serialized = serialize_baggage(complex_baggage)
  assert_true(serialized.contains("user-id=12345"))
  assert_true(serialized.contains("request-id=req-789"))
  assert_true(serialized.contains("service=api-gateway"))
  assert_true(serialized.contains("version=1.2.3"))
  assert_true(serialized.contains("region=us-west"))
  
  // Test baggage parsing
  let parsed_baggage = parse_baggage(serialized)
  assert_eq(parsed_baggage.entries.length(), 3)
  
  match get_baggage_entry(parsed_baggage, "service") {
    Some(entry) => {
      assert_eq(entry.value, "api-gateway")
      assert_eq(entry.properties.length(), 2)
      
      match entry.properties.find(fn(p) { p.0 == "version" }) {
        Some(prop) => assert_eq(prop.1, "1.2.3")
        None => assert_true(false)
      }
      
      match entry.properties.find(fn(p) { p.0 == "region" }) {
        Some(prop) => assert_eq(prop.1, "us-west")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test empty baggage parsing
  let empty_parsed = parse_baggage("")
  assert_eq(empty_parsed.entries.length(), 0)
}

// Test 4: Context Correlation Across Services
test "context correlation across services" {
  // Define service context
  type ServiceContext = {
    service_name: String,
    context: Context,
    baggage: Baggage,
    timestamp: Int
  }
  
  // Define correlation map
  type CorrelationMap = {
    trace_id: String,
    service_contexts: Array[ServiceContext]
  }
  
  // Create service context
  let create_service_context = fn(service_name: String, context: Context, baggage: Baggage, timestamp: Int) {
    {
      service_name,
      context,
      baggage,
      timestamp
    }
  }
  
  // Create correlation map
  let create_correlation_map = fn(trace_id: String) {
    {
      trace_id,
      service_contexts: []
    }
  }
  
  // Add service context to correlation map
  let add_service_context = fn(correlation_map: CorrelationMap, service_context: ServiceContext) {
    {
      trace_id: correlation_map.trace_id,
      service_contexts: correlation_map.service_contexts.push(service_context)
    }
  }
  
  // Find service context by name
  let find_service_context = fn(correlation_map: CorrelationMap, service_name: String) {
    correlation_map.service_contexts.find(fn(ctx) { ctx.service_name == service_name })
  }
  
  // Get context flow
  let get_context_flow = fn(correlation_map: CorrelationMap) {
    correlation_map.service_contexts.sort_by(fn(a, b) { 
      if a.timestamp < b.timestamp { -1 } else if a.timestamp > b.timestamp { 1 } else { 0 }
    })
  }
  
  // Create test contexts
  let trace_id = "trace-12345"
  let root_context = create_root_context(trace_id, "span-root")
  
  let user_key = create_context_key("user_id", "azimuth.auth")
  let user_value = create_context_value("12345", [])
  let auth_context = set_context_value(root_context, user_key, user_value)
  
  // Create service contexts
  let gateway_context = create_child_context(auth_context, "span-gateway")
  let gateway_service_context = create_service_context(
    "api-gateway",
    gateway_context,
    create_baggage([]),
    1640995200000
  )
  
  let auth_service_context = create_service_context(
    "auth-service",
    create_child_context(gateway_context, "span-auth"),
    create_baggage([
      create_baggage_entry("auth-method", "jwt", [("algorithm", "HS256")])
    ]),
    1640995200100
  )
  
  let user_service_context = create_service_context(
    "user-service",
    create_child_context(auth_service_context.context, "span-user"),
    create_baggage([
      create_baggage_entry("cache-hit", "true", [])
    ]),
    1640995200200
  )
  
  // Create correlation map
  let correlation_map = create_correlation_map(trace_id)
  let correlation_map1 = add_service_context(correlation_map, gateway_service_context)
  let correlation_map2 = add_service_context(correlation_map1, auth_service_context)
  let correlation_map3 = add_service_context(correlation_map2, user_service_context)
  
  // Test correlation map
  assert_eq(correlation_map3.trace_id, trace_id)
  assert_eq(correlation_map3.service_contexts.length(), 3)
  
  // Test finding service context
  match find_service_context(correlation_map3, "auth-service") {
    Some(service_ctx) => {
      assert_eq(service_ctx.service_name, "auth-service")
      assert_eq(service_ctx.context.span_id, "span-auth")
      match service_ctx.context.parent_id {
        Some(parent_id) => assert_eq(parent_id, "span-gateway")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test context flow
  let flow = get_context_flow(correlation_map3)
  assert_eq(flow.length(), 3)
  assert_eq(flow[0].service_name, "api-gateway")
  assert_eq(flow[1].service_name, "auth-service")
  assert_eq(flow[2].service_name, "user-service")
  
  // Test context propagation
  match get_context_value(flow[2].context, user_key) {
    Some(entry) => assert_eq(entry.value.data, "12345")
    None => assert_true(false)
  }
  
  // Test baggage propagation
  match get_baggage_entry(flow[1].baggage, "auth-method") {
    Some(entry) => {
      assert_eq(entry.value, "jwt")
      assert_eq(entry.properties.length(), 1)
    }
    None => assert_true(false)
  }
  
  match get_baggage_entry(flow[2].baggage, "cache-hit") {
    Some(entry) => assert_eq(entry.value, "true")
    None => assert_true(false)
  }
  
  // Test trace consistency
  for service_ctx in correlation_map3.service_contexts {
    assert_eq(service_ctx.context.trace_id, trace_id)
  }
}

// Test 5: Context Propagation with Async Operations
test "context propagation with async operations" {
  // Define async operation
  type AsyncOperation = {
    id: String,
    name: String,
    context: Context,
    status: String
  }
  
  // Define async context manager
  type AsyncContextManager = {
    operations: Array[AsyncOperation],
    current_context: Context
  }
  
  // Create async operation
  let create_async_operation = fn(id: String, name: String, context: Context) {
    {
      id,
      name,
      context,
      status: "pending"
    }
  }
  
  // Create async context manager
  let create_async_context_manager = fn(initial_context: Context) {
    {
      operations: [],
      current_context: initial_context
    }
  }
  
  // Start async operation
  let start_async_operation = fn(manager: AsyncContextManager, id: String, name: String) {
    let operation = create_async_operation(id, name, manager.current_context)
    let updated_operations = manager.operations.push(operation)
    
    {
      operations: updated_operations,
      current_context: manager.current_context
    }
  }
  
  // Complete async operation
  let complete_async_operation = fn(manager: AsyncContextManager, id: String, result_context: Context) {
    let updated_operations = manager.operations.map(fn(op) {
      if op.id == id {
        { op | status: "completed", context: result_context }
      } else {
        op
      }
    })
    
    {
      operations: updated_operations,
      current_context: result_context
    }
  }
  
  // Get operation context
  let get_operation_context = fn(manager: AsyncContextManager, id: String) {
    manager.operations.find(fn(op) { op.id == id })
  }
  
  // Create test context
  let root_context = create_root_context("trace-async", "span-root")
  let correlation_key = create_context_key("correlation_id", "azimuth.tracing")
  let correlation_value = create_context_value("corr-12345", [])
  let initial_context = set_context_value(root_context, correlation_key, correlation_value)
  
  // Create async context manager
  let manager = create_async_context_manager(initial_context)
  
  // Start first async operation
  let manager1 = start_async_operation(manager, "op-1", "fetch-user")
  
  // Modify context for first operation
  let user_key = create_context_key("user_id", "azimuth.auth")
  let user_value = create_context_value("12345", [])
  let op1_context = set_context_value(manager1.current_context, user_key, user_value)
  
  // Complete first operation
  let manager2 = complete_async_operation(manager1, "op-1", op1_context)
  
  // Start second async operation (should inherit from completed first operation)
  let manager3 = start_async_operation(manager2, "op-2", "fetch-orders")
  
  // Modify context for second operation
  let orders_key = create_context_key("order_count", "azimuth.orders")
  let orders_value = create_context_value("5", [])
  let op2_context = set_context_value(manager3.current_context, orders_key, orders_value)
  
  // Complete second operation
  let manager4 = complete_async_operation(manager3, "op-2", op2_context)
  
  // Test async operations
  assert_eq(manager4.operations.length(), 2)
  
  // Test first operation context
  match get_operation_context(manager4, "op-1") {
    Some(op) => {
      assert_eq(op.status, "completed")
      assert_eq(op.context.span_id, "span-root")
      
      match get_context_value(op.context, correlation_key) {
        Some(entry) => assert_eq(entry.value.data, "corr-12345")
        None => assert_true(false)
      }
      
      match get_context_value(op.context, user_key) {
        Some(entry) => assert_eq(entry.value.data, "12345")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test second operation context
  match get_operation_context(manager4, "op-2") {
    Some(op) => {
      assert_eq(op.status, "completed")
      
      // Should inherit from first operation
      match get_context_value(op.context, correlation_key) {
        Some(entry) => assert_eq(entry.value.data, "corr-12345")
        None => assert_true(false)
      }
      
      match get_context_value(op.context, user_key) {
        Some(entry) => assert_eq(entry.value.data, "12345")
        None => assert_true(false)
      }
      
      // Should have its own additions
      match get_context_value(op.context, orders_key) {
        Some(entry) => assert_eq(entry.value.data, "5")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test current context after operations
  assert_eq(manager4.current_context.trace_id, "trace-async")
  
  match get_context_value(manager4.current_context, correlation_key) {
    Some(entry) => assert_eq(entry.value.data, "corr-12345")
    None => assert_true(false)
  }
  
  match get_context_value(manager4.current_context, user_key) {
    Some(entry) => assert_eq(entry.value.data, "12345")
    None => assert_true(false)
  }
  
  match get_context_value(manager4.current_context, orders_key) {
    Some(entry) => assert_eq(entry.value.data, "5")
    None => assert_true(false)
  }
}