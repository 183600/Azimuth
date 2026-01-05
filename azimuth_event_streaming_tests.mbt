// Azimuth Event Streaming Test Suite
// This file contains test cases for event streaming and processing

// Test 1: Event Production and Publishing
test "event production and publishing mechanisms" {
  // Define event structure
  type Event = {
    id: String,
    type: String,
    source: String,
    subject: String,
    data_content_type: String,
    data: String,
    timestamp: Int,
    attributes: Array[(String, String)>
  }
  
  // Define producer configuration
  type ProducerConfig = {
    client_id: String,
    bootstrap_servers: Array[String],
    acks: String,  // "0", "1", "all"
    retries: Int,
    batch_size: Int,
    linger_ms: Int
  }
  
  // Define producer state
  type ProducerState = {
    config: ProducerConfig,
    pending_events: Array[Event>,
    sent_events: Array[Event],
    failed_events: Array[(Event, String)]
  }
  
  // Create producer
  let create_producer = fn(config: ProducerConfig) {
    {
      config,
      pending_events: [],
      sent_events: [],
      failed_events: []
    }
  }
  
  // Create event
  let create_event = fn(event_type: String, source: String, subject: String, data: String) {
    {
      id: "event-" + (1640995200 + (data.length() * 1000)).to_string(),
      type: event_type,
      source,
      subject,
      data_content_type: "application/json",
      data,
      timestamp: 1640995200,
      attributes: []
    }
  }
  
  // Add event to pending queue
  let send_event = fn(producer: ProducerState, event: Event) {
    { producer | pending_events: producer.pending_events.push(event) }
  }
  
  // Process pending events (simulate batch sending)
  let process_pending_events = fn(producer: ProducerState) {
    let batch_size = producer.config.batch_size
    let mut updated_producer = producer
    
    while updated_producer.pending_events.length() > 0 {
      let batch_size_to_send = if updated_producer.pending_events.length() < batch_size {
        updated_producer.pending_events.length()
      } else {
        batch_size
      }
      
      let batch = updated_producer.pending_events.slice(0, batch_size_to_send)
      
      // Simulate sending batch (95% success rate)
      let mut successful_events = []
      let mut failed_events = []
      
      for event in batch {
        if event.id.length() % 20 != 0 {  // Simple pseudo-random success
          successful_events = successful_events.push(event)
        } else {
          failed_events = failed_events.push((event, "Network timeout"))
        }
      }
      
      // Update producer state
      updated_producer = {
        config: updated_producer.config,
        pending_events: updated_producer.pending_events.slice(batch_size_to_send, 
                                                           updated_producer.pending_events.length()),
        sent_events: updated_producer.sent_events + successful_events,
        failed_events: updated_producer.failed_events + failed_events
      }
    }
    
    updated_producer
  }
  
  // Test producer creation
  let producer_config = {
    client_id: "telemetry-producer-1",
    bootstrap_servers: ["kafka-1:9092", "kafka-2:9092"],
    acks: "all",
    retries: 3,
    batch_size: 3,
    linger_ms: 10
  }
  
  let producer = create_producer(producer_config)
  assert_eq(producer.config.client_id, "telemetry-producer-1")
  assert_eq(producer.config.batch_size, 3)
  assert_eq(producer.pending_events.length(), 0)
  
  // Test event creation
  let user_event = create_event("user.created", "user-service", "user-123", 
                               "{\"id\": \"123\", \"name\": \"John Doe\"}")
  assert_eq(user_event.type, "user.created")
  assert_eq(user_event.source, "user-service")
  assert_eq(user_event.subject, "user-123")
  assert_eq(user_event.data_content_type, "application/json")
  assert_eq(user_event.timestamp, 1640995200)
  
  // Test event sending
  let producer1 = send_event(producer, user_event)
  assert_eq(producer1.pending_events.length(), 1)
  assert_eq(producer1.pending_events[0].id, user_event.id)
  
  // Send multiple events
  let order_event = create_event("order.placed", "order-service", "order-456", 
                                "{\"id\": \"456\", \"amount\": 99.99}")
  let payment_event = create_event("payment.processed", "payment-service", "payment-789", 
                                   "{\"id\": \"789\", \"status\": \"completed\"}")
  let inventory_event = create_event("inventory.updated", "inventory-service", "product-123", 
                                     "{\"id\": \"123\", \"quantity\": 99}")
  
  let producer2 = send_event(producer1, order_event)
  let producer3 = send_event(producer2, payment_event)
  let producer4 = send_event(producer3, inventory_event)
  
  assert_eq(producer4.pending_events.length(), 4)
  
  // Test batch processing
  let processed_producer = process_pending_events(producer4)
  assert_eq(processed_producer.pending_events.length(), 0)  // All events processed
  assert_true(processed_producer.sent_events.length() > 0)  // Some events sent successfully
  assert_eq(processed_producer.sent_events.length() + processed_producer.failed_events.length(), 4)
  
  // Test event attributes
  let add_event_attributes = fn(event: Event, attributes: Array[(String, String)]) {
    { event | attributes: event.attributes + attributes }
  }
  
  let enriched_event = add_event_attributes(user_event, [
    ("trace_id", "trace-1234567890"),
    ("span_id", "span-0987654321"),
    ("version", "1.0")
  ])
  
  assert_eq(enriched_event.attributes.length(), 3)
  assert_eq(enriched_event.attributes[0], ("trace_id", "trace-1234567890"))
  assert_eq(enriched_event.attributes[1], ("span_id", "span-0987654321"))
  assert_eq(enriched_event.attributes[2], ("version", "1.0"))
  
  // Test event validation
  let validate_event = fn(event: Event) {
    let mut errors = []
    
    if event.id.length() == 0 {
      errors = errors.push("Event ID cannot be empty")
    }
    
    if event.type.length() == 0 {
      errors = errors.push("Event type cannot be empty")
    }
    
    if event.source.length() == 0 {
      errors = errors.push("Event source cannot be empty")
    }
    
    if event.timestamp <= 0 {
      errors = errors.push("Event timestamp must be positive")
    }
    
    {
      is_valid: errors.length() == 0,
      errors
    }
  }
  
  let validation_result = validate_event(user_event)
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  let invalid_event = { user_event | id: "", type: "", timestamp: -1 }
  let invalid_validation = validate_event(invalid_event)
  assert_false(invalid_validation.is_valid)
  assert_eq(invalid_validation.errors.length(), 3)
}

// Test 2: Event Consumption and Processing
test "event consumption and processing patterns" {
  // Define consumer configuration
  type ConsumerConfig = {
    client_id: String,
    group_id: String,
    bootstrap_servers: Array[String],
    topics: Array[String],
    auto_offset_reset: String,  // "earliest", "latest"
    enable_auto_commit: Bool,
    max_poll_records: Int
  }
  
  // Define consumer state
  type ConsumerState = {
    config: ConsumerConfig,
    current_offset: Int,
    processed_events: Array[Event],
    unprocessed_events: Array[Event],
    committed_offsets: Array[(String, Int)]
  }
  
  // Define processing strategy
  enum ProcessingStrategy {
    AtMostOnce
    AtLeastOnce
    ExactlyOnce
  }
  
  // Create consumer
  let create_consumer = fn(config: ConsumerConfig) {
    {
      config,
      current_offset: 0,
      processed_events: [],
      unprocessed_events: [],
      committed_offsets: []
    }
  }
  
  // Poll events (simulate receiving from broker)
  let poll_events = fn(consumer: ConsumerState, max_records: Int) {
    // Simulate receiving events based on current offset
    let event_stream = [
      {
        id: "event-001",
        type: "user.created",
        source: "user-service",
        subject: "user-123",
        data_content_type: "application/json",
        data: "{\"id\": \"123\", \"name\": \"Alice\"}",
        timestamp: 1640995200,
        attributes: []
      },
      {
        id: "event-002",
        type: "order.placed",
        source: "order-service",
        subject: "order-456",
        data_content_type: "application/json",
        data: "{\"id\": \"456\", \"total\": 99.99}",
        timestamp: 1640995205,
        attributes: []
      },
      {
        id: "event-003",
        type: "payment.processed",
        source: "payment-service",
        subject: "payment-789",
        data_content_type: "application/json",
        data: "{\"id\": \"789\", \"status\": \"completed\"}",
        timestamp: 1640995210,
        attributes: []
      }
    ]
    
    let records_to_fetch = if max_records < event_stream.length() - consumer.current_offset {
      max_records
    } else {
      event_stream.length() - consumer.current_offset
    }
    
    let new_events = event_stream.slice(consumer.current_offset, 
                                       consumer.current_offset + records_to_fetch)
    
    { consumer | 
      current_offset: consumer.current_offset + records_to_fetch,
      unprocessed_events: consumer.unprocessed_events + new_events
    }
  }
  
  // Process events
  let process_events = fn(consumer: ConsumerState, processor: (Event) -> Bool) {
    let mut updated_consumer = consumer
    let mut successfully_processed = []
    let mut failed_events = []
    
    for event in updated_consumer.unprocessed_events {
      if processor(event) {
        successfully_processed = successfully_processed.push(event)
      } else {
        failed_events = failed_events.push(event)
      }
    }
    
    updated_consumer = {
      config: updated_consumer.config,
      current_offset: updated_consumer.current_offset,
      processed_events: updated_consumer.processed_events + successfully_processed,
      unprocessed_events: failed_events,
      committed_offsets: updated_consumer.committed_offsets
    }
    
    updated_consumer
  }
  
  // Commit offsets
  let commit_offsets = fn(consumer: ConsumerState) {
    let topic_offsets = consumer.config.topics.map(fn(topic) {
      (topic, consumer.current_offset)
    })
    
    { consumer | 
      committed_offsets: consumer.committed_offsets + topic_offsets
    }
  }
  
  // Test consumer creation
  let consumer_config = {
    client_id: "telemetry-consumer-1",
    group_id: "telemetry-processors",
    bootstrap_servers: ["kafka-1:9092"],
    topics: ["user-events", "order-events", "payment-events"],
    auto_offset_reset: "earliest",
    enable_auto_commit: false,
    max_poll_records: 10
  }
  
  let consumer = create_consumer(consumer_config)
  assert_eq(consumer.config.group_id, "telemetry-processors")
  assert_eq(consumer.config.topics.length(), 3)
  assert_eq(consumer.current_offset, 0)
  
  // Test event polling
  let consumer1 = poll_events(consumer, 2)
  assert_eq(consumer1.current_offset, 2)
  assert_eq(consumer1.unprocessed_events.length(), 2)
  assert_eq(consumer1.unprocessed_events[0].id, "event-001")
  assert_eq(consumer1.unprocessed_events[1].id, "event-002")
  
  // Test event processing
  let event_processor = fn(event: Event) {
    // Simple processor that succeeds for user events, fails for others
    event.type == "user.created"
  }
  
  let consumer2 = process_events(consumer1, event_processor)
  assert_eq(consumer2.processed_events.length(), 1)  // Only user.created event processed
  assert_eq(consumer2.processed_events[0].id, "event-001")
  assert_eq(consumer2.unprocessed_events.length(), 1)  // order.placed event failed
  assert_eq(consumer2.unprocessed_events[0].id, "event-002")
  
  // Test offset committing
  let consumer3 = commit_offsets(consumer2)
  assert_eq(consumer3.committed_offsets.length(), 3)  // 3 topics
  assert_eq(consumer3.committed_offsets[0], ("user-events", 2))
  assert_eq(consumer3.committed_offsets[1], ("order-events", 2))
  assert_eq(consumer3.committed_offsets[2], ("payment-events", 2))
  
  // Test event filtering
  let filter_events = fn(events: Array[Event], filter_fn: (Event) -> Bool) {
    events.filter(filter_fn)
  }
  
  let user_event_filter = fn(event: Event) {
    event.type.starts_with("user.")
  }
  
  let user_events = filter_events(consumer1.unprocessed_events, user_event_filter)
  assert_eq(user_events.length(), 1)
  assert_eq(user_events[0].type, "user.created")
  
  // Test event transformation
  let transform_events = fn(events: Array[Event], transformer: (Event) -> Event) {
    events.map(transformer)
  }
  
  let add_processing_timestamp = fn(event: Event) {
    { event | 
      attributes: event.attributes + [("processed_at", "1640995300")]
    }
  }
  
  let transformed_events = transform_events(consumer1.unprocessed_events, add_processing_timestamp)
  assert_eq(transformed_events.length(), 2)
  assert_eq(transformed_events[0].attributes.length(), 1)
  assert_eq(transformed_events[0].attributes[0], ("processed_at", "1640995300"))
  
  // Test event aggregation
  type EventAggregation = {
    key: String,
    count: Int,
    first_event: Event,
    last_event: Event,
    data_summary: String
  }
  
  let aggregate_events = fn(events: Array[Event], key_extractor: (Event) -> String) {
    let mut aggregations = []
    let mut processed_keys = []
    
    for event in events {
      let key = key_extractor(event)
      
      if not(processed_keys.contains(key)) {
        processed_keys = processed_keys.push(key)
        
        let key_events = events.filter(fn(e) { key_extractor(e) == key })
        let count = key_events.length()
        let first_event = key_events[0]
        let last_event = key_events[key_events.length() - 1]
        
        let data_summary = if key_events.length() > 0 {
          "Processed " + count.to_string() + " events of type " + first_event.type
        } else {
          "No events"
        }
        
        aggregations = aggregations.push({
          key,
          count,
          first_event,
          last_event,
          data_summary
        })
      }
    }
    
    aggregations
  }
  
  // Test event aggregation
  let aggregations = aggregate_events(consumer1.unprocessed_events, fn(e) { e.type })
  assert_eq(aggregations.length(), 2)
  
  let user_aggregation = aggregations.find(fn(a) { a.key == "user.created" })
  match user_aggregation {
    Some(agg) => {
      assert_eq(agg.count, 1)
      assert_eq(agg.first_event.id, "event-001")
      assert_eq(agg.last_event.id, "event-001")
      assert_eq(agg.data_summary, "Processed 1 events of type user.created")
    }
    None => assert_true(false)
  }
  
  // Test dead letter queue handling
  type DeadLetterEvent = {
    original_event: Event,
    error_message: String,
    failure_timestamp: Int,
    retry_count: Int
  }
  
  let handle_failed_events = fn(failed_events: Array[Event], error_messages: Array[String]) {
    let mut dead_letter_events = []
    
    for i in 0..failed_events.length() {
      let event = failed_events[i]
      let error_message = if i < error_messages.length() {
        error_messages[i]
      } else {
        "Unknown error"
      }
      
      dead_letter_events = dead_letter_events.push({
        original_event: event,
        error_message,
        failure_timestamp: 1640995300,
        retry_count: 3
      })
    }
    
    dead_letter_events
  }
  
  let dead_letter_events = handle_failed_events(consumer2.unprocessed_events, ["Processing failed"])
  assert_eq(dead_letter_events.length(), 1)
  assert_eq(dead_letter_events[0].original_event.id, "event-002")
  assert_eq(dead_letter_events[0].error_message, "Processing failed")
  assert_eq(dead_letter_events[0].retry_count, 3)
}

// Test 3: Event Schema and Evolution
test "event schema management and evolution" {
  // Define schema version
  type SchemaVersion = {
    major: Int,
    minor: Int,
    patch: Int
  }
  
  // Define field definition
  type FieldDefinition = {
    name: String,
    type: String,
    required: Bool,
    default_value: Option[String],
    description: String
  }
  
  // Define event schema
  type EventSchema = {
    name: String,
    version: SchemaVersion,
    fields: Array[FieldDefinition>,
    compatibility_mode: String  // "BACKWARD", "FORWARD", "FULL", "NONE"
  }
  
  // Define schema registry
  type SchemaRegistry = {
    schemas: Array[EventSchema>,
    default_compatibility: String
  }
  
  // Create schema registry
  let create_schema_registry = fn(default_compatibility: String) {
    {
      schemas: [],
      default_compatibility
    }
  }
  
  // Register schema
  let register_schema = fn(registry: SchemaRegistry, schema: EventSchema) {
    let existing = registry.schemas.find_index(fn(s) { 
      s.name == schema.name && s.version.major == schema.version.major 
    })
    
    match existing {
      Some(index) => {
        // Update existing schema
        let updated_schemas = registry.schemas.slice(0, index) + 
                             [schema] + 
                             registry.schemas.slice(index + 1, registry.schemas.length())
        { registry | schemas: updated_schemas }
      }
      None => {
        // Add new schema
        { registry | schemas: registry.schemas.push(schema) }
      }
    }
  }
  
  // Get latest schema for event type
  let get_latest_schema = fn(registry: SchemaRegistry, event_name: String) {
    let name_schemas = registry.schemas.filter(fn(s) { s.name == event_name })
    
    if name_schemas.length() == 0 {
      None
    } else {
      // Find schema with highest version
      let latest = name_schemas.reduce(fn(max, schema) {
        if schema.version.major > max.version.major ||
           (schema.version.major == max.version.major && schema.version.minor > max.version.minor) ||
           (schema.version.major == max.version.major && schema.version.minor == max.version.minor && schema.version.patch > max.version.patch) {
          schema
        } else {
          max
        }
      }, name_schemas[0])
      
      Some(latest)
    }
  }
  
  // Validate event against schema
  let validate_event_against_schema = fn(event_data: String, schema: EventSchema) {
    // Simplified validation - just check if required fields are present in JSON
    let mut errors = []
    
    for field in schema.fields {
      if field.required {
        if not(event_data.contains("\"" + field.name + "\"")) {
          errors = errors.push("Missing required field: " + field.name)
        }
      }
    }
    
    {
      is_valid: errors.length() == 0,
      errors
    }
  }
  
  // Check schema compatibility
  let check_schema_compatibility = fn(old_schema: EventSchema, new_schema: EventSchema) {
    let compatibility_mode = new_schema.compatibility_mode
    
    if compatibility_mode == "NONE" {
      { is_compatible: true, compatibility_issues: [] }
    } else if compatibility_mode == "BACKWARD" {
      // New schema must be consumable by old consumers
      let mut issues = []
      
      for old_field in old_schema.fields {
        if old_field.required {
          let new_field = new_schema.fields.find(fn(f) { f.name == old_field.name })
          match new_field {
            Some(field) => {
              // Field exists in new schema
              if field.type != old_field.type {
                issues = issues.push("Type change for required field: " + old_field.name)
              }
            }
            None => {
              // Required field removed
              issues = issues.push("Removed required field: " + old_field.name)
            }
          }
        }
      }
      
      {
        is_compatible: issues.length() == 0,
        compatibility_issues: issues
      }
    } else if compatibility_mode == "FORWARD" {
      // Old schema must be consumable by new consumers
      let mut issues = []
      
      for new_field in new_schema.fields {
        if new_field.required {
          let old_field = old_schema.fields.find(fn(f) { f.name == new_field.name })
          match old_field {
            Some(field) => {
              // Field exists in old schema
              if field.type != new_field.type {
                issues = issues.push("Type change for required field: " + new_field.name)
              }
            }
            None => {
              // New required field added
              issues = issues.push("Added required field: " + new_field.name)
            }
          }
        }
      }
      
      {
        is_compatible: issues.length() == 0,
        compatibility_issues: issues
      }
    } else {  // FULL compatibility
      // Both backward and forward compatibility
      let backward_check = check_schema_compatibility(old_schema, { new_schema | compatibility_mode: "BACKWARD" })
      let forward_check = check_schema_compatibility(old_schema, { new_schema | compatibility_mode: "FORWARD" })
      
      let all_issues = backward_check.compatibility_issues + forward_check.compatibility_issues
      
      {
        is_compatible: all_issues.length() == 0,
        compatibility_issues: all_issues
      }
    }
  }
  
  // Test schema registry creation
  let registry = create_schema_registry("FULL")
  assert_eq(registry.default_compatibility, "FULL")
  assert_eq(registry.schemas.length(), 0)
  
  // Create user event schema v1
  let user_schema_v1 = {
    name: "user.created",
    version: { major: 1, minor: 0, patch: 0 },
    fields: [
      {
        name: "id",
        type: "string",
        required: true,
        default_value: None,
        description: "Unique user identifier"
      },
      {
        name: "name",
        type: "string",
        required: true,
        default_value: None,
        description: "User's full name"
      },
      {
        name: "email",
        type: "string",
        required: false,
        default_value: None,
        description: "User's email address"
      }
    ],
    compatibility_mode: "FULL"
  }
  
  // Register schema
  let registry1 = register_schema(registry, user_schema_v1)
  assert_eq(registry1.schemas.length(), 1)
  assert_eq(registry1.schemas[0].name, "user.created")
  assert_eq(registry1.schemas[0].version.major, 1)
  
  // Test schema retrieval
  let latest_schema = get_latest_schema(registry1, "user.created")
  match latest_schema {
    Some(schema) => {
      assert_eq(schema.name, "user.created")
      assert_eq(schema.version.major, 1)
      assert_eq(schema.fields.length(), 3)
    }
    None => assert_true(false)
  }
  
  let non_existent_schema = get_latest_schema(registry1, "order.placed")
  assert_eq(non_existent_schema, None)
  
  // Test event validation
  let valid_user_event = "{\"id\": \"123\", \"name\": \"John Doe\", \"email\": \"john@example.com\"}"
  let validation_result = validate_event_against_schema(valid_user_event, user_schema_v1)
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  let invalid_user_event = "{\"id\": \"123\", \"name\": \"John Doe\"}"  // Missing required email
  let invalid_validation = validate_event_against_schema(invalid_user_event, user_schema_v1)
  assert_false(invalid_validation.is_valid)
  assert_eq(invalid_validation.errors.length(), 1)
  
  // Test schema evolution - v2
  let user_schema_v2 = {
    name: "user.created",
    version: { major: 1, minor: 1, patch: 0 },
    fields: [
      {
        name: "id",
        type: "string",
        required: true,
        default_value: None,
        description: "Unique user identifier"
      },
      {
        name: "name",
        type: "string",
        required: true,
        default_value: None,
        description: "User's full name"
      },
      {
        name: "email",
        type: "string",
        required: true,  // Made required in v2
        default_value: None,
        description: "User's email address"
      },
      {
        name: "phone",
        type: "string",
        required: false,
        default_value: None,
        description: "User's phone number"
      }
    ],
    compatibility_mode: "FULL"
  }
  
  // Register v2 schema
  let registry2 = register_schema(registry1, user_schema_v2)
  assert_eq(registry2.schemas.length(), 2)
  
  // Test compatibility check
  let compatibility_check = check_schema_compatibility(user_schema_v1, user_schema_v2)
  assert_false(compatibility_check.is_compatible)
  assert_eq(compatibility_check.compatibility_issues.length(), 1)
  assert_true(compatibility_check.compatibility_issues[0].contains("Added required field: email"))
  
  // Test backward compatible evolution
  let user_schema_v2_backward = {
    name: "user.created",
    version: { major: 1, minor: 1, patch: 0 },
    fields: [
      {
        name: "id",
        type: "string",
        required: true,
        default_value: None,
        description: "Unique user identifier"
      },
      {
        name: "name",
        type: "string",
        required: true,
        default_value: None,
        description: "User's full name"
      },
      {
        name: "email",
        type: "string",
        required: false,  // Still optional
        default_value: None,
        description: "User's email address"
      },
      {
        name: "phone",
        type: "string",
        required: false,
        default_value: None,
        description: "User's phone number"
      }
    ],
    compatibility_mode: "BACKWARD"
  }
  
  let backward_compatibility = check_schema_compatibility(user_schema_v1, user_schema_v2_backward)
  assert_true(backward_compatibility.is_compatible)
  assert_eq(backward_compatibility.compatibility_issues.length(), 0)
  
  // Test schema version comparison
  let compare_schema_versions = fn(v1: SchemaVersion, v2: SchemaVersion) {
    if v1.major < v2.major {
      "OLDER"
    } else if v1.major > v2.major {
      "NEWER"
    } else if v1.minor < v2.minor {
      "OLDER"
    } else if v1.minor > v2.minor {
      "NEWER"
    } else if v1.patch < v2.patch {
      "OLDER"
    } else if v1.patch > v2.patch {
      "NEWER"
    } else {
      "EQUAL"
    }
  }
  
  let version_comparison = compare_schema_versions(
    { major: 1, minor: 0, patch: 0 },
    { major: 1, minor: 1, patch: 0 }
  )
  assert_eq(version_comparison, "OLDER")
  
  let equal_comparison = compare_schema_versions(
    { major: 1, minor: 1, patch: 0 },
    { major: 1, minor: 1, patch: 0 }
  )
  assert_eq(equal_comparison, "EQUAL")
  
  // Test schema migration
  type SchemaMigration = {
    from_version: SchemaVersion,
    to_version: SchemaVersion,
    migration_function: (String) -> String
  }
  
  let migrate_event_data = fn(data: String, migration: SchemaMigration) {
    migration.migration_function(data)
  }
  
  // Create migration from v1 to v2
  let v1_to_v2_migration = {
    from_version: { major: 1, minor: 0, patch: 0 },
    to_version: { major: 1, minor: 1, patch: 0 },
    migration_function: fn(data: String) {
      // Simple migration: add phone field with default value
      if data.contains("\"phone\"") {
        data
      } else {
        data.replace("}", ", \"phone\": \"\"}")
      }
    }
  }
  
  let v1_event = "{\"id\": \"123\", \"name\": \"John Doe\"}"
  let migrated_event = migrate_event_data(v1_event, v1_to_v2_migration)
  assert_eq(migrated_event, "{\"id\": \"123\", \"name\": \"John Doe\", \"phone\": \"\"}")
  
  // Test multiple migrations
  let chain_migrations = fn(data: String, migrations: Array<SchemaMigration>) {
    migrations.reduce(fn(current_data, migration) {
      migrate_event_data(current_data, migration)
    }, data)
  }
  
  let v2_to_v3_migration = {
    from_version: { major: 1, minor: 1, patch: 0 },
    to_version: { major: 1, minor: 2, patch: 0 },
    migration_function: fn(data: String) {
      // Add created_at field
      if data.contains("\"created_at\"") {
        data
      } else {
        data.replace("}", ", \"created_at\": \"2022-01-01T00:00:00Z\"}")
      }
    }
  }
  
  let chain_migrated = chain_migrations(v1_event, [v1_to_v2_migration, v2_to_v3_migration])
  assert_eq(chain_migrated, "{\"id\": \"123\", \"name\": \"John Doe\", \"phone\": \"\", \"created_at\": \"2022-01-01T00:00:00Z\"}")
}

// Test 4: Event Routing and Filtering
test "event routing and filtering strategies" {
  // Define routing rule
  type RoutingRule = {
    rule_id: String,
    name: String,
    condition: String,  // Simplified condition expression
    destination_topic: String,
    priority: Int
  }
  
  // Define event router
  type EventRouter = {
    rules: Array[RoutingRule>,
    default_topic: String
  }
  
  // Define filter expression
  type FilterExpression = {
    field: String,
    operator: String,  // "equals", "contains", "starts_with", "regex"
    value: String
  }
  
  // Create event router
  let create_event_router = fn(default_topic: String) {
    {
      rules: [],
      default_topic
    }
  }
  
  // Add routing rule
  let add_routing_rule = fn(router: EventRouter, rule: RoutingRule) {
    { router | rules: router.rules.push(rule) }
  }
  
  // Evaluate filter expression against event
  let evaluate_filter = fn(filter: FilterExpression, event: {type: String, source: String, subject: String, data: String}) {
    let field_value = match filter.field {
      "type" => event.type
      "source" => event.source
      "subject" => event.subject
      "data" => event.data
      _ => ""
    }
    
    match filter.operator {
      "equals" => field_value == filter.value,
      "contains" => field_value.contains(filter.value),
      "starts_with" => field_value.starts_with(filter.value),
      "regex" => field_value.contains(filter.value),  // Simplified regex
      _ => false
    }
  }
  
  // Parse and evaluate condition (simplified)
  let evaluate_condition = fn(condition: String, event: {type: String, source: String, subject: String, data: String}) -> Bool {
    // Simplified condition parsing - just handle basic patterns
    if condition.contains("type = ") {
      let value = condition.split(" = ")[1].replace("\"", "")
      evaluate_filter({
        field: "type",
        operator: "equals",
        value
      }, event)
    } else if condition.contains("source contains ") {
      let value = condition.split(" contains ")[1].replace("\"", "")
      evaluate_filter({
        field: "source",
        operator: "contains",
        value
      }, event)
    } else if condition.contains("subject starts_with ") {
      let value = condition.split(" starts_with ")[1].replace("\"", "")
      evaluate_filter({
        field: "subject",
        operator: "starts_with",
        value
      }, event)
    } else {
      true  // Default to true for unknown conditions
    }
  }
  
  // Route event to destination topic
  let route_event = fn(router: EventRouter, event: {type: String, source: String, subject: String, data: String}) {
    // Find matching rules (sorted by priority)
    let matching_rules = router.rules
      .filter(fn(rule) { evaluate_condition(rule.condition, event) })
      .sort(fn(a, b) { if a.priority > b.priority { -1 } else if a.priority < b.priority { 1 } else { 0 } })
    
    if matching_rules.length() > 0 {
      matching_rules[0].destination_topic
    } else {
      router.default_topic
    }
  }
  
  // Test router creation
  let router = create_event_router("default-events")
  assert_eq(router.default_topic, "default-events")
  assert_eq(router.rules.length(), 0)
  
  // Add routing rules
  let user_event_rule = {
    rule_id: "user-events",
    name: "User Event Routing",
    condition: "type = \"user.created\" OR type = \"user.updated\"",
    destination_topic: "user-events",
    priority: 100
  }
  
  let payment_event_rule = {
    rule_id: "payment-events",
    name: "Payment Event Routing",
    condition: "source contains \"payment\"",
    destination_topic: "payment-events",
    priority: 90
  }
  
  let high_value_order_rule = {
    rule_id: "high-value-orders",
    name: "High Value Order Routing",
    condition: "subject starts_with \"order\" AND data contains \"\\\"amount\\\":999\"",
    destination_topic: "high-value-orders",
    priority: 200
  }
  
  let router1 = add_routing_rule(router, user_event_rule)
  let router2 = add_routing_rule(router1, payment_event_rule)
  let router3 = add_routing_rule(router2, high_value_order_rule)
  
  assert_eq(router3.rules.length(), 3)
  
  // Test event routing
  let user_event = {
    type: "user.created",
    source: "user-service",
    subject: "user-123",
    data: "{\"id\": \"123\", \"name\": \"John\"}"
  }
  
  let user_destination = route_event(router3, user_event)
  assert_eq(user_destination, "user-events")
  
  let payment_event = {
    type: "payment.processed",
    source: "payment-service",
    subject: "payment-456",
    data: "{\"id\": \"456\", \"status\": \"completed\"}"
  }
  
  let payment_destination = route_event(router3, payment_event)
  assert_eq(payment_destination, "payment-events")
  
  let high_value_order_event = {
    type: "order.placed",
    source: "order-service",
    subject: "order-789",
    data: "{\"id\": \"789\", \"amount\": 999.99}"
  }
  
  let high_value_destination = route_event(router3, high_value_order_event)
  assert_eq(high_value_destination, "high-value-orders")
  
  let default_event = {
    type: "system.started",
    source: "system-service",
    subject: "system-001",
    data: "{\"service\": \"api-gateway\"}"
  }
  
  let default_destination = route_event(router3, default_event)
  assert_eq(default_destination, "default-events")
  
  // Test content-based routing
  let content_based_router = {
    rules: [
      {
        rule_id: "error-events",
        name: "Error Event Routing",
        condition: "data contains \"\\\"error\\\"\"",
        destination_topic: "error-events",
        priority: 150
      },
      {
        rule_id: "test-events",
        name: "Test Event Routing",
        condition: "source contains \"test\"",
        destination_topic: "test-events",
        priority: 50
      }
    ],
    default_topic: "application-events"
  }
  
  let error_event = {
    type: "user.login.failed",
    source: "auth-service",
    subject: "user-123",
    data: "{\"error\": \"Invalid credentials\", \"user_id\": \"123\"}"
  }
  
  let error_destination = route_event(content_based_router, error_event)
  assert_eq(error_destination, "error-events")
  
  let test_event = {
    type: "user.created",
    source: "test-user-service",
    subject: "test-user-456",
    data: "{\"id\": \"456\", \"name\": \"Test User\"}"
  }
  
  let test_destination = route_event(content_based_router, test_event)
  assert_eq(test_destination, "test-events")
  
  // Test event filtering
  type EventFilter = {
    filter_id: String,
    name: String,
    expressions: Array<FilterExpression>,
    action: String  // "INCLUDE", "EXCLUDE"
  }
  
  let apply_event_filter = fn(filter: EventFilter, event: {type: String, source: String, subject: String, data: String}) {
    let all_expressions_match = filter.expressions.all(fn(expr) {
      evaluate_filter(expr, event)
    })
    
    match filter.action {
      "INCLUDE" => all_expressions_match,
      "EXCLUDE" => not(all_expressions_match),
      _ => true
    }
  }
  
  // Create filters
  let user_service_filter = {
    filter_id: "user-service-filter",
    name: "User Service Filter",
    expressions: [
      {
        field: "source",
        operator: "equals",
        value: "user-service"
      }
    ],
    action: "INCLUDE"
  }
  
  let error_filter = {
    filter_id: "error-filter",
    name: "Error Filter",
    expressions: [
      {
        field: "data",
        operator: "contains",
        value: "\"error\""
      }
    ],
    action: "EXCLUDE"
  }
  
  // Test filtering
  let user_service_event = {
    type: "user.updated",
    source: "user-service",
    subject: "user-123",
    data: "{\"id\": \"123\", \"name\": \"John Doe\"}"
  }
  
  let user_service_result = apply_event_filter(user_service_filter, user_service_event)
  assert_true(user_service_result)
  
  let non_user_service_event = {
    type: "order.placed",
    source: "order-service",
    subject: "order-456",
    data: "{\"id\": \"456\", \"total\": 99.99}"
  }
  
  let non_user_service_result = apply_event_filter(user_service_filter, non_user_service_event)
  assert_false(non_user_service_result)
  
  let error_data_event = {
    type: "user.login.failed",
    source: "auth-service",
    subject: "user-789",
    data: "{\"error\": \"Account locked\", \"user_id\": \"789\"}"
  }
  
  let error_result = apply_event_filter(error_filter, error_data_event)
  assert_false(error_result)  // Excluded due to error
  
  let non_error_data_event = {
    type: "user.login.success",
    source: "auth-service",
    subject: "user-789",
    data: "{\"success\": true, \"user_id\": \"789\"}"
  }
  
  let non_error_result = apply_event_filter(error_filter, non_error_data_event)
  assert_true(non_error_result)  // Not excluded
  
  // Test complex routing with multiple conditions
  type ComplexRoutingRule = {
    rule_id: String,
    name: String,
    conditions: Array<FilterExpression>,
    logical_operator: String,  // "AND", "OR"
    destination_topic: String,
    priority: Int
  }
  
  let evaluate_complex_condition = fn(conditions: Array<FilterExpression>, operator: String, 
                                     event: {type: String, source: String, subject: String, data: String}) {
    let results = conditions.map(fn(condition) {
      evaluate_filter(condition, event)
    })
    
    match operator {
      "AND" => results.all(fn(result) { result }),
      "OR" => results.any(fn(result) { result }),
      _ => false
    }
  }
  
  let complex_rule = {
    rule_id: "critical-user-events",
    name: "Critical User Events",
    conditions: [
      {
        field: "type",
        operator: "contains",
        value: "user"
      },
      {
        field: "data",
        operator: "contains",
        value: "\"priority\":\"high\""
      }
    ],
    logical_operator: "AND",
    destination_topic: "critical-user-events",
    priority: 300
  }
  
  let critical_user_event = {
    type: "user.deleted",
    source: "user-service",
    subject: "user-999",
    data: "{\"id\": \"999\", \"priority\": \"high\", \"reason\": \"GDPR request\"}"
  }
  
  let complex_condition_result = evaluate_complex_condition(
    complex_rule.conditions,
    complex_rule.logical_operator,
    critical_user_event
  )
  assert_true(complex_condition_result)
  
  let non_critical_user_event = {
    type: "user.created",
    source: "user-service",
    subject: "user-888",
    data: "{\"id\": \"888\", \"priority\": \"low\", \"name\": \"New User\"}"
  }
  
  let non_critical_result = evaluate_complex_condition(
    complex_rule.conditions,
    complex_rule.logical_operator,
    non_critical_user_event
  )
  assert_false(non_critical_result)
}

// Test 5: Event Stream Processing Patterns
test "event stream processing patterns and topologies" {
  // Define processing node
  type ProcessingNode = {
    node_id: String,
    node_type: String,  // "SOURCE", "PROCESSOR", "SINK"
    processing_function: (Array<Event>) -> Array[Event],
    config: Array[(String, String)]
  }
  
  // Define stream topology
  type StreamTopology = {
    topology_id: String,
    nodes: Array[ProcessingNode>,
    edges: Array[(String, String)]  // (source_node_id, target_node_id)
  }
  
  // Define stream processor
  type StreamProcessor = {
    topology: StreamTopology,
    state: Array[(String, Array<Event])]  // Node state (node_id -> events)
  }
  
  // Create stream topology
  let create_topology = fn(topology_id: String) {
    {
      topology_id,
      nodes: [],
      edges: []
    }
  }
  
  // Add node to topology
  let add_node = fn(topology: StreamTopology, node: ProcessingNode) {
    { topology | nodes: topology.nodes.push(node) }
  }
  
  // Add edge to topology
  let add_edge = fn(topology: StreamTopology, source_id: String, target_id: String) {
    { topology | edges: topology.edges.push((source_id, target_id)) }
  }
  
  // Create stream processor
  let create_stream_processor = fn(topology: StreamTopology) {
    let initial_state = topology.nodes.map(fn(node) {
      (node.node_id, [])
    })
    
    {
      topology,
      state: initial_state
    }
  }
  
  // Process events through topology
  let process_events = fn(processor: StreamProcessor, input_events: Array[Event], source_node_id: String) {
    let mut updated_processor = processor
    
    // Set input events for source node
    let mut new_state = updated_processor.state.map(fn(node_state) {
      if node_state.0 == source_node_id {
        (node_state.0, input_events)
      } else {
        node_state
      }
    })
    
    // Process nodes in topological order (simplified - just process in order)
    for node in updated_processor.topology.nodes {
      if node.node_type != "SOURCE" {
        // Get input events for this node
        let input_events_for_node = []
        for edge in updated_processor.topology.edges {
          if edge.1 == node.node_id {
            let source_state = new_state.find(fn(s) { s.0 == edge.0 })
            match source_state {
              Some((_, events)) => {
                input_events_for_node = input_events_for_node + events
              }
              None => {}
            }
          }
        }
        
        // Process events
        let output_events = node.processing_function(input_events_for_node)
        
        // Update node state
        new_state = new_state.map(fn(node_state) {
          if node_state.0 == node.node_id {
            (node_state.0, output_events)
          } else {
            node_state
          }
        })
      }
    }
    
    { updated_processor | state: new_state }
  }
  
  // Create processing functions
  let filter_events_by_type = fn(event_types: Array[String]) {
    fn(events: Array[Event]) {
      events.filter(fn(event) { event_types.contains(event.type) })
    }
  }
  
  let transform_events = fn(transform_fn: (Event) -> Event) {
    fn(events: Array[Event]) {
      events.map(transform_fn)
    }
  }
  
  let aggregate_events = fn(key_extractor: (Event) -> String, aggregation_fn: (Array<Event>) -> Event) {
    fn(events: Array[Event]) {
      let mut grouped = []
      let mut processed_keys = []
      
      for event in events {
        let key = key_extractor(event)
        
        if not(processed_keys.contains(key)) {
          processed_keys = processed_keys.push(key)
          let key_events = events.filter(fn(e) { key_extractor(e) == key })
          let aggregated = aggregation_fn(key_events)
          grouped = grouped.push(aggregated)
        }
      }
      
      grouped
    }
  }
  
  // Test topology creation
  let topology = create_topology("user-analytics-pipeline")
  assert_eq(topology.topology_id, "user-analytics-pipeline")
  assert_eq(topology.nodes.length(), 0)
  assert_eq(topology.edges.length(), 0)
  
  // Create source node
  let source_node = {
    node_id: "user-events-source",
    node_type: "SOURCE",
    processing_function: fn(events: Array[Event]) { events },  // Pass-through
    config: [("topic", "user-events")]
  }
  
  // Create filter node
  let filter_node = {
    node_id: "user-filter",
    node_type: "PROCESSOR",
    processing_function: filter_events_by_type(["user.created", "user.updated"]),
    config: [("event_types", "user.created,user.updated")]
  }
  
  // Create enrichment node
  let enrichment_node = {
    node_id: "user-enrichment",
    node_type: "PROCESSOR",
    processing_function: transform_events(fn(event) {
      { event | 
        attributes: event.attributes + [("processed_at", "1640995300")]
      }
    }),
    config: [("add_timestamp", "true")]
  }
  
  // Create aggregation node
  let aggregation_node = {
    node_id: "user-aggregation",
    node_type: "PROCESSOR",
    processing_function: aggregate_events(
      fn(e) { e.source },
      fn(events) {
        let count = events.length()
        let first_event = events[0]
        {
          id: "agg-" + first_event.source + "-" + first_event.timestamp.to_string(),
          type: "user.activity.aggregated",
          source: "aggregator",
          subject: first_event.source,
          data_content_type: "application/json",
          data: "{\"count\": " + count.to_string() + ", \"source\": \"" + first_event.source + "\"}",
          timestamp: first_event.timestamp,
          attributes: []
        }
      }
    ),
    config: [("aggregation_window", "60s")]
  }
  
  // Create sink node
  let sink_node = {
    node_id: "analytics-sink",
    node_type: "SINK",
    processing_function: fn(events: Array[Event]) { events },  // Pass-through
    config: [("topic", "user-analytics")]
  }
  
  // Add nodes to topology
  let topology1 = add_node(topology, source_node)
  let topology2 = add_node(topology1, filter_node)
  let topology3 = add_node(topology2, enrichment_node)
  let topology4 = add_node(topology3, aggregation_node)
  let topology5 = add_node(topology4, sink_node)
  
  assert_eq(topology5.nodes.length(), 5)
  
  // Add edges to create pipeline
  let topology6 = add_edge(topology5, "user-events-source", "user-filter")
  let topology7 = add_edge(topology6, "user-filter", "user-enrichment")
  let topology8 = add_edge(topology7, "user-enrichment", "user-aggregation")
  let topology9 = add_edge(topology8, "user-aggregation", "analytics-sink")
  
  assert_eq(topology9.edges.length(), 4)
  
  // Create stream processor
  let processor = create_stream_processor(topology9)
  assert_eq(processor.topology.nodes.length(), 5)
  assert_eq(processor.state.length(), 5)
  
  // Create test events
  let test_events = [
    {
      id: "event-001",
      type: "user.created",
      source: "user-service",
      subject: "user-123",
      data_content_type: "application/json",
      data: "{\"id\": \"123\", \"name\": \"Alice\"}",
      timestamp: 1640995200,
      attributes: []
    },
    {
      id: "event-002",
      type: "order.placed",
      source: "order-service",
      subject: "order-456",
      data_content_type: "application/json",
      data: "{\"id\": \"456\", \"total\": 99.99}",
      timestamp: 1640995205,
      attributes: []
    },
    {
      id: "event-003",
      type: "user.updated",
      source: "user-service",
      subject: "user-123",
      data_content_type: "application/json",
      data: "{\"id\": \"123\", \"name\": \"Alice Smith\"}",
      timestamp: 1640995210,
      attributes: []
    },
    {
      id: "event-004",
      type: "user.created",
      source: "auth-service",
      subject: "user-789",
      data_content_type: "application/json",
      data: "{\"id\": \"789\", \"name\": \"Bob\"}",
      timestamp: 1640995215,
      attributes: []
    }
  ]
  
  // Process events through topology
  let processed_processor = process_events(processor, test_events, "user-events-source")
  
  // Check final output at sink node
  let sink_state = processed_processor.state.find(fn(s) { s.0 == "analytics-sink" })
  match sink_state {
    Some((_, output_events)) => {
      assert_eq(output_events.length(), 2)  // Two aggregated events (one per source)
      
      // Check user-service aggregation
      let user_service_agg = output_events.find(fn(e) { e.data.contains("user-service") })
      match user_service_agg {
        Some(agg) => {
          assert_eq(agg.type, "user.activity.aggregated")
          assert_eq(agg.source, "aggregator")
          assert_eq(agg.subject, "user-service")
          assert_true(agg.data.contains("\"count\": 2"))
          assert_true(agg.data.contains("\"source\": \"user-service\""))
        }
        None => assert_true(false)
      }
      
      // Check auth-service aggregation
      let auth_service_agg = output_events.find(fn(e) { e.data.contains("auth-service") })
      match auth_service_agg {
        Some(agg) => {
          assert_eq(agg.type, "user.activity.aggregated")
          assert_eq(agg.source, "aggregator")
          assert_eq(agg.subject, "auth-service")
          assert_true(agg.data.contains("\"count\": 1"))
          assert_true(agg.data.contains("\"source\": \"auth-service\""))
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test windowed aggregation
  type TimeWindow = {
    window_size_ms: Int,
    slide_interval_ms: Int
  }
  
  let windowed_aggregation = fn(events: Array[Event], window: TimeWindow) {
    if events.length() == 0 {
      []
    } else {
      let mut windows = []
      let first_timestamp = events[0].timestamp
      let last_timestamp = events[events.length() - 1].timestamp
      
      let mut window_start = first_timestamp
      while window_start <= last_timestamp {
        let window_end = window_start + window.window_size_ms
        
        let window_events = events.filter(fn(event) {
          event.timestamp >= window_start && event.timestamp < window_end
        })
        
        if window_events.length() > 0 {
          windows = windows.push({
            window_start,
            window_end,
            events: window_events,
            count: window_events.length()
          })
        }
        
        window_start = window_start + window.slide_interval_ms
      }
      
      windows
    }
  }
  
  // Test windowed aggregation
  let window = {
    window_size_ms: 10000,  // 10 seconds
    slide_interval_ms: 5000  // 5 seconds
  }
  
  let windowed_events = [
    { test_events[0] | timestamp: 1640995200 },  // 0s
    { test_events[1] | timestamp: 1640995203 },  // 3s
    { test_events[2] | timestamp: 1640995207 },  // 7s
    { test_events[3] | timestamp: 16409952012 }  // 12s
  ]
  
  let windows = windowed_aggregation(windowed_events, window)
  assert_eq(windows.length(), 3)
  
  // First window: 0-10s
  assert_eq(windows[0].window_start, 1640995200)
  assert_eq(windows[0].window_end, 1640995210)
  assert_eq(windows[0].count, 3)
  
  // Second window: 5-15s
  assert_eq(windows[1].window_start, 1640995205)
  assert_eq(windows[1].window_end, 1640995215)
  assert_eq(windows[1].count, 1)
  
  // Third window: 10-20s
  assert_eq(windows[2].window_start, 1640995210)
  assert_eq(windows[2].window_end, 1640995220)
  assert_eq(windows[2].count, 1)
  
  // Test event joining
  let join_events = fn(left_events: Array[Event], right_events: Array[Event], 
                      join_key: (Event) -> String, join_type: String) {
    let mut joined_events = []
    
    for left_event in left_events {
      let left_key = join_key(left_event)
      let matching_right = right_events.filter(fn(right_event) {
        join_key(right_event) == left_key
      })
      
      match join_type {
        "INNER" => {
          for right_event in matching_right {
            joined_events = joined_events.push({
              id: left_event.id + "-joined-" + right_event.id,
              type: "joined." + left_event.type + "." + right_event.type,
              source: "join-processor",
              subject: left_event.subject,
              data_content_type: "application/json",
              data: "{\"left\": " + left_event.data + ", \"right\": " + right_event.data + "}",
              timestamp: left_event.timestamp,
              attributes: []
            })
          }
        }
        "LEFT" => {
          if matching_right.length() > 0 {
            for right_event in matching_right {
              joined_events = joined_events.push({
                id: left_event.id + "-joined-" + right_event.id,
                type: "joined." + left_event.type + "." + right_event.type,
                source: "join-processor",
                subject: left_event.subject,
                data_content_type: "application/json",
                data: "{\"left\": " + left_event.data + ", \"right\": " + right_event.data + "}",
                timestamp: left_event.timestamp,
                attributes: []
              })
            }
          } else {
            joined_events = joined_events.push({
              id: left_event.id + "-joined-null",
              type: "joined." + left_event.type + ".null",
              source: "join-processor",
              subject: left_event.subject,
              data_content_type: "application/json",
              data: "{\"left\": " + left_event.data + ", \"right\": null}",
              timestamp: left_event.timestamp,
              attributes: []
            })
          }
        }
        _ => {}
      }
    }
    
    joined_events
  }
  
  // Test event joining
  let user_events = [
    {
      id: "user-001",
      type: "user.created",
      source: "user-service",
      subject: "user-123",
      data_content_type: "application/json",
      data: "{\"id\": \"123\", \"name\": \"Alice\"}",
      timestamp: 1640995200,
      attributes: []
    },
    {
      id: "user-002",
      type: "user.created",
      source: "user-service",
      subject: "user-456",
      data_content_type: "application/json",
      data: "{\"id\": \"456\", \"name\": \"Bob\"}",
      timestamp: 1640995205,
      attributes: []
    }
  ]
  
  let order_events = [
    {
      id: "order-001",
      type: "order.placed",
      source: "order-service",
      subject: "user-123",
      data_content_type: "application/json",
      data: "{\"id\": \"789\", \"user_id\": \"123\", \"total\": 99.99}",
      timestamp: 1640995210,
      attributes: []
    }
  ]
  
  let inner_join_result = join_events(user_events, order_events, fn(e) { e.subject }, "INNER")
  assert_eq(inner_join_result.length(), 1)  // Only user-123 has matching order
  assert_eq(inner_join_result[0].type, "joined.user.created.order.placed")
  assert_eq(inner_join_result[0].subject, "user-123")
  
  let left_join_result = join_events(user_events, order_events, fn(e) { e.subject }, "LEFT")
  assert_eq(left_join_result.length(), 2)  // Both users included
  assert_eq(left_join_result[0].type, "joined.user.created.order.placed")   // user-123 with order
  assert_eq(left_join_result[1].type, "joined.user.created.null")           // user-456 without order
}