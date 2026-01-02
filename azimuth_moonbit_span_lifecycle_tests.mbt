// Azimuth Telemetry System - MoonBit Span Lifecycle Tests
// This file contains test cases for span lifecycle management

// Define additional types needed for span lifecycle testing
pub enum SpanKind {
  Server
  Client
  Producer
  Consumer
  Internal
}

pub enum SpanStatus {
  Ok
  Error
  Unset
}

pub struct Span {
  name : String
  context : SpanContext
  kind : SpanKind
  parent_span_id : Option[String]
  start_time : Int
  end_time : Option[Int]
  status : SpanStatus
  attributes : Attributes
  events : Array[SpanEvent]
  links : Array[SpanLink]
}

pub struct SpanEvent {
  name : String
  timestamp : Int
  attributes : Attributes
}

pub struct SpanLink {
  context : SpanContext
  attributes : Attributes
}

// Test 1: Span creation and initialization
test "span creation and initialization" {
  let span_context = SpanContext({
    trace_id = "trace-123456789",
    span_id = "span-111111111",
    sampled = true,
    trace_state = "key1=value1,key2=value2"
  })
  
  let span = Span({
    name = "test-span",
    context = span_context,
    kind = Server,
    parent_span_id = None,
    start_time = 1640995200000,  // 2022-01-01 00:00:00 UTC in milliseconds
    end_time = None,
    status = Unset,
    attributes = Attributes({ values = [] }),
    events = [],
    links = []
  })
  
  assert_eq(span.name, "test-span")
  assert_eq(span.context.trace_id, "trace-123456789")
  assert_eq(span.context.span_id, "span-111111111")
  assert_true(span.context.sampled)
  match span.kind {
    Server => assert_true(true)
    _ => assert_true(false)
  }
  match span.parent_span_id {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  assert_eq(span.start_time, 1640995200000)
  match span.end_time {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  match span.status {
    Unset => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(span.attributes.values.length(), 0)
  assert_eq(span.events.length(), 0)
  assert_eq(span.links.length(), 0)
}

// Test 2: Span with parent relationship
test "span with parent relationship" {
  let parent_context = SpanContext({
    trace_id = "trace-123456789",
    span_id = "span-parent-111",
    sampled = true,
    trace_state = "key1=value1,key2=value2"
  })
  
  let child_context = SpanContext({
    trace_id = "trace-123456789",  // Same trace ID
    span_id = "span-child-222",     // Different span ID
    sampled = true,
    trace_state = "key1=value1,key2=value2"
  })
  
  let parent_span = Span({
    name = "parent-span",
    context = parent_context,
    kind = Server,
    parent_span_id = None,
    start_time = 1640995200000,
    end_time = None,
    status = Unset,
    attributes = Attributes({ values = [] }),
    events = [],
    links = []
  })
  
  let child_span = Span({
    name = "child-span",
    context = child_context,
    kind = Internal,
    parent_span_id = Some("span-parent-111"),  // Reference to parent
    start_time = 1640995200100,
    end_time = None,
    status = Unset,
    attributes = Attributes({ values = [] }),
    events = [],
    links = []
  })
  
  // Verify parent-child relationship
  assert_eq(parent_span.context.trace_id, child_span.context.trace_id)
  assert_true(parent_span.context.span_id != child_span.context.span_id)
  
  match child_span.parent_span_id {
    None => assert_true(false)
    Some(parent_id) => {
      assert_eq(parent_id, parent_span.context.span_id)
      assert_eq(parent_id, "span-parent-111")
    }
  }
  
  // Verify timing (child starts after parent)
  assert_true(child_span.start_time >= parent_span.start_time)
}

// Test 3: Span status transitions
test "span status transitions" {
  let span_context = SpanContext({
    trace_id = "trace-123456789",
    span_id = "span-111111111",
    sampled = true,
    trace_state = "key1=value1,key2=value2"
  })
  
  let span = Span({
    name = "test-span",
    context = span_context,
    kind = Server,
    parent_span_id = None,
    start_time = 1640995200000,
    end_time = None,
    status = Unset,
    attributes = Attributes({ values = [] }),
    events = [],
    links = []
  })
  
  // Initial status should be Unset
  match span.status {
    Unset => assert_true(true)
    _ => assert_true(false)
  }
  
  // Update to Error status
  let error_span = { span with status = Error }
  match error_span.status {
    Error => assert_true(true)
    _ => assert_true(false)
  }
  
  // Update to Ok status
  let ok_span = { error_span with status = Ok }
  match ok_span.status {
    Ok => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 4: Span completion and timing
test "span completion and timing" {
  let span_context = SpanContext({
    trace_id = "trace-123456789",
    span_id = "span-111111111",
    sampled = true,
    trace_state = "key1=value1,key2=value2"
  })
  
  let active_span = Span({
    name = "test-span",
    context = span_context,
    kind = Server,
    parent_span_id = None,
    start_time = 1640995200000,
    end_time = None,  // Active span
    status = Unset,
    attributes = Attributes({ values = [] }),
    events = [],
    links = []
  })
  
  // Verify active span has no end time
  match active_span.end_time {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Complete the span
  let completed_span = { active_span with 
    end_time = Some(1640995200500),
    status = Ok
  }
  
  // Verify completed span has end time
  match completed_span.end_time {
    None => assert_true(false)
    Some(end_time) => {
      assert_eq(end_time, 1640995200500)
      assert_true(end_time >= completed_span.start_time)
    }
  }
  
  // Verify status is Ok
  match completed_span.status {
    Ok => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 5: Span attributes management
test "span attributes management" {
  let span_context = SpanContext({
    trace_id = "trace-123456789",
    span_id = "span-111111111",
    sampled = true,
    trace_state = "key1=value1,key2=value2"
  })
  
  let span = Span({
    name = "test-span",
    context = span_context,
    kind = Server,
    parent_span_id = None,
    start_time = 1640995200000,
    end_time = None,
    status = Unset,
    attributes = Attributes({ values = [
      ("http.method", StringValue("GET")),
      ("http.url", StringValue("https://example.com/api")),
      ("http.status_code", IntValue(200))
    ] }),
    events = [],
    links = []
  })
  
  // Verify attributes
  assert_eq(span.attributes.values.length(), 3)
  
  // Verify specific attributes
  let mut found_method = false
  let mut found_url = false
  let mut found_status_code = false
  
  for (key, value) in span.attributes.values {
    match key {
      "http.method" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "GET")
            found_method = true
          }
          _ => assert_true(false)
        }
      }
      "http.url" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "https://example.com/api")
            found_url = true
          }
          _ => assert_true(false)
        }
      }
      "http.status_code" => {
        match value {
          IntValue(v) => {
            assert_eq(v, 200)
            found_status_code = true
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)  // Unexpected key
    }
  }
  
  assert_true(found_method)
  assert_true(found_url)
  assert_true(found_status_code)
}

// Test 6: Span events management
test "span events management" {
  let span_context = SpanContext({
    trace_id = "trace-123456789",
    span_id = "span-111111111",
    sampled = true,
    trace_state = "key1=value1,key2=value2"
  })
  
  let event1 = SpanEvent({
    name = "db.query.start",
    timestamp = 1640995200100,
    attributes = Attributes({ values = [
      ("db.statement", StringValue("SELECT * FROM users"))
    ] })
  })
  
  let event2 = SpanEvent({
    name = "db.query.end",
    timestamp = 1640995200200,
    attributes = Attributes({ values = [
      ("db.rows", IntValue(10))
    ] })
  })
  
  let span = Span({
    name = "test-span",
    context = span_context,
    kind = Server,
    parent_span_id = None,
    start_time = 1640995200000,
    end_time = None,
    status = Unset,
    attributes = Attributes({ values = [] }),
    events = [event1, event2],
    links = []
  })
  
  // Verify events
  assert_eq(span.events.length(), 2)
  
  // Verify first event
  let db_start_event = span.events[0]
  assert_eq(db_start_event.name, "db.query.start")
  assert_eq(db_start_event.timestamp, 1640995200100)
  assert_eq(db_start_event.attributes.values.length(), 1)
  
  let (key, value) = db_start_event.attributes.values[0]
  assert_eq(key, "db.statement")
  match value {
    StringValue(v) => assert_eq(v, "SELECT * FROM users")
    _ => assert_true(false)
  }
  
  // Verify second event
  let db_end_event = span.events[1]
  assert_eq(db_end_event.name, "db.query.end")
  assert_eq(db_end_event.timestamp, 1640995200200)
  assert_eq(db_end_event.attributes.values.length(), 1)
  
  let (key2, value2) = db_end_event.attributes.values[0]
  assert_eq(key2, "db.rows")
  match value2 {
    IntValue(v) => assert_eq(v, 10)
    _ => assert_true(false)
  }
  
  // Verify event timing
  assert_true(db_start_event.timestamp >= span.start_time)
  assert_true(db_end_event.timestamp >= db_start_event.timestamp)
}

// Test 7: Span links management
test "span links management" {
  let linked_context1 = SpanContext({
    trace_id = "trace-111111111",
    span_id = "span-111111111",
    sampled = true,
    trace_state = "key1=value1"
  })
  
  let linked_context2 = SpanContext({
    trace_id = "trace-222222222",
    span_id = "span-222222222",
    sampled = true,
    trace_state = "key2=value2"
  })
  
  let link1 = SpanLink({
    context = linked_context1,
    attributes = Attributes({ values = [
      ("link.type", StringValue("caused_by"))
    ] })
  })
  
  let link2 = SpanLink({
    context = linked_context2,
    attributes = Attributes({ values = [
      ("link.type", StringValue("related_to"))
    ] })
  })
  
  let span_context = SpanContext({
    trace_id = "trace-123456789",
    span_id = "span-333333333",
    sampled = true,
    trace_state = "key3=value3"
  })
  
  let span = Span({
    name = "test-span",
    context = span_context,
    kind = Server,
    parent_span_id = None,
    start_time = 1640995200000,
    end_time = None,
    status = Unset,
    attributes = Attributes({ values = [] }),
    events = [],
    links = [link1, link2]
  })
  
  // Verify links
  assert_eq(span.links.length(), 2)
  
  // Verify first link
  let first_link = span.links[0]
  assert_eq(first_link.context.trace_id, "trace-111111111")
  assert_eq(first_link.context.span_id, "span-111111111")
  assert_eq(first_link.attributes.values.length(), 1)
  
  let (key, value) = first_link.attributes.values[0]
  assert_eq(key, "link.type")
  match value {
    StringValue(v) => assert_eq(v, "caused_by")
    _ => assert_true(false)
  }
  
  // Verify second link
  let second_link = span.links[1]
  assert_eq(second_link.context.trace_id, "trace-222222222")
  assert_eq(second_link.context.span_id, "span-222222222")
  assert_eq(second_link.attributes.values.length(), 1)
  
  let (key2, value2) = second_link.attributes.values[0]
  assert_eq(key2, "link.type")
  match value2 {
    StringValue(v) => assert_eq(v, "related_to")
    _ => assert_true(false)
  }
  
  // Verify span has different trace ID from linked spans
  assert_true(span.context.trace_id != first_link.context.trace_id)
  assert_true(span.context.trace_id != second_link.context.trace_id)
  assert_true(first_link.context.trace_id != second_link.context.trace_id)
}

// Test 8: Span kind variations
test "span kind variations" {
  let span_context = SpanContext({
    trace_id = "trace-123456789",
    span_id = "span-111111111",
    sampled = true,
    trace_state = "key1=value1,key2=value2"
  })
  
  // Test Server span
  let server_span = Span({
    name = "server-span",
    context = span_context,
    kind = Server,
    parent_span_id = None,
    start_time = 1640995200000,
    end_time = None,
    status = Unset,
    attributes = Attributes({ values = [] }),
    events = [],
    links = []
  })
  
  match server_span.kind {
    Server => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test Client span
  let client_span = { server_span with kind = Client }
  match client_span.kind {
    Client => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test Producer span
  let producer_span = { server_span with kind = Producer }
  match producer_span.kind {
    Producer => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test Consumer span
  let consumer_span = { server_span with kind = Consumer }
  match consumer_span.kind {
    Consumer => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test Internal span
  let internal_span = { server_span with kind = Internal }
  match internal_span.kind {
    Internal => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 9: Span duration calculation
test "span duration calculation" {
  let span_context = SpanContext({
    trace_id = "trace-123456789",
    span_id = "span-111111111",
    sampled = true,
    trace_state = "key1=value1,key2=value2"
  })
  
  let active_span = Span({
    name = "test-span",
    context = span_context,
    kind = Server,
    parent_span_id = None,
    start_time = 1640995200000,
    end_time = None,  // Active span
    status = Unset,
    attributes = Attributes({ values = [] }),
    events = [],
    links = []
  })
  
  // Active span should have no duration
  match active_span.end_time {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Complete the span
  let completed_span = { active_span with 
    end_time = Some(1640995200500)
  }
  
  // Calculate duration
  match completed_span.end_time {
    None => assert_true(false)
    Some(end_time) => {
      let duration = end_time - completed_span.start_time
      assert_eq(duration, 500)  // 500ms
      assert_true(duration >= 0)
    }
  }
}

// Test 10: Span hierarchy and relationships
test "span hierarchy and relationships" {
  // Create root span
  let root_context = SpanContext({
    trace_id = "trace-123456789",
    span_id = "span-root-111",
    sampled = true,
    trace_state = "key1=value1,key2=value2"
  })
  
  let root_span = Span({
    name = "root-span",
    context = root_context,
    kind = Server,
    parent_span_id = None,
    start_time = 1640995200000,
    end_time = Some(1640995200500),
    status = Ok,
    attributes = Attributes({ values = [] }),
    events = [],
    links = []
  })
  
  // Create child span
  let child_context = SpanContext({
    trace_id = "trace-123456789",  // Same trace
    span_id = "span-child-222",     // Different span
    sampled = true,
    trace_state = "key1=value1,key2=value2"
  })
  
  let child_span = Span({
    name = "child-span",
    context = child_context,
    kind = Internal,
    parent_span_id = Some("span-root-111"),  // Parent reference
    start_time = 1640995200100,  // Starts after parent
    end_time = Some(1640995200400),
    status = Ok,
    attributes = Attributes({ values = [] }),
    events = [],
    links = []
  })
  
  // Create grandchild span
  let grandchild_context = SpanContext({
    trace_id = "trace-123456789",  // Same trace
    span_id = "span-grandchild-333",  // Different span
    sampled = true,
    trace_state = "key1=value1,key2=value2"
  })
  
  let grandchild_span = Span({
    name = "grandchild-span",
    context = grandchild_context,
    kind = Client,
    parent_span_id = Some("span-child-222"),  // Parent reference
    start_time = 1640995200200,  // Starts after parent
    end_time = Some(1640995200300),
    status = Ok,
    attributes = Attributes({ values = [] }),
    events = [],
    links = []
  })
  
  // Verify hierarchy
  // Root span has no parent
  match root_span.parent_span_id {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Child span has root as parent
  match child_span.parent_span_id {
    None => assert_true(false)
    Some(parent_id) => assert_eq(parent_id, root_span.context.span_id)
  }
  
  // Grandchild span has child as parent
  match grandchild_span.parent_span_id {
    None => assert_true(false)
    Some(parent_id) => assert_eq(parent_id, child_span.context.span_id)
  }
  
  // All spans have same trace ID
  assert_eq(root_span.context.trace_id, child_span.context.trace_id)
  assert_eq(child_span.context.trace_id, grandchild_span.context.trace_id)
  
  // All spans have unique span IDs
  assert_true(root_span.context.span_id != child_span.context.span_id)
  assert_true(child_span.context.span_id != grandchild_span.context.span_id)
  assert_true(root_span.context.span_id != grandchild_span.context.span_id)
  
  // Verify timing (parent starts before child)
  assert_true(child_span.start_time >= root_span.start_time)
  assert_true(grandchild_span.start_time >= child_span.start_time)
  
  // Verify child completes within parent's lifetime
  match root_span.end_time {
    None => assert_true(false)
    Some(root_end) => {
      match child_span.end_time {
        None => assert_true(false)
        Some(child_end) => {
          assert_true(child_end >= child_span.start_time)
          assert_true(child_end <= root_end)
        }
      }
    }
  }
}