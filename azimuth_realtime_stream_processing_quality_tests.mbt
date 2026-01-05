// Azimuth Real-time Stream Processing Test Suite
// This file contains high-quality test cases for real-time stream processing

// Test 1: Stream Buffer Management
test "stream buffer management" {
  // Define stream event
  type StreamEvent = {
    id: String,
    timestamp: Int,
    data: String,
    priority: Int
  }
  
  // Define stream buffer
  type StreamBuffer = {
    events: Array[StreamEvent],
    max_size: Int,
    drop_policy: String  // "oldest", "lowest_priority", "newest"
  }
  
  // Create stream buffer
  let create_stream_buffer = fn(max_size: Int, drop_policy: String) {
    {
      events: [],
      max_size: max_size,
      drop_policy: drop_policy
    }
  }
  
  // Add event to buffer
  let add_event = fn(buffer: StreamBuffer, event: StreamEvent) {
    if buffer.events.length() < buffer.max_size {
      // Buffer not full, just add
      {
        events: buffer.events.push(event),
        max_size: buffer.max_size,
        drop_policy: buffer.drop_policy
      }
    } else {
      // Buffer full, apply drop policy
      match buffer.drop_policy {
        "oldest" => {
          // Drop oldest event
          let updated_events = buffer.events.slice(1).push(event)
          {
            events: updated_events,
            max_size: buffer.max_size,
            drop_policy: buffer.drop_policy
          }
        }
        "lowest_priority" => {
          // Find lowest priority event
          let mut lowest_priority = buffer.events[0].priority
          let mut lowest_index = 0
          
          for i in 1..buffer.events.length() {
            if buffer.events[i].priority < lowest_priority {
              lowest_priority = buffer.events[i].priority
              lowest_index = i
            }
          }
          
          // Replace lowest priority event if new event has higher or equal priority
          if event.priority >= lowest_priority {
            let updated_events = buffer.events.update(lowest_index, event)
            {
              events: updated_events,
              max_size: buffer.max_size,
              drop_policy: buffer.drop_policy
            }
          } else {
            // New event has lower priority, drop it
            buffer
          }
        }
        "newest" => {
          // Drop newest event (don't add the new one)
          buffer
        }
        _ => buffer  // Unknown policy
      }
    }
  }
  
  // Get events from buffer
  let get_events = fn(buffer: StreamBuffer, count: Int) {
    if count >= buffer.events.length() {
      {
        events: [],
        max_size: buffer.max_size,
        drop_policy: buffer.drop_policy
      }
    } else {
      let taken_events = buffer.events.slice(0, count)
      let remaining_events = buffer.events.slice(count)
      
      {
        events: remaining_events,
        max_size: buffer.max_size,
        drop_policy: buffer.drop_policy
      }
    }
  }
  
  // Test stream buffer management
  let buffer = create_stream_buffer(3, "oldest")
  
  assert_eq(buffer.events.length(), 0)
  
  // Add events
  let event1 = { id: "event-1", timestamp: 1000, data: "data-1", priority: 1 }
  let event2 = { id: "event-2", timestamp: 1005, data: "data-2", priority: 2 }
  let event3 = { id: "event-3", timestamp: 1010, data: "data-3", priority: 3 }
  
  let buffer1 = add_event(buffer, event1)
  let buffer2 = add_event(buffer1, event2)
  let buffer3 = add_event(buffer2, event3)
  
  assert_eq(buffer3.events.length(), 3)
  assert_eq(buffer3.events[0].id, "event-1")
  assert_eq(buffer3.events[1].id, "event-2")
  assert_eq(buffer3.events[2].id, "event-3")
  
  // Add event when buffer is full (oldest policy)
  let event4 = { id: "event-4", timestamp: 1015, data: "data-4", priority: 4 }
  let buffer4 = add_event(buffer3, event4)
  
  assert_eq(buffer4.events.length(), 3)
  assert_eq(buffer4.events[0].id, "event-2")  // event-1 dropped
  assert_eq(buffer4.events[1].id, "event-3")
  assert_eq(buffer4.events[2].id, "event-4")
  
  // Test lowest priority policy
  let priority_buffer = create_stream_buffer(3, "lowest_priority")
  
  let priority_buffer1 = add_event(priority_buffer, event3)  // priority 3
  let priority_buffer2 = add_event(priority_buffer1, event1)  // priority 1
  let priority_buffer3 = add_event(priority_buffer2, event2)  // priority 2
  
  assert_eq(priority_buffer3.events.length(), 3)
  
  // Add high priority event
  let event5 = { id: "event-5", timestamp: 1020, data: "data-5", priority: 5 }
  let priority_buffer4 = add_event(priority_buffer3, event5)
  
  assert_eq(priority_buffer4.events.length(), 3)
  // event-1 (priority 1) should be replaced by event-5 (priority 5)
  assert_false(priority_buffer4.events.some(fn(e) { e.id == "event-1" }))
  assert_true(priority_buffer4.events.some(fn(e) { e.id == "event-5" }))
  
  // Add low priority event
  let event6 = { id: "event-6", timestamp: 1025, data: "data-6", priority: 0 }
  let priority_buffer5 = add_event(priority_buffer4, event6)
  
  // event-6 should be dropped (lower priority than all existing events)
  assert_eq(priority_buffer5.events.length(), 3)
  assert_false(priority_buffer5.events.some(fn(e) { e.id == "event-6" }))
  
  // Test getting events
  let (buffer5, retrieved_events) = (get_events(buffer4, 2), buffer4.events.slice(0, 2))
  
  assert_eq(retrieved_events.length(), 2)
  assert_eq(retrieved_events[0].id, "event-2")
  assert_eq(retrieved_events[1].id, "event-3")
}

// Test 2: Event Window Processing
test "event window processing" {
  // Define time window
  type TimeWindow = {
    start_time: Int,
    end_time: Int,
    events: Array[StreamEvent]
  }
  
  // Define window processor
  type WindowProcessor = {
    window_size_ms: Int,
    slide_interval_ms: Int,
    current_window: TimeWindow
  }
  
  // Create window processor
  let create_window_processor = fn(window_size_ms: Int, slide_interval_ms: Int, start_time: Int) {
    {
      window_size_ms: window_size_ms,
      slide_interval_ms: slide_interval_ms,
      current_window: {
        start_time: start_time,
        end_time: start_time + window_size_ms,
        events: []
      }
    }
  }
  
  // Add event to window
  let add_event_to_window = fn(processor: WindowProcessor, event: StreamEvent) {
    if event.timestamp >= processor.current_window.start_time and 
       event.timestamp < processor.current_window.end_time {
      // Event fits in current window
      let updated_events = processor.current_window.events.push(event)
      let updated_window = {
        start_time: processor.current_window.start_time,
        end_time: processor.current_window.end_time,
        events: updated_events
      }
      
      {
        window_size_ms: processor.window_size_ms,
        slide_interval_ms: processor.slide_interval_ms,
        current_window: updated_window
      }
    } else {
      // Event doesn't fit in current window
      processor
    }
  }
  
  // Slide window to next time period
  let slide_window = fn(processor: WindowProcessor) {
    let new_start_time = processor.current_window.start_time + processor.slide_interval_ms
    let new_end_time = new_start_time + processor.window_size_ms
    
    // Filter events that fall in the new window
    let filtered_events = processor.current_window.events.filter(fn(event) {
      event.timestamp >= new_start_time and event.timestamp < new_end_time
    })
    
    {
      window_size_ms: processor.window_size_ms,
      slide_interval_ms: processor.slide_interval_ms,
      current_window: {
        start_time: new_start_time,
        end_time: new_end_time,
        events: filtered_events
      }
    }
  }
  
  // Get window statistics
  let get_window_stats = fn(window: TimeWindow) {
    let event_count = window.events.length()
    
    if event_count == 0 {
      {
        count: 0,
        avg_timestamp: 0,
        min_timestamp: 0,
        max_timestamp: 0,
        priority_sum: 0
      }
    } else {
      let mut timestamp_sum = 0
      let mut min_timestamp = window.events[0].timestamp
      let mut max_timestamp = window.events[0].timestamp
      let mut priority_sum = 0
      
      for i in 0..window.events.length() {
        let event = window.events[i]
        timestamp_sum = timestamp_sum + event.timestamp
        
        if event.timestamp < min_timestamp {
          min_timestamp = event.timestamp
        }
        
        if event.timestamp > max_timestamp {
          max_timestamp = event.timestamp
        }
        
        priority_sum = priority_sum + event.priority
      }
      
      {
        count: event_count,
        avg_timestamp: timestamp_sum / event_count,
        min_timestamp: min_timestamp,
        max_timestamp: max_timestamp,
        priority_sum: priority_sum
      }
    }
  }
  
  // Test event window processing
  let processor = create_window_processor(1000, 500, 1000)  // 1s window, 0.5s slide
  
  assert_eq(processor.current_window.start_time, 1000)
  assert_eq(processor.current_window.end_time, 2000)
  assert_eq(processor.current_window.events.length(), 0)
  
  // Add events within window
  let event1 = { id: "event-1", timestamp: 1100, data: "data-1", priority: 1 }
  let event2 = { id: "event-2", timestamp: 1200, data: "data-2", priority: 2 }
  let event3 = { id: "event-3", timestamp: 1300, data: "data-3", priority: 3 }
  
  let processor1 = add_event_to_window(processor, event1)
  let processor2 = add_event_to_window(processor1, event2)
  let processor3 = add_event_to_window(processor2, event3)
  
  assert_eq(processor3.current_window.events.length(), 3)
  
  // Add event outside window
  let event4 = { id: "event-4", timestamp: 2100, data: "data-4", priority: 4 }
  let processor4 = add_event_to_window(processor3, event4)
  
  assert_eq(processor4.current_window.events.length(), 3)  // event4 not added
  
  // Get window statistics
  let stats = get_window_stats(processor4.current_window)
  assert_eq(stats.count, 3)
  assert_eq(stats.avg_timestamp, 1200)  // (1100 + 1200 + 1300) / 3
  assert_eq(stats.min_timestamp, 1100)
  assert_eq(stats.max_timestamp, 1300)
  assert_eq(stats.priority_sum, 6)  // 1 + 2 + 3
  
  // Slide window
  let processor5 = slide_window(processor4)
  
  assert_eq(processor5.current_window.start_time, 1500)  // 1000 + 500
  assert_eq(processor5.current_window.end_time, 2500)     // 1500 + 1000
  
  // After sliding, only event3 (timestamp 1300) should be dropped
  assert_eq(processor5.current_window.events.length(), 0)
  
  // Add events to new window
  let event5 = { id: "event-5", timestamp: 1600, data: "data-5", priority: 5 }
  let event6 = { id: "event-6", timestamp: 1700, data: "data-6", priority: 6 }
  
  let processor6 = add_event_to_window(processor5, event5)
  let processor7 = add_event_to_window(processor6, event6)
  
  assert_eq(processor7.current_window.events.length(), 2)
  
  // Add event that spans multiple slides
  let event7 = { id: "event-7", timestamp: 1800, data: "data-7", priority: 7 }
  let processor8 = add_event_to_window(processor7, event7)
  
  assert_eq(processor8.current_window.events.length(), 3)
  
  // Slide again
  let processor9 = slide_window(processor8)
  
  assert_eq(processor9.current_window.start_time, 2000)  // 1500 + 500
  assert_eq(processor9.current_window.end_time, 3000)     // 2000 + 1000
  
  // Only event7 (timestamp 1800) should be dropped
  assert_eq(processor9.current_window.events.length(), 0)
}

// Test 3: Stream Aggregation
test "stream aggregation" {
  // Define aggregation type
  enum AggregationType {
    Count
    Sum
    Average
    Min
    Max
  }
  
  // Define aggregation function
  type AggregationFunction = {
    type: AggregationType,
    field: String
  }
  
  // Define aggregation result
  type AggregationResult = {
    key: String,
    value: Float,
    count: Int
  }
  
  // Define stream aggregator
  type StreamAggregator = {
    aggregations: Array[AggregationFunction],
    grouped_data: Map[String, Array[StreamEvent]]
  }
  
  // Create stream aggregator
  let create_stream_aggregator = fn(aggregations: Array[AggregationFunction]) {
    {
      aggregations: aggregations,
      grouped_data: []
    }
  }
  
  // Add event to aggregator
  let add_event_to_aggregator = fn(aggregator: StreamAggregator, event: StreamEvent, group_key: String) {
    let existing_group = aggregator.grouped_data.find(fn(pair) { pair.0 == group_key })
    
    match existing_group {
      Some((_, events)) => {
        // Add to existing group
        let updated_events = events.push(event)
        let updated_groups = aggregator.grouped_data.map(fn(pair) {
          if pair.0 == group_key {
            (group_key, updated_events)
          } else {
            pair
          }
        })
        
        {
          aggregations: aggregator.aggregations,
          grouped_data: updated_groups
        }
      }
      None => {
        // Create new group
        let updated_groups = aggregator.grouped_data.push((group_key, [event]))
        
        {
          aggregations: aggregator.aggregations,
          grouped_data: updated_groups
        }
      }
    }
  }
  
  // Extract numeric value from event (simplified)
  let extract_numeric_value = fn(event: StreamEvent, field: String) {
    match field {
      "priority" => Some(event.priority as Float)
      "timestamp" => Some(event.timestamp as Float)
      _ => None
    }
  }
  
  // Calculate aggregation
  let calculate_aggregation = fn(events: Array[StreamEvent], aggregation: AggregationFunction) {
    if events.length() == 0 {
      None
    } else {
      let values = events.map(fn(event) { extract_numeric_value(event, aggregation.field) })
                           .filter(fn(opt) { opt.is_some() })
                           .map(fn(opt) { match opt { Some(v) => v, None => 0.0 } })
      
      if values.length() == 0 {
        None
      } else {
        let result = match aggregation.type {
          Count => values.length() as Float
          Sum => {
            let mut sum = 0.0
            for i in 0..values.length() {
              sum = sum + values[i]
            }
            sum
          }
          Average => {
            let mut sum = 0.0
            for i in 0..values.length() {
              sum = sum + values[i]
            }
            sum / values.length() as Float
          }
          Min => {
            let mut min = values[0]
            for i in 1..values.length() {
              if values[i] < min {
                min = values[i]
              }
            }
            min
          }
          Max => {
            let mut max = values[0]
            for i in 1..values.length() {
              if values[i] > max {
                max = values[i]
              }
            }
            max
          }
        }
        
        Some(result)
      }
    }
  }
  
  // Get aggregation results
  let get_aggregation_results = fn(aggregator: StreamAggregator) {
    let mut results = []
    
    for i in 0..aggregator.grouped_data.length() {
      let group = aggregator.grouped_data[i]
      let group_key = group.0
      let events = group.1
      
      for j in 0..aggregator.aggregations.length() {
        let aggregation = aggregator.aggregations[j]
        let aggregation_value = calculate_aggregation(events, aggregation)
        
        match aggregation_value {
          Some(value) => {
            results = results.push({
              key: group_key + "." + aggregation.field + "." + match aggregation.type {
                Count => "count"
                Sum => "sum"
                Average => "avg"
                Min => "min"
                Max => "max"
              },
              value: value,
              count: events.length()
            })
          }
          None => {}
        }
      }
    }
    
    results
  }
  
  // Test stream aggregation
  let aggregations = [
    { type: Count, field: "priority" },
    { type: Sum, field: "priority" },
    { type: Average, field: "priority" },
    { type: Min, field: "priority" },
    { type: Max, field: "priority" }
  ]
  
  let aggregator = create_stream_aggregator(aggregations)
  
  // Add events
  let event1 = { id: "event-1", timestamp: 1000, data: "data-1", priority: 1 }
  let event2 = { id: "event-2", timestamp: 1005, data: "data-2", priority: 2 }
  let event3 = { id: "event-3", timestamp: 1010, data: "data-3", priority: 3 }
  let event4 = { id: "event-4", timestamp: 1015, data: "data-4", priority: 4 }
  let event5 = { id: "event-5", timestamp: 1020, data: "data-5", priority: 5 }
  
  let aggregator1 = add_event_to_aggregator(aggregator, event1, "group-A")
  let aggregator2 = add_event_to_aggregator(aggregator1, event2, "group-A")
  let aggregator3 = add_event_to_aggregator(aggregator2, event3, "group-A")
  let aggregator4 = add_event_to_aggregator(aggregator3, event4, "group-B")
  let aggregator5 = add_event_to_aggregator(aggregator4, event5, "group-B")
  
  // Get aggregation results
  let results = get_aggregation_results(aggregator5)
  
  // Should have 10 results (5 aggregations × 2 groups)
  assert_eq(results.length(), 10)
  
  // Check group-A results
  let group_a_count = results.find(fn(r) { r.key == "group-A.priority.count" })
  match group_a_count {
    Some(result) => {
      assert_eq(result.value, 3.0)  // 3 events
      assert_eq(result.count, 3)
    }
    None => assert_true(false)
  }
  
  let group_a_sum = results.find(fn(r) { r.key == "group-A.priority.sum" })
  match group_a_sum {
    Some(result) => {
      assert_eq(result.value, 6.0)  // 1 + 2 + 3
    }
    None => assert_true(false)
  }
  
  let group_a_avg = results.find(fn(r) { r.key == "group-A.priority.avg" })
  match group_a_avg {
    Some(result) => {
      assert_eq(result.value, 2.0)  // (1 + 2 + 3) / 3
    }
    None => assert_true(false)
  }
  
  let group_a_min = results.find(fn(r) { r.key == "group-A.priority.min" })
  match group_a_min {
    Some(result) => {
      assert_eq(result.value, 1.0)
    }
    None => assert_true(false)
  }
  
  let group_a_max = results.find(fn(r) { r.key == "group-A.priority.max" })
  match group_a_max {
    Some(result) => {
      assert_eq(result.value, 3.0)
    }
    None => assert_true(false)
  }
  
  // Check group-B results
  let group_b_count = results.find(fn(r) { r.key == "group-B.priority.count" })
  match group_b_count {
    Some(result) => {
      assert_eq(result.value, 2.0)  // 2 events
      assert_eq(result.count, 2)
    }
    None => assert_true(false)
  }
  
  let group_b_sum = results.find(fn(r) { r.key == "group-B.priority.sum" })
  match group_b_sum {
    Some(result) => {
      assert_eq(result.value, 9.0)  // 4 + 5
    }
    None => assert_true(false)
  }
}

// Test 4: Stream Filtering and Transformation
test "stream filtering and transformation" {
  // Define filter predicate
  type FilterPredicate = {
    field: String,
    operator: String,  // "eq", "ne", "gt", "lt", "gte", "lte", "contains"
    value: String
  }
  
  // Define transformation function
  type TransformationFunction = {
    field: String,
    operation: String  // "add", "multiply", "uppercase", "lowercase", "substring"
    parameter: String
  }
  
  // Define stream processor
  type StreamProcessor = {
    filters: Array[FilterPredicate],
    transformations: Array[TransformationFunction]
  }
  
  // Create stream processor
  let create_stream_processor = fn(filters: Array[FilterPredicate], transformations: Array[TransformationFunction]) {
    {
      filters: filters,
      transformations: transformations
    }
  }
  
  // Get field value from event
  let get_field_value = fn(event: StreamEvent, field: String) {
    match field {
      "id" => event.id
      "data" => event.data
      "priority" => event.priority.to_string()
      "timestamp" => event.timestamp.to_string()
      _ => ""
    }
  }
  
  // Apply filter to event
  let apply_filter = fn(event: StreamEvent, filter: FilterPredicate) {
    let field_value = get_field_value(event, filter.field)
    
    match filter.operator {
      "eq" => field_value == filter.value
      "ne" => field_value != filter.value
      "gt" => field_value.to_float() > filter.value.to_float()
      "lt" => field_value.to_float() < filter.value.to_float()
      "gte" => field_value.to_float() >= filter.value.to_float()
      "lte" => field_value.to_float() <= filter.value.to_float()
      "contains" => field_value.contains(filter.value)
      _ => false
    }
  }
  
  // Apply transformation to event
  let apply_transformation = fn(event: StreamEvent, transformation: TransformationFunction) {
    let field_value = get_field_value(event, transformation.field)
    
    let transformed_value = match transformation.operation {
      "add" => (field_value.to_float() + transformation.parameter.to_float()).to_string()
      "multiply" => (field_value.to_float() * transformation.parameter.to_float()).to_string()
      "uppercase" => field_value.to_uppercase()
      "lowercase" => field_value.to_lowercase()
      "substring" => {
        let start = transformation.parameter.to_int()
        if start < field_value.length() {
          field_value.substring(start, field_value.length() - start)
        } else {
          field_value
        }
      }
      _ => field_value
    }
    
    // Update event with transformed value
    match transformation.field {
      "id" => { event | id: transformed_value }
      "data" => { event | data: transformed_value }
      "priority" => { event | priority: transformed_value.to_int() }
      "timestamp" => { event | timestamp: transformed_value.to_int() }
      _ => event
    }
  }
  
  // Process event through filters and transformations
  let process_event = fn(processor: StreamProcessor, event: StreamEvent) {
    // Apply filters
    let mut passes_filters = true
    
    for i in 0..processor.filters.length() {
      if not(apply_filter(event, processor.filters[i])) {
        passes_filters = false
        break
      }
    }
    
    if not(passes_filters) {
      None
    } else {
      // Apply transformations
      let mut transformed_event = event
      
      for i in 0..processor.transformations.length() {
        transformed_event = apply_transformation(transformed_event, processor.transformations[i])
      }
      
      Some(transformed_event)
    }
  }
  
  // Process stream of events
  let process_stream = fn(processor: StreamProcessor, events: Array[StreamEvent]) {
    let mut results = []
    
    for i in 0..events.length() {
      let processed = process_event(processor, events[i])
      
      match processed {
        Some(event) => results = results.push(event)
        None => {}
      }
    }
    
    results
  }
  
  // Test stream filtering and transformation
  let filters = [
    { field: "priority", operator: "gt", value: "2" },
    { field: "data", operator: "contains", value: "important" }
  ]
  
  let transformations = [
    { field: "data", operation: "uppercase", parameter: "" },
    { field: "priority", operation: "multiply", parameter: "2" }
  ]
  
  let processor = create_stream_processor(filters, transformations)
  
  // Create test events
  let events = [
    { id: "event-1", timestamp: 1000, data: "important data", priority: 1 },  // Fails priority filter
    { id: "event-2", timestamp: 1005, data: "normal data", priority: 3 },     // Fails data filter
    { id: "event-3", timestamp: 1010, data: "important data", priority: 3 }, // Passes both
    { id: "event-4", timestamp: 1015, data: "IMPORTANT DATA", priority: 4 }, // Passes both
    { id: "event-5", timestamp: 1020, data: "important data", priority: 2 }  // Fails priority filter
  ]
  
  // Process stream
  let results = process_stream(processor, events)
  
  // Should have 2 results (event-3 and event-4)
  assert_eq(results.length(), 2)
  
  // Check event-3 transformation
  let result3 = results.find(fn(e) { e.id == "event-3" })
  match result3 {
    Some(event) => {
      assert_eq(event.data, "IMPORTANT DATA")  // Uppercased
      assert_eq(event.priority, 6)             // Multiplied by 2
    }
    None => assert_true(false)
  }
  
  // Check event-4 transformation
  let result4 = results.find(fn(e) { e.id == "event-4" })
  match result4 {
    Some(event) => {
      assert_eq(event.data, "IMPORTANT DATA")  // Uppercased (already uppercase)
      assert_eq(event.priority, 8)             // Multiplied by 2
    }
    None => assert_true(false)
  }
}

// Test 5: Stream Join Operations
test "stream join operations" {
  // Define join type
  enum JoinType {
    Inner
    Left
    Right
    Full
  }
  
  // Define stream joiner
  type StreamJoiner = {
    join_type: JoinType,
    left_key: String,
    right_key: String,
    time_window_ms: Int,
    left_buffer: Array[StreamEvent],
    right_buffer: Array[StreamEvent]
  }
  
  // Define joined event
  type JoinedEvent = {
    left: Option[StreamEvent],
    right: Option[StreamEvent],
    join_key: String
  }
  
  // Create stream joiner
  let create_stream_joiner = fn(join_type: JoinType, left_key: String, right_key: String, time_window_ms: Int) {
    {
      join_type: join_type,
      left_key: left_key,
      right_key: right_key,
      time_window_ms: time_window_ms,
      left_buffer: [],
      right_buffer: []
    }
  }
  
  // Add event to left buffer
  let add_left_event = fn(joiner: StreamJoiner, event: StreamEvent) {
    {
      join_type: joiner.join_type,
      left_key: joiner.left_key,
      right_key: joiner.right_key,
      time_window_ms: joiner.time_window_ms,
      left_buffer: joiner.left_buffer.push(event),
      right_buffer: joiner.right_buffer
    }
  }
  
  // Add event to right buffer
  let add_right_event = fn(joiner: StreamJoiner, event: StreamEvent) {
    {
      join_type: joiner.join_type,
      left_key: joiner.left_key,
      right_key: joiner.right_key,
      time_window_ms: joiner.time_window_ms,
      left_buffer: joiner.left_buffer,
      right_buffer: joiner.right_buffer.push(event)
    }
  }
  
  // Perform join operation
  let perform_join = fn(joiner: StreamJoiner, current_time: Int) {
    let mut results = []
    
    // Clean up old events outside time window
    let min_time = current_time - joiner.time_window_ms
    
    let valid_left_events = joiner.left_buffer.filter(fn(event) { event.timestamp >= min_time })
    let valid_right_events = joiner.right_buffer.filter(fn(event) { event.timestamp >= min_time })
    
    // Find matching pairs
    for i in 0..valid_left_events.length() {
      let left_event = valid_left_events[i]
      let left_key_value = get_field_value(left_event, joiner.left_key)
      
      let matching_right_events = valid_right_events.filter(fn(event) {
        get_field_value(event, joiner.right_key) == left_key_value
      })
      
      if matching_right_events.length() > 0 {
        // Have matches
        for j in 0..matching_right_events.length() {
          let right_event = matching_right_events[j]
          
          results = results.push({
            left: Some(left_event),
            right: Some(right_event),
            join_key: left_key_value
          })
        }
      } else {
        // No matches
        match joiner.join_type {
          Left => {
            results = results.push({
              left: Some(left_event),
              right: None,
              join_key: left_key_value
            })
          }
          Full => {
            results = results.push({
              left: Some(left_event),
              right: None,
              join_key: left_key_value
            })
          }
          _ => {}
        }
      }
    }
    
    // Find right events without matches
    for i in 0..valid_right_events.length() {
      let right_event = valid_right_events[i]
      let right_key_value = get_field_value(right_event, joiner.right_key)
      
      let matching_left_events = valid_left_events.filter(fn(event) {
        get_field_value(event, joiner.left_key) == right_key_value
      })
      
      if matching_left_events.length() == 0 {
        // No matches
        match joiner.join_type {
          Right => {
            results = results.push({
              left: None,
              right: Some(right_event),
              join_key: right_key_value
            })
          }
          Full => {
            results = results.push({
              left: None,
              right: Some(right_event),
              join_key: right_key_value
            })
          }
          _ => {}
        }
      }
    }
    
    results
  }
  
  // Helper function to get field value (reused from previous test)
  let get_field_value = fn(event: StreamEvent, field: String) {
    match field {
      "id" => event.id
      "data" => event.data
      "priority" => event.priority.to_string()
      "timestamp" => event.timestamp.to_string()
      _ => ""
    }
  }
  
  // Test stream join operations
  let inner_joiner = create_stream_joiner(Inner, "data", "data", 5000)
  
  // Add left events
  let left_event1 = { id: "left-1", timestamp: 1000, data: "user-123", priority: 1 }
  let left_event2 = { id: "left-2", timestamp: 1005, data: "user-456", priority: 2 }
  let left_event3 = { id: "left-3", timestamp: 1010, data: "user-789", priority: 3 }
  
  let inner_joiner1 = add_left_event(inner_joiner, left_event1)
  let inner_joiner2 = add_left_event(inner_joiner1, left_event2)
  let inner_joiner3 = add_left_event(inner_joiner2, left_event3)
  
  // Add right events
  let right_event1 = { id: "right-1", timestamp: 1015, data: "user-123", priority: 1 }
  let right_event2 = { id: "right-2", timestamp: 1020, data: "user-999", priority: 2 }
  let right_event3 = { id: "right-3", timestamp: 1025, data: "user-456", priority: 3 }
  
  let inner_joiner4 = add_right_event(inner_joiner3, right_event1)
  let inner_joiner5 = add_right_event(inner_joiner4, right_event2)
  let inner_joiner6 = add_right_event(inner_joiner5, right_event3)
  
  // Perform inner join
  let inner_results = perform_join(inner_joiner6, 2000)
  
  // Should have 2 results: (left-1, right-1) and (left-2, right-3)
  assert_eq(inner_results.length(), 2)
  
  let result1 = inner_results.find(fn(r) { r.join_key == "user-123" })
  match result1 {
    Some(joined) => {
      assert_eq(joined.left, Some(left_event1))
      assert_eq(joined.right, Some(right_event1))
    }
    None => assert_true(false)
  }
  
  let result2 = inner_results.find(fn(r) { r.join_key == "user-456" })
  match result2 {
    Some(joined) => {
      assert_eq(joined.left, Some(left_event2))
      assert_eq(joined.right, Some(right_event3))
    }
    None => assert_true(false)
  }
  
  // Test left join
  let left_joiner = create_stream_joiner(Left, "data", "data", 5000)
  
  let left_joiner1 = add_left_event(left_joiner, left_event1)
  let left_joiner2 = add_left_event(left_joiner1, left_event2)
  let left_joiner3 = add_left_event(left_joiner2, left_event3)
  
  let left_joiner4 = add_right_event(left_joiner3, right_event1)
  let left_joiner5 = add_right_event(left_joiner4, right_event2)
  
  // Only add right_event1 and right_event2 (not right_event3)
  
  let left_results = perform_join(left_joiner5, 2000)
  
  // Should have 3 results:
  // 1. (left-1, right-1) - match
  // 2. (left-2, None) - no match
  // 3. (left-3, None) - no match
  assert_eq(left_results.length(), 3)
  
  let left_result1 = left_results.find(fn(r) { r.join_key == "user-123" })
  match left_result1 {
    Some(joined) => {
      assert_eq(joined.left, Some(left_event1))
      assert_eq(joined.right, Some(right_event1))
    }
    None => assert_true(false)
  }
  
  let left_result2 = left_results.find(fn(r) { r.join_key == "user-456" })
  match left_result2 {
    Some(joined) => {
      assert_eq(joined.left, Some(left_event2))
      assert_eq(joined.right, None)
    }
    None => assert_true(false)
  }
  
  let left_result3 = left_results.find(fn(r) { r.join_key == "user-789" })
  match left_result3 {
    Some(joined) => {
      assert_eq(joined.left, Some(left_event3))
      assert_eq(joined.right, None)
    }
    None => assert_true(false)
  }
}

// Test 6: Stream Backpressure Handling
test "stream backpressure handling" {
  // Define backpressure strategy
  enum BackpressureStrategy {
    DropOldest
    DropNewest
    Buffer
    Throttle
  }
  
  // Define stream processor with backpressure
  type BackpressureProcessor = {
    buffer_size: Int,
    current_buffer: Array[StreamEvent],
    strategy: BackpressureStrategy,
    processing_rate: Int,  // events per second
    last_process_time: Int,
    dropped_events: Int
  }
  
  // Create backpressure processor
  let create_backpressure_processor = fn(buffer_size: Int, strategy: BackpressureStrategy, processing_rate: Int) {
    {
      buffer_size: buffer_size,
      current_buffer: [],
      strategy: strategy,
      processing_rate: processing_rate,
      last_process_time: 0,
      dropped_events: 0
    }
  }
  
  // Add event with backpressure handling
  let add_event_with_backpressure = fn(processor: BackpressureProcessor, event: StreamEvent, current_time: Int) {
    if processor.current_buffer.length() < processor.buffer_size {
      // Buffer not full, just add
      {
        buffer_size: processor.buffer_size,
        current_buffer: processor.current_buffer.push(event),
        strategy: processor.strategy,
        processing_rate: processor.processing_rate,
        last_process_time: processor.last_process_time,
        dropped_events: processor.dropped_events
      }
    } else {
      // Buffer full, apply backpressure strategy
      match processor.strategy {
        DropOldest => {
          // Drop oldest event
          let updated_buffer = processor.current_buffer.slice(1).push(event)
          
          {
            buffer_size: processor.buffer_size,
            current_buffer: updated_buffer,
            strategy: processor.strategy,
            processing_rate: processor.processing_rate,
            last_process_time: processor.last_process_time,
            dropped_events: processor.dropped_events + 1
          }
        }
        DropNewest => {
          // Drop the new event
          {
            buffer_size: processor.buffer_size,
            current_buffer: processor.current_buffer,
            strategy: processor.strategy,
            processing_rate: processor.processing_rate,
            last_process_time: processor.last_process_time,
            dropped_events: processor.dropped_events + 1
          }
        }
        Buffer => {
          // Try to process some events first
          let time_since_last_process = current_time - processor.last_process_time
          let events_to_process = if time_since_last_process > 0 {
            (time_since_last_process * processor.processing_rate) / 1000
          } else {
            0
          }
          
          if events_to_process > 0 {
            let processed_count = if events_to_process > processor.current_buffer.length() {
              processor.current_buffer.length()
            } else {
              events_to_process
            }
            
            let remaining_buffer = processor.current_buffer.slice(processed_count)
            
            if remaining_buffer.length() < processor.buffer_size {
              // Space available after processing
              {
                buffer_size: processor.buffer_size,
                current_buffer: remaining_buffer.push(event),
                strategy: processor.strategy,
                processing_rate: processor.processing_rate,
                last_process_time: current_time,
                dropped_events: processor.dropped_events
              }
            } else {
              // Still no space after processing
              {
                buffer_size: processor.buffer_size,
                current_buffer: remaining_buffer,
                strategy: processor.strategy,
                processing_rate: processor.processing_rate,
                last_process_time: current_time,
                dropped_events: processor.dropped_events + 1
              }
            }
          } else {
            // Can't process any events yet
            {
              buffer_size: processor.buffer_size,
              current_buffer: processor.current_buffer,
              strategy: processor.strategy,
              processing_rate: processor.processing_rate,
              last_process_time: processor.last_process_time,
              dropped_events: processor.dropped_events + 1
            }
          }
        }
        Throttle => {
          // Simulate throttling by checking if we can accept this event
          let time_since_last_process = current_time - processor.last_process_time
          let can_process = time_since_last_process >= (1000 / processor.processing_rate)
          
          if can_process {
            // Can process this event
            {
              buffer_size: processor.buffer_size,
              current_buffer: processor.current_buffer.push(event),
              strategy: processor.strategy,
              processing_rate: processor.processing_rate,
              last_process_time: current_time,
              dropped_events: processor.dropped_events
            }
          } else {
            // Throttling, drop the event
            {
              buffer_size: processor.buffer_size,
              current_buffer: processor.current_buffer,
              strategy: processor.strategy,
              processing_rate: processor.processing_rate,
              last_process_time: processor.last_process_time,
              dropped_events: processor.dropped_events + 1
            }
          }
        }
      }
    }
  }
  
  // Process events from buffer
  let process_buffered_events = fn(processor: BackpressureProcessor, current_time: Int) {
    let time_since_last_process = current_time - processor.last_process_time
    let events_to_process = if time_since_last_process > 0 {
      (time_since_last_process * processor.processing_rate) / 1000
    } else {
      0
    }
    
    let processed_count = if events_to_process > processor.current_buffer.length() {
      processor.current_buffer.length()
    } else {
      events_to_process
    }
    
    let remaining_buffer = processor.current_buffer.slice(processed_count)
    
    {
      buffer_size: processor.buffer_size,
      current_buffer: remaining_buffer,
      strategy: processor.strategy,
      processing_rate: processor.processing_rate,
      last_process_time: current_time,
      dropped_events: processor.dropped_events
    }
  }
  
  // Test stream backpressure handling
  let drop_oldest_processor = create_backpressure_processor(3, DropOldest, 10)
  
  // Fill buffer
  let event1 = { id: "event-1", timestamp: 1000, data: "data-1", priority: 1 }
  let event2 = { id: "event-2", timestamp: 1005, data: "data-2", priority: 2 }
  let event3 = { id: "event-3", timestamp: 1010, data: "data-3", priority: 3 }
  
  let processor1 = add_event_with_backpressure(drop_oldest_processor, event1, 1000)
  let processor2 = add_event_with_backpressure(processor1, event2, 1005)
  let processor3 = add_event_with_backpressure(processor2, event3, 1010)
  
  assert_eq(processor3.current_buffer.length(), 3)
  assert_eq(processor3.dropped_events, 0)
  
  // Add event when buffer is full
  let event4 = { id: "event-4", timestamp: 1015, data: "data-4", priority: 4 }
  let processor4 = add_event_with_backpressure(processor3, event4, 1015)
  
  assert_eq(processor4.current_buffer.length(), 3)
  assert_eq(processor4.dropped_events, 1)
  assert_eq(processor4.current_buffer[0].id, "event-2")  // event-1 dropped
  assert_eq(processor4.current_buffer[2].id, "event-4")
  
  // Test throttle strategy
  let throttle_processor = create_backpressure_processor(10, Throttle, 2)  // 2 events per second
  
  let throttle_processor1 = add_event_with_backpressure(throttle_processor, event1, 1000)
  let throttle_processor2 = add_event_with_backpressure(throttle_processor1, event2, 1000)
  let throttle_processor3 = add_event_with_backpressure(throttle_processor2, event3, 1000)
  
  assert_eq(throttle_processor3.current_buffer.length(), 1)  // Only event1 accepted
  assert_eq(throttle_processor3.dropped_events, 2)  // event2 and event3 dropped
  
  // Wait and try again
  let throttle_processor4 = add_event_with_backpressure(throttle_processor3, event4, 1500)
  
  assert_eq(throttle_processor4.current_buffer.length(), 2)  // event4 accepted
  assert_eq(throttle_processor4.dropped_events, 2)
  
  // Test buffer strategy
  let buffer_processor = create_backpressure_processor(2, Buffer, 5)  // 5 events per second
  
  let buffer_processor1 = add_event_with_backpressure(buffer_processor, event1, 1000)
  let buffer_processor2 = add_event_with_backpressure(buffer_processor1, event2, 1000)
  
  assert_eq(buffer_processor2.current_buffer.length(), 2)
  
  // Add event when buffer is full
  let buffer_processor3 = add_event_with_backpressure(buffer_processor2, event3, 2000)
  
  // Should process some events and then add the new one
  assert_eq(buffer_processor3.current_buffer.length(), 1)  // event3 added after processing
  assert_eq(buffer_processor3.dropped_events, 0)
  
  // Add more events to test processing rate
  let buffer_processor4 = add_event_with_backpressure(buffer_processor3, event4, 3000)
  
  assert_eq(buffer_processor4.current_buffer.length(), 2)
  assert_eq(buffer_processor4.dropped_events, 0)
}