// Azimuth Premium Realtime Stream Processing Tests
// This file contains high-quality test cases for realtime stream processing

// Test 1: Stream Data Structures
test "stream data structures" {
  // Stream event types
  type StreamEvent {
    id : String
    timestamp : Int
    data : String
    metadata : Array[(String, String)]
  }
  
  // Stream window types
  type WindowType {
    Tumbling(Int)    // Fixed size window in ms
    Sliding(Int, Int) // Window size and slide interval in ms
    Session(Int)     // Session timeout in ms
  }
  
  // Stream window
  type StreamWindow {
    window_type : WindowType
    start_time : Int
    end_time : Int
    events : Array[StreamEvent]
  }
  
  // Create stream event
  let create_event = fn(id : String, timestamp : Int, data : String, metadata : Array[(String, String)]) -> StreamEvent {
    StreamEvent {
      id: id,
      timestamp: timestamp,
      data: data,
      metadata: metadata
    }
  }
  
  // Create stream window
  let create_window = fn(window_type : WindowType, start_time : Int, end_time : Int) -> StreamWindow {
    StreamWindow {
      window_type: window_type,
      start_time: start_time,
      end_time: end_time,
      events: []
    }
  }
  
  // Add event to window
  let add_event_to_window = fn(window : StreamWindow, event : StreamEvent) -> StreamWindow {
    if event.timestamp >= window.start_time && event.timestamp < window.end_time {
      StreamWindow {
        window_type: window.window_type,
        start_time: window.start_time,
        end_time: window.end_time,
        events: window.events.push(event)
      }
    } else {
      window
    }
  }
  
  // Test event creation
  let event1 = create_event("1", 1000, "data1", [("type", "sensor"), ("source", "device1")])
  let event2 = create_event("2", 1500, "data2", [("type", "sensor"), ("source", "device2")])
  let event3 = create_event("3", 2000, "data3", [("type", "alert"), ("source", "device1")])
  
  assert_eq(event1.id, "1")
  assert_eq(event1.timestamp, 1000)
  assert_eq(event1.data, "data1")
  assert_eq(event1.metadata.length(), 2)
  
  // Test window creation
  let tumbling_window = create_window(Tumbling(1000), 1000, 2000)
  assert_eq(tumbling_window.start_time, 1000)
  assert_eq(tumbling_window.end_time, 2000)
  assert_eq(tumbling_window.events.length(), 0)
  
  // Test adding events to window
  let window_with_events = tumbling_window
    |> add_event_to_window(event1)
    |> add_event_to_window(event2)
    |> add_event_to_window(event3) // Should be outside window
  
  assert_eq(window_with_events.events.length(), 2)
  assert_eq(window_with_events.events[0].id, "1")
  assert_eq(window_with_events.events[1].id, "2")
  
  // Test sliding window
  let sliding_window = create_window(Sliding(2000, 1000), 1000, 3000)
  let sliding_with_events = sliding_window
    |> add_event_to_window(event1)
    |> add_event_to_window(event2)
    |> add_event_to_window(event3)
  
  assert_eq(sliding_with_events.events.length(), 3)
  
  // Test session window
  let session_window = create_window(Session(500), 1000, 1500)
  let session_with_events = session_window
    |> add_event_to_window(event1)
    |> add_event_to_window(event2)
    |> add_event_to_window(event3) // Should be outside session
  
  assert_eq(session_with_events.events.length(), 2)
}

// Test 2: Stream Operations
test "stream operations" {
  // Stream operation types
  type StreamOperation {
    Filter(String -> Bool)
    Map(String -> String)
    FlatMap(String -> Array[String])
    Reduce((String, String) -> String, String)
    Aggregate(AggregationFunction)
    GroupBy(String -> String)
    Window(WindowType)
  }
  
  type AggregationFunction {
    Count
    Sum
    Average
    Min
    Max
  }
  
  // Apply stream operation
  let apply_operation = fn(events : Array[StreamEvent], operation : StreamOperation) -> Array[StreamEvent] {
    match operation {
      Filter(predicate) => {
        events.filter(|event| predicate(event.data))
      }
      Map(transformer) => {
        events.map(|event| StreamEvent {
          id: event.id,
          timestamp: event.timestamp,
          data: transformer(event.data),
          metadata: event.metadata
        })
      }
      FlatMap(transformer) => {
        let mut result = []
        
        for event in events {
          let transformed = transformer(event.data)
          
          for data in transformed {
            result.push(StreamEvent {
              id: event.id + "_" + data,
              timestamp: event.timestamp,
              data: data,
              metadata: event.metadata
            })
          }
        }
        
        result
      }
      Reduce(reducer, initial) => {
        if events.length() == 0 {
          return []
        }
        
        let mut accumulator = initial
        
        for event in events {
          accumulator = reducer(accumulator, event.data)
        }
        
        [StreamEvent {
          id: "reduced",
          timestamp: events[0].timestamp,
          data: accumulator,
          metadata: []
        }]
      }
      Aggregate(func) => {
        if events.length() == 0 {
          return []
        }
        
        let result = match func {
          Count => events.length().to_string()
          Sum => {
            let mut sum = 0
            for event in events {
              match event.data.to_int() {
                Some(value) => sum = sum + value
                None => ()
              }
            }
            sum.to_string()
          }
          Average => {
            let mut sum = 0
            let mut count = 0
            
            for event in events {
              match event.data.to_int() {
                Some(value) => {
                  sum = sum + value
                  count = count + 1
                }
                None => ()
              }
            }
            
            if count > 0 { (sum / count).to_string() } else { "0" }
          }
          Min => {
            let mut min = ""
            let mut first = true
            
            for event in events {
              if first || event.data < min {
                min = event.data
                first = false
              }
            }
            
            min
          }
          Max => {
            let mut max = ""
            let mut first = true
            
            for event in events {
              if first || event.data > max {
                max = event.data
                first = false
              }
            }
            
            max
          }
        }
        
        [StreamEvent {
          id: "aggregated",
          timestamp: events[0].timestamp,
          data: result,
          metadata: []
        }]
      }
      GroupBy(key_extractor) => {
        let mut groups = []
        
        for event in events {
          let key = key_extractor(event.data)
          let mut found = false
          
          for i = 0; i < groups.length(); i = i + 1 {
            if groups[i].0 == key {
              groups[i] = (key, groups[i].1.push(event))
              found = true
              break
            }
          }
          
          if !found {
            groups.push((key, [event]))
          }
        }
        
        let mut result = []
        
        for (key, group_events) in groups {
          let group_data = group_events.map(|e| e.data).join(",")
          
          result.push(StreamEvent {
            id: "group_" + key,
            timestamp: group_events[0].timestamp,
            data: key + ":" + group_data,
            metadata: [("group", key)]
          })
        }
        
        result
      }
      Window(window_type) => {
        // Window operation is handled at a higher level
        events
      }
    }
  }
  
  // Create test events
  let events = [
    create_event("1", 1000, "10", []),
    create_event("2", 1100, "20", []),
    create_event("3", 1200, "30", []),
    create_event("4", 1300, "15", []),
    create_event("5", 1400, "25", [])
  ]
  
  // Test filter
  let filtered = apply_operation(events, Filter(|data| data.to_int().unwrap_or(0) > 15))
  assert_eq(filtered.length(), 3)
  assert_eq(filtered[0].data, "20")
  assert_eq(filtered[1].data, "30")
  assert_eq(filtered[2].data, "25")
  
  // Test map
  let mapped = apply_operation(events, Map(|data| "value:" + data))
  assert_eq(mapped.length(), 5)
  assert_eq(mapped[0].data, "value:10")
  assert_eq(mapped[1].data, "value:20")
  
  // Test flat map
  let flat_mapped = apply_operation(events.slice(0, 2), FlatMap(|data| [data + "_a", data + "_b"]))
  assert_eq(flat_mapped.length(), 4)
  assert_eq(flat_mapped[0].data, "10_a")
  assert_eq(flat_mapped[1].data, "10_b")
  assert_eq(flat_mapped[2].data, "20_a")
  assert_eq(flat_mapped[3].data, "20_b")
  
  // Test reduce
  let reduced = apply_operation(events, Reduce(|acc, val| acc + "," + val, ""))
  assert_eq(reduced.length(), 1)
  assert_eq(reduced[0].data, ",10,20,30,15,25")
  
  // Test aggregation
  let counted = apply_operation(events, Aggregate(Count))
  assert_eq(counted.length(), 1)
  assert_eq(counted[0].data, "5")
  
  let summed = apply_operation(events, Aggregate(Sum))
  assert_eq(summed.length(), 1)
  assert_eq(summed[0].data, "100") // 10+20+30+15+25
  
  let averaged = apply_operation(events, Aggregate(Average))
  assert_eq(averaged.length(), 1)
  assert_eq(averaged[0].data, "20") // 100/5
  
  let minned = apply_operation(events, Aggregate(Min))
  assert_eq(minned.length(), 1)
  assert_eq(minned[0].data, "10")
  
  let maxed = apply_operation(events, Aggregate(Max))
  assert_eq(maxed.length(), 1)
  assert_eq(maxed[0].data, "30")
  
  // Test group by
  let category_events = [
    create_event("1", 1000, "cat:10", []),
    create_event("2", 1100, "dog:20", []),
    create_event("3", 1200, "cat:15", []),
    create_event("4", 1300, "bird:25", []),
    create_event("5", 1400, "dog:30", [])
  ]
  
  let grouped = apply_operation(category_events, GroupBy(|data| data.split(":")[0]))
  assert_eq(grouped.length(), 3)
  
  // Sort by group name for consistent testing
  let sorted_groups = grouped.sort(|a, b| a.data - b.data)
  assert_eq(sorted_groups[0].data, "bird:25")
  assert_eq(sorted_groups[1].data, "cat:10,15")
  assert_eq(sorted_groups[2].data, "dog:20,30")
}

// Test 3: Stream Windowing
test "stream windowing" {
  // Window assignment
  type WindowAssignment {
    window_id : String
    start_time : Int
    end_time : Int
  }
  
  // Assign events to windows
  let assign_to_windows = fn(event : StreamEvent, window_type : WindowType, current_time : Int) -> Array[WindowAssignment] {
    match window_type {
      Tumbling(size) => {
        let window_start = (event.timestamp / size) * size
        let window_end = window_start + size
        
        [WindowAssignment {
          window_id: window_start.to_string(),
          start_time: window_start,
          end_time: window_end
        }]
      }
      Sliding(size, slide) => {
        let mut assignments = []
        let window_start = (event.timestamp / slide) * slide - size + slide
        
        while window_start <= event.timestamp {
          let window_end = window_start + size
          
          if event.timestamp < window_end {
            assignments.push(WindowAssignment {
              window_id: window_start.to_string(),
              start_time: window_start,
              end_time: window_end
            })
          }
          
          window_start = window_start + slide
        }
        
        assignments
      }
      Session(timeout) => {
        // Session windowing requires state, simplified here
        [WindowAssignment {
          window_id: "session_" + event.timestamp.to_string(),
          start_time: event.timestamp,
          end_time: event.timestamp + timeout
        }]
      }
    }
  }
  
  // Update windows with new event
  let update_windows = fn(windows : Array[StreamWindow], assignments : Array[WindowAssignment], event : StreamEvent) -> Array[StreamWindow] {
    let mut updated_windows = windows
    let mut processed_assignments = []
    
    // Update existing windows
    for i = 0; i < updated_windows.length(); i = i + 1 {
      let window = updated_windows[i]
      let mut updated = false
      
      for assignment in assignments {
        if window.start_time == assignment.start_time && window.end_time == assignment.end_time {
          updated_windows[i] = add_event_to_window(window, event)
          processed_assignments.push(assignment)
          updated = true
          break
        }
      }
      
      if !updated {
        // Keep window as is
        updated_windows[i] = window
      }
    }
    
    // Create new windows for unprocessed assignments
    for assignment in assignments {
      let mut already_processed = false
      
      for processed in processed_assignments {
        if assignment.start_time == processed.start_time && assignment.end_time == processed.end_time {
          already_processed = true
          break
        }
      }
      
      if !already_processed {
        let new_window = create_window(Tumbling(1000), assignment.start_time, assignment.end_time)
        updated_windows.push(add_event_to_window(new_window, event))
      }
    }
    
    updated_windows
  }
  
  // Test tumbling windows
  let events = [
    create_event("1", 1000, "data1", []),
    create_event("2", 1500, "data2", []),
    create_event("3", 2500, "data3", []),
    create_event("4", 2800, "data4", [])
  ]
  
  let mut windows = []
  
  for event in events {
    let assignments = assign_to_windows(event, Tumbling(2000), event.timestamp)
    windows = update_windows(windows, assignments, event)
  }
  
  assert_eq(windows.length(), 2)
  
  // First window (0-2000ms)
  let window1 = windows.find(|w| w.start_time == 0).unwrap()
  assert_eq(window1.events.length(), 2)
  assert_eq(window1.events[0].id, "1")
  assert_eq(window1.events[1].id, "2")
  
  // Second window (2000-4000ms)
  let window2 = windows.find(|w| w.start_time == 2000).unwrap()
  assert_eq(window2.events.length(), 2)
  assert_eq(window2.events[0].id, "3")
  assert_eq(window2.events[1].id, "4")
  
  // Test sliding windows
  windows = []
  
  for event in events {
    let assignments = assign_to_windows(event, Sliding(2000, 1000), event.timestamp)
    windows = update_windows(windows, assignments, event)
  }
  
  assert_eq(windows.length(), 4)
  
  // Test session windows
  windows = []
  
  for event in events {
    let assignments = assign_to_windows(event, Session(1000), event.timestamp)
    windows = update_windows(windows, assignments, event)
  }
  
  assert_eq(windows.length(), 4) // Each event gets its own session
}

// Test 4: Stream Joins
test "stream joins" {
  // Join types
  type JoinType {
    InnerJoin
    LeftJoin
    RightJoin
    FullOuterJoin
  }
  
  // Join condition
  type JoinCondition {
    On(String)  // Join on key
    Within(Int) // Time window in ms
  }
  
  // Perform stream join
  let join_streams = fn(
    left_events : Array[StreamEvent],
    right_events : Array[StreamEvent],
    join_type : JoinType,
    condition : JoinCondition
  ) -> Array[StreamEvent] {
    let mut results = []
    let key = match condition {
      On(key) => key
      Within(_) => "id" // Default key
    }
    
    for left_event in left_events {
      let mut left_matched = false
      
      for right_event in right_events {
        let left_key = extract_key(left_event, key)
        let right_key = extract_key(right_event, key)
        
        if left_key == right_key {
          // Check time window if specified
          let time_match = match condition {
            Within(window) => {
              (left_event.timestamp - right_event.timestamp).abs() <= window
            }
            On(_) => true
          }
          
          if time_match {
            let joined_data = left_event.data + "|" + right_event.data
            let joined_metadata = left_event.metadata.concat(right_event.metadata)
            
            results.push(StreamEvent {
              id: left_event.id + "_" + right_event.id,
              timestamp: left_event.timestamp, // Use left timestamp
              data: joined_data,
              metadata: joined_metadata
            })
            
            left_matched = true
          }
        }
      }
      
      // Handle left join
      if !left_matched && (join_type == LeftJoin || join_type == FullOuterJoin) {
        results.push(StreamEvent {
          id: left_event.id + "_null",
          timestamp: left_event.timestamp,
          data: left_event.data + "|null",
          metadata: left_event.metadata
        })
      }
    }
    
    // Handle right join and full outer join
    if join_type == RightJoin || join_type == FullOuterJoin {
      for right_event in right_events {
        let mut right_matched = false
        
        for left_event in left_events {
          let left_key = extract_key(left_event, key)
          let right_key = extract_key(right_event, key)
          
          if left_key == right_key {
            right_matched = true
            break
          }
        }
        
        if !right_matched && join_type == FullOuterJoin {
          results.push(StreamEvent {
            id: "null_" + right_event.id,
            timestamp: right_event.timestamp,
            data: "null|" + right_event.data,
            metadata: right_event.metadata
          })
        }
      }
    }
    
    results
  }
  
  // Extract key from event
  let extract_key = fn(event : StreamEvent, key : String) -> String {
    for (k, v) in event.metadata {
      if k == key {
        return v
      }
    }
    
    // Try to extract from data if not in metadata
    let parts = event.data.split(":")
    if parts.length() > 1 && parts[0] == key {
      return parts[1]
    }
    
    event.id
  }
  
  // Create test events
  let left_events = [
    create_event("1", 1000, "user:1", [("user_id", "1")]),
    create_event("2", 1100, "user:2", [("user_id", "2")]),
    create_event("3", 1200, "user:3", [("user_id", "3")]),
    create_event("4", 1300, "user:4", [("user_id", "4")])
  ]
  
  let right_events = [
    create_event("5", 1050, "order:1", [("user_id", "1")]),
    create_event("6", 1150, "order:3", [("user_id", "3")]),
    create_event("7", 1250, "order:5", [("user_id", "5")]),
    create_event("8", 1350, "order:4", [("user_id", "4")])
  ]
  
  // Test inner join
  let inner_joined = join_streams(left_events, right_events, InnerJoin, On("user_id"))
  assert_eq(inner_joined.length(), 3)
  
  // Sort by ID for consistent testing
  let sorted_inner = inner_joined.sort(|a, b| a.id - b.id)
  assert_eq(sorted_inner[0].data, "user:1|order:1")
  assert_eq(sorted_inner[1].data, "user:3|order:3")
  assert_eq(sorted_inner[2].data, "user:4|order:4")
  
  // Test left join
  let left_joined = join_streams(left_events, right_events, LeftJoin, On("user_id"))
  assert_eq(left_joined.length(), 4)
  
  // Test right join
  let right_joined = join_streams(left_events, right_events, RightJoin, On("user_id"))
  assert_eq(right_joined.length(), 4)
  
  // Test full outer join
  let outer_joined = join_streams(left_events, right_events, FullOuterJoin, On("user_id"))
  assert_eq(outer_joined.length(), 6)
  
  // Test time window join
  let time_events1 = [
    create_event("1", 1000, "event1", [("key", "A")]),
    create_event("2", 2000, "event2", [("key", "B")])
  ]
  
  let time_events2 = [
    create_event("3", 1050, "event3", [("key", "A")]),
    create_event("4", 2100, "event4", [("key", "B")]),
    create_event("5", 3000, "event5", [("key", "A")])
  ]
  
  let time_joined = join_streams(time_events1, time_events2, InnerJoin, Within("key", 200))
  assert_eq(time_joined.length(), 1)
  assert_eq(time_joined[0].data, "event1|event3")
}

// Test 5: Stream State Management
test "stream state management" {
  // Stream state
  type StreamState {
    key : String
    value : String
    last_updated : Int
    ttl : Option[Int>  // Time to live in ms
  }
  
  // State store
  type StateStore {
    states : Array[StreamState]
  }
  
  let create_state_store = fn() -> StateStore {
    StateStore { states: [] }
  }
  
  let get_state = fn(store : StateStore, key : String, current_time : Int) -> Option[String] {
    for state in store.states {
      if state.key == key {
        // Check TTL
        match state.ttl {
          Some(ttl) => {
            if current_time - state.last_updated <= ttl {
              return Some(state.value)
            }
          }
          None => return Some(state.value)
        }
      }
    }
    
    None
  }
  
  let put_state = fn(store : StateStore, key : String, value : String, current_time : Int, ttl : Option<Int>) -> StateStore {
    let mut new_states = []
    let mut updated = false
    
    // Update existing state
    for state in store.states {
      if state.key == key {
        new_states.push(StreamState {
          key: key,
          value: value,
          last_updated: current_time,
          ttl: ttl
        })
        updated = true
      } else {
        new_states.push(state)
      }
    }
    
    // Add new state if not updated
    if !updated {
      new_states.push(StreamState {
        key: key,
        value: value,
        last_updated: current_time,
        ttl: ttl
      })
    }
    
    StateStore { states: new_states }
  }
  
  let remove_state = fn(store : StateStore, key : String) -> StateStore {
    let mut new_states = []
    
    for state in store.states {
      if state.key != key {
        new_states.push(state)
      }
    }
    
    StateStore { states: new_states }
  }
  
  let cleanup_expired = fn(store : StateStore, current_time : Int) -> StateStore {
    let mut new_states = []
    
    for state in store.states {
      let keep = match state.ttl {
        Some(ttl) => current_time - state.last_updated <= ttl
        None => true
      }
      
      if keep {
        new_states.push(state)
      }
    }
    
    StateStore { states: new_states }
  }
  
  // Test state operations
  let store = create_state_store()
  
  // Put state
  let store1 = put_state(store, "key1", "value1", 1000, None)
  assert_eq(store1.states.length(), 1)
  assert_eq(store1.states[0].key, "key1")
  assert_eq(store1.states[0].value, "value1")
  
  // Get state
  let value1 = get_state(store1, "key1", 1000)
  assert_eq(value1, Some("value1"))
  
  // Update state
  let store2 = put_state(store1, "key1", "value1_updated", 1100, None)
  let value1_updated = get_state(store2, "key1", 1100)
  assert_eq(value1_updated, Some("value1_updated"))
  
  // Add more states
  let store3 = store2
    |> put_state("key2", "value2", 1200, Some(500)) // With TTL
    |> put_state("key3", "value3", 1300, None)
  
  assert_eq(store3.states.length(), 3)
  
  // Test TTL
  let value2_valid = get_state(store3, "key2", 1300)
  assert_eq(value2_valid, Some("value2")) // Within TTL
  
  let value2_expired = get_state(store3, "key2", 1800)
  assert_eq(value2_expired, None) // Expired
  
  // Test cleanup
  let store4 = cleanup_expired(store3, 1800)
  assert_eq(store4.states.length(), 2) // key2 should be removed
  
  // Test remove
  let store5 = remove_state(store4, "key1")
  assert_eq(store5.states.length(), 1)
  assert_eq(store5.states[0].key, "key3")
  
  // Test get non-existent
  let non_existent = get_state(store5, "non_existent", 2000)
  assert_eq(non_existent, None)
}

// Test 6: Stream Analytics
test "stream analytics" {
  // Analytics metric types
  type MetricType {
    Counter
    Gauge
    Histogram
    Timer
  }
  
  // Analytics metric
  type AnalyticsMetric {
    name : String
    metric_type : MetricType
    value : Float
    timestamp : Int
    tags : Array[(String, String)]
  }
  
  // Pattern detection result
  type PatternResult {
    pattern_name : String
    confidence : Float
    events : Array[StreamEvent]
  }
  
  // Anomaly detection result
  type AnomalyResult {
    anomaly_type : String
    severity : Float
    description : String
    events : Array[StreamEvent]
  }
  
  // Count events
  let count_events = fn(events : Array[StreamEvent], window_start : Int, window_end : Int) -> AnalyticsMetric {
    let count = events.length()
    
    AnalyticsMetric {
      name: "event_count",
      metric_type: Counter,
      value: count.to_float(),
      timestamp: window_end,
      tags: [("window_start", window_start.to_string()), ("window_end", window_end.to_string())]
    }
  }
  
  // Calculate rate
  let calculate_rate = fn(events : Array[StreamEvent], window_start : Int, window_end : Int) -> AnalyticsMetric {
    let duration = (window_end - window_start).to_float() / 1000.0 // Convert to seconds
    let count = events.length()
    let rate = if duration > 0.0 { count.to_float() / duration } else { 0.0 }
    
    AnalyticsMetric {
      name: "event_rate",
      metric_type: Gauge,
      value: rate,
      timestamp: window_end,
      tags: [("window_start", window_start.to_string()), ("window_end", window_end.to_string())]
    }
  }
  
  // Detect pattern
  let detect_pattern = fn(events : Array[StreamEvent], pattern_name : String, detector : Array[StreamEvent] -> Float) -> Option[PatternResult] {
    let confidence = detector(events)
    
    if confidence > 0.7 {
      Some(PatternResult {
        pattern_name: pattern_name,
        confidence: confidence,
        events: events
      })
    } else {
      None
    }
  }
  
  // Detect anomaly
  let detect_anomaly = fn(events : Array[StreamEvent], detector : Array[StreamEvent] -> Option[AnomalyResult]) -> Option[AnomalyResult] {
    detector(events)
  }
  
  // Create test events
  let events = [
    create_event("1", 1000, "10", []),
    create_event("2", 1100, "15", []),
    create_event("3", 1200, "20", []),
    create_event("4", 1300, "25", []),
    create_event("5", 1400, "30", [])
  ]
  
  // Test counting
  let count_metric = count_events(events, 1000, 2000)
  assert_eq(count_metric.name, "event_count")
  assert_eq(count_metric.metric_type, Counter)
  assert_eq(count_metric.value, 5.0)
  
  // Test rate calculation
  let rate_metric = calculate_rate(events, 1000, 2000)
  assert_eq(rate_metric.name, "event_rate")
  assert_eq(rate_metric.metric_type, Gauge)
  assert_eq(rate_metric.value, 5.0 / 1.0) // 5 events in 1 second
  
  // Test pattern detection (increasing values)
  let increasing_pattern = detect_pattern(events, "increasing_values", |events| {
    if events.length() < 2 {
      return 0.0
    }
    
    let mut increasing = true
    let mut differences = []
    
    for i = 1; i < events.length(); i = i + 1 {
      let prev = events[i-1].data.to_int().unwrap_or(0)
      let curr = events[i].data.to_int().unwrap_or(0)
      
      if curr <= prev {
        increasing = false
      }
      
      differences.push(curr - prev)
    }
    
    if increasing {
      // Check if differences are consistent
      let avg_diff = differences.reduce(|acc, val| acc + val, 0) / differences.length()
      let variance = differences.reduce(|acc, val| acc + (val - avg_diff) * (val - avg_diff), 0) / differences.length()
      
      // Higher confidence for consistent differences
      if variance < 2.0 {
        0.9
      } else {
        0.7
      }
    } else {
      0.0
    }
  })
  
  match increasing_pattern {
    Some(pattern) => {
      assert_eq(pattern.pattern_name, "increasing_values")
      assert_eq(pattern.confidence, 0.9)
    }
    None => assert_true(false)
  }
  
  // Test anomaly detection (sudden spike)
  let normal_events = [
    create_event("1", 1000, "10", []),
    create_event("2", 1100, "12", []),
    create_event("3", 1200, "11", []),
    create_event("4", 1300, "13", []),
    create_event("5", 1400, "100", []) // Spike
  ]
  
  let spike_anomaly = detect_anomaly(normal_events, |events| {
    if events.length() < 3 {
      return None
    }
    
    let values = events.map(|e| e.data.to_int().unwrap_or(0))
    let avg = values.reduce(|acc, val| acc + val, 0) / values.length()
    let variance = values.reduce(|acc, val| acc + (val - avg) * (val - avg), 0) / values.length()
    let std_dev = variance.sqrt()
    
    // Check if last value is an outlier
    let last_value = values[values.length() - 1]
    let z_score = if std_dev > 0 { (last_value - avg).to_float() / std_dev.to_float() } else { 0.0 }
    
    if z_score > 3.0 {
      Some(AnomalyResult {
        anomaly_type: "spike",
        severity: (z_score / 10.0).min(1.0),
        description: "Value spike detected: " + last_value.to_string(),
        events: events
      })
    } else {
      None
    }
  })
  
  match spike_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.anomaly_type, "spike")
      assert_true(anomaly.severity > 0.0)
      assert_eq(anomaly.description, "Value spike detected: 100")
    }
    None => assert_true(false)
  }
}