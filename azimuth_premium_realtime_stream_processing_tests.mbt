// Azimuth Real-time Stream Processing Tests
// This file contains test cases for real-time stream processing

// Test 1: Stream Data Processing
test "实时流数据处理" {
  // 定义流事件
  type StreamEvent = {
    id: String,
    timestamp: Int,
    data: Map[String, String],
    metadata: Map[String, String]
  }
  
  // 定义流处理器
  type StreamProcessor = {
    name: String,
    process_fn: (StreamEvent) -> StreamEvent
  }
  
  // 定义流窗口
  enum WindowType {
    Tumbling(Int)  // 滚动窗口，参数为窗口大小（毫秒）
    Sliding(Int, Int)  // 滑动窗口，参数为窗口大小和滑动间隔（毫秒）
    Session(Int)  // 会话窗口，参数为超时时间（毫秒）
  }
  
  // 创建流事件
  let create_stream_event = fn(id: String, data: Map[String, String], metadata: Map[String, String]) {
    {
      id,
      timestamp: Time::now(),
      data,
      metadata
    }
  }
  
  // 过滤流事件
  let filter_stream = fn(events: Array[StreamEvent], predicate: (StreamEvent) -> Bool) {
    events.filter(predicate)
  }
  
  // 映射流事件
  let map_stream = fn(events: Array[StreamEvent], mapper: (StreamEvent) -> StreamEvent) {
    events.map(mapper)
  }
  
  // 扁平映射流事件
  let flat_map_stream = fn(events: Array[StreamEvent], mapper: (StreamEvent) -> Array[StreamEvent]) {
    let mut result = []
    
    for event in events {
      let mapped_events = mapper(event)
      result = result + mapped_events
    }
    
    result
  }
  
  // 聚合流事件
  let aggregate_stream = fn(events: Array[StreamEvent], aggregator: (Array[StreamEvent]) -> StreamEvent) {
    if events.length() > 0 {
      Some(aggregator(events))
    } else {
      None
    }
  }
  
  // 按时间窗口分组
  let window_by_time = fn(events: Array[StreamEvent], window_type: WindowType) {
    match window_type {
      WindowType::Tumbling(window_size) => {
        if events.length() == 0 {
          return []
        }
        
        // 按时间排序
        let sorted_events = events.sort(fn(a, b) {
          if a.timestamp < b.timestamp { -1 }
          else if a.timestamp > b.timestamp { 1 }
          else { 0 }
        })
        
        let mut windows = []
        let mut current_window = []
        let mut window_start = sorted_events[0].timestamp
        
        for event in sorted_events {
          if event.timestamp < window_start + window_size {
            current_window = current_window.push(event)
          } else {
            if current_window.length() > 0 {
              windows = windows.push(current_window)
            }
            current_window = [event]
            window_start = event.timestamp
          }
        }
        
        // 添加最后一个窗口
        if current_window.length() > 0 {
          windows = windows.push(current_window)
        }
        
        windows
      }
      WindowType::Sliding(window_size, slide_interval) => {
        // 简化实现，实际会更复杂
        if events.length() == 0 {
          return []
        }
        
        let sorted_events = events.sort(fn(a, b) {
          if a.timestamp < b.timestamp { -1 }
          else if a.timestamp > b.timestamp { 1 }
          else { 0 }
        })
        
        let mut windows = []
        let start_time = sorted_events[0].timestamp
        let end_time = sorted_events[sorted_events.length() - 1].timestamp
        
        let mut window_start = start_time
        while window_start <= end_time {
          let window_end = window_start + window_size
          let window_events = sorted_events.filter(fn(event) {
            event.timestamp >= window_start && event.timestamp < window_end
          })
          
          if window_events.length() > 0 {
            windows = windows.push(window_events)
          }
          
          window_start = window_start + slide_interval
        }
        
        windows
      }
      WindowType::Session(timeout) => {
        // 简化实现，实际会更复杂
        if events.length() == 0 {
          return []
        }
        
        let sorted_events = events.sort(fn(a, b) {
          if a.timestamp < b.timestamp { -1 }
          else if a.timestamp > b.timestamp { 1 }
          else { 0 }
        })
        
        let mut windows = []
        let mut current_session = []
        let mut last_event_time = 0
        
        for event in sorted_events {
          if current_session.length() == 0 {
            current_session = [event]
            last_event_time = event.timestamp
          } else if event.timestamp - last_event_time <= timeout {
            current_session = current_session.push(event)
            last_event_time = event.timestamp
          } else {
            windows = windows.push(current_session)
            current_session = [event]
            last_event_time = event.timestamp
          }
        }
        
        // 添加最后一个会话
        if current_session.length() > 0 {
          windows = windows.push(current_session)
        }
        
        windows
      }
    }
  }
  
  // 创建测试事件
  let base_time = Time::now()
  let test_events = [
    {
      id: "event1",
      timestamp: base_time,
      data: Map::from_array([("type", "click"), ("user", "user1")]),
      metadata: Map::from_array([("source", "web")])
    },
    {
      id: "event2",
      timestamp: base_time + 500,
      data: Map::from_array([("type", "view"), ("user", "user2")]),
      metadata: Map::from_array([("source", "mobile")])
    },
    {
      id: "event3",
      timestamp: base_time + 1200,
      data: Map::from_array([("type", "click"), ("user", "user1")]),
      metadata: Map::from_array([("source", "web")])
    },
    {
      id: "event4",
      timestamp: base_time + 1800,
      data: Map::from_array([("type", "purchase"), ("user", "user3")]),
      metadata: Map::from_array([("source", "web")])
    },
    {
      id: "event5",
      timestamp: base_time + 2500,
      data: Map::from_array([("type", "view"), ("user", "user2")]),
      metadata: Map::from_array([("source", "mobile")])
    }
  ]
  
  // 测试过滤
  let click_events = filter_stream(test_events, fn(event) {
    match Map::get(event.data, "type") {
      Some(event_type) => event_type == "click"
      None => false
    }
  })
  
  assert_eq(click_events.length(), 2)
  assert_eq(click_events[0].id, "event1")
  assert_eq(click_events[1].id, "event3")
  
  // 测试映射
  let enriched_events = map_stream(test_events, fn(event) {
    let enriched_data = Map::insert(event.data, "processed", "true")
    {
      ...event,
      data: enriched_data
    }
  })
  
  for event in enriched_events {
    match Map::get(event.data, "processed") {
      Some(value) => assert_eq(value, "true")
      None => assert_true(false)
    }
  }
  
  // 测试扁平映射
  let split_events = flat_map_stream(test_events, fn(event) {
    match Map::get(event.data, "type") {
      Some("click") => [
        {
          ...event,
          id: event.id + "_start",
          data: Map::insert(event.data, "phase", "start")
        },
        {
          ...event,
          id: event.id + "_end",
          data: Map::insert(event.data, "phase", "end")
        }
      ]
      _ => [event]
    }
  })
  
  assert_eq(split_events.length(), 7)  // 5个原始事件 + 2个点击事件的拆分
  
  // 测试时间窗口
  let tumbling_windows = window_by_time(test_events, WindowType::Tumbling(1000))
  assert_eq(tumbling_windows.length(), 2)  // 两个1秒窗口
  
  assert_eq(tumbling_windows[0].length(), 2)  // event1, event2
  assert_eq(tumbling_windows[1].length(), 3)  // event3, event4, event5
  
  // 测试滑动窗口
  let sliding_windows = window_by_time(test_events, WindowType::Sliding(1500, 500))
  assert_true(sliding_windows.length() >= 3)  // 至少3个窗口
  
  // 测试会话窗口
  let session_windows = window_by_time(test_events, WindowType::Session(1000))
  assert_eq(session_windows.length(), 1)  // 所有事件在一个会话中（间隔不超过1秒）
}

// Test 2: Stream Aggregation and Analytics
test "流聚合和分析" {
  // 定义流事件
  type StreamEvent = {
    id: String,
    timestamp: Int,
    data: Map[String, String],
    metadata: Map[String, String]
  }
  
  // 定义聚合结果
  type AggregationResult = {
    window_start: Int,
    window_end: Int,
    count: Int,
    metrics: Map[String, Float],
    dimensions: Map[String, String]
  }
  
  // 计数聚合
  let count_aggregator = fn(events: Array[StreamEvent]) {
    let count = events.length()
    let window_start = if events.length() > 0 { events[0].timestamp } else { 0 }
    let window_end = if events.length() > 0 { events[events.length() - 1].timestamp } else { 0 }
    
    {
      window_start,
      window_end,
      count,
      metrics: Map::from_array([("count", count as Float)]),
      dimensions: Map::empty()
    }
  }
  
  // 按维度分组聚合
  let group_by_aggregator = fn(events: Array[StreamEvent], dimension_key: String) {
    let mut groups = Map::empty()
    
    for event in events {
      let dimension_value = match Map::get(event.data, dimension_key) {
        Some(value) => value
        None => "unknown"
      }
      
      let group_events = match Map::get(groups, dimension_value) {
        Some(existing_events) => existing_events
        None => []
      }
      
      let updated_events = group_events.push(event)
      let _ = Map::insert(groups, dimension_value, updated_events)
    }
    
    // 对每个组进行聚合
    let mut results = []
    for (dimension_value, group_events) in groups {
      let window_start = if group_events.length() > 0 { group_events[0].timestamp } else { 0 }
      let window_end = if group_events.length() > 0 { group_events[group_events.length() - 1].timestamp } else { 0 }
      
      let result = {
        window_start,
        window_end,
        count: group_events.length(),
        metrics: Map::from_array([
          ("count", group_events.length() as Float)
        ]),
        dimensions: Map::from_array([(dimension_key, dimension_value)])
      }
      
      results = results.push(result)
    }
    
    results
  }
  
  // 数值聚合
  let numeric_aggregator = fn(events: Array[StreamEvent], value_key: String) {
    let mut values = []
    
    for event in events {
      match Map::get(event.data, value_key) {
        Some(value_str) => {
          match value_str.parse_float() {
            Some(value) => values = values.push(value)
            None => ()
          }
        }
        None => ()
      }
    }
    
    let window_start = if events.length() > 0 { events[0].timestamp } else { 0 }
    let window_end = if events.length() > 0 { events[events.length() - 1].timestamp } else { 0 }
    
    if values.length() > 0 {
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      let avg = sum / (values.length() as Float)
      let sorted_values = values.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
      let min = sorted_values[0]
      let max = sorted_values[values.length() - 1]
      
      {
        window_start,
        window_end,
        count: events.length(),
        metrics: Map::from_array([
          ("sum", sum),
          ("avg", avg),
          ("min", min),
          ("max", max),
          ("count", values.length() as Float)
        ]),
        dimensions: Map::empty()
      }
    } else {
      {
        window_start,
        window_end,
        count: events.length(),
        metrics: Map::empty(),
        dimensions: Map::empty()
      }
    }
  }
  
  // 复合聚合
  let composite_aggregator = fn(events: Array[StreamEvent]) {
    let mut click_count = 0
    let mut view_count = 0
    let mut purchase_count = 0
    let mut users = []
    let mut revenue = 0.0
    
    for event in events {
      match Map::get(event.data, "type") {
        Some("click") => click_count = click_count + 1
        Some("view") => view_count = view_count + 1
        Some("purchase") => {
          purchase_count = purchase_count + 1
          match Map::get(event.data, "amount") {
            Some(amount_str) => {
              match amount_str.parse_float() {
                Some(amount) => revenue = revenue + amount
                None => ()
              }
            }
            None => ()
          }
        }
        _ => ()
      }
      
      match Map::get(event.data, "user") {
        Some(user) => {
          if not users.contains(user) {
            users = users.push(user)
          }
        }
        None => ()
      }
    }
    
    let window_start = if events.length() > 0 { events[0].timestamp } else { 0 }
    let window_end = if events.length() > 0 { events[events.length() - 1].timestamp } else { 0 }
    
    {
      window_start,
      window_end,
      count: events.length(),
      metrics: Map::from_array([
        ("click_count", click_count as Float),
        ("view_count", view_count as Float),
        ("purchase_count", purchase_count as Float),
        ("unique_users", users.length() as Float),
        ("revenue", revenue),
        ("conversion_rate", if view_count > 0 { (purchase_count as Float) / (view_count as Float) } else { 0.0 })
      ]),
      dimensions: Map::empty()
    }
  }
  
  // 创建测试事件
  let test_events = [
    {
      id: "event1",
      timestamp: 1000,
      data: Map::from_array([("type", "view"), ("user", "user1")]),
      metadata: Map::empty()
    },
    {
      id: "event2",
      timestamp: 1100,
      data: Map::from_array([("type", "click"), ("user", "user1")]),
      metadata: Map::empty()
    },
    {
      id: "event3",
      timestamp: 1200,
      data: Map::from_array([("type", "view"), ("user", "user2")]),
      metadata: Map::empty()
    },
    {
      id: "event4",
      timestamp: 1300,
      data: Map::from_array([("type", "purchase"), ("user", "user1"), ("amount", "100.0")]),
      metadata: Map::empty()
    },
    {
      id: "event5",
      timestamp: 1400,
      data: Map::from_array([("type", "click"), ("user", "user3")]),
      metadata: Map::empty()
    }
  ]
  
  // 测试计数聚合
  let count_result = count_aggregator(test_events)
  assert_eq(count_result.count, 5)
  assert_eq(count_result.window_start, 1000)
  assert_eq(count_result.window_end, 1400)
  
  match Map::get(count_result.metrics, "count") {
    Some(count) => assert_eq(count, 5.0)
    None => assert_true(false)
  }
  
  // 测试按维度分组聚合
  let type_grouped = group_by_aggregator(test_events, "type")
  assert_eq(type_grouped.length(), 3)  // view, click, purchase
  
  let view_group = type_grouped.find(fn(result) {
    match Map::get(result.dimensions, "type") {
      Some(value) => value == "view"
      None => false
    }
  })
  
  match view_group {
    Some(group) => assert_eq(group.count, 2)
    None => assert_true(false)
  }
  
  let click_group = type_grouped.find(fn(result) {
    match Map::get(result.dimensions, "type") {
      Some(value) => value == "click"
      None => false
    }
  })
  
  match click_group {
    Some(group) => assert_eq(group.count, 2)
    None => assert_true(false)
  }
  
  let purchase_group = type_grouped.find(fn(result) {
    match Map::get(result.dimensions, "type") {
      Some(value) => value == "purchase"
      None => false
    }
  })
  
  match purchase_group {
    Some(group) => assert_eq(group.count, 1)
    None => assert_true(false)
  }
  
  // 测试数值聚合
  let events_with_values = [
    {
      id: "event1",
      timestamp: 1000,
      data: Map::from_array([("value", "10.5")]),
      metadata: Map::empty()
    },
    {
      id: "event2",
      timestamp: 1100,
      data: Map::from_array([("value", "15.2")]),
      metadata: Map::empty()
    },
    {
      id: "event3",
      timestamp: 1200,
      data: Map::from_array([("value", "8.7")]),
      metadata: Map::empty()
    }
  ]
  
  let numeric_result = numeric_aggregator(events_with_values, "value")
  assert_eq(numeric_result.count, 3)
  
  match Map::get(numeric_result.metrics, "sum") {
    Some(sum) => assert_eq(sum, 34.4)  // 10.5 + 15.2 + 8.7
    None => assert_true(false)
  }
  
  match Map::get(numeric_result.metrics, "avg") {
    Some(avg) => assert_eq(avg, 11.466666666666666)  // 34.4 / 3
    None => assert_true(false)
  }
  
  match Map::get(numeric_result.metrics, "min") {
    Some(min) => assert_eq(min, 8.7)
    None => assert_true(false)
  }
  
  match Map::get(numeric_result.metrics, "max") {
    Some(max) => assert_eq(max, 15.2)
    None => assert_true(false)
  }
  
  // 测试复合聚合
  let composite_result = composite_aggregator(test_events)
  assert_eq(composite_result.count, 5)
  
  match Map::get(composite_result.metrics, "click_count") {
    Some(count) => assert_eq(count, 2.0)
    None => assert_true(false)
  }
  
  match Map::get(composite_result.metrics, "view_count") {
    Some(count) => assert_eq(count, 2.0)
    None => assert_true(false)
  }
  
  match Map::get(composite_result.metrics, "purchase_count") {
    Some(count) => assert_eq(count, 1.0)
    None => assert_true(false)
  }
  
  match Map::get(composite_result.metrics, "unique_users") {
    Some(count) => assert_eq(count, 3.0)  // user1, user2, user3
    None => assert_true(false)
  }
  
  match Map::get(composite_result.metrics, "revenue") {
    Some(revenue) => assert_eq(revenue, 100.0)
    None => assert_true(false)
  }
  
  match Map::get(composite_result.metrics, "conversion_rate") {
    Some(rate) => assert_eq(rate, 0.5)  // 1 purchase / 2 views
    None => assert_true(false)
  }
}

// Test 3: Stream Pattern Detection
test "流模式检测" {
  // 定义流事件
  type StreamEvent = {
    id: String,
    timestamp: Int,
    data: Map[String, String],
    metadata: Map[String, String]
  }
  
  // 定义模式匹配结果
  type PatternMatch = {
    pattern_name: String,
    matched_events: Array[StreamEvent],
    confidence: Float,
    metadata: Map[String, String]
  }
  
  // 序列模式检测
  let detect_sequence_pattern = fn(events: Array[StreamEvent], pattern: Array[String], time_window: Int) {
    if events.length() < pattern.length() {
      return []
    }
    
    let mut matches = []
    
    for i in 0..(events.length() - pattern.length() + 1) {
      let mut is_match = true
      let matched_events = []
      
      for j in 0..pattern.length() {
        let event = events[i + j]
        match Map::get(event.data, "type") {
          Some(event_type) => {
            if event_type != pattern[j] {
              is_match = false
              break
            }
          }
          None => {
            is_match = false
            break
          }
        }
      }
      
      if is_match {
        let start_time = events[i].timestamp
        let end_time = events[i + pattern.length() - 1].timestamp
        
        if end_time - start_time <= time_window {
          let sequence_events = []
          for j in 0..pattern.length() {
            sequence_events = sequence_events.push(events[i + j])
          }
          
          matches = matches.push({
            pattern_name: "sequence",
            matched_events: sequence_events,
            confidence: 1.0,
            metadata: Map::from_array([
              ("sequence_length", pattern.length().to_string()),
              ("time_window", (end_time - start_time).to_string())
            ])
          })
        }
      }
    }
    
    matches
  }
  
  // 频率模式检测
  let detect_frequency_pattern = fn(events: Array[StreamEvent], threshold: Int, time_window: Int) {
    let mut matches = []
    
    // 按事件类型分组
    let mut type_groups = Map::empty()
    
    for event in events {
      match Map::get(event.data, "type") {
        Some(event_type) => {
          let type_events = match Map::get(type_groups, event_type) {
            Some(existing_events) => existing_events
            None => []
          }
          let updated_events = type_events.push(event)
          let _ = Map::insert(type_groups, event_type, updated_events)
        }
        None => ()
      }
    }
    
    // 检查每种类型是否超过阈值
    for (event_type, type_events) in type_groups {
      if type_events.length() >= threshold {
        let start_time = type_events[0].timestamp
        let end_time = type_events[type_events.length() - 1].timestamp
        
        if end_time - start_time <= time_window {
          matches = matches.push({
            pattern_name: "frequency",
            matched_events: type_events,
            confidence: (type_events.length() as Float) / (threshold as Float),
            metadata: Map::from_array([
              ("event_type", event_type),
              ("count", type_events.length().to_string()),
              ("threshold", threshold.to_string())
            ])
          })
        }
      }
    }
    
    matches
  }
  
  // 异常模式检测
  let detect_anomaly_pattern = fn(events: Array[StreamEvent], baseline_fn: (Array[StreamEvent]) -> Float) {
    let mut matches = []
    
    if events.length() < 2 {
      return matches
    }
    
    // 计算基线
    let baseline = baseline_fn(events)
    
    // 检查每个事件是否异常
    for event in events {
      match Map::get(event.data, "value") {
        Some(value_str) => {
          match value_str.parse_float() {
            Some(value) => {
              let deviation = (value - baseline).abs()
              let relative_deviation = deviation / baseline.abs()
              
              if relative_deviation > 2.0 {  // 超过基线200%
                matches = matches.push({
                  pattern_name: "anomaly",
                  matched_events: [event],
                  confidence: relative_deviation,
                  metadata: Map::from_array([
                    ("baseline", baseline.to_string()),
                    ("value", value.to_string()),
                    ("deviation", deviation.to_string())
                  ])
                })
              }
            }
            None => ()
          }
        }
        None => ()
      }
    }
    
    matches
  }
  
  // 创建测试事件
  let test_events = [
    {
      id: "event1",
      timestamp: 1000,
      data: Map::from_array([("type", "view"), ("user", "user1")]),
      metadata: Map::empty()
    },
    {
      id: "event2",
      timestamp: 1100,
      data: Map::from_array([("type", "click"), ("user", "user1")]),
      metadata: Map::empty()
    },
    {
      id: "event3",
      timestamp: 1200,
      data: Map::from_array([("type", "purchase"), ("user", "user1")]),
      metadata: Map::empty()
    },
    {
      id: "event4",
      timestamp: 2000,
      data: Map::from_array([("type", "view"), ("user", "user2")]),
      metadata: Map::empty()
    },
    {
      id: "event5",
      timestamp: 2100,
      data: Map::from_array([("type", "click"), ("user", "user2")]),
      metadata: Map::empty()
    },
    {
      id: "event6",
      timestamp: 2200,
      data: Map::from_array([("type", "purchase"), ("user", "user2")]),
      metadata: Map::empty()
    },
    {
      id: "event7",
      timestamp: 3000,
      data: Map::from_array([("type", "view"), ("user", "user3")]),
      metadata: Map::empty()
    },
    {
      id: "event8",
      timestamp: 3100,
      data: Map::from_array([("type", "view"), ("user", "user4")]),
      metadata: Map::empty()
    },
    {
      id: "event9",
      timestamp: 3200,
      data: Map::from_array([("type", "view"), ("user", "user5")]),
      metadata: Map::empty()
    }
  ]
  
  // 测试序列模式检测
  let sequence_pattern = ["view", "click", "purchase"]
  let sequence_matches = detect_sequence_pattern(test_events, sequence_pattern, 500)
  
  assert_eq(sequence_matches.length(), 2)  // 两个匹配序列
  
  let first_match = sequence_matches[0]
  assert_eq(first_match.matched_events.length(), 3)
  assert_eq(first_match.matched_events[0].id, "event1")
  assert_eq(first_match.matched_events[1].id, "event2")
  assert_eq(first_match.matched_events[2].id, "event3")
  
  let second_match = sequence_matches[1]
  assert_eq(second_match.matched_events.length(), 3)
  assert_eq(second_match.matched_events[0].id, "event4")
  assert_eq(second_match.matched_events[1].id, "event5")
  assert_eq(second_match.matched_events[2].id, "event6")
  
  // 测试频率模式检测
  let frequency_matches = detect_frequency_pattern(test_events, 3, 1000)
  
  assert_eq(frequency_matches.length(), 1)  // 一个类型超过阈值
  
  let view_frequency = frequency_matches[0]
  assert_eq(view_frequency.matched_events.length(), 4)  // 4个view事件
  assert_eq(view_frequency.confidence, 4.0 / 3.0)  // 4/3
  
  match Map::get(view_frequency.metadata, "event_type") {
    Some(event_type) => assert_eq(event_type, "view")
    None => assert_true(false)
  }
  
  // 测试异常模式检测
  let events_with_values = [
    {
      id: "event1",
      timestamp: 1000,
      data: Map::from_array([("value", "10.0")]),
      metadata: Map::empty()
    },
    {
      id: "event2",
      timestamp: 1100,
      data: Map::from_array([("value", "12.0")]),
      metadata: Map::empty()
    },
    {
      id: "event3",
      timestamp: 1200,
      data: Map::from_array([("value", "11.0")]),
      metadata: Map::empty()
    },
    {
      id: "event4",
      timestamp: 1300,
      data: Map::from_array([("value", "50.0")]),  // 异常值
      metadata: Map::empty()
    },
    {
      id: "event5",
      timestamp: 1400,
      data: Map::from_array([("value", "9.0")]),
      metadata: Map::empty()
    }
  ]
  
  let baseline_fn = fn(events: Array[StreamEvent]) {
    let mut values = []
    
    for event in events {
      match Map::get(event.data, "value") {
        Some(value_str) => {
          match value_str.parse_float() {
            Some(value) => values = values.push(value)
            None => ()
          }
        }
        None => ()
      }
    }
    
    if values.length() > 0 {
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      sum / (values.length() as Float)
    } else {
      0.0
    }
  }
  
  let anomaly_matches = detect_anomaly_pattern(events_with_values, baseline_fn)
  
  assert_eq(anomaly_matches.length(), 1)  // 一个异常值
  
  let anomaly_match = anomaly_matches[0]
  assert_eq(anomaly_match.matched_events.length(), 1)
  assert_eq(anomaly_match.matched_events[0].id, "event4")
  
  match Map::get(anomaly_match.metadata, "value") {
    Some(value) => assert_eq(value, "50.0")
    None => assert_true(false)
  }
}