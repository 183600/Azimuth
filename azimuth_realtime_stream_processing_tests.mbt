// Azimuth Realtime Stream Processing Test Suite
// 实时流处理测试用例

// 测试1: 流数据结构
test "流数据结构测试" {
  // 流事件结构
  struct StreamEvent[T] {
    id : String
    timestamp : Int64
    data : T
    metadata : Array[(String, String)]
  }
  
  // 流窗口结构
  struct StreamWindow[T] {
    start_time : Int64
    end_time : Int64
    events : Array[StreamEvent[T]]
    size : Int
  }
  
  // 流结构
  struct Stream[T] {
    name : String
    events : Array[StreamEvent[T]]
    window_size : Option[Int64]  // 窗口大小（纳秒）
    max_size : Option[Int]       // 最大事件数量
  }
  
  // 创建流事件
  fn StreamEvent::new[T](id : String, timestamp : Int64, data : T) -> StreamEvent[T] {
    StreamEvent::{ id, timestamp, data, metadata: [] }
  }
  
  fn StreamEvent::with_metadata[T](id : String, timestamp : Int64, data : T, metadata : Array[(String, String)]) -> StreamEvent[T] {
    StreamEvent::{ id, timestamp, data, metadata }
  }
  
  // 创建流
  fn Stream::new[T](name : String) -> Stream[T] {
    Stream::{ name, events: [], window_size: None, max_size: None }
  }
  
  fn Stream::with_window[T](name : String, window_size : Int64) -> Stream[T] {
    Stream::{ name, events: [], window_size: Some(window_size), max_size: None }
  }
  
  fn Stream::with_max_size[T](name : String, max_size : Int) -> Stream[T] {
    Stream::{ name, events: [], window_size: None, max_size: Some(max_size) }
  }
  
  // 添加事件到流
  fn Stream::add_event[T](stream : Stream[T], event : StreamEvent[T]) -> Stream[T] {
    let mut new_events = stream.events.push(event)
    
    // 应用最大大小限制
    match stream.max_size {
      Some(max) => {
        if new_events.length() > max {
          new_events = new_events.slice(new_events.length() - max, new_events.length())
        }
      }
      None => ()
    }
    
    Stream::{ 
      name: stream.name, 
      events: new_events, 
      window_size: stream.window_size, 
      max_size: stream.max_size 
    }
  }
  
  // 获取流窗口
  fn Stream::get_window[T](stream : Stream[T], end_time : Int64) -> StreamWindow[T] {
    match stream.window_size {
      Some(window_size) => {
        let start_time = end_time - window_size
        let window_events = stream.events.filter(fn(event) { 
          event.timestamp >= start_time && event.timestamp <= end_time 
        })
        
        StreamWindow::{ 
          start_time, 
          end_time, 
          events: window_events, 
          size: window_events.length() 
        }
      }
      None => {
        StreamWindow::{ 
          start_time: 0L, 
          end_time, 
          events: stream.events, 
          size: stream.events.length() 
        }
      }
    }
  }
  
  // 测试流事件创建
  let event1 = StreamEvent::new("event-1", 1640995200000000000L, "test-data-1")
  let event2 = StreamEvent::with_metadata("event-2", 1640995200000001000L, "test-data-2", [("source", "sensor-a")])
  
  assert_eq(event1.id, "event-1")
  assert_eq(event1.data, "test-data-1")
  assert_eq(event1.metadata.length(), 0)
  
  assert_eq(event2.id, "event-2")
  assert_eq(event2.data, "test-data-2")
  assert_eq(event2.metadata.length(), 1)
  assert_eq(event2.metadata[0], ("source", "sensor-a"))
  
  // 测试流创建
  let stream1 = Stream::new[String]("test-stream")
  assert_eq(stream1.name, "test-stream")
  assert_eq(stream1.events.length(), 0)
  assert_eq(stream1.window_size, None)
  assert_eq(stream1.max_size, None)
  
  let stream2 = Stream::with_window[String]("windowed-stream", 60000000000L)  // 1分钟窗口
  assert_eq(stream2.name, "windowed-stream")
  assert_eq(stream2.window_size, Some(60000000000L))
  
  let stream3 = Stream::with_max_size[String]("bounded-stream", 100)
  assert_eq(stream3.name, "bounded-stream")
  assert_eq(stream3.max_size, Some(100))
  
  // 测试添加事件
  let stream1_1 = Stream::add_event(stream1, event1)
  let stream1_2 = Stream::add_event(stream1_1, event2)
  
  assert_eq(stream1_2.events.length(), 2)
  assert_eq(stream1_2.events[0].id, "event-1")
  assert_eq(stream1_2.events[1].id, "event-2")
  
  // 测试流窗口
  let window = Stream::get_window(stream1_2, 1640995200000001000L)
  assert_eq(window.start_time, 1640995200000001000L - 60000000000L)
  assert_eq(window.end_time, 1640995200000001000L)
  assert_eq(window.size, 2)
}

// 测试2: 流聚合操作
test "流聚合操作测试" {
  // 复用上一测试的结构定义
  
  // 计数聚合
  fn count_aggregate[T](window : StreamWindow[T]) -> Int {
    window.size
  }
  
  // 求和聚合（针对数值数据）
  fn sum_aggregate(window : StreamWindow[Double]) -> Double {
    let mut sum = 0.0
    for event in window.events {
      sum = sum + event.data
    }
    sum
  }
  
  // 平均值聚合
  fn avg_aggregate(window : StreamWindow[Double]) -> Double {
    if window.size == 0 {
      0.0
    } else {
      sum_aggregate(window) / window.size.to_double()
    }
  }
  
  // 最小值聚合
  fn min_aggregate(window : StreamWindow[Double]) -> Double {
    match window.events {
      [] => 0.0
      [head, ..tail] => {
        let mut min = head.data
        for event in tail {
          if event.data < min {
            min = event.data
          }
        }
        min
      }
    }
  }
  
  // 最大值聚合
  fn max_aggregate(window : StreamWindow[Double]) -> Double {
    match window.events {
      [] => 0.0
      [head, ..tail] => {
        let mut max = head.data
        for event in tail {
          if event.data > max {
            max = event.data
          }
        }
        max
      }
    }
  }
  
  // 分组聚合
  fn group_by[T, K](window : StreamWindow[T], key_fn : (T) -> K) -> Array[(K, Array[StreamEvent[T]])] {
    let mut groups = []
    
    for event in window.events {
      let key = key_fn(event.data)
      
      // 查找或创建分组
      let mut found_group = false
      let mut updated_groups = []
      
      for (group_key, group_events) in groups {
        if group_key == key {
          updated_groups = updated_groups.push((group_key, group_events.push(event)))
          found_group = true
        } else {
          updated_groups = updated_groups.push((group_key, group_events))
        }
      }
      
      if !found_group {
        updated_groups = updated_groups.push((key, [event]))
      }
      
      groups = updated_groups
    }
    
    groups
  }
  
  // 创建测试流（数值数据）
  let numeric_stream = Stream::with_window[Double]("numeric-stream", 60000000000L)
  
  let base_time = 1640995200000000000L
  let mut test_stream = numeric_stream
  
  // 添加测试事件
  for i in 0..100 {
    let timestamp = base_time + (i * 1000000000L)  // 每秒一个事件
    let value = 10.0 + (i % 20).to_double()  // 10-29之间的值
    let event = StreamEvent::new("numeric-" + i.to_string(), timestamp, value)
    test_stream = Stream::add_event(test_stream, event)
  }
  
  // 获取窗口
  let window = Stream::get_window(test_stream, base_time + 99000000000L)  // 99秒时的窗口
  
  // 测试计数聚合
  let count = count_aggregate(window)
  assert_eq(count, 100)  // 所有事件都在1分钟窗口内
  
  // 测试求和聚合
  let sum = sum_aggregate(window)
  assert_true(sum > 1900.0 && sum < 2100.0)  // 100个事件，平均值约20
  
  // 测试平均值聚合
  let avg = avg_aggregate(window)
  assert_true(avg > 19.0 && avg < 21.0)
  
  // 测试最小值聚合
  let min = min_aggregate(window)
  assert_eq(min, 10.0)
  
  // 测试最大值聚合
  let max = max_aggregate(window)
  assert_eq(max, 29.0)
  
  // 创建分类数据流
  let categorical_stream = Stream::with_window[String]("categorical-stream", 60000000000L)
  let mut cat_test_stream = categorical_stream
  
  // 添加分类事件
  let categories = ["error", "warning", "info", "debug"]
  for i in 0..50 {
    let timestamp = base_time + (i * 2000000000L)  // 每2秒一个事件
    let category = categories[i % categories.length()]
    let event = StreamEvent::new("log-" + i.to_string(), timestamp, category)
    cat_test_stream = Stream::add_event(cat_test_stream, event)
  }
  
  // 获取分类窗口
  let cat_window = Stream::get_window(cat_test_stream, base_time + 98000000000L)
  
  // 测试分组聚合
  let groups = group_by(cat_window, fn(data) { data })
  
  assert_eq(groups.length(), 4)  // 应该有4个分组
  
  // 验证每个分组的事件数量
  for (category, events) in groups {
    assert_eq(events.length(), 13)  // 50个事件分成4组，每组12或13个
    assert_true(categories.contains(category))
  }
}

// 测试3: 流过滤和转换
test "流过滤和转换测试" {
  // 复用结构定义
  
  // 流过滤
  fn filter_stream[T](stream : Stream[T], predicate : (StreamEvent[T]) -> Bool) -> Stream[T] {
    let filtered_events = stream.events.filter(predicate)
    Stream::{ 
      name: stream.name + ".filtered", 
      events: filtered_events, 
      window_size: stream.window_size, 
      max_size: stream.max_size 
    }
  }
  
  // 流映射
  fn map_stream[T, U](stream : Stream[T], mapper : (StreamEvent[T]) -> StreamEvent[U]) -> Stream[U] {
    let mapped_events = stream.events.map(mapper)
    Stream::{ 
      name: stream.name + ".mapped", 
      events: mapped_events, 
      window_size: stream.window_size, 
      max_size: stream.max_size 
    }
  }
  
  // 流扁平化
  fn flat_map_stream[T, U](stream : Stream[T], mapper : (StreamEvent[T]) -> Array[StreamEvent[U]]) -> Stream[U] {
    let mut flat_events = []
    
    for event in stream.events {
      let mapped = mapper(event)
      for mapped_event in mapped {
        flat_events = flat_events.push(mapped_event)
      }
    }
    
    Stream::{ 
      name: stream.name + ".flat_mapped", 
      events: flat_events, 
      window_size: stream.window_size, 
      max_size: stream.max_size 
    }
  }
  
  // 流去重
  fn distinct_stream[T](stream : Stream[T], key_fn : (StreamEvent[T]) -> String) -> Stream[T] {
    let mut seen_keys = []
    let mut distinct_events = []
    
    for event in stream.events {
      let key = key_fn(event)
      
      if !seen_keys.contains(key) {
        seen_keys = seen_keys.push(key)
        distinct_events = distinct_events.push(event)
      }
    }
    
    Stream::{ 
      name: stream.name + ".distinct", 
      events: distinct_events, 
      window_size: stream.window_size, 
      max_size: stream.max_size 
    }
  }
  
  // 流合并
  fn merge_streams[T](stream1 : Stream[T], stream2 : Stream[T]) -> Stream[T] {
    let merged_events = stream1.events.concat(stream2.events)
    
    // 按时间戳排序
    let sorted_events = merged_events.sort_by(fn(a, b) { 
      if a.timestamp < b.timestamp { -1 } 
      else if a.timestamp > b.timestamp { 1 } 
      else { 0 } 
    })
    
    Stream::{ 
      name: stream1.name + "+" + stream2.name, 
      events: sorted_events, 
      window_size: stream1.window_size, 
      max_size: stream1.max_size 
    }
  }
  
  // 创建测试流
  let base_time = 1640995200000000000L
  let test_stream = Stream::new[Int]("test-stream")
  
  let mut stream = test_stream
  for i in 0..20 {
    let timestamp = base_time + (i * 5000000000L)  // 每5秒一个事件
    let event = StreamEvent::new("event-" + i.to_string(), timestamp, i)
    stream = Stream::add_event(stream, event)
  }
  
  // 测试流过滤（只保留偶数）
  let filtered_stream = filter_stream(stream, fn(event) { event.data % 2 == 0 })
  assert_eq(filtered_stream.name, "test-stream.filtered")
  assert_eq(filtered_stream.events.length(), 10)  // 20个事件中应该有10个偶数
  
  for event in filtered_stream.events {
    assert_eq(event.data % 2, 0)
  }
  
  // 测试流映射（将数值转换为字符串）
  let mapped_stream = map_stream(stream, fn(event) { 
    StreamEvent::new(event.id, event.timestamp, "value-" + event.data.to_string()) 
  })
  assert_eq(mapped_stream.name, "test-stream.mapped")
  assert_eq(mapped_stream.events.length(), 20)
  
  for event in mapped_stream.events {
    assert_true(event.data.contains("value-"))
  }
  
  // 测试流扁平化（每个事件生成多个事件）
  let flat_mapped_stream = flat_map_stream(stream, fn(event) { 
    [
      StreamEvent::new(event.id + "-a", event.timestamp, event.data * 2),
      StreamEvent::new(event.id + "-b", event.timestamp, event.data * 3)
    ]
  })
  assert_eq(flat_mapped_stream.name, "test-stream.flat_mapped")
  assert_eq(flat_mapped_stream.events.length(), 40)  // 20个事件每个生成2个
  
  // 测试流去重
  let stream_with_duplicates = Stream::new[Int]("duplicate-stream")
  let mut dup_stream = stream_with_duplicates
  
  // 添加重复事件
  for i in 0..10 {
    let timestamp = base_time + (i * 10000000000L)
    let event1 = StreamEvent::new("event-" + (i % 5).to_string(), timestamp, i)
    let event2 = StreamEvent::new("event-" + (i % 5).to_string(), timestamp + 1000000000L, i + 10)
    dup_stream = Stream::add_event(dup_stream, event1)
    dup_stream = Stream::add_event(dup_stream, event2)
  }
  
  let distinct_stream = distinct_stream(dup_stream, fn(event) { event.id })
  assert_eq(distinct_stream.name, "duplicate-stream.distinct")
  assert_eq(distinct_stream.events.length(), 5)  // 应该只有5个不同的ID
  
  // 测试流合并
  let stream1 = Stream::new[Int]("stream1")
  let stream2 = Stream::new[Int]("stream2")
  
  let mut s1 = stream1
  let mut s2 = stream2
  
  for i in 0..5 {
    let timestamp1 = base_time + (i * 10000000000L)
    let timestamp2 = base_time + (i * 10000000000L) + 5000000000L  // 交错时间
    
    let event1 = StreamEvent::new("s1-" + i.to_string(), timestamp1, i * 10)
    let event2 = StreamEvent::new("s2-" + i.to_string(), timestamp2, i * 100)
    
    s1 = Stream::add_event(s1, event1)
    s2 = Stream::add_event(s2, event2)
  }
  
  let merged_stream = merge_streams(s1, s2)
  assert_eq(merged_stream.name, "stream1+stream2")
  assert_eq(merged_stream.events.length(), 10)
  
  // 验证时间戳排序
  for i in 1..merged_stream.events.length() {
    assert_true(merged_stream.events[i].timestamp >= merged_stream.events[i-1].timestamp)
  }
}

// 测试4: 流窗口操作
test "流窗口操作测试" {
  // 复用结构定义
  
  // 滑动窗口
  fn sliding_window[T](stream : Stream[T], window_size : Int64, slide : Int64) -> Array[StreamWindow[T]] {
    match stream.events {
      [] => []
      [head, ..] => {
        let mut windows = []
        let mut start_time = head.timestamp
        let end_time = start_time + window_size
        
        while start_time <= stream.events[stream.events.length() - 1].timestamp {
          let window_events = stream.events.filter(fn(event) { 
            event.timestamp >= start_time && event.timestamp < end_time 
          })
          
          windows = windows.push(StreamWindow::{ 
            start_time, 
            end_time, 
            events: window_events, 
            size: window_events.length() 
          })
          
          start_time = start_time + slide
          end_time = end_time + slide
        }
        
        windows
      }
    }
  }
  
  // 会话窗口（基于超时）
  fn session_window[T](stream : Stream[T], timeout : Int64) -> Array[StreamWindow[T]] {
    match stream.events {
      [] => []
      [head, ..] => {
        let mut windows = []
        let mut session_events = [head]
        let mut last_event_time = head.timestamp
        
        for i in 1..stream.events.length() {
          let event = stream.events[i]
          
          if event.timestamp - last_event_time <= timeout {
            session_events = session_events.push(event)
          } else {
            // 结束当前会话，开始新会话
            windows = windows.push(StreamWindow::{ 
              start_time: session_events[0].timestamp, 
              end_time: session_events[session_events.length() - 1].timestamp, 
              events: session_events, 
              size: session_events.length() 
            })
            
            session_events = [event]
          }
          
          last_event_time = event.timestamp
        }
        
        // 添加最后一个会话
        if session_events.length() > 0 {
          windows = windows.push(StreamWindow::{ 
            start_time: session_events[0].timestamp, 
            end_time: session_events[session_events.length() - 1].timestamp, 
            events: session_events, 
            size: session_events.length() 
          })
        }
        
        windows
      }
    }
  }
  
  // 全局窗口
  fn global_window[T](stream : Stream[T]) -> StreamWindow[T] {
    match stream.events {
      [] => StreamWindow::{ start_time: 0L, end_time: 0L, events: [], size: 0 }
      [head, ..] => {
        StreamWindow::{ 
          start_time: head.timestamp, 
          end_time: stream.events[stream.events.length() - 1].timestamp, 
          events: stream.events, 
          size: stream.events.length() 
        }
      }
    }
  }
  
  // 创建测试流
  let base_time = 1640995200000000000L
  let test_stream = Stream::new[Int]("test-stream")
  
  let mut stream = test_stream
  
  // 添加事件（有些间隔较大，有些间隔较小）
  for i in 0..20 {
    let interval = if i % 5 == 0 { 30000000000L } else { 5000000000L }  // 每5个事件有一个30秒间隔，其他5秒间隔
    let timestamp = base_time + (i * interval)
    let event = StreamEvent::new("event-" + i.to_string(), timestamp, i)
    stream = Stream::add_event(stream, event)
  }
  
  // 测试滑动窗口
  let sliding_windows = sliding_window(stream, 20000000000L, 10000000000L)  // 20秒窗口，10秒滑动
  assert_true(sliding_windows.length() > 0)
  
  // 验证窗口大小和滑动
  for i in 1..sliding_windows.length() {
    let current_window = sliding_windows[i]
    let prev_window = sliding_windows[i-1]
    
    assert_eq(current_window.start_time, prev_window.start_time + 10000000000L)
    assert_eq(current_window.end_time, prev_window.end_time + 10000000000L)
  }
  
  // 测试会话窗口
  let session_windows = session_window(stream, 15000000000L)  // 15秒超时
  assert_true(session_windows.length() > 1)  // 应该有多个会话
  
  // 验证会话窗口的间隔
  for i in 1..session_windows.length() {
    let current_session = session_windows[i]
    let prev_session = session_windows[i-1]
    
    // 会话之间应该有超过15秒的间隔
    assert_true(current_session.start_time - prev_session.end_time > 15000000000L)
  }
  
  // 测试全局窗口
  let global_win = global_window(stream)
  assert_eq(global_win.size, 20)
  assert_eq(global_win.start_time, base_time)
  assert_eq(global_win.end_time, stream.events[stream.events.length() - 1].timestamp)
}

// 测试5: 流式连接操作
test "流式连接操作测试" {
  // 复用结构定义
  
  // 内连接
  fn join_streams[T, U, K](
    left_stream : Stream[T], 
    right_stream : Stream[U], 
    left_key_fn : (StreamEvent[T]) -> K, 
    right_key_fn : (StreamEvent[U]) -> K,
    window_size : Int64
  ) -> Array[(StreamEvent[T], StreamEvent[U])] {
    let mut joined_pairs = []
    
    for left_event in left_stream.events {
      for right_event in right_stream.events {
        // 检查时间窗口
        let time_diff = (left_event.timestamp - right_event.timestamp).abs()
        if time_diff <= window_size {
          // 检查键匹配
          let left_key = left_key_fn(left_event)
          let right_key = right_key_fn(right_event)
          
          if left_key == right_key {
            joined_pairs = joined_pairs.push((left_event, right_event))
          }
        }
      }
    }
    
    joined_pairs
  }
  
  // 左连接
  fn left_join_streams[T, U, K](
    left_stream : Stream[T], 
    right_stream : Stream[U], 
    left_key_fn : (StreamEvent[T]) -> K, 
    right_key_fn : (StreamEvent[U]) -> K,
    window_size : Int64
  ) -> Array[(StreamEvent[T], Option[StreamEvent[U]])] {
    let mut joined_pairs = []
    
    for left_event in left_stream.events {
      let mut matched = false
      
      for right_event in right_stream.events {
        let time_diff = (left_event.timestamp - right_event.timestamp).abs()
        if time_diff <= window_size {
          let left_key = left_key_fn(left_event)
          let right_key = right_key_fn(right_event)
          
          if left_key == right_key {
            joined_pairs = joined_pairs.push((left_event, Some(right_event)))
            matched = true
          }
        }
      }
      
      if !matched {
        joined_pairs = joined_pairs.push((left_event, None))
      }
    }
    
    joined_pairs
  }
  
  // 基于时间的连接
  fn temporal_join[T, U](
    left_stream : Stream[T], 
    right_stream : Stream[U],
    window_size : Int64
  ) -> Array[(StreamEvent[T], StreamEvent[U])] {
    let mut joined_pairs = []
    
    for left_event in left_stream.events {
      for right_event in right_stream.events {
        let time_diff = (left_event.timestamp - right_event.timestamp).abs()
        if time_diff <= window_size {
          joined_pairs = joined_pairs.push((left_event, right_event))
        }
      }
    }
    
    joined_pairs
  }
  
  // 创建测试流
  let base_time = 1640995200000000000L
  
  // 左流：用户点击事件
  let click_stream = Stream::new[String]("click-stream")
  let mut clicks = click_stream
  
  let click_events = [
    ("user-1", "product-a", base_time),
    ("user-2", "product-b", base_time + 5000000000L),
    ("user-1", "product-c", base_time + 10000000000L),
    ("user-3", "product-a", base_time + 15000000000L),
    ("user-2", "product-d", base_time + 20000000000L)
  ]
  
  for (user_id, product_id, timestamp) in click_events {
    let event_data = user_id + ":" + product_id
    let event = StreamEvent::with_metadata("click-" + user_id, timestamp, event_data, [("user", user_id), ("product", product_id)])
    clicks = Stream::add_event(clicks, event)
  }
  
  // 右流：购买事件
  let purchase_stream = Stream::new[String]("purchase-stream")
  let mut purchases = purchase_stream
  
  let purchase_events = [
    ("user-1", "product-a", base_time + 2000000000L),  // 在点击后2秒购买
    ("user-2", "product-b", base_time + 7000000000L),  // 在点击后2秒购买
    ("user-1", "product-c", base_time + 12000000000L), // 在点击后2秒购买
    ("user-4", "product-e", base_time + 18000000000L)  // 没有点击事件的购买
  ]
  
  for (user_id, product_id, timestamp) in purchase_events {
    let event_data = user_id + ":" + product_id
    let event = StreamEvent::with_metadata("purchase-" + user_id, timestamp, event_data, [("user", user_id), ("product", product_id)])
    purchases = Stream::add_event(purchases, event)
  }
  
  // 测试内连接（基于用户ID和产品ID）
  let inner_joined = join_streams(
    clicks, 
    purchases,
    fn(event) { event.data },  // 使用完整的用户:产品作为键
    fn(event) { event.data },
    5000000000L  // 5秒时间窗口
  )
  
  assert_eq(inner_joined.length(), 3)  // 应该有3个匹配的点击-购买对
  
  // 验证连接结果
  for (click_event, purchase_event) in inner_joined {
    assert_eq(click_event.data, purchase_event.data)  // 用户和产品应该匹配
    assert_true((purchase_event.timestamp - click_event.timestamp).abs() <= 5000000000L)  // 时间差应该在窗口内
  }
  
  // 测试左连接
  let left_joined = left_join_streams(
    clicks, 
    purchases,
    fn(event) { event.data },
    fn(event) { event.data },
    5000000000L
  )
  
  assert_eq(left_joined.length(), 5)  // 所有点击事件都应该包含
  
  let matched_count = left_joined.count(fn(pair) { match pair { (_, Some(_)) => true, (_, None) => false } })
  let unmatched_count = left_joined.count(fn(pair) { match pair { (_, Some(_)) => false, (_, None) => true } })
  
  assert_eq(matched_count, 3)  // 3个匹配的
  assert_eq(unmatched_count, 2)  // 2个未匹配的
  
  // 测试基于时间的连接
  let temporal_joined = temporal_join(clicks, purchases, 10000000000L)  // 10秒时间窗口
  assert_true(temporal_joined.length() >= 3)  // 至少有3个时间上匹配的对
  
  // 验证时间连接结果
  for (click_event, purchase_event) in temporal_joined {
    assert_true((purchase_event.timestamp - click_event.timestamp).abs() <= 10000000000L)
  }
}