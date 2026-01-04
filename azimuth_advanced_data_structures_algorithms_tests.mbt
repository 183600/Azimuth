// Azimuth Advanced Data Structures and Algorithms Tests
// This file contains comprehensive tests for advanced data structures and algorithms

// Test 1: Efficient Trace Tree Structure
test "efficient trace tree structure for hierarchical telemetry data" {
  // Define trace tree node
  type TraceTreeNode = {
    span_id: String,
    trace_id: String,
    parent_id: Option<String>,
    service_name: String,
    operation_name: String,
    start_time: Int,
    end_time: Int,
    children: Array<TraceTreeNode>,
    depth: Int,
    metadata: Array<(String, String)>
  }
  
  // Define trace tree
  type TraceTree = {
    trace_id: String,
    root_nodes: Array<TraceTreeNode>,
    total_spans: Int,
    max_depth: Int,
    total_duration: Int
  }
  
  // Create trace tree
  let create_trace_tree = fn(trace_id: String) {
    {
      trace_id: trace_id,
      root_nodes: [],
      total_spans: 0,
      max_depth: 0,
      total_duration: 0
    }
  }
  
  // Add span to tree
  let add_span_to_tree = fn(tree: TraceTree, span: TraceTreeNode) {
    let mut updated_root_nodes = tree.root_nodes
    let mut added = false
    let mut total_spans = tree.total_spans + 1
    let mut max_depth = tree.max_depth
    
    # Find parent and add as child
    if span.parent_id.length > 0 {
      let parent_id = span.parent_id.get
      let mut updated_nodes = []
      
      for node in updated_root_nodes {
        let updated_node = add_child_to_node(node, parent_id, span)
        updated_nodes = updated_nodes.push(updated_node)
        
        # Update max depth if this span increased depth
        if updated_node.depth > max_depth {
          max_depth = updated_node.depth
        }
      }
      
      updated_root_nodes = updated_nodes
      added = true
    } else {
      # Root span
      let root_span = { span | depth = 1 }
      updated_root_nodes = updated_root_nodes.push(root_span)
      added = true
      if 1 > max_depth {
        max_depth = 1
      }
    }
    
    # Calculate total duration
    let total_duration = calculate_tree_duration(updated_root_nodes)
    
    {
      trace_id: tree.trace_id,
      root_nodes: updated_root_nodes,
      total_spans: total_spans,
      max_depth: max_depth,
      total_duration: total_duration
    }
  }
  
  // Add child to node (recursive)
  let add_child_to_node = fn(node: TraceTreeNode, parent_id: String, child_span: TraceTreeNode) {
    if node.span_id == parent_id {
      # Add as child
      let child_with_depth = { child_span | depth = node.depth + 1 }
      {
        span_id: node.span_id,
        trace_id: node.trace_id,
        parent_id: node.parent_id,
        service_name: node.service_name,
        operation_name: node.operation_name,
        start_time: node.start_time,
        end_time: node.end_time,
        children: node.children.push(child_with_depth),
        depth: node.depth,
        metadata: node.metadata
      }
    } else {
      # Recursively check children
      let mut updated_children = []
      for child in node.children {
        updated_children = updated_children.push(add_child_to_node(child, parent_id, child_span))
      }
      
      {
        span_id: node.span_id,
        trace_id: node.trace_id,
        parent_id: node.parent_id,
        service_name: node.service_name,
        operation_name: node.operation_name,
        start_time: node.start_time,
        end_time: node.end_time,
        children: updated_children,
        depth: node.depth,
        metadata: node.metadata
      }
    }
  }
  
  // Calculate total duration of tree
  let calculate_tree_duration = fn(nodes: Array<TraceTreeNode>) {
    if nodes.length() == 0 {
      0
    } else {
      let root_start = nodes[0].start_time
      let mut max_end = root_start
      
      for node in nodes {
        if node.end_time > max_end {
          max_end = node.end_time
        }
        
        # Check children recursively
        let child_max = find_max_end_time(node.children, node.end_time)
        if child_max > max_end {
          max_end = child_max
        }
      }
      
      max_end - root_start
    }
  }
  
  // Find max end time in subtree
  let find_max_end_time = fn(nodes: Array<TraceTreeNode>, current_max: Int) {
    let mut max = current_max
    
    for node in nodes {
      if node.end_time > max {
        max = node.end_time
      }
      
      # Check children
      let child_max = find_max_end_time(node.children, max)
      if child_max > max {
        max = child_max
      }
    }
    
    max
  }
  
  // Test trace tree construction
  let tree = create_trace_tree("trace-123")
  
  # Add spans
  let root_span = {
    span_id: "span-1",
    trace_id: "trace-123",
    parent_id: None,
    service_name: "api-gateway",
    operation_name: "process_request",
    start_time: 1640995200,
    end_time: 1640995250,
    children: [],
    depth: 0,
    metadata: [("http.method", "GET"), ("http.path", "/api/users")]
  }
  
  let child_span1 = {
    span_id: "span-2",
    trace_id: "trace-123",
    parent_id: Some("span-1"),
    service_name: "auth-service",
    operation_name: "authenticate",
    start_time: 1640995210,
    end_time: 1640995220,
    children: [],
    depth: 0,
    metadata: [("user.id", "user-123")]
  }
  
  let child_span2 = {
    span_id: "span-3",
    trace_id: "trace-123",
    parent_id: Some("span-1"),
    service_name: "user-service",
    operation_name: "get_user",
    start_time: 1640995225,
    end_time: 1640995240,
    children: [],
    depth: 0,
    metadata: [("user.id", "user-123")]
  }
  
  let grandchild_span = {
    span_id: "span-4",
    trace_id: "trace-123",
    parent_id: Some("span-3"),
    service_name: "database",
    operation_name: "query_user",
    start_time: 1640995230,
    end_time: 1640995235,
    children: [],
    depth: 0,
    metadata: [("query", "SELECT * FROM users WHERE id = ?")]
  }
  
  # Build tree
  let tree1 = add_span_to_tree(tree, root_span)
  let tree2 = add_span_to_tree(tree1, child_span1)
  let tree3 = add_span_to_tree(tree2, child_span2)
  let tree4 = add_span_to_tree(tree3, grandchild_span)
  
  # Verify tree structure
  assert_eq(tree4.trace_id, "trace-123")
  assert_eq(tree4.total_spans, 4)
  assert_eq(tree4.max_depth, 3)
  assert_eq(tree4.total_duration, 50)  # 1640995250 - 1640995200
  assert_eq(tree4.root_nodes.length(), 1)
  
  # Verify hierarchy
  let root = tree4.root_nodes[0]
  assert_eq(root.span_id, "span-1")
  assert_eq(root.depth, 1)
  assert_eq(root.children.length(), 2)
  
  let auth_child = root.children.filter(fn(c) { c.span_id == "span-2" })[0]
  assert_eq(auth_child.depth, 2)
  assert_eq(auth_child.children.length(), 0)
  
  let user_child = root.children.filter(fn(c) { c.span_id == "span-3" })[0]
  assert_eq(user_child.depth, 2)
  assert_eq(user_child.children.length(), 1)
  
  let db_grandchild = user_child.children[0]
  assert_eq(db_grandchild.span_id, "span-4")
  assert_eq(db_grandchild.depth, 3)
  assert_eq(db_grandchild.children.length(), 0)
  
  # Test tree traversal
  let traverse_tree = fn(nodes: Array<TraceTreeNode>) {
    let mut result = []
    
    for node in nodes {
      result = result.push(node.span_id)
      
      # Traverse children
      let child_ids = traverse_tree(node.children)
      result = result + child_ids
    }
    
    result
  }
  
  let traversal_result = traverse_tree(tree4.root_nodes)
  assert_eq(traversal_result.length(), 4)
  assert_true(traversal_result.contains("span-1"))
  assert_true(traversal_result.contains("span-2"))
  assert_true(traversal_result.contains("span-3"))
  assert_true(traversal_result.contains("span-4"))
  
  # Test tree search
  let find_span = fn(nodes: Array<TraceTreeNode>, span_id: String) {
    for node in nodes {
      if node.span_id == span_id {
        return Some(node)
      }
      
      # Search children
      let child_result = find_span(node.children, span_id)
      if child_result.length > 0 {
        return child_result
      }
    }
    None
  }
  
  let found_span = find_span(tree4.root_nodes, "span-4")
  assert_true(found_span.length > 0)
  let span4 = found_span.get
  assert_eq(span4.service_name, "database")
  assert_eq(span4.operation_name, "query_user")
}

// Test 2: Time-Series Data Structure for Metrics
test "time-series data structure for efficient metrics storage and querying" {
  // Define time series point
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    tags: Array<(String, String)>
  }
  
  // Define time series chunk
  type TimeSeriesChunk = {
    chunk_id: String,
    start_time: Int,
    end_time: Int,
    points: Array<TimeSeriesPoint>,
    compressed: Bool
  }
  
  // Define time series
  type TimeSeries = {
    series_id: String,
    metric_name: String,
    tags: Array<(String, String)>,
    chunks: Array<TimeSeriesChunk>,
    total_points: Int,
    min_value: Float,
    max_value: Float,
    avg_value: Float
  }
  
  // Create time series
  let create_time_series = fn(series_id: String, metric_name: String, tags: Array<(String, String)>) {
    {
      series_id: series_id,
      metric_name: metric_name,
      tags: tags,
      chunks: [],
      total_points: 0,
      min_value: 0.0,
      max_value: 0.0,
      avg_value: 0.0
    }
  }
  
  // Add point to time series
  let add_point = fn(series: TimeSeries, timestamp: Int, value: Float, tags: Array<(String, String)>) {
    let point = {
      timestamp: timestamp,
      value: value,
      tags: tags
    }
    
    # Find or create chunk
    let chunk_size = 3600  # 1 hour chunks
    let chunk_start = (timestamp / chunk_size) * chunk_size
    let chunk_end = chunk_start + chunk_size
    
    let mut updated_chunks = series.chunks
    let mut chunk_found = false
    
    for i in 0..updated_chunks.length() {
      if updated_chunks[i].start_time == chunk_start {
        # Add to existing chunk
        let updated_points = updated_chunks[i].points.push(point)
        updated_chunks[i] = {
          chunk_id: updated_chunks[i].chunk_id,
          start_time: updated_chunks[i].start_time,
          end_time: updated_chunks[i].end_time,
          points: updated_points,
          compressed: updated_chunks[i].compressed
        }
        chunk_found = true
        break
      }
    }
    
    if not(chunk_found) {
      # Create new chunk
      let new_chunk = {
        chunk_id: "chunk-" + chunk_start.to_string(),
        start_time: chunk_start,
        end_time: chunk_end,
        points: [point],
        compressed: false
      }
      updated_chunks = updated_chunks.push(new_chunk)
    }
    
    # Update statistics
    let total_points = series.total_points + 1
    let min_value = if total_points == 1 {
      value
    } else if value < series.min_value {
      value
    } else {
      series.min_value
    }
    
    let max_value = if total_points == 1 {
      value
    } else if value > series.max_value {
      value
    } else {
      series.max_value
    }
    
    let avg_value = if total_points > 0 {
      let sum = series.avg_value * (total_points - 1).to_float() + value
      sum / total_points.to_float()
    } else {
      value
    }
    
    {
      series_id: series.series_id,
      metric_name: series.metric_name,
      tags: series.tags,
      chunks: updated_chunks,
      total_points: total_points,
      min_value: min_value,
      max_value: max_value,
      avg_value: avg_value
    }
  }
  
  // Query time series
  let query_time_series = fn(series: TimeSeries, start_time: Int, end_time: Int) {
    let mut result_points = []
    
    for chunk in series.chunks {
      # Check if chunk overlaps with query range
      if chunk.end_time >= start_time && chunk.start_time <= end_time {
        for point in chunk.points {
          if point.timestamp >= start_time && point.timestamp <= end_time {
            result_points = result_points.push(point)
          }
        }
      }
    }
    
    result_points
  }
  
  // Test time series
  let series = create_time_series("series-1", "response_time", [("service", "api"), ("endpoint", "/users")])
  
  # Add points
  let series1 = add_point(series, 1640995200, 100.0, [("status", "success")])
  let series2 = add_point(series1, 1640995300, 120.0, [("status", "success")])
  let series3 = add_point(series2, 1640995400, 80.0, [("status", "success")])
  let series4 = add_point(series3, 1640995500, 150.0, [("status", "error")])
  let series5 = add_point(series4, 1640995600, 90.0, [("status", "success")])
  let series6 = add_point(series5, 1640995700, 110.0, [("status", "success")])
  let series7 = add_point(series6, 1640995800, 95.0, [("status", "success")])
  let series8 = add_point(series7, 1640998800, 130.0, [("status", "success")])  # Different hour
  
  # Verify series statistics
  assert_eq(series8.series_id, "series-1")
  assert_eq(series8.metric_name, "response_time")
  assert_eq(series8.total_points, 8)
  assert_eq(series8.min_value, 80.0)
  assert_eq(series8.max_value, 150.0)
  assert_eq(series8.avg_value, (100.0 + 120.0 + 80.0 + 150.0 + 90.0 + 110.0 + 95.0 + 130.0) / 8.0)
  
  # Verify chunks
  assert_eq(series8.chunks.length(), 2)  # Two hour chunks
  
  let chunk1 = series8.chunks.filter(fn(c) { c.start_time == 1640995200 })[0]
  assert_eq(chunk1.points.length(), 7)
  assert_eq(chunk1.end_time, 1640998800)
  
  let chunk2 = series8.chunks.filter(fn(c) { c.start_time == 1640998800 })[0]
  assert_eq(chunk2.points.length(), 1)
  assert_eq(chunk2.end_time, 1641002400)
  
  # Test querying
  let query1 = query_time_series(series8, 1640995250, 1640995650)  # Middle range
  assert_eq(query1.length(), 4)  # Points at 1640995300, 1640995400, 1640995500, 1640995600
  
  let query2 = query_time_series(series8, 1640998500, 1640998900)  # Range across chunks
  assert_eq(query2.length(), 2)  # Points at 1640998800 from both chunks
  
  let query3 = query_time_series(series8, 1640999000, 1640999500)  # Empty range
  assert_eq(query3.length(), 0)
  
  # Test aggregation
  let aggregate_query = fn(series: TimeSeries, start_time: Int, end_time: Int, aggregation: String) {
    let points = query_time_series(series, start_time, end_time)
    
    if points.length() == 0 {
      0.0
    } else {
      match aggregation {
        "avg" => {
          let sum = points.reduce(fn(acc, p) { acc + p.value }, 0.0)
          sum / points.length().to_float()
        }
        "min" => {
          points.reduce(fn(min, p) { if p.value < min { p.value } else { min } }, points[0].value)
        }
        "max" => {
          points.reduce(fn(max, p) { if p.value > max { p.value } else { max } }, points[0].value)
        }
        "sum" => {
          points.reduce(fn(acc, p) { acc + p.value }, 0.0)
        }
        "count" => {
          points.length().to_float()
        }
        _ => 0.0
      }
    }
  }
  
  # Test aggregations
  let avg_result = aggregate_query(series8, 1640995200, 1640995800, "avg")
  assert_eq(avg_result, (100.0 + 120.0 + 80.0 + 150.0 + 90.0 + 110.0 + 95.0) / 7.0)
  
  let min_result = aggregate_query(series8, 1640995200, 1640995800, "min")
  assert_eq(min_result, 80.0)
  
  let max_result = aggregate_query(series8, 1640995200, 1640995800, "max")
  assert_eq(max_result, 150.0)
  
  let count_result = aggregate_query(series8, 1640995200, 1640995800, "count")
  assert_eq(count_result, 7.0)
}

// Test 3: Efficient Ring Buffer for Circular Data Storage
test "efficient ring buffer for circular telemetry data storage" {
  // Define ring buffer
  type RingBuffer = {
    buffer: Array<String>,
    size: Int,
    head: Int,
    tail: Int,
    count: Int,
    is_full: Bool
  }
  
  // Create ring buffer
  let create_ring_buffer = fn(size: Int) {
    let mut buffer = []
    for i in 0..size {
      buffer = buffer.push("")
    }
    
    {
      buffer: buffer,
      size: size,
      head: 0,
      tail: 0,
      count: 0,
      is_full: false
    }
  }
  
  // Add item to ring buffer
  let ring_buffer_push = fn(buffer: RingBuffer, item: String) {
    let new_buffer = buffer.buffer
    
    # Add item at head position
    let updated_buffer = new_buffer.with(buffer.head, item)
    
    # Update head
    let new_head = (buffer.head + 1) % buffer.size
    
    # Update tail if buffer was full
    let new_tail = if buffer.is_full {
      (buffer.tail + 1) % buffer.size
    } else {
      buffer.tail
    }
    
    # Update count and full status
    let new_count = if buffer.is_full {
      buffer.size
    } else {
      buffer.count + 1
    }
    
    let new_is_full = new_count == buffer.size
    
    {
      buffer: updated_buffer,
      size: buffer.size,
      head: new_head,
      tail: new_tail,
      count: new_count,
      is_full: new_is_full
    }
  }
  
  // Get item from ring buffer
  let ring_buffer_get = fn(buffer: RingBuffer, index: Int) {
    if index >= buffer.count {
      None
    } else {
      let actual_index = (buffer.tail + index) % buffer.size
      Some(buffer.buffer[actual_index])
    }
  }
  
  // Get all items in order
  let ring_buffer_to_array = fn(buffer: RingBuffer) {
    let mut result = []
    
    for i in 0..buffer.count {
      match ring_buffer_get(buffer, i) {
        Some(item) => result = result.push(item)
        None => {}
      }
    }
    
    result
  }
  
  // Test ring buffer
  let buffer = create_ring_buffer(5)
  
  # Add items
  let buffer1 = ring_buffer_push(buffer, "item-1")
  let buffer2 = ring_buffer_push(buffer1, "item-2")
  let buffer3 = ring_buffer_push(buffer2, "item-3")
  let buffer4 = ring_buffer_push(buffer3, "item-4")
  let buffer5 = ring_buffer_push(buffer4, "item-5")
  
  # Verify buffer state
  assert_eq(buffer5.count, 5)
  assert_true(buffer5.is_full)
  assert_eq(buffer5.head, 0)  # Wrapped around
  assert_eq(buffer5.tail, 0)
  
  # Get items
  let items = ring_buffer_to_array(buffer5)
  assert_eq(items.length(), 5)
  assert_eq(items[0], "item-1")
  assert_eq(items[1], "item-2")
  assert_eq(items[2], "item-3")
  assert_eq(items[3], "item-4")
  assert_eq(items[4], "item-5")
  
  # Add more items (should overwrite oldest)
  let buffer6 = ring_buffer_push(buffer5, "item-6")
  let buffer7 = ring_buffer_push(buffer6, "item-7")
  
  # Verify buffer state
  assert_eq(buffer7.count, 5)
  assert_true(buffer7.is_full)
  assert_eq(buffer7.head, 2)
  assert_eq(buffer7.tail, 2)
  
  # Get items (oldest should be overwritten)
  let items2 = ring_buffer_to_array(buffer7)
  assert_eq(items2.length(), 5)
  assert_eq(items2[0], "item-3")  # item-1 and item-2 overwritten
  assert_eq(items2[1], "item-4")
  assert_eq(items2[2], "item-5")
  assert_eq(items2[3], "item-6")
  assert_eq(items2[4], "item-7")
  
  # Test individual item access
  let item0 = ring_buffer_get(buffer7, 0)
  assert_eq(item0, Some("item-3"))
  
  let item2 = ring_buffer_get(buffer7, 2)
  assert_eq(item2, Some("item-5"))
  
  let item4 = ring_buffer_get(buffer7, 4)
  assert_eq(item4, Some("item-7"))
  
  let item5 = ring_buffer_get(buffer7, 5)  # Out of bounds
  assert_eq(item5, None)
  
  # Test buffer clearing
  let clear_ring_buffer = fn(buffer: RingBuffer) {
    let mut cleared_buffer = buffer.buffer
    for i in 0..cleared_buffer.length() {
      cleared_buffer = cleared_buffer.with(i, "")
    }
    
    {
      buffer: cleared_buffer,
      size: buffer.size,
      head: 0,
      tail: 0,
      count: 0,
      is_full: false
    }
  }
  
  let cleared_buffer = clear_ring_buffer(buffer7)
  assert_eq(cleared_buffer.count, 0)
  assert_false(cleared_buffer.is_full)
  assert_eq(cleared_buffer.head, 0)
  assert_eq(cleared_buffer.tail, 0)
  
  # Test telemetry data ring buffer
  let telemetry_buffer = create_ring_buffer(100)
  
  # Add telemetry events
  let mut telemetry_with_events = telemetry_buffer
  for i in 0..150 {
    telemetry_with_events = ring_buffer_push(telemetry_with_events, "telemetry-event-" + i.to_string())
  }
  
  # Should only have last 100 events
  assert_eq(telemetry_with_events.count, 100)
  assert_true(telemetry_with_events.is_full)
  
  let telemetry_events = ring_buffer_to_array(telemetry_with_events)
  assert_eq(telemetry_events.length(), 100)
  assert_eq(telemetry_events[0], "telemetry-event-50")  # First 50 events overwritten
  assert_eq(telemetry_events[99], "telemetry-event-149")
}

// Test 4: Bloom Filter for Efficient Duplicate Detection
test "bloom filter for efficient duplicate detection in telemetry" {
  // Define bloom filter
  type BloomFilter = {
    bit_array: Array[Bool],
    size: Int,
    hash_count: Int,
    items_added: Int
  }
  
  // Create bloom filter
  let create_bloom_filter = fn(size: Int, hash_count: Int) {
    let mut bit_array = []
    for i in 0..size {
      bit_array = bit_array.push(false)
    }
    
    {
      bit_array: bit_array,
      size: size,
      hash_count: hash_count,
      items_added: 0
    }
  }
  
  // Simple hash functions (for testing)
  let hash1 = fn(item: String, size: Int) {
    let mut hash = 0
    for i in 0..item.length() {
      hash = (hash + item[i].to_int()) % size
    }
    hash
  }
  
  let hash2 = fn(item: String, size: Int) {
    let mut hash = 0
    for i in 0..item.length() {
      hash = (hash * 31 + item[i].to_int()) % size
    }
    hash
  }
  
  let hash3 = fn(item: String, size: Int) {
    let mut hash = 0
    for i in 0..item.length() {
      hash = (hash * 17 + item[i].to_int()) % size
    }
    hash
  }
  
  // Add item to bloom filter
  let bloom_filter_add = fn(filter: BloomFilter, item: String) {
    let mut updated_bit_array = filter.bit_array
    
    # Apply hash functions and set bits
    let hash1_result = hash1(item, filter.size)
    let hash2_result = hash2(item, filter.size)
    let hash3_result = hash3(item, filter.size)
    
    updated_bit_array = updated_bit_array.with(hash1_result, true)
    updated_bit_array = updated_bit_array.with(hash2_result, true)
    updated_bit_array = updated_bit_array.with(hash3_result, true)
    
    {
      bit_array: updated_bit_array,
      size: filter.size,
      hash_count: filter.hash_count,
      items_added: filter.items_added + 1
    }
  }
  
  // Check if item might exist in bloom filter
  let bloom_filter_might_contain = fn(filter: BloomFilter, item: String) {
    let hash1_result = hash1(item, filter.size)
    let hash2_result = hash2(item, filter.size)
    let hash3_result = hash3(item, filter.size)
    
    filter.bit_array[hash1_result] && 
    filter.bit_array[hash2_result] && 
    filter.bit_array[hash3_result]
  }
  
  // Test bloom filter
  let filter = create_bloom_filter(100, 3)
  
  # Add items
  let filter1 = bloom_filter_add(filter, "trace-123")
  let filter2 = bloom_filter_add(filter1, "trace-456")
  let filter3 = bloom_filter_add(filter2, "trace-789")
  
  # Check for existing items
  assert_true(bloom_filter_might_contain(filter3, "trace-123"))
  assert_true(bloom_filter_might_contain(filter3, "trace-456"))
  assert_true(bloom_filter_might_contain(filter3, "trace-789"))
  
  # Check for non-existing items
  assert_false(bloom_filter_might_contain(filter3, "trace-999"))
  assert_false(bloom_filter_might_contain(filter3, "trace-000"))
  
  # Check for false positives (possible with bloom filters)
  let filter4 = bloom_filter_add(filter3, "span-111")
  let filter5 = bloom_filter_add(filter4, "span-222")
  let filter6 = bloom_filter_add(filter5, "span-333")
  let filter7 = bloom_filter_add(filter6, "span-444")
  let filter8 = bloom_filter_add(filter7, "span-555")
  
  # Some non-existing items might return true due to hash collisions
  # This is expected behavior for bloom filters
  let false_positive_check = bloom_filter_might_contain(filter8, "non-existing-item")
  # This might be true or false depending on hash collisions
  
  # Test bloom filter statistics
  let get_bloom_filter_stats = fn(filter: BloomFilter) {
    let set_bits = filter.bit_array.filter(fn(bit) { bit }).length()
    let load_factor = set_bits.to_float() / filter.size.to_float()
    
    # Theoretical false positive rate
    let theoretical_fp_rate = (1.0 - (1.0 - 1.0 / filter.size.to_float()).to_float().pow(filter.hash_count.to_float())).pow(filter.items_added.to_float())
    
    {
      size: filter.size,
      hash_count: filter.hash_count,
      items_added: filter.items_added,
      set_bits: set_bits,
      load_factor: load_factor,
      theoretical_false_positive_rate: theoretical_fp_rate
    }
  }
  
  let stats = get_bloom_filter_stats(filter8)
  assert_eq(stats.size, 100)
  assert_eq(stats.hash_count, 3)
  assert_eq(stats.items_added, 8)
  assert_true(stats.set_bits > 0)
  assert_true(stats.load_factor > 0.0 && stats.load_factor <= 1.0)
  assert_true(stats.theoretical_false_positive_rate >= 0.0 && stats.theoretical_false_positive_rate <= 1.0)
  
  # Test bloom filter for telemetry duplicate detection
  let telemetry_filter = create_bloom_filter(1000, 3)
  
  # Simulate processing telemetry events
  let process_telemetry_event = fn(filter: BloomFilter, event_id: String) {
    if bloom_filter_might_contain(filter, event_id) {
      # Might be duplicate, need exact check
      (filter, "duplicate")
    } else {
      # Definitely new, add to filter
      (bloom_filter_add(filter, event_id), "new")
    }
  }
  
  let mut current_filter = telemetry_filter
  let mut results = []
  
  # Process events
  let events = ["event-1", "event-2", "event-3", "event-1", "event-4", "event-2", "event-5"]
  
  for event in events {
    let (new_filter, result) = process_telemetry_event(current_filter, event)
    current_filter = new_filter
    results = results.push((event, result))
  }
  
  # Verify results
  assert_eq(results[0], ("event-1", "new"))      # First time
  assert_eq(results[1], ("event-2", "new"))      # First time
  assert_eq(results[2], ("event-3", "new"))      # First time
  assert_eq(results[3], ("event-1", "duplicate")) # Duplicate
  assert_eq(results[4], ("event-4", "new"))      # First time
  assert_eq(results[5], ("event-2", "duplicate")) # Duplicate
  assert_eq(results[6], ("event-5", "new"))      # First time
}

// Test 5: Skip List for Efficient Range Queries
test "skip list for efficient range queries on telemetry data" {
  // Define skip list node
  type SkipListNode = {
    key: Int,
    value: String,
    forward: Array[Option<SkipListNode>]  # Forward pointers at different levels
  }
  
  // Define skip list
  type SkipList = {
    head: SkipListNode,
    max_level: Int,
    current_level: Int,
    size: Int
  }
  
  // Create skip list node
  let create_skip_list_node = fn(key: Int, value: String, level: Int) {
    let mut forward = []
    for i in 0..level {
      forward = forward.push(None)
    }
    
    {
      key: key,
      value: value,
      forward: forward
    }
  }
  
  // Create skip list
  let create_skip_list = fn(max_level: Int) {
    let head_node = create_skip_list_node(-2147483648, "HEAD", max_level)  # Negative infinity head
    
    {
      head: head_node,
      max_level: max_level,
      current_level: 1,
      size: 0
    }
  }
  
  // Random level generator (simplified)
  let random_level = fn(max_level: Int) {
    # Simplified random level generation for testing
    # In real implementation, would use proper random number generator
    let level = 1 + (1640995200 % max_level)  # Use timestamp as pseudo-random
    if level > max_level {
      max_level
    } else {
      level
    }
  }
  
  // Insert into skip list
  let skip_list_insert = fn(list: SkipList, key: Int, value: String) {
    let level = random_level(list.max_level)
    
    # Create new node
    let new_node = create_skip_list_node(key, value, level)
    
    # Find insertion position
    let mut update = []
    for i in 0..list.max_level {
      update = update.push(list.head)
    }
    
    let mut current = list.head
    
    # Find insertion points at each level
    for i in (list.current_level - 1)..0 {
      while current.forward[i].length > 0 && 
            current.forward[i].get.key < key {
        current = current.forward[i].get
      }
      update = update.with(i, current)
    }
    
    # Insert new node
    for i in 0..level {
      new_node.forward = new_node.forward.with(i, update[i].forward[i])
      update[i].forward = update[i].forward.with(i, Some(new_node))
    }
    
    # Update current level if needed
    let new_current_level = if level > list.current_level {
      level
    } else {
      list.current_level
    }
    
    {
      head: list.head,
      max_level: list.max_level,
      current_level: new_current_level,
      size: list.size + 1
    }
  }
  
  // Search in skip list
  let skip_list_search = fn(list: SkipList, key: Int) {
    let mut current = list.head
    
    # Start from highest level
    for i in (list.current_level - 1)..0 {
      while current.forward[i].length > 0 && 
            current.forward[i].get.key < key {
        current = current.forward[i].get
      }
    }
    
    # Check at level 0
    current = current.forward[0].get
    
    if current.length > 0 && current.key == key {
      Some(current.value)
    } else {
      None
    }
  }
  
  # Range query in skip list
  let skip_list_range_query = fn(list: SkipList, start_key: Int, end_key: Int) {
    let mut result = []
    let mut current = list.head
    
    # Find start position
    for i in (list.current_level - 1)..0 {
      while current.forward[i].length > 0 && 
            current.forward[i].get.key < start_key {
        current = current.forward[i].get
      }
    }
    
    # Collect all items in range
    current = current.forward[0].get
    while current.length > 0 && current.key <= end_key {
      result = result.push((current.key, current.value))
      current = current.forward[0].get
    }
    
    result
  }
  
  # Test skip list
  let list = create_skip_list(4)
  
  # Insert items
  let list1 = skip_list_insert(list, 10, "value-10")
  let list2 = skip_list_insert(list1, 20, "value-20")
  let list3 = skip_list_insert(list2, 5, "value-5")
  let list4 = skip_list_insert(list3, 15, "value-15")
  let list5 = skip_list_insert(list4, 25, "value-25")
  let list6 = skip_list_insert(list5, 30, "value-30")
  
  # Verify size
  assert_eq(list6.size, 6)
  
  # Test search
  let search10 = skip_list_search(list6, 10)
  assert_eq(search10, Some("value-10"))
  
  let search15 = skip_list_search(list6, 15)
  assert_eq(search15, Some("value-15"))
  
  let search35 = skip_list_search(list6, 35)
  assert_eq(search35, None)
  
  # Test range query
  let range1 = skip_list_range_query(list6, 10, 20)
  assert_eq(range1.length(), 3)
  assert_true(range1.contains((10, "value-10")))
  assert_true(range1.contains((15, "value-15")))
  assert_true(range1.contains((20, "value-20")))
  
  let range2 = skip_list_range_query(list6, 12, 28)
  assert_eq(range2.length(), 3)
  assert_true(range2.contains((15, "value-15")))
  assert_true(range2.contains((20, "value-20")))
  assert_true(range2.contains((25, "value-25")))
  
  let range3 = skip_list_range_query(list6, 35, 40)
  assert_eq(range3.length(), 0)
  
  # Test telemetry timestamp indexing
  let telemetry_list = create_skip_list(8)
  
  # Insert telemetry events with timestamps
  let telemetry_events = [
    (1640995200, "telemetry-event-1"),
    (1640995300, "telemetry-event-2"),
    (1640995400, "telemetry-event-3"),
    (1640995500, "telemetry-event-4"),
    (1640995600, "telemetry-event-5"),
    (1640995700, "telemetry-event-6"),
    (1640995800, "telemetry-event-7"),
    (1640995900, "telemetry-event-8"),
    (1640996000, "telemetry-event-9"),
    (1640996100, "telemetry-event-10")
  ]
  
  let mut telemetry_with_events = telemetry_list
  for event in telemetry_events {
    telemetry_with_events = skip_list_insert(telemetry_with_events, event[0], event[1])
  }
  
  # Query telemetry events in time range
  let time_range_events = skip_list_range_query(telemetry_with_events, 1640995300, 1640995800)
  assert_eq(time_range_events.length(), 5)
  assert_true(time_range_events.contains((1640995300, "telemetry-event-2")))
  assert_true(time_range_events.contains((1640995400, "telemetry-event-3")))
  assert_true(time_range_events.contains((1640995500, "telemetry-event-4")))
  assert_true(time_range_events.contains((1640995600, "telemetry-event-5")))
  assert_true(time_range_events.contains((1640995700, "telemetry-event-6")))
  
  # Test recent events
  let recent_events = skip_list_range_query(telemetry_with_events, 1640996000, 1640996200)
  assert_eq(recent_events.length(), 2)
  assert_true(recent_events.contains((1640996000, "telemetry-event-9")))
  assert_true(recent_events.contains((1640996100, "telemetry-event-10")))
}