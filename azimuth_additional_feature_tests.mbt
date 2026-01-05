// Additional Feature Tests for Azimuth
// This file contains additional test cases for various features

// Test 1: JSON Data Processing
test "json data processing" {
  // Simulate JSON parsing and manipulation
  let json_string = "{\"name\":\"service\",\"version\":\"1.0.0\",\"endpoints\":[\"/api\",\"/health\"]}"
  
  // Extract key-value pairs (simplified JSON parsing)
  let extract_value = fn(json: String, key: String) {
    let key_pattern = "\"" + key + "\":\""
    let start_index = json.index_of(key_pattern)
    if start_index >= 0 {
      let value_start = start_index + key_pattern.length()
      let end_index = json.index_of("\"", value_start)
      if end_index >= 0 {
        json.substring(value_start, end_index - value_start)
      } else {
        ""
      }
    } else {
      ""
    }
  }
  
  // Test JSON value extraction
  let name = extract_value(json_string, "name")
  let version = extract_value(json_string, "version")
  
  assert_eq(name, "service")
  assert_eq(version, "1.0.0")
  
  // Simulate JSON array parsing
  let extract_array = fn(json: String, key: String) {
    let key_pattern = "\"" + key + "\":["
    let start_index = json.index_of(key_pattern)
    if start_index >= 0 {
      let array_start = start_index + key_pattern.length()
      let end_index = json.index_of("]", array_start)
      if end_index >= 0 {
        let array_content = json.substring(array_start, end_index - array_start)
        array_content.split(",")
      } else {
        []
      }
    } else {
      []
    }
  }
  
  let endpoints = extract_array(json_string, "endpoints")
  assert_eq(endpoints.length(), 2)
  assert_eq(endpoints[0], "\"/api\"")
  assert_eq(endpoints[1], "\"/health\"")
  
  // Simulate JSON building
  let build_json = fn(pairs: Array[(String, String)]) {
    let mut result = "{"
    for i in 0..pairs.length() {
      let (key, value) = pairs[i]
      result = result + "\"" + key + "\":\"" + value + "\""
      if i < pairs.length() - 1 {
        result = result + ","
      }
    }
    result = result + "}"
    result
  }
  
  let json_data = [
    ("service", "telemetry"),
    ("status", "active"),
    ("region", "us-west-2")
  ]
  
  let built_json = build_json(json_data)
  assert_true(built_json.contains("\"service\":\"telemetry\""))
  assert_true(built_json.contains("\"status\":\"active\""))
  assert_true(built_json.contains("\"region\":\"us-west-2\""))
}

// Test 2: Regular Expression Pattern Matching
test "regular expression pattern matching" {
  // Simulate regular expression operations
  let matches_pattern = fn(text: String, pattern: String) {
    // Simple pattern matching for demonstration
    if pattern == "^[a-z]+$" {
      // Check if text contains only lowercase letters
      let mut all_lower = true
      for i in 0..text.length() {
        let char = text[i]
        if not((char >= 'a' and char <= 'z')) {
          all_lower = false
        }
      }
      all_lower
    } else if pattern == "^\\d+$" {
      // Check if text contains only digits
      let mut all_digits = true
      for i in 0..text.length() {
        let char = text[i]
        if not((char >= '0' and char <= '9')) {
          all_digits = false
        }
      }
      all_digits
    } else if pattern == "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" {
      // Simple email validation
      text.contains("@") and text.contains(".") and text.length() > 5
    } else {
      false
    }
  }
  
  // Test pattern matching
  assert_true(matches_pattern("hello", "^[a-z]+$"))
  assert_false(matches_pattern("Hello", "^[a-z]+$"))
  assert_false(matches_pattern("hello123", "^[a-z]+$"))
  
  assert_true(matches_pattern("12345", "^\\d+$"))
  assert_false(matches_pattern("12a45", "^\\d+$"))
  assert_false(matches_pattern("abc", "^\\d+$"))
  
  assert_true(matches_pattern("user@example.com", "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"))
  assert_false(matches_pattern("user@example", "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"))
  assert_false(matches_pattern("user.example.com", "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"))
  
  // Simulate pattern replacement
  let replace_pattern = fn(text: String, pattern: String, replacement: String) {
    if pattern == "\\d+" {
      // Replace digits with replacement
      let mut result = ""
      let mut i = 0
      while i < text.length() {
        let char = text[i]
        if char >= '0' and char <= '9' {
          result = result + replacement
        } else {
          result = result + char.to_string()
        }
        i = i + 1
      }
      result
    } else {
      text
    }
  }
  
  let replaced_text = replace_pattern("item123price45", "\\d+", "X")
  assert_eq(replaced_text, "itemXXXpriceXX")
  
  // Simulate pattern extraction
  let extract_pattern = fn(text: String, pattern: String) {
    if pattern == "\\d+" {
      // Extract all digit sequences
      let mut sequences = []
      let mut current_sequence = ""
      let mut in_digit_sequence = false
      
      for i in 0..text.length() {
        let char = text[i]
        if char >= '0' and char <= '9' {
          current_sequence = current_sequence + char.to_string()
          in_digit_sequence = true
        } else {
          if in_digit_sequence {
            sequences = sequences.push(current_sequence)
            current_sequence = ""
            in_digit_sequence = false
          }
        }
      }
      
      if in_digit_sequence {
        sequences = sequences.push(current_sequence)
      }
      
      sequences
    } else {
      []
    }
  }
  
  let extracted_numbers = extract_pattern("test123data456info789", "\\d+")
  assert_eq(extracted_numbers.length(), 3)
  assert_eq(extracted_numbers[0], "123")
  assert_eq(extracted_numbers[1], "456")
  assert_eq(extracted_numbers[2], "789")
}

// Test 3: Stack and Queue Data Structures
test "stack and queue data structures" {
  // Stack implementation using array
  let stack_push = fn(stack: Array[String], item: String) {
    stack.push(item)
  }
  
  let stack_pop = fn(stack: Array[String]) {
    if stack.length() > 0 {
      let item = stack[stack.length() - 1]
      (stack.slice(0, stack.length() - 1), item)
    } else {
      (stack, "")
    }
  }
  
  // Test stack operations
  let mut stack = []
  stack = stack_push(stack, "first")
  stack = stack_push(stack, "second")
  stack = stack_push(stack, "third")
  
  assert_eq(stack.length(), 3)
  
  let (stack_after_pop1, popped1) = stack_pop(stack)
  assert_eq(popped1, "third")
  assert_eq(stack_after_pop1.length(), 2)
  
  let (stack_after_pop2, popped2) = stack_pop(stack_after_pop1)
  assert_eq(popped2, "second")
  assert_eq(stack_after_pop2.length(), 1)
  
  let (stack_after_pop3, popped3) = stack_pop(stack_after_pop2)
  assert_eq(popped3, "first")
  assert_eq(stack_after_pop3.length(), 0)
  
  // Queue implementation using array
  let queue_enqueue = fn(queue: Array[String], item: String) {
    queue.push(item)
  }
  
  let queue_dequeue = fn(queue: Array[String]) {
    if queue.length() > 0 {
      let item = queue[0]
      (queue.slice(1, queue.length()), item)
    } else {
      (queue, "")
    }
  }
  
  // Test queue operations
  let mut queue = []
  queue = queue_enqueue(queue, "first")
  queue = queue_enqueue(queue, "second")
  queue = queue_enqueue(queue, "third")
  
  assert_eq(queue.length(), 3)
  
  let (queue_after_dequeue1, dequeued1) = queue_dequeue(queue)
  assert_eq(dequeued1, "first")
  assert_eq(queue_after_dequeue1.length(), 2)
  
  let (queue_after_dequeue2, dequeued2) = queue_dequeue(queue_after_dequeue1)
  assert_eq(dequeued2, "second")
  assert_eq(queue_after_dequeue2.length(), 1)
  
  let (queue_after_dequeue3, dequeued3) = queue_dequeue(queue_after_dequeue2)
  assert_eq(dequeued3, "third")
  assert_eq(queue_after_dequeue3.length(), 0)
  
  // Test stack with numeric values
  let mut number_stack = []
  number_stack = number_stack.push(1)
  number_stack = number_stack.push(2)
  number_stack = number_stack.push(3)
  
  let (num_stack_after_pop, num_popped) = (number_stack.slice(0, number_stack.length() - 1), number_stack[number_stack.length() - 1])
  assert_eq(num_popped, 3)
  assert_eq(num_stack_after_pop, [1, 2])
  
  // Test queue with numeric values
  let mut number_queue = []
  number_queue = number_queue.push(10)
  number_queue = number_queue.push(20)
  number_queue = number_queue.push(30)
  
  let (num_queue_after_dequeue, num_dequeued) = (number_queue.slice(1, number_queue.length()), number_queue[0])
  assert_eq(num_dequeued, 10)
  assert_eq(num_queue_after_dequeue, [20, 30])
}

// Test 4: Recursive Algorithms
test "recursive algorithms" {
  // Recursive factorial
  let factorial = fn(n: Int) {
    if n <= 1 {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(5), 120)
  assert_eq(factorial(7), 5040)
  
  // Recursive Fibonacci
  let fibonacci = fn(n: Int) {
    if n <= 1 {
      n
    } else {
      fibonacci(n - 1) + fibonacci(n - 2)
    }
  }
  
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(5), 5)
  assert_eq(fibonacci(7), 13)
  
  // Recursive GCD (Greatest Common Divisor)
  let gcd = fn(a: Int, b: Int) {
    if b == 0 {
      a
    } else {
      gcd(b, a % b)
    }
  }
  
  assert_eq(gcd(48, 18), 6)
  assert_eq(gcd(54, 24), 6)
  assert_eq(gcd(17, 13), 1)
  
  // Recursive binary search
  let binary_search = fn(arr: Array[Int], target: Int, left: Int, right: Int) {
    if left > right {
      -1  // Not found
    } else {
      let mid = (left + right) / 2
      if arr[mid] == target {
        mid
      } else if arr[mid] > target {
        binary_search(arr, target, left, mid - 1)
      } else {
        binary_search(arr, target, mid + 1, right)
      }
    }
  }
  
  let sorted_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
  assert_eq(binary_search(sorted_array, 7, 0, sorted_array.length() - 1), 3)
  assert_eq(binary_search(sorted_array, 1, 0, sorted_array.length() - 1), 0)
  assert_eq(binary_search(sorted_array, 19, 0, sorted_array.length() - 1), 9)
  assert_eq(binary_search(sorted_array, 8, 0, sorted_array.length() - 1), -1)
  
  // Recursive reverse string
  let reverse_string = fn(s: String) {
    if s.length() <= 1 {
      s
    } else {
      reverse_string(s.substring(1, s.length() - 1)) + s[0].to_string()
    }
  }
  
  assert_eq(reverse_string("hello"), "olleh")
  assert_eq(reverse_string("abcde"), "edcba")
  assert_eq(reverse_string(""), "")
  assert_eq(reverse_string("a"), "a")
  
  // Recursive power calculation
  let power = fn(base: Int, exp: Int) {
    if exp == 0 {
      1
    } else if exp % 2 == 0 {
      let half_power = power(base, exp / 2)
      half_power * half_power
    } else {
      base * power(base, exp - 1)
    }
  }
  
  assert_eq(power(2, 0), 1)
  assert_eq(power(2, 5), 32)
  assert_eq(power(3, 4), 81)
  assert_eq(power(5, 3), 125)
}

// Test 5: Stream Data Processing
test "stream data processing" {
  // Simulate stream processing with batch operations
  let process_stream = fn(data_stream: Array[String], batch_size: Int, processor: (Array[String]) -> Array[String]) {
    let mut processed_data = []
    let mut i = 0
    
    while i < data_stream.length() {
      let end_index = if i + batch_size < data_stream.length() {
        i + batch_size
      } else {
        data_stream.length()
      }
      
      let batch = data_stream.slice(i, end_index)
      let processed_batch = processor(batch)
      processed_data = processed_data + processed_batch
      
      i = end_index
    }
    
    processed_data
  }
  
  // Test stream processing with uppercase transformation
  let stream_data = ["item1", "item2", "item3", "item4", "item5"]
  let to_uppercase = fn(batch: Array[String>) {
    batch.map(fn(item) { item.to_uppercase() })
  }
  
  let processed_stream = process_stream(stream_data, 2, to_uppercase)
  assert_eq(processed_stream, ["ITEM1", "ITEM2", "ITEM3", "ITEM4", "ITEM5"])
  
  // Test stream filtering
  let filter_stream = fn(data_stream: Array[Int], batch_size: Int, predicate: (Int) -> Bool) {
    let mut filtered_data = []
    let mut i = 0
    
    while i < data_stream.length() {
      let end_index = if i + batch_size < data_stream.length() {
        i + batch_size
      } else {
        data_stream.length()
      }
      
      let batch = data_stream.slice(i, end_index)
      let filtered_batch = batch.filter(predicate)
      filtered_data = filtered_data + filtered_batch
      
      i = end_index
    }
    
    filtered_data
  }
  
  let number_stream = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let is_even = fn(n: Int) { n % 2 == 0 }
  
  let even_numbers = filter_stream(number_stream, 3, is_even)
  assert_eq(even_numbers, [2, 4, 6, 8, 10])
  
  // Test stream aggregation
  let aggregate_stream = fn(data_stream: Array[Int], batch_size: Int, aggregator: (Array[Int]) -> Int) {
    let mut results = []
    let mut i = 0
    
    while i < data_stream.length() {
      let end_index = if i + batch_size < data_stream.length() {
        i + batch_size
      } else {
        data_stream.length()
      }
      
      let batch = data_stream.slice(i, end_index)
      let aggregated_value = aggregator(batch)
      results = results.push(aggregated_value)
      
      i = end_index
    }
    
    results
  }
  
  let sum_batch = fn(batch: Array[Int]) {
    let mut sum = 0
    for value in batch {
      sum = sum + value
    }
    sum
  }
  
  let batch_sums = aggregate_stream(number_stream, 4, sum_batch)
  assert_eq(batch_sums, [10, 26, 19])  // [1+2+3+4, 5+6+7+8, 9+10]
  
  // Test stream windowing
  let window_stream = fn(data_stream: Array[Int], window_size: Int) {
    let mut windows = []
    
    for i in 0..=(data_stream.length() - window_size) {
      let window = data_stream.slice(i, i + window_size)
      windows = windows.push(window)
    }
    
    windows
  }
  
  let windows = window_stream(number_stream, 3)
  assert_eq(windows.length(), 8)
  assert_eq(windows[0], [1, 2, 3])
  assert_eq(windows[1], [2, 3, 4])
  assert_eq(windows[7], [8, 9, 10])
  
  // Test stream transformation with state
  let transform_with_state = fn(data_stream: Array[String], batch_size: Int) {
    let mut counter = 1
    let mut transformed_data = []
    let mut i = 0
    
    while i < data_stream.length() {
      let end_index = if i + batch_size < data_stream.length() {
        i + batch_size
      } else {
        data_stream.length()
      }
      
      let batch = data_stream.slice(i, end_index)
      let mut transformed_batch = []
      
      for item in batch {
        transformed_batch = transformed_batch.push(item + "_" + counter.to_string())
        counter = counter + 1
      }
      
      transformed_data = transformed_data + transformed_batch
      i = end_index
    }
    
    transformed_data
  }
  
  let numbered_items = transform_with_state(stream_data, 2)
  assert_eq(numbered_items, ["item1_1", "item2_2", "item3_3", "item4_4", "item5_5"])
}

// Test 6: Caching Mechanism
test "caching mechanism" {
  // Simulate LRU (Least Recently Used) cache
  type CacheEntry[T] = {
    key: String,
    value: T,
    last_accessed: Int
  }
  
  type LRUCache[T] = {
    capacity: Int,
    entries: Array[CacheEntry[T]],
    current_time: Int
  }
  
  let create_cache = fn(capacity: Int) {
    {
      capacity,
      entries: [],
      current_time: 0
    }
  }
  
  let get_cache = fn(cache: LRUCache[T], key: String) {
    let mut found_index = -1
    
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        found_index = i
      }
    }
    
    if found_index >= 0 {
      let entry = cache.entries[found_index]
      let updated_entry = { entry | last_accessed: cache.current_time }
      let updated_entries = cache.entries.slice(0, found_index) + 
                           [updated_entry] + 
                           cache.entries.slice(found_index + 1, cache.entries.length())
      
      (Some(entry.value), { cache | entries: updated_entries, current_time: cache.current_time + 1 })
    } else {
      (None, { cache | current_time: cache.current_time + 1 })
    }
  }
  
  let put_cache = fn(cache: LRUCache[T], key: String, value: T) {
    let new_entry = {
      key,
      value,
      last_accessed: cache.current_time
    }
    
    let mut found_index = -1
    
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        found_index = i
      }
    }
    
    if found_index >= 0 {
      // Update existing entry
      let updated_entries = cache.entries.slice(0, found_index) + 
                           [new_entry] + 
                           cache.entries.slice(found_index + 1, cache.entries.length())
      { cache | entries: updated_entries, current_time: cache.current_time + 1 }
    } else if cache.entries.length() < cache.capacity {
      // Add new entry if cache is not full
      { cache | entries: cache.entries.push(new_entry), current_time: cache.current_time + 1 }
    } else {
      // Remove least recently used entry and add new one
      let mut lru_index = 0
      let mut lru_time = cache.entries[0].last_accessed
      
      for i in 1..cache.entries.length() {
        if cache.entries[i].last_accessed < lru_time {
          lru_index = i
          lru_time = cache.entries[i].last_accessed
        }
      }
      
      let updated_entries = cache.entries.slice(0, lru_index) + 
                           cache.entries.slice(lru_index + 1, cache.entries.length()).push(new_entry)
      { cache | entries: updated_entries, current_time: cache.current_time + 1 }
    }
  }
  
  // Test cache operations
  let cache = create_cache(3)
  
  // Add entries
  let cache1 = put_cache(cache, "key1", "value1")
  let cache2 = put_cache(cache1, "key2", "value2")
  let cache3 = put_cache(cache2, "key3", "value3")
  
  assert_eq(cache3.entries.length(), 3)
  
  // Get existing entry
  let (value1, cache4) = get_cache(cache3, "key2")
  assert_eq(value1, Some("value1"))
  
  // Add entry that exceeds capacity (should evict LRU)
  let cache5 = put_cache(cache4, "key4", "value4")
  assert_eq(cache5.entries.length(), 3)
  
  // Check that LRU entry was evicted
  let (evicted_value, _) = get_cache(cache5, "key1")
  assert_eq(evicted_value, None)
  
  // Check that other entries still exist
  let (existing_value1, _) = get_cache(cache5, "key2")
  let (existing_value2, _) = get_cache(cache5, "key3")
  let (existing_value3, _) = get_cache(cache5, "key4")
  
  assert_eq(existing_value1, Some("value2"))
  assert_eq(existing_value2, Some("value3"))
  assert_eq(existing_value3, Some("value4"))
  
  // Test cache with numeric values
  let num_cache = create_cache(2)
  let num_cache1 = put_cache(num_cache, "num1", 100)
  let num_cache2 = put_cache(num_cache1, "num2", 200)
  
  let (num_value1, _) = get_cache(num_cache2, "num1")
  let (num_value2, _) = get_cache(num_cache2, "num2")
  let (num_value3, _) = get_cache(num_cache2, "num3")
  
  assert_eq(num_value1, Some(100))
  assert_eq(num_value2, Some(200))
  assert_eq(num_value3, None)
}

// Test 7: Event Bus System
test "event bus system" {
  // Simulate event bus with subscribers and publishers
  type Event = {
    type: String,
    data: String
  }
  
  type Subscriber = {
    id: String,
    event_types: Array[String],
    received_events: Array[Event]
  }
  
  type EventBus = {
    subscribers: Array[Subscriber],
    published_events: Array[Event]
  }
  
  let create_event_bus = fn() {
    {
      subscribers: [],
      published_events: []
    }
  }
  
  let subscribe = fn(event_bus: EventBus, subscriber_id: String, event_types: Array[String]) {
    let new_subscriber = {
      id: subscriber_id,
      event_types,
      received_events: []
    }
    { event_bus | subscribers: event_bus.subscribers.push(new_subscriber) }
  }
  
  let publish = fn(event_bus: EventBus, event_type: String, event_data: String) {
    let new_event = {
      type: event_type,
      data: event_data
    }
    
    let mut updated_subscribers = []
    
    for subscriber in event_bus.subscribers {
      if subscriber.event_types.contains(event_type) {
        let updated_subscriber = {
          subscriber | 
          received_events: subscriber.received_events.push(new_event)
        }
        updated_subscribers = updated_subscribers.push(updated_subscriber)
      } else {
        updated_subscribers = updated_subscribers.push(subscriber)
      }
    }
    
    {
      subscribers: updated_subscribers,
      published_events: event_bus.published_events.push(new_event)
    }
  }
  
  let get_subscriber_events = fn(event_bus: EventBus, subscriber_id: String) {
    let mut subscriber_events = []
    
    for subscriber in event_bus.subscribers {
      if subscriber.id == subscriber_id {
        subscriber_events = subscriber.received_events
      }
    }
    
    subscriber_events
  }
  
  // Test event bus operations
  let event_bus = create_event_bus()
  
  // Add subscribers
  let event_bus1 = subscribe(event_bus, "sub1", ["user.created", "user.updated"])
  let event_bus2 = subscribe(event_bus1, "sub2", ["user.deleted"])
  let event_bus3 = subscribe(event_bus2, "sub3", ["user.created", "user.deleted"])
  
  assert_eq(event_bus3.subscribers.length(), 3)
  
  // Publish events
  let event_bus4 = publish(event_bus3, "user.created", "User John created")
  let event_bus5 = publish(event_bus4, "user.updated", "User John updated")
  let event_bus6 = publish(event_bus5, "user.deleted", "User Jane deleted")
  
  assert_eq(event_bus6.published_events.length(), 3)
  
  // Check subscriber events
  let sub1_events = get_subscriber_events(event_bus6, "sub1")
  let sub2_events = get_subscriber_events(event_bus6, "sub2")
  let sub3_events = get_subscriber_events(event_bus6, "sub3")
  
  assert_eq(sub1_events.length(), 2)  // user.created, user.updated
  assert_eq(sub2_events.length(), 1)  // user.deleted
  assert_eq(sub3_events.length(), 2)  // user.created, user.deleted
  
  assert_eq(sub1_events[0].type, "user.created")
  assert_eq(sub1_events[0].data, "User John created")
  assert_eq(sub1_events[1].type, "user.updated")
  assert_eq(sub1_events[1].data, "User John updated")
  
  assert_eq(sub2_events[0].type, "user.deleted")
  assert_eq(sub2_events[0].data, "User Jane deleted")
  
  assert_eq(sub3_events[0].type, "user.created")
  assert_eq(sub3_events[0].data, "User John created")
  assert_eq(sub3_events[1].type, "user.deleted")
  assert_eq(sub3_events[1].data, "User Jane deleted")
  
  // Test unsubscribe functionality
  let unsubscribe = fn(event_bus: EventBus, subscriber_id: String) {
    let mut updated_subscribers = []
    
    for subscriber in event_bus.subscribers {
      if subscriber.id != subscriber_id {
        updated_subscribers = updated_subscribers.push(subscriber)
      }
    }
    
    { event_bus | subscribers: updated_subscribers }
  }
  
  let event_bus7 = unsubscribe(event_bus6, "sub2")
  assert_eq(event_bus7.subscribers.length(), 2)
  
  // Publish another event and verify unsubscribed subscriber doesn't receive it
  let event_bus8 = publish(event_bus7, "user.deleted", "User Bob deleted")
  
  let sub1_events_after = get_subscriber_events(event_bus8, "sub1")
  let sub2_events_after = get_subscriber_events(event_bus8, "sub2")
  let sub3_events_after = get_subscriber_events(event_bus8, "sub3")
  
  // sub1 and sub3 should still have their original events
  assert_eq(sub1_events_after.length(), 2)
  assert_eq(sub3_events_after.length(), 2)
  
  // sub2 should be gone (empty array)
  assert_eq(sub2_events_after.length(), 0)
}

// Test 8: State Machine
test "state machine" {
  // Define state machine states
  enum State {
    Idle
    Active
    Paused
    Completed
    Error
  }
  
  // Define state machine events
  enum Event {
    Start
    Pause
    Resume
    Complete
    Fail
    Reset
  }
  
  // Define state machine
  type StateMachine = {
    current_state: State,
    state_history: Array[State]
  }
  
  // Create state transition function
  let transition = fn(state_machine: StateMachine, event: Event) {
    let new_state = match (state_machine.current_state, event) {
      (State::Idle, Event::Start) => State::Active
      (State::Active, Event::Pause) => State::Paused
      (State::Active, Event::Complete) => State::Completed
      (State::Active, Event::Fail) => State::Error
      (State::Paused, Event::Resume) => State::Active
      (State::Paused, Event::Fail) => State::Error
      (State::Completed, Event::Reset) => State::Idle
      (State::Error, Event::Reset) => State::Idle
      _ => state_machine.current_state  // No transition for invalid event
    }
    
    {
      current_state: new_state,
      state_history: state_machine.state_history.push(new_state)
    }
  }
  
  // Create initial state machine
  let initial_state_machine = {
    current_state: State::Idle,
    state_history: [State::Idle]
  }
  
  // Test state transitions
  let sm1 = transition(initial_state_machine, Event::Start)
  assert_eq(sm1.current_state, State::Active)
  
  let sm2 = transition(sm1, Event::Pause)
  assert_eq(sm2.current_state, State::Paused)
  
  let sm3 = transition(sm2, Event::Resume)
  assert_eq(sm3.current_state, State::Active)
  
  let sm4 = transition(sm3, Event::Complete)
  assert_eq(sm4.current_state, State::Completed)
  
  let sm5 = transition(sm4, Event::Reset)
  assert_eq(sm5.current_state, State::Idle)
  
  // Test error path
  let sm_error1 = transition(initial_state_machine, Event::Start)
  let sm_error2 = transition(sm_error1, Event::Fail)
  assert_eq(sm_error2.current_state, State::Error)
  
  let sm_error3 = transition(sm_error2, Event::Reset)
  assert_eq(sm_error3.current_state, State::Idle)
  
  // Test invalid transitions (should remain in current state)
  let sm_invalid1 = transition(initial_state_machine, Event::Pause)
  assert_eq(sm_invalid1.current_state, State::Idle)  // Can't pause from Idle
  
  let sm_invalid2 = transition(sm1, Event::Start)
  assert_eq(sm_invalid2.current_state, State::Active)  // Already Active
  
  // Test state history
  assert_eq(sm5.state_history.length(), 6)  // Initial + 5 transitions
  assert_eq(sm5.state_history[0], State::Idle)
  assert_eq(sm5.state_history[1], State::Active)
  assert_eq(sm5.state_history[2], State::Paused)
  assert_eq(sm5.state_history[3], State::Active)
  assert_eq(sm5.state_history[4], State::Completed)
  assert_eq(sm5.state_history[5], State::Idle)
  
  // Test state machine with additional context
  type ContextualStateMachine = {
    current_state: State,
    state_history: Array[State],
    context: String,
    error_message: Option[String]
  }
  
  let contextual_transition = fn(state_machine: ContextualStateMachine, event: Event, context: String) {
    let new_state = match (state_machine.current_state, event) {
      (State::Idle, Event::Start) => State::Active
      (State::Active, Event::Pause) => State::Paused
      (State::Active, Event::Complete) => State::Completed
      (State::Active, Event::Fail) => State::Error
      (State::Paused, Event::Resume) => State::Active
      (State::Paused, Event::Fail) => State::Error
      (State::Completed, Event::Reset) => State::Idle
      (State::Error, Event::Reset) => State::Idle
      _ => state_machine.current_state
    }
    
    let error_message = if new_state == State::Error {
      Some("Error occurred during " + context)
    } else {
      None
    }
    
    {
      current_state: new_state,
      state_history: state_machine.state_history.push(new_state),
      context,
      error_message
    }
  }
  
  let contextual_sm = {
    current_state: State::Idle,
    state_history: [State::Idle],
    context: "",
    error_message: None
  }
  
  let csm1 = contextual_transition(contextual_sm, Event::Start, "starting process")
  let csm2 = contextual_transition(csm1, Event::Fail, "processing data")
  
  assert_eq(csm2.current_state, State::Error)
  assert_eq(csm2.context, "processing data")
  assert_eq(csm2.error_message, Some("Error occurred during processing data"))
}

// Test 9: Pipeline Pattern
test "pipeline pattern" {
  // Define pipeline stage function
  type PipelineStage[T] = (T) -> T
  
  // Create pipeline executor
  let execute_pipeline = fn(initial_value: T, stages: Array[PipelineStage[T]]) {
    let mut result = initial_value
    
    for stage in stages {
      result = stage(result)
    }
    
    result
  }
  
  // Test with string processing pipeline
  let string_stages = [
    fn(s: String) { s.to_uppercase() },
    fn(s: String) { s + "_PROCESSED" },
    fn(s: String) { "PREFIX_" + s }
  ]
  
  let string_result = execute_pipeline("test", string_stages)
  assert_eq(string_result, "PREFIX_TEST_PROCESSED")
  
  // Test with numeric processing pipeline
  let number_stages = [
    fn(n: Int) { n * 2 },
    fn(n: Int) { n + 10 },
    fn(n: Int) { n / 3 }
  ]
  
  let number_result = execute_pipeline(5, number_stages)
  assert_eq(number_result, 6)  // ((5 * 2) + 10) / 3 = 20 / 3 = 6 (integer division)
  
  // Test conditional pipeline stages
  let conditional_stage = fn(condition: Bool, true_stage: PipelineStage[T], false_stage: PipelineStage[T]) {
    fn(value: T) {
      if condition {
        true_stage(value)
      } else {
        false_stage(value)
      }
    }
  }
  
  let conditional_stages = [
    conditional_stage(true, fn(n: Int) { n * 2 }, fn(n: Int) { n + 2 }),
    conditional_stage(false, fn(n: Int) { n * 3 }, fn(n: Int) { n + 3 }),
    conditional_stage(true, fn(n: Int) { n - 1 }, fn(n: Int) { n - 2 })
  ]
  
  let conditional_result = execute_pipeline(10, conditional_stages)
  assert_eq(conditional_result, 24)  // ((10 * 2) + 3) - 1 = 23 - 1 = 22
  
  // Test pipeline with error handling
  type PipelineResult[T] = {
    success: Bool,
    value: Option[T],
    error: Option[String]
  }
  
  let safe_stage = fn(stage: PipelineStage[T], error_message: String) {
    fn(input: T) {
      // Simulate potential error condition
      if input == 0 {
        {
          success: false,
          value: None,
          error: Some(error_message)
        }
      } else {
        let output = stage(input)
        {
          success: true,
          value: Some(output),
          error: None
        }
      }
    }
  }
  
  let execute_safe_pipeline = fn(initial_value: T, stages: Array[(T) -> PipelineResult[T]]) {
    let mut current_result = {
      success: true,
      value: Some(initial_value),
      error: None
    }
    
    for stage in stages {
      if current_result.success {
        match current_result.value {
          Some(value) => {
            current_result = stage(value)
          }
          None => {
            current_result = {
              success: false,
              value: None,
              error: Some("No value to process")
            }
          }
        }
      }
    }
    
    current_result
  }
  
  let safe_stages = [
    safe_stage(fn(n: Int) { n * 2 }, "Error in stage 1"),
    safe_stage(fn(n: Int) { n + 5 }, "Error in stage 2"),
    safe_stage(fn(n: Int) { n / 0 }, "Error in stage 3")  // This would cause an error
  ]
  
  let safe_result = execute_safe_pipeline(10, safe_stages)
  assert_false(safe_result.success)
  assert_eq(safe_result.error, Some("Error in stage 3"))
  
  // Test pipeline with successful execution
  let successful_stages = [
    safe_stage(fn(n: Int) { n * 2 }, "Error in stage 1"),
    safe_stage(fn(n: Int) { n + 5 }, "Error in stage 2"),
    safe_stage(fn(n: Int) { n - 3 }, "Error in stage 3")
  ]
  
  let successful_result = execute_safe_pipeline(10, successful_stages)
  assert_true(successful_result.success)
  assert_eq(successful_result.value, Some(22))  // ((10 * 2) + 5) - 3 = 25 - 3 = 22
}

// Test 10: Resource Management
test "resource management" {
  // Define resource types
  enum ResourceType {
    DatabaseConnection
    FileHandle
    NetworkSocket
    MemoryBlock
  }
  
  // Define resource state
  enum ResourceState {
    Available
    InUse
    Released
    Error
  }
  
  // Define resource
  type Resource = {
    id: String,
    resource_type: ResourceType,
    state: ResourceState,
    created_at: Int,
    last_accessed: Int
  }
  
  // Define resource manager
  type ResourceManager = {
    resources: Array[Resource],
    resource_limits: (ResourceType) -> Int,
    current_time: Int
  }
  
  // Create resource manager
  let create_resource_manager = fn() {
    {
      resources: [],
      resource_limits: fn(resource_type: ResourceType) {
        match resource_type {
          ResourceType::DatabaseConnection => 5
          ResourceType::FileHandle => 10
          ResourceType::NetworkSocket => 20
          ResourceType::MemoryBlock => 100
        }
      },
      current_time: 0
    }
  }
  
  // Acquire resource
  let acquire_resource = fn(resource_manager: ResourceManager, resource_type: ResourceType) {
    let limit = resource_manager.resource_limits(resource_type)
    let mut current_count = 0
    
    for resource in resource_manager.resources {
      if resource.resource_type == resource_type and resource.state != ResourceState::Released {
        current_count = current_count + 1
      }
    }
    
    if current_count < limit {
      let new_resource = {
        id: resource_type.to_string() + "_" + (resource_manager.current_time).to_string(),
        resource_type,
        state: ResourceState::InUse,
        created_at: resource_manager.current_time,
        last_accessed: resource_manager.current_time
      }
      
      (Some(new_resource.id), {
        resources: resource_manager.resources.push(new_resource),
        resource_limits: resource_manager.resource_limits,
        current_time: resource_manager.current_time + 1
      })
    } else {
      (None, {
        resources: resource_manager.resources,
        resource_limits: resource_manager.resource_limits,
        current_time: resource_manager.current_time + 1
      })
    }
  }
  
  // Release resource
  let release_resource = fn(resource_manager: ResourceManager, resource_id: String) {
    let mut updated_resources = []
    let mut found = false
    
    for resource in resource_manager.resources {
      if resource.id == resource_id {
        found = true
        updated_resources = updated_resources.push({
          resource | 
          state: ResourceState::Released,
          last_accessed: resource_manager.current_time
        })
      } else {
        updated_resources = updated_resources.push(resource)
      }
    }
    
    (found, {
      resources: updated_resources,
      resource_limits: resource_manager.resource_limits,
      current_time: resource_manager.current_time + 1
    })
  }
  
  // Get resource by ID
  let get_resource = fn(resource_manager: ResourceManager, resource_id: String) {
    let mut found_resource = None
    
    for resource in resource_manager.resources {
      if resource.id == resource_id {
        found_resource = Some(resource)
      }
    }
    
    found_resource
  }
  
  // Test resource management
  let resource_manager = create_resource_manager()
  
  // Acquire database connections
  let (db1_id, rm1) = acquire_resource(resource_manager, ResourceType::DatabaseConnection)
  let (db2_id, rm2) = acquire_resource(rm1, ResourceType::DatabaseConnection)
  let (db3_id, rm3) = acquire_resource(rm2, ResourceType::DatabaseConnection)
  
  assert_eq(db1_id, Some("DatabaseConnection_0"))
  assert_eq(db2_id, Some("DatabaseConnection_1"))
  assert_eq(db3_id, Some("DatabaseConnection_2"))
  
  // Acquire file handles
  let (file1_id, rm4) = acquire_resource(rm3, ResourceType::FileHandle)
  let (file2_id, rm5) = acquire_resource(rm4, ResourceType::FileHandle)
  
  assert_eq(file1_id, Some("FileHandle_3"))
  assert_eq(file2_id, Some("FileHandle_4"))
  
  // Check resource states
  let db1_resource = get_resource(rm5, db1_id.unwrap())
  let file1_resource = get_resource(rm5, file1_id.unwrap())
  
  match db1_resource {
    Some(resource) => assert_eq(resource.state, ResourceState::InUse)
    None => assert_true(false)
  }
  
  match file1_resource {
    Some(resource) => assert_eq(resource.state, ResourceState::InUse)
    None => assert_true(false)
  }
  
  // Release a resource
  let (release_success, rm6) = release_resource(rm5, db1_id.unwrap())
  assert_true(release_success)
  
  let released_db1_resource = get_resource(rm6, db1_id.unwrap())
  match released_db1_resource {
    Some(resource) => assert_eq(resource.state, ResourceState::Released)
    None => assert_true(false)
  }
  
  // Test resource limits
  let mut rm = rm6
  let mut db_ids = []
  
  // Acquire remaining database connections up to the limit
  for i in 0..4 {
    let (id, new_rm) = acquire_resource(rm, ResourceType::DatabaseConnection)
    match id {
      Some(resource_id) => db_ids = db_ids.push(resource_id)
      None => {}
    }
    rm = new_rm
  }
  
  // Try to acquire one more (should fail due to limit)
  let (excess_id, _) = acquire_resource(rm, ResourceType::DatabaseConnection)
  assert_eq(excess_id, None)
  
  // Test resource cleanup
  let cleanup_expired_resources = fn(resource_manager: ResourceManager, max_age: Int) {
    let mut updated_resources = []
    
    for resource in resource_manager.resources {
      let age = resource_manager.current_time - resource.last_accessed
      if resource.state == ResourceState::Released and age > max_age {
        // Skip expired resources (don't add to updated list)
      } else {
        updated_resources = updated_resources.push(resource)
      }
    }
    
    {
      resources: updated_resources,
      resource_limits: resource_manager.resource_limits,
      current_time: resource_manager.current_time + 10  // Advance time
    }
  }
  
  let rm_after_cleanup = cleanup_expired_resources(rm, 5)
  
  // Released resources should be cleaned up
  let cleaned_up_resource = get_resource(rm_after_cleanup, db1_id.unwrap())
  assert_eq(cleaned_up_resource, None)
  
  // Active resources should remain
  let active_resource = get_resource(rm_after_cleanup, db_ids[0])
  assert_not_eq(active_resource, None)
}