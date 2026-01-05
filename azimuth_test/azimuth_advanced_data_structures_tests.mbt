// Azimuth Advanced Data Structures Test Suite
// This file contains test cases for advanced data structures functionality

// Test 1: Trie (Prefix Tree) Implementation
test "trie prefix tree implementation" {
  // Define trie node structure
  type TrieNode = {
    children: Array[(Char, TrieNode)],
    is_end_of_word: Bool,
    value: Option[String]
  }
  
  // Create empty trie node
  let create_empty_node = fn() {
    {
      children: [],
      is_end_of_word: false,
      value: None
    }
  }
  
  // Insert a word into trie
  let insert = fn(root: TrieNode, word: String, value: String) {
    let mut current = root
    
    for i in 0..word.length() {
      let char = word.to_char_array()[i]
      
      // Check if character exists in children
      let existing_child = current.children.find(fn(child) { child.0 == char })
      
      match existing_child {
        Some(_) => {
          // Character already exists, move to it
          current = current.children.map(fn(child) {
            if child.0 == char {
              child
            } else {
              child
            }
          })[0].1
        }
        None => {
          // Create new child node
          let new_node = create_empty_node()
          current = {
            current |
            children: current.children.push((char, new_node))
          }.children.filter(fn(child) { child.0 == char })[0].1
        }
      }
    }
    
    // Mark last node as end of word and store value
    {
      current |
      is_end_of_word: true,
      value: Some(value)
    }
  }
  
  // Search for a word in trie
  let search = fn(root: TrieNode, word: String) {
    let mut current = root
    
    for i in 0..word.length() {
      let char = word.to_char_array()[i]
      
      // Check if character exists in children
      let existing_child = current.children.find(fn(child) { child.0 == char })
      
      match existing_child {
        Some(child) => {
          current = child.1
        }
        None => {
          return None  // Word not found
        }
      }
    }
    
    // Check if it's the end of a word
    if current.is_end_of_word {
      current.value
    } else {
      None
    }
  }
  
  // Check if any word starts with prefix
  let starts_with = fn(root: TrieNode, prefix: String) {
    let mut current = root
    
    for i in 0..prefix.length() {
      let char = prefix.to_char_array()[i]
      
      // Check if character exists in children
      let existing_child = current.children.find(fn(child) { child.0 == char })
      
      match existing_child {
        Some(child) => {
          current = child.1
        }
        None => {
          return false  // Prefix not found
        }
      }
    }
    
    true  // Prefix found
  }
  
  // Create trie and insert words
  let root = create_empty_node()
  
  // Insert telemetry-related terms
  let trie1 = insert(root, "trace", "Distributed tracing data")
  let trie2 = insert(trie1, "metric", "Performance metrics")
  let trie3 = insert(trie2, "log", "Application logs")
  let trie4 = insert(trie3, "span", "Trace span data")
  let trie5 = insert(trie4, "tracing", "Tracing information")
  let trie6 = insert(trie5, "traceability", "System traceability")
  
  // Test search for existing words
  let trace_result = search(trie6, "trace")
  assert_true(trace_result.is_some())
  assert_eq(trace_result.unwrap(), "Distributed tracing data")
  
  let metric_result = search(trie6, "metric")
  assert_true(metric_result.is_some())
  assert_eq(metric_result.unwrap(), "Performance metrics")
  
  let span_result = search(trie6, "span")
  assert_true(span_result.is_some())
  assert_eq(span_result.unwrap(), "Trace span data")
  
  // Test search for non-existing words
  let non_existent_result = search(trie6, "nonexistent")
  assert_true(non_existent_result.is_none())
  
  // Test prefix search
  assert_true(starts_with(trie6, "tra"))
  assert_true(starts_with(trie6, "trace"))
  assert_true(starts_with(trie6, "trac"))
  assert_false(starts_with(trie6, "xyz"))
  
  // Test prefix that is a complete word
  assert_true(starts_with(trie6, "log"))
  
  // Count words with given prefix
  let count_words_with_prefix = fn(root: TrieNode, prefix: String) {
    let mut current = root
    
    // Navigate to the prefix
    for i in 0..prefix.length() {
      let char = prefix.to_char_array()[i]
      
      let existing_child = current.children.find(fn(child) { child.0 == char })
      
      match existing_child {
        Some(child) => {
          current = child.1
        }
        None => {
          return 0  // Prefix not found
        }
      }
    }
    
    // Count all words from this node
    let count_words_from_node = fn(node: TrieNode) {
      let mut count = 0
      
      // Count this node if it's end of word
      if node.is_end_of_word {
        count = count + 1
      }
      
      // Recursively count in children
      for child in node.children {
        count = count + count_words_from_node(child.1)
      }
      
      count
    }
    
    count_words_from_node(current)
  }
  
  // Test word count with prefix
  assert_eq(count_words_with_prefix(trie6, "tr"), 3)  // trace, tracing, traceability
  assert_eq(count_words_with_prefix(trie6, "trace"), 2)  // trace, traceability
  assert_eq(count_words_with_prefix(trie6, "tracing"), 1)  // tracing
  assert_eq(count_words_with_prefix(trie6, "log"), 1)  // log
  assert_eq(count_words_with_prefix(trie6, "xyz"), 0)  // None
}

// Test 2: Bloom Filter Implementation
test "bloom filter implementation" {
  // Define bloom filter structure
  type BloomFilter = {
    bit_array: Array[Bool],
    size: Int,
    hash_count: Int,
    items_added: Int
  }
  
  // Simple hash functions for demonstration
  let hash1 = fn(item: String, size: Int) {
    let mut hash = 0
    let chars = item.to_char_array()
    
    for i in 0..chars.length() {
      hash = (hash * 31 + chars[i].to_int()) % size
    }
    
    if hash < 0 { hash + size } else { hash }
  }
  
  let hash2 = fn(item: String, size: Int) {
    let mut hash = 5381
    let chars = item.to_char_array()
    
    for i in 0..chars.length() {
      hash = ((hash << 5) + hash) + chars[i].to_int()  // hash * 33 + char
      hash = hash % size
    }
    
    if hash < 0 { hash + size } else { hash }
  }
  
  let hash3 = fn(item: String, size: Int) {
    let mut hash = 0
    let chars = item.to_char_array()
    
    for i in 0..chars.length() {
      hash = hash + chars[i].to_int() * (i + 1)
      hash = hash % size
    }
    
    if hash < 0 { hash + size } else { hash }
  }
  
  // Create bloom filter
  let create_bloom_filter = fn(size: Int, hash_count: Int) {
    {
      bit_array: [false; size],
      size,
      hash_count,
      items_added: 0
    }
  }
  
  // Add item to bloom filter
  let add = fn(filter: BloomFilter, item: String) {
    let mut new_bit_array = filter.bit_array
    
    // Apply hash functions and set bits
    let hash1_result = hash1(item, filter.size)
    let hash2_result = hash2(item, filter.size)
    let hash3_result = hash3(item, filter.size)
    
    new_bit_array[hash1_result] = true
    new_bit_array[hash2_result] = true
    new_bit_array[hash3_result] = true
    
    {
      bit_array: new_bit_array,
      size: filter.size,
      hash_count: filter.hash_count,
      items_added: filter.items_added + 1
    }
  }
  
  // Check if item might exist in bloom filter
  let contains = fn(filter: BloomFilter, item: String) {
    // Apply hash functions
    let hash1_result = hash1(item, filter.size)
    let hash2_result = hash2(item, filter.size)
    let hash3_result = hash3(item, filter.size)
    
    // Check if all bits are set
    filter.bit_array[hash1_result] and 
    filter.bit_array[hash2_result] and 
    filter.bit_array[hash3_result]
  }
  
  // Create bloom filter with size 100 and 3 hash functions
  let bloom_filter = create_bloom_filter(100, 3)
  
  // Add telemetry service names
  let filter1 = add(bloom_filter, "order-service")
  let filter2 = add(filter1, "payment-service")
  let filter3 = add(filter2, "user-service")
  let filter4 = add(filter3, "inventory-service")
  let filter5 = add(filter4, "notification-service")
  
  assert_eq(filter5.items_added, 5)
  
  // Test for existing items
  assert_true(contains(filter5, "order-service"))
  assert_true(contains(filter5, "payment-service"))
  assert_true(contains(filter5, "user-service"))
  assert_true(contains(filter5, "inventory-service"))
  assert_true(contains(filter5, "notification-service"))
  
  // Test for non-existing items
  assert_false(contains(filter5, "analytics-service"))
  assert_false(contains(filter5, "shipping-service"))
  assert_false(contains(filter5, "reporting-service"))
  
  // Test for false positives (possible with bloom filter)
  let filter6 = add(filter5, "auth-service")
  let filter7 = add(filter6, "config-service")
  
  // These might show false positives depending on hash collisions
  let possible_false_positive_1 = contains(filter7, "unknown-service-1")
  let possible_false_positive_2 = contains(filter7, "unknown-service-2")
  
  // Calculate false positive rate (simplified)
  let calculate_false_positive_rate = fn(filter: BloomFilter, test_items: Array[String]) {
    let mut false_positives = 0
    
    for item in test_items {
      if contains(filter, item) {
        false_positives = false_positives + 1
      }
    }
    
    if test_items.length() > 0 {
      (false_positives.to_float() / test_items.length().to_float()) * 100.0
    } else {
      0.0
    }
  }
  
  let test_items = [
    "analytics-service",
    "shipping-service", 
    "reporting-service",
    "unknown-service-1",
    "unknown-service-2",
    "unknown-service-3",
    "unknown-service-4",
    "unknown-service-5"
  ]
  
  let false_positive_rate = calculate_false_positive_rate(filter7, test_items)
  
  // False positive rate should be relatively low for a good bloom filter
  assert_true(false_positive_rate < 50.0)  // Allow some false positives but not too many
  
  // Calculate current load factor
  let calculate_load_factor = fn(filter: BloomFilter) {
    let set_bits = filter.bit_array.filter(fn(bit) { bit }).length()
    (set_bits.to_float() / filter.size.to_float()) * 100.0
  }
  
  let load_factor = calculate_load_factor(filter7)
  assert_true(load_factor > 0.0)  // Some bits should be set
  assert_true(load_factor <= 100.0)  // Can't exceed 100%
}

// Test 3: Skip List Implementation
test "skip list implementation" {
  // Define skip list node structure
  type SkipListNode = {
    key: Int,
    value: String,
    forward: Array[Option[SkipListNode]]
  }
  
  type SkipList = {
    head: SkipListNode,
    max_level: Int,
    current_level: Int,
    size: Int
  }
  
  // Create skip list node
  let create_node = fn(key: Int, value: String, level: Int) {
    {
      key,
      value,
      forward: [None; level + 1]
    }
  }
  
  // Create skip list
  let create_skip_list = fn(max_level: Int) {
    let head = create_node(-1, "HEAD", max_level)
    
    {
      head,
      max_level,
      current_level: 0,
      size: 0
    }
  }
  
  // Random level generator (simplified)
  let random_level = fn(max_level: Int) {
    let mut level = 0
    
    // 50% chance to increase level
    while level < max_level and (level % 2 == 0) {
      level = level + 1
    }
    
    level
  }
  
  // Insert key-value pair into skip list
  let insert = fn(list: SkipList, key: Int, value: String) {
    let level = random_level(list.max_level)
    
    // Create new node
    let new_node = create_node(key, value, level)
    
    // Update list level if needed
    let updated_list = if level > list.current_level {
      { list | current_level: level }
    } else {
      list
    }
    
    // Find insertion point and update pointers
    let mut current = updated_list.head
    
    for i in updated_list.current_level..0 {
      while match current.forward[i] {
        Some(node) => node.key < key
        None => false
      } {
        current = current.forward[i].unwrap_or(updated_list.head)
      }
      
      // Update forward pointer
      new_node.forward[i] = current.forward[i]
      current.forward[i] = Some(new_node)
    }
    
    {
      updated_list |
      size: updated_list.size + 1
    }
  }
  
  // Search for key in skip list
  let search = fn(list: SkipList, key: Int) {
    let mut current = list.head
    
    // Start from highest level
    for i in list.current_level..0 {
      while match current.forward[i] {
        Some(node) => node.key < key
        None => false
      } {
        current = current.forward[i].unwrap_or(list.head)
      }
    }
    
    // Check next node at level 0
    match current.forward[0] {
      Some(node) => {
        if node.key == key {
          Some(node.value)
        } else {
          None
        }
      }
      None => None
    }
  }
  
  // Delete key from skip list
  let delete = fn(list: SkipList, key: Int) {
    let mut current = list.head
    let mut update = [None; list.max_level + 1]
    
    // Find node to delete
    for i in list.current_level..0 {
      while match current.forward[i] {
        Some(node) => node.key < key
        None => false
      } {
        current = current.forward[i].unwrap_or(list.head)
      }
      
      update[i] = Some(current)
    }
    
    // Check if node exists
    match current.forward[0] {
      Some(node) => {
        if node.key == key {
          // Remove node from list
          for i in 0..list.current_level + 1 {
            match update[i] {
              Some(update_node) => {
                if match update_node.forward[i] {
                  Some(forward_node) => forward_node.key == key
                  None => false
                } {
                  update_node.forward[i] = node.forward[i]
                }
              }
              None => {}
            }
          }
          
          // Update current level if needed
          let mut new_current_level = list.current_level
          while new_current_level > 0 and 
                match list.head.forward[new_current_level] {
                  Some(_) => false
                  None => true
                } {
            new_current_level = new_current_level - 1
          }
          
          {
            list |
            current_level: new_current_level,
            size: list.size - 1
          }
        } else {
          // Key not found
          list
        }
      }
      None => list
    }
  }
  
  // Create skip list
  let skip_list = create_skip_list(4)
  
  // Insert key-value pairs
  let list1 = insert(skip_list, 10, "trace-001")
  let list2 = insert(list1, 20, "metric-001")
  let list3 = insert(list2, 30, "log-001")
  let list4 = insert(list3, 40, "span-001")
  let list5 = insert(list4, 25, "trace-002")
  
  assert_eq(list5.size, 5)
  
  // Test search for existing keys
  let result1 = search(list5, 10)
  assert_true(result1.is_some())
  assert_eq(result1.unwrap(), "trace-001")
  
  let result2 = search(list5, 30)
  assert_true(result2.is_some())
  assert_eq(result2.unwrap(), "log-001")
  
  let result3 = search(list5, 25)
  assert_true(result3.is_some())
  assert_eq(result3.unwrap(), "trace-002")
  
  // Test search for non-existing keys
  let result4 = search(list5, 15)
  assert_true(result4.is_none())
  
  let result5 = search(list5, 50)
  assert_true(result5.is_none())
  
  // Test delete
  let list6 = delete(list5, 20)
  assert_eq(list6.size, 4)
  
  let deleted_result = search(list6, 20)
  assert_true(deleted_result.is_none())
  
  // Verify other keys still exist
  let remaining_result1 = search(list6, 10)
  assert_true(remaining_result1.is_some())
  assert_eq(remaining_result1.unwrap(), "trace-001")
  
  let remaining_result2 = search(list6, 30)
  assert_true(remaining_result2.is_some())
  assert_eq(remaining_result2.unwrap(), "log-001")
  
  // Calculate skip list performance metrics
  let calculate_performance_metrics = fn(list: SkipList) {
    let theoretical_height = (list.size.to_float().log2() + 1.0).to_int()
    let actual_height = list.current_level + 1
    
    let space_efficiency = if theoretical_height > 0 {
      (actual_height.to_float() / theoretical_height.to_float()) * 100.0
    } else {
      100.0
    }
    
    {
      size: list.size,
      theoretical_height,
      actual_height,
      space_efficiency_percent: space_efficiency
    }
  }
  
  let performance_metrics = calculate_performance_metrics(list6)
  assert_eq(performance_metrics.size, 4)
  assert_true(performance_metrics.actual_height > 0)
  assert_true(performance_metrics.space_efficiency_percent > 0.0)
}

// Test 4: LRU Cache Implementation
test "lru cache implementation" {
  // Define doubly linked list node
  type LRUNode = {
    key: String,
    value: String,
    prev: Option[LRUNode],
    next: Option[LRUNode>
  }
  
  // Define LRU cache
  type LRUCache = {
    capacity: Int,
    size: Int,
    map: Array[(String, LRUNode)>,
    head: Option[LRUNode>,
    tail: Option[LRUNode>
  }
  
  // Create LRU cache
  let create_lru_cache = fn(capacity: Int) {
    {
      capacity,
      size: 0,
      map: [],
      head: None,
      tail: None
    }
  }
  
  // Create new node
  let create_node = fn(key: String, value: String) {
    {
      key,
      value,
      prev: None,
      next: None
    }
  }
  
  // Add node to front of list (most recently used)
  let add_to_front = fn(cache: LRUCache, node: LRUNode) {
    match cache.head {
      None => {
        // Empty list
        {
          cache |
          head: Some(node),
          tail: Some(node)
        }
      }
      Some(head) => {
        // Non-empty list
        let updated_node = { node | next: Some(head) }
        let updated_head = { head | prev: Some(updated_node) }
        
        {
          cache |
          head: Some(updated_node)
        }
      }
    }
  }
  
  // Remove node from list
  let remove_node = fn(cache: LRUCache, node: LRUNode) {
    match (node.prev, node.next) {
      (None, None) => {
        // Only node in list
        {
          cache |
          head: None,
          tail: None
        }
      }
      
      (None, Some(next)) => {
        // Head node
        let updated_next = { next | prev: None }
        
        {
          cache |
          head: Some(updated_next)
        }
      }
      
      (Some(prev), None) => {
        // Tail node
        let updated_prev = { prev | next: None }
        
        {
          cache |
          tail: Some(updated_prev)
        }
      }
      
      (Some(prev), Some(next)) => {
        // Middle node
        let updated_prev = { prev | next: Some(next) }
        let updated_next = { next | prev: Some(updated_prev) }
        
        cache  // No change to head/tail
      }
    }
  }
  
  // Move node to front (most recently used)
  let move_to_front = fn(cache: LRUCache, node: LRUNode) {
    let cache_without_node = remove_node(cache, node)
    let isolated_node = { node | prev: None, next: None }
    add_to_front(cache_without_node, isolated_node)
  }
  
  // Get value from cache
  let get = fn(cache: LRUCache, key: String) {
    let node_entry = cache.map.find(fn(entry) { entry.0 == key })
    
    match node_entry {
      Some(entry) => {
        // Move to front (most recently used)
        let updated_cache = move_to_front(cache, entry.1)
        
        // Update map reference
        let updated_map = updated_cache.map.map(fn(map_entry) {
          if map_entry.0 == key {
            (key, updated_cache.head.unwrap_or(entry.1))
          } else {
            map_entry
          }
        })
        
        (Some(entry.1.value), { updated_cache | map: updated_map })
      }
      None => (None, cache)
    }
  }
  
  // Put value in cache
  let put = fn(cache: LRUCache, key: String, value: String) {
    let node_entry = cache.map.find(fn(entry) { entry.0 == key })
    
    match node_entry {
      Some(entry) => {
        // Update existing node
        let updated_node = { entry.1 | value }
        let updated_cache = move_to_front(cache, updated_node)
        
        // Update map reference
        let updated_map = updated_cache.map.map(fn(map_entry) {
          if map_entry.0 == key {
            (key, updated_cache.head.unwrap_or(updated_node))
          } else {
            map_entry
          }
        })
        
        { updated_cache | map: updated_map }
      }
      
      None => {
        // Add new node
        let new_node = create_node(key, value)
        
        if cache.size >= cache.capacity {
          // Remove least recently used (tail)
          match cache.tail {
            Some(tail) => {
              let cache_without_tail = remove_node(cache, tail)
              let cache_with_new = add_to_front(cache_without_tail, new_node)
              
              // Update map
              let updated_map = cache_with_new.map
                .filter(fn(entry) { entry.0 != tail.key })
                .push((key, cache_with_new.head.unwrap_or(new_node)))
              
              {
                cache_with_new |
                map: updated_map,
                size: cache.size  // Size stays the same
              }
            }
            
            None => {
              // Empty list (shouldn't happen if size >= capacity)
              let cache_with_new = add_to_front(cache, new_node)
              
              {
                cache_with_new |
                map: cache_with_new.map.push((key, cache_with_new.head.unwrap_or(new_node))),
                size: cache.size + 1
              }
            }
          }
        } else {
          // Add to front
          let cache_with_new = add_to_front(cache, new_node)
          
          {
            cache_with_new |
            map: cache_with_new.map.push((key, cache_with_new.head.unwrap_or(new_node))),
            size: cache.size + 1
          }
        }
      }
    }
  }
  
  // Create LRU cache with capacity 3
  let lru_cache = create_lru_cache(3)
  
  // Add items
  let cache1 = put(lru_cache, "trace-001", "Distributed tracing data")
  let cache2 = put(cache1, "metric-001", "Performance metrics")
  let cache3 = put(cache2, "log-001", "Application logs")
  
  assert_eq(cache3.size, 3)
  
  // Get existing item (should move to front)
  let (value1, cache4) = get(cache3, "metric-001")
  assert_true(value1.is_some())
  assert_eq(value1.unwrap(), "Performance metrics")
  assert_eq(cache4.size, 3)
  
  // Add another item (should evict least recently used)
  let cache5 = put(cache4, "span-001", "Trace span data")
  assert_eq(cache5.size, 3)
  
  // Check that trace-001 was evicted
  let (evicted_value, _) = get(cache5, "trace-001")
  assert_true(evicted_value.is_none())
  
  // Check that other items still exist
  let (value2, _) = get(cache5, "metric-001")
  assert_true(value2.is_some())
  assert_eq(value2.unwrap(), "Performance metrics")
  
  let (value3, _) = get(cache5, "log-001")
  assert_true(value3.is_some())
  assert_eq(value3.unwrap(), "Application logs")
  
  let (value4, _) = get(cache5, "span-001")
  assert_true(value4.is_some())
  assert_eq(value4.unwrap(), "Trace span data")
  
  // Update existing item
  let cache6 = put(cache5, "metric-001", "Updated performance metrics")
  assert_eq(cache6.size, 3)
  
  let (updated_value, _) = get(cache6, "metric-001")
  assert_true(updated_value.is_some())
  assert_eq(updated_value.unwrap(), "Updated performance metrics")
  
  // Calculate cache statistics
  let calculate_cache_stats = fn(cache: LRUCache) {
    let utilization_rate = if cache.capacity > 0 {
      (cache.size.to_float() / cache.capacity.to_float()) * 100.0
    } else {
      0.0
    }
    
    {
      capacity: cache.capacity,
      size: cache.size,
      utilization_rate_percent: utilization_rate
    }
  }
  
  let cache_stats = calculate_cache_stats(cache6)
  assert_eq(cache_stats.capacity, 3)
  assert_eq(cache_stats.size, 3)
  assert_eq(cache_stats.utilization_rate_percent, 100.0)
}