// Advanced Data Structures Operations Tests
// This file contains comprehensive test cases for advanced data structures operations

// Test 1: Trie-based Attribute Storage
test "trie-based attribute storage" {
  let trie = AttributeTrie::new()
  
  // Test insertion
  AttributeTrie::insert(trie, "service.name", StringValue("auth-service"))
  AttributeTrie::insert(trie, "service.version", StringValue("1.0.0"))
  AttributeTrie::insert(trie, "service.instance.id", StringValue("instance-123"))
  AttributeTrie::insert(trie, "http.method", StringValue("GET"))
  AttributeTrie::insert(trie, "http.status_code", IntValue(200))
  AttributeTrie::insert(trie, "http.url", StringValue("/api/users"))
  
  // Test exact match lookup
  let service_name = AttributeTrie::get(trie, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "auth-service"),
    _ => assert_true(false)
  }
  
  let status_code = AttributeTrie::get(trie, "http.status_code")
  match status_code {
    Some(IntValue(code)) => assert_eq(code, 200),
    _ => assert_true(false)
  }
  
  // Test prefix search
  let service_attrs = AttributeTrie::search_by_prefix(trie, "service.")
  assert_eq(service_attrs.length(), 3)
  
  let http_attrs = AttributeTrie::search_by_prefix(trie, "http.")
  assert_eq(http_attrs.length(), 3)
  
  // Test non-existent key
  let non_existent = AttributeTrie::get(trie, "non.existent.key")
  assert_true(non_existent.is_none())
  
  // Test deletion
  AttributeTrie::delete(trie, "service.version")
  let deleted_version = AttributeTrie::get(trie, "service.version")
  assert_true(deleted_version.is_none())
  
  // Verify other keys still exist
  let remaining_service_attrs = AttributeTrie::search_by_prefix(trie, "service.")
  assert_eq(remaining_service_attrs.length(), 2)
}

// Test 2: Circular Buffer for Telemetry Events
test "circular buffer for telemetry events" {
  let buffer = CircularBuffer::new(5) // Buffer with capacity of 5
  
  // Test insertion when buffer is not full
  CircularBuffer::push(buffer, TelemetryEvent::new("event1", 1000L))
  CircularBuffer::push(buffer, TelemetryEvent::new("event2", 2000L))
  CircularBuffer::push(buffer, TelemetryEvent::new("event3", 3000L))
  
  assert_eq(CircularBuffer::size(buffer), 3)
  assert_eq(CircularBuffer::is_full(buffer), false)
  
  // Test retrieval
  let event1 = CircularBuffer::get(buffer, 0)
  match event1 {
    Some(event) => assert_eq(TelemetryEvent::name(event), "event1"),
    None => assert_true(false)
  }
  
  // Test insertion when buffer becomes full
  CircularBuffer::push(buffer, TelemetryEvent::new("event4", 4000L))
  CircularBuffer::push(buffer, TelemetryEvent::new("event5", 5000L))
  
  assert_eq(CircularBuffer::size(buffer), 5)
  assert_eq(CircularBuffer::is_full(buffer), true)
  
  // Test circular behavior (oldest should be overwritten)
  CircularBuffer::push(buffer, TelemetryEvent::new("event6", 6000L))
  
  assert_eq(CircularBuffer::size(buffer), 5)
  assert_eq(CircularBuffer::is_full(buffer), true)
  
  // event1 should be overwritten by event6
  let oldest_event = CircularBuffer::get(buffer, 0)
  match oldest_event {
    Some(event) => assert_eq(TelemetryEvent::name(event), "event2"),
    None => assert_true(false)
  }
  
  let newest_event = CircularBuffer::get(buffer, 4)
  match newest_event {
    Some(event) => assert_eq(TelemetryEvent::name(event), "event6"),
    None => assert_true(false)
  }
  
  // Test iteration
  let event_names = []
  for event in CircularBuffer::iterator(buffer) {
    event_names = event_names + [TelemetryEvent::name(event)]
  }
  
  assert_eq(event_names, ["event2", "event3", "event4", "event5", "event6"])
  
  // Test clear
  CircularBuffer::clear(buffer)
  assert_eq(CircularBuffer::size(buffer), 0)
  assert_eq(CircularBuffer::is_empty(buffer), true)
}

// Test 3: Bloom Filter for Duplicate Detection
test "bloom filter for duplicate detection" {
  let bloom_filter = BloomFilter::new(1000, 0.01) // 1000 items, 1% false positive rate
  
  // Test insertion
  BloomFilter::add(bloom_filter, "trace1")
  BloomFilter::add(bloom_filter, "trace2")
  BloomFilter::add(bloom_filter, "trace3")
  
  // Test positive cases (definitely might be present)
  assert_true(BloomFilter::might_contain(bloom_filter, "trace1"))
  assert_true(BloomFilter::might_contain(bloom_filter, "trace2"))
  assert_true(BloomFilter::might_contain(bloom_filter, "trace3"))
  
  // Test negative cases (definitely not present)
  assert_false(BloomFilter::might_contain(bloom_filter, "trace4"))
  assert_false(BloomFilter::might_contain(bloom_filter, "trace5"))
  
  // Test with many items
  for i in 0..1000 {
    BloomFilter::add(bloom_filter, "trace_" + i.to_string())
  }
  
  // All inserted items should be detected
  for i in 0..1000 {
    assert_true(BloomFilter::might_contain(bloom_filter, "trace_" + i.to_string()))
  }
  
  // Test false positive rate
  let false_positives = 0
  let false_positive_tests = 1000
  
  for i in 1000..2000 {
    if BloomFilter::might_contain(bloom_filter, "trace_" + i.to_string()) {
      false_positives = false_positives + 1
    }
  }
  
  let actual_false_positive_rate = (false_positives as Float) / (false_positive_tests as Float)
  assert_true(actual_false_positive_rate < 0.02) // Should be close to 1%
  
  // Test bloom filter statistics
  let stats = BloomFilter::get_statistics(bloom_filter)
  assert_eq(BloomFilterStats::items_added(stats), 1003)
  assert_eq(BloomFilterStats::capacity(stats), 1000)
  assert_true(BloomFilterStats::false_positive_rate(stats) < 0.02)
}

// Test 4: Skip List for Time-Ordered Data
test "skip list for time-ordered data" {
  let skip_list = SkipList::new()
  
  // Test insertion
  let base_time = 1640995200L // January 1, 2022 00:00:00 UTC
  
  SkipList::insert(skip_list, TelemetryData::with_timestamp("metric1", 10.0, "unit", base_time + 100L))
  SkipList::insert(skip_list, TelemetryData::with_timestamp("metric2", 20.0, "unit", base_time + 50L))
  SkipList::insert(skip_list, TelemetryData::with_timestamp("metric3", 30.0, "unit", base_time + 150L))
  SkipList::insert(skip_list, TelemetryData::with_timestamp("metric4", 40.0, "unit", base_time + 25L))
  SkipList::insert(skip_list, TelemetryData::with_timestamp("metric5", 50.0, "unit", base_time + 75L))
  
  // Test size
  assert_eq(SkipList::size(skip_list), 5)
  
  // Test ordered traversal
  let ordered_values = []
  for item in SkipList::iterator(skip_list) {
    ordered_values = ordered_values + [TelemetryData::value(item)]
  }
  
  // Values should be in timestamp order
  assert_eq(ordered_values, [40.0, 20.0, 50.0, 10.0, 30.0])
  
  // Test range query
  let range_items = SkipList::range_query(skip_list, base_time + 40L, base_time + 120L)
  let range_values = []
  for item in range_items {
    range_values = range_values + [TelemetryData::value(item)]
  }
  
  assert_eq(range_values, [50.0, 10.0])
  
  // Test search
  let search_result = SkipList::find(skip_list, base_time + 50L)
  match search_result {
    Some(item) => assert_eq(TelemetryData::value(item), 20.0),
    None => assert_true(false)
  }
  
  let non_existent = SkipList::find(skip_list, base_time + 999L)
  assert_true(non_existent.is_none())
  
  // Test deletion
  SkipList::delete(skip_list, base_time + 50L)
  assert_eq(SkipList::size(skip_list), 4)
  
  let deleted_search = SkipList::find(skip_list, base_time + 50L)
  assert_true(deleted_search.is_none())
  
  // Test performance characteristics
  let performance_start = PerformanceTimer::start()
  
  // Insert many items
  for i in 0..10000 {
    let timestamp = base_time + (i * 10)
    SkipList::insert(skip_list, TelemetryData::with_timestamp("perf_metric", i.to_int() as Float, "unit", timestamp))
  }
  
  let performance_end = PerformanceTimer::end()
  let insertion_time = PerformanceTimer::elapsed_ms(performance_start, performance_end)
  
  // Skip list should handle large insertions efficiently
  assert_true(insertion_time < 1000.0) // Less than 1 second for 10000 insertions
  assert_eq(SkipList::size(skip_list), 10004) // 5 original + 10000 new - 1 deleted
}

// Test 5: LRU Cache for Telemetry Data
test "lru cache for telemetry data" {
  let cache = LRUCache::new(3) // Cache with capacity of 3
  
  // Test insertion
  LRUCache::put(cache, "key1", "value1")
  LRUCache::put(cache, "key2", "value2")
  LRUCache::put(cache, "key3", "value3")
  
  // Test retrieval
  let value1 = LRUCache::get(cache, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1"),
    None => assert_true(false)
  }
  
  // Test LRU eviction
  LRUCache::put(cache, "key4", "value4") // Should evict key2 (least recently used)
  
  let value2 = LRUCache::get(cache, "key2")
  assert_true(value2.is_none()) // key2 should be evicted
  
  let value4 = LRUCache::get(cache, "key4")
  match value4 {
    Some(v) => assert_eq(v, "value4"),
    None => assert_true(false)
  }
  
  // Test access updates LRU order
  let value1 = LRUCache::get(cache, "key1") // Now key1 is most recently used
  
  LRUCache::put(cache, "key5", "value5") // Should evict key3
  
  let value3 = LRUCache::get(cache, "key3")
  assert_true(value3.is_none()) // key3 should be evicted
  
  // Verify cache contains expected keys
  assert_eq(LRUCache::size(cache), 3)
  assert_true(LRUCache::contains(cache, "key1"))
  assert_true(LRUCache::contains(cache, "key4"))
  assert_true(LRUCache::contains(cache, "key5"))
  
  // Test update existing key
  LRUCache::put(cache, "key1", "updated_value1")
  let updated_value1 = LRUCache::get(cache, "key1")
  match updated_value1 {
    Some(v) => assert_eq(v, "updated_value1"),
    None => assert_true(false)
  }
  
  // Test clear
  LRUCache::clear(cache)
  assert_eq(LRUCache::size(cache), 0)
  assert_true(LRUCache::is_empty(cache))
}

// Test 6: HyperLogLog for Cardinality Estimation
test "hyperloglog for cardinality estimation" {
  let hll = HyperLogLog::new(12) // 12 bits for register indexing
  
  // Test with known small sets
  for i in 0..100 {
    HyperLogLog::add(hll, "item_" + i.to_string())
  }
  
  let estimate_100 = HyperLogLog::estimate(hll)
  assert_true((estimate_100 - 100).abs() < 10) // Should be close to 100
  
  // Test with larger sets
  for i in 100..10000 {
    HyperLogLog::add(hll, "item_" + i.to_string())
  }
  
  let estimate_10000 = HyperLogLog::estimate(hll)
  let error_rate = ((estimate_10000 - 10000) as Float) / 10000.0
  assert_true(error_rate.abs() < 0.02) // Should be within 2% error
  
  // Test merge operation
  let hll2 = HyperLogLog::new(12)
  for i in 10000..20000 {
    HyperLogLog::add(hll2, "item_" + i.to_string())
  }
  
  let estimate_hll2 = HyperLogLog::estimate(hll2)
  assert_true((estimate_hll2 - 10000).abs() < 200) // Should be close to 10000
  
  // Merge the two HLLs
  let merged_hll = HyperLogLog::merge(hll, hll2)
  let estimate_merged = HyperLogLog::estimate(merged_hll)
  
  // Should be approximately 20000
  let merged_error_rate = ((estimate_merged - 20000) as Float) / 20000.0
  assert_true(merged_error_rate.abs() < 0.02) // Should be within 2% error
  
  // Test with duplicate items
  let hll_duplicates = HyperLogLog::new(12)
  for i in 0..1000 {
    // Add each item 5 times
    for j in 0..5 {
      HyperLogLog::add(hll_duplicates, "item_" + i.to_string())
    }
  }
  
  let estimate_duplicates = HyperLogLog::estimate(hll_duplicates)
  // Should still be approximately 1000, not 5000
  let duplicate_error_rate = ((estimate_duplicates - 1000) as Float) / 1000.0
  assert_true(duplicate_error_rate.abs() < 0.02) // Should be within 2% error
}

// Test 7: Segment Tree for Range Aggregations
test "segment tree for range aggregations" {
  let values = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]
  let segment_tree = SegmentTree::new(values)
  
  // Test range sum queries
  let sum_0_3 = SegmentTree::range_sum(segment_tree, 0, 3)
  assert_eq(sum_0_3, 10.0) // 1 + 2 + 3 + 4
  
  let sum_2_5 = SegmentTree::range_sum(segment_tree, 2, 5)
  assert_eq(sum_2_5, 18.0) // 3 + 4 + 5 + 6
  
  let sum_0_7 = SegmentTree::range_sum(segment_tree, 0, 7)
  assert_eq(sum_0_7, 36.0) // Sum of all values
  
  // Test range min queries
  let min_0_3 = SegmentTree::range_min(segment_tree, 0, 3)
  assert_eq(min_0_3, 1.0)
  
  let min_2_5 = SegmentTree::range_min(segment_tree, 2, 5)
  assert_eq(min_2_5, 3.0)
  
  let min_4_7 = SegmentTree::range_min(segment_tree, 4, 7)
  assert_eq(min_4_7, 5.0)
  
  // Test range max queries
  let max_0_3 = SegmentTree::range_max(segment_tree, 0, 3)
  assert_eq(max_0_3, 4.0)
  
  let max_2_5 = SegmentTree::range_max(segment_tree, 2, 5)
  assert_eq(max_2_5, 6.0)
  
  let max_4_7 = SegmentTree::range_max(segment_tree, 4, 7)
  assert_eq(max_4_7, 8.0)
  
  // Test point updates
  SegmentTree::update(segment_tree, 3, 10.0) // Change value at index 3 from 4.0 to 10.0
  
  let new_sum_0_3 = SegmentTree::range_sum(segment_tree, 0, 3)
  assert_eq(new_sum_0_3, 16.0) // 1 + 2 + 3 + 10
  
  let new_max_0_3 = SegmentTree::range_max(segment_tree, 0, 3)
  assert_eq(new_max_0_3, 10.0)
  
  // Test range average queries
  let avg_0_3 = SegmentTree::range_average(segment_tree, 0, 3)
  assert_eq(avg_0_3, 4.0) // (1 + 2 + 3 + 10) / 4
  
  let avg_2_5 = SegmentTree::range_average(segment_tree, 2, 5)
  assert_eq(avg_2_5, 6.0) // (3 + 10 + 5 + 6) / 4
  
  // Test performance with large dataset
  let large_values = []
  for i in 0..10000 {
    large_values = large_values + [(i as Float)]
  }
  
  let large_segment_tree = SegmentTree::new(large_values)
  
  let performance_start = PerformanceTimer::start()
  
  // Perform many range queries
  for i in 0..1000 {
    let start = i * 10
    let end = start + 100
    SegmentTree::range_sum(large_segment_tree, start, end)
  }
  
  let performance_end = PerformanceTimer::end()
  let query_time = PerformanceTimer::elapsed_ms(performance_start, performance_end)
  
  // Segment tree should handle range queries efficiently
  assert_true(query_time < 100.0) // Less than 100ms for 1000 range queries
}

// Test 8: Count-Min Sketch for Frequency Estimation
test "count-min sketch for frequency estimation" {
  let sketch = CountMinSketch::new(1000, 5) // 1000 buckets, 5 hash functions
  
  // Test with known frequencies
  for i in 0..100 {
    // Add item1 100 times
    for j in 0..100 {
      CountMinSketch::add(sketch, "item1")
    }
    
    // Add item2 50 times
    for j in 0..50 {
      CountMinSketch::add(sketch, "item2")
    }
    
    // Add item3 25 times
    for j in 0..25 {
      CountMinSketch::add(sketch, "item3")
    }
  }
  
  // Test frequency estimates
  let estimate_item1 = CountMinSketch::estimate(sketch, "item1")
  let estimate_item2 = CountMinSketch::estimate(sketch, "item2")
  let estimate_item3 = CountMinSketch::estimate(sketch, "item3")
  
  // Estimates should be close to actual frequencies (with some overestimation)
  assert_true(estimate_item1 >= 10000) // At least 10000
  assert_true(estimate_item2 >= 5000)  // At least 5000
  assert_true(estimate_item3 >= 2500)  // At least 2500
  
  // Test with non-existent items
  let estimate_non_existent = CountMinSketch::estimate(sketch, "non_existent")
  assert_eq(estimate_non_existent, 0)
  
  // Test merge operation
  let sketch2 = CountMinSketch::new(1000, 5)
  
  for i in 0..50 {
    // Add item1 50 times
    for j in 0..50 {
      CountMinSketch::add(sketch2, "item1")
    }
    
    // Add item4 100 times
    for j in 0..100 {
      CountMinSketch::add(sketch2, "item4")
    }
  }
  
  // Merge the two sketches
  let merged_sketch = CountMinSketch::merge(sketch, sketch2)
  
  let merged_estimate_item1 = CountMinSketch::estimate(merged_sketch, "item1")
  let merged_estimate_item4 = CountMinSketch::estimate(merged_sketch, "item4")
  
  // Merged estimates should reflect combined frequencies
  assert_true(merged_estimate_item1 >= 15000) // At least 15000 (10000 + 5000)
  assert_true(merged_estimate_item4 >= 5000)   // At least 5000
  
  // Test heavy hitter identification
  let heavy_hitters = CountMinSketch::get_heavy_hitters(merged_sketch, 0.1) // Top 10%
  assert_true(heavy_hitters.length() > 0)
  assert_true(heavy_hitters.any(fn(item) { item == "item1" }))
}

// Test 9: Radix Tree for Prefix Matching
test "radix tree for prefix matching" {
  let radix_tree = RadixTree::new()
  
  // Test insertion
  RadixTree::insert(radix_tree, "service.auth", "auth-service-data")
  RadixTree::insert(radix_tree, "service.api", "api-service-data")
  RadixTree::insert(radix_tree, "service.db", "db-service-data")
  RadixTree::insert(radix_tree, "service.cache", "cache-service-data")
  RadixTree::insert(radix_tree, "http.request", "request-data")
  RadixTree::insert(radix_tree, "http.response", "response-data")
  
  // Test exact match
  let auth_data = RadixTree::get(radix_tree, "service.auth")
  match auth_data {
    Some(data) => assert_eq(data, "auth-service-data"),
    None => assert_true(false)
  }
  
  // Test prefix search
  let service_matches = RadixTree::search_by_prefix(radix_tree, "service.")
  assert_eq(service_matches.length(), 4)
  
  let http_matches = RadixTree::search_by_prefix(radix_tree, "http.")
  assert_eq(http_matches.length(), 2)
  
  // Test non-existent prefix
  let non_existent_matches = RadixTree::search_by_prefix(radix_tree, "non.existent.")
  assert_eq(non_existent_matches.length(), 0)
  
  // Test deletion
  RadixTree::delete(radix_tree, "service.cache")
  
  let deleted_data = RadixTree::get(radix_tree, "service.cache")
  assert_true(deleted_data.is_none())
  
  let updated_service_matches = RadixTree::search_by_prefix(radix_tree, "service.")
  assert_eq(updated_service_matches.length(), 3)
  
  // Test longest prefix match
  let longest_match = RadixTree::longest_prefix_match(radix_tree, "service.auth.user")
  match longest_match {
    Some((prefix, data)) => {
      assert_eq(prefix, "service.auth")
      assert_eq(data, "auth-service-data")
    }
    None => assert_true(false)
  }
  
  // Test with common prefixes
  RadixTree::insert(radix_tree, "service.auth.v1", "auth-v1-data")
  RadixTree::insert(radix_tree, "service.auth.v2", "auth-v2-data")
  
  let auth_sub_matches = RadixTree::search_by_prefix(radix_tree, "service.auth.")
  assert_eq(auth_sub_matches.length(), 3) // service.auth, service.auth.v1, service.auth.v2
  
  // Test performance with many keys
  let performance_start = PerformanceTimer::start()
  
  for i in 0..10000 {
    let key = "prefix." + i.to_string() + ".suffix"
    let value = "data-" + i.to_string()
    RadixTree::insert(radix_tree, key, value)
  }
  
  let performance_end = PerformanceTimer::end()
  let insertion_time = PerformanceTimer::elapsed_ms(performance_start, performance_end)
  
  // Radix tree should handle many insertions efficiently
  assert_true(insertion_time < 1000.0) // Less than 1 second for 10000 insertions
}

// Test 10: B-Tree for Large-Scale Data Storage
test "b-tree for large-scale data storage" {
  let btree = BTree::new(4) // B-tree with minimum degree 4
  
  // Test insertion
  for i in 0..100 {
    BTree::insert(btree, i, "value_" + i.to_string())
  }
  
  // Test search
  for i in 0..100 {
    let value = BTree::search(btree, i)
    match value {
      Some(v) => assert_eq(v, "value_" + i.to_string()),
      None => assert_true(false)
    }
  }
  
  // Test range query
  let range_values = BTree::range_query(btree, 20, 30)
  assert_eq(range_values.length(), 11) // 20 to 30 inclusive
  
  for i in 0..11 {
    let key = 20 + i
    let expected_value = "value_" + key.to_string()
    assert_true(range_values.any(fn((k, v)) { k == key && v == expected_value }))
  }
  
  // Test deletion
  BTree::delete(btree, 50)
  let deleted_value = BTree::search(btree, 50)
  assert_true(deleted_value.is_none())
  
  // Verify other values still exist
  let value_49 = BTree::search(btree, 49)
  match value_49 {
    Some(v) => assert_eq(v, "value_49"),
    None => assert_true(false)
  }
  
  let value_51 = BTree::search(btree, 51)
  match value_51 {
    Some(v) => assert_eq(v, "value_51"),
    None => assert_true(false)
  }
  
  // Test update
  BTree::update(btree, 25, "updated_value_25")
  let updated_value = BTree::search(btree, 25)
  match updated_value {
    Some(v) => assert_eq(v, "updated_value_25"),
    None => assert_true(false)
  }
  
  // Test with larger dataset
  let performance_start = PerformanceTimer::start()
  
  for i in 100..10000 {
    BTree::insert(btree, i, "large_value_" + i.to_string())
  }
  
  let performance_end = PerformanceTimer::end()
  let insertion_time = PerformanceTimer::elapsed_ms(performance_start, performance_end)
  
  // B-tree should handle large insertions efficiently
  assert_true(insertion_time < 2000.0) // Less than 2 seconds for 9900 insertions
  
  // Test search performance
  let search_start = PerformanceTimer::start()
  
  for i in 0..10000 {
    if i != 50 { // Skip the deleted key
      BTree::search(btree, i)
    }
  }
  
  let search_end = PerformanceTimer::end()
  let search_time = PerformanceTimer::elapsed_ms(search_start, search_end)
  
  // B-tree should handle searches efficiently
  assert_true(search_time < 1000.0) // Less than 1 second for 9999 searches
  
  // Verify tree properties
  assert_true(BTree::is_valid(btree)) // Should maintain B-tree properties
  assert_eq(BTree::size(btree), 9999) // 10000 - 1 deleted
}