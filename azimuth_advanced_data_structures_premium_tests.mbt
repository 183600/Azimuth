// Premium Advanced Data Structures Tests for Azimuth
// This file contains comprehensive test cases for advanced data structures used in telemetry

// Test 1: Time Series Data Structure
test "time series data structure" {
  let time_series = TimeSeries::new()
  
  // Add data points
  let base_timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  for i in 0..=100 {
    let timestamp = base_timestamp + i * 60000 // 1 minute intervals
    let value = (50.0 + Math::sin(i.to_float() * 0.1) * 10.0)
    
    TimeSeries::add_point(time_series, timestamp, value)
  }
  
  // Verify data points were added
  assert_eq(TimeSeries::size(time_series), 101)
  
  // Test point retrieval
  let first_point = TimeSeries::get_point(time_series, 0)
  match first_point {
    Some(point) => {
      assert_eq(TimeSeriesPoint::timestamp(point), base_timestamp)
      assert_eq(TimeSeriesPoint::value(point), 50.0)
    }
    None => assert_true(false)
  }
  
  // Test range query
  let start_time = base_timestamp + 30 * 60000 // 30 minutes in
  let end_time = base_timestamp + 60 * 60000 // 60 minutes in
  
  let range_points = TimeSeries::get_range(time_series, start_time, end_time)
  assert_eq(range_points.length(), 31) // Inclusive range
  
  // Test aggregation
  let avg_value = TimeSeries::aggregate(time_series, start_time, end_time, AggregationType::Average)
  match avg_value {
    Some(value) => assert_true(value > 40.0 && value < 60.0)
    None => assert_true(false)
  }
  
  let max_value = TimeSeries::aggregate(time_series, start_time, end_time, AggregationType::Max)
  match max_value {
    Some(value) => assert_true(value >= 50.0)
    None => assert_true(false)
  }
  
  let min_value = TimeSeries::aggregate(time_series, start_time, end_time, AggregationType::Min)
  match min_value {
    Some(value) => assert_true(value <= 60.0)
    None => assert_true(false)
  }
  
  // Test downsampling
  let downsampled = TimeSeries::downsample(time_series, 5 * 60000) // 5 minute intervals
  assert_eq(downsampled.size(), 21) // 101 points / 5 = ~21 points
  
  // Test interpolation
  let interpolated_value = TimeSeries::interpolate(time_series, base_timestamp + 30 * 60000 + 30000) // 30.5 minutes
  match interpolated_value {
    Some(value) => assert_true(value > 40.0 && value < 60.0)
    None => assert_true(false)
  }
}

// Test 2: Circular Buffer for Telemetry Events
test "circular buffer for telemetry events" {
  let buffer_size = 100
  let circular_buffer = CircularBuffer::new(buffer_size)
  
  // Fill buffer beyond capacity
  for i in 0..=150 {
    let event = TelemetryEvent::new("event_" + i.to_string(), i.to_float())
    CircularBuffer::push(circular_buffer, event)
  }
  
  // Buffer should only contain the most recent items
  assert_eq(CircularBuffer::size(circular_buffer), buffer_size)
  
  // Test FIFO behavior
  let first_event = CircularBuffer::get(circular_buffer, 0)
  match first_event {
    Some(event) => {
      assert_eq(TelemetryEvent::name(event), "event_51") // Should be the 51st event (151-100)
    }
    None => assert_true(false)
  }
  
  let last_event = CircularBuffer::get(circular_buffer, buffer_size - 1)
  match last_event {
    Some(event) => {
      assert_eq(TelemetryEvent::name(event), "event_150") // Should be the last event
    }
    None => assert_true(false)
  }
  
  // Test iteration
  let mut count = 0
  CircularBuffer::for_each(circular_buffer, fn(event) {
    count = count + 1
  })
  assert_eq(count, buffer_size)
  
  // Test batch operations
  let batch_size = 10
  let batches = CircularBuffer::get_batches(circular_buffer, batch_size)
  assert_eq(batches.length(), (buffer_size + batch_size - 1) / batch_size) // Ceiling division
  
  // Test clearing
  CircularBuffer::clear(circular_buffer)
  assert_eq(CircularBuffer::size(circular_buffer), 0)
}

// Test 3: Hierarchical Attribute Store
test "hierarchical attribute store" {
  let attr_store = HierarchicalAttributeStore::new()
  
  // Build hierarchy
  HierarchicalAttributeStore::set_root(attr_store, "service", StringValue("auth-service"))
  HierarchicalAttributeStore::add_child(attr_store, "service", "version", StringValue("1.2.3"))
  HierarchicalAttributeStore::add_child(attr_store, "service", "instance", StringValue("instance-123"))
  
  HierarchicalAttributeStore::add_child(attr_store, "version", "build_date", StringValue("2021-01-01"))
  HierarchicalAttributeStore::add_child(attr_store, "version", "git_commit", StringValue("abc123"))
  
  HierarchicalAttributeStore::add_child(attr_store, "instance", "host", StringValue("host-456"))
  HierarchicalAttributeStore::add_child(attr_store, "instance", "region", StringValue("us-east"))
  
  // Test direct access
  let service_value = HierarchicalAttributeStore::get(attr_store, "service")
  match service_value {
    Some(StringValue(value)) => assert_eq(value, "auth-service")
    _ => assert_true(false)
  }
  
  let version_value = HierarchicalAttributeStore::get(attr_store, "service.version")
  match version_value {
    Some(StringValue(value)) => assert_eq(value, "1.2.3")
    _ => assert_true(false)
  }
  
  let region_value = HierarchicalAttributeStore::get(attr_store, "service.instance.region")
  match region_value {
    Some(StringValue(value)) => assert_eq(value, "us-east")
    _ => assert_true(false)
  }
  
  // Test subtree access
  let service_subtree = HierarchicalAttributeStore::get_subtree(attr_store, "service")
  assert_true(service_subtree.size() > 0)
  
  // Test wildcard access
  let all_version_attrs = HierarchicalAttributeStore::get_with_wildcard(attr_store, "*.version")
  assert_eq(all_version_attrs.length(), 1)
  
  let all_build_dates = HierarchicalAttributeStore::get_with_wildcard(attr_store, "*.build_date")
  assert_eq(all_build_dates.length(), 1)
  
  // Test hierarchical search
  let search_results = HierarchicalAttributeStore::search(attr_store, "build")
  assert_true(search_results.length() > 0)
  
  // Test attribute inheritance
  HierarchicalAttributeStore::set_inheritance(attr_store, "service.instance", "service", true)
  let inherited_service = HierarchicalAttributeStore::get_with_inheritance(attr_store, "service.instance.service")
  match inherited_service {
    Some(StringValue(value)) => assert_eq(value, "auth-service")
    _ => assert_true(false)
  }
}

// Test 4: Efficient Histogram for Metrics
test "efficient histogram for metrics" {
  let histogram = EfficientHistogram::new(0.0, 1000.0, 20) // Min=0, Max=1000, 20 buckets
  
  // Add values
  let values = [
    10.5, 25.3, 50.7, 75.2, 100.8,
    150.3, 200.7, 300.2, 400.8, 500.3,
    600.7, 700.2, 800.8, 900.3, 999.7
  ]
  
  for value in values {
    EfficientHistogram::add_value(histogram, value)
  }
  
  // Verify bucket counts
  assert_eq(EfficientHistogram::get_total_count(histogram), 15)
  
  // Test percentile calculation
  let p50 = EfficientHistogram::percentile(histogram, 50.0)
  assert_true(p50 >= 400.0 && p50 <= 600.0) // Should be around the middle
  
  let p95 = EfficientHistogram::percentile(histogram, 95.0)
  assert_true(p95 >= 900.0) // Should be near the high end
  
  let p99 = EfficientHistogram::percentile(histogram, 99.0)
  assert_true(p99 >= 950.0) // Should be very high
  
  // Test bucket boundaries
  let buckets = EfficientHistogram::get_buckets(histogram)
  assert_eq(buckets.length(), 20)
  
  for i in 0..buckets.length() - 1 {
    let bucket = buckets[i]
    let next_bucket = buckets[i + 1]
    
    assert_true(EfficientHistogramBucket::upper_bound(bucket) <= EfficientHistogramBucket::lower_bound(next_bucket))
  }
  
  // Test merging histograms
  let histogram2 = EfficientHistogram::new(0.0, 1000.0, 20)
  for value in values {
    EfficientHistogram::add_value(histogram2, value)
  }
  
  let merged_histogram = EfficientHistogram::merge(histogram, histogram2)
  assert_eq(EfficientHistogram::get_total_count(merged_histogram), 30)
  
  let merged_p50 = EfficientHistogram::percentile(merged_histogram, 50.0)
  assert_true(merged_p50 >= 400.0 && merged_p50 <= 600.0)
}

// Test 5: LRU Cache for Telemetry Data
test "lru cache for telemetry data" {
  let cache_size = 100
  let lru_cache = LRUCache::new(cache_size)
  
  // Fill cache
  for i in 0..=cache_size {
    let key = "key_" + i.to_string()
    let value = TelemetryData::new(i.to_int(), "service_" + i.to_string(), "operation_" + i.to_string(), i.to_float(), true, Attributes::new())
    LRUCache::put(lru_cache, key, value)
  }
  
  // Cache should be at capacity
  assert_eq(LRUCache::size(lru_cache), cache_size)
  
  // First item should be evicted
  let first_item = LRUCache::get(lru_cache, "key_0")
  assert_true(first_item.is_none())
  
  // Last item should be present
  let last_item = LRUCache::get(lru_cache, "key_" + cache_size.to_string())
  assert_true(last_item.is_some())
  
  // Access an item to update its position
  let middle_item = LRUCache::get(lru_cache, "key_50")
  assert_true(middle_item.is_some())
  
  // Add a new item
  let new_key = "key_new"
  let new_value = TelemetryData::new(999, "new_service", "new_operation", 999.0, true, Attributes::new())
  LRUCache::put(lru_cache, new_key, new_value)
  
  // New item should be present
  let retrieved_new = LRUCache::get(lru_cache, new_key)
  assert_true(retrieved_new.is_some())
  
  // Key_1 should now be evicted (not key_50 since we accessed it)
  let second_item = LRUCache::get(lru_cache, "key_1")
  assert_true(second_item.is_none())
  
  // Key_50 should still be present
  let middle_item_again = LRUCache::get(lru_cache, "key_50")
  assert_true(middle_item_again.is_some())
  
  // Test cache statistics
  let stats = LRUCache::get_statistics(lru_cache)
  assert_true(stats.hits > 0)
  assert_true(stats.misses > 0)
  assert_true(stats.evictions > 0)
  assert_eq(stats.size, cache_size)
}

// Test 6: Bloom Filter for Telemetry Events
test "bloom filter for telemetry events" {
  let expected_items = 1000
  let false_positive_rate = 0.01 // 1%
  let bloom_filter = BloomFilter::new(expected_items, false_positive_rate)
  
  // Add items to filter
  let items = []
  for i in 0..=expected_items - 1 {
    let item = "event_" + i.to_string()
    BloomFilter::add(bloom_filter, item)
    items.push(item)
  }
  
  // Test positive cases (items that were added)
  for item in items {
    assert_true(BloomFilter::might_contain(bloom_filter, item))
  }
  
  // Test negative cases (items that were not added)
  let false_positives = 0
  let negative_tests = 100
  
  for i in 0..=negative_tests - 1 {
    let item = "not_added_" + i.to_string()
    if BloomFilter::might_contain(bloom_filter, item) {
      false_positives = false_positives + 1
    }
  }
  
  // Verify false positive rate is within expected bounds
  let actual_false_positive_rate = false_positives.to_float() / negative_tests.to_float()
  assert_true(actual_false_positive_rate <= false_positive_rate * 2.0) // Allow some tolerance
  
  // Test filter statistics
  let stats = BloomFilter::get_statistics(bloom_filter)
  assert_eq(stats.added_items, expected_items)
  assert_true(stats.bit_array_size > 0)
  assert_true(stats.hash_functions > 0)
}

// Test 7: Skip List for Time-Ordered Data
test "skip list for time-ordered data" {
  let skip_list = SkipList::new()
  
  // Insert time-ordered data
  let base_timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  for i in 0..=100 {
    let timestamp = base_timestamp + i * 1000 // 1 second intervals
    let value = TelemetryEvent::new("event_" + i.to_string(), i.to_float())
    
    SkipList::insert(skip_list, timestamp, value)
  }
  
  // Verify size
  assert_eq(SkipList::size(skip_list), 101)
  
  // Test range query
  let start_time = base_timestamp + 30 * 1000 // 30 seconds in
  let end_time = base_timestamp + 60 * 1000 // 60 seconds in
  
  let range_results = SkipList::range_query(skip_list, start_time, end_time)
  assert_eq(range_results.length(), 31) // Inclusive range
  
  // Test ordered iteration
  let mut last_timestamp = 0L
  let mut count = 0
  
  SkipList::for_each(skip_list, fn(timestamp, event) {
    assert_true(timestamp >= last_timestamp) // Should be ordered
    last_timestamp = timestamp
    count = count + 1
  })
  
  assert_eq(count, 101)
  
  // Test search
  let search_timestamp = base_timestamp + 50 * 1000 // 50 seconds in
  let search_result = SkipList::find(skip_list, search_timestamp)
  
  match search_result {
    Some((timestamp, event)) => {
      assert_eq(timestamp, search_timestamp)
      assert_eq(TelemetryEvent::name(event), "event_50")
    }
    None => assert_true(false)
  }
  
  // Test deletion
  let delete_timestamp = base_timestamp + 25 * 1000 // 25 seconds in
  let deleted = SkipList::delete(skip_list, delete_timestamp)
  assert_true(deleted)
  
  assert_eq(SkipList::size(skip_list), 100)
  
  let search_after_delete = SkipList::find(skip_list, delete_timestamp)
  assert_true(search_after_delete.is_none())
}

// Test 8: Trie for Efficient String Matching
test "trie for efficient string matching" {
  let trie = Trie::new()
  
  // Insert telemetry-related strings
  let telemetry_strings = [
    "authentication",
    "authorization",
    "authenticated",
    "author",
    "trace",
    "tracing",
    "tracker",
    "metric",
    "metrics",
    "monitor",
    "monitoring",
    "log",
    "logging",
    "logger"
  ]
  
  for string in telemetry_strings {
    Trie::insert(trie, string)
  }
  
  // Test exact matches
  for string in telemetry_strings {
    assert_true(Trie::contains(trie, string))
  }
  
  // Test non-matches
  assert_false(Trie::contains(trie, "not_in_trie"))
  assert_false(Trie::contains(trie, "authentication_failed"))
  
  // Test prefix search
  let auth_results = Trie::prefix_search(trie, "auth")
  assert_eq(auth_results.length(), 4) // auth, authentication, authenticated, author
  
  let trace_results = Trie::prefix_search(trie, "trac")
  assert_eq(trace_results.length(), 3) // trace, tracing, tracker
  
  let log_results = Trie::prefix_search(trie, "log")
  assert_eq(log_results.length(), 3) // log, logging, logger
  
  // Test fuzzy search
  let fuzzy_results = Trie::fuzzy_search(trie, "authen", 1) // Allow 1 edit distance
  assert_true(fuzzy_results.length() >= 1)
  
  // Test deletion
  let deleted = Trie::delete(trie, "author")
  assert_true(deleted)
  assert_false(Trie::contains(trie, "author"))
  
  // Other strings with same prefix should still exist
  assert_true(Trie::contains(trie, "authentication"))
  assert_true(Trie::contains(trie, "authorization"))
  assert_true(Trie::contains(trie, "authenticated"))
  
  // Test trie statistics
  let stats = Trie::get_statistics(trie)
  assert_true(stats.node_count > 0)
  assert_true(stats.word_count == telemetry_strings.length() - 1) // One was deleted
  assert_true(stats.max_depth > 0)
}

// Test 9: Spatial Index for Geographic Telemetry
test "spatial index for geographic telemetry" {
  let spatial_index = SpatialIndex::new()
  
  // Add geographic telemetry points
  let points = [
    (40.7128, -74.0060, "New York"),      // NYC
    (34.0522, -118.2437, "Los Angeles"),  // LA
    (41.8781, -87.6298, "Chicago"),       // Chicago
    (29.7604, -95.3698, "Houston"),       // Houston
    (33.4484, -112.0740, "Phoenix"),      // Phoenix
    (39.7392, -104.9903, "Denver"),       // Denver
    (25.7617, -80.1918, "Miami"),         // Miami
    (42.3601, -71.0589, "Boston"),        // Boston
    (47.6062, -122.3321, "Seattle"),      // Seattle
    (37.7749, -122.4194, "San Francisco") // SF
  ]
  
  for (lat, lon, name) in points {
    let telemetry_point = GeographicTelemetryPoint::new(lat, lon, name)
    SpatialIndex::insert(spatial_index, telemetry_point)
  }
  
  // Test radius search
  let nyc_center = GeographicPoint::new(40.7128, -74.0060)
  let radius_km = 500.0 // 500 km radius
  
  let nearby_points = SpatialIndex::radius_search(spatial_index, nyc_center, radius_km)
  assert_true(nearby_points.length() >= 1) // Should include NYC itself
  assert_true(nearby_points.length() <= 3) // Might include Boston and possibly others
  
  // Test bounding box search
  let southwest = GeographicPoint::new(25.0, -125.0) // Southwest US
  let northeast = GeographicPoint::new(50.0, -65.0)  // Northeast US
  
  let bbox_results = SpatialIndex::bounding_box_search(spatial_index, southwest, northeast)
  assert_true(bbox_results.length() >= 8) // Should include most US cities
  
  // Test nearest neighbor
  let query_point = GeographicPoint::new(39.0, -77.0) // Near Washington DC
  let nearest = SpatialIndex::nearest_neighbor(spatial_index, query_point)
  
  match nearest {
    Some(point) => {
      let name = GeographicTelemetryPoint::name(point)
      // Should be one of the eastern cities
      assert_true(name == "New York" || name == "Boston" || name == "Miami" || name == "Chicago" || name == "Houston")
    }
    None => assert_true(false)
  }
  
  // Test k-nearest neighbors
  let k_nearest = SpatialIndex::k_nearest_neighbors(spatial_index, query_point, 3)
  assert_eq(k_nearest.length(), 3)
  
  // Verify they are ordered by distance
  for i in 0..k_nearest.length() - 1 {
    let point1 = k_nearest[i]
    let point2 = k_nearest[i + 1]
    
    let dist1 = GeographicPoint::distance(query_point, GeographicTelemetryPoint::location(point1))
    let dist2 = GeographicPoint::distance(query_point, GeographicTelemetryPoint::location(point2))
    
    assert_true(dist1 <= dist2)
  }
  
  // Test deletion
  let la_point = GeographicPoint::new(34.0522, -118.2437)
  let deleted = SpatialIndex::delete(spatial_index, la_point)
  assert_true(deleted)
  
  // LA should no longer appear in searches
  let la_search = SpatialIndex::nearest_neighbor(spatial_index, la_point)
  match la_search {
    Some(point) => assert_ne(GeographicTelemetryPoint::name(point), "Los Angeles")
    None => assert_true(false) // Should find another point
  }
}

// Test 10: Probabilistic Data Structure for Cardinality Estimation
test "probabilistic data structure for cardinality estimation" {
  let hyperloglog = HyperLogLog::new(12) // 12 bits for register indexing
  
  // Add unique elements
  let unique_elements = 10000
  let actual_unique = []
  
  for i in 0..=unique_elements - 1 {
    let element = "element_" + i.to_string()
    HyperLogLog::add(hyperloglog, element)
    actual_unique.push(element)
  }
  
  // Add some duplicates
  for i in 0..=999 {
    let element = "element_" + (i % 100).to_string() // 1000 duplicates of first 100 elements
    HyperLogLog::add(hyperloglog, element)
  }
  
  // Estimate cardinality
  let estimated_cardinality = HyperLogLog::estimate(hyperloglog)
  
  // Verify estimate is close to actual (within reasonable error margin)
  let error_rate = (estimated_cardinality - unique_elements.to_float()).abs() / unique_elements.to_float()
  assert_true(error_rate < 0.05) // Should be within 5% error
  
  // Test merging HyperLogLogs
  let hyperloglog2 = HyperLogLog::new(12)
  
  // Add different elements to second HLL
  for i in 0..=4999 {
    let element = "different_element_" + i.to_string()
    HyperLogLog::add(hyperloglog2, element)
  }
  
  // Merge the two HLLs
  let merged_hll = HyperLogLog::merge(hyperloglog, hyperloglog2)
  let merged_estimate = HyperLogLog::estimate(merged_hll)
  
  // Should be approximately the sum of both sets
  let expected_merged = unique_elements + 5000
  let merged_error_rate = (merged_estimate - expected_merged.to_float()).abs() / expected_merged.to_float()
  assert_true(merged_error_rate < 0.05) // Should be within 5% error
  
  // Test serialization and deserialization
  let serialized = HyperLogLog::serialize(hyperloglog)
  assert_true(serialized.length() > 0)
  
  let deserialized = HyperLogLog::deserialize(serialized)
  let deserialized_estimate = HyperLogLog::estimate(deserialized)
  
  // Should be the same as original
  assert_eq(estimated_cardinality, deserialized_estimate)
  
  // Test HLL statistics
  let stats = HyperLogLog::get_statistics(hyperloglog)
  assert_eq(stats.register_count, 1 << 12) // 2^12 registers
  assert_true(stats.zero_registers < stats.register_count)
  assert_true(stats.estimate == estimated_cardinality)
}