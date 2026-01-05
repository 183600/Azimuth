// Azimuth Performance Optimization Test Suite
// This file contains test cases for performance optimization functionality

// Test 1: Caching Strategies and Performance
test "caching strategies and performance" {
  // Define cache structure
  type CacheEntry = {
    key: String,
    value: String,
    created_at: Int,
    last_accessed: Int,
    access_count: Int,
    size_bytes: Int,
    ttl: Int
  }
  
  type CacheConfig = {
    cache_id: String,
    name: String,
    max_size_mb: Int,
    eviction_policy: String,
    default_ttl: Int,
    cleanup_interval: Int
  }
  
  type CacheMetrics = {
    cache_id: String,
    hits: Int,
    misses: Int,
    evictions: Int,
    current_size_mb: Float,
    hit_rate: Float,
    avg_access_time_ms: Float
  }
  
  // Create cache configuration
  let cache_configs = [
    {
      cache_id: "lru-cache",
      name: "LRU Cache for API Responses",
      max_size_mb: 100,
      eviction_policy: "lru",
      default_ttl: 3600,  // 1 hour
      cleanup_interval: 300  // 5 minutes
    },
    {
      cache_id: "lfu-cache",
      name: "LFU Cache for Database Queries",
      max_size_mb: 50,
      eviction_policy: "lfu",
      default_ttl: 1800,  // 30 minutes
      cleanup_interval: 600  // 10 minutes
    },
    {
      cache_id: "ttl-cache",
      name: "TTL Cache for Session Data",
      max_size_mb: 25,
      eviction_policy: "ttl",
      default_ttl: 900,  // 15 minutes
      cleanup_interval: 180  // 3 minutes
    }
  ]
  
  // Create initial cache entries
  let cache_entries = [
    {
      key: "user:123",
      value: "{\"id\":123,\"name\":\"John Doe\",\"email\":\"john@example.com\"}",
      created_at: 1640995000,
      last_accessed: 1640995200,
      access_count: 5,
      size_bytes: 65,
      ttl: 3600
    },
    {
      key: "product:456",
      value: "{\"id\":456,\"name\":\"Widget\",\"price\":19.99,\"stock\":100}",
      created_at: 1640995100,
      last_accessed: 1640995150,
      access_count: 3,
      size_bytes: 58,
      ttl: 1800
    },
    {
      key: "order:789",
      value: "{\"id\":789,\"user_id\":123,\"total\":125.50,\"status\":\"completed\"}",
      created_at: 1640995200,
      last_accessed: 1640995200,
      access_count: 1,
      size_bytes: 62,
      ttl: 3600
    }
  ]
  
  // Simulate cache lookup
  let cache_lookup = fn(entries: Array[CacheEntry], key: String, current_time: Int) {
    let entry = entries.find(fn(e) { e.key == key })
    
    match entry {
      Some(e) => {
        // Check if entry is expired
        if current_time - e.created_at > e.ttl {
          (None, 0, 1)  // Cache miss due to expiration
        } else {
          // Update access statistics
          let updated_entry = {
            e |
            last_accessed: current_time,
            access_count: e.access_count + 1
          }
          
          (Some(updated_entry), 1, 0)  // Cache hit
        }
      }
      
      None => (None, 0, 1)  // Cache miss
    }
  }
  
  // Simulate cache store
  let cache_store = fn(entries: Array[CacheEntry], key: String, value: String, current_time: Int, ttl: Int) {
    let size_bytes = value.length()
    let new_entry = {
      key,
      value,
      created_at: current_time,
      last_accessed: current_time,
      access_count: 1,
      size_bytes,
      ttl
    }
    
    // Check if key already exists
    let existing_entry = entries.find(fn(e) { e.key == key })
    
    match existing_entry {
      Some(_) => {
        // Update existing entry
        entries.map(fn(e) {
          if e.key == key {
            {
              key,
              value,
              created_at: e.created_at,  // Keep original creation time
              last_accessed: current_time,
              access_count: e.access_count + 1,
              size_bytes,
              ttl
            }
          } else {
            e
          }
        })
      }
      
      None => {
        // Add new entry
        entries.push(new_entry)
      }
    }
  }
  
  // Simulate LRU eviction
  let lru_evict = fn(entries: Array[CacheEntry], cache_config: CacheConfig, new_entry_size: Int) {
    let current_size = entries.reduce(fn(acc, e) { acc + e.size_bytes }, 0)
    let max_size_bytes = cache_config.max_size_mb * 1024 * 1024
    
    if current_size + new_entry_size <= max_size_bytes {
      // No eviction needed
      entries
    } else {
      // Sort by last accessed time (oldest first)
      let sorted_entries = entries.sort(fn(a, b) { a.last_accessed - b.last_accessed })
      
      let mut remaining_size = 0
      let mut kept_entries = []
      
      // Keep entries until we have enough space
      for entry in sorted_entries.reverse() {  // Process newest first
        if remaining_size + entry.size_bytes <= max_size_bytes - new_entry_size {
          remaining_size = remaining_size + entry.size_bytes
          kept_entries = kept_entries.push(entry)
        }
        // else: evict this entry
      }
      
      kept_entries
    }
  }
  
  // Simulate LFU eviction
  let lfu_evict = fn(entries: Array[CacheEntry], cache_config: CacheConfig, new_entry_size: Int) {
    let current_size = entries.reduce(fn(acc, e) { acc + e.size_bytes }, 0)
    let max_size_bytes = cache_config.max_size_mb * 1024 * 1024
    
    if current_size + new_entry_size <= max_size_bytes {
      // No eviction needed
      entries
    } else {
      // Sort by access count (least frequent first)
      let sorted_entries = entries.sort(fn(a, b) { a.access_count - b.access_count })
      
      let mut remaining_size = 0
      let mut kept_entries = []
      
      // Keep entries until we have enough space
      for entry in sorted_entries.reverse() {  // Process most frequent first
        if remaining_size + entry.size_bytes <= max_size_bytes - new_entry_size {
          remaining_size = remaining_size + entry.size_bytes
          kept_entries = kept_entries.push(entry)
        }
        // else: evict this entry
      }
      
      kept_entries
    }
  }
  
  // Test cache lookup with hit
  let current_time = 1640995300
  let (hit_entry, hit, miss) = cache_lookup(cache_entries, "user:123", current_time)
  assert_eq(hit, 1)
  assert_eq(miss, 0)
  assert_true(hit_entry.is_some())
  assert_eq(hit_entry.unwrap().key, "user:123")
  assert_eq(hit_entry.unwrap().access_count, 6)  // Incremented from 5
  
  // Test cache lookup with miss (non-existent key)
  let (miss_entry, hit2, miss2) = cache_lookup(cache_entries, "product:999", current_time)
  assert_eq(hit2, 0)
  assert_eq(miss2, 1)
  assert_true(miss_entry.is_none())
  
  // Test cache lookup with miss (expired key)
  let expired_entries = [
    {
      key: "expired:123",
      value: "expired data",
      created_at: 1640990000,  // Created long ago
      last_accessed: 1640990000,
      access_count: 1,
      size_bytes: 12,
      ttl: 1800  // 30 minutes TTL, should be expired
    }
  ]
  let (expired_entry, hit3, miss3) = cache_lookup(expired_entries, "expired:123", current_time)
  assert_eq(hit3, 0)
  assert_eq(miss3, 1)
  assert_true(expired_entry.is_none())
  
  // Test cache store with new key
  let updated_entries = cache_store(cache_entries, "session:abc", "session data", current_time, 900)
  assert_eq(updated_entries.length(), 4)
  
  let new_entry = updated_entries.find(fn(e) { e.key == "session:abc" })
  assert_true(new_entry.is_some())
  assert_eq(new_entry.unwrap().value, "session data")
  assert_eq(new_entry.unwrap().access_count, 1)
  
  // Test cache store with existing key
  let updated_entries2 = cache_store(updated_entries, "user:123", "updated user data", current_time, 3600)
  assert_eq(updated_entries2.length(), 4)  // Same size, just updated
  
  let updated_entry = updated_entries2.find(fn(e) { e.key == "user:123" })
  assert_true(updated_entry.is_some())
  assert_eq(updated_entry.unwrap().value, "updated user data")
  assert_eq(updated_entry.unwrap().access_count, 7)  // Incremented again
  
  // Test LRU eviction
  let lru_config = cache_configs.filter(fn(c) { c.cache_id == "lru-cache" })[0]
  
  // Create a small cache to test eviction
  let small_cache = [
    {
      key: "old1",
      value: "data1",
      created_at: 1640995000,
      last_accessed: 1640995000,  // Oldest
      access_count: 1,
      size_bytes: 5000000,  // 5MB
      ttl: 3600
    },
    {
      key: "old2",
      value: "data2",
      created_at: 1640995100,
      last_accessed: 1640995100,  // Second oldest
      access_count: 1,
      size_bytes: 4000000,  // 4MB
      ttl: 3600
    },
    {
      key: "recent",
      value: "data3",
      created_at: 1640995200,
      last_accessed: 1640995300,  // Most recent
      access_count: 1,
      size_bytes: 3000000,  // 3MB
      ttl: 3600
    }
  ]
  
  // Small cache config (10MB max)
  let small_lru_config = {
    cache_id: "small-lru",
    name: "Small LRU Cache",
    max_size_mb: 10,
    eviction_policy: "lru",
    default_ttl: 3600,
    cleanup_interval: 300
  }
  
  // Current size: 5 + 4 + 3 = 12MB (exceeds 10MB)
  // Try to add a 2MB entry, should evict oldest entries
  let after_lru_eviction = lru_evict(small_cache, small_lru_config, 2000000)  // 2MB
  
  // Should keep "recent" (3MB) and one of the older ones to make room for 2MB
  assert_eq(after_lru_eviction.length(), 2)  // Two entries kept
  assert_true(after_lru_eviction.any(fn(e) { e.key == "recent" }))
  
  // Test LFU eviction
  let lfu_config = cache_configs.filter(fn(c) { c.cache_id == "lfu-cache" })[0]
  
  // Create a cache with different access frequencies
  let freq_cache = [
    {
      key: "rare",
      value: "data1",
      created_at: 1640995000,
      last_accessed: 1640995300,
      access_count: 1,  // Least frequent
      size_bytes: 3000000,  // 3MB
      ttl: 3600
    },
    {
      key: "frequent",
      value: "data2",
      created_at: 1640995100,
      last_accessed: 1640995300,
      access_count: 10,  // Most frequent
      size_bytes: 4000000,  // 4MB
      ttl: 3600
    },
    {
      key: "medium",
      value: "data3",
      created_at: 1640995200,
      last_accessed: 1640995300,
      access_count: 5,  // Medium frequency
      size_bytes: 5000000,  // 5MB
      ttl: 3600
    }
  ]
  
  // Small cache config (10MB max)
  let small_lfu_config = {
    cache_id: "small-lfu",
    name: "Small LFU Cache",
    max_size_mb: 10,
    eviction_policy: "lfu",
    default_ttl: 3600,
    cleanup_interval: 600
  }
  
  // Current size: 3 + 4 + 5 = 12MB (exceeds 10MB)
  // Try to add a 2MB entry, should evict least frequent entries
  let after_lfu_eviction = lfu_evict(freq_cache, small_lfu_config, 2000000)  // 2MB
  
  // Should keep "frequent" (4MB) and "medium" (5MB), evict "rare" (3MB)
  assert_eq(after_lfu_eviction.length(), 2)  // Two entries kept
  assert_true(after_lfu_eviction.any(fn(e) { e.key == "frequent" }))
  assert_true(after_lfu_eviction.any(fn(e) { e.key == "medium" }))
  assert_false(after_lfu_eviction.any(fn(e) { e.key == "rare" }))
  
  // Calculate cache metrics
  let calculate_cache_metrics = fn(cache_id: String, entries: Array[CacheEntry], hits: Int, misses: Int, evictions: Int, access_times: Array[Int]) {
    let total_requests = hits + misses
    let hit_rate = if total_requests > 0 {
      (hits.to_float() / total_requests.to_float()) * 100.0
    } else {
      0.0
    }
    
    let current_size_bytes = entries.reduce(fn(acc, e) { acc + e.size_bytes }, 0)
    let current_size_mb = current_size_bytes.to_float() / (1024.0 * 1024.0)
    
    let avg_access_time = if access_times.length() > 0 {
      access_times.reduce(fn(acc, t) { acc + t }, 0) / access_times.length()
    } else {
      0
    }
    
    {
      cache_id,
      hits,
      misses,
      evictions,
      current_size_mb,
      hit_rate,
      avg_access_time_ms: avg_access_time.to_float()
    }
  }
  
  // Simulate access times (in milliseconds)
  let cache_access_times = [5, 3, 7, 2, 4, 6, 3, 5]  // Cache hits are fast
  let db_access_times = [150, 200, 120, 180, 160, 140, 190, 170]  // DB misses are slow
  
  let all_access_times = cache_access_times + db_access_times
  let cache_metrics = calculate_cache_metrics("lru-cache", updated_entries2, 8, 4, 2, all_access_times)
  
  assert_eq(cache_metrics.cache_id, "lru-cache")
  assert_eq(cache_metrics.hits, 8)
  assert_eq(cache_metrics.misses, 4)
  assert_eq(cache_metrics.evictions, 2)
  assert_eq(cache_metrics.hit_rate, 66.67)  // 8/12 * 100
  
  // Calculate performance improvement
  let calculate_performance_improvement = fn(cache_hit_rate: Float, avg_cache_time: Float, avg_db_time: Float) {
    let weighted_avg_time = (cache_hit_rate / 100.0) * avg_cache_time + 
                           ((100.0 - cache_hit_rate) / 100.0) * avg_db_time
    
    let improvement_percent = ((avg_db_time - weighted_avg_time) / avg_db_time) * 100.0
    
    {
      avg_response_time_ms: weighted_avg_time,
      performance_improvement_percent: improvement_percent
    }
  }
  
  let performance = calculate_performance_improvement(
    cache_metrics.hit_rate, 
    cache_metrics.avg_access_time_ms, 
    160.0  // Average DB time
  )
  
  assert_eq(performance.avg_response_time_ms, 59.33)  // 0.6667 * 4 + 0.3333 * 160
  assert_eq(performance.performance_improvement_percent, 62.92)  // (160 - 59.33) / 160 * 100
}

// Test 2: Database Query Optimization
test "database query optimization" {
  // Define query structure
  type DatabaseQuery = {
    query_id: String,
    query_text: String,
    query_type: String,
    tables: Array[String],
    execution_time_ms: Int,
    rows_examined: Int,
    rows_returned: Int,
    index_used: Option[String],
    timestamp: Int
  }
  
  type QueryOptimization = {
    optimization_id: String,
    query_id: String,
    original_query: String,
    optimized_query: String,
    optimization_type: String,
    improvement_percent: Float,
    applied_at: Int
  }
  
  type IndexSuggestion = {
    suggestion_id: String,
    table_name: String,
    columns: Array[String],
    index_type: String,
    estimated_improvement: Float,
    priority: String
  }
  
  // Create database queries
  let database_queries = [
    {
      query_id: "query-001",
      query_text: "SELECT * FROM users WHERE email = 'john@example.com'",
      query_type: "SELECT",
      tables: ["users"],
      execution_time_ms: 250,
      rows_examined: 10000,
      rows_returned: 1,
      index_used: None,  // No index used, full table scan
      timestamp: 1640995200
    },
    {
      query_id: "query-002",
      query_text: "SELECT o.*, u.name FROM orders o JOIN users u ON o.user_id = u.id WHERE o.status = 'completed' AND o.created_at > '2022-01-01'",
      query_type: "SELECT",
      tables: ["orders", "users"],
      execution_time_ms: 1200,
      rows_examined: 50000,
      rows_returned: 250,
      index_used: Some("idx_orders_status"),
      timestamp: 1640995210
    },
    {
      query_id: "query-003",
      query_text: "SELECT COUNT(*) FROM products WHERE category = 'electronics' AND price > 100",
      query_type: "SELECT",
      tables: ["products"],
      execution_time_ms: 450,
      rows_examined: 20000,
      rows_returned: 1,
      index_used: Some("idx_products_category"),
      timestamp: 1640995220
    }
  ]
  
  // Create query optimizations
  let query_optimizations = [
    {
      optimization_id: "opt-001",
      query_id: "query-001",
      original_query: "SELECT * FROM users WHERE email = 'john@example.com'",
      optimized_query: "SELECT id, name, email FROM users WHERE email = 'john@example.com'",
      optimization_type: "column_pruning",
      improvement_percent: 15.0,
      applied_at: 1640995300
    },
    {
      optimization_id: "opt-002",
      query_id: "query-001",
      original_query: "SELECT * FROM users WHERE email = 'john@example.com'",
      optimized_query: "SELECT id, name, email FROM users WHERE email = 'john@example.com'",
      optimization_type: "index_addition",
      improvement_percent: 80.0,
      applied_at: 1640995400
    },
    {
      optimization_id: "opt-003",
      query_id: "query-002",
      original_query: "SELECT o.*, u.name FROM orders o JOIN users u ON o.user_id = u.id WHERE o.status = 'completed' AND o.created_at > '2022-01-01'",
      optimized_query: "SELECT o.id, o.user_id, o.total, u.name FROM orders o FORCE INDEX (idx_orders_status_created) JOIN users u ON o.user_id = u.id WHERE o.status = 'completed' AND o.created_at > '2022-01-01'",
      optimization_type: "composite_index",
      improvement_percent: 65.0,
      applied_at: 1640995500
    }
  ]
  
  // Create index suggestions
  let index_suggestions = [
    {
      suggestion_id: "idx-001",
      table_name: "users",
      columns: ["email"],
      index_type: "btree",
      estimated_improvement: 80.0,
      priority: "high"
    },
    {
      suggestion_id: "idx-002",
      table_name: "orders",
      columns: ["status", "created_at"],
      index_type: "composite",
      estimated_improvement: 65.0,
      priority: "high"
    },
    {
      suggestion_id: "idx-003",
      table_name: "products",
      columns: ["category", "price"],
      index_type: "composite",
      estimated_improvement: 40.0,
      priority: "medium"
    }
  ]
  
  // Identify slow queries
  let identify_slow_queries = fn(queries: Array[DatabaseQuery], threshold_ms: Int) {
    queries.filter(fn(q) { q.execution_time_ms > threshold_ms })
  }
  
  let slow_queries = identify_slow_queries(database_queries, 500)
  assert_eq(slow_queries.length(), 2)
  assert_true(slow_queries.any(fn(q) { q.query_id == "query-001" }))
  assert_true(slow_queries.any(fn(q) { q.query_id == "query-002" }))
  
  // Identify full table scans
  let identify_full_table_scans = fn(queries: Array[DatabaseQuery]) {
    queries.filter(fn(q) { 
      q.index_used.is_none() and q.rows_examined > 1000 
    })
  }
  
  let full_table_scans = identify_full_table_scans(database_queries)
  assert_eq(full_table_scans.length(), 1)
  assert_eq(full_table_scans[0].query_id, "query-001")
  
  // Calculate query efficiency
  let calculate_query_efficiency = fn(query: DatabaseQuery) {
    let selectivity = if query.rows_examined > 0 {
      query.rows_returned.to_float() / query.rows_examined.to_float()
    } else {
      0.0
    }
    
    let efficiency_score = if selectivity > 0.0 {
      100.0 / selectivity
    } else {
      0.0
    }
    
    {
      selectivity_percent: selectivity * 100.0,
      efficiency_score
    }
  }
  
  let query001_efficiency = calculate_query_efficiency(database_queries[0])
  assert_eq(query001_efficiency.selectivity_percent, 0.01)  // 1/10000 * 100
  assert_eq(query001_efficiency.efficiency_score, 10000.0)
  
  let query002_efficiency = calculate_query_efficiency(database_queries[1])
  assert_eq(query002_efficiency.selectivity_percent, 0.5)  // 250/50000 * 100
  assert_eq(query002_efficiency.efficiency_score, 200.0)
  
  let query003_efficiency = calculate_query_efficiency(database_queries[2])
  assert_eq(query003_efficiency.selectivity_percent, 0.005)  // 1/20000 * 100
  assert_eq(query003_efficiency.efficiency_score, 20000.0)
  
  // Generate optimization suggestions
  let generate_optimization_suggestions = fn(query: DatabaseQuery, index_suggestions: Array[IndexSuggestion]) {
    let mut suggestions = []
    
    // Check for full table scan
    if query.index_used.is_none() and query.rows_examined > 1000 {
      let table_suggestions = index_suggestions.filter(fn(idx) { 
        query.tables.contains(idx.table_name) 
      })
      
      for suggestion in table_suggestions {
        suggestions = suggestions.push({
          query_id: query.query_id,
          optimization_type: "index_addition",
          description: "Add " + suggestion.index_type + " index on " + 
                      suggestion.table_name + "(" + suggestion.columns.join(",") + ")",
          estimated_improvement: suggestion.estimated_improvement,
          priority: suggestion.priority
        })
      }
    }
    
    // Check for SELECT *
    if query.query_text.contains("SELECT *") {
      suggestions = suggestions.push({
        query_id: query.query_id,
        optimization_type: "column_pruning",
        description: "Replace SELECT * with specific columns",
        estimated_improvement: 15.0,
        priority: "medium"
      })
    }
    
    // Check for inefficient JOINs
    if query.tables.length() > 1 and query.execution_time_ms > 1000 {
      suggestions = suggestions.push({
        query_id: query.query_id,
        optimization_type: "join_optimization",
        description: "Optimize JOIN conditions or add composite indexes",
        estimated_improvement: 30.0,
        priority: "high"
      })
    }
    
    suggestions
  }
  
  // Generate suggestions for each query
  let query001_suggestions = generate_optimization_suggestions(database_queries[0], index_suggestions)
  assert_eq(query001_suggestions.length(), 2)  // Index addition and column pruning
  assert_true(query001_suggestions.any(fn(s) { s.optimization_type == "index_addition" }))
  assert_true(query001_suggestions.any(fn(s) { s.optimization_type == "column_pruning" }))
  
  let query002_suggestions = generate_optimization_suggestions(database_queries[1], index_suggestions)
  assert_eq(query002_suggestions.length(), 1)  // JOIN optimization
  assert_true(query002_suggestions.any(fn(s) { s.optimization_type == "join_optimization" }))
  
  let query003_suggestions = generate_optimization_suggestions(database_queries[2], index_suggestions)
  assert_eq(query003_suggestions.length(), 0)  // Already using index, no obvious optimizations
  
  // Calculate overall database performance metrics
  let calculate_database_metrics = fn(queries: Array[DatabaseQuery], optimizations: Array[QueryOptimization]) {
    let total_queries = queries.length()
    let total_execution_time = queries.reduce(fn(acc, q) { acc + q.execution_time_ms }, 0)
    let avg_execution_time = if total_queries > 0 {
      total_execution_time / total_queries
    } else {
      0
    }
    
    let slow_queries_count = queries.filter(fn(q) { q.execution_time_ms > 500 }).length()
    let slow_query_rate = if total_queries > 0 {
      (slow_queries_count.to_float() / total_queries.to_float()) * 100.0
    } else {
      0.0
    }
    
    let full_table_scans_count = queries.filter(fn(q) { q.index_used.is_none() }).length()
    let full_table_scan_rate = if total_queries > 0 {
      (full_table_scans_count.to_float() / total_queries.to_float()) * 100.0
    } else {
      0.0
    }
    
    let total_rows_examined = queries.reduce(fn(acc, q) { acc + q.rows_examined }, 0)
    let total_rows_returned = queries.reduce(fn(acc, q) { acc + q.rows_returned }, 0)
    let overall_selectivity = if total_rows_examined > 0 {
      (total_rows_returned.to_float() / total_rows_examined.to_float()) * 100.0
    } else {
      0.0
    }
    
    let optimizations_applied = optimizations.length()
    let total_improvement = optimizations.reduce(fn(acc, o) { acc + o.improvement_percent }, 0.0)
    let avg_improvement = if optimizations_applied > 0 {
      total_improvement / optimizations_applied.to_float()
    } else {
      0.0
    }
    
    {
      total_queries,
      avg_execution_time_ms: avg_execution_time,
      slow_query_rate,
      full_table_scan_rate,
      overall_selectivity_percent: overall_selectivity,
      optimizations_applied,
      avg_improvement_percent: avg_improvement
    }
  }
  
  let db_metrics = calculate_database_metrics(database_queries, query_optimizations)
  assert_eq(db_metrics.total_queries, 3)
  assert_eq(db_metrics.avg_execution_time_ms, 633)  // (250 + 1200 + 450) / 3
  assert_eq(db_metrics.slow_query_rate, 66.67)  // 2/3 * 100
  assert_eq(db_metrics.full_table_scan_rate, 33.33)  // 1/3 * 100
  assert_eq(db_metrics.overall_selectivity_percent, 0.17)  // 252/80000 * 100
  assert_eq(db_metrics.optimizations_applied, 3)
  assert_eq(db_metrics.avg_improvement_percent, 53.33)  // (15 + 80 + 65) / 3
}