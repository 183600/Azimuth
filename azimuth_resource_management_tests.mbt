// Azimuth Telemetry System - Resource Management Tests
// This file contains comprehensive test cases for resource management functionality

// Test 1: Basic Resource Allocation and Deallocation
test "basic resource allocation and deallocation" {
  let resource_manager = ResourceManager::new()
  
  // Test memory resource allocation
  let memory_resource = ResourceManager::allocate_memory(resource_manager, 1024) // 1KB
  assert_true(Resource::is_valid(memory_resource))
  assert_eq(Resource::size(memory_resource), 1024)
  
  // Test file handle allocation
  let file_resource = ResourceManager::allocate_file_handle(resource_manager, "/tmp/test.txt")
  assert_true(Resource::is_valid(file_resource))
  assert_eq(Resource::path(file_resource), "/tmp/test.txt")
  
  // Test network socket allocation
  let socket_resource = ResourceManager::allocate_socket(resource_manager, "tcp", "localhost", 8080)
  assert_true(Resource::is_valid(socket_resource))
  assert_eq(Resource::protocol(socket_resource), "tcp")
  
  // Test resource deallocation
  ResourceManager::deallocate(resource_manager, memory_resource)
  assert_false(Resource::is_valid(memory_resource))
  
  ResourceManager::deallocate(resource_manager, file_resource)
  assert_false(Resource::is_valid(file_resource))
  
  ResourceManager::deallocate(resource_manager, socket_resource)
  assert_false(Resource::is_valid(socket_resource))
}

// Test 2: Resource Pool Management
test "resource pool management" {
  let pool_manager = ResourcePoolManager::new()
  
  // Create memory pool
  let memory_pool = ResourcePoolManager::create_memory_pool(pool_manager, 10240, 10) // 10KB max, 10 resources max
  
  // Test pool checkout
  let memory1 = ResourcePool::checkout(memory_pool)
  assert_true(Resource::is_valid(memory1))
  assert_eq(ResourcePool::available_count(memory_pool), 9)
  assert_eq(ResourcePool::checked_out_count(memory_pool), 1)
  
  let memory2 = ResourcePool::checkout(memory_pool)
  assert_true(Resource::is_valid(memory2))
  assert_eq(ResourcePool::available_count(memory_pool), 8)
  assert_eq(ResourcePool::checked_out_count(memory_pool), 2)
  
  // Test pool checkin
  ResourcePool::checkin(memory_pool, memory1)
  assert_eq(ResourcePool::available_count(memory_pool), 9)
  assert_eq(ResourcePool::checked_out_count(memory_pool), 1)
  
  // Test pool exhaustion
  let resources = []
  
  for i in 0..=10 {
    let resource = ResourcePool::checkout(memory_pool)
    match resource {
      Some(r) => resources.push(r)
      None => {
        // Should fail on the 11th attempt
        assert_eq(i, 9)
      }
    }
  }
  
  assert_eq(resources.length(), 10)
  assert_eq(ResourcePool::available_count(memory_pool), 0)
  assert_eq(ResourcePool::checked_out_count(memory_pool), 10)
  
  // Check in all resources
  for resource in resources {
    ResourcePool::checkin(memory_pool, Some(resource))
  }
  
  assert_eq(ResourcePool::available_count(memory_pool), 10)
  assert_eq(ResourcePool::checked_out_count(memory_pool), 0)
}

// Test 3: Resource Leak Detection
test "resource leak detection" {
  let leak_detector = ResourceLeakDetector::new()
  
  // Enable leak detection
  LeakDetector::enable(leak_detector)
  
  // Allocate resources without deallocating them
  let resource1 = LeakDetector::track_allocation(leak_detector, "memory", 1024)
  let resource2 = LeakDetector::track_allocation(leak_detector, "file", "/tmp/leak_test.txt")
  let resource3 = LeakDetector::track_allocation(leak_detector, "socket", "tcp://localhost:8080")
  
  // Check for leaks
  let leaks = LeakDetector::detect_leaks(leak_detector)
  assert_eq(leaks.length(), 3)
  
  let memory_leak = leaks.find(|l| Leak::resource_type(l) == "memory")
  match memory_leak {
    Some(leak) => {
      assert_eq(Leak::resource_id(leak), Resource::id(resource1))
      assert_eq(Leak::size(leak), 1024)
    }
    None => assert_true(false)
  }
  
  // Deallocate some resources
  LeakDetector::track_deallocation(leak_detector, resource2)
  
  // Check for remaining leaks
  let remaining_leaks = LeakDetector::detect_leaks(leak_detector)
  assert_eq(remaining_leaks.length(), 2)
  
  // Generate leak report
  let leak_report = LeakDetector::generate_report(leak_detector)
  match leak_report {
    LeakReport(total_leaks, total_size, leak_details) => {
      assert_eq(total_leaks, 2)
      assert_eq(total_size, 1024)
      assert_eq(leak_details.length(), 2)
    }
    _ => assert_true(false)
  }
  
  // Clean up remaining resources
  LeakDetector::track_deallocation(leak_detector, resource1)
  LeakDetector::track_deallocation(leak_detector, resource3)
  
  // Verify no more leaks
  let final_leaks = LeakDetector::detect_leaks(leak_detector)
  assert_eq(final_leaks.length(), 0)
}

// Test 4: Resource Usage Monitoring
test "resource usage monitoring" {
  let monitor = ResourceMonitor::new()
  
  // Start monitoring
  ResourceMonitor::start(monitor)
  
  // Allocate and use resources
  let resources = []
  
  for i in 0..=100 {
    let resource = ResourceMonitor::allocate_tracked(monitor, "memory", 1024 * (i + 1))
    resources.push(resource)
  }
  
  // Get resource usage statistics
  let usage_stats = ResourceMonitor::get_usage_statistics(monitor)
  
  match usage_stats {
    UsageStats(total_memory, total_files, total_sockets, peak_memory, peak_files, peak_sockets) => {
      assert_eq(total_memory, 1024 * 5050) // Sum of 1KB to 101KB
      assert_eq(total_files, 0)
      assert_eq(total_sockets, 0)
      assert_eq(peak_memory, 1024 * 101) // Largest allocation
      assert_eq(peak_files, 0)
      assert_eq(peak_sockets, 0)
    }
    _ => assert_true(false)
  }
  
  // Get resource type breakdown
  let memory_stats = ResourceMonitor::get_type_statistics(monitor, "memory")
  match memory_stats {
    ResourceTypeStats(count, total_size, average_size, min_size, max_size) => {
      assert_eq(count, 101)
      assert_eq(total_size, 1024 * 5050)
      assert_eq(average_size, 1024 * 5050 / 101)
      assert_eq(min_size, 1024)
      assert_eq(max_size, 1024 * 101)
    }
    _ => assert_true(false)
  }
  
  // Deallocate some resources
  for i in 0..=50 {
    ResourceMonitor::deallocate_tracked(monitor, resources[i])
  }
  
  // Get updated statistics
  let updated_stats = ResourceMonitor::get_usage_statistics(monitor)
  
  match updated_stats {
    UsageStats(total_memory, _, _, _, _, _) => {
      assert_eq(total_memory, 1024 * (5050 - (51 * 52) / 2)) // Subtract first 51 allocations
    }
    _ => assert_true(false)
  }
  
  // Stop monitoring
  let monitoring_report = ResourceMonitor::stop(monitor)
  
  match monitoring_report {
    MonitoringReport(duration, total_allocations, total_deallocations, peak_usage) => {
      assert_true(duration > 0L)
      assert_eq(total_allocations, 101)
      assert_eq(total_deallocations, 51)
      assert_true(peak_usage > 0)
    }
    _ => assert_true(false)
  }
}

// Test 5: Resource Quota Management
test "resource quota management" {
  let quota_manager = ResourceQuotaManager::new()
  
  // Set resource quotas
  QuotaManager::set_memory_quota(quota_manager, 10485760) // 10MB
  QuotaManager::set_file_quota(quota_manager, 100) // 100 files
  QuotaManager::set_socket_quota(quota_manager, 50) // 50 sockets
  
  // Test quota enforcement
  assert_true(QuotaManager::can_allocate_memory(quota_manager, 5242880)) // 5MB
  assert_true(QuotaManager::can_allocate_memory(quota_manager, 5242880)) // Another 5MB
  assert_false(QuotaManager::can_allocate_memory(quota_manager, 1048576)) // 1MB more would exceed quota
  
  // Test quota tracking
  QuotaManager::allocate_memory(quota_manager, 5242880) // 5MB
  assert_eq(QuotaManager::get_memory_usage(quota_manager), 5242880)
  
  QuotaManager::allocate_memory(quota_manager, 5242880) // Another 5MB
  assert_eq(QuotaManager::get_memory_usage(quota_manager), 10485760) // At quota limit
  
  // Test quota exceeded handling
  let allocation_result = QuotaManager::try_allocate_memory(quota_manager, 1048576) // 1MB
  match allocation_result {
    QuotaExceeded(current_usage, quota_limit) => {
      assert_eq(current_usage, 10485760)
      assert_eq(quota_limit, 10485760)
    }
    AllocationSuccess(_) => assert_true(false)
  }
  
  // Test quota release
  QuotaManager::release_memory(quota_manager, 5242880) // Release 5MB
  assert_eq(QuotaManager::get_memory_usage(quota_manager), 5242880)
  
  // Now allocation should succeed
  let allocation_result2 = QuotaManager::try_allocate_memory(quota_manager, 1048576) // 1MB
  match allocation_result2 {
    AllocationSuccess(resource) => assert_true(Resource::is_valid(resource))
    QuotaExceeded(_, _) => assert_true(false)
  }
  
  // Test quota reporting
  let quota_report = QuotaManager::generate_quota_report(quota_manager)
  
  match quota_report {
    QuotaReport(memory_usage, memory_quota, file_usage, file_quota, socket_usage, socket_quota) => {
      assert_eq(memory_usage, 6291456) // 5MB + 1MB
      assert_eq(memory_quota, 10485760) // 10MB
      assert_eq(file_usage, 0)
      assert_eq(file_quota, 100)
      assert_eq(socket_usage, 0)
      assert_eq(socket_quota, 50)
    }
    _ => assert_true(false)
  }
}

// Test 6: Resource Cleanup Automation
test "resource cleanup automation" {
  let cleanup_manager = ResourceCleanupManager::new()
  
  // Register cleanup strategies
  let memory_cleanup = MemoryCleanupStrategy::new()
  let file_cleanup = FileCleanupStrategy::new()
  let socket_cleanup = SocketCleanupStrategy::new()
  
  CleanupManager::register_strategy(cleanup_manager, "memory", memory_cleanup)
  CleanupManager::register_strategy(cleanup_manager, "file", file_cleanup)
  CleanupManager::register_strategy(cleanup_manager, "socket", socket_cleanup)
  
  // Set cleanup policies
  CleanupManager::set_policy(cleanup_manager, "memory", CleanupPolicy::new(
    10485760, // 10MB threshold
    0.8,      // 80% usage threshold
    300       // 5 minute timeout
  ))
  
  CleanupManager::set_policy(cleanup_manager, "file", CleanupPolicy::new(
    50,       // 50 files threshold
    0.9,      // 90% usage threshold
    600       // 10 minute timeout
  ))
  
  // Allocate resources
  let resources = []
  
  for i in 0..=100 {
    let resource = CleanupManager::allocate_tracked(cleanup_manager, "memory", 102400) // 100KB each
    resources.push(resource)
  }
  
  // Check if cleanup is triggered
  let cleanup_needed = CleanupManager::is_cleanup_needed(cleanup_manager, "memory")
  assert_true(cleanup_needed) // 100 * 100KB = 10MB, which should trigger cleanup
  
  // Perform cleanup
  let cleanup_result = CleanupManager::perform_cleanup(cleanup_manager, "memory")
  
  match cleanup_result {
    CleanupSuccess(resources_cleaned, memory_freed) => {
      assert_true(resources_cleaned > 0)
      assert_true(memory_freed > 0)
    }
    CleanupNotNeeded => assert_true(false)
    CleanupFailed(error) => assert_true(false)
  }
  
  // Test automatic cleanup scheduling
  CleanupManager::schedule_automatic_cleanup(cleanup_manager, "memory", 60000) // Every minute
  CleanupManager::schedule_automatic_cleanup(cleanup_manager, "file", 300000) // Every 5 minutes
  
  // Get scheduled cleanups
  let scheduled_cleanups = CleanupManager::get_scheduled_cleanups(cleanup_manager)
  assert_eq(scheduled_cleanups.length(), 2)
  
  // Test cleanup history
  let cleanup_history = CleanupManager::get_cleanup_history(cleanup_manager)
  assert_true(cleanup_history.length() > 0)
  
  let last_cleanup = cleanup_history[0]
  match last_cleanup {
    CleanupEvent(resource_type, timestamp, resources_cleaned, memory_freed) => {
      assert_eq(resource_type, "memory")
      assert_true(timestamp > 0L)
      assert_true(resources_cleaned > 0)
      assert_true(memory_freed > 0)
    }
    _ => assert_true(false)
  }
}

// Test 7: Resource Priority Management
test "resource priority management" {
  let priority_manager = ResourcePriorityManager::new()
  
  // Define resource priorities
  PriorityManager::set_priority_level(priority_manager, "critical", 1)
  PriorityManager::set_priority_level(priority_manager, "high", 2)
  PriorityManager::set_priority_level(priority_manager, "medium", 3)
  PriorityManager::set_priority_level(priority_manager, "low", 4)
  
  // Allocate resources with different priorities
  let critical_resource = PriorityManager::allocate_with_priority(priority_manager, "memory", 1024, "critical")
  let high_resource = PriorityManager::allocate_with_priority(priority_manager, "memory", 1024, "high")
  let medium_resource = PriorityManager::allocate_with_priority(priority_manager, "memory", 1024, "medium")
  let low_resource = PriorityManager::allocate_with_priority(priority_manager, "memory", 1024, "low")
  
  assert_true(Resource::is_valid(critical_resource))
  assert_true(Resource::is_valid(high_resource))
  assert_true(Resource::is_valid(medium_resource))
  assert_true(Resource::is_valid(low_resource))
  
  // Test resource preemption under pressure
  PriorityManager::simulate_memory_pressure(priority_manager, 2048) // Only 2KB available
  
  // Low priority resource should be preempted first
  let preemption_result = PriorityManager::preempt_for_allocation(priority_manager, 1024)
  match preemption_result {
    PreemptionSuccess(preempted_resources, freed_memory) => {
      assert_true(preempted_resources.length() > 0)
      assert_true(freed_memory >= 1024)
      
      // Check that low priority resources were preempted first
      let preempted_priorities = preempted_resources.map(|r| Resource::priority(r))
      assert_true(preempted_priorities.contains(4)) // Low priority
    }
    PreemptionNotNeeded => assert_true(false)
    PreemptionFailed(error) => assert_true(false)
  }
  
  // Test priority queue for resource allocation
  let allocation_requests = [
    ResourceRequest::new("memory", 512, "low"),
    ResourceRequest::new("memory", 512, "critical"),
    ResourceRequest::new("memory", 512, "medium"),
    ResourceRequest::new("memory", 512, "high")
  ]
  
  let allocation_order = PriorityManager::get_allocation_order(priority_manager, allocation_requests)
  
  // Critical should be first, then high, then medium, then low
  assert_eq(allocation_order[0].priority, "critical")
  assert_eq(allocation_order[1].priority, "high")
  assert_eq(allocation_order[2].priority, "medium")
  assert_eq(allocation_order[3].priority, "low")
  
  // Test priority-based resource limits
  PriorityManager::set_priority_limits(priority_manager, "memory", [
    ("critical", 5120),   // 5KB for critical
    ("high", 3072),       // 3KB for high
    ("medium", 2048),     // 2KB for medium
    ("low", 1024)         // 1KB for low
  ])
  
  // Try to exceed priority limits
  let critical_resources = []
  for i in 0..=6 {
    let resource = PriorityManager::allocate_with_priority(priority_manager, "memory", 1024, "critical")
    match resource {
      Some(r) => critical_resources.push(r)
      None => {
        // Should fail on the 6th attempt (5KB limit)
        assert_eq(i, 5)
      }
    }
  }
  
  assert_eq(critical_resources.length(), 5)
}

// Test 8: Resource Dependency Management
test "resource dependency management" {
  let dependency_manager = ResourceDependencyManager::new()
  
  // Create resources with dependencies
  let database_connection = DependencyManager::allocate_resource(dependency_manager, "database_connection")
  let transaction = DependencyManager::allocate_resource(dependency_manager, "transaction")
  let query_result = DependencyManager::allocate_resource(dependency_manager, "query_result")
  
  // Set up dependencies
  DependencyManager::add_dependency(dependency_manager, transaction, database_connection)
  DependencyManager::add_dependency(dependency_manager, query_result, transaction)
  
  // Test dependency graph
  let graph = DependencyManager::get_dependency_graph(dependency_manager)
  
  let transaction_deps = DependencyGraph::get_dependencies(graph, transaction)
  assert_eq(transaction_deps.length(), 1)
  assert_eq(transaction_deps[0], database_connection)
  
  let query_result_deps = DependencyGraph::get_dependencies(graph, query_result)
  assert_eq(query_result_deps.length(), 1)
  assert_eq(query_result_deps[0], transaction)
  
  let database_connection_deps = DependencyGraph::get_dependencies(graph, database_connection)
  assert_eq(database_connection_deps.length(), 0)
  
  // Test dependency resolution order
  let resolution_order = DependencyManager::get_resolution_order(dependency_manager)
  assert_eq(resolution_order[0], database_connection)
  assert_eq(resolution_order[1], transaction)
  assert_eq(resolution_order[2], query_result)
  
  // Test circular dependency detection
  let circular_resource1 = DependencyManager::allocate_resource(dependency_manager, "circular1")
  let circular_resource2 = DependencyManager::allocate_resource(dependency_manager, "circular2")
  
  DependencyManager::add_dependency(dependency_manager, circular_resource1, circular_resource2)
  DependencyManager::add_dependency(dependency_manager, circular_resource2, circular_resource1)
  
  let circular_deps = DependencyManager::detect_circular_dependencies(dependency_manager)
  assert_eq(circular_deps.length(), 1)
  assert_eq(circular_deps[0].length(), 2)
  
  // Test cascade deallocation
  DependencyManager::deallocate_with_dependents(dependency_manager, database_connection)
  
  // All dependent resources should be deallocated
  assert_false(Resource::is_valid(transaction))
  assert_false(Resource::is_valid(query_result))
  assert_false(Resource::is_valid(database_connection))
  
  // Test safe deallocation (won't deallocate if dependents exist)
  let safe_resource = DependencyManager::allocate_resource(dependency_manager, "safe_resource")
  let dependent_resource = DependencyManager::allocate_resource(dependency_manager, "dependent_resource")
  
  DependencyManager::add_dependency(dependency_manager, dependent_resource, safe_resource)
  
  let deallocation_result = DependencyManager::try_deallocate_safe(dependency_manager, safe_resource)
  match deallocation_result {
    DeallocationBlocked(dependents) => {
      assert_eq(dependents.length(), 1)
      assert_eq(dependents[0], dependent_resource)
    }
    DeallocationSuccess => assert_true(false)
  }
  
  // Deallocate dependent first
  DependencyManager::deallocate_with_dependents(dependency_manager, dependent_resource)
  
  // Now safe deallocation should work
  let deallocation_result2 = DependencyManager::try_deallocate_safe(dependency_manager, safe_resource)
  match deallocation_result2 {
    DeallocationSuccess => assert_true(true)
    DeallocationBlocked(_) => assert_true(false)
  }
}

// Test 9: Resource Sharing and Isolation
test "resource sharing and isolation" {
  let sharing_manager = ResourceSharingManager::new()
  
  // Create shared resource
  let shared_resource = SharingManager::create_shared_resource(sharing_manager, "shared_cache", 1048576) // 1MB cache
  
  // Test resource sharing
  let accessor1 = SharingManager::get_accessor(sharing_manager, shared_resource, "reader1")
  let accessor2 = SharingManager::get_accessor(sharing_manager, shared_resource, "reader2")
  
  assert_true(ResourceAccessor::is_valid(accessor1))
  assert_true(ResourceAccessor::is_valid(accessor2))
  
  // Test concurrent access
  let access_result1 = ResourceAccessor::access(accessor1, || {
    // Simulate read access
    "data_from_cache"
  })
  
  match access_result1 {
    AccessSuccess(data) => assert_eq(data, "data_from_cache")
    AccessDenied(reason) => assert_true(false)
  }
  
  let access_result2 = ResourceAccessor::access(accessor2, || {
    // Simulate read access
    "data_from_cache"
  })
  
  match access_result2 {
    AccessSuccess(data) => assert_eq(data, "data_from_cache")
    AccessDenied(reason) => assert_true(false)
  }
  
  // Test exclusive access
  let exclusive_accessor = SharingManager::get_exclusive_accessor(sharing_manager, shared_resource, "writer1")
  
  let exclusive_result = ResourceAccessor::access_exclusive(exclusive_accessor, || {
    // Simulate write access
    "write_successful"
  })
  
  match exclusive_result {
    AccessSuccess(data) => assert_eq(data, "write_successful")
    AccessDenied(reason) => assert_true(false)
  }
  
  // Test access control
  let read_only_accessor = SharingManager::get_readonly_accessor(sharing_manager, shared_resource, "reader3")
  
  let write_attempt = ResourceAccessor::try_write(read_only_accessor, "new_data")
  match write_attempt {
    AccessDenied(reason) => assert_true(true)
    AccessSuccess(_) => assert_true(false)
  }
  
  // Test resource isolation
  let isolated_context = SharingManager::create_isolated_context(sharing_manager)
  let isolated_resource = SharingManager::allocate_in_context(isolated_context, "isolated_memory", 1024)
  
  assert_true(Resource::is_valid(isolated_resource))
  
  // Try to access isolated resource from outside context
  let external_access = SharingManager::try_access_from_outside(sharing_manager, isolated_resource)
  match external_access {
    AccessDenied(reason) => assert_true(true)
    AccessSuccess(_) => assert_true(false)
  }
  
  // Test access statistics
  let access_stats = SharingManager::get_access_statistics(sharing_manager, shared_resource)
  
  match access_stats {
    AccessStats(total_accesses, concurrent_accessors, access_frequency, last_access_time) => {
      assert_eq(total_accesses, 3) // accessor1, accessor2, exclusive_accessor
      assert_eq(concurrent_accessors, 3)
      assert_true(access_frequency > 0.0)
      assert_true(last_access_time > 0L)
    }
    _ => assert_true(false)
  }
}

// Test 10: Resource Performance Optimization
test "resource performance optimization" {
  let optimizer = ResourceOptimizer::new()
  
  // Create performance profile
  let profile = ResourcePerformanceProfile::new()
  
  // Add resource usage patterns
  profile.add_pattern("database_connection", {
    allocation_time: 100,      // 100ms
    deallocation_time: 50,     // 50ms
    access_frequency: 1000,    // 1000 accesses per second
    peak_concurrent: 10,       // 10 concurrent connections
    average_lifetime: 300000   // 5 minutes average lifetime
  })
  
  profile.add_pattern("file_handle", {
    allocation_time: 10,       // 10ms
    deallocation_time: 5,      // 5ms
    access_frequency: 100,     // 100 accesses per second
    peak_concurrent: 50,       // 50 concurrent handles
    average_lifetime: 60000    // 1 minute average lifetime
  })
  
  // Analyze performance bottlenecks
  let bottlenecks = Optimizer::analyze_bottlenecks(optimizer, profile)
  
  assert_true(bottlenecks.length() > 0)
  
  let db_bottleneck = bottlenecks.find(|b| Bottleneck::resource_type(b) == "database_connection")
  match db_bottleneck {
    Some(bottleneck) => {
      assert_eq(Bottleneck::type(bottleneck), AllocationTime)
      assert_true(Bottleneck::impact(bottleneck) > 0.0)
    }
    None => assert_true(false)
  }
  
  // Generate optimization recommendations
  let recommendations = Optimizer::generate_recommendations(optimizer, profile)
  
  assert_true(recommendations.length() > 0)
  
  let db_recommendation = recommendations.find(|r| Recommendation::resource_type(r) == "database_connection")
  match db_recommendation {
    Some(recommendation) => {
      assert_eq(Recommendation::type(recommendation), Pooling)
      assert_true(Recommendation::expected_improvement(recommendation) > 0.0)
    }
    None => assert_true(false)
  }
  
  // Apply optimizations
  let optimization_result = Optimizer::apply_optimizations(optimizer, recommendations)
  
  match optimization_result {
    OptimizationSuccess(optimizations_applied, performance_improvement) => {
      assert_true(optimizations_applied.length() > 0)
      assert_true(performance_improvement > 0.0)
    }
    OptimizationFailed(error) => assert_true(false)
  }
  
  // Test optimization effectiveness
  let optimized_profile = Optimizer::measure_performance(optimizer, profile)
  
  let db_optimized = optimized_profile.get_pattern("database_connection")
  match db_optimized {
    Some(pattern) => {
      assert_true(pattern.allocation_time < 100) // Should be improved
      assert_true(pattern.deallocation_time < 50) // Should be improved
    }
    None => assert_true(false)
  }
  
  // Test resource caching optimization
  let cache_optimizer = ResourceCacheOptimizer::new()
  
  CacheOptimizer::enable_caching(cache_optimizer, "database_connection", 10) // Cache 10 connections
  
  // Simulate repeated allocations and deallocations
  let start_time = Time::now()
  
  for i in 0..=100 {
    let connection = CacheOptimizer::get_cached(cache_optimizer, "database_connection")
    CacheOptimizer::return_cached(cache_optimizer, connection)
  }
  
  let cached_time = Time::now() - start_time
  
  // Compare with non-cached performance
  let start_time2 = Time::now()
  
  for i in 0..=100 {
    let connection = ResourceOptimizer::allocate_direct(optimizer, "database_connection")
    ResourceOptimizer::deallocate_direct(optimizer, connection)
  }
  
  let direct_time = Time::now() - start_time2
  
  // Caching should be faster
  assert_true(cached_time < direct_time)
  
  // Get cache statistics
  let cache_stats = CacheOptimizer::get_cache_statistics(cache_optimizer, "database_connection")
  
  match cache_stats {
    CacheStats(hits, misses, hit_ratio, cache_size) => {
      assert_true(hits > 0)
      assert_eq(misses, 1) // First allocation is a miss
      assert_true(hit_ratio > 0.9) // High hit ratio
      assert_eq(cache_size, 10)
    }
    _ => assert_true(false)
  }
}