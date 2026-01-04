// Azimuth Telemetry System - Resource Management Tests
// This file contains comprehensive test cases for resource management functionality

// Test 1: Memory Management
test "memory management" {
  // Create memory manager
  let memory_manager = MemoryManager::new()
  
  // Get initial memory state
  let initial_state = MemoryManager::get_state(memory_manager)
  assert_true(initial_state.total_allocated >= 0)
  assert_true(initial_state.total_available > 0)
  
  // Allocate memory for telemetry data
  let telemetry_data = []
  for i in 0..=1000 {
    telemetry_data.push(TelemetryData::new(
      "metric_" + i.to_string(),
      i.to_float(),
      "count",
      1234567890L + i.to_long()
    ))
  }
  
  // Check memory state after allocation
  let after_allocation_state = MemoryManager::get_state(memory_manager)
  assert_true(after_allocation_state.total_allocated > initial_state.total_allocated)
  assert_true(after_allocation_state.total_available < initial_state.total_available)
  
  // Test memory pool for telemetry data
  let telemetry_pool = MemoryManager::create_pool(memory_manager, 100, fn() {
    TelemetryData::new("pool_metric", 0.0, "count", 0L)
  })
  
  // Acquire objects from pool
  let pooled_objects = []
  for i in 0..=50 {
    let obj = MemoryManager::acquire_from_pool(telemetry_pool)
    pooled_objects.push(obj)
  }
  
  // Verify pool status
  let pool_stats = MemoryManager::get_pool_stats(telemetry_pool)
  assert_eq(pool_stats.total_created, 51) // Initial + 50 acquired
  assert_eq(pool_stats.in_use, 50)
  assert_eq(pool_stats.available, 1)
  
  // Release objects back to pool
  for obj in pooled_objects {
    MemoryManager::release_to_pool(telemetry_pool, obj)
  }
  
  // Verify pool status after release
  pool_stats = MemoryManager::get_pool_stats(telemetry_pool)
  assert_eq(pool_stats.in_use, 0)
  assert_eq(pool_stats.available, 51)
  
  // Test memory monitoring
  let memory_monitor = MemoryMonitor::new()
  MemoryMonitor::start_monitoring(memory_monitor)
  
  // Perform memory-intensive operations
  let large_data = []
  for i in 0..=10000 {
    large_data.push("large_string_data_" + i.to_string())
  }
  
  // Get memory metrics
  let metrics = MemoryMonitor::get_metrics(memory_monitor)
  assert_true(metrics.peak_usage > 0)
  assert_true(metrics.current_usage > 0)
  assert_true(metrics.allocation_rate > 0)
  
  // Test memory leak detection
  let leak_detector = MemoryLeakDetector::new()
  LeakDetector::start_tracking(leak_detector)
  
  // Simulate memory leak (intentionally not releasing resources)
  let leaked_resources = []
  for i in 0..=100 {
    let resource = Resource::new("leaked_resource_" + i.to_string())
    leaked_resources.push(resource)
    // Not releasing resources intentionally
  }
  
  // Stop tracking and detect leaks
  let leak_report = LeakDetector::stop_and_detect(leak_detector)
  assert_true(leak_report.leaked_objects > 0)
  
  // Clean up leaked resources
  for resource in leaked_resources {
    Resource::release(resource)
  }
}

// Test 2: Connection Pool Management
test "connection pool management" {
  // Create connection pool
  let connection_pool = ConnectionPool::new(
    min_connections = 2,
    max_connections = 10,
    connection_timeout = 5000, // 5 seconds
    idle_timeout = 30000      // 30 seconds
  )
  
  // Get initial pool state
  let initial_state = ConnectionPool::get_state(connection_pool)
  assert_eq(initial_state.active_connections, 0)
  assert_eq(initial_state.idle_connections, 2) // Should create minimum connections
  assert_eq(initial_state.total_connections, 2)
  
  // Acquire connections
  let connections = []
  for i in 0..=5 {
    let connection = ConnectionPool::acquire(connection_pool)
    connections.push(connection)
  }
  
  // Verify pool state after acquiring connections
  let after_acquire_state = ConnectionPool::get_state(connection_pool)
  assert_eq(after_acquire_state.active_connections, 5)
  assert_eq(after_acquire_state.total_connections, 5)
  
  // Release connections
  for connection in connections {
    ConnectionPool::release(connection_pool, connection)
  }
  
  // Verify pool state after releasing connections
  let after_release_state = ConnectionPool::get_state(connection_pool)
  assert_eq(after_release_state.active_connections, 0)
  assert_eq(after_release_state.idle_connections, 5)
  assert_eq(after_release_state.total_connections, 5)
  
  // Test connection validation
  let connection = ConnectionPool::acquire(connection_pool)
  let is_valid = ConnectionPool::validate_connection(connection_pool, connection)
  assert_true(is_valid)
  
  // Simulate connection failure
  Connection::simulate_failure(connection)
  let is_valid_after_failure = ConnectionPool::validate_connection(connection_pool, connection)
  assert_false(is_valid_after_failure)
  
  // Release failed connection
  ConnectionPool::release(connection_pool, connection)
  
  // Verify failed connection was removed
  let after_failure_state = ConnectionPool::get_state(connection_pool)
  assert_eq(after_failure_state.total_connections, 4) // One connection removed
  
  // Test connection timeout
  let slow_connection = ConnectionPool::acquire_with_timeout(connection_pool, 100) // 100ms timeout
  match slow_connection {
    Some(_) => assert_true(true) // Should succeed quickly
    None => assert_true(false)
  }
  
  // Test max connections limit
  let many_connections = []
  let mut acquire_success = true
  for i in 0..=15 { // Try to acquire more than max
    match ConnectionPool::acquire(connection_pool) {
      Some(conn) => many_connections.push(conn)
      None => {
        acquire_success = false
        break
      }
    }
  }
  
  assert_false(acquire_success) // Should fail when reaching max connections
  assert_true(many_connections.length() <= 10) // Should not exceed max
  
  // Release all connections
  for conn in many_connections {
    ConnectionPool::release(connection_pool, conn)
  }
}

// Test 3: Thread Pool Management
test "thread pool management" {
  // Create thread pool
  let thread_pool = ThreadPool::new(
    min_threads = 2,
    max_threads = 8,
    keep_alive_time = 60000 // 1 minute
  )
  
  // Get initial pool state
  let initial_state = ThreadPool::get_state(thread_pool)
  assert_eq(initial_state.active_threads, 0)
  assert_eq(initial_state.idle_threads, 2) // Should create minimum threads
  assert_eq(initial_state.total_threads, 2)
  
  // Submit tasks to thread pool
  let task_results = []
  let completion_futures = []
  
  for i in 0..=10 {
    let future = ThreadPool::submit(thread_pool, fn() {
      Thread::sleep(100) // Simulate work
      "task_" + i.to_string() + "_completed"
    })
    completion_futures.push(future)
  }
  
  // Wait for all tasks to complete
  for future in completion_futures {
    let result = Future::get(future)
    task_results.push(result)
  }
  
  // Verify all tasks completed
  assert_eq(task_results.length(), 11)
  assert_eq(task_results[0], "task_0_completed")
  assert_eq(task_results[10], "task_10_completed")
  
  // Verify thread pool state
  let after_tasks_state = ThreadPool::get_state(thread_pool)
  assert_eq(after_tasks_state.active_threads, 0)
  assert_true(after_tasks_state.total_threads >= 2)
  assert_true(after_tasks_state.completed_tasks > 0)
  
  // Test task with return value
  let math_future = ThreadPool::submit(thread_pool, fn() {
    let mut sum = 0
    for i in 0..=100 {
      sum = sum + i
    }
    sum
  })
  
  let math_result = Future::get(math_future)
  assert_eq(math_result, 5050) // Sum of 0 to 100
  
  // Test task with exception
  let exception_future = ThreadPool::submit(thread_pool, fn() {
    panic!("Intentional exception for testing")
  })
  
  match Future::get(exception_future) {
    Ok(_) => assert_true(false) // Should not succeed
    Err(error) => assert_eq(error.message, "Intentional exception for testing")
  }
  
  // Test scheduled task
  let scheduled_future = ThreadPool::schedule(thread_pool, fn() {
    "scheduled_task_completed"
  }, 1000) // Execute after 1 second
  
  Thread::sleep(1100) // Wait for scheduled task
  let scheduled_result = Future::get(scheduled_future)
  assert_eq(scheduled_result, "scheduled_task_completed")
  
  // Test thread pool shutdown
  ThreadPool::shutdown(thread_pool)
  
  // Try to submit task after shutdown
  let shutdown_future = ThreadPool::submit(thread_pool, fn() {
    "should_not_execute"
  })
  
  match shutdown_future {
    Some(_) => assert_true(false) // Should not accept new tasks
    None => assert_true(true)
  }
}

// Test 4: Resource Cleanup and Lifecycle Management
test "resource cleanup and lifecycle management" {
  // Create resource lifecycle manager
  let lifecycle_manager = ResourceLifecycleManager::new()
  
  // Register resources with different cleanup strategies
  let file_resource = FileResource::new("test_file.txt")
  LifecycleManager::register(lifecycle_manager, file_resource, CleanupStrategy::Immediate)
  
  let db_connection = DatabaseConnection::new("test_db")
  LifecycleManager::register(lifecycle_manager, db_connection, CleanupStrategy::OnShutdown)
  
  let cache_resource = CacheResource::new("test_cache")
  LifecycleManager::register(lifecycle_manager, cache_resource, CleanupStrategy::Periodic(60000)) // 1 minute
  
  // Verify resource registration
  let registered_resources = LifecycleManager::get_registered_resources(lifecycle_manager)
  assert_eq(registered_resources.length(), 3)
  
  // Test immediate cleanup
  LifecycleManager::cleanup_resource(lifecycle_manager, file_resource.id)
  assert_false(FileResource::is_open(file_resource))
  
  // Test resource state tracking
  let resource_states = LifecycleManager::get_resource_states(lifecycle_manager)
  assert_eq(resource_states[file_resource.id], ResourceState::Released)
  assert_eq(resource_states[db_connection.id], ResourceState::Active)
  assert_eq(resource_states[cache_resource.id], ResourceState::Active)
  
  // Test resource dependency management
  let parent_resource = ParentResource::new("parent")
  let child_resource = ChildResource::new("child", parent_resource)
  
  LifecycleManager::register(lifecycle_manager, parent_resource, CleanupStrategy::OnShutdown)
  LifecycleManager::register(lifecycle_manager, child_resource, CleanupStrategy::OnShutdown)
  
  // Set up dependency
  LifecycleManager::add_dependency(lifecycle_manager, child_resource.id, parent_resource.id)
  
  // Test dependency-based cleanup
  LifecycleManager::cleanup_resource(lifecycle_manager, parent_resource.id)
  
  // Child should also be cleaned up due to dependency
  assert_false(ParentResource::is_active(parent_resource))
  assert_false(ChildResource::is_active(child_resource))
  
  // Test automatic cleanup on shutdown
  LifecycleManager::shutdown(lifecycle_manager)
  
  // All resources should be cleaned up
  let final_states = LifecycleManager::get_resource_states(lifecycle_manager)
  for (resource_id, state) in final_states {
    assert_eq(state, ResourceState::Released)
  }
}

// Test 5: Resource Quota Management
test "resource quota management" {
  // Create quota manager
  let quota_manager = QuotaManager::new()
  
  // Define resource quotas
  QuotaManager::set_quota(quota_manager, "cpu_usage", 80.0, QuotaUnit::Percent)
  QuotaManager::set_quota(quota_manager, "memory_usage", 1024.0, QuotaUnit::MB)
  QuotaManager::set_quota(quota_manager, "disk_io", 100.0, QuotaUnit::MBPerSecond)
  QuotaManager::set_quota(quota_manager, "network_io", 50.0, QuotaUnit::MBPerSecond)
  
  // Test quota checking within limits
  let cpu_usage = 50.0 // 50% CPU usage
  let cpu_check = QuotaManager::check_quota(quota_manager, "cpu_usage", cpu_usage)
  assert_true(cpu_check.within_limit)
  assert_eq(cpu_check.remaining_quota, 30.0) // 80% - 50%
  
  // Test quota exceeding limits
  let high_cpu_usage = 90.0 // 90% CPU usage
  let cpu_check_exceeded = QuotaManager::check_quota(quota_manager, "cpu_usage", high_cpu_usage)
  assert_false(cpu_check_exceeded.within_limit)
  assert_eq(cpu_check_exceeded.remaining_quota, 0.0)
  assert_eq(cpu_check_exceeded.exceeded_by, 10.0) // 90% - 80%
  
  // Test quota allocation
  let memory_allocation = QuotaManager::allocate(quota_manager, "memory_usage", 512.0)
  assert_true(memory_allocation.success)
  assert_eq(memory_allocation.allocated_amount, 512.0)
  assert_eq(memory_allocation.remaining_quota, 512.0) // 1024 - 512
  
  // Test quota deallocation
  let deallocation_result = QuotaManager::deallocate(quota_manager, "memory_usage", 256.0)
  assert_true(deallocation_result.success)
  
  let memory_state = QuotaManager::get_quota_state(quota_manager, "memory_usage")
  assert_eq(memory_state.used_amount, 256.0) // 512 - 256
  assert_eq(memory_state.available_amount, 768.0) // 1024 - 256
  
  // Test quota reservation
  let reservation = QuotaManager::reserve(quota_manager, "network_io", 30.0, 60000) // Reserve for 60 seconds
  assert_true(reservation.success)
  assert_eq(reservation.reserved_amount, 30.0)
  assert_eq(reservation.expiration_time, Time::current_timestamp() + 60000)
  
  // Check quota after reservation
  let network_state = QuotaManager::get_quota_state(quota_manager, "network_io")
  assert_eq(network_state.reserved_amount, 30.0)
  assert_eq(network_state.available_amount, 20.0) // 50 - 30
  
  // Test quota exceeding due to reservations
  let network_usage = QuotaManager::check_quota(quota_manager, "network_io", 25.0)
  assert_false(network_usage.within_limit) // 25 + 30 (reserved) > 50
  
  // Release reservation
  let release_result = QuotaManager::release_reservation(quota_manager, reservation.id)
  assert_true(release_result.success)
  
  // Verify quota is available again
  let network_usage_after = QuotaManager::check_quota(quota_manager, "network_io", 25.0)
  assert_true(network_usage_after.within_limit)
}

// Test 6: Resource Monitoring and Metrics
test "resource monitoring and metrics" {
  // Create resource monitor
  let resource_monitor = ResourceMonitor::new()
  
  // Start monitoring
  ResourceMonitor::start(resource_monitor)
  
  // Simulate resource usage
  let cpu_monitor = CPUMonitor::new()
  let memory_monitor = MemoryMonitor::new()
  let disk_monitor = DiskMonitor::new()
  let network_monitor = NetworkMonitor::new()
  
  // Get initial metrics
  let initial_cpu_metrics = CPUMonitor::get_metrics(cpu_monitor)
  let initial_memory_metrics = MemoryMonitor::get_metrics(memory_monitor)
  let initial_disk_metrics = DiskMonitor::get_metrics(disk_monitor)
  let initial_network_metrics = NetworkMonitor::get_metrics(network_monitor)
  
  // Simulate work to generate resource usage
  let work_data = []
  for i in 0..=10000 {
    work_data.push("work_item_" + i.to_string())
  }
  
  // Sort the data to consume CPU
  work_data.sort()
  
  // Get updated metrics
  let updated_cpu_metrics = CPUMonitor::get_metrics(cpu_monitor)
  let updated_memory_metrics = MemoryMonitor::get_metrics(memory_monitor)
  
  // Verify metrics changed
  assert_true(updated_cpu_metrics.usage_percentage >= initial_cpu_metrics.usage_percentage)
  assert_true(updated_memory_metrics.current_usage >= initial_memory_metrics.current_usage)
  
  // Test resource thresholds and alerts
  ResourceMonitor::set_threshold(resource_monitor, "cpu_usage", 80.0)
  ResourceMonitor::set_threshold(resource_monitor, "memory_usage", 512.0) // 512 MB
  
  // Simulate high resource usage
  ResourceMonitor::simulate_usage(resource_monitor, "cpu_usage", 85.0)
  ResourceMonitor::simulate_usage(resource_monitor, "memory_usage", 600.0)
  
  // Check for alerts
  let alerts = ResourceMonitor::get_alerts(resource_monitor)
  assert_eq(alerts.length(), 2)
  
  let cpu_alert = alerts.find(fn(alert) { alert.resource_type == "cpu_usage" })
  match cpu_alert {
    Some(alert) => {
      assert_eq(alert.severity, AlertSeverity::Warning)
      assert_eq(alert.current_value, 85.0)
      assert_eq(alert.threshold, 80.0)
    }
    None => assert_true(false)
  }
  
  let memory_alert = alerts.find(fn(alert) { alert.resource_type == "memory_usage" })
  match memory_alert {
    Some(alert) => {
      assert_eq(alert.severity, AlertSeverity::Warning)
      assert_eq(alert.current_value, 600.0)
      assert_eq(alert.threshold, 512.0)
    }
    None => assert_true(false)
  }
  
  // Test resource trend analysis
  let cpu_trend = ResourceMonitor::get_trend(resource_monitor, "cpu_usage", TimeRange::last_hour())
  assert_true(cpu_trend.data_points.length() > 0)
  
  let memory_trend = ResourceMonitor::get_trend(resource_monitor, "memory_usage", TimeRange::last_hour())
  assert_true(memory_trend.data_points.length() > 0)
  
  // Test resource efficiency metrics
  let efficiency_metrics = ResourceMonitor::get_efficiency_metrics(resource_monitor)
  assert_true(efficiency_metrics.cpu_efficiency >= 0.0)
  assert_true(efficiency_metrics.cpu_efficiency <= 1.0)
  assert_true(efficiency_metrics.memory_efficiency >= 0.0)
  assert_true(efficiency_metrics.memory_efficiency <= 1.0)
  
  // Test resource capacity planning
  let capacity_report = ResourceMonitor::generate_capacity_report(resource_monitor)
  assert_true(capacity_report.contains("Resource Capacity Report"))
  assert_true(capacity_report.contains("CPU Usage"))
  assert_true(capacity_report.contains("Memory Usage"))
  assert_true(capacity_report.contains("Recommendations"))
  
  // Stop monitoring
  ResourceMonitor::stop(resource_monitor)
}

// Test 7: Resource Allocation and Scheduling
test "resource allocation and scheduling" {
  // Create resource scheduler
  let scheduler = ResourceScheduler::new()
  
  // Define resource pools
  let cpu_pool = ResourcePool::new("cpu", 8.0, ResourceUnit::Cores)
  let memory_pool = ResourcePool::new("memory", 4096.0, ResourceUnit::MB)
  let disk_pool = ResourcePool::new("disk", 100.0, ResourceUnit::GB)
  
  // Register pools with scheduler
  ResourceScheduler::register_pool(scheduler, cpu_pool)
  ResourceScheduler::register_pool(scheduler, memory_pool)
  ResourceScheduler::register_pool(scheduler, disk_pool)
  
  // Define resource requirements for tasks
  let task1_requirements = ResourceRequirements::new([
    ("cpu", 2.0),
    ("memory", 512.0),
    ("disk", 10.0)
  ])
  
  let task2_requirements = ResourceRequirements::new([
    ("cpu", 4.0),
    ("memory", 1024.0),
    ("disk", 20.0)
  ])
  
  let task3_requirements = ResourceRequirements::new([
    ("cpu", 1.0),
    ("memory", 256.0),
    ("disk", 5.0)
  ])
  
  // Test resource allocation
  let task1_allocation = ResourceScheduler::allocate(scheduler, "task1", task1_requirements)
  assert_true(task1_allocation.success)
  
  let task2_allocation = ResourceScheduler::allocate(scheduler, "task2", task2_requirements)
  assert_true(task2_allocation.success)
  
  let task3_allocation = ResourceScheduler::allocate(scheduler, "task3", task3_requirements)
  assert_true(task3_allocation.success)
  
  // Check pool states after allocations
  let cpu_pool_state = ResourcePool::get_state(cpu_pool)
  assert_eq(cpu_pool_state.allocated, 7.0) // 2 + 4 + 1
  assert_eq(cpu_pool_state.available, 1.0) // 8 - 7
  
  let memory_pool_state = ResourcePool::get_state(memory_pool)
  assert_eq(memory_pool_state.allocated, 1792.0) // 512 + 1024 + 256
  assert_eq(memory_pool_state.available, 2304.0) // 4096 - 1792
  
  // Test allocation failure due to insufficient resources
  let large_task_requirements = ResourceRequirements::new([
    ("cpu", 5.0), // Only 1.0 available
    ("memory", 2048.0),
    ("disk", 50.0)
  ])
  
  let large_task_allocation = ResourceScheduler::allocate(scheduler, "large_task", large_task_requirements)
  assert_false(large_task_allocation.success)
  
  // Test resource deallocation
  ResourceScheduler::deallocate(scheduler, "task1")
  
  // Check pool states after deallocation
  let cpu_pool_state_after = ResourcePool::get_state(cpu_pool)
  assert_eq(cpu_pool_state_after.allocated, 5.0) // 7 - 2
  assert_eq(cpu_pool_state_after.available, 3.0) // 1 + 2
  
  // Test priority-based allocation
  ResourceScheduler::set_priority(scheduler, "high_priority_task", Priority::High)
  
  let high_priority_requirements = ResourceRequirements::new([
    ("cpu", 2.0),
    ("memory", 1024.0),
    ("disk", 15.0)
  ])
  
  // This should succeed even if resources are tight due to high priority
  let high_priority_allocation = ResourceScheduler::allocate(scheduler, "high_priority_task", high_priority_requirements)
  assert_true(high_priority_allocation.success)
  
  // Test resource reservation
  let reservation = ResourceScheduler::reserve(scheduler, "future_task", task1_requirements, 60000) // Reserve for 1 minute
  assert_true(reservation.success)
  
  // Check that reserved resources are not available for other tasks
  let another_task_allocation = ResourceScheduler::allocate(scheduler, "another_task", task1_requirements)
  assert_false(another_task_allocation.success) // Should fail due to reservation
  
  // Test reservation fulfillment
  let fulfillment_result = ResourceScheduler::fulfill_reservation(scheduler, reservation.id)
  assert_true(fulfillment_result.success)
}

// Test 8: Resource Optimization and Scaling
test "resource optimization and scaling" {
  // Create resource optimizer
  let optimizer = ResourceOptimizer::new()
  
  // Define scaling policies
  let cpu_scaling_policy = ScalingPolicy::new(
    "cpu",
    70.0,  // Scale up threshold
    30.0,  // Scale down threshold
    1,     // Min instances
    5,     // Max instances
    2      // Scale up/down step
  )
  
  let memory_scaling_policy = ScalingPolicy::new(
    "memory",
    80.0,  // Scale up threshold
    40.0,  // Scale down threshold
    1,     // Min instances
    3,     // Max instances
    1      // Scale up/down step
  )
  
  // Register policies with optimizer
  ResourceOptimizer::register_policy(optimizer, cpu_scaling_policy)
  ResourceOptimizer::register_policy(optimizer, memory_scaling_policy)
  
  // Create resource group to manage
  let resource_group = ResourceGroup::new("web_servers", [
    ResourceInstance::new("server1", ["cpu", "memory"]),
    ResourceInstance::new("server2", ["cpu", "memory"])
  ])
  
  // Simulate resource usage
  ResourceOptimizer::set_usage(optimizer, "server1", "cpu", 80.0) // Above scale up threshold
  ResourceOptimizer::set_usage(optimizer, "server1", "memory", 85.0) // Above scale up threshold
  ResourceOptimizer::set_usage(optimizer, "server2", "cpu", 75.0) // Above scale up threshold
  ResourceOptimizer::set_usage(optimizer, "server2", "memory", 40.0) // Within thresholds
  
  // Run optimization
  let optimization_result = ResourceOptimizer::optimize(optimizer, resource_group)
  assert_true(optimization_result.success)
  
  // Verify scaling decisions
  assert_eq(optimization_result.scaling_actions.length(), 2) // Should scale up both CPU and memory
  
  let cpu_scaling = optimization_result.scaling_actions.find(fn(action) { action.resource_type == "cpu" })
  match cpu_scaling {
    Some(action) => {
      assert_eq(action.action_type, ScalingActionType::ScaleUp)
      assert_eq(action.new_instance_count, 4) // 2 + 2 (scale up step)
    }
    None => assert_true(false)
  }
  
  let memory_scaling = optimization_result.scaling_actions.find(fn(action) { action.resource_type == "memory" })
  match memory_scaling {
    Some(action) => {
      assert_eq(action.action_type, ScalingActionType::ScaleUp)
      assert_eq(action.new_instance_count, 3) // 2 + 1 (scale up step)
    }
    None => assert_true(false)
  }
  
  // Simulate decreased resource usage
  ResourceOptimizer::set_usage(optimizer, "server1", "cpu", 25.0) // Below scale down threshold
  ResourceOptimizer::set_usage(optimizer, "server1", "memory", 35.0) // Below scale down threshold
  ResourceOptimizer::set_usage(optimizer, "server2", "cpu", 20.0) // Below scale down threshold
  ResourceOptimizer::set_usage(optimizer, "server2", "memory", 30.0) // Below scale down threshold
  
  // Run optimization again
  let optimization_result2 = ResourceOptimizer::optimize(optimizer, resource_group)
  assert_true(optimization_result2.success)
  
  // Verify scale down decisions
  assert_eq(optimization_result2.scaling_actions.length(), 2) // Should scale down both CPU and memory
  
  let cpu_scaling_down = optimization_result2.scaling_actions.find(fn(action) { action.resource_type == "cpu" })
  match cpu_scaling_down {
    Some(action) => {
      assert_eq(action.action_type, ScalingActionType::ScaleDown)
      assert_eq(action.new_instance_count, 2) // 4 - 2 (scale down step)
    }
    None => assert_true(false)
  }
  
  // Test cost optimization
  let cost_optimizer = CostOptimizer::new()
  
  // Define cost models
  CostOptimizer::set_cost_model(cost_optimizer, "cpu", 0.05) // $0.05 per CPU hour
  CostOptimizer::set_cost_model(cost_optimizer, "memory", 0.01) // $0.01 per GB hour
  
  // Get cost optimization recommendations
  let cost_recommendations = CostOptimizer::get_recommendations(cost_optimizer, resource_group)
  assert_true(cost_recommendations.length() > 0)
  
  // Verify recommendations include cost-saving opportunities
  let has_cost_saving = cost_recommendations.any(fn(rec) { rec.potential_savings > 0.0 })
  assert_true(has_cost_saving)
  
  // Test performance optimization
  let performance_optimizer = PerformanceOptimizer::new()
  
  // Get performance optimization recommendations
  let performance_recommendations = PerformanceOptimizer::get_recommendations(performance_optimizer, resource_group)
  assert_true(performance_recommendations.length() > 0)
  
  // Verify recommendations include performance improvements
  let has_performance_improvement = performance_recommendations.any(fn(rec) {
    rec.expected_performance_gain > 0.0
  })
  assert_true(has_performance_improvement)
}