// Azimuth Telemetry System - Scalability and Load Tests
// This file contains comprehensive test cases for scalability and load testing

// Test 1: Horizontal Scaling with Multiple Nodes
test "horizontal scaling with multiple nodes" {
  // Create cluster manager
  let cluster_manager = ClusterManager::new()
  
  // Initialize cluster with different node counts
  let node_counts = [1, 2, 4, 8, 16]
  
  for node_count in node_counts {
    // Create cluster with specified number of nodes
    let cluster = ClusterManager::create_cluster(cluster_manager, node_count)
    
    // Distribute workload across nodes
    let workload_size = 100000
    let workload = generate_scalability_workload(workload_size)
    let distributed_workload = ClusterManager::distribute_workload(cluster, workload)
    
    // Measure processing time
    let start_time = Time::now()
    let results = ClusterManager::process_distributed_workload(cluster, distributed_workload)
    let end_time = Time::now()
    let processing_time = end_time - start_time
    
    // Calculate throughput
    let throughput = workload_size / processing_time
    
    // Record scalability metrics
    ClusterManager::record_metrics(cluster, node_count, processing_time, throughput)
    
    // Verify scalability expectations
    // Throughput should increase with more nodes (though not perfectly linear)
    if node_count > 1 {
      let single_node_metrics = ClusterManager::get_metrics(cluster_manager, 1)
      let speedup = throughput / single_node_metrics.throughput
      let efficiency = speedup / node_count.to_float()
      
      // Efficiency should be reasonable (accounting for communication overhead)
      assert_true(efficiency > 0.5) // At least 50% efficiency
    }
    
    // Verify load balancing
    let node_loads = ClusterManager::get_node_loads(cluster)
    let avg_load = node_loads.fold(0.0, fn(acc, load) { acc + load }) / node_loads.length().to_float()
    
    for load in node_loads {
      // Load should be balanced within 20% of average
      assert_true(load >= avg_load * 0.8 && load <= avg_load * 1.2)
    }
    
    // Verify result consistency
    assert_eq(results.length(), workload_size)
    let accuracy = ClusterManager::calculate_accuracy(cluster, results)
    assert_true(accuracy > 0.99) // At least 99% accuracy
    
    // Clean up cluster
    ClusterManager::cleanup_cluster(cluster_manager, cluster)
  }
  
  // Analyze scalability curve
  let scalability_analysis = ClusterManager::analyze_scalability(cluster_manager)
  
  // Verify good scalability characteristics
  assert_true(scalability_analysis.max_efficiency > 0.7) // At least 70% efficiency at optimal node count
  assert_true(scalability_analysis.optimal_node_count >= 4) // Should benefit from multiple nodes
  
  // Test dynamic scaling
  let dynamic_cluster = ClusterManager::create_cluster(cluster_manager, 4)
  let dynamic_workload = generate_scalability_workload(50000)
  
  // Start processing with 4 nodes
  let distributed_workload = ClusterManager::distribute_workload(dynamic_cluster, dynamic_workload)
  ClusterManager::start_processing(dynamic_cluster, distributed_workload)
  
  // Scale up to 8 nodes during processing
  ClusterManager::scale_up(dynamic_cluster, 4)
  
  // Verify scaling without data loss
  let dynamic_results = ClusterManager::get_results(dynamic_cluster)
  assert_eq(dynamic_results.length(), 50000) // No data loss during scaling
  
  // Scale down to 2 nodes
  ClusterManager::scale_down(dynamic_cluster, 6)
  
  // Verify results consistency after scaling down
  let final_results = ClusterManager::get_results(dynamic_cluster)
  assert_eq(final_results.length(), 50000) // No data loss during scaling
  let final_accuracy = ClusterManager::calculate_accuracy(dynamic_cluster, final_results)
  assert_true(final_accuracy > 0.99) // Accuracy maintained
  
  // Clean up
  ClusterManager::cleanup_cluster(cluster_manager, dynamic_cluster)
}

// Test 2: Vertical Scaling with Increased Resources
test "vertical scaling with increased resources" {
  // Create resource manager
  let resource_manager = ResourceManager::new()
  
  // Test different resource configurations
  let resource_configs = [
    ResourceConfig::new(2, 4096, 10),    // 2 cores, 4GB RAM, 10GB disk
    ResourceConfig::new(4, 8192, 20),    // 4 cores, 8GB RAM, 20GB disk
    ResourceConfig::new(8, 16384, 40),   // 8 cores, 16GB RAM, 40GB disk
    ResourceConfig::new(16, 32768, 80)   // 16 cores, 32GB RAM, 80GB disk
  ]
  
  for config in resource_configs {
    // Create node with specified resources
    let node = ResourceManager::create_node(resource_manager, config)
    
    // Generate workload proportional to resources
    let workload_size = config.cpu_cores * 25000 // 25K items per core
    let workload = generate_cpu_intensive_workload(workload_size)
    
    // Measure resource utilization during processing
    let utilization_monitor = ResourceUtilizationMonitor::new()
    ResourceUtilizationMonitor::start_monitoring(utilization_monitor, node)
    
    // Process workload
    let start_time = Time::now()
    let results = ResourceManager::process_workload(node, workload)
    let end_time = Time::now()
    let processing_time = end_time - start_time
    
    // Stop monitoring
    let utilization = ResourceUtilizationMonitor::stop_monitoring(utilization_monitor)
    
    // Calculate throughput
    let throughput = workload_size / processing_time
    
    // Record vertical scaling metrics
    ResourceManager::record_metrics(node, config, processing_time, throughput, utilization)
    
    // Verify resource utilization is reasonable
    assert_true(utilization.cpu_utilization > 0.7) // Should utilize at least 70% of CPU
    assert_true(utilization.memory_utilization > 0.5) // Should utilize at least 50% of memory
    assert_true(utilization.disk_utilization < 0.8) // Should not exceed 80% disk usage
    
    // Verify throughput scales with resources
    if config.cpu_cores > 2 {
      let baseline_config = resource_configs[0]
      let baseline_metrics = ResourceManager::get_metrics(resource_manager, baseline_config)
      
      let cpu_scaling_factor = config.cpu_cores / baseline_config.cpu_cores
      let throughput_scaling = throughput / baseline_metrics.throughput
      
      // Throughput should scale reasonably with CPU cores
      assert_true(throughput_scaling > cpu_scaling_factor * 0.6) // At least 60% of ideal scaling
    }
    
    // Verify result quality
    assert_eq(results.length(), workload_size)
    let accuracy = ResourceManager::calculate_accuracy(node, results)
    assert_true(accuracy > 0.99) // At least 99% accuracy
    
    // Clean up
    ResourceManager::cleanup_node(resource_manager, node)
  }
  
  // Analyze vertical scaling efficiency
  let scaling_analysis = ResourceManager::analyze_vertical_scaling(resource_manager)
  
  // Verify good vertical scaling characteristics
  assert_true(scaling_analysis.cpu_efficiency > 0.6) // At least 60% CPU efficiency
  assert_true(scaling_analysis.memory_efficiency > 0.7) // At least 70% memory efficiency
  
  // Test resource hot-plugging (adding resources during operation)
  let hotplug_node = ResourceManager::create_node(resource_manager, resource_configs[1]) // Start with 4 cores, 8GB RAM
  let hotplug_workload = generate_cpu_intensive_workload(100000)
  
  // Start processing
  ResourceManager::start_processing(hotplug_node, hotplug_workload)
  
  // Add CPU cores during processing
  ResourceManager::add_cpu_cores(hotplug_node, 4) // Add 4 more cores (total 8)
  
  // Add memory during processing
  ResourceManager::add_memory(hotplug_node, 8192) // Add 8GB more RAM (total 16GB)
  
  // Verify processing continues without interruption
  let hotplug_results = ResourceManager::get_results(hotplug_node)
  assert_eq(hotplug_results.length(), 100000) // No data loss during hot-plugging
  
  // Verify improved performance after adding resources
  let post_hotplug_throughput = ResourceManager::measure_throughput(hotplug_node)
  let pre_hotplug_metrics = ResourceManager::get_current_metrics(hotplug_node)
  
  assert_true(post_hotplug_throughput > pre_hotplug_metrics.throughput * 1.3) // At least 30% improvement
  
  // Clean up
  ResourceManager::cleanup_node(resource_manager, hotplug_node)
}

// Test 3: Data Volume Scaling
test "data volume scaling" {
  // Create data volume tester
  let data_tester = DataVolumeTester::new()
  
  // Test with different data volumes
  let data_volumes = [1000000, 10000000, 100000000, 1000000000] // 1M to 1B items
  
  for volume in data_volumes {
    // Generate test data
    let test_data = generate_large_telemetry_dataset(volume)
    
    // Test ingestion performance
    let ingestion_start = Time::now()
    let ingestion_result = DataVolumeTester::ingest_data(data_tester, test_data)
    let ingestion_end = Time::now()
    let ingestion_time = ingestion_end - ingestion_start
    let ingestion_throughput = volume / ingestion_time
    
    // Test storage performance
    let storage_start = Time::now()
    let storage_result = DataVolumeTester::store_data(data_tester, ingestion_result)
    let storage_end = Time::now()
    let storage_time = storage_end - storage_start
    let storage_throughput = volume / storage_time
    
    // Test query performance
    let query_start = Time::now()
    let query_result = DataVolumeTester::query_data(data_tester, volume / 1000) // Query 1% of data
    let query_end = Time::now()
    let query_time = query_end - query_start
    let query_throughput = (volume / 1000) / query_time
    
    // Test aggregation performance
    let aggregation_start = Time::now()
    let aggregation_result = DataVolumeTester::aggregate_data(data_tester)
    let aggregation_end = Time::now()
    let aggregation_time = aggregation_end - aggregation_start
    let aggregation_throughput = volume / aggregation_time
    
    // Record metrics
    DataVolumeTester::record_metrics(data_tester, volume, ingestion_throughput, 
                                   storage_throughput, query_throughput, aggregation_throughput)
    
    // Verify performance expectations
    // Throughput should remain reasonable even with large volumes
    assert_true(ingestion_throughput > 10000)   // At least 10K items/second
    assert_true(storage_throughput > 5000)     // At least 5K items/second
    assert_true(query_throughput > 1000)       // At least 1K items/second
    assert_true(aggregation_throughput > 100)  // At least 100 items/second
    
    // Query time should scale sub-linearly with data volume
    if volume > 1000000 {
      let baseline_metrics = DataVolumeTester::get_metrics(data_tester, 1000000)
      let volume_scaling = volume / 1000000
      let query_time_scaling = query_time / baseline_metrics.query_time
      
      // Query time should scale less than linearly
      assert_true(query_time_scaling < volume_scaling * 0.8) // Less than 80% of linear scaling
    }
    
    // Verify data integrity
    assert_eq(ingestion_result.processed_count, volume)
    assert_eq(storage_result.stored_count, volume)
    assert_eq(query_result.returned_count, volume / 1000)
    assert_true(aggregation_result.is_complete)
    
    // Clean up
    DataVolumeTester::cleanup_data(data_tester, volume)
  }
  
  // Analyze data volume scaling
  let scaling_analysis = DataVolumeTester::analyze_scaling(data_tester)
  
  // Verify good scaling characteristics
  assert_true(scaling_analysis.ingestion_scaling_factor < 2.0) // Ingestion time should less than double with 10x data
  assert_true(scaling_analysis.query_scaling_factor < 3.0)   // Query time should less than triple with 10x data
  
  // Test data partitioning
  let partition_test_data = generate_large_telemetry_dataset(500000000) // 500M items
  let partition_strategies = ["hash", "range", "time_based", "consistent_hashing"]
  
  for strategy in partition_strategies {
    // Partition data using strategy
    let partitioned_data = DataVolumeTester::partition_data(data_tester, partition_test_data, strategy, 10)
    
    // Test distributed processing of partitions
    let distributed_start = Time::now()
    let distributed_result = DataVolumeTester::process_partitions(data_tester, partitioned_data)
    let distributed_end = Time::now()
    let distributed_time = distributed_end - distributed_start
    
    // Test sequential processing for comparison
    let sequential_start = Time::now()
    let sequential_result = DataVolumeTester::process_sequential(data_tester, partition_test_data)
    let sequential_end = Time::now()
    let sequential_time = sequential_end - sequential_start
    
    // Calculate speedup
    let speedup = sequential_time / distributed_time
    
    // Record partition metrics
    DataVolumeTester::record_partition_metrics(data_tester, strategy, speedup)
    
    // Verify partitioning improves performance
    assert_true(speedup > 2.0) // At least 2x speedup with partitioning
    
    // Verify result consistency
    assert_eq(distributed_result.processed_count, sequential_result.processed_count)
    let consistency = DataVolumeTester::compare_results(distributed_result, sequential_result)
    assert_true(consistency > 0.99) // At least 99% consistency
    
    // Clean up
    DataVolumeTester::cleanup_partitions(data_tester, partitioned_data)
  }
  
  // Analyze partitioning strategies
  let partition_analysis = DataVolumeTester::analyze_partitioning(data_tester)
  
  // Verify optimal partitioning strategy
  assert_true(partition_analysis.best_speedup > 3.0) // Best strategy should provide at least 3x speedup
}

// Test 4: Concurrent User Scaling
test "concurrent user scaling" {
  // Create concurrent user tester
  let user_tester = ConcurrentUserTester::new()
  
  // Test with different numbers of concurrent users
  let user_counts = [100, 500, 1000, 5000, 10000]
  
  for user_count in user_counts {
    // Simulate concurrent users
    let users = ConcurrentUserTester::create_users(user_tester, user_count)
    
    // Start monitoring system resources
    let monitor = SystemMonitor::new()
    SystemMonitor::start_monitoring(monitor)
    
    // Simulate user operations
    let operations_start = Time::now()
    let operation_results = ConcurrentUserTester::simulate_operations(user_tester, users)
    let operations_end = Time::now()
    let operations_time = operations_end - operations_start
    
    // Stop monitoring
    let system_metrics = SystemMonitor::stop_monitoring(monitor)
    
    // Calculate metrics
    let total_operations = user_count * 10 // 10 operations per user
    let operations_per_second = total_operations / operations_time
    let average_response_time = operation_results.fold(0, fn(acc, result) { acc + result.response_time }) / operation_results.length()
    let error_rate = operation_results.filter(fn(result) { result.is_error }).length().to_float() / operation_results.length().to_float()
    
    // Record metrics
    ConcurrentUserTester::record_metrics(user_tester, user_count, operations_per_second, 
                                       average_response_time, error_rate, system_metrics)
    
    // Verify performance expectations
    // System should handle increasing load gracefully
    assert_true(operations_per_second > 100) // At least 100 operations per second
    assert_true(average_response_time < 5000) // Average response time less than 5 seconds
    assert_true(error_rate < 0.05) // Error rate less than 5%
    
    // Resource utilization should be reasonable
    assert_true(system_metrics.cpu_utilization < 90.0) // CPU usage less than 90%
    assert_true(system_metrics.memory_utilization < 85.0) // Memory usage less than 85%
    assert_true(system_metrics.disk_io_utilization < 80.0) // Disk I/O less than 80%
    
    // Verify scaling characteristics
    if user_count > 100 {
      let baseline_metrics = ConcurrentUserTester::get_metrics(user_tester, 100)
      let user_scaling = user_count / 100
      let throughput_scaling = operations_per_second / baseline_metrics.operations_per_second
      
      // Throughput should scale reasonably with user count
      assert_true(throughput_scaling > user_scaling * 0.3) // At least 30% of ideal scaling
      
      // Response time degradation should be limited
      let response_time_degradation = average_response_time / baseline_metrics.average_response_time
      assert_true(response_time_degradation < user_scaling * 0.5) // Less than 50% of linear degradation
    }
    
    // Clean up
    ConcurrentUserTester::cleanup_users(user_tester, users)
  }
  
  // Analyze concurrent user scaling
  let scaling_analysis = ConcurrentUserTester::analyze_scaling(user_tester)
  
  // Verify good scaling characteristics
  assert_true(scaling_analysis.max_concurrent_users > 5000) // Should support at least 5000 concurrent users
  assert_true(scaling_analysis.response_time_degradation < 10.0) // Response time shouldn't degrade more than 10x
  
  // Test load balancing strategies
  let load_balancer = LoadBalancer::new()
  let balancing_strategies = ["round_robin", "least_connections", "weighted_round_robin", "ip_hash"]
  
  for strategy in balancing_strategies {
    // Configure load balancer with strategy
    LoadBalancer::set_strategy(load_balancer, strategy)
    
    // Create backend servers
    let servers = LoadBalancer::create_servers(load_balancer, 5)
    
    // Distribute load
    let test_users = ConcurrentUserTester::create_users(user_tester, 1000)
    let load_distribution = LoadBalancer::distribute_load(load_balancer, test_users, servers)
    
    // Measure performance
    let performance_metrics = LoadBalancer::measure_performance(load_balancer, load_distribution)
    
    // Record load balancing metrics
    LoadBalancer::record_metrics(load_balancer, strategy, performance_metrics)
    
    // Verify load balancing effectiveness
    let server_loads = performance_metrics.server_loads
    let avg_load = server_loads.fold(0, fn(acc, load) { acc + load }) / server_loads.length()
    
    for load in server_loads {
      // Load should be balanced within 30% of average
      assert_true(load >= avg_load * 0.7 && load <= avg_load * 1.3)
    }
    
    // Verify overall performance
    assert_true(performance_metrics.average_response_time < 3000) // Less than 3 seconds
    assert_true(performance_metrics.error_rate < 0.03) // Less than 3% error rate
    
    // Clean up
    LoadBalancer::cleanup_servers(load_balancer, servers)
    ConcurrentUserTester::cleanup_users(user_tester, test_users)
  }
  
  // Analyze load balancing strategies
  let balancing_analysis = LoadBalancer::analyze_strategies(load_balancer)
  
  // Verify optimal strategy
  assert_true(balancing_analysis.best_response_time < 2000) // Best strategy should have response time less than 2 seconds
}

// Test 5: Geographic Distribution Scaling
test "geographic distribution scaling" {
  // Create geo-distribution manager
  let geo_manager = GeoDistributionManager::new()
  
  // Define geographic regions
  let regions = [
    GeoRegion::new("us-east-1", "North Virginia", "US East"),
    GeoRegion::new("us-west-2", "Oregon", "US West"),
    GeoRegion::new("eu-west-1", "Ireland", "Europe"),
    GeoRegion::new("ap-southeast-1", "Singapore", "Asia Pacific"),
    GeoRegion::new("ap-northeast-1", "Tokyo", "Asia Pacific")
  ]
  
  // Test with different numbers of regions
  let region_counts = [1, 2, 3, 5]
  
  for region_count in region_counts {
    // Select regions for this test
    let selected_regions = regions.slice(0, region_count)
    
    // Deploy to selected regions
    let deployment = GeoDistributionManager::deploy_regions(geo_manager, selected_regions)
    
    // Generate geo-distributed workload
    let workload_size = 100000
    let geo_workload = generate_geo_distributed_workload(workload_size, selected_regions)
    
    // Process workload across regions
    let processing_start = Time::now()
    let processing_result = GeoDistributionManager::process_workload(geo_manager, deployment, geo_workload)
    let processing_end = Time::now()
    let processing_time = processing_end - processing_start
    
    // Calculate metrics
    let throughput = workload_size / processing_time
    let average_latency = processing_result.fold(0, fn(acc, result) { acc + result.latency }) / processing_result.length()
    let data_transfer_cost = GeoDistributionManager::calculate_transfer_cost(geo_manager, deployment, processing_result)
    
    // Record metrics
    GeoDistributionManager::record_metrics(geo_manager, region_count, processing_time, throughput, 
                                         average_latency, data_transfer_cost)
    
    // Verify performance expectations
    assert_true(throughput > 5000) // At least 5K items/second
    assert_true(average_latency < 2000) // Average latency less than 2 seconds
    
    // Latency should improve with more regions (closer to users)
    if region_count > 1 {
      let single_region_metrics = GeoDistributionManager::get_metrics(geo_manager, 1)
      let latency_improvement = single_region_metrics.average_latency / average_latency
      
      assert_true(latency_improvement > 1.2) // At least 20% latency improvement with multiple regions
    }
    
    // Verify data consistency across regions
    let consistency_check = GeoDistributionManager::check_consistency(geo_manager, deployment)
    assert_true(consistency_check.consistency_rate > 0.95) // At least 95% consistency
    
    // Test cross-region failover
    let failover_result = GeoDistributionManager::test_failover(geo_manager, deployment, selected_regions[0])
    assert_true(failover_result.is_successful)
    assert_true(failover_result.downtime < 5000) // Less than 5 seconds downtime
    assert_true(failover_result.data_loss < 0.01) // Less than 1% data loss
    
    // Clean up
    GeoDistributionManager::cleanup_deployment(geo_manager, deployment)
  }
  
  // Analyze geographic distribution scaling
  let scaling_analysis = GeoDistributionManager::analyze_scaling(geo_manager)
  
  // Verify good scaling characteristics
  assert_true(scaling_analysis.latency_improvement > 1.5) // At least 50% latency improvement with multiple regions
  assert_true(scaling_analysis.consistency_rate > 0.95) // At least 95% consistency across regions
  
  // Test data replication strategies
  let replication_strategies = ["sync", "async", "eventual", "quorum"]
  
  for strategy in replication_strategies {
    // Configure replication strategy
    GeoDistributionManager::set_replication_strategy(geo_manager, strategy)
    
    // Deploy with replication
    let replication_deployment = GeoDistributionManager::deploy_with_replication(geo_manager, regions, strategy)
    
    // Test write performance
    let write_workload = generate_write_workload(10000)
    let write_start = Time::now()
    let write_result = GeoDistributionManager::process_write_workload(geo_manager, replication_deployment, write_workload)
    let write_end = Time::now()
    let write_time = write_end - write_start
    let write_throughput = 10000 / write_time
    
    // Test read performance
    let read_workload = generate_read_workload(10000)
    let read_start = Time::now()
    let read_result = GeoDistributionManager::process_read_workload(geo_manager, replication_deployment, read_workload)
    let read_end = Time::now()
    let read_time = read_end - read_start
    let read_throughput = 10000 / read_time
    
    // Test consistency guarantees
    let consistency_check = GeoDistributionManager::test_consistency_guarantees(geo_manager, replication_deployment)
    
    // Record replication metrics
    GeoDistributionManager::record_replication_metrics(geo_manager, strategy, write_throughput, 
                                                    read_throughput, consistency_check)
    
    // Verify replication characteristics
    assert_true(write_throughput > 100) // At least 100 writes/second
    assert_true(read_throughput > 1000) // At least 1000 reads/second
    
    // Different strategies have different trade-offs
    match strategy {
      "sync" => {
        assert_true(consistency_check.strong_consistency) // Sync replication should provide strong consistency
        assert_true(write_throughput < 500) // But lower write throughput
      }
      "async" => {
        assert_false(consistency_check.strong_consistency) // Async replication doesn't provide strong consistency
        assert_true(write_throughput > 500) // But higher write throughput
      }
      "eventual" => {
        assert_false(consistency_check.strong_consistency) // Eventual consistency
        assert_true(write_throughput > 1000) // Highest write throughput
      }
      "quorum" => {
        assert_true(consistency_check.quorum_consistency) // Quorum consistency
        assert_true(write_throughput > 200) // Moderate write throughput
      }
      _ => {}
    }
    
    // Clean up
    GeoDistributionManager::cleanup_deployment(geo_manager, replication_deployment)
  }
  
  // Analyze replication strategies
  let replication_analysis = GeoDistributionManager::analyze_replication(geo_manager)
  
  // Verify optimal strategies for different use cases
  assert_true(replication_analysis.best_read_throughput > 2000) // Best strategy for reads should provide at least 2000 reads/second
  assert_true(replication_analysis.best_write_throughput > 500) // Best strategy for writes should provide at least 500 writes/second
}

// Helper functions for generating test data
fn generate_scalability_workload(size : Int) : Array[WorkloadItem] {
  let workload = []
  
  for i in 0..=size {
    let item = WorkloadItem::new(
      "item_" + i.to_string(),
      (Math::random() * 100).to_float(),
      Attributes::with_data([
        ("category", StringValue("cat_" + (i % 10).to_string())),
        ("priority", IntValue(i % 5))
      ])
    )
    workload.push(item)
  }
  
  workload
}

fn generate_cpu_intensive_workload(size : Int) : Array[CPUIntensiveTask] {
  let workload = []
  
  for i in 0..=size {
    let task = CPUIntensiveTask::new(
      "task_" + i.to_string(),
      1000 + (i % 5000), // 1-6 seconds of CPU work
      (Math::random() * 1000000).to_int() // Random data for processing
    )
    workload.push(task)
  }
  
  workload
}

fn generate_geo_distributed_workload(size : Int, regions : Array[GeoRegion]) : Array[GeoWorkloadItem] {
  let workload = []
  
  for i in 0..=size {
    let region = regions[i % regions.length()]
    let item = GeoWorkloadItem::new(
      "item_" + i.to_string(),
      region,
      (Math::random() * 100).to_float(),
      Attributes::with_data([
        ("user_location", StringValue(region.name)),
        ("request_time", IntValue(Time::now()))
      ])
    )
    workload.push(item)
  }
  
  workload
}

fn generate_write_workload(size : Int) : Array[WriteOperation] {
  let workload = []
  
  for i in 0..=size {
    let operation = WriteOperation::new(
      "write_" + i.to_string(),
      TelemetryPoint::new(
        "metric_" + (i % 10).to_string(),
        (Math::random() * 100).to_float(),
        Attributes::with_data([
          ("source", StringValue("source_" + (i % 5).to_string())),
          ("timestamp", IntValue(Time::now()))
        ])
      )
    )
    workload.push(operation)
  }
  
  workload
}

fn generate_read_workload(size : Int) : Array[ReadOperation] {
  let workload = []
  
  for i in 0..=size {
    let operation = ReadOperation::new(
      "read_" + i.to_string(),
      "metric_" + (i % 10).to_string(),
      Time::now() - (i * 1000) // Read from different time points
    )
    workload.push(operation)
  }
  
  workload
}