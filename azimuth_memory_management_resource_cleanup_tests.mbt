// Azimuth Memory Management Resource Cleanup Tests
// This file contains test cases for memory management and resource cleanup functionality

// Test 1: Basic Memory Allocation and Deallocation
test "basic memory allocation and deallocation" {
  let memory_manager = MemoryManager::new()
  
  // Test basic allocation
  let ptr1 = MemoryManager::allocate(memory_manager, 1024)  // 1KB
  assert_eq(ptr1 != null, true)
  
  let ptr2 = MemoryManager::allocate(memory_manager, 2048)  // 2KB
  assert_eq(ptr2 != null, true)
  
  // Test memory usage tracking
  let initial_usage = MemoryManager::get_usage(memory_manager)
  assert_eq(initial_usage.allocated_bytes, 3072)  // 1024 + 2048
  assert_eq(initial_usage.allocated_blocks, 2)
  
  // Test deallocation
  MemoryManager::deallocate(memory_manager, ptr1)
  
  let after_dealloc = MemoryManager::get_usage(memory_manager)
  assert_eq(after_dealloc.allocated_bytes, 2048)
  assert_eq(after_dealloc.allocated_blocks, 1)
  
  // Test deallocation of remaining block
  MemoryManager::deallocate(memory_manager, ptr2)
  
  let final_usage = MemoryManager::get_usage(memory_manager)
  assert_eq(final_usage.allocated_bytes, 0)
  assert_eq(final_usage.allocated_blocks, 0)
  
  // Test allocation with alignment
  let aligned_ptr = MemoryManager::allocate_aligned(memory_manager, 4096, 16)  // 4KB aligned to 16 bytes
  assert_eq(aligned_ptr != null, true)
  assert_eq(aligned_ptr % 16, 0)  // Should be aligned to 16 bytes
  
  MemoryManager::deallocate_aligned(memory_manager, aligned_ptr)
}

// Test 2: Memory Pool Management
test "memory pool management" {
  let pool_manager = MemoryPoolManager::new()
  
  // Create a memory pool with fixed block size
  let pool = MemoryPool::new(pool_manager, 1024, 10)  // 1KB blocks, 10 blocks initially
  
  // Test pool allocation
  let block1 = MemoryPool::allocate(pool)
  assert_eq(block1 != null, true)
  
  let block2 = MemoryPool::allocate(pool)
  assert_eq(block2 != null, true)
  
  // Test pool statistics
  let stats = MemoryPool::get_statistics(pool)
  assert_eq(stats.allocated_blocks, 2)
  assert_eq(stats.free_blocks, 8)
  assert_eq(stats.total_blocks, 10)
  
  // Test block deallocation back to pool
  MemoryPool::deallocate(pool, block1)
  
  let after_dealloc_stats = MemoryPool::get_statistics(pool)
  assert_eq(after_dealloc_stats.allocated_blocks, 1)
  assert_eq(after_dealloc_stats.free_blocks, 9)
  
  // Test pool exhaustion and expansion
  let remaining_blocks = []
  for i = 0; i < 9; i = i + 1 {
    let block = MemoryPool::allocate(pool)
    assert_eq(block != null, true)
    remaining_blocks.push(block)
  }
  
  // Pool should now be exhausted
  let exhausted_block = MemoryPool::allocate(pool)
  assert_eq(exhausted_block != null, false)  // Should fail
  
  // Return one block to pool
  MemoryPool::deallocate(pool, remaining_blocks[0])
  
  // Should now be able to allocate again
  let new_block = MemoryPool::allocate(pool)
  assert_eq(new_block != null, true)
  
  // Clean up remaining blocks
  for i = 1; i < remaining_blocks.length(); i = i + 1 {
    MemoryPool::deallocate(pool, remaining_blocks[i])
  }
  MemoryPool::deallocate(pool, new_block)
  
  // Test pool destruction
  MemoryPool::destroy(pool)
}

// Test 3: Smart Pointers and RAII
test "smart pointers and raii" {
  // Test unique pointer
  let resource = "test_resource"
  let unique_ptr = UniquePtr::new(resource)
  
  assert_eq(UniquePtr::get(unique_ptr), resource)
  assert_eq(UniquePtr::is_valid(unique_ptr), true)
  
  // Test unique pointer move semantics
  let moved_ptr = UniquePtr::move(unique_ptr)
  assert_eq(UniquePtr::is_valid(unique_ptr), false)  // Original is now invalid
  assert_eq(UniquePtr::is_valid(moved_ptr), true)
  assert_eq(UniquePtr::get(moved_ptr), resource)
  
  // Test unique pointer reset
  let new_resource = "new_resource"
  UniquePtr::reset(moved_ptr, new_resource)
  assert_eq(UniquePtr::get(moved_ptr), new_resource)
  
  // Test shared pointer
  let shared_resource = "shared_resource"
  let shared_ptr1 = SharedPtr::new(shared_resource)
  
  assert_eq(SharedPtr::use_count(shared_ptr1), 1)
  assert_eq(SharedPtr::get(shared_ptr1), shared_resource)
  
  // Create another shared pointer to same resource
  let shared_ptr2 = SharedPtr::copy(shared_ptr1)
  
  assert_eq(SharedPtr::use_count(shared_ptr1), 2)
  assert_eq(SharedPtr::use_count(shared_ptr2), 2)
  assert_eq(SharedPtr::get(shared_ptr1), SharedPtr::get(shared_ptr2))
  
  // Test weak pointer
  let weak_ptr = WeakPtr::from_shared(shared_ptr1)
  assert_eq(WeakPtr::expired(weak_ptr), false)
  
  // Destroy one shared pointer
  SharedPtr::destroy(shared_ptr1)
  
  assert_eq(SharedPtr::use_count(shared_ptr2), 1)
  assert_eq(WeakPtr::expired(weak_ptr), false)
  
  // Destroy the last shared pointer
  SharedPtr::destroy(shared_ptr2)
  
  assert_eq(WeakPtr::expired(weak_ptr), true)
  
  // Test custom deleter with smart pointer
  let mut deletion_called = false
  let custom_deleter = fn(resource) {
    deletion_called = true
    assert_eq(resource, "custom_deletable_resource")
  }
  
  let custom_resource = "custom_deletable_resource"
  let custom_ptr = UniquePtr::with_deleter(custom_resource, custom_deleter)
  
  // When custom_ptr goes out of scope, deleter should be called
  UniquePtr::destroy(custom_ptr)
  assert_eq(deletion_called, true)
}

// Test 4: Resource Leak Detection
test "resource leak detection" {
  let leak_detector = LeakDetector::new()
  leak_detector.enable()
  
  // Test memory leak detection
  let leaked_ptr = MemoryManager::allocate(leak_detector.get_memory_manager(), 1024)
  
  // Don't deallocate - should be detected as leak
  let leaks = LeakDetector::detect_leaks(leak_detector)
  assert_eq(leaks.length(), 1)
  assert_eq(leaks[0].size, 1024)
  assert_eq(leaks[0].type, LeakType::Memory)
  
  // Fix the leak
  MemoryManager::deallocate(leak_detector.get_memory_manager(), leaked_ptr)
  
  let no_leaks = LeakDetector::detect_leaks(leak_detector)
  assert_eq(no_leaks.length(), 0)
  
  // Test file handle leak detection
  let file_manager = FileManager::new()
  leak_detector.register_file_manager(file_manager)
  
  let file1 = FileManager::open(file_manager, "test1.txt", "w")
  let file2 = FileManager::open(file_manager, "test2.txt", "w")
  
  // Close only one file handle
  FileManager::close(file_manager, file1)
  
  let file_leaks = LeakDetector::detect_file_leaks(leak_detector)
  assert_eq(file_leaks.length(), 1)
  assert_eq(file_leaks[0].path, "test2.txt")
  
  // Close remaining file
  FileManager::close(file_manager, file2)
  
  let no_file_leaks = LeakDetector::detect_file_leaks(leak_detector)
  assert_eq(no_file_leaks.length(), 0)
  
  // Test automatic leak detection on scope exit
  {
    let scoped_leak_detector = ScopedLeakDetector::new()
    let auto_leaked_ptr = MemoryManager::allocate(scoped_leak_detector.get_memory_manager(), 512)
    
    // Don't explicitly deallocate - should be automatically detected when scope exits
  }
  
  // Scoped leak detector should have detected the leak
  let scoped_leak_report = ScopedLeakDetector::get_report()
  assert_eq(scoped_leak_report.leaks_found, 1)
}

// Test 5: Memory Usage Monitoring and Profiling
test "memory usage monitoring and profiling" {
  let memory_profiler = MemoryProfiler::new()
  memory_profiler.start_profiling()
  
  // Allocate some memory
  let ptr1 = MemoryManager::allocate(memory_profiler.get_memory_manager(), 1024)
  let ptr2 = MemoryManager::allocate(memory_profiler.get_memory_manager(), 2048)
  
  // Create allocation markers
  memory_profiler.add_marker("test_allocation_1")
  
  let ptr3 = MemoryManager::allocate(memory_profiler.get_memory_manager(), 4096)
  
  memory_profiler.add_marker("test_allocation_2")
  
  let ptr4 = MemoryManager::allocate(memory_profiler.get_memory_manager(), 8192)
  
  // Deallocate some memory
  MemoryManager::deallocate(memory_profiler.get_memory_manager(), ptr2)
  
  memory_profiler.add_marker("deallocation_1")
  
  MemoryManager::deallocate(memory_profiler.get_memory_manager(), ptr4)
  
  // Stop profiling and get report
  memory_profiler.stop_profiling()
  let profile_report = memory_profiler.get_profile_report()
  
  assert_eq(profile_report.total_allocated, 15360)  // 1024 + 2048 + 4096 + 8192
  assert_eq(profile_report.total_deallocated, 10240)  // 2048 + 8192
  assert_eq(profile_report.current_usage, 5120)     // 1024 + 4096
  assert_eq(profile_report.peak_usage, 15360)
  
  // Test memory fragmentation analysis
  let fragmentation = memory_profiler.analyze_fragmentation()
  assert_eq(fragmentation.total_blocks, 2)
  assert_eq(fragmentation.fragmentation_index >= 0.0, true)
  assert_eq(fragmentation.fragmentation_index <= 1.0, true)
  
  // Test allocation size distribution
  let size_distribution = memory_profiler.get_size_distribution()
  assert_eq(size_distribution.get("1KB"), Some(1))
  assert_eq(size_distribution.get("2KB"), Some(0))  // Deallocated
  assert_eq(size_distribution.get("4KB"), Some(1))
  assert_eq(size_distribution.get("8KB"), Some(0))  // Deallocated
  
  // Clean up remaining allocations
  MemoryManager::deallocate(memory_profiler.get_memory_manager(), ptr1)
  MemoryManager::deallocate(memory_profiler.get_memory_manager(), ptr3)
  
  // Test memory pressure monitoring
  let pressure_monitor = MemoryPressureMonitor::new()
  pressure_monitor.set_threshold(0.8)  // Alert at 80% memory usage
  
  // Simulate memory pressure
  let large_allocations = []
  let total_memory = 1000000  // Simulate 1MB total memory
  
  // Allocate up to threshold
  for i = 0; i < 8; i = i + 1 {
    let ptr = MemoryManager::allocate(pressure_monitor.get_memory_manager(), 100000)  // 100KB each
    large_allocations.push(ptr)
  }
  
  // Should be at 80% threshold
  assert_eq(pressure_monitor.is_pressure_detected(), true)
  
  // Get pressure level
  let pressure_level = pressure_monitor.get_pressure_level()
  assert_eq(pressure_level, PressureLevel::High)
  
  // Clean up
  for ptr in large_allocations {
    MemoryManager::deallocate(pressure_monitor.get_memory_manager(), ptr)
  }
}

// Test 6: Garbage Collection
test "garbage collection" {
  let gc = GarbageCollector::new()
  
  // Create some garbage objects
  let obj1 = gc.allocate_object("object1")
  let obj2 = gc.allocate_object("object2")
  let obj3 = gc.allocate_object("object3")
  
  // Create references between objects
  gc.add_reference(obj1, obj2)  // obj1 references obj2
  gc.add_reference(obj2, obj3)  // obj2 references obj3
  
  // Drop reference to obj1 (making obj1, obj2, obj3 reachable through obj1)
  let root_set = [obj1]
  
  // Run garbage collection
  let gc_stats = gc.collect(root_set)
  
  assert_eq(gc_stats.objects_collected, 0)  // No objects should be collected
  assert_eq(gc_stats.objects_remaining, 3)
  
  // Remove reference to obj1 from root set
  let empty_root_set = []
  
  // Run garbage collection again
  let gc_stats2 = gc.collect(empty_root_set)
  
  // All objects should be collected as they're no longer reachable
  assert_eq(gc_stats2.objects_collected, 3)
  assert_eq(gc_stats2.objects_remaining, 0)
  
  // Test generational garbage collection
  let generational_gc = GenerationalGC::new()
  
  // Create objects in different generations
  let gen0_obj1 = generational_gc.allocate_object("gen0_1")
  let gen0_obj2 = generational_gc.allocate_object("gen0_2")
  
  // Run minor GC (only gen0)
  let minor_gc_stats = generational_gc.collect_minor([gen0_obj1])
  
  assert_eq(minor_gc_stats.objects_collected, 1)  // gen0_2 should be collected
  assert_eq(minor_gc_stats.objects_remaining, 1)
  
  // Promote surviving object to gen1
  generational_gc.promote(gen0_obj1)
  
  // Create new gen0 objects
  let gen0_obj3 = generational_gc.allocate_object("gen0_3")
  let gen0_obj4 = generational_gc.allocate_object("gen0_4")
  
  // Run major GC (all generations)
  let major_gc_stats = generational_gc.collect_major([gen0_obj3])
  
  assert_eq(major_gc_stats.objects_collected, 2)  // gen0_obj4 and promoted gen0_obj1
  assert_eq(major_gc_stats.objects_remaining, 1)
  
  // Test GC tuning
  gc.set_collection_threshold(100)  // Collect after 100 allocations
  gc.set_collection_strategy(GCStrategy::Incremental)
  
  // Create many objects to trigger automatic collection
  let many_objects = []
  for i = 0; i < 150; i = i + 1 {
    let obj = gc.allocate_object("object_" + i.to_string())
    many_objects.push(obj)
  }
  
  // GC should have automatically triggered
  let auto_gc_stats = gc.get_last_collection_stats()
  assert_eq(auto_gc_stats.was_automatic, true)
}

// Test 7: Resource Lifetime Management
test "resource lifetime management" {
  let resource_manager = ResourceManager::new()
  
  // Test scoped resource
  let scoped_resource = ScopedResource::new(resource_manager, "test_resource")
  
  assert_eq(ScopedResource::is_valid(scoped_resource), true)
  assert_eq(ScopedResource::get(scoped_resource), "test_resource")
  
  // Test resource transfer
  let transferred_resource = ScopedResource::transfer(scoped_resource)
  assert_eq(ScopedResource::is_valid(scoped_resource), false)  // Original is invalid
  assert_eq(ScopedResource::is_valid(transferred_resource), true)
  
  // Test resource pool
  let resource_pool = ResourcePool::new(|| { "pooled_resource" })
  
  let pooled1 = ResourcePool::acquire(resource_pool)
  let pooled2 = ResourcePool::acquire(resource_pool)
  
  assert_eq(pooled1 != null, true)
  assert_eq(pooled2 != null, true)
  
  // Return resources to pool
  ResourcePool::release(resource_pool, pooled1)
  ResourcePool::release(resource_pool, pooled2)
  
  // Pool statistics
  let pool_stats = ResourcePool::get_statistics(resource_pool)
  assert_eq(pool_stats.available_resources, 2)
  assert_eq(pool_stats.acquired_resources, 0)
  
  // Test lazy resource initialization
  let mut init_called = false
  let lazy_resource = LazyResource::new(|| {
    init_called = true
    return "lazy_initialized_resource"
  })
  
  assert_eq(init_called, false)  // Not initialized yet
  
  let resource = LazyResource::get(lazy_resource)
  assert_eq(init_called, true)    // Now initialized
  assert_eq(resource, "lazy_initialized_resource")
  
  // Subsequent access should not reinitialize
  init_called = false
  let resource2 = LazyResource::get(lazy_resource)
  assert_eq(init_called, false)  // Not reinitialized
  assert_eq(resource2, "lazy_initialized_resource")
  
  // Test resource dependency management
  let dependency_manager = ResourceDependencyManager::new()
  
  let resource_a = dependency_manager.create_resource("resource_a")
  let resource_b = dependency_manager.create_resource("resource_b")
  let resource_c = dependency_manager.create_resource("resource_c")
  
  // Set up dependencies: c depends on b, b depends on a
  dependency_manager.add_dependency(resource_b, resource_a)
  dependency_manager.add_dependency(resource_c, resource_b)
  
  // Try to release resource_a (should fail due to dependencies)
  let release_result = dependency_manager.try_release(resource_a)
  assert_eq(release_result, false)
  
  // Release in correct order
  dependency_manager.release(resource_c)
  dependency_manager.release(resource_b)
  let final_release = dependency_manager.try_release(resource_a)
  assert_eq(final_release, true)
}

// Test 8: Memory Safety and Bounds Checking
test "memory safety and bounds checking" {
  let safe_memory_manager = SafeMemoryManager::new()
  
  // Test bounds-checked allocation
  let safe_array = SafeMemoryManager::allocate_array(safe_memory_manager, 10, 4)  // 10 elements of 4 bytes each
  
  // Test safe access
  let value = SafeMemoryManager::get_array_element(safe_array, 5)
  assert_eq(value != null, true)
  
  // Test bounds checking (should panic or return error for out-of-bounds)
  let out_of_bounds_result = SafeMemoryManager::get_array_element_safe(safe_array, 15)
  match out_of_bounds_result {
    Ok(_) => assert_true(false)  // Should fail
    Err(error) => assert_eq(error.type, MemoryError::OutOfBounds)
  }
  
  // Test safe write
  SafeMemoryManager::set_array_element(safe_array, 3, 42)
  let retrieved_value = SafeMemoryManager::get_array_element(safe_array, 3)
  assert_eq(retrieved_value, 42)
  
  // Test buffer overflow protection
  let buffer = SafeMemoryManager::allocate_buffer(safe_memory_manager, 100)
  
  let safe_write_result = SafeMemoryManager::write_to_buffer_safe(buffer, "test_data", 0)
  match safe_write_result {
    Ok(bytes_written) => assert_eq(bytes_written, 9)  // "test_data" length
    Err(_) => assert_true(false)
  }
  
  // Write beyond buffer bounds should fail
  let overflow_result = SafeMemoryManager::write_to_buffer_safe(buffer, "x" * 200, 0)
  match overflow_result {
    Ok(_) => assert_true(false)  // Should fail
    Err(error) => assert_eq(error.type, MemoryError::BufferOverflow)
  }
  
  // Test memory sanitization
  let sensitive_data = "sensitive_information"
  let secure_buffer = SafeMemoryManager::allocate_secure_buffer(safe_memory_manager, 100)
  
  SafeMemoryManager::write_to_buffer(secure_buffer, sensitive_data, 0)
  
  // Verify data is written
  let read_data = SafeMemoryManager::read_from_buffer(secure_buffer, 0, sensitive_data.length())
  assert_eq(read_data, sensitive_data)
  
  // Sanitize buffer
  SafeMemoryManager::sanitize_buffer(secure_buffer)
  
  // Verify data is no longer readable
  let sanitized_data = SafeMemoryManager::read_from_buffer(secure_buffer, 0, sensitive_data.length())
  assert_eq(sanitized_data != sensitive_data, true)
  
  // Test memory guard pages
  let guarded_memory = SafeMemoryManager::allocate_with_guard_pages(safe_memory_manager, 1024)
  
  // Access within bounds should work
  let in_bounds_access = SafeMemoryManager::access_safe(guarded_memory, 512)
  assert_eq(in_bounds_access.is_ok(), true)
  
  // Access near guard page should fail
  let near_guard_access = SafeMemoryManager::access_safe(guarded_memory, 1024)
  assert_eq(near_guard_access.is_err(), true)
  
  // Clean up
  SafeMemoryManager::deallocate_array(safe_memory_manager, safe_array)
  SafeMemoryManager::deallocate_buffer(safe_memory_manager, buffer)
  SafeMemoryManager::deallocate_secure_buffer(safe_memory_manager, secure_buffer)
  SafeMemoryManager::deallocate_with_guard_pages(safe_memory_manager, guarded_memory)
}

// Test 9: Memory Mapping and Virtual Memory
test "memory mapping and virtual memory" {
  let virtual_memory_manager = VirtualMemoryManager::new()
  
  // Test virtual memory allocation
  let vm_region = VirtualMemoryManager::allocate_region(virtual_memory_manager, 4096 * 16)  // 16 pages
  assert_eq(vm_region != null, true)
  assert_eq(VirtualMemoryManager::get_region_size(vm_region), 4096 * 16)
  
  // Test memory protection
  VirtualMemoryManager::protect_region(vm_region, MemoryProtection::ReadWrite)
  
  // Write to region
  VirtualMemoryManager::write_to_region(vm_region, 0, "test_data")
  
  // Read from region
  let read_data = VirtualMemoryManager::read_from_region(vm_region, 0, 9)
  assert_eq(read_data, "test_data")
  
  // Change protection to read-only
  VirtualMemoryManager::protect_region(vm_region, MemoryProtection::ReadOnly)
  
  // Try to write to read-only region (should fail)
  let write_result = VirtualMemoryManager::try_write_to_region(vm_region, 0, "new_data")
  assert_eq(write_result.is_err(), true)
  
  // Test memory mapping
  let temp_file = TempFile::new("test_mmap")
  TempFile::write(temp_file, "mapped_data_content")
  
  let mapped_memory = VirtualMemoryManager::map_file(virtual_memory_manager, temp_file)
  assert_eq(mapped_memory != null, true)
  
  // Read mapped memory
  let mapped_content = VirtualMemoryManager::read_from_region(mapped_memory, 0, 19)
  assert_eq(mapped_content, "mapped_data_content")
  
  // Modify mapped memory
  VirtualMemoryManager::write_to_region(mapped_memory, 0, "modified_data")
  
  // Unmap
  VirtualMemoryManager::unmap_region(virtual_memory_manager, mapped_memory)
  
  // Test huge pages
  let huge_page_region = VirtualMemoryManager::allocate_huge_pages(virtual_memory_manager, 4096 * 256)  // 256 huge pages
  assert_eq(huge_page_region != null, true)
  
  // Check if region uses huge pages
  let is_huge_page = VirtualMemoryManager::is_huge_page_region(huge_page_region)
  assert_eq(is_huge_page, true)
  
  // Test address space layout randomization (ASLR)
  let regions = []
  for i = 0; i < 10; i = i + 1 {
    let region = VirtualMemoryManager::allocate_region(virtual_memory_manager, 4096)
    regions.push(region)
  }
  
  // Check that regions have different addresses (ASLR working)
  let mut addresses = []
  for region in regions {
    addresses.push(VirtualMemoryManager::get_region_address(region))
  }
  
  // Sort addresses and check for uniqueness
  addresses.sort()
  let mut all_unique = true
  for i = 1; i < addresses.length(); i = i + 1 {
    if addresses[i] == addresses[i-1] {
      all_unique = false
      break
    }
  }
  assert_eq(all_unique, true)
  
  // Clean up
  VirtualMemoryManager::deallocate_region(virtual_memory_manager, vm_region)
  VirtualMemoryManager::deallocate_region(virtual_memory_manager, huge_page_region)
  for region in regions {
    VirtualMemoryManager::deallocate_region(virtual_memory_manager, region)
  }
  TempFile::delete(temp_file)
}

// Test 10: Memory-Efficient Data Structures
test "memory efficient data structures" {
  // Test compact string storage
  let compact_strings = CompactStringSet::new()
  
  // Add many strings
  for i = 0; i < 1000; i = i + 1 {
    CompactStringSet::add(compact_strings, "string_" + i.to_string())
  }
  
  // Check memory usage
  let memory_usage = CompactStringSet::get_memory_usage(compact_strings)
  assert_eq(memory_usage.used_bytes < 100000, true)  // Should be compact
  
  // Test string interning
  let string_interner = StringInterner::new()
  
  let interned1 = StringInterner::intern(string_interner, "frequently_used_string")
  let interned2 = StringInterner::intern(string_interner, "frequently_used_string")
  let interned3 = StringInterner::intern(string_interner, "another_string")
  
  // interned1 and interned2 should point to same memory
  assert_eq(interned1 == interned2, true)
  assert_eq(interned2 == interned3, false)
  
  // Check interner statistics
  let interner_stats = StringInterner::get_statistics(string_interner)
  assert_eq(interner_stats.unique_strings, 2)
  assert_eq(interner_stats.total_interns, 3)
  
  // Test memory-efficient hash map
  let compact_map = CompactHashMap::new()
  
  // Add many key-value pairs
  for i = 0; i < 1000; i = i + 1 {
    CompactHashMap::insert(compact_map, "key_" + i.to_string(), "value_" + i.to_string())
  }
  
  let map_memory_usage = CompactHashMap::get_memory_usage(compact_map)
  assert_eq(map_memory_usage.used_bytes < 200000, true)  // Should be compact
  
  // Test sparse array
  let sparse_array = SparseArray::new(10000)  // Capacity for 10000 elements
  
  // Set only a few elements
  SparseArray::set(sparse_array, 100, "value_at_100")
  SparseArray::set(sparse_array, 5000, "value_at_5000")
  SparseArray::set(sparse_array, 9999, "value_at_9999")
  
  // Memory usage should be proportional to actual elements, not capacity
  let sparse_memory_usage = SparseArray::get_memory_usage(sparse_array)
  assert_eq(sparse_memory_usage.used_bytes < 10000, true)  // Much less than 10000 * element_size
  
  // Verify values
  assert_eq(SparseArray::get(sparse_array, 100), Some("value_at_100"))
  assert_eq(SparseArray::get(sparse_array, 5000), Some("value_at_5000"))
  assert_eq(SparseArray::get(sparse_array, 9999), Some("value_at_9999"))
  assert_eq(SparseArray::get(sparse_array, 200), None)
  
  // Test memory-efficient bitmap
  let bitmap = Bitmap::new(10000)  // 10000 bits
  
  // Set some bits
  Bitmap::set(bitmap, 100)
  Bitmap::set(bitmap, 5000)
  Bitmap::set(bitmap, 9999)
  
  // Memory usage should be minimal (10000 bits = 1250 bytes)
  let bitmap_memory_usage = Bitmap::get_memory_usage(bitmap)
  assert_eq(bitmap_memory_usage.used_bytes, 1250)
  
  // Verify bits
  assert_eq(Bitmap::is_set(bitmap, 100), true)
  assert_eq(Bitmap::is_set(bitmap, 5000), true)
  assert_eq(Bitmap::is_set(bitmap, 9999), true)
  assert_eq(Bitmap::is_set(bitmap, 200), false)
  
  // Test memory pool for small objects
  let small_object_pool = SmallObjectPool::new()
  
  let small_objects = []
  for i = 0; i < 1000; i = i + 1 {
    let obj = SmallObjectPool::allocate(small_object_pool, 64)  // 64-byte objects
    small_objects.push(obj)
  }
  
  let pool_memory_usage = SmallObjectPool::get_memory_usage(small_object_pool)
  
  // Should have much less overhead than individual allocations
  assert_eq(pool_memory_usage.overhead_bytes < 10000, true)
  
  // Clean up
  for obj in small_objects {
    SmallObjectPool::deallocate(small_object_pool, obj)
  }
}