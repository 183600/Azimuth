// 额外增强的 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行高质量测试

test "binary_operations_test" {
  // 测试二进制运算模拟
  let a = 12
  let b = 5
  
  // 模拟位与运算
  let bit_and_result = multiply(divide_with_ceil(a, 4), 4)
  assert_eq(12, bit_and_result)
  
  // 模拟位或运算
  let bit_or_result = add(a, b)
  assert_eq(17, bit_or_result)
}

test "statistical_analysis_test" {
  // 测试统计分析计算
  let values = [15, 22, 18, 25, 20]
  let sum = add(add(add(add(values[0], values[1]), values[2]), values[3]), values[4])
  let mean = divide_with_ceil(sum, 5)
  assert_eq(100, sum)
  assert_eq(20, mean)
}

test "resource_allocation_test" {
  // 测试资源分配算法
  let total_resources = 100
  let project1_needs = 35
  let project2_needs = 40
  let project3_needs = 30
  
  let allocated1 = project1_needs
  let allocated2 = project2_needs
  let remaining = add(total_resources, add(-allocated1, -allocated2))
  let allocated3 = if remaining >= project3_needs { project3_needs } else { remaining }
  
  assert_eq(25, allocated3)
}

test "circular_buffer_test" {
  // 测试循环缓冲区索引计算
  let buffer_size = 16
  let current_index = 14
  let step_forward = 5
  
  let next_index = add(current_index, step_forward)
  let wrapped_index = next_index - multiply(buffer_size, divide_with_ceil(next_index, buffer_size))
  
  assert_eq(3, wrapped_index)
}

test "checksum_calculation_test" {
  // 测试校验和计算
  let data = [10, 20, 30, 40, 50]
  let checksum = add(add(add(add(data[0], data[1]), data[2]), data[3]), data[4])
  let mod_checksum = checksum - multiply(100, divide_with_ceil(checksum, 100))
  
  assert_eq(150, checksum)
  assert_eq(50, mod_checksum)
}

test "load_balancing_test" {
  // 测试负载均衡算法
  let total_requests = 47
  let server_count = 4
  
  let base_requests_per_server = divide_with_ceil(total_requests, server_count)
  let total_allocated = multiply(base_requests_per_server, server_count)
  let remaining_requests = add(total_requests, -total_allocated)
  
  assert_eq(12, base_requests_per_server)
  assert_eq(-1, remaining_requests)
}

test "page_allocation_test" {
  // 测试内存页分配计算
  let total_memory = 1024
  let page_size = 64
  let needed_pages = divide_with_ceil(total_memory, page_size)
  
  let actual_allocated = multiply(needed_pages, page_size)
  let waste = add(actual_allocated, -total_memory)
  
  assert_eq(16, needed_pages)
  assert_eq(1024, actual_allocated)
  assert_eq(0, waste)
}

test "priority_queue_test" {
  // 测试优先级队列模拟
  let task1_priority = 3
  let task2_priority = 1
  let task3_priority = 2
  
  let highest_priority = if task1_priority < task2_priority {
    if task1_priority < task3_priority { task1_priority } else { task3_priority }
  } else {
    if task2_priority < task3_priority { task2_priority } else { task3_priority }
  }
  
  assert_eq(1, highest_priority)
}

test "hash_table_test" {
  // 测试哈希表索引计算
  let table_size = 16
  let key_value = 42
  
  let hash_index = multiply(key_value, 7) % table_size
  let collision_resolution = add(hash_index, 1) - multiply(table_size, divide_with_ceil(add(hash_index, 1), table_size))
  
  assert_eq(10, hash_index)
  assert_eq(11, collision_resolution)
}

test "compression_ratio_test" {
  // 测试压缩比计算
  let original_size = 1000
  let compressed_size = 350
  
  let compression_ratio = multiply(100, divide_with_ceil(compressed_size, original_size))
  let space_saved = add(100, -compression_ratio)
  
  assert_eq(35, compression_ratio)
  assert_eq(65, space_saved)
}