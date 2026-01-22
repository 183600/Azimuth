// 新的标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，针对 azimuth 库的核心功能

test "binary_arithmetic_operations" {
  // 测试二进制算术运算
  let a = 12
  let b = 8
  
  // 测试加法
  let sum = add(a, b)
  assert_eq(20, sum)
  
  // 测试乘法
  let product = multiply(a, b)
  assert_eq(96, product)
  
  // 测试组合运算
  let combined = add(multiply(a, 2), multiply(b, 3))
  assert_eq(48, combined)  // 12*2 + 8*3 = 24 + 24 = 48
}

test "divide_with_ceil_boundary_conditions" {
  // 测试向上取整除法的边界条件
  // 测试相等的情况
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(3, divide_with_ceil(9, 3))
  
  // 测试有余数的情况
  assert_eq(6, divide_with_ceil(11, 2))
  assert_eq(4, divide_with_ceil(13, 4))
  
  // 测试被除数小于除数
  assert_eq(1, divide_with_ceil(3, 10))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 测试除数为零
  assert_eq(0, divide_with_ceil(50, 0))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  // 空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 单字符
  assert_eq_string("Hello, A!", greet("A"))
  
  // 长字符串
  assert_eq_string("Hello, This is a very long name!", greet("This is a very long name"))
  
  // 包含空格的字符串
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
}

test "mathematical_sequence_calculations" {
  // 测试数学序列计算
  // 计算前n个自然数的和：n(n+1)/2
  let n = 10
  let sum_natural = divide_with_ceil(multiply(n, add(n, 1)), 2)
  assert_eq(55, sum_natural)
  
  // 计算前n个偶数的和：n(n+1)
  let sum_even = multiply(n, add(n, 1))
  assert_eq(110, sum_even)
  
  // 计算前n个奇数的和：n²
  let sum_odd = multiply(n, n)
  assert_eq(100, sum_odd)
}

test "real_world_shopping_cart" {
  // 真实世界购物车计算
  let apple_price = 5
  let apple_quantity = 6
  let banana_price = 3
  let banana_quantity = 8
  let orange_price = 7
  let orange_quantity = 4
  
  // 计算各项总价
  let apple_total = multiply(apple_price, apple_quantity)
  let banana_total = multiply(banana_price, banana_quantity)
  let orange_total = multiply(orange_price, orange_quantity)
  
  // 计算购物车总价
  let cart_total = add(add(apple_total, banana_total), orange_total)
  
  assert_eq(30, apple_total)
  assert_eq(24, banana_total)
  assert_eq(28, orange_total)
  assert_eq(82, cart_total)
}

test "time_zone_conversion" {
  // 时区转换计算
  let local_time = 14  // 下午2点
  let time_difference = 8  // 与目标时区相差8小时
  
  // 计算目标时间
  let target_time = add(local_time, time_difference)
  
  // 处理24小时制
  let adjusted_time = if target_time >= 24 {
    add(target_time, -24)
  } else {
    target_time
  }
  
  assert_eq(22, adjusted_time)  // 14 + 8 = 22
}

test "inventory_restock_calculation" {
  // 库存补货计算
  let current_stock = 45
  let daily_sales = 12
  let days_until_restock = 7
  let safety_buffer = 20
  
  // 计算预期库存
  let expected_sales = multiply(daily_sales, days_until_restock)
  let projected_stock = add(current_stock, -expected_sales)
  
  // 计算需要补货的数量
  let restock_needed = if projected_stock < safety_buffer {
    add(safety_buffer, -projected_stock)
  } else {
    0
  }
  
  assert_eq(84, expected_sales)
  assert_eq(-39, projected_stock)
  assert_eq(59, restock_needed)
}

test "data_storage_optimization" {
  // 数据存储优化计算
  let file_size_mb = 125
  let compression_ratio = 60  // 压缩到60%
  let storage_capacity_mb = 1000
  
  // 计算压缩后的大小
  let compressed_size = multiply(file_size_mb, compression_ratio) / 100
  
  // 计算可以存储的压缩文件数量
  let files_storable = divide_with_ceil(storage_capacity_mb, compressed_size)
  
  assert_eq(75, compressed_size)
  assert_eq(14, files_storable)
}

test "network_packet_routing" {
  // 网络数据包路由计算
  let total_packets = 234
  let routes_count = 5
  let priority_packets = 34
  
  // 计算每条路由的基本数据包数量
  let base_packets_per_route = divide_with_ceil(add(total_packets, -priority_packets), routes_count)
  
  // 计算剩余数据包数量
  let distributed_packets = multiply(base_packets_per_route, routes_count)
  let remaining_packets = add(add(total_packets, -priority_packets), -distributed_packets)
  
  assert_eq(40, base_packets_per_route)
  assert_eq(200, distributed_packets)
  assert_eq(0, remaining_packets)
}

test "algorithm_complexity_analysis" {
  // 算法复杂度分析
  let input_size = 100
  let linear_factor = 2
  let quadratic_factor = 3
  
  // O(n) 复杂度
  let linear_operations = multiply(input_size, linear_factor)
  
  // O(n²) 复杂度
  let quadratic_operations = multiply(multiply(input_size, input_size), quadratic_factor) / 100
  
  // 总操作数
  let total_operations = add(linear_operations, quadratic_operations)
  
  assert_eq(200, linear_operations)
  assert_eq(300, quadratic_operations)
  assert_eq(500, total_operations)
}