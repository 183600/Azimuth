// 额外的综合测试用例
// 为 azimuth 库添加的高质量测试用例，使用标准 MoonBit 测试语法

// 测试加法函数的边界情况和特殊值
test "add_function_boundary_cases" {
  // 测试零值加法
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(-5, @azimuth.add(0, -5))
  
  // 测试相反数相加
  @azimuth.assert_eq(0, @azimuth.add(10, -10))
  @azimuth.assert_eq(0, @azimuth.add(-100, 100))
  
  // 测试连续加法
  @azimuth.assert_eq(15, @azimuth.add(@azimuth.add(5, 5), 5))
  @azimuth.assert_eq(0, @azimuth.add(@azimuth.add(-5, 5), 0))
}

// 测试乘法函数的幂运算特性
test "multiply_function_power_properties" {
  // 测试平方运算
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  
  // 测试立方运算
  @azimuth.assert_eq(125, @azimuth.multiply(@azimuth.multiply(5, 5), 5))
  @azimuth.assert_eq(1000, @azimuth.multiply(@azimuth.multiply(10, 10), 10))
  
  // 测试负数的平方
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
}

// 测试向上取整除法的精度和舍入
test "divide_with_ceil_precision_test" {
  // 测试小数部分的向上取整
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(5, 2))
  
  // 测试大数的精度保持
  @azimuth.assert_eq(1000, @azimuth.divide_with_ceil(1999, 2))
  @azimuth.assert_eq(667, @azimuth.divide_with_ceil(2000, 3))
  
  // 测试接近整数的除法
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(999, 10))
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(1000, 10))
  @azimuth.assert_eq(101, @azimuth.divide_with_ceil(1001, 10))
}

// 测试字符串函数的多语言支持
test "greet_function_multilingual_support" {
  // 测试欧洲语言
  @azimuth.assert_eq_string("Hello, Español!", @azimuth.greet("Español"))
  @azimuth.assert_eq_string("Hello, Français!", @azimuth.greet("Français"))
  @azimuth.assert_eq_string("Hello, Deutsch!", @azimuth.greet("Deutsch"))
  @azimuth.assert_eq_string("Hello, Italiano!", @azimuth.greet("Italiano"))
  
  // 测试亚洲语言
  @azimuth.assert_eq_string("Hello, 日本語!", @azimuth.greet("日本語"))
  @azimuth.assert_eq_string("Hello, 한국어!", @azimuth.greet("한국어"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  
  // 测试复合字符
  @azimuth.assert_eq_string("Hello, Café!", @azimuth.greet("Café"))
  @azimuth.assert_eq_string("Hello, Résumé!", @azimuth.greet("Résumé"))
}

// 测试复杂业务场景：库存管理
test "inventory_management_business_logic" {
  // 场景：计算仓库存储需求
  let product_a_quantity = 156
  let product_b_quantity = 89
  let product_c_quantity = 234
  
  let box_capacity = 25
  
  // 计算每种产品需要的箱子数
  let boxes_a = @azimuth.divide_with_ceil(product_a_quantity, box_capacity)
  let boxes_b = @azimuth.divide_with_ceil(product_b_quantity, box_capacity)
  let boxes_c = @azimuth.divide_with_ceil(product_c_quantity, box_capacity)
  
  // 验证箱子数量
  @azimuth.assert_eq(7, boxes_a)  // ceil(156/25) = 7
  @azimuth.assert_eq(4, boxes_b)  // ceil(89/25) = 4
  @azimuth.assert_eq(10, boxes_c) // ceil(234/25) = 10
  
  // 计算总箱子数和总容量
  let total_boxes = @azimuth.add(@azimuth.add(boxes_a, boxes_b), boxes_c)
  let total_capacity = @azimuth.multiply(total_boxes, box_capacity)
  
  // 验证总容量足够
  @azimuth.assert_eq(21, total_boxes)
  @azimuth.assert_eq(525, total_capacity)
}

// 测试数学运算的结合律
test "mathematical_associative_laws" {
  // 加法结合律: (a + b) + c = a + (b + c)
  let a = 15
  let b = 27
  let c = 42
  
  let left_add = @azimuth.add(@azimuth.add(a, b), c)
  let right_add = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_add, right_add)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let left_multiply = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_multiply = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_multiply, right_multiply)
  
  // 验证具体数值
  @azimuth.assert_eq(84, left_add)
  @azimuth.assert_eq(17010, left_multiply)
}

// 测试资源分配优化算法
test "resource_allocation_optimization" {
  // 场景：服务器资源分配
  let total_memory = 32768  // GB
  let total_cpu_cores = 128
  let services = 5
  
  // 计算每个服务的平均资源
  let memory_per_service = @azimuth.divide_with_ceil(total_memory, services)
  let cpu_per_service = @azimuth.divide_with_ceil(total_cpu_cores, services)
  
  // 验证分配结果
  @azimuth.assert_eq(6554, memory_per_service)  // ceil(32768/5)
  @azimuth.assert_eq(26, cpu_per_service)       // ceil(128/5)
  
  // 计算实际分配的总资源
  let allocated_memory = @azimuth.multiply(memory_per_service, services)
  let allocated_cpu = @azimuth.multiply(cpu_per_service, services)
  
  // 验证资源利用率
  @azimuth.assert_eq(32770, allocated_memory)
  @azimuth.assert_eq(130, allocated_cpu)
  
  // 计算资源浪费
  let memory_waste = @azimuth.add(allocated_memory, @azimuth.multiply(total_memory, -1))
  let cpu_waste = @azimuth.add(allocated_cpu, @azimuth.multiply(total_cpu_cores, -1))
  
  @azimuth.assert_eq(2, memory_waste)
  @azimuth.assert_eq(2, cpu_waste)
}

// 测试错误处理和异常情况
test "error_handling_and_edge_cases" {
  // 测试除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试极值运算
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(1, -1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
  
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello, null!", @azimuth.greet("null"))
  @azimuth.assert_eq_string("Hello, undefined!", @azimuth.greet("undefined"))
  @azimuth.assert_eq_string("Hello, 0!", @azimuth.greet("0"))
  @azimuth.assert_eq_string("Hello, false!", @azimuth.greet("false"))
  @azimuth.assert_eq_string("Hello, true!", @azimuth.greet("true"))
}