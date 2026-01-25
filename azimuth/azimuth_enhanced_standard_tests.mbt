// Azimuth 增强标准测试用例
// 高质量的 MoonBit 测试，覆盖各种边界情况和实际应用场景

test "add_function_overflow_protection" {
  // 测试加法函数的溢出保护
  assert_eq(2147483646, add(2147483645, 1))  // 接近 Int 最大值
  assert_eq(-2147483647, add(-2147483646, -1))  // 接近 Int 最小值
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与最小值相加
  assert_eq(100000, add(99999, 1))  // 5位数边界
}

test "multiply_function_power_operations" {
  // 测试乘法函数的幂运算
  assert_eq(1, multiply(1, 1))  // 1的平方
  assert_eq(4, multiply(2, 2))  // 2的平方
  assert_eq(9, multiply(3, 3))  // 3的平方
  assert_eq(16, multiply(4, 4))  // 4的平方
  assert_eq(1024, multiply(32, 32))  // 32的平方
  assert_eq(-1024, multiply(-32, 32))  // 负数乘法
}

test "greet_function_empty_and_whitespace" {
  // 测试问候函数的空字符串和空白字符处理
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t!", greet("\t"))
  assert_eq_string("Hello, \n!", greet("\n"))
  assert_eq_string("Hello, \r!", greet("\r"))
}

test "divide_with_ceil_fractional_precision" {
  // 测试向上取整除法的小数精度处理
  assert_eq(1, divide_with_ceil(1, 2))  // 0.5 -> 1
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(3, divide_with_ceil(5, 2))  // 2.5 -> 3
  assert_eq(4, divide_with_ceil(7, 2))  // 3.5 -> 4
  assert_eq(5, divide_with_ceil(9, 2))  // 4.5 -> 5
  assert_eq(100, divide_with_ceil(199, 2))  // 99.5 -> 100
}

test "resource_allocation_optimization" {
  // 资源分配优化场景测试
  // 场景：云服务器资源分配
  let total_memory = 64000  // GB
  let servers = 13
  let memory_per_server = divide_with_ceil(total_memory, servers)
  
  assert_eq(4924, memory_per_server)  // ceil(64000/13)
  
  // 计算实际分配的总内存
  let total_allocated = multiply(memory_per_server, servers)
  let memory_waste = subtract(total_allocated, total_memory)
  
  assert_eq(64012, total_allocated)
  assert_eq(12, memory_waste)
  
  // 优化后的分配策略
  let optimized_servers = add(servers, 2)
  let optimized_memory_per_server = divide_with_ceil(total_memory, optimized_servers)
  assert_eq(4000, optimized_memory_per_server)  // ceil(64000/15)
}

test "complex_inventory_management" {
  // 复杂库存管理场景测试
  // 场景：多产品仓库管理
  let product_a = 127
  let product_b = 89
  let product_c = 234
  let box_capacity = 25
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a, box_capacity)  // ceil(127/25) = 6
  let boxes_b = divide_with_ceil(product_b, box_capacity)  // ceil(89/25) = 4
  let boxes_c = divide_with_ceil(product_c, box_capacity)  // ceil(234/25) = 10
  
  assert_eq(6, boxes_a)
  assert_eq(4, boxes_b)
  assert_eq(10, boxes_c)
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = multiply(total_boxes, box_capacity)
  
  assert_eq(20, total_boxes)
  assert_eq(500, total_capacity)
  
  // 计算总产品数
  let total_products = add(add(product_a, product_b), product_c)
  assert_eq(450, total_products)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(50, remaining_space)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算测试
  // 场景：计算等差数列和等比数列
  
  // 等差数列：2, 5, 8, 11, 14 (公差为3)
  let arithmetic_sum = add(add(add(add(2, 5), 8), 11), 14)
  assert_eq(40, arithmetic_sum)
  
  // 等比数列：2, 4, 8, 16, 32 (公比为2)
  let geometric_sum = add(add(add(add(2, 4), 8), 16), 32)
  assert_eq(62, geometric_sum)
  
  // 复杂计算：结合加法和乘法
  let complex_result = add(multiply(3, 4), multiply(5, 6))
  assert_eq(42, complex_result)  // 3*4 + 5*6 = 12 + 30 = 42
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 测试各种错误情况和边界条件
  
  // 除零错误的所有情况
  assert_eq(0, divide_with_ceil(0, 0))  // 0/0
  assert_eq(0, divide_with_ceil(1, 0))  // 1/0
  assert_eq(0, divide_with_ceil(-1, 0))  // -1/0
  assert_eq(0, divide_with_ceil(100, 0))  // 100/0
  assert_eq(0, divide_with_ceil(-100, 0))  // -100/0
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))  // 最小正数相除
  assert_eq(1, divide_with_ceil(-1, -1))  // 最小负数相除
  assert_eq(0, divide_with_ceil(0, 1))  // 零除以正数
  assert_eq(0, divide_with_ceil(0, -1))  // 零除以负数
  
  // 大数测试
  assert_eq(1, divide_with_ceil(2147483647, 2147483647))  // 最大值自除
  assert_eq(2, divide_with_ceil(2147483647, 2147483646))  // 接近最大值相除
}

test "string_encoding_internationalization" {
  // 字符串编码和国际化测试
  // 测试各种字符编码和国际化字符串
  
  // 中文字符
  assert_eq_string("Hello, 你好世界!", greet("你好世界"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  
  // 日文字符
  assert_eq_string("Hello, こんにちは!", greet("こんにちは"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  
  // 韩文字符
  assert_eq_string("Hello, 안녕하세요!", greet("안녕하세요"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  
  // 阿拉伯字符
  assert_eq_string("Hello, مرحبا!", greet("مرحبا"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  
  // 印地语字符
  assert_eq_string("Hello, नमस्ते!", greet("नमस्ते"))
  assert_eq_string("Hello, हिन्दी!", greet("हिन्दी"))
  
  // 混合字符和表情符号
  assert_eq_string("Hello, Hello🌍World!", greet("Hello🌍World"))
  assert_eq_string("Hello, 🎉🚀🌟!", greet("🎉🚀🌟"))
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}